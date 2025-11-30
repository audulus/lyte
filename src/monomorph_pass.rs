use crate::*;
use std::collections::{HashMap, HashSet, VecDeque};

/// Manages the monomorphization process, generating specialized versions
/// of generic functions and structs.
pub struct MonomorphPass {
    /// Maps (function_name, type_args) -> mangled_name of the specialized version
    instantiations: HashMap<MonomorphKey, Name>,

    /// Detects infinite generic recursion
    recursion_detector: RecursionDetector,

    /// Newly generated specialized declarations
    specialized_decls: Vec<Decl>,

    /// Work queue of functions that need to be processed
    worklist: VecDeque<Name>,

    /// Functions we've already processed (to avoid duplication)
    processed: HashSet<Name>,
}

impl MonomorphPass {
    pub fn new() -> Self {
        Self {
            instantiations: HashMap::new(),
            recursion_detector: RecursionDetector::new(),
            specialized_decls: Vec::new(),
            worklist: VecDeque::new(),
            processed: HashSet::new(),
        }
    }

    /// Main entry point: monomorphize all functions starting from the entry point.
    ///
    /// This performs a demand-driven monomorphization, only specializing
    /// generic functions that are actually called with concrete types.
    ///
    /// Returns all declarations (original + specialized).
    pub fn monomorphize(
        &mut self,
        decls: &DeclTable,
        entry_point: Name,
    ) -> Result<Vec<Decl>, String> {
        // Start with the entry point
        self.worklist.push_back(entry_point);

        // Process the worklist until empty
        while let Some(func_name) = self.worklist.pop_front() {
            if self.processed.contains(&func_name) {
                continue;
            }
            self.processed.insert(func_name);

            // Find the function declaration
            let func_decls = decls.find(func_name);
            if func_decls.is_empty() {
                // Built-in function or external function
                continue;
            }

            // Process each overload
            for decl in func_decls {
                if let Decl::Func(fdecl) = decl {
                    self.process_function(fdecl, decls)?;
                }
            }
        }

        // Collect all declarations: original + specialized
        let mut all_decls = decls.decls.clone();
        all_decls.extend(self.specialized_decls.clone());
        Ok(all_decls)
    }

    /// Process a single function, finding all generic calls within it
    fn process_function(
        &mut self,
        fdecl: &FuncDecl,
        decls: &DeclTable,
    ) -> Result<(), String> {
        if let Some(body) = fdecl.body {
            self.process_expr(body, fdecl, decls)?;
        }
        Ok(())
    }

    /// Recursively process an expression, looking for function calls
    fn process_expr(
        &mut self,
        expr_id: ExprID,
        fdecl: &FuncDecl,
        decls: &DeclTable,
    ) -> Result<(), String> {
        match &fdecl.arena[expr_id] {
            Expr::Call(fn_id, arg_ids) => {
                // Process the function expression
                self.process_expr(*fn_id, fdecl, decls)?;

                // Process arguments
                for arg_id in arg_ids {
                    self.process_expr(*arg_id, fdecl, decls)?;
                }

                // Check if this is a call to a generic function
                if let Expr::Id(fn_name) = &fdecl.arena[*fn_id] {
                    let fn_type = fdecl.types[*fn_id];
                    self.handle_generic_call(fn_name, fn_type, fdecl, decls)?;
                }
            }
            Expr::Binop(_, lhs, rhs) => {
                self.process_expr(*lhs, fdecl, decls)?;
                self.process_expr(*rhs, fdecl, decls)?;
            }
            Expr::Unop(_, arg) => {
                self.process_expr(*arg, fdecl, decls)?;
            }
            Expr::Let(_, init, _) => {
                self.process_expr(*init, fdecl, decls)?;
            }
            Expr::Var(_, init, _) => {
                if let Some(init_id) = init {
                    self.process_expr(*init_id, fdecl, decls)?;
                }
            }
            Expr::Block(exprs) => {
                for expr in exprs {
                    self.process_expr(*expr, fdecl, decls)?;
                }
            }
            Expr::Field(lhs, _) => {
                self.process_expr(*lhs, fdecl, decls)?;
            }
            Expr::ArrayIndex(lhs, rhs) => {
                self.process_expr(*lhs, fdecl, decls)?;
                self.process_expr(*rhs, fdecl, decls)?;
            }
            Expr::ArrayLiteral(elements) => {
                for elem in elements {
                    self.process_expr(*elem, fdecl, decls)?;
                }
            }
            Expr::If(cond, then_expr, else_expr) => {
                self.process_expr(*cond, fdecl, decls)?;
                self.process_expr(*then_expr, fdecl, decls)?;
                if let Some(else_id) = else_expr {
                    self.process_expr(*else_id, fdecl, decls)?;
                }
            }
            Expr::While(cond, body) => {
                self.process_expr(*cond, fdecl, decls)?;
                self.process_expr(*body, fdecl, decls)?;
            }
            Expr::Lambda { body, .. } => {
                self.process_expr(*body, fdecl, decls)?;
            }
            // Leaf expressions
            Expr::Id(_) | Expr::Int(_) | Expr::UInt(_) | Expr::Real(_) |
            Expr::String(_) | Expr::Char(_) | Expr::True | Expr::False |
            Expr::Return(_) | Expr::Enum(_) | Expr::Error | Expr::Cast |
            Expr::Tuple(_) | Expr::Arena(_) | Expr::Array(_, _) |
            Expr::AsTy(_, _) | Expr::Assign(_, _) | Expr::Macro(_, _) |
            Expr::For { .. } => {}
        }
        Ok(())
    }

    /// Handle a call to a potentially generic function
    fn handle_generic_call(
        &mut self,
        fn_name: &Name,
        fn_type: TypeID,
        caller_fdecl: &FuncDecl,
        decls: &DeclTable,
    ) -> Result<(), String> {
        // Look up the function declaration
        let fn_decls = decls.find(*fn_name);
        if fn_decls.is_empty() {
            // Built-in or external function
            return Ok(());
        }

        // Check if any overload is generic
        for decl in fn_decls {
            if let Decl::Func(target_fdecl) = decl {
                if !target_fdecl.typevars.is_empty() {
                    // This is a generic function - compute type arguments
                    let type_args = self.infer_type_arguments(
                        target_fdecl,
                        fn_type,
                        caller_fdecl,
                    )?;

                    if !type_args.is_empty() {
                        // Create a specialized version
                        self.instantiate_function(*fn_name, type_args, target_fdecl)?;
                    }
                }
            }
        }

        Ok(())
    }

    /// Infer concrete type arguments for a generic function call
    fn infer_type_arguments(
        &self,
        _generic_fdecl: &FuncDecl,
        call_site_type: TypeID,
        _caller_fdecl: &FuncDecl,
    ) -> Result<Vec<TypeID>, String> {
        // Extract concrete types from the call site
        // For now, we'll extract types from a function type
        if let Type::Func(dom, rng) = &*call_site_type {
            // Collect all concrete types mentioned in domain and range
            let mut type_args = Vec::new();
            self.collect_concrete_types(*dom, &mut type_args);
            self.collect_concrete_types(*rng, &mut type_args);
            return Ok(type_args);
        }

        Ok(Vec::new())
    }

    /// Collect all non-variable types from a type expression
    fn collect_concrete_types(&self, ty: TypeID, result: &mut Vec<TypeID>) {
        match &*ty {
            Type::Var(_) | Type::Anon(_) => {
                // Skip type variables
            }
            Type::Tuple(types) => {
                for t in types {
                    self.collect_concrete_types(*t, result);
                }
            }
            Type::Array(elem, _) => {
                self.collect_concrete_types(*elem, result);
            }
            Type::Func(dom, rng) => {
                self.collect_concrete_types(*dom, result);
                self.collect_concrete_types(*rng, result);
            }
            Type::Name(_, params) => {
                result.push(ty);
                for param in params {
                    self.collect_concrete_types(*param, result);
                }
            }
            _ => {
                // Concrete primitive type
                result.push(ty);
            }
        }
    }

    /// Create a specialized version of a generic function
    fn instantiate_function(
        &mut self,
        name: Name,
        type_args: Vec<TypeID>,
        generic_fdecl: &FuncDecl
    ) -> Result<Name, String> {
        let key = MonomorphKey::new(name, type_args.clone());

        // Check if already instantiated
        if let Some(mangled) = self.instantiations.get(&key) {
            return Ok(*mangled);
        }

        // Check for infinite recursion
        self.recursion_detector.check(&key)?;
        self.recursion_detector.begin_instantiation(key.clone());

        // Generate mangled name
        let mangled_name = key.mangled_name();

        // Create type substitution map
        let mut instance = Instance::new();
        for (type_param, type_arg) in generic_fdecl.typevars.iter().zip(type_args.iter()) {
            let type_var = typevar(&type_param.to_string());
            instance.insert(type_var, *type_arg);
        }

        // Clone and specialize the function declaration
        let mut specialized = generic_fdecl.clone();
        specialized.name = mangled_name;
        specialized.typevars = Vec::new(); // No longer generic

        // Substitute types in return type
        specialized.ret = specialized.ret.subst(&instance);

        // Substitute types in parameters
        for param in &mut specialized.params {
            if let Some(ref mut ty) = param.ty {
                *ty = ty.subst(&instance);
            }
        }

        // Substitute types in the body's type map
        for ty in specialized.types.iter_mut() {
            *ty = ty.subst(&instance);
        }

        // Record the instantiation
        self.instantiations.insert(key, mangled_name);

        // Add to specialized decls
        self.specialized_decls.push(Decl::Func(specialized.clone()));

        // Add to worklist to process its body
        self.worklist.push_back(mangled_name);

        self.recursion_detector.end_instantiation();

        Ok(mangled_name)
    }

    /// Get all generated specialized declarations
    pub fn specialized_declarations(&self) -> &[Decl] {
        &self.specialized_decls
    }

    /// Get the mangled name for a specific instantiation, if it exists
    pub fn get_instantiation(&self, key: &MonomorphKey) -> Option<Name> {
        self.instantiations.get(key).copied()
    }

    /// Get the full rewrite map for all instantiations
    pub fn get_rewrite_map(&self) -> &HashMap<MonomorphKey, Name> {
        &self.instantiations
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn mk_simple_func(name: &str, typevars: Vec<&str>) -> FuncDecl {
        FuncDecl {
            name: Name::str(name),
            typevars: typevars.iter().map(|s| Name::str(s)).collect(),
            params: Vec::new(),
            constraints: Vec::new(),
            ret: mk_type(Type::Void),
            body: Some(0),
            arena: ExprArena::new(),
            types: Vec::new(),
            loc: test_loc(),
        }
    }

    #[test]
    fn test_monomorph_pass_creation() {
        let pass = MonomorphPass::new();
        assert_eq!(pass.instantiations.len(), 0);
        assert_eq!(pass.specialized_decls.len(), 0);
    }

    #[test]
    fn test_instantiate_simple_function() {
        let mut pass = MonomorphPass::new();
        let generic_func = mk_simple_func("id", vec!["T"]);

        let type_args = vec![mk_type(Type::Int32)];
        let result = pass.instantiate_function(
            Name::str("id"),
            type_args,
            &generic_func,
        );

        assert!(result.is_ok());
        let mangled = result.unwrap();
        assert_eq!(mangled, Name::str("id$i32"));
        assert_eq!(pass.specialized_decls.len(), 1);
    }

    #[test]
    fn test_instantiate_same_function_twice() {
        let mut pass = MonomorphPass::new();
        let generic_func = mk_simple_func("id", vec!["T"]);

        let type_args = vec![mk_type(Type::Int32)];

        // First instantiation
        let result1 = pass.instantiate_function(
            Name::str("id"),
            type_args.clone(),
            &generic_func,
        );
        assert!(result1.is_ok());

        // Second instantiation - should reuse
        let result2 = pass.instantiate_function(
            Name::str("id"),
            type_args,
            &generic_func,
        );
        assert!(result2.is_ok());
        assert_eq!(result1.unwrap(), result2.unwrap());

        // Should only have one specialized version
        assert_eq!(pass.specialized_decls.len(), 1);
    }

    #[test]
    fn test_instantiate_different_type_args() {
        let mut pass = MonomorphPass::new();
        let generic_func = mk_simple_func("id", vec!["T"]);

        // id<i32>
        let result1 = pass.instantiate_function(
            Name::str("id"),
            vec![mk_type(Type::Int32)],
            &generic_func,
        );
        assert!(result1.is_ok());
        assert_eq!(result1.unwrap(), Name::str("id$i32"));

        // id<bool>
        let result2 = pass.instantiate_function(
            Name::str("id"),
            vec![mk_type(Type::Bool)],
            &generic_func,
        );
        assert!(result2.is_ok());
        assert_eq!(result2.unwrap(), Name::str("id$bool"));

        // Should have two specialized versions
        assert_eq!(pass.specialized_decls.len(), 2);
    }

    #[test]
    fn test_instantiate_multiple_type_params() {
        let mut pass = MonomorphPass::new();
        let generic_func = mk_simple_func("map", vec!["T0", "T1"]);

        let type_args = vec![mk_type(Type::Int32), mk_type(Type::Bool)];
        let result = pass.instantiate_function(
            Name::str("map"),
            type_args,
            &generic_func,
        );

        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Name::str("map$i32$bool"));
    }

    #[test]
    fn test_collect_concrete_types_primitive() {
        let pass = MonomorphPass::new();
        let mut result = Vec::new();

        pass.collect_concrete_types(mk_type(Type::Int32), &mut result);
        assert_eq!(result.len(), 1);
        assert_eq!(*result[0], Type::Int32);
    }

    #[test]
    fn test_collect_concrete_types_ignores_type_vars() {
        let pass = MonomorphPass::new();
        let mut result = Vec::new();

        pass.collect_concrete_types(typevar("T"), &mut result);
        assert_eq!(result.len(), 0);
    }

    #[test]
    fn test_collect_concrete_types_tuple() {
        let pass = MonomorphPass::new();
        let mut result = Vec::new();

        let tuple_ty = mk_type(Type::Tuple(vec![
            mk_type(Type::Int32),
            mk_type(Type::Bool),
        ]));

        pass.collect_concrete_types(tuple_ty, &mut result);
        assert_eq!(result.len(), 2);
    }

    #[test]
    fn test_collect_concrete_types_array() {
        let pass = MonomorphPass::new();
        let mut result = Vec::new();

        let array_ty = mk_type(Type::Array(mk_type(Type::Float32), 10));

        pass.collect_concrete_types(array_ty, &mut result);
        assert_eq!(result.len(), 1);
        assert_eq!(*result[0], Type::Float32);
    }

    #[test]
    fn test_collect_concrete_types_named() {
        let pass = MonomorphPass::new();
        let mut result = Vec::new();

        let named_ty = mk_type(Type::Name(
            Name::str("Vec"),
            vec![mk_type(Type::Int32)],
        ));

        pass.collect_concrete_types(named_ty, &mut result);
        // Should collect both the named type and its parameter
        assert!(result.len() >= 1);
    }

    #[test]
    fn test_process_expr_block() {
        let mut pass = MonomorphPass::new();
        let decls = DeclTable::new(vec![]);

        let mut arena = ExprArena::new();
        let expr1 = arena.add(Expr::Int(1), test_loc());
        let expr2 = arena.add(Expr::Int(2), test_loc());
        let block = arena.add(Expr::Block(vec![expr1, expr2]), test_loc());

        let fdecl = FuncDecl {
            name: Name::str("test"),
            typevars: Vec::new(),
            params: Vec::new(),
            constraints: Vec::new(),
            ret: mk_type(Type::Void),
            body: Some(block),
            arena,
            types: vec![mk_type(Type::Void); 3],
            loc: test_loc(),
        };

        let result = pass.process_expr(block, &fdecl, &decls);
        assert!(result.is_ok());
    }

    #[test]
    fn test_process_expr_binop() {
        let mut pass = MonomorphPass::new();
        let decls = DeclTable::new(vec![]);

        let mut arena = ExprArena::new();
        let lhs = arena.add(Expr::Int(1), test_loc());
        let rhs = arena.add(Expr::Int(2), test_loc());
        let binop = arena.add(Expr::Binop(Binop::Plus, lhs, rhs), test_loc());

        let fdecl = FuncDecl {
            name: Name::str("test"),
            typevars: Vec::new(),
            params: Vec::new(),
            constraints: Vec::new(),
            ret: mk_type(Type::Void),
            body: Some(binop),
            arena,
            types: vec![mk_type(Type::Int32); 3],
            loc: test_loc(),
        };

        let result = pass.process_expr(binop, &fdecl, &decls);
        assert!(result.is_ok());
    }

    #[test]
    fn test_process_expr_array_literal() {
        let mut pass = MonomorphPass::new();
        let decls = DeclTable::new(vec![]);

        let mut arena = ExprArena::new();
        let elem1 = arena.add(Expr::Int(1), test_loc());
        let elem2 = arena.add(Expr::Int(2), test_loc());
        let array = arena.add(Expr::ArrayLiteral(vec![elem1, elem2]), test_loc());

        let fdecl = FuncDecl {
            name: Name::str("test"),
            typevars: Vec::new(),
            params: Vec::new(),
            constraints: Vec::new(),
            ret: mk_type(Type::Void),
            body: Some(array),
            arena,
            types: vec![mk_type(Type::Int32); 3],
            loc: test_loc(),
        };

        let result = pass.process_expr(array, &fdecl, &decls);
        assert!(result.is_ok());
    }

    #[test]
    fn test_get_instantiation() {
        let mut pass = MonomorphPass::new();
        let generic_func = mk_simple_func("id", vec!["T"]);

        let type_args = vec![mk_type(Type::Int32)];
        let key = MonomorphKey::new(Name::str("id"), type_args.clone());

        // Before instantiation
        assert!(pass.get_instantiation(&key).is_none());

        // After instantiation
        pass.instantiate_function(Name::str("id"), type_args, &generic_func)
            .unwrap();
        assert_eq!(pass.get_instantiation(&key), Some(Name::str("id$i32")));
    }

    #[test]
    fn test_specialized_declarations() {
        let mut pass = MonomorphPass::new();
        let generic_func = mk_simple_func("id", vec!["T"]);

        assert_eq!(pass.specialized_declarations().len(), 0);

        pass.instantiate_function(
            Name::str("id"),
            vec![mk_type(Type::Int32)],
            &generic_func,
        ).unwrap();

        assert_eq!(pass.specialized_declarations().len(), 1);
    }

    #[test]
    fn test_type_substitution_in_specialized_func() {
        let mut pass = MonomorphPass::new();

        // Create a generic function id<T>(x: T) -> T
        let t_var = typevar("T");
        let mut generic_func = mk_simple_func("id", vec!["T"]);
        generic_func.params = vec![Param {
            name: Name::str("x"),
            ty: Some(t_var),
        }];
        generic_func.ret = t_var;

        // Instantiate with i32
        pass.instantiate_function(
            Name::str("id"),
            vec![mk_type(Type::Int32)],
            &generic_func,
        ).unwrap();

        // Check the specialized declaration
        let specialized = &pass.specialized_decls[0];
        if let Decl::Func(fdecl) = specialized {
            assert_eq!(fdecl.name, Name::str("id$i32"));
            assert_eq!(fdecl.typevars.len(), 0); // No longer generic

            // Check that return type was substituted
            assert_eq!(*fdecl.ret, Type::Int32);

            // Check that parameter type was substituted
            assert_eq!(fdecl.params.len(), 1);
            assert_eq!(*fdecl.params[0].ty.unwrap(), Type::Int32);
        } else {
            panic!("Expected function declaration");
        }
    }

    #[test]
    fn test_monomorphize_with_empty_decls() {
        let mut pass = MonomorphPass::new();
        let decls = DeclTable::new(vec![]);

        let result = pass.monomorphize(&decls, Name::str("main"));
        assert!(result.is_ok());
        assert_eq!(result.unwrap().len(), 0);
    }

    #[test]
    fn test_worklist_processing() {
        let mut pass = MonomorphPass::new();

        // Add some items to worklist
        pass.worklist.push_back(Name::str("func1"));
        pass.worklist.push_back(Name::str("func2"));

        assert_eq!(pass.worklist.len(), 2);

        // Simulate processing
        let func1 = pass.worklist.pop_front().unwrap();
        assert_eq!(func1, Name::str("func1"));
        assert_eq!(pass.worklist.len(), 1);
    }

    #[test]
    fn test_processed_tracking() {
        let mut pass = MonomorphPass::new();

        let name = Name::str("test");
        assert!(!pass.processed.contains(&name));

        pass.processed.insert(name);
        assert!(pass.processed.contains(&name));
    }

    #[test]
    fn test_instantiate_with_nested_generic() {
        let mut pass = MonomorphPass::new();

        // Create a generic function that works with nested types
        let mut generic_func = mk_simple_func("process", vec!["T"]);
        let t_var = typevar("T");
        let array_of_t = mk_type(Type::Array(t_var, 10));
        generic_func.params = vec![Param {
            name: Name::str("arr"),
            ty: Some(array_of_t),
        }];
        generic_func.ret = t_var;

        // Instantiate with i32 -> should create process$i32
        let result = pass.instantiate_function(
            Name::str("process"),
            vec![mk_type(Type::Int32)],
            &generic_func,
        );

        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Name::str("process$i32"));

        // Check that the specialized version has the correct array type
        let specialized = &pass.specialized_decls[0];
        if let Decl::Func(fdecl) = specialized {
            assert_eq!(fdecl.params.len(), 1);
            if let Type::Array(elem_ty, size) = &*fdecl.params[0].ty.unwrap() {
                assert_eq!(**elem_ty, Type::Int32);
                assert_eq!(*size, 10);
            } else {
                panic!("Expected array type");
            }
        }
    }

    #[test]
    fn test_multiple_instantiations_same_function() {
        let mut pass = MonomorphPass::new();

        let generic_func = mk_simple_func("id", vec!["T"]);

        // Create three different instantiations
        let types = vec![
            mk_type(Type::Int32),
            mk_type(Type::Bool),
            mk_type(Type::Float32),
        ];

        for ty in types {
            pass.instantiate_function(
                Name::str("id"),
                vec![ty],
                &generic_func,
            ).unwrap();
        }

        // Should have 3 specialized versions
        assert_eq!(pass.specialized_decls.len(), 3);
        assert_eq!(pass.instantiations.len(), 3);
    }

    #[test]
    fn test_handle_generic_call_with_non_generic() {
        let mut pass = MonomorphPass::new();

        // Create a non-generic function
        let non_generic = FuncDecl {
            name: Name::str("non_generic"),
            typevars: Vec::new(),
            params: Vec::new(),
            constraints: Vec::new(),
            ret: mk_type(Type::Void),
            body: Some(0),
            arena: ExprArena::new(),
            types: Vec::new(),
            loc: test_loc(),
        };

        let decls = DeclTable::new(vec![Decl::Func(non_generic.clone())]);

        let fn_type = mk_type(Type::Func(
            mk_type(Type::Tuple(Vec::new())),
            mk_type(Type::Void),
        ));

        // Call handle_generic_call - should not create any specializations
        let result = pass.handle_generic_call(
            &Name::str("non_generic"),
            fn_type,
            &non_generic,
            &decls,
        );

        assert!(result.is_ok());
        assert_eq!(pass.specialized_decls.len(), 0);
    }

    #[test]
    fn test_instantiate_function_with_constraints() {
        let mut pass = MonomorphPass::new();

        // Create a generic function with interface constraints
        let mut generic_func = mk_simple_func("add", vec!["T"]);
        generic_func.constraints = vec![InterfaceConstraint {
            interface_name: Name::str("Addable"),
            typevars: vec![Name::str("T")],
        }];

        let result = pass.instantiate_function(
            Name::str("add"),
            vec![mk_type(Type::Int32)],
            &generic_func,
        );

        assert!(result.is_ok());

        // Check that constraints are preserved (they're on the original, not the specialized)
        let specialized = &pass.specialized_decls[0];
        if let Decl::Func(fdecl) = specialized {
            // Specialized version should have constraints copied
            assert_eq!(fdecl.constraints.len(), 1);
            assert_eq!(fdecl.constraints[0].interface_name, Name::str("Addable"));
        }
    }

    #[test]
    fn test_collect_concrete_types_function_type() {
        let pass = MonomorphPass::new();
        let mut result = Vec::new();

        let func_ty = mk_type(Type::Func(
            mk_type(Type::Int32),
            mk_type(Type::Bool),
        ));

        pass.collect_concrete_types(func_ty, &mut result);
        assert_eq!(result.len(), 2);
        assert_eq!(*result[0], Type::Int32);
        assert_eq!(*result[1], Type::Bool);
    }

    #[test]
    fn test_instantiation_deduplication() {
        let mut pass = MonomorphPass::new();

        let generic_func = mk_simple_func("id", vec!["T"]);
        let type_args = vec![mk_type(Type::Int32)];

        // First instantiation
        let key1 = MonomorphKey::new(Name::str("id"), type_args.clone());
        pass.instantiate_function(
            Name::str("id"),
            type_args.clone(),
            &generic_func,
        ).unwrap();

        assert_eq!(pass.specialized_decls.len(), 1);

        // Same instantiation again - should not create duplicate
        pass.instantiate_function(
            Name::str("id"),
            type_args.clone(),
            &generic_func,
        ).unwrap();

        assert_eq!(pass.specialized_decls.len(), 1);
        assert!(pass.instantiations.contains_key(&key1));
    }

    #[test]
    fn test_expr_traversal_coverage() {
        let mut pass = MonomorphPass::new();
        let decls = DeclTable::new(vec![]);

        // Test If expression
        let mut arena = ExprArena::new();
        let cond = arena.add(Expr::True, test_loc());
        let then_expr = arena.add(Expr::Int(1), test_loc());
        let else_expr = arena.add(Expr::Int(2), test_loc());
        let if_expr = arena.add(Expr::If(cond, then_expr, Some(else_expr)), test_loc());

        let fdecl = FuncDecl {
            name: Name::str("test"),
            typevars: Vec::new(),
            params: Vec::new(),
            constraints: Vec::new(),
            ret: mk_type(Type::Void),
            body: Some(if_expr),
            arena,
            types: vec![mk_type(Type::Bool), mk_type(Type::Int32), mk_type(Type::Int32), mk_type(Type::Int32)],
            loc: test_loc(),
        };

        let result = pass.process_expr(if_expr, &fdecl, &decls);
        assert!(result.is_ok());
    }

    #[test]
    fn test_monomorphize_single_entry_point() {
        let mut pass = MonomorphPass::new();

        // Create a simple non-generic entry point function
        // main() { 42 }
        let mut arena = ExprArena::new();
        let body_expr = arena.add(Expr::Int(42), test_loc());

        let entry_func = FuncDecl {
            name: Name::str("main"),
            typevars: Vec::new(),
            params: Vec::new(),
            constraints: Vec::new(),
            ret: mk_type(Type::Int32),
            body: Some(body_expr),
            arena,
            types: vec![mk_type(Type::Int32)],
            loc: test_loc(),
        };

        let decls = DeclTable::new(vec![Decl::Func(entry_func)]);

        // Monomorphize starting from "main"
        let result = pass.monomorphize(&decls, Name::str("main"));

        assert!(result.is_ok());
        let all_decls = result.unwrap();

        // Should have 1 decl (just main, no specializations)
        assert_eq!(all_decls.len(), 1);

        // The entry point should have been processed
        assert!(pass.processed.contains(&Name::str("main")));
    }

    #[test]
    fn test_monomorphize_entry_point_calling_generic() {
        let mut pass = MonomorphPass::new();

        // Create a generic function id<T>(x: T) -> T { x }
        let t_var = typevar("T");
        let mut id_arena = ExprArena::new();
        let id_param_expr = id_arena.add(Expr::Id(Name::str("x")), test_loc());

        let id_func = FuncDecl {
            name: Name::str("id"),
            typevars: vec![Name::str("T")],
            params: vec![Param {
                name: Name::str("x"),
                ty: Some(t_var),
            }],
            constraints: Vec::new(),
            ret: t_var,
            body: Some(id_param_expr),
            arena: id_arena,
            types: vec![t_var],
            loc: test_loc(),
        };

        // Create entry point function that calls id(42)
        // main() { id(42) }
        let mut main_arena = ExprArena::new();
        let arg_expr = main_arena.add(Expr::Int(42), test_loc());
        let fn_expr = main_arena.add(Expr::Id(Name::str("id")), test_loc());
        let call_expr = main_arena.add(Expr::Call(fn_expr, vec![arg_expr]), test_loc());

        let i32_type = mk_type(Type::Int32);
        let func_type = mk_type(Type::Func(i32_type, i32_type));

        let main_func = FuncDecl {
            name: Name::str("main"),
            typevars: Vec::new(),
            params: Vec::new(),
            constraints: Vec::new(),
            ret: i32_type,
            body: Some(call_expr),
            arena: main_arena,
            types: vec![i32_type, func_type, i32_type],
            loc: test_loc(),
        };

        let decls = DeclTable::new(vec![
            Decl::Func(id_func),
            Decl::Func(main_func),
        ]);

        // Monomorphize starting from "main"
        let result = pass.monomorphize(&decls, Name::str("main"));

        assert!(result.is_ok());
        let all_decls = result.unwrap();

        // Should have 3 decls: id (generic), main, id$i32 (specialized)
        assert_eq!(all_decls.len(), 3);

        // Find the specialized version
        let specialized_id = all_decls.iter().find(|d| {
            if let Decl::Func(f) = d {
                f.name.to_string().starts_with("id$")
            } else {
                false
            }
        });

        assert!(specialized_id.is_some());
        if let Some(Decl::Func(fdecl)) = specialized_id {
            // Should be specialized (no type variables)
            assert_eq!(fdecl.typevars.len(), 0);
            // Return type should be i32
            assert_eq!(*fdecl.ret, Type::Int32);
            // Name should start with "id$"
            assert!(fdecl.name.to_string().starts_with("id$"));

            // The specialized id function should be in the processed set
            assert!(pass.processed.contains(&fdecl.name));
        } else {
            panic!("Expected function declaration");
        }

        // Both main and the specialized id function should be processed
        assert!(pass.processed.contains(&Name::str("main")));
    }
}
