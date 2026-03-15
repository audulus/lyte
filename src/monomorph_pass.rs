use crate::*;
use std::collections::{HashMap, HashSet};

/// Manages the monomorphization process, generating specialized versions
/// of generic functions and structs.
pub struct MonomorphPass {
    /// Maps (function_name, type_args) -> mangled_name of the specialized version
    instantiations: HashMap<MonomorphKey, Name>,

    /// Detects infinite generic recursion
    recursion_detector: RecursionDetector,

    /// Newly generated specialized declarations
    out_decls: Vec<Decl>,

    /// Non-generic functions whose bodies have been processed (to prevent reprocessing).
    processed_non_generic: HashSet<Name>,
}

impl MonomorphPass {
    pub fn new() -> Self {
        Self {
            instantiations: HashMap::new(),
            recursion_detector: RecursionDetector::new(),
            out_decls: Vec::new(),
            processed_non_generic: HashSet::new(),
        }
    }

    /// Main entry point: monomorphize all functions starting from the entry point.
    ///
    /// This performs a demand-driven monomorphization, only specializing
    /// generic functions that are actually called with concrete types.
    ///
    /// Returns all reached function declarations and other declarations.
    pub fn monomorphize(
        &mut self,
        decls: &DeclTable,
        entry_point: Name,
    ) -> Result<Vec<Decl>, String> {
        self.monomorphize_multi(decls, &[entry_point])
    }

    /// Monomorphize starting from multiple entry points.
    ///
    /// Each entry point is processed as a root. The `processed_non_generic`
    /// set prevents reprocessing shared functions reached from multiple roots.
    pub fn monomorphize_multi(
        &mut self,
        decls: &DeclTable,
        entry_points: &[Name],
    ) -> Result<Vec<Decl>, String> {
        for &entry_point in entry_points {
            let func_decls = decls.find(entry_point);
            if func_decls.is_empty() {
                return Err(format!(
                    "entry point function '{}' not found",
                    entry_point
                ));
            }

            if func_decls.len() > 1 {
                return Err(format!(
                    "Multiple overloads found for entry point function '{}'",
                    entry_point
                ));
            }

            if let Decl::Func(fdecl) = &func_decls[0] {
                if !self.processed_non_generic.contains(&fdecl.name) {
                    self.processed_non_generic.insert(fdecl.name);
                    let mut fdecl = fdecl.clone();
                    self.process_function(&mut fdecl, decls)?;
                    self.out_decls.push(Decl::Func(fdecl));
                }
            } else {
                return Err(format!("Entry point '{}' is not a function", entry_point));
            }
        }

        for decl in decls.decls.iter() {
            if let Decl::Func(_) = decl {
                // Do nothing - functions are processed on demand
            } else {
                // Non-function declarations - include as is
                self.out_decls.push(decl.clone());
            }
        }

        // Collect all declarations: original + specialized
        Ok(self.out_decls.clone())
    }

    /// Process a single function, finding all generic calls within it
    fn process_function(&mut self, fdecl: &mut FuncDecl, decls: &DeclTable) -> Result<(), String> {
        if let Some(body) = fdecl.body {
            self.process_expr(body, fdecl, decls)?;
        }
        Ok(())
    }

    /// Recursively process an expression, looking for function calls
    fn process_expr(
        &mut self,
        expr_id: ExprID,
        fdecl: &mut FuncDecl,
        decls: &DeclTable,
    ) -> Result<(), String> {
        // Clone the expression to avoid borrow checker issues
        let expr = fdecl.arena[expr_id].clone();

        match &expr {
            Expr::Id(name) => {
                // Check if this identifier refers to a function
                // Get the solved type for this expression
                let solved_type = fdecl.types[expr_id];

                // Look up the declaration
                let fn_decls = decls.find(*name);
                for decl in fn_decls {
                    if let Decl::Func(target_fdecl) = decl {
                        if !target_fdecl.typevars.is_empty() {
                            // This is a generic function - compute type arguments from solved type
                            let type_args =
                                self.infer_type_arguments(target_fdecl, solved_type, fdecl)?;

                            if !type_args.is_empty() {
                                // Create a specialized version
                                let mangled_name = self.instantiate_function(
                                    *name,
                                    type_args,
                                    target_fdecl,
                                    decls,
                                )?;

                                // Rewrite the identifier to use the mangled name
                                fdecl.arena.exprs[expr_id] = Expr::Id(mangled_name);
                            }
                        } else {
                            // Non-generic function - include it and recursively process its body.
                            let non_generic_overload_count = fn_decls
                                .iter()
                                .filter(|d| matches!(d, Decl::Func(f) if f.typevars.is_empty()))
                                .count();

                            if non_generic_overload_count > 1 {
                                let func_ty = TypeID::new(Type::Func(
                                    target_fdecl.domain(),
                                    target_fdecl.ret,
                                ));
                                if func_ty == solved_type {
                                    let param_types = target_fdecl.param_types();
                                    let mangled =
                                        crate::mangle::mangle_overload(*name, &param_types);

                                    if !self.processed_non_generic.contains(&mangled) {
                                        self.processed_non_generic.insert(mangled);
                                        let mut func = target_fdecl.clone();
                                        func.name = mangled;
                                        self.process_function(&mut func, decls)?;
                                        self.out_decls.push(Decl::Func(func));
                                    }

                                    fdecl.arena.exprs[expr_id] = Expr::Id(mangled);
                                }
                            } else {
                                if !self.processed_non_generic.contains(&target_fdecl.name) {
                                    self.processed_non_generic.insert(target_fdecl.name);
                                    let mut func = target_fdecl.clone();
                                    self.process_function(&mut func, decls)?;
                                    self.out_decls.push(Decl::Func(func));
                                }
                            }
                        }
                    }
                }
            }
            Expr::Call(fn_id, arg_ids) => {
                // Process arguments first
                let arg_ids = arg_ids.clone();
                for arg_id in &arg_ids {
                    self.process_expr(*arg_id, fdecl, decls)?;
                }

                let fn_id = *fn_id;

                // Check if this is a call to a size-var generic function.
                // We handle it here because we need the solved argument types.
                if let Expr::Id(fn_name) = fdecl.arena[fn_id].clone() {
                    let fn_decls = decls.find(fn_name);
                    let size_var_func = fn_decls.iter().find_map(|d| {
                        if let Decl::Func(f) = d {
                            if !f.size_vars.is_empty() {
                                Some(f.clone())
                            } else {
                                None
                            }
                        } else {
                            None
                        }
                    });
                    if let Some(target_fdecl) = size_var_func {
                        // Infer size bindings from argument types vs generic param types.
                        let size_bindings = infer_size_bindings(&target_fdecl, &arg_ids, fdecl);
                        if !size_bindings.is_empty() {
                            let size_args: Vec<i32> = target_fdecl
                                .size_vars
                                .iter()
                                .map(|sv| size_bindings.get(sv).copied().unwrap_or(0))
                                .collect();
                            // Also infer type arguments if the function has type vars.
                            let type_args = if !target_fdecl.typevars.is_empty() {
                                let solved_type = fdecl.types[fn_id];
                                self.infer_type_arguments(&target_fdecl, solved_type, fdecl)?
                            } else {
                                vec![]
                            };
                            let mangled = self.instantiate_function_with_sizes(
                                fn_name,
                                type_args,
                                size_args,
                                &target_fdecl,
                                decls,
                            )?;
                            fdecl.arena.exprs[fn_id] = Expr::Id(mangled);
                            // Update ALL caller types to substitute the resolved size vars.
                            for ty in fdecl.types.iter_mut() {
                                *ty = subst_size_vars(*ty, &size_bindings);
                            }
                            return Ok(());
                        }
                    }
                }

                // Process the function expression (which handles Id rewriting for type-var generics)
                self.process_expr(fn_id, fdecl, decls)?;
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
            Expr::Return(inner) => {
                self.process_expr(*inner, fdecl, decls)?;
            }
            Expr::For {
                start, end, body, ..
            } => {
                let (start, end, body) = (*start, *end, *body);
                self.process_expr(start, fdecl, decls)?;
                self.process_expr(end, fdecl, decls)?;
                self.process_expr(body, fdecl, decls)?;
            }
            Expr::AsTy(inner, _) => {
                self.process_expr(*inner, fdecl, decls)?;
            }
            Expr::Tuple(elems) => {
                for e in elems.clone() {
                    self.process_expr(e, fdecl, decls)?;
                }
            }
            Expr::Arena(inner) => {
                self.process_expr(*inner, fdecl, decls)?;
            }
            Expr::Array(val, sz) => {
                let (val, sz) = (*val, *sz);
                self.process_expr(val, fdecl, decls)?;
                self.process_expr(sz, fdecl, decls)?;
            }
            // True leaves
            Expr::Int(_)
            | Expr::UInt(_)
            | Expr::Real(_)
            | Expr::String(_)
            | Expr::Char(_)
            | Expr::True
            | Expr::False
            | Expr::Enum(_)
            | Expr::Error
            | Expr::Macro(_, _)
            | Expr::Break
            | Expr::Continue => {}
        }
        Ok(())
    }

    /// Infer concrete type arguments for a generic function call.
    fn infer_type_arguments(
        &self,
        generic_fdecl: &FuncDecl,
        call_site_type: TypeID,
        _caller_fdecl: &FuncDecl,
    ) -> Result<Vec<TypeID>, String> {
        if generic_fdecl.typevars.is_empty() {
            return Ok(Vec::new());
        }
        // Build the generic function type using the same convention as the
        // type checker: domain is always a tuple of parameter types.
        let generic_func_type = generic_fdecl.ty();
        // Convert named type variables (Var) to anonymous (Anon) for unification.
        let mut fresh_index = 1000;
        let mut fresh_inst = Instance::new();
        let fresh_func_type = generic_func_type.fresh_aux(&mut fresh_index, &mut fresh_inst);
        // Build map from typevar name to its Anon type.
        let var_to_anon: Vec<(Name, TypeID)> = generic_fdecl
            .typevars
            .iter()
            .map(|tv_name| {
                let tv = typevar(&tv_name.to_string());
                let anon_ty = fresh_inst.get(&tv).copied().unwrap_or(tv);
                (*tv_name, anon_ty)
            })
            .collect();
        // Unify the fresh generic type with the call-site type.
        let mut unify_inst = Instance::new();
        if !unify(fresh_func_type, call_site_type, &mut unify_inst) {
            return Err(format!(
                "Cannot infer type arguments for {}",
                generic_fdecl.name
            ));
        }
        // Extract type arguments: look up what each Anon resolved to.
        let mut type_args = Vec::new();
        for (_tv_name, anon_ty) in &var_to_anon {
            let resolved = find(*anon_ty, &unify_inst);
            type_args.push(resolved);
        }
        Ok(type_args)
    }

    /// Create a specialized version of a generic function (type vars only).
    fn instantiate_function(
        &mut self,
        name: Name,
        type_args: Vec<TypeID>,
        generic_fdecl: &FuncDecl,
        decls: &DeclTable,
    ) -> Result<Name, String> {
        self.instantiate_function_with_sizes(name, type_args, vec![], generic_fdecl, decls)
    }

    /// Create a specialized version of a generic function with both type and size args.
    fn instantiate_function_with_sizes(
        &mut self,
        name: Name,
        type_args: Vec<TypeID>,
        size_args: Vec<i32>,
        generic_fdecl: &FuncDecl,
        decls: &DeclTable,
    ) -> Result<Name, String> {
        let key = MonomorphKey::new_with_sizes(name, type_args.clone(), size_args.clone());

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

        // Create size substitution map: Name -> i32
        let size_bindings: HashMap<Name, i32> = generic_fdecl
            .size_vars
            .iter()
            .zip(size_args.iter())
            .map(|(sv, &n)| (*sv, n))
            .collect();

        // Clone and specialize the function declaration
        let mut specialized = generic_fdecl.clone();
        specialized.name = mangled_name;
        specialized.typevars = Vec::new();
        specialized.size_vars = Vec::new(); // No longer generic

        // Substitute types in return type (type vars + size vars in array sizes)
        specialized.ret = subst_size_vars(specialized.ret.subst(&instance), &size_bindings);

        // Substitute types in parameters
        for param in &mut specialized.params {
            if let Some(ref mut ty) = param.ty {
                *ty = subst_size_vars(ty.subst(&instance), &size_bindings);
            }
        }

        // Substitute types in the body's type map
        for ty in specialized.types.iter_mut() {
            *ty = subst_size_vars(ty.subst(&instance), &size_bindings);
        }

        // Substitute size vars in the body expressions (e.g. Expr::Id("N") → Expr::Int(3))
        if !size_bindings.is_empty() {
            substitute_size_var_exprs(&mut specialized.arena, &size_bindings);
        }

        // Record the instantiation
        self.instantiations.insert(key, mangled_name);

        // Recursively process the specialized function's body immediately
        self.process_function(&mut specialized, decls)?;

        // Add to specialized decls
        self.out_decls.push(Decl::Func(specialized));

        self.recursion_detector.end_instantiation();

        Ok(mangled_name)
    }

    /// Get all generated specialized declarations
    pub fn specialized_declarations(&self) -> &[Decl] {
        &self.out_decls
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

/// Substitute `ArraySize::Var(N)` → `Known(n)` in a type using the size bindings.
fn subst_size_vars(ty: TypeID, bindings: &HashMap<Name, i32>) -> TypeID {
    match &*ty {
        Type::Array(elem, ArraySize::Var(name)) => {
            let elem2 = subst_size_vars(*elem, bindings);
            let size = bindings
                .get(name)
                .copied()
                .map(ArraySize::Known)
                .unwrap_or_else(|| ArraySize::Var(*name));
            mk_type(Type::Array(elem2, size))
        }
        Type::Array(elem, sz) => mk_type(Type::Array(subst_size_vars(*elem, bindings), sz.clone())),
        Type::Tuple(vs) => mk_type(Type::Tuple(
            vs.iter().map(|t| subst_size_vars(*t, bindings)).collect(),
        )),
        Type::Func(a, b) => mk_type(Type::Func(
            subst_size_vars(*a, bindings),
            subst_size_vars(*b, bindings),
        )),
        Type::Name(n, ps) => mk_type(Type::Name(
            *n,
            ps.iter().map(|t| subst_size_vars(*t, bindings)).collect(),
        )),
        _ => ty,
    }
}

/// Replace `Expr::Id(N)` with `Expr::Int(n)` for each size var binding, across all arena slots.
fn substitute_size_var_exprs(arena: &mut ExprArena, bindings: &HashMap<Name, i32>) {
    for slot in arena.exprs.iter_mut() {
        if let Expr::Id(name) = slot {
            if let Some(&val) = bindings.get(name) {
                *slot = Expr::Int(val as i64);
            }
        }
    }
}

/// Walk a type pair (generic param type vs concrete arg type) to extract size var bindings.
fn infer_size_bindings_pair(generic: TypeID, concrete: TypeID, out: &mut HashMap<Name, i32>) {
    match (&*generic, &*concrete) {
        (Type::Array(ge, ArraySize::Var(name)), Type::Array(ce, ArraySize::Known(n)))
            if *n != 0 =>
        {
            out.insert(*name, *n);
            infer_size_bindings_pair(*ge, *ce, out);
        }
        (Type::Array(ge, _), Type::Array(ce, _)) => infer_size_bindings_pair(*ge, *ce, out),
        (Type::Tuple(gs), Type::Tuple(cs)) => {
            for (g, c) in gs.iter().zip(cs.iter()) {
                infer_size_bindings_pair(*g, *c, out);
            }
        }
        (Type::Func(ga, gb), Type::Func(ca, cb)) => {
            infer_size_bindings_pair(*ga, *ca, out);
            infer_size_bindings_pair(*gb, *cb, out);
        }
        _ => {}
    }
}

/// Infer size var bindings for a call to `target` given the solved types of the argument expressions.
fn infer_size_bindings(
    target: &FuncDecl,
    arg_ids: &[ExprID],
    caller: &FuncDecl,
) -> HashMap<Name, i32> {
    let mut out = HashMap::new();
    for (param, &arg_id) in target.params.iter().zip(arg_ids.iter()) {
        if let Some(param_ty) = param.ty {
            let concrete_ty = caller.types[arg_id];
            infer_size_bindings_pair(param_ty, concrete_ty, &mut out);
        }
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;

    fn mk_simple_func(name: &str, typevars: Vec<&str>) -> FuncDecl {
        let mut arena = ExprArena::new();
        let body_expr = arena.add(Expr::Int(0), test_loc());

        FuncDecl {
            name: Name::str(name),
            typevars: typevars.iter().map(|s| Name::str(s)).collect(),
            size_vars: vec![],
            params: Vec::new(),
            constraints: Vec::new(),
            ret: mk_type(Type::Void),
            body: Some(body_expr),
            arena,
            types: vec![mk_type(Type::Int32)],
            loc: test_loc(),
            closure_vars: vec![],
        }
    }

    #[test]
    fn test_monomorph_pass_creation() {
        let pass = MonomorphPass::new();
        assert_eq!(pass.instantiations.len(), 0);
        assert_eq!(pass.out_decls.len(), 0);
    }

    #[test]
    fn test_instantiate_simple_function() {
        let mut pass = MonomorphPass::new();
        let generic_func = mk_simple_func("id", vec!["T"]);
        let decls = DeclTable::new(vec![]);

        let type_args = vec![mk_type(Type::Int32)];
        let result = pass.instantiate_function(Name::str("id"), type_args, &generic_func, &decls);

        assert!(result.is_ok());
        let mangled = result.unwrap();
        assert_eq!(mangled, Name::str("id$i32"));
        assert_eq!(pass.out_decls.len(), 1);
    }

    #[test]
    fn test_instantiate_same_function_twice() {
        let mut pass = MonomorphPass::new();
        let generic_func = mk_simple_func("id", vec!["T"]);
        let decls = DeclTable::new(vec![]);

        let type_args = vec![mk_type(Type::Int32)];

        // First instantiation
        let result1 =
            pass.instantiate_function(Name::str("id"), type_args.clone(), &generic_func, &decls);
        assert!(result1.is_ok());

        // Second instantiation - should reuse
        let result2 = pass.instantiate_function(Name::str("id"), type_args, &generic_func, &decls);
        assert!(result2.is_ok());
        assert_eq!(result1.unwrap(), result2.unwrap());

        // Should only have one specialized version
        assert_eq!(pass.out_decls.len(), 1);
    }

    #[test]
    fn test_instantiate_different_type_args() {
        let mut pass = MonomorphPass::new();
        let generic_func = mk_simple_func("id", vec!["T"]);
        let decls = DeclTable::new(vec![]);

        // id<i32>
        let result1 = pass.instantiate_function(
            Name::str("id"),
            vec![mk_type(Type::Int32)],
            &generic_func,
            &decls,
        );
        assert!(result1.is_ok());
        assert_eq!(result1.unwrap(), Name::str("id$i32"));

        // id<bool>
        let result2 = pass.instantiate_function(
            Name::str("id"),
            vec![mk_type(Type::Bool)],
            &generic_func,
            &decls,
        );
        assert!(result2.is_ok());
        assert_eq!(result2.unwrap(), Name::str("id$bool"));

        // Should have two specialized versions
        assert_eq!(pass.out_decls.len(), 2);
    }

    #[test]
    fn test_instantiate_multiple_type_params() {
        let mut pass = MonomorphPass::new();
        let generic_func = mk_simple_func("map", vec!["T0", "T1"]);
        let decls = DeclTable::new(vec![]);

        let type_args = vec![mk_type(Type::Int32), mk_type(Type::Bool)];
        let result = pass.instantiate_function(Name::str("map"), type_args, &generic_func, &decls);

        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Name::str("map$i32$bool"));
    }

    #[test]
    fn test_process_expr_block() {
        let mut pass = MonomorphPass::new();
        let decls = DeclTable::new(vec![]);

        let mut arena = ExprArena::new();
        let expr1 = arena.add(Expr::Int(1), test_loc());
        let expr2 = arena.add(Expr::Int(2), test_loc());
        let block = arena.add(Expr::Block(vec![expr1, expr2]), test_loc());

        let mut fdecl = FuncDecl {
            name: Name::str("test"),
            typevars: Vec::new(),
            size_vars: Vec::new(),
            params: Vec::new(),
            constraints: Vec::new(),
            ret: mk_type(Type::Void),
            body: Some(block),
            arena,
            types: vec![mk_type(Type::Void); 3],
            loc: test_loc(),
            closure_vars: vec![],
        };

        let result = pass.process_expr(block, &mut fdecl, &decls);
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

        let mut fdecl = FuncDecl {
            name: Name::str("test"),
            typevars: Vec::new(),
            size_vars: Vec::new(),
            params: Vec::new(),
            constraints: Vec::new(),
            ret: mk_type(Type::Void),
            body: Some(binop),
            arena,
            types: vec![mk_type(Type::Int32); 3],
            loc: test_loc(),
            closure_vars: vec![],
        };

        let result = pass.process_expr(binop, &mut fdecl, &decls);
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

        let mut fdecl = FuncDecl {
            name: Name::str("test"),
            typevars: Vec::new(),
            size_vars: Vec::new(),
            params: Vec::new(),
            constraints: Vec::new(),
            ret: mk_type(Type::Void),
            body: Some(array),
            arena,
            types: vec![mk_type(Type::Int32); 3],
            loc: test_loc(),
            closure_vars: vec![],
        };

        let result = pass.process_expr(array, &mut fdecl, &decls);
        assert!(result.is_ok());
    }

    #[test]
    fn test_get_instantiation() {
        let mut pass = MonomorphPass::new();
        let generic_func = mk_simple_func("id", vec!["T"]);
        let decls = DeclTable::new(vec![]);

        let type_args = vec![mk_type(Type::Int32)];
        let key = MonomorphKey::new(Name::str("id"), type_args.clone());

        // Before instantiation
        assert!(pass.get_instantiation(&key).is_none());

        // After instantiation
        pass.instantiate_function(Name::str("id"), type_args, &generic_func, &decls)
            .unwrap();
        assert_eq!(pass.get_instantiation(&key), Some(Name::str("id$i32")));
    }

    #[test]
    fn test_specialized_declarations() {
        let mut pass = MonomorphPass::new();
        let generic_func = mk_simple_func("id", vec!["T"]);
        let decls = DeclTable::new(vec![]);

        assert_eq!(pass.specialized_declarations().len(), 0);

        pass.instantiate_function(
            Name::str("id"),
            vec![mk_type(Type::Int32)],
            &generic_func,
            &decls,
        )
        .unwrap();

        assert_eq!(pass.specialized_declarations().len(), 1);
    }

    #[test]
    fn test_type_substitution_in_specialized_func() {
        let mut pass = MonomorphPass::new();
        let decls = DeclTable::new(vec![]);

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
            &decls,
        )
        .unwrap();

        // Check the specialized declaration
        let specialized = &pass.out_decls[0];
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
        // Should fail because the entry point doesn't exist
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("not found"));
    }

    #[test]
    fn test_instantiate_with_nested_generic() {
        let mut pass = MonomorphPass::new();
        let decls = DeclTable::new(vec![]);

        // Create a generic function that works with nested types
        let mut generic_func = mk_simple_func("process", vec!["T"]);
        let t_var = typevar("T");
        let array_of_t = mk_type(Type::Array(t_var, ArraySize::Known(10)));
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
            &decls,
        );

        assert!(result.is_ok());
        assert_eq!(result.unwrap(), Name::str("process$i32"));

        // Check that the specialized version has the correct array type
        let specialized = &pass.out_decls[0];
        if let Decl::Func(fdecl) = specialized {
            assert_eq!(fdecl.params.len(), 1);
            if let Type::Array(elem_ty, size) = &*fdecl.params[0].ty.unwrap() {
                assert_eq!(**elem_ty, Type::Int32);
                assert_eq!(*size, ArraySize::Known(10));
            } else {
                panic!("Expected array type");
            }
        }
    }

    #[test]
    fn test_multiple_instantiations_same_function() {
        let mut pass = MonomorphPass::new();
        let decls = DeclTable::new(vec![]);

        let generic_func = mk_simple_func("id", vec!["T"]);

        // Create three different instantiations
        let types = vec![
            mk_type(Type::Int32),
            mk_type(Type::Bool),
            mk_type(Type::Float32),
        ];

        for ty in types {
            pass.instantiate_function(Name::str("id"), vec![ty], &generic_func, &decls)
                .unwrap();
        }

        // Should have 3 specialized versions
        assert_eq!(pass.out_decls.len(), 3);
        assert_eq!(pass.instantiations.len(), 3);
    }

    #[test]
    fn test_instantiate_function_with_constraints() {
        let mut pass = MonomorphPass::new();
        let decls = DeclTable::new(vec![]);

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
            &decls,
        );

        assert!(result.is_ok());

        // Check that constraints are preserved (they're on the original, not the specialized)
        let specialized = &pass.out_decls[0];
        if let Decl::Func(fdecl) = specialized {
            // Specialized version should have constraints copied
            assert_eq!(fdecl.constraints.len(), 1);
            assert_eq!(fdecl.constraints[0].interface_name, Name::str("Addable"));
        }
    }

    #[test]
    fn test_instantiation_deduplication() {
        let mut pass = MonomorphPass::new();
        let decls = DeclTable::new(vec![]);

        let generic_func = mk_simple_func("id", vec!["T"]);
        let type_args = vec![mk_type(Type::Int32)];

        // First instantiation
        let key1 = MonomorphKey::new(Name::str("id"), type_args.clone());
        pass.instantiate_function(Name::str("id"), type_args.clone(), &generic_func, &decls)
            .unwrap();

        assert_eq!(pass.out_decls.len(), 1);

        // Same instantiation again - should not create duplicate
        pass.instantiate_function(Name::str("id"), type_args.clone(), &generic_func, &decls)
            .unwrap();

        assert_eq!(pass.out_decls.len(), 1);
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

        let mut fdecl = FuncDecl {
            name: Name::str("test"),
            typevars: Vec::new(),
            size_vars: Vec::new(),
            params: Vec::new(),
            constraints: Vec::new(),
            ret: mk_type(Type::Void),
            body: Some(if_expr),
            arena,
            types: vec![
                mk_type(Type::Bool),
                mk_type(Type::Int32),
                mk_type(Type::Int32),
                mk_type(Type::Int32),
            ],
            loc: test_loc(),
            closure_vars: vec![],
        };

        let result = pass.process_expr(if_expr, &mut fdecl, &decls);
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
            size_vars: Vec::new(),
            params: Vec::new(),
            constraints: Vec::new(),
            ret: mk_type(Type::Int32),
            body: Some(body_expr),
            arena,
            types: vec![mk_type(Type::Int32)],
            loc: test_loc(),
            closure_vars: vec![],
        };

        let decls = DeclTable::new(vec![Decl::Func(entry_func)]);

        // Monomorphize starting from "main"
        let result = pass.monomorphize(&decls, Name::str("main"));

        assert!(result.is_ok());
        let all_decls = result.unwrap();

        // Should have 1 decl (just main, no specializations)
        assert_eq!(all_decls.len(), 1);
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
            size_vars: vec![],
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
            closure_vars: vec![],
        };

        // Create entry point function that calls id(42)
        // main() { id(42) }
        let mut main_arena = ExprArena::new();
        let arg_expr = main_arena.add(Expr::Int(42), test_loc());
        let fn_expr = main_arena.add(Expr::Id(Name::str("id")), test_loc());
        let call_expr = main_arena.add(Expr::Call(fn_expr, vec![arg_expr]), test_loc());

        let i32_type = mk_type(Type::Int32);
        let func_type = mk_type(Type::Func(tuple(vec![i32_type]), i32_type));

        let main_func = FuncDecl {
            name: Name::str("main"),
            typevars: Vec::new(),
            size_vars: Vec::new(),
            params: Vec::new(),
            constraints: Vec::new(),
            ret: i32_type,
            body: Some(call_expr),
            arena: main_arena,
            types: vec![i32_type, func_type, i32_type],
            loc: test_loc(),
            closure_vars: vec![],
        };

        let decls = DeclTable::new(vec![Decl::Func(id_func), Decl::Func(main_func)]);

        // Monomorphize starting from "main"
        let result = pass.monomorphize(&decls, Name::str("main"));

        assert!(result.is_ok());
        let all_decls = result.unwrap();

        // Should have 2 decls: main, id$i32 (specialized)
        assert_eq!(all_decls.len(), 2);

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
        } else {
            panic!("Expected function declaration");
        }

        assert_eq!(all_decls[0].pretty_print(), "id$i32(x: i32) → i32 x");
        assert_eq!(all_decls[1].pretty_print(), "main() → i32 id$i32(42)");
    }
}
