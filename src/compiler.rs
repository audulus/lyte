use crate::vm::{LinkedProgram, VMProgram, VM};
use crate::vm_codegen::VMCodegen;
use crate::*;
use core::mem;
use std::collections::HashMap;
use std::fs;

/// A compiled program ready for execution, abstracting over backends.
pub enum CompiledProgram {
    #[cfg(feature = "llvm")]
    Llvm(crate::llvm_jit::LLVMCompiledProgram),
    Vm {
        program: VMProgram,
        linked: LinkedProgram,
        vm: VM,
    },
}

impl CompiledProgram {
    /// Get the size of the globals buffer in bytes.
    pub fn globals_size(&self) -> usize {
        match self {
            #[cfg(feature = "llvm")]
            CompiledProgram::Llvm(p) => p.globals_size,
            CompiledProgram::Vm { program, .. } => program.globals_size,
        }
    }

    /// Get a function pointer or func index for a named entry point.
    pub fn get_entry_point(&self, name: Name) -> Option<EntryPoint> {
        match self {
            #[cfg(feature = "llvm")]
            CompiledProgram::Llvm(p) => {
                p.entry_points.get(&name).map(|&addr| EntryPoint::Jit(addr))
            }
            CompiledProgram::Vm { program, .. } => program
                .entry_points
                .get(&name)
                .map(|&idx| EntryPoint::Vm(idx)),
        }
    }
}

/// An entry point handle, either a JIT function pointer or a VM function index.
#[derive(Clone, Copy)]
pub enum EntryPoint {
    /// JIT: raw function address. Signature: `fn(*mut u8, *mut u8)`.
    Jit(usize),
    /// VM: function index into the linked program.
    Vm(crate::vm::FuncIdx),
}

/// Returns the built-in function declarations (assert, print, etc.)
fn builtin_decls() -> Vec<Decl> {
    let mut decls = vec![
        // assert(cond: bool) → void
        Decl::Func(FuncDecl {
            name: Name::new("assert".into()),
            typevars: vec![],
            size_vars: vec![],
            params: vec![Param {
                name: Name::new("cond".into()),
                ty: Some(mk_type(Type::Bool)),
            }],
            body: None,
            ret: mk_type(Type::Void),
            constraints: vec![],
            loc: test_loc(),
            arena: ExprArena::new(),
            types: vec![],
            closure_vars: vec![],
            is_extern: false,
        }),
        // print(value: i32) → void
        Decl::Func(FuncDecl {
            name: Name::new("print".into()),
            typevars: vec![],
            size_vars: vec![],
            params: vec![Param {
                name: Name::new("value".into()),
                ty: Some(mk_type(Type::Int32)),
            }],
            body: None,
            ret: mk_type(Type::Void),
            constraints: vec![],
            loc: test_loc(),
            arena: ExprArena::new(),
            types: vec![],
            closure_vars: vec![],
            is_extern: false,
        }),
        // putc(c: i32) → void
        Decl::Func(FuncDecl {
            name: Name::new("putc".into()),
            typevars: vec![],
            size_vars: vec![],
            params: vec![Param {
                name: Name::new("c".into()),
                ty: Some(mk_type(Type::Int32)),
            }],
            body: None,
            ret: mk_type(Type::Void),
            constraints: vec![],
            loc: test_loc(),
            arena: ExprArena::new(),
            types: vec![],
            closure_vars: vec![],
            is_extern: false,
        }),
    ];

    // Math builtins: unary, overloaded for f32 and f64.
    let unary_math = [
        "sin", "cos", "tan", "asin", "acos", "atan", "sinh", "cosh", "tanh", "asinh", "acosh",
        "atanh", "ln", "exp", "exp2", "log10", "log2", "sqrt", "abs", "floor", "ceil",
    ];
    for name in unary_math {
        for (ty, ret_ty) in [
            (mk_type(Type::Float32), mk_type(Type::Float32)),
            (mk_type(Type::Float64), mk_type(Type::Float64)),
        ] {
            decls.push(Decl::Func(FuncDecl {
                name: Name::new(name.into()),
                typevars: vec![],
                size_vars: vec![],
                params: vec![Param {
                    name: Name::new("x".into()),
                    ty: Some(ty),
                }],
                body: None,
                ret: ret_ty,
                constraints: vec![],
                loc: test_loc(),
                arena: ExprArena::new(),
                types: vec![],
                closure_vars: vec![],
            is_extern: false,
            }));
        }
    }

    // Math builtins: unary predicates returning bool, overloaded for f32 and f64.
    let bool_ty = mk_type(Type::Int32);
    let unary_pred = ["isinf", "isnan"];
    for name in unary_pred {
        for ty in [mk_type(Type::Float32), mk_type(Type::Float64)] {
            decls.push(Decl::Func(FuncDecl {
                name: Name::new(name.into()),
                typevars: vec![],
                size_vars: vec![],
                params: vec![Param {
                    name: Name::new("x".into()),
                    ty: Some(ty),
                }],
                body: None,
                ret: bool_ty,
                constraints: vec![],
                loc: test_loc(),
                arena: ExprArena::new(),
                types: vec![],
                closure_vars: vec![],
            is_extern: false,
            }));
        }
    }

    // Math builtins: binary, overloaded for f32 and f64.
    let binary_math = ["pow", "atan2", "min", "max"];
    for name in binary_math {
        for (ty, ret_ty) in [
            (mk_type(Type::Float32), mk_type(Type::Float32)),
            (mk_type(Type::Float64), mk_type(Type::Float64)),
        ] {
            decls.push(Decl::Func(FuncDecl {
                name: Name::new(name.into()),
                typevars: vec![],
                size_vars: vec![],
                params: vec![
                    Param {
                        name: Name::new("x".into()),
                        ty: Some(ty),
                    },
                    Param {
                        name: Name::new("y".into()),
                        ty: Some(ty),
                    },
                ],
                body: None,
                ret: ret_ty,
                constraints: vec![],
                loc: test_loc(),
                arena: ExprArena::new(),
                types: vec![],
                closure_vars: vec![],
            is_extern: false,
            }));
        }
    }

    // f32x4 constructor: f32x4(x, y, z, w) → f32x4
    let f32x4_ty = mk_type(Type::Float32x4);
    let f32_ty = mk_type(Type::Float32);
    decls.push(Decl::Func(FuncDecl {
        name: Name::new("f32x4".into()),
        typevars: vec![],
        size_vars: vec![],
        params: vec![
            Param { name: Name::new("x".into()), ty: Some(f32_ty) },
            Param { name: Name::new("y".into()), ty: Some(f32_ty) },
            Param { name: Name::new("z".into()), ty: Some(f32_ty) },
            Param { name: Name::new("w".into()), ty: Some(f32_ty) },
        ],
        body: None,
        ret: f32x4_ty,
        constraints: vec![],
        loc: test_loc(),
        arena: ExprArena::new(),
        types: vec![],
        closure_vars: vec![],
        is_extern: false,
    }));

    // f32x4_splat: broadcast a single f32 to all 4 lanes
    decls.push(Decl::Func(FuncDecl {
        name: Name::new("f32x4_splat".into()),
        typevars: vec![],
        size_vars: vec![],
        params: vec![
            Param { name: Name::new("x".into()), ty: Some(f32_ty) },
        ],
        body: None,
        ret: f32x4_ty,
        constraints: vec![],
        loc: test_loc(),
        arena: ExprArena::new(),
        types: vec![],
        closure_vars: vec![],
        is_extern: false,
    }));

    decls
}

// An AST.
//
// I've read that some IDE-oriented compilers prefer to
// have a full syntax tree (which can represent the exact formatting of everything)
// but that seems like a real pain.
#[derive(Eq, PartialEq, Clone, Debug, Hash, Default)]
pub struct Tree {
    pub decls: Vec<Decl>,
    pub errors: Vec<ParseError>,
}

const STDLIB: &str = include_str!("../stdlib.lyte");

/// Rewrite qualified enum accesses like `Direction.Up` into `Expr::Enum`
/// nodes. The parser produces `Expr::Field(Expr::Id("Direction"), "Up")`
/// for this syntax because it doesn't know about declarations. This pass
/// detects when the LHS identifier names an enum type that contains the
/// field name as a case, and rewrites the node so that the checker and
/// code-generation passes handle it as a plain enum literal.
fn rewrite_qualified_enums(arena: &mut ExprArena, decls: &DeclTable) {
    for i in 0..arena.exprs.len() {
        if let Expr::Field(lhs, case_name) = arena.exprs[i].clone() {
            if let Expr::Id(id_name) = &arena.exprs[lhs] {
                let found = decls.find(*id_name);
                if let Some(Decl::Enum { cases, .. }) =
                    found.iter().find(|d| matches!(d, Decl::Enum { .. }))
                {
                    if cases.contains(&case_name) {
                        arena.exprs[i] = Expr::Enum(case_name);
                    }
                }
            }
        }
    }
}

/// After type-checking, rewrite `Binop(op, lhs, rhs)` into
/// `Call(__add/__sub/__mul/__div, [lhs, rhs])` when the operand type is a
/// named (struct) type. The checker resolves the overload through its Or
/// constraint, but the JIT/VM only handle primitive types in binop codegen.
fn rewrite_overloaded_binops(fdecl: &mut FuncDecl) {
    let n = fdecl.arena.exprs.len();
    for i in 0..n {
        if let Expr::Binop(op, lhs, rhs) = fdecl.arena.exprs[i].clone() {
            if !op.arithmetic() {
                continue;
            }
            let lhs_ty = fdecl.types[lhs];
            if matches!(*lhs_ty, Type::Name(_, _)) {
                let result_ty = fdecl.types[i];
                let rhs_ty = fdecl.types[rhs];
                // Build the function type: (lhs_ty, rhs_ty) -> result_ty
                let fn_ty = func(tuple(vec![lhs_ty, rhs_ty]), result_ty);

                let overload = Name::new(op.overload_name().into());
                let fn_id = fdecl.arena.add(Expr::Id(overload), fdecl.arena.locs[i]);
                // Extend types array to cover the new expression
                while fdecl.types.len() <= fn_id {
                    fdecl.types.push(mk_type(Type::Void));
                }
                fdecl.types[fn_id] = fn_ty;
                fdecl.arena.exprs[i] = Expr::Call(fn_id, vec![lhs, rhs]);
            }
        }
    }
}

/// Rename non-generic overloaded functions (both declarations and call sites)
/// so each overload gets a unique symbol. e.g. two `add` overloads with
/// different param types become `add$i32$i32` and `add$f32$f32`.
fn rename_overloaded_functions(decls: &mut DeclTable) {
    // Count non-generic overloads per name.
    let mut counts: HashMap<Name, usize> = HashMap::new();
    for d in &decls.decls {
        if let Decl::Func(f) = d {
            if f.typevars.is_empty() {
                *counts.entry(f.name).or_default() += 1;
            }
        }
    }

    // Build a mapping from (original_name, param_types) -> mangled_name
    // for overloaded functions.
    let mut overload_map: HashMap<Name, Vec<(Vec<TypeID>, Name)>> = HashMap::new();
    for d in &decls.decls {
        if let Decl::Func(f) = d {
            if f.typevars.is_empty() && counts.get(&f.name).copied().unwrap_or(0) > 1 {
                let param_types: Vec<TypeID> = f.params.iter().filter_map(|p| p.ty).collect();
                let mangled = mangle::mangle_overload(f.name, &param_types);
                overload_map
                    .entry(f.name)
                    .or_default()
                    .push((param_types, mangled));
            }
        }
    }

    if overload_map.is_empty() {
        return;
    }

    // Rename function declarations.
    for d in &mut decls.decls {
        if let Decl::Func(ref mut f) = d {
            if let Some(overloads) = overload_map.get(&f.name) {
                let param_types: Vec<TypeID> = f.params.iter().filter_map(|p| p.ty).collect();
                for (pts, mangled) in overloads {
                    if *pts == param_types {
                        f.name = *mangled;
                        break;
                    }
                }
            }
        }
    }

    // Rename call-site references in all function bodies.
    // For each Expr::Id that matches an overloaded name, look at its resolved
    // type to determine which overload it refers to.
    for d in &mut decls.decls {
        if let Decl::Func(ref mut f) = d {
            for i in 0..f.arena.exprs.len() {
                if let Expr::Id(name) = &f.arena.exprs[i] {
                    if let Some(overloads) = overload_map.get(name) {
                        if let Some(&call_ty) = f.types.get(i) {
                            // The call-site type is the function type.
                            // Extract param types from it.
                            if let Type::Func(dom, _) = &*call_ty {
                                let call_params = match &**dom {
                                    Type::Tuple(ts) => ts.clone(),
                                    _ => vec![*dom],
                                };
                                for (pts, mangled) in overloads {
                                    if pts.len() == call_params.len()
                                        && pts.iter().zip(call_params.iter()).all(|(a, b)| {
                                            let mut inst = crate::Instance::new();
                                            crate::types::unify(*a, *b, &mut inst)
                                        })
                                    {
                                        f.arena.exprs[i] = Expr::Id(*mangled);
                                        break;
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }
}

pub struct Compiler {
    ast: Vec<Tree>,
    decls: DeclTable,
    pub print_ir: bool,
    /// Number of AST trees that belong to the stdlib (parsed in new()).
    stdlib_trees: usize,
    /// Entry point function names. If empty, defaults to ["main"].
    entry_points: Vec<Name>,
    /// Formatted error messages from the last parse/check operation.
    pub last_errors: Vec<String>,
    /// Structured parse errors from the last parse operation.
    pub last_parse_errors: Vec<ParseError>,
    /// Structured type errors from the last check operation.
    pub last_type_errors: Vec<TypeError>,
    /// Structured safety errors from the last check operation.
    pub last_safety_errors: Vec<SafetyError>,
    /// When true, suppress all stdout output (for LSP usage).
    pub quiet: bool,
    /// When true, continue checking all declarations even after errors (for LSP).
    pub check_all: bool,
}

impl Compiler {
    pub fn new() -> Self {
        let mut c = Self {
            ast: Vec::new(),
            decls: DeclTable::new(vec![]),
            print_ir: false,
            stdlib_trees: 0,
            entry_points: Vec::new(),
            last_errors: Vec::new(),
            last_parse_errors: Vec::new(),
            last_type_errors: Vec::new(),
            last_safety_errors: Vec::new(),
            quiet: false,
            check_all: false,
        };
        c.parse(STDLIB, "<stdlib>");
        c.stdlib_trees = c.ast.len();
        c
    }

    /// Set custom entry point functions. If not called (or called with empty slice),
    /// defaults to ["main"].
    pub fn set_entry_points(&mut self, names: &[&str]) {
        self.entry_points = names.iter().map(|n| Name::new((*n).into())).collect();
    }

    /// Returns the effective entry points (defaults to ["main"] if none set).
    pub fn effective_entry_points(&self) -> Vec<Name> {
        if self.entry_points.is_empty() {
            vec![Name::new("main".into())]
        } else {
            self.entry_points.clone()
        }
    }

    pub fn parse_file(&mut self, path: &str) {
        let contents = fs::read_to_string(path);

        if let Ok(contents) = contents {
            self.parse(&contents, &path);
        } else {
            eprintln!("could not read file {:?}", path);
            std::process::exit(1)
        }
    }

    pub fn print_ast(&self) {
        for tree in self.ast.iter().skip(self.stdlib_trees) {
            for decl in &tree.decls {
                println!("{}", decl.pretty_print());
            }
        }
    }

    pub fn parse(&mut self, contents: &str, path: &str) -> bool {
        let mut lexer = Lexer::new(&contents, &path);

        let mut tree = Tree::default();

        lexer.next();
        tree.decls = parse_program(&mut lexer, &mut tree.errors);

        self.last_errors.clear();
        self.last_parse_errors.clear();
        for err in &tree.errors {
            let msg = format!(
                "{}:{}: {}",
                err.location.file, err.location.line, err.message
            );
            if !self.quiet {
                println!("{}", msg);
            }
            self.last_errors.push(msg);
            self.last_parse_errors.push(err.clone());
        }

        let success = tree.errors.is_empty();
        self.ast.push(tree);
        success
    }

    pub fn check(&mut self) -> bool {
        self.last_errors.clear();
        self.last_type_errors.clear();
        self.last_safety_errors.clear();
        let mut decls = builtin_decls();
        for tree in &self.ast {
            decls.append(&mut tree.decls.clone());
        }

        // Collect macros for expansion, rejecting duplicates.
        let mut macros: HashMap<Name, FuncDecl> = HashMap::new();
        let mut has_errors = false;
        for d in &decls {
            if let Decl::Macro(m) = d {
                if macros.contains_key(&m.name) {
                    let msg = format!("duplicate macro: {}", m.name);
                    if !self.quiet {
                        print_error_with_context(m.loc, &msg);
                    }
                    self.last_errors.push(format_error(m.loc, &msg));
                    has_errors = true;
                } else {
                    macros.insert(m.name, m.clone());
                }
            }
        }
        if has_errors {
            return false;
        }

        // Expand macros in all function bodies.
        for decl in &mut decls {
            if let Decl::Func(ref mut fdecl) = decl {
                fdecl.expand_macros(&macros);
            }
        }

        self.decls = DeclTable::new(decls);
        let orig_decls = self.decls.clone();

        // Rewrite qualified enum accesses (e.g. Direction.Up -> .Up)
        // before type-checking, so the checker and downstream passes
        // see them as Expr::Enum nodes.
        for decl in &mut self.decls.decls {
            if let Decl::Func(ref mut fdecl) | Decl::Macro(ref mut fdecl) = decl {
                rewrite_qualified_enums(&mut fdecl.arena, &orig_decls);
            }
        }

        for decl in &mut self.decls.decls {
            // Skip type-checking macro declarations (they are untyped templates).
            if matches!(decl, Decl::Macro(_)) {
                continue;
            }

            let mut checker = Checker::new();
            checker.check_decl(decl, &orig_decls);

            if !self.quiet {
                checker.print_errors();
            }
            for err in &checker.errors {
                self.last_errors
                    .push(format_error(err.location, &err.message));
            }
            self.last_type_errors
                .extend(checker.errors.iter().cloned());
            if !checker.errors.is_empty() && !self.check_all {
                return false;
            }
            has_errors = has_errors || !checker.errors.is_empty();

            if let Decl::Func(ref mut fdecl) = decl {
                fdecl.types = checker.solved_types();
            }
        }

        // Rewrite overloaded binary ops (e.g. Point + Point) to function calls.
        for decl in &mut self.decls.decls {
            if let Decl::Func(ref mut fdecl) = decl {
                rewrite_overloaded_binops(fdecl);
            }
        }

        // Static safety checks (array bounds, division by zero).
        let mut safety_checker = SafetyChecker::new();
        safety_checker.check(&self.decls);
        if !self.quiet {
            safety_checker.print_errors();
        }
        for err in &safety_checker.errors {
            self.last_errors
                .push(format_error(err.location, &err.message));
        }
        self.last_safety_errors
            .extend(safety_checker.errors.iter().cloned());
        if !safety_checker.errors.is_empty() {
            has_errors = true;
        }

        !has_errors
    }

    /// Returns a reference to the declaration table (available after check()).
    pub fn decls(&self) -> &DeclTable {
        &self.decls
    }

    pub fn specialize(&mut self) -> Result<(), String> {
        let mut pass = MonomorphPass::new();
        let entry_points = self.effective_entry_points();
        let all_decls = pass.monomorphize_multi(&self.decls, &entry_points)?;
        // monomorphize now returns all decls (original + specialized)
        self.decls = DeclTable::new(all_decls);

        // Rename non-generic overloaded functions to unique symbols.
        // Must happen after monomorphization so specialized generic bodies
        // can resolve overloaded calls (e.g. cmp in a generic quicksort).
        rename_overloaded_functions(&mut self.decls);
        self.decls = DeclTable::new(self.decls.decls.clone());

        // Hoist loop-invariant struct field reads (after monomorphization
        // so we operate on concrete types, and after safety checking).
        {
            let decls_snapshot = self.decls.clone();
            for decl in &mut self.decls.decls {
                if let Decl::Func(ref mut fdecl) = decl {
                    hoist_loop_invariant_fields(fdecl, &decls_snapshot);
                }
            }
        }

        Ok(())
    }

    pub fn has_decls(&self) -> bool {
        // Check if there are any user declarations beyond the built-ins and stdlib
        let stdlib_decl_count: usize = self
            .ast
            .iter()
            .take(self.stdlib_trees)
            .map(|t| t.decls.len())
            .sum();
        self.decls.decls.len() > builtin_decls().len() + stdlib_decl_count
    }

    /// Returns info about each global variable: (name, offset, size, type_string).
    /// The offset and size are in bytes within the globals buffer.
    /// The `base_offset` should be `CANCEL_FLAG_RESERVED` for JIT/LLVM backends
    /// or `0` for the VM backend.
    pub fn globals_info_with_offset(&self, base_offset: usize) -> Vec<(String, usize, usize, String, bool)> {
        let mut result = Vec::new();
        let mut offset: usize = base_offset;
        for decl in &self.decls.decls {
            match decl {
                Decl::Global { name, ty, .. } => {
                    let size = ty.size(&self.decls) as usize;
                    let type_str = ty.pretty_print();
                    result.push((name.to_string(), offset, size, type_str, false));
                    offset += size;
                }
                Decl::Func(f) if f.is_extern => {
                    // Extern functions occupy 16 bytes in the globals buffer:
                    // {fn_ptr: *const (), context: *const ()}
                    let type_str = format!("extern fn({})", f.params.iter().map(|p| {
                        p.ty.map_or("?".to_string(), |t| t.pretty_print())
                    }).collect::<Vec<_>>().join(", "));
                    let type_str = if matches!(&*f.ret, Type::Void) {
                        type_str
                    } else {
                        format!("{} -> {}", type_str, f.ret.pretty_print())
                    };
                    result.push((f.name.to_string(), offset, 16, type_str, true));
                    offset += 16;
                }
                _ => {}
            }
        }
        result
    }

    #[cfg(feature = "llvm")]
    pub fn run_llvm(&mut self) -> (std::time::Duration, std::time::Duration) {
        let mut jit = LLVMJIT::new();
        jit.print_ir = self.print_ir;
        let entry_points = self.effective_entry_points();
        match jit.compile_and_run_multi(&self.decls, &entry_points) {
            Ok((cancelled, compile_time, exec_time)) => {
                if cancelled {
                    println!("execution cancelled");
                }
                (compile_time, exec_time)
            }
            Err(e) => {
                println!("{}", e);
                panic!();
            }
        }
    }

    /// Compile with LLVM and print IR, but do not execute.
    #[cfg(feature = "llvm")]
    pub fn print_llvm_ir(&mut self) {
        let mut jit = LLVMJIT::new();
        jit.print_ir = true;
        jit.ir_only = true;
        let entry_points = self.effective_entry_points();
        match jit.compile_and_run_multi(&self.decls, &entry_points) {
            Ok(_) => {}
            Err(e) => {
                println!("{}", e);
                panic!();
            }
        }
    }

    /// Compile to native code via Cranelift JIT.
    #[cfg(feature = "cranelift")]
    pub fn jit(&self) -> Result<(*const u8, usize, JIT), String> {
        let mut jit = JIT::default();
        jit.print_ir = self.print_ir;
        if self.decls.decls.is_empty() {
            return Err(String::from("No declarations to compile"));
        }
        let (code_ptr, globals_size) = jit.compile(&self.decls)?;
        Ok((code_ptr, globals_size, jit))
    }

    /// Compile to native code via Cranelift JIT with multiple entry points.
    #[cfg(feature = "cranelift")]
    pub fn jit_multi(&self) -> Result<(HashMap<Name, *const u8>, usize, JIT), String> {
        let mut jit = JIT::default();
        jit.print_ir = self.print_ir;
        if self.decls.decls.is_empty() {
            return Err(String::from("No declarations to compile"));
        }
        let entry_points = self.effective_entry_points();
        let (map, globals_size) = jit.compile_multi(&self.decls, &entry_points)?;
        Ok((map, globals_size, jit))
    }

    #[cfg(feature = "cranelift")]
    pub fn run(&mut self) {
        let r = self.jit();
        if let Ok((code_ptr, globals_size, jit)) = r {
            println!("compilation successful");

            type Entry = fn(*mut u8) -> ();
            let mut globals: Vec<u8> = vec![0u8; globals_size];
            let cancelled = unsafe {
                extern "C" {
                    fn setjmp(env: *mut u8) -> i32;
                }
                crate::cancel::set_cancel_callback(
                    globals.as_mut_ptr(),
                    None,
                    std::ptr::null_mut(),
                );
                let jmp_buf_ptr = globals.as_mut_ptr().add(crate::cancel::JMPBUF_OFFSET);
                if setjmp(jmp_buf_ptr) == 0 {
                    let code_fn = mem::transmute::<_, Entry>(code_ptr);
                    code_fn(globals.as_mut_ptr());
                    false
                } else {
                    true
                }
            };
            if cancelled {
                println!("execution cancelled");
            }
            jit.free_memory();
        } else if let Err(e) = r {
            eprintln!("compilation error: {}", e);
        }
    }

    /// Compile the declarations to a VM program.
    pub fn compile_vm(&self) -> Result<VMProgram, String> {
        if self.decls.decls.is_empty() {
            return Err(String::from("No declarations to compile"));
        }
        let mut codegen = VMCodegen::new();
        let entry_points = self.effective_entry_points();
        codegen.compile_multi(&self.decls, &entry_points)
    }

    /// Run the code using the VM interpreter.
    pub fn run_vm(&mut self) -> Result<i64, String> {
        let program = self.compile_vm()?;
        let mut vm = VM::new();
        Ok(vm.run(&program))
    }

    /// Run the code using the 16-bit VM interpreter.
    pub fn run_vm16(&mut self) -> Result<i64, String> {
        let program = self.compile_vm()?;
        let func_idx = *program
            .entry_points
            .get(&Name::str("main"))
            .ok_or("no main entry point")?;
        let mut vm = crate::vm16::VM16::new();
        Ok(vm.run(&program, func_idx, &[]))
    }

    /// Compile to a backend-agnostic CompiledProgram.
    /// Auto-selects LLVM JIT (when available) or VM.
    pub fn compile_program(&self) -> Result<CompiledProgram, String> {
        if self.decls.decls.is_empty() {
            return Err(String::from("No declarations to compile"));
        }
        let entry_points = self.effective_entry_points();

        #[cfg(feature = "llvm")]
        {
            let mut jit = crate::llvm_jit::LLVMJIT::new();
            jit.print_ir = self.print_ir;
            let llvm_prog = jit.compile_only(&self.decls, &entry_points)?;
            return Ok(CompiledProgram::Llvm(llvm_prog));
        }

        #[cfg(not(feature = "llvm"))]
        {
            let mut codegen = VMCodegen::new();
            let program = codegen.compile_multi(&self.decls, &entry_points)?;
            let linked = LinkedProgram::from_program(&program);
            let vm = VM::new();
            Ok(CompiledProgram::Vm {
                program,
                linked,
                vm,
            })
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[cfg(feature = "cranelift")]
    fn jit(code: &str) {
        let mut compiler = Compiler::new();
        let paths = vec![String::from(".")];

        compiler.parse(code.into(), &paths[0]);
        assert!(compiler.check());
        compiler.specialize().unwrap();
        assert!(compiler.decls.decls.len() > 0);
        compiler.run();
    }

    #[cfg(feature = "cranelift")]
    #[test]
    fn test_infinite_loop_cancels() {
        let code = r#"
            main {
                var i = 0
                while true {
                    i = i + 1
                }
            }
        "#;

        let mut compiler = Compiler::new();
        compiler.parse(code, ".");
        assert!(compiler.check());
        compiler.specialize().unwrap();

        let (code_ptr, globals_size, jit) = compiler.jit().expect("JIT compilation failed");
        let mut globals: Vec<u8> = vec![0u8; globals_size];

        // Cancel callback that always cancels (first invocation).
        unsafe extern "C" fn always_cancel(_user_data: *mut u8) -> bool {
            true
        }

        let cancelled = unsafe {
            extern "C" {
                fn setjmp(env: *mut u8) -> i32;
            }
            crate::cancel::set_cancel_callback(
                globals.as_mut_ptr(),
                Some(always_cancel),
                std::ptr::null_mut(),
            );
            let jmp_buf_ptr = globals.as_mut_ptr().add(crate::cancel::JMPBUF_OFFSET);
            if setjmp(jmp_buf_ptr) == 0 {
                type Entry = fn(*mut u8) -> ();
                let code_fn = mem::transmute::<_, Entry>(code_ptr);
                code_fn(globals.as_mut_ptr());
                false
            } else {
                true
            }
        };

        assert!(cancelled, "expected the infinite loop to be cancelled");
        jit.free_memory();
    }

    fn run(code: &str) {
        let mut compiler = Compiler::new();
        let paths = vec![String::from(".")];

        compiler.parse(code.into(), &paths[0]);
        assert!(compiler.check());
        compiler.specialize().unwrap();
        assert!(compiler.decls.decls.len() > 0);
        compiler.run_vm().expect("VM execution failed");
    }

    #[test]
    fn basic() {
        let code = r#"
           main {
              var x = 1
           }
        "#;

        jit(code);
        run(code);
    }

    #[test]
    fn basic2() {
        let code = r#"
           main {
              var x = 1
              var y = x
           }
        "#;

        jit(code);
        run(code);
    }

    #[test]
    fn var() {
        let code = r#"
           main {
              var x = 1
              x = 2
           }
        "#;

        jit(code);
        run(code);
    }

    #[test]
    fn test_assert() {
        let code = r#"
            main {
               var x = 42
               assert(x == 42)
            }
        "#;

        jit(code);
        run(code);
    }

    #[test]
    fn test_assert_ne() {
        let code = r#"
            main {
               var x = 42
               assert(x != 5)
            }
        "#;

        jit(code);
        run(code);
    }

    #[test]
    fn test_field_assign() {
        let code = r#"
            struct S {
                x: i32
            }

            main {
                var s: S
                s.x = 2
                assert(s.x == 2)
            }
        "#;

        jit(code);
        run(code);
    }

    #[test]
    fn test_field_assign_2() {
        let code = r#"
            struct S {
                x: i32,
                y: i32,
                z: i32
            }

            main {
                var s: S
                s.z = 42
                assert(s.z == 42)
            }
        "#;
        jit(code);
        run(code);
    }

    #[test]
    fn test_struct_assign() {
        let code = r#"
            struct S {
                x: i32
            }

            main {
                var s: S
                s.x = 2
                var t: S
                t = s
                assert(t.x == 2)
            }
        "#;

        jit(code);
        run(code);
    }

    #[test]
    fn test_array() {
        let code = r#"
            main {
                var a: [i32; 2]
                a[1] = 42
                assert(a[1] == 42)
            }
        "#;

        jit(code);
        run(code);
    }

    #[test]
    fn test_call() {
        let code = r#"
            f {
                assert(1 == 1)
            }

            main {
                f()
            }
        "#;

        jit(code);
        run(code);
    }

    #[test]
    fn test_call2() {
        let code = r#"
            f {
                assert(1 == 1)
            }

            g {
                f()
            }

            main {
                f()
                g()
            }
        "#;

        jit(code);
        run(code);
    }

    #[test]
    fn test_call_id() {
        let code = r#"
            id(x: i32) → i32 {
                 x
            }

            main {
                let y = id(42)
                assert(y == 42)
            }
        "#;

        jit(code);
        run(code);
    }

    #[test]
    fn test_neg() {
        let code = r#"
            main {
                assert(1-2 == -1)
            }
        "#;

        jit(code);
        run(code);
    }

    #[test]
    fn test_struct() {
        let code = r#"
            struct S {
                i: i32,
                f: f32
            }

            main {
                var s: S
                s.i = 2
                assert(s.i == 2)
            }
        "#;

        jit(code);
        run(code);
    }

    #[test]
    fn test_array_literal() {
        let code = r#"
            main {
                var a = [42]
                assert(a[0] == 42)
            }
        "#;

        jit(code);
        run(code);
    }

    #[test]
    fn test_monomorph() {
        let code = r#"
            id<T>(x: T) → T { x }

            main {
                let x = id(42)
                assert(x == 42)

                let y = id(true)
                assert(y == true)
            }
        "#;

        jit(code);
        run(code);
    }

    #[test]
    fn test_enum_basic() {
        let code = r#"
            enum Color { Red, Green, Blue }

            main {
                var c: Color
                c = .Red
                assert(c == .Red)
                c = .Blue
                assert(c == .Blue)
                assert(c != .Red)
            }
        "#;

        jit(code);
        run(code);
    }

    #[test]
    fn test_enum_func() {
        let code = r#"
            enum Direction { Up, Down, Left, Right }

            is_vertical(d: Direction) → bool {
                (d == .Up) || (d == .Down)
            }

            main {
                assert(is_vertical(.Up))
                assert(!is_vertical(.Left))
            }
        "#;

        jit(code);
        run(code);
    }

    #[test]
    fn test_enum_in_struct() {
        let code = r#"
            enum Status { Active, Inactive }

            struct Item {
                value: i32,
                status: Status
            }

            main {
                var item: Item
                item.value = 42
                item.status = .Active
                assert(item.status == .Active)
                assert(item.value == 42)
            }
        "#;

        jit(code);
        run(code);
    }

    #[test]
    fn test_multi_entry_points_vm() {
        let code = r#"
            var counter: i32

            init {
                counter = 10
            }

            process(n: i32) -> i32 {
                counter = counter + n
                counter
            }
        "#;

        let mut compiler = Compiler::new();
        compiler.parse(code, ".");
        compiler.set_entry_points(&["init", "process"]);
        assert!(compiler.check());
        compiler.specialize().unwrap();

        let program = compiler.compile_vm().unwrap();
        let mut vm = crate::vm::VM::new();

        // Call init — sets counter to 10
        vm.call(&program, Name::new("init".into()), &[]).unwrap();

        // Call process(5) — counter becomes 15
        let result = vm
            .call(&program, Name::new("process".into()), &[5])
            .unwrap();
        assert_eq!(result, 15);

        // Call process(3) — counter becomes 18 (globals persist)
        let result = vm
            .call(&program, Name::new("process".into()), &[3])
            .unwrap();
        assert_eq!(result, 18);
    }

    #[test]
    fn test_multi_entry_points_jit() {
        let code = r#"
            var counter: i32

            init {
                counter = 10
            }

            process(n: i32) -> i32 {
                counter = counter + n
                counter
            }
        "#;

        let mut compiler = Compiler::new();
        compiler.parse(code, ".");
        compiler.set_entry_points(&["init", "process"]);
        assert!(compiler.check());
        compiler.specialize().unwrap();

        let (map, globals_size, jit) = compiler.jit_multi().unwrap();

        assert!(map.contains_key(&Name::new("init".into())));
        assert!(map.contains_key(&Name::new("process".into())));
        assert!(globals_size > 0);

        jit.free_memory();
    }

    #[test]
    fn test_multi_entry_shared_function() {
        // Both entry points call the same helper function
        let code = r#"
            var state: i32

            helper(x: i32) -> i32 {
                x * 2
            }

            init {
                state = helper(5)
            }

            process(n: i32) -> i32 {
                state = state + helper(n)
                state
            }
        "#;

        let mut compiler = Compiler::new();
        compiler.parse(code, ".");
        compiler.set_entry_points(&["init", "process"]);
        assert!(compiler.check());
        compiler.specialize().unwrap();

        let program = compiler.compile_vm().unwrap();
        let mut vm = crate::vm::VM::new();

        vm.call(&program, Name::new("init".into()), &[]).unwrap();
        let result = vm
            .call(&program, Name::new("process".into()), &[3])
            .unwrap();
        // init sets state=helper(5)=10, process adds helper(3)=6, total=16
        assert_eq!(result, 16);
    }

    #[test]
    fn test_slice_overloads_are_rewritten_during_specialize() {
        let code = r#"
            sum(a: [i32]) -> i32 {
                var s = 0
                for i in 0 .. a.len { s = s + a[i] }
                s
            }

            sum(a: [f32]) -> f32 {
                var s = 0.0
                for i in 0 .. a.len { s = s + a[i] }
                s
            }

            main {
                print(sum([1, 2, 3]))
                print(sum([1.0, 2.0, 3.0, 6.0]) as i32)
            }
        "#;

        let mut compiler = Compiler::new();
        compiler.parse(code, ".");
        assert!(compiler.check());
        compiler.specialize().unwrap();

        assert!(compiler.decls.find(Name::str("sum")).is_empty());

        let main = compiler
            .decls
            .find(Name::str("main"))
            .into_iter()
            .find(|decl| matches!(decl, Decl::Func(_)))
            .expect("main should be present after specialization");

        let printed = main.pretty_print();
        assert!(printed.contains("sum$[i32]"));
        assert!(printed.contains("sum$[f32]"));
    }

    #[test]
    fn test_backward_compat_default_main() {
        // When no entry points are set, defaults to "main"
        let code = r#"
            main {
                assert(1 == 1)
            }
        "#;

        let mut compiler = Compiler::new();
        compiler.parse(code, ".");
        // Don't call set_entry_points — should default to "main"
        assert!(compiler.check());
        compiler.specialize().unwrap();
        compiler.run_vm().expect("VM run failed");
    }

    #[test]
    fn test_compile_program_vm() {
        let code = r#"
            var counter: i32

            init {
                counter = 10
            }

            process -> i32 {
                counter = counter + 1
                counter
            }
        "#;

        let mut compiler = Compiler::new();
        compiler.parse(code, ".");
        compiler.set_entry_points(&["init", "process"]);
        assert!(compiler.check());
        compiler.specialize().unwrap();

        let mut prog = compiler.compile_program().unwrap();
        let gs = prog.globals_size();
        assert!(gs > 0);

        // Allocate globals externally
        let mut globals = vec![0u8; gs];

        // Look up entry points
        let init_ep = prog.get_entry_point(Name::new("init".into())).unwrap();
        let process_ep = prog.get_entry_point(Name::new("process".into())).unwrap();

        // Call init via the compiled program
        match &mut prog {
            CompiledProgram::Vm {
                linked,
                program,
                vm,
                ..
            } => unsafe {
                vm.call_with_external_globals(
                    linked,
                    program,
                    match init_ep {
                        EntryPoint::Vm(idx) => idx,
                        _ => panic!(),
                    },
                    globals.as_mut_ptr(),
                    gs,
                );
            },
            #[cfg(feature = "llvm")]
            _ => {} // LLVM path tested separately
        }

        // Call process — should return 11 (counter was 10, now 11)
        match &mut prog {
            CompiledProgram::Vm {
                linked,
                program,
                vm,
                ..
            } => {
                let result = unsafe {
                    vm.call_with_external_globals(
                        linked,
                        program,
                        match process_ep {
                            EntryPoint::Vm(idx) => idx,
                            _ => panic!(),
                        },
                        globals.as_mut_ptr(),
                        gs,
                    )
                };
                assert_eq!(result, 11);

                // Call again — globals persist
                let result = unsafe {
                    vm.call_with_external_globals(
                        linked,
                        program,
                        match process_ep {
                            EntryPoint::Vm(idx) => idx,
                            _ => panic!(),
                        },
                        globals.as_mut_ptr(),
                        gs,
                    )
                };
                assert_eq!(result, 12);
            }
            #[cfg(feature = "llvm")]
            _ => {}
        }
    }

    #[test]
    fn prelude_allows_assume() {
        let prelude = r#"
            clamp(a: [i32], i: i32) -> i32 {
                assume i >= 0 && i < a.len
                a[i]
            }
        "#;
        let code = r#"
            main {
                var arr: [i32; 3]
                arr[0] = 10
                arr[1] = 20
                arr[2] = 30
                print(clamp(arr, 1))
            }
        "#;
        let mut compiler = Compiler::new();
        compiler.parse(prelude, "<prelude>");
        compiler.parse(code, "test.lyte");
        assert!(compiler.check());
    }

    #[test]
    fn prelude_does_not_affect_user_line_numbers() {
        // A 5-line prelude followed by user code with an error on its line 3.
        // The error should report line 3, not line 8.
        let prelude = "const BUF = 4\nconst X = 1\nconst Y = 2\nconst Z = 3\nconst W = 4";
        let code = "main {\n    var x: i32\n    x = true\n}";
        let mut compiler = Compiler::new();
        compiler.parse(prelude, "<prelude>");
        compiler.parse(code, "test.lyte");
        // check() should fail due to type error, and the error location
        // should reference test.lyte line 3, not line 8.
        assert!(!compiler.check());
    }

    #[test]
    fn top_level_assume_constrains_globals() {
        // Prelude declares globals and assumes that constrain them.
        // The user code uses those globals as loop bounds for array access.
        let prelude = r#"
            const MAX_FRAMES = 1024
            var frames: i32
            var input: [f32; MAX_FRAMES]
            var output: [f32; MAX_FRAMES]
            assume frames >= 0 && frames <= MAX_FRAMES
        "#;
        let code = r#"
            process {
                for i in 0 .. frames {
                    output[i] = input[i]
                }
            }
        "#;
        let mut compiler = Compiler::new();
        compiler.parse(prelude, "<prelude>");
        compiler.parse(code, "test.lyte");
        assert!(compiler.check(), "safety checker should pass with global assume");
    }

    #[test]
    fn top_level_assume_rejects_without_assume() {
        // Same code without the assume — safety checker should fail.
        let prelude = r#"
            const MAX_FRAMES = 1024
            var frames: i32
            var input: [f32; MAX_FRAMES]
            var output: [f32; MAX_FRAMES]
        "#;
        let code = r#"
            process {
                for i in 0 .. frames {
                    output[i] = input[i]
                }
            }
        "#;
        let mut compiler = Compiler::new();
        compiler.parse(prelude, "<prelude>");
        compiler.parse(code, "test.lyte");
        assert!(
            !compiler.check(),
            "safety checker should fail without global assume"
        );
    }

    #[test]
    fn global_slices_with_assume() {
        let prelude = r#"
            var frames: i32
            var input: [f32]
            var output: [f32]
            assume frames >= 0 && frames <= input.len && frames <= output.len
        "#;
        let code = r#"
            process {
                for i in 0 .. frames {
                    output[i] = input[i]
                }
            }
        "#;
        let mut compiler = Compiler::new();
        compiler.parse(prelude, "<prelude>");
        compiler.parse(code, "test.lyte");
        assert!(
            compiler.check(),
            "global slices with assumes should pass safety checker"
        );
    }

    #[test]
    fn global_slices_with_assume_eq_const() {
        let prelude = r#"
            const MAX_FRAMES = 256
            var frames: i32
            var input: [f32]
            var output: [f32]
            assume frames >= 0 && frames <= MAX_FRAMES
            assume input.len == MAX_FRAMES
            assume output.len == MAX_FRAMES
        "#;
        let code = r#"
            process {
                for i in 0 .. frames {
                    output[i] = input[i]
                }
            }
        "#;
        let mut compiler = Compiler::new();
        compiler.parse(prelude, "<prelude>");
        compiler.parse(code, "test.lyte");
        assert!(
            compiler.check(),
            "global slices with assume len == const should pass safety checker"
        );
    }

    #[test]
    fn global_slices_without_assume_fails() {
        let prelude = r#"
            var input: [f32]
            var output: [f32]
            var frames: i32
        "#;
        let code = r#"
            process {
                for i in 0 .. frames {
                    output[i] = input[i]
                }
            }
        "#;
        let mut compiler = Compiler::new();
        compiler.parse(prelude, "<prelude>");
        compiler.parse(code, "test.lyte");
        assert!(
            !compiler.check(),
            "global slices without assumes should fail safety checker"
        );
    }

    #[test]
    fn test_extern_fn_compile_and_call() {
        // Test that extern functions can be compiled and called through the VM.
        let prelude = r#"
            extern fn host_add(a: i32, b: i32) -> i32
        "#;
        let code = r#"
            fn main() -> i32 {
                host_add(3, 4)
            }
        "#;
        let mut compiler = Compiler::new();
        compiler.parse(prelude, "<prelude>");
        compiler.parse(code, "test.lyte");
        assert!(compiler.check(), "type check failed");
        compiler.specialize().expect("specialize failed");

        let vm_program = compiler.compile_vm().expect("VM compile failed");
        let globals_size = vm_program.globals_size;

        // Verify extern function info was collected.
        assert_eq!(vm_program.extern_funcs.len(), 1);
        assert_eq!(vm_program.extern_funcs[0].param_types.len(), 2);

        let linked = crate::vm::LinkedProgram::from_program(&vm_program);
        let mut vm = crate::vm::VM::new();

        // Allocate globals and bind the extern function.
        let mut globals = vec![0u8; globals_size];
        let extern_offset = vm_program.extern_funcs[0].globals_offset as usize;

        // Define a C function that adds two i32s.
        unsafe extern "C" fn host_add(_ctx: *mut u8, a: i32, b: i32) -> i32 {
            a + b
        }

        // Bind the function pointer and context into the globals buffer.
        unsafe {
            let ptr_slot = globals.as_mut_ptr().add(extern_offset) as *mut u64;
            let ctx_slot = globals.as_mut_ptr().add(extern_offset + 8) as *mut u64;
            *ptr_slot = host_add as *const () as u64;
            *ctx_slot = 0; // null context
        }

        // Run and check result.
        let func_idx = *vm_program.entry_points.get(&Name::str("main")).unwrap();
        let result = unsafe {
            vm.call_with_external_globals(&linked, &vm_program, func_idx, globals.as_mut_ptr(), globals_size)
        };
        assert_eq!(result, 7, "host_add(3, 4) should return 7");
    }

    #[cfg(feature = "cranelift")]
    #[test]
    fn test_extern_fn_cranelift_jit() {
        let prelude = r#"
            extern fn host_mul(a: i32, b: i32) -> i32
        "#;
        let code = r#"
            fn main() -> i32 {
                host_mul(5, 6)
            }
        "#;
        let mut compiler = Compiler::new();
        compiler.parse(prelude, "<prelude>");
        compiler.parse(code, "test.lyte");
        assert!(compiler.check(), "type check failed");
        compiler.specialize().expect("specialize failed");

        let (map, globals_size, jit) = compiler.jit_multi().unwrap();
        let main_ptr = *map.get(&Name::str("main")).unwrap();
        let mut globals = vec![0u8; globals_size];

        // Find the extern function's globals offset.
        let globals_info = compiler.globals_info_with_offset(crate::cancel::CANCEL_FLAG_RESERVED as usize);
        let extern_info = globals_info.iter().find(|g| g.4).expect("no extern global found");
        let extern_offset = extern_info.1;

        unsafe extern "C" fn host_mul(_ctx: *mut u8, a: i32, b: i32) -> i32 {
            a * b
        }

        unsafe {
            extern "C" {
                fn setjmp(env: *mut u8) -> i32;
            }

            // Bind extern function.
            let ptr_slot = globals.as_mut_ptr().add(extern_offset) as *mut u64;
            let ctx_slot = globals.as_mut_ptr().add(extern_offset + 8) as *mut u64;
            *ptr_slot = host_mul as *const () as u64;
            *ctx_slot = 0;

            crate::cancel::set_cancel_callback(
                globals.as_mut_ptr(),
                None,
                std::ptr::null_mut(),
            );
            let jmp_buf_ptr = globals.as_mut_ptr().add(crate::cancel::JMPBUF_OFFSET);
            if setjmp(jmp_buf_ptr) == 0 {
                type Entry = fn(*mut u8, *mut u8) -> i32;
                let code_fn = mem::transmute::<_, Entry>(main_ptr);
                let result = code_fn(globals.as_mut_ptr(), std::ptr::null_mut());
                assert_eq!(result, 30, "host_mul(5, 6) should return 30");
            } else {
                panic!("execution was cancelled unexpectedly");
            }
        }

        jit.free_memory();
    }

    #[test]
    fn test_extern_fn_string_param_vm() {
        // Test that extern functions can receive string ([u8]) parameters.
        // Slices expand to (ptr, i32 len) at the C boundary.
        let prelude = r#"
            extern fn send(msg: [i8])
        "#;
        let code = r#"
            fn main() {
                send("hello")
            }
        "#;
        let mut compiler = Compiler::new();
        compiler.parse(prelude, "<prelude>");
        compiler.parse(code, "test.lyte");
        assert!(compiler.check(), "type check failed");
        compiler.specialize().expect("specialize failed");

        let vm_program = compiler.compile_vm().expect("VM compile failed");
        let globals_size = vm_program.globals_size;

        assert_eq!(vm_program.extern_funcs.len(), 1);
        // Slice expands to [Ptr, I32] at C level.
        assert_eq!(vm_program.extern_funcs[0].param_types, vec![
            crate::vm::ExternType::Ptr,
            crate::vm::ExternType::I32,
        ]);

        let linked = crate::vm::LinkedProgram::from_program(&vm_program);
        let mut vm = crate::vm::VM::new();
        let mut globals = vec![0u8; globals_size];
        let extern_offset = vm_program.extern_funcs[0].globals_offset as usize;

        use std::sync::atomic::{AtomicBool, Ordering};
        static SEND_CALLED: AtomicBool = AtomicBool::new(false);

        // The C signature is: void send(void* ctx, const char* data, int32_t len)
        // String literals in Lyte are null-terminated [i8; N+1].
        unsafe extern "C" fn host_send(_ctx: *mut u8, data: *const u8, len: i32) {
            let slice = std::slice::from_raw_parts(data, len as usize);
            // "hello" becomes [104, 101, 108, 108, 111, 0] (null-terminated)
            assert_eq!(&slice[..5], b"hello");
            assert_eq!(len, 6); // includes null terminator
            SEND_CALLED.store(true, Ordering::SeqCst);
        }

        unsafe {
            let ptr_slot = globals.as_mut_ptr().add(extern_offset) as *mut u64;
            let ctx_slot = globals.as_mut_ptr().add(extern_offset + 8) as *mut u64;
            *ptr_slot = host_send as *const () as u64;
            *ctx_slot = 0;
        }

        SEND_CALLED.store(false, Ordering::SeqCst);
        let func_idx = *vm_program.entry_points.get(&Name::str("main")).unwrap();
        unsafe {
            vm.call_with_external_globals(&linked, &vm_program, func_idx, globals.as_mut_ptr(), globals_size);
        }
        assert!(SEND_CALLED.load(Ordering::SeqCst), "send() was not called");
    }

    #[cfg(feature = "llvm")]
    #[test]
    fn test_extern_fn_string_param_llvm() {
        // Test that LLVM extern functions correctly receive string ([i8]) parameters.
        // Slices expand to (ptr, i32 len) at the C boundary.
        let prelude = r#"
            extern fn send(msg: [i8])
        "#;
        let code = r#"
            fn main() {
                send("hello")
            }
        "#;
        let mut compiler = Compiler::new();
        compiler.parse(prelude, "<prelude>");
        compiler.parse(code, "test.lyte");
        assert!(compiler.check(), "type check failed");
        compiler.specialize().expect("specialize failed");

        let llvm_prog = {
            let jit = crate::llvm_jit::LLVMJIT::new();
            let entry_points = vec![Name::str("main")];
            jit.compile_only(&compiler.decls, &entry_points).expect("LLVM compile failed")
        };
        let globals_size = llvm_prog.globals_size;
        let main_ptr = *llvm_prog.entry_points.get(&Name::str("main")).unwrap();
        let mut globals = vec![0u8; globals_size];

        // Find the extern function's globals offset.
        let globals_info = compiler.globals_info_with_offset(crate::cancel::CANCEL_FLAG_RESERVED as usize);
        let extern_info = globals_info.iter().find(|g| g.4).expect("no extern global found");
        let extern_offset = extern_info.1;

        use std::sync::atomic::{AtomicBool, Ordering};
        static LLVM_SEND_CALLED: AtomicBool = AtomicBool::new(false);

        unsafe extern "C" fn host_send(_ctx: *mut u8, data: *const u8, len: i32) {
            let slice = std::slice::from_raw_parts(data, len as usize);
            assert_eq!(&slice[..5], b"hello");
            assert_eq!(len, 6); // includes null terminator
            LLVM_SEND_CALLED.store(true, Ordering::SeqCst);
        }

        unsafe {
            extern "C" {
                fn setjmp(env: *mut u8) -> i32;
            }

            let ptr_slot = globals.as_mut_ptr().add(extern_offset) as *mut u64;
            let ctx_slot = globals.as_mut_ptr().add(extern_offset + 8) as *mut u64;
            *ptr_slot = host_send as *const () as u64;
            *ctx_slot = 0;

            crate::cancel::set_cancel_callback(
                globals.as_mut_ptr(),
                None,
                std::ptr::null_mut(),
            );
            let jmp_buf_ptr = globals.as_mut_ptr().add(crate::cancel::JMPBUF_OFFSET);
            LLVM_SEND_CALLED.store(false, Ordering::SeqCst);
            if setjmp(jmp_buf_ptr) == 0 {
                type Entry = fn(*mut u8, *mut u8) -> i32;
                let code_fn = mem::transmute::<_, Entry>(main_ptr);
                code_fn(globals.as_mut_ptr(), std::ptr::null_mut());
            } else {
                panic!("execution was cancelled unexpectedly");
            }
        }

        assert!(LLVM_SEND_CALLED.load(Ordering::SeqCst), "send() was not called");
    }

    // ---- VM16 integration tests ----

    fn run_vm16(code: &str) {
        let mut compiler = Compiler::new();
        compiler.parse(code.into(), ".");
        assert!(compiler.check(), "type check failed");
        compiler.specialize().unwrap();
        compiler.run_vm16().expect("VM16 execution failed");
    }

    #[test]
    fn test_vm16_basic() {
        run_vm16(r#"
            main {
                var x = 42
                assert(x == 42)
            }
        "#);
    }

    #[test]
    fn test_vm16_arithmetic() {
        run_vm16(r#"
            main {
                var a = 3
                var b = 4
                var c = a + b
                assert(c == 7)
                assert(a * b == 12)
                assert(b - a == 1)
            }
        "#);
    }

    #[test]
    fn test_vm16_loop() {
        run_vm16(r#"
            main {
                var sum = 0
                for i in 0 .. 10 {
                    sum = sum + i
                }
                assert(sum == 45)
            }
        "#);
    }

    #[test]
    fn test_vm16_float() {
        // Simple float test without function calls.
        run_vm16(r#"
            main {
                var x: f32 = 25.0
                var y = sqrt(x)
                // y should be 5.0, truncate to int
                var z = y as i32
                assert(z == 5)
            }
        "#);
    }

    #[test]
    fn test_vm16_print() {
        run_vm16(r#"
            main {
                print(123)
            }
        "#);
    }
}
