use crate::vm::{VM, VMProgram};
use crate::vm_codegen::VMCodegen;
use crate::*;
use core::mem;
use std::collections::HashMap;
use std::fs;

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
        }),
    ];

    // Math builtins: unary f32 and f64 variants.
    let unary_math = [
        ("sinf", "sind"),
        ("cosf", "cosd"),
        ("tanf", "tand"),
        ("lnf", "lnd"),
        ("expf", "expd"),
        ("sqrtf", "sqrtd"),
        ("absf", "absd"),
        ("floorf", "floord"),
        ("ceilf", "ceild"),
    ];
    for (f32_name, f64_name) in unary_math {
        decls.push(Decl::Func(FuncDecl {
            name: Name::new(f32_name.into()),
            typevars: vec![],
            size_vars: vec![],
            params: vec![Param {
                name: Name::new("x".into()),
                ty: Some(mk_type(Type::Float32)),
            }],
            body: None,
            ret: mk_type(Type::Float32),
            constraints: vec![],
            loc: test_loc(),
            arena: ExprArena::new(),
            types: vec![],
            closure_vars: vec![],
        }));
        decls.push(Decl::Func(FuncDecl {
            name: Name::new(f64_name.into()),
            typevars: vec![],
            size_vars: vec![],
            params: vec![Param {
                name: Name::new("x".into()),
                ty: Some(mk_type(Type::Float64)),
            }],
            body: None,
            ret: mk_type(Type::Float64),
            constraints: vec![],
            loc: test_loc(),
            arena: ExprArena::new(),
            types: vec![],
            closure_vars: vec![],
        }));
    }

    // Math builtins: binary f32 and f64 variants.
    let binary_math = [
        ("powf", "powd"),
        ("atan2f", "atan2d"),
    ];
    for (f32_name, f64_name) in binary_math {
        decls.push(Decl::Func(FuncDecl {
            name: Name::new(f32_name.into()),
            typevars: vec![],
            size_vars: vec![],
            params: vec![
                Param { name: Name::new("x".into()), ty: Some(mk_type(Type::Float32)) },
                Param { name: Name::new("y".into()), ty: Some(mk_type(Type::Float32)) },
            ],
            body: None,
            ret: mk_type(Type::Float32),
            constraints: vec![],
            loc: test_loc(),
            arena: ExprArena::new(),
            types: vec![],
            closure_vars: vec![],
        }));
        decls.push(Decl::Func(FuncDecl {
            name: Name::new(f64_name.into()),
            typevars: vec![],
            size_vars: vec![],
            params: vec![
                Param { name: Name::new("x".into()), ty: Some(mk_type(Type::Float64)) },
                Param { name: Name::new("y".into()), ty: Some(mk_type(Type::Float64)) },
            ],
            body: None,
            ret: mk_type(Type::Float64),
            constraints: vec![],
            loc: test_loc(),
            arena: ExprArena::new(),
            types: vec![],
            closure_vars: vec![],
        }));
    }

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

pub struct Compiler {
    ast: Vec<Tree>,
    decls: DeclTable,
    pub print_ir: bool,
    /// Number of AST trees that belong to the stdlib (parsed in new()).
    stdlib_trees: usize,
}

impl Compiler {
    pub fn new() -> Self {
        let mut c = Self { ast: Vec::new(), decls: DeclTable::new(vec![]), print_ir: false, stdlib_trees: 0 };
        c.parse(STDLIB, "<stdlib>");
        c.stdlib_trees = c.ast.len();
        c
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

        for err in &tree.errors {
            println!(
                "{}:{}: {}",
                err.location.file, err.location.line, err.message
            );
        }

        let success = tree.errors.is_empty();
        self.ast.push(tree);
        success
    }

    pub fn check(&mut self) -> bool {

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
                    print_error_with_context(m.loc, &format!("duplicate macro: {}", m.name));
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

        for decl in &mut self.decls.decls {
            // Skip type-checking macro declarations (they are untyped templates).
            if matches!(decl, Decl::Macro(_)) {
                continue;
            }

            let mut checker = Checker::new();
            checker.check_decl(decl, &orig_decls);

            checker.print_errors();
            if !checker.errors.is_empty() {
                return false;
            }

            if let Decl::Func(ref mut fdecl) = decl {
                fdecl.types = checker.solved_types();
            }
        }

        // Static safety checks (array bounds, division by zero).
        let mut safety_checker = SafetyChecker::new();
        safety_checker.check(&self.decls);
        safety_checker.print_errors();
        if !safety_checker.errors.is_empty() {
            return false;
        }

        true
    }

    pub fn specialize(&mut self) -> Result<(), String> {
        let mut pass = MonomorphPass::new();
        let name = Name::new("main".into());
        let all_decls = pass.monomorphize(&self.decls, name)?;
        // monomorphize now returns all decls (original + specialized)
        self.decls = DeclTable::new(all_decls);
        Ok(())
    }

    pub fn has_decls(&self) -> bool {
        // Check if there are any user declarations beyond the built-ins and stdlib
        let stdlib_decl_count: usize = self.ast.iter().take(self.stdlib_trees).map(|t| t.decls.len()).sum();
        self.decls.decls.len() > builtin_decls().len() + stdlib_decl_count
    }

    /// Returns info about each global variable: (name, offset, size, type_string).
    /// The offset and size are in bytes within the globals buffer.
    pub fn globals_info(&self) -> Vec<(String, usize, usize, String)> {
        let mut result = Vec::new();
        // Mirror the layout used by JIT::declare_globals: user globals start
        // after the cancel flag reservation.
        let mut offset: usize = CANCEL_FLAG_RESERVED as usize;
        for decl in &self.decls.decls {
            if let Decl::Global { name, ty } = decl {
                let size = ty.size(&self.decls) as usize;
                let type_str = ty.pretty_print();
                result.push((name.to_string(), offset, size, type_str));
                offset += size;
            }
        }
        result
    }

    pub fn jit(&self) -> Result<(*const u8, usize), String> {
        let mut jit = JIT::default();
        jit.print_ir = self.print_ir;
        if self.decls.decls.is_empty() {
            return Err(String::from("No declarations to compile"));
        }
        jit.compile(&self.decls)
    }

    pub fn run(&mut self) {
        let r = self.jit();
        if let Ok((code_ptr, globals_size)) = r {
            println!("compilation successful");

            // Allocate zeroed global memory and pass to main.
            // globals[0] is the cancel flag; globals[8] holds the longjmp target.
            type Entry = fn(*mut u8) -> ();
            let mut globals: Vec<u8> = vec![0u8; globals_size];
            let cancelled = unsafe {
                // globals[0]: cancel flag; globals[8]: JmpBuf for longjmp target.
                extern "C" { fn setjmp(env: *mut u8) -> i32; }
                let jmp_buf_ptr = globals.as_mut_ptr().add(8);
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
        } else {
            println!("{:?}", r);
            panic!();
        }
    }

    /// Compile the declarations to a VM program.
    pub fn compile_vm(&self) -> Result<VMProgram, String> {
        if self.decls.decls.is_empty() {
            return Err(String::from("No declarations to compile"));
        }
        let mut codegen = VMCodegen::new();
        codegen.compile(&self.decls)
    }

    /// Run the code using the VM interpreter.
    pub fn run_vm(&mut self) -> Result<i64, String> {
        let program = self.compile_vm()?;
        let mut vm = VM::new();
        Ok(vm.run(&program))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn jit(code: &str) {

        let mut compiler = Compiler::new();
        let paths = vec![String::from(".")];

        compiler.parse(code.into(), &paths[0]);
        assert!(compiler.check());
        compiler.specialize().unwrap();
        assert!(compiler.decls.decls.len() > 0);
        compiler.run();
    }

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

        let (code_ptr, globals_size) = compiler.jit().expect("JIT compilation failed");
        let mut globals: Vec<u8> = vec![0u8; globals_size];

        // Wrap the raw pointer so it can be sent to another thread.
        struct CancelPtr(*mut u8);
        unsafe impl Send for CancelPtr {}
        let sender = CancelPtr(globals.as_mut_ptr());

        // After 50 ms, set globals[0] = 1 to trigger cancellation.
        let _handle = std::thread::spawn(move || {
            std::thread::sleep(std::time::Duration::from_millis(50));
            unsafe { sender.0.write_volatile(1) };
        });

        let cancelled = unsafe {
            extern "C" { fn setjmp(env: *mut u8) -> i32; }
            let jmp_buf_ptr = globals.as_mut_ptr().add(8);
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
}
