use crate::vm::{VM, VMProgram};
use crate::vm_codegen::VMCodegen;
use crate::*;
use core::mem;
use std::fs;

/// Returns the built-in function declarations (assert, print, etc.)
fn builtin_decls() -> Vec<Decl> {
    vec![
        // assert(cond: bool) → void
        Decl::Func(FuncDecl {
            name: Name::new("assert".into()),
            typevars: vec![],
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
        }),
        // print(value: i32) → void
        Decl::Func(FuncDecl {
            name: Name::new("print".into()),
            typevars: vec![],
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
        }),
    ]
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

pub struct Compiler {
    ast: Vec<Tree>,
    decls: DeclTable,
    pub print_ir: bool,
}

impl Compiler {
    pub fn new() -> Self {
        Self { ast: Vec::new(), decls: DeclTable::new(vec![]), print_ir: false }
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

        self.decls = DeclTable::new(decls);
        let orig_decls = self.decls.clone();

        for decl in &mut self.decls.decls {
            let mut checker = Checker::new();
            checker.check_decl(decl, &orig_decls);

            checker.print_errors();
            if !checker.errors.is_empty() {
                return false;
            }

            match decl {
                Decl::Func(ref mut fdecl) => {
                    fdecl.types = checker.solved_types();
                }
                Decl::Macro(ref mut mdecl) => {
                    mdecl.types = checker.solved_types();
                }
                _ => {}
            }
        }

        true
    }

    pub fn specialize(&mut self) -> bool {
        let mut pass = MonomorphPass::new();
        let name = Name::new("main".into());
        if let Ok(all_decls) = pass.monomorphize(&self.decls, name ) {
            // monomorphize now returns all decls (original + specialized)
            self.decls = DeclTable::new(all_decls);
            return true;
        }
        false
    }

    pub fn has_decls(&self) -> bool {
        // Check if there are any user declarations beyond the built-ins
        self.decls.decls.len() > builtin_decls().len()
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
            type Entry = fn(*mut u8) -> ();
            let mut globals: Vec<u8> = vec![0u8; globals_size];
            unsafe {
                let code_fn = mem::transmute::<_, Entry>(code_ptr);
                code_fn(globals.as_mut_ptr());
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
        compiler.specialize();
        assert!(compiler.decls.decls.len() > 0);
        compiler.run();
    }

    fn run(code: &str) {
        let mut compiler = Compiler::new();
        let paths = vec![String::from(".")];

        compiler.parse(code.into(), &paths[0]);
        assert!(compiler.check());
        compiler.specialize();
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
}
