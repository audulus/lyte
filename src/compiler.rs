use crate::*;
use core::mem;
use std::fs;

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

        let mut decls = vec![];
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

    pub fn jit(&self) -> Result<*const u8, String> {
        let mut jit = JIT::default();
        jit.print_ir = self.print_ir;
        if self.decls.decls.is_empty() {
            return Err(String::from("No declarations to compile"));
        }
        jit.compile(&self.decls)
    }

    pub fn run(&mut self) {
        let r = self.jit();
        if let Ok(code_ptr) = r {
            println!("compilation successful");

            type Entry = fn() -> ();

            unsafe {
                let code_fn = mem::transmute::<_, Entry>(code_ptr);
                code_fn();
            }
        } else {
            println!("{:?}", r);
        }
    }

}

mod tests {

    fn jit(code: &str) {
        
        let mut compiler = crate::Compiler::new();
        let paths = vec![String::from(".")];

        compiler.parse(code.into(), &paths[0]);
        assert!(compiler.check());
        compiler.run();
    }

    #[test]
    fn basic() {
        let code = r#"
           main {
              var x = 1
           }
        "#;

        jit(code);
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
    }

    #[test]
    fn test_assert() {
        let code = r#"
            assert(value: bool) → void
            main {
               var x = 42
               assert(x == 42)
            }
        "#;

        jit(code);
    }

    #[test]
    fn test_assert_ne() {
        let code = r#"
            assert(value: bool) → void
            main {
               var x = 42
               assert(x != 5)
            }
        "#;

        jit(code);
    }

    #[test]
    fn test_field_assign() {
        let code = r#"
            assert(cond: bool) → void

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
    }

    #[test]
    fn test_field_assign_2() {
        jit(r#"
            assert(cond: bool) → void

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
        "#);
    }

    #[test]
    fn test_struct_assign() {
        let code = r#"
            assert(cond: bool) → void

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
    }

    #[test]
    fn test_array() {
        let code = r#"
            assert(cond: bool) → void

            main {
                var a: [i32; 2]
                a[1] = 42
                assert(a[1] == 42)
            }
        "#;

        jit(code);
    }

    #[test]
    fn test_call() {
        let code = r#"
            assert(cond: bool) → void

            f {
                assert(1 == 1)
            }

            main {
                f()
            }
        "#;

        jit(code);
    }

    #[test]
    fn test_call2() {
        let code = r#"
            assert(cond: bool) → void

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
    }

    #[test]
    fn test_neg() {
        let code = r#"
            assert(cond: bool) → void

            main {
                assert(1-2 == -1)
            }
        "#;

        jit(code);
    }

    #[test]
    fn test_struct() {
        let code = r#"
            assert(cond: bool) → void

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
    }
}
