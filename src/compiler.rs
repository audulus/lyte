use crate::*;
use core::mem;
use std::sync::Arc;
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

#[salsa::query_group(InputsStorage)]
pub trait InputQueries {
    #[salsa::input]
    fn paths(&self) -> Vec<String>;

    #[salsa::input]
    fn source_text(&self, path: String) -> String;
}

#[salsa::query_group(ParserStorage)]
trait ParserQueries: InputQueries {
    /// AST for a single file.
    fn ast(&self, path: String) -> Arc<Tree>;

    /// ASTs for all files.
    fn program_ast(&self) -> Vec<Arc<Tree>>;

    /// All decls in the program.
    fn decls(&self) -> DeclTable;

    /// Did the program parse successfully?
    fn parsed(&self) -> bool;
}

/// The AST for a file.
fn ast(db: &dyn ParserQueries, path: String) -> Arc<Tree> {
    let input_string = db.source_text(path.clone());
    let mut lexer = Lexer::new(&input_string, &path);

    let mut tree = Tree::default();

    lexer.next();
    tree.decls = parse_program(&mut lexer, &mut tree.errors);

    for err in &tree.errors {
        println!(
            "{}:{}: {}",
            err.location.file, err.location.line, err.message
        );
    }

    Arc::new(tree)
}

/// ASTs for all files.
fn program_ast(db: &dyn ParserQueries) -> Vec<Arc<Tree>> {
    let paths = db.paths();
    let mut trees = vec![];

    for path in paths {
        trees.push(db.ast(path));
    }

    trees
}

/// Declarations in all files.
fn decls(db: &dyn ParserQueries) -> DeclTable {
    let mut decls = vec![];
    let mut trees = db.program_ast();

    for tree in &mut trees {
        decls.append(&mut tree.decls.clone());
    }

    DeclTable::new(decls)
}

/// Did the program successfully parse?
fn parsed(db: &dyn ParserQueries) -> bool {
    let trees = db.program_ast();

    for tree in trees {
        if !tree.errors.is_empty() {
            return false;
        }
    }
    true
}

#[salsa::query_group(CheckerStorage)]
trait CheckerQueries: ParserQueries {
    fn check_decl(&self, decl: Decl) -> Result<Decl, ()>;
    fn check(&self) -> bool;
    fn checked_decls(&self) -> Result<DeclTable, ()>;
}

fn print_fn_decl(fdecl: &FuncDecl, checker: &Checker) {
    for (i, expr) in fdecl.arena.exprs.iter().enumerate() {
        println!("{}: {:?}, {:?}", i, expr, checker.types[i]);
    }
}

/// Check a single declaration.
fn check_decl(db: &dyn CheckerQueries, mut decl: Decl) -> Result<Decl, ()> {
    let decls = db.decls();

    let mut checker = Checker::new();

    checker.check_decl(&decl, &decls);

    checker.print_errors();

    if !checker.errors.is_empty() {
        return Err(());
    }

    // Update function decl with computed types.
    if let Decl::Func(ref mut fdecl) = &mut decl {
        // Apply substitution to all types.
        fdecl.types = checker.solved_types();

        // print_fn_decl(fdecl, &checker);
    }

    Ok(decl)
}

/// Check all decls in the program.
fn check(db: &dyn CheckerQueries) -> bool {
    let decls = db.decls();
    let mut result = true;

    for decl in &decls.decls {
        if db.check_decl(decl.clone()).is_err() {
            result = false;
        }
    }

    result
}

fn checked_decls(db: &dyn CheckerQueries) -> Result<DeclTable, ()> {
    let mut decls = db.decls();

    for decl in &mut decls.decls {
        if let Ok(d) = db.check_decl(decl.clone()) {
            *decl = d;
        } else {
            return Err(());
        }
    }

    Ok(decls)
}

#[salsa::query_group(CompilerStorage)]
trait CompilerQueries: CheckerQueries {
    /// JIT using cranelift.
    fn program_jit(&self) -> Result<*const u8, String>;
}

fn program_jit(db: &dyn CompilerQueries) -> Result<*const u8, String> {
    let decls = if let Ok(decls) = db.checked_decls() {
        decls
    } else {
        return Err(String::from("Error"));
    };

    let mut jit = JIT::default();

    jit.compile(&decls)
}

#[salsa::database(InputsStorage, ParserStorage, CheckerStorage, CompilerStorage)]
#[derive(Default)]
struct Database {
    storage: salsa::Storage<Self>,
}

impl salsa::Database for Database {}

pub struct Compiler2 {
    ast: Vec<Tree>,
    decls: DeclTable,
}

impl Compiler2 {
    pub fn new() -> Self {
        Self { ast: Vec::new(), decls: DeclTable::new(vec![]) }
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

    fn parse(&mut self, contents: &str, path: &str) {
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

        self.ast.push(tree);
    }

    pub fn check(&mut self) -> bool {

        let mut decls = vec![];
        for tree in &self.ast {
            decls.append(&mut tree.decls.clone());
        }

        self.decls = DeclTable::new(decls);

        let mut checker = Checker::new();

        println!("Checking program...");

        for tree in &self.ast {
            for decl in &tree.decls {
                println!("Checking decl: {:?}...", decl);
                checker.check_decl(decl, &self.decls);
            }
        }

        checker.print_errors();

        if !checker.errors.is_empty() {
            return false;
        }

        // Update function decls with computed types.
        for tree in &mut self.ast {
            for decl in &mut tree.decls {
                if let Decl::Func(ref mut fdecl) = decl {
                    fdecl.types = checker.solved_types();
                }
            }
        }

        true
    }

    pub fn jit(&self) -> Result<*const u8, String> {
        let mut jit = JIT::default();
        if self.decls.decls.is_empty() {
            return Err(String::from("No declarations to compile"));
        }
        jit.compile(&self.decls)
    }

}

pub struct Compiler {
    db: Database,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            db: Database::default(),
        }
    }

    /// Let the compiler know that the contents of a file
    /// has changed.
    pub fn update_path(&mut self, path: &str, contents: String) {
        self.db.set_source_text(path.into(), contents);
    }

    pub fn set_paths(&mut self, paths: Vec<String>) {
        self.db.set_paths(paths);
    }

    pub fn parsed(&mut self) -> bool {
        self.db.parsed()
    }

    pub fn check(&mut self) -> bool {
        self.db.check()
    }

    pub fn print_ast(&mut self) {
        println!("{:?}", self.db.program_ast());
    }

    pub fn jit(&mut self) {
        let r = self.db.program_jit();
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

impl Default for Compiler {
    fn default() -> Self {
        Self::new()
    }
}

mod tests {

    fn jit(code: &str) {
        
        let mut compiler = crate::Compiler2::new();
        let paths = vec![String::from(".")];

        compiler.parse(code.into(), &paths[0]);
        compiler.check();
        // compiler.jit(); 

        /*
        let mut compiler = crate::Compiler::new();
        let paths = vec![String::from(".")];

        compiler.update_path(&paths[0], code.into());
        compiler.set_paths(paths);

        compiler.jit();*/
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
}
