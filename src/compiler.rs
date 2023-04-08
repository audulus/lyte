use crate::*;
use std::sync::Arc;

// An AST.
//
// I've read that some IDE-oriented compilers prefer to
// have a full syntax tree (which can represent the exact formatting of everything)
// but that seems like a real pain.
#[derive(Eq, PartialEq, Clone, Debug, Hash)]
pub struct Tree {
    pub decls: Vec<Decl>,
    pub errors: Vec<ParseError>,
}

impl Tree {
    pub fn new() -> Self {
        Self {
            decls: vec![],
            errors: vec![],
        }
    }
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

    let mut tree = Tree::new();

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
    let mut i = 0;
    for expr in &fdecl.arena.exprs {
        println!("{}: {:?}, {:?}", i, expr, checker.types[i]);
        i += 1;
    }
}

/// Check a single declaration.
fn check_decl(db: &dyn CheckerQueries, mut decl: Decl) -> Result<Decl, ()> {
    let decls = db.decls();

    let mut checker = Checker::new();

    checker.check_decl(&decl, &decls);

    for err in &checker.errors {
        println!(
            "❌ {}:{}: {}",
            err.location.file, err.location.line, err.message
        );
    }

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
        if let Ok(ptr) = r {
            println!("compilation successful");
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

    #[test]
    fn basic() {
        let code = r#"
           main {
              let x = 1
           }
        "#;

        let mut compiler = crate::Compiler::new();
        let paths = vec![String::from(".")];

        compiler.update_path(&paths[0], code.into());
        compiler.set_paths(paths);

        compiler.jit();
    }
}
