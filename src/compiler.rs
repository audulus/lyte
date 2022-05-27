use crate::*;
use std::sync::Arc;

// An AST.
// 
// I've read that some IDE-oriented compilers prefer to
// have a full syntax tree (which can represent the exact formatting of evertying)
// but that seems like a real pain.
#[derive(Eq, PartialEq, Clone, Debug, Hash)]
pub struct Tree {
    pub decls: Vec<Decl>,
    pub exprs: ExprArena,
}

impl Tree {
    pub fn new() -> Self {
        Self {
            decls: vec![],
            exprs: ExprArena::new(),
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
    fn ast(&self, path: String) -> Arc<Tree>;
    fn program_ast(&self) -> Vec<Arc<Tree>>;
    fn decls(&self) -> Vec<Decl>;
    fn parsed(&self) -> bool;
}

/// The AST for a file.
fn ast(db: &dyn ParserQueries, path: String) -> Arc<Tree> {
    let input_string = db.source_text(path.clone());
    let mut lexer = Lexer::new(&input_string, &path);

    let mut tree = Tree::new();

    lexer.next();
    tree.decls = parse_program(&mut lexer, &mut tree.exprs);

    for err in &tree.exprs.errors {
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
fn decls(db: &dyn ParserQueries) -> Vec<Decl> {
    let mut decls = vec![];
    let mut trees = db.program_ast();

    for tree in &mut trees {
        decls.append(&mut tree.decls.clone());
    }

    decls
}

fn parsed(db: &dyn ParserQueries) -> bool {
    let trees = db.program_ast();

    for tree in trees {
        if !tree.exprs.errors.is_empty() {
            return false;
        }
    }
    true
}

#[salsa::query_group(CheckerStorage)]
trait CheckerQueries: ParserQueries {
    fn check_decl(&self, decl: Decl, tree: Arc<Tree>) -> bool;
    fn check(&self) -> bool;
}

/// Check a single declaration.
fn check_decl(db: &dyn CheckerQueries, decl: Decl, tree: Arc<Tree>) -> bool {
    let decls = db.decls();

    let mut checker = Checker::new();

    checker.check_decl(&decl, &tree.exprs, &decls);

    for err in &checker.errors {
        println!(
            "❌ {}:{}: {}",
            err.location.file, err.location.line, err.message
        );
    }

    // let mut i = 0;
    // for expr in &tree.exprs.exprs {
    //     println!("{}: {:?}, {:?}", i, expr, checker.types[i]);
    //     i += 1;
    // }

    checker.errors.is_empty()
}

fn check(db: &dyn CheckerQueries) -> bool {
    let trees = db.program_ast();
    let mut result = true;

    for tree in trees {
        for decl in &tree.decls {
            if !db.check_decl(decl.clone(), tree.clone()) {
                result = false;
            }
        }
    }

    result
}

#[salsa::database(InputsStorage, ParserStorage, CheckerStorage)]
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
}

impl Default for Compiler {
    fn default() -> Self {
        Self::new()
    }
}
