use crate::*;
use std::fs;

#[derive(Eq, PartialEq, Clone, Debug, Hash)]
pub struct Tree {
    pub decls: Vec<Decl>,
    pub exprs: ExprArena
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
pub trait Inputs {
    #[salsa::input]
    fn paths(&self) -> Vec<String>;

    #[salsa::input]
    fn source_text(&self, path: String) -> String;
}

#[salsa::query_group(ParserStorage)]
trait Parser: Inputs {
    fn ast(&self, path: String) -> Tree;
    fn program_ast(&self) -> Tree;
    fn program_ast2(&self) -> Vec<Tree>;
    fn decls(&self) -> Vec<Decl>;
}

/// The AST for a file.
fn ast(db: &dyn Parser, path: String) -> Tree {

    // Read the input string:
    let input_string = db.source_text(path.clone());
    let mut lexer = Lexer::new(&input_string, &path);

    let mut tree = Tree::new();

    lexer.next();
    match parse_program(&mut lexer, &mut tree.exprs) {
        Ok(decls) => {
            tree.decls.extend(decls);
        }
        Err(err) => {
            println!(
                "{}:{}: {}",
                err.location.file, err.location.line, err.message
            );
        }
    }

    tree
}

fn program_ast(db: &dyn Parser) -> Tree {
    let paths = db.paths();

    assert_eq!(paths.len(), 1); // XXX: for now

    db.ast(paths[0].clone())
}

/// ASTs for all files.
fn program_ast2(db: &dyn Parser) -> Vec<Tree> {
    let paths = db.paths();
    let mut trees = vec![];

    for path in paths {
        trees.push(db.ast(path));
    }

    trees
}

/// Declarations in all files.
fn decls(db: &dyn Parser) -> Vec<Decl> {

    let mut decls = vec![];
    let mut trees = db.program_ast2();

    for tree in &mut trees {
        decls.append(&mut tree.decls);
    }

    decls
}

#[salsa::query_group(CheckerStorage)]
trait Checker2: Parser {
    fn check_decl(&self, decl: Decl, tree: Tree) -> bool;
    fn check(&self) -> bool;
}

/// Check a single declaration.
fn check_decl(_db: &dyn Checker2, decl: Decl, tree: Tree) -> bool {

    let mut checker = Checker::new();

    let result = match checker.check_decl(&decl, &tree.exprs, &tree.decls) {
        Ok(_) => true,
        Err(err) => {
            println!(
                "❌ {}:{}: {}",
                err.location.file, err.location.line, err.message
            );
            false
        }
    };

    result
}

fn check(db: &dyn Checker2) -> bool {

    let tree = db.program_ast();

    let mut checker = Checker::new();

    let result = match checker.check(&tree.exprs, &tree.decls) {
        Ok(_) => true,
        Err(err) => {
            println!(
                "❌ {}:{}: {}",
                err.location.file, err.location.line, err.message
            );
            false
        }
    };

    let mut i = 0;
    for expr in &tree.exprs.exprs {
        println!("{}: {:?}, {:?}", i, expr, checker.types[i]);
        i += 1;
    }

    result
    
}

#[salsa::database(InputsStorage, ParserStorage, CheckerStorage)]
#[derive(Default)]
struct Database {
    storage: salsa::Storage<Self>
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
    pub fn update_path(&mut self, path: &str) {
        if let Ok(string) = fs::read_to_string(path) {
            self.db.set_source_text(path.into(), string);
        }
    }

    pub fn set_paths(&mut self, paths: Vec<String>) {
        self.db.set_paths(paths);
    }

    pub fn check(&mut self) -> bool {
        self.db.check()
    }

}
