use crate::*;
use std::fs;
use std::path::Path;

#[derive(Eq, PartialEq, Clone, Debug)]
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
}

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

#[salsa::query_group(CheckerStorage)]
trait Checker2: Parser {
    fn check(&self) -> bool;
}

fn check(db: &dyn Checker2) -> bool {

    let tree = db.program_ast();

    let mut checker = Checker::new();

    match checker.check(&tree.exprs, &tree.decls) {
        Ok(_) => {}
        Err(err) => {
            println!(
                "❌ {}:{}: {}",
                err.location.file, err.location.line, err.message
            );
            return false;
        }
    }
    true
    
}

#[salsa::database(InputsStorage, ParserStorage)]
#[derive(Default)]
struct Database {
    storage: salsa::Storage<Self>
}

impl salsa::Database for Database {}

pub struct Compiler {
    db: Database,
    pub decls: Vec<Decl>,
    pub exprs: ExprArena,
    pub checker: Checker,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            db: Database::default(),
            decls: vec![],
            exprs: ExprArena::new(),
            checker: Checker::new(),
        }
    }

    /// Let the compiler know that the contents of a file
    /// has changed.
    pub fn update_path(&mut self, path: &str) {
        if let Ok(string) = fs::read_to_string(path) {
            self.db.set_source_text(path.into(), string);
        }
    }

    /// Returns all declarations in the program.
    // pub fn get_decls(&mut self) -> Vec<Decl> {

    //     let decls = vec![];

    //     for (path, tree) in &mut self.trees {
    //         if tree.is_none() {
    //             *tree = parse_file(&path);
    //         }
    //     }

    //     decls
    // }

    pub fn parse_file(&mut self, path: &Path) -> bool {
        if let Ok(string) = fs::read_to_string(path) {
            println!("parsing file: {:?}", path);
            let mut lexer = Lexer::new(&string, path.to_str().unwrap());
            lexer.next();
            match parse_program(&mut lexer, &mut self.exprs) {
                Ok(decls) => {
                    self.decls.extend(decls);
                    true
                }
                Err(err) => {
                    println!(
                        "{}:{}: {}",
                        err.location.file, err.location.line, err.message
                    );
                    false
                }
            }
        } else {
            println!("error reading file: {:?}", path);
            false
        }
    }

    pub fn check(&mut self) -> bool {
        match self.checker.check(&self.exprs, &self.decls) {
            Ok(_) => {}
            Err(err) => {
                println!(
                    "❌ {}:{}: {}",
                    err.location.file, err.location.line, err.message
                );
                return false;
            }
        }
        true
    }

    pub fn print_exprs(&self) {
        let mut i = 0;
        for expr in &self.exprs.exprs {
            println!("{}: {:?}, {:?}", i, expr, self.checker.types[i]);
            i += 1;
        }
    }
}
