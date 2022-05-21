use crate::*;
use std::fs;
use std::path::Path;
use std::collections::HashMap;
use std::sync::Arc;

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
    fn input_file(&self, name: String) -> String;
}

#[salsa::query_group(ParserStorage)]
trait Parser {

    #[salsa::input]
    fn source(&self, key: ()) -> Arc<String>;

    fn ast(&self, key: ()) -> Tree;
}

fn ast(db: &dyn Parser, (): ()) -> Tree {
    // Read the input string:
    let input_string = db.source(());

    let mut tree = Tree::new();

    let mut lexer = Lexer::new(&input_string, "unknown path".into());

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

#[salsa::database(ParserStorage)]
pub struct Compiler {
    storage: salsa::Storage<Self>,
    pub trees: HashMap<String, Option<Tree>>,
    pub decls: Vec<Decl>,
    pub exprs: ExprArena,
    pub checker: Checker,
}

impl salsa::Database for Compiler {}

fn parse_file(path: &str) -> Option<Tree> {

    let mut tree = Tree::new();

    let path = Path::new(path);

    if let Ok(string) = fs::read_to_string(path) {
        println!("parsing file: {:?}", path);
        let mut lexer = Lexer::new(&string, path.to_str().unwrap());
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
                return None;
            }
        }
    } else {
        println!("error reading file: {:?}", path);
        return None;
    }

    Some(tree)
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            storage: salsa::Storage::default(),
            trees: HashMap::new(),
            decls: vec![],
            exprs: ExprArena::new(),
            checker: Checker::new(),
        }
    }

    /// Add a path to the program.
    pub fn add_path(&mut self, path: &str) {
        self.trees.entry(path.into()).or_insert(None);
    }

    /// Remove a file path from the program.
    pub fn remove_path(&mut self, path: &str) {
        self.trees.remove(path.into());
    }

    /// Let the compiler know that the contents of a file
    /// has changed.
    pub fn update_path(&mut self, path: &str) {
        *self.trees.entry(path.into()).or_insert(None) = None;
    }

    /// Returns all declarations in the program.
    pub fn get_decls(&mut self) -> Vec<Decl> {

        let decls = vec![];

        for (path, tree) in &mut self.trees {
            if tree.is_none() {
                *tree = parse_file(&path);
            }
        }

        decls
    }

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
