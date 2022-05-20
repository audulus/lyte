use crate::*;
use std::fs;
use std::path::Path;
use std::collections::HashMap;

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

pub struct Compiler {
    pub trees: HashMap<String, Option<Tree>>,
    pub decls: Vec<Decl>,
    pub exprs: ExprArena,
    pub checker: Checker,
}

fn parse_file(path: &str, tree_ref: &mut Option<Tree>) {

    if tree_ref.is_none() {
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
                }
            }
        } else {
            println!("error reading file: {:?}", path);
        }

        *tree_ref = Some(tree)
    }
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            trees: HashMap::new(),
            decls: vec![],
            exprs: ExprArena::new(),
            checker: Checker::new(),
        }
    }

    pub fn add_path(&mut self, path: &str) {
        self.trees.entry(path.into()).or_insert(None);
    }

    pub fn remove_path(&mut self, path: &str) {
        self.trees.remove(path.into());
    }

    pub fn update_path(&mut self, path: &str) {
        *self.trees.entry(path.into()).or_insert(None) = None;
    }

    pub fn get_decls(&mut self) -> Vec<Decl> {

        let decls = vec![];

        for (path, tree) in &mut self.trees {
            parse_file(&path, tree);
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
