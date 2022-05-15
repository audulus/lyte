use crate::*;
use std::fs;
use std::path::Path;

pub struct Compiler {
    pub decls: Vec<Decl>,
    pub exprs: ExprArena,
    pub checker: Checker,
}

impl Compiler {
    pub fn new() -> Self {
        Self {
            decls: vec![],
            exprs: ExprArena::new(),
            checker: Checker::new()
        }
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
                    "{}:{}: {}",
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
