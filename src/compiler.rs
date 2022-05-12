use crate::*;
use std::fs;
use std::path::Path;

pub struct Compiler {
    pub decls: Vec<Decl>,
}

impl Compiler {
    pub fn new() -> Self {
        Self { decls: vec![] }
    }

    pub fn parse_file(&mut self, path: &Path) {
        if let Ok(string) = fs::read_to_string(path) {
            println!("parsing file: {:?}", path);
            let mut lexer = Lexer::new(&string, path.to_str().unwrap());
            lexer.next();
            let mut arena = ExprArena::new();
            match parse_program(&mut lexer, &mut arena) {
                Ok(decls) => {
                    println!("parsed {:?}, exprs: {:?}", decls, arena);

                    self.decls.extend(decls);
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
    }
}
