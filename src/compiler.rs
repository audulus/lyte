use crate::*;
use std::fs;
use std::path::Path;

pub struct Compiler {
    pub decls: Vec<Decl>,
    pub exprs: ExprArena,
}

impl Compiler {
    pub fn new() -> Self {
        Self { decls: vec![], exprs: ExprArena::new() }
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
}
