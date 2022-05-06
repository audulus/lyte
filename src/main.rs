#![allow(dead_code)]

mod defs;
use defs::*;
mod types;
//use types::*;
mod typegraph;
// use typegraph::*;
mod lexer;
use lexer::*;

mod parser;
use parser::*;

use clap::Parser;
use std::fs;
use std::path::Path;

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    file: String,
}

impl Compiler {
    pub fn new() -> Self {
        Self { decls: vec![] }
    }

    pub fn parse_file(&mut self, path: &Path) {
        if let Ok(string) = fs::read_to_string(path) {
            println!("parsing file: {:?}", path);
            let mut lexer = Lexer::new(&string);
            lexer.next();
            let mut arena = ExprArena::new();
            match parse_program(&mut lexer, &mut arena) {
                Ok(decls) => {
                    println!("parsed {:?}", decls);

                    self.decls.extend(decls);
                }
                Err(err) => {
                    println!("parse error: {:?}", err);
                }
            }
        } else {
            println!("error reading file: {:?}", path);
        }
    }
}

fn main() {
    println!("🎸");

    let args = Args::parse();
    let mut compiler = Compiler::new();

    if let Ok(paths) = fs::read_dir(args.file.clone()) {
        for path in paths {
            compiler.parse_file(&path.unwrap().path());
        }
    } else {
        let file = args.file;
        compiler.parse_file(Path::new(&file));
    }
}
