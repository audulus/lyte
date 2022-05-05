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

use clap::Parser;
use std::fs;

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    file: String,
}

impl Compiler {
    pub fn new() -> Self {
        Self { names: vec![] }
    }
}

fn main() {
    println!("🎸");

    let args = Args::parse();

    if let Ok(string) = fs::read_to_string(args.file) {
        let mut lexer = Lexer::new(&String::from(string));
        lexer.next();
    } else {
        println!("error reading file");
    }
}
