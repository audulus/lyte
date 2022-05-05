#![allow(dead_code)]

mod defs;
use defs::*;
mod types;
//use types::*;
mod typegraph;
// use typegraph::*;
mod lexer;
// use lexer::*;

mod parser;

impl Compiler {
    pub fn new() -> Self {
        Self { names: vec![] }
    }
}

fn main() {
    println!("🎸")
}
