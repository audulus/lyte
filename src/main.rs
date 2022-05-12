#![allow(dead_code)]

mod defs;
use defs::*;
mod types;
use types::*;

mod typegraph;
use typegraph::*;

mod lexer;
use lexer::*;

mod parser;
use parser::*;

mod checker;
mod compiler;

use clap::Parser;
use std::fs;
use std::path::Path;

#[derive(Parser, Debug)]
#[clap(author, version, about, long_about = None)]
struct Args {
    file: String,
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
