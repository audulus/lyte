#![allow(dead_code)]

mod defs;
//use defs::*;
mod types;
//use types::*;
mod typegraph;
// use typegraph::*;
mod lexer;
// use lexer::*;
mod nom_parser;

mod pest_parser;

#[macro_use]
extern crate lalrpop_util;

mod lalrpop_parser_tests;

extern crate pest;
#[macro_use]
extern crate pest_derive;

#[derive(Parser)]
#[grammar = "lyte.pest"]
struct LyteParser;

fn main() {
    println!("🎸")
}
