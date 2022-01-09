#![allow(dead_code)]

mod defs;
use defs::*;

mod types;
use types::*;
mod typegraph;
// use typegraph::*;

mod pest_parser;

extern crate pest;
#[macro_use]
extern crate pest_derive;

use internment::Intern;

use pest::Parser;

#[derive(Parser)]
#[grammar = "lyte.pest"]
struct LyteParser;

fn main() {
    println!("🎸")
}

