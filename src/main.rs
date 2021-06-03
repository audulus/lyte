#![allow(dead_code)]

mod types;
//use types::*;
mod ast;
// use ast::*;
mod typegraph;
// use typegraph::*;

extern crate pest;
#[macro_use]
extern crate pest_derive;

use pest::Parser;

#[derive(Parser)]
#[grammar = "lyte.pest"]
struct LyteParser;

fn main() {
    println!("yo")
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn test_parse_type() {
        LyteParser::parse(Rule::lyte_type, &"i8").expect("parse");
        LyteParser::parse(Rule::lyte_type, &"[i8]").expect("parse");
        LyteParser::parse(Rule::lyte_type, &"[ i8 ]").expect("parse");
        LyteParser::parse(Rule::lyte_type, &"MyType").expect("parse");
        LyteParser::parse(Rule::lyte_type, &"⟨ T ⟩").expect("parse");
    }
}