#![allow(dead_code)]

mod defs;
use defs::*;

mod types;
use types::*;
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

impl Compiler {
    fn build_type(&mut self, pair: pest::iterators::Pair<Rule>) -> TypeID {
        match pair.as_rule() {
            Rule::ty => self.build_type(pair.into_inner().next().unwrap()),
            Rule::int8 => INT8,
            Rule::int32 => INT32,
            _ => TypeID{index: 0}
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn type_test(s: &str, t: TypeID) {
        let mut compiler = Compiler::new();
        let result = LyteParser::parse(Rule::ty, &s);
        let mut tested = false;
        for pairs in result {
            for p in pairs {
                tested = true;
                assert_eq!(compiler.build_type(p), t);
            }
        }
        assert!(tested);
    }

    #[test]
    pub fn test_parse_type() {
        LyteParser::parse(Rule::ty, &"i8").expect("parse");
        LyteParser::parse(Rule::ty, &"[i8]").expect("parse");
        LyteParser::parse(Rule::ty, &"[ i8 ]").expect("parse");
        LyteParser::parse(Rule::ty, &"MyType").expect("parse");
        LyteParser::parse(Rule::ty, &"⟨ T ⟩").expect("parse");
        LyteParser::parse(Rule::ty, &"_foo").expect("parse");

        type_test("i8", INT8);
        type_test("i32", INT32);
    }

    #[test]
    pub fn test_parse_atom() {
        LyteParser::parse(Rule::atom, &"x").expect("parse");
        LyteParser::parse(Rule::atom, &"(x)").expect("parse");
        assert!(match LyteParser::parse(Rule::atom, &"(x") {
            Ok(_) => false,
            Err(_) => true
        });
        LyteParser::parse(Rule::atom, &"{ }").expect("parse");
        assert!(match LyteParser::parse(Rule::atom, &"?") {
            Ok(_) => false,
            Err(_) => true
        });
    }

    #[test]
    pub fn test_parse_prefix() {
        LyteParser::parse(Rule::prefix, &"f(x)").expect("parse");
        LyteParser::parse(Rule::prefix, &"a[i]").expect("parse");
    }
}