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

use internment::Intern;

use pest::Parser;

#[derive(Parser)]
#[grammar = "lyte.pest"]
struct LyteParser;

fn main() {
    println!("yo")
}

fn mk_id(s: &str) -> Expr {
    Expr::Id(Intern::new(String::from(s)))
}

impl Compiler {
    fn build_type(&mut self, pair: pest::iterators::Pair<Rule>) -> TypeID {
        match pair.as_rule() {
            Rule::ty => self.build_type(pair.into_inner().next().unwrap()),
            Rule::int8 => mk_type(Type::Int8),
            Rule::int32 => mk_type(Type::Int32),
            Rule::array_type => {
                let id = self.build_type(pair.into_inner().next().unwrap());
                mk_type(Type::Array(id))
            },
            Rule::typevar => {
                mk_type(Type::Var(0)) // XXX: for now
            }
            _ => mk_type(Type::Void)
        }
    }

    fn build_expr(&mut self, pair: pest::iterators::Pair<Rule>) -> Expr {
        match pair.as_rule() {
            Rule::id => {
                let ix = Intern::new(String::from(pair.as_str()));
                Expr::Id(ix)
            },
            Rule::index => {
                let mut inner = pair.into_inner();
                let lhs = self.build_expr(inner.next().unwrap());
                let rhs = self.build_expr(inner.next().unwrap());
                Expr::Array(Box::new(lhs), Box::new(rhs))
            },
            Rule::field => {
                let mut inner = pair.into_inner();
                let lhs = self.build_expr(inner.next().unwrap());
                inner.next(); // skip the dot
                let ix = Intern::new(String::from(inner.next().unwrap().as_str()));
                Expr::Field(Box::new(lhs), ix)
            },
            Rule::suffix => {
                self.build_expr(pair.into_inner().next().unwrap())
            },
            Rule::prefix => {
                let mut inner = pair.into_inner();
                let e = self.build_expr(inner.next().unwrap());
                while let Some(pair) = inner.next() {
                    match pair.as_rule() {

                        _ => ()
                    }
                }
                e
            },
            Rule::atom => {
                self.build_expr(pair.into_inner().next().unwrap())
            },
            Rule::factor => {
                self.build_expr(pair.into_inner().next().unwrap())
            },
            Rule::term => {
                self.build_expr(pair.into_inner().next().unwrap())
            },
            Rule::expr => {
                self.build_expr(pair.into_inner().next().unwrap())
            },
            unknown_term => panic!("Unexpected term: {:?}", unknown_term),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn type_test(s: &str, t: TypeID, compiler: &mut Compiler) {
        
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

        let int8 = mk_type(Type::Int8);
        let int32 = mk_type(Type::Int32);

        let mut compiler = Compiler::new();

        type_test("i8", int8, &mut compiler);
        type_test("i32", int32, &mut compiler);

        let id = mk_type(Type::Array(int8));
        type_test("[i8]", id, &mut compiler);

        let id2 = mk_type(Type::Var(0));
        type_test("⟨T⟩", id2, &mut compiler);
        type_test("⟨ T ⟩", id2, &mut compiler);
    }

    fn expr_test(s: &str, e: Expr, compiler: &mut Compiler) {
        
        let result = LyteParser::parse(Rule::expr, &s);
        let mut tested = false;
        for pairs in result {
            for p in pairs {
                tested = true;
                assert_eq!(compiler.build_expr(p), e);
            }
        }
        assert!(tested);
    }

    fn suffix_test(s: &str, e: Expr, compiler: &mut Compiler) {
        
        let result = LyteParser::parse(Rule::suffix, &s);
        let mut tested = false;
        for pairs in result {
            for p in pairs {
                tested = true;
                assert_eq!(compiler.build_expr(p), e);
            }
        }
        assert!(tested);
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

        let mut compiler = Compiler::new();
        expr_test("x", Expr::Id(Intern::new(String::from("x"))), &mut compiler);
        
    }

    #[test]
    pub fn test_parse_array() {
        let mut compiler = Compiler::new();
        expr_test("x[y]", Expr::Array(Box::new(mk_id("x")), Box::new(mk_id("y"))), &mut compiler);
    }

    #[test]
    pub fn test_parse_field() {
        let mut compiler = Compiler::new();
        expr_test("x.y", Expr::Field(Box::new(mk_id("x")), Intern::new(String::from("y"))), &mut compiler);
    }

    #[test]
    pub fn test_parse_prefix() {
        LyteParser::parse(Rule::prefix, &"f(x)").expect("parse");
        LyteParser::parse(Rule::prefix, &"a[i]").expect("parse");
        LyteParser::parse(Rule::factor, &"a[i]").expect("parse");
        LyteParser::parse(Rule::term, &"a[i]").expect("parse");
        LyteParser::parse(Rule::expr, &"a[i]").expect("parse");
    }

    #[test]
    pub fn test_parse_factor() {
        let result = LyteParser::parse(Rule::factor, &"x+y");
        println!("{:?}", result);
    }

    #[test]
    pub fn test_parse_block() {
        // LyteParser::parse(Rule::block, &"{f()}").expect("parse");
    }

    #[test]
    pub fn test_parse_program() {
        let result = LyteParser::parse(Rule::program, &"f(x) { g(x) }");
        println!("{:?}", result);
    }
}