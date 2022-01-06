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
            Rule::array_type => {
                let id = self.build_type(pair.into_inner().next().unwrap());
                self.mk_type(Type::Array(id))
            },
            Rule::typevar => {
                self.typevar(pair.into_inner().as_str())
            }
            _ => TypeID{index: 0}
        }
    }

    fn mk_expr(&mut self, expr: Expr) -> ExprID {
        let ix = self.exprs.len();
        self.exprs.push(expr);
        return ExprID{index: ix as u32};
    }

    fn build_expr(&mut self, pair: pest::iterators::Pair<Rule>) -> ExprID {
        match pair.as_rule() {
            Rule::id => {
                let ix = self.mk_str(pair.as_str());
                self.mk_expr(Expr::Id(ix))
            },
            Rule::index => {
                let mut inner = pair.into_inner();
                let lhs = self.build_expr(inner.next().unwrap());
                let rhs = self.build_expr(inner.next().unwrap());
                self.mk_expr(Expr::Array(lhs, rhs))
            },
            Rule::field => {
                let mut inner = pair.into_inner();
                inner.next(); // skip the dot
                let ix = self.mk_str(inner.next().unwrap().as_str());
                self.mk_expr(Expr::Field(ix))
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

        let mut compiler = Compiler::new();

        type_test("i8", INT8, &mut compiler);
        type_test("i32", INT32, &mut compiler);

        let id = compiler.mk_type(Type::Array(INT8));
        type_test("[i8]", id, &mut compiler);

        let id2 = compiler.mk_type(Type::Var(0));
        type_test("⟨T⟩", id2, &mut compiler);
        assert_eq!(compiler.typevar_names[0], "T");

        type_test("⟨ T ⟩", id2, &mut compiler);
    }

    fn expr_test(s: &str, e: ExprID, compiler: &mut Compiler) {
        
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

    fn suffix_test(s: &str, e: ExprID, compiler: &mut Compiler) {
        
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
        expr_test("x", ExprID { index: 0 }, &mut compiler);
        
    }

    #[test]
    pub fn test_parse_array() {
        let mut compiler = Compiler::new();
        expr_test("x[y]", ExprID{index: 0}, &mut compiler);
    }

    #[test]
    pub fn test_parse_field() {
        let mut compiler = Compiler::new();
        expr_test("x.y", ExprID{index: 0}, &mut compiler);
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
        let result = LyteParser::parse(Rule::program, &"f() {}");
        println!("{:?}", result);
    }
}