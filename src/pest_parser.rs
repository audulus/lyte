#![allow(dead_code)]

use crate::defs::*;
use crate::types::*;

use internment::Intern;

use pest::Parser;

#[derive(Parser)]
#[grammar = "lyte.pest"]
struct LyteParser;

fn mk_id(s: &str) -> Expr {
    Expr::Id(Intern::new(String::from(s)))
}

fn build_type(pair: pest::iterators::Pair<Rule>) -> TypeID {
    match pair.as_rule() {
        Rule::ty => build_type(pair.into_inner().next().unwrap()),
        Rule::int8 => mk_type(Type::Int8),
        Rule::int32 => mk_type(Type::Int32),
        Rule::array_type => {
            let id = build_type(pair.into_inner().next().unwrap());
            mk_type(Type::Array(id))
        }
        Rule::typevar => {
            typevar("T") // XXX: for now
        }
        _ => unreachable!(),
    }
}

fn build_expr(pair: pest::iterators::Pair<Rule>) -> Expr {
    match pair.as_rule() {
        Rule::id => {
            let ix = Intern::new(String::from(pair.as_str()));
            Expr::Id(ix)
        }
        Rule::prefix => {
            let mut inner = pair.into_inner();
            let mut e = build_expr(inner.next().unwrap());
            while let Some(pair) = inner.next() {
                match pair.as_rule() {
                    Rule::lparen => {
                        e = Expr::Call(Box::new(e), vec![])
                    }
                    Rule::lbracket => {
                        e = Expr::Subscript(
                            Box::new(e),
                            Box::new(build_expr(inner.next().unwrap())),
                        )
                    }
                    Rule::dot => {
                        let field = Intern::new(String::from(inner.next().unwrap().as_str()));
                        e = Expr::Field(Box::new(e), field);
                    }
                    Rule::colon => {
                        let ty = build_type(inner.next().unwrap());
                        e = Expr::AsTy(Box::new(e), ty);
                    }
                    _ => (),
                }
            }
            e
        }
        Rule::atom => build_expr(pair.into_inner().next().unwrap()),
        Rule::factor => {
            let mut inner = pair.into_inner();
            let mut e = build_expr(inner.next().unwrap());
            while let Some(pair) = inner.next() {
                match pair.as_rule() {
                    Rule::times => {
                        e = Expr::Binop(
                            Binop::Times,
                            Box::new(e),
                            Box::new(build_expr(inner.next().unwrap())),
                        )
                    }
                    Rule::div => {
                        e = Expr::Binop(
                            Binop::Div,
                            Box::new(e),
                            Box::new(build_expr(inner.next().unwrap())),
                        )
                    }
                    _ => (),
                }
            }
            e
        }
        Rule::term => {
            let mut inner = pair.into_inner();
            let mut e = build_expr(inner.next().unwrap());
            while let Some(pair) = inner.next() {
                match pair.as_rule() {
                    Rule::plus => {
                        e = Expr::Binop(
                            Binop::Plus,
                            Box::new(e),
                            Box::new(build_expr(inner.next().unwrap())),
                        )
                    }
                    Rule::minus => {
                        e = Expr::Binop(
                            Binop::Minus,
                            Box::new(e),
                            Box::new(build_expr(inner.next().unwrap())),
                        )
                    }
                    _ => (),
                }
            }
            e
        }
        unknown_term => panic!("Unexpected term: {:?}", unknown_term),
    }
}

fn build_block(pair: pest::iterators::Pair<Rule>) -> Block {
    let mut block = Block::new();
    let mut inner = pair.into_inner();
    while let Some(pair) = inner.next() {
        match pair.as_rule() {
            Rule::expr => {
                block.exprs.push(build_expr(pair));
            }
            _ => unreachable!(),
        }
    }

    block
}

fn build_fndecl(pair: pest::iterators::Pair<Rule>) -> Decl {
    let mut inner = pair.into_inner();

    let name = inner.next().unwrap().as_str();
    let block = match inner.next().unwrap().as_rule() {
        Rule::block => build_block(inner.next().unwrap()),
        _ => unreachable!(),
    };

    Decl::Func(Intern::new(String::from(name)), block)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn type_test(s: &str, t: TypeID) {
        let result = LyteParser::parse(Rule::ty, &s);
        let mut tested = false;
        for pairs in result {
            for p in pairs {
                tested = true;
                assert_eq!(build_type(p), t);
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

        type_test("i8", int8);
        type_test("i32", int32);

        let id = mk_type(Type::Array(int8));
        type_test("[i8]", id);

        let id2 = typevar("T");
        type_test("⟨T⟩", id2);
        type_test("⟨ T ⟩", id2);
    }

    fn expr_test(s: &str, e: Expr) {
        let result = LyteParser::parse(Rule::expr, &s);
        let mut tested = false;
        for pairs in result {
            for p in pairs {
                tested = true;
                assert_eq!(build_expr(p), e);
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
            Err(_) => true,
        });
        LyteParser::parse(Rule::atom, &"{ }").expect("parse");
        assert!(match LyteParser::parse(Rule::atom, &"?") {
            Ok(_) => false,
            Err(_) => true,
        });

        expr_test("x", Expr::Id(Intern::new(String::from("x"))));
    }

    #[test]
    pub fn test_parse_subscript() {
        expr_test(
            "x[y]",
            Expr::Subscript(Box::new(mk_id("x")), Box::new(mk_id("y"))),
        );
    }

    #[test]
    pub fn test_parse_field() {
        expr_test(
            "x.y",
            Expr::Field(Box::new(mk_id("x")), Intern::new(String::from("y"))),
        );
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
        println!("{:#?}", result);

        let result2 = LyteParser::parse(Rule::program, &"f() { g(x) }");
        println!("{:#?}", result2);

        let result3 = LyteParser::parse(Rule::program, &"f() -> t { }");
        println!("{:#?}", result3);
    }
}
