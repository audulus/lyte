use crate::defs::*;
use crate::lexer::*;
use crate::types::*;
use internment::Intern;

#[derive(Clone, Debug)]
struct ParseError {
    location: usize,
    message: String,
}

fn expect(lexer: &Lexer, tok: Token) -> Result<(), ParseError> {
    if lexer.tok == tok {
        Ok(())
    } else {
        let message = format!("expected {:?}, got {:?}", tok, lexer.tok);
        println!("{:?}", message);
        Err(ParseError {
            location: lexer.i,
            message: message,
        })
    }
}

fn parse_basic_type(lexer: &mut Lexer) -> Result<TypeID, ParseError> {
    Ok(mk_type(match &lexer.tok {
        Token::Void => Type::Void,
        Token::Bool => Type::Bool,
        Token::Int8 => Type::Int8,
        Token::Int32 => Type::Int32,
        Token::Lmath => {
            lexer.next();
            if let Token::Id(name) = lexer.tok.clone() {
                lexer.next();

                expect(lexer, Token::Rmath)?;

                return Ok(typevar(&name));
            } else {
                return Err(ParseError {
                    location: lexer.i,
                    message: String::from("Expected identifier"),
                });
            }
        }
        Token::Lbracket => {
            lexer.next();
            let r = parse_basic_type(lexer)?;
            lexer.next();
            expect(lexer, Token::Rbracket)?;
            Type::Array(r)
        }
        Token::Id(name) => Type::Name(Intern::new(name.clone())),
        _ => return Err(ParseError {
            location: lexer.i,
            message: String::from("Expected type")
        })
    }))
}

fn parse_paramlist(lexer: &mut Lexer) -> Result<Vec<Param>, ParseError> {
    let mut r = vec![];
    
    loop {
        if let Token::Id(name) = &lexer.tok {
            let name = name.clone();
            lexer.next();
            expect(lexer, Token::Colon)?;
            lexer.next();
            let ty = parse_basic_type(lexer)?;
            lexer.next();
            r.push(Param{ name, ty })
        }
        
        if lexer.tok != Token::Comma {
            break
        }

        lexer.next();
    }

    Ok(r)
}

fn binop(tok: &Token, lhs: Expr, rhs: Expr) -> Expr {

    let op = match tok {
        Token::Plus => Binop::Plus,
        Token::Minus => Binop::Minus,
        Token::Mult => Binop::Mult,
        Token::Div => Binop::Div,
        Token::Leq => Binop::Leq,
        Token::Geq => Binop::Geq,
        Token::Less => Binop::Less,
        Token::Greater => Binop::Greater,
        Token::Equal => Binop::Equal,
        Token::NotEqual => Binop::NotEqual,
        Token::Power => Binop::Pow,
        _ => {
            println!("got {:?}", tok);
            unreachable!()
        }
    };

    Expr::Binop(op, Box::new(lhs), Box::new(rhs))
}

fn parse_lambda(lexer: &mut Lexer) -> Result<Expr, ParseError> {

    if lexer.tok == Token::Pipe {
        lexer.next();
        let params = parse_paramlist(lexer)?;
        expect(lexer, Token::Pipe)?;
        lexer.next();

        let body = Box::new(parse_lambda(lexer)?);

        Ok(Expr::Lambda{ params, body })
    } else {
        parse_expr(lexer)
    }
}

fn parse_expr(lexer: &mut Lexer) -> Result<Expr, ParseError> {

    if lexer.tok == Token::If {
        lexer.next();
        let cond = parse_expr(lexer)?;
        let then = parse_block(lexer)?;

        let els = if lexer.tok == Token::Else {
            lexer.next();
            Some(parse_block(lexer)?)
        } else {
            None
        };

        Ok(Expr::If(Box::new(cond), then, els))
    } else {
        parse_eq(lexer)
    }
}

fn parse_eq(lexer: &mut Lexer) -> Result<Expr, ParseError> {
    let mut lhs = parse_rel(lexer)?;

    while lexer.tok == Token::Equal || lexer.tok == Token::NotEqual {
        let t = lexer.tok.clone();
        lexer.next();
        let rhs = parse_rel(lexer)?;

        lhs = binop(&t, lhs, rhs)
    }

    Ok(lhs)
}

fn parse_rel(lexer: &mut Lexer) -> Result<Expr, ParseError> {
    let mut lhs = parse_sum(lexer)?;

    while lexer.tok == Token::Leq || lexer.tok == Token::Geq || lexer.tok == Token::Less ||
           lexer.tok == Token::Greater {
        let t = lexer.tok.clone();
        lexer.next();
        let rhs = parse_sum(lexer)?;

        lhs = binop(&t, lhs, rhs)
    }

    Ok(lhs)
}

fn parse_sum(lexer: &mut Lexer) -> Result<Expr, ParseError> {
    let mut lhs = parse_term(lexer)?;

    while lexer.tok == Token::Plus || lexer.tok == Token::Minus {
        let t = lexer.tok.clone();
        lexer.next();
        let rhs = parse_term(lexer)?;

        lhs = binop(&t, lhs, rhs)
    }

    Ok(lhs)
}

fn parse_term(lexer: &mut Lexer) -> Result<Expr, ParseError> {
    let mut lhs = parse_exp(lexer)?;

    while lexer.tok == Token::Mult || lexer.tok == Token::Div {
        let t = lexer.tok.clone();
        lexer.next();
        let rhs = parse_exp(lexer)?;

        lhs = binop(&t, lhs, rhs)
    }

    Ok(lhs)
}

fn parse_exp(lexer: &mut Lexer) -> Result<Expr, ParseError> {
    let mut lhs = parse_factor(lexer)?;
    
    while lexer.tok == Token::Power {
        lexer.next();

        let rhs = parse_factor(lexer)?;

        lhs = binop(&Token::Power, lhs, rhs)
    }

    Ok(lhs)
}

fn parse_factor(lexer: &mut Lexer) -> Result<Expr, ParseError> {
    if lexer.tok == Token::Minus {
        lexer.next();
        return Ok(Expr::Unop(Box::new(parse_atom(lexer)?)));
    }

    if lexer.tok == Token::Plus {
        lexer.next();
        return parse_atom(lexer);
    }

    parse_postfix(lexer)
}

fn parse_postfix(lexer: &mut Lexer) -> Result<Expr, ParseError> {
    let lhs = parse_atom(lexer)?;

    if lexer.tok == Token::Lparen {
        lexer.next();
        let args = if lexer.tok != Token::Rparen {
            let args = parse_exprlist(lexer)?;
            expect(lexer, Token::Rparen)?;
            lexer.next();
            args
        } else {
            lexer.next();
            vec![]
        };
        Ok(Expr::Call(Box::new(lhs), args))
    } else if lexer.tok == Token::Colon {
        lexer.next();
        let t = parse_basic_type(lexer)?;
        lexer.next();
        Ok(Expr::AsTy(Box::new(lhs), t))
    } else {
        Ok(lhs)
    }
}

fn parse_atom(lexer: &mut Lexer) -> Result<Expr, ParseError> {
    Ok(match &lexer.tok {
        Token::Id(id) => {
            let e = Expr::Id(Intern::new(id.clone()));
            lexer.next();
            e
        }
        Token::Lparen => {

            lexer.next();
            let rr = parse_lambda(lexer)?;
            expect(lexer, Token::Rparen)?;

            lexer.next();
            rr

        }
        Token::Lbrace => {
            Expr::Block(parse_block(lexer)?)
        }
        _ => return Err(ParseError {
            location: lexer.i,
            message: String::from("Expected expression")
        })
    })
}

fn parse_exprlist(lexer: &mut Lexer) -> Result<Vec<Expr>, ParseError> {
    let mut r = vec![];
    
    loop {
        r.push(parse_lambda(lexer)?);
        
        if lexer.tok != Token::Comma {
            break
        }

        lexer.next();
    }

    Ok(r)
}

fn parse_stmt(lexer: &mut Lexer) -> Result<Expr, ParseError> {
    match lexer.tok.clone() {
        // Token::Id(name) => {
        //     lexer.next();
        //     let n = Intern::new(name);
        //     match lexer.tok {
        //         Token::Lparen => {
        //             lexer.next();
        //             let r = Ok(Expr::Call(Box::new(Expr::Id(n)), parse_exprlist(lexer)?));
        //             expect(lexer, Token::Rparen)?;
        //             lexer.next();
        //             r
        //         },
        //         Token::Equal => {
        //             lexer.next();
        //             Ok(Expr::Assign(n, Box::new(parse_lambda(lexer)?)))
        //         },
        //         _ => Err(ParseError {
        //             location: lexer.i,
        //             message: String::from("Expected assignment or function call")
        //         })
        //     }
        // },
        Token::Var | Token::Let => {
            lexer.next();
            match &lexer.tok {
                Token::Id(name) => {
                    let n = Intern::new(name.clone());
                    lexer.next();
                    expect(lexer, Token::Equal)?;
                    lexer.next();
                    Ok(Expr::Var(n, Box::new(parse_lambda(lexer)?)))
                },
                _ => Err(ParseError {
                    location: lexer.i,
                    message: String::from("Expected assignment or function call")
                })
            }
        }
        _ => parse_expr(lexer)
    }
}

fn parse_block(lexer: &mut Lexer) -> Result<Block, ParseError> {
    let mut r = vec![];
    expect(lexer, Token::Lbrace)?;

    lexer.next();

    loop {
        if lexer.tok == Token::Rbrace {
            break
        }

        r.push(parse_stmt(lexer)?);
    }

    lexer.next();

    Ok(Block::new(r))
}

fn parse_decl(lexer: &mut Lexer) -> Result<Decl, ParseError> {
    match lexer.tok.clone() {
        Token::Id(name) => {
            // Function declaration.
            lexer.next();
            expect(lexer, Token::Lparen)?;
            lexer.next();
            let params = parse_paramlist(lexer)?;
            expect(lexer, Token::Rparen)?;
            lexer.next();

            match lexer.tok.clone() {
                Token::Lbrace => {
                    Ok(Decl::Func{name: Intern::new(name),
                        params: params,
                        body: parse_block(lexer)?})
                },
                Token::Arrow => {
                    lexer.next();
                    let _t = parse_basic_type(lexer)?;
                    lexer.next();
                    Ok(Decl::Func{name: Intern::new(name),
                        params: params,
                        body: parse_block(lexer)?})
                },
                _ => Err(ParseError {
                    location: lexer.i,
                    message: String::from("Expected return type or function body")
                })
            }
            
        }
        _ => Err(ParseError {
            location: lexer.i,
            message: String::from("Expected declaration")
        })
    }
}

fn parse_program(lexer: &mut Lexer) -> Result<Vec<Decl>, ParseError> {

    let mut decls = vec![];

    while lexer.tok != Token::End {
        decls.push(parse_decl(lexer)?)
    }

    Ok(decls)
}

#[cfg(test)]
mod tests {

    use super::*;

    fn type_parser(string: &str) -> TypeID {
        let mut lexer = Lexer::new(&String::from(string));
        lexer.next();
        parse_basic_type(&mut lexer).unwrap()
    }

    fn test_type(string: &str, ty: TypeID) {
        assert_eq!(type_parser(string), ty);
    }

    #[test]
    fn test_parse_type() {
        test_type("void", mk_type(Type::Void));
        test_type("i8", mk_type(Type::Int8));
        test_type("i32", mk_type(Type::Int32));
        test_type("⟨T⟩", typevar("T"));
        test_type("[i32]", mk_type(Type::Array(mk_type(Type::Int32))));
    }

    fn parse_fn<T: std::fmt::Debug>(string: &str, f: fn(&mut Lexer) -> Result<T, ParseError>) -> Result<T, ParseError> {
        let mut lexer = Lexer::new(&String::from(string));
        lexer.next();
        let r = f(&mut lexer)?;
        println!("{} ==> {:?}", string, r);
        expect(&lexer, Token::End)?;
        Ok(r)
    }

    fn test<T: std::fmt::Debug>(string: &str, f: fn(&mut Lexer) -> Result<T, ParseError>) {
        assert!(parse_fn(string, f).is_ok());
    }

    #[test]
    fn test_parse() {
        test("x", parse_atom);
        test("(x)", parse_atom);
        test("x*y", parse_term);
        test("x+y", parse_sum);
        test("x-y", parse_sum);
        test("f()", parse_expr);
        test("f(x)", parse_expr);
        test("f(x, y)", parse_expr);
        test("f(x) + g(x)", parse_expr);
        test("|| x", parse_lambda);
        test("|x: i8| x", parse_lambda);
        test("x = y", parse_stmt);
        test("f(x)", parse_stmt);
        test("var x = y", parse_stmt);
        test("let x = y", parse_stmt);
        test("let x = || x", parse_stmt);
        test("let x = if x { a } else { b }", parse_stmt);
        test("let x = if x { a+b } else { b }", parse_stmt);
        test("if x { }", parse_stmt);
        test("if x { } else { }", parse_stmt);
        test("x", parse_stmt);
        test("{ x }", parse_stmt);
        test("{ }", parse_block);
        test("{ x x }", parse_block);
        test("{ x = y }", parse_block);
        test("{ f(x) }", parse_block);
        test("{ x = y z = w }", parse_block);
        test("{ f(x) g(y) }", parse_block);
        test("{ var x = y var z = w }", parse_block);
        test("f(){}", parse_decl);
        test("f(x: i8) { g(x) }", parse_decl);
        test("f(x: i8) -> i8 { g(x) }", parse_decl);
        test("f(x: i8, y: i8) { g(x) }", parse_decl);
        test("", parse_program);
        test("f(){} g(){}", parse_program);
    }
}
