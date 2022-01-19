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

fn parse_expr(lexer: &mut Lexer) -> Result<Expr, ParseError> {
    let mut r = parse_eq(lexer)?;

    while lexer.tok == Token::Cond {
        lexer.next();
        let if_true = parse_expr(lexer)?;

        expect(lexer, Token::Colon)?;
        lexer.next();

        let if_false = parse_expr(lexer)?;
        r = binop(&Token::Cond, if_true, if_false);
    }

    Ok(r)
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
        let args = parse_exprlist(lexer)?;
        expect(lexer, Token::Rparen)?;
        lexer.next();
        Ok(Expr::Call(Box::new(lhs), args))
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
            let rr = parse_expr(lexer)?;
            expect(lexer, Token::Rparen)?;

            lexer.next();
            rr

        },
        _ => return Err(ParseError {
            location: lexer.i,
            message: String::from("Expected expression")
        })
    })
}

fn parse_exprlist(lexer: &mut Lexer) -> Result<Vec<Expr>, ParseError> {
    let mut r = vec![];
    
    loop {
        r.push(parse_expr(lexer)?);
        
        if lexer.tok != Token::Comma {
            break
        }

        lexer.next();
    }

    Ok(r)
}

fn parse_stmt(lexer: &mut Lexer) -> Result<Expr, ParseError> {
    match lexer.tok.clone() {
        Token::Id(name) => {
            lexer.next();
            let n = Intern::new(name);
            match lexer.tok {
                Token::Lparen => {
                    lexer.next();
                    let r = Ok(Expr::Call(Box::new(Expr::Id(n)), parse_exprlist(lexer)?));
                    expect(lexer, Token::Rparen)?;
                    lexer.next();
                    r
                },
                Token::Equal => {
                    lexer.next();
                    Ok(Expr::Assign(n, Box::new(parse_expr(lexer)?)))
                },
                _ => Err(ParseError {
                    location: lexer.i,
                    message: String::from("Expected assignment or function call")
                })
            }
        },
        Token::Var | Token::Let => {
            lexer.next();
            match &lexer.tok {
                Token::Id(name) => {
                    let n = Intern::new(name.clone());
                    lexer.next();
                    expect(lexer, Token::Equal)?;
                    lexer.next();
                    Ok(Expr::Var(n, Box::new(parse_expr(lexer)?)))
                },
                _ => Err(ParseError {
                    location: lexer.i,
                    message: String::from("Expected assignment or function call")
                })
            }
        }
        _ => Err(ParseError {
            location: lexer.i,
            message: String::from("Expected statement")
        })
    }
}

fn parse_block(lexer: &mut Lexer) -> Result<Vec<Expr>, ParseError> {
    let mut r = vec![];
    expect(lexer, Token::Lbrace)?;

    lexer.next();

    loop {

        r.push(parse_stmt(lexer)?);

        if lexer.tok == Token::Rbrace {
            break
        }
    }

    lexer.next();

    Ok(r)
}

#[cfg(test)]
mod tests {

    use super::*;

    fn type_parser(string: &str) -> TypeID {
        let mut lexer = Lexer::new(&String::from(string));
        lexer.next();
        parse_basic_type(&mut lexer).unwrap()
    }

    #[test]
    fn test_parse_type() {
        assert_eq!(type_parser("void"), mk_type(Type::Void));
        assert_eq!(type_parser("i8"), mk_type(Type::Int8));
        assert_eq!(type_parser("i32"), mk_type(Type::Int32));
        assert_eq!(type_parser("⟨T⟩"), typevar("T"));
        assert_eq!(
            type_parser("[i32]"),
            mk_type(Type::Array(mk_type(Type::Int32)))
        );
    }

    fn parse_fn<T>(string: &str, f: fn(&mut Lexer) -> Result<T, ParseError>) -> Result<T, ParseError> {
        let mut lexer = Lexer::new(&String::from(string));
        lexer.next();
        let r = f(&mut lexer)?;
        expect(&lexer, Token::End)?;
        Ok(r)
    }

    #[test]
    fn test_parse() {
        assert!(parse_fn("x", parse_atom).is_ok());
        assert!(parse_fn("(x)", parse_atom).is_ok());
        assert!(parse_fn("x*y", parse_term).is_ok());
        assert!(parse_fn("x+y", parse_sum).is_ok());
        assert!(parse_fn("x-y", parse_sum).is_ok());
        assert!(parse_fn("f(x)", parse_expr).is_ok());
        assert!(parse_fn("f(x, y)", parse_expr).is_ok());
        assert!(parse_fn("f(x) + g(x)", parse_expr).is_ok());
        assert!(parse_fn("x = y", parse_stmt).is_ok());
        assert!(parse_fn("f(x)", parse_stmt).is_ok());
        assert!(parse_fn("var x = y", parse_stmt).is_ok());
        assert!(parse_fn("let x = y", parse_stmt).is_ok());
        assert!(parse_fn("{ x = y }", parse_block).is_ok());
        assert!(parse_fn("{ f(x) }", parse_block).is_ok());
        assert!(parse_fn("{ x = y z = w }", parse_block).is_ok());
        assert!(parse_fn("{ f(x) g(y) }", parse_block).is_ok());
        assert!(parse_fn("{ var x = y var z = w }", parse_block).is_ok());
    }
}
