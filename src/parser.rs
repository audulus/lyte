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

fn parse_rel(lexer: &mut Lexer) -> Result<Expr, ParseError> {
    let mut lhs = parse_sum(lexer)?;

    while lexer.tok == Token::Leq || lexer.tok == Token::Geq || lexer.tok == Token::Less ||
           lexer.tok == Token::Greater {
        let t = lexer.tok.clone();
        lexer.next();
        let rhs = parse_sum(lexer)?;

        let op = match t {
            Token::Leq => Binop::Leq,
            Token::Geq => Binop::Geq,
            Token::Less => Binop::Less,
            Token::Greater => Binop::Greater,
            _ => unreachable!()
        };

        lhs = Expr::Binop(op, Box::new(lhs), Box::new(rhs))
    }

    Ok(lhs)
}

fn parse_sum(lexer: &mut Lexer) -> Result<Expr, ParseError> {
    let mut lhs = parse_term(lexer)?;

    while lexer.tok == Token::Plus || lexer.tok == Token::Minus {
        let t = lexer.tok.clone();
        lexer.next();
        let rhs = parse_term(lexer)?;

        lhs = Expr::Binop(if t == Token::Plus { Binop::Plus } else { Binop::Minus },
                          Box::new(lhs), Box::new(rhs))
    }

    Ok(lhs)
}

fn parse_term(lexer: &mut Lexer) -> Result<Expr, ParseError> {
    let mut lhs = parse_exp(lexer)?;

    while lexer.tok == Token::Mult || lexer.tok == Token::Div {
        let t = lexer.tok.clone();
        lexer.next();
        let rhs = parse_exp(lexer)?;

        lhs = Expr::Binop(if t == Token::Mult { Binop::Times } else { Binop::Div},
                          Box::new(lhs), Box::new(rhs))
    }

    Ok(lhs)
}

fn parse_exp(lexer: &mut Lexer) -> Result<Expr, ParseError> {
    let mut lhs = parse_factor(lexer)?;
    
    while lexer.tok == Token::Power {
        lexer.next();

        let rhs = parse_factor(lexer)?;

        lhs = Expr::Binop(Binop::Pow, Box::new(lhs), Box::new(rhs))
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

    parse_atom(lexer)
}

fn parse_atom(lexer: &mut Lexer) -> Result<Expr, ParseError> {
    Ok(match &lexer.tok {
        Token::Id(id) => Expr::Id(Intern::new(id.clone())),
        _ => return Err(ParseError {
            location: lexer.i,
            message: String::from("Expected expression")
        })
    })
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
}
