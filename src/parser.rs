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
