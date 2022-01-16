
use crate::defs::*;
use crate::lexer::*;
use crate::types::*;

#[derive(Clone, Debug)]
struct ParseError {
    location: usize,
    message: String
}

fn expect(lexer: &Lexer, tok: Token) -> Result<(), ParseError> {
    if lexer.tok == tok { Ok(()) } else {
        let message = format!("expected {:?}, got {:?}", tok, lexer.tok);
        println!("{:?}", message);
        Err(ParseError{location: lexer.i, message: message})
    }
}

fn parse_basic_type(lexer: &mut Lexer) -> Result<TypeID, ParseError> {

    Ok(mk_type(match lexer.tok {
        Token::Void => Type::Void,
        Token::Int8 => Type::Int8,
        Token::Int32 => Type::Int32,
        Token::Lmath => {

            lexer.next();

            lexer.next();

            expect(lexer, Token::Rmath)?;
            Type::Var(0)
        },
        _ => unreachable!()
    }))
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
    }

}