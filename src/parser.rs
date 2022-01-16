
use crate::defs::*;
use crate::lexer::*;
use crate::types::*;

fn parse_basic_type(lexer: &mut Lexer) -> TypeID {

    mk_type(match lexer.tok {
        Token::Void => Type::Void,
        Token::Int8 => Type::Int8,
        Token::Int32 => Type::Int32,
        Token::Lmath => {

            Type::Var(0)
        },
        _ => unreachable!()
    })
}

#[cfg(test)]
mod tests {

    use super::*;

    fn type_parser(string: &str) -> TypeID {
        let mut lexer = Lexer::new(&String::from(string));
        lexer.next();
        parse_basic_type(&mut lexer)
    }

    #[test]
    fn test_parse_type() {
        assert_eq!(type_parser("void"), mk_type(Type::Void));
        assert_eq!(type_parser("i8"), mk_type(Type::Int8));
    }

}