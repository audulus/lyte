
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