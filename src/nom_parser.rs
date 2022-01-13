extern crate nom;
use crate::defs::*;
use crate::types::*;

use nom::{
    branch::alt,
    bytes::complete::tag,
    character::complete::{alpha1, alphanumeric1},
    combinator::recognize,
    combinator::map_res,
    multi::many0,
    sequence::pair,
    IResult,
    sequence::delimited,
};

// https://github.com/Geal/nom/blob/main/doc/choosing_a_combinator.md

pub fn identifier(input: &str) -> IResult<&str, &str> {
    recognize(pair(
        alt((alpha1, tag("_"))),
        many0(alt((alphanumeric1, tag("_")))),
    ))(input)
}

fn ty_from_id(_input: &str) -> Result<TypeID, std::num::ParseIntError> {
    Ok(mk_type(Type::Int32))
}

pub fn typevar(input: &str) -> IResult<&str, TypeID> {
    map_res(
        delimited(tag("⟨"), identifier, tag("⟩")),
        ty_from_id
    )(input)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    pub fn test_parse_typevar() {
        assert!(typevar("⟨T⟩").is_ok());
        assert!(typevar("⟨T").is_err());
    }
}