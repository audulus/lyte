use crate::types::TypeID;

#[derive(Clone, Hash, Eq, PartialEq, Debug)]
pub struct Loc {
    pub file: String,
    pub line: u32,
}

pub enum ExprEnum {
    Id(String),
    Int(i64),
    Real(f64),
    Call(Box<Expr>, Box<Expr>),
    Binop(Box<Expr>, Box<Expr>),
    Unop(Box<Expr>),
    Lambda,
    String(String),
    Char(u8),
    Subscript,
    Cast,
    Field,
    Array,
    New,
    True,
    False
}

pub struct Expr {
    pub expr: ExprEnum,
    pub loc: Loc,
    pub t: TypeID,
    pub lvalue: bool
}