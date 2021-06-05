
#[derive(Clone, Copy, Hash, Eq, PartialEq, Debug)]
pub struct ExprID {
    index: u32,
}

#[derive(Clone, Hash, Eq, PartialEq, Debug)]
pub struct Loc {
    pub file: String,
    pub line: u32,
}

pub enum ExprKind {
    Id,
    Int,
    Real,
    Call,
    Binop,
    Unop,
    Lambda,
    String,
    Char,
    Subscript,
    Cast,
    Field,
    Array,
    New,
    True,
    False
}

