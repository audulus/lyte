use internment::Intern;

pub type TypeID = Intern<Type>;
pub type Name = Intern<String>;

#[derive(Clone, Copy, Hash, Eq, PartialEq, Debug)]
pub enum Type {
    Void,
    Bool,
    Int8,
    UInt8,
    Int32,
    Float32,
    Tuple(TypeID, TypeID),
    Var(Name),
    Func(TypeID, TypeID),
    Array(TypeID, i64),
    Name(Name),
}

#[derive(Clone, Hash, Eq, PartialEq, Debug)]
pub struct Loc {
    pub file: String,
    pub line: u32,
}

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Binop {
    Plus,
    Minus,
    Mult,
    Div,
    Pow,
    Equal,
    NotEqual,
    Less,
    Greater,
    Leq,
    Geq,
    Cond,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Id(Intern<String>),
    Int(i64),
    Real(f64),
    Call(Box<Expr>, Vec<Expr>),
    Binop(Binop, Box<Expr>, Box<Expr>),
    Unop(Box<Expr>),
    Lambda { params: Vec<Param>, body: Box<Expr> },
    String(String),
    Char,
    Subscript(Box<Expr>, Box<Expr>),
    Cast,
    Field(Box<Expr>, Intern<String>),
    Array(Box<Expr>, Box<Expr>),
    New,
    True,
    False,
    AsTy(Box<Expr>, TypeID),
    Assign(Name, Box<Expr>),
    Let(Name, Box<Expr>),
    Var(Name, Box<Expr>),
    If(Box<Expr>, Block, Option<Block>),
    Block(Block),
    Return(Box<Expr>),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Param {
    pub name: String,
    pub ty: TypeID,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Block {
    pub exprs: Vec<Expr>,
}

impl Block {
    pub fn new(exprs: Vec<Expr>) -> Self {
        Self { exprs }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Field {
    pub name: Name,
    pub ty: TypeID,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Decl {
    Func {
        name: Intern<String>,
        params: Vec<Param>,
        body: Block,
    },
    Struct {
        name: Name,
        fields: Vec<Field>
    },
    Enum {
        name: Name,
        cases: Vec<Name>
    }
}

pub struct Compiler {
}
