use internment::Intern;

pub type TypeID = Intern<Type>;
pub type Name = Intern<String>;

#[derive(Clone, Copy, Hash, Eq, PartialEq, Debug)]
pub enum Type {
    Void,
    Int8,
    UInt8,
    Int32,
    Float32,
    Tuple(TypeID, TypeID),
    Var(Name),
    Func(TypeID, TypeID),
    Array(TypeID),
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
    Cond
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Expr {
    Id(Intern<String>),
    Int(i64),
    // Real(f64),
    Call(Box<Expr>, Vec<Expr>),
    Binop(Binop, Box<Expr>, Box<Expr>),
    Unop(Box<Expr>),
    Lambda,
    String,
    Char,
    Subscript(Box<Expr>, Box<Expr>),
    Cast,
    Field(Box<Expr>, Intern<String>),
    Array(Box<Expr>, Box<Expr>),
    New,
    True,
    False,
    AsTy(Box<Expr>, TypeID),
    Block(Vec<Expr>),
    Assign(Name, Box<Expr>),
    Let(Name, Box<Expr>),
    Var(Name, Box<Expr>)
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Block {
    pub exprs: Vec<Expr>,
}

impl Block {
    pub fn new() -> Self {
        Self { exprs: vec![] }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Decl {
    Func{ name: Intern<String>, params: Vec<Expr>, body: Vec<Expr>},
}

pub struct Compiler {
    pub names: Vec<String>,
}

impl Compiler {
    pub fn new() -> Self {
        Self { names: vec![] }
    }
}
