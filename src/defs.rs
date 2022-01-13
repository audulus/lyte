use internment::Intern;

pub type TypeID = Intern<Type>;

#[derive(Clone, Copy, Hash, Eq, PartialEq, Debug)]
pub enum Type {
    Void,
    Int8,
    UInt8,
    Int32,
    Tuple(TypeID, TypeID),
    Var(usize),
    Func(TypeID, TypeID),
    Array(TypeID),
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
    Times,
    Div,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Expr {
    Id(Intern<String>),
    Int(i64),
    // Real(f64),
    Call(Box<Expr>, Box<Expr>),
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
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Block {
    pub exprs: Vec<Expr>,
}

impl Block {
    pub fn new() -> Block {
        Block { exprs: vec![] }
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Decl {
    Func(Intern<String>, Block),
}

pub struct Compiler {
    pub names: Vec<String>,
}

impl Compiler {
    pub fn new() -> Compiler {
        Compiler { names: Vec::new() }
    }
}
