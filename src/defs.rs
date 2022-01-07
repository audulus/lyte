use internment::Intern;

pub type TypeID = Intern<Type>;

#[derive(Clone, Copy, Hash, Eq, PartialEq, Debug)]
pub enum Type {
    Void,
    Int8,
    Int32,
    Tuple(TypeID, TypeID),
    Var(usize),
    Func(TypeID, TypeID),
    Array(TypeID)
}

#[derive(Clone, Hash, Eq, PartialEq, Debug)]
pub struct Loc {
    pub file: String,
    pub line: u32,
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum Expr {
    Id(Intern<String>),
    Int(i64),
   // Real(f64),
    Call,
    Binop(Box<Expr>, Box<Expr>),
    Unop(Box<Expr>),
    Lambda,
    String,
    Char,
    Subscript,
    Cast,
    Field(Box<Expr>, Intern<String>),
    Array(Box<Expr>, Box<Expr>),
    New,
    True,
    False
}

pub struct Compiler {
    pub names: Vec<String>,
    pub types: Vec<Type>,
}

impl Compiler {
    pub fn new() -> Compiler {
        Compiler {
            names: Vec::new(),
            types: vec![Type::Void, Type::Int8, Type::Int32],
        }
    }
}