

#[derive(Clone, Copy, Hash, Eq, PartialEq, Debug)]
pub struct TypeID {
    pub index: u32,
}

#[derive(Clone, Copy, Hash, Eq, PartialEq, Debug)]
pub enum Type {
    Void,
    Int8,
    Int32,
    Tuple(TypeID, TypeID),
    Var(u32),
    Func(TypeID, TypeID),
    Array(TypeID)
}

#[derive(Clone, Copy, Hash, Eq, PartialEq, Debug)]
pub struct ExprID {
    pub index: u32,
}

#[derive(Clone, Hash, Eq, PartialEq, Debug)]
pub struct Loc {
    pub file: String,
    pub line: u32,
}

#[derive(Clone, Copy, Debug)]
pub enum Expr {
    Id(u32),
    Int(i64),
    Real(f64),
    Call,
    Binop(ExprID, ExprID),
    Unop(ExprID),
    Lambda,
    String,
    Char,
    Subscript,
    Cast,
    Field,
    Array(ExprID, ExprID),
    New,
    True,
    False
}

pub struct Compiler {
    pub names: Vec<String>,
    pub types: Vec<Type>,
    pub typevar_names: Vec<String>,
    pub exprs: Vec<Expr>
}

impl Compiler {
    pub fn new() -> Compiler {
        Compiler {
            names: Vec::new(),
            types: vec![Type::Void, Type::Int8, Type::Int32],
            typevar_names: Vec::new(),
            exprs: Vec::new()
        }
    }
}