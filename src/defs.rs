use internment::Intern;

pub type TypeID = Intern<Type>;
pub type Name = Intern<String>;

#[derive(Clone, Hash, Eq, PartialEq, Debug)]
pub enum Type {
    Void,
    Bool,
    Int8,
    UInt8,
    Int32,
    Float32,
    Tuple(TypeID, TypeID),
    Var(Name, usize),
    Func(TypeID, TypeID),
    Array(TypeID, i64),
    Name(Name),
}

#[derive(Clone, Copy, Hash, Eq, PartialEq, Debug)]
pub struct Loc {
    pub file: Name,
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

pub type ExprID = usize;

#[derive(Clone, Debug, PartialEq)]
pub enum Expr {
    Id(Intern<String>),
    Int(i64),
    Real(f64),
    Call(ExprID, Vec<ExprID>),
    Binop(Binop, ExprID, ExprID),
    Unop(ExprID),
    Lambda { params: Vec<Param>, body: ExprID },
    String(String),
    Char,
    Subscript(ExprID, ExprID),
    Cast,
    Field(ExprID, Intern<String>),
    Array(ExprID, ExprID),
    True,
    False,
    AsTy(ExprID, TypeID),
    Assign(Name, ExprID),
    Let(Name, ExprID),
    Var(Name, ExprID),
    If(ExprID, ExprID, Option<ExprID>),
    Block(Vec<ExprID>),
    Return(ExprID),
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Param {
    pub name: String,
    pub ty: TypeID,
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
        body: ExprID,
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
    pub decls: Vec<Decl>
}
