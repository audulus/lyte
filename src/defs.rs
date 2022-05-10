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
    Tuple(Vec<TypeID>),
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

pub fn test_loc() -> Loc {
    Loc {
        file: Name::new("".into()),
        line: 0,
    }
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
    Enum(Name),
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
        name: Name,
        params: Vec<Param>,
        body: ExprID,
        ret: TypeID,
    },
    Struct {
        name: Name,
        fields: Vec<Field>,
    },
    Enum {
        name: Name,
        cases: Vec<Name>,
    }
}

impl Decl {
    pub fn find_field(&self, name: Name) -> Option<Field> {
        if let Decl::Struct{fields, ..} = self {
            for field in fields {
                if field.name == name {
                    return Some(field.clone())
                }
            }
        }
        None
    }
}

impl Decl {
    pub fn name(&self) -> Name {
        match self {
            Decl::Func { name, .. } => *name,
            Decl::Struct { name, .. } => *name,
            Decl::Enum { name, .. } => *name
        }
    }
}

pub struct Compiler {
    pub decls: Vec<Decl>
}

pub fn find_decls(decls: &Vec<Decl>, name: Name, f: &mut impl FnMut(&Decl)) {
    for d in decls {
        if d.name() == name {
            f(d)
        }
    }
}
