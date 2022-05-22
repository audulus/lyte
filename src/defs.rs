use crate::*;
use internment::Intern;
use std::fmt;
use std::hash::Hash;
use std::ops::Deref;

#[derive(Clone, Copy, Hash, Eq, PartialEq)]
pub struct Name(Intern<String>);

impl Name {
    pub fn new(s: String) -> Self {
        Self(Intern::new(s))
    }
}

impl Deref for Name {
    type Target = String;

    fn deref(&self) -> &Self::Target {
        &*self.0
    }
}

impl fmt::Debug for Name {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", *self.0)
    }
}

impl fmt::Display for Name {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", *self.0)
    }
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

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub enum Binop {
    Assign,
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

impl Binop {
    pub fn equality(self) -> bool {
        self == Binop::Equal || self == Binop::NotEqual
    }

    pub fn relational(self) -> bool {
        self == Binop::Less || self == Binop::Greater || self == Binop::Leq || self == Binop::Geq
    }

    pub fn arithmetic(self) -> bool {
        self == Binop::Plus || self == Binop::Minus || self == Binop::Mult || self == Binop::Div
    }
}

pub type ExprID = usize;

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum Expr {
    Id(Name),
    Int(i64),
    Real(String), // f64 is not hashable so we just use the string representation
    Call(ExprID, Vec<ExprID>),
    Macro(Name, Vec<ExprID>),
    Binop(Binop, ExprID, ExprID),
    Unop(ExprID),
    Lambda { params: Vec<Param>, body: ExprID },
    String(String),
    Char(char),
    Subscript(ExprID, ExprID),
    Cast,
    Field(ExprID, Name),
    Array(ExprID, ExprID),
    ArrayLiteral(Vec<ExprID>),
    ArrayIndex(ExprID, ExprID),
    True,
    False,
    AsTy(ExprID, TypeID),
    Assign(Name, ExprID),
    Let(Name, ExprID),
    Var(Name, Option<ExprID>, Option<TypeID>),
    If(ExprID, ExprID, Option<ExprID>),
    While(ExprID, ExprID),
    Block(Vec<ExprID>),
    Return(ExprID),
    Enum(Name),
    Tuple(Vec<ExprID>),
    Arena(ExprID),
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct Param {
    pub name: Name,
    pub ty: Option<TypeID>,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct Field {
    pub name: Name,
    pub ty: TypeID,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct InterfaceConstraint {
    pub interface_name: Name,
    pub typevars: Vec<Name>,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct FuncDecl {
    pub name: Name,
    pub typevars: Vec<Name>,
    pub params: Vec<Param>,
    pub body: Option<ExprID>,
    pub ret: TypeID,
    pub constraints: Vec<InterfaceConstraint>,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum Decl {
    Func(FuncDecl),
    Macro(FuncDecl),
    Struct {
        name: Name,
        typevars: Vec<Name>,
        fields: Vec<Field>,
    },
    Enum {
        name: Name,
        cases: Vec<Name>,
    },
    Global {
        name: Name,
        ty: TypeID,
    },
    Interface {
        name: Name,
        funcs: Vec<FuncDecl>,
    },
}

impl Decl {
    pub fn find_field(&self, name: Name) -> Option<Field> {
        if let Decl::Struct { fields, .. } = self {
            for field in fields {
                if field.name == name {
                    return Some(field.clone());
                }
            }
        }
        None
    }
}

impl Decl {
    pub fn name(&self) -> Name {
        match self {
            Decl::Func(FuncDecl { name, .. }) => *name,
            Decl::Macro(FuncDecl { name, .. }) => *name,
            Decl::Struct { name, .. } => *name,
            Decl::Enum { name, .. } => *name,
            Decl::Global { name, .. } => *name,
            Decl::Interface { name, .. } => *name,
        }
    }
}

/// Finds all decls which match name.
pub fn find_decls(decls: &[Decl], name: Name, f: &mut impl FnMut(&Decl)) {
    for d in decls {
        if d.name() == name {
            f(d)
        }
    }
}

pub fn find_decl(decls: &[Decl], name: Name) -> Option<&Decl> {
    for d in decls {
        if d.name() == name {
            return Some(d);
        }
    }
    None
}

/// Finds all enums with a case.
/// This is for resolving .enum_name expressions.
pub fn find_enums_with_case(decls: &[Decl], case_name: Name, f: &mut impl FnMut(Name)) {
    for d in decls {
        if let Decl::Enum {
            name: enum_name,
            cases,
        } = d
        {
            for case in cases {
                if *case == case_name {
                    f(*enum_name);
                    break;
                }
            }
        }
    }
}
