use crate::*;
use internment::Intern;

use std::fmt;
use std::hash::Hash;
use std::ops::Deref;

/// An interned string.
#[derive(Clone, Copy, Hash, Eq, PartialEq, Ord, PartialOrd)]
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

/// Source code location.
///
/// Should this be a span instead?
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
pub enum Unop {
    Neg,
    // others?
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

    pub fn overload_name(self) -> &'static str {
        match self {
            Binop::Plus => "__add",
            Binop::Minus => "__sub",
            Binop::Mult => "__mul",
            Binop::Div => "__div",
            _ => panic!(),
        }
    }
}

/// Index of an expression in an ExprArena.
pub type ExprID = usize;

/// Expressions.
///
/// We use indexes instead of boxing. This gives
/// each expression a unique ID which can be used
/// for derived data without mutating the expression
/// tree. It's also faster. Most hierarchical data
/// should be represented this way.
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
pub struct ParseError {
    pub location: Loc,
    pub message: String,
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct ExprArena {
    pub exprs: Vec<Expr>,
    pub locs: Vec<Loc>,
    pub types: Vec<TypeID>,
}

impl ExprArena {
    pub fn new() -> Self {
        Self {
            exprs: vec![],
            locs: vec![],
            types: vec![],
        }
    }

    /// Appends an expression and returns an ID for that expression.
    pub fn add(&mut self, expr: Expr, loc: Loc) -> ExprID {
        let id = self.exprs.len();
        self.exprs.push(expr);
        self.locs.push(loc);
        id
    }
}

/// Formal parameter.
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct Param {
    pub name: Name,
    pub ty: Option<TypeID>,
}

/// Struct field.
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct Field {
    pub name: Name,
    pub ty: TypeID,
    pub loc: Loc,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct InterfaceConstraint {
    pub interface_name: Name,
    pub typevars: Vec<Name>,
}

/// Function declaration.
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct FuncDecl {
    pub name: Name,
    pub typevars: Vec<Name>,
    pub params: Vec<Param>,
    pub body: Option<ExprID>,
    pub ret: TypeID,
    pub constraints: Vec<InterfaceConstraint>,
    pub loc: Loc,
    pub arena: ExprArena,
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct StructDecl {
    pub name: Name,
    pub typevars: Vec<Name>,
    pub fields: Vec<Field>,
}

impl StructDecl {
    pub fn find_field(&self, name: &Name) -> Option<Field> {
        for field in &self.fields {
            if field.name == *name {
                return Some(field.clone());
            }
        }
        None
    }

    pub fn field_offset(&self, name: &Name, decls: &DeclTable, inst: &Instance) -> i32 {
        let mut off = 0;
        for field in &self.fields {
            if field.name == *name {
                return off;
            }
            off += field.ty.subst(inst).size(decls);
        }
        panic!()
    }
}

/// Provides a set of functions that some type variables
/// must satisfy.
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct Interface {
    pub name: Name,
    pub typevars: Vec<Name>,
    pub funcs: Vec<FuncDecl>,
    pub loc: Loc,
}

/// Top-level declaration.
#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub enum Decl {
    Func(FuncDecl),
    Macro(FuncDecl),
    Struct(StructDecl),
    Enum { name: Name, cases: Vec<Name> },
    Global { name: Name, ty: TypeID },
    Interface(Interface),
}

impl Decl {
    pub fn find_field(&self, name: &Name) -> Option<Field> {
        if let Decl::Struct(st) = self {
            st.find_field(name)
        } else {
            None
        }
    }
}

pub fn find_field(fields: &[Field], name: Name) -> Option<Field> {
    for field in fields {
        if field.name == name {
            return Some(field.clone());
        }
    }
    None
}

impl Decl {
    pub fn name(&self) -> Name {
        match self {
            Decl::Func(FuncDecl { name, .. }) => *name,
            Decl::Macro(FuncDecl { name, .. }) => *name,
            Decl::Struct(StructDecl { name, .. }) => *name,
            Decl::Enum { name, .. } => *name,
            Decl::Global { name, .. } => *name,
            Decl::Interface(Interface { name, .. }) => *name,
        }
    }
}
