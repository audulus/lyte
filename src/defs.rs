use crate::*;
use internment::Intern;
use std::cmp::Ordering;
use std::fmt;
use std::hash::Hash;
use std::ops::Deref;
use superslice::Ext;

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
pub struct Param {
    pub name: Name,
    pub ty: Option<TypeID>,
}

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

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct FuncDecl {
    pub name: Name,
    pub typevars: Vec<Name>,
    pub params: Vec<Param>,
    pub body: Option<ExprID>,
    pub ret: TypeID,
    pub constraints: Vec<InterfaceConstraint>,
    pub loc: Loc,
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
        typevars: Vec<Name>,
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
            Decl::Struct { name, .. } => *name,
            Decl::Enum { name, .. } => *name,
            Decl::Global { name, .. } => *name,
            Decl::Interface { name, .. } => *name,
        }
    }
}

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct SortedDecls {
    pub decls: Vec<Decl>
}

fn decl_cmp(a: &Decl, b: &Decl) -> Ordering {
    a.name().cmp(&b.name())
}

impl SortedDecls {
    pub fn new(mut decls: Vec<Decl>) -> Self {
        decls.sort_by(decl_cmp);
        Self { decls: decls }
    }

    /// Returns a slice of all decls which match name.
    pub fn find(&self, name: Name) -> &[Decl] {
        let range = self.decls.equal_range_by(|x| x.name().cmp(&name));
        &self.decls[range]
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

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn test_sorted_decls() {
        let decls = vec![
            Decl::Global { name: Name::new("a".into()), ty: mk_type(Type::Void) },
            Decl::Global { name: Name::new("b".into()), ty: mk_type(Type::Void) },
            Decl::Global { name: Name::new("b".into()), ty: mk_type(Type::Void) },
            Decl::Global { name: Name::new("c".into()), ty: mk_type(Type::Void) },
        ];

        let sorted = SortedDecls::new(decls);
        let d = sorted.find(Name::new("z".into()));
        assert_eq!(d.len(), 0);

        let d = sorted.find(Name::new("b".into()));
        assert_eq!(d.len(), 2);
        assert_eq!(d[0].name(), Name::new("b".into()));
        assert_eq!(d[1].name(), Name::new("b".into()));
    }
}
