use crate::*;

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
    Lambda {
        params: Vec<Param>,
        body: ExprID,
    },
    String(String),
    Char(char),
    Cast,
    Field(ExprID, Name),
    Array(ExprID, ExprID),
    ArrayLiteral(Vec<ExprID>),
    ArrayIndex(ExprID, ExprID),
    True,
    False,
    AsTy(ExprID, TypeID),
    Assign(Name, ExprID),
    Let(Name, ExprID, Option<TypeID>),
    Var(Name, Option<ExprID>, Option<TypeID>),
    If(ExprID, ExprID, Option<ExprID>),
    While(ExprID, ExprID),
    For {
        var: Name,
        start: ExprID,
        end: ExprID,
        body: ExprID,
    },
    Block(Vec<ExprID>),
    Return(ExprID),
    Enum(Name),
    Tuple(Vec<ExprID>),
    Arena(ExprID),
    Error,
}
