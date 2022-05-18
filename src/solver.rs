use crate::*;

pub enum Constraint2 {
    /// Equality.
    Equal(Type, Type, Loc),

    /// Function overloads.
    Or(Type, Vec<Type>, Loc),

    /// Field access.
    Field(Type, Name, Type, Loc),
}
