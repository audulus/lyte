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

    pub fn str(s: &str) -> Self {
        Self(Intern::new(s.into()))
    }
}

impl Deref for Name {
    type Target = String;

    fn deref(&self) -> &Self::Target {
        &self.0
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
    pub col: u32,
}

pub fn test_loc() -> Loc {
    Loc {
        file: Name::new("".into()),
        line: 0,
        col: 0,
    }
}

/// Prints an error message with source context showing the exact location.
///
/// Output format:
/// ```text
/// ❌ file.lyte:10:5: error message
///     source line content
///     ^
/// ```
/// Formats an error message as `file:line:col: message`.
pub fn format_error(loc: Loc, message: &str) -> String {
    format!("{}:{}:{}: {}", loc.file, loc.line, loc.col, message)
}

pub fn print_error_with_context(loc: Loc, message: &str) {
    println!("❌ {}:{}:{}: {}", loc.file, loc.line, loc.col, message);

    // Try to read the source file and display the relevant line
    if let Ok(contents) = std::fs::read_to_string(&*loc.file) {
        if let Some(line) = contents.lines().nth(loc.line as usize - 1) {
            println!("    {}", line);
            // Print caret at the right column (col is 1-indexed)
            let spaces = " ".repeat(loc.col as usize - 1 + 4); // +4 for the leading indent
            println!("{}^", spaces);
        }
    }
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub enum Unop {
    Neg,
    Not,
    // others?
}

#[derive(Clone, Copy, Debug, Eq, PartialEq, Hash)]
pub enum Binop {
    Assign,
    Plus,
    Minus,
    Mult,
    Div,
    Mod,
    Pow,
    Equal,
    NotEqual,
    Less,
    Greater,
    Leq,
    Geq,
    And,
    Or,
}

impl Binop {
    pub fn equality(self) -> bool {
        self == Binop::Equal || self == Binop::NotEqual
    }

    pub fn relational(self) -> bool {
        self == Binop::Less || self == Binop::Greater || self == Binop::Leq || self == Binop::Geq
    }

    pub fn arithmetic(self) -> bool {
        self == Binop::Plus
            || self == Binop::Minus
            || self == Binop::Mult
            || self == Binop::Div
            || self == Binop::Mod
    }

    pub fn overload_name(self) -> &'static str {
        match self {
            Binop::Plus => "__add",
            Binop::Minus => "__sub",
            Binop::Mult => "__mul",
            Binop::Div => "__div",
            Binop::Mod => "__mod",
            _ => panic!(),
        }
    }
}

/// Index of an expression in an ExprArena.
pub type ExprID = usize;

#[derive(Clone, Debug, Eq, PartialEq, Hash)]
pub struct ParseError {
    pub location: Loc,
    pub message: String,
}

#[derive(Debug, Eq, PartialEq, Hash, Clone)]
pub struct ExprArena {
    pub exprs: Vec<Expr>,
    pub locs: Vec<Loc>,
}

impl ExprArena {
    pub fn new() -> Self {
        Self {
            exprs: vec![],
            locs: vec![],
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

impl Default for ExprArena {
    fn default() -> Self {
        Self::new()
    }
}
