#![allow(dead_code)]

mod defs;
use defs::*;
mod types;
use types::*;
mod expr;
use expr::*;
mod decl;
use decl::*;

pub mod lexer;
use lexer::*;

pub mod parser;
use parser::*;

mod checker;
pub use checker::*;

mod array_checker;
pub use array_checker::*;

mod compiler;
pub use compiler::*;

mod solver;
pub use solver::*;

mod decl_table;
pub use decl_table::*;

mod jit;
pub use jit::*;
