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

mod interval;
pub use interval::*;

mod safety_checker;
pub use safety_checker::*;

mod compiler;
pub use compiler::*;

mod solver;
pub use solver::*;

mod decl_table;
pub use decl_table::*;

mod jit;
pub use jit::*;

pub mod mangle;

mod hoist;
use hoist::*;

mod monomorph;
pub use monomorph::*;

mod monomorph_pass;
pub use monomorph_pass::*;

pub mod vm;

mod vm_optimize;

pub mod vm_codegen;

pub mod ffi;
