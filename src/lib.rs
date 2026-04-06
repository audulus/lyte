#![allow(dead_code)]

mod defs;
pub use defs::*;
mod types;
pub use types::*;
mod expr;
pub use expr::*;
mod decl;
pub use decl::*;

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

pub mod cancel;
pub use cancel::*;

#[cfg(feature = "cranelift")]
mod jit;
#[cfg(feature = "cranelift")]
pub use jit::*;

#[cfg(feature = "llvm")]
mod llvm_jit;
#[cfg(feature = "llvm")]
pub use llvm_jit::*;

pub mod mangle;

mod hoist;
use hoist::*;

mod monomorph;
pub use monomorph::*;

mod monomorph_pass;
pub use monomorph_pass::*;

pub mod opcode;

pub mod vm;

#[cfg(target_arch = "aarch64")]
pub mod vm_arm64;

pub mod vm_optimize;

pub mod vm_codegen;

pub mod stack_ir;

pub mod stack_codegen;

pub mod stack_optimize;

pub mod stack_depth;

pub mod stack_vm;

pub mod stack_interp_bridge;

pub mod ffi;
