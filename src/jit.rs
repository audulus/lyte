// Pulled from https://github.com/bytecodealliance/cranelift-jit-demo

use crate::defs::*;
use crate::ir::*;
use crate::ExprArena;
use cranelift::prelude::*;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{DataContext, Linkage, Module};
use std::collections::HashMap;
use std::slice;

/// The basic JIT class.
pub struct JIT {
    /// The function builder context, which is reused across multiple
    /// FunctionBuilder instances.
    builder_context: FunctionBuilderContext,

    /// The main Cranelift context, which holds the state for codegen. Cranelift
    /// separates this from `Module` to allow for parallel compilation, with a
    /// context per thread, though this isn't in the simple demo here.
    ctx: codegen::Context,

    /// The data context, which is to data objects what `ctx` is to functions.
    data_ctx: DataContext,

    /// The module, with the jit backend, which manages the JIT'd
    /// functions.
    module: JITModule,
}

impl Default for JIT {
    fn default() -> Self {
        let builder = JITBuilder::new(cranelift_module::default_libcall_names());
        let module = JITModule::new(builder.unwrap());
        Self {
            builder_context: FunctionBuilderContext::new(),
            ctx: module.make_context(),
            data_ctx: DataContext::new(),
            module,
        }
    }
}

impl JIT {
    /// Compile our IR into native code.
    pub fn compile(&mut self, input: &BlockArena) -> Result<*const u8, String> {
        let name = "main";

        // Next, declare the function to jit. Functions must be declared
        // before they can be called, or defined.
        //
        // TODO: This may be an area where the API should be streamlined; should
        // we have a version of `declare_function` that automatically declares
        // the function?
        let id = self
            .module
            .declare_function(&name, Linkage::Export, &self.ctx.func.signature)
            .map_err(|e| e.to_string())?;

        // Define the function to jit. This finishes compilation, although
        // there may be outstanding relocations to perform. Currently, jit
        // cannot finish relocations until all functions to be called are
        // defined. For this toy demo for now, we'll just finalize the
        // function below.
        self.module
            .define_function(id, &mut self.ctx)
            .map_err(|e| e.to_string())?;

        // Now that compilation is finished, we can clear out the context state.
        self.module.clear_context(&mut self.ctx);

        // Finalize the functions which we just defined, which resolves any
        // outstanding relocations (patching in addresses, now that they're
        // available).
        self.module.finalize_definitions();

        // We can now retrieve a pointer to the machine code.
        let code = self.module.get_finalized_function(id);

        Ok(code)
    }
}

/// A collection of state used for translating from toy-language AST nodes
/// into Cranelift IR.
struct FunctionTranslator<'a> {
    int: types::Type,
    builder: FunctionBuilder<'a>,
    //variables: HashMap<String, Variable>,
    module: &'a mut JITModule,
}

impl<'a> FunctionTranslator<'a> {
    fn translate_expr(
        &mut self,
        expr: ExprID,
        arena: &ExprArena,
        types: &[crate::types::TypeID],
    ) -> Value {
        match arena[expr] {
            Expr::Binop(op, lhs_id, rhs_id) => {
                self.translate_binop(op, lhs_id, rhs_id, arena, types)
            }
            _ => todo!(),
        }
    }

    fn translate_binop(
        &mut self,
        binop: Binop,
        lhs_id: ExprID,
        rhs_id: ExprID,
        arena: &ExprArena,
        types: &[crate::types::TypeID],
    ) -> Value {
        let lhs = self.translate_expr(lhs_id, arena, types);
        let rhs = self.translate_expr(rhs_id, arena, types);
        let t = types[lhs_id];

        match binop {
            Binop::Plus => {
                if *t == crate::types::Type::Int32 {
                    self.builder.ins().iadd(lhs, rhs)
                } else if *t == crate::types::Type::Float32 {
                    self.builder.ins().fadd(lhs, rhs)
                } else {
                    todo!()
                }
            }
            Binop::Minus => {
                if *t == crate::types::Type::Int32 {
                    self.builder.ins().isub(lhs, rhs)
                } else if *t == crate::types::Type::Float32 {
                    self.builder.ins().fsub(lhs, rhs)
                } else {
                    todo!()
                }
            }
            Binop::Mult => {
                if *t == crate::types::Type::Int32 {
                    self.builder.ins().imul(lhs, rhs)
                } else if *t == crate::types::Type::Float32 {
                    self.builder.ins().fmul(lhs, rhs)
                } else {
                    todo!()
                }
            }
            Binop::Div => {
                if *t == crate::types::Type::Int32 {
                    self.builder.ins().udiv(lhs, rhs)
                } else if *t == crate::types::Type::Float32 {
                    self.builder.ins().fdiv(lhs, rhs)
                } else {
                    todo!()
                }
            }
            _ => todo!(),
        }
    }
}
