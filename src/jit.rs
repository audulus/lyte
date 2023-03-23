// Pulled from https://github.com/bytecodealliance/cranelift-jit-demo

use crate::defs::*;
use crate::ExprArena;
use crate::Instance;
use crate::DeclTable;
use cranelift::prelude::isa::CallConv;
use cranelift::prelude::types::*;
use cranelift::prelude::*;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{DataContext, Linkage, Module};
use std::collections::HashMap;
use std::slice;
use std::vec;

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
    pub fn compile(&mut self, decls: &DeclTable, arena: &ExprArena, types: &[crate::types::TypeID]) -> Result<*const u8, String> {
        let name = "main";

        // Translate into cranelift IR.
        // Create the builder to build a function.
        let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_context);
        let mut trans = FunctionTranslator::new(builder, &mut self.module);

        // Find the main function.
        let main_decl = if let Decl::Func(d) = &decls.find(Name::new(name.into()))[0] { d } else { panic!() };

        trans.translate_fn(&main_decl, arena, types, decls);

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

impl crate::Type {
    fn cranelift_type(&self) -> Type {
        match self {
            crate::Type::Void => INVALID,
            crate::Type::Float32 => F32,
            crate::Type::Float64 => F64,

            // Should this be smaller?
            crate::Type::Bool => I32,

            // Functions probably need a closure pointer?
            crate::Type::Func(_, _) => I64,

            // Structs are represented as pointers.
            crate::Type::Name(_, _) => I64,

            // Pair of pointer and length.
            crate::Type::Array(_, _) => I64X2,

            // Tuple is a pointer.
            crate::Type::Tuple(_) => I64,
            _ => todo!(),
        }
    }
}

/// A collection of state used for translating from toy-language AST nodes
/// into Cranelift IR.
struct FunctionTranslator<'a> {
    builder: FunctionBuilder<'a>,
    variables: HashMap<String, Variable>,
    module: &'a mut JITModule,

    /// Next variable index.
    next_index: usize,

    /// For generating code for generics.
    current_instance: Instance
}

impl<'a> FunctionTranslator<'a> {

    fn new(builder: FunctionBuilder<'a>, module: &'a mut JITModule) -> Self {
        Self {
            builder,
            variables: HashMap::new(),
            module,
            next_index: 0,
            current_instance: Instance::new()
        }
    }

    fn translate_fn(
        &mut self,
        decl: &FuncDecl,
        arena: &ExprArena,
        types: &[crate::types::TypeID],
        decls: &DeclTable,
    ) {

    }

    fn translate_expr(
        &mut self,
        expr: ExprID,
        arena: &ExprArena,
        types: &[crate::types::TypeID],
        decls: &DeclTable,
    ) -> Value {
        match &arena[expr] {
            Expr::Id(name) => {
                let variable = self.variables.get(&**name).unwrap();
                self.builder.use_var(*variable)
            }
            Expr::Binop(op, lhs_id, rhs_id) => {
                self.translate_binop(*op, *lhs_id, *rhs_id, arena, types, decls)
            }
            Expr::Call(fn_id, arg_ids) => {
                let f = self.translate_expr(*fn_id, arena, types, decls);

                let mut args = vec![];
                for arg_id in arg_ids {
                    args.push(self.translate_expr(*arg_id, arena, types, decls))
                }

                if let crate::Type::Func(from, to) = *(types[expr]) {
                    let mut sig = Signature::new(CallConv::Fast);
                    if let crate::Type::Tuple(args) = &*from {
                        sig.params = args
                            .iter()
                            .map(|t| AbiParam::new(t.cranelift_type()))
                            .collect();
                    } else {
                        panic!();
                    }
                    sig.returns = vec![AbiParam::new(to.cranelift_type())];
                    let sref = self.builder.import_signature(sig);
                    let call = self.builder.ins().call_indirect(sref, f, &args);
                    self.builder.inst_results(call)[0]
                } else {
                    panic!()
                }
            }
            Expr::Let(name, init) => self.translate_expr(*init, arena, types, decls),
            Expr::Var(name, init, _) => {
                let ty = &types[expr];

                // Allocate a new stack slot with a size of the variable.
                let slot = self.builder.create_sized_stack_slot(StackSlotData {
                    kind: StackSlotKind::ExplicitSlot,
                    size: ty.size(decls) as u32,
                });

                // Create an instruction that loads the address of the stack slot.
                let addr = self.builder.ins().stack_addr(I32, slot, 0);

                if let Some(init_id) = init {
                    let init_value = self.translate_expr(*init_id, arena, types, decls);

                    self.builder
                        .ins()
                        .store(MemFlags::new(), addr, init_value, 0);
                }

                addr
            }
            Expr::Field(lhs, name) => {
                let lhs_ty = types[*lhs];
                let lhs_val = self.translate_expr(*lhs, arena, types, decls);
                if let crate::Type::Name(struct_name, _) = &*lhs_ty {
                    let decl = decls.find(*struct_name);
                    if let crate::Decl::Struct(s) = &decl[0] {
                        let off = s.field_offset(name, decls, &self.current_instance);
                        let load_ty = types[expr].cranelift_type();
                        self.builder
                            .ins()
                            .load(load_ty, MemFlags::new(), lhs_val, off as i32)
                    } else {
                        panic!();
                    }
                } else {
                    panic!();
                }
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
        decls: &crate::DeclTable,
    ) -> Value {
        let lhs = self.translate_expr(lhs_id, arena, types, decls);
        let rhs = self.translate_expr(rhs_id, arena, types, decls);
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
            Binop::Assign => {
                if *t == crate::types::Type::Int32 {
                    let _ = self.builder.ins().store(MemFlags::new(), rhs, lhs, 0);
                    lhs
                } else {
                    todo!()
                }
            }
            _ => todo!(),
        }
    }

    fn delcare_variable(&mut self, name: &String, ty: Type) -> Variable {
        let var = Variable::new(self.next_index);
        if !self.variables.contains_key(name) {
            self.variables.insert(name.into(), var);
            self.builder.declare_var(var, ty);
            self.next_index += 1;
        }
        var
    }
}
