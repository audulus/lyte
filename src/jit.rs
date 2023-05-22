// Pulled from https://github.com/bytecodealliance/cranelift-jit-demo

use crate::decl::*;
use crate::defs::*;
use crate::expr::*;
use crate::DeclTable;
use crate::Instance;
extern crate cranelift_codegen;
use cranelift::prelude::isa::CallConv;
use cranelift::prelude::types::*;
use cranelift::prelude::*;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{DataContext, Linkage, Module};
use cranelift_codegen::verifier::verify_function;
use std::collections::HashMap;
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
    pub fn compile(&mut self, decls: &DeclTable) -> Result<*const u8, String> {
        let name = "main";

        // Find the main function.
        let main_decl = if let Decl::Func(d) = &decls.find(Name::new(name.into()))[0] {
            d
        } else {
            panic!()
        };

        self.function_body(decls, main_decl);

        // Next, declare the function to jit. Functions must be declared
        // before they can be called, or defined.
        //
        // TODO: This may be an area where the API should be streamlined; should
        // we have a version of `declare_function` that automatically declares
        // the function?
        let id = self
            .module
            .declare_function(name, Linkage::Export, &self.ctx.func.signature)
            .map_err(|e| e.to_string())?;

        // Define the function to jit. This finishes compilation, although
        // there may be outstanding relocations to perform. Currently, jit
        // cannot finish relocations until all functions to be called are
        // defined. For this toy demo for now, we'll just finalize the
        // function below.
        self.module
            .define_function(id, &mut self.ctx)
            .map_err(|e| {
                println!("error: {:?}", e);
                e.to_string()
            })?;

        // Now that compilation is finished, we can clear out the context state.
        self.module.clear_context(&mut self.ctx);

        // Finalize the functions which we just defined, which resolves any
        // outstanding relocations (patching in addresses, now that they're
        // available).
        self.module
            .finalize_definitions()
            .map_err(|err| err.to_string())?;

        // We can now retrieve a pointer to the machine code.
        let code = self.module.get_finalized_function(id);

        Ok(code)
    }

    fn function_body(&mut self, decls: &DeclTable, decl: &FuncDecl) {

        // Translate into cranelift IR.
        // Create the builder to build a function.
        let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_context);

        // Create the entry block, to start emitting code in.
        let entry_block = builder.create_block();

        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);

        let mut trans = FunctionTranslator::new(builder, &mut self.module);

        trans.translate_fn(decl, decls);

        // Need a return instruction at the end of the function's block.
        trans.builder.ins().return_(&[]);

        // Indicate we're finished with the function.
        trans.builder.finalize();

        let flags = settings::Flags::new(settings::builder());
        let res = verify_function(&self.ctx.func, &flags);
        // println!("{}", self.ctx.func.display());
        if let Err(errors) = res {
            panic!("{}", errors);
        }

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

            crate::Type::Anon(_) => panic!("anonymous type should have been solved for!"),
            _ => {
                println!("cranelift_type for {:?} is not yet implemented", self);
                todo!()
            }
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
    current_instance: Instance,
}

impl<'a> FunctionTranslator<'a> {
    fn new(builder: FunctionBuilder<'a>, module: &'a mut JITModule) -> Self {
        Self {
            builder,
            variables: HashMap::new(),
            module,
            next_index: 0,
            current_instance: Instance::new(),
        }
    }

    fn translate_fn(&mut self, decl: &FuncDecl, decls: &DeclTable) {
        self.translate_expr(decl.body.unwrap(), decl, decls);
    }

    fn translate_expr(&mut self, expr: ExprID, decl: &FuncDecl, decls: &DeclTable) -> Value {
        match &decl.arena[expr] {
            Expr::Int(imm) => {
                self.builder.ins().iconst(I32, *imm)
            }
            Expr::Id(name) => {
                let variable = self.variables.get(&**name).unwrap();
                self.builder.use_var(*variable)
            }
            Expr::Binop(op, lhs_id, rhs_id) => {
                self.translate_binop(*op, *lhs_id, *rhs_id, decl, decls)
            }
            Expr::Call(fn_id, arg_ids) => {
                let f = self.translate_expr(*fn_id, decl, decls);

                let mut args = vec![];
                for arg_id in arg_ids {
                    args.push(self.translate_expr(*arg_id, decl, decls))
                }

                if let crate::Type::Func(from, to) = *(decl.types[expr]) {
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
            Expr::Let(_name, init) => self.translate_expr(*init, decl, decls),
            Expr::Var(_name, init, _) => {
                let ty = &decl.types[expr];
                let sz = ty.size(decls) as u32;

                // Allocate a new stack slot with a size of the variable.
                let slot = self.builder.create_sized_stack_slot(StackSlotData {
                    kind: StackSlotKind::ExplicitSlot,
                    size: sz,
                });

                // Create an instruction that loads the address of the stack slot.
                let addr = self.builder.ins().stack_addr(I32, slot, 0);

                if let Some(init_id) = init {
                    let init_value = self.translate_expr(*init_id, decl, decls);

                    self.builder
                        .ins()
                        .store(MemFlags::new(), addr, init_value, 0);
                }

                addr
            }
            Expr::Field(lhs, name) => {
                let lhs_ty = decl.types[*lhs];
                let lhs_val = self.translate_expr(*lhs, decl, decls);
                if let crate::Type::Name(struct_name, _) = &*lhs_ty {
                    let struct_decl = decls.find(*struct_name);
                    if let crate::Decl::Struct(s) = &struct_decl[0] {
                        let off = s.field_offset(name, decls, &self.current_instance);
                        let load_ty = decl.types[expr].cranelift_type();
                        self.builder
                            .ins()
                            .load(load_ty, MemFlags::new(), lhs_val, off)
                    } else {
                        panic!();
                    }
                } else {
                    panic!();
                }
            }
            Expr::Block(exprs) => {
                for expr in exprs {
                    self.translate_expr(*expr, decl, decls);
                }
                self.builder.ins().iconst(I32, 0)
            }
            _ => {
                println!("unimplemented expression: {:?}", &decl.arena[expr]);
                todo!();
            }
        }
    }

    fn translate_binop(
        &mut self,
        binop: Binop,
        lhs_id: ExprID,
        rhs_id: ExprID,
        decl: &FuncDecl,
        decls: &crate::DeclTable,
    ) -> Value {
        let lhs = self.translate_expr(lhs_id, decl, decls);
        let rhs = self.translate_expr(rhs_id, decl, decls);
        let t = decl.types[lhs_id];

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
