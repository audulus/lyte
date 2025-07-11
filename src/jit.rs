// Pulled from https://github.com/bytecodealliance/cranelift-jit-demo

use crate::decl::*;
use crate::defs::*;
use crate::expr::*;
use crate::DeclTable;
use crate::Instance;
extern crate cranelift_codegen;
use cranelift::codegen::{self, settings};
use cranelift::prelude::isa::CallConv;
use cranelift::prelude::types::*;
use cranelift::prelude::*;
use cranelift_codegen::verifier::verify_function;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{Linkage, Module};
use core::panic;
use std::collections::{HashMap, HashSet};
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

    /// The module, with the jit backend, which manages the JIT'd
    /// functions.
    module: JITModule,

    defined_functions: HashSet<Name>,

    pub print_ir: bool,
}

impl Default for JIT {
    fn default() -> Self {
        // let builder = JITBuilder::new(cranelift_module::default_libcall_names());

        // See https://github.com/bytecodealliance/wasmtime/issues/2735#issuecomment-801476541

        let mut flag_builder = settings::builder();
        // On at least AArch64, "colocated" calls use shorter-range relocations,
        // which might not reach all definitions; we can't handle that here, so
        // we require long-range relocation types.
        flag_builder.set("use_colocated_libcalls", "false").unwrap();
        flag_builder.set("is_pic", "false").unwrap();
        let isa_builder = cranelift_native::builder().unwrap_or_else(|msg| {
            panic!("host machine is not supported: {}", msg);
        });
        let isa = isa_builder
            .finish(settings::Flags::new(flag_builder))
            .unwrap();
        let builder = JITBuilder::with_isa(isa, cranelift_module::default_libcall_names());

        let module = JITModule::new(builder);
        Self {
            builder_context: FunctionBuilderContext::new(),
            ctx: module.make_context(),
            module,
            defined_functions: HashSet::new(),
            print_ir: false,
        }
    }
}

impl JIT {
    /// Compile our AST into native code.
    pub fn compile(&mut self, decls: &DeclTable) -> Result<*const u8, String> {
        let name = "main";

        let main_decls = &decls.find(Name::new(name.into()));

        if main_decls.is_empty() {
            panic!("no main function found");
        }

        // Find the main function.
        let main_decl = if let Decl::Func(d) = &main_decls[0] {
            d
        } else {
            panic!("no main function found");
        };

        let id = self.compile_function(decls, main_decl)?;

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

    fn compile_function(
        &mut self,
        decls: &DeclTable,
        decl: &FuncDecl,
    ) -> Result<cranelift_module::FuncId, String> {
        // self.ctx.func.signature =

        let called_functions = self.function_body(decls, decl);

        // Print the generated IR
        if self.print_ir {
            println!("Generated IR:\n{}", self.ctx.func.display());
        }

        // Next, declare the function to jit. Functions must be declared
        // before they can be called, or defined.
        let id = self
            .module
            .declare_function(&*decl.name, Linkage::Export, &self.ctx.func.signature)
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

        self.defined_functions.insert(decl.name);

        // Compile any called functions that haven't already been defined.
        for name in called_functions {
            if self.defined_functions.contains(&name) {
                continue;
            }

            let decl = if let Decl::Func(d) = &decls.find(name)[0] {
                d
            } else {
                panic!()
            };

            self.compile_function(decls, decl)?;
        }

        Ok(id)
    }

    fn function_body(&mut self, decls: &DeclTable, decl: &FuncDecl) -> HashSet<Name> {
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

        let called = trans.called_functions;

        let flags = settings::Flags::new(settings::builder());
        let res = verify_function(&self.ctx.func, &flags);
        // println!("{}", self.ctx.func.display());
        if let Err(errors) = res {
            panic!("{}", errors);
        }

        called
    }
}

impl crate::Type {
    fn cranelift_type(&self) -> Type {
        match self {
            crate::Type::Void => I32,
            crate::Type::Int32 => I32,
            crate::Type::UInt32 => I32,
            crate::Type::Float32 => F32,
            crate::Type::Float64 => F64,
            crate::Type::Bool => I8,

            // Functions probably need a closure pointer?
            crate::Type::Func(_, _) => I64,

            // Structs are represented as pointers.
            crate::Type::Name(_, _) => I64,

            // Pointer to start of array. (how should we do slices?)
            crate::Type::Array(_, _) => I64,

            // Tuple is a pointer.
            crate::Type::Tuple(_) => I64,

            crate::Type::Anon(_) => panic!("anonymous type should have been solved for!"),
            _ => {
                println!("cranelift_type for {:?} is not yet implemented", self);
                todo!()
            }
        }
    }

    // Are we representing values of this type
    // using a pointer or is the value in the register?
    fn is_ptr(&self) -> bool {
        match self {
            crate::Type::Name(_, _) | crate::Type::Tuple(_) | crate::Type::Array(_, _) => true,
            _ => false,
        }
    }
}

fn fn_sig(module: &JITModule, from: crate::TypeID, to: crate::TypeID) -> Signature {
    let mut sig = module.make_signature();
    if let crate::Type::Tuple(args) = &*from {
        sig.params = args
            .iter()
            .map(|t| AbiParam::new(t.cranelift_type()))
            .collect();
    } else {
        panic!();
    }

    if *to != crate::Type::Void {
        sig.returns = vec![AbiParam::new(to.cranelift_type())];
    }
    sig
}

struct FunctionTranslator<'a> {
    builder: FunctionBuilder<'a>,
    variables: HashMap<String, Variable>,
    module: &'a mut JITModule,

    /// Next variable index.
    next_index: usize,

    /// For generating code for generics.
    current_instance: Instance,

    called_functions: HashSet<Name>,
}

impl<'a> FunctionTranslator<'a> {
    fn new(builder: FunctionBuilder<'a>, module: &'a mut JITModule) -> Self {
        Self {
            builder,
            variables: HashMap::new(),
            module,
            next_index: 0,
            current_instance: Instance::new(),
            called_functions: HashSet::new(),
        }
    }

    fn translate_fn(&mut self, decl: &FuncDecl, decls: &DeclTable) {
        self.translate_expr(decl.body.unwrap(), decl, decls);
    }

    fn translate_lvalue(&mut self, expr: ExprID, decl: &FuncDecl, decls: &DeclTable) -> Value {
        match &decl.arena[expr] {
            Expr::Id(name) => {
                let var = *self.variables.get(&**name).unwrap();
                self.builder.use_var(var)
            }
            Expr::Field(lhs, name) => {
                let lhs_ty = decl.types[*lhs];
                let lhs_value = self.translate_lvalue(*lhs, decl, decls);
                if let crate::Type::Name(struct_name, _) = &*lhs_ty {
                    let struct_decl = decls.find(*struct_name);
                    if let crate::Decl::Struct(s) = &struct_decl[0] {
                        let off = s.field_offset(name, decls, &self.current_instance);
                        let off_value = self.builder.ins().iconst(I64, off as i64);
                        self.builder.ins().iadd(lhs_value, off_value)
                    } else {
                        panic!();
                    }
                } else {
                    panic!();
                }
            }
            Expr::ArrayIndex(lhs, rhs) => {
                let lhs_ty = decl.types[*lhs];
                let lhs_val = self.translate_lvalue(*lhs, decl, decls);
                let rhs_val = self.translate_expr(*rhs, decl, decls);
                if let crate::Type::Array(ty, _) = &*lhs_ty {
                    let off = self.builder.ins().imul_imm(rhs_val, ty.size(decls) as i64);
                    let off = self.builder.ins().uextend(I64, off);
                    self.builder.ins().iadd(lhs_val, off)
                } else {
                    panic!("subscript expression not on array. should be caught by type checker");
                }
            }
            _ => {
                println!("unimplemented expression: {:?}", &decl.arena[expr]);
                todo!();
            }
        }
    }

    fn translate_expr(&mut self, expr: ExprID, decl: &FuncDecl, decls: &DeclTable) -> Value {
        match &decl.arena[expr] {
            Expr::False => self.builder.ins().iconst(I8, 0),
            Expr::True => self.builder.ins().iconst(I8, 1),
            Expr::Int(imm) => self.builder.ins().iconst(I32, *imm),
            Expr::Id(name) => {
                let ty = &decl.types[expr];
                if let Some(variable) = self.variables.get(&**name) {
                    let p = self.builder.use_var(*variable);
                    if ty.is_ptr() {
                        p
                    } else {
                        self.builder
                            .ins()
                            .load(ty.cranelift_type(), MemFlags::new(), p, 0)
                    }
                } else {
                    self.translate_func(name, &*ty)
                }
            }
            Expr::Binop(op, lhs_id, rhs_id) => {
                self.translate_binop(*op, *lhs_id, *rhs_id, decl, decls)
            }
            Expr::Unop(op, arg_id) => self.translate_unop(*op, *arg_id, decl, decls),
            Expr::Call(fn_id, arg_ids) => {
                let f = self.translate_expr(*fn_id, decl, decls);

                let mut args = vec![];
                for arg_id in arg_ids {
                    args.push(self.translate_expr(*arg_id, decl, decls))
                }

                if let crate::Type::Func(from, to) = *(decl.types[*fn_id]) {
                    let sig = fn_sig(&self.module, from, to);
                    let sref = self.builder.import_signature(sig);
                    let call = self.builder.ins().call_indirect(sref, f, &args);
                    if let Some(result) = self.builder.inst_results(call).first() {
                        *result
                    } else {
                        self.builder.ins().iconst(I32, 0)
                    }
                } else {
                    panic!("tried to call non-function. should be caught by checker");
                }
            }
            Expr::Let(name, init, _) => {
                let ty = &decl.types[expr];
                let init_val = self.translate_expr(*init, decl, decls);
                let var = self.declare_variable(name, ty.cranelift_type());
                self.builder.def_var(var, init_val);
                init_val
            }
            Expr::Var(name, init, _) => {
                let ty = &decl.types[expr];
                let var = self.declare_variable(name, I64);

                let sz = ty.size(decls) as u32;
                assert!(sz > 0, "variable size must be greater than 0");

                // Allocate a new stack slot with a size of the variable.
                let slot = self.builder.create_sized_stack_slot(StackSlotData {
                    kind: StackSlotKind::ExplicitSlot,
                    size: sz,
                    align_shift: 0,
                });

                // Create an instruction that loads the address of the stack slot.
                let addr = self.builder.ins().stack_addr(I64, slot, 0);

                // Define the variable as the address of the slot so we can assign to it.
                self.builder.def_var(var, addr);

                if let Some(init_id) = init {
                    let init_value = self.translate_expr(*init_id, decl, decls);
                    self.gen_copy(*ty, addr, init_value, decls);
                } else {
                    self.gen_zero(*ty, addr, decls);
                }

                self.builder.ins().iconst(I32, 0)
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
                        panic!("unknown struct. should be caught by checker");
                    }
                } else {
                    panic!("lhs of field expression is not a struct. should be caught by checker");
                }
            }
            Expr::ArrayIndex(lhs, rhs) => {
                let lhs_ty = decl.types[*lhs];
                let lhs_val = self.translate_expr(*lhs, decl, decls);
                let rhs_val = self.translate_expr(*rhs, decl, decls);
                if let crate::Type::Array(ty, _) = &*lhs_ty {
                    let off = self.builder.ins().imul_imm(rhs_val, ty.size(decls) as i64);
                    let off = self.builder.ins().uextend(I64, off);
                    let p = self.builder.ins().iadd(lhs_val, off);
                    let load_ty = decl.types[expr].cranelift_type();
                    self.builder.ins().load(load_ty, MemFlags::new(), p, 0)
                } else {
                    panic!("subscript expression not on array. should be caught by type checker");
                }
            }
            Expr::ArrayLiteral(elements) => {
                let element_values: Vec<Value> = elements
                    .iter()
                    .map(|e| self.translate_expr(*e, decl, decls))
                    .collect();
                
                let ty = decl.types[expr];

                if let crate::Type::Array(ty, _) = &*ty { 

                    let sz = ty.size(decls) as u32;
                    let element_size = sz / elements.len() as u32;

                    // Allocate a new stack slot with a size of the variable.
                    let slot = self.builder.create_sized_stack_slot(StackSlotData {
                        kind: StackSlotKind::ExplicitSlot,
                        size: sz,
                        align_shift: 0,
                    });

                    // Create an instruction that loads the address of the stack slot.
                    let addr = self.builder.ins().stack_addr(I64, slot, 0);

                    // Store each element in the stack slot.
                    for (i, value) in element_values.iter().enumerate() {
                        let offset = i as i32 * element_size as i32;
                        self.builder.ins().stack_store(*value, slot, offset);
                    }

                    addr

                } else {
                    panic!("array literal not on array type.");
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

    fn translate_unop(
        &mut self,
        unop: Unop,
        arg_id: ExprID,
        decl: &FuncDecl,
        decls: &crate::DeclTable,
    ) -> Value {
        let v = self.translate_expr(arg_id, decl, decls);
        match unop {
            Unop::Neg => self.builder.ins().imul_imm(v, -1),
            Unop::Not => {
                let bnot = self.builder.ins().bnot(v);
                self.builder.ins().band_imm(bnot, 1)
            }
        }
    }

    fn gen_copy(
        &mut self,
        t: crate::TypeID,
        dst: Value,
        src: Value,
        decls: &crate::DeclTable,
    ) {
        if t.is_ptr() {
            let size = t.size(decls) as u64;
            self.builder.emit_small_memory_copy(
                self.module.isa().frontend_config(),
                dst,
                src,
                size,
                4,
                4,
                true,
                MemFlags::trusted(),
            );
        } else {
            self.builder.ins().store(MemFlags::new(), src, dst, 0);
        }
    }

    fn gen_zero(
        &mut self,
        t: crate::TypeID,
        dst: Value,
        decls: &crate::DeclTable,
    ) {
        let size = t.size(decls) as u32;
        let zero = self.builder.ins().iconst(I8, 0);
        // Store zero byte-by-byte for the size of the type
        for offset in 0..size {
            self.builder.ins().store(MemFlags::new(), zero, dst, offset as i32);
        }
    }

    fn gen_eq(
        &mut self,
        t: crate::TypeID,
        dst: Value,
        src: Value,
        decls: &crate::DeclTable,
    ) -> Value {
        if t.is_ptr() {
            let size = t.size(decls) as u64;
            let align = std::num::NonZeroU8::new(4).unwrap();
            self.builder.emit_small_memory_compare(
                self.module.isa().frontend_config(),
                IntCC::Equal,
                dst,
                src,
                size,
                align,
                align,
                MemFlags::trusted(),
            )
        } else {
            match *t {
                crate::types::Type::Bool | crate::types::Type::Int32 | crate::types::Type::UInt32 | crate::types::Type::Int8 | crate::types::Type::UInt8 => {
                    self.builder.ins().icmp(IntCC::Equal, dst, src)
                }
                crate::types::Type::Float32 | crate::types::Type::Float64 => {
                    self.builder.ins().fcmp(FloatCC::Equal, dst, src)
                }
                _ => todo!(),
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
        match binop {
            Binop::Plus => {
                let lhs = self.translate_expr(lhs_id, decl, decls);
                let rhs = self.translate_expr(rhs_id, decl, decls);
                let t = decl.types[lhs_id];

                match *t {
                    crate::types::Type::Int32 | crate::types::Type::UInt32 | crate::types::Type::Int8 | crate::types::Type::UInt8 => {
                        self.builder.ins().iadd(lhs, rhs)
                    }
                    crate::types::Type::Float32 | crate::types::Type::Float64 => {
                        self.builder.ins().fadd(lhs, rhs)
                    }
                    _ => todo!(),
                }
            }
            Binop::Minus => {
                let lhs = self.translate_expr(lhs_id, decl, decls);
                let rhs = self.translate_expr(rhs_id, decl, decls);
                let t = decl.types[lhs_id];

                match *t {
                    crate::types::Type::Int32 | crate::types::Type::UInt32 | crate::types::Type::Int8 | crate::types::Type::UInt8 => {
                        self.builder.ins().isub(lhs, rhs)
                    }
                    crate::types::Type::Float32 | crate::types::Type::Float64 => {
                        self.builder.ins().fsub(lhs, rhs)
                    }
                    _ => todo!(),
                }
            }
            Binop::Mult => {
                let lhs = self.translate_expr(lhs_id, decl, decls);
                let rhs = self.translate_expr(rhs_id, decl, decls);
                let t = decl.types[lhs_id];
                match *t {
                    crate::types::Type::Int32 | crate::types::Type::UInt32 | crate::types::Type::Int8 | crate::types::Type::UInt8 => {
                        self.builder.ins().imul(lhs, rhs)
                    }
                    crate::types::Type::Float32 | crate::types::Type::Float64 => {
                        self.builder.ins().fmul(lhs, rhs)
                    }
                    _ => todo!(),
                }
            }
            Binop::Div => {
                let lhs = self.translate_expr(lhs_id, decl, decls);
                let rhs = self.translate_expr(rhs_id, decl, decls);
                let t = decl.types[lhs_id];
                match *t {
                    crate::types::Type::Int32 | crate::types::Type::UInt32 | crate::types::Type::Int8 | crate::types::Type::UInt8 => {
                        self.builder.ins().udiv(lhs, rhs)
                    }
                    crate::types::Type::Float32 | crate::types::Type::Float64 => {
                        self.builder.ins().fdiv(lhs, rhs)
                    }
                    _ => todo!(),
                }
            }
            Binop::Assign => {
                let lhs = self.translate_lvalue(lhs_id, decl, decls);
                let rhs = self.translate_expr(rhs_id, decl, decls);
                let t = decl.types[lhs_id];
                self.gen_copy(t, lhs, rhs, decls);
                rhs
            }
            Binop::Equal => {
                let lhs = self.translate_expr(lhs_id, decl, decls);
                let rhs = self.translate_expr(rhs_id, decl, decls);
                let t = decl.types[lhs_id];
                self.gen_eq(t, lhs, rhs, decls)
            }
            Binop::NotEqual => {
                let lhs = self.translate_expr(lhs_id, decl, decls);
                let rhs = self.translate_expr(rhs_id, decl, decls);
                let t = decl.types[lhs_id];

                match *t {
                    crate::types::Type::Bool | crate::types::Type::Int32 | crate::types::Type::UInt32 | crate::types::Type::Int8 | crate::types::Type::UInt8 => {
                        self.builder.ins().icmp(IntCC::NotEqual, lhs, rhs)
                    }
                    crate::types::Type::Float32 | crate::types::Type::Float64 => {
                        self.builder.ins().fcmp(FloatCC::NotEqual, lhs, rhs)
                    }
                    _ => todo!(),
                }
            }
            Binop::Less => {
                let lhs = self.translate_expr(lhs_id, decl, decls);
                let rhs = self.translate_expr(rhs_id, decl, decls);
                let t = decl.types[lhs_id];

                match *t {
                    crate::types::Type::Int32 | crate::types::Type::Int8 => {
                        self.builder.ins().icmp(IntCC::SignedLessThan, lhs, rhs)
                    }
                    crate::types::Type::UInt32 | crate::types::Type::UInt8 => {
                        self.builder.ins().icmp(IntCC::UnsignedLessThan, lhs, rhs)
                    }
                    crate::types::Type::Float32 | crate::types::Type::Float64 => {
                        self.builder.ins().fcmp(FloatCC::LessThan, lhs, rhs)
                    }
                    _ => todo!(),
                }
            }
            Binop::Greater => {
                let lhs = self.translate_expr(lhs_id, decl, decls);
                let rhs = self.translate_expr(rhs_id, decl, decls);
                let t = decl.types[lhs_id];

                match *t {
                    crate::types::Type::Int32 | crate::types::Type::Int8 => {
                        self.builder.ins().icmp(IntCC::SignedGreaterThan, lhs, rhs)
                    }
                    crate::types::Type::UInt32 | crate::types::Type::UInt8 => {
                        self.builder.ins().icmp(IntCC::UnsignedGreaterThan, lhs, rhs)
                    }
                    crate::types::Type::Float32 | crate::types::Type::Float64 => {
                        self.builder.ins().fcmp(FloatCC::GreaterThan, lhs, rhs)
                    }
                    _ => todo!(),
                }
            }
            Binop::And => {
                let lhs = self.translate_expr(lhs_id, decl, decls);
                let rhs = self.translate_expr(rhs_id, decl, decls);
                self.builder.ins().band(lhs, rhs)
            }
            Binop::Or => {
                let lhs = self.translate_expr(lhs_id, decl, decls);
                let rhs = self.translate_expr(rhs_id, decl, decls);
                self.builder.ins().bor(lhs, rhs)
            }
            _ => {
                panic!("unimplemented binary operation: {:?}", binop);
            },
        }
    }

    fn debug_print_i64(&mut self, value: Value) {
        let f_ptr = self.builder.ins().iconst(I64, lyte_print_i64 as i64);

        let mut sig = Signature::new(CallConv::Fast);
        sig.params = vec![AbiParam::new(I64)];
        sig.returns = vec![AbiParam::new(I32)];
        let sref = self.builder.import_signature(sig);
        self.builder.ins().call_indirect(sref, f_ptr, &vec![value]);
    }

    fn translate_func(&mut self, name: &Name, ty: &crate::Type) -> Value {
        if *name == Name::str("assert") {
            return self.builder.ins().iconst(I64, lyte_assert as i64);
        }

        if *name == Name::str("print") {
            return self.builder.ins().iconst(I64, lyte_print_i32 as i64);
        }

        if let crate::Type::Func(dom, rng) = ty {
            let sig = fn_sig(&self.module, *dom, *rng);
            let callee = self
                .module
                .declare_function(&name, Linkage::Import, &sig)
                .expect("problem declaring function");
            let local_callee = self.module.declare_func_in_func(callee, self.builder.func);

            self.called_functions.insert(*name);

            self.builder.ins().func_addr(I64, local_callee)
        } else {
            panic!("expected function type. got {:?}", ty);
        }
    }

    fn declare_variable(&mut self, name: &String, ty: Type) -> Variable {
        let var = Variable::new(self.next_index);
        if !self.variables.contains_key(name) {
            self.variables.insert(name.into(), var);
            self.builder.declare_var(var, ty);
            self.next_index += 1;
        }
        var
    }
}

extern "C" fn lyte_assert(val: i8) {
    assert!(val != 0);
}

extern "C" fn lyte_print_i32(val: i32) {
    println!("{}", val);
}

extern "C" fn lyte_print_i64(val: i64) {
    println!("{}", val);
}
