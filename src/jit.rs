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

    /// Global variable offsets from base pointer.
    globals: HashMap<Name, i32>,

    /// Total size of global memory needed.
    globals_size: usize,

    pub print_ir: bool,

    /// Counter for generating unique lambda names.
    lambda_counter: usize,
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
            globals: HashMap::new(),
            globals_size: 0,
            print_ir: false,
            lambda_counter: 0,
        }
    }
}

impl JIT {
    /// Compile our AST into native code.
    /// Returns (code_ptr, globals_size).
    pub fn compile(&mut self, decls: &DeclTable) -> Result<(*const u8, usize), String> {
        // First, declare all global variables.
        self.declare_globals(decls);

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

        Ok((code, self.globals_size))
    }

    fn compile_function(
        &mut self,
        decls: &DeclTable,
        decl: &FuncDecl,
    ) -> Result<cranelift_module::FuncId, String> {
        self.ctx.func.signature = fn_sig(&self.module,
            decl.domain(),
            decl.ret);

        // Add globals base pointer as first parameter to all functions.
        self.ctx.func.signature.params.insert(0, AbiParam::new(I64));

        // Declare the function first, before generating its body.
        // This ensures that any functions called within the body get
        // different func IDs than the function being compiled.
        let id = self
            .module
            .declare_function(&*decl.name, Linkage::Export, &self.ctx.func.signature)
            .map_err(|e| e.to_string())?;

        // All functions now receive globals pointer as first parameter.
        let (called_functions, pending_lambdas) = self.function_body(decls, decl, true);

        // Print the generated IR
        if self.print_ir {
            println!("Generated IR for \"{}\":\n{}", decl.name, self.ctx.func.display());
        }

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

        // Compile lambda functions extracted from this function's body.
        for lambda_decl in pending_lambdas {
            self.compile_function(decls, &lambda_decl)?;
        }

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

    fn declare_globals(&mut self, decls: &DeclTable) {
        let mut offset: i32 = 0;
        for decl in &decls.decls {
            if let Decl::Global { name, ty } = decl {
                self.globals.insert(*name, offset);
                offset += ty.size(decls) as i32;
            }
        }
        self.globals_size = offset as usize;
    }

    fn function_body(&mut self, decls: &DeclTable, decl: &FuncDecl, has_globals: bool) -> (HashSet<Name>, Vec<FuncDecl>) {
        // Translate into cranelift IR.
        // Create the builder to build a function.
        let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_context);

        // Create the entry block, to start emitting code in.
        let entry_block = builder.create_block();

        // Add block parameters for function parameters.
        builder.append_block_params_for_function_params(entry_block);

        builder.switch_to_block(entry_block);
        builder.seal_block(entry_block);

        // Get the block parameters (function arguments).
        let block_params: Vec<_> = builder.block_params(entry_block).to_vec();

        // Track current parameter index.
        let mut param_idx = 0;

        // If this function has globals, the first block param is the globals base pointer.
        let globals_base = if has_globals {
            let base = block_params[param_idx];
            param_idx += 1;
            Some(base)
        } else {
            None
        };

        // If return type is a pointer type, first param (after globals) is output pointer.
        let output_ptr = if returns_via_pointer(decl.ret) {
            let ptr = block_params[param_idx];
            param_idx += 1;
            Some(ptr)
        } else {
            None
        };

        let mut trans = FunctionTranslator::new(builder, &mut self.module, &self.globals, globals_base, output_ptr, &mut self.lambda_counter);

        // Add variables for the function parameters and define them with block param values.
        for (i, param) in decl.params.iter().enumerate() {
            let ty = param.ty.expect("expected type").cranelift_type();
            let var = trans.declare_variable(&param.name, ty);
            trans.builder.def_var(var, block_params[i + param_idx]);
            // Function parameters are like let bindings - they hold values directly.
            trans.let_bindings.insert(param.name.to_string());
        }

        let result = trans.translate_fn(decl, decls);

        // Need a return instruction at the end of the function's block.
        // Skip if the block is already unreachable (e.g., body ended with explicit return).
        if !trans.builder.is_unreachable() {
            if returns_via_pointer(decl.ret) {
                // Copy result to output pointer and return void.
                let output = trans.output_ptr.unwrap();
                let size = decl.ret.size(decls) as i64;
                let size_val = trans.builder.ins().iconst(I64, size);
                // Use memcpy to copy the data.
                trans.builder.call_memcpy(trans.module.target_config(), output, result, size_val);
                trans.builder.ins().return_(&[]);
            } else if *decl.ret == crate::Type::Void {
                trans.builder.ins().return_(&[]);
            } else {
                trans.builder.ins().return_(&[result]);
            }
        }

        // Indicate we're finished with the function.
        trans.builder.finalize();

        let called = trans.called_functions;
        let pending_lambdas = trans.pending_lambdas;

        let flags = settings::Flags::new(settings::builder());
        let res = verify_function(&self.ctx.func, &flags);
        // println!("{}", self.ctx.func.display());
        if let Err(errors) = res {
            panic!("{}", errors);
        }

        (called, pending_lambdas)
    }
}

impl crate::Type {
    fn cranelift_type(&self) -> Type {
        match self {
            crate::Type::Void => I32,
            crate::Type::Int8 => I8,
            crate::Type::UInt8 => I8,
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
            crate::Type::Var(_) => panic!("type variable should not be seen by codegen!"),
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

/// Returns true if this type is returned via an output pointer parameter.
fn returns_via_pointer(ty: crate::TypeID) -> bool {
    ty.is_ptr()
}

fn fn_sig(module: &JITModule, from: crate::TypeID, to: crate::TypeID) -> Signature {
    let mut sig = module.make_signature();

    // If return type is a pointer type (array, struct, tuple), add output pointer as first param.
    if returns_via_pointer(to) {
        sig.params.push(AbiParam::new(I64));
    }

    if let crate::Type::Tuple(args) = &*from {
        for t in args.iter() {
            sig.params.push(AbiParam::new(t.cranelift_type()));
        }
    } else {
        panic!();
    }

    // Only add return value if not void and not returned via pointer.
    if *to != crate::Type::Void && !returns_via_pointer(to) {
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

    /// Lambda functions extracted from this function body, to be compiled afterward.
    pending_lambdas: Vec<FuncDecl>,

    /// Tracks which variables are `let` bindings (hold values directly)
    /// vs `var` bindings (hold pointers to stack slots).
    let_bindings: HashSet<String>,

    /// Global variable offsets from base pointer.
    globals: &'a HashMap<Name, i32>,

    /// Base pointer for globals (passed as first param to main).
    globals_base: Option<Value>,

    /// Counter for generating unique lambda names.
    lambda_counter: &'a mut usize,

    /// Output pointer for functions returning via pointer (arrays, structs, tuples).
    output_ptr: Option<Value>,
}

impl<'a> FunctionTranslator<'a> {
    fn new(
        builder: FunctionBuilder<'a>,
        module: &'a mut JITModule,
        globals: &'a HashMap<Name, i32>,
        globals_base: Option<Value>,
        output_ptr: Option<Value>,
        lambda_counter: &'a mut usize,
    ) -> Self {
        Self {
            builder,
            variables: HashMap::new(),
            module,
            next_index: 0,
            current_instance: Instance::new(),
            called_functions: HashSet::new(),
            pending_lambdas: Vec::new(),
            let_bindings: HashSet::new(),
            globals,
            globals_base,
            output_ptr,
            lambda_counter,
        }
    }

    fn translate_fn(&mut self, decl: &FuncDecl, decls: &DeclTable) -> Value {
        self.translate_expr(decl.body.unwrap(), decl, decls)
    }

    fn translate_lvalue(&mut self, expr: ExprID, decl: &FuncDecl, decls: &DeclTable) -> Value {
        match &decl.arena[expr] {
            Expr::Id(name) => {
                if let Some(variable) = self.variables.get(&**name) {
                    self.builder.use_var(*variable)
                } else if let Some(&offset) = self.globals.get(name) {
                    // Global variable - compute address from base + offset.
                    let base = self.globals_base.expect("globals_base not set");
                    self.builder.ins().iadd_imm(base, offset as i64)
                } else {
                    panic!("unknown lvalue: {:?}", name);
                }
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
                // For other expressions (e.g. type ascription), evaluate and
                // treat the result as an address.
                self.translate_expr(expr, decl, decls)
            }
        }
    }

    fn translate_expr(&mut self, expr: ExprID, decl: &FuncDecl, decls: &DeclTable) -> Value {
        match &decl.arena[expr] {
            Expr::False => self.builder.ins().iconst(I8, 0),
            Expr::True => self.builder.ins().iconst(I8, 1),
            Expr::Int(imm) => self.builder.ins().iconst(I32, *imm),
            Expr::Real(s) => {
                let val: f64 = s.parse().expect("invalid float literal");
                self.builder.ins().f64const(val)
            }
            Expr::Char(c) => self.builder.ins().iconst(I8, *c as i64),
            Expr::Id(name) => {
                let ty = &decl.types[expr];
                if let Some(variable) = self.variables.get(&**name) {
                    let val = self.builder.use_var(*variable);
                    // Let bindings hold values directly; var bindings hold pointers
                    if self.let_bindings.contains(&**name) || ty.is_ptr() {
                        val
                    } else {
                        self.builder
                            .ins()
                            .load(ty.cranelift_type(), MemFlags::new(), val, 0)
                    }
                } else if let Some(&offset) = self.globals.get(name) {
                    // Global variable - load from base + offset.
                    let base = self.globals_base.expect("globals_base not set");
                    let addr = self.builder.ins().iadd_imm(base, offset as i64);
                    self.builder.ins().load(ty.cranelift_type(), MemFlags::new(), addr, 0)
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

                if let crate::Type::Func(from, to) = *(decl.types[*fn_id]) {
                    let mut args = vec![];

                    // Pass globals pointer as first argument to all user-defined functions.
                    // Check if this is a user-defined function (not a builtin like assert/print).
                    let is_user_func = if let Expr::Id(name) = &decl.arena[*fn_id] {
                        *name != Name::str("assert") && *name != Name::str("print")
                    } else {
                        true // Assume indirect calls are to user functions
                    };

                    if is_user_func {
                        args.push(self.globals_base.expect("globals_base not set"));
                    }

                    // If return type is pointer, allocate stack space and pass as first arg.
                    let output_slot = if returns_via_pointer(to) {
                        let size = to.size(decls) as u32;
                        let slot = self.builder.create_sized_stack_slot(StackSlotData {
                            kind: StackSlotKind::ExplicitSlot,
                            size,
                            align_shift: 0,
                            key: None,
                        });
                        let addr = self.builder.ins().stack_addr(I64, slot, 0);
                        args.push(addr);
                        Some(addr)
                    } else {
                        None
                    };

                    for arg_id in arg_ids {
                        args.push(self.translate_expr(*arg_id, decl, decls))
                    }

                    let mut sig = fn_sig(&self.module, from, to);
                    // Add globals pointer parameter for user-defined functions.
                    if is_user_func {
                        sig.params.insert(0, AbiParam::new(I64));
                    }
                    let sref = self.builder.import_signature(sig);
                    let call = self.builder.ins().call_indirect(sref, f, &args);

                    if let Some(addr) = output_slot {
                        // Return the address of the output buffer.
                        addr
                    } else if let Some(result) = self.builder.inst_results(call).first() {
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
                self.let_bindings.insert(name.to_string());
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
                    key: None,
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
                // Handle array.len - return the compile-time length.
                if let crate::Type::Array(_, len) = &*lhs_ty {
                    if **name == "len" {
                        return self.builder.ins().iconst(I32, *len as i64);
                    }
                }
                let lhs_val = self.translate_expr(*lhs, decl, decls);
                if let crate::Type::Name(struct_name, _) = &*lhs_ty {
                    let struct_decl = decls.find(*struct_name);
                    if let crate::Decl::Struct(s) = &struct_decl[0] {
                        let off = s.field_offset(name, decls, &self.current_instance);
                        let field_ty = &decl.types[expr];
                        // Arrays are stored inline, so return the address of the field
                        if field_ty.is_ptr() {
                            let off_val = self.builder.ins().iconst(I64, off as i64);
                            self.builder.ins().iadd(lhs_val, off_val)
                        } else {
                            let load_ty = field_ty.cranelift_type();
                            self.builder
                                .ins()
                                .load(load_ty, MemFlags::new(), lhs_val, off)
                        }
                    } else {
                        panic!("unknown struct. should be caught by checker");
                    }
                } else if let crate::Type::Tuple(elem_types) = &*lhs_ty {
                    // Tuple field access: x.0, x.1, etc.
                    let index: usize = name.parse().expect("tuple field should be numeric");
                    let mut off = 0i32;
                    for i in 0..index {
                        off += elem_types[i].size(decls) as i32;
                    }
                    let field_ty = &decl.types[expr];
                    if field_ty.is_ptr() {
                        let off_val = self.builder.ins().iconst(I64, off as i64);
                        self.builder.ins().iadd(lhs_val, off_val)
                    } else {
                        let load_ty = field_ty.cranelift_type();
                        self.builder
                            .ins()
                            .load(load_ty, MemFlags::new(), lhs_val, off)
                    }
                } else {
                    panic!("lhs of field expression is not a struct or tuple. should be caught by checker");
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

                if let crate::Type::Array(elem_ty, _) = &*ty {
                    let element_size = elem_ty.size(decls) as u32;
                    let total_size = element_size * elements.len() as u32;

                    // Allocate a new stack slot with a size of the array.
                    let slot = self.builder.create_sized_stack_slot(StackSlotData {
                        kind: StackSlotKind::ExplicitSlot,
                        size: total_size,
                        align_shift: 0,
                        key: None,
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
                if exprs.is_empty() {
                    self.builder.ins().iconst(I32, 0)
                } else {
                    let mut result = None;
                    for expr in exprs {
                        result = Some(self.translate_expr(*expr, decl, decls));
                    }
                    result.unwrap()
                }
            }
            Expr::If(cond_id, then_id, else_id) => {
                let cond_val = self.translate_expr(*cond_id, decl, decls);

                let then_block = self.builder.create_block();
                let else_block = self.builder.create_block();
                let merge_block = self.builder.create_block();

                // Branch based on condition.
                self.builder.ins().brif(cond_val, then_block, &[], else_block, &[]);

                // Then block.
                self.builder.switch_to_block(then_block);
                self.builder.seal_block(then_block);
                let _then_val = self.translate_expr(*then_id, decl, decls);
                // Only jump if block is not already terminated (e.g., by a return).
                if !self.builder.is_unreachable() {
                    self.builder.ins().jump(merge_block, &[]);
                }

                // Else block.
                self.builder.switch_to_block(else_block);
                self.builder.seal_block(else_block);
                let _else_val = if let Some(else_expr_id) = else_id {
                    self.translate_expr(*else_expr_id, decl, decls)
                } else {
                    self.builder.ins().iconst(I32, 0)
                };
                // Only jump if block is not already terminated (e.g., by a return).
                if !self.builder.is_unreachable() {
                    self.builder.ins().jump(merge_block, &[]);
                }

                // Merge block - continue execution here.
                self.builder.switch_to_block(merge_block);
                self.builder.seal_block(merge_block);

                // Return a dummy value since if-as-statement doesn't produce a value.
                // If we need if-as-expression, we'd use block parameters.
                self.builder.ins().iconst(I32, 0)
            }
            Expr::For { var, start, end, body } => {
                // Evaluate start and end values.
                let start_val = self.translate_expr(*start, decl, decls);
                let end_val = self.translate_expr(*end, decl, decls);

                // Create a variable for the loop counter.
                let loop_var = self.declare_variable(var, I32);
                self.builder.def_var(loop_var, start_val);
                self.let_bindings.insert(var.to_string());

                // Create blocks for header, body, and exit.
                let header_block = self.builder.create_block();
                let body_block = self.builder.create_block();
                let exit_block = self.builder.create_block();

                // Jump to header.
                self.builder.ins().jump(header_block, &[]);

                // Header block: check condition (loop_var < end).
                self.builder.switch_to_block(header_block);
                let current_val = self.builder.use_var(loop_var);
                let cond = self.builder.ins().icmp(IntCC::SignedLessThan, current_val, end_val);
                self.builder.ins().brif(cond, body_block, &[], exit_block, &[]);

                // Body block.
                self.builder.switch_to_block(body_block);
                self.builder.seal_block(body_block);
                self.translate_expr(*body, decl, decls);

                // Increment loop variable.
                let current_val = self.builder.use_var(loop_var);
                let incremented = self.builder.ins().iadd_imm(current_val, 1);
                self.builder.def_var(loop_var, incremented);

                // Jump back to header.
                self.builder.ins().jump(header_block, &[]);

                // Seal header after all predecessors are known.
                self.builder.seal_block(header_block);

                // Exit block.
                self.builder.switch_to_block(exit_block);
                self.builder.seal_block(exit_block);

                self.builder.ins().iconst(I32, 0)
            }
            Expr::Return(expr_id) => {
                let result = self.translate_expr(*expr_id, decl, decls);
                let ret_ty = decl.types[*expr_id];

                if returns_via_pointer(ret_ty) {
                    // Copy result to output pointer and return void.
                    let output = self.output_ptr.expect("output_ptr not set for pointer return");
                    let size = ret_ty.size(decls) as i64;
                    let size_val = self.builder.ins().iconst(I64, size);
                    self.builder.call_memcpy(self.module.target_config(), output, result, size_val);
                    self.builder.ins().return_(&[]);
                } else {
                    self.builder.ins().return_(&[result]);
                }

                // Create an unreachable block for any code after return.
                let unreachable_block = self.builder.create_block();
                self.builder.switch_to_block(unreachable_block);
                self.builder.seal_block(unreachable_block);
                // Create a dummy value before adding trap (trap terminates the block).
                let dummy = self.builder.ins().iconst(I32, 0);
                // Add a trap - this code is unreachable.
                self.builder.ins().trap(TrapCode::user(1).unwrap());
                dummy
            }
            Expr::While(cond_id, body_id) => {
                // Create blocks for header, body, and exit.
                let header_block = self.builder.create_block();
                let body_block = self.builder.create_block();
                let exit_block = self.builder.create_block();

                // Jump to header.
                self.builder.ins().jump(header_block, &[]);

                // Header block: evaluate condition.
                self.builder.switch_to_block(header_block);
                let cond_val = self.translate_expr(*cond_id, decl, decls);
                self.builder.ins().brif(cond_val, body_block, &[], exit_block, &[]);

                // Body block.
                self.builder.switch_to_block(body_block);
                self.builder.seal_block(body_block);
                self.translate_expr(*body_id, decl, decls);

                // Jump back to header.
                self.builder.ins().jump(header_block, &[]);

                // Seal header after all predecessors are known.
                self.builder.seal_block(header_block);

                // Exit block.
                self.builder.switch_to_block(exit_block);
                self.builder.seal_block(exit_block);

                self.builder.ins().iconst(I32, 0)
            }
            Expr::Tuple(elements) => {
                // Evaluate all element expressions first.
                let element_values: Vec<Value> = elements
                    .iter()
                    .map(|e| self.translate_expr(*e, decl, decls))
                    .collect();

                let ty = decl.types[expr];

                if let crate::Type::Tuple(elem_types) = &*ty {
                    // Calculate total size.
                    let total_size: u32 = elem_types.iter().map(|t| t.size(decls) as u32).sum();

                    // Allocate a stack slot for the tuple.
                    let slot = self.builder.create_sized_stack_slot(StackSlotData {
                        kind: StackSlotKind::ExplicitSlot,
                        size: total_size,
                        align_shift: 0,
                        key: None,
                    });

                    // Get address of the stack slot.
                    let addr = self.builder.ins().stack_addr(I64, slot, 0);

                    // Store each element at its offset.
                    let mut offset = 0i32;
                    for (i, value) in element_values.iter().enumerate() {
                        self.builder.ins().stack_store(*value, slot, offset);
                        offset += elem_types[i].size(decls) as i32;
                    }

                    addr
                } else {
                    panic!("tuple expression not of tuple type");
                }
            }
            Expr::Lambda { params, body } => {
                let lambda_ty = decl.types[expr];
                if let crate::Type::Func(dom, rng) = *lambda_ty {
                    if let crate::Type::Tuple(param_types) = &*dom {
                        let id = *self.lambda_counter;
                        *self.lambda_counter += 1;
                        let lambda_name = Name::new(format!("__lambda_{}", id));

                        let lambda_params: Vec<Param> = params.iter().zip(param_types.iter())
                            .map(|(p, ty)| Param { name: p.name, ty: Some(*ty) })
                            .collect();

                        let lambda_decl = FuncDecl {
                            name: lambda_name,
                            typevars: vec![],
                            params: lambda_params,
                            body: Some(*body),
                            ret: rng,
                            constraints: vec![],
                            loc: decl.loc,
                            arena: decl.arena.clone(),
                            types: decl.types.clone(),
                        };

                        self.pending_lambdas.push(lambda_decl);

                        let mut sig = fn_sig(&self.module, dom, rng);
                        sig.params.insert(0, AbiParam::new(I64));
                        let callee = self.module
                            .declare_function(&*lambda_name, Linkage::Export, &sig)
                            .expect("problem declaring lambda");
                        let local_callee = self.module.declare_func_in_func(callee, self.builder.func);
                        self.builder.ins().func_addr(I64, local_callee)
                    } else {
                        panic!("lambda domain should be a tuple type");
                    }
                } else {
                    panic!("lambda expression should have function type");
                }
            }
            Expr::UInt(n) => self.builder.ins().iconst(I32, *n as i64),
            Expr::AsTy(expr_id, target_ty) => {
                let val = self.translate_expr(*expr_id, decl, decls);
                let src_ty = decl.types[*expr_id];
                match (&*src_ty, &**target_ty) {
                    (crate::Type::Int32, crate::Type::Float32) => {
                        self.builder.ins().fcvt_from_sint(F32, val)
                    }
                    (crate::Type::UInt32, crate::Type::Float32) => {
                        self.builder.ins().fcvt_from_uint(F32, val)
                    }
                    (crate::Type::Float32, crate::Type::Int32) => {
                        self.builder.ins().fcvt_to_sint_sat(I32, val)
                    }
                    (crate::Type::Float32, crate::Type::UInt32) => {
                        self.builder.ins().fcvt_to_uint_sat(I32, val)
                    }
                    (crate::Type::Int32, crate::Type::Float64) => {
                        self.builder.ins().fcvt_from_sint(F64, val)
                    }
                    (crate::Type::UInt32, crate::Type::Float64) => {
                        self.builder.ins().fcvt_from_uint(F64, val)
                    }
                    (crate::Type::Float64, crate::Type::Int32) => {
                        self.builder.ins().fcvt_to_sint_sat(I32, val)
                    }
                    (crate::Type::Float64, crate::Type::UInt32) => {
                        self.builder.ins().fcvt_to_uint_sat(I32, val)
                    }
                    (crate::Type::Float32, crate::Type::Float64) => {
                        self.builder.ins().fpromote(F64, val)
                    }
                    (crate::Type::Float64, crate::Type::Float32) => {
                        self.builder.ins().fdemote(F32, val)
                    }
                    _ => val,
                }
            }
            Expr::Assign(name, value_id) => {
                let value = self.translate_expr(*value_id, decl, decls);
                let ty = decl.types[*value_id];
                if let Some(&var) = self.variables.get(&**name) {
                    if !self.let_bindings.contains(&**name) {
                        let addr = self.builder.use_var(var);
                        self.gen_copy(ty, addr, value, decls);
                    }
                } else if let Some(&offset) = self.globals.get(name) {
                    let base = self.globals_base.expect("globals_base not set");
                    let addr = self.builder.ins().iadd_imm(base, offset as i64);
                    self.gen_copy(ty, addr, value, decls);
                }
                value
            }
            Expr::Enum(case_name) => {
                if let crate::Type::Name(enum_name, _) = &*decl.types[expr] {
                    let enum_decls = decls.find(*enum_name);
                    if let Some(crate::Decl::Enum { cases, .. }) = enum_decls.iter().find(|d| matches!(d, crate::Decl::Enum { .. })) {
                        let index = cases.iter().position(|c| c == case_name).unwrap_or(0);
                        self.builder.ins().iconst(I32, index as i64)
                    } else {
                        self.builder.ins().iconst(I32, 0)
                    }
                } else {
                    self.builder.ins().iconst(I32, 0)
                }
            }
            Expr::Arena(block_id) => self.translate_expr(*block_id, decl, decls),
            _ => {
                panic!("unimplemented expression: {:?}", &decl.arena[expr]);
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
            let align = if size >= 4 { 4u8 } else { 1u8 };
            self.builder.emit_small_memory_copy(
                self.module.isa().frontend_config(),
                dst,
                src,
                size,
                align,
                align,
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
            let align = std::num::NonZeroU8::new(if size >= 4 { 4 } else { 1 }).unwrap();
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
                _ => self.builder.ins().icmp(IntCC::Equal, dst, src),
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
                    _ => {
                        // For pointer types (structs, tuples, arrays): negate memcmp result.
                        let eq = self.gen_eq(t, lhs, rhs, decls);
                        self.builder.ins().bxor_imm(eq, 1)
                    }
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
            Binop::Leq => {
                let lhs = self.translate_expr(lhs_id, decl, decls);
                let rhs = self.translate_expr(rhs_id, decl, decls);
                let t = decl.types[lhs_id];

                match *t {
                    crate::types::Type::Int32 | crate::types::Type::Int8 => {
                        self.builder.ins().icmp(IntCC::SignedLessThanOrEqual, lhs, rhs)
                    }
                    crate::types::Type::UInt32 | crate::types::Type::UInt8 => {
                        self.builder.ins().icmp(IntCC::UnsignedLessThanOrEqual, lhs, rhs)
                    }
                    crate::types::Type::Float32 | crate::types::Type::Float64 => {
                        self.builder.ins().fcmp(FloatCC::LessThanOrEqual, lhs, rhs)
                    }
                    _ => todo!(),
                }
            }
            Binop::Geq => {
                let lhs = self.translate_expr(lhs_id, decl, decls);
                let rhs = self.translate_expr(rhs_id, decl, decls);
                let t = decl.types[lhs_id];

                match *t {
                    crate::types::Type::Int32 | crate::types::Type::Int8 => {
                        self.builder.ins().icmp(IntCC::SignedGreaterThanOrEqual, lhs, rhs)
                    }
                    crate::types::Type::UInt32 | crate::types::Type::UInt8 => {
                        self.builder.ins().icmp(IntCC::UnsignedGreaterThanOrEqual, lhs, rhs)
                    }
                    crate::types::Type::Float32 | crate::types::Type::Float64 => {
                        self.builder.ins().fcmp(FloatCC::GreaterThanOrEqual, lhs, rhs)
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
            let mut sig = fn_sig(&self.module, *dom, *rng);
            // Add globals pointer as first parameter for user-defined functions.
            sig.params.insert(0, AbiParam::new(I64));
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
        if !self.variables.contains_key(name) {
            let var = self.builder.declare_var(ty);
            self.variables.insert(name.into(), var);
            self.next_index += 1;
            var
        } else {
            *self.variables.get(name).unwrap()
        }
    }
}

extern "C" fn lyte_assert(val: i8) {
    println!("assert({})", val != 0);
    assert!(val != 0);
}

extern "C" fn lyte_print_i32(val: i32) {
    println!("{}", val);
}

extern "C" fn lyte_print_i64(val: i64) {
    println!("{}", val);
}
