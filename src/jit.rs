// Pulled from https://github.com/bytecodealliance/cranelift-jit-demo

use crate::decl::*;
use crate::defs::*;
use crate::expr::*;
use crate::DeclTable;
use crate::Instance;
extern crate cranelift_codegen;
use core::panic;
use cranelift::codegen::{self, settings};
use cranelift::prelude::isa::CallConv;
use cranelift::prelude::types::*;
use cranelift::prelude::*;
use cranelift_codegen::verifier::verify_function;
use cranelift_jit::{JITBuilder, JITModule};
use cranelift_module::{Linkage, Module};
use std::collections::{HashMap, HashSet};
use std::vec;

// Opaque jmp_buf: 256 bytes (32 * u64), 8-byte aligned.
// macOS arm64 jmp_buf ≤ 192 bytes; macOS x86_64 ≤ 148 bytes.
#[repr(C, align(8))]
struct JmpBuf([u64; 32]);

extern "C" {
    fn setjmp(env: *mut u8) -> i32;
    fn longjmp(env: *mut u8, val: i32) -> !;
}

/// Bytes reserved at the start of the globals buffer for the runtime header:
///   offset 0: cancel flag (u8) — write non-zero to cancel running JIT code
///   offset 8..(8 + size_of::<JmpBuf>): the longjmp target
///
/// User globals start at `CANCEL_FLAG_RESERVED`.
/// The jmp_buf starts at offset 8 so it is naturally aligned.
pub const CANCEL_FLAG_RESERVED: i32 = 8 + std::mem::size_of::<JmpBuf>() as i32;

/// Called from JIT-generated code when `globals[0]` (the cancel flag) is non-zero.
/// Receives the globals base pointer and longjmps to the checkpoint stored at `globals[8]`.
unsafe extern "C" fn lyte_abort(globals: *mut u8) {
    longjmp(globals.add(8), 1);
}

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
        flag_builder.set("opt_level", "speed").unwrap();
        let isa_builder = cranelift_native::builder().unwrap_or_else(|msg| {
            panic!("host machine is not supported: {}", msg);
        });
        let isa = isa_builder
            .finish(settings::Flags::new(flag_builder))
            .unwrap();
        let mut builder = JITBuilder::with_isa(isa, cranelift_module::default_libcall_names());
        builder.symbol("__lyte_abort", lyte_abort as *const u8);
        register_math_symbols(&mut builder);

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
            return Err("no main function found".to_string());
        }

        // Find the main function.
        let main_decl = if let Decl::Func(d) = &main_decls[0] {
            d
        } else {
            return Err("main is not a function".to_string());
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
        self.ctx.func.signature = fn_sig(&self.module, decl.domain(), decl.ret);

        // Add globals base pointer as first parameter to all functions.
        self.ctx.func.signature.params.insert(0, AbiParam::new(I64));
        // Add closure pointer as second parameter (null for non-closures).
        self.ctx.func.signature.params.insert(1, AbiParam::new(I64));

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
            println!(
                "Generated IR for \"{}\":\n{}",
                decl.name,
                self.ctx.func.display()
            );
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

            let found = decls.find(name);
            assert!(!found.is_empty(), "called function '{}' not found", name);
            let decl = if let Decl::Func(d) = &found[0] {
                d
            } else {
                panic!()
            };

            // Skip builtins — they have no body and are called via raw function pointers.
            if decl.body.is_none() {
                continue;
            }

            self.compile_function(decls, decl)?;
        }

        Ok(id)
    }

    fn declare_globals(&mut self, decls: &DeclTable) {
        // Reserve the first CANCEL_FLAG_RESERVED bytes for the cancel flag at offset 0.
        let mut offset: i32 = CANCEL_FLAG_RESERVED;
        for decl in &decls.decls {
            if let Decl::Global { name, ty } = decl {
                self.globals.insert(*name, offset);
                offset += ty.size(decls) as i32;
            }
        }
        self.globals_size = offset as usize;
    }

    fn function_body(
        &mut self,
        decls: &DeclTable,
        decl: &FuncDecl,
        has_globals: bool,
    ) -> (HashSet<Name>, Vec<FuncDecl>) {
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

        // Second block param (for user functions) is the closure pointer.
        // It points to a contiguous array of i64 slots, one per closure_var.
        let closure_ptr = if has_globals {
            let ptr = block_params[param_idx];
            param_idx += 1;
            Some(ptr)
        } else {
            None
        };

        // If return type is a pointer type, first param (after globals+closure) is output pointer.
        let output_ptr = if returns_via_pointer(decl.ret) {
            let ptr = block_params[param_idx];
            param_idx += 1;
            Some(ptr)
        } else {
            None
        };

        let mut trans = FunctionTranslator::new(
            builder,
            &mut self.module,
            &self.globals,
            globals_base,
            output_ptr,
            &mut self.lambda_counter,
        );

        // Set up captured variables from the closure pointer.
        if let Some(closure_ptr_val) = closure_ptr {
            for (i, cv) in decl.closure_vars.iter().enumerate() {
                // Each slot in the closure struct holds a pointer to the captured variable's storage.
                let var_ptr =
                    trans
                        .builder
                        .ins()
                        .load(I64, MemFlags::new(), closure_ptr_val, (i * 8) as i32);
                let var = trans.declare_variable(&cv.name.to_string(), I64);
                trans.builder.def_var(var, var_ptr);
                // NOT added to let_bindings → acts as a var binding (accessed through the pointer).
            }
        }

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
                trans
                    .builder
                    .call_memcpy(trans.module.target_config(), output, result, size_val);
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
            panic!("cranelift IR verification failed:\n{}", errors);
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

            // Pointer to start of array.
            crate::Type::Array(_, _) => I64,

            // Slice is a pointer to a fat pointer {data_ptr, len}.
            crate::Type::Slice(_) => I64,

            // Tuple is a pointer.
            crate::Type::Tuple(_) => I64,

            crate::Type::Anon(_) => panic!("anonymous type should have been solved for!"),
            crate::Type::Var(_) => panic!("type variable should not be seen by codegen!"),
        }
    }
}

/// Returns true if this type is returned via an output pointer parameter.
fn returns_via_pointer(ty: crate::TypeID) -> bool {
    ty.is_ptr()
}

/// Returns true if the type is a slice.
fn is_slice(ty: crate::TypeID) -> bool {
    matches!(&*ty, crate::Type::Slice(_))
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
        panic!("fn_sig: expected Tuple for `from` type, got {:?}", from);
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
        self.emit_cancel_check();
        self.translate_expr(decl.body.unwrap(), decl, decls)
    }

    /// Emit a cancellation check at the current position.
    ///
    /// Reads `globals[0]` (the cancel flag).  If it is non-zero, calls
    /// `lyte_abort(globals_base)` which longjmps to the checkpoint stored at
    /// `globals[8]`, immediately unwinding the entire JIT call stack.
    /// Falls through into a fresh `continue_block` when the flag is clear.
    fn emit_cancel_check(&mut self) {
        let cancel_block = self.builder.create_block();
        let continue_block = self.builder.create_block();

        let base = self.globals_base.expect("globals_base not set");
        let flag = self.builder.ins().load(I8, MemFlags::trusted(), base, 0);
        let is_set = self.builder.ins().icmp_imm(IntCC::NotEqual, flag, 0);
        self.builder
            .ins()
            .brif(is_set, cancel_block, &[], continue_block, &[]);

        self.builder.switch_to_block(cancel_block);
        self.builder.seal_block(cancel_block);
        let fn_ptr = self
            .builder
            .ins()
            .iconst(I64, lyte_abort as *const () as usize as i64);
        let mut sig = self.module.make_signature();
        sig.params.push(AbiParam::new(I64)); // globals_base
        let sref = self.builder.import_signature(sig);
        self.builder.ins().call_indirect(sref, fn_ptr, &[base]);
        self.builder.ins().trap(TrapCode::user(1).unwrap()); // unreachable after longjmp

        self.builder.switch_to_block(continue_block);
        self.builder.seal_block(continue_block);
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
                    panic!(
                        "JIT: unknown lvalue variable {:?} (not local or global)",
                        name
                    );
                }
            }
            Expr::Field(lhs, name) => {
                let lhs_ty = decl.types[*lhs];
                let lhs_value = self.translate_lvalue(*lhs, decl, decls);
                if let crate::Type::Name(struct_name, type_args) = &*lhs_ty {
                    let struct_decl = decls.find(*struct_name);
                    if let crate::Decl::Struct(s) = &struct_decl[0] {
                        let inst: crate::Instance = s
                            .typevars
                            .iter()
                            .zip(type_args.iter())
                            .map(|(tv, ty)| (crate::types::mk_type(crate::Type::Var(*tv)), *ty))
                            .collect();
                        let off = s.field_offset(name, decls, &inst);
                        let off_value = self.builder.ins().iconst(I64, off as i64);
                        self.builder.ins().iadd(lhs_value, off_value)
                    } else {
                        panic!(
                            "JIT lvalue field: expected struct decl for {:?}, got {:?}",
                            struct_name, struct_decl[0]
                        );
                    }
                } else {
                    panic!("JIT lvalue field: expected struct type, got {:?}", lhs_ty);
                }
            }
            Expr::ArrayIndex(lhs, rhs) => {
                let lhs_ty = decl.types[*lhs];
                let lhs_val = self.translate_lvalue(*lhs, decl, decls);
                let rhs_val = self.translate_expr(*rhs, decl, decls);
                let (elem_ty, is_sl) = match &*lhs_ty {
                    crate::Type::Array(ty, _) => (*ty, false),
                    crate::Type::Slice(ty) => (*ty, true),
                    _ => panic!(
                        "JIT lvalue subscript: expected array or slice type, got {:?}",
                        lhs_ty
                    ),
                };
                let data_ptr = if is_sl {
                    self.builder.ins().load(I64, MemFlags::new(), lhs_val, 0)
                } else {
                    lhs_val
                };
                let off = self
                    .builder
                    .ins()
                    .imul_imm(rhs_val, elem_ty.size(decls) as i64);
                let off = self.builder.ins().uextend(I64, off);
                self.builder.ins().iadd(data_ptr, off)
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
                match &*decl.types[expr] {
                    crate::Type::Float32 => self.builder.ins().f32const(val as f32),
                    _ => self.builder.ins().f64const(val),
                }
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
                    self.builder
                        .ins()
                        .load(ty.cranelift_type(), MemFlags::new(), addr, 0)
                } else {
                    self.translate_func(name, &*ty)
                }
            }
            Expr::Binop(op, lhs_id, rhs_id) => {
                self.translate_binop(*op, *lhs_id, *rhs_id, decl, decls)
            }
            Expr::Unop(op, arg_id) => self.translate_unop(*op, *arg_id, decl, decls),
            Expr::Call(fn_id, arg_ids) => {
                // Determine if this is a builtin (assert/print) which has a raw fn_ptr
                // and no globals/closure parameters, vs a user function with a fat pointer.
                let is_builtin = if let Expr::Id(name) = &decl.arena[*fn_id] {
                    is_builtin_name(name)
                } else {
                    false
                };

                if let crate::Type::Func(from, to) = *(decl.types[*fn_id]) {
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
                        Some(addr)
                    } else {
                        None
                    };

                    // Get the callee parameter types to detect slice params.
                    let param_types: Vec<crate::TypeID> = if let crate::Type::Tuple(pts) = &*from {
                        pts.clone()
                    } else {
                        vec![]
                    };

                    // Check if this is a math builtin that can use a direct call.
                    let math_sym = if let Expr::Id(name) = &decl.arena[*fn_id] {
                        math_builtin_symbol(name)
                    } else {
                        None
                    };

                    let call = if let Some(sym) = math_sym {
                        // Math builtin: use a direct call via declared function.
                        let sig = fn_sig(&self.module, from, to);
                        let callee = self
                            .module
                            .declare_function(sym, Linkage::Import, &sig)
                            .expect("problem declaring math builtin");
                        let local_callee =
                            self.module.declare_func_in_func(callee, self.builder.func);
                        let mut args = vec![];
                        if let Some(addr) = output_slot {
                            args.push(addr);
                        }
                        for arg_id in arg_ids {
                            args.push(self.translate_expr(*arg_id, decl, decls));
                        }
                        self.builder.ins().call(local_callee, &args)
                    } else if is_builtin {
                        // Other builtins (assert, print, putc): indirect call.
                        let f = self.translate_expr(*fn_id, decl, decls);
                        let mut args = vec![];
                        if let Some(addr) = output_slot {
                            args.push(addr);
                        }
                        for arg_id in arg_ids {
                            args.push(self.translate_expr(*arg_id, decl, decls));
                        }
                        let sig = fn_sig(&self.module, from, to);
                        let sref = self.builder.import_signature(sig);
                        self.builder.ins().call_indirect(sref, f, &args)
                    } else {
                        // User function: evaluate callee as a fat pointer {fn_ptr, closure_ptr}.
                        let fat_ptr = self.translate_expr(*fn_id, decl, decls);
                        let fn_ptr = self.builder.ins().load(I64, MemFlags::new(), fat_ptr, 0);
                        let closure_ptr = self.builder.ins().load(I64, MemFlags::new(), fat_ptr, 8);

                        let mut args = vec![
                            self.globals_base.expect("globals_base not set"),
                            closure_ptr,
                        ];
                        if let Some(addr) = output_slot {
                            args.push(addr);
                        }
                        for (i, arg_id) in arg_ids.iter().enumerate() {
                            let arg_val = self.translate_expr(*arg_id, decl, decls);
                            // If the callee expects a slice, wrap sized arrays in a fat pointer.
                            if i < param_types.len() {
                                if is_slice(param_types[i]) {
                                    args.push(self.wrap_as_slice(
                                        arg_val,
                                        decl.types[*arg_id],
                                        decls,
                                    ));
                                    continue;
                                }
                            }
                            args.push(arg_val);
                        }
                        let mut sig = fn_sig(&self.module, from, to);
                        sig.params.insert(0, AbiParam::new(I64)); // globals_base
                        sig.params.insert(1, AbiParam::new(I64)); // closure_ptr
                        let sref = self.builder.import_signature(sig);
                        self.builder.ins().call_indirect(sref, fn_ptr, &args)
                    };

                    if let Some(addr) = output_slot {
                        addr
                    } else if let Some(result) = self.builder.inst_results(call).first() {
                        *result
                    } else {
                        self.builder.ins().iconst(I32, 0)
                    }
                } else {
                    panic!(
                        "JIT call: expected function type, got {:?}",
                        decl.types[*fn_id]
                    );
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
                // Remove from let_bindings in case this var shadows a let binding
                // (e.g., a for-loop counter with the same name).
                self.let_bindings.remove(&name.to_string());
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
                // Handle array.len / slice.len.
                if **name == "len" {
                    match &*lhs_ty {
                        crate::Type::Slice(_) => {
                            let lhs_val = self.translate_expr(*lhs, decl, decls);
                            return self.builder.ins().load(I32, MemFlags::new(), lhs_val, 8);
                        }
                        crate::Type::Array(_, sz) => {
                            return self.builder.ins().iconst(I32, sz.known() as i64);
                        }
                        _ => {}
                    }
                }
                let lhs_val = self.translate_expr(*lhs, decl, decls);
                if let crate::Type::Name(struct_name, type_args) = &*lhs_ty {
                    let struct_decl = decls.find(*struct_name);
                    if let crate::Decl::Struct(s) = &struct_decl[0] {
                        let inst: crate::Instance = s
                            .typevars
                            .iter()
                            .zip(type_args.iter())
                            .map(|(tv, ty)| (crate::types::mk_type(crate::Type::Var(*tv)), *ty))
                            .collect();
                        let off = s.field_offset(name, decls, &inst);
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
                        panic!(
                            "JIT field access: no struct declaration found for {:?}",
                            struct_name
                        );
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
                    panic!(
                        "JIT field access: expected struct or tuple type, got {:?}",
                        lhs_ty
                    );
                }
            }
            Expr::ArrayIndex(lhs, rhs) => {
                let lhs_ty = decl.types[*lhs];
                let lhs_val = self.translate_expr(*lhs, decl, decls);
                let rhs_val = self.translate_expr(*rhs, decl, decls);
                let (elem_ty, is_sl) = match &*lhs_ty {
                    crate::Type::Array(ty, _) => (*ty, false),
                    crate::Type::Slice(ty) => (*ty, true),
                    _ => panic!(
                        "JIT subscript: expected array or slice type, got {:?}",
                        lhs_ty
                    ),
                };
                let data_ptr = if is_sl {
                    self.builder.ins().load(I64, MemFlags::new(), lhs_val, 0)
                } else {
                    lhs_val
                };
                let off = self
                    .builder
                    .ins()
                    .imul_imm(rhs_val, elem_ty.size(decls) as i64);
                let off = self.builder.ins().uextend(I64, off);
                let p = self.builder.ins().iadd(data_ptr, off);
                let result_ty = decl.types[expr];
                if result_ty.is_ptr() {
                    // Composite types (arrays, structs, tuples) are represented
                    // as pointers — return the address directly.
                    p
                } else {
                    let load_ty = result_ty.cranelift_type();
                    self.builder.ins().load(load_ty, MemFlags::new(), p, 0)
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
                    panic!(
                        "JIT array literal: expected array type, got {:?}",
                        decl.types[expr]
                    );
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

                // Determine if this if-else produces a value. Both branches must
                // have the same concrete (non-void) type for the result to be usable.
                let result_ty = decl.types[expr];
                let is_value = if let Some(else_expr_id) = else_id {
                    let else_ty = decl.types[*else_expr_id];
                    !matches!(
                        &*result_ty,
                        crate::Type::Void | crate::Type::Anon(_) | crate::Type::Var(_)
                    ) && result_ty == else_ty
                } else {
                    false
                };

                if is_value {
                    let cl_ty = result_ty.cranelift_type();
                    self.builder.append_block_param(merge_block, cl_ty);
                }

                // Branch based on condition.
                self.builder
                    .ins()
                    .brif(cond_val, then_block, &[], else_block, &[]);

                // Then block.
                self.builder.switch_to_block(then_block);
                self.builder.seal_block(then_block);
                let then_val = self.translate_expr(*then_id, decl, decls);
                if !self.builder.is_unreachable() {
                    if is_value {
                        self.builder.ins().jump(merge_block, &[codegen::ir::BlockArg::Value(then_val)]);
                    } else {
                        self.builder.ins().jump(merge_block, &[]);
                    }
                }

                // Else block.
                self.builder.switch_to_block(else_block);
                self.builder.seal_block(else_block);
                let else_val = if let Some(else_expr_id) = else_id {
                    self.translate_expr(*else_expr_id, decl, decls)
                } else {
                    self.builder.ins().iconst(I32, 0)
                };
                if !self.builder.is_unreachable() {
                    if is_value {
                        self.builder.ins().jump(merge_block, &[codegen::ir::BlockArg::Value(else_val)]);
                    } else {
                        self.builder.ins().jump(merge_block, &[]);
                    }
                }

                // Merge block.
                self.builder.switch_to_block(merge_block);
                self.builder.seal_block(merge_block);

                if is_value {
                    self.builder.block_params(merge_block)[0]
                } else {
                    self.builder.ins().iconst(I32, 0)
                }
            }
            Expr::For {
                var,
                start,
                end,
                body,
            } => {
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
                let cond = self
                    .builder
                    .ins()
                    .icmp(IntCC::SignedLessThan, current_val, end_val);
                self.builder
                    .ins()
                    .brif(cond, body_block, &[], exit_block, &[]);

                // Body block.
                self.builder.switch_to_block(body_block);
                self.builder.seal_block(body_block);
                self.translate_expr(*body, decl, decls);

                // Increment loop variable.
                let current_val = self.builder.use_var(loop_var);
                let incremented = self.builder.ins().iadd_imm(current_val, 1);
                self.builder.def_var(loop_var, incremented);

                // Check for cancellation before jumping back.
                if !self.builder.is_unreachable() {
                    self.emit_cancel_check();
                }

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
                    let output = self
                        .output_ptr
                        .expect("output_ptr not set for pointer return");
                    let size = ret_ty.size(decls) as i64;
                    let size_val = self.builder.ins().iconst(I64, size);
                    self.builder
                        .call_memcpy(self.module.target_config(), output, result, size_val);
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
                self.builder
                    .ins()
                    .brif(cond_val, body_block, &[], exit_block, &[]);

                // Body block.
                self.builder.switch_to_block(body_block);
                self.builder.seal_block(body_block);
                self.translate_expr(*body_id, decl, decls);

                // Check for cancellation before jumping back.
                if !self.builder.is_unreachable() {
                    self.emit_cancel_check();
                }

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
                    panic!(
                        "JIT tuple expression: expected tuple type, got {:?}",
                        decl.types[expr]
                    );
                }
            }
            Expr::Lambda { params, body } => {
                let lambda_ty = decl.types[expr];
                if let crate::Type::Func(dom, rng) = *lambda_ty {
                    if let crate::Type::Tuple(param_types) = &*dom {
                        let id = *self.lambda_counter;
                        *self.lambda_counter += 1;
                        let lambda_name = Name::new(format!("__lambda_{}", id));

                        let lambda_params: Vec<Param> = params
                            .iter()
                            .zip(param_types.iter())
                            .map(|(p, ty)| Param {
                                name: p.name,
                                ty: Some(*ty),
                            })
                            .collect();

                        // Compute free variables captured from the enclosing scope.
                        let param_names: std::collections::HashSet<String> =
                            params.iter().map(|p| p.name.to_string()).collect();
                        let free_vars = collect_free_var_names(
                            *body,
                            &decl.arena,
                            &param_names,
                            &self.variables,
                            &decl.types,
                        );

                        // Allocate a closure struct on the stack; each slot holds the address
                        // of one captured variable's storage.
                        let closure_ptr_val = if !free_vars.is_empty() {
                            let size = (free_vars.len() * 8) as u32;
                            let slot = self.builder.create_sized_stack_slot(StackSlotData {
                                kind: StackSlotKind::ExplicitSlot,
                                size,
                                align_shift: 0,
                                key: None,
                            });
                            let closure_addr = self.builder.ins().stack_addr(I64, slot, 0);
                            for (i, (name, ty)) in free_vars.iter().enumerate() {
                                let var_ptr = self.get_var_address(name, *ty, decls);
                                self.builder.ins().store(
                                    MemFlags::new(),
                                    var_ptr,
                                    closure_addr,
                                    (i * 8) as i32,
                                );
                            }
                            closure_addr
                        } else {
                            self.builder.ins().iconst(I64, 0)
                        };

                        let closure_vars: Vec<ClosureVar> = free_vars
                            .iter()
                            .map(|(name, ty)| ClosureVar {
                                name: Name::new(name.clone()),
                                ty: *ty,
                            })
                            .collect();

                        let lambda_decl = FuncDecl {
                            name: lambda_name,
                            typevars: vec![],
                            size_vars: vec![],
                            params: lambda_params,
                            body: Some(*body),
                            ret: rng,
                            constraints: vec![],
                            loc: decl.loc,
                            arena: decl.arena.clone(),
                            types: decl.types.clone(),
                            closure_vars,
                        };

                        self.pending_lambdas.push(lambda_decl);

                        let mut sig = fn_sig(&self.module, dom, rng);
                        sig.params.insert(0, AbiParam::new(I64)); // globals_base
                        sig.params.insert(1, AbiParam::new(I64)); // closure_ptr
                        let callee = self
                            .module
                            .declare_function(&*lambda_name, Linkage::Export, &sig)
                            .expect("problem declaring lambda");
                        let local_callee =
                            self.module.declare_func_in_func(callee, self.builder.func);
                        let fn_ptr = self.builder.ins().func_addr(I64, local_callee);

                        // Build a fat pointer pair {fn_ptr, closure_ptr} on the stack.
                        let pair_slot = self.builder.create_sized_stack_slot(StackSlotData {
                            kind: StackSlotKind::ExplicitSlot,
                            size: 16,
                            align_shift: 0,
                            key: None,
                        });
                        let pair_addr = self.builder.ins().stack_addr(I64, pair_slot, 0);
                        self.builder
                            .ins()
                            .store(MemFlags::new(), fn_ptr, pair_addr, 0);
                        self.builder
                            .ins()
                            .store(MemFlags::new(), closure_ptr_val, pair_addr, 8);
                        pair_addr
                    } else {
                        panic!("JIT lambda: expected tuple domain type, got {:?}", dom);
                    }
                } else {
                    panic!("JIT lambda: expected function type, got {:?}", lambda_ty);
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
                    (crate::Type::Int8, crate::Type::Int32) => self.builder.ins().sextend(I32, val),
                    (crate::Type::Int32, crate::Type::Int8) => self.builder.ins().ireduce(I8, val),
                    _ => val,
                }
            }
            Expr::Enum(case_name) => {
                let index = if let crate::Type::Name(enum_name, _) = &*decl.types[expr] {
                    let enum_decls = decls.find(*enum_name);
                    if let Some(crate::Decl::Enum { cases, .. }) = enum_decls
                        .iter()
                        .find(|d| matches!(d, crate::Decl::Enum { .. }))
                    {
                        cases.iter().position(|c| c == case_name).unwrap_or(0) as i64
                    } else {
                        0
                    }
                } else {
                    0
                };
                // Enums are Type::Name, which is a pointer type. Allocate a
                // stack slot for the i32 discriminant and return its address.
                let slot = self.builder.create_sized_stack_slot(StackSlotData {
                    kind: StackSlotKind::ExplicitSlot,
                    size: 4,
                    align_shift: 0,
                    key: None,
                });
                let addr = self.builder.ins().stack_addr(I64, slot, 0);
                let val = self.builder.ins().iconst(I32, index);
                self.builder.ins().store(MemFlags::trusted(), val, addr, 0);
                addr
            }
            Expr::Arena(block_id) => self.translate_expr(*block_id, decl, decls),
            Expr::String(s) => {
                let bytes = s.as_bytes();
                let total_size = bytes.len() as u32 + 1;
                let slot = self.builder.create_sized_stack_slot(StackSlotData {
                    kind: StackSlotKind::ExplicitSlot,
                    size: total_size,
                    align_shift: 0,
                    key: None,
                });
                let addr = self.builder.ins().stack_addr(I64, slot, 0);
                for (i, &b) in bytes.iter().enumerate() {
                    let val = self.builder.ins().iconst(I8, b as i64);
                    self.builder.ins().stack_store(val, slot, i as i32);
                }
                let null = self.builder.ins().iconst(I8, 0);
                self.builder
                    .ins()
                    .stack_store(null, slot, bytes.len() as i32);
                addr
            }
            _ => {
                panic!("JIT: unimplemented expression: {:?}", &decl.arena[expr]);
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
            Unop::Neg => {
                let t = decl.types[arg_id];
                match *t {
                    crate::types::Type::Float32 | crate::types::Type::Float64 => {
                        self.builder.ins().fneg(v)
                    }
                    _ => self.builder.ins().imul_imm(v, -1),
                }
            }
            Unop::Not => {
                let bnot = self.builder.ins().bnot(v);
                self.builder.ins().band_imm(bnot, 1)
            }
        }
    }

    /// Compute the natural alignment of a type for use with Cranelift memory
    /// operations. Cranelift requires that alignment ≤ greatest_divisible_power_of_two(size),
    /// so we base alignment on the element type rather than the total size.
    fn type_align(t: &crate::TypeID, decls: &crate::DeclTable) -> u8 {
        let size = t.size(decls) as u64;
        if size == 0 {
            return 1;
        }
        // Cranelift requires align ≤ greatest_divisible_power_of_two(size).
        // Compute the natural alignment, then clamp to that limit.
        let natural = match &**t {
            crate::Type::Array(elem, _) => Self::type_align(elem, decls),
            crate::Type::Slice(_) => 4,
            crate::Type::Int8 | crate::Type::UInt8 | crate::Type::Bool => 1,
            crate::Type::Float64 => 8,
            crate::Type::Tuple(fields) => fields
                .iter()
                .map(|f| Self::type_align(f, decls))
                .max()
                .unwrap_or(1),
            crate::Type::Name(name, vars) => {
                let decl = decls.find(*name);
                if decl.len() == 1 {
                    if let crate::decl::Decl::Struct(sdecl) = &decl[0] {
                        let inst: crate::Instance = sdecl
                            .typevars
                            .iter()
                            .zip(vars.iter())
                            .map(|(tv, ty)| (crate::types::mk_type(crate::Type::Var(*tv)), *ty))
                            .collect();
                        sdecl
                            .fields
                            .iter()
                            .map(|f| Self::type_align(&f.ty.subst(&inst), decls))
                            .max()
                            .unwrap_or(1)
                    } else {
                        4
                    }
                } else {
                    4
                }
            }
            _ => 4,
        };
        // Clamp: alignment must not exceed the largest power of 2 dividing size.
        let max_align = size & size.wrapping_neg(); // greatest_divisible_power_of_two
        std::cmp::min(natural, max_align as u8)
    }

    fn gen_copy(&mut self, t: crate::TypeID, dst: Value, src: Value, decls: &crate::DeclTable) {
        if t.is_ptr() {
            let size = t.size(decls) as u64;
            let align = Self::type_align(&t, decls);
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

    fn gen_zero(&mut self, t: crate::TypeID, dst: Value, decls: &crate::DeclTable) {
        let size = t.size(decls) as u32;
        let zero = self.builder.ins().iconst(I8, 0);
        // Store zero byte-by-byte for the size of the type
        for offset in 0..size {
            self.builder
                .ins()
                .store(MemFlags::new(), zero, dst, offset as i32);
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
            let align = std::num::NonZeroU8::new(Self::type_align(&t, decls)).unwrap();
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
                crate::types::Type::Bool
                | crate::types::Type::Int32
                | crate::types::Type::UInt32
                | crate::types::Type::Int8
                | crate::types::Type::UInt8 => self.builder.ins().icmp(IntCC::Equal, dst, src),
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
                    crate::types::Type::Int32
                    | crate::types::Type::UInt32
                    | crate::types::Type::Int8
                    | crate::types::Type::UInt8 => self.builder.ins().iadd(lhs, rhs),
                    crate::types::Type::Float32 | crate::types::Type::Float64 => {
                        self.builder.ins().fadd(lhs, rhs)
                    }
                    _ => unreachable!("type {:?} not supported for this binary op", t),
                }
            }
            Binop::Minus => {
                let lhs = self.translate_expr(lhs_id, decl, decls);
                let rhs = self.translate_expr(rhs_id, decl, decls);
                let t = decl.types[lhs_id];

                match *t {
                    crate::types::Type::Int32
                    | crate::types::Type::UInt32
                    | crate::types::Type::Int8
                    | crate::types::Type::UInt8 => self.builder.ins().isub(lhs, rhs),
                    crate::types::Type::Float32 | crate::types::Type::Float64 => {
                        self.builder.ins().fsub(lhs, rhs)
                    }
                    _ => unreachable!("type {:?} not supported for this binary op", t),
                }
            }
            Binop::Mult => {
                let lhs = self.translate_expr(lhs_id, decl, decls);
                let rhs = self.translate_expr(rhs_id, decl, decls);
                let t = decl.types[lhs_id];
                match *t {
                    crate::types::Type::Int32
                    | crate::types::Type::UInt32
                    | crate::types::Type::Int8
                    | crate::types::Type::UInt8 => self.builder.ins().imul(lhs, rhs),
                    crate::types::Type::Float32 | crate::types::Type::Float64 => {
                        self.builder.ins().fmul(lhs, rhs)
                    }
                    _ => unreachable!("type {:?} not supported for this binary op", t),
                }
            }
            Binop::Div => {
                let lhs = self.translate_expr(lhs_id, decl, decls);
                let rhs = self.translate_expr(rhs_id, decl, decls);
                let t = decl.types[lhs_id];
                match *t {
                    crate::types::Type::Int32
                    | crate::types::Type::UInt32
                    | crate::types::Type::Int8
                    | crate::types::Type::UInt8 => self.builder.ins().udiv(lhs, rhs),
                    crate::types::Type::Float32 | crate::types::Type::Float64 => {
                        self.builder.ins().fdiv(lhs, rhs)
                    }
                    _ => unreachable!("type {:?} not supported for this binary op", t),
                }
            }
            Binop::Mod => {
                let lhs = self.translate_expr(lhs_id, decl, decls);
                let rhs = self.translate_expr(rhs_id, decl, decls);
                let t = decl.types[lhs_id];
                match *t {
                    crate::types::Type::Int32
                    | crate::types::Type::UInt32
                    | crate::types::Type::Int8
                    | crate::types::Type::UInt8 => self.builder.ins().urem(lhs, rhs),
                    _ => unreachable!("type {:?} not supported for modulo", t),
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
                    crate::types::Type::Bool
                    | crate::types::Type::Int32
                    | crate::types::Type::UInt32
                    | crate::types::Type::Int8
                    | crate::types::Type::UInt8 => {
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
                    _ => unreachable!("type {:?} not supported for this binary op", t),
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
                    crate::types::Type::UInt32 | crate::types::Type::UInt8 => self
                        .builder
                        .ins()
                        .icmp(IntCC::UnsignedGreaterThan, lhs, rhs),
                    crate::types::Type::Float32 | crate::types::Type::Float64 => {
                        self.builder.ins().fcmp(FloatCC::GreaterThan, lhs, rhs)
                    }
                    _ => unreachable!("type {:?} not supported for this binary op", t),
                }
            }
            Binop::Leq => {
                let lhs = self.translate_expr(lhs_id, decl, decls);
                let rhs = self.translate_expr(rhs_id, decl, decls);
                let t = decl.types[lhs_id];

                match *t {
                    crate::types::Type::Int32 | crate::types::Type::Int8 => self
                        .builder
                        .ins()
                        .icmp(IntCC::SignedLessThanOrEqual, lhs, rhs),
                    crate::types::Type::UInt32 | crate::types::Type::UInt8 => self
                        .builder
                        .ins()
                        .icmp(IntCC::UnsignedLessThanOrEqual, lhs, rhs),
                    crate::types::Type::Float32 | crate::types::Type::Float64 => {
                        self.builder.ins().fcmp(FloatCC::LessThanOrEqual, lhs, rhs)
                    }
                    _ => unreachable!("type {:?} not supported for this binary op", t),
                }
            }
            Binop::Geq => {
                let lhs = self.translate_expr(lhs_id, decl, decls);
                let rhs = self.translate_expr(rhs_id, decl, decls);
                let t = decl.types[lhs_id];

                match *t {
                    crate::types::Type::Int32 | crate::types::Type::Int8 => self
                        .builder
                        .ins()
                        .icmp(IntCC::SignedGreaterThanOrEqual, lhs, rhs),
                    crate::types::Type::UInt32 | crate::types::Type::UInt8 => self
                        .builder
                        .ins()
                        .icmp(IntCC::UnsignedGreaterThanOrEqual, lhs, rhs),
                    crate::types::Type::Float32 | crate::types::Type::Float64 => self
                        .builder
                        .ins()
                        .fcmp(FloatCC::GreaterThanOrEqual, lhs, rhs),
                    _ => unreachable!("type {:?} not supported for this binary op", t),
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
                panic!("JIT: unimplemented binary operation: {:?}", binop);
            }
        }
    }

    fn debug_print_i64(&mut self, value: Value) {
        let f_ptr = self
            .builder
            .ins()
            .iconst(I64, lyte_print_i64 as *const () as i64);

        let mut sig = Signature::new(CallConv::Fast);
        sig.params = vec![AbiParam::new(I64)];
        sig.returns = vec![AbiParam::new(I32)];
        let sref = self.builder.import_signature(sig);
        self.builder.ins().call_indirect(sref, f_ptr, &vec![value]);
    }

    fn translate_func(&mut self, name: &Name, ty: &crate::Type) -> Value {
        if *name == Name::str("assert") {
            return self
                .builder
                .ins()
                .iconst(I64, lyte_assert as *const () as i64);
        }

        if *name == Name::str("print") {
            return self
                .builder
                .ins()
                .iconst(I64, lyte_print_i32 as *const () as i64);
        }

        if *name == Name::str("putc") {
            return self
                .builder
                .ins()
                .iconst(I64, lyte_putc as *const () as i64);
        }

        if let Some(ptr) = math_builtin_ptr(name) {
            return self.builder.ins().iconst(I64, ptr);
        }

        if let crate::Type::Func(dom, rng) = ty {
            let mut sig = fn_sig(&self.module, *dom, *rng);
            // Add globals and closure pointers as first two parameters.
            sig.params.insert(0, AbiParam::new(I64));
            sig.params.insert(1, AbiParam::new(I64));
            let callee = self
                .module
                .declare_function(&name, Linkage::Import, &sig)
                .expect("problem declaring function");
            let local_callee = self.module.declare_func_in_func(callee, self.builder.func);

            self.called_functions.insert(*name);

            let fn_ptr = self.builder.ins().func_addr(I64, local_callee);

            // Wrap in a fat pointer pair {fn_ptr, null_closure} on the stack.
            let slot = self.builder.create_sized_stack_slot(StackSlotData {
                kind: StackSlotKind::ExplicitSlot,
                size: 16,
                align_shift: 0,
                key: None,
            });
            let pair_addr = self.builder.ins().stack_addr(I64, slot, 0);
            let null = self.builder.ins().iconst(I64, 0);
            self.builder
                .ins()
                .store(MemFlags::new(), fn_ptr, pair_addr, 0);
            self.builder
                .ins()
                .store(MemFlags::new(), null, pair_addr, 8);
            pair_addr
        } else {
            panic!(
                "JIT wrap_as_func_pair: expected function type, got {:?}",
                ty
            );
        }
    }

    /// Returns the address of a variable's storage for use in a closure struct.
    /// For var bindings the variable already holds a pointer; for let bindings
    /// a fresh stack slot is allocated and the value is copied into it.
    fn get_var_address(
        &mut self,
        name: &str,
        ty: crate::TypeID,
        decls: &crate::DeclTable,
    ) -> Value {
        if let Some(&var) = self.variables.get(name) {
            if self.let_bindings.contains(name) {
                // let binding: allocate a slot, copy the value in, capture the slot address.
                let val = self.builder.use_var(var);
                let sz = ty.size(decls) as u32;
                let slot = self.builder.create_sized_stack_slot(StackSlotData {
                    kind: StackSlotKind::ExplicitSlot,
                    size: sz,
                    align_shift: 0,
                    key: None,
                });
                let addr = self.builder.ins().stack_addr(I64, slot, 0);
                self.builder.ins().store(MemFlags::new(), val, addr, 0);
                addr
            } else {
                // var binding: variable holds a pointer to the stack slot directly.
                self.builder.use_var(var)
            }
        } else if let Some(&offset) = self.globals.get(&Name::new(name.to_string())) {
            let base = self.globals_base.expect("globals_base not set");
            self.builder.ins().iadd_imm(base, offset as i64)
        } else {
            panic!("unknown variable in closure capture: {}", name)
        }
    }

    /// Wraps a sized array value in a slice fat pointer {data_ptr, len} on the stack.
    /// If the value is already a slice, returns it as-is.
    fn wrap_as_slice(
        &mut self,
        val: Value,
        actual_ty: crate::TypeID,
        _decls: &crate::DeclTable,
    ) -> Value {
        match &*actual_ty {
            crate::Type::Slice(_) => {
                // Already a slice fat pointer, pass through.
                val
            }
            crate::Type::Array(_, sz) => {
                // Sized array: construct fat pointer {data_ptr: I64, len: I32}.
                let len_val = self.builder.ins().iconst(I32, sz.known() as i64);
                let slot = self.builder.create_sized_stack_slot(StackSlotData {
                    kind: StackSlotKind::ExplicitSlot,
                    size: 12,
                    align_shift: 0,
                    key: None,
                });
                let fat_addr = self.builder.ins().stack_addr(I64, slot, 0);
                self.builder.ins().store(MemFlags::new(), val, fat_addr, 0);
                self.builder
                    .ins()
                    .store(MemFlags::new(), len_val, fat_addr, 8);
                fat_addr
            }
            _ => {
                panic!(
                    "JIT wrap_as_slice: expected array type, got {:?}",
                    actual_ty
                );
            }
        }
    }

    fn declare_variable(&mut self, name: &String, ty: Type) -> Variable {
        if let Some(&existing) = self.variables.get(name) {
            // If the variable already exists with the same type, reuse it.
            // If the type differs (e.g., a for-loop counter i32 vs a var binding i64),
            // create a new variable to shadow the old one.
            let val = self.builder.use_var(existing);
            let existing_ty = self.builder.func.dfg.value_type(val);
            if existing_ty == ty {
                return existing;
            }
        }
        let var = self.builder.declare_var(ty);
        self.variables.insert(name.into(), var);
        self.next_index += 1;
        var
    }
}

/// Collect the names (and their types) of free variables referenced in `body`
/// that are present in `local_vars` but not in `exclude` (the lambda's own params).
/// Returns each name at most once.
fn collect_free_var_names(
    body: crate::ExprID,
    arena: &crate::ExprArena,
    exclude: &std::collections::HashSet<String>,
    local_vars: &HashMap<String, Variable>,
    types: &[crate::TypeID],
) -> Vec<(String, crate::TypeID)> {
    let mut result = Vec::new();
    let mut seen = std::collections::HashSet::new();
    collect_free_vars_rec(
        body,
        arena,
        exclude,
        local_vars,
        types,
        &mut result,
        &mut seen,
    );
    result
}

fn collect_free_vars_rec(
    expr: crate::ExprID,
    arena: &crate::ExprArena,
    exclude: &std::collections::HashSet<String>,
    local_vars: &HashMap<String, Variable>,
    types: &[crate::TypeID],
    result: &mut Vec<(String, crate::TypeID)>,
    seen: &mut std::collections::HashSet<String>,
) {
    match &arena[expr] {
        Expr::Id(name) => {
            let s = name.to_string();
            if local_vars.contains_key(&s) && !exclude.contains(&s) && !seen.contains(&s) {
                result.push((s.clone(), types[expr]));
                seen.insert(s);
            }
        }
        Expr::Call(fn_id, args) => {
            collect_free_vars_rec(*fn_id, arena, exclude, local_vars, types, result, seen);
            for a in args {
                collect_free_vars_rec(*a, arena, exclude, local_vars, types, result, seen);
            }
        }
        Expr::Binop(_, lhs, rhs) => {
            collect_free_vars_rec(*lhs, arena, exclude, local_vars, types, result, seen);
            collect_free_vars_rec(*rhs, arena, exclude, local_vars, types, result, seen);
        }
        Expr::Unop(_, arg) => {
            collect_free_vars_rec(*arg, arena, exclude, local_vars, types, result, seen);
        }
        Expr::Let(_, init, _) => {
            collect_free_vars_rec(*init, arena, exclude, local_vars, types, result, seen);
        }
        Expr::Var(_, init, _) => {
            if let Some(init_id) = init {
                collect_free_vars_rec(*init_id, arena, exclude, local_vars, types, result, seen);
            }
        }
        Expr::If(cond, then, else_) => {
            collect_free_vars_rec(*cond, arena, exclude, local_vars, types, result, seen);
            collect_free_vars_rec(*then, arena, exclude, local_vars, types, result, seen);
            if let Some(e) = else_ {
                collect_free_vars_rec(*e, arena, exclude, local_vars, types, result, seen);
            }
        }
        Expr::While(cond, body) => {
            collect_free_vars_rec(*cond, arena, exclude, local_vars, types, result, seen);
            collect_free_vars_rec(*body, arena, exclude, local_vars, types, result, seen);
        }
        Expr::For {
            start, end, body, ..
        } => {
            collect_free_vars_rec(*start, arena, exclude, local_vars, types, result, seen);
            collect_free_vars_rec(*end, arena, exclude, local_vars, types, result, seen);
            collect_free_vars_rec(*body, arena, exclude, local_vars, types, result, seen);
        }
        Expr::Block(exprs) => {
            for e in exprs {
                collect_free_vars_rec(*e, arena, exclude, local_vars, types, result, seen);
            }
        }
        Expr::Return(e) => {
            collect_free_vars_rec(*e, arena, exclude, local_vars, types, result, seen);
        }
        Expr::Field(e, _) => {
            collect_free_vars_rec(*e, arena, exclude, local_vars, types, result, seen);
        }
        Expr::ArrayIndex(arr, idx) => {
            collect_free_vars_rec(*arr, arena, exclude, local_vars, types, result, seen);
            collect_free_vars_rec(*idx, arena, exclude, local_vars, types, result, seen);
        }
        Expr::ArrayLiteral(elems) => {
            for e in elems {
                collect_free_vars_rec(*e, arena, exclude, local_vars, types, result, seen);
            }
        }
        Expr::Tuple(elems) => {
            for e in elems {
                collect_free_vars_rec(*e, arena, exclude, local_vars, types, result, seen);
            }
        }
        Expr::AsTy(e, _) => {
            collect_free_vars_rec(*e, arena, exclude, local_vars, types, result, seen);
        }
        Expr::Arena(e) => {
            collect_free_vars_rec(*e, arena, exclude, local_vars, types, result, seen);
        }
        Expr::Array(ty_expr, size_expr) => {
            collect_free_vars_rec(*ty_expr, arena, exclude, local_vars, types, result, seen);
            collect_free_vars_rec(*size_expr, arena, exclude, local_vars, types, result, seen);
        }
        Expr::Lambda { params, body } => {
            // Nested lambda: add its params to the exclusion set.
            let mut inner_exclude = exclude.clone();
            for p in params {
                inner_exclude.insert(p.name.to_string());
            }
            collect_free_vars_rec(
                *body,
                arena,
                &inner_exclude,
                local_vars,
                types,
                result,
                seen,
            );
        }
        Expr::Macro(_, args) => {
            for a in args {
                collect_free_vars_rec(*a, arena, exclude, local_vars, types, result, seen);
            }
        }
        // Terminal expressions — no sub-expressions.
        Expr::Int(_)
        | Expr::UInt(_)
        | Expr::Real(_)
        | Expr::String(_)
        | Expr::Char(_)
        | Expr::True
        | Expr::False
        | Expr::Enum(_)
        | Expr::Error => {}
    }
}

extern "C" fn lyte_assert(val: i8) {
    println!("assert({})", val != 0);
    assert!(val != 0);
}

extern "C" fn lyte_print_i32(val: i32) {
    println!("{}", val);
}

extern "C" fn lyte_putc(val: i32) {
    if let Some(c) = char::from_u32(val as u32) {
        print!("{}", c);
    }
}

extern "C" fn lyte_print_i64(val: i64) {
    println!("{}", val);
}

// Math builtins — f32 unary
extern "C" fn lyte_sinf(x: f32) -> f32 {
    x.sin()
}
extern "C" fn lyte_cosf(x: f32) -> f32 {
    x.cos()
}
extern "C" fn lyte_tanf(x: f32) -> f32 {
    x.tan()
}
extern "C" fn lyte_asinf(x: f32) -> f32 {
    x.asin()
}
extern "C" fn lyte_acosf(x: f32) -> f32 {
    x.acos()
}
extern "C" fn lyte_atanf(x: f32) -> f32 {
    x.atan()
}
extern "C" fn lyte_sinhf(x: f32) -> f32 {
    x.sinh()
}
extern "C" fn lyte_coshf(x: f32) -> f32 {
    x.cosh()
}
extern "C" fn lyte_tanhf(x: f32) -> f32 {
    x.tanh()
}
extern "C" fn lyte_asinhf(x: f32) -> f32 {
    x.asinh()
}
extern "C" fn lyte_acoshf(x: f32) -> f32 {
    x.acosh()
}
extern "C" fn lyte_atanhf(x: f32) -> f32 {
    x.atanh()
}
extern "C" fn lyte_lnf(x: f32) -> f32 {
    x.ln()
}
extern "C" fn lyte_expf(x: f32) -> f32 {
    x.exp()
}
extern "C" fn lyte_exp2f(x: f32) -> f32 {
    x.exp2()
}
extern "C" fn lyte_log10f(x: f32) -> f32 {
    x.log10()
}
extern "C" fn lyte_log2f(x: f32) -> f32 {
    x.log2()
}
extern "C" fn lyte_sqrtf(x: f32) -> f32 {
    x.sqrt()
}
extern "C" fn lyte_absf(x: f32) -> f32 {
    x.abs()
}
extern "C" fn lyte_floorf(x: f32) -> f32 {
    x.floor()
}
extern "C" fn lyte_ceilf(x: f32) -> f32 {
    x.ceil()
}

// Math builtins — f64 unary
extern "C" fn lyte_sind(x: f64) -> f64 {
    x.sin()
}
extern "C" fn lyte_cosd(x: f64) -> f64 {
    x.cos()
}
extern "C" fn lyte_tand(x: f64) -> f64 {
    x.tan()
}
extern "C" fn lyte_asind(x: f64) -> f64 {
    x.asin()
}
extern "C" fn lyte_acosd(x: f64) -> f64 {
    x.acos()
}
extern "C" fn lyte_atand(x: f64) -> f64 {
    x.atan()
}
extern "C" fn lyte_sinhd(x: f64) -> f64 {
    x.sinh()
}
extern "C" fn lyte_coshd(x: f64) -> f64 {
    x.cosh()
}
extern "C" fn lyte_tanhd(x: f64) -> f64 {
    x.tanh()
}
extern "C" fn lyte_asinhd(x: f64) -> f64 {
    x.asinh()
}
extern "C" fn lyte_acoshd(x: f64) -> f64 {
    x.acosh()
}
extern "C" fn lyte_atanhd(x: f64) -> f64 {
    x.atanh()
}
extern "C" fn lyte_lnd(x: f64) -> f64 {
    x.ln()
}
extern "C" fn lyte_expd(x: f64) -> f64 {
    x.exp()
}
extern "C" fn lyte_exp2d(x: f64) -> f64 {
    x.exp2()
}
extern "C" fn lyte_log10d(x: f64) -> f64 {
    x.log10()
}
extern "C" fn lyte_log2d(x: f64) -> f64 {
    x.log2()
}
extern "C" fn lyte_sqrtd(x: f64) -> f64 {
    x.sqrt()
}
extern "C" fn lyte_absd(x: f64) -> f64 {
    x.abs()
}
extern "C" fn lyte_floord(x: f64) -> f64 {
    x.floor()
}
extern "C" fn lyte_ceild(x: f64) -> f64 {
    x.ceil()
}

// Math builtins — predicates (return i32 for bool)
extern "C" fn lyte_isinff(x: f32) -> i32 {
    x.is_infinite() as i32
}
extern "C" fn lyte_isinfd(x: f64) -> i32 {
    x.is_infinite() as i32
}
extern "C" fn lyte_isnanf(x: f32) -> i32 {
    x.is_nan() as i32
}
extern "C" fn lyte_isnand(x: f64) -> i32 {
    x.is_nan() as i32
}

// Math builtins — f32 binary
extern "C" fn lyte_powf(x: f32, y: f32) -> f32 {
    x.powf(y)
}
extern "C" fn lyte_atan2f(x: f32, y: f32) -> f32 {
    x.atan2(y)
}
extern "C" fn lyte_minf(x: f32, y: f32) -> f32 {
    x.min(y)
}
extern "C" fn lyte_maxf(x: f32, y: f32) -> f32 {
    x.max(y)
}

// Math builtins — f64 binary
extern "C" fn lyte_powd(x: f64, y: f64) -> f64 {
    x.powf(y)
}
extern "C" fn lyte_atan2d(x: f64, y: f64) -> f64 {
    x.atan2(y)
}
extern "C" fn lyte_mind(x: f64, y: f64) -> f64 {
    x.min(y)
}
extern "C" fn lyte_maxd(x: f64, y: f64) -> f64 {
    x.max(y)
}

const BUILTIN_NAMES: &[&str] = &[
    "assert",
    "print",
    "putc",
    // unary f32/f64
    "sin$f32",
    "sin$f64",
    "cos$f32",
    "cos$f64",
    "tan$f32",
    "tan$f64",
    "asin$f32",
    "asin$f64",
    "acos$f32",
    "acos$f64",
    "atan$f32",
    "atan$f64",
    "sinh$f32",
    "sinh$f64",
    "cosh$f32",
    "cosh$f64",
    "tanh$f32",
    "tanh$f64",
    "asinh$f32",
    "asinh$f64",
    "acosh$f32",
    "acosh$f64",
    "atanh$f32",
    "atanh$f64",
    "ln$f32",
    "ln$f64",
    "exp$f32",
    "exp$f64",
    "exp2$f32",
    "exp2$f64",
    "log10$f32",
    "log10$f64",
    "log2$f32",
    "log2$f64",
    "sqrt$f32",
    "sqrt$f64",
    "abs$f32",
    "abs$f64",
    "floor$f32",
    "floor$f64",
    "ceil$f32",
    "ceil$f64",
    // predicates
    "isinf$f32",
    "isinf$f64",
    "isnan$f32",
    "isnan$f64",
    // binary f32/f64
    "pow$f32$f32",
    "pow$f64$f64",
    "atan2$f32$f32",
    "atan2$f64$f64",
    "min$f32$f32",
    "min$f64$f64",
    "max$f32$f32",
    "max$f64$f64",
];

fn is_builtin_name(name: &Name) -> bool {
    BUILTIN_NAMES.iter().any(|&n| *name == Name::str(n))
}

fn math_builtin_ptr(name: &Name) -> Option<i64> {
    let pairs: &[(&str, i64)] = &[
        // unary f32
        ("sin$f32", lyte_sinf as *const () as i64),
        ("cos$f32", lyte_cosf as *const () as i64),
        ("tan$f32", lyte_tanf as *const () as i64),
        ("asin$f32", lyte_asinf as *const () as i64),
        ("acos$f32", lyte_acosf as *const () as i64),
        ("atan$f32", lyte_atanf as *const () as i64),
        ("sinh$f32", lyte_sinhf as *const () as i64),
        ("cosh$f32", lyte_coshf as *const () as i64),
        ("tanh$f32", lyte_tanhf as *const () as i64),
        ("asinh$f32", lyte_asinhf as *const () as i64),
        ("acosh$f32", lyte_acoshf as *const () as i64),
        ("atanh$f32", lyte_atanhf as *const () as i64),
        ("ln$f32", lyte_lnf as *const () as i64),
        ("exp$f32", lyte_expf as *const () as i64),
        ("exp2$f32", lyte_exp2f as *const () as i64),
        ("log10$f32", lyte_log10f as *const () as i64),
        ("log2$f32", lyte_log2f as *const () as i64),
        ("sqrt$f32", lyte_sqrtf as *const () as i64),
        ("abs$f32", lyte_absf as *const () as i64),
        ("floor$f32", lyte_floorf as *const () as i64),
        ("ceil$f32", lyte_ceilf as *const () as i64),
        // unary f64
        ("sin$f64", lyte_sind as *const () as i64),
        ("cos$f64", lyte_cosd as *const () as i64),
        ("tan$f64", lyte_tand as *const () as i64),
        ("asin$f64", lyte_asind as *const () as i64),
        ("acos$f64", lyte_acosd as *const () as i64),
        ("atan$f64", lyte_atand as *const () as i64),
        ("sinh$f64", lyte_sinhd as *const () as i64),
        ("cosh$f64", lyte_coshd as *const () as i64),
        ("tanh$f64", lyte_tanhd as *const () as i64),
        ("asinh$f64", lyte_asinhd as *const () as i64),
        ("acosh$f64", lyte_acoshd as *const () as i64),
        ("atanh$f64", lyte_atanhd as *const () as i64),
        ("ln$f64", lyte_lnd as *const () as i64),
        ("exp$f64", lyte_expd as *const () as i64),
        ("exp2$f64", lyte_exp2d as *const () as i64),
        ("log10$f64", lyte_log10d as *const () as i64),
        ("log2$f64", lyte_log2d as *const () as i64),
        ("sqrt$f64", lyte_sqrtd as *const () as i64),
        ("abs$f64", lyte_absd as *const () as i64),
        ("floor$f64", lyte_floord as *const () as i64),
        ("ceil$f64", lyte_ceild as *const () as i64),
        // predicates
        ("isinf$f32", lyte_isinff as *const () as i64),
        ("isinf$f64", lyte_isinfd as *const () as i64),
        ("isnan$f32", lyte_isnanf as *const () as i64),
        ("isnan$f64", lyte_isnand as *const () as i64),
        // binary f32
        ("pow$f32$f32", lyte_powf as *const () as i64),
        ("atan2$f32$f32", lyte_atan2f as *const () as i64),
        ("min$f32$f32", lyte_minf as *const () as i64),
        ("max$f32$f32", lyte_maxf as *const () as i64),
        // binary f64
        ("pow$f64$f64", lyte_powd as *const () as i64),
        ("atan2$f64$f64", lyte_atan2d as *const () as i64),
        ("min$f64$f64", lyte_mind as *const () as i64),
        ("max$f64$f64", lyte_maxd as *const () as i64),
    ];
    for &(n, ptr) in pairs {
        if *name == Name::str(n) {
            return Some(ptr);
        }
    }
    None
}

/// Return the JIT symbol name for a math builtin, if applicable.
fn math_builtin_symbol(name: &Name) -> Option<&'static str> {
    // Same list as math_builtin_ptr but returns the symbol name for direct calls.
    let syms: &[(&str, &str)] = &[
        ("sin$f32", "__lyte_sinf"),
        ("cos$f32", "__lyte_cosf"),
        ("tan$f32", "__lyte_tanf"),
        ("asin$f32", "__lyte_asinf"),
        ("acos$f32", "__lyte_acosf"),
        ("atan$f32", "__lyte_atanf"),
        ("sinh$f32", "__lyte_sinhf"),
        ("cosh$f32", "__lyte_coshf"),
        ("tanh$f32", "__lyte_tanhf"),
        ("asinh$f32", "__lyte_asinhf"),
        ("acosh$f32", "__lyte_acoshf"),
        ("atanh$f32", "__lyte_atanhf"),
        ("ln$f32", "__lyte_lnf"),
        ("exp$f32", "__lyte_expf"),
        ("exp2$f32", "__lyte_exp2f"),
        ("log10$f32", "__lyte_log10f"),
        ("log2$f32", "__lyte_log2f"),
        ("sqrt$f32", "__lyte_sqrtf"),
        ("abs$f32", "__lyte_absf"),
        ("floor$f32", "__lyte_floorf"),
        ("ceil$f32", "__lyte_ceilf"),
        ("sin$f64", "__lyte_sind"),
        ("cos$f64", "__lyte_cosd"),
        ("tan$f64", "__lyte_tand"),
        ("asin$f64", "__lyte_asind"),
        ("acos$f64", "__lyte_acosd"),
        ("atan$f64", "__lyte_atand"),
        ("sinh$f64", "__lyte_sinhd"),
        ("cosh$f64", "__lyte_coshd"),
        ("tanh$f64", "__lyte_tanhd"),
        ("asinh$f64", "__lyte_asinhd"),
        ("acosh$f64", "__lyte_acoshd"),
        ("atanh$f64", "__lyte_atanhd"),
        ("ln$f64", "__lyte_lnd"),
        ("exp$f64", "__lyte_expd"),
        ("exp2$f64", "__lyte_exp2d"),
        ("log10$f64", "__lyte_log10d"),
        ("log2$f64", "__lyte_log2d"),
        ("sqrt$f64", "__lyte_sqrtd"),
        ("abs$f64", "__lyte_absd"),
        ("floor$f64", "__lyte_floord"),
        ("ceil$f64", "__lyte_ceild"),
        ("isinf$f32", "__lyte_isinff"),
        ("isinf$f64", "__lyte_isinfd"),
        ("isnan$f32", "__lyte_isnanf"),
        ("isnan$f64", "__lyte_isnand"),
        ("pow$f32$f32", "__lyte_powf"),
        ("atan2$f32$f32", "__lyte_atan2f"),
        ("min$f32$f32", "__lyte_minf"),
        ("max$f32$f32", "__lyte_maxf"),
        ("pow$f64$f64", "__lyte_powd"),
        ("atan2$f64$f64", "__lyte_atan2d"),
        ("min$f64$f64", "__lyte_mind"),
        ("max$f64$f64", "__lyte_maxd"),
    ];
    for &(n, sym) in syms {
        if *name == Name::str(n) {
            return Some(sym);
        }
    }
    None
}

/// Register all math builtin symbols on the JIT builder for direct calls.
fn register_math_symbols(builder: &mut JITBuilder) {
    let syms: &[(&str, *const u8)] = &[
        ("__lyte_sinf", lyte_sinf as *const u8),
        ("__lyte_cosf", lyte_cosf as *const u8),
        ("__lyte_tanf", lyte_tanf as *const u8),
        ("__lyte_asinf", lyte_asinf as *const u8),
        ("__lyte_acosf", lyte_acosf as *const u8),
        ("__lyte_atanf", lyte_atanf as *const u8),
        ("__lyte_sinhf", lyte_sinhf as *const u8),
        ("__lyte_coshf", lyte_coshf as *const u8),
        ("__lyte_tanhf", lyte_tanhf as *const u8),
        ("__lyte_asinhf", lyte_asinhf as *const u8),
        ("__lyte_acoshf", lyte_acoshf as *const u8),
        ("__lyte_atanhf", lyte_atanhf as *const u8),
        ("__lyte_lnf", lyte_lnf as *const u8),
        ("__lyte_expf", lyte_expf as *const u8),
        ("__lyte_exp2f", lyte_exp2f as *const u8),
        ("__lyte_log10f", lyte_log10f as *const u8),
        ("__lyte_log2f", lyte_log2f as *const u8),
        ("__lyte_sqrtf", lyte_sqrtf as *const u8),
        ("__lyte_absf", lyte_absf as *const u8),
        ("__lyte_floorf", lyte_floorf as *const u8),
        ("__lyte_ceilf", lyte_ceilf as *const u8),
        ("__lyte_sind", lyte_sind as *const u8),
        ("__lyte_cosd", lyte_cosd as *const u8),
        ("__lyte_tand", lyte_tand as *const u8),
        ("__lyte_asind", lyte_asind as *const u8),
        ("__lyte_acosd", lyte_acosd as *const u8),
        ("__lyte_atand", lyte_atand as *const u8),
        ("__lyte_sinhd", lyte_sinhd as *const u8),
        ("__lyte_coshd", lyte_coshd as *const u8),
        ("__lyte_tanhd", lyte_tanhd as *const u8),
        ("__lyte_asinhd", lyte_asinhd as *const u8),
        ("__lyte_acoshd", lyte_acoshd as *const u8),
        ("__lyte_atanhd", lyte_atanhd as *const u8),
        ("__lyte_lnd", lyte_lnd as *const u8),
        ("__lyte_expd", lyte_expd as *const u8),
        ("__lyte_exp2d", lyte_exp2d as *const u8),
        ("__lyte_log10d", lyte_log10d as *const u8),
        ("__lyte_log2d", lyte_log2d as *const u8),
        ("__lyte_sqrtd", lyte_sqrtd as *const u8),
        ("__lyte_absd", lyte_absd as *const u8),
        ("__lyte_floord", lyte_floord as *const u8),
        ("__lyte_ceild", lyte_ceild as *const u8),
        ("__lyte_isinff", lyte_isinff as *const u8),
        ("__lyte_isinfd", lyte_isinfd as *const u8),
        ("__lyte_isnanf", lyte_isnanf as *const u8),
        ("__lyte_isnand", lyte_isnand as *const u8),
        ("__lyte_powf", lyte_powf as *const u8),
        ("__lyte_atan2f", lyte_atan2f as *const u8),
        ("__lyte_minf", lyte_minf as *const u8),
        ("__lyte_maxf", lyte_maxf as *const u8),
        ("__lyte_powd", lyte_powd as *const u8),
        ("__lyte_atan2d", lyte_atan2d as *const u8),
        ("__lyte_mind", lyte_mind as *const u8),
        ("__lyte_maxd", lyte_maxd as *const u8),
    ];
    for &(name, ptr) in syms {
        builder.symbol(name, ptr);
    }
}
