// LLVM JIT backend for the Lyte compiler, using inkwell.
// Mirrors the Cranelift JIT backend in jit.rs.

use crate::decl::*;
use crate::defs::*;
use crate::expr::*;
use crate::DeclTable;

use std::convert::TryFrom;

use inkwell::basic_block::BasicBlock;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::{Linkage, Module};
use inkwell::passes::PassBuilderOptions;
use inkwell::targets::{CodeModel, InitializationConfig, RelocMode, Target, TargetMachine};
use inkwell::types::{BasicMetadataTypeEnum, BasicType, BasicTypeEnum, FunctionType};
use inkwell::values::{BasicMetadataValueEnum, BasicValueEnum, FunctionValue, PointerValue};
use inkwell::AddressSpace;
use inkwell::FloatPredicate;
use inkwell::IntPredicate;
use inkwell::OptimizationLevel;

use std::collections::{HashMap, HashSet};
use std::time::{Duration, Instant};

// Re-export CANCEL_FLAG_RESERVED so it matches the Cranelift JIT layout.
pub use crate::cancel::CANCEL_FLAG_RESERVED;

// External builtins (same implementations as in jit.rs).
extern "C" fn llvm_lyte_assert(val: i8) {
    crate::vm::println_output(&format!("assert({})", val != 0));
    assert!(val != 0, "lyte assertion failed");
}

extern "C" fn llvm_lyte_print_i32(val: i32) {
    crate::vm::println_output(&format!("{}", val));
}

extern "C" fn llvm_lyte_putc(val: i32) {
    if let Some(c) = char::from_u32(val as u32) {
        crate::vm::print_output(&format!("{}", c));
    }
}

// Math builtins — f32 unary
extern "C" fn llvm_lyte_sinf(x: f32) -> f32 {
    x.sin()
}
extern "C" fn llvm_lyte_cosf(x: f32) -> f32 {
    x.cos()
}
extern "C" fn llvm_lyte_tanf(x: f32) -> f32 {
    x.tan()
}
extern "C" fn llvm_lyte_asinf(x: f32) -> f32 {
    x.asin()
}
extern "C" fn llvm_lyte_acosf(x: f32) -> f32 {
    x.acos()
}
extern "C" fn llvm_lyte_atanf(x: f32) -> f32 {
    x.atan()
}
extern "C" fn llvm_lyte_sinhf(x: f32) -> f32 {
    x.sinh()
}
extern "C" fn llvm_lyte_coshf(x: f32) -> f32 {
    x.cosh()
}
extern "C" fn llvm_lyte_tanhf(x: f32) -> f32 {
    x.tanh()
}
extern "C" fn llvm_lyte_asinhf(x: f32) -> f32 {
    x.asinh()
}
extern "C" fn llvm_lyte_acoshf(x: f32) -> f32 {
    x.acosh()
}
extern "C" fn llvm_lyte_atanhf(x: f32) -> f32 {
    x.atanh()
}
extern "C" fn llvm_lyte_lnf(x: f32) -> f32 {
    x.ln()
}
extern "C" fn llvm_lyte_expf(x: f32) -> f32 {
    x.exp()
}
extern "C" fn llvm_lyte_exp2f(x: f32) -> f32 {
    x.exp2()
}
extern "C" fn llvm_lyte_log10f(x: f32) -> f32 {
    x.log10()
}
extern "C" fn llvm_lyte_log2f(x: f32) -> f32 {
    x.log2()
}
extern "C" fn llvm_lyte_sqrtf(x: f32) -> f32 {
    x.sqrt()
}
extern "C" fn llvm_lyte_absf(x: f32) -> f32 {
    x.abs()
}
extern "C" fn llvm_lyte_floorf(x: f32) -> f32 {
    x.floor()
}
extern "C" fn llvm_lyte_ceilf(x: f32) -> f32 {
    x.ceil()
}
// f64 unary
extern "C" fn llvm_lyte_sind(x: f64) -> f64 {
    x.sin()
}
extern "C" fn llvm_lyte_cosd(x: f64) -> f64 {
    x.cos()
}
extern "C" fn llvm_lyte_tand(x: f64) -> f64 {
    x.tan()
}
extern "C" fn llvm_lyte_asind(x: f64) -> f64 {
    x.asin()
}
extern "C" fn llvm_lyte_acosd(x: f64) -> f64 {
    x.acos()
}
extern "C" fn llvm_lyte_atand(x: f64) -> f64 {
    x.atan()
}
extern "C" fn llvm_lyte_sinhd(x: f64) -> f64 {
    x.sinh()
}
extern "C" fn llvm_lyte_coshd(x: f64) -> f64 {
    x.cosh()
}
extern "C" fn llvm_lyte_tanhd(x: f64) -> f64 {
    x.tanh()
}
extern "C" fn llvm_lyte_asinhd(x: f64) -> f64 {
    x.asinh()
}
extern "C" fn llvm_lyte_acoshd(x: f64) -> f64 {
    x.acosh()
}
extern "C" fn llvm_lyte_atanhd(x: f64) -> f64 {
    x.atanh()
}
extern "C" fn llvm_lyte_lnd(x: f64) -> f64 {
    x.ln()
}
extern "C" fn llvm_lyte_expd(x: f64) -> f64 {
    x.exp()
}
extern "C" fn llvm_lyte_exp2d(x: f64) -> f64 {
    x.exp2()
}
extern "C" fn llvm_lyte_log10d(x: f64) -> f64 {
    x.log10()
}
extern "C" fn llvm_lyte_log2d(x: f64) -> f64 {
    x.log2()
}
extern "C" fn llvm_lyte_sqrtd(x: f64) -> f64 {
    x.sqrt()
}
extern "C" fn llvm_lyte_absd(x: f64) -> f64 {
    x.abs()
}
extern "C" fn llvm_lyte_floord(x: f64) -> f64 {
    x.floor()
}
extern "C" fn llvm_lyte_ceild(x: f64) -> f64 {
    x.ceil()
}
// predicates
extern "C" fn llvm_lyte_isinff(x: f32) -> i32 {
    x.is_infinite() as i32
}
extern "C" fn llvm_lyte_isinfd(x: f64) -> i32 {
    x.is_infinite() as i32
}
extern "C" fn llvm_lyte_isnanf(x: f32) -> i32 {
    x.is_nan() as i32
}
extern "C" fn llvm_lyte_isnand(x: f64) -> i32 {
    x.is_nan() as i32
}
// binary f32
extern "C" fn llvm_lyte_powf(x: f32, y: f32) -> f32 {
    x.powf(y)
}
extern "C" fn llvm_lyte_atan2f(y: f32, x: f32) -> f32 {
    y.atan2(x)
}
extern "C" fn llvm_lyte_minf(x: f32, y: f32) -> f32 {
    x.min(y)
}
extern "C" fn llvm_lyte_maxf(x: f32, y: f32) -> f32 {
    x.max(y)
}
// binary f64
extern "C" fn llvm_lyte_powd(x: f64, y: f64) -> f64 {
    x.powf(y)
}
extern "C" fn llvm_lyte_atan2d(y: f64, x: f64) -> f64 {
    y.atan2(x)
}
extern "C" fn llvm_lyte_mind(x: f64, y: f64) -> f64 {
    x.min(y)
}
extern "C" fn llvm_lyte_maxd(x: f64, y: f64) -> f64 {
    x.max(y)
}

/// Called from LLVM JIT code when the cancel counter reaches zero.
/// Same logic as lyte_cancel_check in jit.rs.
unsafe extern "C" fn llvm_lyte_cancel_check(globals: *mut u8) {
    extern "C" {
        fn longjmp(env: *mut u8, val: i32) -> !;
    }
    use crate::cancel::*;

    // Reset counter
    *(globals.add(CANCEL_COUNTER_OFFSET as usize) as *mut i32) = CANCEL_CHECK_INTERVAL;

    // Read callback
    let cb_ptr = *(globals.add(CANCEL_CALLBACK_OFFSET) as *const usize);
    if cb_ptr == 0 {
        return;
    }
    let callback: CancelCallback = std::mem::transmute(cb_ptr);
    let user_data = *(globals.add(CANCEL_USERDATA_OFFSET) as *const *mut u8);

    if callback(user_data) {
        longjmp(globals.add(JMPBUF_OFFSET), 1);
    }
}

/// Called from LLVM JIT code to compare two slices by contents.
/// Each slice is a fat pointer: data_ptr (8 bytes) + len (4 bytes).
/// Returns 1 if equal, 0 if not.
unsafe extern "C" fn lyte_slice_eq(a: *const u8, b: *const u8, elem_size: u64) -> u64 {
    let ptr_a = (a as *const u64).read_unaligned() as *const u8;
    let len_a = (a.add(8) as *const u32).read_unaligned() as usize;
    let ptr_b = (b as *const u64).read_unaligned() as *const u8;
    let len_b = (b.add(8) as *const u32).read_unaligned() as usize;
    if len_a != len_b {
        return 0;
    }
    let total = len_a * elem_size as usize;
    (std::slice::from_raw_parts(ptr_a, total) == std::slice::from_raw_parts(ptr_b, total)) as u64
}

const BUILTIN_NAMES: &[&str] = &["assert", "print", "putc"];

fn is_builtin_name(name: &Name) -> bool {
    BUILTIN_NAMES.iter().any(|&n| *name == Name::str(n))
}

/// Return the function pointer for a math builtin, if any.
fn math_builtin_ptr(name: &Name) -> Option<usize> {
    let pairs: &[(&str, usize)] = &[
        ("sin$f32", llvm_lyte_sinf as *const () as usize),
        ("cos$f32", llvm_lyte_cosf as *const () as usize),
        ("tan$f32", llvm_lyte_tanf as *const () as usize),
        ("asin$f32", llvm_lyte_asinf as *const () as usize),
        ("acos$f32", llvm_lyte_acosf as *const () as usize),
        ("atan$f32", llvm_lyte_atanf as *const () as usize),
        ("sinh$f32", llvm_lyte_sinhf as *const () as usize),
        ("cosh$f32", llvm_lyte_coshf as *const () as usize),
        ("tanh$f32", llvm_lyte_tanhf as *const () as usize),
        ("asinh$f32", llvm_lyte_asinhf as *const () as usize),
        ("acosh$f32", llvm_lyte_acoshf as *const () as usize),
        ("atanh$f32", llvm_lyte_atanhf as *const () as usize),
        ("ln$f32", llvm_lyte_lnf as *const () as usize),
        ("exp$f32", llvm_lyte_expf as *const () as usize),
        ("exp2$f32", llvm_lyte_exp2f as *const () as usize),
        ("log10$f32", llvm_lyte_log10f as *const () as usize),
        ("log2$f32", llvm_lyte_log2f as *const () as usize),
        ("sqrt$f32", llvm_lyte_sqrtf as *const () as usize),
        ("abs$f32", llvm_lyte_absf as *const () as usize),
        ("floor$f32", llvm_lyte_floorf as *const () as usize),
        ("ceil$f32", llvm_lyte_ceilf as *const () as usize),
        ("sin$f64", llvm_lyte_sind as *const () as usize),
        ("cos$f64", llvm_lyte_cosd as *const () as usize),
        ("tan$f64", llvm_lyte_tand as *const () as usize),
        ("asin$f64", llvm_lyte_asind as *const () as usize),
        ("acos$f64", llvm_lyte_acosd as *const () as usize),
        ("atan$f64", llvm_lyte_atand as *const () as usize),
        ("sinh$f64", llvm_lyte_sinhd as *const () as usize),
        ("cosh$f64", llvm_lyte_coshd as *const () as usize),
        ("tanh$f64", llvm_lyte_tanhd as *const () as usize),
        ("asinh$f64", llvm_lyte_asinhd as *const () as usize),
        ("acosh$f64", llvm_lyte_acoshd as *const () as usize),
        ("atanh$f64", llvm_lyte_atanhd as *const () as usize),
        ("ln$f64", llvm_lyte_lnd as *const () as usize),
        ("exp$f64", llvm_lyte_expd as *const () as usize),
        ("exp2$f64", llvm_lyte_exp2d as *const () as usize),
        ("log10$f64", llvm_lyte_log10d as *const () as usize),
        ("log2$f64", llvm_lyte_log2d as *const () as usize),
        ("sqrt$f64", llvm_lyte_sqrtd as *const () as usize),
        ("abs$f64", llvm_lyte_absd as *const () as usize),
        ("floor$f64", llvm_lyte_floord as *const () as usize),
        ("ceil$f64", llvm_lyte_ceild as *const () as usize),
        ("isinf$f32", llvm_lyte_isinff as *const () as usize),
        ("isinf$f64", llvm_lyte_isinfd as *const () as usize),
        ("isnan$f32", llvm_lyte_isnanf as *const () as usize),
        ("isnan$f64", llvm_lyte_isnand as *const () as usize),
        ("pow$f32$f32", llvm_lyte_powf as *const () as usize),
        ("atan2$f32$f32", llvm_lyte_atan2f as *const () as usize),
        ("min$f32$f32", llvm_lyte_minf as *const () as usize),
        ("max$f32$f32", llvm_lyte_maxf as *const () as usize),
        ("pow$f64$f64", llvm_lyte_powd as *const () as usize),
        ("atan2$f64$f64", llvm_lyte_atan2d as *const () as usize),
        ("min$f64$f64", llvm_lyte_mind as *const () as usize),
        ("max$f64$f64", llvm_lyte_maxd as *const () as usize),
    ];
    for &(n, ptr) in pairs {
        if *name == Name::str(n) {
            return Some(ptr);
        }
    }
    None
}

/// Returns true if the type is returned via an output pointer.
fn returns_via_pointer(ty: crate::TypeID) -> bool {
    ty.is_ptr()
}

fn is_slice(ty: crate::TypeID) -> bool {
    matches!(&*ty, crate::Type::Slice(_))
}

impl crate::Type {
    fn llvm_basic_type<'ctx>(&self, ctx: &'ctx Context) -> BasicTypeEnum<'ctx> {
        let ptr_ty = ctx.ptr_type(AddressSpace::default());
        match self {
            crate::Type::Void => ctx.i32_type().into(),
            crate::Type::Bool => ctx.i8_type().into(),
            crate::Type::Int8 => ctx.i8_type().into(),
            crate::Type::UInt8 => ctx.i8_type().into(),
            crate::Type::Int32 => ctx.i32_type().into(),
            crate::Type::UInt32 => ctx.i32_type().into(),
            crate::Type::Float32 => ctx.f32_type().into(),
            crate::Type::Float64 => ctx.f64_type().into(),
            // Everything pointer-sized in Cranelift maps to a ptr or i64 in LLVM.
            crate::Type::Func(_, _) => ptr_ty.into(),
            crate::Type::Name(_, _) => ptr_ty.into(),
            crate::Type::Array(_, _) => ptr_ty.into(),
            crate::Type::Slice(_) => ptr_ty.into(),
            crate::Type::Tuple(_) => ptr_ty.into(),
            crate::Type::Anon(_) => panic!("anonymous type in codegen"),
            crate::Type::Var(_) => panic!("type var in codegen"),
        }
    }
}

/// Build an LLVM FunctionType for a Lyte function signature.
/// Prepends (globals_ptr, closure_ptr) and, if the return type needs a pointer,
/// an output_ptr parameter.
fn build_fn_type<'ctx>(
    ctx: &'ctx Context,
    dom: crate::TypeID,
    to: crate::TypeID,
    with_globals: bool,
) -> FunctionType<'ctx> {
    let ptr_ty = ctx.ptr_type(AddressSpace::default());
    let mut param_types: Vec<BasicMetadataTypeEnum<'ctx>> = vec![];

    if with_globals {
        param_types.push(ptr_ty.into()); // globals_base
        param_types.push(ptr_ty.into()); // closure_ptr
    }

    // Output pointer for types returned via pointer.
    if returns_via_pointer(to) {
        param_types.push(ptr_ty.into());
    }

    // Regular parameters from domain tuple.
    if let crate::Type::Tuple(args) = &*dom {
        for t in args {
            param_types.push(t.llvm_basic_type(ctx).into());
        }
    } else {
        panic!("build_fn_type: expected Tuple for domain, got {:?}", dom);
    }

    if *to == crate::Type::Void || returns_via_pointer(to) {
        ctx.void_type().fn_type(&param_types, false)
    } else {
        to.llvm_basic_type(ctx).fn_type(&param_types, false)
    }
}

// ─── Public API ────────────────────────────────────────────────────────────────

pub struct LLVMJIT {
    pub print_ir: bool,
    pub ir_only: bool,
}

/// Compile entry points and prepare the LLVM module (shared by compile_and_run and compile-only).
/// Returns (state, compile_time).
fn compile_with_context<'ctx>(
    context: &'ctx Context,
    decls: &DeclTable,
    entry_points: &[Name],
    print_ir: bool,
) -> Result<(LLVMJITState<'ctx>, Duration), String> {
    let mut state = LLVMJITState {
        context,
        module: context.create_module("lyte"),
        builder: context.create_builder(),
        globals: HashMap::new(),
        globals_size: 0,
        defined_functions: HashSet::new(),
        lambda_counter: 0,
        print_ir,
        extern_fns: Vec::new(),
    };

    let compile_start = Instant::now();

    state.declare_globals(decls);

    for &ep_name in entry_points {
        let ep_decls = decls.find(ep_name);
        if ep_decls.is_empty() {
            return Err(format!("entry point function '{}' not found", ep_name));
        }
        let ep_decl = if let Decl::Func(d) = &ep_decls[0] {
            d
        } else {
            return Err(format!("'{}' is not a function", ep_name));
        };
        state.compile_function(decls, ep_decl)?;
    }

    if print_ir {
        let ir = state.module.print_to_string().to_string();
        println!("LLVM IR:");
        for line in ir.lines() {
            if !line.is_empty() {
                println!("{}", line);
            }
        }
    }

    state
        .module
        .verify()
        .map_err(|e| format!("LLVM module verification failed: {}", e))?;

    // Run LLVM optimization passes (mem2reg, GVN, LICM, vectorization, etc.).
    let triple = TargetMachine::get_default_triple();
    let cpu = TargetMachine::get_host_cpu_name();
    let features = TargetMachine::get_host_cpu_features();
    let target =
        Target::from_triple(&triple).map_err(|e| format!("LLVM target from triple: {}", e))?;
    let machine = target
        .create_target_machine(
            &triple,
            cpu.to_str().unwrap_or("generic"),
            features.to_str().unwrap_or(""),
            OptimizationLevel::Aggressive,
            RelocMode::Default,
            CodeModel::JITDefault,
        )
        .ok_or("failed to create target machine")?;
    let pass_options = PassBuilderOptions::create();
    state
        .module
        .run_passes("default<O3>", &machine, pass_options)
        .map_err(|e| format!("LLVM pass pipeline failed: {}", e))?;

    if print_ir {
        let ir = state.module.print_to_string().to_string();
        println!("Optimized LLVM IR:");
        for line in ir.lines() {
            if !line.is_empty() {
                println!("{}", line);
            }
        }
    }

    let compile_elapsed = compile_start.elapsed();
    Ok((state, compile_elapsed))
}

/// Compile and run result: Ok(cancelled) or Err(msg).
fn compile_and_run_with_context(
    context: &Context,
    decls: &DeclTable,
    entry_points: &[Name],
    print_ir: bool,
    ir_only: bool,
) -> Result<(bool, Duration, Duration), String> {
    let (state, compile_elapsed) = compile_with_context(context, decls, entry_points, print_ir)?;

    if ir_only {
        return Ok((false, compile_elapsed, Duration::ZERO));
    }

    // Create JIT execution engine.
    let ee = state
        .module
        .create_jit_execution_engine(OptimizationLevel::Aggressive)
        .map_err(|e| format!("failed to create JIT execution engine: {}", e))?;

    // Register external symbol mappings.
    // Only map functions that still exist in the module after optimization —
    // LLVM O3 may remove unused declarations.
    for (name, addr) in &state.extern_fns {
        if let Some(fv) = state.module.get_function(name) {
            ee.add_global_mapping(&fv, *addr);
        }
    }

    // Look up the first entry point for execution.
    let run_name = &*entry_points[0];
    let fn_addr = ee
        .get_function_address(run_name)
        .map_err(|e| format!("function '{}' not found in JIT: {:?}", run_name, e))?;

    println!("compilation successful");

    // Execute while the EE and context are still alive.
    let exec_start = Instant::now();
    type Entry = unsafe extern "C" fn(*mut u8, *mut u8);
    let mut globals: Vec<u8> = vec![0u8; state.globals_size];
    let cancelled = unsafe {
        extern "C" {
            fn setjmp(env: *mut u8) -> i32;
        }
        // Initialize cancel counter (no callback = no cancellation).
        crate::cancel::set_cancel_callback(globals.as_mut_ptr(), None, std::ptr::null_mut());
        let jmp_buf_ptr = globals.as_mut_ptr().add(crate::cancel::JMPBUF_OFFSET);
        if setjmp(jmp_buf_ptr) == 0 {
            let code_fn: Entry = std::mem::transmute(fn_addr);
            let null_closure: *mut u8 = std::ptr::null_mut();
            code_fn(globals.as_mut_ptr(), null_closure);
            false
        } else {
            true
        }
    };
    let exec_elapsed = exec_start.elapsed();

    Ok((cancelled, compile_elapsed, exec_elapsed))
}

/// A compiled LLVM JIT program that keeps the execution engine alive.
/// Function pointers remain valid for the lifetime of this struct.
pub struct LLVMCompiledProgram {
    /// Entry point function addresses (name -> address).
    pub entry_points: HashMap<Name, usize>,
    /// Total size of globals buffer needed.
    pub globals_size: usize,
    /// The execution engine (must be dropped before _context).
    /// Safety: the 'static lifetime is erased; the actual lifetime is tied to _context.
    _ee: Option<inkwell::execution_engine::ExecutionEngine<'static>>,
    /// The LLVM context that owns all IR. Dropped after _ee.
    _context: Option<Box<Context>>,
}

impl Drop for LLVMCompiledProgram {
    fn drop(&mut self) {
        // Drop EE first (it references the context).
        self._ee.take();
        self._context.take();
    }
}

impl LLVMJIT {
    pub fn new() -> Self {
        Self {
            print_ir: false,
            ir_only: false,
        }
    }

    /// Compile entry points and return a handle that keeps function pointers alive.
    pub fn compile_only(
        &self,
        decls: &DeclTable,
        entry_points: &[Name],
    ) -> Result<LLVMCompiledProgram, String> {
        Target::initialize_native(&InitializationConfig::default())
            .map_err(|e| format!("LLVM target init failed: {}", e))?;

        let context = Box::new(Context::create());

        // Safety: we transmute the context reference to 'static because we own
        // the Context in the same struct and guarantee drop order (EE before Context).
        let context_ref: &'static Context = unsafe { &*(context.as_ref() as *const Context) };

        let (state, _compile_elapsed) =
            compile_with_context(context_ref, decls, entry_points, self.print_ir)?;

        let ee = state
            .module
            .create_jit_execution_engine(OptimizationLevel::Aggressive)
            .map_err(|e| format!("failed to create JIT execution engine: {}", e))?;

        // Register external symbol mappings.
        for (name, addr) in &state.extern_fns {
            if let Some(fv) = state.module.get_function(name) {
                ee.add_global_mapping(&fv, *addr);
            }
        }

        // Look up all entry point addresses.
        let mut ep_map = HashMap::new();
        for &ep_name in entry_points {
            let fn_addr = ee
                .get_function_address(&*ep_name)
                .map_err(|e| format!("function '{}' not found in JIT: {:?}", ep_name, e))?;
            ep_map.insert(ep_name, fn_addr);
        }

        // Erase EE lifetime to 'static (safe because we control drop order).
        let ee_static: inkwell::execution_engine::ExecutionEngine<'static> =
            unsafe { std::mem::transmute(ee) };

        Ok(LLVMCompiledProgram {
            entry_points: ep_map,
            globals_size: state.globals_size,
            _ee: Some(ee_static),
            _context: Some(context),
        })
    }

    /// Compile and execute main. Returns Ok((cancelled, compile_time, exec_time)) or Err.
    pub fn compile_and_run(&self, decls: &DeclTable) -> Result<(bool, Duration, Duration), String> {
        let main = Name::new("main".into());
        self.compile_and_run_multi(decls, &[main])
    }

    /// Compile multiple entry points and execute the first one.
    /// Returns Ok((cancelled, compile_time, exec_time)) or Err.
    pub fn compile_and_run_multi(
        &self,
        decls: &DeclTable,
        entry_points: &[Name],
    ) -> Result<(bool, Duration, Duration), String> {
        Target::initialize_native(&InitializationConfig::default())
            .map_err(|e| format!("LLVM target init failed: {}", e))?;

        let context = Context::create();
        compile_and_run_with_context(&context, decls, entry_points, self.print_ir, self.ir_only)
    }
}

// ─── Internal compilation state ────────────────────────────────────────────────

struct LLVMJITState<'ctx> {
    context: &'ctx Context,
    module: Module<'ctx>,
    builder: Builder<'ctx>,
    globals: HashMap<Name, i32>,
    globals_size: usize,
    defined_functions: HashSet<Name>,
    lambda_counter: usize,
    print_ir: bool,
    /// External function names and addresses, to be mapped after EE creation.
    /// We store names (not FunctionValues) because LLVM optimization passes may
    /// delete unused declarations, invalidating FunctionValue pointers.
    extern_fns: Vec<(String, usize)>,
}

impl<'ctx> LLVMJITState<'ctx> {
    fn ptr_ty(&self) -> inkwell::types::PointerType<'ctx> {
        self.context.ptr_type(AddressSpace::default())
    }

    fn i8_ty(&self) -> inkwell::types::IntType<'ctx> {
        self.context.i8_type()
    }
    fn i32_ty(&self) -> inkwell::types::IntType<'ctx> {
        self.context.i32_type()
    }
    fn i64_ty(&self) -> inkwell::types::IntType<'ctx> {
        self.context.i64_type()
    }
    fn f32_ty(&self) -> inkwell::types::FloatType<'ctx> {
        self.context.f32_type()
    }
    fn f64_ty(&self) -> inkwell::types::FloatType<'ctx> {
        self.context.f64_type()
    }

    fn declare_globals(&mut self, decls: &DeclTable) {
        let mut offset: i32 = CANCEL_FLAG_RESERVED;
        for decl in &decls.decls {
            match decl {
                Decl::Global { name, ty, .. } => {
                    self.globals.insert(*name, offset);
                    offset += ty.size(decls) as i32;
                }
                Decl::Func(f) if f.is_extern => {
                    // Extern functions get 16 bytes: {fn_ptr, context}
                    self.globals.insert(f.name, offset);
                    offset += 16;
                }
                _ => {}
            }
        }
        self.globals_size = offset as usize;
    }

    fn get_or_declare_extern(
        &mut self,
        sym: &str,
        fn_type: FunctionType<'ctx>,
        addr: usize,
    ) -> FunctionValue<'ctx> {
        if let Some(f) = self.module.get_function(sym) {
            f
        } else {
            let f = self
                .module
                .add_function(sym, fn_type, Some(Linkage::External));
            self.extern_fns.push((sym.to_string(), addr));
            f
        }
    }

    fn compile_function(
        &mut self,
        decls: &DeclTable,
        decl: &FuncDecl,
    ) -> Result<FunctionValue<'ctx>, String> {
        // Build the LLVM function type (globals + closure + params → ret).
        let fn_type = build_fn_type(self.context, decl.domain(), decl.ret, true);

        // Get or add the function in the module.
        let function = if let Some(f) = self.module.get_function(&*decl.name) {
            f
        } else {
            self.module
                .add_function(&*decl.name, fn_type, Some(Linkage::External))
        };

        // If no body, it's an external declaration — just return.
        let body_id = match decl.body {
            Some(id) => id,
            None => {
                self.defined_functions.insert(decl.name);
                return Ok(function);
            }
        };

        // Create entry basic block.
        let entry_bb = self.context.append_basic_block(function, "entry");
        self.builder.position_at_end(entry_bb);

        // Extract parameters.
        let params: Vec<BasicValueEnum<'ctx>> = function.get_params();
        let mut param_idx = 0usize;

        let globals_base: PointerValue<'ctx> = params[param_idx].into_pointer_value();
        param_idx += 1;
        let closure_ptr: PointerValue<'ctx> = params[param_idx].into_pointer_value();
        param_idx += 1;

        let output_ptr: Option<PointerValue<'ctx>> = if returns_via_pointer(decl.ret) {
            let p = params[param_idx].into_pointer_value();
            param_idx += 1;
            Some(p)
        } else {
            None
        };

        // Set up local variable map: name → alloca pointer.
        let mut variables: HashMap<String, PointerValue<'ctx>> = HashMap::new();
        let mut let_bindings: HashSet<String> = HashSet::new();

        // Load closure variables (captured variables).
        for (i, cv) in decl.closure_vars.iter().enumerate() {
            // Each slot in the closure struct holds a pointer to the captured var.
            let slot_ptr = unsafe {
                self.builder
                    .build_in_bounds_gep(
                        self.i8_ty(),
                        closure_ptr,
                        &[self.i64_ty().const_int((i * 8) as u64, false).into()],
                        "cv_slot",
                    )
                    .unwrap()
            };
            // The slot holds a pointer (the address of the variable's storage).
            let var_ptr = self
                .builder
                .build_load(self.ptr_ty(), slot_ptr, "cv_ptr")
                .unwrap()
                .into_pointer_value();
            // Store the pointer in an alloca so we can use_var-style access.
            let alloca = self
                .builder
                .build_alloca(self.ptr_ty(), &cv.name.to_string())
                .unwrap();
            self.builder.build_store(alloca, var_ptr).unwrap();
            variables.insert(cv.name.to_string(), alloca);
            // closure vars are treated like var bindings (pointer-indirected)
        }

        // Function parameters → let bindings.
        for (i, param) in decl.params.iter().enumerate() {
            let param_val = params[param_idx + i];
            let ty = param.ty.expect("param ty");

            // Slice parameters get noalias: the type checker enforces that no two
            // slice arguments in a call can alias (Fortran-style restrict).
            if matches!(&*ty, crate::Type::Slice(_)) {
                let noalias = self.context.create_enum_attribute(
                    inkwell::attributes::Attribute::get_named_enum_kind_id("noalias"),
                    0,
                );
                // LLVM attribute index: 0 = return, params start at 1.
                function.add_attribute(
                    inkwell::attributes::AttributeLoc::Param((param_idx + i) as u32),
                    noalias,
                );
            }

            let alloca = self
                .builder
                .build_alloca(ty.llvm_basic_type(self.context), &param.name.to_string())
                .unwrap();
            self.builder.build_store(alloca, param_val).unwrap();
            variables.insert(param.name.to_string(), alloca);
            let_bindings.insert(param.name.to_string());
        }

        let mut trans = FunctionTranslator {
            state: self,
            function,
            variables,
            let_bindings,
            globals_base,
            output_ptr,
            decls,
            called_functions: HashSet::new(),
            pending_lambdas: Vec::new(),
            loop_stack: Vec::new(),
        };

        // Cancel-check at function entry.
        trans.emit_cancel_check();

        let result = trans.translate_expr(body_id, decl);

        // Emit return if the current block isn't already terminated
        // (e.g. by an explicit `return` statement).
        if !trans.is_block_terminated() {
            if returns_via_pointer(decl.ret) && result.is_pointer_value() {
                // memcpy result to output_ptr, return void.
                let out = trans.output_ptr.unwrap();
                let size = decl.ret.size(decls) as u64;
                trans.emit_memcpy(out, result.into_pointer_value(), size);
                trans.state.builder.build_return(None).unwrap();
            } else if *decl.ret == crate::Type::Void || returns_via_pointer(decl.ret) {
                trans.state.builder.build_return(None).unwrap();
            } else {
                let coerced = trans.coerce_return(result, decl.ret);
                trans.state.builder.build_return(Some(&coerced)).unwrap();
            }
        }

        let called = trans.called_functions.clone();
        let pending = trans.pending_lambdas.clone();

        self.defined_functions.insert(decl.name);

        // Compile pending lambdas.
        for lambda_decl in pending {
            if !self.defined_functions.contains(&lambda_decl.name) {
                self.compile_function(decls, &lambda_decl)?;
            }
        }

        // Compile called user functions.
        for name in called {
            if self.defined_functions.contains(&name) {
                continue;
            }
            let found = decls.find(name);
            if found.is_empty() {
                continue;
            }
            if let Decl::Func(d) = &found[0] {
                if d.body.is_none() {
                    continue;
                }
                self.compile_function(decls, d)?;
            }
        }

        Ok(function)
    }
}

// ─── Per-function translator ────────────────────────────────────────────────────

struct FunctionTranslator<'a, 'ctx> {
    state: &'a mut LLVMJITState<'ctx>,
    function: FunctionValue<'ctx>,
    /// name → alloca. For let-bindings the alloca holds the value directly.
    /// For var-bindings the alloca holds a pointer to the actual stack slot.
    variables: HashMap<String, PointerValue<'ctx>>,
    let_bindings: HashSet<String>,
    globals_base: PointerValue<'ctx>,
    output_ptr: Option<PointerValue<'ctx>>,
    decls: &'a DeclTable,
    called_functions: HashSet<Name>,
    pending_lambdas: Vec<FuncDecl>,
    /// Stack of (continue_bb, break_bb) for nested loops.
    loop_stack: Vec<(BasicBlock<'ctx>, BasicBlock<'ctx>)>,
}

impl<'a, 'ctx> FunctionTranslator<'a, 'ctx> {
    fn ctx(&self) -> &'ctx Context {
        self.state.context
    }
    fn builder(&self) -> &Builder<'ctx> {
        &self.state.builder
    }
    fn module(&self) -> &Module<'ctx> {
        &self.state.module
    }

    fn ptr_ty(&self) -> inkwell::types::PointerType<'ctx> {
        self.ctx().ptr_type(AddressSpace::default())
    }
    fn i8_ty(&self) -> inkwell::types::IntType<'ctx> {
        self.ctx().i8_type()
    }
    fn i32_ty(&self) -> inkwell::types::IntType<'ctx> {
        self.ctx().i32_type()
    }
    fn i64_ty(&self) -> inkwell::types::IntType<'ctx> {
        self.ctx().i64_type()
    }

    fn zero_i32(&self) -> BasicValueEnum<'ctx> {
        self.i32_ty().const_int(0, false).into()
    }

    /// Ensure a value matches its expected LLVM type.
    /// Mainly handles i1 → i8 promotion for booleans.
    fn coerce_to_type(
        &self,
        val: BasicValueEnum<'ctx>,
        expected: BasicTypeEnum<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        if val.get_type() == expected {
            return val;
        }
        // i1 → i8 (bool comparison result → Bool type)
        if val.is_int_value() && expected.is_int_type() {
            let iv = val.into_int_value();
            let et = expected.into_int_type();
            if iv.get_type().get_bit_width() < et.get_bit_width() {
                return self
                    .builder()
                    .build_int_z_extend(iv, et, "zext")
                    .unwrap()
                    .into();
            }
            if iv.get_type().get_bit_width() > et.get_bit_width() {
                return self
                    .builder()
                    .build_int_truncate(iv, et, "trunc")
                    .unwrap()
                    .into();
            }
        }
        val
    }

    /// Ensure value is i1 for use as a branch condition.
    fn to_i1(&self, val: inkwell::values::IntValue<'ctx>) -> inkwell::values::IntValue<'ctx> {
        if val.get_type().get_bit_width() == 1 {
            return val;
        }
        let zero = val.get_type().const_int(0, false);
        self.builder()
            .build_int_compare(IntPredicate::NE, val, zero, "tobool")
            .unwrap()
    }

    /// Coerce a return value to match the function's LLVM return type.
    fn coerce_return(
        &self,
        val: BasicValueEnum<'ctx>,
        ret_ty: crate::TypeID,
    ) -> BasicValueEnum<'ctx> {
        if *ret_ty == crate::Type::Void || returns_via_pointer(ret_ty) {
            return val;
        }
        let expected = ret_ty.llvm_basic_type(self.ctx());
        self.coerce_to_type(val, expected)
    }

    fn is_block_terminated(&self) -> bool {
        let bb = self.builder().get_insert_block().unwrap();
        bb.get_terminator().is_some()
    }

    fn append_bb(&self, name: &str) -> inkwell::basic_block::BasicBlock<'ctx> {
        self.ctx().append_basic_block(self.function, name)
    }

    /// Create an alloca in the entry block so it doesn't grow the stack in loops.
    fn entry_alloca(&self, ty: BasicTypeEnum<'ctx>, name: &str) -> PointerValue<'ctx> {
        let entry_bb = self.function.get_first_basic_block().unwrap();
        let current_bb = self.builder().get_insert_block().unwrap();
        // Position at the start of entry block (before the first instruction).
        if let Some(first_inst) = entry_bb.get_first_instruction() {
            self.builder().position_before(&first_inst);
        } else {
            self.builder().position_at_end(entry_bb);
        }
        let alloca = self.builder().build_alloca(ty, name).unwrap();
        // Restore position.
        self.builder().position_at_end(current_bb);
        alloca
    }

    /// Create an array alloca in the entry block.
    fn entry_array_alloca(
        &self,
        elem_ty: inkwell::types::IntType<'ctx>,
        size: u64,
        name: &str,
    ) -> PointerValue<'ctx> {
        let entry_bb = self.function.get_first_basic_block().unwrap();
        let current_bb = self.builder().get_insert_block().unwrap();
        if let Some(first_inst) = entry_bb.get_first_instruction() {
            self.builder().position_before(&first_inst);
        } else {
            self.builder().position_at_end(entry_bb);
        }
        let alloca = self
            .builder()
            .build_array_alloca(elem_ty, self.i64_ty().const_int(size, false), name)
            .unwrap();
        self.builder().position_at_end(current_bb);
        alloca
    }

    fn emit_cancel_check(&mut self) {
        let check_bb = self.append_bb("cancel_check");
        let cont_bb = self.append_bb("cont");

        // Load cancel counter (i32 at offset 0 of globals).
        let counter_ptr = self.globals_base; // offset 0
        let counter = self
            .builder()
            .build_load(self.i32_ty(), counter_ptr, "cancel_counter")
            .unwrap()
            .into_int_value();

        // Decrement counter
        let one = self.i32_ty().const_int(1, false);
        let new_counter = self
            .builder()
            .build_int_sub(counter, one, "new_counter")
            .unwrap();
        self.builder()
            .build_store(counter_ptr, new_counter)
            .unwrap();

        // If counter <= 0, call the cancel check function
        let zero = self.i32_ty().const_int(0, false);
        let is_zero = self
            .builder()
            .build_int_compare(IntPredicate::SLE, new_counter, zero, "counter_expired")
            .unwrap();
        self.builder()
            .build_conditional_branch(is_zero, check_bb, cont_bb)
            .unwrap();

        // Check block: call __llvm_lyte_cancel_check(globals_base).
        // It resets the counter and calls the user callback.
        // If callback returns true, it longjmps (never returns).
        self.builder().position_at_end(check_bb);
        let check_ty = self
            .ctx()
            .void_type()
            .fn_type(&[self.ptr_ty().into()], false);
        let check_fn = self.state.get_or_declare_extern(
            "__llvm_lyte_cancel_check",
            check_ty,
            llvm_lyte_cancel_check as *const () as usize,
        );
        self.builder()
            .build_call(check_fn, &[self.globals_base.into()], "")
            .unwrap();
        self.builder().build_unconditional_branch(cont_bb).unwrap();

        self.builder().position_at_end(cont_bb);
    }

    fn emit_memcpy(&self, dst: PointerValue<'ctx>, src: PointerValue<'ctx>, size: u64) {
        let size_val = self.i64_ty().const_int(size, false);
        self.builder()
            .build_memcpy(dst, 1, src, 1, size_val)
            .unwrap();
    }

    fn emit_memzero(&self, dst: PointerValue<'ctx>, size: usize) {
        let i8_zero = self.i8_ty().const_int(0, false);
        let size_val = self.i64_ty().const_int(size as u64, false);
        self.builder()
            .build_memset(dst, 1, i8_zero, size_val)
            .unwrap();
    }

    /// Declare (alloca) a new variable. Returns the alloca pointer.
    fn declare_var(&mut self, name: &str, ty: BasicTypeEnum<'ctx>) -> PointerValue<'ctx> {
        if let Some(&ptr) = self.variables.get(name) {
            return ptr;
        }
        let alloca = self.entry_alloca(ty, name);
        self.variables.insert(name.to_string(), alloca);
        alloca
    }

    /// Get the address of a variable (for lvalue use or closure capture).
    fn get_var_addr(&mut self, name: &str, _ty: crate::TypeID) -> PointerValue<'ctx> {
        if let Some(&alloca) = self.variables.get(name) {
            if self.let_bindings.contains(name) {
                // let binding: alloca holds the value — alloca itself is the address.
                return alloca;
            } else {
                // var binding: alloca holds a pointer to the slot.
                return self
                    .builder()
                    .build_load(self.ptr_ty(), alloca, "var_ptr")
                    .unwrap()
                    .into_pointer_value();
            }
        }
        if let Some(&offset) = self.state.globals.get(&Name::new(name.to_string())) {
            return self.ptr_at_offset(self.globals_base, offset as u64);
        }
        panic!("unknown variable in closure capture: {}", name)
    }

    /// Compute globals_base + byte_offset as a pointer.
    fn ptr_at_offset(&self, base: PointerValue<'ctx>, byte_offset: u64) -> PointerValue<'ctx> {
        if byte_offset == 0 {
            return base;
        }
        unsafe {
            self.builder()
                .build_in_bounds_gep(
                    self.i8_ty(),
                    base,
                    &[self.i64_ty().const_int(byte_offset, false).into()],
                    "gep",
                )
                .unwrap()
        }
    }

    /// Compute ptr + byte_offset (i64 offset value).
    fn ptr_add_i64(
        &self,
        base: PointerValue<'ctx>,
        offset: inkwell::values::IntValue<'ctx>,
    ) -> PointerValue<'ctx> {
        unsafe {
            self.builder()
                .build_in_bounds_gep(self.i8_ty(), base, &[offset.into()], "ptr_add")
                .unwrap()
        }
    }

    fn gen_copy(&mut self, ty: crate::TypeID, dst: PointerValue<'ctx>, src: BasicValueEnum<'ctx>) {
        if ty.is_ptr() {
            let size = ty.size(self.decls) as u64;
            self.emit_memcpy(dst, src.into_pointer_value(), size);
        } else {
            self.builder().build_store(dst, src).unwrap();
        }
    }

    fn gen_zero_mem(&self, dst: PointerValue<'ctx>, size: usize) {
        self.emit_memzero(dst, size);
    }

    fn gen_eq(
        &mut self,
        ty: crate::TypeID,
        lhs: BasicValueEnum<'ctx>,
        rhs: BasicValueEnum<'ctx>,
    ) -> BasicValueEnum<'ctx> {
        if let crate::Type::Slice(elem) = &*ty {
            let elem_size = elem.size(self.decls) as u64;
            return self.gen_slice_eq(lhs, rhs, elem_size);
        }
        if ty.is_ptr() {
            // memcmp via LLVM memcmp intrinsic or byte-by-byte.
            // Simple approach: call memcmp.
            let size = ty.size(self.decls) as u64;
            let memcmp_ty = self.i32_ty().fn_type(
                &[
                    self.ptr_ty().into(),
                    self.ptr_ty().into(),
                    self.i64_ty().into(),
                ],
                false,
            );
            let memcmp_fn = self.state.get_or_declare_extern(
                "memcmp",
                memcmp_ty,
                libc::memcmp as *const () as usize,
            );
            let size_val = self.i64_ty().const_int(size, false);
            let result = self
                .builder()
                .build_call(
                    memcmp_fn,
                    &[lhs.into(), rhs.into(), size_val.into()],
                    "memcmp",
                )
                .unwrap()
                .try_as_basic_value()
                .unwrap_basic()
                .into_int_value();
            let zero = self.i32_ty().const_int(0, false);
            self.builder()
                .build_int_compare(IntPredicate::EQ, result, zero, "eq")
                .unwrap()
                .into()
        } else {
            match *ty {
                crate::Type::Float32 | crate::Type::Float64 => {
                    let l = lhs.into_float_value();
                    let r = rhs.into_float_value();
                    self.builder()
                        .build_float_compare(FloatPredicate::OEQ, l, r, "feq")
                        .unwrap()
                        .into()
                }
                _ => {
                    let l = lhs.into_int_value();
                    let r = rhs.into_int_value();
                    self.builder()
                        .build_int_compare(IntPredicate::EQ, l, r, "ieq")
                        .unwrap()
                        .into()
                }
            }
        }
    }

    /// Compare two slices by contents. Calls `lyte_slice_eq(a, b, elem_size)`.
    fn gen_slice_eq(
        &mut self,
        lhs: BasicValueEnum<'ctx>,
        rhs: BasicValueEnum<'ctx>,
        elem_size: u64,
    ) -> BasicValueEnum<'ctx> {
        let fn_ty = self.i64_ty().fn_type(
            &[
                self.ptr_ty().into(),
                self.ptr_ty().into(),
                self.i64_ty().into(),
            ],
            false,
        );
        let slice_eq_fn = self.state.get_or_declare_extern(
            "__llvm_lyte_slice_eq",
            fn_ty,
            lyte_slice_eq as *const () as usize,
        );
        let elem_size_val = self.i64_ty().const_int(elem_size, false);
        let result = self
            .builder()
            .build_call(
                slice_eq_fn,
                &[lhs.into(), rhs.into(), elem_size_val.into()],
                "slice_eq",
            )
            .unwrap()
            .try_as_basic_value()
            .unwrap_basic()
            .into_int_value();
        // Truncate i64 result to i1
        let zero = self.i64_ty().const_int(0, false);
        self.builder()
            .build_int_compare(IntPredicate::NE, result, zero, "slice_eq_bool")
            .unwrap()
            .into()
    }

    /// Match `{ var = expr }` or `var = expr` — returns (var_name, rhs_id) if matched.
    fn match_single_var_assign(arena: &ExprArena, expr_id: ExprID) -> Option<(Name, ExprID)> {
        let expr = &arena.exprs[expr_id];
        let inner = if let Expr::Block(stmts) = expr {
            if stmts.len() == 1 {
                &arena.exprs[stmts[0]]
            } else {
                return None;
            }
        } else {
            expr
        };
        if let Expr::Binop(Binop::Assign, lhs, rhs) = inner {
            if let Expr::Id(name) = &arena.exprs[*lhs] {
                return Some((*name, *rhs));
            }
        }
        None
    }

    // ── lvalue ─────────────────────────────────────────────────────────────────

    fn translate_lvalue(&mut self, expr: ExprID, decl: &FuncDecl) -> PointerValue<'ctx> {
        match &decl.arena[expr] {
            Expr::Id(name) => {
                if let Some(&alloca) = self.variables.get(&**name) {
                    if self.let_bindings.contains(&**name) {
                        let ty = decl.types[expr];
                        if ty.is_ptr() || matches!(&*ty, crate::Type::Slice(_)) {
                            // Pointer-type let binding (e.g. slice/array/struct param):
                            // alloca holds a pointer to the data, load it.
                            self.builder()
                                .build_load(self.ptr_ty(), alloca, "let_ptr")
                                .unwrap()
                                .into_pointer_value()
                        } else {
                            alloca
                        }
                    } else {
                        self.builder()
                            .build_load(self.ptr_ty(), alloca, "var_addr")
                            .unwrap()
                            .into_pointer_value()
                    }
                } else if let Some(&offset) = self.state.globals.get(name) {
                    self.ptr_at_offset(self.globals_base, offset as u64)
                } else {
                    panic!("JIT lvalue: unknown variable {:?}", name)
                }
            }
            Expr::Field(lhs_id, field_name) => {
                let lhs_ty = decl.types[*lhs_id];
                let base_ptr = self.translate_lvalue(*lhs_id, decl);
                self.compute_field_ptr(base_ptr, lhs_ty, field_name, decl)
            }
            Expr::ArrayIndex(lhs_id, idx_id) => {
                let lhs_ty = decl.types[*lhs_id];
                let lhs_ptr = self.translate_lvalue(*lhs_id, decl);
                let idx_val = self.translate_expr(*idx_id, decl).into_int_value();
                self.compute_array_elem_ptr(lhs_ptr, lhs_ty, idx_val)
            }
            _ => self.translate_expr(expr, decl).into_pointer_value(),
        }
    }

    fn compute_field_ptr(
        &mut self,
        base: PointerValue<'ctx>,
        lhs_ty: crate::TypeID,
        field_name: &Name,
        _decl: &FuncDecl,
    ) -> PointerValue<'ctx> {
        if let crate::Type::Name(struct_name, type_args) = &*lhs_ty {
            let struct_decl = self.decls.find(*struct_name);
            if let crate::Decl::Struct(s) = &struct_decl[0] {
                let inst: crate::Instance = s
                    .typevars
                    .iter()
                    .zip(type_args.iter())
                    .map(|(tv, ty)| (crate::types::mk_type(crate::Type::Var(*tv)), *ty))
                    .collect();
                let off = s.field_offset(field_name, self.decls, &inst);
                return self.ptr_at_offset(base, off as u64);
            }
        } else if let crate::Type::Tuple(elem_types) = &*lhs_ty {
            let index: usize = field_name.parse().expect("tuple field numeric");
            let mut off = 0u64;
            for i in 0..index {
                off += elem_types[i].size(self.decls) as u64;
            }
            return self.ptr_at_offset(base, off);
        }
        panic!("compute_field_ptr: unexpected type {:?}", lhs_ty)
    }

    fn compute_array_elem_ptr(
        &mut self,
        base: PointerValue<'ctx>,
        lhs_ty: crate::TypeID,
        idx_val: inkwell::values::IntValue<'ctx>,
    ) -> PointerValue<'ctx> {
        let (elem_ty, is_sl) = match &*lhs_ty {
            crate::Type::Array(ty, _) => (*ty, false),
            crate::Type::Slice(ty) => (*ty, true),
            _ => panic!("array index on non-array type"),
        };
        let data_ptr = if is_sl {
            self.builder()
                .build_load(self.ptr_ty(), base, "slice_data")
                .unwrap()
                .into_pointer_value()
        } else {
            base
        };
        let elem_size = self
            .i64_ty()
            .const_int(elem_ty.size(self.decls) as u64, false);
        // Extend idx to i64.
        let idx_i64 = self
            .builder()
            .build_int_z_extend(idx_val, self.i64_ty(), "idx64")
            .unwrap();
        let byte_off = self
            .builder()
            .build_int_mul(idx_i64, elem_size, "byte_off")
            .unwrap();
        self.ptr_add_i64(data_ptr, byte_off)
    }

    // ── rvalue ─────────────────────────────────────────────────────────────────

    fn translate_expr(&mut self, expr: ExprID, decl: &FuncDecl) -> BasicValueEnum<'ctx> {
        match &decl.arena[expr] {
            Expr::True => self.i8_ty().const_int(1, false).into(),
            Expr::False => self.i8_ty().const_int(0, false).into(),
            Expr::Int(n) => self.i32_ty().const_int(*n as u64, true).into(),
            Expr::UInt(n) => self.i32_ty().const_int(*n, false).into(),
            Expr::Real(s) => {
                let val: f64 = s.parse().expect("invalid float literal");
                match &*decl.types[expr] {
                    crate::Type::Float32 => self.state.f32_ty().const_float(val).into(),
                    _ => self.state.f64_ty().const_float(val).into(),
                }
            }
            Expr::Char(c) => self.i8_ty().const_int(*c as u64, false).into(),
            Expr::Id(name) => {
                let ty = decl.types[expr];
                if let Some(&alloca) = self.variables.get(&**name) {
                    if self.let_bindings.contains(&**name) || ty.is_ptr() {
                        // let binding or pointer type: load the value from the alloca.
                        self.builder()
                            .build_load(ty.llvm_basic_type(self.ctx()), alloca, &**name)
                            .unwrap()
                    } else {
                        // var binding: alloca holds a pointer to the slot; load ptr, then load value.
                        let slot_ptr = self
                            .builder()
                            .build_load(self.ptr_ty(), alloca, "var_ptr")
                            .unwrap()
                            .into_pointer_value();
                        self.builder()
                            .build_load(ty.llvm_basic_type(self.ctx()), slot_ptr, &**name)
                            .unwrap()
                    }
                } else if let Some(&offset) = self.state.globals.get(name) {
                    let addr = self.ptr_at_offset(self.globals_base, offset as u64);
                    // Composite types (arrays, structs) are pointer-represented:
                    // return the address, don't load.
                    if ty.is_ptr() {
                        addr.into()
                    } else {
                        self.builder()
                            .build_load(ty.llvm_basic_type(self.ctx()), addr, &**name)
                            .unwrap()
                    }
                } else {
                    // Must be a function reference.
                    self.translate_func_ref(name, &*ty)
                }
            }
            Expr::Binop(op, lhs_id, rhs_id) => {
                let (op, lhs, rhs) = (*op, *lhs_id, *rhs_id);
                self.translate_binop(op, lhs, rhs, decl)
            }
            Expr::Unop(op, arg_id) => {
                let (op, arg_id) = (*op, *arg_id);
                self.translate_unop(op, arg_id, decl)
            }
            Expr::Call(fn_id, arg_ids) => {
                let (fn_id, arg_ids) = (*fn_id, arg_ids.clone());
                self.translate_call(fn_id, &arg_ids, expr, decl)
            }
            Expr::Let(name, init_id, _) => {
                let (name, init_id) = (*name, *init_id);
                let ty = decl.types[expr];
                let init_val = self.translate_expr(init_id, decl);
                let alloca = self.entry_alloca(ty.llvm_basic_type(self.ctx()), &*name);
                self.builder().build_store(alloca, init_val).unwrap();
                self.variables.insert(name.to_string(), alloca);
                self.let_bindings.insert(name.to_string());
                init_val
            }
            Expr::Var(name, init_id, _) => {
                let (name, init_id) = (*name, *init_id);
                let ty = decl.types[expr];
                let sz = ty.size(self.decls) as usize;
                assert!(sz > 0, "var size must be > 0");

                if !ty.is_ptr() {
                    // Scalar var: use a single typed alloca (same as let bindings).
                    // This avoids double-indirection and lets LLVM promote to SSA.
                    let alloca = self.entry_alloca(ty.llvm_basic_type(self.ctx()), &*name);
                    if let Some(init_id) = init_id {
                        let init_val = self.translate_expr(init_id, decl);
                        self.builder().build_store(alloca, init_val).unwrap();
                    } else {
                        let zero = ty.llvm_basic_type(self.ctx()).const_zero();
                        self.builder().build_store(alloca, zero).unwrap();
                    }
                    self.variables.insert(name.to_string(), alloca);
                    self.let_bindings.insert(name.to_string());
                } else {
                    // Pointer/struct var: allocate byte storage + ptr alloca.
                    let storage = self.entry_array_alloca(
                        self.i8_ty(),
                        sz as u64,
                        &format!("{}_storage", name),
                    );
                    let alloca = self.entry_alloca(self.ptr_ty().into(), &*name);
                    self.builder().build_store(alloca, storage).unwrap();
                    self.variables.insert(name.to_string(), alloca);

                    if let Some(init_id) = init_id {
                        let init_val = self.translate_expr(init_id, decl);
                        self.gen_copy(ty, storage, init_val);
                    } else {
                        self.gen_zero_mem(storage, sz);
                    }
                }
                self.zero_i32()
            }
            Expr::Field(lhs_id, field_name) => {
                let (lhs_id, field_name) = (*lhs_id, *field_name);
                let lhs_ty = decl.types[lhs_id];
                // Handle .len.
                if *field_name == "len" {
                    match &*lhs_ty {
                        crate::Type::Slice(_) => {
                            let lhs_val = self.translate_expr(lhs_id, decl).into_pointer_value();
                            let len_ptr = self.ptr_at_offset(lhs_val, 8);
                            return self
                                .builder()
                                .build_load(self.i32_ty(), len_ptr, "slice_len")
                                .unwrap();
                        }
                        crate::Type::Array(_, sz) => {
                            return self.i32_ty().const_int(sz.known() as u64, false).into();
                        }
                        _ => {}
                    }
                }
                let lhs_val = self.translate_expr(lhs_id, decl).into_pointer_value();
                let field_ty = decl.types[expr];
                let field_ptr = self.compute_field_ptr(lhs_val, lhs_ty, &field_name, decl);
                if field_ty.is_ptr() {
                    field_ptr.into()
                } else {
                    self.builder()
                        .build_load(field_ty.llvm_basic_type(self.ctx()), field_ptr, "field")
                        .unwrap()
                }
            }
            Expr::ArrayIndex(lhs_id, rhs_id) => {
                let (lhs_id, rhs_id) = (*lhs_id, *rhs_id);
                let lhs_ty = decl.types[lhs_id];
                let lhs_val = self.translate_expr(lhs_id, decl).into_pointer_value();
                let rhs_val = self.translate_expr(rhs_id, decl).into_int_value();
                let elem_ptr = self.compute_array_elem_ptr(lhs_val, lhs_ty, rhs_val);
                let result_ty = decl.types[expr];
                if result_ty.is_ptr() {
                    elem_ptr.into()
                } else {
                    self.builder()
                        .build_load(result_ty.llvm_basic_type(self.ctx()), elem_ptr, "elem")
                        .unwrap()
                }
            }
            Expr::ArrayLiteral(elements) => {
                let elements = elements.clone();
                let ty = decl.types[expr];
                let elem_values: Vec<BasicValueEnum<'ctx>> = elements
                    .iter()
                    .map(|e| self.translate_expr(*e, decl))
                    .collect();
                if let crate::Type::Array(elem_ty, _) = &*ty {
                    let elem_size = elem_ty.size(self.decls) as u64;
                    let total_size = elem_size * elements.len() as u64;
                    let storage = self.entry_array_alloca(self.i8_ty(), total_size, "arr_lit");
                    for (i, val) in elem_values.iter().enumerate() {
                        let off = self.i64_ty().const_int(i as u64 * elem_size, false);
                        let ptr = self.ptr_add_i64(storage, off);
                        self.builder().build_store(ptr, *val).unwrap();
                    }
                    storage.into()
                } else {
                    panic!("array literal: expected array type");
                }
            }
            Expr::Block(exprs) => {
                let exprs = exprs.clone();
                if exprs.is_empty() {
                    self.zero_i32()
                } else {
                    // Save variable scope — declarations inside this block
                    // shadow outer names only for the duration of the block.
                    let saved_vars = self.variables.clone();
                    let saved_lets = self.let_bindings.clone();
                    let mut result = self.zero_i32();
                    for e in &exprs {
                        if self.is_block_terminated() {
                            break;
                        }
                        result = self.translate_expr(*e, decl);
                    }
                    self.variables = saved_vars;
                    self.let_bindings = saved_lets;
                    result
                }
            }
            Expr::If(cond_id, then_id, else_id) => {
                let (cond_id, then_id, else_id) = (*cond_id, *then_id, *else_id);

                // Branchless select: `if cond { var = expr }` with no else.
                if else_id.is_none() {
                    if let Some((var_name, new_val_id)) =
                        Self::match_single_var_assign(&decl.arena, then_id)
                    {
                        let var_ty = decl.types[new_val_id];
                        let is_scalar_float =
                            matches!(*var_ty, crate::Type::Float32 | crate::Type::Float64);
                        if is_scalar_float && self.variables.contains_key(&var_name.to_string()) {
                            let cond_raw = self.translate_expr(cond_id, decl).into_int_value();
                            let cond_val = self.to_i1(cond_raw);
                            let alloca = self.variables[&var_name.to_string()];
                            let is_let = self.let_bindings.contains(&var_name.to_string());
                            // Get the storage pointer.
                            let slot_ptr = if is_let {
                                alloca
                            } else {
                                self.builder()
                                    .build_load(self.ptr_ty(), alloca, "slot")
                                    .unwrap()
                                    .into_pointer_value()
                            };
                            let old_val = self
                                .builder()
                                .build_load(var_ty.llvm_basic_type(self.ctx()), slot_ptr, "old")
                                .unwrap();
                            let new_val = self.translate_expr(new_val_id, decl);
                            let selected = self
                                .builder()
                                .build_select(cond_val, new_val, old_val, "sel")
                                .unwrap();
                            self.builder().build_store(slot_ptr, selected).unwrap();
                            return self.zero_i32();
                        }
                    }
                }

                // Determine if this if-else produces a value (both branches
                // have the same concrete, non-void type).
                let result_ty = decl.types[expr];
                let is_value = if let Some(else_expr_id) = else_id {
                    let else_ty = decl.types[else_expr_id];
                    !matches!(
                        &*result_ty,
                        crate::Type::Void | crate::Type::Anon(_) | crate::Type::Var(_)
                    ) && result_ty == else_ty
                        && !result_ty.is_ptr()
                } else {
                    false
                };

                let cond_raw = self.translate_expr(cond_id, decl).into_int_value();
                let cond_val = self.to_i1(cond_raw);
                let then_bb = self.append_bb("then");
                let else_bb = self.append_bb("else");
                let merge_bb = self.append_bb("merge");

                self.builder()
                    .build_conditional_branch(cond_val, then_bb, else_bb)
                    .unwrap();

                // Then block.
                self.builder().position_at_end(then_bb);
                let then_val = self.translate_expr(then_id, decl);
                let then_exit_bb = self.builder().get_insert_block().unwrap();
                if !self.is_block_terminated() {
                    self.builder().build_unconditional_branch(merge_bb).unwrap();
                }

                // Else block.
                self.builder().position_at_end(else_bb);
                let else_val = if let Some(else_id) = else_id {
                    self.translate_expr(else_id, decl)
                } else {
                    self.zero_i32()
                };
                let else_exit_bb = self.builder().get_insert_block().unwrap();
                if !self.is_block_terminated() {
                    self.builder().build_unconditional_branch(merge_bb).unwrap();
                }

                self.builder().position_at_end(merge_bb);
                if is_value {
                    let llvm_ty = result_ty.llvm_basic_type(self.ctx());
                    let phi = self.builder().build_phi(llvm_ty, "if_val").unwrap();
                    phi.add_incoming(&[(&then_val, then_exit_bb), (&else_val, else_exit_bb)]);
                    phi.as_basic_value().into()
                } else {
                    self.zero_i32()
                }
            }
            Expr::While(cond_id, body_id) => {
                let (cond_id, body_id) = (*cond_id, *body_id);
                let header_bb = self.append_bb("while_header");
                let body_bb = self.append_bb("while_body");
                let exit_bb = self.append_bb("while_exit");

                // continue → header (re-check condition), break → exit.
                self.loop_stack.push((header_bb, exit_bb));

                self.builder()
                    .build_unconditional_branch(header_bb)
                    .unwrap();

                self.builder().position_at_end(header_bb);
                let cond_raw = self.translate_expr(cond_id, decl).into_int_value();
                let cond_val = self.to_i1(cond_raw);
                self.builder()
                    .build_conditional_branch(cond_val, body_bb, exit_bb)
                    .unwrap();

                self.builder().position_at_end(body_bb);
                self.translate_expr(body_id, decl);
                if !self.is_block_terminated() {
                    self.emit_cancel_check();
                    self.builder()
                        .build_unconditional_branch(header_bb)
                        .unwrap();
                }

                self.loop_stack.pop();
                self.builder().position_at_end(exit_bb);
                self.zero_i32()
            }
            Expr::For {
                var,
                start,
                end,
                body,
            } => {
                let (var, start, end, body) = (*var, *start, *end, *body);
                let start_val = self.translate_expr(start, decl).into_int_value();
                let end_val = self.translate_expr(end, decl).into_int_value();

                // Allocate loop counter in entry block.
                let loop_alloca = self.entry_alloca(self.i32_ty().into(), &*var);
                self.builder().build_store(loop_alloca, start_val).unwrap();
                // Treat as let binding (holds value directly).
                self.variables.insert(var.to_string(), loop_alloca);
                self.let_bindings.insert(var.to_string());

                let header_bb = self.append_bb("for_header");
                let body_bb = self.append_bb("for_body");
                let latch_bb = self.append_bb("for_latch");
                let exit_bb = self.append_bb("for_exit");

                // continue → latch (increment then re-check), break → exit.
                self.loop_stack.push((latch_bb, exit_bb));

                self.builder()
                    .build_unconditional_branch(header_bb)
                    .unwrap();

                self.builder().position_at_end(header_bb);
                let cur = self
                    .builder()
                    .build_load(self.i32_ty(), loop_alloca, "loop_i")
                    .unwrap()
                    .into_int_value();
                let cond = self
                    .builder()
                    .build_int_compare(IntPredicate::SLT, cur, end_val, "loop_cond")
                    .unwrap();
                self.builder()
                    .build_conditional_branch(cond, body_bb, exit_bb)
                    .unwrap();

                self.builder().position_at_end(body_bb);
                self.translate_expr(body, decl);

                if !self.is_block_terminated() {
                    self.builder().build_unconditional_branch(latch_bb).unwrap();
                }

                // Latch block: increment and jump back to header.
                self.builder().position_at_end(latch_bb);
                let cur2 = self
                    .builder()
                    .build_load(self.i32_ty(), loop_alloca, "loop_i2")
                    .unwrap()
                    .into_int_value();
                let one = self.i32_ty().const_int(1, false);
                let next = self
                    .builder()
                    .build_int_add(cur2, one, "loop_next")
                    .unwrap();
                self.builder().build_store(loop_alloca, next).unwrap();
                self.emit_cancel_check();
                self.builder()
                    .build_unconditional_branch(header_bb)
                    .unwrap();

                self.loop_stack.pop();
                self.builder().position_at_end(exit_bb);
                self.zero_i32()
            }
            Expr::Assume(_) => {
                // No-op: assume is only used by the safety checker.
                self.zero_i32()
            }
            Expr::Return(ret_id) => {
                let ret_id = *ret_id;
                let result = self.translate_expr(ret_id, decl);
                let ret_ty = decl.types[ret_id];

                if returns_via_pointer(ret_ty) {
                    let out = self.output_ptr.unwrap();
                    let size = ret_ty.size(self.decls) as u64;
                    self.emit_memcpy(out, result.into_pointer_value(), size);
                    self.builder().build_return(None).unwrap();
                } else if *ret_ty == crate::Type::Void {
                    self.builder().build_return(None).unwrap();
                } else {
                    let coerced = self.coerce_return(result, decl.ret);
                    self.builder().build_return(Some(&coerced)).unwrap();
                }

                // Unreachable block after return — add terminator so epilogue is skipped.
                let dead_bb = self.append_bb("dead");
                self.builder().position_at_end(dead_bb);
                self.builder().build_unreachable().unwrap();
                self.zero_i32()
            }
            Expr::Tuple(elements) => {
                let elements = elements.clone();
                let ty = decl.types[expr];
                let elem_vals: Vec<BasicValueEnum<'ctx>> = elements
                    .iter()
                    .map(|e| self.translate_expr(*e, decl))
                    .collect();
                if let crate::Type::Tuple(elem_types) = &*ty {
                    let total_size: u64 =
                        elem_types.iter().map(|t| t.size(self.decls) as u64).sum();
                    let storage = self.entry_array_alloca(self.i8_ty(), total_size, "tuple");
                    let mut off = 0u64;
                    for (i, val) in elem_vals.iter().enumerate() {
                        let ptr = self.ptr_at_offset(storage, off);
                        self.builder().build_store(ptr, *val).unwrap();
                        off += elem_types[i].size(self.decls) as u64;
                    }
                    storage.into()
                } else {
                    panic!("tuple expr: expected tuple type");
                }
            }
            Expr::Lambda { params, body } => {
                let (params, body) = (params.clone(), *body);
                self.translate_lambda(&params, body, expr, decl)
            }
            Expr::Enum(case_name) => {
                let case_name = *case_name;
                let index = if let crate::Type::Name(enum_name, _) = &*decl.types[expr] {
                    let enum_decls = self.decls.find(*enum_name);
                    if let Some(crate::Decl::Enum { cases, .. }) = enum_decls
                        .iter()
                        .find(|d| matches!(d, crate::Decl::Enum { .. }))
                    {
                        cases.iter().position(|c| *c == case_name).unwrap_or(0) as u64
                    } else {
                        0
                    }
                } else {
                    0
                };
                // Enums are pointer types — allocate i32 discriminant on stack.
                let storage = self.entry_alloca(self.i32_ty().into(), "enum_disc");
                let val = self.i32_ty().const_int(index, false);
                self.builder().build_store(storage, val).unwrap();
                storage.into()
            }
            Expr::Arena(inner_id) => {
                let inner_id = *inner_id;
                self.translate_expr(inner_id, decl)
            }
            Expr::String(s) => {
                let bytes = s.as_bytes();
                let total = bytes.len() + 1;
                let storage = self.entry_array_alloca(self.i8_ty(), total as u64, "str");
                for (i, &b) in bytes.iter().enumerate() {
                    let ptr = self.ptr_at_offset(storage, i as u64);
                    self.builder()
                        .build_store(ptr, self.i8_ty().const_int(b as u64, false))
                        .unwrap();
                }
                let null_ptr = self.ptr_at_offset(storage, bytes.len() as u64);
                self.builder()
                    .build_store(null_ptr, self.i8_ty().const_int(0, false))
                    .unwrap();
                storage.into()
            }
            Expr::AsTy(src_id, target_ty) => {
                let (src_id, target_ty) = (*src_id, *target_ty);
                let val = self.translate_expr(src_id, decl);
                let src_ty = decl.types[src_id];
                self.translate_cast(val, src_ty, target_ty)
            }
            Expr::Break => {
                let (_, break_bb) = *self.loop_stack.last().expect("break outside loop");
                self.builder().build_unconditional_branch(break_bb).unwrap();
                let unreachable_bb = self.append_bb("after_break");
                self.builder().position_at_end(unreachable_bb);
                self.zero_i32()
            }
            Expr::Continue => {
                let (continue_bb, _) = *self.loop_stack.last().expect("continue outside loop");
                self.builder()
                    .build_unconditional_branch(continue_bb)
                    .unwrap();
                let unreachable_bb = self.append_bb("after_continue");
                self.builder().position_at_end(unreachable_bb);
                self.zero_i32()
            }
            Expr::StructLit(struct_name, fields) => {
                let struct_name = *struct_name;
                let fields: Vec<(Name, ExprID)> = fields.clone();
                let ty = decl.types[expr];
                let sz = ty.size(self.decls) as u64;
                let storage = self.entry_array_alloca(self.i8_ty(), sz, "struct_lit");
                // Zero-initialize.
                let zero = self.i8_ty().const_int(0, false);
                let size_val = self.i64_ty().const_int(sz, false);
                self.builder()
                    .build_memset(storage, 1, zero, size_val)
                    .unwrap();

                if let crate::Type::Name(_, type_args) = &*ty {
                    let struct_decl = self.decls.find(struct_name);
                    if let crate::Decl::Struct(s) = &struct_decl[0] {
                        let inst: crate::Instance = s
                            .typevars
                            .iter()
                            .map(|tv| crate::types::mk_type(crate::Type::Var(*tv)))
                            .zip(type_args.iter().copied())
                            .collect();
                        for (fname, fval) in &fields {
                            let val = self.translate_expr(*fval, decl);
                            let off = s.field_offset(fname, self.decls, &inst);
                            let field_ptr = self.ptr_at_offset(storage, off as u64);
                            self.builder().build_store(field_ptr, val).unwrap();
                        }
                    }
                }
                storage.into()
            }
            Expr::Array(value_expr, _size_expr) => {
                // Fill-array expression: [value; size], e.g. [0; 5]
                let value_expr = *value_expr;
                let fill_value = self.translate_expr(value_expr, decl);
                let ty = decl.types[expr];
                if let crate::Type::Array(elem_ty, sz) = &*ty {
                    let count = sz.known();
                    let elem_size = elem_ty.size(self.decls) as u64;
                    let total_size = elem_size * count as u64;
                    let storage = self.entry_array_alloca(self.i8_ty(), total_size, "arr_fill");
                    for i in 0..count {
                        let off = self.i64_ty().const_int(i as u64 * elem_size, false);
                        let ptr = self.ptr_add_i64(storage, off);
                        self.builder().build_store(ptr, fill_value).unwrap();
                    }
                    storage.into()
                } else {
                    panic!("LLVM JIT array fill: expected array type, got {:?}", ty);
                }
            }
            _ => {
                panic!(
                    "LLVM JIT: unimplemented expression: {:?}",
                    &decl.arena[expr]
                )
            }
        }
    }

    fn translate_unop(
        &mut self,
        op: Unop,
        arg_id: ExprID,
        decl: &FuncDecl,
    ) -> BasicValueEnum<'ctx> {
        let val = self.translate_expr(arg_id, decl);
        let ty = decl.types[arg_id];
        match op {
            Unop::Neg => match *ty {
                crate::Type::Float32 | crate::Type::Float64 => self
                    .builder()
                    .build_float_neg(val.into_float_value(), "fneg")
                    .unwrap()
                    .into(),
                _ => self
                    .builder()
                    .build_int_neg(val.into_int_value(), "ineg")
                    .unwrap()
                    .into(),
            },
            Unop::Not => {
                let v = val.into_int_value();
                let not = self.builder().build_not(v, "not").unwrap();
                // Mask to 1 bit.
                let one = v.get_type().const_int(1, false);
                self.builder().build_and(not, one, "not1").unwrap().into()
            }
        }
    }

    fn translate_binop(
        &mut self,
        op: Binop,
        lhs_id: ExprID,
        rhs_id: ExprID,
        decl: &FuncDecl,
    ) -> BasicValueEnum<'ctx> {
        match op {
            Binop::Plus => {
                let t = decl.types[lhs_id];
                let is_float = matches!(*t, crate::Type::Float32 | crate::Type::Float64);

                // FMA: a*b + c
                if is_float {
                    if let Expr::Binop(Binop::Mult, ma, mb) = decl.arena.exprs[lhs_id] {
                        let a = self.translate_expr(ma, decl).into_float_value();
                        let b = self.translate_expr(mb, decl).into_float_value();
                        let c = self.translate_expr(rhs_id, decl).into_float_value();
                        return self.build_fma(a, b, c, t).into();
                    }
                    if let Expr::Binop(Binop::Mult, ma, mb) = decl.arena.exprs[rhs_id] {
                        let c = self.translate_expr(lhs_id, decl).into_float_value();
                        let a = self.translate_expr(ma, decl).into_float_value();
                        let b = self.translate_expr(mb, decl).into_float_value();
                        return self.build_fma(a, b, c, t).into();
                    }
                }

                let lhs = self.translate_expr(lhs_id, decl);
                let rhs = self.translate_expr(rhs_id, decl);
                if is_float {
                    self.builder()
                        .build_float_add(lhs.into_float_value(), rhs.into_float_value(), "fadd")
                        .unwrap()
                        .into()
                } else {
                    self.builder()
                        .build_int_add(lhs.into_int_value(), rhs.into_int_value(), "iadd")
                        .unwrap()
                        .into()
                }
            }
            Binop::Minus => {
                let t = decl.types[lhs_id];
                let is_float = matches!(*t, crate::Type::Float32 | crate::Type::Float64);

                // FMA: c - a*b => fma(-a, b, c)
                if is_float {
                    if let Expr::Binop(Binop::Mult, ma, mb) = decl.arena.exprs[rhs_id] {
                        let c = self.translate_expr(lhs_id, decl).into_float_value();
                        let a = self.translate_expr(ma, decl).into_float_value();
                        let b = self.translate_expr(mb, decl).into_float_value();
                        let neg_a = self.builder().build_float_neg(a, "neg_a").unwrap();
                        return self.build_fma(neg_a, b, c, t).into();
                    }
                }

                let lhs = self.translate_expr(lhs_id, decl);
                let rhs = self.translate_expr(rhs_id, decl);
                if is_float {
                    self.builder()
                        .build_float_sub(lhs.into_float_value(), rhs.into_float_value(), "fsub")
                        .unwrap()
                        .into()
                } else {
                    self.builder()
                        .build_int_sub(lhs.into_int_value(), rhs.into_int_value(), "isub")
                        .unwrap()
                        .into()
                }
            }
            Binop::Mult => {
                let t = decl.types[lhs_id];
                let lhs = self.translate_expr(lhs_id, decl);
                let rhs = self.translate_expr(rhs_id, decl);
                if matches!(*t, crate::Type::Float32 | crate::Type::Float64) {
                    self.builder()
                        .build_float_mul(lhs.into_float_value(), rhs.into_float_value(), "fmul")
                        .unwrap()
                        .into()
                } else {
                    self.builder()
                        .build_int_mul(lhs.into_int_value(), rhs.into_int_value(), "imul")
                        .unwrap()
                        .into()
                }
            }
            Binop::Div => {
                let t = decl.types[lhs_id];
                let lhs = self.translate_expr(lhs_id, decl);
                let rhs = self.translate_expr(rhs_id, decl);
                match *t {
                    crate::Type::Float32 | crate::Type::Float64 => self
                        .builder()
                        .build_float_div(lhs.into_float_value(), rhs.into_float_value(), "fdiv")
                        .unwrap()
                        .into(),
                    crate::Type::Int32 | crate::Type::Int8 => self
                        .builder()
                        .build_int_signed_div(lhs.into_int_value(), rhs.into_int_value(), "sdiv")
                        .unwrap()
                        .into(),
                    _ => self
                        .builder()
                        .build_int_unsigned_div(lhs.into_int_value(), rhs.into_int_value(), "udiv")
                        .unwrap()
                        .into(),
                }
            }
            Binop::Mod => {
                let t = decl.types[lhs_id];
                let lhs = self.translate_expr(lhs_id, decl);
                let rhs = self.translate_expr(rhs_id, decl);
                match *t {
                    crate::Type::Int32 | crate::Type::Int8 => self
                        .builder()
                        .build_int_signed_rem(lhs.into_int_value(), rhs.into_int_value(), "srem")
                        .unwrap()
                        .into(),
                    _ => self
                        .builder()
                        .build_int_unsigned_rem(lhs.into_int_value(), rhs.into_int_value(), "urem")
                        .unwrap()
                        .into(),
                }
            }
            Binop::Assign => {
                let lhs_addr = self.translate_lvalue(lhs_id, decl);
                let rhs_val = self.translate_expr(rhs_id, decl);
                let t = decl.types[lhs_id];
                self.gen_copy(t, lhs_addr, rhs_val);
                rhs_val
            }
            Binop::Equal => {
                let lhs = self.translate_expr(lhs_id, decl);
                let rhs = self.translate_expr(rhs_id, decl);
                let t = decl.types[lhs_id];
                self.gen_eq(t, lhs, rhs)
            }
            Binop::NotEqual => {
                let lhs = self.translate_expr(lhs_id, decl);
                let rhs = self.translate_expr(rhs_id, decl);
                let t = decl.types[lhs_id];
                let eq = self.gen_eq(t, lhs, rhs).into_int_value();
                let one = eq.get_type().const_int(1, false);
                self.builder().build_xor(eq, one, "ne").unwrap().into()
            }
            Binop::Less => {
                let t = decl.types[lhs_id];
                let lhs = self.translate_expr(lhs_id, decl);
                let rhs = self.translate_expr(rhs_id, decl);
                match *t {
                    crate::Type::Float32 | crate::Type::Float64 => self
                        .builder()
                        .build_float_compare(
                            FloatPredicate::OLT,
                            lhs.into_float_value(),
                            rhs.into_float_value(),
                            "flt",
                        )
                        .unwrap()
                        .into(),
                    crate::Type::Int32 | crate::Type::Int8 => self
                        .builder()
                        .build_int_compare(
                            IntPredicate::SLT,
                            lhs.into_int_value(),
                            rhs.into_int_value(),
                            "slt",
                        )
                        .unwrap()
                        .into(),
                    _ => self
                        .builder()
                        .build_int_compare(
                            IntPredicate::ULT,
                            lhs.into_int_value(),
                            rhs.into_int_value(),
                            "ult",
                        )
                        .unwrap()
                        .into(),
                }
            }
            Binop::Greater => {
                let t = decl.types[lhs_id];
                let lhs = self.translate_expr(lhs_id, decl);
                let rhs = self.translate_expr(rhs_id, decl);
                match *t {
                    crate::Type::Float32 | crate::Type::Float64 => self
                        .builder()
                        .build_float_compare(
                            FloatPredicate::OGT,
                            lhs.into_float_value(),
                            rhs.into_float_value(),
                            "fgt",
                        )
                        .unwrap()
                        .into(),
                    crate::Type::Int32 | crate::Type::Int8 => self
                        .builder()
                        .build_int_compare(
                            IntPredicate::SGT,
                            lhs.into_int_value(),
                            rhs.into_int_value(),
                            "sgt",
                        )
                        .unwrap()
                        .into(),
                    _ => self
                        .builder()
                        .build_int_compare(
                            IntPredicate::UGT,
                            lhs.into_int_value(),
                            rhs.into_int_value(),
                            "ugt",
                        )
                        .unwrap()
                        .into(),
                }
            }
            Binop::Leq => {
                let t = decl.types[lhs_id];
                let lhs = self.translate_expr(lhs_id, decl);
                let rhs = self.translate_expr(rhs_id, decl);
                match *t {
                    crate::Type::Float32 | crate::Type::Float64 => self
                        .builder()
                        .build_float_compare(
                            FloatPredicate::OLE,
                            lhs.into_float_value(),
                            rhs.into_float_value(),
                            "fle",
                        )
                        .unwrap()
                        .into(),
                    crate::Type::Int32 | crate::Type::Int8 => self
                        .builder()
                        .build_int_compare(
                            IntPredicate::SLE,
                            lhs.into_int_value(),
                            rhs.into_int_value(),
                            "sle",
                        )
                        .unwrap()
                        .into(),
                    _ => self
                        .builder()
                        .build_int_compare(
                            IntPredicate::ULE,
                            lhs.into_int_value(),
                            rhs.into_int_value(),
                            "ule",
                        )
                        .unwrap()
                        .into(),
                }
            }
            Binop::Geq => {
                let t = decl.types[lhs_id];
                let lhs = self.translate_expr(lhs_id, decl);
                let rhs = self.translate_expr(rhs_id, decl);
                match *t {
                    crate::Type::Float32 | crate::Type::Float64 => self
                        .builder()
                        .build_float_compare(
                            FloatPredicate::OGE,
                            lhs.into_float_value(),
                            rhs.into_float_value(),
                            "fge",
                        )
                        .unwrap()
                        .into(),
                    crate::Type::Int32 | crate::Type::Int8 => self
                        .builder()
                        .build_int_compare(
                            IntPredicate::SGE,
                            lhs.into_int_value(),
                            rhs.into_int_value(),
                            "sge",
                        )
                        .unwrap()
                        .into(),
                    _ => self
                        .builder()
                        .build_int_compare(
                            IntPredicate::UGE,
                            lhs.into_int_value(),
                            rhs.into_int_value(),
                            "uge",
                        )
                        .unwrap()
                        .into(),
                }
            }
            Binop::And => {
                let lhs_raw = self.translate_expr(lhs_id, decl).into_int_value();
                let lhs = self.to_i1(lhs_raw);
                let rhs_raw = self.translate_expr(rhs_id, decl).into_int_value();
                let rhs = self.to_i1(rhs_raw);
                let result = self.builder().build_and(lhs, rhs, "and").unwrap();
                self.builder()
                    .build_int_z_extend(result, self.ctx().i8_type(), "and_zext")
                    .unwrap()
                    .into()
            }
            Binop::Or => {
                let lhs_raw = self.translate_expr(lhs_id, decl).into_int_value();
                let lhs = self.to_i1(lhs_raw);
                let rhs_raw = self.translate_expr(rhs_id, decl).into_int_value();
                let rhs = self.to_i1(rhs_raw);
                let result = self.builder().build_or(lhs, rhs, "or").unwrap();
                self.builder()
                    .build_int_z_extend(result, self.ctx().i8_type(), "or_zext")
                    .unwrap()
                    .into()
            }
            Binop::Pow => {
                // pow is handled as a call — should have been rewritten.
                panic!("LLVM JIT: Binop::Pow should be rewritten to a function call")
            }
        }
    }

    fn build_fma(
        &mut self,
        a: inkwell::values::FloatValue<'ctx>,
        b: inkwell::values::FloatValue<'ctx>,
        c: inkwell::values::FloatValue<'ctx>,
        ty: crate::TypeID,
    ) -> inkwell::values::FloatValue<'ctx> {
        let fma_name = if matches!(*ty, crate::Type::Float32) {
            "llvm.fma.f32"
        } else {
            "llvm.fma.f64"
        };
        let fma_intrinsic =
            inkwell::intrinsics::Intrinsic::find(fma_name).expect("llvm.fma intrinsic not found");
        let ft = if matches!(*ty, crate::Type::Float32) {
            self.state.f32_ty().into()
        } else {
            self.state.f64_ty().into()
        };
        let fma_fn = fma_intrinsic
            .get_declaration(self.module(), &[ft])
            .expect("fma declaration failed");
        self.builder()
            .build_call(fma_fn, &[a.into(), b.into(), c.into()], "fma")
            .unwrap()
            .try_as_basic_value()
            .unwrap_basic()
            .into_float_value()
    }

    fn translate_cast(
        &mut self,
        val: BasicValueEnum<'ctx>,
        src_ty: crate::TypeID,
        dst_ty: crate::TypeID,
    ) -> BasicValueEnum<'ctx> {
        match (&*src_ty, &*dst_ty) {
            (crate::Type::Int32, crate::Type::Float32) => self
                .builder()
                .build_signed_int_to_float(val.into_int_value(), self.state.f32_ty(), "i2f32")
                .unwrap()
                .into(),
            (crate::Type::UInt32, crate::Type::Float32) => self
                .builder()
                .build_unsigned_int_to_float(val.into_int_value(), self.state.f32_ty(), "u2f32")
                .unwrap()
                .into(),
            (crate::Type::Float32, crate::Type::Int32) => self
                .builder()
                .build_float_to_signed_int(val.into_float_value(), self.i32_ty(), "f2i32")
                .unwrap()
                .into(),
            (crate::Type::Float32, crate::Type::UInt32) => self
                .builder()
                .build_float_to_unsigned_int(val.into_float_value(), self.i32_ty(), "f2u32")
                .unwrap()
                .into(),
            (crate::Type::Int32, crate::Type::Float64) => self
                .builder()
                .build_signed_int_to_float(val.into_int_value(), self.state.f64_ty(), "i2f64")
                .unwrap()
                .into(),
            (crate::Type::UInt32, crate::Type::Float64) => self
                .builder()
                .build_unsigned_int_to_float(val.into_int_value(), self.state.f64_ty(), "u2f64")
                .unwrap()
                .into(),
            (crate::Type::Float64, crate::Type::Int32) => self
                .builder()
                .build_float_to_signed_int(val.into_float_value(), self.i32_ty(), "f642i")
                .unwrap()
                .into(),
            (crate::Type::Float64, crate::Type::UInt32) => self
                .builder()
                .build_float_to_unsigned_int(val.into_float_value(), self.i32_ty(), "f642u")
                .unwrap()
                .into(),
            (crate::Type::Float32, crate::Type::Float64) => self
                .builder()
                .build_float_cast(val.into_float_value(), self.state.f64_ty(), "fpromote")
                .unwrap()
                .into(),
            (crate::Type::Float64, crate::Type::Float32) => self
                .builder()
                .build_float_cast(val.into_float_value(), self.state.f32_ty(), "fdemote")
                .unwrap()
                .into(),
            (crate::Type::Int8, crate::Type::Int32) => self
                .builder()
                .build_int_s_extend(val.into_int_value(), self.i32_ty(), "sext")
                .unwrap()
                .into(),
            (crate::Type::Int32, crate::Type::Int8) => self
                .builder()
                .build_int_truncate(val.into_int_value(), self.i8_ty(), "trunc")
                .unwrap()
                .into(),
            _ => val,
        }
    }

    // ── function calls ─────────────────────────────────────────────────────────

    fn translate_call(
        &mut self,
        fn_id: ExprID,
        arg_ids: &[ExprID],
        _call_expr_id: ExprID,
        decl: &FuncDecl,
    ) -> BasicValueEnum<'ctx> {
        let is_builtin = if let Expr::Id(name) = &decl.arena[fn_id] {
            is_builtin_name(name)
        } else {
            false
        };

        let fn_type = decl.types[fn_id];
        if let crate::Type::Func(from, to) = *fn_type {
            let param_types: Vec<crate::TypeID> = if let Expr::Id(callee_name) = &decl.arena[fn_id]
            {
                let callee_decls = self.decls.find(*callee_name);
                if let Some(crate::Decl::Func(f)) = callee_decls.first() {
                    f.param_types()
                } else if let crate::Type::Tuple(pts) = &*from {
                    pts.clone()
                } else {
                    vec![]
                }
            } else if let crate::Type::Tuple(pts) = &*from {
                pts.clone()
            } else {
                vec![]
            };

            // Allocate output slot if returning via pointer.
            let output_slot: Option<PointerValue<'ctx>> = if returns_via_pointer(to) {
                let sz = to.size(self.decls) as u64;
                let slot = self.entry_array_alloca(self.i8_ty(), sz, "ret_slot");
                Some(slot)
            } else {
                None
            };

            // Check for math builtin by name.
            let math_intrinsic = if let Expr::Id(name) = &decl.arena[fn_id] {
                self.llvm_intrinsic_name(name)
            } else {
                None
            };
            let math_sym = if math_intrinsic.is_none() {
                if let Expr::Id(name) = &decl.arena[fn_id] {
                    self.math_builtin_name(name, from)
                } else {
                    None
                }
            } else {
                None
            };

            let result_val: BasicValueEnum<'ctx> = if let Some((intrinsic_name, float_ty)) =
                math_intrinsic
            {
                // Use LLVM intrinsic directly.
                let mut intrinsic_args: Vec<BasicValueEnum<'ctx>> = vec![];
                for arg_id in arg_ids {
                    intrinsic_args.push(self.translate_expr(*arg_id, decl));
                }
                self.emit_llvm_intrinsic(&intrinsic_name, float_ty, &intrinsic_args)
            } else if let Some((sym, fn_ty, addr)) = math_sym {
                let callee = self.state.get_or_declare_extern(&sym, fn_ty, addr);
                let mut args: Vec<BasicMetadataValueEnum<'ctx>> = vec![];
                if let Some(slot) = output_slot {
                    args.push(slot.into());
                }
                for arg_id in arg_ids {
                    args.push(self.translate_expr(*arg_id, decl).into());
                }
                let call = self
                    .builder()
                    .build_call(callee, &args, "math_call")
                    .unwrap();
                if let Some(slot) = output_slot {
                    slot.into()
                } else {
                    call.try_as_basic_value()
                        .basic()
                        .unwrap_or_else(|| self.zero_i32())
                }
            } else if {
                // Check for extern function.
                if let Expr::Id(callee_name) = &decl.arena[fn_id] {
                    let callee_decls = self.decls.find(*callee_name);
                    callee_decls.first().map_or(false, |d| matches!(d, crate::Decl::Func(f) if f.is_extern))
                } else { false }
            } {
                // Extern function: load {fn_ptr, context} from globals buffer.
                let callee_name = if let Expr::Id(n) = &decl.arena[fn_id] { *n } else { unreachable!() };
                let globals_offset = *self.state.globals.get(&callee_name).expect("extern fn not in globals");
                let fn_ptr_addr = self.ptr_at_offset(self.globals_base, globals_offset as i64);
                let fn_ptr = self.builder().build_load(self.ptr_ty(), fn_ptr_addr, "extern_fn_ptr").unwrap().into_pointer_value();
                let ctx_addr = self.ptr_at_offset(self.globals_base, globals_offset as i64 + 8);
                let context = self.builder().build_load(self.ptr_ty(), ctx_addr, "extern_ctx").unwrap().into_pointer_value();

                // Build function type: (context: ptr, params...) -> ret
                let raw_fn_ty = self.build_raw_fn_type(from, to);
                let mut param_tys: Vec<BasicMetadataTypeEnum<'ctx>> = vec![self.ptr_ty().into()];
                param_tys.extend(raw_fn_ty.get_param_types().iter().map(|t| BasicMetadataTypeEnum::from(*t)));
                let extern_fn_ty = if *to == crate::Type::Void || returns_via_pointer(to) {
                    self.ctx().void_type().fn_type(&param_tys, false)
                } else {
                    to.llvm_basic_type(self.ctx()).fn_type(&param_tys, false)
                };

                let mut args: Vec<BasicMetadataValueEnum<'ctx>> = vec![context.into()];
                if let Some(slot) = output_slot {
                    args.push(slot.into());
                }
                for arg_id in arg_ids {
                    args.push(self.translate_expr(*arg_id, decl).into());
                }
                let call = self.builder().build_indirect_call(extern_fn_ty, fn_ptr, &args, "extern_call").unwrap();
                if let Some(slot) = output_slot {
                    slot.into()
                } else {
                    call.try_as_basic_value().basic().unwrap_or_else(|| self.zero_i32())
                }
            } else if is_builtin {
                // assert / print / putc — load raw fn ptr from fat pointer and indirect call.
                let fat_ptr = self.translate_expr(fn_id, decl).into_pointer_value();
                let fn_ptr_val = self
                    .builder()
                    .build_load(self.ptr_ty(), fat_ptr, "builtin_fn_ptr")
                    .unwrap()
                    .into_pointer_value();
                // Build the LLVM function type without globals/closure.
                let raw_fn_ty = self.build_raw_fn_type(from, to);
                let param_types = raw_fn_ty.get_param_types();
                let mut args: Vec<BasicMetadataValueEnum<'ctx>> = vec![];
                let mut pi = 0;
                if let Some(slot) = output_slot {
                    args.push(slot.into());
                    pi += 1;
                }
                for arg_id in arg_ids {
                    let val = self.translate_expr(*arg_id, decl);
                    let val = if pi < param_types.len() {
                        if let Ok(basic_ty) = BasicTypeEnum::try_from(param_types[pi]) {
                            self.coerce_to_type(val, basic_ty)
                        } else {
                            val
                        }
                    } else {
                        val
                    };
                    args.push(val.into());
                    pi += 1;
                }
                let call = self
                    .builder()
                    .build_indirect_call(raw_fn_ty, fn_ptr_val, &args, "builtin_call")
                    .unwrap();
                if let Some(slot) = output_slot {
                    slot.into()
                } else {
                    call.try_as_basic_value()
                        .basic()
                        .unwrap_or_else(|| self.zero_i32())
                }
            } else {
                // User function: fat pointer {fn_ptr, closure_ptr}.
                let fat_ptr = self.translate_expr(fn_id, decl).into_pointer_value();
                // Load fn_ptr (offset 0) and closure_ptr (offset 8).
                let fn_ptr = self
                    .builder()
                    .build_load(self.ptr_ty(), fat_ptr, "fn_ptr")
                    .unwrap()
                    .into_pointer_value();
                let clos_ptr_addr = self.ptr_at_offset(fat_ptr, 8);
                let clos_ptr = self
                    .builder()
                    .build_load(self.ptr_ty(), clos_ptr_addr, "clos_ptr")
                    .unwrap()
                    .into_pointer_value();

                // Build the full function type with globals+closure prefix.
                let full_fn_ty = build_fn_type(self.ctx(), from, to, true);
                let mut args: Vec<BasicMetadataValueEnum<'ctx>> =
                    vec![self.globals_base.into(), clos_ptr.into()];
                if let Some(slot) = output_slot {
                    args.push(slot.into());
                }
                let full_param_types = full_fn_ty.get_param_types();
                let arg_start = args.len(); // offset for globals+closure+output_ptr
                for (i, arg_id) in arg_ids.iter().enumerate() {
                    let arg_val = self.translate_expr(*arg_id, decl);
                    if i < param_types.len() && is_slice(param_types[i]) {
                        let wrapped =
                            self.wrap_as_slice(arg_val.into_pointer_value(), decl.types[*arg_id]);
                        args.push(wrapped.into());
                    } else {
                        let pi = arg_start + i;
                        let arg_val = if pi < full_param_types.len() {
                            if let Ok(basic_ty) = BasicTypeEnum::try_from(full_param_types[pi]) {
                                self.coerce_to_type(arg_val, basic_ty)
                            } else {
                                arg_val
                            }
                        } else {
                            arg_val
                        };
                        args.push(arg_val.into());
                    }
                }
                let call = self
                    .builder()
                    .build_indirect_call(full_fn_ty, fn_ptr, &args, "user_call")
                    .unwrap();
                if let Some(slot) = output_slot {
                    slot.into()
                } else {
                    call.try_as_basic_value()
                        .basic()
                        .unwrap_or_else(|| self.zero_i32())
                }
            };

            result_val
        } else {
            panic!("LLVM JIT call: expected function type, got {:?}", fn_type)
        }
    }

    /// Returns (intrinsic_name, float_type) for math builtins that have LLVM intrinsics.
    fn llvm_intrinsic_name(&self, name: &Name) -> Option<(String, BasicTypeEnum<'ctx>)> {
        let (base, ty): (&str, BasicTypeEnum<'ctx>) = if name.contains("$f32") {
            let stripped = name.strip_suffix("$f32").unwrap_or(&**name);
            (stripped, self.state.f32_ty().into())
        } else if name.contains("$f64") {
            let stripped = name.strip_suffix("$f64").unwrap_or(&**name);
            (stripped, self.state.f64_ty().into())
        } else if name.contains("$f32$f32") {
            let stripped = name.strip_suffix("$f32$f32").unwrap_or(&**name);
            (stripped, self.state.f32_ty().into())
        } else if name.contains("$f64$f64") {
            let stripped = name.strip_suffix("$f64$f64").unwrap_or(&**name);
            (stripped, self.state.f64_ty().into())
        } else {
            return None;
        };
        let intrinsic = match base {
            "sin" => "llvm.sin",
            "cos" => "llvm.cos",
            "exp" => "llvm.exp",
            "exp2" => "llvm.exp2",
            "ln" => "llvm.log",
            "log10" => "llvm.log10",
            "log2" => "llvm.log2",
            "sqrt" => "llvm.sqrt",
            "abs" => "llvm.fabs",
            "floor" => "llvm.floor",
            "ceil" => "llvm.ceil",
            "pow" => "llvm.pow",
            "min" => "llvm.minnum",
            "max" => "llvm.maxnum",
            _ => return None,
        };
        let suffix = if ty == self.state.f32_ty().into() {
            "f32"
        } else {
            "f64"
        };
        Some((format!("{}.{}", intrinsic, suffix), ty))
    }

    /// Emit an LLVM intrinsic call.
    fn emit_llvm_intrinsic(
        &mut self,
        intrinsic_name: &str,
        float_ty: BasicTypeEnum<'ctx>,
        args: &[BasicValueEnum<'ctx>],
    ) -> BasicValueEnum<'ctx> {
        let param_types: Vec<BasicMetadataTypeEnum<'ctx>> =
            args.iter().map(|_| float_ty.into()).collect();
        let fn_ty = float_ty.fn_type(&param_types, false);
        let func = if let Some(f) = self.state.module.get_function(intrinsic_name) {
            f
        } else {
            self.state.module.add_function(intrinsic_name, fn_ty, None)
        };
        let call_args: Vec<BasicMetadataValueEnum<'ctx>> =
            args.iter().map(|a| (*a).into()).collect();
        self.builder()
            .build_call(func, &call_args, "intrinsic")
            .unwrap()
            .try_as_basic_value()
            .unwrap_basic()
    }

    /// Returns (symbol_name, fn_type) for a math builtin, or None.
    fn math_builtin_name(
        &self,
        name: &Name,
        from: crate::TypeID,
    ) -> Option<(String, FunctionType<'ctx>, usize)> {
        let addr = math_builtin_ptr(name)?;
        let sym = format!("__llvm_lyte_{}", self.math_sym_suffix(name));
        // Build the LLVM fn type for this math builtin.
        let fn_ty = self.build_raw_fn_type(from, /* to: same param type */ {
            // Determine return type from the function name.
            if name.contains("isnan") || name.contains("isinf") {
                crate::types::mk_type(crate::Type::Int32)
            } else if name.contains("$f32") {
                crate::types::mk_type(crate::Type::Float32)
            } else {
                crate::types::mk_type(crate::Type::Float64)
            }
        });
        Some((sym, fn_ty, addr))
    }

    fn math_sym_suffix(&self, name: &Name) -> &'static str {
        let pairs: &[(&str, &str)] = &[
            ("sin$f32", "sinf"),
            ("cos$f32", "cosf"),
            ("tan$f32", "tanf"),
            ("asin$f32", "asinf"),
            ("acos$f32", "acosf"),
            ("atan$f32", "atanf"),
            ("sinh$f32", "sinhf"),
            ("cosh$f32", "coshf"),
            ("tanh$f32", "tanhf"),
            ("asinh$f32", "asinhf"),
            ("acosh$f32", "acoshf"),
            ("atanh$f32", "atanhf"),
            ("ln$f32", "lnf"),
            ("exp$f32", "expf"),
            ("exp2$f32", "exp2f"),
            ("log10$f32", "log10f"),
            ("log2$f32", "log2f"),
            ("sqrt$f32", "sqrtf"),
            ("abs$f32", "absf"),
            ("floor$f32", "floorf"),
            ("ceil$f32", "ceilf"),
            ("sin$f64", "sind"),
            ("cos$f64", "cosd"),
            ("tan$f64", "tand"),
            ("asin$f64", "asind"),
            ("acos$f64", "acosd"),
            ("atan$f64", "atand"),
            ("sinh$f64", "sinhd"),
            ("cosh$f64", "coshd"),
            ("tanh$f64", "tanhd"),
            ("asinh$f64", "asinhd"),
            ("acosh$f64", "acoshd"),
            ("atanh$f64", "atanhd"),
            ("ln$f64", "lnd"),
            ("exp$f64", "expd"),
            ("exp2$f64", "exp2d"),
            ("log10$f64", "log10d"),
            ("log2$f64", "log2d"),
            ("sqrt$f64", "sqrtd"),
            ("abs$f64", "absd"),
            ("floor$f64", "floord"),
            ("ceil$f64", "ceild"),
            ("isinf$f32", "isinff"),
            ("isinf$f64", "isinfd"),
            ("isnan$f32", "isnanf"),
            ("isnan$f64", "isnand"),
            ("pow$f32$f32", "powf"),
            ("atan2$f32$f32", "atan2f"),
            ("min$f32$f32", "minf"),
            ("max$f32$f32", "maxf"),
            ("pow$f64$f64", "powd"),
            ("atan2$f64$f64", "atan2d"),
            ("min$f64$f64", "mind"),
            ("max$f64$f64", "maxd"),
        ];
        for &(n, s) in pairs {
            if **name == *n {
                return s;
            }
        }
        panic!("unknown math builtin: {}", name)
    }

    /// Build a raw fn type (no globals/closure prefix).
    fn build_raw_fn_type(&self, from: crate::TypeID, to: crate::TypeID) -> FunctionType<'ctx> {
        let mut params: Vec<BasicMetadataTypeEnum<'ctx>> = vec![];
        if returns_via_pointer(to) {
            params.push(self.ptr_ty().into());
        }
        if let crate::Type::Tuple(args) = &*from {
            for t in args {
                params.push(t.llvm_basic_type(self.ctx()).into());
            }
        }
        if *to == crate::Type::Void || returns_via_pointer(to) {
            self.ctx().void_type().fn_type(&params, false)
        } else {
            to.llvm_basic_type(self.ctx()).fn_type(&params, false)
        }
    }

    /// Translate a function reference (not a call), returning a fat pointer.
    fn translate_func_ref(&mut self, name: &Name, ty: &crate::Type) -> BasicValueEnum<'ctx> {
        // Builtin raw function pointers.
        if *name == Name::str("assert") {
            let fn_ty = self
                .ctx()
                .void_type()
                .fn_type(&[self.i8_ty().into()], false);
            let f = self.state.get_or_declare_extern(
                "__llvm_lyte_assert",
                fn_ty,
                llvm_lyte_assert as *const () as usize,
            );
            let ptr = f.as_global_value().as_pointer_value();
            return self.make_fat_ptr(ptr, None);
        }
        if *name == Name::str("print") {
            let fn_ty = self
                .ctx()
                .void_type()
                .fn_type(&[self.i32_ty().into()], false);
            let f = self.state.get_or_declare_extern(
                "__llvm_lyte_print",
                fn_ty,
                llvm_lyte_print_i32 as *const () as usize,
            );
            let ptr = f.as_global_value().as_pointer_value();
            return self.make_fat_ptr(ptr, None);
        }
        if *name == Name::str("putc") {
            let fn_ty = self
                .ctx()
                .void_type()
                .fn_type(&[self.i32_ty().into()], false);
            let f = self.state.get_or_declare_extern(
                "__llvm_lyte_putc",
                fn_ty,
                llvm_lyte_putc as *const () as usize,
            );
            let ptr = f.as_global_value().as_pointer_value();
            return self.make_fat_ptr(ptr, None);
        }
        if let Some(addr) = math_builtin_ptr(name) {
            let sym = format!("__llvm_lyte_{}", self.math_sym_suffix(name));
            let fn_ty = self.ctx().void_type().fn_type(&[], false);
            let f = self.state.get_or_declare_extern(&sym, fn_ty, addr);
            let ptr = f.as_global_value().as_pointer_value();
            return self.make_fat_ptr(ptr, None);
        }

        // User function reference.
        if let crate::Type::Func(dom, rng) = ty {
            let full_fn_ty = build_fn_type(self.ctx(), *dom, *rng, true);
            let f = if let Some(existing) = self.module().get_function(&**name) {
                existing
            } else {
                self.module()
                    .add_function(&**name, full_fn_ty, Some(Linkage::External))
            };
            self.called_functions.insert(*name);
            let ptr = f.as_global_value().as_pointer_value();
            self.make_fat_ptr(ptr, None)
        } else {
            panic!("translate_func_ref: expected function type for {}", name)
        }
    }

    /// Build a fat pointer {fn_ptr, closure_ptr} on the stack and return it.
    fn make_fat_ptr(
        &self,
        fn_ptr: PointerValue<'ctx>,
        closure: Option<PointerValue<'ctx>>,
    ) -> BasicValueEnum<'ctx> {
        let storage = self.entry_array_alloca(self.i8_ty(), 16, "fat_ptr");
        storage.as_instruction().unwrap().set_alignment(8).unwrap();
        self.builder().build_store(storage, fn_ptr).unwrap();
        let clos_slot = self.ptr_at_offset(storage, 8);
        let clos_val: PointerValue<'ctx> = closure.unwrap_or_else(|| self.ptr_ty().const_null());
        self.builder().build_store(clos_slot, clos_val).unwrap();
        storage.into()
    }

    fn wrap_as_slice(
        &self,
        val: PointerValue<'ctx>,
        actual_ty: crate::TypeID,
    ) -> PointerValue<'ctx> {
        match &*actual_ty {
            crate::Type::Slice(_) => val,
            crate::Type::Array(_, sz) => {
                let len = self.i32_ty().const_int(sz.known() as u64, false);
                let storage = self.entry_array_alloca(self.i8_ty(), 12, "slice_fat");
                storage.as_instruction().unwrap().set_alignment(8).unwrap();
                self.builder().build_store(storage, val).unwrap();
                let len_ptr = self.ptr_at_offset(storage, 8);
                self.builder().build_store(len_ptr, len).unwrap();
                storage
            }
            _ => panic!("wrap_as_slice: unexpected type"),
        }
    }

    fn translate_lambda(
        &mut self,
        params: &[Param],
        body: ExprID,
        expr_id: ExprID,
        decl: &FuncDecl,
    ) -> BasicValueEnum<'ctx> {
        let lambda_ty = decl.types[expr_id];
        if let crate::Type::Func(dom, rng) = *lambda_ty {
            if let crate::Type::Tuple(param_types) = &*dom {
                let id = self.state.lambda_counter;
                self.state.lambda_counter += 1;
                let lambda_name = Name::new(format!("__lambda_{}", id));

                let lambda_params: Vec<Param> = params
                    .iter()
                    .zip(param_types.iter())
                    .map(|(p, ty)| Param {
                        name: p.name,
                        ty: Some(*ty),
                    })
                    .collect();

                // Collect free variables.
                let param_names: HashSet<String> =
                    params.iter().map(|p| p.name.to_string()).collect();
                let free_vars = collect_free_var_names_llvm(
                    body,
                    &decl.arena,
                    &param_names,
                    &self.variables,
                    &decl.types,
                );

                // Build closure struct on stack.
                let closure_ptr_val: PointerValue<'ctx> = if !free_vars.is_empty() {
                    let sz = (free_vars.len() * 8) as u64;
                    let clos_storage = self.entry_array_alloca(self.i8_ty(), sz, "closure");
                    for (i, (name, ty)) in free_vars.iter().enumerate() {
                        let var_ptr = self.get_var_addr(name, *ty);
                        let slot = self.ptr_at_offset(clos_storage, i as u64 * 8);
                        self.builder().build_store(slot, var_ptr).unwrap();
                    }
                    clos_storage
                } else {
                    self.ptr_ty().const_null()
                };

                let closure_vars: Vec<ClosureVar> = free_vars
                    .iter()
                    .map(|(n, ty)| ClosureVar {
                        name: Name::new(n.clone()),
                        ty: *ty,
                    })
                    .collect();

                let lambda_decl = FuncDecl {
                    name: lambda_name,
                    typevars: vec![],
                    size_vars: vec![],
                    params: lambda_params,
                    body: Some(body),
                    ret: rng,
                    constraints: vec![],
                    loc: decl.loc,
                    arena: decl.arena.clone(),
                    types: decl.types.clone(),
                    closure_vars,
                    is_extern: false,
                };

                self.pending_lambdas.push(lambda_decl);

                // Declare the lambda function to get a pointer.
                let full_fn_ty = build_fn_type(self.ctx(), dom, rng, true);
                let lambda_fn = if let Some(f) = self.module().get_function(&*lambda_name) {
                    f
                } else {
                    self.module()
                        .add_function(&*lambda_name, full_fn_ty, Some(Linkage::External))
                };
                let fn_ptr = lambda_fn.as_global_value().as_pointer_value();

                self.make_fat_ptr(fn_ptr, Some(closure_ptr_val))
            } else {
                panic!("lambda: expected tuple domain")
            }
        } else {
            panic!("lambda: expected function type")
        }
    }
}

// ─── Free variable collection ───────────────────────────────────────────────────

fn collect_free_var_names_llvm(
    body: ExprID,
    arena: &ExprArena,
    exclude: &HashSet<String>,
    local_vars: &HashMap<String, PointerValue<'_>>,
    types: &[crate::TypeID],
) -> Vec<(String, crate::TypeID)> {
    let mut result = Vec::new();
    let mut seen = HashSet::new();
    collect_free_vars_rec_llvm(
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

fn collect_free_vars_rec_llvm(
    expr: ExprID,
    arena: &ExprArena,
    exclude: &HashSet<String>,
    local_vars: &HashMap<String, PointerValue<'_>>,
    types: &[crate::TypeID],
    result: &mut Vec<(String, crate::TypeID)>,
    seen: &mut HashSet<String>,
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
            collect_free_vars_rec_llvm(*fn_id, arena, exclude, local_vars, types, result, seen);
            for a in args {
                collect_free_vars_rec_llvm(*a, arena, exclude, local_vars, types, result, seen);
            }
        }
        Expr::Binop(_, lhs, rhs) => {
            collect_free_vars_rec_llvm(*lhs, arena, exclude, local_vars, types, result, seen);
            collect_free_vars_rec_llvm(*rhs, arena, exclude, local_vars, types, result, seen);
        }
        Expr::Unop(_, arg) => {
            collect_free_vars_rec_llvm(*arg, arena, exclude, local_vars, types, result, seen)
        }
        Expr::Let(_, init, _) => {
            collect_free_vars_rec_llvm(*init, arena, exclude, local_vars, types, result, seen)
        }
        Expr::Var(_, init, _) => {
            if let Some(i) = init {
                collect_free_vars_rec_llvm(*i, arena, exclude, local_vars, types, result, seen);
            }
        }
        Expr::If(c, t, e) => {
            collect_free_vars_rec_llvm(*c, arena, exclude, local_vars, types, result, seen);
            collect_free_vars_rec_llvm(*t, arena, exclude, local_vars, types, result, seen);
            if let Some(e) = e {
                collect_free_vars_rec_llvm(*e, arena, exclude, local_vars, types, result, seen);
            }
        }
        Expr::While(c, b) => {
            collect_free_vars_rec_llvm(*c, arena, exclude, local_vars, types, result, seen);
            collect_free_vars_rec_llvm(*b, arena, exclude, local_vars, types, result, seen);
        }
        Expr::For {
            start, end, body, ..
        } => {
            collect_free_vars_rec_llvm(*start, arena, exclude, local_vars, types, result, seen);
            collect_free_vars_rec_llvm(*end, arena, exclude, local_vars, types, result, seen);
            collect_free_vars_rec_llvm(*body, arena, exclude, local_vars, types, result, seen);
        }
        Expr::Block(exprs) => {
            for e in exprs {
                collect_free_vars_rec_llvm(*e, arena, exclude, local_vars, types, result, seen);
            }
        }
        Expr::Return(e) | Expr::Assume(e) => {
            collect_free_vars_rec_llvm(*e, arena, exclude, local_vars, types, result, seen)
        }
        Expr::Field(e, _) => {
            collect_free_vars_rec_llvm(*e, arena, exclude, local_vars, types, result, seen)
        }
        Expr::ArrayIndex(a, i) => {
            collect_free_vars_rec_llvm(*a, arena, exclude, local_vars, types, result, seen);
            collect_free_vars_rec_llvm(*i, arena, exclude, local_vars, types, result, seen);
        }
        Expr::ArrayLiteral(elems) => {
            for e in elems {
                collect_free_vars_rec_llvm(*e, arena, exclude, local_vars, types, result, seen);
            }
        }
        Expr::Tuple(elems) => {
            for e in elems {
                collect_free_vars_rec_llvm(*e, arena, exclude, local_vars, types, result, seen);
            }
        }
        Expr::AsTy(e, _) => {
            collect_free_vars_rec_llvm(*e, arena, exclude, local_vars, types, result, seen)
        }
        Expr::Arena(e) => {
            collect_free_vars_rec_llvm(*e, arena, exclude, local_vars, types, result, seen)
        }
        Expr::Array(t, s) => {
            collect_free_vars_rec_llvm(*t, arena, exclude, local_vars, types, result, seen);
            collect_free_vars_rec_llvm(*s, arena, exclude, local_vars, types, result, seen);
        }
        Expr::Lambda { params, body } => {
            let mut inner = exclude.clone();
            for p in params {
                inner.insert(p.name.to_string());
            }
            collect_free_vars_rec_llvm(*body, arena, &inner, local_vars, types, result, seen);
        }
        Expr::StructLit(_, fields) => {
            for (_, fval) in fields {
                collect_free_vars_rec_llvm(*fval, arena, exclude, local_vars, types, result, seen);
            }
        }
        _ => {}
    }
}
