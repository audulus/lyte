// LLVM ahead-of-time (AOT) backend.
//
// Produces a Mach-O arm64 object file (.o) and a C header alongside it, so
// host code (e.g. an iOS app written in C/C++/Swift) can link the compiled
// program directly instead of running the stack interpreter at runtime.
//
// Design notes
//
//   * Target is hard-coded to `arm64-apple-ios`. Multiple AOT objects can be
//     linked into the same binary without symbol collisions because every
//     externally-visible symbol is prefixed (default: derived from the output
//     filename stem). All internal Lyte functions are demoted to private
//     linkage during post-processing.
//
//   * IO builtins (`print`, `putc`, `assert`) become weak prefixed hooks
//     `<prefix>_print_i32`, `<prefix>_putc`, `<prefix>_assert`; the .o defines
//     no-op / abort-on-failure defaults so unset hooks still link. Math
//     builtins map directly to libm symbols (`sinf`, `cos`, `fabsf`, …).
//
//   * Requires `--no-recursion`. The call-depth machinery in the JIT path
//     depends on Rust-side trap helpers that aren't available at link time.
//     Cancel-check is also elided — hosts that want cancellation can long-jump
//     out of the trap hook.
//
//   * For each entry point, a thin C-ABI wrapper `<prefix>_<entry>(state, …)`
//     is emitted that calls the internal mangled function with the appropriate
//     globals_ptr + null closure_ptr prefix.

use crate::decl::*;
use crate::llvm_jit::{
    build_module, run_default_passes, AotConfig, LLVMJITState,
};
use crate::{DeclTable, Name};

use inkwell::context::Context;
use inkwell::module::Linkage;
use inkwell::targets::{CodeModel, FileType, InitializationConfig, RelocMode, Target};
use inkwell::types::{BasicMetadataTypeEnum, BasicType, FunctionType};
use inkwell::values::{BasicMetadataValueEnum, FunctionValue, PointerValue};
use inkwell::{AddressSpace, OptimizationLevel};

use std::collections::HashSet;
use std::path::Path;

/// Metadata about a single global, surfaced through the .h header so the host
/// can read/write the AOT program's state buffer at known offsets.
#[derive(Debug, Clone)]
pub struct AotGlobal {
    pub name: String,
    pub offset: usize,
    pub size: usize,
    /// Human-readable type string (e.g. "f32", "[f32]", "i32"). Mirrors the
    /// existing C-API's `lyte_program_get_global_type`.
    pub type_str: String,
    /// 0=scalar, 1=slice (fat ptr — data+len), 2=extern fn (fn_ptr+ctx).
    pub kind: AotGlobalKind,
}

#[repr(u32)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum AotGlobalKind {
    Scalar = 0,
    Slice = 1,
    ExternFn = 2,
}

/// Information about a single entry point, surfaced through the header.
#[derive(Debug, Clone)]
pub struct AotEntry {
    pub name: String,
    /// Parameter (name, C type string) pairs. Slice parameters become two
    /// header parameters: a `const T*` data pointer and an `int32_t` length.
    pub params: Vec<(String, String)>,
    /// C return type (or "void"). When the Lyte return type is pointer-sized,
    /// the wrapper takes an extra out-pointer parameter instead.
    pub ret: String,
    /// True if return uses an output-pointer parameter.
    pub returns_via_ptr: bool,
}

/// Compile entry points to a Mach-O arm64 object file and write a companion
/// `.h` header. Output paths are derived from `output_path` (e.g. for
/// `path/to/biquad.o` we also write `path/to/biquad.h`).
pub fn compile_aot(
    decls: &DeclTable,
    entry_points: &[Name],
    output_path: &Path,
    prefix: &str,
    print_ir: bool,
) -> Result<(), String> {
    // Set up the iOS target machine. Must initialize AArch64 specifically;
    // `Target::initialize_native` only sets up the host arch, which on x86_64
    // hosts wouldn't be enough.
    Target::initialize_aarch64(&InitializationConfig::default());

    let triple = inkwell::targets::TargetTriple::create("arm64-apple-ios16.0");
    let target =
        Target::from_triple(&triple).map_err(|e| format!("LLVM target from triple: {}", e))?;
    let machine = target
        .create_target_machine(
            &triple,
            "apple-a12",
            "",
            OptimizationLevel::Aggressive,
            RelocMode::PIC,
            CodeModel::Default,
        )
        .ok_or("failed to create AArch64 target machine")?;

    let context = Context::create();
    let (mut state, _build_elapsed) = build_module(
        &context,
        decls,
        entry_points,
        print_ir,
        // AOT requires no_recursion: the call-depth check refers to a Rust
        // trap helper that isn't available at link time.
        /* no_recursion */ true,
        Some(AotConfig {
            prefix: prefix.to_string(),
        }),
    )?;

    // Tell LLVM what we're targeting so optimization passes (and the AArch64
    // backend) can specialize correctly.
    state.module.set_triple(&triple);
    let data_layout = machine.get_target_data().get_data_layout();
    state.module.set_data_layout(&data_layout);

    // Collect entry-point info (signatures, return-by-pointer flags) BEFORE
    // we mess with linkage, since we need the original FuncDecls.
    let entries = collect_entries(decls, entry_points)?;

    // Emit wrappers + weak hooks, then demote private functions.
    let mut public_names: HashSet<String> = HashSet::new();
    emit_entry_wrappers(&mut state, &entries, prefix, &mut public_names)?;
    emit_weak_hooks(&mut state, prefix, &mut public_names);
    set_internal_linkage(&mut state, &public_names);

    // Verify after our edits so problems surface here, not after opt.
    state
        .module
        .verify()
        .map_err(|e| format!("AOT module verification failed: {}", e))?;

    if print_ir {
        let ir = state.module.print_to_string().to_string();
        println!("Pre-opt AOT IR:");
        for line in ir.lines() {
            if !line.is_empty() {
                println!("{}", line);
            }
        }
    }

    run_default_passes(&state.module, &machine)?;

    if print_ir {
        let ir = state.module.print_to_string().to_string();
        println!("Optimized AOT IR:");
        for line in ir.lines() {
            if !line.is_empty() {
                println!("{}", line);
            }
        }
    }

    machine
        .write_to_file(&state.module, FileType::Object, output_path)
        .map_err(|e| format!("write object file: {}", e))?;

    // Header lives next to the object file, with the same stem.
    let header_path = output_path.with_extension("h");
    let header = build_header(prefix, &entries, &collect_globals(decls, state.globals_size));
    std::fs::write(&header_path, header)
        .map_err(|e| format!("write header {}: {}", header_path.display(), e))?;

    Ok(())
}

fn collect_entries(decls: &DeclTable, entry_points: &[Name]) -> Result<Vec<AotEntry>, String> {
    let mut out = Vec::with_capacity(entry_points.len());
    for &ep_name in entry_points {
        let found = decls.find(ep_name);
        if found.is_empty() {
            return Err(format!("entry point '{}' not found", ep_name));
        }
        let f = match &found[0] {
            Decl::Func(d) => d,
            _ => return Err(format!("'{}' is not a function", ep_name)),
        };
        let mut params = Vec::new();
        for p in &f.params {
            let ty = p.ty.expect("param ty");
            let pname = p.name.to_string();
            if let crate::Type::Slice(inner) = &*ty {
                let elem_c = c_type_for(*inner);
                params.push((pname.clone(), format!("const {}*", elem_c)));
                params.push((format!("{}_len", pname), "int32_t".to_string()));
            } else if let crate::Type::Reference(inner) = &*ty {
                params.push((pname, format!("{}*", c_type_for(*inner))));
            } else {
                params.push((pname, c_type_for(ty)));
            }
        }
        let returns_via_ptr = !is_value_return(f.ret);
        let ret = if *f.ret == crate::Type::Void || returns_via_ptr {
            "void".to_string()
        } else {
            c_type_for(f.ret)
        };
        out.push(AotEntry {
            name: ep_name.to_string(),
            params,
            ret,
            returns_via_ptr,
        });
    }
    Ok(out)
}

fn is_value_return(ty: crate::TypeID) -> bool {
    matches!(
        &*ty,
        crate::Type::Void
            | crate::Type::Bool
            | crate::Type::Int8
            | crate::Type::UInt8
            | crate::Type::Int32
            | crate::Type::UInt32
            | crate::Type::Float32
            | crate::Type::Float64
            | crate::Type::Float32x4
    )
}

fn c_type_for(ty: crate::TypeID) -> String {
    match &*ty {
        crate::Type::Void => "void".to_string(),
        crate::Type::Bool => "int8_t".to_string(),
        crate::Type::Int8 => "int8_t".to_string(),
        crate::Type::UInt8 => "uint8_t".to_string(),
        crate::Type::Int32 => "int32_t".to_string(),
        crate::Type::UInt32 => "uint32_t".to_string(),
        crate::Type::Float32 => "float".to_string(),
        crate::Type::Float64 => "double".to_string(),
        // Anything pointer-sized in the Lyte ABI is exposed as void*.
        _ => "void*".to_string(),
    }
}

fn collect_globals(decls: &DeclTable, globals_size: usize) -> AotGlobalsLayout {
    let mut offset: i32 = crate::cancel::CANCEL_FLAG_RESERVED;
    let mut entries = Vec::new();
    for decl in &decls.decls {
        match decl {
            Decl::Global { name, ty, .. } => {
                let size = ty.size(decls);
                let kind = if matches!(&**ty, crate::Type::Slice(_)) {
                    AotGlobalKind::Slice
                } else {
                    AotGlobalKind::Scalar
                };
                entries.push(AotGlobal {
                    name: name.to_string(),
                    offset: offset as usize,
                    size: size as usize,
                    type_str: ty.pretty_print(),
                    kind,
                });
                offset += size as i32;
            }
            Decl::Func(f) if f.is_extern => {
                entries.push(AotGlobal {
                    name: f.name.to_string(),
                    offset: offset as usize,
                    size: 16,
                    type_str: format!("extern {}", f.name),
                    kind: AotGlobalKind::ExternFn,
                });
                offset += 16;
            }
            _ => {}
        }
    }
    AotGlobalsLayout {
        entries,
        state_size: globals_size,
    }
}

struct AotGlobalsLayout {
    entries: Vec<AotGlobal>,
    state_size: usize,
}

// ─── Wrapper emission ──────────────────────────────────────────────────────────

fn emit_entry_wrappers(
    state: &mut LLVMJITState<'_>,
    entries: &[AotEntry],
    prefix: &str,
    public: &mut HashSet<String>,
) -> Result<(), String> {
    for entry in entries {
        let name = emit_wrapper(state, entry, prefix)?;
        public.insert(name);
    }
    Ok(())
}

fn emit_wrapper(
    state: &mut LLVMJITState<'_>,
    entry: &AotEntry,
    prefix: &str,
) -> Result<String, String> {
    let ctx = state.context;
    let ptr_ty = ctx.ptr_type(AddressSpace::default());

    let inner = state
        .module
        .get_function(&entry.name)
        .ok_or_else(|| format!("entry point '{}' missing after build", entry.name))?;
    let inner_ty = inner.get_type();

    // Inner signature: (globals, closure, [out_ptr,] ...params).
    let mut idx = 2;
    if entry.returns_via_ptr {
        idx += 1;
    }
    let user_param_tys: Vec<_> =
        inner_ty.get_param_types().iter().skip(idx).copied().collect();

    // Wrapper signature: (state, ...user_params, [out_ptr]?).
    let mut param_tys: Vec<BasicMetadataTypeEnum<'_>> = vec![ptr_ty.into()];
    for t in &user_param_tys {
        param_tys.push((*t).into());
    }
    if entry.returns_via_ptr {
        param_tys.push(ptr_ty.into());
    }

    let wrapper_ret_ty = inner_ty.get_return_type();
    let wrapper_ty: FunctionType<'_> = match wrapper_ret_ty {
        Some(rt) => rt.fn_type(&param_tys, false),
        None => ctx.void_type().fn_type(&param_tys, false),
    };

    let wrapper_name = format!("{}_{}", prefix, entry.name);
    let wrapper = state
        .module
        .add_function(&wrapper_name, wrapper_ty, Some(Linkage::External));
    let bb = ctx.append_basic_block(wrapper, "entry");
    state.builder.position_at_end(bb);

    let state_ptr: PointerValue<'_> = wrapper.get_nth_param(0).unwrap().into_pointer_value();
    let null_closure = ptr_ty.const_null();

    let mut call_args: Vec<BasicMetadataValueEnum<'_>> =
        vec![state_ptr.into(), null_closure.into()];
    let user_param_count = user_param_tys.len();
    if entry.returns_via_ptr {
        let out_ptr = wrapper
            .get_nth_param((1 + user_param_count) as u32)
            .unwrap();
        call_args.push(out_ptr.into());
    }
    for i in 0..user_param_count {
        let v = wrapper.get_nth_param((1 + i) as u32).unwrap();
        call_args.push(v.into());
    }

    let call = state
        .builder
        .build_call(inner, &call_args, "ret")
        .map_err(|e| format!("wrapper call: {:?}", e))?;

    if wrapper_ret_ty.is_some() && !entry.returns_via_ptr {
        let val = call.try_as_basic_value().unwrap_basic();
        state.builder.build_return(Some(&val)).unwrap();
    } else {
        state.builder.build_return(None).unwrap();
    }

    // Demote the inner so it can be inlined into the wrapper and so it can't
    // collide with other AOT objects.
    inner.set_linkage(Linkage::Internal);
    Ok(wrapper_name)
}

// ─── Weak hooks ────────────────────────────────────────────────────────────────

fn emit_weak_hooks(
    state: &mut LLVMJITState<'_>,
    prefix: &str,
    public: &mut HashSet<String>,
) {
    let asserts_sym = format!("{}_assert", prefix);
    let print_sym = format!("{}_print_i32", prefix);
    let putc_sym = format!("{}_putc", prefix);
    public.insert(asserts_sym.clone());
    public.insert(print_sym.clone());
    public.insert(putc_sym.clone());

    let ctx = state.context;
    let void_ty = ctx.void_type();
    let ptr_ty = ctx.ptr_type(AddressSpace::default());
    let i8_ty = ctx.i8_type();
    let i32_ty = ctx.i32_type();

    // Find a previously-declared abort, or declare it now.
    let abort_ty = void_ty.fn_type(&[], false);
    let abort_fn = state
        .module
        .get_function("abort")
        .unwrap_or_else(|| {
            state
                .module
                .add_function("abort", abort_ty, Some(Linkage::External))
        });

    // <prefix>_assert — default aborts on cond == 0.
    {
        let fn_ty = void_ty.fn_type(&[ptr_ty.into(), i8_ty.into()], false);
        define_or_extend_weak_hook(state, &asserts_sym, fn_ty, |state, function| {
            let bb_entry = state.context.append_basic_block(function, "entry");
            let bb_trap = state.context.append_basic_block(function, "trap");
            let bb_ok = state.context.append_basic_block(function, "ok");
            state.builder.position_at_end(bb_entry);
            let cond = function.get_nth_param(1).unwrap().into_int_value();
            let zero = state.context.i8_type().const_int(0, false);
            let is_zero = state
                .builder
                .build_int_compare(inkwell::IntPredicate::EQ, cond, zero, "is_zero")
                .unwrap();
            state
                .builder
                .build_conditional_branch(is_zero, bb_trap, bb_ok)
                .unwrap();
            state.builder.position_at_end(bb_trap);
            state.builder.build_call(abort_fn, &[], "").unwrap();
            state.builder.build_unreachable().unwrap();
            state.builder.position_at_end(bb_ok);
            state.builder.build_return(None).unwrap();
        });
    }

    // <prefix>_print_i32 — default no-op.
    {
        let fn_ty = void_ty.fn_type(&[i32_ty.into()], false);
        define_or_extend_weak_hook(state, &print_sym, fn_ty, |state, function| {
            let bb = state.context.append_basic_block(function, "entry");
            state.builder.position_at_end(bb);
            state.builder.build_return(None).unwrap();
        });
    }

    // <prefix>_putc — default no-op.
    {
        let fn_ty = void_ty.fn_type(&[i32_ty.into()], false);
        define_or_extend_weak_hook(state, &putc_sym, fn_ty, |state, function| {
            let bb = state.context.append_basic_block(function, "entry");
            state.builder.position_at_end(bb);
            state.builder.build_return(None).unwrap();
        });
    }
}

/// Define a weak hook. If a same-named declaration already exists (because
/// user code referenced the hook during build_module), set its linkage to
/// WeakAny and add a body to it. Otherwise add a fresh weak function.
fn define_or_extend_weak_hook<'ctx>(
    state: &mut LLVMJITState<'ctx>,
    name: &str,
    fn_ty: FunctionType<'ctx>,
    body: impl FnOnce(&mut LLVMJITState<'ctx>, FunctionValue<'ctx>),
) {
    let function = match state.module.get_function(name) {
        Some(f) => {
            // Pre-existing extern declaration from get_or_declare_extern;
            // promote it to a weak definition.
            f.set_linkage(Linkage::WeakAny);
            f
        }
        None => state
            .module
            .add_function(name, fn_ty, Some(Linkage::WeakAny)),
    };
    let ctx = state.context;
    let noinline = ctx.create_enum_attribute(
        inkwell::attributes::Attribute::get_named_enum_kind_id("noinline"),
        0,
    );
    function.add_attribute(inkwell::attributes::AttributeLoc::Function, noinline);
    let optnone = ctx.create_enum_attribute(
        inkwell::attributes::Attribute::get_named_enum_kind_id("optnone"),
        0,
    );
    function.add_attribute(inkwell::attributes::AttributeLoc::Function, optnone);
    body(state, function);
}

fn define_weak_hook<'ctx>(
    state: &mut LLVMJITState<'ctx>,
    name: &str,
    fn_ty: FunctionType<'ctx>,
    body: impl FnOnce(&mut LLVMJITState<'ctx>, FunctionValue<'ctx>),
) {
    let function = state
        .module
        .add_function(name, fn_ty, Some(Linkage::WeakAny));
    // Prevent the optimizer from inlining the default body or removing it
    // when call sites are within the same module. Without `noinline`, an
    // O3 pass will happily inline a no-op weak default into every call
    // site and then delete the weak definition — at which point a host
    // strong override never gets a chance to win at link time.
    let ctx = state.context;
    let noinline = ctx.create_enum_attribute(
        inkwell::attributes::Attribute::get_named_enum_kind_id("noinline"),
        0,
    );
    function.add_attribute(inkwell::attributes::AttributeLoc::Function, noinline);
    let optnone = ctx.create_enum_attribute(
        inkwell::attributes::Attribute::get_named_enum_kind_id("optnone"),
        0,
    );
    function.add_attribute(inkwell::attributes::AttributeLoc::Function, optnone);
    body(state, function);
}

// ─── Linkage post-processing ───────────────────────────────────────────────────

/// Walk every function in the module and demote anything that isn't a public
/// wrapper, a weak hook, an LLVM intrinsic, or an external library symbol to
/// `Internal` linkage. This is what lets multiple AOT object files coexist in
/// a single linked binary without symbol collisions.
fn set_internal_linkage(state: &mut LLVMJITState<'_>, public_names: &HashSet<String>) {
    // Standard libm / libc symbols that the .o references and must stay
    // external so the host linker resolves them from the system libraries.
    let libm_externs: &[&str] = &[
        "sinf", "cosf", "tanf", "asinf", "acosf", "atanf", "sinhf", "coshf", "tanhf", "asinhf",
        "acoshf", "atanhf", "logf", "expf", "exp2f", "log10f", "log2f", "sqrtf", "fabsf",
        "floorf", "ceilf", "powf", "atan2f", "fminf", "fmaxf", "sin", "cos", "tan", "asin",
        "acos", "atan", "sinh", "cosh", "tanh", "asinh", "acosh", "atanh", "log", "exp", "exp2",
        "log10", "log2", "sqrt", "fabs", "floor", "ceil", "pow", "atan2", "fmin", "fmax",
        "__isnanf", "__isnand", "__isinff", "__isinfd", "memcmp", "memcpy", "memmove", "memset",
        "abort",
    ];

    let mut func = state.module.get_first_function();
    while let Some(f) = func {
        let next = f.get_next_function();
        let name = f.get_name().to_string_lossy().to_string();
        let is_intrinsic = name.starts_with("llvm.");
        let is_libm_or_libc = libm_externs.contains(&name.as_str());
        let is_public = public_names.contains(&name);
        let is_declaration = f.count_basic_blocks() == 0;
        if is_intrinsic || is_libm_or_libc || is_public || is_declaration {
            // Leave declarations external (linker resolves); leave wrappers
            // External; leave weak hooks WeakAny.
        } else {
            f.set_linkage(Linkage::Internal);
        }
        func = next;
    }
}

// ─── Header generation ────────────────────────────────────────────────────────

fn build_header(prefix: &str, entries: &[AotEntry], globals: &AotGlobalsLayout) -> String {
    let upper = prefix.to_uppercase();
    let mut s = String::new();
    use std::fmt::Write;

    let _ = writeln!(s, "// Generated by the lyte AOT backend. Do not edit.");
    let _ = writeln!(s, "//");
    let _ = writeln!(
        s,
        "// Host usage:\n//   uint8_t state[{p}_STATE_SIZE] = {{0}};\n//   {p}_<entry>(state, ...);\n//   // Optionally read trap reason from state[{p}_TRAP_REASON_OFFSET].",
        p = prefix
    );
    let _ = writeln!(s, "#ifndef {}_H", upper);
    let _ = writeln!(s, "#define {}_H", upper);
    let _ = writeln!(s);
    let _ = writeln!(s, "#include <stddef.h>");
    let _ = writeln!(s, "#include <stdint.h>");
    let _ = writeln!(s);
    let _ = writeln!(s, "#ifdef __cplusplus");
    let _ = writeln!(s, "extern \"C\" {{");
    let _ = writeln!(s, "#endif");
    let _ = writeln!(s);
    let _ = writeln!(s, "// State buffer ----------------------------------------------------------");
    let _ = writeln!(s, "#define {}_STATE_SIZE ((size_t){})", upper, globals.state_size);
    let _ = writeln!(
        s,
        "#define {}_TRAP_REASON_OFFSET ((size_t){})",
        upper,
        crate::cancel::TRAP_REASON_OFFSET
    );
    let _ = writeln!(s, "#define {}_JMPBUF_OFFSET ((size_t){})", upper, crate::cancel::JMPBUF_OFFSET);
    let _ = writeln!(s);
    let _ = writeln!(s, "// Trap reason codes -----------------------------------------------------");
    let _ = writeln!(s, "#define {}_TRAP_NONE             0u", upper);
    let _ = writeln!(s, "#define {}_TRAP_CANCELLED        1u", upper);
    let _ = writeln!(s, "#define {}_TRAP_STACK_OVERFLOW   2u", upper);
    let _ = writeln!(s, "#define {}_TRAP_ASSERTION_FAILED 3u", upper);
    let _ = writeln!(s);

    // Globals
    let _ = writeln!(s, "// Globals (compile-time layout) ----------------------------------------");
    let _ = writeln!(s, "#define {}_GLOBAL_COUNT ((size_t){})", upper, globals.entries.len());
    for g in &globals.entries {
        let safe = sanitize_macro_name(&g.name);
        let kind = match g.kind {
            AotGlobalKind::Scalar => 0,
            AotGlobalKind::Slice => 1,
            AotGlobalKind::ExternFn => 2,
        };
        let _ = writeln!(
            s,
            "#define {U}_OFFSET_{N} ((size_t){o})  // {ty}",
            U = upper,
            N = safe,
            o = g.offset,
            ty = g.type_str
        );
        let _ = writeln!(s, "#define {U}_SIZE_{N}   ((size_t){sz})", U = upper, N = safe, sz = g.size);
        let _ = writeln!(s, "#define {U}_KIND_{N}   ((int){k})", U = upper, N = safe, k = kind);
    }
    let _ = writeln!(s);

    // Runtime metadata table — defined `static const` so each TU including
    // the header gets a private copy. Tables are small (a handful of globals)
    // so the duplication overhead is negligible, and it avoids needing a
    // matching .c file or a per-program data section in the .o.
    let struct_name = aot_meta_struct_name(prefix);
    let _ = writeln!(s, "// Runtime metadata table (introspection by name) ----------------------");
    let _ = writeln!(s, "typedef struct {} {{", struct_name);
    let _ = writeln!(s, "    const char* name;");
    let _ = writeln!(s, "    size_t offset;");
    let _ = writeln!(s, "    size_t size;");
    let _ = writeln!(s, "    int kind; // 0=scalar, 1=slice (data+len fat pointer), 2=extern fn");
    let _ = writeln!(s, "    const char* type_string;");
    let _ = writeln!(s, "}} {};", struct_name);
    let _ = writeln!(s);
    if globals.entries.is_empty() {
        let _ = writeln!(
            s,
            "static const {ty} {p}_globals[1] = {{ {{0, 0, 0, 0, 0}} }}; // sentinel",
            ty = struct_name,
            p = prefix
        );
    } else {
        let _ = writeln!(
            s,
            "static const {ty} {p}_globals[{n}] = {{",
            ty = struct_name,
            p = prefix,
            n = globals.entries.len()
        );
        for g in &globals.entries {
            let kind = match g.kind {
                AotGlobalKind::Scalar => 0,
                AotGlobalKind::Slice => 1,
                AotGlobalKind::ExternFn => 2,
            };
            let _ = writeln!(
                s,
                "    {{ \"{name}\", (size_t){off}, (size_t){sz}, {k}, \"{ty}\" }},",
                name = c_string_escape(&g.name),
                off = g.offset,
                sz = g.size,
                k = kind,
                ty = c_string_escape(&g.type_str),
            );
        }
        let _ = writeln!(s, "}};");
    }
    let _ = writeln!(s);

    // Slice/extern helpers as static inlines so the host doesn't need them in
    // every .o.
    let _ = writeln!(s, "// Helpers ------------------------------------------------------------");
    let _ = writeln!(
        s,
        "static inline void {p}_bind_slice(void* state, size_t offset, const void* data, int32_t len) {{",
        p = prefix
    );
    let _ = writeln!(s, "    *(const void**)((uint8_t*)state + offset) = data;");
    let _ = writeln!(s, "    *(int32_t*)((uint8_t*)state + offset + 8) = len;");
    let _ = writeln!(s, "}}");
    let _ = writeln!(s);
    let _ = writeln!(
        s,
        "static inline void {p}_bind_extern(void* state, size_t offset, void* fn_ptr, void* ctx) {{",
        p = prefix
    );
    let _ = writeln!(s, "    *(void**)((uint8_t*)state + offset) = fn_ptr;");
    let _ = writeln!(s, "    *(void**)((uint8_t*)state + offset + 8) = ctx;");
    let _ = writeln!(s, "}}");
    let _ = writeln!(s);
    let _ = writeln!(
        s,
        "static inline uint32_t {p}_trap_reason(const void* state) {{",
        p = prefix
    );
    let _ = writeln!(
        s,
        "    return *(const uint32_t*)((const uint8_t*)state + {U}_TRAP_REASON_OFFSET);",
        U = upper
    );
    let _ = writeln!(s, "}}");
    let _ = writeln!(s);

    // Entry points
    let _ = writeln!(s, "// Entry points -------------------------------------------------------");
    for e in entries {
        let _ = write!(s, "{} {}_{}(void* state", e.ret, prefix, e.name);
        for (pname, pty) in &e.params {
            let _ = write!(s, ", {} {}", pty, pname);
        }
        if e.returns_via_ptr {
            let _ = write!(s, ", void* out");
        }
        let _ = writeln!(s, ");");
    }
    let _ = writeln!(s);

    // Hooks
    let _ = writeln!(s, "// Weak host hooks (override to customize) -----------------------------");
    let _ = writeln!(s, "// All have no-op / abort defaults provided by the compiled object.");
    let _ = writeln!(s, "void {p}_assert(void* state, int8_t cond);", p = prefix);
    let _ = writeln!(s, "void {p}_print_i32(int32_t value);", p = prefix);
    let _ = writeln!(s, "void {p}_putc(int32_t codepoint);", p = prefix);
    let _ = writeln!(s);

    let _ = writeln!(s, "#ifdef __cplusplus");
    let _ = writeln!(s, "}}");
    let _ = writeln!(s, "#endif");
    let _ = writeln!(s);
    let _ = writeln!(s, "#endif // {}_H", upper);
    s
}

fn aot_meta_struct_name(prefix: &str) -> String {
    format!("{}_global_info", prefix)
}

fn sanitize_macro_name(s: &str) -> String {
    s.chars()
        .map(|c| if c.is_alphanumeric() || c == '_' { c.to_ascii_uppercase() } else { '_' })
        .collect()
}

fn c_string_escape(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    for c in s.chars() {
        match c {
            '\\' => out.push_str("\\\\"),
            '"' => out.push_str("\\\""),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            c if (c as u32) < 0x20 => {
                use std::fmt::Write;
                let _ = write!(out, "\\x{:02x}", c as u32);
            }
            c => out.push(c),
        }
    }
    out
}
