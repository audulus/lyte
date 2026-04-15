use std::ffi::{CStr, CString};
use std::os::raw::c_char;
use std::panic::{self, AssertUnwindSafe};
use std::ptr;

use crate::compiler::Compiler;
#[cfg(all(not(feature = "llvm"), target_os = "ios"))]
use crate::stack_interp_bridge::StackBackend;
use crate::vm::PrintCallbackFn;
#[cfg(all(not(feature = "llvm"), not(target_os = "ios")))]
use crate::vm::{LinkedProgram, VMProgram, VM};

/// Info about a single global variable, with pre-computed C strings.
struct GlobalInfo {
    name: CString,
    offset: usize,
    size: usize,
    ty: CString,
    is_extern: bool,
}

/// An entry point: backend-specific data.
/// Index matches the order of entry points passed to lyte_compiler_new.
struct EntryPointInfo {
    #[cfg(feature = "llvm")]
    fn_addr: usize,
    #[cfg(all(not(feature = "llvm"), not(target_os = "ios")))]
    func_idx: crate::vm::FuncIdx,
    #[cfg(all(not(feature = "llvm"), target_os = "ios"))]
    func_idx: u32,
}

/// Opaque handle to a lyte compiler instance.
pub struct LyteCompiler {
    compiler: Compiler,
    last_error: Option<CString>,
    /// Whether an internal compiler error (panic) has occurred,
    /// rendering this compiler instance invalid.
    ice_occurred: bool,
}

impl LyteCompiler {
    fn set_error(&mut self, msg: &str) {
        self.last_error = Some(CString::new(msg).unwrap_or_default());
    }
}

/// Run a closure, catching any panic and storing it as the last error.
/// Returns `false` if a panic occurred or if the compiler is in an invalid
/// state due to a previous ICE.
unsafe fn catch_panic(c: *mut LyteCompiler, f: impl FnOnce(&mut LyteCompiler) -> bool) -> bool {
    if (*c).ice_occurred {
        (*c).set_error("compiler is in an invalid state after a previous internal compiler error");
        return false;
    }
    let result = panic::catch_unwind(AssertUnwindSafe(|| f(&mut *c)));
    match result {
        Ok(v) => v,
        Err(e) => {
            let msg = if let Some(s) = e.downcast_ref::<&str>() {
                format!("internal compiler error: {}", s)
            } else if let Some(s) = e.downcast_ref::<String>() {
                format!("internal compiler error: {}", s)
            } else {
                "internal compiler error".to_string()
            };
            (*c).set_error(&msg);
            (*c).ice_occurred = true;
            false
        }
    }
}

/// Create a new compiler instance with the given entry point names.
/// Pass NULL/0 to default to a single "main" entry point.
///
/// Compilers created through this FFI enable `--no-recursion` by
/// default: the safety checker rejects any recursive call graph and
/// the native backends skip the per-function call-depth check, which
/// recovers the ~2x DSP regression documented in
/// docs/LLVM_CALL_DEPTH_REGRESSION.md. Lyte code compiled through the
/// xcframework must therefore have a DAG call graph.
#[no_mangle]
pub unsafe extern "C" fn lyte_compiler_new(
    entry_points: *const *const c_char,
    count: usize,
) -> *mut LyteCompiler {
    let mut compiler = Compiler::new();
    compiler.no_recursion = true;

    if !entry_points.is_null() && count > 0 {
        let mut names = Vec::with_capacity(count);
        for i in 0..count {
            let name_ptr = *entry_points.add(i);
            if !name_ptr.is_null() {
                if let Ok(s) = CStr::from_ptr(name_ptr).to_str() {
                    names.push(s.to_string());
                }
            }
        }
        let refs: Vec<&str> = names.iter().map(|s| s.as_str()).collect();
        compiler.set_entry_points(&refs);
    }

    Box::into_raw(Box::new(LyteCompiler {
        compiler,
        last_error: None,
        ice_occurred: false,
    }))
}

/// Free a compiler instance.
#[no_mangle]
pub unsafe extern "C" fn lyte_compiler_free(ptr: *mut LyteCompiler) {
    if !ptr.is_null() {
        drop(Box::from_raw(ptr));
    }
}

/// Returns true if an internal compiler error (panic) has occurred,
/// rendering this compiler instance invalid. A new compiler must be created.
#[no_mangle]
pub unsafe extern "C" fn lyte_compiler_had_ice(ptr: *const LyteCompiler) -> bool {
    if ptr.is_null() {
        return false;
    }
    (*ptr).ice_occurred
}

/// Get the last error message, or NULL if no error.
#[no_mangle]
pub unsafe extern "C" fn lyte_compiler_get_error(ptr: *const LyteCompiler) -> *const c_char {
    if ptr.is_null() {
        return ptr::null();
    }
    match &(*ptr).last_error {
        Some(s) => s.as_ptr(),
        None => ptr::null(),
    }
}

/// Add source code to the compiler. May be called multiple times.
/// Returns true on success.
#[no_mangle]
pub unsafe extern "C" fn lyte_compiler_add_source(
    ptr: *mut LyteCompiler,
    source: *const c_char,
    filename: *const c_char,
) -> bool {
    if ptr.is_null() || source.is_null() || filename.is_null() {
        return false;
    }
    let src = source;
    let fname = filename;
    catch_panic(ptr, |c| {
        let source = match CStr::from_ptr(src).to_str() {
            Ok(s) => s,
            Err(_) => {
                c.set_error("invalid UTF-8 in source");
                return false;
            }
        };
        let filename = match CStr::from_ptr(fname).to_str() {
            Ok(s) => s,
            Err(_) => {
                c.set_error("invalid UTF-8 in filename");
                return false;
            }
        };
        c.last_error = None;
        if !c.compiler.parse(source, filename) {
            c.set_error(&c.compiler.last_errors.join("\n"));
            return false;
        }
        true
    })
}

/// Add trusted prelude source code to the compiler.
/// The prelude is allowed to use `assume` statements.
/// Line numbers in the prelude are independent of the main source.
/// Returns true on success.
#[no_mangle]
pub unsafe extern "C" fn lyte_compiler_add_prelude(
    ptr: *mut LyteCompiler,
    source: *const c_char,
) -> bool {
    if ptr.is_null() || source.is_null() {
        return false;
    }
    let src = source;
    catch_panic(ptr, |c| {
        let source = match CStr::from_ptr(src).to_str() {
            Ok(s) => s,
            Err(_) => {
                c.set_error("invalid UTF-8 in prelude source");
                return false;
            }
        };
        c.last_error = None;
        if !c.compiler.parse(source, "<prelude>") {
            c.set_error(&c.compiler.last_errors.join("\n"));
            return false;
        }
        true
    })
}

// ============ Program API ============

// On LLVM builds: JIT backend with function pointers.
#[cfg(feature = "llvm")]
pub struct LyteProgram {
    inner: crate::llvm_jit::LLVMCompiledProgram,
    globals_size: usize,
    globals_info: Vec<GlobalInfo>,
    entry_points: Vec<EntryPointInfo>,
    cancel_callback: Option<unsafe extern "C" fn(*mut u8) -> bool>,
    cancel_userdata: *mut u8,
    print_callback: Option<PrintCallbackFn>,
    print_userdata: *mut u8,
}

// On non-LLVM non-iOS builds: byte-VM backend with function indices.
// Used by the default (non-xcframework) build, CLI, fuzz targets, etc.
#[cfg(all(not(feature = "llvm"), not(target_os = "ios")))]
pub struct LyteProgram {
    vm_program: VMProgram,
    linked: LinkedProgram,
    vm: VM,
    globals_size: usize,
    globals_info: Vec<GlobalInfo>,
    entry_points: Vec<EntryPointInfo>,
    cancel_callback: Option<unsafe extern "C" fn(*mut u8) -> bool>,
    cancel_userdata: *mut u8,
    print_callback: Option<PrintCallbackFn>,
    print_userdata: *mut u8,
}

// On iOS (non-LLVM) builds: stack interpreter backend via the C interp.
// Requires has_stack_interp (set by build.rs when compiling with clang,
// which is always the case for iOS cross-builds). The cancel callback is
// stored for API compatibility but the stack interpreter does not
// currently poll it.
#[cfg(all(not(feature = "llvm"), target_os = "ios"))]
pub struct LyteProgram {
    backend: StackBackend,
    globals_size: usize,
    globals_info: Vec<GlobalInfo>,
    entry_points: Vec<EntryPointInfo>,
    cancel_callback: Option<unsafe extern "C" fn(*mut u8) -> bool>,
    cancel_userdata: *mut u8,
    print_callback: Option<PrintCallbackFn>,
    print_userdata: *mut u8,
}

/// Parse, type-check, specialize, and compile all added source into
/// a LyteProgram (auto-selects JIT or VM backend).
/// Returns NULL on error. Caller must free with lyte_program_free.
#[no_mangle]
pub unsafe extern "C" fn lyte_compiler_compile(ptr: *mut LyteCompiler) -> *mut LyteProgram {
    if ptr.is_null() {
        return ptr::null_mut();
    }
    let c = &mut *ptr;
    if c.ice_occurred {
        c.set_error("compiler is in an invalid state after a previous internal compiler error");
        return ptr::null_mut();
    }
    let result = panic::catch_unwind(AssertUnwindSafe(|| {
        c.last_error = None;

        if !c.compiler.check() {
            c.set_error(&c.compiler.last_errors.join("\n"));
            return ptr::null_mut();
        }
        if let Err(e) = c.compiler.specialize() {
            c.set_error(&e);
            return ptr::null_mut();
        }

        let make_globals_info = |base_offset: usize| -> Vec<GlobalInfo> {
            c.compiler
                .globals_info_with_offset(base_offset)
                .into_iter()
                .map(|(name, offset, size, ty, is_extern)| GlobalInfo {
                    name: CString::new(name).unwrap_or_default(),
                    offset,
                    size,
                    ty: CString::new(ty).unwrap_or_default(),
                    is_extern,
                })
                .collect()
        };
        let entry_point_names = c.compiler.effective_entry_points();

        #[cfg(feature = "llvm")]
        {
            match c.compiler.compile_program() {
                Ok(compiled) => {
                    let globals_size = compiled.globals_size();
                    let globals_info =
                        make_globals_info(crate::cancel::CANCEL_FLAG_RESERVED as usize);
                    let inner = match compiled {
                        crate::compiler::CompiledProgram::Llvm(p) => p,
                        _ => unreachable!(),
                    };

                    let mut entry_points = Vec::new();
                    for ep_name in &entry_point_names {
                        if let Some(&fn_addr) = inner.entry_points.get(ep_name) {
                            entry_points.push(EntryPointInfo { fn_addr });
                        }
                    }

                    let program = Box::new(LyteProgram {
                        inner,
                        globals_size,
                        globals_info,
                        entry_points,
                        cancel_callback: None,
                        cancel_userdata: ptr::null_mut(),
                        print_callback: None,
                        print_userdata: ptr::null_mut(),
                    });

                    Box::into_raw(program)
                }
                Err(e) => {
                    c.set_error(&e);
                    ptr::null_mut()
                }
            }
        }

        #[cfg(all(not(feature = "llvm"), not(target_os = "ios")))]
        {
            match c.compiler.compile_vm() {
                Ok(vm_program) => {
                    let globals_size = vm_program.globals_size;
                    let globals_info = make_globals_info(0);
                    let linked = LinkedProgram::from_program(&vm_program);
                    let vm = VM::new();

                    let mut entry_points = Vec::new();
                    for ep_name in &entry_point_names {
                        if let Some(&func_idx) = vm_program.entry_points.get(ep_name) {
                            entry_points.push(EntryPointInfo { func_idx });
                        }
                    }

                    let program = Box::new(LyteProgram {
                        vm_program,
                        linked,
                        vm,
                        globals_size,
                        globals_info,
                        entry_points,
                        cancel_callback: None,
                        cancel_userdata: ptr::null_mut(),
                        print_callback: None,
                        print_userdata: ptr::null_mut(),
                    });

                    Box::into_raw(program)
                }
                Err(e) => {
                    c.set_error(&e);
                    ptr::null_mut()
                }
            }
        }

        #[cfg(all(not(feature = "llvm"), target_os = "ios"))]
        {
            match c.compiler.compile_stack() {
                Ok(stack_program) => {
                    let globals_size = stack_program.globals_size;
                    let globals_info = make_globals_info(0);

                    let mut entry_points = Vec::new();
                    for ep_name in &entry_point_names {
                        if let Some(&func_idx) = stack_program.entry_points.get(ep_name) {
                            entry_points.push(EntryPointInfo { func_idx });
                        }
                    }

                    let backend = StackBackend::new(&stack_program);

                    let program = Box::new(LyteProgram {
                        backend,
                        globals_size,
                        globals_info,
                        entry_points,
                        cancel_callback: None,
                        cancel_userdata: ptr::null_mut(),
                        print_callback: None,
                        print_userdata: ptr::null_mut(),
                    });

                    Box::into_raw(program)
                }
                Err(e) => {
                    c.set_error(&e);
                    ptr::null_mut()
                }
            }
        }
    }));
    match result {
        Ok(p) => p,
        Err(e) => {
            let msg = if let Some(s) = e.downcast_ref::<&str>() {
                format!("internal compiler error: {}", s)
            } else if let Some(s) = e.downcast_ref::<String>() {
                format!("internal compiler error: {}", s)
            } else {
                "internal compiler error".to_string()
            };
            c.set_error(&msg);
            c.ice_occurred = true;
            ptr::null_mut()
        }
    }
}

/// Free a compiled program.
#[no_mangle]
pub unsafe extern "C" fn lyte_program_free(ptr: *mut LyteProgram) {
    if !ptr.is_null() {
        drop(Box::from_raw(ptr));
    }
}

/// Get the size in bytes of the globals buffer needed.
#[no_mangle]
pub unsafe extern "C" fn lyte_program_get_globals_size(ptr: *const LyteProgram) -> usize {
    if ptr.is_null() {
        return 0;
    }
    (*ptr).globals_size
}

/// Get the number of global variables.
#[no_mangle]
pub unsafe extern "C" fn lyte_program_get_globals_count(ptr: *const LyteProgram) -> usize {
    if ptr.is_null() {
        return 0;
    }
    (*ptr).globals_info.len()
}

/// Get the name of a global variable by index.
#[no_mangle]
pub unsafe extern "C" fn lyte_program_get_global_name(
    ptr: *const LyteProgram,
    index: usize,
) -> *const c_char {
    if ptr.is_null() {
        return ptr::null();
    }
    let p = &*ptr;
    p.globals_info
        .get(index)
        .map_or(ptr::null(), |g| g.name.as_ptr())
}

/// Get the byte offset of a global variable.
#[no_mangle]
pub unsafe extern "C" fn lyte_program_get_global_offset(
    ptr: *const LyteProgram,
    index: usize,
) -> usize {
    if ptr.is_null() {
        return 0;
    }
    let p = &*ptr;
    p.globals_info.get(index).map_or(0, |g| g.offset)
}

/// Get the size in bytes of a global variable.
#[no_mangle]
pub unsafe extern "C" fn lyte_program_get_global_size(
    ptr: *const LyteProgram,
    index: usize,
) -> usize {
    if ptr.is_null() {
        return 0;
    }
    let p = &*ptr;
    p.globals_info.get(index).map_or(0, |g| g.size)
}

/// Get the type of a global variable as a string.
#[no_mangle]
pub unsafe extern "C" fn lyte_program_get_global_type(
    ptr: *const LyteProgram,
    index: usize,
) -> *const c_char {
    if ptr.is_null() {
        return ptr::null();
    }
    let p = &*ptr;
    p.globals_info
        .get(index)
        .map_or(ptr::null(), |g| g.ty.as_ptr())
}

/// Returns true if the global at this index is an extern function.
#[no_mangle]
pub unsafe extern "C" fn lyte_program_get_global_is_extern(
    ptr: *const LyteProgram,
    index: usize,
) -> bool {
    if ptr.is_null() {
        return false;
    }
    let p = &*ptr;
    p.globals_info.get(index).map_or(false, |g| g.is_extern)
}

/// Bind a host function to an extern function slot in the globals buffer.
/// The offset should come from lyte_program_get_global_offset for an extern function global.
/// The host function must use C calling conventions with a prepended context parameter:
///   extern fn foo(i32) -> f32  →  float (*)(void* ctx, int32_t)
/// The caller must ensure func_ptr and context remain valid for program execution.
#[no_mangle]
pub unsafe extern "C" fn lyte_globals_bind_extern(
    globals: *mut u8,
    offset: usize,
    func_ptr: *const u8,
    context: *mut u8,
) {
    if globals.is_null() {
        return;
    }
    // Extern layout: {fn_ptr: *const (), context: *const ()} — 16 bytes
    ptr::write_unaligned(globals.add(offset) as *mut usize, func_ptr as usize);
    ptr::write_unaligned(globals.add(offset + 8) as *mut usize, context as usize);
}

/// Call an entry point by index with an external globals buffer.
/// The index corresponds to the order of entry points passed to lyte_compiler_new.
/// Returns true on success, false if cancelled or invalid index.
#[no_mangle]
pub unsafe extern "C" fn lyte_entry_point_call(
    ptr: *mut LyteProgram,
    entry_point: usize,
    globals: *mut u8,
) -> bool {
    if ptr.is_null() || globals.is_null() {
        return false;
    }
    let program = &mut *ptr;
    let ep = match program.entry_points.get(entry_point) {
        Some(ep) => ep,
        None => return false,
    };

    // Install print callback for the duration of this call.
    crate::vm::set_print_callback(program.print_callback, program.print_userdata);

    let result;

    #[cfg(feature = "llvm")]
    {
        extern "C" {
            fn setjmp(env: *mut u8) -> i32;
        }
        type Entry = unsafe extern "C" fn(*mut u8, *mut u8);
        crate::cancel::set_cancel_callback(
            globals,
            program.cancel_callback,
            program.cancel_userdata,
        );
        let jmp_buf_ptr = globals.add(crate::cancel::JMPBUF_OFFSET);
        if setjmp(jmp_buf_ptr) == 0 {
            let code_fn: Entry = std::mem::transmute(ep.fn_addr);
            code_fn(globals, ptr::null_mut());
            result = true;
        } else {
            result = false; // cancelled
        }
    }

    #[cfg(all(not(feature = "llvm"), not(target_os = "ios")))]
    {
        let func_idx = ep.func_idx;
        if let Some(cb) = program.cancel_callback {
            program
                .vm
                .set_cancel_callback(Some(cb), program.cancel_userdata);
        }
        let gs = program.globals_size;
        #[cfg(target_arch = "aarch64")]
        {
            program.vm.call_with_external_globals_asm(
                &program.linked,
                &program.vm_program,
                func_idx,
                globals,
                gs,
            );
        }
        #[cfg(not(target_arch = "aarch64"))]
        {
            program.vm.call_with_external_globals(
                &program.linked,
                &program.vm_program,
                func_idx,
                globals,
                gs,
            );
        }
        result = !program.vm.cancelled;
    }

    #[cfg(all(not(feature = "llvm"), target_os = "ios"))]
    {
        // The stack interpreter does not currently poll a cancel callback.
        // Binding it here is a no-op; stored only so the ffi surface
        // matches other backends.
        let _ = (program.cancel_callback, program.cancel_userdata);
        program.backend.call_entry(ep.func_idx, globals);
        result = true;
    }

    // Clear print callback.
    crate::vm::set_print_callback(None, ptr::null_mut());
    result
}

/// Bind an external buffer to a global slice variable.
/// Writes the data pointer and length into the globals buffer at the given offset.
#[no_mangle]
pub unsafe extern "C" fn lyte_globals_bind_slice(
    globals: *mut u8,
    offset: usize,
    data: *const u8,
    len: i32,
) {
    if globals.is_null() {
        return;
    }
    // Slice layout: data_ptr (i64, 8 bytes) at offset, len (i32, 4 bytes) at offset+8
    ptr::write_unaligned(globals.add(offset) as *mut usize, data as usize);
    ptr::write_unaligned(globals.add(offset + 8) as *mut i32, len);
}

/// Allocate a zeroed globals buffer of the correct size.
/// Caller must free with lyte_globals_free().
#[no_mangle]
pub unsafe extern "C" fn lyte_globals_alloc(ptr: *const LyteProgram) -> *mut u8 {
    if ptr.is_null() {
        return ptr::null_mut();
    }
    let size = (*ptr).globals_size;
    if size == 0 {
        return ptr::null_mut();
    }
    let layout = std::alloc::Layout::from_size_align(size, 16).unwrap();
    let mem = std::alloc::alloc_zeroed(layout);
    // Initialize cancel counter.
    if !mem.is_null() {
        *(mem as *mut i32) = crate::cancel::CANCEL_CHECK_INTERVAL;
    }
    mem
}

/// Free a globals buffer allocated by lyte_globals_alloc.
#[no_mangle]
pub unsafe extern "C" fn lyte_globals_free(globals: *mut u8, size: usize) {
    if !globals.is_null() && size > 0 {
        let layout = std::alloc::Layout::from_size_align(size, 16).unwrap();
        std::alloc::dealloc(globals, layout);
    }
}

/// Set a cancel callback for this program.
/// The callback is called periodically at backward jumps.
/// If it returns true, execution is cancelled.
#[no_mangle]
pub unsafe extern "C" fn lyte_program_set_cancel_callback(
    ptr: *mut LyteProgram,
    callback: Option<unsafe extern "C" fn(*mut u8) -> bool>,
    user_data: *mut u8,
) {
    if ptr.is_null() {
        return;
    }
    (*ptr).cancel_callback = callback;
    (*ptr).cancel_userdata = user_data;
}

/// Set a print callback for this program.
/// When set, all print/println/putc output from lyte scripts is routed
/// through this callback instead of stdout. Pass NULL to restore default behavior.
#[no_mangle]
pub unsafe extern "C" fn lyte_program_set_print_callback(
    ptr: *mut LyteProgram,
    callback: Option<PrintCallbackFn>,
    user_data: *mut u8,
) {
    if ptr.is_null() {
        return;
    }
    (*ptr).print_callback = callback;
    (*ptr).print_userdata = user_data;
}
