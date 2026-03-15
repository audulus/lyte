use std::collections::HashMap;
use std::ffi::{CStr, CString};
use std::os::raw::c_char;
use std::panic::{self, AssertUnwindSafe};
use std::ptr;

use crate::compiler::{CompiledProgram, Compiler, EntryPoint};
use crate::Name;
use crate::vm::{VMProgram, VM};

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

/// Info about a single global variable, with pre-computed C strings.
struct GlobalInfo {
    name: CString,
    offset: usize,
    size: usize,
    ty: CString,
}

/// Opaque handle to a lyte compiler instance.
pub struct LyteCompiler {
    compiler: Compiler,
    last_error: Option<CString>,
    /// JIT result: code pointer and globals size.
    jit_result: Option<(*const u8, usize, crate::jit::JIT)>,
    /// JIT entry point code pointers (populated after multi-entry JIT).
    jit_entry_points: HashMap<Name, *const u8>,
    /// Cached global variable info (populated after JIT or VM compile).
    globals: Vec<GlobalInfo>,
    /// VM program (populated after VM compile).
    vm_program: Option<VMProgram>,
    /// VM instance (populated after VM run).
    vm: Option<VM>,
    /// Whether an internal compiler error (panic) has occurred,
    /// rendering this compiler instance invalid.
    ice_occurred: bool,
}

impl Drop for LyteCompiler {
    fn drop(&mut self) {
        if let Some((_, _, jit)) = self.jit_result.take() {
            jit.free_memory();
        }
    }
}

impl LyteCompiler {
    fn set_error(&mut self, msg: &str) {
        self.last_error = Some(CString::new(msg).unwrap_or_default());
    }
}

/// Create a new compiler instance.
#[no_mangle]
pub extern "C" fn lyte_compiler_new() -> *mut LyteCompiler {
    Box::into_raw(Box::new(LyteCompiler {
        compiler: Compiler::new(),
        last_error: None,
        jit_result: None,
        jit_entry_points: HashMap::new(),
        globals: Vec::new(),
        vm_program: None,
        vm: None,
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
    lyte_compiler_parse(ptr, source, filename)
}

/// Parse source code. Returns true on success.
#[no_mangle]
pub unsafe extern "C" fn lyte_compiler_parse(
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
            c.set_error("parse error");
            return false;
        }
        true
    })
}

/// Type-check the parsed source. Returns true on success.
#[no_mangle]
pub unsafe extern "C" fn lyte_compiler_check(ptr: *mut LyteCompiler) -> bool {
    if ptr.is_null() {
        return false;
    }
    catch_panic(ptr, |c| {
        c.last_error = None;
        if !c.compiler.check() {
            c.set_error("type check error");
            return false;
        }
        true
    })
}

/// Monomorphize generics. Returns true on success.
#[no_mangle]
pub unsafe extern "C" fn lyte_compiler_specialize(ptr: *mut LyteCompiler) -> bool {
    if ptr.is_null() {
        return false;
    }
    catch_panic(ptr, |c| {
        c.last_error = None;
        if let Err(e) = c.compiler.specialize() {
            c.set_error(&e);
            return false;
        }
        true
    })
}

/// JIT compile. Returns true on success.
/// On success, use lyte_compiler_get_code_ptr and lyte_compiler_get_globals_size.
#[no_mangle]
pub unsafe extern "C" fn lyte_compiler_jit(ptr: *mut LyteCompiler) -> bool {
    if ptr.is_null() {
        return false;
    }
    catch_panic(ptr, |c| {
        c.last_error = None;
        // Free previous JIT executable memory before compiling again.
        if let Some((_, _, jit)) = c.jit_result.take() {
            jit.free_memory();
        }
        c.globals.clear();
        c.jit_entry_points.clear();
        match c.compiler.jit_multi() {
            Ok((map, globals_size, jit)) => {
                // Store first entry point as the main code_ptr for backward compat.
                let first_ptr = map.values().next().copied().unwrap_or(ptr::null());
                c.jit_entry_points = map;
                c.jit_result = Some((first_ptr, globals_size, jit));
                c.globals = c
                    .compiler
                    .globals_info()
                    .into_iter()
                    .map(|(name, offset, size, ty)| GlobalInfo {
                        name: CString::new(name).unwrap_or_default(),
                        offset,
                        size,
                        ty: CString::new(ty).unwrap_or_default(),
                    })
                    .collect();
                true
            }
            Err(msg) => {
                c.set_error(&msg);
                false
            }
        }
    })
}

/// Get the JIT-compiled code pointer (the `main` function).
/// The function signature is `void main(uint8_t* globals)`.
/// Returns NULL if JIT has not been run or failed.
#[no_mangle]
pub unsafe extern "C" fn lyte_compiler_get_code_ptr(ptr: *const LyteCompiler) -> *const u8 {
    if ptr.is_null() {
        return ptr::null();
    }
    match (*ptr).jit_result {
        Some((code_ptr, _, _)) => code_ptr,
        None => ptr::null(),
    }
}

/// Get the size in bytes of the globals buffer needed by the JIT-compiled code.
/// Returns 0 if JIT has not been run or failed.
#[no_mangle]
pub unsafe extern "C" fn lyte_compiler_get_globals_size(ptr: *const LyteCompiler) -> usize {
    if ptr.is_null() {
        return 0;
    }
    match (*ptr).jit_result {
        Some((_, size, _)) => size,
        None => 0,
    }
}

/// Convenience: parse, check, specialize, and JIT compile in one call.
/// Returns true on success.
#[no_mangle]
pub unsafe extern "C" fn lyte_compile(
    ptr: *mut LyteCompiler,
    source: *const c_char,
    filename: *const c_char,
) -> bool {
    lyte_compiler_parse(ptr, source, filename)
        && lyte_compiler_check(ptr)
        && lyte_compiler_specialize(ptr)
        && lyte_compiler_jit(ptr)
}

/// Get the number of global variables.
/// Returns 0 if JIT has not been run.
#[no_mangle]
pub unsafe extern "C" fn lyte_compiler_get_globals_count(ptr: *const LyteCompiler) -> usize {
    if ptr.is_null() {
        return 0;
    }
    (*ptr).globals.len()
}

/// Get the name of a global variable by index.
/// Returns NULL if index is out of bounds.
#[no_mangle]
pub unsafe extern "C" fn lyte_compiler_get_global_name(
    ptr: *const LyteCompiler,
    index: usize,
) -> *const c_char {
    if ptr.is_null() {
        return ptr::null();
    }
    match (&(*ptr).globals).get(index) {
        Some(info) => info.name.as_ptr(),
        None => ptr::null(),
    }
}

/// Get the byte offset of a global variable within the globals buffer.
/// Returns 0 if index is out of bounds.
#[no_mangle]
pub unsafe extern "C" fn lyte_compiler_get_global_offset(
    ptr: *const LyteCompiler,
    index: usize,
) -> usize {
    if ptr.is_null() {
        return 0;
    }
    match (&(*ptr).globals).get(index) {
        Some(info) => info.offset,
        None => 0,
    }
}

/// Get the size in bytes of a global variable.
/// Returns 0 if index is out of bounds.
#[no_mangle]
pub unsafe extern "C" fn lyte_compiler_get_global_size(
    ptr: *const LyteCompiler,
    index: usize,
) -> usize {
    if ptr.is_null() {
        return 0;
    }
    match (&(*ptr).globals).get(index) {
        Some(info) => info.size,
        None => 0,
    }
}

/// Get the type of a global variable as a lyte syntax string.
/// Returns NULL if index is out of bounds.
#[no_mangle]
pub unsafe extern "C" fn lyte_compiler_get_global_type(
    ptr: *const LyteCompiler,
    index: usize,
) -> *const c_char {
    if ptr.is_null() {
        return ptr::null();
    }
    match (&(*ptr).globals).get(index) {
        Some(info) => info.ty.as_ptr(),
        None => ptr::null(),
    }
}

// ============ VM API ============

/// Compile to VM bytecode. Returns true on success.
/// Must call parse, check, and specialize first.
#[no_mangle]
pub unsafe extern "C" fn lyte_compiler_compile_vm(ptr: *mut LyteCompiler) -> bool {
    if ptr.is_null() {
        return false;
    }
    catch_panic(ptr, |c| {
        c.last_error = None;
        c.vm_program = None;
        c.vm = None;
        c.globals.clear();
        match c.compiler.compile_vm() {
            Ok(program) => {
                c.globals = c
                    .compiler
                    .globals_info()
                    .into_iter()
                    .map(|(name, offset, size, ty)| GlobalInfo {
                        name: CString::new(name).unwrap_or_default(),
                        offset,
                        size,
                        ty: CString::new(ty).unwrap_or_default(),
                    })
                    .collect();
                c.vm_program = Some(program);
                true
            }
            Err(msg) => {
                c.set_error(&msg);
                false
            }
        }
    })
}

/// Run the VM program. Returns true on success.
/// Must call lyte_compiler_compile_vm first.
/// After running, globals can be read via lyte_compiler_get_vm_globals_ptr.
#[no_mangle]
pub unsafe extern "C" fn lyte_compiler_run_vm(ptr: *mut LyteCompiler) -> bool {
    if ptr.is_null() {
        return false;
    }
    catch_panic(ptr, |c| {
        c.last_error = None;
        match &c.vm_program {
            Some(program) => {
                let mut vm = VM::new();
                vm.run(program);
                c.vm = Some(vm);
                true
            }
            None => {
                c.set_error("no VM program compiled");
                false
            }
        }
    })
}

/// Get a pointer to the VM's globals buffer.
/// Returns NULL if the VM has not been run.
/// The pointer is valid until the next call to lyte_compiler_run_vm
/// or lyte_compiler_free.
#[no_mangle]
pub unsafe extern "C" fn lyte_compiler_get_vm_globals_ptr(ptr: *mut LyteCompiler) -> *mut u8 {
    if ptr.is_null() {
        return ptr::null_mut();
    }
    match &mut (*ptr).vm {
        Some(vm) => vm.globals_ptr(),
        None => ptr::null_mut(),
    }
}

/// Get the size in bytes of the VM globals buffer.
/// Returns 0 if no VM program has been compiled.
#[no_mangle]
pub unsafe extern "C" fn lyte_compiler_get_vm_globals_size(ptr: *const LyteCompiler) -> usize {
    if ptr.is_null() {
        return 0;
    }
    match &(*ptr).vm_program {
        Some(program) => program.globals_size,
        None => 0,
    }
}

/// Convenience: parse, check, specialize, and compile to VM in one call.
/// Returns true on success.
#[no_mangle]
pub unsafe extern "C" fn lyte_compile_vm(
    ptr: *mut LyteCompiler,
    source: *const c_char,
    filename: *const c_char,
) -> bool {
    lyte_compiler_parse(ptr, source, filename)
        && lyte_compiler_check(ptr)
        && lyte_compiler_specialize(ptr)
        && lyte_compiler_compile_vm(ptr)
}

// ============ Multi-Entry-Point API ============

/// Set entry point function names before calling specialize.
/// If not called, defaults to ["main"].
/// Returns true on success.
#[no_mangle]
pub unsafe extern "C" fn lyte_compiler_set_entry_points(
    ptr: *mut LyteCompiler,
    names: *const *const c_char,
    count: usize,
) -> bool {
    if ptr.is_null() || names.is_null() {
        return false;
    }
    catch_panic(ptr, |c| {
        c.last_error = None;
        let mut entry_names = Vec::with_capacity(count);
        for i in 0..count {
            let name_ptr = *names.add(i);
            if name_ptr.is_null() {
                c.set_error("null entry point name");
                return false;
            }
            match CStr::from_ptr(name_ptr).to_str() {
                Ok(s) => entry_names.push(s.to_string()),
                Err(_) => {
                    c.set_error("invalid UTF-8 in entry point name");
                    return false;
                }
            }
        }
        let refs: Vec<&str> = entry_names.iter().map(|s| s.as_str()).collect();
        c.compiler.set_entry_points(&refs);
        true
    })
}

/// Get the JIT-compiled code pointer for a named entry point.
/// Returns NULL if the entry point is not found or JIT has not been run.
#[no_mangle]
pub unsafe extern "C" fn lyte_compiler_get_entry_point(
    ptr: *const LyteCompiler,
    name: *const c_char,
) -> *const u8 {
    if ptr.is_null() || name.is_null() {
        return ptr::null();
    }
    let name_str = match CStr::from_ptr(name).to_str() {
        Ok(s) => s,
        Err(_) => return ptr::null(),
    };
    let key = Name::new(name_str.into());
    match (*ptr).jit_entry_points.get(&key) {
        Some(&code_ptr) => code_ptr,
        None => ptr::null(),
    }
}

/// Initialize VM globals (zeroed) without running any function.
/// Must call compile_vm first. Returns true on success.
#[no_mangle]
pub unsafe extern "C" fn lyte_compiler_init_vm(ptr: *mut LyteCompiler) -> bool {
    if ptr.is_null() {
        return false;
    }
    catch_panic(ptr, |c| {
        c.last_error = None;
        match &c.vm_program {
            Some(program) => {
                let mut vm = VM::new();
                // Allocate zeroed globals without running.
                vm.globals = vec![0u8; program.globals_size];
                c.vm = Some(vm);
                true
            }
            None => {
                c.set_error("no VM program compiled");
                false
            }
        }
    })
}

/// Call a specific VM function by name with i64 arguments.
/// Globals persist between calls. Must call init_vm or run_vm first.
/// Returns true on success.
#[no_mangle]
pub unsafe extern "C" fn lyte_compiler_vm_call(
    ptr: *mut LyteCompiler,
    name: *const c_char,
    args: *const i64,
    arg_count: usize,
) -> bool {
    if ptr.is_null() || name.is_null() {
        return false;
    }
    let name_cstr = name;
    let args_ptr = args;
    let argc = arg_count;
    catch_panic(ptr, |c| {
        c.last_error = None;
        let name_str = match CStr::from_ptr(name_cstr).to_str() {
            Ok(s) => s,
            Err(_) => {
                c.set_error("invalid UTF-8 in function name");
                return false;
            }
        };
        let key = Name::new(name_str.into());

        let args_slice: &[i64] = if args_ptr.is_null() || argc == 0 {
            &[]
        } else {
            std::slice::from_raw_parts(args_ptr, argc)
        };

        let program = match &c.vm_program {
            Some(p) => p,
            None => {
                c.set_error("no VM program compiled");
                return false;
            }
        };

        // Clone program ref to avoid borrow conflict with c.vm.
        let program_clone = program.clone();

        let vm = c.vm.get_or_insert_with(VM::new);
        match vm.call(&program_clone, key, args_slice) {
            Ok(_) => true,
            Err(e) => {
                c.set_error(&e);
                false
            }
        }
    })
}

/// Set a cancel callback for VM execution.
/// The callback is called approximately every 1024 backward jumps.
/// If it returns true, execution is cancelled. Pass NULL to disable.
#[no_mangle]
pub unsafe extern "C" fn lyte_compiler_set_vm_cancel_callback(
    ptr: *mut LyteCompiler,
    callback: Option<unsafe extern "C" fn(*mut u8) -> bool>,
    user_data: *mut u8,
) {
    if ptr.is_null() {
        return;
    }
    let vm = (*ptr).vm.get_or_insert_with(VM::new);
    vm.set_cancel_callback(callback, user_data);
}

// ============ Backend-Agnostic Program API ============

/// A compiled program, abstracting over JIT and VM backends.
pub struct LyteProgram {
    inner: CompiledProgram,
    globals_size: usize,
    globals_info: Vec<GlobalInfo>,
    entry_points: Vec<LyteEntryPoint>,
    cancel_callback: Option<unsafe extern "C" fn(*mut u8) -> bool>,
    cancel_userdata: *mut u8,
    last_error: Option<CString>,
}

/// An entry point handle for calling compiled functions.
pub struct LyteEntryPoint {
    program: *mut LyteProgram,
    kind: EntryPoint,
    name: CString,
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

        // Run all compilation phases.
        if !c.compiler.check() {
            c.set_error("type check error");
            return ptr::null_mut();
        }
        if let Err(e) = c.compiler.specialize() {
            c.set_error(&e);
            return ptr::null_mut();
        }

        match c.compiler.compile_program() {
            Ok(compiled) => {
                let globals_size = compiled.globals_size();
                let globals_info: Vec<GlobalInfo> = c
                    .compiler
                    .globals_info()
                    .into_iter()
                    .map(|(name, offset, size, ty)| GlobalInfo {
                        name: CString::new(name).unwrap_or_default(),
                        offset,
                        size,
                        ty: CString::new(ty).unwrap_or_default(),
                    })
                    .collect();

                let mut program = Box::new(LyteProgram {
                    inner: compiled,
                    globals_size,
                    globals_info,
                    entry_points: Vec::new(),
                    cancel_callback: None,
                    cancel_userdata: ptr::null_mut(),
                    last_error: None,
                });

                // Build entry point handles.
                let prog_ptr: *mut LyteProgram = &mut *program;
                let entry_point_names = c.compiler.effective_entry_points();
                for ep_name in &entry_point_names {
                    if let Some(kind) = program.inner.get_entry_point(*ep_name) {
                        program.entry_points.push(LyteEntryPoint {
                            program: prog_ptr,
                            kind,
                            name: CString::new(ep_name.to_string()).unwrap_or_default(),
                        });
                    }
                }

                Box::into_raw(program)
            }
            Err(e) => {
                c.set_error(&e);
                ptr::null_mut()
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
    if ptr.is_null() { return 0; }
    (*ptr).globals_size
}

/// Get the number of global variables.
#[no_mangle]
pub unsafe extern "C" fn lyte_program_get_globals_count(ptr: *const LyteProgram) -> usize {
    if ptr.is_null() { return 0; }
    (*ptr).globals_info.len()
}

/// Get the name of a global variable by index.
#[no_mangle]
pub unsafe extern "C" fn lyte_program_get_global_name(
    ptr: *const LyteProgram,
    index: usize,
) -> *const c_char {
    if ptr.is_null() { return ptr::null(); }
    let p = &*ptr;
    p.globals_info.get(index).map_or(ptr::null(), |g| g.name.as_ptr())
}

/// Get the byte offset of a global variable.
#[no_mangle]
pub unsafe extern "C" fn lyte_program_get_global_offset(
    ptr: *const LyteProgram,
    index: usize,
) -> usize {
    if ptr.is_null() { return 0; }
    let p = &*ptr;
    p.globals_info.get(index).map_or(0, |g| g.offset)
}

/// Get the size in bytes of a global variable.
#[no_mangle]
pub unsafe extern "C" fn lyte_program_get_global_size(
    ptr: *const LyteProgram,
    index: usize,
) -> usize {
    if ptr.is_null() { return 0; }
    let p = &*ptr;
    p.globals_info.get(index).map_or(0, |g| g.size)
}

/// Get the type of a global variable as a string.
#[no_mangle]
pub unsafe extern "C" fn lyte_program_get_global_type(
    ptr: *const LyteProgram,
    index: usize,
) -> *const c_char {
    if ptr.is_null() { return ptr::null(); }
    let p = &*ptr;
    p.globals_info.get(index).map_or(ptr::null(), |g| g.ty.as_ptr())
}

/// Look up an entry point by name. Returns NULL if not found.
/// The returned handle is owned by the program; do NOT free it.
#[no_mangle]
pub unsafe extern "C" fn lyte_program_get_entry_point(
    ptr: *const LyteProgram,
    name: *const c_char,
) -> *const LyteEntryPoint {
    if ptr.is_null() || name.is_null() {
        return ptr::null();
    }
    let name_cstr = CStr::from_ptr(name);
    for ep in &(*ptr).entry_points {
        if ep.name.as_c_str() == name_cstr {
            return ep as *const LyteEntryPoint;
        }
    }
    ptr::null()
}

/// Call an entry point with an external globals buffer.
/// For JIT: direct function pointer call.
/// For VM: runs interpreter with cached linked bytecode.
/// Returns true on success, false if cancelled.
#[no_mangle]
pub unsafe extern "C" fn lyte_entry_point_call(
    ep: *const LyteEntryPoint,
    globals: *mut u8,
) -> bool {
    if ep.is_null() || globals.is_null() {
        return false;
    }
    let ep = &*ep;
    let program = &mut *ep.program;

    match ep.kind {
        EntryPoint::Jit(fn_addr) => {
            extern "C" {
                fn setjmp(env: *mut u8) -> i32;
            }
            type Entry = unsafe extern "C" fn(*mut u8, *mut u8);
            crate::jit::set_cancel_callback(
                globals,
                program.cancel_callback,
                program.cancel_userdata,
            );
            let jmp_buf_ptr = globals.add(crate::jit::JMPBUF_OFFSET);
            if setjmp(jmp_buf_ptr) == 0 {
                let code_fn: Entry = std::mem::transmute(fn_addr);
                code_fn(globals, ptr::null_mut());
                true
            } else {
                false // cancelled
            }
        }
        EntryPoint::Vm(func_idx) => {
            // Read cancel callback before borrowing inner mutably.
            let cancel_cb = program.cancel_callback;
            let cancel_ud = program.cancel_userdata;
            let gs = program.globals_size;

            match &mut program.inner {
                CompiledProgram::Vm {
                    program: ref vm_program,
                    ref linked,
                    ref mut vm,
                } => {
                    if let Some(cb) = cancel_cb {
                        vm.set_cancel_callback(Some(cb), cancel_ud);
                    }
                    vm.call_with_external_globals(
                        linked,
                        vm_program,
                        func_idx,
                        globals,
                        gs,
                    );
                    !vm.cancelled
                }
                #[cfg(feature = "llvm")]
                _ => false, // shouldn't happen
            }
        }
    }
}

/// Allocate a zeroed globals buffer of the correct size.
/// Caller must free with lyte_globals_free().
#[no_mangle]
pub unsafe extern "C" fn lyte_globals_alloc(ptr: *const LyteProgram) -> *mut u8 {
    if ptr.is_null() { return ptr::null_mut(); }
    let size = (*ptr).globals_size;
    if size == 0 { return ptr::null_mut(); }
    let layout = std::alloc::Layout::from_size_align(size, 16).unwrap();
    let mem = std::alloc::alloc_zeroed(layout);
    // Initialize cancel counter.
    if !mem.is_null() {
        *(mem as *mut i32) = crate::jit::CANCEL_CHECK_INTERVAL;
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
    if ptr.is_null() { return; }
    (*ptr).cancel_callback = callback;
    (*ptr).cancel_userdata = user_data;
}
