use std::collections::HashMap;
use std::ffi::{CStr, CString};
use std::os::raw::c_char;
use std::panic::{self, AssertUnwindSafe};
use std::ptr;

use crate::compiler::Compiler;
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
