use std::ffi::{CStr, CString};
use std::os::raw::c_char;
use std::ptr;

use crate::compiler::Compiler;

/// Opaque handle to a lyte compiler instance.
pub struct LyteCompiler {
    compiler: Compiler,
    last_error: Option<CString>,
    /// JIT result: code pointer and globals size.
    jit_result: Option<(*const u8, usize)>,
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
    }))
}

/// Free a compiler instance.
#[no_mangle]
pub unsafe extern "C" fn lyte_compiler_free(ptr: *mut LyteCompiler) {
    if !ptr.is_null() {
        drop(Box::from_raw(ptr));
    }
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
    let c = &mut *ptr;
    let source = match CStr::from_ptr(source).to_str() {
        Ok(s) => s,
        Err(_) => {
            c.set_error("invalid UTF-8 in source");
            return false;
        }
    };
    let filename = match CStr::from_ptr(filename).to_str() {
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
}

/// Type-check the parsed source. Returns true on success.
#[no_mangle]
pub unsafe extern "C" fn lyte_compiler_check(ptr: *mut LyteCompiler) -> bool {
    if ptr.is_null() {
        return false;
    }
    let c = &mut *ptr;
    c.last_error = None;
    if !c.compiler.check() {
        c.set_error("type check error");
        return false;
    }
    true
}

/// Monomorphize generics. Returns true on success.
#[no_mangle]
pub unsafe extern "C" fn lyte_compiler_specialize(ptr: *mut LyteCompiler) -> bool {
    if ptr.is_null() {
        return false;
    }
    let c = &mut *ptr;
    c.last_error = None;
    if !c.compiler.specialize() {
        c.set_error("specialization error");
        return false;
    }
    true
}

/// JIT compile. Returns true on success.
/// On success, use lyte_compiler_get_code_ptr and lyte_compiler_get_globals_size.
#[no_mangle]
pub unsafe extern "C" fn lyte_compiler_jit(ptr: *mut LyteCompiler) -> bool {
    if ptr.is_null() {
        return false;
    }
    let c = &mut *ptr;
    c.last_error = None;
    c.jit_result = None;
    match c.compiler.jit() {
        Ok(result) => {
            c.jit_result = Some(result);
            true
        }
        Err(msg) => {
            c.set_error(&msg);
            false
        }
    }
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
        Some((code_ptr, _)) => code_ptr,
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
        Some((_, size)) => size,
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
