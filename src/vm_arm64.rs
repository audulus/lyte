// ARM64 assembly VM interpreter with register-pinned dispatch loop.
// Only compiled on aarch64 targets.

#![allow(dead_code)]

use crate::vm::{LinkedProgram, VMProgram, VM};
use std::arch::global_asm;

// Include the ARM64 assembly dispatch loop.
// On macOS/iOS, C symbols get underscore prefix; on Linux they don't.
#[cfg(any(target_os = "macos", target_os = "ios"))]
global_asm!(include_str!("vm_arm64.S"));

#[cfg(target_os = "linux")]
global_asm!(include_str!("vm_arm64_linux.S")); // future: linux variant

// ============ Context struct passed to assembly ============
// Field offsets are hardcoded in the assembly, so DO NOT reorder fields.

/// Call frame for the assembly VM (fixed layout, no padding surprises)
#[derive(Clone)]
#[repr(C)]
pub struct AsmCallFrame {
    pub func_idx: u64,
    pub ip: u64,
    pub locals_base: u64,
}

/// Context struct shared between Rust and ARM64 assembly.
/// The assembly function reads/writes these fields at known offsets.
#[repr(C)]
pub struct AsmContext {
    // === Hot state: loaded into pinned registers ===
    pub ops: *const u32,              // 0:   -> x19
    pub regs: *mut u64,               // 8:   -> x20
    pub ip: u64,                      // 16:  -> x21
    pub locals_base: u64,             // 24:  -> x22
    pub locals_ptr: *mut u8,          // 32:  -> x23
    pub globals_ptr: *mut u8,         // 40:  -> x26
    pub func_offsets: *const u64,     // 48:  (usize = u64 on 64-bit)
    pub func_locals: *const u32,      // 56:

    // === Constant pools ===
    pub wide_i64: *const i64,         // 64:
    pub wide_f64: *const f64,         // 72:
    pub f32_pool: *const f32,         // 80:
    pub constants: *const u64,        // 88:

    // === VM state ===
    pub cancelled_ptr: *mut u8,       // 96:
    pub closure_ptr: u64,             // 104:

    // === Call stack (pre-allocated fixed-size buffer) ===
    pub call_stack: *mut AsmCallFrame, // 112:
    pub call_stack_len: u64,           // 120:

    // === Current function index ===
    pub current_func: u64,            // 128:

    // === Locals capacity (for resize check) ===
    pub locals_cap: u64,              // 136:

    // === Helper function pointers (called from assembly) ===
    pub fn_print_i32: unsafe extern "C" fn(i64),         // 144:
    pub fn_print_f32: unsafe extern "C" fn(u32),         // 152:
    pub fn_assert: unsafe extern "C" fn(u64, u64) -> u64, // 160: (val, ip) -> 0=ok, 1=fail
    pub fn_putc: unsafe extern "C" fn(u64),              // 168:
    pub fn_grow_locals: unsafe extern "C" fn(*mut AsmContext, u64), // 176: (ctx, needed)
}

// Verify critical offsets with compile-time assertions
const _: () = {
    assert!(std::mem::offset_of!(AsmContext, ops) == 0);
    assert!(std::mem::offset_of!(AsmContext, regs) == 8);
    assert!(std::mem::offset_of!(AsmContext, ip) == 16);
    assert!(std::mem::offset_of!(AsmContext, locals_base) == 24);
    assert!(std::mem::offset_of!(AsmContext, locals_ptr) == 32);
    assert!(std::mem::offset_of!(AsmContext, globals_ptr) == 40);
    assert!(std::mem::offset_of!(AsmContext, func_offsets) == 48);
    assert!(std::mem::offset_of!(AsmContext, func_locals) == 56);
    assert!(std::mem::offset_of!(AsmContext, wide_i64) == 64);
    assert!(std::mem::offset_of!(AsmContext, wide_f64) == 72);
    assert!(std::mem::offset_of!(AsmContext, f32_pool) == 80);
    assert!(std::mem::offset_of!(AsmContext, constants) == 88);
    assert!(std::mem::offset_of!(AsmContext, cancelled_ptr) == 96);
    assert!(std::mem::offset_of!(AsmContext, closure_ptr) == 104);
    assert!(std::mem::offset_of!(AsmContext, call_stack) == 112);
    assert!(std::mem::offset_of!(AsmContext, call_stack_len) == 120);
    assert!(std::mem::offset_of!(AsmContext, current_func) == 128);
    assert!(std::mem::offset_of!(AsmContext, locals_cap) == 136);
    assert!(std::mem::offset_of!(AsmContext, fn_print_i32) == 144);
    assert!(std::mem::offset_of!(AsmContext, fn_print_f32) == 152);
    assert!(std::mem::offset_of!(AsmContext, fn_assert) == 160);
    assert!(std::mem::offset_of!(AsmContext, fn_putc) == 168);
    assert!(std::mem::offset_of!(AsmContext, fn_grow_locals) == 176);
};

// ============ Assembly entry point ============

extern "C" {
    /// ARM64 assembly dispatch loop. Returns the value in register 0 (i64).
    /// The context struct is read/written during execution.
    fn vm_arm64_enter(ctx: *mut AsmContext) -> i64;
}

// ============ Helper functions called from assembly ============
// These use extern "C" so the assembly can call them via `blr`.
// Since our pinned state is in callee-saved registers (x19-x26),
// it survives across these calls automatically.

unsafe extern "C" fn helper_print_i32(val: i64) {
    println!("{}", val as i32);
}

unsafe extern "C" fn helper_print_f32(bits: u32) {
    let v = f32::from_bits(bits);
    println!("{}", v);
}

unsafe extern "C" fn helper_assert(val: u64, ip: u64) -> u64 {
    if val != 0 {
        println!("assert(true)");
        0
    } else {
        println!("assert(false)");
        eprintln!("Assertion failed at instruction {}", ip);
        1 // signal failure
    }
}

unsafe extern "C" fn helper_putc(val: u64) {
    if let Some(c) = char::from_u32(val as u32) {
        print!("{}", c);
    }
}

unsafe extern "C" fn helper_grow_locals(ctx: *mut AsmContext, needed: u64) {
    // We need to grow the locals buffer. The caller (assembly) has already
    // stored locals_base and ip back into the context.
    // We can't directly resize the Vec from here since we don't own it.
    // Instead, we'll use a static thread-local to communicate with run_asm.
    // For now, just panic — the pre-allocated 1MB should be enough for tests.
    panic!(
        "vm_arm64: locals overflow (needed {} bytes, have {})",
        needed,
        (*ctx).locals_cap
    );
}

// ============ Maximum call stack depth ============
const MAX_CALL_DEPTH: usize = 4096;

// ============ VM::run_asm implementation ============

impl VM {
    /// Run a program using the ARM64 assembly interpreter.
    /// This provides the same semantics as `run()` but with a hand-written
    /// dispatch loop that pins VM state in callee-saved registers.
    pub fn run_asm(&mut self, program: &VMProgram) -> i64 {
        let linked = LinkedProgram::from_program(program);

        // Initialize VM state
        self.current_func = program.entry;
        self.registers = [0; 256];
        self.globals = vec![0u8; program.globals_size];
        self.cancelled = false;

        // Pre-allocate call stack
        let mut call_stack = Vec::with_capacity(MAX_CALL_DEPTH);
        call_stack.resize(MAX_CALL_DEPTH, AsmCallFrame {
            func_idx: 0,
            ip: 0,
            locals_base: 0,
        });

        let mut ctx = AsmContext {
            ops: linked.ops.as_ptr() as *const u32,
            regs: self.registers.as_mut_ptr(),
            ip: linked.func_offsets[program.entry as usize] as u64,
            locals_base: 0,
            locals_ptr: self.locals.as_mut_ptr(),
            globals_ptr: self.globals.as_mut_ptr(),
            func_offsets: linked.func_offsets.as_ptr() as *const u64,
            func_locals: linked.func_locals.as_ptr(),
            wide_i64: linked.wide_i64.as_ptr(),
            wide_f64: linked.wide_f64.as_ptr(),
            f32_pool: linked.f32_pool.as_ptr(),
            constants: program.constants.as_ptr(),
            cancelled_ptr: &mut self.cancelled as *mut bool as *mut u8,
            closure_ptr: 0,
            call_stack: call_stack.as_mut_ptr(),
            call_stack_len: 0,
            current_func: program.entry as u64,
            locals_cap: self.locals.len() as u64,
            fn_print_i32: helper_print_i32,
            fn_print_f32: helper_print_f32,
            fn_assert: helper_assert,
            fn_putc: helper_putc,
            fn_grow_locals: helper_grow_locals,
        };

        unsafe { vm_arm64_enter(&mut ctx) }
    }

    /// Run a specific function using the ARM64 assembly interpreter with an
    /// external globals buffer. Used by the FFI entry point API on iOS.
    pub unsafe fn call_with_external_globals_asm(
        &mut self,
        linked: &LinkedProgram,
        program: &VMProgram,
        func_idx: crate::vm::FuncIdx,
        external_globals: *mut u8,
        globals_size: usize,
    ) -> i64 {
        // Initialize VM state
        self.current_func = func_idx;
        self.registers = [0; 256];
        self.cancelled = false;

        // Pre-allocate call stack
        let mut call_stack = Vec::with_capacity(MAX_CALL_DEPTH);
        call_stack.resize(MAX_CALL_DEPTH, AsmCallFrame {
            func_idx: 0,
            ip: 0,
            locals_base: 0,
        });

        let mut ctx = AsmContext {
            ops: linked.ops.as_ptr() as *const u32,
            regs: self.registers.as_mut_ptr(),
            ip: linked.func_offsets[func_idx as usize] as u64,
            locals_base: 0,
            locals_ptr: self.locals.as_mut_ptr(),
            globals_ptr: external_globals,
            func_offsets: linked.func_offsets.as_ptr() as *const u64,
            func_locals: linked.func_locals.as_ptr(),
            wide_i64: linked.wide_i64.as_ptr(),
            wide_f64: linked.wide_f64.as_ptr(),
            f32_pool: linked.f32_pool.as_ptr(),
            constants: program.constants.as_ptr(),
            cancelled_ptr: &mut self.cancelled as *mut bool as *mut u8,
            closure_ptr: 0,
            call_stack: call_stack.as_mut_ptr(),
            call_stack_len: 0,
            current_func: func_idx as u64,
            locals_cap: self.locals.len() as u64,
            fn_print_i32: helper_print_i32,
            fn_print_f32: helper_print_f32,
            fn_assert: helper_assert,
            fn_putc: helper_putc,
            fn_grow_locals: helper_grow_locals,
        };

        vm_arm64_enter(&mut ctx)
    }
}
