// ARM64 assembly dispatch for the 16-bit VM.
// Only compiled on aarch64 targets.
//
// This is an MVP that uses a 256-entry jump table with register-array access
// (same as the 32-bit VM ARM64 backend, but for 16-bit instruction format).
// Future: pin VM registers to CPU registers for zero-load-store hot loops.

#![allow(dead_code)]

use crate::opcode::FuncIdx;
use crate::vm::{VMProgram, ExternFuncInfo, println_output, print_output};
use crate::vm16::VM16;
use crate::vm16_link::LinkedProgram16;
use crate::vm16_lower;
use std::arch::global_asm;

// Include the ARM64 assembly dispatch loop.
#[cfg(target_os = "macos")]
global_asm!(include_str!("vm16_arm64.S"));

// Context struct shared between Rust and ARM64 assembly.
// Field offsets are hardcoded in the assembly — DO NOT reorder fields.
#[repr(C)]
pub struct Asm16Context {
    // === Hot state: loaded into pinned registers ===
    pub ops: *const u16,          // 0:   -> x19  (instruction stream)
    pub regs: *mut u64,           // 8:   -> x20  (VM register array, 16 entries)
    pub ip: u64,                  // 16:  -> x21  (absolute word index)
    pub locals_base: u64,         // 24:  -> x22
    pub locals_ptr: *mut u8,      // 32:  -> x23
    pub globals_ptr: *mut u8,     // 40:  -> x26

    // === Function table ===
    pub func_offsets: *const u64, // 48:
    pub func_locals: *const u32,  // 56:

    // === Constant pools ===
    pub f32_pool: *const f32,     // 64:
    pub f64_pool: *const f64,     // 72:
    pub i64_pool: *const i64,     // 80:

    // === VM state ===
    pub closure_ptr: u64,         // 88:

    // === Call stack ===
    pub call_stack: *mut Asm16CallFrame, // 96:
    pub call_stack_len: u64,      // 104:
    pub current_func: u64,        // 112:

    // === Locals capacity ===
    pub locals_cap: u64,          // 120:

    // === Helper function pointers (called from assembly) ===
    pub fn_print_i32: unsafe extern "C" fn(i64),       // 128:
    pub fn_print_f32: unsafe extern "C" fn(u32),       // 136:
    pub fn_assert: unsafe extern "C" fn(u64) -> u64,   // 144:
    pub fn_putc: unsafe extern "C" fn(u64),             // 152:

    // === Extern function support ===
    pub extern_funcs_ptr: *const ExternFuncInfo, // 160:
    pub extern_funcs_len: u64,                   // 168:
    pub fn_call_extern: unsafe extern "C" fn(*mut Asm16Context, u64, u64), // 176:
}

// Call frame for the assembly VM16.
#[derive(Clone)]
#[repr(C)]
pub struct Asm16CallFrame {
    pub func_idx: u64,
    pub ip: u64,
    pub locals_base: u64,
    pub saved_regs: [u64; 16], // saved caller registers
}

// Verify critical offsets at compile time.
const _: () = {
    assert!(std::mem::offset_of!(Asm16Context, ops) == 0);
    assert!(std::mem::offset_of!(Asm16Context, regs) == 8);
    assert!(std::mem::offset_of!(Asm16Context, ip) == 16);
    assert!(std::mem::offset_of!(Asm16Context, locals_base) == 24);
    assert!(std::mem::offset_of!(Asm16Context, locals_ptr) == 32);
    assert!(std::mem::offset_of!(Asm16Context, globals_ptr) == 40);
    assert!(std::mem::offset_of!(Asm16Context, func_offsets) == 48);
    assert!(std::mem::offset_of!(Asm16Context, func_locals) == 56);
    assert!(std::mem::offset_of!(Asm16Context, f32_pool) == 64);
    assert!(std::mem::offset_of!(Asm16Context, f64_pool) == 72);
    assert!(std::mem::offset_of!(Asm16Context, i64_pool) == 80);
    assert!(std::mem::offset_of!(Asm16Context, closure_ptr) == 88);
    assert!(std::mem::offset_of!(Asm16Context, call_stack) == 96);
    assert!(std::mem::offset_of!(Asm16Context, call_stack_len) == 104);
    assert!(std::mem::offset_of!(Asm16Context, current_func) == 112);
    assert!(std::mem::offset_of!(Asm16Context, locals_cap) == 120);
    assert!(std::mem::offset_of!(Asm16Context, fn_print_i32) == 128);
    assert!(std::mem::offset_of!(Asm16Context, fn_print_f32) == 136);
    assert!(std::mem::offset_of!(Asm16Context, fn_assert) == 144);
    assert!(std::mem::offset_of!(Asm16Context, fn_putc) == 152);
    assert!(std::mem::offset_of!(Asm16Context, extern_funcs_ptr) == 160);
    assert!(std::mem::offset_of!(Asm16Context, extern_funcs_len) == 168);
    assert!(std::mem::offset_of!(Asm16Context, fn_call_extern) == 176);
};

extern "C" {
    fn vm16_arm64_enter(ctx: *mut Asm16Context) -> i64;
}

// Helper functions called from assembly.

unsafe extern "C" fn helper_print_i32(val: i64) {
    println_output(&format!("{}", val as i32));
}

unsafe extern "C" fn helper_print_f32(bits: u32) {
    println_output(&format!("{}", f32::from_bits(bits)));
}

unsafe extern "C" fn helper_assert(val: u64) -> u64 {
    if val != 0 {
        println_output("assert(true)");
        0
    } else {
        println_output("assert(false)");
        1
    }
}

unsafe extern "C" fn helper_putc(val: u64) {
    if let Some(c) = char::from_u32(val as u32) {
        print_output(&format!("{}", c));
    }
}

unsafe extern "C" fn helper_call_extern(ctx: *mut Asm16Context, arg_count: u64, globals_offset: u64) {
    let ctx = &mut *ctx;
    let slot = ctx.globals_ptr.add(globals_offset as usize) as *const u64;
    let fn_ptr = *slot as usize;
    let context = *slot.add(1) as *mut u8;

    if fn_ptr == 0 {
        panic!("called unbound extern function (globals offset {})", globals_offset);
    }

    // Find ExternFuncInfo.
    let extern_funcs = std::slice::from_raw_parts(ctx.extern_funcs_ptr, ctx.extern_funcs_len as usize);
    let info = extern_funcs.iter()
        .find(|e| e.globals_offset == globals_offset as i32)
        .expect("no ExternFuncInfo for extern call");

    let regs = std::slice::from_raw_parts(ctx.regs, 16);
    let result = crate::vm::call_extern_fn(
        fn_ptr,
        context,
        regs,
        0,
        &info.param_types,
        info.ret_type,
    );

    // Store result in r0.
    *ctx.regs = result;
}

impl VM16 {
    /// Run using the ARM64 assembly dispatch loop.
    pub fn run_asm(
        &mut self,
        program: &VMProgram,
        func_idx: FuncIdx,
        args: &[i64],
    ) -> i64 {
        let functions: Vec<_> = program
            .functions
            .iter()
            .map(|f| (f.code.clone(), f.param_count, f.locals_size))
            .collect();

        let lowered = vm16_lower::lower_program(&functions);
        let linked = LinkedProgram16::from_lowered(&lowered);

        if self.globals.is_empty() && program.globals_size > 0 {
            self.globals = vec![0u8; program.globals_size];
        }

        self.run_asm_linked(&linked, program, func_idx, args)
    }

    pub fn run_asm_linked(
        &mut self,
        linked: &LinkedProgram16,
        program: &VMProgram,
        func_idx: FuncIdx,
        args: &[i64],
    ) -> i64 {
        self.regs = [0u64; 16];
        for (i, &arg) in args.iter().enumerate().take(16) {
            self.regs[i] = arg as u64;
        }

        // Pre-allocate locals.
        let total_locals: usize = linked.func_locals.iter().map(|&s| s as usize).sum::<usize>()
            .max(1024 * 64);
        if total_locals > self.locals.len() {
            self.locals.resize(total_locals, 0);
        }

        // Convert func_offsets to u64 for the assembly.
        let func_offsets: Vec<u64> = linked.func_offsets.iter().map(|&o| o as u64).collect();

        // Pre-allocate call stack.
        let max_call_depth = 4096;
        let mut call_stack = vec![Asm16CallFrame {
            func_idx: 0,
            ip: 0,
            locals_base: 0,
            saved_regs: [0u64; 16],
        }; max_call_depth];

        let mut ctx = Asm16Context {
            ops: linked.ops.as_ptr(),
            regs: self.regs.as_mut_ptr(),
            ip: linked.func_offsets[func_idx as usize] as u64,
            locals_base: 0,
            locals_ptr: self.locals.as_mut_ptr(),
            globals_ptr: self.globals.as_mut_ptr(),
            func_offsets: func_offsets.as_ptr(),
            func_locals: linked.func_locals.as_ptr(),
            f32_pool: linked.f32_pool.as_ptr(),
            f64_pool: linked.f64_pool.as_ptr(),
            i64_pool: linked.i64_pool.as_ptr(),
            closure_ptr: 0,
            call_stack: call_stack.as_mut_ptr(),
            call_stack_len: 0,
            current_func: func_idx as u64,
            locals_cap: self.locals.len() as u64,
            fn_print_i32: helper_print_i32,
            fn_print_f32: helper_print_f32,
            fn_assert: helper_assert,
            fn_putc: helper_putc,
            extern_funcs_ptr: program.extern_funcs.as_ptr(),
            extern_funcs_len: program.extern_funcs.len() as u64,
            fn_call_extern: helper_call_extern,
        };

        let result = unsafe { vm16_arm64_enter(&mut ctx) };

        // Copy registers back.
        self.regs = unsafe { *(ctx.regs as *const [u64; 16]) };

        result
    }
}
