//! FFI bridge between Rust StackProgram and C stack interpreter.
//!
//! Converts StackOp instructions into the C Instruction format
//! (handler pointer + 3 immediates) and calls stack_interp_run.

use crate::stack_ir::{StackOp, StackProgram};

// C types matching stack_interp.h
#[repr(C)]
struct Instruction {
    handler: *const (),
    imm: [u64; 3],
}

#[repr(C)]
struct CallFrame {
    return_pc: *mut Instruction,
    saved_locals: *mut u64,
    saved_lm: *mut u8,
    saved_sp: *mut u64,
    func_idx: u32,
    saved_lm_size: usize,
}

#[repr(C)]
struct FuncMeta {
    code: *mut Instruction,
    local_count: u32,
    local_memory: u32,
    param_count: u32,
}

#[repr(C)]
struct Ctx {
    call_stack: *mut CallFrame,
    call_depth: u32,
    call_stack_cap: u32,
    functions: *mut FuncMeta,
    func_count: u32,
    globals: *mut u8,
    local_memory: *mut u8,
    local_memory_size: usize,
    local_memory_cap: usize,
    stack_base: *mut u64,
    closure_ptr: u64,
    result: i64,
    done: i32,
}

extern "C" {
    fn stack_interp_run(ctx: *mut Ctx, entry_func: u32) -> i64;

    // Handler function pointers (defined in stack_interp.c)
    fn op_i64_const();
    fn op_f32_const();
    fn op_f64_const();
    fn op_local_get();
    fn op_local_set();
    fn op_local_tee();
    fn op_local_addr();
    fn op_global_addr();
    fn op_iadd();
    fn op_isub();
    fn op_imul();
    fn op_idiv();
    fn op_udiv();
    fn op_irem();
    fn op_ipow();
    fn op_ineg();
    fn op_iadd_imm();
    fn op_fadd();
    fn op_fsub();
    fn op_fmul();
    fn op_fdiv();
    fn op_fpow();
    fn op_fneg();
    fn op_dadd();
    fn op_dsub();
    fn op_dmul();
    fn op_ddiv();
    fn op_dpow();
    fn op_dneg();
    fn op_ieq();
    fn op_ine();
    fn op_ilt();
    fn op_ile();
    fn op_igt();
    fn op_ige();
    fn op_ult();
    fn op_ugt();
    fn op_feq();
    fn op_fne();
    fn op_flt();
    fn op_fle();
    fn op_fgt();
    fn op_fge();
    fn op_deq();
    fn op_dlt();
    fn op_dle();
    fn op_and();
    fn op_or();
    fn op_xor();
    fn op_not();
    fn op_shl();
    fn op_shr();
    fn op_ushr();
    fn op_i32_to_f32();
    fn op_f32_to_i32();
    fn op_i32_to_f64();
    fn op_f64_to_i32();
    fn op_f32_to_f64();
    fn op_f64_to_f32();
    fn op_i32_to_i8();
    fn op_i8_to_i32();
    fn op_i64_to_u32();
    fn op_load8();
    fn op_load32();
    fn op_load64();
    fn op_load32_off();
    fn op_load64_off();
    fn op_store8();
    fn op_store32();
    fn op_store64();
    fn op_store8_off();
    fn op_store32_off();
    fn op_store64_off();
    fn op_memcopy();
    fn op_memzero();
    fn op_memeq();
    fn op_memne();
    fn op_slice_eq();
    fn op_slice_ne();
    fn op_slice_load32();
    fn op_slice_store32();
    fn op_jump();
    fn op_jump_if_zero();
    fn op_jump_if_not_zero();
    fn op_call();
    fn op_call_closure();
    fn op_call_indirect();
    fn op_return();
    fn op_return_void();
    fn op_drop();
    fn op_sin_f32();
    fn op_cos_f32();
    fn op_tan_f32();
    fn op_asin_f32();
    fn op_acos_f32();
    fn op_atan_f32();
    fn op_sinh_f32();
    fn op_cosh_f32();
    fn op_tanh_f32();
    fn op_asinh_f32();
    fn op_acosh_f32();
    fn op_atanh_f32();
    fn op_ln_f32();
    fn op_exp_f32();
    fn op_exp2_f32();
    fn op_log10_f32();
    fn op_log2_f32();
    fn op_sqrt_f32();
    fn op_abs_f32();
    fn op_floor_f32();
    fn op_ceil_f32();
    fn op_sin_f64();
    fn op_cos_f64();
    fn op_tan_f64();
    fn op_asin_f64();
    fn op_acos_f64();
    fn op_atan_f64();
    fn op_sinh_f64();
    fn op_cosh_f64();
    fn op_tanh_f64();
    fn op_asinh_f64();
    fn op_acosh_f64();
    fn op_atanh_f64();
    fn op_ln_f64();
    fn op_exp_f64();
    fn op_exp2_f64();
    fn op_log10_f64();
    fn op_log2_f64();
    fn op_sqrt_f64();
    fn op_abs_f64();
    fn op_floor_f64();
    fn op_ceil_f64();
    fn op_isnan_f32();
    fn op_isnan_f64();
    fn op_isinf_f32();
    fn op_isinf_f64();
    fn op_atan2_f32();
    fn op_atan2_f64();
    fn op_print_i32();
    fn op_print_f32();
    fn op_putc();
    fn op_assert();
    fn op_get_closure_ptr();
    fn op_halt();
    fn op_nop();
}

/// Get the C handler function pointer for a StackOp.
fn handler_for(op: &StackOp) -> *const () {
    match op {
        StackOp::I64Const(_) => op_i64_const as *const (),
        StackOp::F32Const(_) => op_f32_const as *const (),
        StackOp::F64Const(_) => op_f64_const as *const (),
        StackOp::LocalGet(_) => op_local_get as *const (),
        StackOp::LocalSet(_) => op_local_set as *const (),
        StackOp::LocalTee(_) => op_local_tee as *const (),
        StackOp::LocalAddr(_) => op_local_addr as *const (),
        StackOp::GlobalAddr(_) => op_global_addr as *const (),
        StackOp::IAdd => op_iadd as *const (),
        StackOp::ISub => op_isub as *const (),
        StackOp::IMul => op_imul as *const (),
        StackOp::IDiv => op_idiv as *const (),
        StackOp::UDiv => op_udiv as *const (),
        StackOp::IRem => op_irem as *const (),
        StackOp::IPow => op_ipow as *const (),
        StackOp::INeg => op_ineg as *const (),
        StackOp::IAddImm(_) => op_iadd_imm as *const (),
        StackOp::FAdd => op_fadd as *const (),
        StackOp::FSub => op_fsub as *const (),
        StackOp::FMul => op_fmul as *const (),
        StackOp::FDiv => op_fdiv as *const (),
        StackOp::FPow => op_fpow as *const (),
        StackOp::FNeg => op_fneg as *const (),
        StackOp::DAdd => op_dadd as *const (),
        StackOp::DSub => op_dsub as *const (),
        StackOp::DMul => op_dmul as *const (),
        StackOp::DDiv => op_ddiv as *const (),
        StackOp::DPow => op_dpow as *const (),
        StackOp::DNeg => op_dneg as *const (),
        StackOp::IEq => op_ieq as *const (),
        StackOp::INe => op_ine as *const (),
        StackOp::ILt => op_ilt as *const (),
        StackOp::ILe => op_ile as *const (),
        StackOp::IGt => op_igt as *const (),
        StackOp::IGe => op_ige as *const (),
        StackOp::ULt => op_ult as *const (),
        StackOp::UGt => op_ugt as *const (),
        StackOp::FEq => op_feq as *const (),
        StackOp::FNe => op_fne as *const (),
        StackOp::FLt => op_flt as *const (),
        StackOp::FLe => op_fle as *const (),
        StackOp::FGt => op_fgt as *const (),
        StackOp::FGe => op_fge as *const (),
        StackOp::DEq => op_deq as *const (),
        StackOp::DLt => op_dlt as *const (),
        StackOp::DLe => op_dle as *const (),
        StackOp::And => op_and as *const (),
        StackOp::Or => op_or as *const (),
        StackOp::Xor => op_xor as *const (),
        StackOp::Not => op_not as *const (),
        StackOp::Shl => op_shl as *const (),
        StackOp::Shr => op_shr as *const (),
        StackOp::UShr => op_ushr as *const (),
        StackOp::I32ToF32 => op_i32_to_f32 as *const (),
        StackOp::F32ToI32 => op_f32_to_i32 as *const (),
        StackOp::I32ToF64 => op_i32_to_f64 as *const (),
        StackOp::F64ToI32 => op_f64_to_i32 as *const (),
        StackOp::F32ToF64 => op_f32_to_f64 as *const (),
        StackOp::F64ToF32 => op_f64_to_f32 as *const (),
        StackOp::I32ToI8 => op_i32_to_i8 as *const (),
        StackOp::I8ToI32 => op_i8_to_i32 as *const (),
        StackOp::I64ToU32 => op_i64_to_u32 as *const (),
        StackOp::Load8 => op_load8 as *const (),
        StackOp::Load32 => op_load32 as *const (),
        StackOp::Load64 => op_load64 as *const (),
        StackOp::Load32Off(_) => op_load32_off as *const (),
        StackOp::Load64Off(_) => op_load64_off as *const (),
        StackOp::Store8 => op_store8 as *const (),
        StackOp::Store32 => op_store32 as *const (),
        StackOp::Store64 => op_store64 as *const (),
        StackOp::Store8Off(_) => op_store8_off as *const (),
        StackOp::Store32Off(_) => op_store32_off as *const (),
        StackOp::Store64Off(_) => op_store64_off as *const (),
        StackOp::MemCopy(_) => op_memcopy as *const (),
        StackOp::MemZero(_) => op_memzero as *const (),
        StackOp::MemEq(_) => op_memeq as *const (),
        StackOp::MemNe(_) => op_memne as *const (),
        StackOp::SliceEq(_) => op_slice_eq as *const (),
        StackOp::SliceNe(_) => op_slice_ne as *const (),
        StackOp::SliceLoad32 => op_slice_load32 as *const (),
        StackOp::SliceStore32 => op_slice_store32 as *const (),
        StackOp::Jump(_) => op_jump as *const (),
        StackOp::JumpIfZero(_) => op_jump_if_zero as *const (),
        StackOp::JumpIfNotZero(_) => op_jump_if_not_zero as *const (),
        StackOp::Call { .. } => op_call as *const (),
        StackOp::CallIndirect { .. } => op_call_indirect as *const (),
        StackOp::CallClosure { .. } => op_call_closure as *const (),
        StackOp::Return => op_return as *const (),
        StackOp::ReturnVoid => op_return_void as *const (),
        StackOp::Drop => op_drop as *const (),
        StackOp::SinF32 => op_sin_f32 as *const (),
        StackOp::CosF32 => op_cos_f32 as *const (),
        StackOp::TanF32 => op_tan_f32 as *const (),
        StackOp::AsinF32 => op_asin_f32 as *const (),
        StackOp::AcosF32 => op_acos_f32 as *const (),
        StackOp::AtanF32 => op_atan_f32 as *const (),
        StackOp::SinhF32 => op_sinh_f32 as *const (),
        StackOp::CoshF32 => op_cosh_f32 as *const (),
        StackOp::TanhF32 => op_tanh_f32 as *const (),
        StackOp::AsinhF32 => op_asinh_f32 as *const (),
        StackOp::AcoshF32 => op_acosh_f32 as *const (),
        StackOp::AtanhF32 => op_atanh_f32 as *const (),
        StackOp::LnF32 => op_ln_f32 as *const (),
        StackOp::ExpF32 => op_exp_f32 as *const (),
        StackOp::Exp2F32 => op_exp2_f32 as *const (),
        StackOp::Log10F32 => op_log10_f32 as *const (),
        StackOp::Log2F32 => op_log2_f32 as *const (),
        StackOp::SqrtF32 => op_sqrt_f32 as *const (),
        StackOp::AbsF32 => op_abs_f32 as *const (),
        StackOp::FloorF32 => op_floor_f32 as *const (),
        StackOp::CeilF32 => op_ceil_f32 as *const (),
        StackOp::SinF64 => op_sin_f64 as *const (),
        StackOp::CosF64 => op_cos_f64 as *const (),
        StackOp::TanF64 => op_tan_f64 as *const (),
        StackOp::AsinF64 => op_asin_f64 as *const (),
        StackOp::AcosF64 => op_acos_f64 as *const (),
        StackOp::AtanF64 => op_atan_f64 as *const (),
        StackOp::SinhF64 => op_sinh_f64 as *const (),
        StackOp::CoshF64 => op_cosh_f64 as *const (),
        StackOp::TanhF64 => op_tanh_f64 as *const (),
        StackOp::AsinhF64 => op_asinh_f64 as *const (),
        StackOp::AcoshF64 => op_acosh_f64 as *const (),
        StackOp::AtanhF64 => op_atanh_f64 as *const (),
        StackOp::LnF64 => op_ln_f64 as *const (),
        StackOp::ExpF64 => op_exp_f64 as *const (),
        StackOp::Exp2F64 => op_exp2_f64 as *const (),
        StackOp::Log10F64 => op_log10_f64 as *const (),
        StackOp::Log2F64 => op_log2_f64 as *const (),
        StackOp::SqrtF64 => op_sqrt_f64 as *const (),
        StackOp::AbsF64 => op_abs_f64 as *const (),
        StackOp::FloorF64 => op_floor_f64 as *const (),
        StackOp::CeilF64 => op_ceil_f64 as *const (),
        StackOp::IsnanF32 => op_isnan_f32 as *const (),
        StackOp::IsnanF64 => op_isnan_f64 as *const (),
        StackOp::IsinfF32 => op_isinf_f32 as *const (),
        StackOp::IsinfF64 => op_isinf_f64 as *const (),
        StackOp::Atan2F32 => op_atan2_f32 as *const (),
        StackOp::Atan2F64 => op_atan2_f64 as *const (),
        StackOp::PrintI32 => op_print_i32 as *const (),
        StackOp::PrintF32 => op_print_f32 as *const (),
        StackOp::Putc => op_putc as *const (),
        StackOp::Assert => op_assert as *const (),
        StackOp::GetClosurePtr => op_get_closure_ptr as *const (),
        StackOp::Halt => op_halt as *const (),
        StackOp::Nop => op_nop as *const (),
    }
}

/// Encode a StackOp's immediates into the instruction's imm slots.
fn encode_imm(op: &StackOp, func_idx: u32) -> [u64; 3] {
    match op {
        StackOp::I64Const(v) => [*v as u64, 0, 0],
        StackOp::F32Const(v) => [f32::to_bits(*v) as u64, 0, 0],
        StackOp::F64Const(v) => [f64::to_bits(*v), 0, 0],
        StackOp::LocalGet(n) | StackOp::LocalSet(n) | StackOp::LocalTee(n) | StackOp::LocalAddr(n) => {
            [*n as u64, 0, 0]
        }
        StackOp::GlobalAddr(off) => [*off as u64, 0, 0],
        StackOp::IAddImm(v) => [*v as i64 as u64, 0, 0],
        StackOp::Load32Off(off) | StackOp::Load64Off(off) => [*off as u64, 0, 0],
        StackOp::Store8Off(off) | StackOp::Store32Off(off) | StackOp::Store64Off(off) => [*off as u64, 0, 0],
        StackOp::MemCopy(n) | StackOp::MemZero(n) | StackOp::MemEq(n) | StackOp::MemNe(n) => {
            [*n as u64, 0, 0]
        }
        StackOp::SliceEq(n) | StackOp::SliceNe(n) => [*n as u64, 0, 0],
        StackOp::Jump(off) | StackOp::JumpIfZero(off) | StackOp::JumpIfNotZero(off) => {
            [*off as i64 as u64, 0, 0]
        }
        // Call: imm[0]=func_idx, imm[1]=arg_count, imm[2]=current_func_idx (for call frame)
        StackOp::Call { func, args } => [*func as u64, *args as u64, func_idx as u64],
        StackOp::CallIndirect { args } => [*args as u64, func_idx as u64, 0],
        StackOp::CallClosure { args } => [*args as u64, func_idx as u64, 0],
        _ => [0, 0, 0],
    }
}

/// Convert a StackProgram to C instruction format and run it.
pub fn run(program: &StackProgram) -> i64 {
    // Convert each function's ops to C Instruction arrays.
    let mut c_instructions: Vec<Vec<Instruction>> = Vec::new();

    for (fi, func) in program.functions.iter().enumerate() {
        let mut instrs: Vec<Instruction> = Vec::with_capacity(func.ops.len());
        for op in &func.ops {
            instrs.push(Instruction {
                handler: handler_for(op),
                imm: encode_imm(op, fi as u32),
            });
        }
        // Sentinel instruction at end (halt handler).
        instrs.push(Instruction {
            handler: op_halt as *const (),
            imm: [0, 0, 0],
        });
        c_instructions.push(instrs);
    }

    // Build FuncMeta array.
    let mut func_metas: Vec<FuncMeta> = Vec::new();
    for (i, func) in program.functions.iter().enumerate() {
        func_metas.push(FuncMeta {
            code: c_instructions[i].as_mut_ptr(),
            local_count: func.local_count as u32,
            local_memory: func.local_memory,
            param_count: func.param_count as u32,
        });
    }

    // Allocate call stack.
    let mut call_stack: Vec<CallFrame> = (0..4096)
        .map(|_| CallFrame {
            return_pc: std::ptr::null_mut(),
            saved_locals: std::ptr::null_mut(),
            saved_lm: std::ptr::null_mut(),
            saved_sp: std::ptr::null_mut(),
            func_idx: 0,
            saved_lm_size: 0,
        })
        .collect();

    // Allocate globals.
    let mut globals: Vec<u8> = vec![0u8; program.globals_size];

    // Build context.
    let mut ctx = Ctx {
        call_stack: call_stack.as_mut_ptr(),
        call_depth: 0,
        call_stack_cap: 4096,
        functions: func_metas.as_mut_ptr(),
        func_count: func_metas.len() as u32,
        globals: globals.as_mut_ptr(),
        local_memory: std::ptr::null_mut(),
        local_memory_size: 0,
        local_memory_cap: 0,
        stack_base: std::ptr::null_mut(),
        closure_ptr: 0,
        result: 0,
        done: 0,
    };

    let result = unsafe { stack_interp_run(&mut ctx, program.entry) };

    // Clean up local memory allocated by C.
    if !ctx.local_memory.is_null() {
        unsafe { libc::free(ctx.local_memory as *mut libc::c_void) };
    }

    result
}
