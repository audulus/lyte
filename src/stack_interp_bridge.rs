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
    saved_sp: *mut u64,
    func_idx: u32,
    saved_frame_size: usize,
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
    frame_stack: *mut u64,
    frame_stack_size: usize,
    frame_stack_cap: usize,
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
    // Shallow variants (depth < 4)
    fn op_i64_const_s(); fn op_f32_const_s(); fn op_f64_const_s();
    fn op_local_get_s(); fn op_local_addr_s(); fn op_global_addr_s();
    fn op_get_closure_ptr_s();
    fn op_local_set_s(); fn op_drop_s();
    fn op_jump_if_zero_s(); fn op_jump_if_not_zero_s();
    fn op_iadd_s(); fn op_isub_s(); fn op_imul_s();
    fn op_fadd_s(); fn op_fsub_s(); fn op_fmul_s(); fn op_fdiv_s();
    fn op_dadd_s(); fn op_dsub_s(); fn op_dmul_s(); fn op_ddiv_s();
    fn op_ieq_s(); fn op_ine_s(); fn op_ilt_s(); fn op_ile_s();
    fn op_igt_s(); fn op_ige_s(); fn op_ult_s(); fn op_ugt_s();
    fn op_feq_s(); fn op_fne_s(); fn op_flt_s(); fn op_fle_s();
    fn op_fgt_s(); fn op_fge_s(); fn op_deq_s(); fn op_dlt_s(); fn op_dle_s();
    fn op_and_s(); fn op_or_s(); fn op_xor_s();
    fn op_shl_s(); fn op_shr_s(); fn op_ushr_s();
    fn op_store8_s(); fn op_store32_s(); fn op_store64_s();
    fn op_store8_off_s(); fn op_store32_off_s(); fn op_store64_off_s();
    fn op_memcopy_s();
    fn op_fused_get_get_fmul_s(); fn op_fused_get_get_fadd_s();
    fn op_fused_get_get_fsub_s(); fn op_fused_get_get_iadd_s();
    fn op_fused_get_get_ilt_s(); fn op_fused_addr_load32off_s();
    fn op_fused_addr_get_sload32_s();
    fn op_print_i32_s(); fn op_print_f32_s(); fn op_putc_s(); fn op_assert_s();
    fn op_memzero_s();
    fn op_fused_fmul_fadd_s(); fn op_fused_fmul_fsub_s();
    fn op_fused_get_get_fadd_set();
    fn op_fused_field_copy32();
    fn op_fused_f32const_fgt_jiz();
    fn op_fused_f32const_fgt_jiz_s();
    fn op_fused_addr_get_sstore32();
    fn op_fused_addr_get_sstore32_s();
    fn op_fused_tee_sstore32();
    fn op_fused_tee_sstore32_s();
    fn op_fused_get_set();
    fn op_fused_get_addr_fmul_fadd(); fn op_fused_get_addr_fmul_fsub();
    fn op_fused_addr_load32off_set(); fn op_fused_addr_imm_get_store32();
    fn op_fused_get_get_fmul();
    fn op_fused_get_get_fadd();
    fn op_fused_get_get_fsub();
    fn op_fused_get_get_iadd();
    fn op_fused_get_get_ilt();
    fn op_fused_get_fmul();
    fn op_fused_get_fadd();
    fn op_fused_get_fsub();
    fn op_fused_fmul_fadd();
    fn op_fused_fmul_fsub();
    fn op_fused_addr_load32off();
    fn op_fused_get_addimm_set();
    fn op_fused_get_get_ilt_jiz();
    fn op_fused_const_set();
    fn op_fused_f32const_set();
    fn op_fused_addr_get_sload32();
    // Hot local register handlers
    fn op_local_get_l0(); fn op_local_get_l1(); fn op_local_get_l2();
    fn op_local_set_l0(); fn op_local_set_l1(); fn op_local_set_l2();
    fn op_local_get_l0_s(); fn op_local_get_l1_s(); fn op_local_get_l2_s();
    fn op_local_set_l0_s(); fn op_local_set_l1_s(); fn op_local_set_l2_s();
    fn op_halt();
    fn op_nop();
}

/// Get the C handler function pointer for a StackOp.
/// If `shallow` is true and a shallow variant exists, return it.
fn handler_for(op: &StackOp, shallow: bool) -> *const () {
    if shallow && needs_shallow(op) {
        if let Some(h) = shallow_handler(op) {
            return h;
        }
        // Op needs a shallow variant but doesn't have one: fall through to deep.
        // This can underflow sp! Caller should only set shallow=true for ops
        // where a shallow variant exists. For safety, panic in debug builds.
        debug_assert!(false, "op {:?} at depth < 4 but no shallow variant", op);
    }
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
        StackOp::FusedGetGetFAddSet(_, _, _) => op_fused_get_get_fadd_set as *const (),
        StackOp::FusedFieldCopy32(_, _, _) => op_fused_field_copy32 as *const (),
        StackOp::FusedF32ConstFGtJumpIfZero(_, _) => op_fused_f32const_fgt_jiz as *const (),
        StackOp::FusedAddrGetSliceStore32(_, _) => op_fused_addr_get_sstore32 as *const (),
        StackOp::FusedTeeSliceStore32(_, _, _) => op_fused_tee_sstore32 as *const (),
        StackOp::FusedGetSet(_, _) => op_fused_get_set as *const (),
        StackOp::FusedGetAddrFMulFAdd(_, _, _) => op_fused_get_addr_fmul_fadd as *const (),
        StackOp::FusedGetAddrFMulFSub(_, _, _) => op_fused_get_addr_fmul_fsub as *const (),
        StackOp::FusedAddrLoad32OffSet(_, _, _) => op_fused_addr_load32off_set as *const (),
        StackOp::FusedAddrImmGetStore32(_, _, _) => op_fused_addr_imm_get_store32 as *const (),
        StackOp::FusedGetGetFMul(_, _) => op_fused_get_get_fmul as *const (),
        StackOp::FusedGetGetFAdd(_, _) => op_fused_get_get_fadd as *const (),
        StackOp::FusedGetGetFSub(_, _) => op_fused_get_get_fsub as *const (),
        StackOp::FusedGetGetIAdd(_, _) => op_fused_get_get_iadd as *const (),
        StackOp::FusedGetGetILt(_, _) => op_fused_get_get_ilt as *const (),
        StackOp::FusedGetFMul(_) => op_fused_get_fmul as *const (),
        StackOp::FusedGetFAdd(_) => op_fused_get_fadd as *const (),
        StackOp::FusedGetFSub(_) => op_fused_get_fsub as *const (),
        StackOp::FusedFMulFAdd => op_fused_fmul_fadd as *const (),
        StackOp::FusedFMulFSub => op_fused_fmul_fsub as *const (),
        StackOp::FusedAddrLoad32Off(_, _) => op_fused_addr_load32off as *const (),
        StackOp::FusedGetAddImmSet(_, _, _) => op_fused_get_addimm_set as *const (),
        StackOp::FusedGetGetILtJumpIfZero(_, _, _) => op_fused_get_get_ilt_jiz as *const (),
        StackOp::FusedConstSet(_, _) => op_fused_const_set as *const (),
        StackOp::FusedF32ConstSet(_, _) => op_fused_f32const_set as *const (),
        StackOp::FusedAddrGetSliceLoad32(_, _) => op_fused_addr_get_sload32 as *const (),
        StackOp::LocalGetL0 => op_local_get_l0 as *const (),
        StackOp::LocalGetL1 => op_local_get_l1 as *const (),
        StackOp::LocalGetL2 => op_local_get_l2 as *const (),
        StackOp::LocalSetL0 => op_local_set_l0 as *const (),
        StackOp::LocalSetL1 => op_local_set_l1 as *const (),
        StackOp::LocalSetL2 => op_local_set_l2 as *const (),
        StackOp::Halt => op_halt as *const (),
        StackOp::Nop => op_nop as *const (),
    }
}

/// Get the shallow handler variant for an op, if one exists.
fn shallow_handler(op: &StackOp) -> Option<*const ()> {
    Some(match op {
        // Push ops
        StackOp::I64Const(_) => op_i64_const_s as *const (),
        StackOp::F32Const(_) => op_f32_const_s as *const (),
        StackOp::F64Const(_) => op_f64_const_s as *const (),
        StackOp::LocalGet(_) => op_local_get_s as *const (),
        StackOp::LocalAddr(_) => op_local_addr_s as *const (),
        StackOp::GlobalAddr(_) => op_global_addr_s as *const (),
        StackOp::GetClosurePtr => op_get_closure_ptr_s as *const (),
        // Hot local push ops
        StackOp::LocalGetL0 => op_local_get_l0_s as *const (),
        StackOp::LocalGetL1 => op_local_get_l1_s as *const (),
        StackOp::LocalGetL2 => op_local_get_l2_s as *const (),
        // Pop ops
        StackOp::LocalSet(_) => op_local_set_s as *const (),
        StackOp::LocalSetL0 => op_local_set_l0_s as *const (),
        StackOp::LocalSetL1 => op_local_set_l1_s as *const (),
        StackOp::LocalSetL2 => op_local_set_l2_s as *const (),
        StackOp::Drop => op_drop_s as *const (),
        StackOp::PrintI32 => op_print_i32_s as *const (),
        StackOp::PrintF32 => op_print_f32_s as *const (),
        StackOp::Putc => op_putc_s as *const (),
        StackOp::Assert => op_assert_s as *const (),
        StackOp::MemZero(_) => op_memzero_s as *const (),
        StackOp::JumpIfZero(_) => op_jump_if_zero_s as *const (),
        StackOp::JumpIfNotZero(_) => op_jump_if_not_zero_s as *const (),
        // Binary ops
        StackOp::IAdd => op_iadd_s as *const (),
        StackOp::ISub => op_isub_s as *const (),
        StackOp::IMul => op_imul_s as *const (),
        StackOp::FAdd => op_fadd_s as *const (),
        StackOp::FSub => op_fsub_s as *const (),
        StackOp::FMul => op_fmul_s as *const (),
        StackOp::FDiv => op_fdiv_s as *const (),
        StackOp::DAdd => op_dadd_s as *const (),
        StackOp::DSub => op_dsub_s as *const (),
        StackOp::DMul => op_dmul_s as *const (),
        StackOp::DDiv => op_ddiv_s as *const (),
        StackOp::IEq => op_ieq_s as *const (),
        StackOp::INe => op_ine_s as *const (),
        StackOp::ILt => op_ilt_s as *const (),
        StackOp::ILe => op_ile_s as *const (),
        StackOp::IGt => op_igt_s as *const (),
        StackOp::IGe => op_ige_s as *const (),
        StackOp::ULt => op_ult_s as *const (),
        StackOp::UGt => op_ugt_s as *const (),
        StackOp::FEq => op_feq_s as *const (),
        StackOp::FNe => op_fne_s as *const (),
        StackOp::FLt => op_flt_s as *const (),
        StackOp::FLe => op_fle_s as *const (),
        StackOp::FGt => op_fgt_s as *const (),
        StackOp::FGe => op_fge_s as *const (),
        StackOp::DEq => op_deq_s as *const (),
        StackOp::DLt => op_dlt_s as *const (),
        StackOp::DLe => op_dle_s as *const (),
        StackOp::And => op_and_s as *const (),
        StackOp::Or => op_or_s as *const (),
        StackOp::Xor => op_xor_s as *const (),
        StackOp::Shl => op_shl_s as *const (),
        StackOp::Shr => op_shr_s as *const (),
        StackOp::UShr => op_ushr_s as *const (),
        // Store ops
        StackOp::Store8 => op_store8_s as *const (),
        StackOp::Store32 => op_store32_s as *const (),
        StackOp::Store64 => op_store64_s as *const (),
        StackOp::Store8Off(_) => op_store8_off_s as *const (),
        StackOp::Store32Off(_) => op_store32_off_s as *const (),
        StackOp::Store64Off(_) => op_store64_off_s as *const (),
        StackOp::MemCopy(_) => op_memcopy_s as *const (),
        // Fused push ops
        StackOp::FusedGetGetFMul(_, _) => op_fused_get_get_fmul_s as *const (),
        StackOp::FusedGetGetFAdd(_, _) => op_fused_get_get_fadd_s as *const (),
        StackOp::FusedGetGetFSub(_, _) => op_fused_get_get_fsub_s as *const (),
        StackOp::FusedGetGetIAdd(_, _) => op_fused_get_get_iadd_s as *const (),
        StackOp::FusedGetGetILt(_, _) => op_fused_get_get_ilt_s as *const (),
        StackOp::FusedAddrLoad32Off(_, _) => op_fused_addr_load32off_s as *const (),
        StackOp::FusedAddrGetSliceLoad32(_, _) => op_fused_addr_get_sload32_s as *const (),
        StackOp::FusedF32ConstFGtJumpIfZero(_, _) => op_fused_f32const_fgt_jiz_s as *const (),
        StackOp::FusedAddrGetSliceStore32(_, _) => op_fused_addr_get_sstore32_s as *const (),
        StackOp::FusedTeeSliceStore32(_, _, _) => op_fused_tee_sstore32_s as *const (),
        // Fused FMA ops
        StackOp::FusedFMulFAdd => op_fused_fmul_fadd_s as *const (),
        StackOp::FusedFMulFSub => op_fused_fmul_fsub_s as *const (),
        _ => return None,
    })
}

/// Check if an op accesses sp (needs a shallow variant at depth < 4).
/// Unary ops, jumps, and no-stack-change ops are safe at any depth.
fn needs_shallow(op: &StackOp) -> bool {
    // Ops that don't touch sp in their handler:
    matches!(shallow_handler(op), Some(_)) || matches!(op,
        // Unary (pure t0 transform, no sp access):
        StackOp::INeg | StackOp::FNeg | StackOp::DNeg | StackOp::Not |
        StackOp::IAddImm(_) |
        StackOp::I32ToF32 | StackOp::F32ToI32 | StackOp::I32ToF64 | StackOp::F64ToI32 |
        StackOp::F32ToF64 | StackOp::F64ToF32 | StackOp::I32ToI8 | StackOp::I8ToI32 |
        StackOp::I64ToU32 |
        StackOp::Load8 | StackOp::Load32 | StackOp::Load64 |
        StackOp::Load32Off(_) | StackOp::Load64Off(_) |
        StackOp::SinF32 | StackOp::CosF32 | StackOp::TanF32 |
        StackOp::AsinF32 | StackOp::AcosF32 | StackOp::AtanF32 |
        StackOp::SinhF32 | StackOp::CoshF32 | StackOp::TanhF32 |
        StackOp::AsinhF32 | StackOp::AcoshF32 | StackOp::AtanhF32 |
        StackOp::LnF32 | StackOp::ExpF32 | StackOp::Exp2F32 |
        StackOp::Log10F32 | StackOp::Log2F32 | StackOp::SqrtF32 |
        StackOp::AbsF32 | StackOp::FloorF32 | StackOp::CeilF32 |
        StackOp::SinF64 | StackOp::CosF64 | StackOp::TanF64 |
        StackOp::AsinF64 | StackOp::AcosF64 | StackOp::AtanF64 |
        StackOp::SinhF64 | StackOp::CoshF64 | StackOp::TanhF64 |
        StackOp::AsinhF64 | StackOp::AcoshF64 | StackOp::AtanhF64 |
        StackOp::LnF64 | StackOp::ExpF64 | StackOp::Exp2F64 |
        StackOp::Log10F64 | StackOp::Log2F64 | StackOp::SqrtF64 |
        StackOp::AbsF64 | StackOp::FloorF64 | StackOp::CeilF64 |
        StackOp::IsnanF32 | StackOp::IsnanF64 | StackOp::IsinfF32 | StackOp::IsinfF64 |
        StackOp::FusedGetFMul(_) | StackOp::FusedGetFAdd(_) | StackOp::FusedGetFSub(_) |
        // No stack change:
        StackOp::LocalTee(_) | StackOp::Jump(_) | StackOp::Nop | StackOp::Halt |
        StackOp::Return | StackOp::ReturnVoid |
        StackOp::FusedConstSet(_, _) | StackOp::FusedF32ConstSet(_, _) |
        StackOp::FusedGetAddImmSet(_, _, _) | StackOp::FusedGetGetILtJumpIfZero(_, _, _) |
        StackOp::FusedGetSet(_, _) | StackOp::FusedGetGetFAddSet(_, _, _) |
        StackOp::FusedFieldCopy32(_, _, _) |
        StackOp::FusedGetAddrFMulFAdd(_, _, _) | StackOp::FusedGetAddrFMulFSub(_, _, _) |
        StackOp::FusedAddrLoad32OffSet(_, _, _) | StackOp::FusedAddrImmGetStore32(_, _, _) |
        StackOp::Call { .. } | StackOp::CallIndirect { .. } | StackOp::CallClosure { .. }
    )
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
        // Fused instructions
        StackOp::FusedGetGetFMul(a, b) | StackOp::FusedGetGetFAdd(a, b)
        | StackOp::FusedGetGetFSub(a, b) | StackOp::FusedGetGetIAdd(a, b)
        | StackOp::FusedGetGetILt(a, b) | StackOp::FusedAddrGetSliceLoad32(a, b)
        | StackOp::FusedAddrGetSliceStore32(a, b) => {
            [*a as u64, *b as u64, 0]
        }
        StackOp::FusedGetFMul(a) | StackOp::FusedGetFAdd(a) | StackOp::FusedGetFSub(a) => {
            [*a as u64, 0, 0]
        }
        StackOp::FusedGetSet(a, b) => [*a as u64, *b as u64, 0],
        StackOp::FusedAddrLoad32Off(s, o) => [*s as u64, *o as i64 as u64, 0],
        StackOp::FusedGetAddrFMulFAdd(a, s, o) | StackOp::FusedGetAddrFMulFSub(a, s, o) => {
            [*a as u64, *s as u64, *o as i64 as u64]
        }
        StackOp::FusedAddrLoad32OffSet(s, o, d) => [*s as u64, *o as i64 as u64, *d as u64],
        StackOp::FusedAddrImmGetStore32(s, o, src) => [*s as u64, *o as i64 as u64, *src as u64],
        StackOp::FusedGetGetFAddSet(a, b, d) => [*a as u64, *b as u64, *d as u64],
        StackOp::FusedFieldCopy32(s, src, dst) => [*s as u64, *src as i64 as u64, *dst as i64 as u64],
        StackOp::FusedTeeSliceStore32(n, s, idx) => [*n as u64, *s as u64, *idx as u64],
        StackOp::FusedGetAddImmSet(s, v, d) => [*s as u64, *v as i64 as u64, *d as u64],
        StackOp::FusedGetGetILtJumpIfZero(a, b, off) => [*a as u64, *b as u64, *off as i64 as u64],
        StackOp::FusedF32ConstFGtJumpIfZero(v, off) => [f32::to_bits(*v) as u64, *off as i64 as u64, 0],
        StackOp::FusedConstSet(v, n) => [*v as u64, *n as u64, 0],
        StackOp::FusedF32ConstSet(v, n) => [f32::to_bits(*v) as u64, *n as u64, 0],
        _ => [0, 0, 0],
    }
}

/// Convert a StackProgram to C instruction format and run it.
pub fn run(program: &StackProgram) -> i64 {
    // Convert each function's ops to C Instruction arrays.
    let mut c_instructions: Vec<Vec<Instruction>> = Vec::new();

    for (fi, func) in program.functions.iter().enumerate() {
        let depths = crate::stack_depth::compute_depths(func);
        let mut instrs: Vec<Instruction> = Vec::with_capacity(func.ops.len());
        for (i, op) in func.ops.iter().enumerate() {
            let shallow = (depths[i] as u32) < 4 && needs_shallow(op);
            instrs.push(Instruction {
                handler: handler_for(op, shallow),
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
            saved_sp: std::ptr::null_mut(),
            func_idx: 0,
            saved_frame_size: 0,
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
        frame_stack: std::ptr::null_mut(),
        frame_stack_size: 0,
        frame_stack_cap: 0,
        stack_base: std::ptr::null_mut(),
        closure_ptr: 0,
        result: 0,
        done: 0,
    };

    let result = unsafe { stack_interp_run(&mut ctx, program.entry) };

    // Clean up buffers allocated by C.
    if !ctx.frame_stack.is_null() {
        unsafe { libc::free(ctx.frame_stack as *mut libc::c_void) };
    }

    result
}
