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
    float_stack: *mut f32,
    float_stack_cap: usize,
    current_locals: *mut u64,
    current_fsp: *mut f32,
    closure_ptr: u64,
    result: i64,
    done: i32,
    error: *const std::os::raw::c_char,
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
    fn op_isnan_f64();
    fn op_isinf_f64();
    fn op_atan2_f64();
    fn op_print_i32();
    fn op_putc();
    fn op_assert();
    fn op_get_closure_ptr();
    fn op_fused_get_get_fadd_set();
    fn op_fused_get_get_fsub_set();
    fn op_fused_get_get_fmul_set();
    fn op_fused_get_get_fdiv_set();
    fn op_fused_get_get_iadd_set();
    fn op_fused_get_get_isub_set();
    fn op_fused_get_get_imul_set();
    fn op_fused_field_copy32();
    fn op_fused_addr_get_sstore32();
    fn op_fused_local_array_load32();
    fn op_fused_local_array_store32();
    fn op_fused_tee_sstore32();
    fn op_fused_get_set();
    fn op_fused_addr_load32off_set(); fn op_fused_addr_imm_get_store32();
    fn op_fused_get_get_iadd();
    fn op_fused_get_get_ilt();
    fn op_fused_addr_load32off();
    fn op_fused_get_addimm_set();
    fn op_fused_get_get_ilt_jiz();
    fn op_fused_const_set();
    fn op_fused_f32const_set();
    fn op_fused_addr_get_sload32();
    // Hot local register handlers
    fn op_local_get_l0(); fn op_local_get_l1(); fn op_local_get_l2();
    fn op_local_set_l0(); fn op_local_set_l1(); fn op_local_set_l2();
    fn op_halt();
    fn op_nop();

    // === Float-window handlers (Phase 1+) ===
    fn op_f32_const_f();
    fn op_local_get_f();
    fn op_local_set_f();
    fn op_local_tee_f();
    fn op_drop_f();
    fn op_fadd_f(); fn op_fsub_f(); fn op_fmul_f(); fn op_fdiv_f();
    fn op_fpow_f(); fn op_fneg_f();
    fn op_feq_f(); fn op_fne_f(); fn op_flt_f(); fn op_fle_f();
    fn op_fgt_f(); fn op_fge_f();
    fn op_f32_to_i32_f(); fn op_i32_to_f32_f();
    fn op_to_bits_f(); fn op_from_bits_f();
    fn op_load_f32_f(); fn op_load_f32_off_f();
    fn op_store_f32_f(); fn op_store_f32_off_f();
    fn op_sin_f32_f(); fn op_cos_f32_f(); fn op_tan_f32_f();
    fn op_asin_f32_f(); fn op_acos_f32_f(); fn op_atan_f32_f();
    fn op_sinh_f32_f(); fn op_cosh_f32_f(); fn op_tanh_f32_f();
    fn op_asinh_f32_f(); fn op_acosh_f32_f(); fn op_atanh_f32_f();
    fn op_ln_f32_f(); fn op_exp_f32_f(); fn op_exp2_f32_f();
    fn op_log10_f32_f(); fn op_log2_f32_f();
    fn op_sqrt_f32_f(); fn op_abs_f32_f(); fn op_floor_f32_f(); fn op_ceil_f32_f();
    fn op_atan2_f32_f();
    fn op_isnan_f32_f(); fn op_isinf_f32_f();
    fn op_local_get_l0_f(); fn op_local_get_l1_f(); fn op_local_get_l2_f();
    fn op_local_set_l0_f(); fn op_local_set_l1_f(); fn op_local_set_l2_f();
    fn op_print_f32_f();
    fn op_fused_get_get_fadd_f(); fn op_fused_get_get_fsub_f(); fn op_fused_get_get_fmul_f();
    fn op_fused_get_fmul_f(); fn op_fused_get_fadd_f(); fn op_fused_get_fsub_f();
    fn op_fused_fmul_fadd_f(); fn op_fused_fmul_fsub_f();
    fn op_fused_get_addr_fmul_fadd_f(); fn op_fused_get_addr_fmul_fsub_f();
    fn op_fused_addr_load32off_f();
    fn op_fused_addr_get_sload32_f(); fn op_fused_addr_get_sstore32_f();
    fn op_fused_tee_sstore32_f();
    fn op_fused_local_array_load32_f(); fn op_fused_local_array_store32_f();
    fn op_fused_f32const_fgt_jiz_f();
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
        StackOp::IsnanF64 => op_isnan_f64 as *const (),
        StackOp::IsinfF64 => op_isinf_f64 as *const (),
        StackOp::Atan2F64 => op_atan2_f64 as *const (),
        StackOp::PrintI32 => op_print_i32 as *const (),
        StackOp::Putc => op_putc as *const (),
        StackOp::Assert => op_assert as *const (),
        StackOp::GetClosurePtr => op_get_closure_ptr as *const (),
        StackOp::FusedGetGetFAddSet(_, _, _) => op_fused_get_get_fadd_set as *const (),
        StackOp::FusedGetGetFSubSet(_, _, _) => op_fused_get_get_fsub_set as *const (),
        StackOp::FusedGetGetFMulSet(_, _, _) => op_fused_get_get_fmul_set as *const (),
        StackOp::FusedGetGetFDivSet(_, _, _) => op_fused_get_get_fdiv_set as *const (),
        StackOp::FusedGetGetIAddSet(_, _, _) => op_fused_get_get_iadd_set as *const (),
        StackOp::FusedGetGetISubSet(_, _, _) => op_fused_get_get_isub_set as *const (),
        StackOp::FusedGetGetIMulSet(_, _, _) => op_fused_get_get_imul_set as *const (),
        StackOp::FusedFieldCopy32(_, _, _) => op_fused_field_copy32 as *const (),
        StackOp::FusedAddrGetSliceStore32(_, _) => op_fused_addr_get_sstore32 as *const (),
        StackOp::FusedLocalArrayLoad32(_, _) => op_fused_local_array_load32 as *const (),
        StackOp::FusedLocalArrayStore32(_, _) => op_fused_local_array_store32 as *const (),
        StackOp::FusedTeeSliceStore32(_, _, _) => op_fused_tee_sstore32 as *const (),
        StackOp::FusedGetSet(_, _) => op_fused_get_set as *const (),
        StackOp::FusedAddrLoad32OffSet(_, _, _) => op_fused_addr_load32off_set as *const (),
        StackOp::FusedAddrImmGetStore32(_, _, _) => op_fused_addr_imm_get_store32 as *const (),
        StackOp::FusedGetGetIAdd(_, _) => op_fused_get_get_iadd as *const (),
        StackOp::FusedGetGetILt(_, _) => op_fused_get_get_ilt as *const (),
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

        // === Float-window ops ===
        StackOp::F32ConstF(_) => op_f32_const_f as *const (),
        StackOp::LocalGetF(_) => op_local_get_f as *const (),
        StackOp::LocalSetF(_) => op_local_set_f as *const (),
        StackOp::LocalTeeF(_) => op_local_tee_f as *const (),
        StackOp::DropF => op_drop_f as *const (),
        StackOp::FAddF => op_fadd_f as *const (),
        StackOp::FSubF => op_fsub_f as *const (),
        StackOp::FMulF => op_fmul_f as *const (),
        StackOp::FDivF => op_fdiv_f as *const (),
        StackOp::FPowF => op_fpow_f as *const (),
        StackOp::FNegF => op_fneg_f as *const (),
        StackOp::FEqF => op_feq_f as *const (),
        StackOp::FNeF => op_fne_f as *const (),
        StackOp::FLtF => op_flt_f as *const (),
        StackOp::FLeF => op_fle_f as *const (),
        StackOp::FGtF => op_fgt_f as *const (),
        StackOp::FGeF => op_fge_f as *const (),
        StackOp::F32ToI32F => op_f32_to_i32_f as *const (),
        StackOp::I32ToF32F => op_i32_to_f32_f as *const (),
        StackOp::FToBitsF => op_to_bits_f as *const (),
        StackOp::BitsToFF => op_from_bits_f as *const (),
        StackOp::LoadF32F => op_load_f32_f as *const (),
        StackOp::LoadF32OffF(_) => op_load_f32_off_f as *const (),
        StackOp::StoreF32F => op_store_f32_f as *const (),
        StackOp::StoreF32OffF(_) => op_store_f32_off_f as *const (),
        StackOp::SinF32F => op_sin_f32_f as *const (),
        StackOp::CosF32F => op_cos_f32_f as *const (),
        StackOp::TanF32F => op_tan_f32_f as *const (),
        StackOp::AsinF32F => op_asin_f32_f as *const (),
        StackOp::AcosF32F => op_acos_f32_f as *const (),
        StackOp::AtanF32F => op_atan_f32_f as *const (),
        StackOp::SinhF32F => op_sinh_f32_f as *const (),
        StackOp::CoshF32F => op_cosh_f32_f as *const (),
        StackOp::TanhF32F => op_tanh_f32_f as *const (),
        StackOp::AsinhF32F => op_asinh_f32_f as *const (),
        StackOp::AcoshF32F => op_acosh_f32_f as *const (),
        StackOp::AtanhF32F => op_atanh_f32_f as *const (),
        StackOp::LnF32F => op_ln_f32_f as *const (),
        StackOp::ExpF32F => op_exp_f32_f as *const (),
        StackOp::Exp2F32F => op_exp2_f32_f as *const (),
        StackOp::Log10F32F => op_log10_f32_f as *const (),
        StackOp::Log2F32F => op_log2_f32_f as *const (),
        StackOp::SqrtF32F => op_sqrt_f32_f as *const (),
        StackOp::AbsF32F => op_abs_f32_f as *const (),
        StackOp::FloorF32F => op_floor_f32_f as *const (),
        StackOp::CeilF32F => op_ceil_f32_f as *const (),
        StackOp::Atan2F32F => op_atan2_f32_f as *const (),
        StackOp::IsnanF32F => op_isnan_f32_f as *const (),
        StackOp::IsinfF32F => op_isinf_f32_f as *const (),
        StackOp::LocalGetL0F => op_local_get_l0_f as *const (),
        StackOp::LocalGetL1F => op_local_get_l1_f as *const (),
        StackOp::LocalGetL2F => op_local_get_l2_f as *const (),
        StackOp::LocalSetL0F => op_local_set_l0_f as *const (),
        StackOp::LocalSetL1F => op_local_set_l1_f as *const (),
        StackOp::LocalSetL2F => op_local_set_l2_f as *const (),
        StackOp::PrintF32F => op_print_f32_f as *const (),
        StackOp::FusedGetGetFAddF(_, _) => op_fused_get_get_fadd_f as *const (),
        StackOp::FusedGetGetFSubF(_, _) => op_fused_get_get_fsub_f as *const (),
        StackOp::FusedGetGetFMulF(_, _) => op_fused_get_get_fmul_f as *const (),
        StackOp::FusedGetFMulF(_) => op_fused_get_fmul_f as *const (),
        StackOp::FusedGetFAddF(_) => op_fused_get_fadd_f as *const (),
        StackOp::FusedGetFSubF(_) => op_fused_get_fsub_f as *const (),
        StackOp::FusedFMulFAddF => op_fused_fmul_fadd_f as *const (),
        StackOp::FusedFMulFSubF => op_fused_fmul_fsub_f as *const (),
        StackOp::FusedGetAddrFMulFAddF(_, _, _) => op_fused_get_addr_fmul_fadd_f as *const (),
        StackOp::FusedGetAddrFMulFSubF(_, _, _) => op_fused_get_addr_fmul_fsub_f as *const (),
        StackOp::FusedAddrLoad32OffF(_, _) => op_fused_addr_load32off_f as *const (),
        StackOp::FusedAddrGetSliceLoad32F(_, _) => op_fused_addr_get_sload32_f as *const (),
        StackOp::FusedAddrGetSliceStore32F(_, _) => op_fused_addr_get_sstore32_f as *const (),
        StackOp::FusedTeeSliceStore32F(_, _, _) => op_fused_tee_sstore32_f as *const (),
        StackOp::FusedLocalArrayLoad32F(_, _) => op_fused_local_array_load32_f as *const (),
        StackOp::FusedLocalArrayStore32F(_, _) => op_fused_local_array_store32_f as *const (),
        StackOp::FusedF32ConstFGtJumpIfZeroF(_, _) => op_fused_f32const_fgt_jiz_f as *const (),
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
        // Call: imm[0]=func_idx, imm[1]=(nargs | preserve<<8), imm[2]=current_func_idx
        StackOp::Call { func, args, preserve } => {
            let packed = (*args as u64) | ((*preserve as u64) << 8);
            [*func as u64, packed, func_idx as u64]
        }
        StackOp::CallIndirect { args } => [*args as u64, func_idx as u64, 0],
        StackOp::CallClosure { args } => [*args as u64, func_idx as u64, 0],
        // Fused instructions
        StackOp::FusedGetGetIAdd(a, b)
        | StackOp::FusedGetGetILt(a, b) | StackOp::FusedAddrGetSliceLoad32(a, b)
        | StackOp::FusedAddrGetSliceStore32(a, b)
        | StackOp::FusedLocalArrayLoad32(a, b) | StackOp::FusedLocalArrayStore32(a, b) => {
            [*a as u64, *b as u64, 0]
        }
        StackOp::FusedGetSet(a, b) => [*a as u64, *b as u64, 0],
        StackOp::FusedAddrLoad32Off(s, o) => [*s as u64, *o as i64 as u64, 0],
        StackOp::FusedAddrLoad32OffSet(s, o, d) => [*s as u64, *o as i64 as u64, *d as u64],
        StackOp::FusedAddrImmGetStore32(s, o, src) => [*s as u64, *o as i64 as u64, *src as u64],
        StackOp::FusedGetGetFAddSet(a, b, d)
        | StackOp::FusedGetGetFSubSet(a, b, d)
        | StackOp::FusedGetGetFMulSet(a, b, d)
        | StackOp::FusedGetGetFDivSet(a, b, d)
        | StackOp::FusedGetGetIAddSet(a, b, d)
        | StackOp::FusedGetGetISubSet(a, b, d)
        | StackOp::FusedGetGetIMulSet(a, b, d) => [*a as u64, *b as u64, *d as u64],
        StackOp::FusedFieldCopy32(s, src, dst) => [*s as u64, *src as i64 as u64, *dst as i64 as u64],
        StackOp::FusedTeeSliceStore32(n, s, idx) => [*n as u64, *s as u64, *idx as u64],
        StackOp::FusedGetAddImmSet(s, v, d) => [*s as u64, *v as i64 as u64, *d as u64],
        StackOp::FusedGetGetILtJumpIfZero(a, b, off) => [*a as u64, *b as u64, *off as i64 as u64],
        StackOp::FusedConstSet(v, n) => [*v as u64, *n as u64, 0],
        StackOp::FusedF32ConstSet(v, n) => [f32::to_bits(*v) as u64, *n as u64, 0],

        // === Float-window ops ===
        //
        // For F ops that load a 32-bit float out of a `locals[n]` slot, we
        // pre-multiply the local index by 8 (the u64 stride of locals[])
        // and store it as a byte offset in imm[0]. This lets the handler
        // emit a single `ldr s, [locals, byte_off]` with no scale —
        // aarch64's register-indexed `ldr s, [base, idx, lsl #SCALE]` form
        // requires SCALE=2 to match 4-byte strides, but we need stride 8
        // to reach the next u64 slot, so without pre-shifting the
        // compiler has to emit an extra `lsl x, x, #3`. Pre-shifting
        // eliminates one instruction per handler call on every f32
        // `locals[]` access in the hot loop.
        //
        // The shifted form only applies to F variants that dereference
        // `locals + imm` as `*(float*)`. F variants that reuse int-style
        // handlers (FusedAddrGetSliceLoad32F et al.) keep the plain
        // unshifted local-index encoding because their inner loads
        // already fold the lsl via u64 addressing.
        StackOp::F32ConstF(v) => [f32::to_bits(*v) as u64, 0, 0],
        StackOp::LocalGetF(n) | StackOp::LocalSetF(n) | StackOp::LocalTeeF(n) => {
            [(*n as u64) * 8, 0, 0]
        }
        StackOp::LoadF32OffF(o) | StackOp::StoreF32OffF(o) => [*o as i64 as u64, 0, 0],
        StackOp::FusedGetGetFAddF(a, b)
        | StackOp::FusedGetGetFSubF(a, b)
        | StackOp::FusedGetGetFMulF(a, b) => [(*a as u64) * 8, (*b as u64) * 8, 0],
        StackOp::FusedAddrGetSliceLoad32F(a, b)
        | StackOp::FusedAddrGetSliceStore32F(a, b)
        | StackOp::FusedLocalArrayLoad32F(a, b)
        | StackOp::FusedLocalArrayStore32F(a, b) => [*a as u64, *b as u64, 0],
        StackOp::FusedGetFMulF(a) | StackOp::FusedGetFAddF(a) | StackOp::FusedGetFSubF(a) => {
            [(*a as u64) * 8, 0, 0]
        }
        StackOp::FusedGetAddrFMulFAddF(a, s, o)
        | StackOp::FusedGetAddrFMulFSubF(a, s, o) => {
            [(*a as u64) * 8, *s as u64, *o as i64 as u64]
        }
        StackOp::FusedAddrLoad32OffF(s, o) => [*s as u64, *o as i64 as u64, 0],
        // imm[0] is the byte offset into locals[] for the tee target
        // (pre-shifted — see the note on LocalGetF above). imm[1] is the
        // slot holding the slice fat pointer; still a u64 index because
        // the handler loads it via `locals[imm[1]]` which folds the lsl.
        // imm[2] is the local index of the element index, also u64.
        StackOp::FusedTeeSliceStore32F(n, s, i) => [(*n as u64) * 8, *s as u64, *i as u64],
        StackOp::FusedF32ConstFGtJumpIfZeroF(v, off) => {
            [f32::to_bits(*v) as u64, *off as i64 as u64, 0]
        }
        _ => [0, 0, 0],
    }
}

/// Convert a StackProgram to C instruction format and run it.
pub fn run(program: &StackProgram) -> i64 {
    // Convert each function's ops to C Instruction arrays.
    let mut c_instructions: Vec<Vec<Instruction>> = Vec::new();

    for (fi, func) in program.functions.iter().enumerate() {
        let mut instrs: Vec<Instruction> = Vec::with_capacity(func.ops.len());
        for op in func.ops.iter() {
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

    // Allocate all runtime buffers up front so stack_interp_run does not
    // touch the heap. Realtime embeddings (audio threads) can mirror this
    // setup once and then call stack_interp_run repeatedly.
    let mut call_stack: Vec<CallFrame> = (0..4096)
        .map(|_| CallFrame {
            return_pc: std::ptr::null_mut(),
            saved_locals: std::ptr::null_mut(),
            func_idx: 0,
            saved_frame_size: 0,
        })
        .collect();
    let mut operand_stack: Vec<u64> = vec![0u64; 64 * 1024];
    let frame_stack_cap: usize = 512 * 1024; // 4 MB worth of u64 slots
    let mut frame_stack: Vec<u64> = vec![0u64; frame_stack_cap];
    let float_stack_cap: usize = 64 * 1024;
    let mut float_stack: Vec<f32> = vec![0.0f32; float_stack_cap];
    let mut globals: Vec<u8> = vec![0u8; program.globals_size];

    // Build context.
    let mut ctx = Ctx {
        call_stack: call_stack.as_mut_ptr(),
        call_depth: 0,
        call_stack_cap: 4096,
        functions: func_metas.as_mut_ptr(),
        func_count: func_metas.len() as u32,
        globals: globals.as_mut_ptr(),
        frame_stack: frame_stack.as_mut_ptr(),
        frame_stack_size: 0,
        frame_stack_cap,
        stack_base: operand_stack.as_mut_ptr(),
        float_stack: float_stack.as_mut_ptr(),
        float_stack_cap,
        current_locals: std::ptr::null_mut(),
        current_fsp: std::ptr::null_mut(),
        closure_ptr: 0,
        result: 0,
        done: 0,
        error: std::ptr::null(),
    };

    let result = unsafe { stack_interp_run(&mut ctx, program.entry) };

    // Surface traps (stack overflow, assertion failed, etc.) to stderr.
    // The C interpreter sets ctx.error to a static string and flips done;
    // it never calls exit() so the embedding host can recover.
    if !ctx.error.is_null() {
        let msg = unsafe { std::ffi::CStr::from_ptr(ctx.error) }
            .to_string_lossy();
        eprintln!("trap: {}", msg);
    }

    result
}
