//! Compute static stack depths for each instruction in a stack function.

use crate::stack_ir::{StackOp, StackFunction};

/// Returns the stack depth at entry to each instruction.
/// Conservative: any instruction that is a jump target gets depth 255
/// (forcing the deep handler variant), since control flow merges make
/// the depth unpredictable without full dataflow analysis.
pub fn compute_depths(func: &StackFunction) -> Vec<u8> {
    let len = func.ops.len();
    let mut depths = vec![0u8; len];

    // Find all jump targets.
    let mut is_target = vec![false; len];
    for (i, op) in func.ops.iter().enumerate() {
        let off = match op {
            StackOp::Jump(off) | StackOp::JumpIfZero(off) | StackOp::JumpIfNotZero(off) => Some(*off),
            StackOp::FusedGetGetILtJumpIfZero(_, _, off) => Some(*off),
            StackOp::FusedF32ConstFGtJumpIfZeroF(_, off) => Some(*off),
            _ => None,
        };
        if let Some(off) = off {
            let target = (i as i64 + 1 + off as i64) as usize;
            if target < len {
                is_target[target] = true;
            }
        }
    }

    let mut depth: i32 = 0;
    for (i, op) in func.ops.iter().enumerate() {
        if is_target[i] {
            // Jump target: force deep (conservative).
            depths[i] = 255;
            depth = 255;
        } else {
            depths[i] = (depth.max(0).min(255)) as u8;
        }
        depth += stack_delta(op);
        if depth < 0 { depth = 0; }
    }

    depths
}

/// Stack depth change for an instruction (integer window only).
///
/// Float-window ops that don't touch the int window contribute 0 here;
/// the float-window depth is tracked separately by `float_stack_delta` if
/// needed.
pub fn stack_delta(op: &StackOp) -> i32 {
    match op {
        // Push 1
        StackOp::I64Const(_) | StackOp::F32Const(_) | StackOp::F64Const(_) |
        StackOp::LocalGet(_) | StackOp::LocalAddr(_) | StackOp::GlobalAddr(_) |
        StackOp::GetClosurePtr |
        StackOp::LocalGetL0 | StackOp::LocalGetL1 | StackOp::LocalGetL2 |
        StackOp::FusedGetGetIAdd(_, _) |
        StackOp::FusedGetGetILt(_, _) | StackOp::FusedAddrLoad32Off(_, _) |
        StackOp::FusedAddrGetSliceLoad32(_, _) => 1,

        // Pop 1
        StackOp::LocalSet(_) |
        StackOp::LocalSetL0 | StackOp::LocalSetL1 | StackOp::LocalSetL2 |
        StackOp::Drop |
        StackOp::PrintI32 | StackOp::Putc | StackOp::Assert |
        StackOp::MemZero(_) |
        StackOp::JumpIfZero(_) | StackOp::JumpIfNotZero(_) => -1,

        // Unary (pop 1, push 1) = net 0
        StackOp::INeg | StackOp::DNeg | StackOp::IAddImm(_) |
        StackOp::Not |
        StackOp::I32ToF64 | StackOp::F64ToI32 |
        StackOp::I32ToI8 | StackOp::I8ToI32 |
        StackOp::I64ToU32 |
        StackOp::Load8 | StackOp::Load32 | StackOp::Load64 |
        StackOp::Load32Off(_) | StackOp::Load64Off(_) |
        StackOp::LocalTee(_) |
        StackOp::LocalTeeL0 | StackOp::LocalTeeL1 | StackOp::LocalTeeL2 |
        StackOp::SinF64 | StackOp::CosF64 | StackOp::TanF64 |
        StackOp::AsinF64 | StackOp::AcosF64 | StackOp::AtanF64 |
        StackOp::SinhF64 | StackOp::CoshF64 | StackOp::TanhF64 |
        StackOp::AsinhF64 | StackOp::AcoshF64 | StackOp::AtanhF64 |
        StackOp::LnF64 | StackOp::ExpF64 | StackOp::Exp2F64 |
        StackOp::Log10F64 | StackOp::Log2F64 | StackOp::SqrtF64 |
        StackOp::AbsF64 | StackOp::FloorF64 | StackOp::CeilF64 |
        StackOp::IsnanF64 | StackOp::IsinfF64 => 0,

        // Binary (pop 2, push 1) = net -1
        StackOp::IAdd | StackOp::ISub | StackOp::IMul | StackOp::IDiv |
        StackOp::UDiv | StackOp::IRem | StackOp::IPow |
        StackOp::DAdd | StackOp::DSub | StackOp::DMul | StackOp::DDiv | StackOp::DPow |
        StackOp::IEq | StackOp::INe | StackOp::ILt | StackOp::ILe |
        StackOp::IGt | StackOp::IGe | StackOp::ULt | StackOp::UGt |
        StackOp::DEq | StackOp::DLt | StackOp::DLe |
        StackOp::And | StackOp::Or | StackOp::Xor | StackOp::Shl | StackOp::Shr | StackOp::UShr |
        StackOp::Atan2F64 |
        StackOp::MemEq(_) | StackOp::MemNe(_) |
        StackOp::SliceEq(_) | StackOp::SliceNe(_) |
        StackOp::SliceLoad32 => -1,

        // Store (pop 2) = net -2
        StackOp::Store8 | StackOp::Store32 | StackOp::Store64 |
        StackOp::Store8Off(_) | StackOp::Store32Off(_) | StackOp::Store64Off(_) |
        StackOp::MemCopy(_) => -2,

        // SliceStore32 (pop 3) = net -3
        StackOp::SliceStore32 => -3,
        StackOp::FusedAddrGetSliceStore32(_, _) => -1, // pop value from TOS
        StackOp::FusedTeeSliceStore32(_, _, _) => -1, // pop value from TOS
        StackOp::FusedLocalArrayLoad32(_, _) => 1,    // push loaded value
        StackOp::FusedLocalArrayStore32(_, _) => -1,  // pop value to store

        // No stack change
        StackOp::FusedGetSet(_, _) | StackOp::FusedGetGetFAddSet(_, _, _) |
        StackOp::FusedGetGetFSubSet(_, _, _) | StackOp::FusedGetGetFMulSet(_, _, _) |
        StackOp::FusedGetGetFDivSet(_, _, _) | StackOp::FusedGetGetIAddSet(_, _, _) |
        StackOp::FusedGetGetISubSet(_, _, _) | StackOp::FusedGetGetIMulSet(_, _, _) |
        StackOp::FusedFieldCopy32(_, _, _) |
        StackOp::FusedAddrLoad32OffSet(_, _, _) | StackOp::FusedAddrImmGetStore32(_, _, _) |
        StackOp::Jump(_) | StackOp::Nop | StackOp::Halt |
        StackOp::Return | StackOp::ReturnVoid |
        StackOp::Call { .. } | StackOp::CallIndirect { .. } | StackOp::CallClosure { .. } |
        StackOp::FusedConstSet(_, _) | StackOp::FusedF32ConstSet(_, _) |
        StackOp::FusedGetAddImmSet(_, _, _) | StackOp::FusedGetGetILtJumpIfZero(_, _, _) => 0,

        // === Float-window ops: integer-window deltas ===
        // Most don't touch the int window. Crossings do.
        StackOp::F32ConstF(_) | StackOp::LocalGetF(_) | StackOp::LocalSetF(_)
        | StackOp::LocalTeeF(_) | StackOp::DropF
        | StackOp::FAddF | StackOp::FSubF | StackOp::FMulF | StackOp::FDivF
        | StackOp::FPowF | StackOp::FNegF
        | StackOp::SinF32F | StackOp::CosF32F | StackOp::TanF32F
        | StackOp::AsinF32F | StackOp::AcosF32F | StackOp::AtanF32F
        | StackOp::SinhF32F | StackOp::CoshF32F | StackOp::TanhF32F
        | StackOp::AsinhF32F | StackOp::AcoshF32F | StackOp::AtanhF32F
        | StackOp::LnF32F | StackOp::ExpF32F | StackOp::Exp2F32F
        | StackOp::Log10F32F | StackOp::Log2F32F | StackOp::SqrtF32F
        | StackOp::AbsF32F | StackOp::FloorF32F | StackOp::CeilF32F
        | StackOp::Atan2F32F
        | StackOp::LocalGetL0F | StackOp::LocalGetL1F | StackOp::LocalGetL2F
        | StackOp::LocalSetL0F | StackOp::LocalSetL1F | StackOp::LocalSetL2F
        | StackOp::LocalTeeL0F | StackOp::LocalTeeL1F | StackOp::LocalTeeL2F
        | StackOp::PrintF32F
        | StackOp::FusedGetGetFAddF(_, _) | StackOp::FusedGetGetFSubF(_, _)
        | StackOp::FusedGetGetFMulF(_, _)
        | StackOp::FusedGetFMulF(_) | StackOp::FusedGetFAddF(_) | StackOp::FusedGetFSubF(_)
        | StackOp::FusedFMulFAddF | StackOp::FusedFMulFSubF
        | StackOp::FusedGetAddrFMulFAddF(_, _, _) | StackOp::FusedGetAddrFMulFSubF(_, _, _)
        | StackOp::FusedAddrLoad32OffF(_, _)
        | StackOp::FusedF32ConstFGtJumpIfZeroF(_, _) => 0,

        // Float comparisons: pop 2 from f-window, push 1 to int window.
        StackOp::FEqF | StackOp::FNeF | StackOp::FLtF | StackOp::FLeF
        | StackOp::FGtF | StackOp::FGeF
        | StackOp::IsnanF32F | StackOp::IsinfF32F => 1,

        // Crossings that consume from one window and produce in the other.
        StackOp::F32ToI32F | StackOp::FToBitsF => 1,    // pop f0, push t0
        StackOp::I32ToF32F | StackOp::BitsToFF => -1,   // pop t0, push f0
        StackOp::F32ToF64 => 1,                         // pop f0, push f64 bits
        StackOp::F64ToF32 => -1,                        // pop f64 bits, push f0

        // Float loads: pop addr from int window, push float to f-window.
        StackOp::LoadF32F | StackOp::LoadF32OffF(_)
        | StackOp::FusedAddrGetSliceLoad32F(_, _) => -1,

        // Float stores: pop addr from int window (and pop f0).
        StackOp::StoreF32F | StackOp::StoreF32OffF(_)
        | StackOp::FusedAddrGetSliceStore32F(_, _) => -1,

        // Local-array variants don't take an address from the int window
        // (they read the slot from the imm). FusedTeeSliceStore32F also
        // touches only the float window + locals[].
        StackOp::FusedLocalArrayLoad32F(_, _) | StackOp::FusedLocalArrayStore32F(_, _) => 0,
        StackOp::FusedTeeSliceStore32F(_, _, _) => 0,
    }
}

/// Stack depth change for an instruction (float window only).
///
/// Used to track f0..f3 occupancy independently from the integer window.
pub fn float_stack_delta(op: &StackOp) -> i32 {
    match op {
        // Pushes
        StackOp::F32ConstF(_) | StackOp::LocalGetF(_)
        | StackOp::LocalGetL0F | StackOp::LocalGetL1F | StackOp::LocalGetL2F
        | StackOp::FusedGetGetFAddF(_, _) | StackOp::FusedGetGetFSubF(_, _)
        | StackOp::FusedGetGetFMulF(_, _)
        | StackOp::FusedAddrLoad32OffF(_, _)
        | StackOp::FusedAddrGetSliceLoad32F(_, _)
        | StackOp::FusedLocalArrayLoad32F(_, _) => 1,

        // Pops
        StackOp::LocalSetF(_) | StackOp::DropF
        | StackOp::LocalSetL0F | StackOp::LocalSetL1F | StackOp::LocalSetL2F
        | StackOp::PrintF32F
        | StackOp::FusedAddrGetSliceStore32F(_, _)
        | StackOp::FusedTeeSliceStore32F(_, _, _)
        | StackOp::FusedLocalArrayStore32F(_, _)
        | StackOp::FusedF32ConstFGtJumpIfZeroF(_, _) => -1,

        // LocalTeeF: peek (pop 0)
        StackOp::LocalTeeF(_) => 0,

        // Binary float arith: pop 2, push 1 = -1
        StackOp::FAddF | StackOp::FSubF | StackOp::FMulF | StackOp::FDivF
        | StackOp::FPowF | StackOp::Atan2F32F => -1,

        // Float comparisons: pop 2 from f-window
        StackOp::FEqF | StackOp::FNeF | StackOp::FLtF | StackOp::FLeF
        | StackOp::FGtF | StackOp::FGeF => -2,

        // FMA fused: pop 3, push 1 = -2
        StackOp::FusedFMulFAddF | StackOp::FusedFMulFSubF => -2,

        // Unary in float window: pop 1, push 1 = 0
        StackOp::FNegF
        | StackOp::SinF32F | StackOp::CosF32F | StackOp::TanF32F
        | StackOp::AsinF32F | StackOp::AcosF32F | StackOp::AtanF32F
        | StackOp::SinhF32F | StackOp::CoshF32F | StackOp::TanhF32F
        | StackOp::AsinhF32F | StackOp::AcoshF32F | StackOp::AtanhF32F
        | StackOp::LnF32F | StackOp::ExpF32F | StackOp::Exp2F32F
        | StackOp::Log10F32F | StackOp::Log2F32F | StackOp::SqrtF32F
        | StackOp::AbsF32F | StackOp::FloorF32F | StackOp::CeilF32F => 0,

        // f0 op locals[a]: pop 0 push 0
        StackOp::FusedGetFMulF(_) | StackOp::FusedGetFAddF(_) | StackOp::FusedGetFSubF(_) => 0,

        // f0 ± coeff*state from frame slot — net 0 in f-window.
        StackOp::FusedGetAddrFMulFAddF(_, _, _) | StackOp::FusedGetAddrFMulFSubF(_, _, _) => 0,

        // Crossings: F→int pops f-window
        StackOp::F32ToI32F | StackOp::FToBitsF
        | StackOp::IsnanF32F | StackOp::IsinfF32F
        | StackOp::F32ToF64 => -1,
        // int→F pushes f-window
        StackOp::I32ToF32F | StackOp::BitsToFF
        | StackOp::F64ToF32 => 1,

        // f-window stores pop f0
        StackOp::StoreF32F | StackOp::StoreF32OffF(_) => -1,
        // f-window loads push f0
        StackOp::LoadF32F | StackOp::LoadF32OffF(_) => 1,

        _ => 0,
    }
}
