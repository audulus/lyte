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

/// Stack depth change for an instruction.
fn stack_delta(op: &StackOp) -> i32 {
    match op {
        // Push 1
        StackOp::I64Const(_) | StackOp::F32Const(_) | StackOp::F64Const(_) |
        StackOp::LocalGet(_) | StackOp::LocalAddr(_) | StackOp::GlobalAddr(_) |
        StackOp::GetClosurePtr |
        StackOp::FusedGetGetFMul(_, _) | StackOp::FusedGetGetFAdd(_, _) |
        StackOp::FusedGetGetFSub(_, _) | StackOp::FusedGetGetIAdd(_, _) |
        StackOp::FusedGetGetILt(_, _) | StackOp::FusedAddrLoad32Off(_, _) |
        StackOp::FusedAddrGetSliceLoad32(_, _) => 1,

        // Pop 1
        StackOp::LocalSet(_) | StackOp::Drop |
        StackOp::PrintI32 | StackOp::PrintF32 | StackOp::Putc | StackOp::Assert |
        StackOp::MemZero(_) |
        StackOp::JumpIfZero(_) | StackOp::JumpIfNotZero(_) => -1,

        // Unary (pop 1, push 1) = net 0
        StackOp::INeg | StackOp::FNeg | StackOp::DNeg | StackOp::IAddImm(_) |
        StackOp::Not |
        StackOp::I32ToF32 | StackOp::F32ToI32 | StackOp::I32ToF64 | StackOp::F64ToI32 |
        StackOp::F32ToF64 | StackOp::F64ToF32 | StackOp::I32ToI8 | StackOp::I8ToI32 |
        StackOp::I64ToU32 |
        StackOp::Load8 | StackOp::Load32 | StackOp::Load64 |
        StackOp::Load32Off(_) | StackOp::Load64Off(_) |
        StackOp::LocalTee(_) |
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
        StackOp::FusedGetFMul(_) | StackOp::FusedGetFAdd(_) | StackOp::FusedGetFSub(_) => 0,

        // Binary (pop 2, push 1) = net -1
        StackOp::IAdd | StackOp::ISub | StackOp::IMul | StackOp::IDiv |
        StackOp::UDiv | StackOp::IRem | StackOp::IPow |
        StackOp::FAdd | StackOp::FSub | StackOp::FMul | StackOp::FDiv | StackOp::FPow |
        StackOp::DAdd | StackOp::DSub | StackOp::DMul | StackOp::DDiv | StackOp::DPow |
        StackOp::IEq | StackOp::INe | StackOp::ILt | StackOp::ILe |
        StackOp::IGt | StackOp::IGe | StackOp::ULt | StackOp::UGt |
        StackOp::FEq | StackOp::FNe | StackOp::FLt | StackOp::FLe |
        StackOp::FGt | StackOp::FGe |
        StackOp::DEq | StackOp::DLt | StackOp::DLe |
        StackOp::And | StackOp::Or | StackOp::Xor | StackOp::Shl | StackOp::Shr | StackOp::UShr |
        StackOp::Atan2F32 | StackOp::Atan2F64 |
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

        // FusedFMulFAdd/FSub: pop 3, push 1 = net -2
        StackOp::FusedFMulFAdd | StackOp::FusedFMulFSub => -2,

        // No stack change
        StackOp::FusedGetSet(_, _) |
        StackOp::FusedGetAddrFMulFAdd(_, _, _) | StackOp::FusedGetAddrFMulFSub(_, _, _) |
        StackOp::FusedAddrLoad32OffSet(_, _, _) | StackOp::FusedAddrImmGetStore32(_, _, _) |
        StackOp::Jump(_) | StackOp::Nop | StackOp::Halt |
        StackOp::Return | StackOp::ReturnVoid |
        StackOp::Call { .. } | StackOp::CallIndirect { .. } | StackOp::CallClosure { .. } |
        StackOp::FusedConstSet(_, _) | StackOp::FusedF32ConstSet(_, _) |
        StackOp::FusedGetAddImmSet(_, _, _) | StackOp::FusedGetGetILtJumpIfZero(_, _, _) => 0,
    }
}
