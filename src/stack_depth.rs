//! Compute static stack depths for each instruction in a stack function.

use crate::stack_ir::{StackFunction, StackOp};

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
            StackOp::Jump(off) | StackOp::JumpIfZero(off) | StackOp::JumpIfNotZero(off) => {
                Some(*off)
            }
            StackOp::FusedGetGetILtJumpIfZero(_, _, off) => Some(*off),
            StackOp::FusedBoundsCheck1JumpIfZero(_, off)
            | StackOp::FusedBoundsCheck2JumpIfZero(_, off)
            | StackOp::FusedBoundsCheck3JumpIfZero(_, off)
            | StackOp::FusedBoundsCheck4JumpIfZero(_, off)
            | StackOp::FusedBoundsCheck5JumpIfZero(_, off)
            | StackOp::FusedBoundsCheck6JumpIfZero(_, off)
            | StackOp::FusedBoundsCheck7JumpIfZero(_, off)
            | StackOp::FusedBoundsCheck8JumpIfZero(_, off) => Some(*off),
            StackOp::FusedF32ConstFGtJumpIfZeroF(_, off) => Some(*off),
            StackOp::FusedGetF32ConstFGtJumpIfZeroF(_, _, off) => Some(*off),
            StackOp::FusedF64ConstDGtJumpIfZeroD(_, off) => Some(*off),
            StackOp::FusedGetF64ConstDGtJumpIfZeroD(_, _, off) => Some(*off),
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
        if depth < 0 {
            depth = 0;
        }
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
        StackOp::I64Const(_)
        | StackOp::F32Const(_)
        | StackOp::F64Const(_)
        | StackOp::LocalGet(_)
        | StackOp::LocalAddr(_)
        | StackOp::GlobalAddr(_)
        | StackOp::GetClosurePtr
        | StackOp::FusedGetGetIAdd(_, _)
        | StackOp::FusedGetGetILt(_, _)
        | StackOp::FusedAddrLoad32Off(_, _)
        | StackOp::FusedAddrGetSliceLoad32(_, _) => 1,

        // Pop 1
        StackOp::LocalSet(_)
        | StackOp::Drop
        | StackOp::PrintI32
        | StackOp::Putc
        | StackOp::Assert
        | StackOp::MemZero(_)
        | StackOp::JumpIfZero(_)
        | StackOp::JumpIfNotZero(_) => -1,

        // Unary (pop 1, push 1) = net 0
        StackOp::INeg
        | StackOp::DNeg
        | StackOp::IAddImm(_)
        | StackOp::Not
        | StackOp::I32ToF64
        | StackOp::F64ToI32
        | StackOp::I32ToI8
        | StackOp::I8ToI32
        | StackOp::I64ToU32
        | StackOp::Load8
        | StackOp::Load32
        | StackOp::Load64
        | StackOp::Load32Off(_)
        | StackOp::Load64Off(_)
        | StackOp::LocalTee(_)
        | StackOp::SinF64
        | StackOp::CosF64
        | StackOp::TanF64
        | StackOp::AsinF64
        | StackOp::AcosF64
        | StackOp::AtanF64
        | StackOp::SinhF64
        | StackOp::CoshF64
        | StackOp::TanhF64
        | StackOp::AsinhF64
        | StackOp::AcoshF64
        | StackOp::AtanhF64
        | StackOp::LnF64
        | StackOp::ExpF64
        | StackOp::Exp2F64
        | StackOp::Log10F64
        | StackOp::Log2F64
        | StackOp::SqrtF64
        | StackOp::AbsF64
        | StackOp::FloorF64
        | StackOp::CeilF64
        | StackOp::IsnanF64
        | StackOp::IsinfF64 => 0,

        // Binary (pop 2, push 1) = net -1
        StackOp::IAdd
        | StackOp::ISub
        | StackOp::IMul
        | StackOp::IDiv
        | StackOp::UDiv
        | StackOp::IRem
        | StackOp::IPow
        | StackOp::DAdd
        | StackOp::DSub
        | StackOp::DMul
        | StackOp::DDiv
        | StackOp::DPow
        | StackOp::IEq
        | StackOp::INe
        | StackOp::ILt
        | StackOp::ILe
        | StackOp::IGt
        | StackOp::IGe
        | StackOp::ULt
        | StackOp::UGt
        | StackOp::DEq
        | StackOp::DLt
        | StackOp::DLe
        | StackOp::And
        | StackOp::Or
        | StackOp::Xor
        | StackOp::Shl
        | StackOp::Shr
        | StackOp::UShr
        | StackOp::Atan2F64
        | StackOp::MemEq(_)
        | StackOp::MemNe(_)
        | StackOp::SliceEq(_)
        | StackOp::SliceNe(_)
        | StackOp::SliceLoad32 => -1,

        // Store (pop 2) = net -2
        StackOp::Store8
        | StackOp::Store32
        | StackOp::Store64
        | StackOp::Store8Off(_)
        | StackOp::Store32Off(_)
        | StackOp::Store64Off(_)
        | StackOp::MemCopy(_) => -2,

        // SliceStore32 (pop 3) = net -3
        StackOp::SliceStore32 => -3,
        StackOp::FusedAddrGetSliceStore32(_, _) => -1, // pop value from TOS
        StackOp::FusedTeeSliceStore32(_, _, _) => -1,  // pop value from TOS
        StackOp::FusedLocalArrayLoad32(_, _) => 1,     // push loaded value
        StackOp::FusedLocalArrayStore32(_, _) => -1,   // pop value to store
        StackOp::CallExtern { args, .. } => 1 - *args as i32,

        // No stack change
        StackOp::FusedGetSet(_, _)
        | StackOp::FusedGetGetFAddSet(_, _, _)
        | StackOp::FusedGetGetFSubSet(_, _, _)
        | StackOp::FusedGetGetFMulSet(_, _, _)
        | StackOp::FusedGetGetFDivSet(_, _, _)
        | StackOp::FusedGetGetIAddSet(_, _, _)
        | StackOp::FusedGetGetISubSet(_, _, _)
        | StackOp::FusedGetGetIMulSet(_, _, _)
        | StackOp::FusedFieldCopy32(_, _, _)
        | StackOp::FusedAddrLoad32OffSet(_, _, _)
        | StackOp::FusedAddrImmGetStore32(_, _, _)
        | StackOp::Jump(_)
        | StackOp::Nop
        | StackOp::Halt
        | StackOp::Return
        | StackOp::ReturnVoid
        | StackOp::Call { .. }
        | StackOp::CallIndirect { .. }
        | StackOp::CallClosure { .. }
        | StackOp::FusedConstSet(_, _)
        | StackOp::FusedF32ConstSet(_, _)
        | StackOp::FusedGetAddImmSet(_, _, _)
        | StackOp::FusedGetGetILtJumpIfZero(_, _, _)
        | StackOp::FusedBoundsCheck1JumpIfZero(_, _)
        | StackOp::FusedBoundsCheck2JumpIfZero(_, _)
        | StackOp::FusedBoundsCheck3JumpIfZero(_, _)
        | StackOp::FusedBoundsCheck4JumpIfZero(_, _)
        | StackOp::FusedBoundsCheck5JumpIfZero(_, _)
        | StackOp::FusedBoundsCheck6JumpIfZero(_, _)
        | StackOp::FusedBoundsCheck7JumpIfZero(_, _)
        | StackOp::FusedBoundsCheck8JumpIfZero(_, _)
        | StackOp::FusedGetSetF(_, _)
        | StackOp::FusedGetSet2F(_)
        | StackOp::FusedGetSet3F(_)
        | StackOp::FusedGetSet4F(_)
        | StackOp::FusedGetSet5F(_)
        | StackOp::FusedGetSet6F(_)
        | StackOp::FusedGetSet7F(_)
        | StackOp::FusedGetSet8F(_)
        | StackOp::FusedFMulFAddSetF(_)
        | StackOp::FusedFMulFSubSetF(_) => 0,

        // === Float-window ops: integer-window deltas ===
        // Most don't touch the int window. Crossings do.
        StackOp::F32ConstF(_)
        | StackOp::LocalGetF(_)
        | StackOp::LocalSetF(_)
        | StackOp::LocalTeeF(_)
        | StackOp::DropF
        | StackOp::FAddF
        | StackOp::FSubF
        | StackOp::FMulF
        | StackOp::FDivF
        | StackOp::FPowF
        | StackOp::FNegF
        | StackOp::SinF32F
        | StackOp::CosF32F
        | StackOp::TanF32F
        | StackOp::AsinF32F
        | StackOp::AcosF32F
        | StackOp::AtanF32F
        | StackOp::SinhF32F
        | StackOp::CoshF32F
        | StackOp::TanhF32F
        | StackOp::AsinhF32F
        | StackOp::AcoshF32F
        | StackOp::AtanhF32F
        | StackOp::LnF32F
        | StackOp::ExpF32F
        | StackOp::Exp2F32F
        | StackOp::Log10F32F
        | StackOp::Log2F32F
        | StackOp::SqrtF32F
        | StackOp::AbsF32F
        | StackOp::FloorF32F
        | StackOp::CeilF32F
        | StackOp::Atan2F32F
        | StackOp::PrintF32F
        | StackOp::FusedGetGetFAddF(_, _)
        | StackOp::FusedGetGetFSubF(_, _)
        | StackOp::FusedGetGetFMulF(_, _)
        | StackOp::FusedGetFMulF(_)
        | StackOp::FusedGetFAddF(_)
        | StackOp::FusedGetFSubF(_)
        | StackOp::FusedFMulFAddF
        | StackOp::FusedFMulFSubF
        | StackOp::FusedGetAddrFMulFAddF(_, _, _)
        | StackOp::FusedGetAddrFMulFSubF(_, _, _)
        | StackOp::FusedAddrLoad32OffF(_, _)
        | StackOp::FusedF32ConstFGtJumpIfZeroF(_, _)
        | StackOp::FusedGetF32ConstFGtJumpIfZeroF(_, _, _) => 0,

        // Float comparisons: pop 2 from f-window, push 1 to int window.
        StackOp::FEqF
        | StackOp::FNeF
        | StackOp::FLtF
        | StackOp::FLeF
        | StackOp::FGtF
        | StackOp::FGeF
        | StackOp::IsnanF32F
        | StackOp::IsinfF32F => 1,

        // Crossings that consume from one window and produce in the other.
        StackOp::F32ToI32F | StackOp::FToBitsF => 1, // pop f0, push t0
        StackOp::I32ToF32F | StackOp::BitsToFF => -1, // pop t0, push f0
        StackOp::F32ToF64 => 1,                      // pop f0, push f64 bits
        StackOp::F64ToF32 => -1,                     // pop f64 bits, push f0

        // Generic f32 slice ops consume both int operands directly.
        StackOp::SliceLoad32F | StackOp::SliceStore32F => -2,

        // Float loads: pop addr from int window, push float to f-window.
        StackOp::LoadF32F | StackOp::LoadF32OffF(_) | StackOp::FusedAddrGetSliceLoad32F(_, _) => -1,

        // Float stores: pop addr from int window (and pop f0).
        StackOp::StoreF32F
        | StackOp::StoreF32OffF(_)
        | StackOp::FusedAddrGetSliceStore32F(_, _) => -1,

        // Local-array variants don't take an address from the int window
        // (they read the slot from the imm). FusedTeeSliceStore32F also
        // touches only the float window + locals[].
        StackOp::FusedLocalArrayLoad32F(_, _)
        | StackOp::FusedLocalArrayStore32F(_, _)
        | StackOp::FusedGetGetFMulFAddF(_, _)
        | StackOp::FusedGetGetFMulFSubF(_, _)
        | StackOp::FusedGetGetFMulSum2F(_, _)
        | StackOp::FusedGetGetFMulSum3F(_, _)
        | StackOp::FusedGetGetFMulSum4F(_, _)
        | StackOp::FusedGetGetFMulSum5F(_, _)
        | StackOp::FusedGetGetFMulSum6F(_, _)
        | StackOp::FusedGetGetFMulSum7F(_, _)
        | StackOp::FusedGetGetFMulSum8F(_, _) => 0,
        StackOp::FusedTeeSliceStore32F(_, _, _) => 0,

        // === Double-window (D) ops: integer-window deltas ===
        // Pure d-window ops don't touch the int window.
        StackOp::F64ConstD(_)
        | StackOp::LocalGetD(_)
        | StackOp::LocalSetD(_)
        | StackOp::LocalTeeD(_)
        | StackOp::DropD
        | StackOp::DAddD
        | StackOp::DSubD
        | StackOp::DMulD
        | StackOp::DDivD
        | StackOp::DPowD
        | StackOp::DNegD
        | StackOp::F32ToF64D
        | StackOp::F64ToF32D
        | StackOp::SinF64D
        | StackOp::CosF64D
        | StackOp::TanF64D
        | StackOp::AsinF64D
        | StackOp::AcosF64D
        | StackOp::AtanF64D
        | StackOp::SinhF64D
        | StackOp::CoshF64D
        | StackOp::TanhF64D
        | StackOp::AsinhF64D
        | StackOp::AcoshF64D
        | StackOp::AtanhF64D
        | StackOp::LnF64D
        | StackOp::ExpF64D
        | StackOp::Exp2F64D
        | StackOp::Log10F64D
        | StackOp::Log2F64D
        | StackOp::SqrtF64D
        | StackOp::AbsF64D
        | StackOp::FloorF64D
        | StackOp::CeilF64D
        | StackOp::Atan2F64D
        | StackOp::PrintF64D => 0,

        // d-window → int window: push 1 to the int window.
        StackOp::DEqD
        | StackOp::DNeD
        | StackOp::DLtD
        | StackOp::DLeD
        | StackOp::DGtD
        | StackOp::DGeD
        | StackOp::F64ToI32D
        | StackOp::DToBitsD
        | StackOp::IsnanF64D
        | StackOp::IsinfF64D => 1,

        // int window → d-window: pop 1 from the int window. Loads/stores
        // take the address from the int window (value lives in d-window).
        StackOp::I32ToF64D
        | StackOp::BitsToDD
        | StackOp::LoadF64D
        | StackOp::LoadF64OffD(_)
        | StackOp::StoreF64D
        | StackOp::StoreF64OffD(_) => -1,

        // Fused d-window superinstructions read operands from locals[] and
        // operate only on the d-window, so the int window is untouched.
        StackOp::FusedGetGetDMulD(_, _)
        | StackOp::FusedGetGetDMulDAddD(_, _)
        | StackOp::FusedGetGetDMulDSubD(_, _)
        | StackOp::FusedGetGetDMulSum2D(_, _)
        | StackOp::FusedGetGetDMulSum3D(_, _)
        | StackOp::FusedGetGetDMulSum4D(_, _)
        | StackOp::FusedGetGetDMulSum5D(_, _)
        | StackOp::FusedGetGetDMulSum6D(_, _)
        | StackOp::FusedGetGetDMulSum7D(_, _)
        | StackOp::FusedGetGetDMulSum8D(_, _)
        | StackOp::FusedGetSetD(_, _)
        | StackOp::FusedGetSet2D(_)
        | StackOp::FusedGetSet3D(_)
        | StackOp::FusedGetSet4D(_)
        | StackOp::FusedGetSet5D(_)
        | StackOp::FusedGetSet6D(_)
        | StackOp::FusedGetSet7D(_)
        | StackOp::FusedGetSet8D(_)
        | StackOp::FusedF64ConstDGtJumpIfZeroD(_, _)
        | StackOp::FusedGetF64ConstDGtJumpIfZeroD(_, _, _) => 0,
    }
}

/// Stack depth change for an instruction (double window only).
///
/// The f64 analogue of `float_stack_delta`. Tracks d0..d3 occupancy
/// independently from the integer and float windows. Ops that don't
/// touch the double window contribute 0 via the catch-all.
pub fn double_stack_delta(op: &StackOp) -> i32 {
    match op {
        // Pushes onto the d-window.
        StackOp::F64ConstD(_)
        | StackOp::LocalGetD(_)
        | StackOp::I32ToF64D
        | StackOp::BitsToDD
        | StackOp::F32ToF64D
        | StackOp::LoadF64D
        | StackOp::LoadF64OffD(_) => 1,

        // Pops from the d-window (single value).
        StackOp::LocalSetD(_)
        | StackOp::DropD
        | StackOp::F64ToI32D
        | StackOp::DToBitsD
        | StackOp::F64ToF32D
        | StackOp::IsnanF64D
        | StackOp::IsinfF64D
        | StackOp::StoreF64D
        | StackOp::StoreF64OffD(_)
        | StackOp::PrintF64D => -1,

        // Peek (LocalTeeD) and unary in-window ops: net 0.
        StackOp::LocalTeeD(_)
        | StackOp::DNegD
        | StackOp::SinF64D
        | StackOp::CosF64D
        | StackOp::TanF64D
        | StackOp::AsinF64D
        | StackOp::AcosF64D
        | StackOp::AtanF64D
        | StackOp::SinhF64D
        | StackOp::CoshF64D
        | StackOp::TanhF64D
        | StackOp::AsinhF64D
        | StackOp::AcoshF64D
        | StackOp::AtanhF64D
        | StackOp::LnF64D
        | StackOp::ExpF64D
        | StackOp::Exp2F64D
        | StackOp::Log10F64D
        | StackOp::Log2F64D
        | StackOp::SqrtF64D
        | StackOp::AbsF64D
        | StackOp::FloorF64D
        | StackOp::CeilF64D => 0,

        // Binary d-window arith: pop 2, push 1 = -1.
        StackOp::DAddD
        | StackOp::DSubD
        | StackOp::DMulD
        | StackOp::DDivD
        | StackOp::DPowD
        | StackOp::Atan2F64D => -1,

        // Double comparisons: pop 2 from d-window (result goes to int).
        StackOp::DEqD
        | StackOp::DNeD
        | StackOp::DLtD
        | StackOp::DLeD
        | StackOp::DGtD
        | StackOp::DGeD => -2,

        // Fused d-window superinstructions.
        // Push one result: bare mul and the mul-accumulate sums.
        StackOp::FusedGetGetDMulD(_, _)
        | StackOp::FusedGetGetDMulSum2D(_, _)
        | StackOp::FusedGetGetDMulSum3D(_, _)
        | StackOp::FusedGetGetDMulSum4D(_, _)
        | StackOp::FusedGetGetDMulSum5D(_, _)
        | StackOp::FusedGetGetDMulSum6D(_, _)
        | StackOp::FusedGetGetDMulSum7D(_, _)
        | StackOp::FusedGetGetDMulSum8D(_, _) => 1,

        // Accumulate onto d0 in place (net 0). Get/set move chains and the
        // direct-from-local compare/jump don't touch the d-window.
        StackOp::FusedGetGetDMulDAddD(_, _)
        | StackOp::FusedGetGetDMulDSubD(_, _)
        | StackOp::FusedGetSetD(_, _)
        | StackOp::FusedGetSet2D(_)
        | StackOp::FusedGetSet3D(_)
        | StackOp::FusedGetSet4D(_)
        | StackOp::FusedGetSet5D(_)
        | StackOp::FusedGetSet6D(_)
        | StackOp::FusedGetSet7D(_)
        | StackOp::FusedGetSet8D(_)
        | StackOp::FusedGetF64ConstDGtJumpIfZeroD(_, _, _) => 0,

        // Pops d0 for the comparison before branching.
        StackOp::FusedF64ConstDGtJumpIfZeroD(_, _) => -1,

        _ => 0,
    }
}

/// Stack depth change for an instruction (float window only).
///
/// Used to track f0..f3 occupancy independently from the integer window.
pub fn float_stack_delta(op: &StackOp) -> i32 {
    match op {
        // Pushes
        StackOp::F32ConstF(_)
        | StackOp::LocalGetF(_)
        | StackOp::SliceLoad32F
        | StackOp::FusedGetGetFAddF(_, _)
        | StackOp::FusedGetGetFSubF(_, _)
        | StackOp::FusedGetGetFMulF(_, _)
        | StackOp::FusedGetGetFMulSum2F(_, _)
        | StackOp::FusedGetGetFMulSum3F(_, _)
        | StackOp::FusedGetGetFMulSum4F(_, _)
        | StackOp::FusedGetGetFMulSum5F(_, _)
        | StackOp::FusedGetGetFMulSum6F(_, _)
        | StackOp::FusedGetGetFMulSum7F(_, _)
        | StackOp::FusedGetGetFMulSum8F(_, _)
        | StackOp::FusedAddrLoad32OffF(_, _)
        | StackOp::FusedAddrGetSliceLoad32F(_, _)
        | StackOp::FusedLocalArrayLoad32F(_, _) => 1,

        // Pops
        StackOp::LocalSetF(_)
        | StackOp::DropF
        | StackOp::SliceStore32F
        | StackOp::PrintF32F
        | StackOp::FusedAddrGetSliceStore32F(_, _)
        | StackOp::FusedTeeSliceStore32F(_, _, _)
        | StackOp::FusedLocalArrayStore32F(_, _)
        | StackOp::FusedF32ConstFGtJumpIfZeroF(_, _) => -1,

        // Direct local compare/jump doesn't touch the float window.
        StackOp::FusedGetF32ConstFGtJumpIfZeroF(_, _, _) => 0,

        StackOp::FusedGetSetF(_, _)
        | StackOp::FusedGetSet2F(_)
        | StackOp::FusedGetSet3F(_)
        | StackOp::FusedGetSet4F(_)
        | StackOp::FusedGetSet5F(_)
        | StackOp::FusedGetSet6F(_)
        | StackOp::FusedGetSet7F(_)
        | StackOp::FusedGetSet8F(_)
        | StackOp::FusedGetGetFMulFAddF(_, _)
        | StackOp::FusedGetGetFMulFSubF(_, _) => 0,

        // LocalTeeF: peek (pop 0)
        StackOp::LocalTeeF(_) => 0,

        // Binary float arith: pop 2, push 1 = -1
        StackOp::FAddF
        | StackOp::FSubF
        | StackOp::FMulF
        | StackOp::FDivF
        | StackOp::FPowF
        | StackOp::Atan2F32F => -1,

        // Float comparisons: pop 2 from f-window
        StackOp::FEqF
        | StackOp::FNeF
        | StackOp::FLtF
        | StackOp::FLeF
        | StackOp::FGtF
        | StackOp::FGeF => -2,

        // FMA fused: pop 3, push 1 = -2
        StackOp::FusedFMulFAddF | StackOp::FusedFMulFSubF => -2,
        // Terminal FMA-to-local-store: pop 3, push 0 = -3
        StackOp::FusedFMulFAddSetF(_) | StackOp::FusedFMulFSubSetF(_) => -3,

        // Unary in float window: pop 1, push 1 = 0
        StackOp::FNegF
        | StackOp::SinF32F
        | StackOp::CosF32F
        | StackOp::TanF32F
        | StackOp::AsinF32F
        | StackOp::AcosF32F
        | StackOp::AtanF32F
        | StackOp::SinhF32F
        | StackOp::CoshF32F
        | StackOp::TanhF32F
        | StackOp::AsinhF32F
        | StackOp::AcoshF32F
        | StackOp::AtanhF32F
        | StackOp::LnF32F
        | StackOp::ExpF32F
        | StackOp::Exp2F32F
        | StackOp::Log10F32F
        | StackOp::Log2F32F
        | StackOp::SqrtF32F
        | StackOp::AbsF32F
        | StackOp::FloorF32F
        | StackOp::CeilF32F => 0,

        // f0 op locals[a]: pop 0 push 0
        StackOp::FusedGetFMulF(_) | StackOp::FusedGetFAddF(_) | StackOp::FusedGetFSubF(_) => 0,

        // f0 ± coeff*state from frame slot — net 0 in f-window.
        StackOp::FusedGetAddrFMulFAddF(_, _, _) | StackOp::FusedGetAddrFMulFSubF(_, _, _) => 0,

        // Crossings: F→int pops f-window. F32ToF64D pops the f-window
        // (f32) and pushes the d-window (handled in double_stack_delta).
        StackOp::F32ToI32F
        | StackOp::FToBitsF
        | StackOp::IsnanF32F
        | StackOp::IsinfF32F
        | StackOp::F32ToF64
        | StackOp::F32ToF64D => -1,
        // int→F pushes f-window. F64ToF32D pops the d-window and pushes
        // the f-window (f32).
        StackOp::I32ToF32F | StackOp::BitsToFF | StackOp::F64ToF32 | StackOp::F64ToF32D => 1,

        // f-window stores pop f0
        StackOp::StoreF32F | StackOp::StoreF32OffF(_) => -1,
        // f-window loads push f0
        StackOp::LoadF32F | StackOp::LoadF32OffF(_) => 1,

        _ => 0,
    }
}
