//! Peephole optimizer for stack IR.
//!
//! Fuses common instruction sequences into single superinstructions,
//! reducing dispatch count. This is the key optimization from the
//! Silverfir-nano interpreter design.

use crate::stack_ir::{StackOp, StackFunction};

/// Run all optimization passes on a function.
pub fn optimize(func: &mut StackFunction) {
    // Multiple passes since each pass may expose new opportunities.
    for _ in 0..3 {
        let before = func.ops.len();
        fuse(func);
        strip_nops(func);
        if func.ops.len() == before {
            break;
        }
    }
}

/// Compute the set of instruction indices that are jump targets.
fn compute_jump_targets(ops: &[StackOp]) -> Vec<bool> {
    let len = ops.len();
    let mut is_target = vec![false; len];
    for (i, op) in ops.iter().enumerate() {
        let off = match op {
            StackOp::Jump(off) | StackOp::JumpIfZero(off) | StackOp::JumpIfNotZero(off) => Some(*off),
            _ => None,
        };
        if let Some(off) = off {
            let target = (i as i64 + 1 + off as i64) as usize;
            if target < len {
                is_target[target] = true;
            }
        }
    }
    is_target
}

/// Fuse instruction sequences into superinstructions.
fn fuse(func: &mut StackFunction) {
    let ops = &mut func.ops;
    let len = ops.len();
    let is_target = compute_jump_targets(ops);
    let mut i = 0;

    // Helper: check if any instruction in range (i+1..i+n) is a jump target.
    let spans_target = |start: usize, count: usize| -> bool {
        for j in 1..count {
            if start + j < len && is_target[start + j] {
                return true;
            }
        }
        false
    };

    while i < len {
        // === Dead code elimination ===

        // i64.const + drop → nop (dead Var initialization result)
        if i + 1 < len && !spans_target(i, 2) {
            if matches!(ops[i], StackOp::I64Const(_)) && matches!(ops[i + 1], StackOp::Drop) {
                ops[i] = StackOp::Nop;
                ops[i + 1] = StackOp::Nop;
                i += 2;
                continue;
            }
        }

        // local.tee N + drop → local.set N
        if i + 1 < len && !spans_target(i, 2) {
            if let StackOp::LocalTee(n) = ops[i] {
                if matches!(ops[i + 1], StackOp::Drop) {
                    ops[i] = StackOp::LocalSet(n);
                    ops[i + 1] = StackOp::Nop;
                    i += 2;
                    continue;
                }
            }
        }

        // local.get + drop → nop (dead variable read)
        if i + 1 < len && !spans_target(i, 2) {
            if matches!(ops[i], StackOp::LocalGet(_)) && matches!(ops[i + 1], StackOp::Drop) {
                ops[i] = StackOp::Nop;
                ops[i + 1] = StackOp::Nop;
                i += 2;
                continue;
            }
        }

        // === 4-instruction fusions ===

        // local.addr s + i64.add_imm off + local.get src + i32.store → FusedAddrImmGetStore32
        if i + 3 < len && !spans_target(i, 4) {
            if let (StackOp::LocalAddr(s), StackOp::IAddImm(off), StackOp::LocalGet(src), StackOp::Store32) =
                (&ops[i], &ops[i+1], &ops[i+2], &ops[i+3])
            {
                let s = *s; let off = *off; let src = *src;
                ops[i] = StackOp::FusedAddrImmGetStore32(s, off, src);
                ops[i+1] = StackOp::Nop;
                ops[i+2] = StackOp::Nop;
                ops[i+3] = StackOp::Nop;
                i += 4;
                continue;
            }
        }

        // local.get a + local.get b + i64.lt_s + jump_if_zero off → FusedGetGetILtJumpIfZero
        if i + 3 < len && !spans_target(i, 4) {
            if let (StackOp::LocalGet(a), StackOp::LocalGet(b), StackOp::ILt, StackOp::JumpIfZero(off)) =
                (&ops[i], &ops[i+1], &ops[i+2], &ops[i+3])
            {
                let a = *a; let b = *b;
                // Adjust offset: original JumpIfZero at i+3 targets i+3+1+off.
                // Fused instruction at i targets i+1+new_off. So new_off = off + 3.
                let new_off = *off + 3;
                ops[i] = StackOp::FusedGetGetILtJumpIfZero(a, b, new_off);
                ops[i+1] = StackOp::Nop;
                ops[i+2] = StackOp::Nop;
                ops[i+3] = StackOp::Nop;
                i += 4;
                continue;
            }
        }

        // local.get a + fused.addr_load32off s o + fused.fmul_fadd → FusedGetAddrFMulFAdd
        if i + 2 < len && !spans_target(i, 3) {
            if let (StackOp::LocalGet(a), StackOp::FusedAddrLoad32Off(s, o), StackOp::FusedFMulFAdd) =
                (&ops[i], &ops[i+1], &ops[i+2])
            {
                let a = *a; let s = *s; let o = *o;
                ops[i] = StackOp::FusedGetAddrFMulFAdd(a, s, o);
                ops[i+1] = StackOp::Nop;
                ops[i+2] = StackOp::Nop;
                i += 3;
                continue;
            }
        }

        // local.get a + fused.addr_load32off s o + fused.fmul_fsub → FusedGetAddrFMulFSub
        if i + 2 < len && !spans_target(i, 3) {
            if let (StackOp::LocalGet(a), StackOp::FusedAddrLoad32Off(s, o), StackOp::FusedFMulFSub) =
                (&ops[i], &ops[i+1], &ops[i+2])
            {
                let a = *a; let s = *s; let o = *o;
                ops[i] = StackOp::FusedGetAddrFMulFSub(a, s, o);
                ops[i+1] = StackOp::Nop;
                ops[i+2] = StackOp::Nop;
                i += 3;
                continue;
            }
        }

        // fused.addr_load32off s o + local.set dst → FusedAddrLoad32OffSet
        if i + 1 < len && !spans_target(i, 2) {
            if let (StackOp::FusedAddrLoad32Off(s, o), StackOp::LocalSet(dst)) =
                (&ops[i], &ops[i+1])
            {
                let s = *s; let o = *o; let dst = *dst;
                ops[i] = StackOp::FusedAddrLoad32OffSet(s, o, dst);
                ops[i+1] = StackOp::Nop;
                i += 2;
                continue;
            }
        }

        // local.get src + i64.add_imm v + local.set dst → FusedGetAddImmSet
        if i + 2 < len && !spans_target(i, 3) {
            if let (StackOp::LocalGet(src), StackOp::IAddImm(v), StackOp::LocalSet(dst)) =
                (&ops[i], &ops[i+1], &ops[i+2])
            {
                let src = *src; let v = *v; let dst = *dst;
                ops[i] = StackOp::FusedGetAddImmSet(src, v, dst);
                ops[i+1] = StackOp::Nop;
                ops[i+2] = StackOp::Nop;
                i += 3;
                continue;
            }
        }

        // === 3-instruction fusions ===

        // local.get a + local.get b + f32.mul → FusedGetGetFMul
        if i + 2 < len && !spans_target(i, 3) {
            if let (StackOp::LocalGet(a), StackOp::LocalGet(b), StackOp::FMul) =
                (&ops[i], &ops[i+1], &ops[i+2])
            {
                let a = *a; let b = *b;
                ops[i] = StackOp::FusedGetGetFMul(a, b);
                ops[i+1] = StackOp::Nop;
                ops[i+2] = StackOp::Nop;
                i += 3;
                continue;
            }
        }

        // local.get a + local.get b + f32.add → FusedGetGetFAdd
        if i + 2 < len && !spans_target(i, 3) {
            if let (StackOp::LocalGet(a), StackOp::LocalGet(b), StackOp::FAdd) =
                (&ops[i], &ops[i+1], &ops[i+2])
            {
                let a = *a; let b = *b;
                ops[i] = StackOp::FusedGetGetFAdd(a, b);
                ops[i+1] = StackOp::Nop;
                ops[i+2] = StackOp::Nop;
                i += 3;
                continue;
            }
        }

        // local.get a + local.get b + f32.sub → FusedGetGetFSub
        if i + 2 < len && !spans_target(i, 3) {
            if let (StackOp::LocalGet(a), StackOp::LocalGet(b), StackOp::FSub) =
                (&ops[i], &ops[i+1], &ops[i+2])
            {
                let a = *a; let b = *b;
                ops[i] = StackOp::FusedGetGetFSub(a, b);
                ops[i+1] = StackOp::Nop;
                ops[i+2] = StackOp::Nop;
                i += 3;
                continue;
            }
        }

        // local.get a + local.get b + i64.add → FusedGetGetIAdd
        if i + 2 < len && !spans_target(i, 3) {
            if let (StackOp::LocalGet(a), StackOp::LocalGet(b), StackOp::IAdd) =
                (&ops[i], &ops[i+1], &ops[i+2])
            {
                let a = *a; let b = *b;
                ops[i] = StackOp::FusedGetGetIAdd(a, b);
                ops[i+1] = StackOp::Nop;
                ops[i+2] = StackOp::Nop;
                i += 3;
                continue;
            }
        }

        // local.get a + local.get b + i64.lt_s → FusedGetGetILt
        if i + 2 < len && !spans_target(i, 3) {
            if let (StackOp::LocalGet(a), StackOp::LocalGet(b), StackOp::ILt) =
                (&ops[i], &ops[i+1], &ops[i+2])
            {
                let a = *a; let b = *b;
                ops[i] = StackOp::FusedGetGetILt(a, b);
                ops[i+1] = StackOp::Nop;
                ops[i+2] = StackOp::Nop;
                i += 3;
                continue;
            }
        }

        // local.addr slot + i32.load offset=off → FusedAddrLoad32Off
        if i + 1 < len && !spans_target(i, 2) {
            if let (StackOp::LocalAddr(slot), StackOp::Load32Off(off)) =
                (&ops[i], &ops[i+1])
            {
                let slot = *slot; let off = *off;
                ops[i] = StackOp::FusedAddrLoad32Off(slot, off);
                ops[i+1] = StackOp::Nop;
                i += 2;
                continue;
            }
        }

        // === 2-instruction fusions ===

        // local.get a + f32.mul → FusedGetFMul (one operand already on stack)
        if i + 1 < len && !spans_target(i, 2) {
            if let (StackOp::LocalGet(a), StackOp::FMul) = (&ops[i], &ops[i+1]) {
                let a = *a;
                ops[i] = StackOp::FusedGetFMul(a);
                ops[i+1] = StackOp::Nop;
                i += 2;
                continue;
            }
        }

        // local.get a + f32.add → FusedGetFAdd
        if i + 1 < len && !spans_target(i, 2) {
            if let (StackOp::LocalGet(a), StackOp::FAdd) = (&ops[i], &ops[i+1]) {
                let a = *a;
                ops[i] = StackOp::FusedGetFAdd(a);
                ops[i+1] = StackOp::Nop;
                i += 2;
                continue;
            }
        }

        // local.get a + f32.sub → FusedGetFSub
        if i + 1 < len && !spans_target(i, 2) {
            if let (StackOp::LocalGet(a), StackOp::FSub) = (&ops[i], &ops[i+1]) {
                let a = *a;
                ops[i] = StackOp::FusedGetFSub(a);
                ops[i+1] = StackOp::Nop;
                i += 2;
                continue;
            }
        }

        // f32.mul + f32.add → FusedFMulFAdd (multiply-accumulate)
        if i + 1 < len && !spans_target(i, 2) {
            if matches!(ops[i], StackOp::FMul) && matches!(ops[i+1], StackOp::FAdd) {
                ops[i] = StackOp::FusedFMulFAdd;
                ops[i+1] = StackOp::Nop;
                i += 2;
                continue;
            }
        }

        // f32.mul + f32.sub → FusedFMulFSub (multiply-subtract)
        if i + 1 < len && !spans_target(i, 2) {
            if matches!(ops[i], StackOp::FMul) && matches!(ops[i+1], StackOp::FSub) {
                ops[i] = StackOp::FusedFMulFSub;
                ops[i+1] = StackOp::Nop;
                i += 2;
                continue;
            }
        }

        // local.get a + local.set b → move (common from codegen temporaries)
        // But only if a != b (otherwise it's a no-op, but leave it for now).

        // i64.const v + local.set n → FusedConstSet
        if i + 1 < len && !spans_target(i, 2) {
            if let (StackOp::I64Const(v), StackOp::LocalSet(n)) = (&ops[i], &ops[i+1]) {
                let v = *v; let n = *n;
                ops[i] = StackOp::FusedConstSet(v, n);
                ops[i+1] = StackOp::Nop;
                i += 2;
                continue;
            }
        }

        // f32.const v + local.set n → FusedF32ConstSet
        if i + 1 < len && !spans_target(i, 2) {
            if let (StackOp::F32Const(v), StackOp::LocalSet(n)) = (&ops[i], &ops[i+1]) {
                let v = *v; let n = *n;
                ops[i] = StackOp::FusedF32ConstSet(v, n);
                ops[i+1] = StackOp::Nop;
                i += 2;
                continue;
            }
        }

        // local.addr slot + local.get idx + slice.load32 → FusedAddrGetSliceLoad32
        if i + 2 < len && !spans_target(i, 3) {
            if let (StackOp::LocalAddr(slot), StackOp::LocalGet(idx), StackOp::SliceLoad32) =
                (&ops[i], &ops[i+1], &ops[i+2])
            {
                let slot = *slot; let idx = *idx;
                ops[i] = StackOp::FusedAddrGetSliceLoad32(slot, idx);
                ops[i+1] = StackOp::Nop;
                ops[i+2] = StackOp::Nop;
                i += 3;
                continue;
            }
        }

        i += 1;
    }
}

/// Remove Nop instructions and adjust jump offsets.
fn strip_nops(func: &mut StackFunction) {
    let old_ops = &func.ops;
    let len = old_ops.len();

    // Build mapping from old index to new index.
    let mut new_idx = vec![0usize; len];
    let mut new_pos = 0usize;
    for (old, op) in old_ops.iter().enumerate() {
        new_idx[old] = new_pos;
        if !matches!(op, StackOp::Nop) {
            new_pos += 1;
        }
    }
    let new_len = new_pos;

    if new_len == len {
        return; // Nothing to strip.
    }

    // Build new ops, adjusting jump offsets.
    let mut new_ops = Vec::with_capacity(new_len);
    for (old, op) in old_ops.iter().enumerate() {
        if matches!(op, StackOp::Nop) {
            continue;
        }
        let adjusted = match op {
            StackOp::Jump(off) => {
                let target_old = (old as i64 + 1 + *off as i64) as usize;
                let target_new = new_idx[target_old];
                let new_off = target_new as i32 - new_idx[old] as i32 - 1;
                StackOp::Jump(new_off)
            }
            StackOp::JumpIfZero(off) => {
                let target_old = (old as i64 + 1 + *off as i64) as usize;
                let target_new = new_idx[target_old];
                let new_off = target_new as i32 - new_idx[old] as i32 - 1;
                StackOp::JumpIfZero(new_off)
            }
            StackOp::JumpIfNotZero(off) => {
                let target_old = (old as i64 + 1 + *off as i64) as usize;
                let target_new = new_idx[target_old];
                let new_off = target_new as i32 - new_idx[old] as i32 - 1;
                StackOp::JumpIfNotZero(new_off)
            }
            StackOp::FusedGetGetILtJumpIfZero(a, b, off) => {
                let target_old = (old as i64 + 1 + *off as i64) as usize;
                let target_new = new_idx[target_old];
                let new_off = target_new as i32 - new_idx[old] as i32 - 1;
                StackOp::FusedGetGetILtJumpIfZero(*a, *b, new_off)
            }
            other => other.clone(),
        };
        new_ops.push(adjusted);
    }

    func.ops = new_ops;
}
