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
    is_target
}

/// Fuse instruction sequences into superinstructions.
///
/// Hot local awareness lives in the handler, not the fuser. Every
/// fused op handler that reads or writes a local slot dispatches at
/// runtime via READ_I/WRITE_I/READ_F_OFF/WRITE_F_OFF macros (see
/// stack_interp.c), so a slot in {0, 1, 2} is transparently read from
/// l0/l1/l2 if it's hot and from `locals[n]` otherwise. The fusion
/// pass is therefore oblivious to which slots are hot — it just emits
/// the most aggressive fusion it can find.
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

        // fw.local.tee N + fw.drop → fw.local.set N
        if i + 1 < len && !spans_target(i, 2) {
            if let StackOp::LocalTeeF(n) = ops[i] {
                if matches!(ops[i + 1], StackOp::DropF) {
                    ops[i] = StackOp::LocalSetF(n);
                    ops[i + 1] = StackOp::Nop;
                    i += 2;
                    continue;
                }
            }
        }

        // local.tee N + fused.addr_get_sstore32 s idx → FusedTeeSliceStore32
        // (butterfly store pattern in FFT hot loop)
        if i + 1 < len && !spans_target(i, 2) {
            if let (StackOp::LocalTee(n), StackOp::FusedAddrGetSliceStore32(s, idx)) =
                (&ops[i], &ops[i + 1])
            {
                let n = *n; let s = *s; let idx = *idx;
                // The tee writes locals[n] via memory, and the slice store
                // reads locals[idx] as the index. Skip if either is hot.
                ops[i] = StackOp::FusedTeeSliceStore32(n, s, idx);
                ops[i + 1] = StackOp::Nop;
                i += 2;
                continue;
            }
        }

        // Float-window mirror of the above: fw.local.tee N +
        // fw.fused.addr_get_sstore32 s idx → FusedTeeSliceStore32F.
        // The FFT butterfly hits this four times per iteration.
        if i + 1 < len && !spans_target(i, 2) {
            if let (StackOp::LocalTeeF(n), StackOp::FusedAddrGetSliceStore32F(s, idx)) =
                (&ops[i], &ops[i + 1])
            {
                let n = *n; let s = *s; let idx = *idx;
                ops[i] = StackOp::FusedTeeSliceStore32F(n, s, idx);
                ops[i + 1] = StackOp::Nop;
                i += 2;
                continue;
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

        // fw.local.get + fw.drop → nop
        if i + 1 < len && !spans_target(i, 2) {
            if matches!(
                ops[i],
                StackOp::LocalGetF(_)
                    | StackOp::LocalGetL0F
                    | StackOp::LocalGetL1F
                    | StackOp::LocalGetL2F
            ) && matches!(ops[i + 1], StackOp::DropF)
            {
                ops[i] = StackOp::Nop;
                ops[i + 1] = StackOp::Nop;
                i += 2;
                continue;
            }
        }

        // local.set + i64.const + drop → local.set (dead Var init result)
        if i + 2 < len && !spans_target(i, 3) {
            if let (StackOp::LocalSet(_), StackOp::I64Const(_), StackOp::Drop) =
                (&ops[i], &ops[i+1], &ops[i+2])
            {
                // Keep the LocalSet, nop out the dead const+drop
                ops[i+1] = StackOp::Nop;
                ops[i+2] = StackOp::Nop;
                i += 3;
                continue;
            }
        }

        // local.get a + local.set b → FusedGetSet (variable move)
        if i + 1 < len && !spans_target(i, 2) {
            if let (StackOp::LocalGet(a), StackOp::LocalSet(b)) = (&ops[i], &ops[i+1]) {
                let a = *a; let b = *b;
                // FusedGetSet reads locals[a] and writes locals[b] via
                // memory. Hot slots must stay raw.
                ops[i] = StackOp::FusedGetSet(a, b);
                ops[i+1] = StackOp::Nop;
                i += 2;
                continue;
            }
        }

        // local.set N + local.get N → local.tee N
        // The tee handler writes locals[n] from the TOS window, which
        // would leave l_n stale — skip on hot slots.
        if i + 1 < len && !spans_target(i, 2) {
            if let (StackOp::LocalSet(a), StackOp::LocalGet(b)) = (&ops[i], &ops[i+1]) {
                if *a == *b {
                    let n = *a;
                    ops[i] = StackOp::LocalTee(n);
                    ops[i+1] = StackOp::Nop;
                    i += 2;
                    continue;
                }
            }
        }

        // Float-window mirror: fw.local.set N + fw.local.get N → fw.local.tee N
        if i + 1 < len && !spans_target(i, 2) {
            if let (StackOp::LocalSetF(a), StackOp::LocalGetF(b)) = (&ops[i], &ops[i+1]) {
                if *a == *b {
                    let n = *a;
                    ops[i] = StackOp::LocalTeeF(n);
                    ops[i+1] = StackOp::Nop;
                    i += 2;
                    continue;
                }
            }
        }

        // local.addr s + local.get idx + slice.load32 + local.set dst → FusedAddrGetSliceLoad32Set
        // (already have FusedAddrGetSliceLoad32, but this adds the local.set)

        // (The FusedTeeSinCosSet fusion for the FFT twiddle loop
        // matched int-window CosF32/SinF32, which no longer exist.
        // An F-variant mirror is a future optimization.)

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
                // Reads locals[a], locals[b] via memory — skip on any hot.
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

        // Float-window: fw.local.get a + fw.fused.addr_load32off s o + fw.fused.fmul_fadd
        //              → fw.fused.get_addr_fmul_fadd (biquad hot-path FMA term).
        if i + 2 < len && !spans_target(i, 3) {
            if let (
                StackOp::LocalGetF(a),
                StackOp::FusedAddrLoad32OffF(s, o),
                StackOp::FusedFMulFAddF,
            ) = (&ops[i], &ops[i+1], &ops[i+2])
            {
                let a = *a; let s = *s; let o = *o;
                // Reads locals[a] via memory — skip if hot.
                ops[i] = StackOp::FusedGetAddrFMulFAddF(a, s, o);
                ops[i+1] = StackOp::Nop;
                ops[i+2] = StackOp::Nop;
                i += 3;
                continue;
            }
        }

        // Float-window FusedGetAddrFMulFSub.
        if i + 2 < len && !spans_target(i, 3) {
            if let (
                StackOp::LocalGetF(a),
                StackOp::FusedAddrLoad32OffF(s, o),
                StackOp::FusedFMulFSubF,
            ) = (&ops[i], &ops[i+1], &ops[i+2])
            {
                let a = *a; let s = *s; let o = *o;
                ops[i] = StackOp::FusedGetAddrFMulFSubF(a, s, o);
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
                // Writes locals[dst] via memory — skip if hot.
                ops[i] = StackOp::FusedAddrLoad32OffSet(s, o, dst);
                ops[i+1] = StackOp::Nop;
                i += 2;
                continue;
            }
        }

        // Float-window: fw.f32.const v + fw.f32.gt + jump_if_zero off
        //              → FusedF32ConstFGtJumpIfZeroF.
        if i + 2 < len && !spans_target(i, 3) {
            if let (StackOp::F32ConstF(v), StackOp::FGtF, StackOp::JumpIfZero(off)) =
                (&ops[i], &ops[i+1], &ops[i+2])
            {
                let v = *v;
                let new_off = *off + 2;
                ops[i] = StackOp::FusedF32ConstFGtJumpIfZeroF(v, new_off);
                ops[i+1] = StackOp::Nop;
                ops[i+2] = StackOp::Nop;
                i += 3;
                continue;
            }
        }

        // Float-window: fused.get_get_f* + fw.local.set dst → FusedGetGetF*Set.
        // The 3-address handlers read both operands directly from locals[]
        // and write to locals[] without touching either TOS window, so
        // they work unchanged — but the memory write means we have to
        // skip them when dst is a hot slot.
        if i + 1 < len && !spans_target(i, 2) {
            if let (StackOp::FusedGetGetFAddF(a, b), StackOp::LocalSetF(dst)) =
                (&ops[i], &ops[i+1])
            {
                let a = *a; let b = *b; let dst = *dst;
                ops[i] = StackOp::FusedGetGetFAddSet(a, b, dst);
                ops[i+1] = StackOp::Nop;
                i += 2;
                continue;
            }
        }
        if i + 1 < len && !spans_target(i, 2) {
            if let (StackOp::FusedGetGetFSubF(a, b), StackOp::LocalSetF(dst)) =
                (&ops[i], &ops[i+1])
            {
                let a = *a; let b = *b; let dst = *dst;
                ops[i] = StackOp::FusedGetGetFSubSet(a, b, dst);
                ops[i+1] = StackOp::Nop;
                i += 2;
                continue;
            }
        }
        if i + 1 < len && !spans_target(i, 2) {
            if let (StackOp::FusedGetGetFMulF(a, b), StackOp::LocalSetF(dst)) =
                (&ops[i], &ops[i+1])
            {
                let a = *a; let b = *b; let dst = *dst;
                ops[i] = StackOp::FusedGetGetFMulSet(a, b, dst);
                ops[i+1] = StackOp::Nop;
                i += 2;
                continue;
            }
        }

        // Float-window mirror: fw.fused.addr_load32off s o + fw.local.set dst
        // → FusedAddrLoad32OffSet. The target handler loads 32 bits from
        // memory directly into locals[dst] and doesn't touch either TOS
        // window, so it's reused unchanged.
        if i + 1 < len && !spans_target(i, 2) {
            if let (StackOp::FusedAddrLoad32OffF(s, o), StackOp::LocalSetF(dst)) =
                (&ops[i], &ops[i+1])
            {
                let s = *s; let o = *o; let dst = *dst;
                ops[i] = StackOp::FusedAddrLoad32OffSet(s, o, dst);
                ops[i+1] = StackOp::Nop;
                i += 2;
                continue;
            }
        }

        // Float-window mirror: local.addr s + i64.add_imm off + fw.local.get src
        // + fw.f32.store → FusedAddrImmGetStore32. The generic handler reads
        // the low 32 bits of locals[src] which works for both int and f32
        // bit patterns.
        if i + 3 < len && !spans_target(i, 4) {
            if let (
                StackOp::LocalAddr(s),
                StackOp::IAddImm(off),
                StackOp::LocalGetF(src),
                StackOp::StoreF32F,
            ) = (&ops[i], &ops[i+1], &ops[i+2], &ops[i+3])
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

        // fused.addr_load32off_set s src_off tmp + fused.addr_imm_get_store32 s dst_off tmp → FusedFieldCopy32
        if i + 1 < len && !spans_target(i, 2) {
            if let (StackOp::FusedAddrLoad32OffSet(s1, src_off, tmp1),
                    StackOp::FusedAddrImmGetStore32(s2, dst_off, tmp2)) =
                (&ops[i], &ops[i+1])
            {
                if s1 == s2 && tmp1 == tmp2 {
                    let s = *s1; let src_off = *src_off; let dst_off = *dst_off;
                    ops[i] = StackOp::FusedFieldCopy32(s, src_off, dst_off);
                    ops[i+1] = StackOp::Nop;
                    i += 2;
                    continue;
                }
            }
        }

        // local.get src + i64.add_imm v + local.set dst → FusedGetAddImmSet
        if i + 2 < len && !spans_target(i, 3) {
            if let (StackOp::LocalGet(src), StackOp::IAddImm(v), StackOp::LocalSet(dst)) =
                (&ops[i], &ops[i+1], &ops[i+2])
            {
                let src = *src; let v = *v; let dst = *dst;
                // Reads locals[src], writes locals[dst] via memory.
                ops[i] = StackOp::FusedGetAddImmSet(src, v, dst);
                ops[i+1] = StackOp::Nop;
                ops[i+2] = StackOp::Nop;
                i += 3;
                continue;
            }
        }

        // === 3-instruction fusions ===

        // Float-window: fw.local.get a + fw.local.get b + fw.f32.mul
        //             → fw.fused.get_get_fmul.
        if i + 2 < len && !spans_target(i, 3) {
            if let (StackOp::LocalGetF(a), StackOp::LocalGetF(b), StackOp::FMulF) =
                (&ops[i], &ops[i+1], &ops[i+2])
            {
                let a = *a; let b = *b;
                ops[i] = StackOp::FusedGetGetFMulF(a, b);
                ops[i+1] = StackOp::Nop;
                ops[i+2] = StackOp::Nop;
                i += 3;
                continue;
            }
        }

        // Float-window: fw.local.get a + fw.local.get b + fw.f32.add
        //             → fw.fused.get_get_fadd.
        if i + 2 < len && !spans_target(i, 3) {
            if let (StackOp::LocalGetF(a), StackOp::LocalGetF(b), StackOp::FAddF) =
                (&ops[i], &ops[i+1], &ops[i+2])
            {
                let a = *a; let b = *b;
                ops[i] = StackOp::FusedGetGetFAddF(a, b);
                ops[i+1] = StackOp::Nop;
                ops[i+2] = StackOp::Nop;
                i += 3;
                continue;
            }
        }

        // Float-window: fw.local.get a + fw.local.get b + fw.f32.sub
        //             → fw.fused.get_get_fsub.
        if i + 2 < len && !spans_target(i, 3) {
            if let (StackOp::LocalGetF(a), StackOp::LocalGetF(b), StackOp::FSubF) =
                (&ops[i], &ops[i+1], &ops[i+2])
            {
                let a = *a; let b = *b;
                ops[i] = StackOp::FusedGetGetFSubF(a, b);
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

        // Float-window mirror: local.addr slot + fw.f32.load offset=off
        //                    → fw.fused.addr_load32off.
        if i + 1 < len && !spans_target(i, 2) {
            if let (StackOp::LocalAddr(slot), StackOp::LoadF32OffF(off)) =
                (&ops[i], &ops[i+1])
            {
                let slot = *slot; let off = *off;
                ops[i] = StackOp::FusedAddrLoad32OffF(slot, off);
                ops[i+1] = StackOp::Nop;
                i += 2;
                continue;
            }
        }

        // === 2-instruction fusions ===

        // Float-window: fw.local.get a + fw.f32.mul → fw.fused.get_fmul.
        if i + 1 < len && !spans_target(i, 2) {
            if let (StackOp::LocalGetF(a), StackOp::FMulF) = (&ops[i], &ops[i+1]) {
                let a = *a;
                ops[i] = StackOp::FusedGetFMulF(a);
                ops[i+1] = StackOp::Nop;
                i += 2;
                continue;
            }
        }

        // Float-window: fw.local.get a + fw.f32.add → fw.fused.get_fadd.
        if i + 1 < len && !spans_target(i, 2) {
            if let (StackOp::LocalGetF(a), StackOp::FAddF) = (&ops[i], &ops[i+1]) {
                let a = *a;
                ops[i] = StackOp::FusedGetFAddF(a);
                ops[i+1] = StackOp::Nop;
                i += 2;
                continue;
            }
        }

        // Float-window: fw.local.get a + fw.f32.sub → fw.fused.get_fsub.
        if i + 1 < len && !spans_target(i, 2) {
            if let (StackOp::LocalGetF(a), StackOp::FSubF) = (&ops[i], &ops[i+1]) {
                let a = *a;
                ops[i] = StackOp::FusedGetFSubF(a);
                ops[i+1] = StackOp::Nop;
                i += 2;
                continue;
            }
        }

        // Float-window: fw.f32.mul + fw.f32.add → fw.fused.fmul_fadd.
        if i + 1 < len && !spans_target(i, 2) {
            if matches!(ops[i], StackOp::FMulF) && matches!(ops[i+1], StackOp::FAddF) {
                ops[i] = StackOp::FusedFMulFAddF;
                ops[i+1] = StackOp::Nop;
                i += 2;
                continue;
            }
        }

        // Float-window: fw.f32.mul + fw.f32.sub → fw.fused.fmul_fsub.
        if i + 1 < len && !spans_target(i, 2) {
            if matches!(ops[i], StackOp::FMulF) && matches!(ops[i+1], StackOp::FSubF) {
                ops[i] = StackOp::FusedFMulFSubF;
                ops[i+1] = StackOp::Nop;
                i += 2;
                continue;
            }
        }

        // local.get a + local.set b → move (common from codegen temporaries)
        // But only if a != b (otherwise it's a no-op, but leave it for now).

        // i64.const v + local.set n → FusedConstSet[Ln]
        //
        // When n is a hot slot, emit the register-specialized variant
        // that writes l_n directly; otherwise the generic memory-backed
        // FusedConstSet is correct.
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
            StackOp::FusedF32ConstFGtJumpIfZeroF(v, off) => {
                let target_old = (old as i64 + 1 + *off as i64) as usize;
                let target_new = new_idx[target_old];
                let new_off = target_new as i32 - new_idx[old] as i32 - 1;
                StackOp::FusedF32ConstFGtJumpIfZeroF(*v, new_off)
            }
            other => other.clone(),
        };
        new_ops.push(adjusted);
    }

    func.ops = new_ops;
}

