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

/// Post-fusion pass that detects float expression chains whose
/// intermediate values currently flow through the integer TOS window
/// (`t0`) and rewrites them to use the float TOS window (`f0`) instead.
///
/// Pattern matched: a chain starting with `FusedGetGetFMul` that feeds
/// into one or more `FusedGetAddrFMulFAdd` / `FusedGetAddrFMulFSub`
/// handlers and terminates in a `LocalSetL0/1/2` on the same accumulator.
/// Biquad's inner loop is exactly this shape.
///
/// The rewrite replaces each op with its `*_FW` counterpart so the
/// float accumulator stays in an FP/SIMD register across the entire
/// chain, eliminating all of the GPR↔FP crossings on the hot path.
pub fn float_window_rewrite(func: &mut StackFunction) {
    let ops = &mut func.ops;
    let len = ops.len();
    let is_target = compute_jump_targets(ops);
    let mut i = 0;
    while i < len {
        // Only start at a FusedGetGetFMul. The start can be a jump
        // target (e.g. biquad's filter starts right after the phase-wrap
        // branch) — the rewrite only needs the internal chain to be
        // straight-line.
        if !matches!(ops[i], StackOp::FusedGetGetFMul(_, _)) {
            i += 1;
            continue;
        }
        // Find the end of the chain: consecutive
        // FusedGetAddrFMulFAdd/FSub ops, terminating in LocalSetL0/1/2.
        // No jump targets allowed inside the chain.
        let mut j = i + 1;
        while j < len && !is_target[j] && matches!(
            ops[j],
            StackOp::FusedGetAddrFMulFAdd(_, _, _) | StackOp::FusedGetAddrFMulFSub(_, _, _)
        ) {
            j += 1;
        }
        let terminates = j > i + 1 && j < len && !is_target[j] && matches!(
            ops[j],
            StackOp::LocalSetL0 | StackOp::LocalSetL1 | StackOp::LocalSetL2
        );
        if !terminates {
            i += 1;
            continue;
        }
        // Rewrite ops[i..=j] to their float-window variants.
        if let StackOp::FusedGetGetFMul(a, b) = ops[i] {
            ops[i] = StackOp::FusedGetGetFMulFW(a, b);
        }
        for k in (i + 1)..j {
            let new_op = match &ops[k] {
                StackOp::FusedGetAddrFMulFAdd(a, s, o) => Some(StackOp::FusedGetAddrFMulFAddFW(*a, *s, *o)),
                StackOp::FusedGetAddrFMulFSub(a, s, o) => Some(StackOp::FusedGetAddrFMulFSubFW(*a, *s, *o)),
                _ => None,
            };
            if let Some(new_op) = new_op {
                ops[k] = new_op;
            }
        }
        let new_term = match &ops[j] {
            StackOp::LocalSetL0 => Some(StackOp::LocalSetL0FW),
            StackOp::LocalSetL1 => Some(StackOp::LocalSetL1FW),
            StackOp::LocalSetL2 => Some(StackOp::LocalSetL2FW),
            _ => None,
        };
        if let Some(new_op) = new_term {
            ops[j] = new_op;
        }
        i = j + 1;
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
            StackOp::FusedF32ConstFGtJumpIfZero(_, off) => Some(*off),
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
                ops[i] = StackOp::FusedTeeSliceStore32(n, s, idx);
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
                ops[i] = StackOp::FusedGetSet(a, b);
                ops[i+1] = StackOp::Nop;
                i += 2;
                continue;
            }
        }

        // local.set N + local.get N → local.tee N
        // The codegen emits `var x = expr; use(x)` as set+get; the tee
        // form keeps the value on the stack and writes to the local in
        // one op. Saves 1 op per var declaration that is immediately
        // read — significant in the FFT inner loop's theta/wr/wi chain.
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

        // local.addr s + local.get idx + slice.load32 + local.set dst → FusedAddrGetSliceLoad32Set
        // (already have FusedAddrGetSliceLoad32, but this adds the local.set)

        // === 6-instruction fusions ===

        // FFT twiddle-factor sequence:
        //   local.tee T; f32.cos; local.set C; local.get T; f32.sin; local.set S
        // → FusedTeeSinCosSet(T, C, S)
        //
        // Matches only when the LocalGet reads the same slot as the
        // LocalTee (i.e. the var is theta and is consumed by both cos
        // and sin). The single handler calls sincosf once instead of
        // cosf + sinf separately.
        if i + 5 < len && !spans_target(i, 6) {
            if let (
                StackOp::LocalTee(t),
                StackOp::CosF32,
                StackOp::LocalSet(c),
                StackOp::LocalGet(t2),
                StackOp::SinF32,
                StackOp::LocalSet(s),
            ) = (&ops[i], &ops[i+1], &ops[i+2], &ops[i+3], &ops[i+4], &ops[i+5]) {
                if *t == *t2 {
                    let t = *t; let c = *c; let s = *s;
                    ops[i] = StackOp::FusedTeeSinCosSet(t, c, s);
                    ops[i+1] = StackOp::Nop;
                    ops[i+2] = StackOp::Nop;
                    ops[i+3] = StackOp::Nop;
                    ops[i+4] = StackOp::Nop;
                    ops[i+5] = StackOp::Nop;
                    i += 6;
                    continue;
                }
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

        // Float-window mirror: fw.local.get a + fw.fused.addr_load32off s o + fw.fused.fmul_fadd
        //                    → fw.fused.get_addr_fmul_fadd (biquad hot-path FMA term).
        if i + 2 < len && !spans_target(i, 3) {
            if let (
                StackOp::LocalGetF(a),
                StackOp::FusedAddrLoad32OffF(s, o),
                StackOp::FusedFMulFAddF,
            ) = (&ops[i], &ops[i+1], &ops[i+2])
            {
                let a = *a; let s = *s; let o = *o;
                ops[i] = StackOp::FusedGetAddrFMulFAddF(a, s, o);
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

        // Float-window mirror of FusedGetAddrFMulFSub.
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
                ops[i] = StackOp::FusedAddrLoad32OffSet(s, o, dst);
                ops[i+1] = StackOp::Nop;
                i += 2;
                continue;
            }
        }

        // f32.const v + f32.gt + jump_if_zero off → FusedF32ConstFGtJumpIfZero
        if i + 2 < len && !spans_target(i, 3) {
            if let (StackOp::F32Const(v), StackOp::FGt, StackOp::JumpIfZero(off)) =
                (&ops[i], &ops[i+1], &ops[i+2])
            {
                let v = *v;
                let new_off = *off + 2; // adjust: original jump at i+2, fused at i
                ops[i] = StackOp::FusedF32ConstFGtJumpIfZero(v, new_off);
                ops[i+1] = StackOp::Nop;
                ops[i+2] = StackOp::Nop;
                i += 3;
                continue;
            }
        }

        // Float-window mirror: fw.f32.const v + fw.f32.gt + jump_if_zero off
        //                    → FusedF32ConstFGtJumpIfZeroF.
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

        // fused.get_get_fadd a b + local.set dst → FusedGetGetFAddSet
        if i + 1 < len && !spans_target(i, 2) {
            if let (StackOp::FusedGetGetFAdd(a, b), StackOp::LocalSet(dst)) =
                (&ops[i], &ops[i+1])
            {
                let a = *a; let b = *b; let dst = *dst;
                ops[i] = StackOp::FusedGetGetFAddSet(a, b, dst);
                ops[i+1] = StackOp::Nop;
                i += 2;
                continue;
            }
        }

        // Float-window mirror: fused.get_get_f* + fw.local.set dst →
        // FusedGetGetF*Set. The 3-address handlers read both operands
        // directly from locals[] and write to locals[] without touching
        // either TOS window, so they work unchanged for both windows.
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
        // + fw.f32.store → FusedAddrImmGetStore32. The existing handler reads
        // locals[src] low 32 bits and writes them to the computed address,
        // which works correctly for f32 bit patterns too.
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

        // Float-window mirror: fw.local.get a + fw.local.get b + fw.f32.mul
        //                    → fw.fused.get_get_fmul.
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

        // Float-window mirror: fw.local.get a + fw.local.get b + fw.f32.add
        //                    → fw.fused.get_get_fadd.
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

        // Float-window mirror: fw.local.get a + fw.local.get b + fw.f32.sub
        //                    → fw.fused.get_get_fsub.
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

        // Float-window mirror: fw.local.get a + fw.f32.mul → fw.fused.get_fmul.
        if i + 1 < len && !spans_target(i, 2) {
            if let (StackOp::LocalGetF(a), StackOp::FMulF) = (&ops[i], &ops[i+1]) {
                let a = *a;
                ops[i] = StackOp::FusedGetFMulF(a);
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

        // Float-window mirror.
        if i + 1 < len && !spans_target(i, 2) {
            if let (StackOp::LocalGetF(a), StackOp::FAddF) = (&ops[i], &ops[i+1]) {
                let a = *a;
                ops[i] = StackOp::FusedGetFAddF(a);
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

        // Float-window mirror.
        if i + 1 < len && !spans_target(i, 2) {
            if let (StackOp::LocalGetF(a), StackOp::FSubF) = (&ops[i], &ops[i+1]) {
                let a = *a;
                ops[i] = StackOp::FusedGetFSubF(a);
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

        // Float-window mirror.
        if i + 1 < len && !spans_target(i, 2) {
            if matches!(ops[i], StackOp::FMulF) && matches!(ops[i+1], StackOp::FAddF) {
                ops[i] = StackOp::FusedFMulFAddF;
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

        // Float-window mirror.
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
            StackOp::FusedF32ConstFGtJumpIfZero(v, off) => {
                let target_old = (old as i64 + 1 + *off as i64) as usize;
                let target_new = new_idx[target_old];
                let new_off = target_new as i32 - new_idx[old] as i32 - 1;
                StackOp::FusedF32ConstFGtJumpIfZero(*v, new_off)
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
