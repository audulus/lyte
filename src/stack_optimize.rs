//! Peephole optimizer for stack IR.
//!
//! Fuses common instruction sequences into single superinstructions,
//! reducing dispatch count. This is the key optimization from the
//! Silverfir-nano interpreter design.

use crate::stack_ir::{StackFunction, StackOp};

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

fn make_fused_get_set_f(bytes: &[u8]) -> StackOp {
    match bytes.len() {
        2 => StackOp::FusedGetSetF(bytes[0], bytes[1]),
        4 => StackOp::FusedGetSet2F([bytes[0], bytes[1], bytes[2], bytes[3]]),
        6 => StackOp::FusedGetSet3F([bytes[0], bytes[1], bytes[2], bytes[3], bytes[4], bytes[5]]),
        8 => StackOp::FusedGetSet4F([
            bytes[0], bytes[1], bytes[2], bytes[3], bytes[4], bytes[5], bytes[6], bytes[7],
        ]),
        10 => StackOp::FusedGetSet5F([
            bytes[0], bytes[1], bytes[2], bytes[3], bytes[4], bytes[5], bytes[6], bytes[7],
            bytes[8], bytes[9],
        ]),
        12 => StackOp::FusedGetSet6F([
            bytes[0], bytes[1], bytes[2], bytes[3], bytes[4], bytes[5], bytes[6], bytes[7],
            bytes[8], bytes[9], bytes[10], bytes[11],
        ]),
        14 => StackOp::FusedGetSet7F([
            bytes[0], bytes[1], bytes[2], bytes[3], bytes[4], bytes[5], bytes[6], bytes[7],
            bytes[8], bytes[9], bytes[10], bytes[11], bytes[12], bytes[13],
        ]),
        16 => StackOp::FusedGetSet8F([
            bytes[0], bytes[1], bytes[2], bytes[3], bytes[4], bytes[5], bytes[6], bytes[7],
            bytes[8], bytes[9], bytes[10], bytes[11], bytes[12], bytes[13], bytes[14], bytes[15],
        ]),
        _ => unreachable!("unsupported float get/set chain length"),
    }
}

fn packed_get_set_f_chain(ops: &[StackOp], start: usize, pairs: usize) -> Option<Vec<u8>> {
    let mut bytes = Vec::with_capacity(pairs * 2);
    for j in 0..pairs {
        match (&ops[start + j * 2], &ops[start + j * 2 + 1]) {
            (StackOp::LocalGetF(src), StackOp::LocalSetF(dst)) if *src < 256 && *dst < 256 => {
                bytes.push(*src as u8);
                bytes.push(*dst as u8);
            }
            _ => return None,
        }
    }
    Some(bytes)
}

fn make_fused_get_get_fmul_sum_f(bytes: &[u8], sub_mask: u8) -> StackOp {
    match bytes.len() {
        4 => StackOp::FusedGetGetFMulSum2F([bytes[0], bytes[1], bytes[2], bytes[3]], sub_mask),
        6 => StackOp::FusedGetGetFMulSum3F(
            [bytes[0], bytes[1], bytes[2], bytes[3], bytes[4], bytes[5]],
            sub_mask,
        ),
        8 => StackOp::FusedGetGetFMulSum4F(
            [
                bytes[0], bytes[1], bytes[2], bytes[3], bytes[4], bytes[5], bytes[6], bytes[7],
            ],
            sub_mask,
        ),
        10 => StackOp::FusedGetGetFMulSum5F(
            [
                bytes[0], bytes[1], bytes[2], bytes[3], bytes[4], bytes[5], bytes[6], bytes[7],
                bytes[8], bytes[9],
            ],
            sub_mask,
        ),
        12 => StackOp::FusedGetGetFMulSum6F(
            [
                bytes[0], bytes[1], bytes[2], bytes[3], bytes[4], bytes[5], bytes[6], bytes[7],
                bytes[8], bytes[9], bytes[10], bytes[11],
            ],
            sub_mask,
        ),
        14 => StackOp::FusedGetGetFMulSum7F(
            [
                bytes[0], bytes[1], bytes[2], bytes[3], bytes[4], bytes[5], bytes[6], bytes[7],
                bytes[8], bytes[9], bytes[10], bytes[11], bytes[12], bytes[13],
            ],
            sub_mask,
        ),
        16 => StackOp::FusedGetGetFMulSum8F(
            [
                bytes[0], bytes[1], bytes[2], bytes[3], bytes[4], bytes[5], bytes[6], bytes[7],
                bytes[8], bytes[9], bytes[10], bytes[11], bytes[12], bytes[13], bytes[14],
                bytes[15],
            ],
            sub_mask,
        ),
        _ => unreachable!("unsupported float mul-sum chain length"),
    }
}

fn make_fused_bounds_check_jiz(bytes: &[u8], off: i32) -> StackOp {
    match bytes.len() {
        2 => StackOp::FusedBoundsCheck1JumpIfZero([bytes[0], bytes[1]], off),
        4 => StackOp::FusedBoundsCheck2JumpIfZero([bytes[0], bytes[1], bytes[2], bytes[3]], off),
        6 => StackOp::FusedBoundsCheck3JumpIfZero(
            [bytes[0], bytes[1], bytes[2], bytes[3], bytes[4], bytes[5]],
            off,
        ),
        8 => StackOp::FusedBoundsCheck4JumpIfZero(
            [
                bytes[0], bytes[1], bytes[2], bytes[3], bytes[4], bytes[5], bytes[6], bytes[7],
            ],
            off,
        ),
        10 => StackOp::FusedBoundsCheck5JumpIfZero(
            [
                bytes[0], bytes[1], bytes[2], bytes[3], bytes[4], bytes[5], bytes[6], bytes[7],
                bytes[8], bytes[9],
            ],
            off,
        ),
        12 => StackOp::FusedBoundsCheck6JumpIfZero(
            [
                bytes[0], bytes[1], bytes[2], bytes[3], bytes[4], bytes[5], bytes[6], bytes[7],
                bytes[8], bytes[9], bytes[10], bytes[11],
            ],
            off,
        ),
        14 => StackOp::FusedBoundsCheck7JumpIfZero(
            [
                bytes[0], bytes[1], bytes[2], bytes[3], bytes[4], bytes[5], bytes[6], bytes[7],
                bytes[8], bytes[9], bytes[10], bytes[11], bytes[12], bytes[13],
            ],
            off,
        ),
        16 => StackOp::FusedBoundsCheck8JumpIfZero(
            [
                bytes[0], bytes[1], bytes[2], bytes[3], bytes[4], bytes[5], bytes[6], bytes[7],
                bytes[8], bytes[9], bytes[10], bytes[11], bytes[12], bytes[13], bytes[14],
                bytes[15],
            ],
            off,
        ),
        _ => unreachable!("unsupported packed bounds-check length"),
    }
}

fn packed_bounds_check_chain(ops: &[StackOp], start: usize, pairs: usize) -> Option<Vec<u8>> {
    let mut bytes = Vec::with_capacity(pairs * 2);

    match &ops[start] {
        StackOp::FusedGetGetILt(a, b) if *a < 256 && *b < 256 => {
            bytes.push(*a as u8);
            bytes.push(*b as u8);
        }
        _ => return None,
    }

    let mut cursor = start + 1;
    for _ in 1..pairs {
        match (&ops[cursor], &ops[cursor + 1]) {
            (StackOp::FusedGetGetILt(a, b), StackOp::And) if *a < 256 && *b < 256 => {
                bytes.push(*a as u8);
                bytes.push(*b as u8);
            }
            _ => return None,
        }
        cursor += 2;
    }

    Some(bytes)
}

fn packed_fmul_sum_fused_chain(
    ops: &[StackOp],
    start: usize,
    terms: usize,
) -> Option<(Vec<u8>, u8)> {
    let mut bytes = Vec::with_capacity(terms * 2);
    let mut sub_mask = 0u8;

    match &ops[start] {
        StackOp::FusedGetGetFMulF(a, b) if *a < 256 && *b < 256 => {
            bytes.push(*a as u8);
            bytes.push(*b as u8);
        }
        _ => return None,
    }

    for term in 1..terms {
        match &ops[start + term] {
            StackOp::FusedGetGetFMulFAddF(a, b) if *a < 256 && *b < 256 => {
                bytes.push(*a as u8);
                bytes.push(*b as u8);
            }
            StackOp::FusedGetGetFMulFSubF(a, b) if *a < 256 && *b < 256 => {
                bytes.push(*a as u8);
                bytes.push(*b as u8);
                sub_mask |= 1 << term;
            }
            _ => return None,
        }
    }

    Some((bytes, sub_mask))
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
                let n = *n;
                let s = *s;
                let idx = *idx;
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
                let n = *n;
                let s = *s;
                let idx = *idx;
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
            if matches!(ops[i], StackOp::LocalGetF(_)) && matches!(ops[i + 1], StackOp::DropF) {
                ops[i] = StackOp::Nop;
                ops[i + 1] = StackOp::Nop;
                i += 2;
                continue;
            }
        }

        // local.set + i64.const + drop → local.set (dead Var init result)
        if i + 2 < len && !spans_target(i, 3) {
            if let (StackOp::LocalSet(_), StackOp::I64Const(_), StackOp::Drop) =
                (&ops[i], &ops[i + 1], &ops[i + 2])
            {
                // Keep the LocalSet, nop out the dead const+drop
                ops[i + 1] = StackOp::Nop;
                ops[i + 2] = StackOp::Nop;
                i += 3;
                continue;
            }
        }

        // local.get a + local.set b → FusedGetSet (variable move)
        if i + 1 < len && !spans_target(i, 2) {
            if let (StackOp::LocalGet(a), StackOp::LocalSet(b)) = (&ops[i], &ops[i + 1]) {
                let a = *a;
                let b = *b;
                ops[i] = StackOp::FusedGetSet(a, b);
                ops[i + 1] = StackOp::Nop;
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
            if let (StackOp::LocalSet(a), StackOp::LocalGet(b)) = (&ops[i], &ops[i + 1]) {
                if *a == *b {
                    let n = *a;
                    ops[i] = StackOp::LocalTee(n);
                    ops[i + 1] = StackOp::Nop;
                    i += 2;
                    continue;
                }
            }
        }

        // Float-window mirror: fw.local.set N + fw.local.get N → fw.local.tee N
        if i + 1 < len && !spans_target(i, 2) {
            if let (StackOp::LocalSetF(a), StackOp::LocalGetF(b)) = (&ops[i], &ops[i + 1]) {
                if *a == *b {
                    let n = *a;
                    ops[i] = StackOp::LocalTeeF(n);
                    ops[i + 1] = StackOp::Nop;
                    i += 2;
                    continue;
                }
            }
        }

        let mut fused_get_set_chain = false;
        for pairs in (2..=8).rev() {
            let span = pairs * 2;
            if i + span <= len && !spans_target(i, span) {
                if let Some(bytes) = packed_get_set_f_chain(ops, i, pairs) {
                    ops[i] = make_fused_get_set_f(&bytes);
                    for slot in ops.iter_mut().take(i + span).skip(i + 1) {
                        *slot = StackOp::Nop;
                    }
                    i += span;
                    fused_get_set_chain = true;
                    break;
                }
            }
        }
        if fused_get_set_chain {
            continue;
        }

        if i + 1 < len && !spans_target(i, 2) {
            if let (StackOp::LocalGetF(src), StackOp::LocalSetF(dst)) = (&ops[i], &ops[i + 1]) {
                if *src < 256 && *dst < 256 {
                    ops[i] = StackOp::FusedGetSetF(*src as u8, *dst as u8);
                    ops[i + 1] = StackOp::Nop;
                    i += 2;
                    continue;
                }
            }
        }

        // Float-window compare/jump from a local:
        // fw.local.get n + fw.fused.f32const_fgt_jiz v off
        //   → fw.fused.get_f32const_fgt_jiz n v off
        if i + 1 < len && !spans_target(i, 2) {
            if let (StackOp::LocalGetF(n), StackOp::FusedF32ConstFGtJumpIfZeroF(v, off)) =
                (&ops[i], &ops[i + 1])
            {
                let n = *n;
                let v = *v;
                // Original jump at i+1 targets i+2+off. Fused op at i
                // targets i+1+new_off, so new_off = off + 1.
                let new_off = *off + 1;
                ops[i] = StackOp::FusedGetF32ConstFGtJumpIfZeroF(n, v, new_off);
                ops[i + 1] = StackOp::Nop;
                i += 2;
                continue;
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
            if let (
                StackOp::LocalAddr(s),
                StackOp::IAddImm(off),
                StackOp::LocalGet(src),
                StackOp::Store32,
            ) = (&ops[i], &ops[i + 1], &ops[i + 2], &ops[i + 3])
            {
                let s = *s;
                let off = *off;
                let src = *src;
                ops[i] = StackOp::FusedAddrImmGetStore32(s, off, src);
                ops[i + 1] = StackOp::Nop;
                ops[i + 2] = StackOp::Nop;
                ops[i + 3] = StackOp::Nop;
                i += 4;
                continue;
            }
        }

        // local.get a + local.get b + i64.lt_s + jump_if_zero off → FusedGetGetILtJumpIfZero
        if i + 3 < len && !spans_target(i, 4) {
            if let (
                StackOp::LocalGet(a),
                StackOp::LocalGet(b),
                StackOp::ILt,
                StackOp::JumpIfZero(off),
            ) = (&ops[i], &ops[i + 1], &ops[i + 2], &ops[i + 3])
            {
                let a = *a;
                let b = *b;
                // Adjust offset: original JumpIfZero at i+3 targets i+3+1+off.
                // Fused instruction at i targets i+1+new_off. So new_off = off + 3.
                let new_off = *off + 3;
                ops[i] = if a < 256 && b < 256 {
                    make_fused_bounds_check_jiz(&[a as u8, b as u8], new_off)
                } else {
                    StackOp::FusedGetGetILtJumpIfZero(a, b, new_off)
                };
                ops[i + 1] = StackOp::Nop;
                ops[i + 2] = StackOp::Nop;
                ops[i + 3] = StackOp::Nop;
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
            ) = (&ops[i], &ops[i + 1], &ops[i + 2])
            {
                let a = *a;
                let s = *s;
                let o = *o;
                ops[i] = StackOp::FusedGetAddrFMulFAddF(a, s, o);
                ops[i + 1] = StackOp::Nop;
                ops[i + 2] = StackOp::Nop;
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
            ) = (&ops[i], &ops[i + 1], &ops[i + 2])
            {
                let a = *a;
                let s = *s;
                let o = *o;
                ops[i] = StackOp::FusedGetAddrFMulFSubF(a, s, o);
                ops[i + 1] = StackOp::Nop;
                ops[i + 2] = StackOp::Nop;
                i += 3;
                continue;
            }
        }

        let mut fused_mul_sum = false;
        for terms in (2..=8).rev() {
            let span = terms;
            if i + span <= len && !spans_target(i, span) {
                if let Some((pairs, sub_mask)) = packed_fmul_sum_fused_chain(ops, i, terms) {
                    ops[i] = make_fused_get_get_fmul_sum_f(&pairs, sub_mask);
                    for slot in ops.iter_mut().take(i + span).skip(i + 1) {
                        *slot = StackOp::Nop;
                    }
                    i += span;
                    fused_mul_sum = true;
                    break;
                }
            }
        }
        if fused_mul_sum {
            continue;
        }

        let mut fused_bounds_check = false;
        for pairs in (2..=8).rev() {
            let span = pairs * 2;
            if i + span <= len && !spans_target(i, span) {
                if let Some(bytes) = packed_bounds_check_chain(ops, i, pairs) {
                    if let StackOp::JumpIfZero(off) = ops[i + span - 1] {
                        let new_off = off + (span as i32 - 1);
                        ops[i] = make_fused_bounds_check_jiz(&bytes, new_off);
                        for slot in ops.iter_mut().take(i + span).skip(i + 1) {
                            *slot = StackOp::Nop;
                        }
                        i += span;
                        fused_bounds_check = true;
                        break;
                    }
                }
            }
        }
        if fused_bounds_check {
            continue;
        }

        // Float-window: fw.local.get a + fw.local.get b + fw.f32.mul + fw.f32.add
        //              → fw.fused.get_get_fmul_fadd.
        if i + 3 < len && !spans_target(i, 4) {
            if let (StackOp::LocalGetF(a), StackOp::LocalGetF(b), StackOp::FMulF, StackOp::FAddF) =
                (&ops[i], &ops[i + 1], &ops[i + 2], &ops[i + 3])
            {
                ops[i] = StackOp::FusedGetGetFMulFAddF(*a, *b);
                ops[i + 1] = StackOp::Nop;
                ops[i + 2] = StackOp::Nop;
                ops[i + 3] = StackOp::Nop;
                i += 4;
                continue;
            }
        }

        // Float-window: fw.local.get a + fw.local.get b + fw.f32.mul + fw.f32.sub
        //              → fw.fused.get_get_fmul_fsub.
        if i + 3 < len && !spans_target(i, 4) {
            if let (StackOp::LocalGetF(a), StackOp::LocalGetF(b), StackOp::FMulF, StackOp::FSubF) =
                (&ops[i], &ops[i + 1], &ops[i + 2], &ops[i + 3])
            {
                ops[i] = StackOp::FusedGetGetFMulFSubF(*a, *b);
                ops[i + 1] = StackOp::Nop;
                ops[i + 2] = StackOp::Nop;
                ops[i + 3] = StackOp::Nop;
                i += 4;
                continue;
            }
        }

        // fused.addr_load32off s o + local.set dst → FusedAddrLoad32OffSet
        if i + 1 < len && !spans_target(i, 2) {
            if let (StackOp::FusedAddrLoad32Off(s, o), StackOp::LocalSet(dst)) =
                (&ops[i], &ops[i + 1])
            {
                let s = *s;
                let o = *o;
                let dst = *dst;
                ops[i] = StackOp::FusedAddrLoad32OffSet(s, o, dst);
                ops[i + 1] = StackOp::Nop;
                i += 2;
                continue;
            }
        }

        // Float-window: fw.f32.const v + fw.f32.gt + jump_if_zero off
        //              → FusedF32ConstFGtJumpIfZeroF.
        if i + 2 < len && !spans_target(i, 3) {
            if let (StackOp::F32ConstF(v), StackOp::FGtF, StackOp::JumpIfZero(off)) =
                (&ops[i], &ops[i + 1], &ops[i + 2])
            {
                let v = *v;
                let new_off = *off + 2;
                ops[i] = StackOp::FusedF32ConstFGtJumpIfZeroF(v, new_off);
                ops[i + 1] = StackOp::Nop;
                ops[i + 2] = StackOp::Nop;
                i += 3;
                continue;
            }
        }

        // Float-window: fused.get_get_f* + fw.local.set dst → FusedGetGetF*Set.
        // The 3-address handlers read both operands directly from locals[]
        // and write to locals[] without touching either TOS window, so
        // they work unchanged.
        if i + 1 < len && !spans_target(i, 2) {
            if let (StackOp::FusedGetGetFAddF(a, b), StackOp::LocalSetF(dst)) =
                (&ops[i], &ops[i + 1])
            {
                let a = *a;
                let b = *b;
                let dst = *dst;
                ops[i] = StackOp::FusedGetGetFAddSet(a, b, dst);
                ops[i + 1] = StackOp::Nop;
                i += 2;
                continue;
            }
        }
        if i + 1 < len && !spans_target(i, 2) {
            if let (StackOp::FusedGetGetFSubF(a, b), StackOp::LocalSetF(dst)) =
                (&ops[i], &ops[i + 1])
            {
                let a = *a;
                let b = *b;
                let dst = *dst;
                ops[i] = StackOp::FusedGetGetFSubSet(a, b, dst);
                ops[i + 1] = StackOp::Nop;
                i += 2;
                continue;
            }
        }
        if i + 1 < len && !spans_target(i, 2) {
            if let (StackOp::FusedGetGetFMulF(a, b), StackOp::LocalSetF(dst)) =
                (&ops[i], &ops[i + 1])
            {
                let a = *a;
                let b = *b;
                let dst = *dst;
                ops[i] = StackOp::FusedGetGetFMulSet(a, b, dst);
                ops[i + 1] = StackOp::Nop;
                i += 2;
                continue;
            }
        }
        if i + 1 < len && !spans_target(i, 2) {
            if let (StackOp::FusedFMulFAddF, StackOp::LocalSetF(dst)) = (&ops[i], &ops[i + 1]) {
                let dst = *dst;
                ops[i] = StackOp::FusedFMulFAddSetF(dst);
                ops[i + 1] = StackOp::Nop;
                i += 2;
                continue;
            }
        }
        if i + 1 < len && !spans_target(i, 2) {
            if let (StackOp::FusedFMulFSubF, StackOp::LocalSetF(dst)) = (&ops[i], &ops[i + 1]) {
                let dst = *dst;
                ops[i] = StackOp::FusedFMulFSubSetF(dst);
                ops[i + 1] = StackOp::Nop;
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
                (&ops[i], &ops[i + 1])
            {
                let s = *s;
                let o = *o;
                let dst = *dst;
                ops[i] = StackOp::FusedAddrLoad32OffSet(s, o, dst);
                ops[i + 1] = StackOp::Nop;
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
            ) = (&ops[i], &ops[i + 1], &ops[i + 2], &ops[i + 3])
            {
                let s = *s;
                let off = *off;
                let src = *src;
                ops[i] = StackOp::FusedAddrImmGetStore32(s, off, src);
                ops[i + 1] = StackOp::Nop;
                ops[i + 2] = StackOp::Nop;
                ops[i + 3] = StackOp::Nop;
                i += 4;
                continue;
            }
        }

        // fused.addr_load32off_set s src_off tmp + fused.addr_imm_get_store32 s dst_off tmp → FusedFieldCopy32
        if i + 1 < len && !spans_target(i, 2) {
            if let (
                StackOp::FusedAddrLoad32OffSet(s1, src_off, tmp1),
                StackOp::FusedAddrImmGetStore32(s2, dst_off, tmp2),
            ) = (&ops[i], &ops[i + 1])
            {
                if s1 == s2 && tmp1 == tmp2 {
                    let s = *s1;
                    let src_off = *src_off;
                    let dst_off = *dst_off;
                    ops[i] = StackOp::FusedFieldCopy32(s, src_off, dst_off);
                    ops[i + 1] = StackOp::Nop;
                    i += 2;
                    continue;
                }
            }
        }

        // local.get src + i64.add_imm v + local.set dst → FusedGetAddImmSet
        if i + 2 < len && !spans_target(i, 3) {
            if let (StackOp::LocalGet(src), StackOp::IAddImm(v), StackOp::LocalSet(dst)) =
                (&ops[i], &ops[i + 1], &ops[i + 2])
            {
                let src = *src;
                let v = *v;
                let dst = *dst;
                ops[i] = StackOp::FusedGetAddImmSet(src, v, dst);
                ops[i + 1] = StackOp::Nop;
                ops[i + 2] = StackOp::Nop;
                i += 3;
                continue;
            }
        }

        // === 3-instruction fusions ===

        // Float-window: fw.local.get a + fw.local.get b + fw.f32.mul
        //             → fw.fused.get_get_fmul.
        if i + 2 < len && !spans_target(i, 3) {
            if let (StackOp::LocalGetF(a), StackOp::LocalGetF(b), StackOp::FMulF) =
                (&ops[i], &ops[i + 1], &ops[i + 2])
            {
                let a = *a;
                let b = *b;
                ops[i] = StackOp::FusedGetGetFMulF(a, b);
                ops[i + 1] = StackOp::Nop;
                ops[i + 2] = StackOp::Nop;
                i += 3;
                continue;
            }
        }

        // Float-window: fw.local.get a + fw.local.get b + fw.f32.add
        //             → fw.fused.get_get_fadd.
        if i + 2 < len && !spans_target(i, 3) {
            if let (StackOp::LocalGetF(a), StackOp::LocalGetF(b), StackOp::FAddF) =
                (&ops[i], &ops[i + 1], &ops[i + 2])
            {
                let a = *a;
                let b = *b;
                ops[i] = StackOp::FusedGetGetFAddF(a, b);
                ops[i + 1] = StackOp::Nop;
                ops[i + 2] = StackOp::Nop;
                i += 3;
                continue;
            }
        }

        // Float-window: fw.local.get a + fw.local.get b + fw.f32.sub
        //             → fw.fused.get_get_fsub.
        if i + 2 < len && !spans_target(i, 3) {
            if let (StackOp::LocalGetF(a), StackOp::LocalGetF(b), StackOp::FSubF) =
                (&ops[i], &ops[i + 1], &ops[i + 2])
            {
                let a = *a;
                let b = *b;
                ops[i] = StackOp::FusedGetGetFSubF(a, b);
                ops[i + 1] = StackOp::Nop;
                ops[i + 2] = StackOp::Nop;
                i += 3;
                continue;
            }
        }

        // local.get a + local.get b + i64.add → FusedGetGetIAdd
        if i + 2 < len && !spans_target(i, 3) {
            if let (StackOp::LocalGet(a), StackOp::LocalGet(b), StackOp::IAdd) =
                (&ops[i], &ops[i + 1], &ops[i + 2])
            {
                let a = *a;
                let b = *b;
                ops[i] = StackOp::FusedGetGetIAdd(a, b);
                ops[i + 1] = StackOp::Nop;
                ops[i + 2] = StackOp::Nop;
                i += 3;
                continue;
            }
        }

        // local.get a + local.get b + i64.lt_s → FusedGetGetILt
        if i + 2 < len && !spans_target(i, 3) {
            if let (StackOp::LocalGet(a), StackOp::LocalGet(b), StackOp::ILt) =
                (&ops[i], &ops[i + 1], &ops[i + 2])
            {
                let a = *a;
                let b = *b;
                ops[i] = StackOp::FusedGetGetILt(a, b);
                ops[i + 1] = StackOp::Nop;
                ops[i + 2] = StackOp::Nop;
                i += 3;
                continue;
            }
        }

        // local.addr slot + i32.load offset=off → FusedAddrLoad32Off
        if i + 1 < len && !spans_target(i, 2) {
            if let (StackOp::LocalAddr(slot), StackOp::Load32Off(off)) = (&ops[i], &ops[i + 1]) {
                let slot = *slot;
                let off = *off;
                ops[i] = StackOp::FusedAddrLoad32Off(slot, off);
                ops[i + 1] = StackOp::Nop;
                i += 2;
                continue;
            }
        }

        // Float-window mirror: local.addr slot + fw.f32.load offset=off
        //                    → fw.fused.addr_load32off.
        if i + 1 < len && !spans_target(i, 2) {
            if let (StackOp::LocalAddr(slot), StackOp::LoadF32OffF(off)) = (&ops[i], &ops[i + 1]) {
                let slot = *slot;
                let off = *off;
                ops[i] = StackOp::FusedAddrLoad32OffF(slot, off);
                ops[i + 1] = StackOp::Nop;
                i += 2;
                continue;
            }
        }

        // === 2-instruction fusions ===

        // Float-window: fw.local.get a + fw.f32.mul → fw.fused.get_fmul.
        if i + 1 < len && !spans_target(i, 2) {
            if let (StackOp::LocalGetF(a), StackOp::FMulF) = (&ops[i], &ops[i + 1]) {
                let a = *a;
                ops[i] = StackOp::FusedGetFMulF(a);
                ops[i + 1] = StackOp::Nop;
                i += 2;
                continue;
            }
        }

        // Float-window: fw.local.get a + fw.f32.add → fw.fused.get_fadd.
        if i + 1 < len && !spans_target(i, 2) {
            if let (StackOp::LocalGetF(a), StackOp::FAddF) = (&ops[i], &ops[i + 1]) {
                let a = *a;
                ops[i] = StackOp::FusedGetFAddF(a);
                ops[i + 1] = StackOp::Nop;
                i += 2;
                continue;
            }
        }

        // Float-window: fw.local.get a + fw.f32.sub → fw.fused.get_fsub.
        if i + 1 < len && !spans_target(i, 2) {
            if let (StackOp::LocalGetF(a), StackOp::FSubF) = (&ops[i], &ops[i + 1]) {
                let a = *a;
                ops[i] = StackOp::FusedGetFSubF(a);
                ops[i + 1] = StackOp::Nop;
                i += 2;
                continue;
            }
        }

        // Float-window: fw.f32.mul + fw.f32.add → fw.fused.fmul_fadd.
        if i + 1 < len && !spans_target(i, 2) {
            if matches!(ops[i], StackOp::FMulF) && matches!(ops[i + 1], StackOp::FAddF) {
                ops[i] = StackOp::FusedFMulFAddF;
                ops[i + 1] = StackOp::Nop;
                i += 2;
                continue;
            }
        }

        // Float-window: fw.f32.mul + fw.f32.sub → fw.fused.fmul_fsub.
        if i + 1 < len && !spans_target(i, 2) {
            if matches!(ops[i], StackOp::FMulF) && matches!(ops[i + 1], StackOp::FSubF) {
                ops[i] = StackOp::FusedFMulFSubF;
                ops[i + 1] = StackOp::Nop;
                i += 2;
                continue;
            }
        }

        // local.get a + local.set b → move (common from codegen temporaries)
        // But only if a != b (otherwise it's a no-op, but leave it for now).

        // i64.const v + local.set n → FusedConstSet
        if i + 1 < len && !spans_target(i, 2) {
            if let (StackOp::I64Const(v), StackOp::LocalSet(n)) = (&ops[i], &ops[i + 1]) {
                let v = *v;
                let n = *n;
                ops[i] = StackOp::FusedConstSet(v, n);
                ops[i + 1] = StackOp::Nop;
                i += 2;
                continue;
            }
        }

        // f32.const v + local.set n → FusedF32ConstSet
        if i + 1 < len && !spans_target(i, 2) {
            if let (StackOp::F32Const(v), StackOp::LocalSet(n)) = (&ops[i], &ops[i + 1]) {
                let v = *v;
                let n = *n;
                ops[i] = StackOp::FusedF32ConstSet(v, n);
                ops[i + 1] = StackOp::Nop;
                i += 2;
                continue;
            }
        }

        // local.addr slot + local.get idx + slice.load32 → FusedAddrGetSliceLoad32
        if i + 2 < len && !spans_target(i, 3) {
            if let (StackOp::LocalAddr(slot), StackOp::LocalGet(idx), StackOp::SliceLoad32) =
                (&ops[i], &ops[i + 1], &ops[i + 2])
            {
                let slot = *slot;
                let idx = *idx;
                ops[i] = StackOp::FusedAddrGetSliceLoad32(slot, idx);
                ops[i + 1] = StackOp::Nop;
                ops[i + 2] = StackOp::Nop;
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
            StackOp::FusedBoundsCheck1JumpIfZero(p, off) => {
                let target_old = (old as i64 + 1 + *off as i64) as usize;
                let target_new = new_idx[target_old];
                let new_off = target_new as i32 - new_idx[old] as i32 - 1;
                StackOp::FusedBoundsCheck1JumpIfZero(*p, new_off)
            }
            StackOp::FusedBoundsCheck2JumpIfZero(p, off) => {
                let target_old = (old as i64 + 1 + *off as i64) as usize;
                let target_new = new_idx[target_old];
                let new_off = target_new as i32 - new_idx[old] as i32 - 1;
                StackOp::FusedBoundsCheck2JumpIfZero(*p, new_off)
            }
            StackOp::FusedBoundsCheck3JumpIfZero(p, off) => {
                let target_old = (old as i64 + 1 + *off as i64) as usize;
                let target_new = new_idx[target_old];
                let new_off = target_new as i32 - new_idx[old] as i32 - 1;
                StackOp::FusedBoundsCheck3JumpIfZero(*p, new_off)
            }
            StackOp::FusedBoundsCheck4JumpIfZero(p, off) => {
                let target_old = (old as i64 + 1 + *off as i64) as usize;
                let target_new = new_idx[target_old];
                let new_off = target_new as i32 - new_idx[old] as i32 - 1;
                StackOp::FusedBoundsCheck4JumpIfZero(*p, new_off)
            }
            StackOp::FusedBoundsCheck5JumpIfZero(p, off) => {
                let target_old = (old as i64 + 1 + *off as i64) as usize;
                let target_new = new_idx[target_old];
                let new_off = target_new as i32 - new_idx[old] as i32 - 1;
                StackOp::FusedBoundsCheck5JumpIfZero(*p, new_off)
            }
            StackOp::FusedBoundsCheck6JumpIfZero(p, off) => {
                let target_old = (old as i64 + 1 + *off as i64) as usize;
                let target_new = new_idx[target_old];
                let new_off = target_new as i32 - new_idx[old] as i32 - 1;
                StackOp::FusedBoundsCheck6JumpIfZero(*p, new_off)
            }
            StackOp::FusedBoundsCheck7JumpIfZero(p, off) => {
                let target_old = (old as i64 + 1 + *off as i64) as usize;
                let target_new = new_idx[target_old];
                let new_off = target_new as i32 - new_idx[old] as i32 - 1;
                StackOp::FusedBoundsCheck7JumpIfZero(*p, new_off)
            }
            StackOp::FusedBoundsCheck8JumpIfZero(p, off) => {
                let target_old = (old as i64 + 1 + *off as i64) as usize;
                let target_new = new_idx[target_old];
                let new_off = target_new as i32 - new_idx[old] as i32 - 1;
                StackOp::FusedBoundsCheck8JumpIfZero(*p, new_off)
            }
            StackOp::FusedF32ConstFGtJumpIfZeroF(v, off) => {
                let target_old = (old as i64 + 1 + *off as i64) as usize;
                let target_new = new_idx[target_old];
                let new_off = target_new as i32 - new_idx[old] as i32 - 1;
                StackOp::FusedF32ConstFGtJumpIfZeroF(*v, new_off)
            }
            StackOp::FusedGetF32ConstFGtJumpIfZeroF(n, v, off) => {
                let target_old = (old as i64 + 1 + *off as i64) as usize;
                let target_new = new_idx[target_old];
                let new_off = target_new as i32 - new_idx[old] as i32 - 1;
                StackOp::FusedGetF32ConstFGtJumpIfZeroF(*n, *v, new_off)
            }
            other => other.clone(),
        };
        new_ops.push(adjusted);
    }

    func.ops = new_ops;
}
