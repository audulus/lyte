//! Hot local analysis and rewrite for the stack VM.
//!
//! Identifies the 3 most-accessed locals per function (weighted by loop
//! nesting depth) and rewrites their references so that the C
//! interpreter reads/writes them from l0/l1/l2 register arguments
//! instead of the memory-backed `locals[]` array.
//!
//! There is no physical remap or frame-slot bump. Hot slots stay at
//! their original positions in the frame (memory is wasted on them
//! but never read); instead, the rewrite pass replaces every slot
//! reference to a hot local with one of three special *magic* slot
//! indices reserved at the top of the u16 space:
//!
//!   HOT_L0 = 0xFFFC  →  l0
//!   HOT_L1 = 0xFFFD  →  l1
//!   HOT_L2 = 0xFFFE  →  l2
//!
//! The C handlers' READ_I / WRITE_I / READ_F_OFF / WRITE_F_OFF macros
//! check imm values against these magic constants and dispatch to the
//! register or to `locals[imm]` accordingly. Because legitimate slot
//! indices are bounded by the frame's `local_count` (always well under
//! 0xFFFC), the magic constants never collide with a real slot — even
//! in functions where slot 0/1/2 are parameters.
//!
//! For raw `LocalGet` / `LocalSet` / `LocalTee` ops (and their float-
//! window variants), the rewrite goes one step further: it replaces
//! them with the dedicated `LocalGetL0` / `LocalSetL0` / `LocalTeeL0`
//! handlers, which access `l0..l2` directly without the magic-value
//! branch. That's the fast path for the common case where the hot
//! local is read or written as a standalone op.

use crate::stack_ir::{StackFunction, StackOp};

/// Reserved slot index mapped to `l0` in the C handlers.
pub const HOT_L0: u16 = 0xFFFC;
/// Reserved slot index mapped to `l1` in the C handlers.
pub const HOT_L1: u16 = 0xFFFD;
/// Reserved slot index mapped to `l2` in the C handlers.
pub const HOT_L2: u16 = 0xFFFE;

/// Analyze a function and return the original indices of the top 3 hottest
/// locals, ordered by access weight. `None` means fewer than 3 locals qualify.
pub fn analyze(func: &StackFunction) -> [Option<u16>; 3] {
    if func.local_count == 0 {
        return [None; 3];
    }

    let mut weights = vec![0u64; func.local_count as usize];

    // Compute loop nesting depth at each instruction.
    let len = func.ops.len();
    let mut depth_delta = vec![0i32; len + 1];
    for (i, op) in func.ops.iter().enumerate() {
        let off = match op {
            StackOp::Jump(off) => Some(*off),
            StackOp::FusedGetGetILtJumpIfZero(_, _, off) => Some(*off),
            _ => None,
        };
        if let Some(off) = off {
            if off < 0 {
                let target = (i as i64 + 1 + off as i64) as usize;
                if target < len {
                    depth_delta[target] += 1;
                    depth_delta[i + 1] -= 1;
                }
            }
        }
    }
    let mut nesting = vec![0u32; len];
    let mut d: i32 = 0;
    for i in 0..len {
        d += depth_delta[i];
        nesting[i] = d.max(0) as u32;
    }

    // Weight each local access by the loop nesting depth at its
    // instruction, capped at 10^4 to avoid u64 overflow in deeply
    // nested loops.
    for (i, op) in func.ops.iter().enumerate() {
        let w = 10u64.pow(nesting[i].min(4));
        for idx in local_indices_read(op) {
            if (idx as usize) < weights.len() {
                weights[idx as usize] += w;
            }
        }
        for idx in local_indices_written(op) {
            if (idx as usize) < weights.len() {
                weights[idx as usize] += w;
            }
        }
    }

    // Pick the top 3 non-parameter slots by weight. Parameters are
    // excluded because they live at fixed positions set by the caller —
    // the magic-value rewrite would turn their accesses into register
    // reads, but the caller wrote them into memory.
    let param_count = func.param_count as usize;
    let mut result = [None; 3];
    for slot in 0..3 {
        let mut best_idx = None;
        let mut best_w = 0u64;
        for (idx, &w) in weights.iter().enumerate() {
            if idx < param_count {
                continue;
            }
            if w > best_w {
                let already = result.iter().any(|r| *r == Some(idx as u16));
                if !already {
                    best_w = w;
                    best_idx = Some(idx as u16);
                }
            }
        }
        result[slot] = best_idx;
    }
    result
}

/// Walk the op stream and rewrite every reference to a hot local slot
/// so that the C interpreter reads/writes it from `l0..l2` instead of
/// `locals[]`. Runs AFTER fusion so that fusion can still form the
/// aggressive 3-address patterns over hot slots — the rewrite just
/// swaps the slot imm for a magic value the handler macros recognize.
///
/// Raw `LocalGet` / `LocalSet` / `LocalTee` ops on a hot slot get the
/// additional treatment of being lowered to the dedicated `LocalGetL*`
/// / `LocalSetL*` / `LocalTeeL*` handlers (same for the float-window
/// variants), which access `l0..l2` directly without the runtime
/// magic-value branch.
pub fn rewrite_hot(func: &mut StackFunction) {
    let hot = func.hot_locals;
    // Map original slot index → magic value. None entries in hot_locals
    // don't produce mappings.
    let magic_of = |n: u16| -> Option<u16> {
        for (reg, entry) in hot.iter().enumerate() {
            if *entry == Some(n) {
                return Some(match reg {
                    0 => HOT_L0,
                    1 => HOT_L1,
                    2 => HOT_L2,
                    _ => unreachable!(),
                });
            }
        }
        None
    };
    let to_l_get = |n: u16| -> Option<StackOp> {
        Some(match magic_of(n)? {
            HOT_L0 => StackOp::LocalGetL0,
            HOT_L1 => StackOp::LocalGetL1,
            HOT_L2 => StackOp::LocalGetL2,
            _ => return None,
        })
    };
    let to_l_set = |n: u16| -> Option<StackOp> {
        Some(match magic_of(n)? {
            HOT_L0 => StackOp::LocalSetL0,
            HOT_L1 => StackOp::LocalSetL1,
            HOT_L2 => StackOp::LocalSetL2,
            _ => return None,
        })
    };
    let to_l_tee = |n: u16| -> Option<StackOp> {
        Some(match magic_of(n)? {
            HOT_L0 => StackOp::LocalTeeL0,
            HOT_L1 => StackOp::LocalTeeL1,
            HOT_L2 => StackOp::LocalTeeL2,
            _ => return None,
        })
    };
    let to_l_get_f = |n: u16| -> Option<StackOp> {
        Some(match magic_of(n)? {
            HOT_L0 => StackOp::LocalGetL0F,
            HOT_L1 => StackOp::LocalGetL1F,
            HOT_L2 => StackOp::LocalGetL2F,
            _ => return None,
        })
    };
    let to_l_set_f = |n: u16| -> Option<StackOp> {
        Some(match magic_of(n)? {
            HOT_L0 => StackOp::LocalSetL0F,
            HOT_L1 => StackOp::LocalSetL1F,
            HOT_L2 => StackOp::LocalSetL2F,
            _ => return None,
        })
    };
    let to_l_tee_f = |n: u16| -> Option<StackOp> {
        Some(match magic_of(n)? {
            HOT_L0 => StackOp::LocalTeeL0F,
            HOT_L1 => StackOp::LocalTeeL1F,
            HOT_L2 => StackOp::LocalTeeL2F,
            _ => return None,
        })
    };

    for op in &mut func.ops {
        // Raw local ops → L-handler fast path.
        let replacement = match *op {
            StackOp::LocalGet(n) => to_l_get(n),
            StackOp::LocalSet(n) => to_l_set(n),
            StackOp::LocalTee(n) => to_l_tee(n),
            StackOp::LocalGetF(n) => to_l_get_f(n),
            StackOp::LocalSetF(n) => to_l_set_f(n),
            StackOp::LocalTeeF(n) => to_l_tee_f(n),
            _ => None,
        };
        if let Some(new_op) = replacement {
            *op = new_op;
            continue;
        }

        // Fused ops: rewrite each scalar-local slot arg to a magic value
        // if it refers to a hot local. Memory-slot args (struct / array
        // bases) are left alone — they never correspond to hot locals
        // because the analysis only picks scalar locals.
        rewrite_fused_hot_refs(op, &magic_of);
    }
}

/// Rewrite scalar-local slot args in a fused op to their magic values,
/// leaving memory-slot and byte-offset args untouched.
fn rewrite_fused_hot_refs(op: &mut StackOp, magic_of: &impl Fn(u16) -> Option<u16>) {
    let rw = |idx: &mut u16, magic_of: &dyn Fn(u16) -> Option<u16>| {
        if let Some(magic) = magic_of(*idx) {
            *idx = magic;
        }
    };

    match op {
        // Two-scalar read patterns.
        StackOp::FusedGetGetIAdd(a, b)
        | StackOp::FusedGetGetILt(a, b)
        | StackOp::FusedGetGetILtJumpIfZero(a, b, _)
        | StackOp::FusedGetGetFAddF(a, b)
        | StackOp::FusedGetGetFSubF(a, b)
        | StackOp::FusedGetGetFMulF(a, b)
        | StackOp::FusedGetSet(a, b) => {
            rw(a, &magic_of);
            rw(b, &magic_of);
        }

        // One-scalar read patterns.
        StackOp::FusedGetFMulF(a)
        | StackOp::FusedGetFAddF(a)
        | StackOp::FusedGetFSubF(a) => rw(a, &magic_of),

        // Fused FMA terms: `a` is the hot scalar, the other args are
        // memory slot + byte offset.
        StackOp::FusedGetAddrFMulFAddF(a, _, _)
        | StackOp::FusedGetAddrFMulFSubF(a, _, _) => rw(a, &magic_of),

        // Slice/array accessors: only the index is a scalar local.
        StackOp::FusedAddrGetSliceLoad32(_, i)
        | StackOp::FusedAddrGetSliceStore32(_, i)
        | StackOp::FusedLocalArrayLoad32(_, i)
        | StackOp::FusedLocalArrayStore32(_, i)
        | StackOp::FusedAddrGetSliceLoad32F(_, i)
        | StackOp::FusedAddrGetSliceStore32F(_, i)
        | StackOp::FusedLocalArrayLoad32F(_, i)
        | StackOp::FusedLocalArrayStore32F(_, i) => rw(i, &magic_of),

        // Int const → local.
        StackOp::FusedConstSet(_, n) | StackOp::FusedF32ConstSet(_, n) => rw(n, &magic_of),

        // GetAddImm: reads src, writes dst.
        StackOp::FusedGetAddImmSet(s, _, d) => {
            rw(s, &magic_of);
            rw(d, &magic_of);
        }

        // 3-address set patterns: reads a, b; writes d.
        StackOp::FusedGetGetFAddSet(a, b, d)
        | StackOp::FusedGetGetFSubSet(a, b, d)
        | StackOp::FusedGetGetFMulSet(a, b, d)
        | StackOp::FusedGetGetFDivSet(a, b, d)
        | StackOp::FusedGetGetIAddSet(a, b, d)
        | StackOp::FusedGetGetISubSet(a, b, d)
        | StackOp::FusedGetGetIMulSet(a, b, d) => {
            rw(a, &magic_of);
            rw(b, &magic_of);
            rw(d, &magic_of);
        }

        // Load from memory slot into scalar: dst is the scalar local.
        StackOp::FusedAddrLoad32OffSet(_, _, d) => rw(d, &magic_of),

        // Store scalar into memory field: src is the scalar local.
        StackOp::FusedAddrImmGetStore32(_, _, src) => rw(src, &magic_of),

        // Tee + slice store: `n` is the tee target (scalar), `idx` is
        // the slice index (scalar). `s` is the slice fat-ptr memory slot.
        StackOp::FusedTeeSliceStore32(n, _, idx) => {
            rw(n, &magic_of);
            rw(idx, &magic_of);
        }
        StackOp::FusedTeeSliceStore32F(n, _, idx) => {
            rw(n, &magic_of);
            rw(idx, &magic_of);
        }

        _ => {}
    }
}

/// Extract local indices read by an op (used by the weight analysis).
fn local_indices_read(op: &StackOp) -> Vec<u16> {
    match op {
        StackOp::LocalGet(n) | StackOp::LocalTee(n) => vec![*n],
        StackOp::LocalGetF(n) | StackOp::LocalTeeF(n) => vec![*n],
        StackOp::FusedGetGetIAdd(a, b)
        | StackOp::FusedGetGetILt(a, b)
        | StackOp::FusedGetSet(a, b) => vec![*a, *b],
        StackOp::FusedGetAddImmSet(s, _, _) => vec![*s],
        StackOp::FusedGetGetILtJumpIfZero(a, b, _) => vec![*a, *b],
        StackOp::FusedGetGetFAddSet(a, b, _)
        | StackOp::FusedGetGetFSubSet(a, b, _)
        | StackOp::FusedGetGetFMulSet(a, b, _)
        | StackOp::FusedGetGetFDivSet(a, b, _)
        | StackOp::FusedGetGetIAddSet(a, b, _)
        | StackOp::FusedGetGetISubSet(a, b, _)
        | StackOp::FusedGetGetIMulSet(a, b, _) => vec![*a, *b],
        StackOp::FusedAddrGetSliceLoad32(_, i)
        | StackOp::FusedAddrGetSliceStore32(_, i)
        | StackOp::FusedLocalArrayLoad32(_, i)
        | StackOp::FusedLocalArrayStore32(_, i)
        | StackOp::FusedAddrGetSliceLoad32F(_, i)
        | StackOp::FusedAddrGetSliceStore32F(_, i)
        | StackOp::FusedLocalArrayLoad32F(_, i)
        | StackOp::FusedLocalArrayStore32F(_, i) => vec![*i],
        StackOp::FusedAddrImmGetStore32(_, _, src) => vec![*src],
        StackOp::FusedGetGetFAddF(a, b)
        | StackOp::FusedGetGetFSubF(a, b)
        | StackOp::FusedGetGetFMulF(a, b) => vec![*a, *b],
        StackOp::FusedGetFMulF(a) | StackOp::FusedGetFAddF(a) | StackOp::FusedGetFSubF(a) => {
            vec![*a]
        }
        StackOp::FusedGetAddrFMulFAddF(a, _, _) | StackOp::FusedGetAddrFMulFSubF(a, _, _) => {
            vec![*a]
        }
        StackOp::FusedTeeSliceStore32(_, _, idx) | StackOp::FusedTeeSliceStore32F(_, _, idx) => {
            vec![*idx]
        }
        _ => vec![],
    }
}

/// Extract local indices written by an op (used by the weight analysis).
fn local_indices_written(op: &StackOp) -> Vec<u16> {
    match op {
        StackOp::LocalSet(n) | StackOp::LocalTee(n) => vec![*n],
        StackOp::LocalSetF(n) | StackOp::LocalTeeF(n) => vec![*n],
        StackOp::FusedConstSet(_, n) | StackOp::FusedF32ConstSet(_, n) => vec![*n],
        StackOp::FusedGetAddImmSet(_, _, d) => vec![*d],
        StackOp::FusedGetGetFAddSet(_, _, d)
        | StackOp::FusedGetGetFSubSet(_, _, d)
        | StackOp::FusedGetGetFMulSet(_, _, d)
        | StackOp::FusedGetGetFDivSet(_, _, d)
        | StackOp::FusedGetGetIAddSet(_, _, d)
        | StackOp::FusedGetGetISubSet(_, _, d)
        | StackOp::FusedGetGetIMulSet(_, _, d) => vec![*d],
        StackOp::FusedAddrLoad32OffSet(_, _, d) => vec![*d],
        StackOp::FusedGetSet(_, b) => vec![*b],
        StackOp::FusedTeeSliceStore32(n, _, _) | StackOp::FusedTeeSliceStore32F(n, _, _) => {
            vec![*n]
        }
        _ => vec![],
    }
}
