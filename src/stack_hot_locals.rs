//! Hot local analysis, index remapping, and lowering for the stack VM.
//!
//! Identifies the 3 most-accessed locals per function (weighted by loop nesting
//! depth), remaps them to indices 0/1/2, and lowers LocalGet(0/1/2) /
//! LocalSet(0/1/2) to LocalGetL0..L2 / LocalSetL0..L2 register ops.

use crate::stack_ir::{StackFunction, StackOp};

/// Analyze a function and return the original indices of the top 3 hottest
/// locals, ordered by access weight. `None` means fewer than 3 locals qualify.
pub fn analyze(func: &StackFunction) -> [Option<u16>; 3] {
    if func.local_count == 0 {
        return [None; 3];
    }

    // Count accesses weighted by loop nesting depth.
    // Loop detection: a backward Jump target indicates a loop header.
    let mut weights = vec![0u64; func.local_count as usize];

    // First, compute loop nesting depth at each instruction.
    // A simple heuristic: find backward jumps (off < 0), mark their target..origin
    // range as +1 nesting depth.
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
                // Backward jump from i to target: this is a loop.
                let target = (i as i64 + 1 + off as i64) as usize;
                if target < len {
                    depth_delta[target] += 1;
                    depth_delta[i + 1] -= 1;
                }
            }
        }
    }

    // Compute running nesting depth.
    let mut nesting = vec![0u32; len];
    let mut d: i32 = 0;
    for i in 0..len {
        d += depth_delta[i];
        nesting[i] = d.max(0) as u32;
    }

    // Weight each local access.
    for (i, op) in func.ops.iter().enumerate() {
        let w = 10u64.pow(nesting[i].min(4)); // cap at 10^4 to avoid overflow
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

    // Find top 3 by weight (must have weight > 0).
    // Exclude parameter indices — they can't be remapped because callers
    // pass arguments into locals[0..param_count-1] directly.
    let param_count = func.param_count as usize;
    let mut result = [None; 3];
    for slot in 0..3 {
        let mut best_idx = None;
        let mut best_w = 0u64;
        for (idx, &w) in weights.iter().enumerate() {
            if idx < param_count {
                continue; // Skip parameters.
            }
            if w > best_w {
                // Check not already selected.
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

/// Remap locals so that hot locals occupy indices 0, 1, 2.
/// Swaps the hot local into position and rewrites all references.
/// Only remaps into slots that are not occupied by parameters (since
/// callers pass arguments into locals[0..param_count-1] directly).
pub fn remap(func: &mut StackFunction, hot: &[Option<u16>; 3]) {
    let n = func.local_count as usize;
    let param_count = func.param_count as usize;
    let mut perm: Vec<u16> = (0..n as u16).collect();

    for (target, orig) in hot.iter().enumerate() {
        if target < param_count {
            continue; // Can't remap into a parameter slot.
        }
        if let Some(orig_idx) = orig {
            let orig_idx = *orig_idx;
            let target_idx = target as u16;
            if orig_idx == target_idx {
                continue; // Already in place.
            }
            // Find where orig_idx currently maps to.
            let cur_pos_of_orig = perm.iter().position(|&x| x == orig_idx).unwrap() as u16;
            let cur_pos_of_target = perm.iter().position(|&x| x == target_idx).unwrap() as u16;
            // Swap in the permutation.
            perm.swap(cur_pos_of_orig as usize, cur_pos_of_target as usize);
        }
    }

    // Build reverse map: for each original local index, what new index does it get?
    // perm[slot] = value means slot `slot` holds original local `value`.
    // We want: given original index `o`, find slot `s` such that perm[s] == o.
    let mut remap = vec![0u16; n];
    for (slot, &orig) in perm.iter().enumerate() {
        remap[orig as usize] = slot as u16;
    }

    // Rewrite all local references in the instruction stream.
    for op in &mut func.ops {
        rewrite_local_indices(op, &remap);
    }

    // Ensure at least 3 local slots so l0/l1/l2 always have backing storage.
    if func.local_count < 3 {
        func.local_count = 3;
    }
}

/// Lower LocalGet(0/1/2) → LocalGetL0..L2, LocalSet(0/1/2) → LocalSetL0..L2.
///
/// Called AFTER the fusion optimizer. Only lowers indices that are not
/// parameter slots (parameters can't be remapped, so their l0/l1/l2
/// registers aren't meaningful).
///
/// The L-register get handlers reload from locals[] memory to stay in sync
/// with fused ops that may write to those slots. The L-register set handlers
/// write to both register and memory.
pub fn lower(func: &mut StackFunction) {
    let param_count = func.param_count as u16;
    for op in &mut func.ops {
        *op = match *op {
            StackOp::LocalGet(0) if param_count == 0 => StackOp::LocalGetL0,
            StackOp::LocalGet(1) if param_count <= 1 => StackOp::LocalGetL1,
            StackOp::LocalGet(2) if param_count <= 2 => StackOp::LocalGetL2,
            StackOp::LocalSet(0) if param_count == 0 => StackOp::LocalSetL0,
            StackOp::LocalSet(1) if param_count <= 1 => StackOp::LocalSetL1,
            StackOp::LocalSet(2) if param_count <= 2 => StackOp::LocalSetL2,
            // Float-window variants — same eligibility rules.
            StackOp::LocalGetF(0) if param_count == 0 => StackOp::LocalGetL0F,
            StackOp::LocalGetF(1) if param_count <= 1 => StackOp::LocalGetL1F,
            StackOp::LocalGetF(2) if param_count <= 2 => StackOp::LocalGetL2F,
            StackOp::LocalSetF(0) if param_count == 0 => StackOp::LocalSetL0F,
            StackOp::LocalSetF(1) if param_count <= 1 => StackOp::LocalSetL1F,
            StackOp::LocalSetF(2) if param_count <= 2 => StackOp::LocalSetL2F,
            _ => continue,
        };
    }
}

/// Extract local indices read by an op.
fn local_indices_read(op: &StackOp) -> Vec<u16> {
    match op {
        StackOp::LocalGet(n) | StackOp::LocalTee(n) => vec![*n],
        StackOp::LocalGetF(n) | StackOp::LocalTeeF(n) => vec![*n],
        StackOp::FusedGetGetFMul(a, b) | StackOp::FusedGetGetFAdd(a, b)
        | StackOp::FusedGetGetFSub(a, b) | StackOp::FusedGetGetIAdd(a, b)
        | StackOp::FusedGetGetILt(a, b) | StackOp::FusedGetSet(a, b) => vec![*a, *b],
        StackOp::FusedGetFMul(a) | StackOp::FusedGetFAdd(a) | StackOp::FusedGetFSub(a) => {
            vec![*a]
        }
        StackOp::FusedGetAddImmSet(s, _, _) => vec![*s],
        StackOp::FusedGetGetILtJumpIfZero(a, b, _) => vec![*a, *b],
        StackOp::FusedGetGetFAddSet(a, b, _) => vec![*a, *b],
        StackOp::FusedAddrGetSliceLoad32(_, i) => vec![*i],
        StackOp::FusedAddrGetSliceStore32(_, i) => vec![*i],
        StackOp::FusedGetAddrFMulFAdd(a, _, _) | StackOp::FusedGetAddrFMulFSub(a, _, _) => {
            vec![*a]
        }
        StackOp::FusedAddrImmGetStore32(_, _, src) => vec![*src],
        // Float-window fused ops
        StackOp::FusedGetGetFAddF(a, b) | StackOp::FusedGetGetFSubF(a, b)
        | StackOp::FusedGetGetFMulF(a, b) => vec![*a, *b],
        StackOp::FusedGetFMulF(a) | StackOp::FusedGetFAddF(a) | StackOp::FusedGetFSubF(a) => {
            vec![*a]
        }
        StackOp::FusedGetAddrFMulFAddF(a, _, _) | StackOp::FusedGetAddrFMulFSubF(a, _, _) => {
            vec![*a]
        }
        StackOp::FusedAddrGetSliceLoad32F(_, i) | StackOp::FusedAddrGetSliceStore32F(_, i) => {
            vec![*i]
        }
        _ => vec![],
    }
}

/// Extract local indices written by an op.
fn local_indices_written(op: &StackOp) -> Vec<u16> {
    match op {
        StackOp::LocalSet(n) | StackOp::LocalTee(n) => vec![*n],
        StackOp::LocalSetF(n) | StackOp::LocalTeeF(n) => vec![*n],
        StackOp::FusedConstSet(_, n) | StackOp::FusedF32ConstSet(_, n) => vec![*n],
        StackOp::FusedGetAddImmSet(_, _, d) => vec![*d],
        StackOp::FusedGetGetFAddSet(_, _, d) => vec![*d],
        StackOp::FusedAddrLoad32OffSet(_, _, d) => vec![*d],
        StackOp::FusedGetSet(_, b) => vec![*b],
        _ => vec![],
    }
}

/// Rewrite all local indices in an op according to a remap table.
fn rewrite_local_indices(op: &mut StackOp, remap: &[u16]) {
    fn r(idx: &mut u16, remap: &[u16]) {
        if (*idx as usize) < remap.len() {
            *idx = remap[*idx as usize];
        }
    }

    match op {
        StackOp::LocalGet(n) | StackOp::LocalSet(n) | StackOp::LocalTee(n) => r(n, remap),
        StackOp::LocalGetF(n) | StackOp::LocalSetF(n) | StackOp::LocalTeeF(n) => r(n, remap),
        StackOp::FusedGetGetFAddF(a, b)
        | StackOp::FusedGetGetFSubF(a, b)
        | StackOp::FusedGetGetFMulF(a, b) => {
            r(a, remap);
            r(b, remap);
        }
        StackOp::FusedGetFMulF(a) | StackOp::FusedGetFAddF(a) | StackOp::FusedGetFSubF(a) => {
            r(a, remap);
        }
        StackOp::FusedGetAddrFMulFAddF(a, _, _) | StackOp::FusedGetAddrFMulFSubF(a, _, _) => {
            r(a, remap);
        }
        StackOp::FusedAddrGetSliceLoad32F(_, i) | StackOp::FusedAddrGetSliceStore32F(_, i)
        | StackOp::FusedLocalArrayLoad32F(_, i) | StackOp::FusedLocalArrayStore32F(_, i) => {
            r(i, remap);
        }
        StackOp::FusedGetGetFMul(a, b)
        | StackOp::FusedGetGetFMulFW(a, b)
        | StackOp::FusedGetGetFAdd(a, b)
        | StackOp::FusedGetGetFSub(a, b)
        | StackOp::FusedGetGetIAdd(a, b)
        | StackOp::FusedGetGetILt(a, b) => {
            r(a, remap);
            r(b, remap);
        }
        StackOp::FusedGetFMul(a) | StackOp::FusedGetFAdd(a) | StackOp::FusedGetFSub(a) => {
            r(a, remap);
        }
        StackOp::FusedGetAddImmSet(s, _, d) => {
            r(s, remap);
            r(d, remap);
        }
        StackOp::FusedGetGetILtJumpIfZero(a, b, _) => {
            r(a, remap);
            r(b, remap);
        }
        StackOp::FusedConstSet(_, n) | StackOp::FusedF32ConstSet(_, n) => r(n, remap),
        StackOp::FusedGetSet(a, b) => {
            r(a, remap);
            r(b, remap);
        }
        StackOp::FusedGetGetFAddSet(a, b, d)
        | StackOp::FusedGetGetFSubSet(a, b, d)
        | StackOp::FusedGetGetFMulSet(a, b, d)
        | StackOp::FusedGetGetFDivSet(a, b, d)
        | StackOp::FusedGetGetIAddSet(a, b, d)
        | StackOp::FusedGetGetISubSet(a, b, d)
        | StackOp::FusedGetGetIMulSet(a, b, d) => {
            r(a, remap);
            r(b, remap);
            r(d, remap);
        }
        StackOp::FusedAddrGetSliceLoad32(_, i) | StackOp::FusedAddrGetSliceStore32(_, i)
        | StackOp::FusedLocalArrayLoad32(_, i) | StackOp::FusedLocalArrayStore32(_, i) => {
            r(i, remap);
        }
        StackOp::FusedGetAddrFMulFAdd(a, _, _) | StackOp::FusedGetAddrFMulFSub(a, _, _)
        | StackOp::FusedGetAddrFMulFAddFW(a, _, _) | StackOp::FusedGetAddrFMulFSubFW(a, _, _) => {
            r(a, remap);
        }
        StackOp::FusedAddrLoad32OffSet(_, _, d) => r(d, remap),
        StackOp::FusedAddrImmGetStore32(_, _, src) => r(src, remap),
        // LocalAddr is a memory slot, not a scalar local — don't remap.
        _ => {}
    }
}
