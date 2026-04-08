//! Trivial function inlining pass for the stack VM.
//!
//! Identifies functions whose body is of the form
//!   LocalGet(0), LocalGet(1), ..., LocalGet(N-1), <stackless ops>, Return/ReturnVoid
//! where N == param_count, and inlines their call sites by replacing the
//! Call instruction with the stackless op sequence.
//!
//! This eliminates the call/return overhead for leaf functions like
//! `cmp(a, b) -> a - b` that are called in tight loops.

use crate::stack_ir::{StackFunction, StackOp, StackProgram};

/// Analyze a function to see if it's a trivial inline candidate.
/// Returns the ops to inline (the tail after the local.gets, minus the return)
/// if inlinable, else None.
fn trivial_inline_body(func: &StackFunction) -> Option<Vec<StackOp>> {
    // Must have no local memory and no extra locals beyond params.
    if func.local_memory != 0 {
        return None;
    }
    if func.local_count as u8 != func.param_count {
        return None;
    }

    let ops = &func.ops;
    let pc = func.param_count as usize;

    // Must start with local.get 0, local.get 1, ..., local.get pc-1.
    if ops.len() < pc + 1 {
        return None;
    }
    for i in 0..pc {
        match &ops[i] {
            StackOp::LocalGet(n) if *n as usize == i => {}
            _ => return None,
        }
    }

    // The rest, up to (but excluding) the final Return/ReturnVoid, must be
    // "stackless" ops — no calls, no jumps, no local ops.
    let last = ops.len() - 1;
    match &ops[last] {
        StackOp::Return | StackOp::ReturnVoid => {}
        _ => return None,
    }

    let tail = &ops[pc..last];
    for op in tail {
        if !is_safe_to_inline(op) {
            return None;
        }
    }

    Some(tail.to_vec())
}

/// Whether an op is safe to include in an inlined body.
/// Excludes control flow, calls, local access, and anything that could
/// have side effects across the call boundary.
fn is_safe_to_inline(op: &StackOp) -> bool {
    match op {
        // Arithmetic, comparisons, bitwise — all pure.
        StackOp::IAdd | StackOp::ISub | StackOp::IMul | StackOp::IDiv |
        StackOp::UDiv | StackOp::IRem | StackOp::IPow | StackOp::INeg |
        StackOp::IAddImm(_) |
        StackOp::FAdd | StackOp::FSub | StackOp::FMul | StackOp::FDiv |
        StackOp::FPow | StackOp::FNeg |
        StackOp::DAdd | StackOp::DSub | StackOp::DMul | StackOp::DDiv |
        StackOp::DPow | StackOp::DNeg |
        StackOp::IEq | StackOp::INe | StackOp::ILt | StackOp::ILe |
        StackOp::IGt | StackOp::IGe | StackOp::ULt | StackOp::UGt |
        StackOp::FEq | StackOp::FNe | StackOp::FLt | StackOp::FLe |
        StackOp::FGt | StackOp::FGe |
        StackOp::DEq | StackOp::DLt | StackOp::DLe |
        StackOp::And | StackOp::Or | StackOp::Xor | StackOp::Not |
        StackOp::Shl | StackOp::Shr | StackOp::UShr |
        StackOp::I32ToF32 | StackOp::F32ToI32 | StackOp::I32ToF64 |
        StackOp::F64ToI32 | StackOp::F32ToF64 | StackOp::F64ToF32 |
        StackOp::I32ToI8 | StackOp::I8ToI32 | StackOp::I64ToU32 |
        StackOp::I64Const(_) | StackOp::F32Const(_) | StackOp::F64Const(_) => true,
        _ => false,
    }
}

/// Rewrite a single function, inlining calls to trivial functions from `bodies`.
/// Adjusts all relative jump offsets to account for expanded call sites.
fn inline_calls_in(func: &mut StackFunction, bodies: &[Option<Vec<StackOp>>]) {
    // Build a mapping: old_idx -> new_idx.
    // For each old instruction, find where it (or its replacement) starts in new.
    let old_len = func.ops.len();
    let mut old_to_new = vec![0usize; old_len + 1]; // +1 for end-of-function
    let mut new_ops: Vec<StackOp> = Vec::with_capacity(old_len);

    for (i, op) in func.ops.iter().enumerate() {
        old_to_new[i] = new_ops.len();
        match op {
            StackOp::Call { func: target, .. } => {
                if let Some(body) = &bodies[*target as usize] {
                    new_ops.extend_from_slice(body);
                    continue;
                }
                new_ops.push(op.clone());
            }
            _ => new_ops.push(op.clone()),
        }
    }
    old_to_new[old_len] = new_ops.len();

    // Now fix jump offsets. For each new op that is a jump, find its original
    // index, compute the original target, look up the new position of the target,
    // and compute the new offset.
    //
    // We need to track old_idx alongside new_ops. Easier: re-iterate old ops,
    // track their new position, and fix jumps as we see them.
    let mut new_pos = 0usize;
    for (i, op) in func.ops.iter().enumerate() {
        let (start, len) = match op {
            StackOp::Call { func: target, .. } => {
                if let Some(body) = &bodies[*target as usize] {
                    (new_pos, body.len())
                } else {
                    (new_pos, 1)
                }
            }
            _ => (new_pos, 1),
        };

        // If this is a jump, fix its offset.
        let jump_target_old: Option<usize> = match op {
            StackOp::Jump(off)
            | StackOp::JumpIfZero(off)
            | StackOp::JumpIfNotZero(off) => {
                Some((i as i64 + 1 + *off as i64) as usize)
            }
            StackOp::FusedGetGetILtJumpIfZero(_, _, off) => {
                Some((i as i64 + 1 + *off as i64) as usize)
            }
            StackOp::FusedF32ConstFGtJumpIfZero(_, off) => {
                Some((i as i64 + 1 + *off as i64) as usize)
            }
            _ => None,
        };

        if let Some(target_old) = jump_target_old {
            let target_new = if target_old >= old_to_new.len() {
                new_ops.len()
            } else {
                old_to_new[target_old]
            };
            // The jump is at new position `start`. Its origin for offset
            // computation is `start + 1`. So new_off = target_new - (start + 1).
            let new_off = target_new as i64 - (start as i64 + 1);
            // len == 1 for jumps (they're never inlined).
            debug_assert!(len == 1);
            match &mut new_ops[start] {
                StackOp::Jump(o)
                | StackOp::JumpIfZero(o)
                | StackOp::JumpIfNotZero(o) => *o = new_off as i32,
                StackOp::FusedGetGetILtJumpIfZero(_, _, o) => *o = new_off as i32,
                StackOp::FusedF32ConstFGtJumpIfZero(_, o) => *o = new_off as i32,
                _ => unreachable!(),
            }
        }

        new_pos = start + len;
    }

    func.ops = new_ops;
}

/// Run the inlining pass over a program.
pub fn inline_trivial(program: &mut StackProgram) {
    // Analyze each function to find inline candidates.
    let bodies: Vec<Option<Vec<StackOp>>> = program
        .functions
        .iter()
        .map(trivial_inline_body)
        .collect();

    // Skip if nothing is inlinable.
    if bodies.iter().all(|b| b.is_none()) {
        return;
    }

    // Rewrite each function, inlining calls.
    for func in &mut program.functions {
        inline_calls_in(func, &bodies);
    }
}
