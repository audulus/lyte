//! Peephole optimizer for VM bytecode.
//!
//! Runs as a post-pass on `Vec<Opcode>` after codegen, before execution.
//! Uses NOP replacement (not removal) so jump offsets stay valid.

use crate::vm::{Opcode, Reg};
use std::collections::HashSet;

/// Phase 1: Peephole optimizations (NOP-safe, preserves instruction indices).
pub fn optimize(code: &mut Vec<Opcode>) {
    if code.is_empty() {
        return;
    }
    // Run optimization passes iteratively until no more changes.
    for _ in 0..3 {
        move_forwarding(code);
        redundant_local_addr(code);
        dead_code_elimination(code);
    }
    // Fuse LocalAddr+Load/Store into superinstructions (after all other opts)
    fuse_local_access(code);
}

/// Dead code elimination: NOP any instruction that writes to a register
/// that is never read.
fn dead_code_elimination(code: &mut Vec<Opcode>) {
    let mut uses = compute_use_counts_fast(code);
    // Register 0 is the implicit return value register — always considered live
    uses[0] = uses[0].saturating_add(1);
    for i in 0..code.len() {
        if let Some(dst) = get_dst(&code[i]) {
            if uses[dst as usize] == 0 {
                // Don't eliminate instructions with side effects
                match &code[i] {
                    Opcode::Call { .. } | Opcode::CallIndirect { .. }
                    | Opcode::Store8 { .. } | Opcode::Store32 { .. } | Opcode::Store64 { .. }
                    | Opcode::Store8Off { .. } | Opcode::Store32Off { .. } | Opcode::Store64Off { .. }
                    | Opcode::MemCopy { .. } | Opcode::MemZero { .. }
                    | Opcode::PrintI32 { .. } | Opcode::PrintF32 { .. }
                    | Opcode::Assert { .. } | Opcode::Putc { .. } => {}
                    _ => {
                        code[i] = Opcode::Nop;
                    }
                }
            }
        }
    }
}

/// Phase 2: Strip NOPs and adjust jump offsets. Call after all patching is done.
pub fn compact(code: &mut Vec<Opcode>) {
    strip_nops(code);
}

/// Pass 3: Remove all Nop instructions and adjust jump offsets.
fn strip_nops(code: &mut Vec<Opcode>) {
    let n = code.len();
    // Build a mapping from old index to new index
    let mut new_index = vec![0usize; n];
    let mut new_pos = 0usize;
    for i in 0..n {
        new_index[i] = new_pos;
        if !matches!(code[i], Opcode::Nop) {
            new_pos += 1;
        }
    }
    let new_len = new_pos;
    if new_len == n {
        return; // No Nops to strip
    }

    // Adjust jump offsets. Jump offset semantics: target = current_pos + 1 + offset
    for i in 0..n {
        if matches!(code[i], Opcode::Nop) {
            continue;
        }
        match &mut code[i] {
            Opcode::Jump { offset } => {
                let old_target = (i as i32 + 1 + *offset) as usize;
                let new_src = new_index[i] as i32;
                let new_tgt = new_index[old_target] as i32;
                *offset = new_tgt - new_src - 1;
            }
            Opcode::JumpIfZero { offset, .. }
            | Opcode::JumpIfNotZero { offset, .. }
            | Opcode::ILtJump { offset, .. } => {
                let old_target = (i as i32 + 1 + *offset) as usize;
                let new_src = new_index[i] as i32;
                let new_tgt = new_index[old_target] as i32;
                *offset = new_tgt - new_src - 1;
            }
            _ => {}
        }
    }

    // Compact: remove Nops
    let mut write = 0;
    for read in 0..n {
        if !matches!(code[read], Opcode::Nop) {
            code[write] = code[read].clone();
            write += 1;
        }
    }
    code.truncate(new_len);
}

/// Get the destination register of an instruction, if it has one.
fn get_dst(op: &Opcode) -> Option<Reg> {
    match op {
        Opcode::Move { dst, .. }
        | Opcode::LoadImm { dst, .. }
        | Opcode::LoadF32 { dst, .. }
        | Opcode::LoadF64 { dst, .. }
        | Opcode::LoadConst { dst, .. }
        | Opcode::IAdd { dst, .. }
        | Opcode::ISub { dst, .. }
        | Opcode::IMul { dst, .. }
        | Opcode::IDiv { dst, .. }
        | Opcode::UDiv { dst, .. }
        | Opcode::IRem { dst, .. }
        | Opcode::IPow { dst, .. }
        | Opcode::INeg { dst, .. }
        | Opcode::IAddImm { dst, .. }
        | Opcode::FAdd { dst, .. }
        | Opcode::FSub { dst, .. }
        | Opcode::FMul { dst, .. }
        | Opcode::FDiv { dst, .. }
        | Opcode::FNeg { dst, .. }
        | Opcode::FPow { dst, .. }
        | Opcode::FMulAdd { dst, .. }
        | Opcode::FMulSub { dst, .. }
        | Opcode::DAdd { dst, .. }
        | Opcode::DSub { dst, .. }
        | Opcode::DMul { dst, .. }
        | Opcode::DDiv { dst, .. }
        | Opcode::DNeg { dst, .. }
        | Opcode::DPow { dst, .. }
        | Opcode::DMulAdd { dst, .. }
        | Opcode::DMulSub { dst, .. }
        | Opcode::And { dst, .. }
        | Opcode::Or { dst, .. }
        | Opcode::Xor { dst, .. }
        | Opcode::Not { dst, .. }
        | Opcode::Shl { dst, .. }
        | Opcode::Shr { dst, .. }
        | Opcode::UShr { dst, .. }
        | Opcode::IEq { dst, .. }
        | Opcode::INe { dst, .. }
        | Opcode::ILt { dst, .. }
        | Opcode::ILe { dst, .. }
        | Opcode::ULt { dst, .. }
        | Opcode::FEq { dst, .. }
        | Opcode::FNe { dst, .. }
        | Opcode::FLt { dst, .. }
        | Opcode::FLe { dst, .. }
        | Opcode::MemEq { dst, .. }
        | Opcode::MemNe { dst, .. }
        | Opcode::DEq { dst, .. }
        | Opcode::DLt { dst, .. }
        | Opcode::DLe { dst, .. }
        | Opcode::I32ToF32 { dst, .. }
        | Opcode::F32ToI32 { dst, .. }
        | Opcode::I32ToF64 { dst, .. }
        | Opcode::F64ToI32 { dst, .. }
        | Opcode::F32ToF64 { dst, .. }
        | Opcode::F64ToF32 { dst, .. }
        | Opcode::Load8 { dst, .. }
        | Opcode::Load32 { dst, .. }
        | Opcode::Load64 { dst, .. }
        | Opcode::Load32Off { dst, .. }
        | Opcode::Load64Off { dst, .. }
        | Opcode::LocalAddr { dst, .. }
        | Opcode::GlobalAddr { dst, .. }
        | Opcode::SinF32 { dst, .. }
        | Opcode::CosF32 { dst, .. }
        | Opcode::TanF32 { dst, .. }
        | Opcode::LnF32 { dst, .. }
        | Opcode::ExpF32 { dst, .. }
        | Opcode::SqrtF32 { dst, .. }
        | Opcode::AbsF32 { dst, .. }
        | Opcode::FloorF32 { dst, .. }
        | Opcode::CeilF32 { dst, .. }
        | Opcode::SinF64 { dst, .. }
        | Opcode::CosF64 { dst, .. }
        | Opcode::TanF64 { dst, .. }
        | Opcode::LnF64 { dst, .. }
        | Opcode::ExpF64 { dst, .. }
        | Opcode::SqrtF64 { dst, .. }
        | Opcode::AbsF64 { dst, .. }
        | Opcode::FloorF64 { dst, .. }
        | Opcode::CeilF64 { dst, .. }
        | Opcode::PowF32 { dst, .. }
        | Opcode::Atan2F32 { dst, .. }
        | Opcode::PowF64 { dst, .. }
        | Opcode::Atan2F64 { dst, .. }
        | Opcode::LoadSlot32 { dst, .. } => Some(*dst),

        Opcode::Nop
        | Opcode::StoreSlot32 { .. }
        | Opcode::Halt
        | Opcode::Store8 { .. }
        | Opcode::Store32 { .. }
        | Opcode::Store64 { .. }
        | Opcode::Store8Off { .. }
        | Opcode::Store32Off { .. }
        | Opcode::Store64Off { .. }
        | Opcode::Jump { .. }
        | Opcode::JumpIfZero { .. }
        | Opcode::JumpIfNotZero { .. }
        | Opcode::ILtJump { .. }
        | Opcode::Call { .. }
        | Opcode::CallIndirect { .. }
        | Opcode::Return
        | Opcode::ReturnReg { .. }
        | Opcode::AllocLocals { .. }
        | Opcode::MemCopy { .. }
        | Opcode::MemZero { .. }
        | Opcode::SaveRegs { .. }
        | Opcode::RestoreRegs { .. }
        | Opcode::PrintI32 { .. }
        | Opcode::PrintF32 { .. }
        | Opcode::Assert { .. }
        | Opcode::Putc { .. } => None,
    }
}

/// Rewrite the destination register of an instruction. Returns false if the
/// instruction has no dst field.
fn set_dst(op: &mut Opcode, new_dst: Reg) -> bool {
    match op {
        Opcode::Move { dst, .. }
        | Opcode::LoadImm { dst, .. }
        | Opcode::LoadF32 { dst, .. }
        | Opcode::LoadF64 { dst, .. }
        | Opcode::LoadConst { dst, .. }
        | Opcode::IAdd { dst, .. }
        | Opcode::ISub { dst, .. }
        | Opcode::IMul { dst, .. }
        | Opcode::IDiv { dst, .. }
        | Opcode::UDiv { dst, .. }
        | Opcode::IRem { dst, .. }
        | Opcode::IPow { dst, .. }
        | Opcode::INeg { dst, .. }
        | Opcode::IAddImm { dst, .. }
        | Opcode::FAdd { dst, .. }
        | Opcode::FSub { dst, .. }
        | Opcode::FMul { dst, .. }
        | Opcode::FDiv { dst, .. }
        | Opcode::FNeg { dst, .. }
        | Opcode::FPow { dst, .. }
        | Opcode::FMulAdd { dst, .. }
        | Opcode::FMulSub { dst, .. }
        | Opcode::DAdd { dst, .. }
        | Opcode::DSub { dst, .. }
        | Opcode::DMul { dst, .. }
        | Opcode::DDiv { dst, .. }
        | Opcode::DNeg { dst, .. }
        | Opcode::DPow { dst, .. }
        | Opcode::DMulAdd { dst, .. }
        | Opcode::DMulSub { dst, .. }
        | Opcode::And { dst, .. }
        | Opcode::Or { dst, .. }
        | Opcode::Xor { dst, .. }
        | Opcode::Not { dst, .. }
        | Opcode::Shl { dst, .. }
        | Opcode::Shr { dst, .. }
        | Opcode::UShr { dst, .. }
        | Opcode::IEq { dst, .. }
        | Opcode::INe { dst, .. }
        | Opcode::ILt { dst, .. }
        | Opcode::ILe { dst, .. }
        | Opcode::ULt { dst, .. }
        | Opcode::FEq { dst, .. }
        | Opcode::FNe { dst, .. }
        | Opcode::FLt { dst, .. }
        | Opcode::FLe { dst, .. }
        | Opcode::MemEq { dst, .. }
        | Opcode::MemNe { dst, .. }
        | Opcode::DEq { dst, .. }
        | Opcode::DLt { dst, .. }
        | Opcode::DLe { dst, .. }
        | Opcode::I32ToF32 { dst, .. }
        | Opcode::F32ToI32 { dst, .. }
        | Opcode::I32ToF64 { dst, .. }
        | Opcode::F64ToI32 { dst, .. }
        | Opcode::F32ToF64 { dst, .. }
        | Opcode::F64ToF32 { dst, .. }
        | Opcode::Load8 { dst, .. }
        | Opcode::Load32 { dst, .. }
        | Opcode::Load64 { dst, .. }
        | Opcode::Load32Off { dst, .. }
        | Opcode::Load64Off { dst, .. }
        | Opcode::LocalAddr { dst, .. }
        | Opcode::GlobalAddr { dst, .. }
        | Opcode::SinF32 { dst, .. }
        | Opcode::CosF32 { dst, .. }
        | Opcode::TanF32 { dst, .. }
        | Opcode::LnF32 { dst, .. }
        | Opcode::ExpF32 { dst, .. }
        | Opcode::SqrtF32 { dst, .. }
        | Opcode::AbsF32 { dst, .. }
        | Opcode::FloorF32 { dst, .. }
        | Opcode::CeilF32 { dst, .. }
        | Opcode::SinF64 { dst, .. }
        | Opcode::CosF64 { dst, .. }
        | Opcode::TanF64 { dst, .. }
        | Opcode::LnF64 { dst, .. }
        | Opcode::ExpF64 { dst, .. }
        | Opcode::SqrtF64 { dst, .. }
        | Opcode::AbsF64 { dst, .. }
        | Opcode::FloorF64 { dst, .. }
        | Opcode::CeilF64 { dst, .. }
        | Opcode::PowF32 { dst, .. }
        | Opcode::Atan2F32 { dst, .. }
        | Opcode::PowF64 { dst, .. }
        | Opcode::Atan2F64 { dst, .. }
        | Opcode::LoadSlot32 { dst, .. } => {
            *dst = new_dst;
            true
        }
        _ => false,
    }
}

/// Check if an instruction reads the given register as a source operand.
fn reads_reg(op: &Opcode, reg: Reg) -> bool {
    match op {
        Opcode::Nop | Opcode::Halt | Opcode::Return | Opcode::AllocLocals { .. }
        | Opcode::LoadSlot32 { .. } => false,

        Opcode::Move { src, .. } => *src == reg,
        Opcode::LoadImm { .. } | Opcode::LoadF32 { .. } | Opcode::LoadF64 { .. }
        | Opcode::LoadConst { .. } | Opcode::LocalAddr { .. } | Opcode::GlobalAddr { .. } => false,

        Opcode::IAdd { a, b, .. } | Opcode::ISub { a, b, .. } | Opcode::IMul { a, b, .. }
        | Opcode::IDiv { a, b, .. } | Opcode::UDiv { a, b, .. } | Opcode::IRem { a, b, .. }
        | Opcode::IPow { a, b, .. } => *a == reg || *b == reg,

        Opcode::INeg { src, .. } => *src == reg,
        Opcode::IAddImm { src, .. } => *src == reg,

        Opcode::FAdd { a, b, .. } | Opcode::FSub { a, b, .. } | Opcode::FMul { a, b, .. }
        | Opcode::FDiv { a, b, .. } | Opcode::FPow { a, b, .. } => *a == reg || *b == reg,
        Opcode::FNeg { src, .. } => *src == reg,
        Opcode::FMulAdd { a, b, c, .. } | Opcode::FMulSub { a, b, c, .. } => *a == reg || *b == reg || *c == reg,

        Opcode::DAdd { a, b, .. } | Opcode::DSub { a, b, .. } | Opcode::DMul { a, b, .. }
        | Opcode::DDiv { a, b, .. } | Opcode::DPow { a, b, .. } => *a == reg || *b == reg,
        Opcode::DNeg { src, .. } => *src == reg,
        Opcode::DMulAdd { a, b, c, .. } | Opcode::DMulSub { a, b, c, .. } => *a == reg || *b == reg || *c == reg,

        Opcode::And { a, b, .. } | Opcode::Or { a, b, .. } | Opcode::Xor { a, b, .. }
        | Opcode::Shl { a, b, .. } | Opcode::Shr { a, b, .. } | Opcode::UShr { a, b, .. } => *a == reg || *b == reg,
        Opcode::Not { src, .. } => *src == reg,

        Opcode::IEq { a, b, .. } | Opcode::INe { a, b, .. } | Opcode::ILt { a, b, .. }
        | Opcode::ILe { a, b, .. } | Opcode::ULt { a, b, .. } => *a == reg || *b == reg,
        Opcode::FEq { a, b, .. } | Opcode::FNe { a, b, .. } | Opcode::FLt { a, b, .. }
        | Opcode::FLe { a, b, .. } => *a == reg || *b == reg,
        Opcode::MemEq { a, b, .. } | Opcode::MemNe { a, b, .. } => *a == reg || *b == reg,
        Opcode::DEq { a, b, .. } | Opcode::DLt { a, b, .. } | Opcode::DLe { a, b, .. } => *a == reg || *b == reg,

        Opcode::I32ToF32 { src, .. } | Opcode::F32ToI32 { src, .. }
        | Opcode::I32ToF64 { src, .. } | Opcode::F64ToI32 { src, .. }
        | Opcode::F32ToF64 { src, .. } | Opcode::F64ToF32 { src, .. } => *src == reg,

        Opcode::Load8 { addr, .. } | Opcode::Load32 { addr, .. } | Opcode::Load64 { addr, .. } => *addr == reg,
        Opcode::Load32Off { base, .. } | Opcode::Load64Off { base, .. } => *base == reg,

        Opcode::Store8 { addr, src } | Opcode::Store32 { addr, src } | Opcode::Store64 { addr, src } => *addr == reg || *src == reg,
        Opcode::Store8Off { base, src, .. } | Opcode::Store32Off { base, src, .. }
        | Opcode::Store64Off { base, src, .. } => *base == reg || *src == reg,

        Opcode::Jump { .. } => false,
        Opcode::JumpIfZero { cond, .. } | Opcode::JumpIfNotZero { cond, .. } => *cond == reg,
        Opcode::ILtJump { a, b, .. } => *a == reg || *b == reg,

        // Calls can read any register in the args range, be conservative
        Opcode::Call { args_start, arg_count, .. } => {
            reg >= *args_start && reg < *args_start + *arg_count
        }
        Opcode::CallIndirect { func_reg, args_start, arg_count } => {
            *func_reg == reg || (reg >= *args_start && reg < *args_start + *arg_count)
        }

        Opcode::ReturnReg { src } => *src == reg,

        Opcode::MemCopy { dst, src, .. } => *dst == reg || *src == reg,
        Opcode::MemZero { dst, .. } => *dst == reg,

        // SaveRegs/RestoreRegs are function-level bookkeeping, not data flow
        Opcode::SaveRegs { .. } => false,
        Opcode::RestoreRegs { .. } => false,

        Opcode::PrintI32 { src } | Opcode::PrintF32 { src } | Opcode::Assert { src }
        | Opcode::Putc { src } => *src == reg,

        Opcode::SinF32 { src, .. } | Opcode::CosF32 { src, .. } | Opcode::TanF32 { src, .. }
        | Opcode::LnF32 { src, .. } | Opcode::ExpF32 { src, .. } | Opcode::SqrtF32 { src, .. }
        | Opcode::AbsF32 { src, .. } | Opcode::FloorF32 { src, .. } | Opcode::CeilF32 { src, .. } => *src == reg,

        Opcode::SinF64 { src, .. } | Opcode::CosF64 { src, .. } | Opcode::TanF64 { src, .. }
        | Opcode::LnF64 { src, .. } | Opcode::ExpF64 { src, .. } | Opcode::SqrtF64 { src, .. }
        | Opcode::AbsF64 { src, .. } | Opcode::FloorF64 { src, .. } | Opcode::CeilF64 { src, .. } => *src == reg,

        Opcode::PowF32 { a, b, .. } | Opcode::Atan2F32 { a, b, .. }
        | Opcode::PowF64 { a, b, .. } | Opcode::Atan2F64 { a, b, .. } => *a == reg || *b == reg,

        Opcode::StoreSlot32 { src, .. } => *src == reg,
    }
}

/// Count how many times each register is read as a source operand.
fn compute_use_counts(code: &[Opcode]) -> [u16; 256] {
    let mut counts = [0u16; 256];
    for op in code {
        for r in 0..=255u8 {
            if reads_reg(op, r) {
                counts[r as usize] = counts[r as usize].saturating_add(1);
            }
        }
    }
    counts
}

/// Efficiently compute use counts by only checking registers that exist.
fn compute_use_counts_fast(code: &[Opcode]) -> [u16; 256] {
    let mut counts = [0u16; 256];
    for op in code {
        // Extract source registers directly from each instruction
        match op {
            Opcode::Move { src, .. } => counts[*src as usize] += 1,
            Opcode::IAdd { a, b, .. } | Opcode::ISub { a, b, .. } | Opcode::IMul { a, b, .. }
            | Opcode::IDiv { a, b, .. } | Opcode::UDiv { a, b, .. } | Opcode::IRem { a, b, .. }
            | Opcode::IPow { a, b, .. } => { counts[*a as usize] += 1; counts[*b as usize] += 1; }
            Opcode::INeg { src, .. } | Opcode::IAddImm { src, .. } => counts[*src as usize] += 1,
            Opcode::FAdd { a, b, .. } | Opcode::FSub { a, b, .. } | Opcode::FMul { a, b, .. }
            | Opcode::FDiv { a, b, .. } | Opcode::FPow { a, b, .. } => { counts[*a as usize] += 1; counts[*b as usize] += 1; }
            Opcode::FNeg { src, .. } => counts[*src as usize] += 1,
            Opcode::FMulAdd { a, b, c, .. } | Opcode::FMulSub { a, b, c, .. } => { counts[*a as usize] += 1; counts[*b as usize] += 1; counts[*c as usize] += 1; }
            Opcode::DAdd { a, b, .. } | Opcode::DSub { a, b, .. } | Opcode::DMul { a, b, .. }
            | Opcode::DDiv { a, b, .. } | Opcode::DPow { a, b, .. } => { counts[*a as usize] += 1; counts[*b as usize] += 1; }
            Opcode::DNeg { src, .. } => counts[*src as usize] += 1,
            Opcode::DMulAdd { a, b, c, .. } | Opcode::DMulSub { a, b, c, .. } => { counts[*a as usize] += 1; counts[*b as usize] += 1; counts[*c as usize] += 1; }
            Opcode::And { a, b, .. } | Opcode::Or { a, b, .. } | Opcode::Xor { a, b, .. }
            | Opcode::Shl { a, b, .. } | Opcode::Shr { a, b, .. } | Opcode::UShr { a, b, .. } => { counts[*a as usize] += 1; counts[*b as usize] += 1; }
            Opcode::Not { src, .. } => counts[*src as usize] += 1,
            Opcode::IEq { a, b, .. } | Opcode::INe { a, b, .. } | Opcode::ILt { a, b, .. }
            | Opcode::ILe { a, b, .. } | Opcode::ULt { a, b, .. } => { counts[*a as usize] += 1; counts[*b as usize] += 1; }
            Opcode::FEq { a, b, .. } | Opcode::FNe { a, b, .. } | Opcode::FLt { a, b, .. }
            | Opcode::FLe { a, b, .. } => { counts[*a as usize] += 1; counts[*b as usize] += 1; }
            Opcode::MemEq { a, b, .. } | Opcode::MemNe { a, b, .. } => { counts[*a as usize] += 1; counts[*b as usize] += 1; }
            Opcode::DEq { a, b, .. } | Opcode::DLt { a, b, .. } | Opcode::DLe { a, b, .. } => { counts[*a as usize] += 1; counts[*b as usize] += 1; }
            Opcode::I32ToF32 { src, .. } | Opcode::F32ToI32 { src, .. }
            | Opcode::I32ToF64 { src, .. } | Opcode::F64ToI32 { src, .. }
            | Opcode::F32ToF64 { src, .. } | Opcode::F64ToF32 { src, .. } => counts[*src as usize] += 1,
            Opcode::Load8 { addr, .. } | Opcode::Load32 { addr, .. } | Opcode::Load64 { addr, .. } => counts[*addr as usize] += 1,
            Opcode::Load32Off { base, .. } | Opcode::Load64Off { base, .. } => counts[*base as usize] += 1,
            Opcode::Store8 { addr, src } | Opcode::Store32 { addr, src } | Opcode::Store64 { addr, src } => { counts[*addr as usize] += 1; counts[*src as usize] += 1; }
            Opcode::Store8Off { base, src, .. } | Opcode::Store32Off { base, src, .. }
            | Opcode::Store64Off { base, src, .. } => { counts[*base as usize] += 1; counts[*src as usize] += 1; }
            Opcode::JumpIfZero { cond, .. } | Opcode::JumpIfNotZero { cond, .. } => counts[*cond as usize] += 1,
            Opcode::ILtJump { a, b, .. } => { counts[*a as usize] += 1; counts[*b as usize] += 1; }
            Opcode::Call { args_start, arg_count, .. } => {
                for r in *args_start..(*args_start + *arg_count) { counts[r as usize] += 1; }
            }
            Opcode::CallIndirect { func_reg, args_start, arg_count } => {
                counts[*func_reg as usize] += 1;
                for r in *args_start..(*args_start + *arg_count) { counts[r as usize] += 1; }
            }
            Opcode::ReturnReg { src } => counts[*src as usize] += 1,
            Opcode::MemCopy { dst, src, .. } => { counts[*dst as usize] += 1; counts[*src as usize] += 1; }
            Opcode::MemZero { dst, .. } => counts[*dst as usize] += 1,
            // SaveRegs/RestoreRegs are function-level bookkeeping, not data flow.
            // Exclude them from use counts so move forwarding works correctly.
            Opcode::SaveRegs { .. } => {}
            Opcode::RestoreRegs { .. } => {}
            Opcode::PrintI32 { src } | Opcode::PrintF32 { src } | Opcode::Assert { src }
            | Opcode::Putc { src } => counts[*src as usize] += 1,
            Opcode::SinF32 { src, .. } | Opcode::CosF32 { src, .. } | Opcode::TanF32 { src, .. }
            | Opcode::LnF32 { src, .. } | Opcode::ExpF32 { src, .. } | Opcode::SqrtF32 { src, .. }
            | Opcode::AbsF32 { src, .. } | Opcode::FloorF32 { src, .. } | Opcode::CeilF32 { src, .. } => counts[*src as usize] += 1,
            Opcode::SinF64 { src, .. } | Opcode::CosF64 { src, .. } | Opcode::TanF64 { src, .. }
            | Opcode::LnF64 { src, .. } | Opcode::ExpF64 { src, .. } | Opcode::SqrtF64 { src, .. }
            | Opcode::AbsF64 { src, .. } | Opcode::FloorF64 { src, .. } | Opcode::CeilF64 { src, .. } => counts[*src as usize] += 1,
            Opcode::PowF32 { a, b, .. } | Opcode::Atan2F32 { a, b, .. }
            | Opcode::PowF64 { a, b, .. } | Opcode::Atan2F64 { a, b, .. } => { counts[*a as usize] += 1; counts[*b as usize] += 1; }
            Opcode::StoreSlot32 { src, .. } => counts[*src as usize] += 1,
            _ => {}
        }
    }
    counts
}

/// Pass 1: Move forwarding.
///
/// Pattern: instruction writes to R, next instruction is `Move { dst: D, src: R }`,
/// and R is only used by that Move. Rewrite the instruction to write to D directly
/// and NOP the Move.
fn move_forwarding(code: &mut [Opcode]) {
    let uses = compute_use_counts_fast(code);

    for i in 0..code.len().saturating_sub(1) {
        if let Opcode::Move { dst: move_dst, src: move_src } = code[i + 1] {
            // Check: instruction at i writes to move_src
            if let Some(prev_dst) = get_dst(&code[i]) {
                if prev_dst == move_src {
                    // Check: move_src is only read by this one Move
                    if uses[move_src as usize] == 1 {
                        // Don't forward if previous instruction is also a Move
                        // (would create a chain we can't simplify)
                        if matches!(code[i], Opcode::Move { .. }) {
                            // Still safe to forward Move→Move:
                            // Move { dst: R, src: X } + Move { dst: D, src: R }
                            // → Move { dst: D, src: X } + Nop
                        }
                        // Rewrite: make instruction i write to move_dst directly
                        set_dst(&mut code[i], move_dst);
                        code[i + 1] = Opcode::Nop;
                    }
                }
            }
        }
    }
}

/// Pass 2: Redundant LocalAddr elimination.
///
/// Within a basic block, if the same slot is loaded into a different register,
/// rewrite downstream users to reference the original register and NOP the
/// duplicate LocalAddr.
fn redundant_local_addr(code: &mut [Opcode]) {
    // Collect jump targets
    let mut targets = HashSet::new();
    for (i, op) in code.iter().enumerate() {
        let offset = match op {
            Opcode::Jump { offset } | Opcode::JumpIfZero { offset, .. }
            | Opcode::JumpIfNotZero { offset, .. } | Opcode::ILtJump { offset, .. } => Some(*offset),
            _ => None,
        };
        if let Some(off) = offset {
            let target = (i as i32 + 1 + off) as usize;
            targets.insert(target);
        }
    }

    // Track slot → register that holds its address
    // Key: slot, Value: register
    let mut slot_reg: std::collections::HashMap<u16, Reg> = std::collections::HashMap::new();

    for i in 0..code.len() {
        // Clear tracking at jump targets (control flow merge point)
        if targets.contains(&i) {
            slot_reg.clear();
        }

        match code[i] {
            Opcode::LocalAddr { dst, slot } => {
                if let Some(&existing_reg) = slot_reg.get(&slot) {
                    // Same slot already in existing_reg. Rewrite users of dst
                    // to use existing_reg instead, then NOP this instruction.
                    let old_reg = dst;
                    replace_reg_uses_until_clobber(code, i + 1, old_reg, existing_reg, &targets);
                    code[i] = Opcode::Nop;
                } else {
                    slot_reg.insert(slot, dst);
                }
            }
            // Instructions that can clobber registers: clear tracking
            Opcode::Call { .. } | Opcode::CallIndirect { .. }
            | Opcode::SaveRegs { .. } | Opcode::RestoreRegs { .. } => {
                slot_reg.clear();
            }
            // Jumps: clear tracking (we're leaving this basic block)
            Opcode::Jump { .. } | Opcode::JumpIfZero { .. }
            | Opcode::JumpIfNotZero { .. } | Opcode::ILtJump { .. } => {
                slot_reg.clear();
            }
            // If something else writes to a register that's tracked, invalidate
            _ => {
                if let Some(dst) = get_dst(&code[i]) {
                    slot_reg.retain(|_, &mut reg| reg != dst);
                }
            }
        }
    }
}

/// Replace reads of `old_reg` with `new_reg` in instructions starting from `start`,
/// stopping at control flow boundaries or when `new_reg` is clobbered.
fn replace_reg_uses_until_clobber(
    code: &mut [Opcode],
    start: usize,
    old_reg: Reg,
    new_reg: Reg,
    targets: &HashSet<usize>,
) {
    for i in start..code.len() {
        // Stop at control flow boundaries
        if targets.contains(&i) {
            break;
        }
        match &code[i] {
            Opcode::Jump { .. } | Opcode::JumpIfZero { .. }
            | Opcode::JumpIfNotZero { .. } | Opcode::ILtJump { .. }
            | Opcode::Call { .. } | Opcode::CallIndirect { .. }
            | Opcode::SaveRegs { .. } | Opcode::RestoreRegs { .. } => break,
            _ => {}
        }
        // Stop if new_reg gets clobbered
        if let Some(dst) = get_dst(&code[i]) {
            if dst == new_reg {
                break;
            }
        }
        // Replace reads of old_reg with new_reg
        replace_src_reg(&mut code[i], old_reg, new_reg);
    }
}

/// Replace all source register references of `old` with `new` in an instruction.
fn replace_src_reg(op: &mut Opcode, old: Reg, new: Reg) {
    match op {
        Opcode::Move { src, .. } => { if *src == old { *src = new; } }
        Opcode::IAdd { a, b, .. } | Opcode::ISub { a, b, .. } | Opcode::IMul { a, b, .. }
        | Opcode::IDiv { a, b, .. } | Opcode::UDiv { a, b, .. } | Opcode::IRem { a, b, .. }
        | Opcode::IPow { a, b, .. } => { if *a == old { *a = new; } if *b == old { *b = new; } }
        Opcode::INeg { src, .. } | Opcode::IAddImm { src, .. } => { if *src == old { *src = new; } }
        Opcode::FAdd { a, b, .. } | Opcode::FSub { a, b, .. } | Opcode::FMul { a, b, .. }
        | Opcode::FDiv { a, b, .. } | Opcode::FPow { a, b, .. } => { if *a == old { *a = new; } if *b == old { *b = new; } }
        Opcode::FNeg { src, .. } => { if *src == old { *src = new; } }
        Opcode::FMulAdd { a, b, c, .. } | Opcode::FMulSub { a, b, c, .. } => { if *a == old { *a = new; } if *b == old { *b = new; } if *c == old { *c = new; } }
        Opcode::DAdd { a, b, .. } | Opcode::DSub { a, b, .. } | Opcode::DMul { a, b, .. }
        | Opcode::DDiv { a, b, .. } | Opcode::DPow { a, b, .. } => { if *a == old { *a = new; } if *b == old { *b = new; } }
        Opcode::DNeg { src, .. } => { if *src == old { *src = new; } }
        Opcode::DMulAdd { a, b, c, .. } | Opcode::DMulSub { a, b, c, .. } => { if *a == old { *a = new; } if *b == old { *b = new; } if *c == old { *c = new; } }
        Opcode::And { a, b, .. } | Opcode::Or { a, b, .. } | Opcode::Xor { a, b, .. }
        | Opcode::Shl { a, b, .. } | Opcode::Shr { a, b, .. } | Opcode::UShr { a, b, .. } => { if *a == old { *a = new; } if *b == old { *b = new; } }
        Opcode::Not { src, .. } => { if *src == old { *src = new; } }
        Opcode::IEq { a, b, .. } | Opcode::INe { a, b, .. } | Opcode::ILt { a, b, .. }
        | Opcode::ILe { a, b, .. } | Opcode::ULt { a, b, .. } => { if *a == old { *a = new; } if *b == old { *b = new; } }
        Opcode::FEq { a, b, .. } | Opcode::FNe { a, b, .. } | Opcode::FLt { a, b, .. }
        | Opcode::FLe { a, b, .. } => { if *a == old { *a = new; } if *b == old { *b = new; } }
        Opcode::MemEq { a, b, .. } | Opcode::MemNe { a, b, .. } => { if *a == old { *a = new; } if *b == old { *b = new; } }
        Opcode::DEq { a, b, .. } | Opcode::DLt { a, b, .. } | Opcode::DLe { a, b, .. } => { if *a == old { *a = new; } if *b == old { *b = new; } }
        Opcode::I32ToF32 { src, .. } | Opcode::F32ToI32 { src, .. }
        | Opcode::I32ToF64 { src, .. } | Opcode::F64ToI32 { src, .. }
        | Opcode::F32ToF64 { src, .. } | Opcode::F64ToF32 { src, .. } => { if *src == old { *src = new; } }
        Opcode::Load8 { addr, .. } | Opcode::Load32 { addr, .. } | Opcode::Load64 { addr, .. } => { if *addr == old { *addr = new; } }
        Opcode::Load32Off { base, .. } | Opcode::Load64Off { base, .. } => { if *base == old { *base = new; } }
        Opcode::Store8 { addr, src } | Opcode::Store32 { addr, src } | Opcode::Store64 { addr, src } => { if *addr == old { *addr = new; } if *src == old { *src = new; } }
        Opcode::Store8Off { base, src, .. } | Opcode::Store32Off { base, src, .. }
        | Opcode::Store64Off { base, src, .. } => { if *base == old { *base = new; } if *src == old { *src = new; } }
        Opcode::JumpIfZero { cond, .. } | Opcode::JumpIfNotZero { cond, .. } => { if *cond == old { *cond = new; } }
        Opcode::ILtJump { a, b, .. } => { if *a == old { *a = new; } if *b == old { *b = new; } }
        Opcode::ReturnReg { src } => { if *src == old { *src = new; } }
        Opcode::MemCopy { dst, src, .. } => { if *dst == old { *dst = new; } if *src == old { *src = new; } }
        Opcode::MemZero { dst, .. } => { if *dst == old { *dst = new; } }
        Opcode::PrintI32 { src } | Opcode::PrintF32 { src } | Opcode::Assert { src }
        | Opcode::Putc { src } => { if *src == old { *src = new; } }
        Opcode::SinF32 { src, .. } | Opcode::CosF32 { src, .. } | Opcode::TanF32 { src, .. }
        | Opcode::LnF32 { src, .. } | Opcode::ExpF32 { src, .. } | Opcode::SqrtF32 { src, .. }
        | Opcode::AbsF32 { src, .. } | Opcode::FloorF32 { src, .. } | Opcode::CeilF32 { src, .. } => { if *src == old { *src = new; } }
        Opcode::SinF64 { src, .. } | Opcode::CosF64 { src, .. } | Opcode::TanF64 { src, .. }
        | Opcode::LnF64 { src, .. } | Opcode::ExpF64 { src, .. } | Opcode::SqrtF64 { src, .. }
        | Opcode::AbsF64 { src, .. } | Opcode::FloorF64 { src, .. } | Opcode::CeilF64 { src, .. } => { if *src == old { *src = new; } }
        Opcode::PowF32 { a, b, .. } | Opcode::Atan2F32 { a, b, .. }
        | Opcode::PowF64 { a, b, .. } | Opcode::Atan2F64 { a, b, .. } => { if *a == old { *a = new; } if *b == old { *b = new; } }
        Opcode::StoreSlot32 { src, .. } => { if *src == old { *src = new; } }
        _ => {}
    }
}

/// Fuse LocalAddr + Load32 → LoadSlot32, LocalAddr + Store32 → StoreSlot32.
///
/// Pattern: `LocalAddr { dst: A, slot: S }` followed by `Load32 { dst: B, addr: A }`
/// where A is only used by the Load32. Fuse into `LoadSlot32 { dst: B, slot: S }`.
/// Similarly for Store32.
fn fuse_local_access(code: &mut Vec<Opcode>) {
    let uses = compute_use_counts_fast(code);

    for i in 0..code.len().saturating_sub(1) {
        if let Opcode::LocalAddr { dst: addr_reg, slot } = code[i] {
            // Only fuse if the address register is used exactly once (by the next instruction)
            if uses[addr_reg as usize] != 1 {
                continue;
            }
            match code[i + 1] {
                Opcode::Load32 { dst, addr } if addr == addr_reg => {
                    code[i] = Opcode::LoadSlot32 { dst, slot };
                    code[i + 1] = Opcode::Nop;
                }
                Opcode::Store32 { addr, src } if addr == addr_reg => {
                    code[i] = Opcode::StoreSlot32 { slot, src };
                    code[i + 1] = Opcode::Nop;
                }
                _ => {}
            }
        }
    }
}
