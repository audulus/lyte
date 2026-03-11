//! Peephole optimizer for VM bytecode.
//!
//! Runs as a post-pass on `Vec<Opcode>` after codegen, before execution.
//! Uses NOP replacement (not removal) so jump offsets stay valid.

use crate::vm::{Opcode, Reg};
use std::collections::HashSet;

/// Phase 1: Peephole optimizations (NOP-safe, preserves instruction indices).
/// Returns (new_reg_count, vreg_to_preg_mapping) after register allocation.
pub fn optimize(code: &mut Vec<Opcode>) -> Option<(u8, [u8; 256])> {
    if code.is_empty() {
        return None;
    }
    // Run optimization passes iteratively until no more changes.
    for _ in 0..3 {
        move_forwarding(code);
        redundant_local_addr(code);
        dead_code_elimination(code);
    }
    // Fuse LocalAddr+Load/Store into superinstructions (after all other opts)
    fuse_local_access(code);
    // Fuse IAddImm+Load/Store into offset-addressing superinstructions
    fuse_offset_access(code);
    // Fuse compare+branch into single instructions
    fuse_compare_branch(code);
    // Register allocation: compact register numbering via linear scan
    let (count, mapping) = register_allocation(code);
    Some((count, mapping))
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
                    Opcode::Call { .. }
                    | Opcode::CallIndirect { .. }
                    | Opcode::Store8 { .. }
                    | Opcode::Store32 { .. }
                    | Opcode::Store64 { .. }
                    | Opcode::Store8Off { .. }
                    | Opcode::Store32Off { .. }
                    | Opcode::Store64Off { .. }
                    | Opcode::MemCopy { .. }
                    | Opcode::MemZero { .. }
                    | Opcode::PrintI32 { .. }
                    | Opcode::PrintF32 { .. }
                    | Opcode::Assert { .. }
                    | Opcode::Putc { .. } => {}
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
            | Opcode::ILtJump { offset, .. }
            | Opcode::FLtJump { offset, .. } => {
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
pub fn get_dst(op: &Opcode) -> Option<Reg> {
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
        | Opcode::AsinF32 { dst, .. }
        | Opcode::AcosF32 { dst, .. }
        | Opcode::AtanF32 { dst, .. }
        | Opcode::SinhF32 { dst, .. }
        | Opcode::CoshF32 { dst, .. }
        | Opcode::TanhF32 { dst, .. }
        | Opcode::AsinhF32 { dst, .. }
        | Opcode::AcoshF32 { dst, .. }
        | Opcode::AtanhF32 { dst, .. }
        | Opcode::Exp2F32 { dst, .. }
        | Opcode::Log10F32 { dst, .. }
        | Opcode::Log2F32 { dst, .. }
        | Opcode::IsinfF32 { dst, .. }
        | Opcode::IsnanF32 { dst, .. }
        | Opcode::SinF64 { dst, .. }
        | Opcode::CosF64 { dst, .. }
        | Opcode::TanF64 { dst, .. }
        | Opcode::LnF64 { dst, .. }
        | Opcode::ExpF64 { dst, .. }
        | Opcode::SqrtF64 { dst, .. }
        | Opcode::AbsF64 { dst, .. }
        | Opcode::FloorF64 { dst, .. }
        | Opcode::CeilF64 { dst, .. }
        | Opcode::AsinF64 { dst, .. }
        | Opcode::AcosF64 { dst, .. }
        | Opcode::AtanF64 { dst, .. }
        | Opcode::SinhF64 { dst, .. }
        | Opcode::CoshF64 { dst, .. }
        | Opcode::TanhF64 { dst, .. }
        | Opcode::AsinhF64 { dst, .. }
        | Opcode::AcoshF64 { dst, .. }
        | Opcode::AtanhF64 { dst, .. }
        | Opcode::Exp2F64 { dst, .. }
        | Opcode::Log10F64 { dst, .. }
        | Opcode::Log2F64 { dst, .. }
        | Opcode::IsinfF64 { dst, .. }
        | Opcode::IsnanF64 { dst, .. }
        | Opcode::PowF32 { dst, .. }
        | Opcode::Atan2F32 { dst, .. }
        | Opcode::MinF32 { dst, .. }
        | Opcode::MaxF32 { dst, .. }
        | Opcode::PowF64 { dst, .. }
        | Opcode::Atan2F64 { dst, .. }
        | Opcode::MinF64 { dst, .. }
        | Opcode::MaxF64 { dst, .. }
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
        | Opcode::FLtJump { .. }
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
        | Opcode::AsinF32 { dst, .. }
        | Opcode::AcosF32 { dst, .. }
        | Opcode::AtanF32 { dst, .. }
        | Opcode::SinhF32 { dst, .. }
        | Opcode::CoshF32 { dst, .. }
        | Opcode::TanhF32 { dst, .. }
        | Opcode::AsinhF32 { dst, .. }
        | Opcode::AcoshF32 { dst, .. }
        | Opcode::AtanhF32 { dst, .. }
        | Opcode::Exp2F32 { dst, .. }
        | Opcode::Log10F32 { dst, .. }
        | Opcode::Log2F32 { dst, .. }
        | Opcode::IsinfF32 { dst, .. }
        | Opcode::IsnanF32 { dst, .. }
        | Opcode::SinF64 { dst, .. }
        | Opcode::CosF64 { dst, .. }
        | Opcode::TanF64 { dst, .. }
        | Opcode::LnF64 { dst, .. }
        | Opcode::ExpF64 { dst, .. }
        | Opcode::SqrtF64 { dst, .. }
        | Opcode::AbsF64 { dst, .. }
        | Opcode::FloorF64 { dst, .. }
        | Opcode::CeilF64 { dst, .. }
        | Opcode::AsinF64 { dst, .. }
        | Opcode::AcosF64 { dst, .. }
        | Opcode::AtanF64 { dst, .. }
        | Opcode::SinhF64 { dst, .. }
        | Opcode::CoshF64 { dst, .. }
        | Opcode::TanhF64 { dst, .. }
        | Opcode::AsinhF64 { dst, .. }
        | Opcode::AcoshF64 { dst, .. }
        | Opcode::AtanhF64 { dst, .. }
        | Opcode::Exp2F64 { dst, .. }
        | Opcode::Log10F64 { dst, .. }
        | Opcode::Log2F64 { dst, .. }
        | Opcode::IsinfF64 { dst, .. }
        | Opcode::IsnanF64 { dst, .. }
        | Opcode::PowF32 { dst, .. }
        | Opcode::Atan2F32 { dst, .. }
        | Opcode::MinF32 { dst, .. }
        | Opcode::MaxF32 { dst, .. }
        | Opcode::PowF64 { dst, .. }
        | Opcode::Atan2F64 { dst, .. }
        | Opcode::MinF64 { dst, .. }
        | Opcode::MaxF64 { dst, .. }
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
        Opcode::Nop
        | Opcode::Halt
        | Opcode::Return
        | Opcode::AllocLocals { .. }
        | Opcode::LoadSlot32 { .. } => false,

        Opcode::Move { src, .. } => *src == reg,
        Opcode::LoadImm { .. }
        | Opcode::LoadF32 { .. }
        | Opcode::LoadF64 { .. }
        | Opcode::LoadConst { .. }
        | Opcode::LocalAddr { .. }
        | Opcode::GlobalAddr { .. } => false,

        Opcode::IAdd { a, b, .. }
        | Opcode::ISub { a, b, .. }
        | Opcode::IMul { a, b, .. }
        | Opcode::IDiv { a, b, .. }
        | Opcode::UDiv { a, b, .. }
        | Opcode::IRem { a, b, .. }
        | Opcode::IPow { a, b, .. } => *a == reg || *b == reg,

        Opcode::INeg { src, .. } => *src == reg,
        Opcode::IAddImm { src, .. } => *src == reg,

        Opcode::FAdd { a, b, .. }
        | Opcode::FSub { a, b, .. }
        | Opcode::FMul { a, b, .. }
        | Opcode::FDiv { a, b, .. }
        | Opcode::FPow { a, b, .. } => *a == reg || *b == reg,
        Opcode::FNeg { src, .. } => *src == reg,
        Opcode::FMulAdd { a, b, c, .. } | Opcode::FMulSub { a, b, c, .. } => {
            *a == reg || *b == reg || *c == reg
        }

        Opcode::DAdd { a, b, .. }
        | Opcode::DSub { a, b, .. }
        | Opcode::DMul { a, b, .. }
        | Opcode::DDiv { a, b, .. }
        | Opcode::DPow { a, b, .. } => *a == reg || *b == reg,
        Opcode::DNeg { src, .. } => *src == reg,
        Opcode::DMulAdd { a, b, c, .. } | Opcode::DMulSub { a, b, c, .. } => {
            *a == reg || *b == reg || *c == reg
        }

        Opcode::And { a, b, .. }
        | Opcode::Or { a, b, .. }
        | Opcode::Xor { a, b, .. }
        | Opcode::Shl { a, b, .. }
        | Opcode::Shr { a, b, .. }
        | Opcode::UShr { a, b, .. } => *a == reg || *b == reg,
        Opcode::Not { src, .. } => *src == reg,

        Opcode::IEq { a, b, .. }
        | Opcode::INe { a, b, .. }
        | Opcode::ILt { a, b, .. }
        | Opcode::ILe { a, b, .. }
        | Opcode::ULt { a, b, .. } => *a == reg || *b == reg,
        Opcode::FEq { a, b, .. }
        | Opcode::FNe { a, b, .. }
        | Opcode::FLt { a, b, .. }
        | Opcode::FLe { a, b, .. } => *a == reg || *b == reg,
        Opcode::MemEq { a, b, .. } | Opcode::MemNe { a, b, .. } => *a == reg || *b == reg,
        Opcode::DEq { a, b, .. } | Opcode::DLt { a, b, .. } | Opcode::DLe { a, b, .. } => {
            *a == reg || *b == reg
        }

        Opcode::I32ToF32 { src, .. }
        | Opcode::F32ToI32 { src, .. }
        | Opcode::I32ToF64 { src, .. }
        | Opcode::F64ToI32 { src, .. }
        | Opcode::F32ToF64 { src, .. }
        | Opcode::F64ToF32 { src, .. } => *src == reg,

        Opcode::Load8 { addr, .. } | Opcode::Load32 { addr, .. } | Opcode::Load64 { addr, .. } => {
            *addr == reg
        }
        Opcode::Load32Off { base, .. } | Opcode::Load64Off { base, .. } => *base == reg,

        Opcode::Store8 { addr, src }
        | Opcode::Store32 { addr, src }
        | Opcode::Store64 { addr, src } => *addr == reg || *src == reg,
        Opcode::Store8Off { base, src, .. }
        | Opcode::Store32Off { base, src, .. }
        | Opcode::Store64Off { base, src, .. } => *base == reg || *src == reg,

        Opcode::Jump { .. } => false,
        Opcode::JumpIfZero { cond, .. } | Opcode::JumpIfNotZero { cond, .. } => *cond == reg,
        Opcode::ILtJump { a, b, .. } | Opcode::FLtJump { a, b, .. } => *a == reg || *b == reg,

        // Calls can read any register in the args range, be conservative
        Opcode::Call {
            args_start,
            arg_count,
            ..
        } => reg >= *args_start && reg < *args_start + *arg_count,
        Opcode::CallIndirect {
            func_reg,
            args_start,
            arg_count,
        } => *func_reg == reg || (reg >= *args_start && reg < *args_start + *arg_count),

        Opcode::ReturnReg { src } => *src == reg,

        Opcode::MemCopy { dst, src, .. } => *dst == reg || *src == reg,
        Opcode::MemZero { dst, .. } => *dst == reg,

        // SaveRegs/RestoreRegs are function-level bookkeeping, not data flow
        Opcode::SaveRegs { .. } => false,
        Opcode::RestoreRegs { .. } => false,

        Opcode::PrintI32 { src }
        | Opcode::PrintF32 { src }
        | Opcode::Assert { src }
        | Opcode::Putc { src } => *src == reg,

        Opcode::SinF32 { src, .. }
        | Opcode::CosF32 { src, .. }
        | Opcode::TanF32 { src, .. }
        | Opcode::LnF32 { src, .. }
        | Opcode::ExpF32 { src, .. }
        | Opcode::SqrtF32 { src, .. }
        | Opcode::AbsF32 { src, .. }
        | Opcode::FloorF32 { src, .. }
        | Opcode::CeilF32 { src, .. }
        | Opcode::AsinF32 { src, .. }
        | Opcode::AcosF32 { src, .. }
        | Opcode::AtanF32 { src, .. }
        | Opcode::SinhF32 { src, .. }
        | Opcode::CoshF32 { src, .. }
        | Opcode::TanhF32 { src, .. }
        | Opcode::AsinhF32 { src, .. }
        | Opcode::AcoshF32 { src, .. }
        | Opcode::AtanhF32 { src, .. }
        | Opcode::Exp2F32 { src, .. }
        | Opcode::Log10F32 { src, .. }
        | Opcode::Log2F32 { src, .. }
        | Opcode::IsinfF32 { src, .. }
        | Opcode::IsnanF32 { src, .. } => *src == reg,

        Opcode::SinF64 { src, .. }
        | Opcode::CosF64 { src, .. }
        | Opcode::TanF64 { src, .. }
        | Opcode::LnF64 { src, .. }
        | Opcode::ExpF64 { src, .. }
        | Opcode::SqrtF64 { src, .. }
        | Opcode::AbsF64 { src, .. }
        | Opcode::FloorF64 { src, .. }
        | Opcode::CeilF64 { src, .. }
        | Opcode::AsinF64 { src, .. }
        | Opcode::AcosF64 { src, .. }
        | Opcode::AtanF64 { src, .. }
        | Opcode::SinhF64 { src, .. }
        | Opcode::CoshF64 { src, .. }
        | Opcode::TanhF64 { src, .. }
        | Opcode::AsinhF64 { src, .. }
        | Opcode::AcoshF64 { src, .. }
        | Opcode::AtanhF64 { src, .. }
        | Opcode::Exp2F64 { src, .. }
        | Opcode::Log10F64 { src, .. }
        | Opcode::Log2F64 { src, .. }
        | Opcode::IsinfF64 { src, .. }
        | Opcode::IsnanF64 { src, .. } => *src == reg,

        Opcode::PowF32 { a, b, .. }
        | Opcode::Atan2F32 { a, b, .. }
        | Opcode::MinF32 { a, b, .. }
        | Opcode::MaxF32 { a, b, .. }
        | Opcode::PowF64 { a, b, .. }
        | Opcode::Atan2F64 { a, b, .. }
        | Opcode::MinF64 { a, b, .. }
        | Opcode::MaxF64 { a, b, .. } => *a == reg || *b == reg,

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
            Opcode::IAdd { a, b, .. }
            | Opcode::ISub { a, b, .. }
            | Opcode::IMul { a, b, .. }
            | Opcode::IDiv { a, b, .. }
            | Opcode::UDiv { a, b, .. }
            | Opcode::IRem { a, b, .. }
            | Opcode::IPow { a, b, .. } => {
                counts[*a as usize] += 1;
                counts[*b as usize] += 1;
            }
            Opcode::INeg { src, .. } | Opcode::IAddImm { src, .. } => counts[*src as usize] += 1,
            Opcode::FAdd { a, b, .. }
            | Opcode::FSub { a, b, .. }
            | Opcode::FMul { a, b, .. }
            | Opcode::FDiv { a, b, .. }
            | Opcode::FPow { a, b, .. } => {
                counts[*a as usize] += 1;
                counts[*b as usize] += 1;
            }
            Opcode::FNeg { src, .. } => counts[*src as usize] += 1,
            Opcode::FMulAdd { a, b, c, .. } | Opcode::FMulSub { a, b, c, .. } => {
                counts[*a as usize] += 1;
                counts[*b as usize] += 1;
                counts[*c as usize] += 1;
            }
            Opcode::DAdd { a, b, .. }
            | Opcode::DSub { a, b, .. }
            | Opcode::DMul { a, b, .. }
            | Opcode::DDiv { a, b, .. }
            | Opcode::DPow { a, b, .. } => {
                counts[*a as usize] += 1;
                counts[*b as usize] += 1;
            }
            Opcode::DNeg { src, .. } => counts[*src as usize] += 1,
            Opcode::DMulAdd { a, b, c, .. } | Opcode::DMulSub { a, b, c, .. } => {
                counts[*a as usize] += 1;
                counts[*b as usize] += 1;
                counts[*c as usize] += 1;
            }
            Opcode::And { a, b, .. }
            | Opcode::Or { a, b, .. }
            | Opcode::Xor { a, b, .. }
            | Opcode::Shl { a, b, .. }
            | Opcode::Shr { a, b, .. }
            | Opcode::UShr { a, b, .. } => {
                counts[*a as usize] += 1;
                counts[*b as usize] += 1;
            }
            Opcode::Not { src, .. } => counts[*src as usize] += 1,
            Opcode::IEq { a, b, .. }
            | Opcode::INe { a, b, .. }
            | Opcode::ILt { a, b, .. }
            | Opcode::ILe { a, b, .. }
            | Opcode::ULt { a, b, .. } => {
                counts[*a as usize] += 1;
                counts[*b as usize] += 1;
            }
            Opcode::FEq { a, b, .. }
            | Opcode::FNe { a, b, .. }
            | Opcode::FLt { a, b, .. }
            | Opcode::FLe { a, b, .. } => {
                counts[*a as usize] += 1;
                counts[*b as usize] += 1;
            }
            Opcode::MemEq { a, b, .. } | Opcode::MemNe { a, b, .. } => {
                counts[*a as usize] += 1;
                counts[*b as usize] += 1;
            }
            Opcode::DEq { a, b, .. } | Opcode::DLt { a, b, .. } | Opcode::DLe { a, b, .. } => {
                counts[*a as usize] += 1;
                counts[*b as usize] += 1;
            }
            Opcode::I32ToF32 { src, .. }
            | Opcode::F32ToI32 { src, .. }
            | Opcode::I32ToF64 { src, .. }
            | Opcode::F64ToI32 { src, .. }
            | Opcode::F32ToF64 { src, .. }
            | Opcode::F64ToF32 { src, .. } => counts[*src as usize] += 1,
            Opcode::Load8 { addr, .. }
            | Opcode::Load32 { addr, .. }
            | Opcode::Load64 { addr, .. } => counts[*addr as usize] += 1,
            Opcode::Load32Off { base, .. } | Opcode::Load64Off { base, .. } => {
                counts[*base as usize] += 1
            }
            Opcode::Store8 { addr, src }
            | Opcode::Store32 { addr, src }
            | Opcode::Store64 { addr, src } => {
                counts[*addr as usize] += 1;
                counts[*src as usize] += 1;
            }
            Opcode::Store8Off { base, src, .. }
            | Opcode::Store32Off { base, src, .. }
            | Opcode::Store64Off { base, src, .. } => {
                counts[*base as usize] += 1;
                counts[*src as usize] += 1;
            }
            Opcode::JumpIfZero { cond, .. } | Opcode::JumpIfNotZero { cond, .. } => {
                counts[*cond as usize] += 1
            }
            Opcode::ILtJump { a, b, .. } | Opcode::FLtJump { a, b, .. } => {
                counts[*a as usize] += 1;
                counts[*b as usize] += 1;
            }
            Opcode::Call {
                args_start,
                arg_count,
                ..
            } => {
                for r in *args_start..(*args_start + *arg_count) {
                    counts[r as usize] += 1;
                }
            }
            Opcode::CallIndirect {
                func_reg,
                args_start,
                arg_count,
            } => {
                counts[*func_reg as usize] += 1;
                for r in *args_start..(*args_start + *arg_count) {
                    counts[r as usize] += 1;
                }
            }
            Opcode::ReturnReg { src } => counts[*src as usize] += 1,
            Opcode::MemCopy { dst, src, .. } => {
                counts[*dst as usize] += 1;
                counts[*src as usize] += 1;
            }
            Opcode::MemZero { dst, .. } => counts[*dst as usize] += 1,
            // SaveRegs/RestoreRegs are function-level bookkeeping, not data flow.
            // Exclude them from use counts so move forwarding works correctly.
            Opcode::SaveRegs { .. } => {}
            Opcode::RestoreRegs { .. } => {}
            Opcode::PrintI32 { src }
            | Opcode::PrintF32 { src }
            | Opcode::Assert { src }
            | Opcode::Putc { src } => counts[*src as usize] += 1,
            Opcode::SinF32 { src, .. }
            | Opcode::CosF32 { src, .. }
            | Opcode::TanF32 { src, .. }
            | Opcode::LnF32 { src, .. }
            | Opcode::ExpF32 { src, .. }
            | Opcode::SqrtF32 { src, .. }
            | Opcode::AbsF32 { src, .. }
            | Opcode::FloorF32 { src, .. }
            | Opcode::CeilF32 { src, .. }
            | Opcode::AsinF32 { src, .. }
            | Opcode::AcosF32 { src, .. }
            | Opcode::AtanF32 { src, .. }
            | Opcode::SinhF32 { src, .. }
            | Opcode::CoshF32 { src, .. }
            | Opcode::TanhF32 { src, .. }
            | Opcode::AsinhF32 { src, .. }
            | Opcode::AcoshF32 { src, .. }
            | Opcode::AtanhF32 { src, .. }
            | Opcode::Exp2F32 { src, .. }
            | Opcode::Log10F32 { src, .. }
            | Opcode::Log2F32 { src, .. }
            | Opcode::IsinfF32 { src, .. }
            | Opcode::IsnanF32 { src, .. } => counts[*src as usize] += 1,
            Opcode::SinF64 { src, .. }
            | Opcode::CosF64 { src, .. }
            | Opcode::TanF64 { src, .. }
            | Opcode::LnF64 { src, .. }
            | Opcode::ExpF64 { src, .. }
            | Opcode::SqrtF64 { src, .. }
            | Opcode::AbsF64 { src, .. }
            | Opcode::FloorF64 { src, .. }
            | Opcode::CeilF64 { src, .. }
            | Opcode::AsinF64 { src, .. }
            | Opcode::AcosF64 { src, .. }
            | Opcode::AtanF64 { src, .. }
            | Opcode::SinhF64 { src, .. }
            | Opcode::CoshF64 { src, .. }
            | Opcode::TanhF64 { src, .. }
            | Opcode::AsinhF64 { src, .. }
            | Opcode::AcoshF64 { src, .. }
            | Opcode::AtanhF64 { src, .. }
            | Opcode::Exp2F64 { src, .. }
            | Opcode::Log10F64 { src, .. }
            | Opcode::Log2F64 { src, .. }
            | Opcode::IsinfF64 { src, .. }
            | Opcode::IsnanF64 { src, .. } => counts[*src as usize] += 1,
            Opcode::PowF32 { a, b, .. }
            | Opcode::Atan2F32 { a, b, .. }
            | Opcode::MinF32 { a, b, .. }
            | Opcode::MaxF32 { a, b, .. }
            | Opcode::PowF64 { a, b, .. }
            | Opcode::Atan2F64 { a, b, .. }
            | Opcode::MinF64 { a, b, .. }
            | Opcode::MaxF64 { a, b, .. } => {
                counts[*a as usize] += 1;
                counts[*b as usize] += 1;
            }
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
        if let Opcode::Move {
            dst: move_dst,
            src: move_src,
        } = code[i + 1]
        {
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
            Opcode::Jump { offset }
            | Opcode::JumpIfZero { offset, .. }
            | Opcode::JumpIfNotZero { offset, .. }
            | Opcode::ILtJump { offset, .. }
            | Opcode::FLtJump { offset, .. } => Some(*offset),
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
            Opcode::Call { .. }
            | Opcode::CallIndirect { .. }
            | Opcode::SaveRegs { .. }
            | Opcode::RestoreRegs { .. } => {
                slot_reg.clear();
            }
            // Jumps: clear tracking (we're leaving this basic block)
            Opcode::Jump { .. }
            | Opcode::JumpIfZero { .. }
            | Opcode::JumpIfNotZero { .. }
            | Opcode::ILtJump { .. }
            | Opcode::FLtJump { .. } => {
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
            Opcode::Jump { .. }
            | Opcode::JumpIfZero { .. }
            | Opcode::JumpIfNotZero { .. }
            | Opcode::ILtJump { .. }
            | Opcode::FLtJump { .. }
            | Opcode::Call { .. }
            | Opcode::CallIndirect { .. }
            | Opcode::SaveRegs { .. }
            | Opcode::RestoreRegs { .. } => break,
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
        Opcode::Move { src, .. } => {
            if *src == old {
                *src = new;
            }
        }
        Opcode::IAdd { a, b, .. }
        | Opcode::ISub { a, b, .. }
        | Opcode::IMul { a, b, .. }
        | Opcode::IDiv { a, b, .. }
        | Opcode::UDiv { a, b, .. }
        | Opcode::IRem { a, b, .. }
        | Opcode::IPow { a, b, .. } => {
            if *a == old {
                *a = new;
            }
            if *b == old {
                *b = new;
            }
        }
        Opcode::INeg { src, .. } | Opcode::IAddImm { src, .. } => {
            if *src == old {
                *src = new;
            }
        }
        Opcode::FAdd { a, b, .. }
        | Opcode::FSub { a, b, .. }
        | Opcode::FMul { a, b, .. }
        | Opcode::FDiv { a, b, .. }
        | Opcode::FPow { a, b, .. } => {
            if *a == old {
                *a = new;
            }
            if *b == old {
                *b = new;
            }
        }
        Opcode::FNeg { src, .. } => {
            if *src == old {
                *src = new;
            }
        }
        Opcode::FMulAdd { a, b, c, .. } | Opcode::FMulSub { a, b, c, .. } => {
            if *a == old {
                *a = new;
            }
            if *b == old {
                *b = new;
            }
            if *c == old {
                *c = new;
            }
        }
        Opcode::DAdd { a, b, .. }
        | Opcode::DSub { a, b, .. }
        | Opcode::DMul { a, b, .. }
        | Opcode::DDiv { a, b, .. }
        | Opcode::DPow { a, b, .. } => {
            if *a == old {
                *a = new;
            }
            if *b == old {
                *b = new;
            }
        }
        Opcode::DNeg { src, .. } => {
            if *src == old {
                *src = new;
            }
        }
        Opcode::DMulAdd { a, b, c, .. } | Opcode::DMulSub { a, b, c, .. } => {
            if *a == old {
                *a = new;
            }
            if *b == old {
                *b = new;
            }
            if *c == old {
                *c = new;
            }
        }
        Opcode::And { a, b, .. }
        | Opcode::Or { a, b, .. }
        | Opcode::Xor { a, b, .. }
        | Opcode::Shl { a, b, .. }
        | Opcode::Shr { a, b, .. }
        | Opcode::UShr { a, b, .. } => {
            if *a == old {
                *a = new;
            }
            if *b == old {
                *b = new;
            }
        }
        Opcode::Not { src, .. } => {
            if *src == old {
                *src = new;
            }
        }
        Opcode::IEq { a, b, .. }
        | Opcode::INe { a, b, .. }
        | Opcode::ILt { a, b, .. }
        | Opcode::ILe { a, b, .. }
        | Opcode::ULt { a, b, .. } => {
            if *a == old {
                *a = new;
            }
            if *b == old {
                *b = new;
            }
        }
        Opcode::FEq { a, b, .. }
        | Opcode::FNe { a, b, .. }
        | Opcode::FLt { a, b, .. }
        | Opcode::FLe { a, b, .. } => {
            if *a == old {
                *a = new;
            }
            if *b == old {
                *b = new;
            }
        }
        Opcode::MemEq { a, b, .. } | Opcode::MemNe { a, b, .. } => {
            if *a == old {
                *a = new;
            }
            if *b == old {
                *b = new;
            }
        }
        Opcode::DEq { a, b, .. } | Opcode::DLt { a, b, .. } | Opcode::DLe { a, b, .. } => {
            if *a == old {
                *a = new;
            }
            if *b == old {
                *b = new;
            }
        }
        Opcode::I32ToF32 { src, .. }
        | Opcode::F32ToI32 { src, .. }
        | Opcode::I32ToF64 { src, .. }
        | Opcode::F64ToI32 { src, .. }
        | Opcode::F32ToF64 { src, .. }
        | Opcode::F64ToF32 { src, .. } => {
            if *src == old {
                *src = new;
            }
        }
        Opcode::Load8 { addr, .. } | Opcode::Load32 { addr, .. } | Opcode::Load64 { addr, .. } => {
            if *addr == old {
                *addr = new;
            }
        }
        Opcode::Load32Off { base, .. } | Opcode::Load64Off { base, .. } => {
            if *base == old {
                *base = new;
            }
        }
        Opcode::Store8 { addr, src }
        | Opcode::Store32 { addr, src }
        | Opcode::Store64 { addr, src } => {
            if *addr == old {
                *addr = new;
            }
            if *src == old {
                *src = new;
            }
        }
        Opcode::Store8Off { base, src, .. }
        | Opcode::Store32Off { base, src, .. }
        | Opcode::Store64Off { base, src, .. } => {
            if *base == old {
                *base = new;
            }
            if *src == old {
                *src = new;
            }
        }
        Opcode::JumpIfZero { cond, .. } | Opcode::JumpIfNotZero { cond, .. } => {
            if *cond == old {
                *cond = new;
            }
        }
        Opcode::ILtJump { a, b, .. } | Opcode::FLtJump { a, b, .. } => {
            if *a == old {
                *a = new;
            }
            if *b == old {
                *b = new;
            }
        }
        Opcode::ReturnReg { src } => {
            if *src == old {
                *src = new;
            }
        }
        Opcode::MemCopy { dst, src, .. } => {
            if *dst == old {
                *dst = new;
            }
            if *src == old {
                *src = new;
            }
        }
        Opcode::MemZero { dst, .. } => {
            if *dst == old {
                *dst = new;
            }
        }
        Opcode::PrintI32 { src }
        | Opcode::PrintF32 { src }
        | Opcode::Assert { src }
        | Opcode::Putc { src } => {
            if *src == old {
                *src = new;
            }
        }
        Opcode::SinF32 { src, .. }
        | Opcode::CosF32 { src, .. }
        | Opcode::TanF32 { src, .. }
        | Opcode::LnF32 { src, .. }
        | Opcode::ExpF32 { src, .. }
        | Opcode::SqrtF32 { src, .. }
        | Opcode::AbsF32 { src, .. }
        | Opcode::FloorF32 { src, .. }
        | Opcode::CeilF32 { src, .. }
        | Opcode::AsinF32 { src, .. }
        | Opcode::AcosF32 { src, .. }
        | Opcode::AtanF32 { src, .. }
        | Opcode::SinhF32 { src, .. }
        | Opcode::CoshF32 { src, .. }
        | Opcode::TanhF32 { src, .. }
        | Opcode::AsinhF32 { src, .. }
        | Opcode::AcoshF32 { src, .. }
        | Opcode::AtanhF32 { src, .. }
        | Opcode::Exp2F32 { src, .. }
        | Opcode::Log10F32 { src, .. }
        | Opcode::Log2F32 { src, .. }
        | Opcode::IsinfF32 { src, .. }
        | Opcode::IsnanF32 { src, .. } => {
            if *src == old {
                *src = new;
            }
        }
        Opcode::SinF64 { src, .. }
        | Opcode::CosF64 { src, .. }
        | Opcode::TanF64 { src, .. }
        | Opcode::LnF64 { src, .. }
        | Opcode::ExpF64 { src, .. }
        | Opcode::SqrtF64 { src, .. }
        | Opcode::AbsF64 { src, .. }
        | Opcode::FloorF64 { src, .. }
        | Opcode::CeilF64 { src, .. }
        | Opcode::AsinF64 { src, .. }
        | Opcode::AcosF64 { src, .. }
        | Opcode::AtanF64 { src, .. }
        | Opcode::SinhF64 { src, .. }
        | Opcode::CoshF64 { src, .. }
        | Opcode::TanhF64 { src, .. }
        | Opcode::AsinhF64 { src, .. }
        | Opcode::AcoshF64 { src, .. }
        | Opcode::AtanhF64 { src, .. }
        | Opcode::Exp2F64 { src, .. }
        | Opcode::Log10F64 { src, .. }
        | Opcode::Log2F64 { src, .. }
        | Opcode::IsinfF64 { src, .. }
        | Opcode::IsnanF64 { src, .. } => {
            if *src == old {
                *src = new;
            }
        }
        Opcode::PowF32 { a, b, .. }
        | Opcode::Atan2F32 { a, b, .. }
        | Opcode::MinF32 { a, b, .. }
        | Opcode::MaxF32 { a, b, .. }
        | Opcode::PowF64 { a, b, .. }
        | Opcode::Atan2F64 { a, b, .. }
        | Opcode::MinF64 { a, b, .. }
        | Opcode::MaxF64 { a, b, .. } => {
            if *a == old {
                *a = new;
            }
            if *b == old {
                *b = new;
            }
        }
        Opcode::StoreSlot32 { src, .. } => {
            if *src == old {
                *src = new;
            }
        }
        Opcode::Call {
            args_start,
            arg_count,
            ..
        } => {
            for r in *args_start..(*args_start + *arg_count) {
                if r == old && r == *args_start {
                    *args_start = new;
                    break;
                }
            }
        }
        Opcode::CallIndirect {
            func_reg,
            args_start,
            arg_count,
        } => {
            if *func_reg == old {
                *func_reg = new;
            }
            for r in *args_start..(*args_start + *arg_count) {
                if r == old && r == *args_start {
                    *args_start = new;
                    break;
                }
            }
        }
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
        if let Opcode::LocalAddr {
            dst: addr_reg,
            slot,
        } = code[i]
        {
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

/// Fuse IAddImm + Load32/Store32 into Load32Off/Store32Off.
///
/// Pattern: `IAddImm { dst: A, src: B, imm: N }` followed by `Store32 { addr: A, src: V }`
/// where A is only used by the Store32. Fuse into `Store32Off { base: B, offset: N, src: V }`.
/// Similarly for Load32, Load64, Store64, etc.
fn fuse_offset_access(code: &mut Vec<Opcode>) {
    let uses = compute_use_counts_fast(code);

    for i in 0..code.len().saturating_sub(1) {
        if let Opcode::IAddImm {
            dst: addr_reg,
            src: base,
            imm,
        } = code[i]
        {
            // Only fuse if the computed address register is used exactly once
            if uses[addr_reg as usize] != 1 {
                continue;
            }
            let offset = imm as i32;
            match code[i + 1] {
                Opcode::Store32 { addr, src } if addr == addr_reg => {
                    code[i] = Opcode::Store32Off { base, offset, src };
                    code[i + 1] = Opcode::Nop;
                }
                Opcode::Store64 { addr, src } if addr == addr_reg => {
                    code[i] = Opcode::Store64Off { base, offset, src };
                    code[i + 1] = Opcode::Nop;
                }
                Opcode::Store8 { addr, src } if addr == addr_reg => {
                    code[i] = Opcode::Store8Off { base, offset, src };
                    code[i + 1] = Opcode::Nop;
                }
                Opcode::Load32 { dst, addr } if addr == addr_reg => {
                    code[i] = Opcode::Load32Off { dst, base, offset };
                    code[i + 1] = Opcode::Nop;
                }
                Opcode::Load64 { dst, addr } if addr == addr_reg => {
                    code[i] = Opcode::Load64Off { dst, base, offset };
                    code[i + 1] = Opcode::Nop;
                }
                _ => {}
            }
        }
    }
}

/// Fuse compare + branch into single superinstructions.
///
/// Patterns:
///   ILt { dst, a, b } + JumpIfZero { cond: dst, offset } → ILtJump { a, b, offset }
///   FLt { dst, a, b } + JumpIfZero { cond: dst, offset } → FLtJump { a, b, offset }
fn fuse_compare_branch(code: &mut Vec<Opcode>) {
    let uses = compute_use_counts_fast(code);

    for i in 0..code.len().saturating_sub(1) {
        match code[i] {
            Opcode::ILt { dst, a, b } => {
                if uses[dst as usize] == 1 {
                    if let Opcode::JumpIfZero { cond, offset } = code[i + 1] {
                        if cond == dst {
                            // offset was relative to JumpIfZero (i+1), now relative to ILt (i)
                            code[i] = Opcode::ILtJump {
                                a,
                                b,
                                offset: offset + 1,
                            };
                            code[i + 1] = Opcode::Nop;
                        }
                    }
                }
            }
            Opcode::FLt { dst, a, b } => {
                if uses[dst as usize] == 1 {
                    if let Opcode::JumpIfZero { cond, offset } = code[i + 1] {
                        if cond == dst {
                            code[i] = Opcode::FLtJump {
                                a,
                                b,
                                offset: offset + 1,
                            };
                            code[i + 1] = Opcode::Nop;
                        }
                    }
                }
            }
            _ => {}
        }
    }
}

/// Compute live ranges for all virtual registers.
/// Returns (def_point, last_use, is_used) arrays indexed by register number.
fn compute_live_ranges(code: &[Opcode]) -> ([u32; 256], [u32; 256], [bool; 256]) {
    let mut def_point = [u32::MAX; 256];
    let mut last_use = [0u32; 256];
    let mut is_used = [false; 256];

    for (i, op) in code.iter().enumerate() {
        let i = i as u32;
        if let Some(dst) = get_dst(op) {
            let d = dst as usize;
            if def_point[d] == u32::MAX {
                def_point[d] = i;
            }
            is_used[d] = true;
        }
        for_each_src(op, |r| {
            let u = r as usize;
            last_use[u] = i;
            is_used[u] = true;
            if def_point[u] == u32::MAX {
                def_point[u] = 0;
            }
        });
    }

    // Extend live ranges through loops (backward jumps).
    for (i, op) in code.iter().enumerate() {
        let offset = match op {
            Opcode::Jump { offset }
            | Opcode::JumpIfZero { offset, .. }
            | Opcode::JumpIfNotZero { offset, .. }
            | Opcode::ILtJump { offset, .. }
            | Opcode::FLtJump { offset, .. } => Some(*offset),
            _ => None,
        };
        if let Some(off) = offset {
            let target = (i as i32 + 1 + off) as u32;
            let i = i as u32;
            if target <= i {
                let loop_start = target;
                let loop_end = i;
                for r in 0..256 {
                    if !is_used[r] {
                        continue;
                    }
                    if def_point[r] < loop_start
                        && last_use[r] >= loop_start
                        && last_use[r] <= loop_end
                    {
                        last_use[r] = loop_end;
                    }
                    if def_point[r] >= loop_start
                        && def_point[r] <= loop_end
                        && last_use[r] >= loop_start
                        && last_use[r] <= loop_end
                    {
                        for j in loop_start..def_point[r] {
                            if reads_reg(&code[j as usize], r as u8) {
                                def_point[r] = loop_start;
                                last_use[r] = loop_end;
                                break;
                            }
                        }
                    }
                }
            }
        }
    }

    (def_point, last_use, is_used)
}

/// Copy coalescing: merge virtual registers connected by Move instructions
/// when their live ranges don't interfere. This eliminates moves and reduces
/// register pressure. Runs as a pre-pass before linear scan allocation.
fn copy_coalesce(code: &mut [Opcode]) {
    // Union-find for register coalescing.
    let mut parent: [u8; 256] = std::array::from_fn(|i| i as u8);

    fn find(parent: &mut [u8; 256], mut x: u8) -> u8 {
        while parent[x as usize] != x {
            parent[x as usize] = parent[parent[x as usize] as usize]; // path compression
            x = parent[x as usize];
        }
        x
    }

    fn union(parent: &mut [u8; 256], a: u8, b: u8) {
        // Prefer lower-numbered register as representative (keeps reg 0 as itself).
        let ra = find(parent, a);
        let rb = find(parent, b);
        if ra != rb {
            if ra < rb {
                parent[rb as usize] = ra;
            } else {
                parent[ra as usize] = rb;
            }
        }
    }

    // Don't coalesce registers used as call arguments (contiguity requirement).
    let mut call_arg_reg = [false; 256];
    for op in code.iter() {
        let (args_start, arg_count) = match op {
            Opcode::Call {
                args_start,
                arg_count,
                ..
            } => (*args_start, *arg_count),
            Opcode::CallIndirect {
                args_start,
                arg_count,
                ..
            } => (*args_start, *arg_count),
            _ => continue,
        };
        for r in args_start..(args_start + arg_count) {
            call_arg_reg[r as usize] = true;
        }
    }

    // Iterate coalescing until no more merges.
    // Coalesce one pair per pass (recompute live ranges each time for safety).
    for _ in 0..64 {
        let (def_point, last_use, is_used) = compute_live_ranges(code);
        let mut coalesced = false;

        for i in 0..code.len() {
            if let Opcode::Move { dst, src } = code[i] {
                if dst == src {
                    code[i] = Opcode::Nop;
                    coalesced = true;
                    break;
                }

                let d = dst as usize;
                let s = src as usize;
                if !is_used[d] || !is_used[s] {
                    continue;
                }
                // Skip registers involved in call arguments.
                if call_arg_reg[d] || call_arg_reg[s] {
                    continue;
                }

                let d_def = def_point[d];
                let s_end = last_use[s].max(def_point[s]);
                let move_point = i as u32;

                // Conservative: only coalesce when src dies at the move and
                // dst is first defined at the move. This guarantees no interference.
                let src_dies_here = s_end == move_point;
                let dst_born_here = d_def == move_point;
                let can_coalesce = src_dies_here && dst_born_here;

                if can_coalesce {
                    union(&mut parent, dst, src);
                    let rep = find(&mut parent, dst);
                    for op in code.iter_mut() {
                        rewrite_coalesced_reg(op, dst, rep);
                        rewrite_coalesced_reg(op, src, rep);
                    }
                    code[i] = Opcode::Nop;
                    coalesced = true;
                    break; // Restart with fresh live ranges
                }
            }
        }
        if !coalesced {
            break;
        }
    }
}

/// Rewrite all occurrences of `old` register to `new` in an instruction.
fn rewrite_coalesced_reg(op: &mut Opcode, old: Reg, new: Reg) {
    if old == new {
        return;
    }
    // Rewrite dst
    match op {
        Opcode::Move { dst, src } => {
            if *dst == old {
                *dst = new;
            }
            if *src == old {
                *src = new;
            }
        }
        _ => {
            if let Some(d) = get_dst(op) {
                if d == old {
                    set_dst(op, new);
                }
            }
            replace_src_reg(op, old, new);
        }
    }
}

/// Linear scan register allocation.
///
/// Maps virtual registers (0..N, potentially sparse and high-numbered) to
/// compacted physical registers (0..M where M << N). This reduces SaveRegs
/// count and improves cache utilization of the register file.
fn register_allocation(code: &mut Vec<Opcode>) -> (u8, [u8; 256]) {
    if code.is_empty() {
        return (0, [255; 256]);
    }

    // Pre-pass: copy coalescing to eliminate moves.
    copy_coalesce(code);

    // Step 1: Find live ranges.
    let (def_point, last_use, is_used) = compute_live_ranges(code);

    // Step 3: Build sorted interval list.
    let mut intervals: Vec<(u8, u32, u32)> = Vec::new(); // (vreg, start, end)
    for r in 0..256u16 {
        if is_used[r as usize] {
            let start = def_point[r as usize];
            let end = last_use[r as usize].max(start);
            intervals.push((r as u8, start, end));
        }
    }
    intervals.sort_by_key(|&(_, start, end)| (start, end));

    // Step 3.5a: Pin registers that are live across calls.
    // Calls clobber all low registers (callee uses its own register range from 0).
    // Registers that span calls must keep their original (high) virtual register numbers
    // to avoid conflicts with callee registers.
    let mut pinned = [false; 256];
    for (i, op) in code.iter().enumerate() {
        let is_call = matches!(op, Opcode::Call { .. } | Opcode::CallIndirect { .. });
        if !is_call {
            continue;
        }
        let i = i as u32;
        for r in 0..256 {
            if is_used[r] && def_point[r] < i && last_use[r] > i {
                pinned[r] = true;
            }
        }
    }

    // Step 3.5b: Identify call argument groups that must be contiguous.
    // Map vreg → (group_start_vreg, offset_in_group).
    let mut call_group: [Option<(u8, u8)>; 256] = [None; 256];
    for op in code.iter() {
        let (args_start, arg_count) = match op {
            Opcode::Call {
                args_start,
                arg_count,
                ..
            } => (*args_start, *arg_count),
            Opcode::CallIndirect {
                args_start,
                arg_count,
                ..
            } => (*args_start, *arg_count),
            _ => continue,
        };
        if arg_count >= 2 {
            for offset in 0..arg_count {
                let vreg = args_start + offset;
                call_group[vreg as usize] = Some((args_start, offset));
            }
        }
    }

    // Step 3.5c: Build register hints from remaining Move instructions.
    // If Move { dst: D, src: S } exists, hint that D should use the same preg as S.
    let mut hint: [u8; 256] = [255; 256]; // vreg -> hinted vreg (255 = no hint)
    for op in code.iter() {
        if let Opcode::Move { dst, src } = op {
            if *dst != *src && !pinned[*dst as usize] && !pinned[*src as usize] {
                hint[*dst as usize] = *src;
            }
        }
    }

    // Step 4: Linear scan — assign physical registers.
    let mut mapping = [255u8; 256]; // vreg -> preg (255 = unassigned)
                                    // Track which physical registers are free. Use a simple bitset.
    let mut preg_free = [true; 256];
    // Active intervals sorted by end point: (end, vreg, preg)
    let mut active: Vec<(u32, u8, u8)> = Vec::new();

    // Pin virtual register 0 to physical register 0 (implicit return value).
    if is_used[0] {
        mapping[0] = 0;
        preg_free[0] = false;
        let end = last_use[0].max(def_point[0]);
        active.push((end, 0, 0));
    }

    // Cross-call registers will be assigned after the main allocation (see below).

    for &(vreg, start, end) in &intervals {
        // Skip vreg 0 — already pinned
        if vreg == 0 {
            continue;
        }
        // Skip cross-call registers — assigned later above the compacted range
        if pinned[vreg as usize] {
            continue;
        }
        // Skip if already allocated (part of a group that was allocated earlier)
        if mapping[vreg as usize] != 255 {
            continue;
        }

        // Expire intervals that ended before this one starts
        active.retain(|&(active_end, _vr, pr)| {
            if active_end < start {
                preg_free[pr as usize] = true;
                false
            } else {
                true
            }
        });

        if let Some((group_start, _offset)) = call_group[vreg as usize] {
            // This vreg is part of a call arg group. Allocate the entire group contiguously.
            // Find group size (excluding pinned regs in the group).
            let mut group_size: u8 = 0;
            while call_group[(group_start + group_size) as usize]
                .map_or(false, |(gs, _)| gs == group_start)
            {
                group_size += 1;
            }

            // Find a contiguous block of `group_size` free physical registers.
            let block_start = (0..=(255 - group_size + 1))
                .find(|&p| (0..group_size).all(|j| preg_free[(p + j) as usize]))
                .expect("register allocation: no contiguous block for call args")
                as u8;

            // Assign the entire group.
            for j in 0..group_size {
                let gvreg = group_start + j;
                if is_used[gvreg as usize] && !pinned[gvreg as usize] {
                    mapping[gvreg as usize] = block_start + j;
                    preg_free[(block_start + j) as usize] = false;
                    let gend = last_use[gvreg as usize].max(def_point[gvreg as usize]);
                    active.push((gend, gvreg, block_start + j));
                }
            }
        } else {
            // Normal register — try hinted register first, then lowest free.
            let hinted_vreg = hint[vreg as usize];
            let hinted_preg = if hinted_vreg != 255 {
                mapping[hinted_vreg as usize]
            } else {
                255
            };
            let preg = if hinted_preg != 255 && preg_free[hinted_preg as usize] {
                hinted_preg
            } else {
                preg_free
                    .iter()
                    .position(|&free| free)
                    .expect("register allocation: ran out of physical registers")
                    as u8
            };
            preg_free[preg as usize] = false;
            mapping[vreg as usize] = preg;
            active.push((end, vreg, preg));
        }
    }

    // Step 4b: Assign cross-call registers above the compacted range.
    // Find max physical register assigned so far.
    let mut max_compact: u8 = 0;
    for r in 0..256 {
        if mapping[r] != 255 {
            max_compact = max_compact.max(mapping[r]);
        }
    }
    let mut next_cross_call = max_compact + 1;
    for r in 0..256 {
        if pinned[r] && is_used[r] && mapping[r] == 255 {
            mapping[r] = next_cross_call;
            next_cross_call += 1;
        }
    }

    // Step 5: Find max physical register used.
    let mut max_preg: u8 = 0;
    for r in 0..256 {
        if is_used[r] && mapping[r] != 255 {
            max_preg = max_preg.max(mapping[r]);
        }
    }
    let new_count = max_preg
        .checked_add(1)
        .expect("register allocation overflow: need >255 physical registers");

    // Step 6: Patch SaveRegs/RestoreRegs BEFORE rewriting (start_reg still has virtual numbers).
    for op in code.iter_mut() {
        match op {
            Opcode::SaveRegs { count, .. } => {
                *count = new_count;
            }
            Opcode::RestoreRegs {
                start_reg, count, ..
            } => {
                if *start_reg == 1 {
                    *count = if new_count > 1 { new_count - 1 } else { 0 };
                } else {
                    *count = new_count;
                }
            }
            _ => {}
        }
    }

    // Step 7: Rewrite all instructions with the new register mapping.
    for op in code.iter_mut() {
        rewrite_regs(op, &mapping);
    }

    (new_count, mapping)
}

/// Call `f` for each source register read by the instruction.
fn for_each_src(op: &Opcode, mut f: impl FnMut(Reg)) {
    match op {
        Opcode::Move { src, .. } => f(*src),
        Opcode::IAdd { a, b, .. }
        | Opcode::ISub { a, b, .. }
        | Opcode::IMul { a, b, .. }
        | Opcode::IDiv { a, b, .. }
        | Opcode::UDiv { a, b, .. }
        | Opcode::IRem { a, b, .. }
        | Opcode::IPow { a, b, .. } => {
            f(*a);
            f(*b);
        }
        Opcode::INeg { src, .. } | Opcode::IAddImm { src, .. } => f(*src),
        Opcode::FAdd { a, b, .. }
        | Opcode::FSub { a, b, .. }
        | Opcode::FMul { a, b, .. }
        | Opcode::FDiv { a, b, .. }
        | Opcode::FPow { a, b, .. } => {
            f(*a);
            f(*b);
        }
        Opcode::FNeg { src, .. } => f(*src),
        Opcode::FMulAdd { a, b, c, .. } | Opcode::FMulSub { a, b, c, .. } => {
            f(*a);
            f(*b);
            f(*c);
        }
        Opcode::DAdd { a, b, .. }
        | Opcode::DSub { a, b, .. }
        | Opcode::DMul { a, b, .. }
        | Opcode::DDiv { a, b, .. }
        | Opcode::DPow { a, b, .. } => {
            f(*a);
            f(*b);
        }
        Opcode::DNeg { src, .. } => f(*src),
        Opcode::DMulAdd { a, b, c, .. } | Opcode::DMulSub { a, b, c, .. } => {
            f(*a);
            f(*b);
            f(*c);
        }
        Opcode::And { a, b, .. }
        | Opcode::Or { a, b, .. }
        | Opcode::Xor { a, b, .. }
        | Opcode::Shl { a, b, .. }
        | Opcode::Shr { a, b, .. }
        | Opcode::UShr { a, b, .. } => {
            f(*a);
            f(*b);
        }
        Opcode::Not { src, .. } => f(*src),
        Opcode::IEq { a, b, .. }
        | Opcode::INe { a, b, .. }
        | Opcode::ILt { a, b, .. }
        | Opcode::ILe { a, b, .. }
        | Opcode::ULt { a, b, .. } => {
            f(*a);
            f(*b);
        }
        Opcode::FEq { a, b, .. }
        | Opcode::FNe { a, b, .. }
        | Opcode::FLt { a, b, .. }
        | Opcode::FLe { a, b, .. } => {
            f(*a);
            f(*b);
        }
        Opcode::MemEq { a, b, .. } | Opcode::MemNe { a, b, .. } => {
            f(*a);
            f(*b);
        }
        Opcode::DEq { a, b, .. } | Opcode::DLt { a, b, .. } | Opcode::DLe { a, b, .. } => {
            f(*a);
            f(*b);
        }
        Opcode::I32ToF32 { src, .. }
        | Opcode::F32ToI32 { src, .. }
        | Opcode::I32ToF64 { src, .. }
        | Opcode::F64ToI32 { src, .. }
        | Opcode::F32ToF64 { src, .. }
        | Opcode::F64ToF32 { src, .. } => f(*src),
        Opcode::Load8 { addr, .. } | Opcode::Load32 { addr, .. } | Opcode::Load64 { addr, .. } => {
            f(*addr)
        }
        Opcode::Load32Off { base, .. } | Opcode::Load64Off { base, .. } => f(*base),
        Opcode::Store8 { addr, src }
        | Opcode::Store32 { addr, src }
        | Opcode::Store64 { addr, src } => {
            f(*addr);
            f(*src);
        }
        Opcode::Store8Off { base, src, .. }
        | Opcode::Store32Off { base, src, .. }
        | Opcode::Store64Off { base, src, .. } => {
            f(*base);
            f(*src);
        }
        Opcode::JumpIfZero { cond, .. } | Opcode::JumpIfNotZero { cond, .. } => f(*cond),
        Opcode::ILtJump { a, b, .. } | Opcode::FLtJump { a, b, .. } => {
            f(*a);
            f(*b);
        }
        Opcode::Call {
            args_start,
            arg_count,
            ..
        } => {
            for r in *args_start..(*args_start + *arg_count) {
                f(r);
            }
        }
        Opcode::CallIndirect {
            func_reg,
            args_start,
            arg_count,
        } => {
            f(*func_reg);
            for r in *args_start..(*args_start + *arg_count) {
                f(r);
            }
        }
        Opcode::ReturnReg { src } => f(*src),
        Opcode::MemCopy { dst, src, .. } => {
            f(*dst);
            f(*src);
        }
        Opcode::MemZero { dst, .. } => f(*dst),
        // SaveRegs/RestoreRegs are function-level bookkeeping, not data flow.
        Opcode::SaveRegs { .. } => {}
        Opcode::RestoreRegs { .. } => {}
        Opcode::PrintI32 { src }
        | Opcode::PrintF32 { src }
        | Opcode::Assert { src }
        | Opcode::Putc { src } => f(*src),
        Opcode::SinF32 { src, .. }
        | Opcode::CosF32 { src, .. }
        | Opcode::TanF32 { src, .. }
        | Opcode::LnF32 { src, .. }
        | Opcode::ExpF32 { src, .. }
        | Opcode::SqrtF32 { src, .. }
        | Opcode::AbsF32 { src, .. }
        | Opcode::FloorF32 { src, .. }
        | Opcode::CeilF32 { src, .. }
        | Opcode::AsinF32 { src, .. }
        | Opcode::AcosF32 { src, .. }
        | Opcode::AtanF32 { src, .. }
        | Opcode::SinhF32 { src, .. }
        | Opcode::CoshF32 { src, .. }
        | Opcode::TanhF32 { src, .. }
        | Opcode::AsinhF32 { src, .. }
        | Opcode::AcoshF32 { src, .. }
        | Opcode::AtanhF32 { src, .. }
        | Opcode::Exp2F32 { src, .. }
        | Opcode::Log10F32 { src, .. }
        | Opcode::Log2F32 { src, .. }
        | Opcode::IsinfF32 { src, .. }
        | Opcode::IsnanF32 { src, .. } => f(*src),
        Opcode::SinF64 { src, .. }
        | Opcode::CosF64 { src, .. }
        | Opcode::TanF64 { src, .. }
        | Opcode::LnF64 { src, .. }
        | Opcode::ExpF64 { src, .. }
        | Opcode::SqrtF64 { src, .. }
        | Opcode::AbsF64 { src, .. }
        | Opcode::FloorF64 { src, .. }
        | Opcode::CeilF64 { src, .. }
        | Opcode::AsinF64 { src, .. }
        | Opcode::AcosF64 { src, .. }
        | Opcode::AtanF64 { src, .. }
        | Opcode::SinhF64 { src, .. }
        | Opcode::CoshF64 { src, .. }
        | Opcode::TanhF64 { src, .. }
        | Opcode::AsinhF64 { src, .. }
        | Opcode::AcoshF64 { src, .. }
        | Opcode::AtanhF64 { src, .. }
        | Opcode::Exp2F64 { src, .. }
        | Opcode::Log10F64 { src, .. }
        | Opcode::Log2F64 { src, .. }
        | Opcode::IsinfF64 { src, .. }
        | Opcode::IsnanF64 { src, .. } => f(*src),
        Opcode::PowF32 { a, b, .. }
        | Opcode::Atan2F32 { a, b, .. }
        | Opcode::MinF32 { a, b, .. }
        | Opcode::MaxF32 { a, b, .. }
        | Opcode::PowF64 { a, b, .. }
        | Opcode::Atan2F64 { a, b, .. }
        | Opcode::MinF64 { a, b, .. }
        | Opcode::MaxF64 { a, b, .. } => {
            f(*a);
            f(*b);
        }
        Opcode::StoreSlot32 { src, .. } => f(*src),
        _ => {}
    }
}

/// Rewrite all register references in an instruction using a mapping table.
fn rewrite_regs(op: &mut Opcode, map: &[u8; 256]) {
    match op {
        Opcode::Move { dst, src } => {
            *dst = map[*dst as usize];
            *src = map[*src as usize];
        }
        Opcode::LoadImm { dst, .. }
        | Opcode::LoadF32 { dst, .. }
        | Opcode::LoadF64 { dst, .. }
        | Opcode::LoadConst { dst, .. } => {
            *dst = map[*dst as usize];
        }
        Opcode::IAdd { dst, a, b }
        | Opcode::ISub { dst, a, b }
        | Opcode::IMul { dst, a, b }
        | Opcode::IDiv { dst, a, b }
        | Opcode::UDiv { dst, a, b }
        | Opcode::IRem { dst, a, b }
        | Opcode::IPow { dst, a, b } => {
            *dst = map[*dst as usize];
            *a = map[*a as usize];
            *b = map[*b as usize];
        }
        Opcode::INeg { dst, src } => {
            *dst = map[*dst as usize];
            *src = map[*src as usize];
        }
        Opcode::IAddImm { dst, src, .. } => {
            *dst = map[*dst as usize];
            *src = map[*src as usize];
        }
        Opcode::FAdd { dst, a, b }
        | Opcode::FSub { dst, a, b }
        | Opcode::FMul { dst, a, b }
        | Opcode::FDiv { dst, a, b }
        | Opcode::FPow { dst, a, b } => {
            *dst = map[*dst as usize];
            *a = map[*a as usize];
            *b = map[*b as usize];
        }
        Opcode::FNeg { dst, src } => {
            *dst = map[*dst as usize];
            *src = map[*src as usize];
        }
        Opcode::FMulAdd { dst, a, b, c } | Opcode::FMulSub { dst, a, b, c } => {
            *dst = map[*dst as usize];
            *a = map[*a as usize];
            *b = map[*b as usize];
            *c = map[*c as usize];
        }
        Opcode::DAdd { dst, a, b }
        | Opcode::DSub { dst, a, b }
        | Opcode::DMul { dst, a, b }
        | Opcode::DDiv { dst, a, b }
        | Opcode::DPow { dst, a, b } => {
            *dst = map[*dst as usize];
            *a = map[*a as usize];
            *b = map[*b as usize];
        }
        Opcode::DNeg { dst, src } => {
            *dst = map[*dst as usize];
            *src = map[*src as usize];
        }
        Opcode::DMulAdd { dst, a, b, c } | Opcode::DMulSub { dst, a, b, c } => {
            *dst = map[*dst as usize];
            *a = map[*a as usize];
            *b = map[*b as usize];
            *c = map[*c as usize];
        }
        Opcode::And { dst, a, b }
        | Opcode::Or { dst, a, b }
        | Opcode::Xor { dst, a, b }
        | Opcode::Shl { dst, a, b }
        | Opcode::Shr { dst, a, b }
        | Opcode::UShr { dst, a, b } => {
            *dst = map[*dst as usize];
            *a = map[*a as usize];
            *b = map[*b as usize];
        }
        Opcode::Not { dst, src } => {
            *dst = map[*dst as usize];
            *src = map[*src as usize];
        }
        Opcode::IEq { dst, a, b }
        | Opcode::INe { dst, a, b }
        | Opcode::ILt { dst, a, b }
        | Opcode::ILe { dst, a, b }
        | Opcode::ULt { dst, a, b } => {
            *dst = map[*dst as usize];
            *a = map[*a as usize];
            *b = map[*b as usize];
        }
        Opcode::FEq { dst, a, b }
        | Opcode::FNe { dst, a, b }
        | Opcode::FLt { dst, a, b }
        | Opcode::FLe { dst, a, b } => {
            *dst = map[*dst as usize];
            *a = map[*a as usize];
            *b = map[*b as usize];
        }
        Opcode::MemEq { dst, a, b, .. } | Opcode::MemNe { dst, a, b, .. } => {
            *dst = map[*dst as usize];
            *a = map[*a as usize];
            *b = map[*b as usize];
        }
        Opcode::DEq { dst, a, b } | Opcode::DLt { dst, a, b } | Opcode::DLe { dst, a, b } => {
            *dst = map[*dst as usize];
            *a = map[*a as usize];
            *b = map[*b as usize];
        }
        Opcode::I32ToF32 { dst, src }
        | Opcode::F32ToI32 { dst, src }
        | Opcode::I32ToF64 { dst, src }
        | Opcode::F64ToI32 { dst, src }
        | Opcode::F32ToF64 { dst, src }
        | Opcode::F64ToF32 { dst, src } => {
            *dst = map[*dst as usize];
            *src = map[*src as usize];
        }
        Opcode::Load8 { dst, addr }
        | Opcode::Load32 { dst, addr }
        | Opcode::Load64 { dst, addr } => {
            *dst = map[*dst as usize];
            *addr = map[*addr as usize];
        }
        Opcode::Load32Off { dst, base, .. } | Opcode::Load64Off { dst, base, .. } => {
            *dst = map[*dst as usize];
            *base = map[*base as usize];
        }
        Opcode::Store8 { addr, src }
        | Opcode::Store32 { addr, src }
        | Opcode::Store64 { addr, src } => {
            *addr = map[*addr as usize];
            *src = map[*src as usize];
        }
        Opcode::Store8Off { base, src, .. }
        | Opcode::Store32Off { base, src, .. }
        | Opcode::Store64Off { base, src, .. } => {
            *base = map[*base as usize];
            *src = map[*src as usize];
        }
        Opcode::LocalAddr { dst, .. } | Opcode::GlobalAddr { dst, .. } => {
            *dst = map[*dst as usize];
        }
        Opcode::JumpIfZero { cond, .. } | Opcode::JumpIfNotZero { cond, .. } => {
            *cond = map[*cond as usize];
        }
        Opcode::ILtJump { a, b, .. } | Opcode::FLtJump { a, b, .. } => {
            *a = map[*a as usize];
            *b = map[*b as usize];
        }
        Opcode::Call { args_start, .. } => {
            *args_start = map[*args_start as usize];
        }
        Opcode::CallIndirect {
            func_reg,
            args_start,
            ..
        } => {
            *func_reg = map[*func_reg as usize];
            *args_start = map[*args_start as usize];
        }
        Opcode::ReturnReg { src } => {
            *src = map[*src as usize];
        }
        Opcode::MemCopy { dst, src, .. } => {
            *dst = map[*dst as usize];
            *src = map[*src as usize];
        }
        Opcode::MemZero { dst, .. } => {
            *dst = map[*dst as usize];
        }
        // SaveRegs/RestoreRegs are patched separately (Step 6), not remapped.
        Opcode::SaveRegs { .. } => {}
        Opcode::RestoreRegs { .. } => {}
        Opcode::PrintI32 { src }
        | Opcode::PrintF32 { src }
        | Opcode::Assert { src }
        | Opcode::Putc { src } => {
            *src = map[*src as usize];
        }
        Opcode::SinF32 { dst, src }
        | Opcode::CosF32 { dst, src }
        | Opcode::TanF32 { dst, src }
        | Opcode::LnF32 { dst, src }
        | Opcode::ExpF32 { dst, src }
        | Opcode::SqrtF32 { dst, src }
        | Opcode::AbsF32 { dst, src }
        | Opcode::FloorF32 { dst, src }
        | Opcode::CeilF32 { dst, src }
        | Opcode::AsinF32 { dst, src }
        | Opcode::AcosF32 { dst, src }
        | Opcode::AtanF32 { dst, src }
        | Opcode::SinhF32 { dst, src }
        | Opcode::CoshF32 { dst, src }
        | Opcode::TanhF32 { dst, src }
        | Opcode::AsinhF32 { dst, src }
        | Opcode::AcoshF32 { dst, src }
        | Opcode::AtanhF32 { dst, src }
        | Opcode::Exp2F32 { dst, src }
        | Opcode::Log10F32 { dst, src }
        | Opcode::Log2F32 { dst, src }
        | Opcode::IsinfF32 { dst, src }
        | Opcode::IsnanF32 { dst, src } => {
            *dst = map[*dst as usize];
            *src = map[*src as usize];
        }
        Opcode::SinF64 { dst, src }
        | Opcode::CosF64 { dst, src }
        | Opcode::TanF64 { dst, src }
        | Opcode::LnF64 { dst, src }
        | Opcode::ExpF64 { dst, src }
        | Opcode::SqrtF64 { dst, src }
        | Opcode::AbsF64 { dst, src }
        | Opcode::FloorF64 { dst, src }
        | Opcode::CeilF64 { dst, src }
        | Opcode::AsinF64 { dst, src }
        | Opcode::AcosF64 { dst, src }
        | Opcode::AtanF64 { dst, src }
        | Opcode::SinhF64 { dst, src }
        | Opcode::CoshF64 { dst, src }
        | Opcode::TanhF64 { dst, src }
        | Opcode::AsinhF64 { dst, src }
        | Opcode::AcoshF64 { dst, src }
        | Opcode::AtanhF64 { dst, src }
        | Opcode::Exp2F64 { dst, src }
        | Opcode::Log10F64 { dst, src }
        | Opcode::Log2F64 { dst, src }
        | Opcode::IsinfF64 { dst, src }
        | Opcode::IsnanF64 { dst, src } => {
            *dst = map[*dst as usize];
            *src = map[*src as usize];
        }
        Opcode::PowF32 { dst, a, b }
        | Opcode::Atan2F32 { dst, a, b }
        | Opcode::MinF32 { dst, a, b }
        | Opcode::MaxF32 { dst, a, b }
        | Opcode::PowF64 { dst, a, b }
        | Opcode::Atan2F64 { dst, a, b }
        | Opcode::MinF64 { dst, a, b }
        | Opcode::MaxF64 { dst, a, b } => {
            *dst = map[*dst as usize];
            *a = map[*a as usize];
            *b = map[*b as usize];
        }
        Opcode::LoadSlot32 { dst, .. } => {
            *dst = map[*dst as usize];
        }
        Opcode::StoreSlot32 { src, .. } => {
            *src = map[*src as usize];
        }
        _ => {}
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vm::Opcode;

    // ========== move_forwarding tests ==========

    #[test]
    fn test_move_forwarding_basic() {
        let mut code = vec![
            Opcode::LoadImm { dst: 0, value: 42 },
            Opcode::Move { dst: 1, src: 0 },
        ];
        move_forwarding(&mut code);
        assert_eq!(code[0], Opcode::LoadImm { dst: 1, value: 42 });
        assert_eq!(code[1], Opcode::Nop);
    }

    #[test]
    fn test_move_forwarding_src_used_elsewhere() {
        // r0 is used by both the Move and the IAdd, so forwarding should NOT happen
        let mut code = vec![
            Opcode::LoadImm { dst: 0, value: 42 },
            Opcode::Move { dst: 1, src: 0 },
            Opcode::IAdd { dst: 2, a: 0, b: 1 },
        ];
        move_forwarding(&mut code);
        // Should remain unchanged because r0 has use count > 1
        assert_eq!(code[0], Opcode::LoadImm { dst: 0, value: 42 });
        assert_eq!(code[1], Opcode::Move { dst: 1, src: 0 });
    }

    #[test]
    fn test_move_forwarding_chain() {
        // Two independent move-forwarding opportunities
        let mut code = vec![
            Opcode::LoadImm { dst: 0, value: 10 },
            Opcode::Move { dst: 2, src: 0 },
            Opcode::LoadImm { dst: 1, value: 20 },
            Opcode::Move { dst: 3, src: 1 },
        ];
        move_forwarding(&mut code);
        assert_eq!(code[0], Opcode::LoadImm { dst: 2, value: 10 });
        assert_eq!(code[1], Opcode::Nop);
        assert_eq!(code[2], Opcode::LoadImm { dst: 3, value: 20 });
        assert_eq!(code[3], Opcode::Nop);
    }

    #[test]
    fn test_move_forwarding_arith() {
        // IAdd writes to r2, then Move r2 -> r5, and r2 only used by that move
        let mut code = vec![
            Opcode::LoadImm { dst: 0, value: 1 },
            Opcode::LoadImm { dst: 1, value: 2 },
            Opcode::IAdd { dst: 2, a: 0, b: 1 },
            Opcode::Move { dst: 5, src: 2 },
        ];
        move_forwarding(&mut code);
        assert_eq!(code[2], Opcode::IAdd { dst: 5, a: 0, b: 1 });
        assert_eq!(code[3], Opcode::Nop);
    }

    // ========== redundant_local_addr tests ==========

    #[test]
    fn test_redundant_local_addr_basic() {
        // Two LocalAddr for the same slot -> second should be eliminated
        let mut code = vec![
            Opcode::LocalAddr { dst: 0, slot: 0 },
            Opcode::Store32 { addr: 0, src: 5 },
            Opcode::LocalAddr { dst: 1, slot: 0 },
            Opcode::Load32 { dst: 2, addr: 1 },
        ];
        redundant_local_addr(&mut code);
        // Second LocalAddr should be NOPed, Load32 should use r0 instead of r1
        assert_eq!(code[2], Opcode::Nop);
        assert_eq!(code[3], Opcode::Load32 { dst: 2, addr: 0 });
    }

    #[test]
    fn test_redundant_local_addr_different_slots() {
        // Different slots -> no elimination
        let mut code = vec![
            Opcode::LocalAddr { dst: 0, slot: 0 },
            Opcode::Store32 { addr: 0, src: 5 },
            Opcode::LocalAddr { dst: 1, slot: 1 },
            Opcode::Load32 { dst: 2, addr: 1 },
        ];
        let original = code.clone();
        redundant_local_addr(&mut code);
        assert_eq!(code, original);
    }

    #[test]
    fn test_redundant_local_addr_clobbered() {
        // Original register gets clobbered before the duplicate -> no elimination
        let mut code = vec![
            Opcode::LocalAddr { dst: 0, slot: 0 },
            Opcode::Store32 { addr: 0, src: 5 },
            Opcode::LoadImm { dst: 0, value: 99 }, // clobbers r0
            Opcode::LocalAddr { dst: 1, slot: 0 },
            Opcode::Load32 { dst: 2, addr: 1 },
        ];
        redundant_local_addr(&mut code);
        // r0 was clobbered, so slot_reg entry for slot 0 was invalidated
        assert_eq!(code[3], Opcode::LocalAddr { dst: 1, slot: 0 });
    }

    #[test]
    fn test_redundant_local_addr_jump_target_clears() {
        // Jump target between the two LocalAddrs should prevent optimization
        let mut code = vec![
            Opcode::LocalAddr { dst: 0, slot: 0 },
            Opcode::Store32 { addr: 0, src: 5 },
            Opcode::Jump { offset: 0 }, // jumps to index 3
            Opcode::LocalAddr { dst: 1, slot: 0 },
            Opcode::Load32 { dst: 2, addr: 1 },
        ];
        redundant_local_addr(&mut code);
        // The jump clears slot_reg, then index 3 is a jump target which also clears.
        assert_eq!(code[3], Opcode::LocalAddr { dst: 1, slot: 0 });
    }

    // ========== dead_code_elimination tests ==========

    #[test]
    fn test_dce_basic() {
        let mut code = vec![
            Opcode::LoadImm { dst: 1, value: 42 }, // r1 never used -> dead
            Opcode::LoadImm { dst: 0, value: 10 }, // r0 is return reg -> live
        ];
        dead_code_elimination(&mut code);
        assert_eq!(code[0], Opcode::Nop);
        assert_eq!(code[1], Opcode::LoadImm { dst: 0, value: 10 });
    }

    #[test]
    fn test_dce_preserves_side_effects() {
        // PrintI32 has side effects; its source register should stay live
        let mut code = vec![
            Opcode::LoadImm { dst: 1, value: 42 },
            Opcode::PrintI32 { src: 1 },
        ];
        dead_code_elimination(&mut code);
        assert_eq!(code[0], Opcode::LoadImm { dst: 1, value: 42 });
        assert_eq!(code[1], Opcode::PrintI32 { src: 1 });
    }

    #[test]
    fn test_dce_keeps_used_registers() {
        let mut code = vec![
            Opcode::LoadImm { dst: 1, value: 5 },
            Opcode::LoadImm { dst: 2, value: 10 },
            Opcode::IAdd { dst: 0, a: 1, b: 2 }, // r0 is live (return)
        ];
        dead_code_elimination(&mut code);
        assert_eq!(code[0], Opcode::LoadImm { dst: 1, value: 5 });
        assert_eq!(code[1], Opcode::LoadImm { dst: 2, value: 10 });
        assert_eq!(code[2], Opcode::IAdd { dst: 0, a: 1, b: 2 });
    }

    #[test]
    fn test_dce_eliminates_unused_chain() {
        // r3 unused, so IAdd to r3 is dead
        let mut code = vec![
            Opcode::LoadImm { dst: 0, value: 99 }, // r0 is return -> live
            Opcode::LoadImm { dst: 1, value: 5 },
            Opcode::LoadImm { dst: 2, value: 10 },
            Opcode::IAdd { dst: 3, a: 1, b: 2 }, // r3 never used -> dead
        ];
        dead_code_elimination(&mut code);
        assert_eq!(code[3], Opcode::Nop);
    }

    // ========== fuse_local_access tests ==========

    #[test]
    fn test_fuse_local_load() {
        let mut code = vec![
            Opcode::LocalAddr { dst: 0, slot: 3 },
            Opcode::Load32 { dst: 1, addr: 0 },
        ];
        fuse_local_access(&mut code);
        assert_eq!(code[0], Opcode::LoadSlot32 { dst: 1, slot: 3 });
        assert_eq!(code[1], Opcode::Nop);
    }

    #[test]
    fn test_fuse_local_store() {
        let mut code = vec![
            Opcode::LocalAddr { dst: 0, slot: 5 },
            Opcode::Store32 { addr: 0, src: 2 },
        ];
        fuse_local_access(&mut code);
        assert_eq!(code[0], Opcode::StoreSlot32 { slot: 5, src: 2 });
        assert_eq!(code[1], Opcode::Nop);
    }

    #[test]
    fn test_fuse_local_addr_used_twice_no_fuse() {
        // addr register used by both Store32 and Load32 -> use count > 1 -> no fusion
        let mut code = vec![
            Opcode::LocalAddr { dst: 0, slot: 3 },
            Opcode::Store32 { addr: 0, src: 1 },
            Opcode::Load32 { dst: 2, addr: 0 },
        ];
        let original = code.clone();
        fuse_local_access(&mut code);
        assert_eq!(code, original);
    }

    // ========== fuse_offset_access tests ==========

    #[test]
    fn test_fuse_offset_store32() {
        let mut code = vec![
            Opcode::IAddImm {
                dst: 1,
                src: 0,
                imm: 8,
            },
            Opcode::Store32 { addr: 1, src: 2 },
        ];
        fuse_offset_access(&mut code);
        assert_eq!(
            code[0],
            Opcode::Store32Off {
                base: 0,
                offset: 8,
                src: 2
            }
        );
        assert_eq!(code[1], Opcode::Nop);
    }

    #[test]
    fn test_fuse_offset_load32() {
        let mut code = vec![
            Opcode::IAddImm {
                dst: 1,
                src: 0,
                imm: 4,
            },
            Opcode::Load32 { dst: 2, addr: 1 },
        ];
        fuse_offset_access(&mut code);
        assert_eq!(
            code[0],
            Opcode::Load32Off {
                dst: 2,
                base: 0,
                offset: 4
            }
        );
        assert_eq!(code[1], Opcode::Nop);
    }

    #[test]
    fn test_fuse_offset_load64() {
        let mut code = vec![
            Opcode::IAddImm {
                dst: 1,
                src: 0,
                imm: 16,
            },
            Opcode::Load64 { dst: 2, addr: 1 },
        ];
        fuse_offset_access(&mut code);
        assert_eq!(
            code[0],
            Opcode::Load64Off {
                dst: 2,
                base: 0,
                offset: 16
            }
        );
        assert_eq!(code[1], Opcode::Nop);
    }

    #[test]
    fn test_fuse_offset_addr_used_twice_no_fuse() {
        // addr reg used by both Store32 and Load32
        let mut code = vec![
            Opcode::IAddImm {
                dst: 1,
                src: 0,
                imm: 8,
            },
            Opcode::Store32 { addr: 1, src: 2 },
            Opcode::Load32 { dst: 3, addr: 1 },
        ];
        let original = code.clone();
        fuse_offset_access(&mut code);
        assert_eq!(code, original);
    }

    // ========== fuse_compare_branch tests ==========

    #[test]
    fn test_fuse_ilt_jump() {
        let mut code = vec![
            Opcode::ILt { dst: 2, a: 0, b: 1 },
            Opcode::JumpIfZero { cond: 2, offset: 5 },
        ];
        fuse_compare_branch(&mut code);
        // offset adjusts: was 5 relative to JumpIfZero (i+1), now 5+1=6 relative to ILt (i)
        assert_eq!(
            code[0],
            Opcode::ILtJump {
                a: 0,
                b: 1,
                offset: 6
            }
        );
        assert_eq!(code[1], Opcode::Nop);
    }

    #[test]
    fn test_fuse_flt_jump() {
        let mut code = vec![
            Opcode::FLt { dst: 3, a: 0, b: 1 },
            Opcode::JumpIfZero {
                cond: 3,
                offset: 10,
            },
        ];
        fuse_compare_branch(&mut code);
        assert_eq!(
            code[0],
            Opcode::FLtJump {
                a: 0,
                b: 1,
                offset: 11
            }
        );
        assert_eq!(code[1], Opcode::Nop);
    }

    #[test]
    fn test_fuse_compare_branch_cond_used_elsewhere() {
        // Compare result used by both JumpIfZero and later instruction -> no fusion
        let mut code = vec![
            Opcode::ILt { dst: 2, a: 0, b: 1 },
            Opcode::JumpIfZero { cond: 2, offset: 5 },
            Opcode::PrintI32 { src: 2 }, // extra use of r2
        ];
        let original = code.clone();
        fuse_compare_branch(&mut code);
        assert_eq!(code, original);
    }

    #[test]
    fn test_fuse_compare_branch_wrong_cond() {
        // JumpIfZero uses a different register than what ILt wrote to
        let mut code = vec![
            Opcode::ILt { dst: 2, a: 0, b: 1 },
            Opcode::JumpIfZero { cond: 3, offset: 5 },
        ];
        let original = code.clone();
        fuse_compare_branch(&mut code);
        assert_eq!(code, original);
    }

    // ========== strip_nops / compact tests ==========

    #[test]
    fn test_compact_strips_nops() {
        let mut code = vec![
            Opcode::LoadImm { dst: 0, value: 1 },
            Opcode::Nop,
            Opcode::LoadImm { dst: 1, value: 2 },
            Opcode::Nop,
            Opcode::IAdd { dst: 0, a: 0, b: 1 },
        ];
        compact(&mut code);
        assert_eq!(code.len(), 3);
        assert_eq!(code[0], Opcode::LoadImm { dst: 0, value: 1 });
        assert_eq!(code[1], Opcode::LoadImm { dst: 1, value: 2 });
        assert_eq!(code[2], Opcode::IAdd { dst: 0, a: 0, b: 1 });
    }

    #[test]
    fn test_compact_adjusts_jump_offsets() {
        // Jump over Nops: offset should shrink
        let mut code = vec![
            Opcode::Jump { offset: 2 }, // target = 0 + 1 + 2 = index 3
            Opcode::Nop,
            Opcode::Nop,
            Opcode::LoadImm { dst: 0, value: 42 },
        ];
        compact(&mut code);
        assert_eq!(code.len(), 2);
        // After compaction: Jump at 0, LoadImm at 1. offset = 1 - 0 - 1 = 0
        assert_eq!(code[0], Opcode::Jump { offset: 0 });
        assert_eq!(code[1], Opcode::LoadImm { dst: 0, value: 42 });
    }

    // ========== move_forwarding edge cases ==========

    #[test]
    fn test_move_forwarding_move_to_move() {
        // Move → Move chain: Move{dst:1, src:0} + Move{dst:2, src:1}
        // Should forward: Move{dst:2, src:0} + Nop
        let mut code = vec![
            Opcode::LoadImm { dst: 0, value: 7 },
            Opcode::Move { dst: 1, src: 0 },
            Opcode::Move { dst: 2, src: 1 },
        ];
        move_forwarding(&mut code);
        // r1 only used by second move, so first Move rewrites dst to 2
        // but r0 is used by both the LoadImm consumer and the first Move src,
        // so we need to check use counts carefully
        // Actually r0 has use_count=1 (only first Move reads it), r1 has use_count=1 (only second Move)
        // First pass: LoadImm→Move: r0 used once → forward LoadImm dst to 1, Nop
        // Then at i=1 (now Nop) + i=2: Move{dst:2,src:1} — src=1, but r1 was result of LoadImm
        // Let's just check the outcome
        assert_eq!(code[1], Opcode::Nop);
    }

    #[test]
    fn test_move_forwarding_self_move() {
        // Move { dst: 0, src: 0 } — trivial, shouldn't crash
        let mut code = vec![
            Opcode::LoadImm { dst: 0, value: 1 },
            Opcode::Move { dst: 0, src: 0 },
        ];
        move_forwarding(&mut code);
        // src==dst, but r0 use count is 1 (the Move reads it)
        // get_dst of LoadImm returns 0, prev_dst==move_src==0, uses[0]==1 → forward
        // set_dst of LoadImm to move_dst=0 (no-op), Nop the Move
        assert_eq!(code[0], Opcode::LoadImm { dst: 0, value: 1 });
        assert_eq!(code[1], Opcode::Nop);
    }

    // ========== DCE edge cases ==========

    #[test]
    fn test_dce_preserves_calls() {
        // Calls have side effects even if their result is unused
        let mut code = vec![
            Opcode::LoadImm { dst: 1, value: 0 },
            Opcode::LoadImm { dst: 2, value: 1 },
            Opcode::Call {
                func: 0,
                args_start: 1,
                arg_count: 2,
            },
            Opcode::LoadImm { dst: 0, value: 99 },
        ];
        dead_code_elimination(&mut code);
        // Call must be preserved; its arg registers (1,2) must stay alive
        assert_eq!(code[0], Opcode::LoadImm { dst: 1, value: 0 });
        assert_eq!(code[1], Opcode::LoadImm { dst: 2, value: 1 });
        assert!(matches!(code[2], Opcode::Call { .. }));
    }

    #[test]
    fn test_dce_preserves_stores() {
        // Store instructions have side effects
        let mut code = vec![
            Opcode::LoadImm { dst: 1, value: 42 },
            Opcode::LocalAddr { dst: 2, slot: 0 },
            Opcode::Store32 { addr: 2, src: 1 },
            Opcode::LoadImm { dst: 0, value: 0 },
        ];
        dead_code_elimination(&mut code);
        // Store32 and its dependencies must survive
        assert_eq!(code[0], Opcode::LoadImm { dst: 1, value: 42 });
        assert_eq!(code[1], Opcode::LocalAddr { dst: 2, slot: 0 });
        assert!(matches!(code[2], Opcode::Store32 { .. }));
    }

    #[test]
    fn test_dce_iterative_dead_chain() {
        // r3 = r1 + r2, r4 = r3 + r1, both dead.
        // First DCE pass kills r4 (unused), second pass kills r3 (now unused).
        let mut code = vec![
            Opcode::LoadImm { dst: 0, value: 99 }, // r0 live (return)
            Opcode::LoadImm { dst: 1, value: 5 },
            Opcode::LoadImm { dst: 2, value: 10 },
            Opcode::IAdd { dst: 3, a: 1, b: 2 },
            Opcode::IAdd { dst: 4, a: 3, b: 1 }, // r4 unused
        ];
        // Single pass: r4 unused → NOP [4]; but r3 now still has use_count from [4]
        dead_code_elimination(&mut code);
        assert_eq!(code[4], Opcode::Nop);
        // After first pass, r3 may still appear used. Run again:
        dead_code_elimination(&mut code);
        assert_eq!(code[3], Opcode::Nop);
        // r1, r2 only fed into dead code, run third pass:
        dead_code_elimination(&mut code);
        assert_eq!(code[1], Opcode::Nop);
        assert_eq!(code[2], Opcode::Nop);
    }

    // ========== fuse_offset_access edge cases ==========

    #[test]
    fn test_fuse_offset_store8() {
        let mut code = vec![
            Opcode::IAddImm {
                dst: 1,
                src: 0,
                imm: 3,
            },
            Opcode::Store8 { addr: 1, src: 2 },
        ];
        fuse_offset_access(&mut code);
        assert_eq!(
            code[0],
            Opcode::Store8Off {
                base: 0,
                offset: 3,
                src: 2
            }
        );
        assert_eq!(code[1], Opcode::Nop);
    }

    #[test]
    fn test_fuse_offset_store64() {
        let mut code = vec![
            Opcode::IAddImm {
                dst: 1,
                src: 0,
                imm: 24,
            },
            Opcode::Store64 { addr: 1, src: 2 },
        ];
        fuse_offset_access(&mut code);
        assert_eq!(
            code[0],
            Opcode::Store64Off {
                base: 0,
                offset: 24,
                src: 2
            }
        );
        assert_eq!(code[1], Opcode::Nop);
    }

    // ========== fuse_compare_branch edge cases ==========

    #[test]
    fn test_fuse_ilt_jump_if_not_zero() {
        // ILt + JumpIfNotZero should NOT fuse (only JumpIfZero fuses)
        let mut code = vec![
            Opcode::ILt { dst: 2, a: 0, b: 1 },
            Opcode::JumpIfNotZero { cond: 2, offset: 5 },
        ];
        let original = code.clone();
        fuse_compare_branch(&mut code);
        assert_eq!(code, original);
    }

    // ========== compact edge cases ==========

    #[test]
    fn test_compact_backward_jump() {
        // Backward jump (loop): Jump at index 3 targets index 1
        // offset = target - (pos+1) = 1 - 4 = -3
        let mut code = vec![
            Opcode::LoadImm { dst: 0, value: 0 },
            Opcode::LoadImm { dst: 1, value: 1 },   // loop start
            Opcode::Nop,
            Opcode::IAdd { dst: 0, a: 0, b: 1 },
            Opcode::Nop,
            Opcode::Jump { offset: -5 }, // target = 5+1+(-5) = 1
        ];
        compact(&mut code);
        // After removing 2 Nops: [LoadImm, LoadImm, IAdd, Jump]
        assert_eq!(code.len(), 4);
        // Jump at new index 3, target should be new index 1
        // offset = 1 - 3 - 1 = -3
        assert_eq!(code[3], Opcode::Jump { offset: -3 });
    }

    #[test]
    fn test_compact_no_nops() {
        let mut code = vec![
            Opcode::LoadImm { dst: 0, value: 1 },
            Opcode::LoadImm { dst: 1, value: 2 },
        ];
        let original = code.clone();
        compact(&mut code);
        assert_eq!(code, original);
    }

    #[test]
    fn test_compact_all_nops() {
        let mut code = vec![Opcode::Nop, Opcode::Nop, Opcode::Nop];
        compact(&mut code);
        assert!(code.is_empty());
    }

    // ========== compute_live_ranges tests ==========

    #[test]
    fn test_live_ranges_basic() {
        let code = vec![
            Opcode::LoadImm { dst: 0, value: 1 },  // def r0 at 0
            Opcode::LoadImm { dst: 1, value: 2 },  // def r1 at 1
            Opcode::IAdd { dst: 2, a: 0, b: 1 },   // use r0,r1 at 2; def r2 at 2
        ];
        let (def_point, last_use, is_used) = compute_live_ranges(&code);
        assert!(is_used[0] && is_used[1] && is_used[2]);
        assert_eq!(def_point[0], 0);
        assert_eq!(def_point[1], 1);
        assert_eq!(def_point[2], 2);
        assert_eq!(last_use[0], 2); // r0 last used at IAdd
        assert_eq!(last_use[1], 2); // r1 last used at IAdd
    }

    #[test]
    fn test_live_ranges_loop_extension() {
        // Backward jump extends live ranges of registers used in the loop
        let code = vec![
            Opcode::LoadImm { dst: 0, value: 0 },    // 0: def r0
            Opcode::LoadImm { dst: 1, value: 10 },   // 1: def r1
            Opcode::IAdd { dst: 0, a: 0, b: 1 },     // 2: use r0,r1 (loop body)
            Opcode::Jump { offset: -2 },              // 3: jump to index 2 (offset = 2 - 3 - 1 = -2)
        ];
        let (def_point, last_use, _is_used) = compute_live_ranges(&code);
        // r1 is defined before loop (at 1), used in loop body (at 2).
        // Backward jump from 3 to 2 should extend r1's last_use to 3 (loop end).
        assert_eq!(def_point[1], 1);
        assert!(last_use[1] >= 3, "r1 last_use should be extended to loop end, got {}", last_use[1]);
    }

    #[test]
    fn test_live_ranges_unused_register() {
        let code = vec![
            Opcode::LoadImm { dst: 5, value: 42 },  // r5 defined but never used
        ];
        let (_def_point, _last_use, is_used) = compute_live_ranges(&code);
        // r5 is "used" because it has a def point (get_dst returns it)
        assert!(is_used[5]);
        // But registers 0-4, 6-255 should not be used
        assert!(!is_used[0]);
        assert!(!is_used[1]);
    }

    // ========== copy_coalesce tests ==========

    #[test]
    fn test_copy_coalesce_basic() {
        // r1 = 42, Move r1 → r2, use r2
        // r1 dies at the Move, r2 is born at the Move → can coalesce
        let mut code = vec![
            Opcode::LoadImm { dst: 1, value: 42 },
            Opcode::Move { dst: 2, src: 1 },
            Opcode::PrintI32 { src: 2 },
        ];
        copy_coalesce(&mut code);
        // Move should be eliminated; r1 and r2 merged to same register
        assert!(matches!(code[1], Opcode::Nop));
        // The PrintI32 should use whichever register the coalescer chose (the lower one = 1)
        assert!(matches!(code[2], Opcode::PrintI32 { src: 1 }));
    }

    #[test]
    fn test_copy_coalesce_skip_call_args() {
        // Registers used as call args shouldn't be coalesced (contiguity requirement)
        let mut code = vec![
            Opcode::LoadImm { dst: 3, value: 1 },
            Opcode::Move { dst: 4, src: 3 },
            Opcode::Call {
                func: 0,
                args_start: 4,
                arg_count: 1,
            },
        ];
        copy_coalesce(&mut code);
        // r4 is a call arg → should NOT be coalesced
        assert!(matches!(code[1], Opcode::Move { dst: 4, src: 3 }));
    }

    #[test]
    fn test_copy_coalesce_self_move() {
        // Move { dst: 1, src: 1 } → should be eliminated as Nop
        let mut code = vec![
            Opcode::LoadImm { dst: 1, value: 5 },
            Opcode::Move { dst: 1, src: 1 },
            Opcode::PrintI32 { src: 1 },
        ];
        copy_coalesce(&mut code);
        assert_eq!(code[1], Opcode::Nop);
    }

    #[test]
    fn test_copy_coalesce_interference() {
        // r1 and r2 both live at the same time → cannot coalesce
        let mut code = vec![
            Opcode::LoadImm { dst: 1, value: 1 },
            Opcode::LoadImm { dst: 2, value: 2 },
            Opcode::Move { dst: 3, src: 1 },   // r1 still live (used below)
            Opcode::IAdd { dst: 0, a: 1, b: 2 },
        ];
        copy_coalesce(&mut code);
        // r1 doesn't die at the Move (it's used at IAdd), so can't coalesce
        assert!(matches!(code[2], Opcode::Move { dst: 3, src: 1 }));
    }

    // ========== register_allocation tests ==========

    #[test]
    fn test_register_allocation_compacts() {
        // Uses r0, r5, r10 → should be compacted to r0, r1, r2
        let mut code = vec![
            Opcode::LoadImm { dst: 5, value: 1 },
            Opcode::LoadImm { dst: 10, value: 2 },
            Opcode::IAdd { dst: 0, a: 5, b: 10 },
        ];
        let (count, mapping) = register_allocation(&mut code);
        // r0 pinned to preg 0
        assert_eq!(mapping[0], 0);
        // r5 and r10 should map to pregs 1 and 2 (or 2 and 1)
        assert!(mapping[5] < count);
        assert!(mapping[10] < count);
        assert_ne!(mapping[5], mapping[10]);
        assert!(count <= 3, "expected at most 3 pregs, got {}", count);
        // Verify instruction was rewritten
        if let Opcode::IAdd { dst, a, b } = code[2] {
            assert_eq!(dst, 0);
            assert_eq!(a, mapping[5]);
            assert_eq!(b, mapping[10]);
        } else {
            panic!("expected IAdd");
        }
    }

    #[test]
    fn test_register_allocation_reuses_expired() {
        // r1 used then dead, r2 used then dead → should reuse same preg
        let mut code = vec![
            Opcode::LoadImm { dst: 1, value: 1 },
            Opcode::Move { dst: 0, src: 1 },       // r1 dies here
            Opcode::LoadImm { dst: 2, value: 2 },
            Opcode::IAdd { dst: 0, a: 0, b: 2 },   // r2 dies here
        ];
        let (count, _mapping) = register_allocation(&mut code);
        // r0 always needed; r1 and r2 don't overlap, so can share → 2 pregs total
        assert!(count <= 2, "expected ≤2 pregs, got {}", count);
    }

    #[test]
    fn test_register_allocation_preserves_call_arg_contiguity() {
        // Call args r3,r4 must remain contiguous after allocation
        let mut code = vec![
            Opcode::LoadImm { dst: 3, value: 10 },
            Opcode::LoadImm { dst: 4, value: 20 },
            Opcode::Call {
                func: 0,
                args_start: 3,
                arg_count: 2,
            },
            Opcode::LoadImm { dst: 0, value: 0 },
        ];
        let (_count, mapping) = register_allocation(&mut code);
        // The mapped registers for r3 and r4 must be contiguous
        let p3 = mapping[3];
        let p4 = mapping[4];
        assert_eq!(
            p4,
            p3 + 1,
            "call args must be contiguous: r3→{}, r4→{}",
            p3,
            p4
        );
        // Verify the Call instruction was updated
        if let Opcode::Call {
            args_start,
            arg_count,
            ..
        } = code[2]
        {
            assert_eq!(args_start, p3);
            assert_eq!(arg_count, 2);
        } else {
            panic!("expected Call");
        }
    }

    #[test]
    fn test_register_allocation_empty_code() {
        let mut code: Vec<Opcode> = vec![];
        let result = optimize(&mut code);
        assert!(result.is_none());
    }

    // ========== full optimize pipeline ==========

    #[test]
    fn test_optimize_pipeline_simple() {
        // LoadImm → Move → PrintI32
        // move_forwarding merges LoadImm+Move, DCE keeps PrintI32 alive
        let mut code = vec![
            Opcode::LoadImm { dst: 5, value: 42 },
            Opcode::Move { dst: 10, src: 5 },
            Opcode::PrintI32 { src: 10 },
        ];
        let result = optimize(&mut code);
        assert!(result.is_some());
        // After optimize, code still has Nops; need compact to remove them
        compact(&mut code);
        // Should be: LoadImm + PrintI32 (move eliminated, regs compacted)
        assert_eq!(code.len(), 2);
        // First instruction loads a value, second prints it
        assert!(matches!(code[0], Opcode::LoadImm { .. }));
        assert!(matches!(code[1], Opcode::PrintI32 { .. }));
    }

    #[test]
    fn test_optimize_pipeline_fuse_and_compact() {
        // LocalAddr + Load32 should fuse to LoadSlot32, then Nop gets stripped
        let mut code = vec![
            Opcode::LocalAddr { dst: 3, slot: 8 },
            Opcode::Load32 { dst: 0, addr: 3 },
        ];
        let result = optimize(&mut code);
        assert!(result.is_some());
        compact(&mut code);
        assert_eq!(code.len(), 1);
        assert!(matches!(code[0], Opcode::LoadSlot32 { .. }));
    }

    #[test]
    fn test_move_forwarding_then_dce() {
        // After move forwarding, the move becomes Nop; r0 stays live as return reg
        let mut code = vec![
            Opcode::LoadImm { dst: 2, value: 42 },
            Opcode::Move { dst: 0, src: 2 }, // r0 is return, r2 only used here
        ];
        move_forwarding(&mut code);
        assert_eq!(code[0], Opcode::LoadImm { dst: 0, value: 42 });
        assert_eq!(code[1], Opcode::Nop);
        dead_code_elimination(&mut code);
        assert_eq!(code[0], Opcode::LoadImm { dst: 0, value: 42 });
    }

    // ========== replace_reg_uses_until_clobber tests ==========

    #[test]
    fn test_replace_reg_uses_stops_at_clobber() {
        let targets = HashSet::new();
        let mut code = vec![
            Opcode::IAdd { dst: 2, a: 0, b: 1 },   // uses r1
            Opcode::LoadImm { dst: 0, value: 99 },  // clobbers r0 (the new_reg)
            Opcode::ISub { dst: 3, a: 0, b: 1 },    // should NOT be rewritten
        ];
        // Replace r1 with r0, starting from index 0
        replace_reg_uses_until_clobber(&mut code, 0, 1, 0, &targets);
        // First instruction: r1 replaced with r0
        assert_eq!(code[0], Opcode::IAdd { dst: 2, a: 0, b: 0 });
        // Third instruction: r0 was clobbered at [1], so replacement stopped
        assert_eq!(code[2], Opcode::ISub { dst: 3, a: 0, b: 1 });
    }

    #[test]
    fn test_replace_reg_uses_stops_at_jump_target() {
        let mut targets = HashSet::new();
        targets.insert(1usize); // index 1 is a jump target
        let mut code = vec![
            Opcode::IAdd { dst: 2, a: 0, b: 1 },   // uses r1
            Opcode::ISub { dst: 3, a: 0, b: 1 },    // at jump target → stop before this
        ];
        replace_reg_uses_until_clobber(&mut code, 0, 1, 0, &targets);
        // Replacement should stop at index 1 (jump target), but index 0 is before it
        assert_eq!(code[0], Opcode::IAdd { dst: 2, a: 0, b: 0 });
        // Index 1 NOT reached (replacement stopped before jump target)
        assert_eq!(code[1], Opcode::ISub { dst: 3, a: 0, b: 1 });
    }

    #[test]
    fn test_replace_reg_uses_stops_at_call() {
        let targets = HashSet::new();
        let mut code = vec![
            Opcode::IAdd { dst: 2, a: 0, b: 1 },
            Opcode::Call {
                func: 0,
                args_start: 0,
                arg_count: 1,
            },
            Opcode::ISub { dst: 3, a: 0, b: 1 },
        ];
        replace_reg_uses_until_clobber(&mut code, 0, 1, 0, &targets);
        assert_eq!(code[0], Opcode::IAdd { dst: 2, a: 0, b: 0 });
        // Call stops replacement; instruction after call untouched
        assert_eq!(code[2], Opcode::ISub { dst: 3, a: 0, b: 1 });
    }
}
