//! Automatic fusion discovery and code generation.
//!
//! Profiles stack IR bytecode to find hot instruction sequences,
//! then generates fused StackOp variants, optimizer rules, and C handlers.

use crate::stack_ir::{StackOp, StackProgram};
use std::collections::HashMap;

/// An abstract instruction tag (ignoring immediates) for pattern matching.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum OpTag {
    I64Const,
    F32Const,
    LocalGet,
    LocalSet,
    LocalTee,
    LocalAddr,
    GlobalAddr,
    IAdd,
    ISub,
    IMul,
    IDiv,
    FAdd,
    FSub,
    FMul,
    FDiv,
    DAdd,
    DSub,
    DMul,
    DDiv,
    FNeg,
    INeg,
    DNeg,
    IAddImm,
    IEq,
    INe,
    ILt,
    ILe,
    IGt,
    IGe,
    ULt,
    UGt,
    FEq,
    FNe,
    FLt,
    FLe,
    FGt,
    FGe,
    DEq,
    DLt,
    DLe,
    And,
    Or,
    Xor,
    Not,
    Shl,
    Shr,
    UShr,
    Load8,
    Load32,
    Load64,
    Load32Off,
    Load64Off,
    Store8,
    Store32,
    Store64,
    Store8Off,
    Store32Off,
    Store64Off,
    MemCopy,
    MemZero,
    Jump,
    JumpIfZero,
    JumpIfNotZero,
    SliceLoad32,
    SliceStore32,
    Drop,
    SinF32,
    CosF32,
    I32ToF32,
    F32ToI32,
    I32ToF64,
    F64ToI32,
    F32ToF64,
    F64ToF32,
    I32ToI8,
    I8ToI32,
    Call,
    Return,
    ReturnVoid,
    Other,
    // Already-fused ops (these shouldn't appear in unfused profiling)
    Fused,
}

fn tag_of(op: &StackOp) -> OpTag {
    match op {
        StackOp::I64Const(_) => OpTag::I64Const,
        StackOp::F32Const(_) => OpTag::F32Const,
        StackOp::LocalGet(_) => OpTag::LocalGet,
        StackOp::LocalSet(_) => OpTag::LocalSet,
        StackOp::LocalTee(_) => OpTag::LocalTee,
        StackOp::LocalAddr(_) => OpTag::LocalAddr,
        StackOp::GlobalAddr(_) => OpTag::GlobalAddr,
        StackOp::IAdd => OpTag::IAdd,
        StackOp::ISub => OpTag::ISub,
        StackOp::IMul => OpTag::IMul,
        StackOp::IDiv => OpTag::IDiv,
        StackOp::DAdd => OpTag::DAdd,
        StackOp::DSub => OpTag::DSub,
        StackOp::DMul => OpTag::DMul,
        StackOp::DDiv => OpTag::DDiv,
        StackOp::INeg => OpTag::INeg,
        StackOp::DNeg => OpTag::DNeg,
        StackOp::IAddImm(_) => OpTag::IAddImm,
        StackOp::IEq => OpTag::IEq,
        StackOp::INe => OpTag::INe,
        StackOp::ILt => OpTag::ILt,
        StackOp::ILe => OpTag::ILe,
        StackOp::IGt => OpTag::IGt,
        StackOp::IGe => OpTag::IGe,
        StackOp::ULt => OpTag::ULt,
        StackOp::UGt => OpTag::UGt,
        StackOp::DEq => OpTag::DEq,
        StackOp::DLt => OpTag::DLt,
        StackOp::DLe => OpTag::DLe,
        StackOp::And => OpTag::And,
        StackOp::Or => OpTag::Or,
        StackOp::Xor => OpTag::Xor,
        StackOp::Not => OpTag::Not,
        StackOp::Shl => OpTag::Shl,
        StackOp::Shr => OpTag::Shr,
        StackOp::UShr => OpTag::UShr,
        StackOp::Load8 => OpTag::Load8,
        StackOp::Load32 => OpTag::Load32,
        StackOp::Load64 => OpTag::Load64,
        StackOp::Load32Off(_) => OpTag::Load32Off,
        StackOp::Load64Off(_) => OpTag::Load64Off,
        StackOp::Store8 => OpTag::Store8,
        StackOp::Store32 => OpTag::Store32,
        StackOp::Store64 => OpTag::Store64,
        StackOp::Store8Off(_) => OpTag::Store8Off,
        StackOp::Store32Off(_) => OpTag::Store32Off,
        StackOp::Store64Off(_) => OpTag::Store64Off,
        StackOp::MemCopy(_) => OpTag::MemCopy,
        StackOp::MemZero(_) => OpTag::MemZero,
        StackOp::Jump(_) => OpTag::Jump,
        StackOp::JumpIfZero(_) => OpTag::JumpIfZero,
        StackOp::JumpIfNotZero(_) => OpTag::JumpIfNotZero,
        StackOp::SliceLoad32 => OpTag::SliceLoad32,
        StackOp::SliceStore32 => OpTag::SliceStore32,
        StackOp::Drop => OpTag::Drop,
        StackOp::I32ToF64 => OpTag::I32ToF64,
        StackOp::F64ToI32 => OpTag::F64ToI32,
        StackOp::F32ToF64 => OpTag::F32ToF64,
        StackOp::F64ToF32 => OpTag::F64ToF32,
        StackOp::I32ToI8 => OpTag::I32ToI8,
        StackOp::I8ToI32 => OpTag::I8ToI32,
        StackOp::Call { .. } => OpTag::Call,
        StackOp::Return => OpTag::Return,
        StackOp::ReturnVoid => OpTag::ReturnVoid,
        _ => OpTag::Other,
    }
}

/// Whether an op tag can participate in fusion (not a branch/call boundary).
fn is_fusable(tag: &OpTag) -> bool {
    !matches!(
        tag,
        OpTag::Jump
            | OpTag::JumpIfZero
            | OpTag::JumpIfNotZero
            | OpTag::Call
            | OpTag::Return
            | OpTag::ReturnVoid
            | OpTag::Other
            | OpTag::Fused
    )
}

/// Stack depth delta for an op tag.
fn tag_delta(tag: &OpTag) -> i32 {
    match tag {
        OpTag::I64Const
        | OpTag::F32Const
        | OpTag::LocalGet
        | OpTag::LocalAddr
        | OpTag::GlobalAddr => 1,
        OpTag::LocalSet | OpTag::Drop => -1,
        OpTag::IAdd
        | OpTag::ISub
        | OpTag::IMul
        | OpTag::IDiv
        | OpTag::FAdd
        | OpTag::FSub
        | OpTag::FMul
        | OpTag::FDiv
        | OpTag::DAdd
        | OpTag::DSub
        | OpTag::DMul
        | OpTag::DDiv
        | OpTag::IEq
        | OpTag::INe
        | OpTag::ILt
        | OpTag::ILe
        | OpTag::IGt
        | OpTag::IGe
        | OpTag::ULt
        | OpTag::UGt
        | OpTag::FEq
        | OpTag::FNe
        | OpTag::FLt
        | OpTag::FLe
        | OpTag::FGt
        | OpTag::FGe
        | OpTag::DEq
        | OpTag::DLt
        | OpTag::DLe
        | OpTag::And
        | OpTag::Or
        | OpTag::Xor
        | OpTag::Shl
        | OpTag::Shr
        | OpTag::UShr
        | OpTag::SliceLoad32 => -1,
        OpTag::Store8
        | OpTag::Store32
        | OpTag::Store64
        | OpTag::Store8Off
        | OpTag::Store32Off
        | OpTag::Store64Off
        | OpTag::MemCopy => -2,
        OpTag::MemZero => -1,
        OpTag::SliceStore32 => -3,
        _ => 0, // unary, tee, etc.
    }
}

/// A discovered fusion pattern.
#[derive(Clone, Debug)]
pub struct FusionPattern {
    pub tags: Vec<OpTag>,
    pub count: usize,
    /// Number of dispatches saved = count * (len - 1)
    pub dispatch_savings: usize,
    /// Net stack delta
    pub stack_delta: i32,
    /// Number of immediate slots needed
    pub imm_count: u8,
}

/// Discover hot instruction patterns in a program (run on UNFUSED bytecode).
pub fn discover(
    programs: &[&StackProgram],
    max_len: usize,
    min_count: usize,
) -> Vec<FusionPattern> {
    let mut ngram_counts: HashMap<Vec<OpTag>, usize> = HashMap::new();

    for program in programs {
        for func in &program.functions {
            let tags: Vec<OpTag> = func.ops.iter().map(|op| tag_of(op)).collect();
            let len = tags.len();

            // Find jump targets to avoid fusing across them.
            let mut is_target = vec![false; len];
            for (i, op) in func.ops.iter().enumerate() {
                let off = match op {
                    StackOp::Jump(o) | StackOp::JumpIfZero(o) | StackOp::JumpIfNotZero(o) => {
                        Some(*o)
                    }
                    _ => None,
                };
                if let Some(off) = off {
                    let target = (i as i64 + 1 + off as i64) as usize;
                    if target < len {
                        is_target[target] = true;
                    }
                }
            }

            for n in 2..=max_len {
                for i in 0..len.saturating_sub(n - 1) {
                    let seq = &tags[i..i + n];

                    // Skip if any element is unfusable or if sequence crosses a jump target.
                    let mut valid = true;
                    for j in 0..n {
                        if !is_fusable(&seq[j]) {
                            valid = false;
                            break;
                        }
                        if j > 0 && is_target[i + j] {
                            valid = false;
                            break;
                        }
                    }
                    if !valid {
                        continue;
                    }

                    *ngram_counts.entry(seq.to_vec()).or_insert(0) += 1;
                }
            }
        }
    }

    let mut patterns: Vec<FusionPattern> = ngram_counts
        .into_iter()
        .filter(|(_, count)| *count >= min_count)
        .map(|(tags, count)| {
            let stack_delta: i32 = tags.iter().map(|t| tag_delta(t)).sum();
            let imm_count = tags
                .iter()
                .filter(|t| {
                    matches!(
                        t,
                        OpTag::LocalGet
                            | OpTag::LocalSet
                            | OpTag::LocalTee
                            | OpTag::LocalAddr
                            | OpTag::GlobalAddr
                            | OpTag::I64Const
                            | OpTag::F32Const
                            | OpTag::IAddImm
                            | OpTag::Load32Off
                            | OpTag::Load64Off
                            | OpTag::Store8Off
                            | OpTag::Store32Off
                            | OpTag::Store64Off
                            | OpTag::MemCopy
                            | OpTag::MemZero
                    )
                })
                .count() as u8;
            let dispatch_savings = count * (tags.len() - 1);
            FusionPattern {
                tags,
                count,
                dispatch_savings,
                stack_delta,
                imm_count,
            }
        })
        .collect();

    // Sort by dispatch savings (most impactful first).
    patterns.sort_by(|a, b| b.dispatch_savings.cmp(&a.dispatch_savings));
    patterns
}

/// Format a pattern for display.
pub fn format_pattern(p: &FusionPattern) -> String {
    let names: Vec<&str> = p
        .tags
        .iter()
        .map(|t| match t {
            OpTag::I64Const => "i64.const",
            OpTag::F32Const => "f32.const",
            OpTag::LocalGet => "local.get",
            OpTag::LocalSet => "local.set",
            OpTag::LocalTee => "local.tee",
            OpTag::LocalAddr => "local.addr",
            OpTag::GlobalAddr => "global.addr",
            OpTag::IAdd => "i64.add",
            OpTag::ISub => "i64.sub",
            OpTag::IMul => "i64.mul",
            OpTag::IDiv => "i64.div",
            OpTag::FAdd => "f32.add",
            OpTag::FSub => "f32.sub",
            OpTag::FMul => "f32.mul",
            OpTag::FDiv => "f32.div",
            OpTag::DAdd => "f64.add",
            OpTag::DSub => "f64.sub",
            OpTag::DMul => "f64.mul",
            OpTag::DDiv => "f64.div",
            OpTag::FNeg => "f32.neg",
            OpTag::INeg => "i64.neg",
            OpTag::DNeg => "f64.neg",
            OpTag::IAddImm => "i64.add_imm",
            OpTag::IEq => "i64.eq",
            OpTag::INe => "i64.ne",
            OpTag::ILt => "i64.lt",
            OpTag::ILe => "i64.le",
            OpTag::IGt => "i64.gt",
            OpTag::IGe => "i64.ge",
            OpTag::ULt => "u64.lt",
            OpTag::UGt => "u64.gt",
            OpTag::FEq => "f32.eq",
            OpTag::FLt => "f32.lt",
            OpTag::FLe => "f32.le",
            OpTag::FNe => "f32.ne",
            OpTag::FGt => "f32.gt",
            OpTag::FGe => "f32.ge",
            OpTag::DEq => "f64.eq",
            OpTag::DLt => "f64.lt",
            OpTag::DLe => "f64.le",
            OpTag::And => "i64.and",
            OpTag::Or => "i64.or",
            OpTag::Xor => "i64.xor",
            OpTag::Not => "i64.not",
            OpTag::Shl => "i64.shl",
            OpTag::Shr => "i64.shr",
            OpTag::UShr => "i64.ushr",
            OpTag::Load8 => "i8.load",
            OpTag::Load32 => "i32.load",
            OpTag::Load64 => "i64.load",
            OpTag::Load32Off => "i32.load_off",
            OpTag::Load64Off => "i64.load_off",
            OpTag::Store8 => "i8.store",
            OpTag::Store32 => "i32.store",
            OpTag::Store64 => "i64.store",
            OpTag::Store8Off => "i8.store_off",
            OpTag::Store32Off => "i32.store_off",
            OpTag::Store64Off => "i64.store_off",
            OpTag::MemCopy => "mem.copy",
            OpTag::MemZero => "mem.zero",
            OpTag::SliceLoad32 => "slice.load32",
            OpTag::SliceStore32 => "slice.store32",
            OpTag::Drop => "drop",
            OpTag::SinF32 => "f32.sin",
            OpTag::CosF32 => "f32.cos",
            OpTag::I32ToF32 => "i32_to_f32",
            OpTag::F32ToI32 => "f32_to_i32",
            OpTag::I32ToF64 => "i32_to_f64",
            OpTag::F64ToI32 => "f64_to_i32",
            OpTag::F32ToF64 => "f32_to_f64",
            OpTag::F64ToF32 => "f64_to_f32",
            OpTag::I32ToI8 => "i32_to_i8",
            OpTag::I8ToI32 => "i8_to_i32",
            _ => "?",
        })
        .collect();
    format!(
        "{:>4} x {:>3} saves {:>5}  [imm:{}] delta:{:+}  {}",
        p.count,
        p.tags.len(),
        p.dispatch_savings,
        p.imm_count,
        p.stack_delta,
        names.join(" + ")
    )
}

/// Run discovery on benchmark programs and print results.
pub fn print_report(programs: &[&StackProgram]) {
    let patterns = discover(programs, 5, 3);

    // Filter to patterns that fit in 3 immediate slots.
    let feasible: Vec<_> = patterns.iter().filter(|p| p.imm_count <= 3).collect();

    println!("Top fusion candidates (fit in 3 imm slots):");
    println!("{:<6} {:<5} {:<7} {}", "count", "len", "saves", "pattern");
    println!("{}", "-".repeat(70));
    for p in feasible.iter().take(30) {
        println!("{}", format_pattern(p));
    }

    let total_savings: usize = feasible.iter().take(30).map(|p| p.dispatch_savings).sum();
    let total_ops: usize = programs
        .iter()
        .map(|p| p.functions.iter().map(|f| f.ops.len()).sum::<usize>())
        .sum();
    println!(
        "\nTop 30 patterns would save {} dispatches out of {} total ({:.0}%)",
        total_savings,
        total_ops,
        total_savings as f64 / total_ops as f64 * 100.0
    );
}
