//! Stack IR instruction set for a Silverfir-nano-style interpreter.
//!
//! This defines a stack-based bytecode designed for efficient interpretation
//! with instruction fusion, TOS register windows, and `preserve_none` dispatch.
//!
//! Key design properties:
//! - Operand stack is implicit (instructions pop inputs, push outputs)
//! - Locals are separate numbered slots, preserved across calls
//! - Stack heights are statically known at each instruction
//! - Common sequences (local.get + arithmetic) are fusable

use std::collections::HashMap;
use std::fmt;

use crate::defs::Name;

/// Stack IR instruction.
///
/// All values on the operand stack are 64-bit (i64/f64/pointer).
/// Type-specific instructions interpret the bits appropriately.
#[derive(Clone, Debug, PartialEq)]
pub enum StackOp {
    // === Constants (push 1) ===
    I64Const(i64),
    F32Const(f32),
    F64Const(f64),

    // === Local variables ===
    /// Push the value of scalar local N onto the stack.
    LocalGet(u16),
    /// Pop the top of stack into scalar local N.
    LocalSet(u16),
    /// Copy top of stack into scalar local N (don't pop).
    LocalTee(u16),
    /// Push the memory address of local slot N.
    LocalAddr(u16),

    // === Global variables ===
    /// Push the address of a global at the given byte offset.
    GlobalAddr(i32),

    // === Integer arithmetic (pop 2, push 1 unless noted) ===
    IAdd,
    ISub,
    IMul,
    IDiv,
    UDiv,
    IRem,
    IPow,
    /// Pop 1, push 1: negate.
    INeg,
    /// Pop 1, push 1: add signed immediate.
    IAddImm(i32),

    // === Float32 arithmetic ===
    FAdd,
    FSub,
    FMul,
    FDiv,
    FPow,
    FNeg,

    // === Float64 arithmetic ===
    DAdd,
    DSub,
    DMul,
    DDiv,
    DPow,
    DNeg,

    // === Comparisons (pop 2, push i64 0 or 1) ===
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

    // === Bitwise ===
    And,
    Or,
    Xor,
    /// Pop 1, push 1.
    Not,
    Shl,
    Shr,
    UShr,

    // === Type conversions (pop 1, push 1) ===
    I32ToF32,
    F32ToI32,
    I32ToF64,
    F64ToI32,
    F32ToF64,
    F64ToF32,
    I32ToI8,
    I8ToI32,
    I64ToU32,

    // === Memory loads (pop addr, push value) ===
    Load8,
    Load32,
    Load64,
    /// Pop base, push *(i32*)(base + offset). Sign-extended to i64.
    Load32Off(i32),
    /// Pop base, push *(i64*)(base + offset).
    Load64Off(i32),

    // === Memory stores (pop value, pop addr) ===
    Store8,
    Store32,
    Store64,
    /// Pop value, pop base, *(i8*)(base + offset) = value.
    Store8Off(i32),
    /// Pop value, pop base, *(i32*)(base + offset) = value.
    Store32Off(i32),
    /// Pop value, pop base, *(i64*)(base + offset) = value.
    Store64Off(i32),

    // === Bulk memory ===
    /// Pop src, pop dst, copy N bytes.
    MemCopy(u32),
    /// Pop dst, zero N bytes.
    MemZero(u32),
    /// Pop b, pop a, push 1 if equal, 0 otherwise.
    MemEq(u32),
    MemNe(u32),

    // === Slice operations ===
    SliceEq(u32),
    SliceNe(u32),
    /// Pop index, pop slice_fat_ptr, push i32 value.
    SliceLoad32,
    /// Pop value, pop index, pop slice_fat_ptr.
    SliceStore32,

    // === Control flow ===
    /// Unconditional relative jump.
    Jump(i32),
    /// Pop condition, jump if zero.
    JumpIfZero(i32),
    /// Pop condition, jump if non-zero.
    JumpIfNotZero(i32),

    // === Function calls ===
    /// Pop N args (first arg deepest), call function, push result.
    /// `preserve` is the number of caller TOS values below the args that
    /// must be spilled to memory before the call (non-arg live TOS values,
    /// 0..=4 - args). Filled in by a post-codegen pass from static stack
    /// depth; codegen emits 0 as a placeholder.
    Call { func: u32, args: u8, preserve: u8 },
    /// Pop func_idx, pop N args, call, push result.
    CallIndirect { args: u8 },
    /// Pop fat_ptr_addr, pop N args, call closure, push result.
    CallClosure { args: u8 },
    /// Pop result value, return to caller.
    Return,
    /// Return without a value (void functions).
    ReturnVoid,

    // === Stack manipulation ===
    /// Pop and discard top of stack.
    Drop,

    // === Math builtins (pop 1, push 1) ===
    SinF32,
    CosF32,
    TanF32,
    AsinF32,
    AcosF32,
    AtanF32,
    SinhF32,
    CoshF32,
    TanhF32,
    AsinhF32,
    AcoshF32,
    AtanhF32,
    LnF32,
    ExpF32,
    Exp2F32,
    Log10F32,
    Log2F32,
    SqrtF32,
    AbsF32,
    FloorF32,
    CeilF32,
    SinF64,
    CosF64,
    TanF64,
    AsinF64,
    AcosF64,
    AtanF64,
    SinhF64,
    CoshF64,
    TanhF64,
    AsinhF64,
    AcoshF64,
    AtanhF64,
    LnF64,
    ExpF64,
    Exp2F64,
    Log10F64,
    Log2F64,
    SqrtF64,
    AbsF64,
    FloorF64,
    CeilF64,
    /// Pop 1, push i64 (1 if NaN, 0 otherwise).
    IsnanF32,
    IsnanF64,
    /// Pop 1, push i64 (1 if infinite, 0 otherwise).
    IsinfF32,
    IsinfF64,
    /// Pop 2, push 1.
    Atan2F32,
    Atan2F64,

    // === Debug ===
    /// Pop and print as i32.
    PrintI32,
    /// Pop and print as f32.
    PrintF32,
    /// Pop and print as char.
    Putc,
    /// Pop, panic if zero.
    Assert,

    /// Push the closure pointer set by the most recent CallClosure.
    GetClosurePtr,

    // === Fused superinstructions (generated by stack_optimize) ===

    /// locals[a] * locals[b] (f32). Pop 0, push 1 (int window).
    FusedGetGetFMul(u16, u16),
    /// locals[a] * locals[b] (f32). Pop 0, push 1 (FLOAT window).
    /// Used when the result will flow through more float ops on f0.
    FusedGetGetFMulFW(u16, u16),
    /// f-window: f0 += locals[a] * *(i32*)(lm + slot*8 + off). Pop 0, push 0.
    FusedGetAddrFMulFAddFW(u16, u16, i32),
    /// f-window: f0 -= locals[a] * *(i32*)(lm + slot*8 + off). Pop 0, push 0.
    FusedGetAddrFMulFSubFW(u16, u16, i32),
    /// Pop f0, store as f32 bit-pattern into hot local register L0.
    /// Pays exactly one FP→GPR crossing; after this the value is in l0.
    LocalSetL0FW,
    LocalSetL1FW,
    LocalSetL2FW,
    /// locals[a] + locals[b] (f32). Pop 0, push 1.
    FusedGetGetFAdd(u16, u16),
    /// locals[a] - locals[b] (f32). Pop 0, push 1.
    FusedGetGetFSub(u16, u16),
    /// locals[a] + locals[b] (i64). Pop 0, push 1.
    FusedGetGetIAdd(u16, u16),
    /// locals[a] < locals[b] (i64 signed). Pop 0, push 1.
    FusedGetGetILt(u16, u16),
    /// TOS * locals[a] (f32). Pop 1, push 1.
    FusedGetFMul(u16),
    /// TOS + locals[a] (f32). Pop 1, push 1.
    FusedGetFAdd(u16),
    /// TOS (with locals[a] subtracted). Pop 1, push 1.
    /// Stack: [accum] → push locals[a] → sub → [accum - locals[a]].
    /// Actually: pop accum, push accum - locals[a].
    FusedGetFSub(u16),
    /// Pop b, pop a, push a*b+TOS(c). 3→1 (fused multiply-accumulate).
    /// Actually: pop c, pop b, pop a, push a*b+c.
    /// Wait — f32.mul pops b,a pushes a*b. Then f32.add pops (a*b), pops c, pushes c+(a*b).
    /// So: pop b, pop a, compute a*b, pop c, push c + a*b.
    FusedFMulFAdd,
    /// Same but subtract: pop b, pop a, compute a*b, pop c, push c - a*b.
    FusedFMulFSub,
    /// Push *(i32*)(lm + slot*8 + offset). Pop 0, push 1.
    FusedAddrLoad32Off(u16, i32),
    /// locals[dst] = locals[src] + imm. Pop 0, push 0.
    FusedGetAddImmSet(u16, i32, u16),
    /// if !(locals[a] < locals[b]) jump. Pop 0, push 0.
    FusedGetGetILtJumpIfZero(u16, u16, i32),
    /// locals[n] = const. Pop 0, push 0.
    FusedConstSet(i64, u16),
    /// locals[n] = f32 const. Pop 0, push 0.
    FusedF32ConstSet(f32, u16),
    /// Push slice_data[locals[idx] * 4] from slice at lm+slot*8. Pop 0, push 1.
    FusedAddrGetSliceLoad32(u16, u16),
    /// *(i32*)(slice_data + locals[idx]*4) = TOS. Pop 1, push 0.
    FusedAddrGetSliceStore32(u16, u16),
    /// Push *(i32*)((uint8_t*)(locals + slot) + locals[idx]*4). Pop 0, push 1.
    /// Fast load from an inline local fixed-size array (no fat-pointer
    /// indirection like the slice variant — the data is inline in the frame).
    FusedLocalArrayLoad32(u16, u16),
    /// *(i32*)((uint8_t*)(locals + slot) + locals[idx]*4) = TOS. Pop 1, push 0.
    FusedLocalArrayStore32(u16, u16),

    /// locals[b] = locals[a]. Pop 0, push 0.
    FusedGetSet(u16, u16),
    /// accum += locals[a] * *(i32*)(lm + slot*8 + off). Pop 0, push 0.
    /// Transforms t0 in place: t0 = t0 + locals[a] * load(slot,off).
    FusedGetAddrFMulFAdd(u16, u16, i32),
    /// accum -= locals[a] * *(i32*)(lm + slot*8 + off). Pop 0, push 0.
    FusedGetAddrFMulFSub(u16, u16, i32),
    /// locals[dst] = *(i32*)(lm + slot*8 + off). Pop 0, push 0.
    FusedAddrLoad32OffSet(u16, i32, u16),
    /// *(i32*)(lm + slot*8 + off) = locals[src]. Pop 0, push 0.
    FusedAddrImmGetStore32(u16, i32, u16),
    /// if !(TOS > f32_const) jump. Pop 1, push 0.
    FusedF32ConstFGtJumpIfZero(f32, i32),
    /// locals[dst] = locals[a] + locals[b] (f32). Pop 0, push 0.
    FusedGetGetFAddSet(u16, u16, u16),
    /// locals[dst] = locals[a] - locals[b] (f32). Pop 0, push 0.
    FusedGetGetFSubSet(u16, u16, u16),
    /// locals[dst] = locals[a] * locals[b] (f32). Pop 0, push 0.
    FusedGetGetFMulSet(u16, u16, u16),
    /// locals[dst] = locals[a] / locals[b] (f32). Pop 0, push 0.
    FusedGetGetFDivSet(u16, u16, u16),
    /// locals[dst] = locals[a] + locals[b] (i64). Pop 0, push 0.
    FusedGetGetIAddSet(u16, u16, u16),
    /// locals[dst] = locals[a] - locals[b] (i64). Pop 0, push 0.
    FusedGetGetISubSet(u16, u16, u16),
    /// locals[dst] = locals[a] * locals[b] (i64). Pop 0, push 0.
    FusedGetGetIMulSet(u16, u16, u16),
    /// Copy struct field: *(i32*)(lm+s*8+dst_off) = *(i32*)(lm+s*8+src_off). Pop 0, push 0.
    FusedFieldCopy32(u16, i32, i32),
    /// locals[n] = TOS; slice[locals[idx]*4] = TOS; pop. Combines local.tee + slice store.
    /// Args: n (local to tee), s (slice slot), idx (index local).
    FusedTeeSliceStore32(u16, u16, u16),
    /// locals[theta] = TOS; sincosf(TOS) -> locals[cos_dst], locals[sin_dst]; pop.
    /// Fuses the FFT twiddle-factor computation
    /// `local.tee theta; f32.cos; local.set cos_dst; local.get theta; f32.sin; local.set sin_dst`
    /// into a single handler that calls sincosf, saving 5 dispatches per
    /// butterfly and amortizing the libm call overhead.
    FusedTeeSinCosSet(u16, u16, u16),

    // === Hot local register ops (generated by stack_hot_locals) ===
    // These access l0/l1/l2 register arguments instead of locals[] memory.

    /// Push hot local register 0.
    LocalGetL0,
    /// Push hot local register 1.
    LocalGetL1,
    /// Push hot local register 2.
    LocalGetL2,
    /// Pop into hot local register 0.
    LocalSetL0,
    /// Pop into hot local register 1.
    LocalSetL1,
    /// Pop into hot local register 2.
    LocalSetL2,

    Halt,
    Nop,
}

/// A compiled stack-based function.
pub struct StackFunction {
    pub name: String,
    pub param_count: u8,
    /// Number of scalar local slots (includes params).
    pub local_count: u16,
    /// Bytes of memory-backed local storage.
    pub local_memory: u32,
    /// The instruction stream.
    pub ops: Vec<StackOp>,
    /// Whether this function returns a value.
    pub has_return_value: bool,
    /// Hot local mapping: hot_locals[i] = original local index that maps to L_i register.
    /// None means this register slot is unused.
    pub hot_locals: [Option<u16>; 3],
}

impl StackFunction {
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            param_count: 0,
            local_count: 0,
            local_memory: 0,
            ops: Vec::new(),
            has_return_value: false,
            hot_locals: [None; 3],
        }
    }

    pub fn emit(&mut self, op: StackOp) {
        self.ops.push(op);
    }

    /// Current instruction index (for jump targets).
    pub fn pos(&self) -> usize {
        self.ops.len()
    }

    /// Patch a jump instruction at `idx` to target the current position.
    pub fn patch_jump(&mut self, idx: usize) {
        let target = self.ops.len() as i32;
        let origin = idx as i32 + 1; // jumps are relative to instruction after the jump
        match &mut self.ops[idx] {
            StackOp::Jump(ref mut off)
            | StackOp::JumpIfZero(ref mut off)
            | StackOp::JumpIfNotZero(ref mut off) => {
                *off = target - origin;
            }
            _ => panic!("patch_jump on non-jump instruction"),
        }
    }
}

/// A compiled stack program.
pub struct StackProgram {
    pub functions: Vec<StackFunction>,
    pub entry: u32,
    pub entry_points: HashMap<Name, u32>,
    pub globals_size: usize,
}

impl StackProgram {
    pub fn new() -> Self {
        Self {
            functions: Vec::new(),
            entry: 0,
            entry_points: HashMap::new(),
            globals_size: 0,
        }
    }

    pub fn add_function(&mut self, func: StackFunction) -> u32 {
        let idx = self.functions.len() as u32;
        self.functions.push(func);
        idx
    }
}

/// Display support for stack IR disassembly.
impl fmt::Display for StackOp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            StackOp::I64Const(v) => write!(f, "i64.const {}", v),
            StackOp::F32Const(v) => write!(f, "f32.const {}", v),
            StackOp::F64Const(v) => write!(f, "f64.const {}", v),
            StackOp::LocalGet(n) => write!(f, "local.get {}", n),
            StackOp::LocalSet(n) => write!(f, "local.set {}", n),
            StackOp::LocalTee(n) => write!(f, "local.tee {}", n),
            StackOp::LocalAddr(n) => write!(f, "local.addr {}", n),
            StackOp::GlobalAddr(off) => write!(f, "global.addr {}", off),
            StackOp::IAdd => write!(f, "i64.add"),
            StackOp::ISub => write!(f, "i64.sub"),
            StackOp::IMul => write!(f, "i64.mul"),
            StackOp::IDiv => write!(f, "i64.div_s"),
            StackOp::UDiv => write!(f, "i64.div_u"),
            StackOp::IRem => write!(f, "i64.rem_s"),
            StackOp::IPow => write!(f, "i64.pow"),
            StackOp::INeg => write!(f, "i64.neg"),
            StackOp::IAddImm(v) => write!(f, "i64.add_imm {}", v),
            StackOp::FAdd => write!(f, "f32.add"),
            StackOp::FSub => write!(f, "f32.sub"),
            StackOp::FMul => write!(f, "f32.mul"),
            StackOp::FDiv => write!(f, "f32.div"),
            StackOp::FPow => write!(f, "f32.pow"),
            StackOp::FNeg => write!(f, "f32.neg"),
            StackOp::DAdd => write!(f, "f64.add"),
            StackOp::DSub => write!(f, "f64.sub"),
            StackOp::DMul => write!(f, "f64.mul"),
            StackOp::DDiv => write!(f, "f64.div"),
            StackOp::DPow => write!(f, "f64.pow"),
            StackOp::DNeg => write!(f, "f64.neg"),
            StackOp::IEq => write!(f, "i64.eq"),
            StackOp::INe => write!(f, "i64.ne"),
            StackOp::ILt => write!(f, "i64.lt_s"),
            StackOp::ILe => write!(f, "i64.le_s"),
            StackOp::IGt => write!(f, "i64.gt_s"),
            StackOp::IGe => write!(f, "i64.ge_s"),
            StackOp::ULt => write!(f, "i64.lt_u"),
            StackOp::UGt => write!(f, "i64.gt_u"),
            StackOp::FEq => write!(f, "f32.eq"),
            StackOp::FNe => write!(f, "f32.ne"),
            StackOp::FLt => write!(f, "f32.lt"),
            StackOp::FLe => write!(f, "f32.le"),
            StackOp::FGt => write!(f, "f32.gt"),
            StackOp::FGe => write!(f, "f32.ge"),
            StackOp::DEq => write!(f, "f64.eq"),
            StackOp::DLt => write!(f, "f64.lt"),
            StackOp::DLe => write!(f, "f64.le"),
            StackOp::And => write!(f, "i64.and"),
            StackOp::Or => write!(f, "i64.or"),
            StackOp::Xor => write!(f, "i64.xor"),
            StackOp::Not => write!(f, "i64.not"),
            StackOp::Shl => write!(f, "i64.shl"),
            StackOp::Shr => write!(f, "i64.shr_s"),
            StackOp::UShr => write!(f, "i64.shr_u"),
            StackOp::I32ToF32 => write!(f, "convert.i32_to_f32"),
            StackOp::F32ToI32 => write!(f, "convert.f32_to_i32"),
            StackOp::I32ToF64 => write!(f, "convert.i32_to_f64"),
            StackOp::F64ToI32 => write!(f, "convert.f64_to_i32"),
            StackOp::F32ToF64 => write!(f, "convert.f32_to_f64"),
            StackOp::F64ToF32 => write!(f, "convert.f64_to_f32"),
            StackOp::I32ToI8 => write!(f, "convert.i32_to_i8"),
            StackOp::I8ToI32 => write!(f, "convert.i8_to_i32"),
            StackOp::I64ToU32 => write!(f, "convert.i64_to_u32"),
            StackOp::Load8 => write!(f, "i8.load"),
            StackOp::Load32 => write!(f, "i32.load"),
            StackOp::Load64 => write!(f, "i64.load"),
            StackOp::Load32Off(off) => write!(f, "i32.load offset={}", off),
            StackOp::Load64Off(off) => write!(f, "i64.load offset={}", off),
            StackOp::Store8 => write!(f, "i8.store"),
            StackOp::Store32 => write!(f, "i32.store"),
            StackOp::Store64 => write!(f, "i64.store"),
            StackOp::Store8Off(off) => write!(f, "i8.store offset={}", off),
            StackOp::Store32Off(off) => write!(f, "i32.store offset={}", off),
            StackOp::Store64Off(off) => write!(f, "i64.store offset={}", off),
            StackOp::MemCopy(n) => write!(f, "memory.copy {}", n),
            StackOp::MemZero(n) => write!(f, "memory.zero {}", n),
            StackOp::MemEq(n) => write!(f, "memory.eq {}", n),
            StackOp::MemNe(n) => write!(f, "memory.ne {}", n),
            StackOp::SliceEq(n) => write!(f, "slice.eq elem_size={}", n),
            StackOp::SliceNe(n) => write!(f, "slice.ne elem_size={}", n),
            StackOp::SliceLoad32 => write!(f, "slice.load32"),
            StackOp::SliceStore32 => write!(f, "slice.store32"),
            StackOp::Jump(off) => write!(f, "jump {}", off),
            StackOp::JumpIfZero(off) => write!(f, "jump_if_zero {}", off),
            StackOp::JumpIfNotZero(off) => write!(f, "jump_if_not_zero {}", off),
            StackOp::Call { func, args, preserve } => {
                write!(f, "call func={} args={} preserve={}", func, args, preserve)
            }
            StackOp::CallIndirect { args } => write!(f, "call_indirect args={}", args),
            StackOp::CallClosure { args } => write!(f, "call_closure args={}", args),
            StackOp::Return => write!(f, "return"),
            StackOp::ReturnVoid => write!(f, "return_void"),
            StackOp::Drop => write!(f, "drop"),
            StackOp::SinF32 => write!(f, "f32.sin"),
            StackOp::CosF32 => write!(f, "f32.cos"),
            StackOp::TanF32 => write!(f, "f32.tan"),
            StackOp::AsinF32 => write!(f, "f32.asin"),
            StackOp::AcosF32 => write!(f, "f32.acos"),
            StackOp::AtanF32 => write!(f, "f32.atan"),
            StackOp::SinhF32 => write!(f, "f32.sinh"),
            StackOp::CoshF32 => write!(f, "f32.cosh"),
            StackOp::TanhF32 => write!(f, "f32.tanh"),
            StackOp::AsinhF32 => write!(f, "f32.asinh"),
            StackOp::AcoshF32 => write!(f, "f32.acosh"),
            StackOp::AtanhF32 => write!(f, "f32.atanh"),
            StackOp::LnF32 => write!(f, "f32.ln"),
            StackOp::ExpF32 => write!(f, "f32.exp"),
            StackOp::Exp2F32 => write!(f, "f32.exp2"),
            StackOp::Log10F32 => write!(f, "f32.log10"),
            StackOp::Log2F32 => write!(f, "f32.log2"),
            StackOp::SqrtF32 => write!(f, "f32.sqrt"),
            StackOp::AbsF32 => write!(f, "f32.abs"),
            StackOp::FloorF32 => write!(f, "f32.floor"),
            StackOp::CeilF32 => write!(f, "f32.ceil"),
            StackOp::SinF64 => write!(f, "f64.sin"),
            StackOp::CosF64 => write!(f, "f64.cos"),
            StackOp::TanF64 => write!(f, "f64.tan"),
            StackOp::AsinF64 => write!(f, "f64.asin"),
            StackOp::AcosF64 => write!(f, "f64.acos"),
            StackOp::AtanF64 => write!(f, "f64.atan"),
            StackOp::SinhF64 => write!(f, "f64.sinh"),
            StackOp::CoshF64 => write!(f, "f64.cosh"),
            StackOp::TanhF64 => write!(f, "f64.tanh"),
            StackOp::AsinhF64 => write!(f, "f64.asinh"),
            StackOp::AcoshF64 => write!(f, "f64.acosh"),
            StackOp::AtanhF64 => write!(f, "f64.atanh"),
            StackOp::LnF64 => write!(f, "f64.ln"),
            StackOp::ExpF64 => write!(f, "f64.exp"),
            StackOp::Exp2F64 => write!(f, "f64.exp2"),
            StackOp::Log10F64 => write!(f, "f64.log10"),
            StackOp::Log2F64 => write!(f, "f64.log2"),
            StackOp::SqrtF64 => write!(f, "f64.sqrt"),
            StackOp::AbsF64 => write!(f, "f64.abs"),
            StackOp::FloorF64 => write!(f, "f64.floor"),
            StackOp::CeilF64 => write!(f, "f64.ceil"),
            StackOp::IsnanF32 => write!(f, "f32.isnan"),
            StackOp::IsnanF64 => write!(f, "f64.isnan"),
            StackOp::IsinfF32 => write!(f, "f32.isinf"),
            StackOp::IsinfF64 => write!(f, "f64.isinf"),
            StackOp::Atan2F32 => write!(f, "f32.atan2"),
            StackOp::Atan2F64 => write!(f, "f64.atan2"),
            StackOp::PrintI32 => write!(f, "debug.print_i32"),
            StackOp::PrintF32 => write!(f, "debug.print_f32"),
            StackOp::Putc => write!(f, "debug.putc"),
            StackOp::Assert => write!(f, "debug.assert"),
            StackOp::GetClosurePtr => write!(f, "get_closure_ptr"),
            StackOp::FusedGetGetFMul(a, b) => write!(f, "fused.get_get_fmul {} {}", a, b),
            StackOp::FusedGetGetFMulFW(a, b) => write!(f, "fused.get_get_fmul_fw {} {}", a, b),
            StackOp::FusedGetAddrFMulFAddFW(a, s, o) => write!(f, "fused.get_addr_fmul_fadd_fw {} {} {}", a, s, o),
            StackOp::FusedGetAddrFMulFSubFW(a, s, o) => write!(f, "fused.get_addr_fmul_fsub_fw {} {} {}", a, s, o),
            StackOp::LocalSetL0FW => write!(f, "local.set_l0_fw"),
            StackOp::LocalSetL1FW => write!(f, "local.set_l1_fw"),
            StackOp::LocalSetL2FW => write!(f, "local.set_l2_fw"),
            StackOp::FusedGetGetFAdd(a, b) => write!(f, "fused.get_get_fadd {} {}", a, b),
            StackOp::FusedGetGetFSub(a, b) => write!(f, "fused.get_get_fsub {} {}", a, b),
            StackOp::FusedGetGetIAdd(a, b) => write!(f, "fused.get_get_iadd {} {}", a, b),
            StackOp::FusedGetGetILt(a, b) => write!(f, "fused.get_get_ilt {} {}", a, b),
            StackOp::FusedGetFMul(a) => write!(f, "fused.get_fmul {}", a),
            StackOp::FusedGetFAdd(a) => write!(f, "fused.get_fadd {}", a),
            StackOp::FusedGetFSub(a) => write!(f, "fused.get_fsub {}", a),
            StackOp::FusedFMulFAdd => write!(f, "fused.fmul_fadd"),
            StackOp::FusedFMulFSub => write!(f, "fused.fmul_fsub"),
            StackOp::FusedAddrLoad32Off(s, o) => write!(f, "fused.addr_load32off {} {}", s, o),
            StackOp::FusedGetAddImmSet(s, v, d) => write!(f, "fused.get_addimm_set {} {} {}", s, v, d),
            StackOp::FusedGetGetILtJumpIfZero(a, b, o) => write!(f, "fused.get_get_ilt_jiz {} {} {}", a, b, o),
            StackOp::FusedConstSet(v, n) => write!(f, "fused.const_set {} {}", v, n),
            StackOp::FusedF32ConstSet(v, n) => write!(f, "fused.f32const_set {} {}", v, n),
            StackOp::FusedAddrGetSliceLoad32(s, i) => write!(f, "fused.addr_get_sload32 {} {}", s, i),
            StackOp::FusedAddrGetSliceStore32(s, i) => write!(f, "fused.addr_get_sstore32 {} {}", s, i),
            StackOp::FusedLocalArrayLoad32(s, i) => write!(f, "fused.local_array_load32 {} {}", s, i),
            StackOp::FusedLocalArrayStore32(s, i) => write!(f, "fused.local_array_store32 {} {}", s, i),
            StackOp::FusedGetSet(a, b) => write!(f, "fused.get_set {} {}", a, b),
            StackOp::FusedGetAddrFMulFAdd(a, s, o) => write!(f, "fused.get_addr_fmul_fadd {} {} {}", a, s, o),
            StackOp::FusedGetAddrFMulFSub(a, s, o) => write!(f, "fused.get_addr_fmul_fsub {} {} {}", a, s, o),
            StackOp::FusedAddrLoad32OffSet(s, o, d) => write!(f, "fused.addr_load32off_set {} {} {}", s, o, d),
            StackOp::FusedAddrImmGetStore32(s, o, src) => write!(f, "fused.addr_imm_get_store32 {} {} {}", s, o, src),
            StackOp::FusedF32ConstFGtJumpIfZero(v, o) => write!(f, "fused.f32const_fgt_jiz {} {}", v, o),
            StackOp::FusedGetGetFAddSet(a, b, d) => write!(f, "fused.get_get_fadd_set {} {} {}", a, b, d),
            StackOp::FusedGetGetFSubSet(a, b, d) => write!(f, "fused.get_get_fsub_set {} {} {}", a, b, d),
            StackOp::FusedGetGetFMulSet(a, b, d) => write!(f, "fused.get_get_fmul_set {} {} {}", a, b, d),
            StackOp::FusedGetGetFDivSet(a, b, d) => write!(f, "fused.get_get_fdiv_set {} {} {}", a, b, d),
            StackOp::FusedGetGetIAddSet(a, b, d) => write!(f, "fused.get_get_iadd_set {} {} {}", a, b, d),
            StackOp::FusedGetGetISubSet(a, b, d) => write!(f, "fused.get_get_isub_set {} {} {}", a, b, d),
            StackOp::FusedGetGetIMulSet(a, b, d) => write!(f, "fused.get_get_imul_set {} {} {}", a, b, d),
            StackOp::FusedFieldCopy32(s, src, dst) => write!(f, "fused.field_copy32 {} {} {}", s, src, dst),
            StackOp::FusedTeeSliceStore32(n, s, idx) => write!(f, "fused.tee_sstore32 {} {} {}", n, s, idx),
            StackOp::FusedTeeSinCosSet(t, c, s) => write!(f, "fused.tee_sincos_set {} {} {}", t, c, s),
            StackOp::LocalGetL0 => write!(f, "local.get_l0"),
            StackOp::LocalGetL1 => write!(f, "local.get_l1"),
            StackOp::LocalGetL2 => write!(f, "local.get_l2"),
            StackOp::LocalSetL0 => write!(f, "local.set_l0"),
            StackOp::LocalSetL1 => write!(f, "local.set_l1"),
            StackOp::LocalSetL2 => write!(f, "local.set_l2"),
            StackOp::Halt => write!(f, "halt"),
            StackOp::Nop => write!(f, "nop"),
        }
    }
}
