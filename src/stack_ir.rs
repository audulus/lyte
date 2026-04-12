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

    // (Float32 arithmetic is F-window only — see F32ConstF, FAddF, etc.
    // below. The int-window variants were removed.)

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
    // (f32 comparisons are F-window only — see FEqF, FNeF, etc. below.)
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
    // (f32↔i32 conversions are F-window variants — I32ToF32F, F32ToI32F.)
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
    Call {
        func: u32,
        args: u8,
        preserve: u8,
    },
    /// Pop func_idx, pop N args, call, push result.
    CallIndirect {
        args: u8,
    },
    /// Pop fat_ptr_addr, pop N args, call closure, push result.
    CallClosure {
        args: u8,
    },
    /// Pop result value, return to caller.
    Return,
    /// Return without a value (void functions).
    ReturnVoid,

    // === Stack manipulation ===
    /// Pop and discard top of stack.
    Drop,

    // === Math builtins (pop 1, push 1) ===
    // (f32 math intrinsics are F-window only — see SinF32F, CosF32F,
    // etc. in the float-window section below.)
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
    IsnanF64,
    /// Pop 1, push i64 (1 if infinite, 0 otherwise).
    IsinfF64,
    /// Pop 2, push 1.
    Atan2F64,

    // === Debug ===
    /// Pop and print as i32.
    PrintI32,
    /// (f32 print is PrintF32F — F-window only.)
    /// Pop and print as char.
    Putc,
    /// Pop, panic if zero.
    Assert,

    /// Push the closure pointer set by the most recent CallClosure.
    GetClosurePtr,

    // === Fused superinstructions (generated by stack_optimize) ===

    // (The int-window f32 fusions — FusedGetGetFMul/FAdd/FSub,
    // FusedGetFMul/FAdd/FSub, FusedFMulFAdd/FSub,
    // FusedGetAddrFMulFAdd/FSub, and the FW-peephole outputs
    // FusedGetGetFMulFW / FusedGetAddrFMulF*FW / LocalSetL*FW — were
    // all removed. f32 fusion happens in F-variants only, in the
    // float-window section below.)
    /// locals[a] + locals[b] (i64). Pop 0, push 1.
    FusedGetGetIAdd(u16, u16),
    /// locals[a] < locals[b] (i64 signed). Pop 0, push 1.
    FusedGetGetILt(u16, u16),
    /// Push *(i32*)(lm + slot*8 + offset). Pop 0, push 1.
    FusedAddrLoad32Off(u16, i32),
    /// locals[dst] = locals[src] + imm. Pop 0, push 0.
    FusedGetAddImmSet(u16, i32, u16),
    /// if !(locals[a] < locals[b]) jump. Pop 0, push 0.
    FusedGetGetILtJumpIfZero(u16, u16, i32),
    /// if any packed `(idx < len)` check fails, jump. Pop 0, push 0.
    FusedBoundsCheck1JumpIfZero([u8; 2], i32),
    FusedBoundsCheck2JumpIfZero([u8; 4], i32),
    FusedBoundsCheck3JumpIfZero([u8; 6], i32),
    FusedBoundsCheck4JumpIfZero([u8; 8], i32),
    FusedBoundsCheck5JumpIfZero([u8; 10], i32),
    FusedBoundsCheck6JumpIfZero([u8; 12], i32),
    FusedBoundsCheck7JumpIfZero([u8; 14], i32),
    FusedBoundsCheck8JumpIfZero([u8; 16], i32),
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
    /// locals[dst] = *(i32*)(lm + slot*8 + off). Pop 0, push 0.
    FusedAddrLoad32OffSet(u16, i32, u16),
    /// *(i32*)(lm + slot*8 + off) = locals[src]. Pop 0, push 0.
    FusedAddrImmGetStore32(u16, i32, u16),
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
    // (FusedTeeSinCosSet removed — the twiddle-factor fusion matched
    // int-window CosF32/SinF32, which no longer exist. The F-variant
    // fusion for the same pattern is a future optimization.)

    // === Float-window (FP register) ops — the only path for f32 ===
    //
    // f32 values live in f0..f3, the FP TOS window (v0-v3 on aarch64,
    // xmm0-xmm3 on x86-64). Every f32 op reads and writes f0..f3
    // directly, so f32 arithmetic never visits a GPR and never pays a
    // GPR↔FP crossing. The legacy integer-window f32 ops were removed
    // because the F handlers are strictly shorter (3+ fewer fmov
    // crossings per hot op) — see docs/FP_CODEGEN_PLAN.md.
    //
    // The window is typed as `float`, not `double`, so f32 arithmetic
    // compiles to direct single-precision FMA/fadd/... without the
    // fcvt round-trips that a double-typed window forces on every op
    // (see §12.2 of the plan).
    /// Push an f32 constant onto the float window.
    F32ConstF(f32),
    /// Push the value of scalar local N (interpreted as f32) onto the float window.
    LocalGetF(u16),
    /// Pop top of float window into scalar local N (stored as f32 bit pattern).
    LocalSetF(u16),
    /// Copy top of float window into scalar local N (don't pop).
    LocalTeeF(u16),
    /// Pop and discard top of float window.
    DropF,

    // Float arithmetic (binary, pop 2 push 1, all in f-window).
    FAddF,
    FSubF,
    FMulF,
    FDivF,
    FPowF,
    /// Pop 1 push 1 (negate) on f-window.
    FNegF,

    // Float comparisons: pop 2 from f-window, push 0/1 to **int** window.
    FEqF,
    FNeF,
    FLtF,
    FLeF,
    FGtF,
    FGeF,

    // Conversions / window crossings
    /// Pop f0 (as float), push int t0 (= signed i32 cast).
    F32ToI32F,
    /// Pop t0 (as i32), push f0.
    I32ToF32F,
    /// Pop f0, push t0 holding the f32 bit pattern (no numeric conversion).
    /// Used at function-call boundaries where args/returns are passed via
    /// the integer locals path.
    FToBitsF,
    /// Inverse of FToBitsF.
    BitsToFF,
    /// Pop index and slice_fat_ptr from the int window, push f32 to f0.
    SliceLoad32F,
    /// Pop f0, then pop index and slice_fat_ptr from the int window.
    SliceStore32F,

    // Float memory loads: pop address from int window, push float to f0.
    LoadF32F,
    LoadF32OffF(i32),
    // Float memory stores: pop float from f0, pop address from int window.
    StoreF32F,
    StoreF32OffF(i32),

    // Float math intrinsics — all read f0, write f0.
    SinF32F,
    CosF32F,
    TanF32F,
    AsinF32F,
    AcosF32F,
    AtanF32F,
    SinhF32F,
    CoshF32F,
    TanhF32F,
    AsinhF32F,
    AcoshF32F,
    AtanhF32F,
    LnF32F,
    ExpF32F,
    Exp2F32F,
    Log10F32F,
    Log2F32F,
    SqrtF32F,
    AbsF32F,
    FloorF32F,
    CeilF32F,
    /// Read f0 and f1, push result to f0.
    Atan2F32F,
    /// Pop f0, push int t0 (1 if NaN, else 0).
    IsnanF32F,
    /// Pop f0, push int t0 (1 if infinite, else 0).
    IsinfF32F,

    // Debug
    /// Pop f0, printf as f32.
    PrintF32F,

    // === Float-window fused superinstructions (Phase 5) ===
    /// f0 = locals[a] + locals[b] (f32). Pop 0, push 1.
    FusedGetGetFAddF(u16, u16),
    /// f0 = locals[a] - locals[b] (f32).
    FusedGetGetFSubF(u16, u16),
    /// f0 = locals[a] * locals[b] (f32).
    FusedGetGetFMulF(u16, u16),
    /// f0 = f0 * locals[a] (f32). Pop 0, push 0.
    FusedGetFMulF(u16),
    /// f0 = f0 + locals[a] (f32).
    FusedGetFAddF(u16),
    /// f0 = f0 - locals[a] (f32).
    FusedGetFSubF(u16),
    /// Pop f0=b, f1=a, f2=c (in f-window), push c + a*b. 3→1.
    FusedFMulFAddF,
    /// Same but a*b - c — i.e. push c - a*b.
    FusedFMulFSubF,
    /// f0 = f0 + locals[a] * locals[b] (f32). Pop 0, push 0.
    FusedGetGetFMulFAddF(u16, u16),
    /// f0 = f0 - locals[a] * locals[b] (f32). Pop 0, push 0.
    FusedGetGetFMulFSubF(u16, u16),
    /// f0 = sum of 2 signed local-local products. `sub_mask` bits select subtraction.
    FusedGetGetFMulSum2F([u8; 4], u8),
    /// f0 = sum of 3 signed local-local products. `sub_mask` bits select subtraction.
    FusedGetGetFMulSum3F([u8; 6], u8),
    /// f0 = sum of 4 signed local-local products. `sub_mask` bits select subtraction.
    FusedGetGetFMulSum4F([u8; 8], u8),
    /// f0 = sum of 5 signed local-local products. `sub_mask` bits select subtraction.
    FusedGetGetFMulSum5F([u8; 10], u8),
    /// f0 = sum of 6 signed local-local products. `sub_mask` bits select subtraction.
    FusedGetGetFMulSum6F([u8; 12], u8),
    /// f0 = sum of 7 signed local-local products. `sub_mask` bits select subtraction.
    FusedGetGetFMulSum7F([u8; 14], u8),
    /// f0 = sum of 8 signed local-local products. `sub_mask` bits select subtraction.
    FusedGetGetFMulSum8F([u8; 16], u8),
    /// f0 = f0 + locals[a] * load_f32(slot, off). Pop 0, push 0.
    FusedGetAddrFMulFAddF(u16, u16, i32),
    /// f0 = f0 - locals[a] * load_f32(slot, off).
    FusedGetAddrFMulFSubF(u16, u16, i32),
    /// f0 = load_f32(slot, off). Pop 0, push 1.
    FusedAddrLoad32OffF(u16, i32),
    /// f0 = slice[locals[idx] * 4] from slice at slot (f32).
    FusedAddrGetSliceLoad32F(u16, u16),
    /// Pop f0, store to slice[locals[idx] * 4] at slot.
    FusedAddrGetSliceStore32F(u16, u16),
    /// Float-window mirror of FusedTeeSliceStore32: pop f0, write its bits
    /// to locals[n], and also store it as f32 to slice[locals[idx]*4] at
    /// `slot`. Order: (local_idx_n, slot, idx_local). Pops 1 from f-window.
    FusedTeeSliceStore32F(u16, u16, u16),
    /// f0 = local_array[locals[idx] * 4] at slot.
    FusedLocalArrayLoad32F(u16, u16),
    /// Pop f0, store to local_array[locals[idx] * 4] at slot.
    FusedLocalArrayStore32F(u16, u16),
    /// locals[dst] = locals[src] (f32). No stack change.
    FusedGetSetF(u8, u8),
    /// Sequential float local copies packed as src0,dst0,src1,dst1,...
    FusedGetSet2F([u8; 4]),
    FusedGetSet3F([u8; 6]),
    FusedGetSet4F([u8; 8]),
    FusedGetSet5F([u8; 10]),
    FusedGetSet6F([u8; 12]),
    FusedGetSet7F([u8; 14]),
    FusedGetSet8F([u8; 16]),
    /// if !(f0 > const) jump. Pop 1 (f-window), conditionally jump.
    FusedF32ConstFGtJumpIfZeroF(f32, i32),
    /// if !(locals[n] > const) jump. Pop 0, conditionally jump.
    FusedGetF32ConstFGtJumpIfZeroF(u16, f32, i32),

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
            StackOp::Call {
                func,
                args,
                preserve,
            } => {
                write!(f, "call func={} args={} preserve={}", func, args, preserve)
            }
            StackOp::CallIndirect { args } => write!(f, "call_indirect args={}", args),
            StackOp::CallClosure { args } => write!(f, "call_closure args={}", args),
            StackOp::Return => write!(f, "return"),
            StackOp::ReturnVoid => write!(f, "return_void"),
            StackOp::Drop => write!(f, "drop"),
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
            StackOp::IsnanF64 => write!(f, "f64.isnan"),
            StackOp::IsinfF64 => write!(f, "f64.isinf"),
            StackOp::Atan2F64 => write!(f, "f64.atan2"),
            StackOp::PrintI32 => write!(f, "debug.print_i32"),
            StackOp::Putc => write!(f, "debug.putc"),
            StackOp::Assert => write!(f, "debug.assert"),
            StackOp::GetClosurePtr => write!(f, "get_closure_ptr"),
            StackOp::FusedGetGetIAdd(a, b) => write!(f, "fused.get_get_iadd {} {}", a, b),
            StackOp::FusedGetGetILt(a, b) => write!(f, "fused.get_get_ilt {} {}", a, b),
            StackOp::FusedAddrLoad32Off(s, o) => write!(f, "fused.addr_load32off {} {}", s, o),
            StackOp::FusedGetAddImmSet(s, v, d) => {
                write!(f, "fused.get_addimm_set {} {} {}", s, v, d)
            }
            StackOp::FusedGetGetILtJumpIfZero(a, b, o) => {
                write!(f, "fused.get_get_ilt_jiz {} {} {}", a, b, o)
            }
            StackOp::FusedBoundsCheck1JumpIfZero(p, o) => {
                write!(f, "fused.bounds_check1_jiz {} {} {}", p[0], p[1], o)
            }
            StackOp::FusedBoundsCheck2JumpIfZero(p, o) => write!(
                f,
                "fused.bounds_check2_jiz {} {} {} {} {}",
                p[0], p[1], p[2], p[3], o
            ),
            StackOp::FusedBoundsCheck3JumpIfZero(p, o) => write!(
                f,
                "fused.bounds_check3_jiz {} {} {} {} {} {} {}",
                p[0], p[1], p[2], p[3], p[4], p[5], o
            ),
            StackOp::FusedBoundsCheck4JumpIfZero(p, o) => write!(
                f,
                "fused.bounds_check4_jiz {} {} {} {} {} {} {} {} {}",
                p[0], p[1], p[2], p[3], p[4], p[5], p[6], p[7], o
            ),
            StackOp::FusedBoundsCheck5JumpIfZero(p, o) => write!(
                f,
                "fused.bounds_check5_jiz {} {} {} {} {} {} {} {} {} {} {}",
                p[0], p[1], p[2], p[3], p[4], p[5], p[6], p[7], p[8], p[9], o
            ),
            StackOp::FusedBoundsCheck6JumpIfZero(p, o) => write!(
                f,
                "fused.bounds_check6_jiz {} {} {} {} {} {} {} {} {} {} {} {} {}",
                p[0], p[1], p[2], p[3], p[4], p[5], p[6], p[7], p[8], p[9], p[10], p[11], o
            ),
            StackOp::FusedBoundsCheck7JumpIfZero(p, o) => write!(
                f,
                "fused.bounds_check7_jiz {} {} {} {} {} {} {} {} {} {} {} {} {} {} {}",
                p[0],
                p[1],
                p[2],
                p[3],
                p[4],
                p[5],
                p[6],
                p[7],
                p[8],
                p[9],
                p[10],
                p[11],
                p[12],
                p[13],
                o
            ),
            StackOp::FusedBoundsCheck8JumpIfZero(p, o) => write!(
                f,
                "fused.bounds_check8_jiz {} {} {} {} {} {} {} {} {} {} {} {} {} {} {} {} {}",
                p[0],
                p[1],
                p[2],
                p[3],
                p[4],
                p[5],
                p[6],
                p[7],
                p[8],
                p[9],
                p[10],
                p[11],
                p[12],
                p[13],
                p[14],
                p[15],
                o
            ),
            StackOp::FusedConstSet(v, n) => write!(f, "fused.const_set {} {}", v, n),
            StackOp::FusedF32ConstSet(v, n) => write!(f, "fused.f32const_set {} {}", v, n),
            StackOp::FusedAddrGetSliceLoad32(s, i) => {
                write!(f, "fused.addr_get_sload32 {} {}", s, i)
            }
            StackOp::FusedAddrGetSliceStore32(s, i) => {
                write!(f, "fused.addr_get_sstore32 {} {}", s, i)
            }
            StackOp::FusedLocalArrayLoad32(s, i) => {
                write!(f, "fused.local_array_load32 {} {}", s, i)
            }
            StackOp::FusedLocalArrayStore32(s, i) => {
                write!(f, "fused.local_array_store32 {} {}", s, i)
            }
            StackOp::FusedGetSet(a, b) => write!(f, "fused.get_set {} {}", a, b),
            StackOp::FusedAddrLoad32OffSet(s, o, d) => {
                write!(f, "fused.addr_load32off_set {} {} {}", s, o, d)
            }
            StackOp::FusedAddrImmGetStore32(s, o, src) => {
                write!(f, "fused.addr_imm_get_store32 {} {} {}", s, o, src)
            }
            StackOp::FusedGetGetFAddSet(a, b, d) => {
                write!(f, "fused.get_get_fadd_set {} {} {}", a, b, d)
            }
            StackOp::FusedGetGetFSubSet(a, b, d) => {
                write!(f, "fused.get_get_fsub_set {} {} {}", a, b, d)
            }
            StackOp::FusedGetGetFMulSet(a, b, d) => {
                write!(f, "fused.get_get_fmul_set {} {} {}", a, b, d)
            }
            StackOp::FusedGetGetFDivSet(a, b, d) => {
                write!(f, "fused.get_get_fdiv_set {} {} {}", a, b, d)
            }
            StackOp::FusedGetGetIAddSet(a, b, d) => {
                write!(f, "fused.get_get_iadd_set {} {} {}", a, b, d)
            }
            StackOp::FusedGetGetISubSet(a, b, d) => {
                write!(f, "fused.get_get_isub_set {} {} {}", a, b, d)
            }
            StackOp::FusedGetGetIMulSet(a, b, d) => {
                write!(f, "fused.get_get_imul_set {} {} {}", a, b, d)
            }
            StackOp::FusedFieldCopy32(s, src, dst) => {
                write!(f, "fused.field_copy32 {} {} {}", s, src, dst)
            }
            StackOp::FusedTeeSliceStore32(n, s, idx) => {
                write!(f, "fused.tee_sstore32 {} {} {}", n, s, idx)
            }
            // === Float-window ops (Phase 1+) ===
            StackOp::F32ConstF(v) => write!(f, "fw.f32.const {}", v),
            StackOp::LocalGetF(n) => write!(f, "fw.local.get {}", n),
            StackOp::LocalSetF(n) => write!(f, "fw.local.set {}", n),
            StackOp::LocalTeeF(n) => write!(f, "fw.local.tee {}", n),
            StackOp::DropF => write!(f, "fw.drop"),
            StackOp::FAddF => write!(f, "fw.f32.add"),
            StackOp::FSubF => write!(f, "fw.f32.sub"),
            StackOp::FMulF => write!(f, "fw.f32.mul"),
            StackOp::FDivF => write!(f, "fw.f32.div"),
            StackOp::FPowF => write!(f, "fw.f32.pow"),
            StackOp::FNegF => write!(f, "fw.f32.neg"),
            StackOp::FEqF => write!(f, "fw.f32.eq"),
            StackOp::FNeF => write!(f, "fw.f32.ne"),
            StackOp::FLtF => write!(f, "fw.f32.lt"),
            StackOp::FLeF => write!(f, "fw.f32.le"),
            StackOp::FGtF => write!(f, "fw.f32.gt"),
            StackOp::FGeF => write!(f, "fw.f32.ge"),
            StackOp::F32ToI32F => write!(f, "fw.convert.f32_to_i32"),
            StackOp::I32ToF32F => write!(f, "fw.convert.i32_to_f32"),
            StackOp::FToBitsF => write!(f, "fw.to_bits"),
            StackOp::BitsToFF => write!(f, "fw.from_bits"),
            StackOp::SliceLoad32F => write!(f, "fw.slice.load32"),
            StackOp::SliceStore32F => write!(f, "fw.slice.store32"),
            StackOp::LoadF32F => write!(f, "fw.f32.load"),
            StackOp::LoadF32OffF(o) => write!(f, "fw.f32.load offset={}", o),
            StackOp::StoreF32F => write!(f, "fw.f32.store"),
            StackOp::StoreF32OffF(o) => write!(f, "fw.f32.store offset={}", o),
            StackOp::SinF32F => write!(f, "fw.f32.sin"),
            StackOp::CosF32F => write!(f, "fw.f32.cos"),
            StackOp::TanF32F => write!(f, "fw.f32.tan"),
            StackOp::AsinF32F => write!(f, "fw.f32.asin"),
            StackOp::AcosF32F => write!(f, "fw.f32.acos"),
            StackOp::AtanF32F => write!(f, "fw.f32.atan"),
            StackOp::SinhF32F => write!(f, "fw.f32.sinh"),
            StackOp::CoshF32F => write!(f, "fw.f32.cosh"),
            StackOp::TanhF32F => write!(f, "fw.f32.tanh"),
            StackOp::AsinhF32F => write!(f, "fw.f32.asinh"),
            StackOp::AcoshF32F => write!(f, "fw.f32.acosh"),
            StackOp::AtanhF32F => write!(f, "fw.f32.atanh"),
            StackOp::LnF32F => write!(f, "fw.f32.ln"),
            StackOp::ExpF32F => write!(f, "fw.f32.exp"),
            StackOp::Exp2F32F => write!(f, "fw.f32.exp2"),
            StackOp::Log10F32F => write!(f, "fw.f32.log10"),
            StackOp::Log2F32F => write!(f, "fw.f32.log2"),
            StackOp::SqrtF32F => write!(f, "fw.f32.sqrt"),
            StackOp::AbsF32F => write!(f, "fw.f32.abs"),
            StackOp::FloorF32F => write!(f, "fw.f32.floor"),
            StackOp::CeilF32F => write!(f, "fw.f32.ceil"),
            StackOp::Atan2F32F => write!(f, "fw.f32.atan2"),
            StackOp::IsnanF32F => write!(f, "fw.f32.isnan"),
            StackOp::IsinfF32F => write!(f, "fw.f32.isinf"),
            StackOp::PrintF32F => write!(f, "fw.debug.print_f32"),
            StackOp::FusedGetGetFAddF(a, b) => write!(f, "fw.fused.get_get_fadd {} {}", a, b),
            StackOp::FusedGetGetFSubF(a, b) => write!(f, "fw.fused.get_get_fsub {} {}", a, b),
            StackOp::FusedGetGetFMulF(a, b) => write!(f, "fw.fused.get_get_fmul {} {}", a, b),
            StackOp::FusedGetFMulF(a) => write!(f, "fw.fused.get_fmul {}", a),
            StackOp::FusedGetFAddF(a) => write!(f, "fw.fused.get_fadd {}", a),
            StackOp::FusedGetFSubF(a) => write!(f, "fw.fused.get_fsub {}", a),
            StackOp::FusedFMulFAddF => write!(f, "fw.fused.fmul_fadd"),
            StackOp::FusedFMulFSubF => write!(f, "fw.fused.fmul_fsub"),
            StackOp::FusedGetGetFMulFAddF(a, b) => {
                write!(f, "fw.fused.get_get_fmul_fadd {} {}", a, b)
            }
            StackOp::FusedGetGetFMulFSubF(a, b) => {
                write!(f, "fw.fused.get_get_fmul_fsub {} {}", a, b)
            }
            StackOp::FusedGetGetFMulSum2F(p, mask) => write!(
                f,
                "fw.fused.get_get_fmul_sum2 {} {} {} {} {}",
                p[0], p[1], p[2], p[3], mask
            ),
            StackOp::FusedGetGetFMulSum3F(p, mask) => write!(
                f,
                "fw.fused.get_get_fmul_sum3 {} {} {} {} {} {} {}",
                p[0], p[1], p[2], p[3], p[4], p[5], mask
            ),
            StackOp::FusedGetGetFMulSum4F(p, mask) => write!(
                f,
                "fw.fused.get_get_fmul_sum4 {} {} {} {} {} {} {} {} {}",
                p[0], p[1], p[2], p[3], p[4], p[5], p[6], p[7], mask
            ),
            StackOp::FusedGetGetFMulSum5F(p, mask) => write!(
                f,
                "fw.fused.get_get_fmul_sum5 {} {} {} {} {} {} {} {} {} {} {}",
                p[0], p[1], p[2], p[3], p[4], p[5], p[6], p[7], p[8], p[9], mask
            ),
            StackOp::FusedGetGetFMulSum6F(p, mask) => write!(
                f,
                "fw.fused.get_get_fmul_sum6 {} {} {} {} {} {} {} {} {} {} {} {} {}",
                p[0], p[1], p[2], p[3], p[4], p[5], p[6], p[7], p[8], p[9], p[10], p[11], mask
            ),
            StackOp::FusedGetGetFMulSum7F(p, mask) => write!(
                f,
                "fw.fused.get_get_fmul_sum7 {} {} {} {} {} {} {} {} {} {} {} {} {} {} {}",
                p[0],
                p[1],
                p[2],
                p[3],
                p[4],
                p[5],
                p[6],
                p[7],
                p[8],
                p[9],
                p[10],
                p[11],
                p[12],
                p[13],
                mask
            ),
            StackOp::FusedGetGetFMulSum8F(p, mask) => write!(
                f,
                "fw.fused.get_get_fmul_sum8 {} {} {} {} {} {} {} {} {} {} {} {} {} {} {} {} {}",
                p[0],
                p[1],
                p[2],
                p[3],
                p[4],
                p[5],
                p[6],
                p[7],
                p[8],
                p[9],
                p[10],
                p[11],
                p[12],
                p[13],
                p[14],
                p[15],
                mask
            ),
            StackOp::FusedGetAddrFMulFAddF(a, s, o) => {
                write!(f, "fw.fused.get_addr_fmul_fadd {} {} {}", a, s, o)
            }
            StackOp::FusedGetAddrFMulFSubF(a, s, o) => {
                write!(f, "fw.fused.get_addr_fmul_fsub {} {} {}", a, s, o)
            }
            StackOp::FusedAddrLoad32OffF(s, o) => write!(f, "fw.fused.addr_load32off {} {}", s, o),
            StackOp::FusedAddrGetSliceLoad32F(s, i) => {
                write!(f, "fw.fused.addr_get_sload32 {} {}", s, i)
            }
            StackOp::FusedAddrGetSliceStore32F(s, i) => {
                write!(f, "fw.fused.addr_get_sstore32 {} {}", s, i)
            }
            StackOp::FusedTeeSliceStore32F(n, s, i) => {
                write!(f, "fw.fused.tee_sstore32 {} {} {}", n, s, i)
            }
            StackOp::FusedLocalArrayLoad32F(s, i) => {
                write!(f, "fw.fused.local_array_load32 {} {}", s, i)
            }
            StackOp::FusedLocalArrayStore32F(s, i) => {
                write!(f, "fw.fused.local_array_store32 {} {}", s, i)
            }
            StackOp::FusedGetSetF(src, dst) => write!(f, "fw.fused.get_set {} {}", src, dst),
            StackOp::FusedGetSet2F(p) => {
                write!(f, "fw.fused.get_set2 {} {} {} {}", p[0], p[1], p[2], p[3])
            }
            StackOp::FusedGetSet3F(p) => write!(
                f,
                "fw.fused.get_set3 {} {} {} {} {} {}",
                p[0], p[1], p[2], p[3], p[4], p[5]
            ),
            StackOp::FusedGetSet4F(p) => write!(
                f,
                "fw.fused.get_set4 {} {} {} {} {} {} {} {}",
                p[0], p[1], p[2], p[3], p[4], p[5], p[6], p[7]
            ),
            StackOp::FusedGetSet5F(p) => write!(
                f,
                "fw.fused.get_set5 {} {} {} {} {} {} {} {} {} {}",
                p[0], p[1], p[2], p[3], p[4], p[5], p[6], p[7], p[8], p[9]
            ),
            StackOp::FusedGetSet6F(p) => write!(
                f,
                "fw.fused.get_set6 {} {} {} {} {} {} {} {} {} {} {} {}",
                p[0], p[1], p[2], p[3], p[4], p[5], p[6], p[7], p[8], p[9], p[10], p[11]
            ),
            StackOp::FusedGetSet7F(p) => write!(
                f,
                "fw.fused.get_set7 {} {} {} {} {} {} {} {} {} {} {} {} {} {}",
                p[0],
                p[1],
                p[2],
                p[3],
                p[4],
                p[5],
                p[6],
                p[7],
                p[8],
                p[9],
                p[10],
                p[11],
                p[12],
                p[13]
            ),
            StackOp::FusedGetSet8F(p) => write!(
                f,
                "fw.fused.get_set8 {} {} {} {} {} {} {} {} {} {} {} {} {} {} {} {}",
                p[0],
                p[1],
                p[2],
                p[3],
                p[4],
                p[5],
                p[6],
                p[7],
                p[8],
                p[9],
                p[10],
                p[11],
                p[12],
                p[13],
                p[14],
                p[15]
            ),
            StackOp::FusedF32ConstFGtJumpIfZeroF(v, o) => {
                write!(f, "fw.fused.f32const_fgt_jiz {} {}", v, o)
            }
            StackOp::FusedGetF32ConstFGtJumpIfZeroF(n, v, o) => {
                write!(f, "fw.fused.get_f32const_fgt_jiz {} {} {}", n, v, o)
            }
            StackOp::Halt => write!(f, "halt"),
            StackOp::Nop => write!(f, "nop"),
        }
    }
}
