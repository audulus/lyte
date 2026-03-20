//! VM instruction set.
//!
//! Defines the Opcode enum and methods for inspecting/rewriting instructions.

/// Register index.
/// Virtual registers during codegen/optimization may exceed 255.
/// Physical registers (after allocation) are always 0-255.
pub type Reg = u16;

/// Instruction pointer offset for jumps
pub type Offset = i32;

/// Function index in the function table
pub type FuncIdx = u32;

/// Constant pool index
pub type ConstIdx = u32;

/// VM instructions for a register-based machine.
///
/// All values are stored as 64-bit in registers.
/// Memory operations handle different sizes based on type info
/// stored separately (not in the instruction stream).
#[derive(Clone, Copy, Debug, PartialEq)]
#[repr(u8)]
pub enum Opcode {
    /// No operation
    Nop,

    /// Halt execution
    Halt,

    // ============ Register Operations ============
    /// Move register to register: dst = src
    Move {
        dst: Reg,
        src: Reg,
    },

    /// Load immediate i64 into register
    LoadImm {
        dst: Reg,
        value: i64,
    },

    /// Load f32 immediate (stored as bits in i64)
    LoadF32 {
        dst: Reg,
        value: f32,
    },

    /// Load f64 immediate
    LoadF64 {
        dst: Reg,
        value: f64,
    },

    /// Load from constant pool
    LoadConst {
        dst: Reg,
        idx: ConstIdx,
    },

    // ============ Integer Arithmetic ============
    /// Integer add: dst = a + b
    IAdd {
        dst: Reg,
        a: Reg,
        b: Reg,
    },

    /// Integer subtract: dst = a - b
    ISub {
        dst: Reg,
        a: Reg,
        b: Reg,
    },

    /// Integer multiply: dst = a * b
    IMul {
        dst: Reg,
        a: Reg,
        b: Reg,
    },

    /// Signed integer divide: dst = a / b
    IDiv {
        dst: Reg,
        a: Reg,
        b: Reg,
    },

    /// Unsigned integer divide: dst = a / b
    UDiv {
        dst: Reg,
        a: Reg,
        b: Reg,
    },

    /// Integer remainder: dst = a % b
    IRem {
        dst: Reg,
        a: Reg,
        b: Reg,
    },

    /// Integer power: dst = a ^ b
    IPow {
        dst: Reg,
        a: Reg,
        b: Reg,
    },

    /// Integer negate: dst = -src
    INeg {
        dst: Reg,
        src: Reg,
    },

    /// Integer add immediate: dst = src + imm
    IAddImm {
        dst: Reg,
        src: Reg,
        imm: i32,
    },

    // ============ Floating Point Arithmetic ============
    /// Float32 add: dst = a + b
    FAdd {
        dst: Reg,
        a: Reg,
        b: Reg,
    },

    /// Float32 subtract: dst = a - b
    FSub {
        dst: Reg,
        a: Reg,
        b: Reg,
    },

    /// Float32 multiply: dst = a * b
    FMul {
        dst: Reg,
        a: Reg,
        b: Reg,
    },

    /// Float32 divide: dst = a / b
    FDiv {
        dst: Reg,
        a: Reg,
        b: Reg,
    },

    /// Float32 negate: dst = -src
    FNeg {
        dst: Reg,
        src: Reg,
    },

    /// Float32 power: dst = a ^ b
    FPow {
        dst: Reg,
        a: Reg,
        b: Reg,
    },

    /// Float32 fused multiply-add: dst = a * b + c
    FMulAdd {
        dst: Reg,
        a: Reg,
        b: Reg,
        c: Reg,
    },

    /// Float32 fused multiply-subtract: dst = a * b - c
    FMulSub {
        dst: Reg,
        a: Reg,
        b: Reg,
        c: Reg,
    },

    // ============ Float64 Arithmetic ============
    /// Float64 add: dst = a + b
    DAdd {
        dst: Reg,
        a: Reg,
        b: Reg,
    },

    /// Float64 subtract: dst = a - b
    DSub {
        dst: Reg,
        a: Reg,
        b: Reg,
    },

    /// Float64 multiply: dst = a * b
    DMul {
        dst: Reg,
        a: Reg,
        b: Reg,
    },

    /// Float64 divide: dst = a / b
    DDiv {
        dst: Reg,
        a: Reg,
        b: Reg,
    },

    /// Float64 negate: dst = -src
    DNeg {
        dst: Reg,
        src: Reg,
    },

    /// Float64 power: dst = a ^ b
    DPow {
        dst: Reg,
        a: Reg,
        b: Reg,
    },

    /// Float64 fused multiply-add: dst = a * b + c
    DMulAdd {
        dst: Reg,
        a: Reg,
        b: Reg,
        c: Reg,
    },

    /// Float64 fused multiply-subtract: dst = a * b - c
    DMulSub {
        dst: Reg,
        a: Reg,
        b: Reg,
        c: Reg,
    },

    // ============ Bitwise Operations ============
    /// Bitwise AND: dst = a & b
    And {
        dst: Reg,
        a: Reg,
        b: Reg,
    },

    /// Bitwise OR: dst = a | b
    Or {
        dst: Reg,
        a: Reg,
        b: Reg,
    },

    /// Bitwise XOR: dst = a ^ b
    Xor {
        dst: Reg,
        a: Reg,
        b: Reg,
    },

    /// Bitwise NOT: dst = !src
    Not {
        dst: Reg,
        src: Reg,
    },

    /// Shift left: dst = a << b
    Shl {
        dst: Reg,
        a: Reg,
        b: Reg,
    },

    /// Arithmetic shift right: dst = a >> b (signed)
    Shr {
        dst: Reg,
        a: Reg,
        b: Reg,
    },

    /// Logical shift right: dst = a >>> b (unsigned)
    UShr {
        dst: Reg,
        a: Reg,
        b: Reg,
    },

    // ============ Comparisons (result is 0 or 1) ============
    /// Integer equal: dst = (a == b)
    IEq {
        dst: Reg,
        a: Reg,
        b: Reg,
    },

    /// Integer not equal: dst = (a != b)
    INe {
        dst: Reg,
        a: Reg,
        b: Reg,
    },

    /// Signed less than: dst = (a < b)
    ILt {
        dst: Reg,
        a: Reg,
        b: Reg,
    },

    /// Signed less or equal: dst = (a <= b)
    ILe {
        dst: Reg,
        a: Reg,
        b: Reg,
    },

    /// Unsigned less than
    ULt {
        dst: Reg,
        a: Reg,
        b: Reg,
    },

    /// Float32 equal
    FEq {
        dst: Reg,
        a: Reg,
        b: Reg,
    },

    /// Float32 not equal
    FNe {
        dst: Reg,
        a: Reg,
        b: Reg,
    },

    /// Float32 less than
    FLt {
        dst: Reg,
        a: Reg,
        b: Reg,
    },

    /// Float32 less or equal
    FLe {
        dst: Reg,
        a: Reg,
        b: Reg,
    },

    /// Memory equal: compare `size` bytes at pointers in a and b
    MemEq {
        dst: Reg,
        a: Reg,
        b: Reg,
        size: u32,
    },

    /// Memory not equal: compare `size` bytes at pointers in a and b
    MemNe {
        dst: Reg,
        a: Reg,
        b: Reg,
        size: u32,
    },

    /// Slice equal: compare two fat pointers (ptr+len) by contents.
    /// `elem_size` is the size of each element in bytes.
    SliceEq {
        dst: Reg,
        a: Reg,
        b: Reg,
        elem_size: u32,
    },

    /// Slice not equal: compare two fat pointers (ptr+len) by contents.
    SliceNe {
        dst: Reg,
        a: Reg,
        b: Reg,
        elem_size: u32,
    },

    /// Float64 equal
    DEq {
        dst: Reg,
        a: Reg,
        b: Reg,
    },

    /// Float64 less than
    DLt {
        dst: Reg,
        a: Reg,
        b: Reg,
    },

    /// Float64 less or equal
    DLe {
        dst: Reg,
        a: Reg,
        b: Reg,
    },

    // ============ Type Conversions ============
    /// Convert i32 to f32
    I32ToF32 {
        dst: Reg,
        src: Reg,
    },

    /// Convert f32 to i32
    F32ToI32 {
        dst: Reg,
        src: Reg,
    },

    /// Convert i32 to f64
    I32ToF64 {
        dst: Reg,
        src: Reg,
    },

    /// Convert f64 to i32
    F64ToI32 {
        dst: Reg,
        src: Reg,
    },

    /// Convert f32 to f64
    F32ToF64 {
        dst: Reg,
        src: Reg,
    },

    /// Convert f64 to f32
    F64ToF32 {
        dst: Reg,
        src: Reg,
    },

    /// Truncate i32 to i8 (sign-extend back to i64 register)
    I32ToI8 {
        dst: Reg,
        src: Reg,
    },

    /// Sign-extend i8 to i32
    I8ToI32 {
        dst: Reg,
        src: Reg,
    },

    /// Mask to u32 (zero-extend)
    I64ToU32 {
        dst: Reg,
        src: Reg,
    },

    // ============ Memory Operations ============
    /// Load 8-bit value from memory: dst = mem[addr]
    Load8 {
        dst: Reg,
        addr: Reg,
    },

    /// Load 32-bit value from memory
    Load32 {
        dst: Reg,
        addr: Reg,
    },

    /// Load 64-bit value from memory
    Load64 {
        dst: Reg,
        addr: Reg,
    },

    /// Load with offset: dst = mem[base + offset]
    Load32Off {
        dst: Reg,
        base: Reg,
        offset: i32,
    },

    /// Load 64-bit with offset
    Load64Off {
        dst: Reg,
        base: Reg,
        offset: i32,
    },

    /// Store 8-bit value to memory: mem[addr] = src
    Store8 {
        addr: Reg,
        src: Reg,
    },

    /// Store 32-bit value to memory
    Store32 {
        addr: Reg,
        src: Reg,
    },

    /// Store 64-bit value to memory
    Store64 {
        addr: Reg,
        src: Reg,
    },

    /// Store 8-bit with offset: mem[base + offset] = src
    Store8Off {
        base: Reg,
        offset: i32,
        src: Reg,
    },

    /// Store with offset: mem[base + offset] = src
    Store32Off {
        base: Reg,
        offset: i32,
        src: Reg,
    },

    /// Store 64-bit with offset
    Store64Off {
        base: Reg,
        offset: i32,
        src: Reg,
    },

    /// Get address of local variable slot
    LocalAddr {
        dst: Reg,
        slot: u16,
    },

    /// Get address of global variable at offset
    GlobalAddr {
        dst: Reg,
        offset: i32,
    },

    // ============ Control Flow ============
    /// Unconditional jump
    Jump {
        offset: Offset,
    },

    /// Jump if register is zero (false)
    JumpIfZero {
        cond: Reg,
        offset: Offset,
    },

    /// Jump if register is non-zero (true)
    JumpIfNotZero {
        cond: Reg,
        offset: Offset,
    },

    // ============ Superinstructions: Compare and Branch ============
    /// Jump if a < b (signed): if !(a < b) jump
    ILtJump {
        a: Reg,
        b: Reg,
        offset: Offset,
    },

    /// Jump if a < b (f32): if !(a < b) jump
    FLtJump {
        a: Reg,
        b: Reg,
        offset: Offset,
    },

    // ============ Superinstructions: Fused Local Slot Access ============
    /// Fused LocalAddr + Load32: load 32-bit value from local slot into register
    LoadSlot32 {
        dst: Reg,
        slot: u16,
    },

    /// Fused LocalAddr + Store32: store 32-bit value from register into local slot
    StoreSlot32 {
        slot: u16,
        src: Reg,
    },

    /// Call function by index, args in registers starting at `args_start`
    /// Result (if any) goes in register 0
    Call {
        func: FuncIdx,
        args_start: Reg,
        arg_count: u8,
    },

    /// Call function pointer in register
    CallIndirect {
        func_reg: Reg,
        args_start: Reg,
        arg_count: u8,
    },

    /// Call through a fat pointer {func_idx, closure_ptr} stored at the address in fat_ptr.
    /// Sets VM.closure_ptr before entering the callee.
    CallClosure {
        fat_ptr: Reg,
        args_start: Reg,
        arg_count: u8,
    },

    /// Load the current closure pointer into dst (set by CallClosure).
    GetClosurePtr {
        dst: Reg,
    },

    /// Return from function (return value in register 0)
    Return,

    /// Return with specific register
    ReturnReg {
        src: Reg,
    },

    // ============ Stack Frame Operations ============
    /// Allocate stack space for locals (in bytes)
    AllocLocals {
        size: u32,
    },

    /// Copy bytes from src to dst
    MemCopy {
        dst: Reg,
        src: Reg,
        size: u32,
    },

    /// Zero memory region
    MemZero {
        dst: Reg,
        size: u32,
    },

    // ============ Register Save/Restore ============
    /// Save registers [start_reg..start_reg+count] to locals at slot offset.
    /// Each register is 8 bytes.
    SaveRegs {
        start_reg: Reg,
        count: u8,
        slot: u32,
    },

    /// Restore registers [start_reg..start_reg+count] from locals at slot offset.
    RestoreRegs {
        start_reg: Reg,
        count: u8,
        slot: u32,
    },

    // ============ Debugging ============
    /// Print integer value (for debugging)
    PrintI32 {
        src: Reg,
    },

    /// Print float value
    PrintF32 {
        src: Reg,
    },

    /// Assert value is non-zero
    Assert {
        src: Reg,
    },

    /// Print a character
    Putc {
        src: Reg,
    },

    // ============ Math Builtins ============
    /// Unary f32 math: dst = op(src)
    SinF32 {
        dst: Reg,
        src: Reg,
    },
    CosF32 {
        dst: Reg,
        src: Reg,
    },
    TanF32 {
        dst: Reg,
        src: Reg,
    },
    AsinF32 {
        dst: Reg,
        src: Reg,
    },
    AcosF32 {
        dst: Reg,
        src: Reg,
    },
    AtanF32 {
        dst: Reg,
        src: Reg,
    },
    SinhF32 {
        dst: Reg,
        src: Reg,
    },
    CoshF32 {
        dst: Reg,
        src: Reg,
    },
    TanhF32 {
        dst: Reg,
        src: Reg,
    },
    AsinhF32 {
        dst: Reg,
        src: Reg,
    },
    AcoshF32 {
        dst: Reg,
        src: Reg,
    },
    AtanhF32 {
        dst: Reg,
        src: Reg,
    },
    LnF32 {
        dst: Reg,
        src: Reg,
    },
    ExpF32 {
        dst: Reg,
        src: Reg,
    },
    Exp2F32 {
        dst: Reg,
        src: Reg,
    },
    Log10F32 {
        dst: Reg,
        src: Reg,
    },
    Log2F32 {
        dst: Reg,
        src: Reg,
    },
    SqrtF32 {
        dst: Reg,
        src: Reg,
    },
    AbsF32 {
        dst: Reg,
        src: Reg,
    },
    FloorF32 {
        dst: Reg,
        src: Reg,
    },
    CeilF32 {
        dst: Reg,
        src: Reg,
    },

    /// Unary f64 math: dst = op(src)
    SinF64 {
        dst: Reg,
        src: Reg,
    },
    CosF64 {
        dst: Reg,
        src: Reg,
    },
    TanF64 {
        dst: Reg,
        src: Reg,
    },
    AsinF64 {
        dst: Reg,
        src: Reg,
    },
    AcosF64 {
        dst: Reg,
        src: Reg,
    },
    AtanF64 {
        dst: Reg,
        src: Reg,
    },
    SinhF64 {
        dst: Reg,
        src: Reg,
    },
    CoshF64 {
        dst: Reg,
        src: Reg,
    },
    TanhF64 {
        dst: Reg,
        src: Reg,
    },
    AsinhF64 {
        dst: Reg,
        src: Reg,
    },
    AcoshF64 {
        dst: Reg,
        src: Reg,
    },
    AtanhF64 {
        dst: Reg,
        src: Reg,
    },
    LnF64 {
        dst: Reg,
        src: Reg,
    },
    ExpF64 {
        dst: Reg,
        src: Reg,
    },
    Exp2F64 {
        dst: Reg,
        src: Reg,
    },
    Log10F64 {
        dst: Reg,
        src: Reg,
    },
    Log2F64 {
        dst: Reg,
        src: Reg,
    },
    SqrtF64 {
        dst: Reg,
        src: Reg,
    },
    AbsF64 {
        dst: Reg,
        src: Reg,
    },
    FloorF64 {
        dst: Reg,
        src: Reg,
    },
    CeilF64 {
        dst: Reg,
        src: Reg,
    },

    /// Predicate math (return i32): dst = op(src)
    IsinfF32 {
        dst: Reg,
        src: Reg,
    },
    IsinfF64 {
        dst: Reg,
        src: Reg,
    },
    IsnanF32 {
        dst: Reg,
        src: Reg,
    },
    IsnanF64 {
        dst: Reg,
        src: Reg,
    },

    /// Binary f32 math: dst = op(a, b)
    PowF32 {
        dst: Reg,
        a: Reg,
        b: Reg,
    },
    Atan2F32 {
        dst: Reg,
        a: Reg,
        b: Reg,
    },
    MinF32 {
        dst: Reg,
        a: Reg,
        b: Reg,
    },
    MaxF32 {
        dst: Reg,
        a: Reg,
        b: Reg,
    },

    /// Binary f64 math: dst = op(a, b)
    PowF64 {
        dst: Reg,
        a: Reg,
        b: Reg,
    },
    Atan2F64 {
        dst: Reg,
        a: Reg,
        b: Reg,
    },
    MinF64 {
        dst: Reg,
        a: Reg,
        b: Reg,
    },
    MaxF64 {
        dst: Reg,
        a: Reg,
        b: Reg,
    },
}

impl Opcode {
    pub fn get_dst(&self) -> Option<Reg> {
        match self {
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
            | Opcode::SliceEq { dst, .. }
            | Opcode::SliceNe { dst, .. }
            | Opcode::DEq { dst, .. }
            | Opcode::DLt { dst, .. }
            | Opcode::DLe { dst, .. }
            | Opcode::I32ToF32 { dst, .. }
            | Opcode::F32ToI32 { dst, .. }
            | Opcode::I32ToF64 { dst, .. }
            | Opcode::F64ToI32 { dst, .. }
            | Opcode::F32ToF64 { dst, .. }
            | Opcode::F64ToF32 { dst, .. }
            | Opcode::I32ToI8 { dst, .. }
            | Opcode::I8ToI32 { dst, .. }
            | Opcode::I64ToU32 { dst, .. }
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

            // Call/CallIndirect/CallClosure implicitly define r0 (return value register).
            Opcode::Call { .. } | Opcode::CallIndirect { .. } | Opcode::CallClosure { .. } => Some(0),

            Opcode::GetClosurePtr { dst } => Some(*dst),

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


    pub fn set_dst(&mut self, new_dst: Reg) -> bool {
        match self {
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
            | Opcode::SliceEq { dst, .. }
            | Opcode::SliceNe { dst, .. }
            | Opcode::DEq { dst, .. }
            | Opcode::DLt { dst, .. }
            | Opcode::DLe { dst, .. }
            | Opcode::I32ToF32 { dst, .. }
            | Opcode::F32ToI32 { dst, .. }
            | Opcode::I32ToF64 { dst, .. }
            | Opcode::F64ToI32 { dst, .. }
            | Opcode::F32ToF64 { dst, .. }
            | Opcode::F64ToF32 { dst, .. }
            | Opcode::I32ToI8 { dst, .. }
            | Opcode::I8ToI32 { dst, .. }
            | Opcode::I64ToU32 { dst, .. }
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
            | Opcode::LoadSlot32 { dst, .. }
            | Opcode::GetClosurePtr { dst } => {
                *dst = new_dst;
                true
            }
            _ => false,
        }
    }


    pub fn reads_reg(&self, reg: Reg) -> bool {
        match self {
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
            Opcode::MemEq { a, b, .. }
            | Opcode::MemNe { a, b, .. }
            | Opcode::SliceEq { a, b, .. }
            | Opcode::SliceNe { a, b, .. } => *a == reg || *b == reg,
            Opcode::DEq { a, b, .. } | Opcode::DLt { a, b, .. } | Opcode::DLe { a, b, .. } => {
                *a == reg || *b == reg
            }

            Opcode::I32ToF32 { src, .. }
            | Opcode::F32ToI32 { src, .. }
            | Opcode::I32ToF64 { src, .. }
            | Opcode::F64ToI32 { src, .. }
            | Opcode::F32ToF64 { src, .. }
            | Opcode::F64ToF32 { src, .. }
            | Opcode::I32ToI8 { src, .. }
            | Opcode::I8ToI32 { src, .. }
            | Opcode::I64ToU32 { src, .. } => *src == reg,

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
            } => reg >= *args_start && reg < *args_start + *arg_count as Reg,
            Opcode::CallIndirect {
                func_reg,
                args_start,
                arg_count,
            } => *func_reg == reg || (reg >= *args_start && reg < *args_start + *arg_count as Reg),
            Opcode::CallClosure {
                fat_ptr,
                args_start,
                arg_count,
            } => *fat_ptr == reg || (reg >= *args_start && reg < *args_start + *arg_count as Reg),

            Opcode::GetClosurePtr { .. } => false,

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


    pub fn replace_src_reg(&mut self, old: Reg, new: Reg) {
        match self {
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
            Opcode::MemEq { a, b, .. }
            | Opcode::MemNe { a, b, .. }
            | Opcode::SliceEq { a, b, .. }
            | Opcode::SliceNe { a, b, .. } => {
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
            | Opcode::F64ToF32 { src, .. }
            | Opcode::I32ToI8 { src, .. }
            | Opcode::I8ToI32 { src, .. }
            | Opcode::I64ToU32 { src, .. } => {
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
                for r in *args_start..(*args_start + *arg_count as Reg) {
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
                for r in *args_start..(*args_start + *arg_count as Reg) {
                    if r == old && r == *args_start {
                        *args_start = new;
                        break;
                    }
                }
            }
            Opcode::CallClosure {
                fat_ptr,
                args_start,
                arg_count,
            } => {
                if *fat_ptr == old {
                    *fat_ptr = new;
                }
                for r in *args_start..(*args_start + *arg_count as Reg) {
                    if r == old && r == *args_start {
                        *args_start = new;
                        break;
                    }
                }
            }
            _ => {}
        }
    }


    pub fn for_each_src(&self, mut f: impl FnMut(Reg)) {
        match self {
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
            Opcode::MemEq { a, b, .. }
            | Opcode::MemNe { a, b, .. }
            | Opcode::SliceEq { a, b, .. }
            | Opcode::SliceNe { a, b, .. } => {
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
            | Opcode::F64ToF32 { src, .. }
            | Opcode::I32ToI8 { src, .. }
            | Opcode::I8ToI32 { src, .. }
            | Opcode::I64ToU32 { src, .. } => f(*src),
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
                for r in *args_start..(*args_start + *arg_count as Reg) {
                    f(r);
                }
            }
            Opcode::CallIndirect {
                func_reg,
                args_start,
                arg_count,
            } => {
                f(*func_reg);
                for r in *args_start..(*args_start + *arg_count as Reg) {
                    f(r);
                }
            }
            Opcode::CallClosure {
                fat_ptr,
                args_start,
                arg_count,
            } => {
                f(*fat_ptr);
                for r in *args_start..(*args_start + *arg_count as Reg) {
                    f(r);
                }
            }
            Opcode::GetClosurePtr { .. } => {}
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


    pub fn rewrite_regs(&mut self, map: &[Reg]) {
        match self {
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
            Opcode::MemEq { dst, a, b, .. }
            | Opcode::MemNe { dst, a, b, .. }
            | Opcode::SliceEq { dst, a, b, .. }
            | Opcode::SliceNe { dst, a, b, .. } => {
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
            | Opcode::F64ToF32 { dst, src }
            | Opcode::I32ToI8 { dst, src }
            | Opcode::I8ToI32 { dst, src }
            | Opcode::I64ToU32 { dst, src } => {
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
            Opcode::Call {
                args_start,
                arg_count,
                ..
            } => {
                if *arg_count > 0 {
                    *args_start = map[*args_start as usize];
                }
            }
            Opcode::CallIndirect {
                func_reg,
                args_start,
                arg_count,
                ..
            } => {
                *func_reg = map[*func_reg as usize];
                if *arg_count > 0 {
                    *args_start = map[*args_start as usize];
                }
            }
            Opcode::CallClosure {
                fat_ptr,
                args_start,
                arg_count,
                ..
            } => {
                *fat_ptr = map[*fat_ptr as usize];
                if *arg_count > 0 {
                    *args_start = map[*args_start as usize];
                }
            }
            Opcode::GetClosurePtr { dst } => {
                *dst = map[*dst as usize];
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


}
