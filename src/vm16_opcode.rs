//! 16-bit VM instruction set.
//!
//! Encoding: `[opcode:8][rA:4][rB:4]` = 16 bits.
//! - 256 opcodes, 16 registers (r0-r15).
//! - Arithmetic is destructive: `FAdd rA, rB` means `rA += rB`.
//! - Some instructions consume a trailing u16 data word.

/// 16-bit register index (0-15).
pub type Reg16 = u8;

/// A packed 16-bit instruction word.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct PackedOp16(pub u16);

impl PackedOp16 {
    /// Create from opcode and two 4-bit register operands.
    #[inline]
    pub fn ab(opcode: u8, ra: Reg16, rb: Reg16) -> Self {
        debug_assert!(ra < 16 && rb < 16);
        Self(((opcode as u16) << 8) | ((ra as u16 & 0xF) << 4) | (rb as u16 & 0xF))
    }

    /// Create from opcode with no register operands.
    #[inline]
    pub fn op(opcode: u8) -> Self {
        Self((opcode as u16) << 8)
    }

    /// Create from opcode with single register operand in A position.
    #[inline]
    pub fn a_only(opcode: u8, ra: Reg16) -> Self {
        debug_assert!(ra < 16);
        Self(((opcode as u16) << 8) | ((ra as u16 & 0xF) << 4))
    }

    /// Extract the 8-bit opcode.
    #[inline]
    pub fn opcode(self) -> u8 {
        (self.0 >> 8) as u8
    }

    /// Extract the 4-bit A register.
    #[inline]
    pub fn ra(self) -> Reg16 {
        ((self.0 >> 4) & 0xF) as Reg16
    }

    /// Extract the 4-bit B register.
    #[inline]
    pub fn rb(self) -> Reg16 {
        (self.0 & 0xF) as Reg16
    }
}

/// Tag constants for the 16-bit instruction set.
///
/// Instructions marked `+trail` consume a following u16 data word.
pub mod tags {
    // === Control (0x00-0x07) ===
    pub const NOP: u8 = 0x00;
    pub const HALT: u8 = 0x01;
    pub const RETURN: u8 = 0x02;        // return r0 implicitly
    pub const RETURN_REG: u8 = 0x03;    // A=src; move rA to r0 then return
    pub const JUMP: u8 = 0x04;          // +trail: i16 offset
    pub const JUMP_IF_ZERO: u8 = 0x05;  // A=cond, +trail: i16 offset
    pub const JUMP_IF_NOT_ZERO: u8 = 0x06; // A=cond, +trail: i16 offset
    // 0x07 reserved

    // === Moves & Loads (0x08-0x10) ===
    pub const MOVE: u8 = 0x08;          // A=dst, B=src
    pub const LOAD_IMM: u8 = 0x09;      // A=dst, +trail: i16 immediate
    pub const LOAD_IMM_WIDE: u8 = 0x0A; // A=dst, +trail: wide_i64 pool index
    pub const LOAD_F32: u8 = 0x0B;      // A=dst, +trail: f32_pool index
    pub const LOAD_F64: u8 = 0x0C;      // A=dst, +trail: wide_f64 pool index
    pub const LOAD_CONST: u8 = 0x0D;    // A=dst, +trail: const pool index
    pub const LOCAL_ADDR: u8 = 0x0E;    // A=dst, +trail: slot index (u16)
    pub const GLOBAL_ADDR: u8 = 0x0F;   // A=dst, +trail: byte offset (u16)
    pub const GET_CLOSURE_PTR: u8 = 0x10; // A=dst

    // === Integer Arithmetic (0x11-0x19) — destructive: rA op= rB ===
    pub const IADD: u8 = 0x11;
    pub const ISUB: u8 = 0x12;
    pub const IMUL: u8 = 0x13;
    pub const IDIV: u8 = 0x14;
    pub const UDIV: u8 = 0x15;
    pub const IREM: u8 = 0x16;
    pub const IPOW: u8 = 0x17;
    pub const INEG: u8 = 0x18;          // A=dst/src (unary, B unused)
    pub const IADD_IMM: u8 = 0x19;      // A=dst/src, +trail: i16 immediate

    // === Float32 Arithmetic (0x1A-0x22) ===
    pub const FADD: u8 = 0x1A;
    pub const FSUB: u8 = 0x1B;
    pub const FMUL: u8 = 0x1C;
    pub const FDIV: u8 = 0x1D;
    pub const FNEG: u8 = 0x1E;          // A=dst/src (unary)
    pub const FPOW: u8 = 0x1F;
    pub const FMUL_ADD: u8 = 0x20;      // A, B, +trail: rC in low 4 bits; rA = rA*rB + rC
    pub const FMUL_SUB: u8 = 0x21;      // rA = rA*rB - rC
    pub const FNMUL_ADD: u8 = 0x22;     // rA = rC - rA*rB

    // === Float64 Arithmetic (0x23-0x2C) ===
    pub const DADD: u8 = 0x23;
    pub const DSUB: u8 = 0x24;
    pub const DMUL: u8 = 0x25;
    pub const DDIV: u8 = 0x26;
    pub const DNEG: u8 = 0x27;
    pub const DPOW: u8 = 0x28;
    pub const DMUL_ADD: u8 = 0x29;
    pub const DMUL_SUB: u8 = 0x2A;
    pub const DNMUL_ADD: u8 = 0x2B;
    // 0x2C reserved

    // === Bitwise (0x2D-0x33) ===
    pub const AND: u8 = 0x2D;
    pub const OR: u8 = 0x2E;
    pub const XOR: u8 = 0x2F;
    pub const NOT: u8 = 0x30;           // unary
    pub const SHL: u8 = 0x31;
    pub const SHR: u8 = 0x32;
    pub const USHR: u8 = 0x33;

    // === Comparisons (0x34-0x3F) — rA = (rA op rB) ===
    pub const IEQ: u8 = 0x34;
    pub const INE: u8 = 0x35;
    pub const ILT: u8 = 0x36;
    pub const ILE: u8 = 0x37;
    pub const ULT: u8 = 0x38;
    pub const FEQ: u8 = 0x39;
    pub const FNE: u8 = 0x3A;
    pub const FLT: u8 = 0x3B;
    pub const FLE: u8 = 0x3C;
    pub const DEQ: u8 = 0x3D;
    pub const DLT: u8 = 0x3E;
    pub const DLE: u8 = 0x3F;

    // === Fused Compare+Branch (0x40-0x43) ===
    pub const ILT_JUMP: u8 = 0x40;     // A, B, +trail: i16 offset. Does NOT clobber rA.
    pub const FLT_JUMP: u8 = 0x41;     // A, B, +trail: i16 offset
    pub const ILE_JUMP: u8 = 0x42;
    pub const FLE_JUMP: u8 = 0x43;

    // === Type Conversions (0x44-0x4E) — unary, in-place ===
    pub const I32_TO_F32: u8 = 0x44;
    pub const F32_TO_I32: u8 = 0x45;
    pub const I32_TO_F64: u8 = 0x46;
    pub const F64_TO_I32: u8 = 0x47;
    pub const F32_TO_F64: u8 = 0x48;
    pub const F64_TO_F32: u8 = 0x49;
    pub const I32_TO_I8: u8 = 0x4A;
    pub const I8_TO_I32: u8 = 0x4B;
    pub const I64_TO_U32: u8 = 0x4C;
    // 0x4D-0x4E reserved

    // === Memory: basic (0x4F-0x59) ===
    pub const LOAD8: u8 = 0x4F;         // A=dst, B=addr
    pub const LOAD32: u8 = 0x50;        // A=dst, B=addr
    pub const LOAD64: u8 = 0x51;        // A=dst, B=addr
    pub const STORE8: u8 = 0x52;        // A=addr, B=src
    pub const STORE32: u8 = 0x53;       // A=addr, B=src
    pub const STORE64: u8 = 0x54;       // A=addr, B=src
    pub const LOAD32_OFF: u8 = 0x55;    // A=dst, B=base, +trail: i16 offset
    pub const LOAD64_OFF: u8 = 0x56;    // A=dst, B=base, +trail: i16 offset
    pub const STORE8_OFF: u8 = 0x57;    // A=base, B=src, +trail: i16 offset
    pub const STORE32_OFF: u8 = 0x58;   // A=base, B=src, +trail: i16 offset
    pub const STORE64_OFF: u8 = 0x59;   // A=base, B=src, +trail: i16 offset

    // 0x5A-0x5F reserved

    // === Memory: offset-encoded (no trailing word) (0x60-0x7D) ===
    // Load32 with fixed byte offsets
    pub const LOAD32_OFF0: u8 = 0x60;   // A=dst, B=base; *(base+0)
    pub const LOAD32_OFF4: u8 = 0x61;
    pub const LOAD32_OFF8: u8 = 0x62;
    pub const LOAD32_OFF12: u8 = 0x63;
    pub const LOAD32_OFF16: u8 = 0x64;
    pub const LOAD32_OFF20: u8 = 0x65;
    pub const LOAD32_OFF24: u8 = 0x66;
    pub const LOAD32_OFF28: u8 = 0x67;
    pub const LOAD32_OFF32: u8 = 0x68;

    // Store32 with fixed byte offsets
    pub const STORE32_OFF0: u8 = 0x69;  // A=base, B=src
    pub const STORE32_OFF4: u8 = 0x6A;
    pub const STORE32_OFF8: u8 = 0x6B;
    pub const STORE32_OFF12: u8 = 0x6C;
    pub const STORE32_OFF16: u8 = 0x6D;
    pub const STORE32_OFF20: u8 = 0x6E;
    pub const STORE32_OFF24: u8 = 0x6F;
    pub const STORE32_OFF28: u8 = 0x70;
    pub const STORE32_OFF32: u8 = 0x71;

    // Load64 with fixed byte offsets
    pub const LOAD64_OFF0: u8 = 0x72;
    pub const LOAD64_OFF8: u8 = 0x73;
    pub const LOAD64_OFF16: u8 = 0x74;
    pub const LOAD64_OFF24: u8 = 0x75;
    pub const LOAD64_OFF32: u8 = 0x76;

    // Store64 with fixed byte offsets
    pub const STORE64_OFF0: u8 = 0x77;
    pub const STORE64_OFF8: u8 = 0x78;
    pub const STORE64_OFF16: u8 = 0x79;
    pub const STORE64_OFF24: u8 = 0x7A;
    pub const STORE64_OFF32: u8 = 0x7B;

    // Fused slot access
    pub const LOAD_SLOT32: u8 = 0x7C;   // A=dst, +trail: slot index
    pub const STORE_SLOT32: u8 = 0x7D;  // A=src, +trail: slot index

    // 0x7E-0x7F reserved

    // === Calls (0x80-0x87) ===
    pub const CALL: u8 = 0x80;          // A=arg_count, +trail: func_idx
    pub const CALL_INDIRECT: u8 = 0x81; // A=func_reg, B=arg_count
    pub const CALL_CLOSURE: u8 = 0x82;  // A=fat_ptr_reg, B=arg_count
    pub const CALL_EXTERN: u8 = 0x83;   // A=arg_count, +trail: globals_offset
    // 0x84-0x87 reserved

    // === Stack Frame (0x88-0x8F) ===
    pub const ALLOC_LOCALS: u8 = 0x88;  // +trail: size in slots
    pub const SAVE_REGS: u8 = 0x89;     // +trail: slot; save all 16 regs
    pub const RESTORE_REGS: u8 = 0x8A;  // +trail: slot; restore r1-r15 (not r0)
    pub const MEM_COPY: u8 = 0x8B;      // A=dst, B=src, +trail: size
    pub const MEM_ZERO: u8 = 0x8C;      // A=dst, +trail: size
    pub const SPILL: u8 = 0x8D;         // A=reg, +trail: slot
    pub const RELOAD: u8 = 0x8E;        // A=reg, +trail: slot
    // 0x8F reserved

    // === Debugging (0x90-0x93) ===
    pub const PRINT_I32: u8 = 0x90;     // A=src
    pub const PRINT_F32: u8 = 0x91;     // A=src
    pub const ASSERT: u8 = 0x92;        // A=src
    pub const PUTC: u8 = 0x93;          // A=src

    // 0x94-0x9F reserved

    // === Math Builtins: Unary f32 (0xA0-0xB4) — in-place ===
    pub const SIN_F32: u8 = 0xA0;
    pub const COS_F32: u8 = 0xA1;
    pub const TAN_F32: u8 = 0xA2;
    pub const ASIN_F32: u8 = 0xA3;
    pub const ACOS_F32: u8 = 0xA4;
    pub const ATAN_F32: u8 = 0xA5;
    pub const SINH_F32: u8 = 0xA6;
    pub const COSH_F32: u8 = 0xA7;
    pub const TANH_F32: u8 = 0xA8;
    pub const ASINH_F32: u8 = 0xA9;
    pub const ACOSH_F32: u8 = 0xAA;
    pub const ATANH_F32: u8 = 0xAB;
    pub const LN_F32: u8 = 0xAC;
    pub const EXP_F32: u8 = 0xAD;
    pub const EXP2_F32: u8 = 0xAE;
    pub const LOG10_F32: u8 = 0xAF;
    pub const LOG2_F32: u8 = 0xB0;
    pub const SQRT_F32: u8 = 0xB1;
    pub const ABS_F32: u8 = 0xB2;
    pub const FLOOR_F32: u8 = 0xB3;
    pub const CEIL_F32: u8 = 0xB4;

    // === Math Builtins: Unary f64 (0xB5-0xC9) ===
    pub const SIN_F64: u8 = 0xB5;
    pub const COS_F64: u8 = 0xB6;
    pub const TAN_F64: u8 = 0xB7;
    pub const ASIN_F64: u8 = 0xB8;
    pub const ACOS_F64: u8 = 0xB9;
    pub const ATAN_F64: u8 = 0xBA;
    pub const SINH_F64: u8 = 0xBB;
    pub const COSH_F64: u8 = 0xBC;
    pub const TANH_F64: u8 = 0xBD;
    pub const ASINH_F64: u8 = 0xBE;
    pub const ACOSH_F64: u8 = 0xBF;
    pub const ATANH_F64: u8 = 0xC0;
    pub const LN_F64: u8 = 0xC1;
    pub const EXP_F64: u8 = 0xC2;
    pub const EXP2_F64: u8 = 0xC3;
    pub const LOG10_F64: u8 = 0xC4;
    pub const LOG2_F64: u8 = 0xC5;
    pub const SQRT_F64: u8 = 0xC6;
    pub const ABS_F64: u8 = 0xC7;
    pub const FLOOR_F64: u8 = 0xC8;
    pub const CEIL_F64: u8 = 0xC9;

    // === Predicates (0xCA-0xCD) ===
    pub const ISINF_F32: u8 = 0xCA;
    pub const ISINF_F64: u8 = 0xCB;
    pub const ISNAN_F32: u8 = 0xCC;
    pub const ISNAN_F64: u8 = 0xCD;

    // === Binary math (0xCE-0xD5) — destructive ===
    pub const POW_F32: u8 = 0xCE;
    pub const ATAN2_F32: u8 = 0xCF;
    pub const MIN_F32: u8 = 0xD0;
    pub const MAX_F32: u8 = 0xD1;
    pub const POW_F64: u8 = 0xD2;
    pub const ATAN2_F64: u8 = 0xD3;
    pub const MIN_F64: u8 = 0xD4;
    pub const MAX_F64: u8 = 0xD5;

    // === SIMD f32x4 (0xD6-0xDA) — destructive ===
    pub const F32X4_ADD: u8 = 0xD6;
    pub const F32X4_SUB: u8 = 0xD7;
    pub const F32X4_MUL: u8 = 0xD8;
    pub const F32X4_DIV: u8 = 0xD9;
    pub const F32X4_NEG: u8 = 0xDA;

    // === Extended (0xDB-0xE1) ===
    pub const MEM_EQ: u8 = 0xDB;        // A, B, +trail: size
    pub const MEM_NE: u8 = 0xDC;        // A, B, +trail: size
    pub const SLICE_EQ: u8 = 0xDD;      // A, B, +trail: elem_size
    pub const SLICE_NE: u8 = 0xDE;      // A, B, +trail: elem_size
    pub const SLICE_LOAD32: u8 = 0xDF;  // A=dst(slice), B=index
    pub const SLICE_STORE32: u8 = 0xE0; // A=slice, B=index; value from trailing reg
    pub const LOAD8_OFF: u8 = 0xE1;     // A=dst, B=base, +trail: offset

    // 0xE2-0xFF reserved (30 slots)
}

/// Returns true if the given opcode tag has a trailing u16 data word.
pub fn has_trailing_word(opcode: u8) -> bool {
    matches!(
        opcode,
        tags::JUMP
            | tags::JUMP_IF_ZERO
            | tags::JUMP_IF_NOT_ZERO
            | tags::LOAD_IMM
            | tags::LOAD_IMM_WIDE
            | tags::LOAD_F32
            | tags::LOAD_F64
            | tags::LOAD_CONST
            | tags::LOCAL_ADDR
            | tags::GLOBAL_ADDR
            | tags::IADD_IMM
            | tags::FMUL_ADD
            | tags::FMUL_SUB
            | tags::FNMUL_ADD
            | tags::DMUL_ADD
            | tags::DMUL_SUB
            | tags::DNMUL_ADD
            | tags::ILT_JUMP
            | tags::FLT_JUMP
            | tags::ILE_JUMP
            | tags::FLE_JUMP
            | tags::LOAD32_OFF
            | tags::LOAD64_OFF
            | tags::STORE8_OFF
            | tags::STORE32_OFF
            | tags::STORE64_OFF
            | tags::LOAD_SLOT32
            | tags::STORE_SLOT32
            | tags::CALL
            | tags::CALL_EXTERN
            | tags::ALLOC_LOCALS
            | tags::SAVE_REGS
            | tags::RESTORE_REGS
            | tags::MEM_COPY
            | tags::MEM_ZERO
            | tags::SPILL
            | tags::RELOAD
            | tags::MEM_EQ
            | tags::MEM_NE
            | tags::SLICE_EQ
            | tags::SLICE_NE
            | tags::LOAD8_OFF
    )
}

/// Get the name of a 16-bit opcode tag for debug printing.
pub fn tag_name(opcode: u8) -> &'static str {
    match opcode {
        tags::NOP => "Nop",
        tags::HALT => "Halt",
        tags::RETURN => "Return",
        tags::RETURN_REG => "ReturnReg",
        tags::JUMP => "Jump",
        tags::JUMP_IF_ZERO => "JumpIfZero",
        tags::JUMP_IF_NOT_ZERO => "JumpIfNotZero",
        tags::MOVE => "Move",
        tags::LOAD_IMM => "LoadImm",
        tags::LOAD_IMM_WIDE => "LoadImmWide",
        tags::LOAD_F32 => "LoadF32",
        tags::LOAD_F64 => "LoadF64",
        tags::LOAD_CONST => "LoadConst",
        tags::LOCAL_ADDR => "LocalAddr",
        tags::GLOBAL_ADDR => "GlobalAddr",
        tags::GET_CLOSURE_PTR => "GetClosurePtr",
        tags::IADD => "IAdd",
        tags::ISUB => "ISub",
        tags::IMUL => "IMul",
        tags::IDIV => "IDiv",
        tags::UDIV => "UDiv",
        tags::IREM => "IRem",
        tags::IPOW => "IPow",
        tags::INEG => "INeg",
        tags::IADD_IMM => "IAddImm",
        tags::FADD => "FAdd",
        tags::FSUB => "FSub",
        tags::FMUL => "FMul",
        tags::FDIV => "FDiv",
        tags::FNEG => "FNeg",
        tags::FPOW => "FPow",
        tags::FMUL_ADD => "FMulAdd",
        tags::FMUL_SUB => "FMulSub",
        tags::FNMUL_ADD => "FNMulAdd",
        tags::DADD => "DAdd",
        tags::DSUB => "DSub",
        tags::DMUL => "DMul",
        tags::DDIV => "DDiv",
        tags::DNEG => "DNeg",
        tags::DPOW => "DPow",
        tags::DMUL_ADD => "DMulAdd",
        tags::DMUL_SUB => "DMulSub",
        tags::DNMUL_ADD => "DNMulAdd",
        tags::AND => "And",
        tags::OR => "Or",
        tags::XOR => "Xor",
        tags::NOT => "Not",
        tags::SHL => "Shl",
        tags::SHR => "Shr",
        tags::USHR => "UShr",
        tags::IEQ => "IEq",
        tags::INE => "INe",
        tags::ILT => "ILt",
        tags::ILE => "ILe",
        tags::ULT => "ULt",
        tags::FEQ => "FEq",
        tags::FNE => "FNe",
        tags::FLT => "FLt",
        tags::FLE => "FLe",
        tags::DEQ => "DEq",
        tags::DLT => "DLt",
        tags::DLE => "DLe",
        tags::ILT_JUMP => "ILtJump",
        tags::FLT_JUMP => "FLtJump",
        tags::ILE_JUMP => "ILeJump",
        tags::FLE_JUMP => "FLeJump",
        tags::I32_TO_F32 => "I32ToF32",
        tags::F32_TO_I32 => "F32ToI32",
        tags::I32_TO_F64 => "I32ToF64",
        tags::F64_TO_I32 => "F64ToI32",
        tags::F32_TO_F64 => "F32ToF64",
        tags::F64_TO_F32 => "F64ToF32",
        tags::I32_TO_I8 => "I32ToI8",
        tags::I8_TO_I32 => "I8ToI32",
        tags::I64_TO_U32 => "I64ToU32",
        tags::LOAD8 => "Load8",
        tags::LOAD32 => "Load32",
        tags::LOAD64 => "Load64",
        tags::STORE8 => "Store8",
        tags::STORE32 => "Store32",
        tags::STORE64 => "Store64",
        tags::LOAD32_OFF => "Load32Off",
        tags::LOAD64_OFF => "Load64Off",
        tags::STORE8_OFF => "Store8Off",
        tags::STORE32_OFF => "Store32Off",
        tags::STORE64_OFF => "Store64Off",
        tags::LOAD32_OFF0 => "Load32Off0",
        tags::LOAD32_OFF4 => "Load32Off4",
        tags::LOAD32_OFF8 => "Load32Off8",
        tags::LOAD32_OFF12 => "Load32Off12",
        tags::LOAD32_OFF16 => "Load32Off16",
        tags::LOAD32_OFF20 => "Load32Off20",
        tags::LOAD32_OFF24 => "Load32Off24",
        tags::LOAD32_OFF28 => "Load32Off28",
        tags::LOAD32_OFF32 => "Load32Off32",
        tags::STORE32_OFF0 => "Store32Off0",
        tags::STORE32_OFF4 => "Store32Off4",
        tags::STORE32_OFF8 => "Store32Off8",
        tags::STORE32_OFF12 => "Store32Off12",
        tags::STORE32_OFF16 => "Store32Off16",
        tags::STORE32_OFF20 => "Store32Off20",
        tags::STORE32_OFF24 => "Store32Off24",
        tags::STORE32_OFF28 => "Store32Off28",
        tags::STORE32_OFF32 => "Store32Off32",
        tags::LOAD64_OFF0 => "Load64Off0",
        tags::LOAD64_OFF8 => "Load64Off8",
        tags::LOAD64_OFF16 => "Load64Off16",
        tags::LOAD64_OFF24 => "Load64Off24",
        tags::LOAD64_OFF32 => "Load64Off32",
        tags::STORE64_OFF0 => "Store64Off0",
        tags::STORE64_OFF8 => "Store64Off8",
        tags::STORE64_OFF16 => "Store64Off16",
        tags::STORE64_OFF24 => "Store64Off24",
        tags::STORE64_OFF32 => "Store64Off32",
        tags::LOAD_SLOT32 => "LoadSlot32",
        tags::STORE_SLOT32 => "StoreSlot32",
        tags::CALL => "Call",
        tags::CALL_INDIRECT => "CallIndirect",
        tags::CALL_CLOSURE => "CallClosure",
        tags::CALL_EXTERN => "CallExtern",
        tags::ALLOC_LOCALS => "AllocLocals",
        tags::SAVE_REGS => "SaveRegs",
        tags::RESTORE_REGS => "RestoreRegs",
        tags::MEM_COPY => "MemCopy",
        tags::MEM_ZERO => "MemZero",
        tags::SPILL => "Spill",
        tags::RELOAD => "Reload",
        tags::PRINT_I32 => "PrintI32",
        tags::PRINT_F32 => "PrintF32",
        tags::ASSERT => "Assert",
        tags::PUTC => "Putc",
        tags::SIN_F32 => "SinF32",
        tags::COS_F32 => "CosF32",
        tags::TAN_F32 => "TanF32",
        tags::ASIN_F32 => "AsinF32",
        tags::ACOS_F32 => "AcosF32",
        tags::ATAN_F32 => "AtanF32",
        tags::SINH_F32 => "SinhF32",
        tags::COSH_F32 => "CoshF32",
        tags::TANH_F32 => "TanhF32",
        tags::ASINH_F32 => "AsinhF32",
        tags::ACOSH_F32 => "AcoshF32",
        tags::ATANH_F32 => "AtanhF32",
        tags::LN_F32 => "LnF32",
        tags::EXP_F32 => "ExpF32",
        tags::EXP2_F32 => "Exp2F32",
        tags::LOG10_F32 => "Log10F32",
        tags::LOG2_F32 => "Log2F32",
        tags::SQRT_F32 => "SqrtF32",
        tags::ABS_F32 => "AbsF32",
        tags::FLOOR_F32 => "FloorF32",
        tags::CEIL_F32 => "CeilF32",
        tags::SIN_F64 => "SinF64",
        tags::COS_F64 => "CosF64",
        tags::TAN_F64 => "TanF64",
        tags::ASIN_F64 => "AsinF64",
        tags::ACOS_F64 => "AcosF64",
        tags::ATAN_F64 => "AtanF64",
        tags::SINH_F64 => "SinhF64",
        tags::COSH_F64 => "CoshF64",
        tags::TANH_F64 => "TanhF64",
        tags::ASINH_F64 => "AsinhF64",
        tags::ACOSH_F64 => "AcoshF64",
        tags::ATANH_F64 => "AtanhF64",
        tags::LN_F64 => "LnF64",
        tags::EXP_F64 => "ExpF64",
        tags::EXP2_F64 => "Exp2F64",
        tags::LOG10_F64 => "Log10F64",
        tags::LOG2_F64 => "Log2F64",
        tags::SQRT_F64 => "SqrtF64",
        tags::ABS_F64 => "AbsF64",
        tags::FLOOR_F64 => "FloorF64",
        tags::CEIL_F64 => "CeilF64",
        tags::ISINF_F32 => "IsinfF32",
        tags::ISINF_F64 => "IsinfF64",
        tags::ISNAN_F32 => "IsnanF32",
        tags::ISNAN_F64 => "IsnanF64",
        tags::POW_F32 => "PowF32",
        tags::ATAN2_F32 => "Atan2F32",
        tags::MIN_F32 => "MinF32",
        tags::MAX_F32 => "MaxF32",
        tags::POW_F64 => "PowF64",
        tags::ATAN2_F64 => "Atan2F64",
        tags::MIN_F64 => "MinF64",
        tags::MAX_F64 => "MaxF64",
        tags::F32X4_ADD => "F32x4Add",
        tags::F32X4_SUB => "F32x4Sub",
        tags::F32X4_MUL => "F32x4Mul",
        tags::F32X4_DIV => "F32x4Div",
        tags::F32X4_NEG => "F32x4Neg",
        tags::MEM_EQ => "MemEq",
        tags::MEM_NE => "MemNe",
        tags::SLICE_EQ => "SliceEq",
        tags::SLICE_NE => "SliceNe",
        tags::SLICE_LOAD32 => "SliceLoad32",
        tags::SLICE_STORE32 => "SliceStore32",
        tags::LOAD8_OFF => "Load8Off",
        _ => "Unknown",
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_packed_op16_encode_decode() {
        let op = PackedOp16::ab(tags::IADD, 3, 7);
        assert_eq!(op.opcode(), tags::IADD);
        assert_eq!(op.ra(), 3);
        assert_eq!(op.rb(), 7);
    }

    #[test]
    fn test_packed_op16_extremes() {
        let op = PackedOp16::ab(0xFF, 15, 15);
        assert_eq!(op.opcode(), 0xFF);
        assert_eq!(op.ra(), 15);
        assert_eq!(op.rb(), 15);

        let op = PackedOp16::ab(0x00, 0, 0);
        assert_eq!(op.opcode(), 0x00);
        assert_eq!(op.ra(), 0);
        assert_eq!(op.rb(), 0);
    }

    #[test]
    fn test_a_only() {
        let op = PackedOp16::a_only(tags::INEG, 5);
        assert_eq!(op.opcode(), tags::INEG);
        assert_eq!(op.ra(), 5);
        assert_eq!(op.rb(), 0);
    }

    #[test]
    fn test_trailing_word() {
        assert!(has_trailing_word(tags::LOAD_IMM));
        assert!(has_trailing_word(tags::JUMP));
        assert!(has_trailing_word(tags::CALL));
        assert!(!has_trailing_word(tags::IADD));
        assert!(!has_trailing_word(tags::MOVE));
        assert!(!has_trailing_word(tags::LOAD32_OFF0));
    }

    #[test]
    fn test_no_duplicate_tags() {
        // Verify all defined tag constants are unique.
        let mut seen = std::collections::HashSet::new();
        let all_tags = [
            tags::NOP, tags::HALT, tags::RETURN, tags::RETURN_REG,
            tags::JUMP, tags::JUMP_IF_ZERO, tags::JUMP_IF_NOT_ZERO,
            tags::MOVE, tags::LOAD_IMM, tags::LOAD_IMM_WIDE,
            tags::LOAD_F32, tags::LOAD_F64, tags::LOAD_CONST,
            tags::LOCAL_ADDR, tags::GLOBAL_ADDR, tags::GET_CLOSURE_PTR,
            tags::IADD, tags::ISUB, tags::IMUL, tags::IDIV,
            tags::UDIV, tags::IREM, tags::IPOW, tags::INEG, tags::IADD_IMM,
            tags::FADD, tags::FSUB, tags::FMUL, tags::FDIV,
            tags::FNEG, tags::FPOW, tags::FMUL_ADD, tags::FMUL_SUB, tags::FNMUL_ADD,
            tags::DADD, tags::DSUB, tags::DMUL, tags::DDIV,
            tags::DNEG, tags::DPOW, tags::DMUL_ADD, tags::DMUL_SUB, tags::DNMUL_ADD,
            tags::AND, tags::OR, tags::XOR, tags::NOT,
            tags::SHL, tags::SHR, tags::USHR,
            tags::IEQ, tags::INE, tags::ILT, tags::ILE, tags::ULT,
            tags::FEQ, tags::FNE, tags::FLT, tags::FLE,
            tags::DEQ, tags::DLT, tags::DLE,
            tags::ILT_JUMP, tags::FLT_JUMP, tags::ILE_JUMP, tags::FLE_JUMP,
            tags::I32_TO_F32, tags::F32_TO_I32, tags::I32_TO_F64, tags::F64_TO_I32,
            tags::F32_TO_F64, tags::F64_TO_F32, tags::I32_TO_I8, tags::I8_TO_I32, tags::I64_TO_U32,
            tags::LOAD8, tags::LOAD32, tags::LOAD64,
            tags::STORE8, tags::STORE32, tags::STORE64,
            tags::LOAD32_OFF, tags::LOAD64_OFF,
            tags::STORE8_OFF, tags::STORE32_OFF, tags::STORE64_OFF,
            tags::LOAD32_OFF0, tags::LOAD32_OFF4, tags::LOAD32_OFF8,
            tags::LOAD32_OFF12, tags::LOAD32_OFF16, tags::LOAD32_OFF20,
            tags::LOAD32_OFF24, tags::LOAD32_OFF28, tags::LOAD32_OFF32,
            tags::STORE32_OFF0, tags::STORE32_OFF4, tags::STORE32_OFF8,
            tags::STORE32_OFF12, tags::STORE32_OFF16, tags::STORE32_OFF20,
            tags::STORE32_OFF24, tags::STORE32_OFF28, tags::STORE32_OFF32,
            tags::LOAD64_OFF0, tags::LOAD64_OFF8, tags::LOAD64_OFF16,
            tags::LOAD64_OFF24, tags::LOAD64_OFF32,
            tags::STORE64_OFF0, tags::STORE64_OFF8, tags::STORE64_OFF16,
            tags::STORE64_OFF24, tags::STORE64_OFF32,
            tags::LOAD_SLOT32, tags::STORE_SLOT32,
            tags::CALL, tags::CALL_INDIRECT, tags::CALL_CLOSURE, tags::CALL_EXTERN,
            tags::ALLOC_LOCALS, tags::SAVE_REGS, tags::RESTORE_REGS,
            tags::MEM_COPY, tags::MEM_ZERO, tags::SPILL, tags::RELOAD,
            tags::PRINT_I32, tags::PRINT_F32, tags::ASSERT, tags::PUTC,
            tags::SIN_F32, tags::COS_F32, tags::TAN_F32,
            tags::ASIN_F32, tags::ACOS_F32, tags::ATAN_F32,
            tags::SINH_F32, tags::COSH_F32, tags::TANH_F32,
            tags::ASINH_F32, tags::ACOSH_F32, tags::ATANH_F32,
            tags::LN_F32, tags::EXP_F32, tags::EXP2_F32,
            tags::LOG10_F32, tags::LOG2_F32, tags::SQRT_F32,
            tags::ABS_F32, tags::FLOOR_F32, tags::CEIL_F32,
            tags::SIN_F64, tags::COS_F64, tags::TAN_F64,
            tags::ASIN_F64, tags::ACOS_F64, tags::ATAN_F64,
            tags::SINH_F64, tags::COSH_F64, tags::TANH_F64,
            tags::ASINH_F64, tags::ACOSH_F64, tags::ATANH_F64,
            tags::LN_F64, tags::EXP_F64, tags::EXP2_F64,
            tags::LOG10_F64, tags::LOG2_F64, tags::SQRT_F64,
            tags::ABS_F64, tags::FLOOR_F64, tags::CEIL_F64,
            tags::ISINF_F32, tags::ISINF_F64, tags::ISNAN_F32, tags::ISNAN_F64,
            tags::POW_F32, tags::ATAN2_F32, tags::MIN_F32, tags::MAX_F32,
            tags::POW_F64, tags::ATAN2_F64, tags::MIN_F64, tags::MAX_F64,
            tags::F32X4_ADD, tags::F32X4_SUB, tags::F32X4_MUL,
            tags::F32X4_DIV, tags::F32X4_NEG,
            tags::MEM_EQ, tags::MEM_NE, tags::SLICE_EQ, tags::SLICE_NE,
            tags::SLICE_LOAD32, tags::SLICE_STORE32, tags::LOAD8_OFF,
        ];
        for &tag in &all_tags {
            assert!(
                seen.insert(tag),
                "duplicate tag value: 0x{:02X} ({})",
                tag,
                tag_name(tag)
            );
        }
    }
}
