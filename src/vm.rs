// Register-based virtual machine for when we can't JIT,
// like on iOS.

use std::convert::TryInto;
use std::fmt;

// ============ Packed Bytecode Format ============
//
// Each instruction is 8 bytes: [tag:u8, r1:u8, r2:u8, r3:u8, imm:i32]
// This is half the size of the Opcode enum (16 bytes), improving cache behavior.
// The `tag` field is a raw u8 discriminant, dispatched via a match that LLVM
// compiles to a jump table — the Rust equivalent of computed goto.

#[derive(Clone, Copy)]
#[repr(C)]
struct PackedOp {
    tag: u8,
    r1: u8,  // dst / first reg
    r2: u8,  // src / second reg
    r3: u8,  // third reg
    imm: i32, // immediate: offset, f32 bits, constant index, or 4th reg in low byte
}

// Tag constants — one per opcode variant
mod tags {
    pub const NOP: u8 = 0;
    pub const HALT: u8 = 1;
    pub const MOVE: u8 = 2;
    pub const LOAD_IMM: u8 = 3;
    pub const LOAD_F32: u8 = 4;
    pub const LOAD_F64: u8 = 5;
    pub const LOAD_CONST: u8 = 6;
    pub const IADD: u8 = 7;
    pub const ISUB: u8 = 8;
    pub const IMUL: u8 = 9;
    pub const IDIV: u8 = 10;
    pub const UDIV: u8 = 11;
    pub const IREM: u8 = 12;
    pub const IPOW: u8 = 13;
    pub const INEG: u8 = 14;
    pub const IADD_IMM: u8 = 15;
    pub const FADD: u8 = 16;
    pub const FSUB: u8 = 17;
    pub const FMUL: u8 = 18;
    pub const FDIV: u8 = 19;
    pub const FNEG: u8 = 20;
    pub const FPOW: u8 = 21;
    pub const FMUL_ADD: u8 = 22;
    pub const FMUL_SUB: u8 = 23;
    pub const DADD: u8 = 24;
    pub const DSUB: u8 = 25;
    pub const DMUL: u8 = 26;
    pub const DDIV: u8 = 27;
    pub const DNEG: u8 = 28;
    pub const DPOW: u8 = 29;
    pub const DMUL_ADD: u8 = 30;
    pub const DMUL_SUB: u8 = 31;
    pub const AND: u8 = 32;
    pub const OR: u8 = 33;
    pub const XOR: u8 = 34;
    pub const NOT: u8 = 35;
    pub const SHL: u8 = 36;
    pub const SHR: u8 = 37;
    pub const USHR: u8 = 38;
    pub const IEQ: u8 = 39;
    pub const INE: u8 = 40;
    pub const ILT: u8 = 41;
    pub const ILE: u8 = 42;
    pub const ULT: u8 = 43;
    pub const FEQ: u8 = 44;
    pub const FNE: u8 = 45;
    pub const FLT: u8 = 46;
    pub const FLE: u8 = 47;
    pub const MEM_EQ: u8 = 48;
    pub const MEM_NE: u8 = 49;
    pub const DEQ: u8 = 50;
    pub const DLT: u8 = 51;
    pub const DLE: u8 = 52;
    pub const I32_TO_F32: u8 = 53;
    pub const F32_TO_I32: u8 = 54;
    pub const I32_TO_F64: u8 = 55;
    pub const F64_TO_I32: u8 = 56;
    pub const F32_TO_F64: u8 = 57;
    pub const F64_TO_F32: u8 = 58;
    pub const LOAD8: u8 = 59;
    pub const LOAD32: u8 = 60;
    pub const LOAD64: u8 = 61;
    pub const LOAD32_OFF: u8 = 62;
    pub const LOAD64_OFF: u8 = 63;
    pub const STORE8: u8 = 64;
    pub const STORE32: u8 = 65;
    pub const STORE64: u8 = 66;
    pub const STORE8_OFF: u8 = 67;
    pub const STORE32_OFF: u8 = 68;
    pub const STORE64_OFF: u8 = 69;
    pub const LOCAL_ADDR: u8 = 70;
    pub const GLOBAL_ADDR: u8 = 71;
    pub const JUMP: u8 = 72;
    pub const JUMP_IF_ZERO: u8 = 73;
    pub const JUMP_IF_NOT_ZERO: u8 = 74;
    pub const ILT_JUMP: u8 = 75;
    pub const CALL: u8 = 76;
    pub const CALL_INDIRECT: u8 = 77;
    pub const RETURN: u8 = 78;
    pub const RETURN_REG: u8 = 79;
    pub const ALLOC_LOCALS: u8 = 80;
    pub const MEM_COPY: u8 = 81;
    pub const MEM_ZERO: u8 = 82;
    pub const SAVE_REGS: u8 = 83;
    pub const RESTORE_REGS: u8 = 84;
    pub const PRINT_I32: u8 = 85;
    pub const PRINT_F32: u8 = 86;
    pub const ASSERT: u8 = 87;
    pub const PUTC: u8 = 88;
    pub const SIN_F32: u8 = 89;
    pub const COS_F32: u8 = 90;
    pub const TAN_F32: u8 = 91;
    pub const LN_F32: u8 = 92;
    pub const EXP_F32: u8 = 93;
    pub const SQRT_F32: u8 = 94;
    pub const ABS_F32: u8 = 95;
    pub const FLOOR_F32: u8 = 96;
    pub const CEIL_F32: u8 = 97;
    pub const SIN_F64: u8 = 98;
    pub const COS_F64: u8 = 99;
    pub const TAN_F64: u8 = 100;
    pub const LN_F64: u8 = 101;
    pub const EXP_F64: u8 = 102;
    pub const SQRT_F64: u8 = 103;
    pub const ABS_F64: u8 = 104;
    pub const FLOOR_F64: u8 = 105;
    pub const CEIL_F64: u8 = 106;
    pub const POW_F32: u8 = 107;
    pub const ATAN2_F32: u8 = 108;
    pub const POW_F64: u8 = 109;
    pub const ATAN2_F64: u8 = 110;
    // Superinstructions: fused LocalAddr+Load/Store
    pub const LOAD_SLOT32: u8 = 111;
    pub const STORE_SLOT32: u8 = 112;
    // For i64/f64 immediates that don't fit in 32 bits
    pub const LOAD_IMM_WIDE: u8 = 113;
    pub const LOAD_F64_WIDE: u8 = 114;
}

/// Linked program: all function code flattened into packed bytecode
struct LinkedProgram {
    ops: Vec<PackedOp>,
    func_offsets: Vec<usize>,
    func_locals: Vec<u32>,
    /// Pool for i64 values that don't fit in 32-bit immediate
    wide_i64: Vec<i64>,
    /// Pool for f64 values
    wide_f64: Vec<f64>,
}

impl LinkedProgram {
    fn from_program(program: &VMProgram) -> Self {
        let mut ops = Vec::new();
        let mut func_offsets = Vec::with_capacity(program.functions.len());
        let mut func_locals = Vec::with_capacity(program.functions.len());
        let mut wide_i64 = Vec::new();
        let mut wide_f64 = Vec::new();

        for func in &program.functions {
            func_offsets.push(ops.len());
            func_locals.push(func.locals_size);

            for op in &func.code {
                ops.push(Self::pack_opcode(op, &mut wide_i64, &mut wide_f64));
            }

            // Insert implicit return if needed
            match func.code.last() {
                Some(Opcode::Return) | Some(Opcode::ReturnReg { .. }) | Some(Opcode::Halt) => {}
                _ => ops.push(PackedOp { tag: tags::RETURN, r1: 0, r2: 0, r3: 0, imm: 0 }),
            }
        }

        LinkedProgram { ops, func_offsets, func_locals, wide_i64, wide_f64 }
    }

    fn pack_opcode(op: &Opcode, wide_i64: &mut Vec<i64>, wide_f64: &mut Vec<f64>) -> PackedOp {
        match *op {
            Opcode::Nop => PackedOp { tag: tags::NOP, r1: 0, r2: 0, r3: 0, imm: 0 },
            Opcode::Halt => PackedOp { tag: tags::HALT, r1: 0, r2: 0, r3: 0, imm: 0 },
            Opcode::Move { dst, src } => PackedOp { tag: tags::MOVE, r1: dst, r2: src, r3: 0, imm: 0 },
            Opcode::LoadImm { dst, value } => {
                if value >= i32::MIN as i64 && value <= i32::MAX as i64 {
                    PackedOp { tag: tags::LOAD_IMM, r1: dst, r2: 0, r3: 0, imm: value as i32 }
                } else {
                    let idx = wide_i64.len();
                    wide_i64.push(value);
                    PackedOp { tag: tags::LOAD_IMM_WIDE, r1: dst, r2: 0, r3: 0, imm: idx as i32 }
                }
            }
            Opcode::LoadF32 { dst, value } => {
                PackedOp { tag: tags::LOAD_F32, r1: dst, r2: 0, r3: 0, imm: value.to_bits() as i32 }
            }
            Opcode::LoadF64 { dst, value } => {
                let idx = wide_f64.len();
                wide_f64.push(value);
                PackedOp { tag: tags::LOAD_F64_WIDE, r1: dst, r2: 0, r3: 0, imm: idx as i32 }
            }
            Opcode::LoadConst { dst, idx } => PackedOp { tag: tags::LOAD_CONST, r1: dst, r2: 0, r3: 0, imm: idx as i32 },
            Opcode::IAdd { dst, a, b } => PackedOp { tag: tags::IADD, r1: dst, r2: a, r3: b, imm: 0 },
            Opcode::ISub { dst, a, b } => PackedOp { tag: tags::ISUB, r1: dst, r2: a, r3: b, imm: 0 },
            Opcode::IMul { dst, a, b } => PackedOp { tag: tags::IMUL, r1: dst, r2: a, r3: b, imm: 0 },
            Opcode::IDiv { dst, a, b } => PackedOp { tag: tags::IDIV, r1: dst, r2: a, r3: b, imm: 0 },
            Opcode::UDiv { dst, a, b } => PackedOp { tag: tags::UDIV, r1: dst, r2: a, r3: b, imm: 0 },
            Opcode::IRem { dst, a, b } => PackedOp { tag: tags::IREM, r1: dst, r2: a, r3: b, imm: 0 },
            Opcode::IPow { dst, a, b } => PackedOp { tag: tags::IPOW, r1: dst, r2: a, r3: b, imm: 0 },
            Opcode::INeg { dst, src } => PackedOp { tag: tags::INEG, r1: dst, r2: src, r3: 0, imm: 0 },
            Opcode::IAddImm { dst, src, imm } => PackedOp { tag: tags::IADD_IMM, r1: dst, r2: src, r3: 0, imm },
            Opcode::FAdd { dst, a, b } => PackedOp { tag: tags::FADD, r1: dst, r2: a, r3: b, imm: 0 },
            Opcode::FSub { dst, a, b } => PackedOp { tag: tags::FSUB, r1: dst, r2: a, r3: b, imm: 0 },
            Opcode::FMul { dst, a, b } => PackedOp { tag: tags::FMUL, r1: dst, r2: a, r3: b, imm: 0 },
            Opcode::FDiv { dst, a, b } => PackedOp { tag: tags::FDIV, r1: dst, r2: a, r3: b, imm: 0 },
            Opcode::FNeg { dst, src } => PackedOp { tag: tags::FNEG, r1: dst, r2: src, r3: 0, imm: 0 },
            Opcode::FPow { dst, a, b } => PackedOp { tag: tags::FPOW, r1: dst, r2: a, r3: b, imm: 0 },
            Opcode::FMulAdd { dst, a, b, c } => PackedOp { tag: tags::FMUL_ADD, r1: dst, r2: a, r3: b, imm: c as i32 },
            Opcode::FMulSub { dst, a, b, c } => PackedOp { tag: tags::FMUL_SUB, r1: dst, r2: a, r3: b, imm: c as i32 },
            Opcode::DAdd { dst, a, b } => PackedOp { tag: tags::DADD, r1: dst, r2: a, r3: b, imm: 0 },
            Opcode::DSub { dst, a, b } => PackedOp { tag: tags::DSUB, r1: dst, r2: a, r3: b, imm: 0 },
            Opcode::DMul { dst, a, b } => PackedOp { tag: tags::DMUL, r1: dst, r2: a, r3: b, imm: 0 },
            Opcode::DDiv { dst, a, b } => PackedOp { tag: tags::DDIV, r1: dst, r2: a, r3: b, imm: 0 },
            Opcode::DNeg { dst, src } => PackedOp { tag: tags::DNEG, r1: dst, r2: src, r3: 0, imm: 0 },
            Opcode::DPow { dst, a, b } => PackedOp { tag: tags::DPOW, r1: dst, r2: a, r3: b, imm: 0 },
            Opcode::DMulAdd { dst, a, b, c } => PackedOp { tag: tags::DMUL_ADD, r1: dst, r2: a, r3: b, imm: c as i32 },
            Opcode::DMulSub { dst, a, b, c } => PackedOp { tag: tags::DMUL_SUB, r1: dst, r2: a, r3: b, imm: c as i32 },
            Opcode::And { dst, a, b } => PackedOp { tag: tags::AND, r1: dst, r2: a, r3: b, imm: 0 },
            Opcode::Or { dst, a, b } => PackedOp { tag: tags::OR, r1: dst, r2: a, r3: b, imm: 0 },
            Opcode::Xor { dst, a, b } => PackedOp { tag: tags::XOR, r1: dst, r2: a, r3: b, imm: 0 },
            Opcode::Not { dst, src } => PackedOp { tag: tags::NOT, r1: dst, r2: src, r3: 0, imm: 0 },
            Opcode::Shl { dst, a, b } => PackedOp { tag: tags::SHL, r1: dst, r2: a, r3: b, imm: 0 },
            Opcode::Shr { dst, a, b } => PackedOp { tag: tags::SHR, r1: dst, r2: a, r3: b, imm: 0 },
            Opcode::UShr { dst, a, b } => PackedOp { tag: tags::USHR, r1: dst, r2: a, r3: b, imm: 0 },
            Opcode::IEq { dst, a, b } => PackedOp { tag: tags::IEQ, r1: dst, r2: a, r3: b, imm: 0 },
            Opcode::INe { dst, a, b } => PackedOp { tag: tags::INE, r1: dst, r2: a, r3: b, imm: 0 },
            Opcode::ILt { dst, a, b } => PackedOp { tag: tags::ILT, r1: dst, r2: a, r3: b, imm: 0 },
            Opcode::ILe { dst, a, b } => PackedOp { tag: tags::ILE, r1: dst, r2: a, r3: b, imm: 0 },
            Opcode::ULt { dst, a, b } => PackedOp { tag: tags::ULT, r1: dst, r2: a, r3: b, imm: 0 },
            Opcode::FEq { dst, a, b } => PackedOp { tag: tags::FEQ, r1: dst, r2: a, r3: b, imm: 0 },
            Opcode::FNe { dst, a, b } => PackedOp { tag: tags::FNE, r1: dst, r2: a, r3: b, imm: 0 },
            Opcode::FLt { dst, a, b } => PackedOp { tag: tags::FLT, r1: dst, r2: a, r3: b, imm: 0 },
            Opcode::FLe { dst, a, b } => PackedOp { tag: tags::FLE, r1: dst, r2: a, r3: b, imm: 0 },
            Opcode::MemEq { dst, a, b, size } => PackedOp { tag: tags::MEM_EQ, r1: dst, r2: a, r3: b, imm: size as i32 },
            Opcode::MemNe { dst, a, b, size } => PackedOp { tag: tags::MEM_NE, r1: dst, r2: a, r3: b, imm: size as i32 },
            Opcode::DEq { dst, a, b } => PackedOp { tag: tags::DEQ, r1: dst, r2: a, r3: b, imm: 0 },
            Opcode::DLt { dst, a, b } => PackedOp { tag: tags::DLT, r1: dst, r2: a, r3: b, imm: 0 },
            Opcode::DLe { dst, a, b } => PackedOp { tag: tags::DLE, r1: dst, r2: a, r3: b, imm: 0 },
            Opcode::I32ToF32 { dst, src } => PackedOp { tag: tags::I32_TO_F32, r1: dst, r2: src, r3: 0, imm: 0 },
            Opcode::F32ToI32 { dst, src } => PackedOp { tag: tags::F32_TO_I32, r1: dst, r2: src, r3: 0, imm: 0 },
            Opcode::I32ToF64 { dst, src } => PackedOp { tag: tags::I32_TO_F64, r1: dst, r2: src, r3: 0, imm: 0 },
            Opcode::F64ToI32 { dst, src } => PackedOp { tag: tags::F64_TO_I32, r1: dst, r2: src, r3: 0, imm: 0 },
            Opcode::F32ToF64 { dst, src } => PackedOp { tag: tags::F32_TO_F64, r1: dst, r2: src, r3: 0, imm: 0 },
            Opcode::F64ToF32 { dst, src } => PackedOp { tag: tags::F64_TO_F32, r1: dst, r2: src, r3: 0, imm: 0 },
            Opcode::Load8 { dst, addr } => PackedOp { tag: tags::LOAD8, r1: dst, r2: addr, r3: 0, imm: 0 },
            Opcode::Load32 { dst, addr } => PackedOp { tag: tags::LOAD32, r1: dst, r2: addr, r3: 0, imm: 0 },
            Opcode::Load64 { dst, addr } => PackedOp { tag: tags::LOAD64, r1: dst, r2: addr, r3: 0, imm: 0 },
            Opcode::Load32Off { dst, base, offset } => PackedOp { tag: tags::LOAD32_OFF, r1: dst, r2: base, r3: 0, imm: offset },
            Opcode::Load64Off { dst, base, offset } => PackedOp { tag: tags::LOAD64_OFF, r1: dst, r2: base, r3: 0, imm: offset },
            Opcode::Store8 { addr, src } => PackedOp { tag: tags::STORE8, r1: addr, r2: src, r3: 0, imm: 0 },
            Opcode::Store32 { addr, src } => PackedOp { tag: tags::STORE32, r1: addr, r2: src, r3: 0, imm: 0 },
            Opcode::Store64 { addr, src } => PackedOp { tag: tags::STORE64, r1: addr, r2: src, r3: 0, imm: 0 },
            Opcode::Store8Off { base, offset, src } => PackedOp { tag: tags::STORE8_OFF, r1: base, r2: src, r3: 0, imm: offset },
            Opcode::Store32Off { base, offset, src } => PackedOp { tag: tags::STORE32_OFF, r1: base, r2: src, r3: 0, imm: offset },
            Opcode::Store64Off { base, offset, src } => PackedOp { tag: tags::STORE64_OFF, r1: base, r2: src, r3: 0, imm: offset },
            Opcode::LocalAddr { dst, slot } => PackedOp { tag: tags::LOCAL_ADDR, r1: dst, r2: 0, r3: 0, imm: slot as i32 },
            Opcode::GlobalAddr { dst, offset } => PackedOp { tag: tags::GLOBAL_ADDR, r1: dst, r2: 0, r3: 0, imm: offset },
            Opcode::Jump { offset } => PackedOp { tag: tags::JUMP, r1: 0, r2: 0, r3: 0, imm: offset },
            Opcode::JumpIfZero { cond, offset } => PackedOp { tag: tags::JUMP_IF_ZERO, r1: cond, r2: 0, r3: 0, imm: offset },
            Opcode::JumpIfNotZero { cond, offset } => PackedOp { tag: tags::JUMP_IF_NOT_ZERO, r1: cond, r2: 0, r3: 0, imm: offset },
            Opcode::ILtJump { a, b, offset } => PackedOp { tag: tags::ILT_JUMP, r1: a, r2: b, r3: 0, imm: offset },
            Opcode::LoadSlot32 { dst, slot } => PackedOp { tag: tags::LOAD_SLOT32, r1: dst, r2: 0, r3: 0, imm: slot as i32 },
            Opcode::StoreSlot32 { slot, src } => PackedOp { tag: tags::STORE_SLOT32, r1: src, r2: 0, r3: 0, imm: slot as i32 },
            Opcode::Call { func, args_start, arg_count } => PackedOp { tag: tags::CALL, r1: args_start, r2: arg_count, r3: 0, imm: func as i32 },
            Opcode::CallIndirect { func_reg, args_start, arg_count } => PackedOp { tag: tags::CALL_INDIRECT, r1: func_reg, r2: args_start, r3: arg_count, imm: 0 },
            Opcode::Return => PackedOp { tag: tags::RETURN, r1: 0, r2: 0, r3: 0, imm: 0 },
            Opcode::ReturnReg { src } => PackedOp { tag: tags::RETURN_REG, r1: src, r2: 0, r3: 0, imm: 0 },
            Opcode::AllocLocals { size } => PackedOp { tag: tags::ALLOC_LOCALS, r1: 0, r2: 0, r3: 0, imm: size as i32 },
            Opcode::MemCopy { dst, src, size } => PackedOp { tag: tags::MEM_COPY, r1: dst, r2: src, r3: 0, imm: size as i32 },
            Opcode::MemZero { dst, size } => PackedOp { tag: tags::MEM_ZERO, r1: dst, r2: 0, r3: 0, imm: size as i32 },
            Opcode::SaveRegs { start_reg, count, slot } => PackedOp { tag: tags::SAVE_REGS, r1: start_reg, r2: count, r3: 0, imm: slot as i32 },
            Opcode::RestoreRegs { start_reg, count, slot } => PackedOp { tag: tags::RESTORE_REGS, r1: start_reg, r2: count, r3: 0, imm: slot as i32 },
            Opcode::PrintI32 { src } => PackedOp { tag: tags::PRINT_I32, r1: src, r2: 0, r3: 0, imm: 0 },
            Opcode::PrintF32 { src } => PackedOp { tag: tags::PRINT_F32, r1: src, r2: 0, r3: 0, imm: 0 },
            Opcode::Assert { src } => PackedOp { tag: tags::ASSERT, r1: src, r2: 0, r3: 0, imm: 0 },
            Opcode::Putc { src } => PackedOp { tag: tags::PUTC, r1: src, r2: 0, r3: 0, imm: 0 },
            Opcode::SinF32 { dst, src } => PackedOp { tag: tags::SIN_F32, r1: dst, r2: src, r3: 0, imm: 0 },
            Opcode::CosF32 { dst, src } => PackedOp { tag: tags::COS_F32, r1: dst, r2: src, r3: 0, imm: 0 },
            Opcode::TanF32 { dst, src } => PackedOp { tag: tags::TAN_F32, r1: dst, r2: src, r3: 0, imm: 0 },
            Opcode::LnF32 { dst, src } => PackedOp { tag: tags::LN_F32, r1: dst, r2: src, r3: 0, imm: 0 },
            Opcode::ExpF32 { dst, src } => PackedOp { tag: tags::EXP_F32, r1: dst, r2: src, r3: 0, imm: 0 },
            Opcode::SqrtF32 { dst, src } => PackedOp { tag: tags::SQRT_F32, r1: dst, r2: src, r3: 0, imm: 0 },
            Opcode::AbsF32 { dst, src } => PackedOp { tag: tags::ABS_F32, r1: dst, r2: src, r3: 0, imm: 0 },
            Opcode::FloorF32 { dst, src } => PackedOp { tag: tags::FLOOR_F32, r1: dst, r2: src, r3: 0, imm: 0 },
            Opcode::CeilF32 { dst, src } => PackedOp { tag: tags::CEIL_F32, r1: dst, r2: src, r3: 0, imm: 0 },
            Opcode::SinF64 { dst, src } => PackedOp { tag: tags::SIN_F64, r1: dst, r2: src, r3: 0, imm: 0 },
            Opcode::CosF64 { dst, src } => PackedOp { tag: tags::COS_F64, r1: dst, r2: src, r3: 0, imm: 0 },
            Opcode::TanF64 { dst, src } => PackedOp { tag: tags::TAN_F64, r1: dst, r2: src, r3: 0, imm: 0 },
            Opcode::LnF64 { dst, src } => PackedOp { tag: tags::LN_F64, r1: dst, r2: src, r3: 0, imm: 0 },
            Opcode::ExpF64 { dst, src } => PackedOp { tag: tags::EXP_F64, r1: dst, r2: src, r3: 0, imm: 0 },
            Opcode::SqrtF64 { dst, src } => PackedOp { tag: tags::SQRT_F64, r1: dst, r2: src, r3: 0, imm: 0 },
            Opcode::AbsF64 { dst, src } => PackedOp { tag: tags::ABS_F64, r1: dst, r2: src, r3: 0, imm: 0 },
            Opcode::FloorF64 { dst, src } => PackedOp { tag: tags::FLOOR_F64, r1: dst, r2: src, r3: 0, imm: 0 },
            Opcode::CeilF64 { dst, src } => PackedOp { tag: tags::CEIL_F64, r1: dst, r2: src, r3: 0, imm: 0 },
            Opcode::PowF32 { dst, a, b } => PackedOp { tag: tags::POW_F32, r1: dst, r2: a, r3: b, imm: 0 },
            Opcode::Atan2F32 { dst, a, b } => PackedOp { tag: tags::ATAN2_F32, r1: dst, r2: a, r3: b, imm: 0 },
            Opcode::PowF64 { dst, a, b } => PackedOp { tag: tags::POW_F64, r1: dst, r2: a, r3: b, imm: 0 },
            Opcode::Atan2F64 { dst, a, b } => PackedOp { tag: tags::ATAN2_F64, r1: dst, r2: a, r3: b, imm: 0 },
        }
    }
}

/// Register index (0-255)
pub type Reg = u8;

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
    Move { dst: Reg, src: Reg },

    /// Load immediate i64 into register
    LoadImm { dst: Reg, value: i64 },

    /// Load f32 immediate (stored as bits in i64)
    LoadF32 { dst: Reg, value: f32 },

    /// Load f64 immediate
    LoadF64 { dst: Reg, value: f64 },

    /// Load from constant pool
    LoadConst { dst: Reg, idx: ConstIdx },

    // ============ Integer Arithmetic ============

    /// Integer add: dst = a + b
    IAdd { dst: Reg, a: Reg, b: Reg },

    /// Integer subtract: dst = a - b
    ISub { dst: Reg, a: Reg, b: Reg },

    /// Integer multiply: dst = a * b
    IMul { dst: Reg, a: Reg, b: Reg },

    /// Signed integer divide: dst = a / b
    IDiv { dst: Reg, a: Reg, b: Reg },

    /// Unsigned integer divide: dst = a / b
    UDiv { dst: Reg, a: Reg, b: Reg },

    /// Integer remainder: dst = a % b
    IRem { dst: Reg, a: Reg, b: Reg },

    /// Integer power: dst = a ^ b
    IPow { dst: Reg, a: Reg, b: Reg },

    /// Integer negate: dst = -src
    INeg { dst: Reg, src: Reg },

    /// Integer add immediate: dst = src + imm
    IAddImm { dst: Reg, src: Reg, imm: i32 },

    // ============ Floating Point Arithmetic ============

    /// Float32 add: dst = a + b
    FAdd { dst: Reg, a: Reg, b: Reg },

    /// Float32 subtract: dst = a - b
    FSub { dst: Reg, a: Reg, b: Reg },

    /// Float32 multiply: dst = a * b
    FMul { dst: Reg, a: Reg, b: Reg },

    /// Float32 divide: dst = a / b
    FDiv { dst: Reg, a: Reg, b: Reg },

    /// Float32 negate: dst = -src
    FNeg { dst: Reg, src: Reg },

    /// Float32 power: dst = a ^ b
    FPow { dst: Reg, a: Reg, b: Reg },

    /// Float32 fused multiply-add: dst = a * b + c
    FMulAdd { dst: Reg, a: Reg, b: Reg, c: Reg },

    /// Float32 fused multiply-subtract: dst = a * b - c
    FMulSub { dst: Reg, a: Reg, b: Reg, c: Reg },

    // ============ Float64 Arithmetic ============

    /// Float64 add: dst = a + b
    DAdd { dst: Reg, a: Reg, b: Reg },

    /// Float64 subtract: dst = a - b
    DSub { dst: Reg, a: Reg, b: Reg },

    /// Float64 multiply: dst = a * b
    DMul { dst: Reg, a: Reg, b: Reg },

    /// Float64 divide: dst = a / b
    DDiv { dst: Reg, a: Reg, b: Reg },

    /// Float64 negate: dst = -src
    DNeg { dst: Reg, src: Reg },

    /// Float64 power: dst = a ^ b
    DPow { dst: Reg, a: Reg, b: Reg },

    /// Float64 fused multiply-add: dst = a * b + c
    DMulAdd { dst: Reg, a: Reg, b: Reg, c: Reg },

    /// Float64 fused multiply-subtract: dst = a * b - c
    DMulSub { dst: Reg, a: Reg, b: Reg, c: Reg },

    // ============ Bitwise Operations ============

    /// Bitwise AND: dst = a & b
    And { dst: Reg, a: Reg, b: Reg },

    /// Bitwise OR: dst = a | b
    Or { dst: Reg, a: Reg, b: Reg },

    /// Bitwise XOR: dst = a ^ b
    Xor { dst: Reg, a: Reg, b: Reg },

    /// Bitwise NOT: dst = !src
    Not { dst: Reg, src: Reg },

    /// Shift left: dst = a << b
    Shl { dst: Reg, a: Reg, b: Reg },

    /// Arithmetic shift right: dst = a >> b (signed)
    Shr { dst: Reg, a: Reg, b: Reg },

    /// Logical shift right: dst = a >>> b (unsigned)
    UShr { dst: Reg, a: Reg, b: Reg },

    // ============ Comparisons (result is 0 or 1) ============

    /// Integer equal: dst = (a == b)
    IEq { dst: Reg, a: Reg, b: Reg },

    /// Integer not equal: dst = (a != b)
    INe { dst: Reg, a: Reg, b: Reg },

    /// Signed less than: dst = (a < b)
    ILt { dst: Reg, a: Reg, b: Reg },

    /// Signed less or equal: dst = (a <= b)
    ILe { dst: Reg, a: Reg, b: Reg },

    /// Unsigned less than
    ULt { dst: Reg, a: Reg, b: Reg },

    /// Float32 equal
    FEq { dst: Reg, a: Reg, b: Reg },

    /// Float32 not equal
    FNe { dst: Reg, a: Reg, b: Reg },

    /// Float32 less than
    FLt { dst: Reg, a: Reg, b: Reg },

    /// Float32 less or equal
    FLe { dst: Reg, a: Reg, b: Reg },

    /// Memory equal: compare `size` bytes at pointers in a and b
    MemEq { dst: Reg, a: Reg, b: Reg, size: u32 },

    /// Memory not equal: compare `size` bytes at pointers in a and b
    MemNe { dst: Reg, a: Reg, b: Reg, size: u32 },

    /// Float64 equal
    DEq { dst: Reg, a: Reg, b: Reg },

    /// Float64 less than
    DLt { dst: Reg, a: Reg, b: Reg },

    /// Float64 less or equal
    DLe { dst: Reg, a: Reg, b: Reg },

    // ============ Type Conversions ============

    /// Convert i32 to f32
    I32ToF32 { dst: Reg, src: Reg },

    /// Convert f32 to i32
    F32ToI32 { dst: Reg, src: Reg },

    /// Convert i32 to f64
    I32ToF64 { dst: Reg, src: Reg },

    /// Convert f64 to i32
    F64ToI32 { dst: Reg, src: Reg },

    /// Convert f32 to f64
    F32ToF64 { dst: Reg, src: Reg },

    /// Convert f64 to f32
    F64ToF32 { dst: Reg, src: Reg },

    // ============ Memory Operations ============

    /// Load 8-bit value from memory: dst = mem[addr]
    Load8 { dst: Reg, addr: Reg },

    /// Load 32-bit value from memory
    Load32 { dst: Reg, addr: Reg },

    /// Load 64-bit value from memory
    Load64 { dst: Reg, addr: Reg },

    /// Load with offset: dst = mem[base + offset]
    Load32Off { dst: Reg, base: Reg, offset: i32 },

    /// Load 64-bit with offset
    Load64Off { dst: Reg, base: Reg, offset: i32 },

    /// Store 8-bit value to memory: mem[addr] = src
    Store8 { addr: Reg, src: Reg },

    /// Store 32-bit value to memory
    Store32 { addr: Reg, src: Reg },

    /// Store 64-bit value to memory
    Store64 { addr: Reg, src: Reg },

    /// Store 8-bit with offset: mem[base + offset] = src
    Store8Off { base: Reg, offset: i32, src: Reg },

    /// Store with offset: mem[base + offset] = src
    Store32Off { base: Reg, offset: i32, src: Reg },

    /// Store 64-bit with offset
    Store64Off { base: Reg, offset: i32, src: Reg },

    /// Get address of local variable slot
    LocalAddr { dst: Reg, slot: u16 },

    /// Get address of global variable at offset
    GlobalAddr { dst: Reg, offset: i32 },

    // ============ Control Flow ============

    /// Unconditional jump
    Jump { offset: Offset },

    /// Jump if register is zero (false)
    JumpIfZero { cond: Reg, offset: Offset },

    /// Jump if register is non-zero (true)
    JumpIfNotZero { cond: Reg, offset: Offset },

    // ============ Superinstructions: Compare and Branch ============

    /// Jump if a < b (signed): if !(a < b) jump
    ILtJump { a: Reg, b: Reg, offset: Offset },

    // ============ Superinstructions: Fused Local Slot Access ============

    /// Fused LocalAddr + Load32: load 32-bit value from local slot into register
    LoadSlot32 { dst: Reg, slot: u16 },

    /// Fused LocalAddr + Store32: store 32-bit value from register into local slot
    StoreSlot32 { slot: u16, src: Reg },

    /// Call function by index, args in registers starting at `args_start`
    /// Result (if any) goes in register 0
    Call { func: FuncIdx, args_start: Reg, arg_count: u8 },

    /// Call function pointer in register
    CallIndirect { func_reg: Reg, args_start: Reg, arg_count: u8 },

    /// Return from function (return value in register 0)
    Return,

    /// Return with specific register
    ReturnReg { src: Reg },

    // ============ Stack Frame Operations ============

    /// Allocate stack space for locals (in bytes)
    AllocLocals { size: u32 },

    /// Copy bytes from src to dst
    MemCopy { dst: Reg, src: Reg, size: u32 },

    /// Zero memory region
    MemZero { dst: Reg, size: u32 },

    // ============ Register Save/Restore ============

    /// Save registers [start_reg..start_reg+count] to locals at slot offset.
    /// Each register is 8 bytes.
    SaveRegs { start_reg: Reg, count: u8, slot: u32 },

    /// Restore registers [start_reg..start_reg+count] from locals at slot offset.
    RestoreRegs { start_reg: Reg, count: u8, slot: u32 },

    // ============ Debugging ============

    /// Print integer value (for debugging)
    PrintI32 { src: Reg },

    /// Print float value
    PrintF32 { src: Reg },

    /// Assert value is non-zero
    Assert { src: Reg },

    /// Print a character
    Putc { src: Reg },

    // ============ Math Builtins ============

    /// Unary f32 math: dst = op(src)
    SinF32 { dst: Reg, src: Reg },
    CosF32 { dst: Reg, src: Reg },
    TanF32 { dst: Reg, src: Reg },
    LnF32 { dst: Reg, src: Reg },
    ExpF32 { dst: Reg, src: Reg },
    SqrtF32 { dst: Reg, src: Reg },
    AbsF32 { dst: Reg, src: Reg },
    FloorF32 { dst: Reg, src: Reg },
    CeilF32 { dst: Reg, src: Reg },

    /// Unary f64 math: dst = op(src)
    SinF64 { dst: Reg, src: Reg },
    CosF64 { dst: Reg, src: Reg },
    TanF64 { dst: Reg, src: Reg },
    LnF64 { dst: Reg, src: Reg },
    ExpF64 { dst: Reg, src: Reg },
    SqrtF64 { dst: Reg, src: Reg },
    AbsF64 { dst: Reg, src: Reg },
    FloorF64 { dst: Reg, src: Reg },
    CeilF64 { dst: Reg, src: Reg },

    /// Binary f32 math: dst = op(a, b)
    PowF32 { dst: Reg, a: Reg, b: Reg },
    Atan2F32 { dst: Reg, a: Reg, b: Reg },

    /// Binary f64 math: dst = op(a, b)
    PowF64 { dst: Reg, a: Reg, b: Reg },
    Atan2F64 { dst: Reg, a: Reg, b: Reg },
}

/// A compiled function for the VM
#[derive(Clone, Debug)]
pub struct VMFunction {
    /// Function name for debugging
    pub name: String,

    /// Number of parameters
    pub param_count: u8,

    /// Number of local variable slots (in 8-byte units)
    pub local_slots: u16,

    /// Size of locals area in bytes
    pub locals_size: u32,

    /// The instruction stream
    pub code: Vec<Opcode>,
}

impl VMFunction {
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            param_count: 0,
            local_slots: 0,
            locals_size: 0,
            code: Vec::new(),
        }
    }

    /// Add an instruction and return its index
    pub fn emit(&mut self, op: Opcode) -> usize {
        let idx = self.code.len();
        self.code.push(op);
        idx
    }

    /// Patch a jump instruction at `idx` to jump to the current position
    pub fn patch_jump(&mut self, idx: usize) {
        let current = self.code.len() as i32;
        let target = idx as i32;
        let offset = current - target - 1;

        match &mut self.code[idx] {
            Opcode::Jump { offset: off } |
            Opcode::JumpIfZero { offset: off, .. } |
            Opcode::JumpIfNotZero { offset: off, .. } |
            Opcode::ILtJump { offset: off, .. } => {
                *off = offset;
            }
            _ => panic!("patch_jump called on non-jump instruction at index {}: {:?}", idx, &self.code[idx]),
        }
    }
}

/// A program ready to run on the VM
#[derive(Clone, Debug, Default)]
pub struct VMProgram {
    /// All functions in the program
    pub functions: Vec<VMFunction>,

    /// Constant pool (for large constants)
    pub constants: Vec<u64>,

    /// Entry point function index
    pub entry: FuncIdx,

    /// Size of global memory needed (in bytes)
    pub globals_size: usize,
}

impl VMProgram {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_function(&mut self, func: VMFunction) -> FuncIdx {
        let idx = self.functions.len() as FuncIdx;
        self.functions.push(func);
        idx
    }
}

/// Call frame for function execution
#[derive(Clone, Debug)]
struct CallFrame {
    /// Function being executed
    func_idx: FuncIdx,

    /// Instruction pointer within the function
    ip: usize,

    /// Base pointer into locals memory
    locals_base: usize,

    /// Register snapshot (for return value handling)
    return_reg: Reg,
}

/// The virtual machine state
pub struct VM {
    /// 256 general-purpose registers (64-bit each)
    registers: [u64; 256],

    /// Memory for local variables (stack-allocated per frame)
    locals: Vec<u8>,

    /// Call stack
    call_stack: Vec<CallFrame>,

    /// Heap memory for dynamic allocations
    heap: Vec<u8>,

    /// Global variable memory (zero-initialized)
    globals: Vec<u8>,

    /// Current instruction pointer
    ip: usize,

    /// Current function index
    current_func: FuncIdx,

    /// Base pointer for current frame's locals
    locals_base: usize,

    /// Current function name for debug diagnostics
    debug_func_name: String,

    /// Set to true if execution was cancelled via cancel_ptr()
    pub cancelled: bool,
}

impl Default for VM {
    fn default() -> Self {
        Self::new()
    }
}

impl VM {
    pub fn new() -> Self {
        Self {
            registers: [0; 256],
            locals: vec![0; 1024 * 1024], // 1MB stack
            call_stack: Vec::with_capacity(1024),
            heap: vec![0; 1024 * 1024], // 1MB heap
            globals: Vec::new(), // Allocated when program runs
            ip: 0,
            current_func: 0,
            locals_base: 0,
            debug_func_name: String::new(),
            cancelled: false,
        }
    }

    /// Get register value as i64
    #[inline(always)]
    fn get_i64(&self, r: Reg) -> i64 {
        self.registers[r as usize] as i64
    }

    /// Get register value as u64
    #[inline(always)]
    fn get_u64(&self, r: Reg) -> u64 {
        self.registers[r as usize]
    }

    /// Get register value as f32
    #[inline(always)]
    fn get_f32(&self, r: Reg) -> f32 {
        f32::from_bits(self.registers[r as usize] as u32)
    }

    /// Get register value as f64
    #[inline(always)]
    fn get_f64(&self, r: Reg) -> f64 {
        f64::from_bits(self.registers[r as usize])
    }

    /// Set register value from i64
    #[inline(always)]
    fn set_i64(&mut self, r: Reg, v: i64) {
        self.registers[r as usize] = v as u64;
    }

    /// Set register value from u64
    #[inline(always)]
    fn set_u64(&mut self, r: Reg, v: u64) {
        self.registers[r as usize] = v;
    }

    /// Set register value from f32
    #[inline(always)]
    fn set_f32(&mut self, r: Reg, v: f32) {
        self.registers[r as usize] = v.to_bits() as u64;
    }

    /// Set register value from f64
    #[inline(always)]
    fn set_f64(&mut self, r: Reg, v: f64) {
        self.registers[r as usize] = v.to_bits();
    }

    /// Set register to boolean (0 or 1)
    #[inline(always)]
    fn set_bool(&mut self, r: Reg, v: bool) {
        self.registers[r as usize] = v as u64;
    }

    /// Get pointer to locals memory
    #[inline(always)]
    fn local_ptr(&self, slot: u16) -> *const u8 {
        let offset = self.locals_base + (slot as usize) * 8;
        debug_assert!(offset < self.locals.len(), "local slot {slot} out of bounds (offset {offset}, locals size {})", self.locals.len());
        unsafe {
            self.locals.as_ptr().add(offset)
        }
    }

    /// Get mutable pointer to locals memory
    #[inline(always)]
    fn local_ptr_mut(&mut self, slot: u16) -> *mut u8 {
        let offset = self.locals_base + (slot as usize) * 8;
        debug_assert!(offset < self.locals.len(), "local slot {slot} out of bounds (offset {offset}, locals size {})", self.locals.len());
        unsafe {
            self.locals.as_mut_ptr().add(offset)
        }
    }

    /// In debug builds, panic if [ptr, ptr+size) is not within locals or globals.
    #[inline(always)]
    fn check_ptr(&self, ptr: u64, size: usize) {
        #[cfg(debug_assertions)]
        {
            let start = ptr as usize;
            let end = start + size;

            let locals_start = self.locals.as_ptr() as usize;
            let locals_end = locals_start + self.locals.len();

            let globals_start = self.globals.as_ptr() as usize;
            let globals_end = globals_start + self.globals.len();

            let heap_start = self.heap.as_ptr() as usize;
            let heap_end = heap_start + self.heap.len();

            let in_locals = start >= locals_start && end <= locals_end;
            let in_globals = !self.globals.is_empty() && start >= globals_start && end <= globals_end;
            let in_heap = !self.heap.is_empty() && start >= heap_start && end <= heap_end;

            if !in_locals && !in_globals && !in_heap {
                let func = &self.debug_func_name;
                panic!(
                    "VM: out-of-bounds memory access at {:#x}, size {size} (ip={}, func={func})\n  \
                     locals: {locals_start:#x}..{locals_end:#x}\n  \
                     globals: {globals_start:#x}..{globals_end:#x}\n  \
                     heap: {heap_start:#x}..{heap_end:#x}",
                    ptr, self.ip - 1,
                );
            }
        }
    }

    /// Run the program and return the result
    pub fn run(&mut self, program: &VMProgram) -> i64 {
        // === Link phase: pack all function code into flat bytecode ===
        let linked = LinkedProgram::from_program(program);

        // === Initialize VM state ===
        self.current_func = program.entry;
        self.ip = linked.func_offsets[program.entry as usize];
        self.locals_base = 0;
        self.registers = [0; 256];
        self.globals = vec![0u8; program.globals_size];
        self.cancelled = false;

        #[cfg(debug_assertions)]
        { self.debug_func_name = program.functions[program.entry as usize].name.clone(); }

        // === Dispatch loop: match on raw u8 tag → LLVM compiles to jump table ===
        // PackedOp is 8 bytes (vs 16 for Opcode enum), halving cache footprint.
        // Raw pointer access to registers avoids &mut self borrow conflicts.
        let ops = linked.ops.as_ptr();
        let regs = self.registers.as_mut_ptr();

        loop {
            let op = unsafe { *ops.add(self.ip) };
            self.ip += 1;

            macro_rules! r {
                ($idx:expr) => { unsafe { *regs.add($idx as usize) } }
            }
            macro_rules! r_set {
                ($idx:expr, $val:expr) => { unsafe { *regs.add($idx as usize) = $val } }
            }
            macro_rules! r_f32 {
                ($idx:expr) => { f32::from_bits(r!($idx) as u32) }
            }
            macro_rules! r_f64 {
                ($idx:expr) => { f64::from_bits(r!($idx)) }
            }
            macro_rules! set_f32 {
                ($idx:expr, $val:expr) => { r_set!($idx, ($val).to_bits() as u64) }
            }
            macro_rules! set_f64 {
                ($idx:expr, $val:expr) => { r_set!($idx, ($val).to_bits()) }
            }
            macro_rules! set_i64 {
                ($idx:expr, $val:expr) => { r_set!($idx, ($val) as u64) }
            }
            macro_rules! get_i64 {
                ($idx:expr) => { r!($idx) as i64 }
            }

            match op.tag {
                tags::NOP => {}

                tags::HALT => {
                    return get_i64!(0u8);
                }

                tags::MOVE => {
                    r_set!(op.r1, r!(op.r2));
                }

                tags::LOAD_IMM => {
                    set_i64!(op.r1, op.imm as i64);
                }

                tags::LOAD_IMM_WIDE => {
                    set_i64!(op.r1, linked.wide_i64[op.imm as usize]);
                }

                tags::LOAD_F32 => {
                    r_set!(op.r1, f32::from_bits(op.imm as u32).to_bits() as u64);
                }

                tags::LOAD_F64_WIDE => {
                    set_f64!(op.r1, linked.wide_f64[op.imm as usize]);
                }

                tags::LOAD_CONST => {
                    r_set!(op.r1, program.constants[op.imm as usize]);
                }

                // Integer arithmetic
                tags::IADD => { set_i64!(op.r1, get_i64!(op.r2).wrapping_add(get_i64!(op.r3))); }
                tags::ISUB => { set_i64!(op.r1, get_i64!(op.r2).wrapping_sub(get_i64!(op.r3))); }
                tags::IMUL => { set_i64!(op.r1, get_i64!(op.r2).wrapping_mul(get_i64!(op.r3))); }
                tags::IDIV => {
                    let b = get_i64!(op.r3);
                    if b == 0 { panic!("Division by zero"); }
                    set_i64!(op.r1, get_i64!(op.r2) / b);
                }
                tags::UDIV => {
                    let b = r!(op.r3);
                    if b == 0 { panic!("Division by zero"); }
                    r_set!(op.r1, r!(op.r2) / b);
                }
                tags::IREM => {
                    let b = get_i64!(op.r3);
                    if b == 0 { panic!("Division by zero"); }
                    set_i64!(op.r1, get_i64!(op.r2) % b);
                }
                tags::IPOW => {
                    set_i64!(op.r1, get_i64!(op.r2).wrapping_pow(get_i64!(op.r3) as u32));
                }
                tags::INEG => { set_i64!(op.r1, -get_i64!(op.r2)); }
                tags::IADD_IMM => { set_i64!(op.r1, get_i64!(op.r2).wrapping_add(op.imm as i64)); }

                // Float32 arithmetic
                tags::FADD => { set_f32!(op.r1, r_f32!(op.r2) + r_f32!(op.r3)); }
                tags::FSUB => { set_f32!(op.r1, r_f32!(op.r2) - r_f32!(op.r3)); }
                tags::FMUL => { set_f32!(op.r1, r_f32!(op.r2) * r_f32!(op.r3)); }
                tags::FDIV => { set_f32!(op.r1, r_f32!(op.r2) / r_f32!(op.r3)); }
                tags::FNEG => { set_f32!(op.r1, -r_f32!(op.r2)); }
                tags::FPOW => { set_f32!(op.r1, r_f32!(op.r2).powf(r_f32!(op.r3))); }
                tags::FMUL_ADD => { set_f32!(op.r1, r_f32!(op.r2).mul_add(r_f32!(op.r3), r_f32!(op.imm as u8))); }
                tags::FMUL_SUB => { set_f32!(op.r1, r_f32!(op.r2) * r_f32!(op.r3) - r_f32!(op.imm as u8)); }

                // Float64 arithmetic
                tags::DADD => { set_f64!(op.r1, r_f64!(op.r2) + r_f64!(op.r3)); }
                tags::DSUB => { set_f64!(op.r1, r_f64!(op.r2) - r_f64!(op.r3)); }
                tags::DMUL => { set_f64!(op.r1, r_f64!(op.r2) * r_f64!(op.r3)); }
                tags::DDIV => { set_f64!(op.r1, r_f64!(op.r2) / r_f64!(op.r3)); }
                tags::DNEG => { set_f64!(op.r1, -r_f64!(op.r2)); }
                tags::DPOW => { set_f64!(op.r1, r_f64!(op.r2).powf(r_f64!(op.r3))); }
                tags::DMUL_ADD => { set_f64!(op.r1, r_f64!(op.r2).mul_add(r_f64!(op.r3), r_f64!(op.imm as u8))); }
                tags::DMUL_SUB => { set_f64!(op.r1, r_f64!(op.r2) * r_f64!(op.r3) - r_f64!(op.imm as u8)); }

                // Bitwise
                tags::AND => { r_set!(op.r1, r!(op.r2) & r!(op.r3)); }
                tags::OR => { r_set!(op.r1, r!(op.r2) | r!(op.r3)); }
                tags::XOR => { r_set!(op.r1, r!(op.r2) ^ r!(op.r3)); }
                tags::NOT => { r_set!(op.r1, !r!(op.r2)); }
                tags::SHL => { r_set!(op.r1, r!(op.r2) << (r!(op.r3) & 63)); }
                tags::SHR => { set_i64!(op.r1, get_i64!(op.r2) >> (r!(op.r3) & 63)); }
                tags::USHR => { r_set!(op.r1, r!(op.r2) >> (r!(op.r3) & 63)); }

                // Comparisons
                tags::IEQ => { r_set!(op.r1, (get_i64!(op.r2) == get_i64!(op.r3)) as u64); }
                tags::INE => { r_set!(op.r1, (get_i64!(op.r2) != get_i64!(op.r3)) as u64); }
                tags::ILT => { r_set!(op.r1, (get_i64!(op.r2) < get_i64!(op.r3)) as u64); }
                tags::ILE => { r_set!(op.r1, (get_i64!(op.r2) <= get_i64!(op.r3)) as u64); }
                tags::ULT => { r_set!(op.r1, (r!(op.r2) < r!(op.r3)) as u64); }
                tags::FEQ => { r_set!(op.r1, (r_f32!(op.r2) == r_f32!(op.r3)) as u64); }
                tags::FNE => { r_set!(op.r1, (r_f32!(op.r2) != r_f32!(op.r3)) as u64); }
                tags::FLT => { r_set!(op.r1, (r_f32!(op.r2) < r_f32!(op.r3)) as u64); }
                tags::FLE => { r_set!(op.r1, (r_f32!(op.r2) <= r_f32!(op.r3)) as u64); }
                tags::DEQ => { r_set!(op.r1, (r_f64!(op.r2) == r_f64!(op.r3)) as u64); }
                tags::DLT => { r_set!(op.r1, (r_f64!(op.r2) < r_f64!(op.r3)) as u64); }
                tags::DLE => { r_set!(op.r1, (r_f64!(op.r2) <= r_f64!(op.r3)) as u64); }

                tags::MEM_EQ => {
                    let pa = r!(op.r2) as *const u8;
                    let pb = r!(op.r3) as *const u8;
                    let eq = unsafe {
                        std::slice::from_raw_parts(pa, op.imm as usize)
                            == std::slice::from_raw_parts(pb, op.imm as usize)
                    };
                    r_set!(op.r1, eq as u64);
                }

                tags::MEM_NE => {
                    let pa = r!(op.r2) as *const u8;
                    let pb = r!(op.r3) as *const u8;
                    let ne = unsafe {
                        std::slice::from_raw_parts(pa, op.imm as usize)
                            != std::slice::from_raw_parts(pb, op.imm as usize)
                    };
                    r_set!(op.r1, ne as u64);
                }

                // Type conversions
                tags::I32_TO_F32 => { set_f32!(op.r1, (get_i64!(op.r2) as i32) as f32); }
                tags::F32_TO_I32 => { set_i64!(op.r1, r_f32!(op.r2) as i32 as i64); }
                tags::I32_TO_F64 => { set_f64!(op.r1, (get_i64!(op.r2) as i32) as f64); }
                tags::F64_TO_I32 => { set_i64!(op.r1, r_f64!(op.r2) as i32 as i64); }
                tags::F32_TO_F64 => { set_f64!(op.r1, r_f32!(op.r2) as f64); }
                tags::F64_TO_F32 => { set_f32!(op.r1, r_f64!(op.r2) as f32); }

                // Memory operations
                tags::LOAD8 => {
                    let ptr = r!(op.r2);
                    self.check_ptr(ptr, 1);
                    set_i64!(op.r1, unsafe { *(ptr as *const u8) } as i64);
                }
                tags::LOAD32 => {
                    let ptr = r!(op.r2);
                    self.check_ptr(ptr, 4);
                    set_i64!(op.r1, unsafe { *(ptr as *const i32) } as i64);
                }
                tags::LOAD64 => {
                    let ptr = r!(op.r2);
                    self.check_ptr(ptr, 8);
                    set_i64!(op.r1, unsafe { *(ptr as *const i64) });
                }
                tags::LOAD32_OFF => {
                    let ptr = (r!(op.r2) as i64 + op.imm as i64) as u64;
                    self.check_ptr(ptr, 4);
                    set_i64!(op.r1, unsafe { *(ptr as *const i32) } as i64);
                }
                tags::LOAD64_OFF => {
                    let ptr = (r!(op.r2) as i64 + op.imm as i64) as u64;
                    self.check_ptr(ptr, 8);
                    set_i64!(op.r1, unsafe { *(ptr as *const i64) });
                }
                tags::STORE8 => {
                    let ptr = r!(op.r1);
                    self.check_ptr(ptr, 1);
                    unsafe { *(ptr as *mut u8) = r!(op.r2) as u8; }
                }
                tags::STORE32 => {
                    let ptr = r!(op.r1);
                    self.check_ptr(ptr, 4);
                    unsafe { *(ptr as *mut i32) = get_i64!(op.r2) as i32; }
                }
                tags::STORE64 => {
                    let ptr = r!(op.r1);
                    self.check_ptr(ptr, 8);
                    unsafe { *(ptr as *mut i64) = get_i64!(op.r2); }
                }
                tags::STORE8_OFF => {
                    let ptr = (r!(op.r1) as i64 + op.imm as i64) as u64;
                    self.check_ptr(ptr, 1);
                    unsafe { *(ptr as *mut u8) = r!(op.r2) as u8; }
                }
                tags::STORE32_OFF => {
                    let ptr = (r!(op.r1) as i64 + op.imm as i64) as u64;
                    self.check_ptr(ptr, 4);
                    unsafe { *(ptr as *mut i32) = get_i64!(op.r2) as i32; }
                }
                tags::STORE64_OFF => {
                    let ptr = (r!(op.r1) as i64 + op.imm as i64) as u64;
                    self.check_ptr(ptr, 8);
                    unsafe { *(ptr as *mut i64) = get_i64!(op.r2); }
                }

                tags::LOCAL_ADDR => {
                    let offset = self.locals_base + (op.imm as usize) * 8;
                    r_set!(op.r1, unsafe { self.locals.as_ptr().add(offset) } as u64);
                }
                tags::LOAD_SLOT32 => {
                    let offset = self.locals_base + (op.imm as usize) * 8;
                    let ptr = unsafe { self.locals.as_ptr().add(offset) };
                    set_i64!(op.r1, unsafe { *(ptr as *const i32) } as i64);
                }
                tags::STORE_SLOT32 => {
                    let offset = self.locals_base + (op.imm as usize) * 8;
                    let ptr = unsafe { self.locals.as_mut_ptr().add(offset) };
                    unsafe { *(ptr as *mut i32) = get_i64!(op.r1) as i32; }
                }

                tags::GLOBAL_ADDR => {
                    r_set!(op.r1, unsafe { self.globals.as_ptr().add(op.imm as usize) } as u64);
                }

                // Control flow
                tags::JUMP => {
                    self.ip = (self.ip as i32 + op.imm) as usize;
                    if op.imm < 0 && self.cancel_flag() {
                        self.cancelled = true;
                        return 0;
                    }
                }

                tags::JUMP_IF_ZERO => {
                    if r!(op.r1) == 0 {
                        self.ip = (self.ip as i32 + op.imm) as usize;
                        if op.imm < 0 && self.cancel_flag() {
                            self.cancelled = true;
                            return 0;
                        }
                    }
                }

                tags::JUMP_IF_NOT_ZERO => {
                    if r!(op.r1) != 0 {
                        self.ip = (self.ip as i32 + op.imm) as usize;
                        if op.imm < 0 && self.cancel_flag() {
                            self.cancelled = true;
                            return 0;
                        }
                    }
                }

                tags::ILT_JUMP => {
                    if get_i64!(op.r1) >= get_i64!(op.r2) {
                        self.ip = (self.ip as i32 + op.imm) as usize;
                        if op.imm < 0 && self.cancel_flag() {
                            self.cancelled = true;
                            return 0;
                        }
                    }
                }

                tags::CALL => {
                    let func = op.imm as u32;
                    let args_start = op.r1 as usize;
                    let arg_count = op.r2 as usize;

                    let frame = CallFrame {
                        func_idx: self.current_func,
                        ip: self.ip,
                        locals_base: self.locals_base,
                        return_reg: 0,
                    };
                    self.call_stack.push(frame);

                    for i in 0..arg_count {
                        if i != args_start + i {
                            self.registers[i] = self.registers[args_start + i];
                        }
                    }

                    self.locals_base += linked.func_locals[self.current_func as usize] as usize;
                    self.current_func = func;
                    self.ip = linked.func_offsets[func as usize];

                    #[cfg(debug_assertions)]
                    { self.debug_func_name = program.functions[func as usize].name.clone(); }

                    let needed = self.locals_base + linked.func_locals[func as usize] as usize;
                    if needed > self.locals.len() {
                        self.locals.resize(needed * 2, 0);
                    }
                }

                tags::CALL_INDIRECT => {
                    let func_idx = r!(op.r1) as FuncIdx;
                    let args_start = op.r2 as usize;
                    let arg_count = op.r3 as usize;

                    let frame = CallFrame {
                        func_idx: self.current_func,
                        ip: self.ip,
                        locals_base: self.locals_base,
                        return_reg: 0,
                    };
                    self.call_stack.push(frame);

                    for i in 0..arg_count {
                        if i != args_start + i {
                            self.registers[i] = self.registers[args_start + i];
                        }
                    }

                    self.locals_base += linked.func_locals[self.current_func as usize] as usize;
                    self.current_func = func_idx;
                    self.ip = linked.func_offsets[func_idx as usize];

                    #[cfg(debug_assertions)]
                    { self.debug_func_name = program.functions[func_idx as usize].name.clone(); }
                }

                tags::RETURN => {
                    if self.call_stack.is_empty() {
                        return get_i64!(0u8);
                    }
                    let frame = self.call_stack.pop().unwrap();
                    self.current_func = frame.func_idx;
                    self.ip = frame.ip;
                    self.locals_base = frame.locals_base;

                    #[cfg(debug_assertions)]
                    { self.debug_func_name = program.functions[frame.func_idx as usize].name.clone(); }
                }

                tags::RETURN_REG => {
                    self.registers[0] = self.registers[op.r1 as usize];
                    if self.call_stack.is_empty() {
                        return get_i64!(0u8);
                    }
                    let frame = self.call_stack.pop().unwrap();
                    self.current_func = frame.func_idx;
                    self.ip = frame.ip;
                    self.locals_base = frame.locals_base;

                    #[cfg(debug_assertions)]
                    { self.debug_func_name = program.functions[frame.func_idx as usize].name.clone(); }
                }

                tags::ALLOC_LOCALS => {
                    let size = op.imm as usize;
                    let needed = self.locals_base + size;
                    if needed > self.locals.len() {
                        self.locals.resize(needed * 2, 0);
                    }
                    for i in 0..size {
                        self.locals[self.locals_base + i] = 0;
                    }
                }

                tags::MEM_COPY => {
                    let dst_ptr = r!(op.r1);
                    let src_ptr = r!(op.r2);
                    let size = op.imm as usize;
                    self.check_ptr(dst_ptr, size);
                    self.check_ptr(src_ptr, size);
                    unsafe {
                        std::ptr::copy_nonoverlapping(src_ptr as *const u8, dst_ptr as *mut u8, size);
                    }
                }

                tags::MEM_ZERO => {
                    let ptr = r!(op.r1);
                    let size = op.imm as usize;
                    self.check_ptr(ptr, size);
                    unsafe { std::ptr::write_bytes(ptr as *mut u8, 0, size); }
                }

                tags::SAVE_REGS => {
                    let base = self.locals_base + op.imm as usize;
                    for i in 0..op.r2 as usize {
                        let reg_val = self.registers[op.r1 as usize + i];
                        let offset = base + i * 8;
                        self.locals[offset..offset + 8].copy_from_slice(&reg_val.to_le_bytes());
                    }
                }

                tags::RESTORE_REGS => {
                    let base = self.locals_base + op.imm as usize;
                    for i in 0..op.r2 as usize {
                        let offset = base + i * 8;
                        let bytes: [u8; 8] = self.locals[offset..offset + 8].try_into().unwrap();
                        self.registers[op.r1 as usize + i] = u64::from_le_bytes(bytes);
                    }
                }

                // Debugging
                tags::PRINT_I32 => { println!("{}", get_i64!(op.r1) as i32); }
                tags::PRINT_F32 => { println!("{}", r_f32!(op.r1)); }
                tags::ASSERT => {
                    let val = r!(op.r1) != 0;
                    println!("assert({})", val);
                    if !val {
                        panic!("Assertion failed at {}:{}",
                            program.functions[self.current_func as usize].name,
                            self.ip - 1);
                    }
                }
                tags::PUTC => {
                    if let Some(c) = char::from_u32(get_i64!(op.r1) as u32) {
                        print!("{}", c);
                    }
                }

                // Math builtins — f32 unary
                tags::SIN_F32 => { set_f32!(op.r1, r_f32!(op.r2).sin()); }
                tags::COS_F32 => { set_f32!(op.r1, r_f32!(op.r2).cos()); }
                tags::TAN_F32 => { set_f32!(op.r1, r_f32!(op.r2).tan()); }
                tags::LN_F32 => { set_f32!(op.r1, r_f32!(op.r2).ln()); }
                tags::EXP_F32 => { set_f32!(op.r1, r_f32!(op.r2).exp()); }
                tags::SQRT_F32 => { set_f32!(op.r1, r_f32!(op.r2).sqrt()); }
                tags::ABS_F32 => { set_f32!(op.r1, r_f32!(op.r2).abs()); }
                tags::FLOOR_F32 => { set_f32!(op.r1, r_f32!(op.r2).floor()); }
                tags::CEIL_F32 => { set_f32!(op.r1, r_f32!(op.r2).ceil()); }

                // Math builtins — f64 unary
                tags::SIN_F64 => { set_f64!(op.r1, r_f64!(op.r2).sin()); }
                tags::COS_F64 => { set_f64!(op.r1, r_f64!(op.r2).cos()); }
                tags::TAN_F64 => { set_f64!(op.r1, r_f64!(op.r2).tan()); }
                tags::LN_F64 => { set_f64!(op.r1, r_f64!(op.r2).ln()); }
                tags::EXP_F64 => { set_f64!(op.r1, r_f64!(op.r2).exp()); }
                tags::SQRT_F64 => { set_f64!(op.r1, r_f64!(op.r2).sqrt()); }
                tags::ABS_F64 => { set_f64!(op.r1, r_f64!(op.r2).abs()); }
                tags::FLOOR_F64 => { set_f64!(op.r1, r_f64!(op.r2).floor()); }
                tags::CEIL_F64 => { set_f64!(op.r1, r_f64!(op.r2).ceil()); }

                // Math builtins — binary
                tags::POW_F32 => { set_f32!(op.r1, r_f32!(op.r2).powf(r_f32!(op.r3))); }
                tags::ATAN2_F32 => { set_f32!(op.r1, r_f32!(op.r2).atan2(r_f32!(op.r3))); }
                tags::POW_F64 => { set_f64!(op.r1, r_f64!(op.r2).powf(r_f64!(op.r3))); }
                tags::ATAN2_F64 => { set_f64!(op.r1, r_f64!(op.r2).atan2(r_f64!(op.r3))); }

                _ => { panic!("VM: unknown opcode tag {}", op.tag); }
            }
        }
    }
    /// Only valid after `run` has been called.
    pub fn globals_ptr(&mut self) -> *mut u8 {
        self.globals.as_mut_ptr()
    }

    /// Returns a pointer to the cancel flag byte.
    /// Write a non-zero value from another thread to stop execution at the
    /// next backward jump.  The pointer is valid for the lifetime of this VM.
    pub fn cancel_ptr(&mut self) -> *mut u8 {
        &mut self.cancelled as *mut bool as *mut u8
    }

    #[inline(always)]
    fn cancel_flag(&self) -> bool {
        self.cancelled
    }

    /// Get the size of the globals buffer in bytes.
    pub fn globals_size(&self) -> usize {
        self.globals.len()
    }

    /// Run the program and return f32 result
    pub fn run_f32(&mut self, program: &VMProgram) -> f32 {
        self.run(program);
        self.get_f32(0)
    }

    /// Run the program and return f64 result
    pub fn run_f64(&mut self, program: &VMProgram) -> f64 {
        self.run(program);
        self.get_f64(0)
    }
}

/// Disassemble a function to a string
impl fmt::Display for VMFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "fn {} (params: {}, locals: {} bytes):",
            self.name, self.param_count, self.locals_size)?;

        for (i, op) in self.code.iter().enumerate() {
            writeln!(f, "  {:4}: {:?}", i, op)?;
        }

        Ok(())
    }
}

// ============ Biquad Filter Benchmark ============

/// Create a biquad filter program in VM assembly.
///
/// A biquad filter computes:
///   y[n] = b0*x[n] + b1*x[n-1] + b2*x[n-2] - a1*y[n-1] - a2*y[n-2]
///
/// State: [x1, x2, y1, y2] (previous samples)
/// Coefficients: [b0, b1, b2, a1, a2]
pub fn create_biquad_program() -> VMProgram {
    let mut program = VMProgram::new();

    // Create the biquad process function
    // Processes a single sample
    // r0 = input sample (f32)
    // r1 = pointer to state [x1, x2, y1, y2] (4 * f32 = 16 bytes)
    // r2 = pointer to coefficients [b0, b1, b2, a1, a2] (5 * f32 = 20 bytes)
    // Returns: output sample in r0
    let mut biquad = VMFunction::new("biquad_process");
    biquad.param_count = 3;

    // Input is in r0 (x0)
    // State pointer in r1
    // Coefficients pointer in r2

    // Load state: x1, x2, y1, y2
    biquad.emit(Opcode::Load32Off { dst: 3, base: 1, offset: 0 });   // r3 = x1
    biquad.emit(Opcode::Load32Off { dst: 4, base: 1, offset: 4 });   // r4 = x2
    biquad.emit(Opcode::Load32Off { dst: 5, base: 1, offset: 8 });   // r5 = y1
    biquad.emit(Opcode::Load32Off { dst: 6, base: 1, offset: 12 });  // r6 = y2

    // Load coefficients: b0, b1, b2, a1, a2
    biquad.emit(Opcode::Load32Off { dst: 10, base: 2, offset: 0 });  // r10 = b0
    biquad.emit(Opcode::Load32Off { dst: 11, base: 2, offset: 4 });  // r11 = b1
    biquad.emit(Opcode::Load32Off { dst: 12, base: 2, offset: 8 });  // r12 = b2
    biquad.emit(Opcode::Load32Off { dst: 13, base: 2, offset: 12 }); // r13 = a1
    biquad.emit(Opcode::Load32Off { dst: 14, base: 2, offset: 16 }); // r14 = a2

    // Compute output using FMulAdd/FMulSub superinstructions:
    // y0 = b0*x0 + b1*x1 + b2*x2 - a1*y1 - a2*y2

    // r20 = b0 * x0
    biquad.emit(Opcode::FMul { dst: 20, a: 10, b: 0 });

    // r20 = b1 * x1 + r20  (b0*x0 + b1*x1)
    biquad.emit(Opcode::FMulAdd { dst: 20, a: 11, b: 3, c: 20 });

    // r20 = b2 * x2 + r20  (b0*x0 + b1*x1 + b2*x2)
    biquad.emit(Opcode::FMulAdd { dst: 20, a: 12, b: 4, c: 20 });

    // r20 = r20 - a1 * y1  (using FMulSub: r20 = a1*y1 - r20, then negate... or use different approach)
    // Actually FMulSub is: dst = a * b - c, so we need: r20 - a1*y1
    // Let's compute a1*y1 first, then subtract
    biquad.emit(Opcode::FMul { dst: 21, a: 13, b: 5 });   // r21 = a1 * y1
    biquad.emit(Opcode::FSub { dst: 20, a: 20, b: 21 }); // r20 = r20 - a1*y1

    // r20 = r20 - a2 * y2
    biquad.emit(Opcode::FMul { dst: 21, a: 14, b: 6 });   // r21 = a2 * y2
    biquad.emit(Opcode::FSub { dst: 20, a: 20, b: 21 }); // r20 = r20 - a2*y2 -> this is y0

    // Update state:
    // x2 = x1, x1 = x0, y2 = y1, y1 = y0
    biquad.emit(Opcode::Store32Off { base: 1, offset: 4, src: 3 });  // x2 = x1
    biquad.emit(Opcode::Store32Off { base: 1, offset: 0, src: 0 });  // x1 = x0
    biquad.emit(Opcode::Store32Off { base: 1, offset: 12, src: 5 }); // y2 = y1
    biquad.emit(Opcode::Store32Off { base: 1, offset: 8, src: 20 }); // y1 = y0

    // Return y0
    biquad.emit(Opcode::ReturnReg { src: 20 });

    let biquad_idx = program.add_function(biquad);

    // Create main function that runs the biquad filter N times
    let mut main = VMFunction::new("main");
    main.locals_size = 64; // Space for state and coefficients

    // Allocate locals
    main.emit(Opcode::AllocLocals { size: 64 });

    // Get pointers to state and coefficients
    main.emit(Opcode::LocalAddr { dst: 10, slot: 0 }); // r10 = &state (slots 0-1)
    main.emit(Opcode::LocalAddr { dst: 11, slot: 2 }); // r11 = &coeffs (slots 2-4)

    // Initialize state to zero (already done by AllocLocals)

    // Initialize coefficients for a simple lowpass filter
    // b0 = 0.0675, b1 = 0.135, b2 = 0.0675
    // a1 = -1.143, a2 = 0.413
    main.emit(Opcode::LoadF32 { dst: 0, value: 0.0675 });
    main.emit(Opcode::Store32Off { base: 11, offset: 0, src: 0 });  // b0
    main.emit(Opcode::LoadF32 { dst: 0, value: 0.135 });
    main.emit(Opcode::Store32Off { base: 11, offset: 4, src: 0 });  // b1
    main.emit(Opcode::LoadF32 { dst: 0, value: 0.0675 });
    main.emit(Opcode::Store32Off { base: 11, offset: 8, src: 0 });  // b2
    main.emit(Opcode::LoadF32 { dst: 0, value: -1.143 });
    main.emit(Opcode::Store32Off { base: 11, offset: 12, src: 0 }); // a1
    main.emit(Opcode::LoadF32 { dst: 0, value: 0.413 });
    main.emit(Opcode::Store32Off { base: 11, offset: 16, src: 0 }); // a2

    // Loop counter
    main.emit(Opcode::LoadImm { dst: 20, value: 0 });        // r20 = i = 0
    main.emit(Opcode::LoadImm { dst: 21, value: 10000 });    // r21 = N = 10000

    // Accumulator for output
    main.emit(Opcode::LoadF32 { dst: 30, value: 0.0 });      // r30 = sum = 0.0

    // Loop start (instruction index for jump target)
    let loop_start = main.code.len();

    // Check i < N using superinstruction (jumps if NOT i < N)
    let jump_end = main.emit(Opcode::ILtJump { a: 20, b: 21, offset: 0 }); // patched later

    // Generate input: simple sawtooth wave
    // input = (i % 100) / 100.0 - 0.5
    main.emit(Opcode::LoadImm { dst: 23, value: 100 });
    main.emit(Opcode::IRem { dst: 24, a: 20, b: 23 });       // r24 = i % 100
    main.emit(Opcode::I32ToF32 { dst: 25, src: 24 });        // r25 = (float)(i % 100)
    main.emit(Opcode::LoadF32 { dst: 26, value: 100.0 });
    main.emit(Opcode::FDiv { dst: 0, a: 25, b: 26 });        // r0 = r25 / 100.0
    main.emit(Opcode::LoadF32 { dst: 26, value: 0.5 });
    main.emit(Opcode::FSub { dst: 0, a: 0, b: 26 });         // r0 = r0 - 0.5 (input)

    // Call biquad_process(input, &state, &coeffs)
    main.emit(Opcode::Move { dst: 1, src: 10 });             // r1 = &state
    main.emit(Opcode::Move { dst: 2, src: 11 });             // r2 = &coeffs
    main.emit(Opcode::Call { func: biquad_idx, args_start: 0, arg_count: 3 });

    // Accumulate output
    main.emit(Opcode::FAdd { dst: 30, a: 30, b: 0 });        // sum += output

    // i++ using superinstruction
    main.emit(Opcode::IAddImm { dst: 20, src: 20, imm: 1 });

    // Jump back to loop start
    let loop_end = main.code.len();
    main.emit(Opcode::Jump { offset: (loop_start as i32) - (loop_end as i32) - 1 });

    // Patch the conditional jump
    main.patch_jump(jump_end);

    // Return the sum (for verification)
    main.emit(Opcode::Move { dst: 0, src: 30 });
    main.emit(Opcode::Return);

    let main_idx = program.add_function(main);
    program.entry = main_idx;

    program
}

/// Run the biquad benchmark and return execution statistics
pub fn run_biquad_benchmark(iterations: u32) -> BenchmarkResult {
    let program = create_biquad_program();
    let mut vm = VM::new();

    let start = std::time::Instant::now();

    for _ in 0..iterations {
        vm.run(&program);
    }

    let elapsed = start.elapsed();
    let total_samples = 10000 * iterations;

    BenchmarkResult {
        total_time: elapsed,
        iterations,
        samples_per_iteration: 10000,
        samples_per_second: (total_samples as f64) / elapsed.as_secs_f64(),
    }
}

#[derive(Debug)]
pub struct BenchmarkResult {
    pub total_time: std::time::Duration,
    pub iterations: u32,
    pub samples_per_iteration: u32,
    pub samples_per_second: f64,
}

impl fmt::Display for BenchmarkResult {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Biquad Filter Benchmark Results:")?;
        writeln!(f, "  Iterations: {}", self.iterations)?;
        writeln!(f, "  Samples per iteration: {}", self.samples_per_iteration)?;
        writeln!(f, "  Total time: {:?}", self.total_time)?;
        writeln!(f, "  Samples/second: {:.2}", self.samples_per_second)?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_arithmetic() {
        let mut func = VMFunction::new("test");
        func.emit(Opcode::LoadImm { dst: 0, value: 10 });
        func.emit(Opcode::LoadImm { dst: 1, value: 20 });
        func.emit(Opcode::IAdd { dst: 0, a: 0, b: 1 });
        func.emit(Opcode::Return);

        let mut program = VMProgram::new();
        program.entry = program.add_function(func);

        let mut vm = VM::new();
        let result = vm.run(&program);
        assert_eq!(result, 30);
    }

    #[test]
    fn test_float_arithmetic() {
        let mut func = VMFunction::new("test");
        func.emit(Opcode::LoadF32 { dst: 0, value: 1.5 });
        func.emit(Opcode::LoadF32 { dst: 1, value: 2.5 });
        func.emit(Opcode::FMul { dst: 0, a: 0, b: 1 });
        func.emit(Opcode::Return);

        let mut program = VMProgram::new();
        program.entry = program.add_function(func);

        let mut vm = VM::new();
        let result = vm.run_f32(&program);
        assert!((result - 3.75).abs() < 0.0001);
    }

    #[test]
    fn test_conditionals() {
        let mut func = VMFunction::new("test");
        func.emit(Opcode::LoadImm { dst: 0, value: 5 });
        func.emit(Opcode::LoadImm { dst: 1, value: 10 });
        func.emit(Opcode::ILt { dst: 2, a: 0, b: 1 });  // 5 < 10 = true
        func.emit(Opcode::JumpIfZero { cond: 2, offset: 2 });
        func.emit(Opcode::LoadImm { dst: 0, value: 100 });  // if true
        func.emit(Opcode::Jump { offset: 1 });
        func.emit(Opcode::LoadImm { dst: 0, value: 200 });  // if false
        func.emit(Opcode::Return);

        let mut program = VMProgram::new();
        program.entry = program.add_function(func);

        let mut vm = VM::new();
        let result = vm.run(&program);
        assert_eq!(result, 100);
    }

    #[test]
    fn test_loop() {
        // Sum 1..10
        let mut func = VMFunction::new("test");
        func.emit(Opcode::LoadImm { dst: 0, value: 0 });   // sum = 0
        func.emit(Opcode::LoadImm { dst: 1, value: 1 });   // i = 1
        func.emit(Opcode::LoadImm { dst: 2, value: 11 });  // limit = 11

        // Loop start (index 3)
        func.emit(Opcode::ILt { dst: 3, a: 1, b: 2 });     // i < 11?
        func.emit(Opcode::JumpIfZero { cond: 3, offset: 4 }); // exit if false
        func.emit(Opcode::IAdd { dst: 0, a: 0, b: 1 });    // sum += i
        func.emit(Opcode::LoadImm { dst: 4, value: 1 });
        func.emit(Opcode::IAdd { dst: 1, a: 1, b: 4 });    // i++
        func.emit(Opcode::Jump { offset: -6 });            // back to loop start

        func.emit(Opcode::Return);

        let mut program = VMProgram::new();
        program.entry = program.add_function(func);

        let mut vm = VM::new();
        let result = vm.run(&program);
        assert_eq!(result, 55);  // 1+2+3+...+10 = 55
    }

    #[test]
    fn test_function_call() {
        // Create a function that doubles its input
        let mut double_fn = VMFunction::new("double");
        double_fn.param_count = 1;
        double_fn.emit(Opcode::IAdd { dst: 0, a: 0, b: 0 });
        double_fn.emit(Opcode::Return);

        // Main function
        let mut main = VMFunction::new("main");
        main.emit(Opcode::LoadImm { dst: 0, value: 21 });
        main.emit(Opcode::Call { func: 0, args_start: 0, arg_count: 1 });
        main.emit(Opcode::Return);

        let mut program = VMProgram::new();
        let double_idx = program.add_function(double_fn);
        program.entry = program.add_function(main);

        // Fix the call instruction to use correct function index
        if let Opcode::Call { func, .. } = &mut program.functions[program.entry as usize].code[1] {
            *func = double_idx;
        }

        let mut vm = VM::new();
        let result = vm.run(&program);
        assert_eq!(result, 42);
    }

    #[test]
    fn test_memory_operations() {
        let mut func = VMFunction::new("test");
        func.locals_size = 32;

        func.emit(Opcode::AllocLocals { size: 32 });
        func.emit(Opcode::LocalAddr { dst: 1, slot: 0 });
        func.emit(Opcode::LoadImm { dst: 0, value: 12345 });
        func.emit(Opcode::Store32 { addr: 1, src: 0 });
        func.emit(Opcode::LoadImm { dst: 0, value: 0 });  // Clear r0
        func.emit(Opcode::Load32 { dst: 0, addr: 1 });
        func.emit(Opcode::Return);

        let mut program = VMProgram::new();
        program.entry = program.add_function(func);

        let mut vm = VM::new();
        let result = vm.run(&program);
        assert_eq!(result, 12345);
    }

    #[test]
    fn test_biquad_program_runs() {
        let program = create_biquad_program();
        let mut vm = VM::new();
        let result = vm.run_f32(&program);

        // The exact value depends on the filter, but it should be non-zero
        // and within a reasonable range for filtered audio
        assert!(result.abs() < 10000.0);
        assert!(result != 0.0);
    }

    #[test]
    fn test_disassembly() {
        let program = create_biquad_program();
        let output = format!("{}", program.functions[0]);
        assert!(output.contains("biquad_process"));
        assert!(output.contains("FMul"));
    }

    #[test]
    fn test_bitwise_operations() {
        let mut func = VMFunction::new("test");
        func.emit(Opcode::LoadImm { dst: 0, value: 0b1010 });
        func.emit(Opcode::LoadImm { dst: 1, value: 0b1100 });
        func.emit(Opcode::And { dst: 2, a: 0, b: 1 });  // 0b1000 = 8
        func.emit(Opcode::Or { dst: 3, a: 0, b: 1 });   // 0b1110 = 14
        func.emit(Opcode::Xor { dst: 4, a: 0, b: 1 });  // 0b0110 = 6
        // Return AND result
        func.emit(Opcode::Move { dst: 0, src: 2 });
        func.emit(Opcode::Return);

        let mut program = VMProgram::new();
        program.entry = program.add_function(func);

        let mut vm = VM::new();
        let result = vm.run(&program);
        assert_eq!(result, 8);
    }

    #[test]
    fn test_type_conversions() {
        let mut func = VMFunction::new("test");
        func.emit(Opcode::LoadImm { dst: 0, value: 42 });
        func.emit(Opcode::I32ToF32 { dst: 1, src: 0 });
        func.emit(Opcode::LoadF32 { dst: 2, value: 0.5 });
        func.emit(Opcode::FAdd { dst: 1, a: 1, b: 2 });  // 42.5
        func.emit(Opcode::F32ToI32 { dst: 0, src: 1 });  // truncates to 42
        func.emit(Opcode::Return);

        let mut program = VMProgram::new();
        program.entry = program.add_function(func);

        let mut vm = VM::new();
        let result = vm.run(&program);
        assert_eq!(result, 42);
    }

    #[test]
    fn test_fmul_add() {
        let mut func = VMFunction::new("test");
        func.emit(Opcode::LoadF32 { dst: 0, value: 2.0 });
        func.emit(Opcode::LoadF32 { dst: 1, value: 3.0 });
        func.emit(Opcode::LoadF32 { dst: 2, value: 4.0 });
        // dst = 2.0 * 3.0 + 4.0 = 10.0
        func.emit(Opcode::FMulAdd { dst: 0, a: 0, b: 1, c: 2 });
        func.emit(Opcode::Return);

        let mut program = VMProgram::new();
        program.entry = program.add_function(func);

        let mut vm = VM::new();
        let result = vm.run_f32(&program);
        assert!((result - 10.0).abs() < 0.0001);
    }

    #[test]
    fn test_fmul_sub() {
        let mut func = VMFunction::new("test");
        func.emit(Opcode::LoadF32 { dst: 0, value: 2.0 });
        func.emit(Opcode::LoadF32 { dst: 1, value: 3.0 });
        func.emit(Opcode::LoadF32 { dst: 2, value: 4.0 });
        // dst = 2.0 * 3.0 - 4.0 = 2.0
        func.emit(Opcode::FMulSub { dst: 0, a: 0, b: 1, c: 2 });
        func.emit(Opcode::Return);

        let mut program = VMProgram::new();
        program.entry = program.add_function(func);

        let mut vm = VM::new();
        let result = vm.run_f32(&program);
        assert!((result - 2.0).abs() < 0.0001);
    }

    #[test]
    fn test_iadd_imm() {
        let mut func = VMFunction::new("test");
        func.emit(Opcode::LoadImm { dst: 0, value: 40 });
        func.emit(Opcode::IAddImm { dst: 0, src: 0, imm: 2 });
        func.emit(Opcode::Return);

        let mut program = VMProgram::new();
        program.entry = program.add_function(func);

        let mut vm = VM::new();
        let result = vm.run(&program);
        assert_eq!(result, 42);
    }

    #[test]
    fn test_ilt_jump() {
        // Test the fused compare-and-branch: ILtJump jumps if NOT (a < b)
        let mut func = VMFunction::new("test");
        func.emit(Opcode::LoadImm { dst: 0, value: 5 });
        func.emit(Opcode::LoadImm { dst: 1, value: 10 });
        // 5 < 10 is true, so we should NOT jump
        func.emit(Opcode::ILtJump { a: 0, b: 1, offset: 2 });
        func.emit(Opcode::LoadImm { dst: 0, value: 100 });  // should execute
        func.emit(Opcode::Jump { offset: 1 });
        func.emit(Opcode::LoadImm { dst: 0, value: 200 });  // should skip
        func.emit(Opcode::Return);

        let mut program = VMProgram::new();
        program.entry = program.add_function(func);

        let mut vm = VM::new();
        let result = vm.run(&program);
        assert_eq!(result, 100);
    }

    #[test]
    fn test_ilt_jump_taken() {
        // Test when jump IS taken: a >= b
        let mut func = VMFunction::new("test");
        func.emit(Opcode::LoadImm { dst: 0, value: 10 });
        func.emit(Opcode::LoadImm { dst: 1, value: 5 });
        // 10 < 5 is false, so we SHOULD jump
        func.emit(Opcode::ILtJump { a: 0, b: 1, offset: 2 });
        func.emit(Opcode::LoadImm { dst: 0, value: 100 });  // should skip
        func.emit(Opcode::Jump { offset: 1 });
        func.emit(Opcode::LoadImm { dst: 0, value: 200 });  // should execute
        func.emit(Opcode::Return);

        let mut program = VMProgram::new();
        program.entry = program.add_function(func);

        let mut vm = VM::new();
        let result = vm.run(&program);
        assert_eq!(result, 200);
    }

    #[test]
    fn test_loop_with_superinstructions() {
        // Sum 1..10 using IAddImm and ILtJump
        let mut func = VMFunction::new("test");
        func.emit(Opcode::LoadImm { dst: 0, value: 0 });   // sum = 0
        func.emit(Opcode::LoadImm { dst: 1, value: 1 });   // i = 1
        func.emit(Opcode::LoadImm { dst: 2, value: 11 });  // limit = 11

        // Loop start (index 3)
        // ILtJump: if !(i < 11) jump to end
        func.emit(Opcode::ILtJump { a: 1, b: 2, offset: 3 });
        func.emit(Opcode::IAdd { dst: 0, a: 0, b: 1 });    // sum += i
        func.emit(Opcode::IAddImm { dst: 1, src: 1, imm: 1 }); // i++
        func.emit(Opcode::Jump { offset: -4 });            // back to loop start

        func.emit(Opcode::Return);

        let mut program = VMProgram::new();
        program.entry = program.add_function(func);

        let mut vm = VM::new();
        let result = vm.run(&program);
        assert_eq!(result, 55);  // 1+2+3+...+10 = 55
    }

    #[test]
    fn test_infinite_loop_cancels() {
        // Infinite loop: Jump { offset: -1 } forever.
        let mut func = VMFunction::new("test");
        func.emit(Opcode::Jump { offset: -1 });

        let mut program = VMProgram::new();
        program.globals_size = 1; // room for the cancel flag
        program.entry = program.add_function(func);

        let mut vm = VM::new();

        struct CancelPtr(*mut u8);
        unsafe impl Send for CancelPtr {}
        let cancel = CancelPtr(vm.cancel_ptr());

        // After 50 ms, write 1 to the cancel flag.
        std::thread::spawn(move || {
            std::thread::sleep(std::time::Duration::from_millis(50));
            unsafe { *cancel.0 = 1; }
        });

        vm.run(&program);
        assert!(vm.cancelled, "expected the infinite loop to be cancelled");
    }
}
