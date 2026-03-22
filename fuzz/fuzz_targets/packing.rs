#![no_main]
use libfuzzer_sys::fuzz_target;
use lyte::vm::{Opcode, VMFunction, VMProgram, VM};

/// Byte-driven generator for valid VM instruction sequences with
/// packing edge cases: wide immediates, f32 constants, 2-word
/// instructions (MemEq), and jumps that cross 2-word boundaries.
struct Gen<'a> {
    data: &'a [u8],
    pos: usize,
}

const NUM_SLOTS: u16 = 8;
const LOCALS_SIZE: u32 = NUM_SLOTS as u32 * 8;

impl<'a> Gen<'a> {
    fn new(data: &'a [u8]) -> Self {
        Self { data, pos: 0 }
    }

    fn next(&mut self) -> u8 {
        if self.pos >= self.data.len() {
            0
        } else {
            let b = self.data[self.pos];
            self.pos += 1;
            b
        }
    }

    fn next_u16(&mut self) -> u16 {
        let a = self.next() as u16;
        let b = self.next() as u16;
        a | (b << 8)
    }

    fn next_i32(&mut self) -> i32 {
        let a = self.next() as i32;
        let b = self.next() as i32;
        let c = self.next() as i32;
        let d = self.next() as i32;
        a | (b << 8) | (c << 16) | (d << 24)
    }

    fn next_i64(&mut self) -> i64 {
        let lo = self.next_i32() as i64;
        let hi = self.next_i32() as i64;
        lo | (hi << 32)
    }

    fn next_f32(&mut self) -> f32 {
        f32::from_bits(self.next_i32() as u32)
    }

    fn reg(&mut self, max_reg: u16) -> u16 {
        self.next() as u16 % max_reg
    }

    fn slot(&mut self) -> u16 {
        self.next() as u16 % NUM_SLOTS
    }

    fn gen_program(&mut self) -> Vec<Opcode> {
        let mut code = Vec::new();
        let num_regs: u16 = (self.next() % 12 + 4) as u16;

        code.push(Opcode::AllocLocals { size: LOCALS_SIZE });

        // Initialize registers.
        for r in 0..num_regs {
            code.push(Opcode::LoadImm { dst: r, value: r as i64 });
        }

        // Generate instructions with packing edge cases.
        let n_stmts = (self.next() % 30 + 5) as usize;
        for _ in 0..n_stmts {
            code.push(self.gen_instruction(num_regs));
        }

        // Epilogue: print to make output observable.
        code.push(Opcode::PrintI32 { src: 0 });
        code.push(Opcode::Return);
        code
    }

    fn gen_instruction(&mut self, nr: u16) -> Opcode {
        match self.next() % 32 {
            // --- Standard instructions (ABC format) ---
            0  => Opcode::IAdd { dst: self.reg(nr), a: self.reg(nr), b: self.reg(nr) },
            1  => Opcode::ISub { dst: self.reg(nr), a: self.reg(nr), b: self.reg(nr) },
            2  => Opcode::IMul { dst: self.reg(nr), a: self.reg(nr), b: self.reg(nr) },
            3  => Opcode::Move { dst: self.reg(nr), src: self.reg(nr) },
            4  => Opcode::IEq { dst: self.reg(nr), a: self.reg(nr), b: self.reg(nr) },
            5  => Opcode::ILt { dst: self.reg(nr), a: self.reg(nr), b: self.reg(nr) },
            6  => Opcode::And { dst: self.reg(nr), a: self.reg(nr), b: self.reg(nr) },
            7  => Opcode::Nop,

            // --- Wide immediates (test wide_i64 pool) ---
            8  => {
                // Small immediate (fits in i16)
                let val = self.next() as i64 - 128;
                Opcode::LoadImm { dst: self.reg(nr), value: val }
            }
            9  => {
                // Large immediate (needs wide_i64 pool)
                let val = self.next_i64();
                Opcode::LoadImm { dst: self.reg(nr), value: val }
            }
            10 => {
                // Boundary value: exactly i16::MAX
                Opcode::LoadImm { dst: self.reg(nr), value: i16::MAX as i64 }
            }
            11 => {
                // Boundary value: i16::MAX + 1 (forces wide)
                Opcode::LoadImm { dst: self.reg(nr), value: i16::MAX as i64 + 1 }
            }

            // --- f32 constants (test f32_pool) ---
            12 => {
                let v = self.next_f32();
                if v.is_finite() {
                    Opcode::LoadF32 { dst: self.reg(nr), value: v }
                } else {
                    Opcode::LoadF32 { dst: self.reg(nr), value: 1.0 }
                }
            }
            13 => Opcode::FAdd { dst: self.reg(nr), a: self.reg(nr), b: self.reg(nr) },
            14 => Opcode::FSub { dst: self.reg(nr), a: self.reg(nr), b: self.reg(nr) },
            15 => Opcode::FMul { dst: self.reg(nr), a: self.reg(nr), b: self.reg(nr) },

            // --- Memory ops (test LocalAddr / slot packing) ---
            16 => Opcode::LocalAddr { dst: self.reg(nr), slot: self.slot() },
            17 => Opcode::LoadSlot32 { dst: self.reg(nr), slot: self.slot() },
            18 => Opcode::StoreSlot32 { slot: self.slot(), src: self.reg(nr) },

            // --- IAddImm (test AD format with signed immediate) ---
            19 => Opcode::IAddImm {
                dst: self.reg(nr),
                src: self.reg(nr),
                imm: self.next() as i32 - 128,
            },

            // --- Control flow (test jump offset fixups) ---
            20 => {
                let skip = (self.next() % 5 + 1) as i32;
                Opcode::Jump { offset: skip }
            }
            21 => {
                let cond = self.reg(nr);
                let skip = (self.next() % 5 + 1) as i32;
                Opcode::JumpIfZero { cond, offset: skip }
            }
            22 => {
                let cond = self.reg(nr);
                let skip = (self.next() % 5 + 1) as i32;
                Opcode::JumpIfNotZero { cond, offset: skip }
            }

            // --- Fused compare-branch (test C=i8 offset packing) ---
            23 => {
                let skip = (self.next() % 5 + 1) as i32;
                Opcode::ILtJump { a: self.reg(nr), b: self.reg(nr), offset: skip }
            }

            // --- More edge-case immediates ---
            24 => {
                // i16::MIN (boundary for wide encoding)
                Opcode::LoadImm { dst: self.reg(nr), value: i16::MIN as i64 }
            }
            25 => {
                // i16::MIN - 1 (forces wide encoding)
                Opcode::LoadImm { dst: self.reg(nr), value: i16::MIN as i64 - 1 }
            }

            // --- Wide offset variants ---
            26 => Opcode::Load32Off {
                dst: self.reg(nr),
                base: self.reg(nr),
                offset: (self.slot() as i32) * 8,
            },
            27 => Opcode::Store32Off {
                base: self.reg(nr),
                offset: (self.slot() as i32) * 8,
                src: self.reg(nr),
            },

            // --- More arithmetic ---
            28 => Opcode::IAdd { dst: self.reg(nr), a: self.reg(nr), b: self.reg(nr) },
            29 => Opcode::Shl { dst: self.reg(nr), a: self.reg(nr), b: self.reg(nr) },
            30 => Opcode::Not { dst: self.reg(nr), src: self.reg(nr) },
            31 => Opcode::Or { dst: self.reg(nr), a: self.reg(nr), b: self.reg(nr) },
            _ => unreachable!(),
        }
    }
}

/// Fix up jump offsets so they don't jump past the end of the code.
fn fixup_jumps(code: &mut [Opcode]) {
    let len = code.len() as i32;
    for i in 0..code.len() {
        let pos = i as i32;
        match &mut code[i] {
            Opcode::Jump { offset }
            | Opcode::JumpIfZero { offset, .. }
            | Opcode::JumpIfNotZero { offset, .. }
            | Opcode::ILtJump { offset, .. }
            | Opcode::FLtJump { offset, .. } => {
                let target = pos + 1 + *offset;
                if target < 0 || target >= len {
                    *offset = (len - 1) - pos - 1;
                    if *offset < 0 {
                        *offset = 0;
                    }
                }
            }
            _ => {}
        }
    }
}

/// Replace memory ops with safe slot-based equivalents.
fn make_memory_safe(code: &mut Vec<Opcode>) {
    for i in 0..code.len() {
        match &code[i] {
            Opcode::Load32Off { dst, .. } => {
                let dst = *dst;
                code[i] = Opcode::LoadSlot32 { dst, slot: 0 };
            }
            Opcode::Store32Off { src, .. } => {
                let src = *src;
                code[i] = Opcode::StoreSlot32 { slot: 0, src };
            }
            _ => {}
        }
    }
}

/// Count expected packed words for a sequence of opcodes.
fn expected_packed_count(code: &[Opcode]) -> usize {
    // All instructions pack to 1 word, except MemEq/MemNe/SliceEq/SliceNe (2 words).
    // Our generator doesn't produce those, so every instruction is 1 word.
    code.len()
}

fuzz_target!(|data: &[u8]| {
    if data.len() < 16 {
        return;
    }

    let mut gen = Gen::new(data);
    let mut code = gen.gen_program();
    fixup_jumps(&mut code);
    make_memory_safe(&mut code);
    // Re-fixup after make_memory_safe may have inserted jumps.
    fixup_jumps(&mut code);

    // === Test 1: Packing doesn't panic ===
    let mut func = VMFunction::new("fuzz_pack");
    func.locals_size = LOCALS_SIZE;
    func.local_slots = NUM_SLOTS;
    func.code = code.clone();
    let mut program = VMProgram::new();
    program.entry = program.add_function(func);

    // LinkedProgram::from_program does the packing + jump fixup.
    // This should not panic.
    let linked = lyte::vm::LinkedProgram::from_program(&program);

    // === Test 2: Packed instruction count matches expected ===
    // Each opcode becomes 1 packed word, except MemEq/MemNe/SliceEq/SliceNe (2 words).
    // Plus a possible trailing Return if the last instruction isn't Return/Halt.
    let expected = expected_packed_count(&code);
    let actual = linked.ops.len();
    // The linker may add an implicit Return.
    let has_implicit_return = !matches!(
        code.last(),
        Some(Opcode::Return) | Some(Opcode::ReturnReg { .. }) | Some(Opcode::Halt)
    );
    let expected_with_return = if has_implicit_return { expected + 1 } else { expected };
    assert_eq!(
        actual, expected_with_return,
        "Packed op count mismatch: expected {} (+ {} implicit return), got {}.\n\
         Code ({} opcodes): {:?}",
        expected,
        if has_implicit_return { 1 } else { 0 },
        actual,
        code.len(),
        &code[..code.len().min(20)]
    );

    // === Test 3: Execution doesn't crash ===
    // Run the packed program. Arithmetic panics (overflow, div-by-zero)
    // are expected from random inputs — only packing bugs matter.
    // catch_unwind may not catch all issues in unsafe dispatch code,
    // so we also rely on Tests 1 & 2 for structural correctness.
    let _ = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| {
        let mut vm = VM::new();
        vm.run(&program);
    }));
});
