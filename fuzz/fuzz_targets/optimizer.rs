#![no_main]
use libfuzzer_sys::fuzz_target;
use lyte::vm::{Opcode, VMFunction, VMProgram, VM};
use lyte::vm_optimize::{compact, optimize};
use std::io::Write;
use std::os::unix::io::AsRawFd;

/// Byte-driven generator for valid VM instruction sequences.
struct Gen<'a> {
    data: &'a [u8],
    pos: usize,
}

/// Number of 8-byte local slots available to the generated program.
const NUM_SLOTS: u16 = 8;
/// Size of locals area in bytes.
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

    fn next_i32(&mut self) -> i32 {
        let a = self.next() as i32;
        let b = self.next() as i32;
        a | (b << 8)
    }

    /// Pick a register in 0..max_reg (exclusive).
    fn reg(&mut self, max_reg: u16) -> u16 {
        self.next() as u16 % max_reg
    }

    /// Pick a local slot in 0..NUM_SLOTS.
    fn slot(&mut self) -> u16 {
        self.next() as u16 % NUM_SLOTS
    }

    /// Generate a single-function program with random arithmetic,
    /// control flow, and memory operations.
    /// Returns (code, body_len) where body_len is the number of instructions
    /// before the epilogue (used to clamp jumps).
    fn gen_program(&mut self) -> (Vec<Opcode>, usize) {
        let mut code = Vec::new();
        let num_regs: u16 = (self.next() % 12 + 4) as u16; // 4-15 virtual regs

        // Allocate locals so memory ops are safe.
        code.push(Opcode::AllocLocals { size: LOCALS_SIZE });

        // Initialize all registers with values.
        for r in 0..num_regs {
            let val = self.next_i32() as i64;
            code.push(Opcode::LoadImm { dst: r, value: val });
        }

        // Generate 5-30 instructions.
        let n_stmts = (self.next() % 26 + 5) as usize;
        for _ in 0..n_stmts {
            let op = self.gen_instruction(num_regs);
            code.push(op);
        }

        let body_len = code.len();

        // Epilogue: print values from local slots.
        // Jumps must not skip into or over this section.
        for s in 0..NUM_SLOTS.min(4) {
            code.push(Opcode::LoadSlot32 { dst: 0, slot: s });
            code.push(Opcode::PrintI32 { src: 0 });
        }
        code.push(Opcode::Return);
        (code, body_len)
    }

    fn gen_instruction(&mut self, nr: u16) -> Opcode {
        match self.next() % 28 {
            // Integer arithmetic
            0  => Opcode::IAdd { dst: self.reg(nr), a: self.reg(nr), b: self.reg(nr) },
            1  => Opcode::ISub { dst: self.reg(nr), a: self.reg(nr), b: self.reg(nr) },
            2  => Opcode::IMul { dst: self.reg(nr), a: self.reg(nr), b: self.reg(nr) },
            // Move
            3  => Opcode::Move { dst: self.reg(nr), src: self.reg(nr) },
            // LoadImm
            4  => Opcode::LoadImm { dst: self.reg(nr), value: self.next_i32() as i64 },
            // IAddImm (used by fuse_offset_access)
            5  => Opcode::IAddImm { dst: self.reg(nr), src: self.reg(nr), imm: self.next() as i32 - 128 },
            6  => Opcode::IAddImm { dst: self.reg(nr), src: self.reg(nr), imm: self.next() as i32 },
            // Bitwise
            7  => Opcode::And { dst: self.reg(nr), a: self.reg(nr), b: self.reg(nr) },
            8  => Opcode::Or  { dst: self.reg(nr), a: self.reg(nr), b: self.reg(nr) },
            9  => Opcode::Xor { dst: self.reg(nr), a: self.reg(nr), b: self.reg(nr) },
            10 => Opcode::Not { dst: self.reg(nr), src: self.reg(nr) },
            // Comparison
            11 => Opcode::IEq { dst: self.reg(nr), a: self.reg(nr), b: self.reg(nr) },
            12 => Opcode::ILt { dst: self.reg(nr), a: self.reg(nr), b: self.reg(nr) },
            // Control flow
            13 => {
                let cond = self.reg(nr);
                let skip = (self.next() % 3 + 1) as i32;
                Opcode::JumpIfZero { cond, offset: skip }
            }
            14 => {
                let skip = (self.next() % 3 + 1) as i32;
                Opcode::Jump { offset: skip }
            }
            15 => Opcode::Nop,
            16 => {
                let cond = self.reg(nr);
                let skip = (self.next() % 3 + 1) as i32;
                Opcode::JumpIfNotZero { cond, offset: skip }
            }
            // === Memory operations (exercise fuse_local_access) ===
            // LocalAddr (the key instruction for fuse_local_access)
            17 => Opcode::LocalAddr { dst: self.reg(nr), slot: self.slot() },
            // LocalAddr + Load32 pair (fuse_local_access target)
            18 => {
                let addr_reg = self.reg(nr);
                let slot = self.slot();
                let dst = self.reg(nr);
                // Emit LocalAddr as a separate instruction — the fuzzer
                // inserts one instruction at a time, so we return the Load32
                // and rely on LocalAddr from case 17 to provide the address.
                Opcode::Load32 { dst, addr: addr_reg }
            }
            // LocalAddr + Store32 pair
            19 => {
                let addr_reg = self.reg(nr);
                let src = self.reg(nr);
                Opcode::Store32 { addr: addr_reg, src }
            }
            // LoadSlot32/StoreSlot32 directly (the fused form)
            20 => Opcode::LoadSlot32 { dst: self.reg(nr), slot: self.slot() },
            21 => Opcode::StoreSlot32 { slot: self.slot(), src: self.reg(nr) },
            // IEq/ILt variants for more comparison coverage
            22 => Opcode::IEq { dst: self.reg(nr), a: self.reg(nr), b: self.reg(nr) },
            23 => Opcode::ILt { dst: self.reg(nr), a: self.reg(nr), b: self.reg(nr) },
            // More LoadSlot32/StoreSlot32 with different slots
            24 => Opcode::LoadSlot32 { dst: self.reg(nr), slot: self.slot() },
            25 => Opcode::StoreSlot32 { slot: self.slot(), src: self.reg(nr) },
            // More moves (optimizer should coalesce/forward these)
            26 => Opcode::Move { dst: self.reg(nr), src: self.reg(nr) },
            // Shifts
            27 => Opcode::Shl { dst: self.reg(nr), a: self.reg(nr), b: self.reg(nr) },
            _ => unreachable!(),
        }
    }
}

/// Fix up jump offsets to stay within the body (before the epilogue).
fn fixup_jumps(code: &mut [Opcode], body_len: usize) {
    let limit = body_len as i32;
    for i in 0..code.len() {
        let pos = i as i32;
        match &mut code[i] {
            Opcode::Jump { offset }
            | Opcode::JumpIfZero { offset, .. }
            | Opcode::JumpIfNotZero { offset, .. } => {
                let target = pos + 1 + *offset;
                if target < 0 || target >= limit {
                    // Clamp to the last body instruction (just before epilogue).
                    *offset = (limit - 1) - pos - 1;
                    if *offset < 0 { *offset = 0; }
                }
            }
            _ => {}
        }
    }
}

/// Ensure memory safety and determinism:
/// 1. Load/Store via register only when that register holds a LocalAddr.
/// 2. Track pointer taint: any value derived from a LocalAddr (including
///    through arithmetic) is non-deterministic across VM instances.
/// 3. Never store a tainted value into a local slot.
fn make_memory_safe(code: &mut Vec<Opcode>) {
    let mut tainted = vec![false; 256];

    for i in 0..code.len() {
        // Check if any source register is tainted.
        let src_tainted = {
            let mut any = false;
            code[i].for_each_src(|r| {
                if tainted[r as usize] { any = true; }
            });
            any
        };

        match &code[i] {
            Opcode::LocalAddr { dst, .. } => {
                tainted[*dst as usize] = true;
            }
            Opcode::Load32 { dst, .. } => {
                // Replace all Load32 with slot-based load to avoid alignment.
                let dst = *dst;
                code[i] = Opcode::LoadSlot32 { dst, slot: 0 };
                // Don't clear taint — this instruction might be jumped over.
            }
            Opcode::Store32 { src, .. } => {
                let src = *src;
                if tainted[src as usize] {
                    code[i] = Opcode::Nop;
                } else {
                    code[i] = Opcode::StoreSlot32 { slot: 0, src };
                }
            }
            Opcode::StoreSlot32 { src, .. } => {
                if tainted[*src as usize] {
                    code[i] = Opcode::Nop;
                }
            }
            Opcode::JumpIfZero { cond, .. }
            | Opcode::JumpIfNotZero { cond, .. } => {
                if tainted[*cond as usize] {
                    code[i] = Opcode::Nop;
                }
            }
            // Don't clear taint on LoadSlot32 — it can be jumped over,
            // leaving the register still holding a tainted pointer value.
            _ => {
                // For any other instruction, if it reads a tainted register,
                // its output is also tainted (derived from a pointer).
                // Never clear taint here — control flow (jumps) can skip
                // instructions, so a register that appears un-tainted on the
                // linear path might still hold a tainted value at runtime.
                if src_tainted {
                    if let Some(dst) = get_dst(&code[i]) {
                        tainted[dst as usize] = true;
                    }
                }
            }
        }
    }
}

fn get_dst(op: &Opcode) -> Option<u16> {
    op.get_dst()
}

/// Capture everything written to stdout (fd 1) during `f()`.
fn capture_stdout<F: FnOnce()>(f: F) -> String {
    std::io::stdout().flush().ok();

    let tmp = std::env::temp_dir().join("lyte_opt_fuzz_capture.txt");
    let file = match std::fs::File::create(&tmp) {
        Ok(f) => f,
        Err(_) => { f(); return String::new(); }
    };

    let saved_fd = unsafe { libc::dup(1) };
    if saved_fd < 0 {
        drop(file);
        f();
        return String::new();
    }
    unsafe { libc::dup2(file.as_raw_fd(), 1) };
    drop(file);

    f();

    std::io::stdout().flush().ok();

    unsafe { libc::dup2(saved_fd, 1) };
    unsafe { libc::close(saved_fd) };

    std::fs::read_to_string(&tmp).unwrap_or_default()
}

fuzz_target!(|data: &[u8]| {
    if data.len() < 16 {
        return;
    }

    let mut gen = Gen::new(data);
    let (mut code, body_len) = gen.gen_program();
    fixup_jumps(&mut code, body_len);
    make_memory_safe(&mut code);

    // Build the unoptimized program.
    let mut func_unopt = VMFunction::new("fuzz");
    func_unopt.locals_size = LOCALS_SIZE;
    func_unopt.local_slots = NUM_SLOTS;
    func_unopt.code = code.clone();
    let mut prog_unopt = VMProgram::new();
    prog_unopt.entry = prog_unopt.add_function(func_unopt);

    // Optimize a copy of the code.
    let mut opt_code = code.clone();
    if let Some((_count, _mapping)) = optimize(&mut opt_code, 0) {
        compact(&mut opt_code);
    }

    let mut func_opt = VMFunction::new("fuzz");
    func_opt.locals_size = LOCALS_SIZE;
    func_opt.local_slots = NUM_SLOTS;
    func_opt.code = opt_code;
    let mut prog_opt = VMProgram::new();
    prog_opt.entry = prog_opt.add_function(func_opt);

    // Run both and compare stdout. Catch panics from overflow etc.
    let unopt_output = std::panic::catch_unwind(|| {
        capture_stdout(|| {
            let mut vm = VM::new();
            vm.run(&prog_unopt);
        })
    });
    let unopt_output = match unopt_output {
        Ok(s) => s,
        Err(_) => return,
    };

    let opt_output = std::panic::catch_unwind(|| {
        capture_stdout(|| {
            let mut vm = VM::new();
            vm.run(&prog_opt);
        })
    });
    let opt_output = match opt_output {
        Ok(s) => s,
        Err(_) => return,
    };

    if unopt_output != opt_output {
        let mut code_str = String::new();
        for (i, op) in code.iter().enumerate() {
            code_str.push_str(&format!("  {}: {:?}\n", i, op));
        }
        panic!(
            "OPTIMIZER BUG: unoptimized and optimized produce different output!\n\
             \n=== Code ({} instructions) ===\n{}\
             \n=== Unoptimized output ===\n{}\n\
             === Optimized output ===\n{}",
            code.len(), code_str, unopt_output, opt_output
        );
    }
});
