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

    /// Generate a single-function program with random arithmetic and control flow.
    fn gen_program(&mut self) -> Vec<Opcode> {
        let mut code = Vec::new();
        let num_regs: u16 = (self.next() % 12 + 4) as u16; // 4-15 virtual regs

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

        // Print register 0 so we can compare outputs.
        code.push(Opcode::PrintI32 { src: 0 });
        code.push(Opcode::Return);
        code
    }

    fn gen_instruction(&mut self, nr: u16) -> Opcode {
        match self.next() % 20 {
            0  => Opcode::IAdd { dst: self.reg(nr), a: self.reg(nr), b: self.reg(nr) },
            1  => Opcode::ISub { dst: self.reg(nr), a: self.reg(nr), b: self.reg(nr) },
            2  => Opcode::IMul { dst: self.reg(nr), a: self.reg(nr), b: self.reg(nr) },
            3  => Opcode::Move { dst: self.reg(nr), src: self.reg(nr) },
            4  => Opcode::LoadImm { dst: self.reg(nr), value: self.next_i32() as i64 },
            5  => Opcode::IAddImm { dst: self.reg(nr), src: self.reg(nr), imm: self.next() as i32 - 128 },
            6  => Opcode::IAddImm { dst: self.reg(nr), src: self.reg(nr), imm: self.next() as i32 },
            7  => Opcode::And { dst: self.reg(nr), a: self.reg(nr), b: self.reg(nr) },
            8  => Opcode::Or  { dst: self.reg(nr), a: self.reg(nr), b: self.reg(nr) },
            9  => Opcode::Xor { dst: self.reg(nr), a: self.reg(nr), b: self.reg(nr) },
            10 => Opcode::Not { dst: self.reg(nr), src: self.reg(nr) },
            11 => Opcode::IEq { dst: self.reg(nr), a: self.reg(nr), b: self.reg(nr) },
            12 => Opcode::ILt { dst: self.reg(nr), a: self.reg(nr), b: self.reg(nr) },
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
            16 => Opcode::Shl { dst: self.reg(nr), a: self.reg(nr), b: self.reg(nr) },
            17 => Opcode::Shr { dst: self.reg(nr), a: self.reg(nr), b: self.reg(nr) },
            18 => Opcode::Move { dst: self.reg(nr), src: self.reg(nr) },
            19 => {
                let cond = self.reg(nr);
                let skip = (self.next() % 3 + 1) as i32;
                Opcode::JumpIfNotZero { cond, offset: skip }
            }
            _ => unreachable!(),
        }
    }
}

/// Fix up jump offsets that would land past the end of the code.
fn fixup_jumps(code: &mut [Opcode]) {
    let len = code.len() as i32;
    for i in 0..code.len() {
        let pos = i as i32;
        match &mut code[i] {
            Opcode::Jump { offset }
            | Opcode::JumpIfZero { offset, .. }
            | Opcode::JumpIfNotZero { offset, .. } => {
                let target = pos + 1 + *offset;
                if target < 0 || target >= len {
                    *offset = len - 1 - pos - 1;
                }
            }
            _ => {}
        }
    }
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
    let mut code = gen.gen_program();
    fixup_jumps(&mut code);

    // Build the unoptimized program.
    let mut func_unopt = VMFunction::new("fuzz");
    func_unopt.code = code.clone();
    let mut prog_unopt = VMProgram::new();
    prog_unopt.entry = prog_unopt.add_function(func_unopt);

    // Optimize a copy of the code.
    let mut opt_code = code.clone();
    if let Some((_count, _mapping)) = optimize(&mut opt_code, 0) {
        compact(&mut opt_code);
    }

    let mut func_opt = VMFunction::new("fuzz");
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
        Err(_) => return, // overflow or other runtime panic — skip
    };

    let opt_output = std::panic::catch_unwind(|| {
        capture_stdout(|| {
            let mut vm = VM::new();
            vm.run(&prog_opt);
        })
    });
    let opt_output = match opt_output {
        Ok(s) => s,
        Err(_) => return, // overflow or other runtime panic — skip
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
