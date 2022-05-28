// See https://github.com/wasm3/wasm3/blob/main/docs/Interpreter.md

// According to wasm3, continuation passing is faster because
// the function call arguments are mapped to CPU registers.
struct Op {
    f: fn(code: &[Op], imm: &[u8], ip: usize, mem: &mut [u8], sp: usize, i: i32, f: f32),
}

fn read4(mem: &[u8], addr: usize) -> [u8; 4] {
    assert!(addr + 3 < mem.len());
    [mem[addr], mem[addr + 1], mem[addr + 2], mem[addr + 3]]
}

fn write4(mem: &mut [u8], addr: usize, word: [u8; 4]) {
    assert!(addr + 3 < mem.len());
    mem[addr] = word[0];
    mem[addr + 1] = word[1];
    mem[addr + 2] = word[2];
    mem[addr + 3] = word[3];
}

fn i_add(code: &[Op], imm: &[u8], ip: usize, mem: &mut [u8], sp: usize, i: i32, f: f32) {
    let x = i32::from_ne_bytes(read4(mem, sp));
    (code[ip + 1].f)(code, imm, ip + 1, mem, sp, i + x, f);
}

fn f_add(code: &[Op], imm: &[u8], ip: usize, mem: &mut [u8], sp: usize, i: i32, f: f32) {
    let x = f32::from_ne_bytes(read4(mem, sp));
    (code[ip + 1].f)(code, imm, ip + 1, mem, sp, i, f + x);
}

fn f_mul(code: &[Op], imm: &[u8], ip: usize, mem: &mut [u8], sp: usize, i: i32, f: f32) {
    let x = f32::from_ne_bytes(read4(mem, sp));
    (code[ip + 1].f)(code, imm, ip + 1, mem, sp, i, f * x);
}

fn f_load(code: &[Op], imm: &[u8], ip: usize, mem: &mut [u8], sp: usize, i: i32, _f: f32) {
    let x = f32::from_ne_bytes(read4(mem, sp));
    (code[ip + 1].f)(code, imm, ip + 1, mem, sp, i, x);
}

fn f_store(code: &[Op], imm: &[u8], ip: usize, mem: &mut [u8], sp: usize, i: i32, f: f32) {
    write4(mem, sp, f.to_ne_bytes());
    (code[ip + 1].f)(code, imm, ip + 1, mem, sp, i, f);
}

fn f_imm(code: &[Op], imm: &[u8], ip: usize, mem: &mut [u8], sp: usize, i: i32, _f: f32) {
    let f = f32::from_ne_bytes(read4(imm, ip * 4));
    (code[ip + 1].f)(code, imm, ip + 1, mem, sp, i, f);
}

fn bz(code: &[Op], imm: &[u8], ip: usize, mem: &mut [u8], sp: usize, i: i32, f: f32) {
    let ip = if i == 0 {
        u32::from_ne_bytes(read4(imm, ip * 4)) as usize
    } else {
        ip + 1
    };
    (code[ip].f)(code, imm, ip, mem, sp, i, f);
}

#[derive(Clone, Copy)]
enum Inst {
    Iadd(Reg, Reg, Reg),
    Fadd(Reg, Reg, Reg),
    LoadIntImm(Reg, usize),
    StoreIntImm(Reg, usize),
    LoadFloatImm(Reg, usize),
    StoreFloatImm(Reg, usize),
}

type Reg = u8;

/// On some platforms (iOS) we can't generate machine code, so
/// here's an attempt at a VM.
struct Vm {
    code: Vec<Inst>,
    mem: Vec<u8>,
}

impl Vm {
    fn read4(&self, addr: usize) -> [u8; 4] {
        [
            self.mem[addr],
            self.mem[addr + 1],
            self.mem[addr + 2],
            self.mem[addr + 3],
        ]
    }

    fn write4(&mut self, addr: usize, word: [u8; 4]) {
        self.mem[addr] = word[0];
        self.mem[addr + 1] = word[1];
        self.mem[addr + 2] = word[2];
        self.mem[addr + 3] = word[3];
    }

    fn run(&mut self) {
        let mut ireg = [0 as i32; 256];
        let mut freg = [0.0 as f32; 256];
        let ip = 0 as usize;

        let inst = self.code[ip];

        match inst {
            Inst::Iadd(dst, a, b) => {
                ireg[dst as usize] = ireg[a as usize] + ireg[b as usize];
            }
            Inst::Fadd(dst, a, b) => {
                freg[dst as usize] = freg[a as usize] + freg[b as usize];
            }
            Inst::LoadIntImm(dst, addr) => {
                ireg[dst as usize] = i32::from_ne_bytes(self.read4(addr))
            }
            Inst::StoreIntImm(dst, addr) => {
                self.write4(addr, ireg[dst as usize].to_ne_bytes());
            }
            Inst::LoadFloatImm(dst, addr) => {
                freg[dst as usize] = f32::from_ne_bytes(self.read4(addr))
            }
            Inst::StoreFloatImm(dst, addr) => {
                self.write4(addr, freg[dst as usize].to_ne_bytes());
            }
        }
    }
}
