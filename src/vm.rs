
// See https://github.com/wasm3/wasm3/blob/main/docs/Interpreter.md

type Reg = u8;

#[derive(Clone, Copy)]
enum Inst {
    Iadd(Reg, Reg, Reg),
    Fadd(Reg, Reg, Reg),
    LoadIntImm(Reg, usize),
    StoreIntImm(Reg, usize),
    LoadFloatImm(Reg, usize),
    StoreFloatImm(Reg, usize),
}

/// On some platforms (iOS) we can't generate machine code, so 
/// here's an attempt at a VM.
struct Vm {
    code: Vec<Inst>,
    mem: Vec<u8>,
    ireg: [i32; 256],
    freg: [f32; 256],
    ip: usize,
}

impl Vm {

    fn read4(&self, addr: usize) -> [u8; 4] {
        [self.mem[addr], self.mem[addr+1], self.mem[addr+2], self.mem[addr+3]]
    }

    fn write4(&mut self, addr: usize, word: [u8; 4]) {
        self.mem[addr] = word[0];
        self.mem[addr+1] = word[1];
        self.mem[addr+2] = word[2];
        self.mem[addr+3] = word[3];
    }
    
    fn run(&mut self) {

        let inst = self.code[self.ip];

        match inst {
            Inst::Iadd(dst, a, b) => {
                self.ireg[dst as usize] = self.ireg[a as usize] + self.ireg[b as usize];
            }
            Inst::Fadd(dst, a, b) => {
                self.freg[dst as usize] = self.freg[a as usize] + self.freg[b as usize];
            }
            Inst::LoadIntImm(dst, addr) => {
                self.ireg[dst as usize] = i32::from_ne_bytes(self.read4(addr))
            }
            Inst::StoreIntImm(dst, addr) => {
                self.write4(addr,  self.ireg[dst as usize].to_ne_bytes());
            }
            Inst::LoadFloatImm(dst, addr) => {
                self.freg[dst as usize] = f32::from_ne_bytes(self.read4(addr))
            }
            Inst::StoreFloatImm(dst, addr) => {
                self.write4(addr,  self.freg[dst as usize].to_ne_bytes());
            }
        }
    }
}
