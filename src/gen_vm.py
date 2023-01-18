
# Generates a virtual machine with lots of instructions.
# The theory is that if you can use machine registers for
# virtual registers, then it will run faster.
# But it may be too hard on the branch predictor.

# References.
# See https://github.com/wasm3/wasm3/blob/main/docs/Interpreter.md
# More specific to rust: https://pliniker.github.io/post/dispatchers/
# "Nostradamus Distributor": http://www.emulators.com/docs/nx25_nostradamus.htm
# https://stackoverflow.com/questions/59257543/when-is-tail-recursion-guaranteed-in-rust
# https://news.ycombinator.com/item?id=34417543

preamble = """
// Generated by gen_vm.py

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

fn cmp(a: i32, b: i32) -> i32 {
    if a < b { 1 } else { 0 }
}
"""

print(preamble)

regs = 8

def instr2(name):
    for i in range(regs):
        for j in range(regs):
            if i != j:
                print("    %s%d_%d," % (name, i, j))

def instr3(name):
    for i in range(regs):
        for j in range(regs):
            for k in range(regs):
                if i != j:
                    print("    %s%d_%d_%d," % (name, k, i, j))

# Generate instruction enum

print("#[derive(Clone, Copy)]")
print("pub enum Inst {")

print("    // Move")
instr2("Mv")
print("    // Integer addition")
instr2("Add")
instr3("Add")
print("    // Integer subtraction")
instr2("Sub")
instr3("Sub")
print("    // Integer multiplication")
instr2("Mul")
instr3("Mul")
print("    // Float addition")
instr2("FAdd")
instr3("FAdd")
print("    // Float subtraction")
instr2("FSub")
instr3("FSub")
print("    // Float multiplication")
instr2("FMul")
instr3("FMul")
print("    // Integer comparison")
instr2("Cmp")
print("    // Integer load")
instr2("Ld")

print("}")

# Generate interpreter function

def exec2(name, r, op):
    for i in range(regs):
        for j in range(regs):
            if i != j:
                print("            Inst::%s%d_%d => { %s%d %s %s%d; ip += 1; }" % (name, i, j, r, i, op, r, j))

def exec3(name, r, op):
    for i in range(regs):
        for j in range(regs):
            for k in range(regs):
                print("            Inst::%s%d_%d_%d => { %s%d = %s%d %s %s%d; ip += 1; }" % (name, i, j, k, r, i, r, j, op, r, k))

def cmp():
    for i in range(regs):
        for j in range(regs):
            if i != j:
                print("            Inst::Cmp%d_%d => { flags = cmp(r%d, r%d); ip += 1; }" % (i, j, i, j))

def ld():
    for i in range(regs):
        for j in range(regs):
            if i != j:
                print("            Inst::Ld%d_%d => { r%d = i32::from_ne_bytes(read4(mem, r%d as usize)); ip += 1; }" % (i, j, i, j))

print("pub fn vm(code: &[Inst], mem: &mut [u8]) {")
print("    let mut ip:i32 = 0;")
print("    let mut flags:i32 = 0;")
for i in range(regs):
    print("    let mut r%d:i32 = 0;" % i)
for i in range(regs):
    print("    let mut f%d:f32 = 0.0;" % i)
print("    loop {")
print("        match code[ip as usize] {")
exec2("Mv", "r", "=")
exec2("Add", "r", "+=")
exec3("Add", "r", "+")
exec2("Sub", "r", "-=")
exec3("Sub", "r", "-")
exec2("Mul", "r", "*=")
exec3("Mul", "r", "*")
exec2("FAdd", "f", "+=")
exec3("FAdd", "f", "+")
exec2("FSub", "f", "-=")
exec3("FSub", "f", "-")
exec2("FMul", "f", "*=")
exec3("FMul", "f", "*")
cmp()
ld()
print("        }")
print("    }")
print("}")
