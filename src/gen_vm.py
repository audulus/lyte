
# Generates a virtual machine with lots of instructions.
# The theory is that if you can use machine registers for
# virtual registers, then it will run faster.
# But it may be too hard on the branch predictor.

# References.
# See https://github.com/wasm3/wasm3/blob/main/docs/Interpreter.md
# More specific to rust: https://pliniker.github.io/post/dispatchers/
# "Nostradamus Distributor": http://www.emulators.com/docs/nx25_nostradamus.htm
# https://stackoverflow.com/questions/59257543/when-is-tail-recursion-guaranteed-in-rust

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

print("enum Inst {")

print("    // Move")
instr2("Mv")
print("    // Integer addition")
instr2("Add")
print("    // Integer subtraction")
instr2("Sub")
print("    // Integer multiplication")
instr2("Mul")
print("    // Float addition")
instr2("FAdd")
print("    // Float subtraction")
instr2("FSub")
print("    // Float multiplication")
instr2("FMul")

print("}")

# Generate interpreter function

def exec2(name, r, op):
    for i in range(regs):
        for j in range(regs):
            if i != j:
                print("            %s%d_%d => { %s%d %s %s%d; ip += 4; }" % (name, i, j, r, i, op, r, j))

print("fn vm() {")
print("    let mut ip = 0;")
for i in range(regs):
    print("    let mut r%d:i32 = 0;" % i)
for i in range(regs):
    print("    let mut f%d:f32 = 0.0;" % i)
print("    loop {")
print("        match op {")
exec2("Add", "r", "+=")
exec2("Sub", "r", "-=")
exec2("Mul", "r", "*=")
exec2("FAdd", "f", "+=")
exec2("FSub", "f", "-=")
exec2("FMul", "f", "*=")
print("        }")
print("    }")
print("}")
