
# Generates a virtual machine with lots of instructions.
# The theory is that if you can use machine registers for
# virtual registers, then it will run faster.
# But it may be too hard on the branch predictor.

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

def exec2(name, op):
    for i in range(regs):
        for j in range(regs):
            if i != j:
                print("            %s%d_%d => { r%d %s r%d; ip += 4; }" % (name, i, j, i, op, j))

print("fn vm() {")
for i in range(regs):
    print("    let mut r%d:i32 = 0;" % i)
for i in range(regs):
    print("    let mut f%d:f32 = 0.0;" % i)
print("    loop {")
print("        match op {")
exec2("Add", "+=")
print("        }")
print("    }")
print("}")
