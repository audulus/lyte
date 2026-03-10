# Lyte VM

A register-based virtual machine for Lyte, designed for platforms where JIT compilation isn't available (e.g., iOS).

## Architecture

- **256 general-purpose 64-bit registers** (r0-r255)
- **Stack-based locals** with per-frame allocation
- **Typed opcodes** - separate instructions for i32/i64/f32/f64 (no runtime type checks)
- **Direct threading** via Rust `match` dispatch

Values are stored as raw 64-bit in registers and interpreted based on the instruction type. This matches Lyte's static typing - the compiler knows types at compile time, so the VM doesn't need to.

## Instruction Set

### Register Operations

| Opcode | Description |
|--------|-------------|
| `Move { dst, src }` | `dst = src` |
| `LoadImm { dst, value }` | Load i64 immediate |
| `LoadF32 { dst, value }` | Load f32 immediate |
| `LoadF64 { dst, value }` | Load f64 immediate |
| `LoadConst { dst, idx }` | Load from constant pool |

### Integer Arithmetic

| Opcode | Description |
|--------|-------------|
| `IAdd { dst, a, b }` | `dst = a + b` |
| `ISub { dst, a, b }` | `dst = a - b` |
| `IMul { dst, a, b }` | `dst = a * b` |
| `IDiv { dst, a, b }` | `dst = a / b` (signed) |
| `UDiv { dst, a, b }` | `dst = a / b` (unsigned) |
| `IRem { dst, a, b }` | `dst = a % b` |
| `INeg { dst, src }` | `dst = -src` |
| `IAddImm { dst, src, imm }` | `dst = src + imm` (superinstruction) |

### Float32 Arithmetic

| Opcode | Description |
|--------|-------------|
| `FAdd { dst, a, b }` | `dst = a + b` |
| `FSub { dst, a, b }` | `dst = a - b` |
| `FMul { dst, a, b }` | `dst = a * b` |
| `FDiv { dst, a, b }` | `dst = a / b` |
| `FNeg { dst, src }` | `dst = -src` |
| `FMulAdd { dst, a, b, c }` | `dst = a * b + c` (superinstruction, uses FMA) |
| `FMulSub { dst, a, b, c }` | `dst = a * b - c` (superinstruction, uses FMA) |

### Float64 Arithmetic

| Opcode | Description |
|--------|-------------|
| `DAdd { dst, a, b }` | `dst = a + b` |
| `DSub { dst, a, b }` | `dst = a - b` |
| `DMul { dst, a, b }` | `dst = a * b` |
| `DDiv { dst, a, b }` | `dst = a / b` |
| `DNeg { dst, src }` | `dst = -src` |
| `DMulAdd { dst, a, b, c }` | `dst = a * b + c` (superinstruction) |
| `DMulSub { dst, a, b, c }` | `dst = a * b - c` (superinstruction) |

### Bitwise Operations

| Opcode | Description |
|--------|-------------|
| `And { dst, a, b }` | `dst = a & b` |
| `Or { dst, a, b }` | `dst = a \| b` |
| `Xor { dst, a, b }` | `dst = a ^ b` |
| `Not { dst, src }` | `dst = !src` |
| `Shl { dst, a, b }` | `dst = a << b` |
| `Shr { dst, a, b }` | `dst = a >> b` (arithmetic) |
| `UShr { dst, a, b }` | `dst = a >>> b` (logical) |

### Comparisons

All comparisons produce 0 (false) or 1 (true).

| Opcode | Description |
|--------|-------------|
| `IEq / INe / ILt / ILe / IGt / IGe` | Signed integer comparisons |
| `ULt / UGt` | Unsigned integer comparisons |
| `FEq / FNe / FLt / FLe / FGt / FGe` | Float32 comparisons |
| `DEq / DLt / DLe` | Float64 comparisons |

### Type Conversions

| Opcode | Description |
|--------|-------------|
| `I32ToF32 / F32ToI32` | i32 <-> f32 |
| `I32ToF64 / F64ToI32` | i32 <-> f64 |
| `F32ToF64 / F64ToF32` | f32 <-> f64 |

### Memory Operations

| Opcode | Description |
|--------|-------------|
| `Load8 / Load32 / Load64` | Load from address in register |
| `Load32Off / Load64Off` | Load with immediate offset |
| `Store8 / Store32 / Store64` | Store to address in register |
| `Store32Off / Store64Off` | Store with immediate offset |
| `LocalAddr { dst, slot }` | Get address of local variable slot |
| `MemCopy { dst, src, size }` | Copy bytes |
| `MemZero { dst, size }` | Zero memory |

### Control Flow

| Opcode | Description |
|--------|-------------|
| `Jump { offset }` | Unconditional jump |
| `JumpIfZero { cond, offset }` | Jump if register is 0 |
| `JumpIfNotZero { cond, offset }` | Jump if register is non-zero |
| `ILtJump { a, b, offset }` | Jump if `!(a < b)` (superinstruction) |
| `IGeJump { a, b, offset }` | Jump if `!(a >= b)` (superinstruction) |
| `Call { func, args_start, arg_count }` | Call function by index |
| `CallIndirect { func_reg, args_start, arg_count }` | Call function pointer |
| `Return` | Return (value in r0) |
| `ReturnReg { src }` | Return value from specific register |

### Stack Frame

| Opcode | Description |
|--------|-------------|
| `AllocLocals { size }` | Allocate stack space (bytes) |

### Debugging

| Opcode | Description |
|--------|-------------|
| `PrintI32 / PrintF32` | Print value |
| `Assert { src }` | Panic if zero |
| `Nop` | No operation |
| `Halt` | Stop execution |

## Superinstructions

Superinstructions reduce dispatch overhead by combining common instruction sequences:

### FMulAdd / FMulSub
Fused multiply-add operations that map to hardware FMA instructions when available. Essential for DSP code like filters.

```
// Without superinstruction (2 dispatches):
FMul { dst: 1, a: 2, b: 3 }
FAdd { dst: 0, a: 0, b: 1 }

// With superinstruction (1 dispatch):
FMulAdd { dst: 0, a: 2, b: 3, c: 0 }
```

### IAddImm
Combines load-immediate and add for loop counters.

```
// Without superinstruction (2 dispatches):
LoadImm { dst: 1, value: 1 }
IAdd { dst: 0, a: 0, b: 1 }

// With superinstruction (1 dispatch):
IAddImm { dst: 0, src: 0, imm: 1 }
```

### ILtJump / IGeJump
Fused compare-and-branch for loop conditions.

```
// Without superinstruction (2 dispatches):
ILt { dst: 1, a: 0, b: 2 }
JumpIfZero { cond: 1, offset: 5 }

// With superinstruction (1 dispatch):
ILtJump { a: 0, b: 2, offset: 5 }
```

## Calling Convention

- Arguments passed in registers starting at `args_start`
- Return value in r0
- Callee-saved: none (registers are shared)
- Stack frame allocated via `AllocLocals`

## Usage

```rust
use lyte::vm::{VM, VMProgram, VMFunction, Opcode};

// Create a function
let mut func = VMFunction::new("add");
func.param_count = 2;
func.emit(Opcode::IAdd { dst: 0, a: 0, b: 1 });
func.emit(Opcode::Return);

// Create a program
let mut program = VMProgram::new();
program.entry = program.add_function(func);

// Run it
let mut vm = VM::new();
let result = vm.run(&program);
```

## Benchmark

The VM includes a biquad filter benchmark that processes 10,000 samples:

```rust
use lyte::vm::run_biquad_benchmark;

let result = run_biquad_benchmark(1000);
println!("{}", result);
```

A biquad filter computes:
```
y[n] = b0*x[n] + b1*x[n-1] + b2*x[n-2] - a1*y[n-1] - a2*y[n-2]
```

This is a representative DSP workload that exercises:
- Floating-point multiply-add chains
- Memory loads/stores for filter state
- Function calls
- Tight loops with compare-and-branch

## Implementation Notes

### Performance Considerations

1. **Dispatch overhead**: The main loop uses a `match` statement. On modern CPUs with good branch prediction, this is reasonably efficient. For better performance, computed goto (not available in safe Rust) or a tracing JIT would be needed.

2. **Register access**: All registers are in a contiguous array, enabling efficient indexing.

3. **Memory**: Uses raw pointers for memory operations. The VM doesn't do bounds checking - that's the compiler's job.

4. **FMA**: The `FMulAdd`/`FMulSub` instructions use Rust's `mul_add()` which maps to hardware FMA when available, providing both speed and better numerical accuracy.

### Safety

The VM uses `unsafe` for memory operations. This is intentional - Lyte's type system and array bounds checker ensure safety at compile time. The VM trusts the bytecode it executes.
