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

### Registers

All functions share a single flat register file (r0–r255). There is no per-function register scope — a callee's writes are visible to the caller.

**r0 is the return value register.** Every `Call` instruction clobbers r0 (the callee's return value lands there), so no variable should live in r0 across a call. The codegen reserves r0 even for 0-parameter functions, and the optimizer refuses to forward or coalesce into r0.

### Scalar Returns (r0)

For types that fit in a register (integers, floats, booleans), the callee moves the result into r0 before returning:

```
Move { dst: 0, src: result }      // put result in r0
RestoreRegs { start_reg: 1, ... } // restore r1..rN (skip r0)
Return
```

`RestoreRegs` starts at r1 so r0 keeps the return value.

### Large Returns (output pointer)

Structs, arrays, tuples, and slices are returned via a hidden output pointer — the same sret convention used by C/LLVM. The caller allocates space and passes a pointer as an extra first argument (r0). The callee writes the result via `MemCopy` and restores all registers including r0:

```
// Caller side:
LocalAddr { dst: 4, slot: 5 }          // r4 = &temp_space
Move { dst: 5, src: 1 }                // shift visible args up
Call { func: 1, args_start: 4, ... }   // r4 (output ptr) becomes r0 in callee

// Callee side:
SaveRegs { start_reg: 0, count: N, ... }  // save all regs including r0
Store64 { addr: slot, src: 0 }             // save output ptr to local slot
...                                        // compute result
Load64 { dst: tmp, addr: slot }            // reload output ptr (r0 was clobbered)
MemCopy { dst: tmp, src: result, size: S } // write result through pointer
RestoreRegs { start_reg: 0, ... }          // restore ALL regs (no r0 return value)
Return
```

The output pointer is saved to a local slot immediately on entry because subcalls will clobber r0.

### SaveRegs / RestoreRegs

The VM uses a callee-save convention implemented with `SaveRegs` and `RestoreRegs`. On function entry, all registers the function will touch are saved to the locals area. On exit, they are restored — minus r0 for scalar returns.

```
SaveRegs { start_reg: 0, count: N, slot: S }
// Saves registers [start_reg .. start_reg+count) to locals at byte offset S

RestoreRegs { start_reg: 1, count: M, slot: S+8 }
// Restores registers [1 .. 1+M) from locals at byte offset S+8
// start_reg=1 skips r0 (scalar return); start_reg=0 restores everything (pointer return)
```

The save area is placed after all local variable slots. Its size and offset are patched after translation, once the final register count is known.

### Call Mechanics

`Call { func, args_start, arg_count }` shifts registers from `[args_start .. args_start+arg_count)` down to `[r0 .. r0+arg_count)`, pushes a call frame, and jumps to the target function. `Return` pops the frame and resumes the caller.

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

### ARM64 Backend

The ARM64 assembly backend (`vm_arm64.S`) replaces the Rust `match` dispatch with hand-written assembly. Eight callee-saved registers are pinned to hot VM state (ops, regs, ip, locals_base, locals_ptr, globals_ptr, jump table, ctx), so the entire dispatch loop runs without touching the stack frame.

Each handler ends with an inline `next` macro — a 5-instruction threaded dispatch sequence:

```asm
ldr     w16, [x19, x21, lsl #2]    // load ops[ip]
add     x21, x21, #1               // ip++
and     w17, w16, #0xff            // extract tag
ldr     x17, [x25, x17, lsl #3]   // handler = jump_table[tag]
br      x17                        // branch to handler
```

Because each handler has its own indirect branch instruction, the CPU's branch target buffer can learn per-handler patterns rather than thrashing a single shared dispatch point.

### Failed Optimization: Next-Handler Preloading

Silverfir-nano (a WebAssembly interpreter) achieves near-JIT performance partly through next-handler preloading: each handler receives the next handler's function pointer pre-loaded by the previous handler, eliminating the load-to-use stall between fetching the handler pointer and branching to it. See `INTERPRETER_DESIGN.md` in their repo for details.

We attempted this for Lyte's ARM64 backend. The idea: dedicate x27 to hold a preloaded handler pointer. The `next` macro branches to the preloaded pointer immediately (no stall) while simultaneously preloading x27 for the handler after that. Non-linear handlers (jumps, calls, returns, multi-word instructions) use a `next_reload` variant that bootstraps the preload chain from scratch.

The result was a consistent ~13% regression across all benchmarks:

| Benchmark | Before | After | Change |
|-----------|--------|-------|--------|
| Biquad | 0.265s | 0.296s | +12% slower |
| Sort | 0.123s | 0.139s | +13% slower |
| FFT | 0.653s | 0.738s | +13% slower |

**Why it failed:**

1. **Dispatch grew from 5 to 7 instructions.** The preloading `next` macro adds a peek load and a register move, increasing per-dispatch overhead by 40%. Silverfir-nano's design amortizes this because its handlers have near-zero prologue/epilogue overhead (`preserve_none` + `musttail`), so the stall is a larger fraction of total cost. Lyte's register-machine handlers are bulkier (register file loads and stores), giving the CPU's out-of-order engine enough work to hide the stall naturally.

2. **Apple M-series branch prediction is excellent.** Profiling shows >95% indirect branch prediction success with threaded dispatch. The CPU speculatively executes through the predicted target, overlapping the handler pointer load with useful work. The preload eliminates a stall that barely exists in practice.

3. **Architectural mismatch.** The technique is designed for stack machines with tiny fused handlers (1-5 instructions of real work). In a register machine where each handler does 5-10 register file accesses, the load-to-use latency is already hidden by the handler body itself.
