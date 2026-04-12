# Lyte Stack VM

A stack-based virtual machine for Lyte, designed as an alternative to the
register VM (`docs/VM.md`) for platforms where JIT compilation isn't
available (e.g., iOS / Audulus). The Stack VM follows the Silverfir-nano
design: each instruction is a direct-threaded handler function, dispatch
is a `preserve_none` + `musttail` call chain, and hot state (operand
window, spill pointers) stays in hardware registers across the entire
handler chain.

On bare-metal Apple M4, the Stack VM runs biquad at 0.104s, sort at
0.080s, and FFT at 0.369s — within 2.5× of `-O3` C on DSP workloads and
6–10× faster than Lua 5.5.

## Architecture

- **Direct-threaded bytecode.** Each instruction is 32 bytes: a handler
  function pointer plus three 64-bit immediates (`src/stack_interp.h:15`).
  Dispatch never reads an opcode byte — the pc directly holds the
  handler to call.
- **`preserve_none` + `musttail` handlers.** Every op handler is a
  tail-call target with a fixed 13-argument signature on aarch64. The
  compiler keeps the argument values in hardware registers across the
  entire handler chain; transitioning between ops is a single indirect
  branch with zero prologue / epilogue.
- **Parallel TOS register windows.** Four integer slots `t0..t3` live in
  GPRs; four float slots `f0..f3` live in FP/SIMD registers. f32
  arithmetic never crosses between register files. The int window spills
  to `*sp`; the float window spills to `*fsp`. Both spill pointers are
  themselves handler arguments, pinned to GPRs.
- **No hot local register cache.** An earlier version reserved three
  preserve_none GPR args `l0..l2` as a top-3-most-accessed local cache.
  It was deleted — see `docs/HOT_LOCALS.md` for the design exploration
  and why the cache never paid off on this VM.
- **No per-op runtime type check.** The codegen tracks each stack slot's
  type statically, and int vs f32 ops pick the right window at emit
  time — `IAdd` reads `t0/t1`, `FAddF` reads `f0/f1`, and so on. f64
  values ride the int window as bit patterns (rare in hot code).
- **Per-window stack-depth validation.** `src/stack_depth.rs` runs
  forward over each function tracking int and float stack depth
  independently; any jump target or call site that doesn't match
  between incoming edges is a codegen bug.

Distinctive vs the register VM: the register VM has 256 general-purpose
registers and Rust `match` dispatch; the Stack VM has 4+4 TOS slots and
C `musttail` dispatch. The register VM stores temporaries in a flat
register file with no allocation; the Stack VM keeps them in the TOS
window and spills to memory when depth exceeds 4.

## Instruction Set

### Constants (push)

| Opcode | Description |
|--------|-------------|
| `I64Const(i64)` | Push int immediate |
| `F32ConstF(f32)` | Push f32 immediate to float window |
| `F64Const(f64)` | Push f64 bit pattern to int window |

### Local variables

| Opcode | Description |
|--------|-------------|
| `LocalGet(n)` / `LocalSet(n)` / `LocalTee(n)` | Int/f64 local read, write, peek-and-write |
| `LocalGetF(n)` / `LocalSetF(n)` / `LocalTeeF(n)` | f32 local, float window |
| `LocalAddr(n)` | Push address of local slot |

### Integer arithmetic (int window)

| Opcode | Description |
|--------|-------------|
| `IAdd`, `ISub`, `IMul`, `IDiv`, `UDiv`, `IRem`, `IPow` | Binary `t1 <op> t0` |
| `INeg` | Unary `-t0` |
| `And`, `Or`, `Xor`, `Not`, `Shl`, `Shr`, `UShr` | Bitwise |
| `IEq`, `INe`, `ILt`, `ILe`, `IGt`, `IGe`, `ULt`, `UGt` | Comparisons (push 0/1) |

### f32 arithmetic (float window)

All read and write `f0..f3` directly — no GPR↔FP crossings.

| Opcode | Description |
|--------|-------------|
| `FAddF`, `FSubF`, `FMulF`, `FDivF`, `FPowF` | Binary `f1 <op> f0` |
| `FNegF` | Unary `-f0` |
| `FEqF`, `FNeF`, `FLtF`, `FLeF`, `FGtF`, `FGeF` | Comparisons (pop 2 from f, push 0/1 to int window) |
| `SinF32F`, `CosF32F`, `SqrtF32F`, `Exp2F32F`, … | Math intrinsics on `f0` |
| `Atan2F32F` | Binary intrinsic on `f0, f1` |
| `F32ToI32F` / `I32ToF32F` | Window crossings |

There are **no** int-window f32 variants — commit `b818fd1` deleted them
once all measurements showed the F path was strictly shorter and (on
bare metal) strictly faster. f32 values only exist in the float window.

### f64 arithmetic (int window, bit-cast)

| Opcode | Description |
|--------|-------------|
| `DAdd`, `DSub`, `DMul`, `DDiv`, `DPow`, `DNeg` | Binary / unary, bit-cast through the int window |
| `DEq`, `DLt`, `DLe` | Comparisons |
| `SinF64`, …, `SqrtF64`, … | Math intrinsics |

f64 is supported for correctness but not hot-pathed — it pays one
GPR↔FP crossing per op. All measured workloads use f32.

### Memory

| Opcode | Description |
|--------|-------------|
| `Load8`, `Load32`, `Load64`, `Load32Off(off)`, `Load64Off(off)` | Loads (address from int window) |
| `Store8`, `Store32`, `Store64`, `Store32Off(off)`, `Store64Off(off)` | Stores |
| `LoadF32F`, `LoadF32OffF(off)` | f32 loads into the float window |
| `StoreF32F`, `StoreF32OffF(off)` | f32 stores from the float window |
| `MemCopy(n)`, `MemZero(n)`, `MemEq(n)`, `MemNe(n)` | Bulk memory ops for struct/array copies |
| `SliceLoad32`, `SliceStore32`, `SliceEq(n)`, `SliceNe(n)` | Slice access with base+index |

### Control flow

| Opcode | Description |
|--------|-------------|
| `Jump(off)` | Unconditional relative jump |
| `JumpIfZero(off)` / `JumpIfNotZero(off)` | Pop int, branch |
| `Call{func, args, preserve}` | Call by func index |
| `CallIndirect{args}` / `CallClosure{args}` | Indirect / closure call |
| `Return` / `ReturnVoid` | Return (value via `t0` or `f0`) |
| `Halt` | Terminate |

### Debugging

| Opcode | Description |
|--------|-------------|
| `PrintI32`, `PrintF32F`, … | Print value from appropriate window |
| `Assert` | Trap if top is zero |
| `Nop` | No-op (produced by dead-code elimination) |

### Fused superinstructions

Fusion is where the Stack VM earns its performance. See §Fusion below.

## Dispatch

Every handler ends in a `NEXT()` or `DISPATCH()` macro expansion that
`musttail`-returns to the next handler. The caller's stack frame is
freed immediately, and `preserve_none` keeps all the hot state in
registers across the jump.

```c
// src/stack_interp.c — aarch64 linear fall-through
#define NEXT() do { \
    Instruction* _next = pc + 1; \
    void* _new_nh = (_next + 1)->handler; \
    __attribute__((musttail)) return ((Handler)_nh_raw)( \
        ctx, _next, sp, fsp, locals, \
        t0, t1, t2, t3, f0, f1, f2, f3, _new_nh); \
} while(0)
```

`DISPATCH()` is the same shape but recomputes the target handler from
`pc->handler` — used after branches, calls, and returns where the
preloaded `nh` pointer is no longer valid.

### Handler signature (aarch64)

```c
// src/stack_interp.h
typedef PRESERVE_NONE void (*Handler)(
    Ctx*          ctx,            // cold state
    Instruction*  pc,             // instruction pointer
    uint64_t*     sp,             // int spill pointer
    float*        fsp,            // float spill pointer
    uint64_t*     locals,         // frame pointer (aarch64 only)
    uint64_t t0, t1, t2, t3,      // int TOS window
    float f0, f1, f2, f3,         // float TOS window
    void*         nh              // preloaded next handler
);
```

That's 13 arguments; `preserve_none` places all of them in callee-unsaved
GPRs and FP registers, and the entire dispatch loop runs without
touching the hardware stack.

### x86-64

`preserve_none`'s x86-64 GPR budget is tighter (~12 arg registers), so
`locals` and `fsp` get stashed in `ctx->current_locals` and
`ctx->current_fsp` and accessed via `#define` macros. LLVM's TBAA
guarantees let each handler CSE these field loads, so the cost is at
most one extra `mov` per handler entry. The handler source code is
identical on both platforms.

### Preloaded next handler

The `nh` parameter holds the handler function pointer of the **next**
instruction, loaded one dispatch ahead. The `NEXT()` macro branches
to it directly while simultaneously preloading `_new_nh` for the
instruction after that. This pattern (from Silverfir-nano) hides the
load-to-use latency on the handler pointer that a naive
`br pc->handler` would incur.

Unlike the register VM (which tried this same technique and regressed
13% — see `docs/VM.md` §Failed Optimization), the Stack VM **benefits**
from preloading because its handlers are much shorter. A fused Stack VM
handler is typically 5–10 instructions; a register VM handler is 20+.
The load-to-use stall the preload eliminates is a larger fraction of
total dispatch cost when the handler itself is short.

## TOS register windows

### Integer window

Four GPRs `t0..t3` form a 4-slot sliding window, with `t0 = top`. Push
and pop rotate the registers and spill the displaced value to / refill
it from `*sp`:

```c
// src/stack_interp.c:78
#define PUSH(val) do { *sp++ = t3; t3 = t2; t2 = t1; t1 = t0; t0 = (val); } while(0)
#define POP(dst)  do { (dst) = t0; t0 = t1; t1 = t2; t2 = t3; t3 = *--sp; } while(0)
```

For binary ops that pop two and push one, a single `BINOP_SHIFT()` macro
collapses the window shift into one read of `*sp`. `DROP2` / `DROP3`
exist for stores (which pop address + value) and slice stores (which
pop base + index + value).

The rotation is register-register — zero memory traffic until the
logical depth actually exceeds 4. In the common case (arithmetic chains
with depth 2–3), the int spill buffer is never touched.

### Float window

Four FP registers `f0..f3` form a parallel window, typed as `float`:

```c
// src/stack_interp.c:162
#define FPUSH(val) do { *fsp++ = f3; f3 = f2; f2 = f1; f1 = f0; f0 = (val); } while(0)
#define FPOP(dst)  do { (dst) = f0; f0 = f1; f1 = f2; f2 = f3; f3 = *--fsp; } while(0)
```

Two non-obvious choices here, both load-bearing:

1. **Typed `float`, not `double`.** Earlier versions typed the float
   window as `double` to share register slots with f64. That cost two
   `fcvt` round-trips per f32 op (~6 cycles) because LLVM couldn't
   prove the narrow+widen away across the `musttail` boundary. Flipping
   to `float` removes the casts entirely and lets the compiler emit a
   single `fmadd s, s, s, s` on the biquad critical path. Commit
   `c67ed9e`.
2. **`fsp` is a handler argument.** An earlier version kept the float
   spill pointer as `ctx->float_stack_base + ctx->float_sp_off`,
   paying a 3–4 cycle memory chase on every `FPUSH` / `FPOP`. Promoting
   it to a `preserve_none`-pinned handler arg makes `*fsp++` a single
   register-indirect store. Commit `21f2949`.

f32 and f64 coexist on the logical operand stack. The codegen tracks
each slot's type statically; f32 slots live in the float window, f64
slots (rare) ride the int window as bit patterns.

## Hot local cache (removed)

The VM originally reserved three preserve_none GPR args `l0..l2` as a
top-3-most-accessed local cache. The design was flawed in practice —
it turned out to be a no-op that never delivered its intended benefit,
and the attempts to make it work either added runtime dispatch cost
everywhere or exploded into a combinatorial number of specialized
handlers. The cache was deleted; see `docs/HOT_LOCALS.md` for the full
exploration, including the concrete numbers, the bugs that surfaced
along the way, and three alternative directions (deeper TOS window,
scoped specialization, or leaving the VM alone).

## Fusion / Superinstructions

The peephole pass (`src/stack_optimize.rs`) runs three fixed-point
iterations over each function, pattern-matching common op sequences
and replacing them with single fused handlers. Fusion is what lets a
tight biquad FMA chain or an FFT butterfly iteration execute in near
`-O3` C performance.

### Dead-code and tee fusions

| Pattern | Fused result |
|---|---|
| `LocalGet(n) + Drop` | `Nop + Nop` (dead read) |
| `I64Const + Drop` | `Nop + Nop` |
| `LocalTee + Drop` | `LocalSet` |
| `LocalSet(n) + LocalGet(n)` | `LocalTee(n)` |
| `LocalSetF(n) + LocalGetF(n)` | `LocalTeeF(n)` |
| `LocalTee(n) + FusedAddrGetSliceStore32` | `FusedTeeSliceStore32` |
| `LocalTeeF(n) + FusedAddrGetSliceStore32F` | `FusedTeeSliceStore32F` (FFT butterfly) |

### Int-window 3-address fusions

| Pattern | Fused result |
|---|---|
| `LocalGet(a) + LocalGet(b) + IAdd + LocalSet(c)` | `FusedGetGetIAddSet(a, b, c)` — `c = a + b`, no stack traffic |
| `LocalGet(a) + LocalGet(b) + ISub + LocalSet(c)` | `FusedGetGetISubSet(a, b, c)` |
| `LocalGet(a) + LocalGet(b) + IMul + LocalSet(c)` | `FusedGetGetIMulSet(a, b, c)` |
| `LocalGet(s) + I64Const(k) + IAdd + LocalSet(d)` | `FusedGetAddImmSet(s, k, d)` |
| `LocalGet(a) + LocalGet(b) + ILt + JumpIfZero` | `FusedGetGetILtJumpIfZero(a, b, off)` — loop tests |
| `LocalGet(a) + LocalGet(b) + ILt` | `FusedGetGetILt(a, b)` |

### Float-window 3-address fusions

| Pattern | Fused result |
|---|---|
| `LocalGetF(a) + LocalGetF(b) + FAddF` | `FusedGetGetFAddF(a, b)` |
| `LocalGetF(a) + LocalGetF(b) + FMulF` | `FusedGetGetFMulF(a, b)` |
| `LocalGetF(a) + FMulF` | `FusedGetFMulF(a)` — `f0 *= locals[a]` |
| `FusedGetGetFMulF + FAddF` | `FusedFMulFAddF` — `f0 = f2 + f1*f0` (FMA) |
| `FusedGetGetFMulF + FSubF` | `FusedFMulFSubF` (FFT butterfly) |
| `FusedAddrLoad32OffF + FusedGetFMulF + FAddF` | `FusedGetAddrFMulFAddF(a, s, off)` — biquad term |
| `LocalTeeF + FusedAddrGetSliceStore32F` | `FusedTeeSliceStore32F` (FFT output) |

### Slice / struct fusions

| Pattern | Fused result |
|---|---|
| `LocalAddr(s) + Load32Off(o)` | `FusedAddrLoad32Off(s, o)` |
| `LocalAddr(s) + LocalGet(idx) + SliceLoad32` | `FusedAddrGetSliceLoad32(s, idx)` |
| float variant | `FusedAddrGetSliceLoad32F(s, idx)` |
| `LocalArray(s) + Load32` | `FusedLocalArrayLoad32(s)` |

### Representative fused handler

The biquad FMA term — five of these per sample in the inner loop:

```c
// src/stack_interp.c (float-window FMA biquad term)
HANDLER(op_fused_get_addr_fmul_fadd_f) {
    float coeff = *(float*)((uint8_t*)locals + pc->imm[0]);
    float state = *(float*)((uint8_t*)locals + pc->imm[1] * 8 + (int32_t)pc->imm[2]);
    f0 = f0 + coeff * state;
    NEXT();
}
```

This compiles to ten aarch64 instructions total, including dispatch:
two `ldr s` (direct f32 loads), one `fmadd s, s, s, s`, the handler
pointer load, the pc advance, and the final `br`. No GPR↔FP crossings;
the accumulator stays in `s0` across the entire multiply-accumulate
chain.

## Per-window stack-depth tracking

`src/stack_depth.rs` computes, for each instruction, the int and float
stack depth at entry. Two passes:

1. **Forward propagation** with per-op deltas. Every op reports `(int_delta,
   float_delta)` — e.g., `FAddF` is `(0, -1)` (pop 2 f, push 1 f),
   `FEqF` is `(+1, -2)` (pop 2 f, push 1 int), `FusedFMulFAddF` is
   `(0, -2)` (pop 3 f, push 1 f).
2. **Jump target validation**: any label with multiple incoming edges
   must have matching depths on all of them. Mismatches are codegen bugs.

The computed depth feeds the `Call.preserve` field, which tells
`op_call` how many caller values below the arguments must be spilled
into the new frame's saved area before the call proceeds. This is the
only place per-call-site stack metadata escapes from the codegen; at
runtime, the handler chain never computes depths.

## Calling convention

Every function frame is a contiguous `uint64_t[]` allocated from a bump
arena (`ctx->frame_stack`). The frame holds `max(local_count, 3)` scalar
local slots followed by memory-backed locals (structs / fixed arrays
accessed by byte offset). The `locals` pointer points at the start of
the scalars, which is also the frame pointer.

### Arguments

Arguments are in the top of the caller's int window at the call site
(or spilled to `*sp` if there are more than 4). `op_call` copies them
into `new_locals[0..nargs-1]` in the callee's frame. Float arguments
currently travel through the int window as bit patterns — float call
boundaries are rare enough in hot code that the two extra crossings
per call don't dominate. See `FP_CODEGEN_PLAN.md` §6.3.

### Returns

Scalar returns land in `t0` (int / f64 bit pattern) or `f0` (f32). Large
returns (structs, arrays) use an sret-style output pointer: the caller
allocates space in its own frame, pushes the address as a hidden first
argument, and the callee `MemCopy`s the result through it. This matches
the register VM's convention.

### No saved `sp` / `fsp`

`CallFrame` holds only `return_pc`, `saved_locals`, `func_idx`, and
`saved_frame_size` — no `saved_sp`, no `saved_fsp`. Balanced push/pop
discipline means both spill pointers naturally end up at the value the
caller expects by the time control returns. Dropping these fields saved
one store per call and 16 bytes per frame. Commits `bc44a11`, `b999162`.

## Codegen pipeline

```
AST  →  stack_codegen::FunctionTranslator  →  StackFunction (raw ops)
          │
          ▼
      stack_inline::inline_trivial      (inline leaf functions)
          │
          ▼
      stack_rebase_lm::rebase           (shift memory-slot refs by local_count)
          │
          ▼
      stack_optimize::optimize          (3-pass fusion peephole)
          │
          ▼
      stack_rebase_lm::patch_call_preserve  (fill Call.preserve from stack depth)
          │
          ▼
      stack_interp_bridge::encode       (StackOp → Instruction[])
          │
          ▼
      stack_interp_run (C entry point)  (run the handler chain)
```

The bridge does one more optimization worth calling out: it
**pre-shifts f32 local indices into byte offsets** on the way into the
C representation. aarch64's scaled f32 load is `ldr s, [base, idx, lsl
#2]` (stride 4), but `locals[]` is a `uint64_t` array (stride 8). By
pre-shifting the index to `idx * 8` in the bridge, the handler emits
`ldr s, [locals, byte_off]` with no runtime scale — one fewer `lsl`
per hot-path f32 local access. Commit `b40ecc1`.

## Benchmarks

10-run median wall time on Apple M4 (Mac16,13, macOS 26.4), full
pipeline (codegen + fusion + execution):

| Workload | Stack VM | Register VM | C `-O3` | Lua 5.5 |
|---|---|---|---|---|
| Biquad (10M samples, 1 kHz LPF) | **0.104 s** | 0.265 s | ~0.040 s | ~0.6 s |
| Sort (10K × 50 iterations)      | **0.080 s** | 0.123 s | ~0.035 s | ~0.8 s |
| FFT (1024-pt × 2000 iterations) | **0.369 s** | 0.653 s | ~0.140 s | ~2.5 s |

The Stack VM is 2.5–3× faster than the register VM on DSP workloads and
comes within 2.5–3× of `-O3` C — on a pure interpreter with no JIT.

Why: the hot ops (biquad FMA terms, FFT butterflies, tight loop tests)
fuse to single handlers, each 5–10 aarch64 instructions including
dispatch. The register VM's equivalent ops are 20+ instructions because
each handler pays Rust-`match` dispatch overhead and a heavier
register-file access pattern.

See `docs/FP_CODEGEN_PLAN.md` for the float-codegen history and the
bare-metal-vs-VM benchmark post-mortem.

## Implementation notes

### Incremental optimizations that mattered

A number of changes that each look minor delivered 10–25% on one or
more benchmarks. Listed in rough order of impact:

1. **Float window typed as `float`, not `double`** (`c67ed9e`). Removed
   2× `fcvt` per f32 op. Biquad FMA critical path dropped from ~12
   cycles per op to ~3.
2. **`fsp` promoted to a handler argument** (`21f2949`). `FPUSH` /
   `FPOP` went from chasing memory through `ctx` to a single
   `str s, [x23], #0x4`. Biquad −17%, FFT −52% under `--fp-window`.
3. **Direct f32 stores** (`f2a9c95`). `locals[n] = from_f32(f0)` was
   compiling to `fmov w, s; str x` (crossing + 8-byte store);
   rewriting as `*(float*)(locals + n) = f0` emits one `str s`.
4. **Pre-shifted f32 local indices** (`b40ecc1`). Eliminated one `lsl`
   per hot f32 local access (see §Codegen pipeline).
5. **Dropped `saved_sp` / `saved_fsp`** (`bc44a11`, `b999162`). Balanced
   stack discipline made them redundant. 16 bytes / one store saved
   per call.
6. **Deleted the int-window f32 path** (`b818fd1`). After all the fixes
   above, the F path was both simpler and faster. Removed ~1000 lines
   of dual-codegen complexity.

### Safety

Handlers use raw pointer arithmetic throughout — array bounds checking
is the Rust-side compiler's job (`src/safety_checker.rs`). The C
interpreter trusts the bytecode it executes. The FFI boundary is
`stack_interp_run(ctx)`, where `ctx` must be fully populated before
the call (operand stack, float stack, frame stack, function table,
call stack). No heap allocation happens during execution — the Stack
VM is safe to call from a real-time audio thread.

### Limitations

- **f64 is second-class.** f64 values ride the int window, pay GPR↔FP
  crossings on every op. Fine for correctness tests, not hot-pathed.
- **No SIMD vector types in the VM.** `f32x4` lowers to per-lane
  scalar f32 ops via fused loads/stores. A proper SIMD window would
  need another register tier.
- **Fixed 4-slot TOS windows.** Deeper operand chains spill to memory;
  analyses that need many live temporaries pay the spill cost. Works
  out OK in practice because fusion collapses most chains before they
  get that deep.
- **16-argument preserve_none signature** is at the edge of what's
  portable. aarch64 and x86-64 are supported; other targets would
  need a handler-signature audit.

### References

- **Silverfir-nano interpreter design** — `INTERPRETER_DESIGN.md` in
  the Silverfir-nano repo. The `preserve_none` + `musttail` +
  preloaded-next-handler dispatch pattern is from there.
- **`docs/FP_CODEGEN_PLAN.md`** — history of the float-window refactor
  and the full benchmark post-mortem, including the bare-metal-vs-VM
  FFT finding.
- **`docs/VM.md`** — the register VM. Useful as a contrast and to
  understand why the Stack VM's dispatch technique works here but
  failed on the register VM (§Failed Optimization).
