# Float-Window Codegen Refactor Plan

A plan for moving the Stack VM from "f32 values bit-cast through the integer
TOS window" to "f32 values live in a dedicated float TOS window (f0..f3) in
FP/SIMD registers throughout expression evaluation."

> **Implementation status** (updated after commit `b818fd1` and a
> bare-metal post-mortem)
>
> The original plan's goal — a single uniform F-variant codegen path,
> with the legacy int-window f32 ops deleted — **is now in place**
> and the FFT regression that motivated §13 of this document turned
> out to be a virtualization artifact: on bare-metal Apple M4, the F
> path is actually ~6.5% *faster* than the int path on FFT, not
> 25% slower. See §13 for the measurements and the post-mortem.
>
> Six unplanned fixes (§12) progressively improved both paths' f32
> performance to the point where biquad, sort, and FFT are all at or
> above plan target. The FFT investigation recorded in §13.1–§13.3
> (BTB aliasing, NEON forwarding, store buffer, etc.) was chasing a
> phantom: everything that looked like a regression inside the VM
> the plan was developed in, looks like a small win on real hardware,
> with measurements that exactly match the theoretical analysis of
> per-handler instruction counts.
>
> **The six unplanned fixes** that came before the deletion:
>
> 1. **`fsp` register-arg promotion** (`21f2949`). `FPUSH`/`FDROP` used
>    to chase `ctx->float_stack` and `ctx->float_sp_off` through memory
>    on every call; promoting the float spill pointer to a
>    `preserve_none`-pinned handler argument made it as cheap as the
>    integer `*sp++`.
>
> 2. **Float-typed window** (`c67ed9e`). §6.1 typed the float TOS
>    window as `double` so f64 could share the register file. Every
>    f32 op paid two `fcvt` round-trips because LLVM couldn't prove
>    the narrow+widen away across the `musttail` dispatch boundary.
>    Flipping to `float` collapsed the biquad FMA critical path from
>    ~12 cycles per op to 3.
>
> 3. **`saved_sp` / `saved_fsp` removal** (`bc44a11`, `b999162`). Both
>    fields were written on every call and never read — balanced stack
>    discipline leaves `sp` and `fsp` at the value the caller expects.
>    One store eliminated per non-tail call, 16 bytes of `CallFrame`
>    shrinkage.
>
> 4. **Direct f32 stores + tee fusions** (`f2a9c95`). F handlers writing
>    `locals[n] = from_f32(f0)` compiled to `fmov w, s; str x`;
>    rewriting as `*(float*)(locals + n) = f0` emits one `str s`. Also
>    added `FusedTeeSliceStore32F` and the `LocalSet+LocalGet→LocalTee`
>    F peephole.
>
> 5. **Pre-shifted f32 local indices** (`b40ecc1`). aarch64's indexed
>    f32 load only supports `lsl #2`, but `locals[]` has stride 8.
>    Encoding the local index as a pre-shifted byte offset in the
>    bridge lets the handler emit `ldr s, [locals, byte_off]` with no
>    scale, saving one instruction per handler call.
>
> 6. **Delete the int f32 path** (`b818fd1`). Remove all int-window
>    f32 handlers, StackOp variants, the `float_window_rewrite`
>    peephole pass, the int-f32 fusion rules, `emit_float`, the
>    `stack_fp_window` flag, and the `--fp-window` CLI. Net:
>    +243 / −1246 lines. Leaves F as the only path for f32 arithmetic.
>
> Current benchmark state. Two rows, because the VM and bare-metal
> numbers diverge sharply on FFT: VM numbers are 10-run medians via
> `benchmark/run.sh` on aarch64 M-series inside a Linux VM (the
> environment the refactor was developed in); bare-metal numbers are
> 10-run medians via a Python harness on Apple M4 macOS (Mac16,13,
> sysctl `Apple M4`, macOS 26.4). Both binaries: release build of
> `67eb53c` (pre-deletion int-path default) and HEAD (post-deletion
> F-only).
>
> | Workload | Start (`5b8c48f`) | `b40ecc1` int default (VM) | `b818fd1` F-only (VM) | `b40ecc1` int default (M4) | HEAD F-only (M4) | Plan target |
> |---|---|---|---|---|---|---|
> | biquad | 0.218s | 0.100s | 0.102s | 0.107s | **0.104s** | 0.15–0.18s ✅ |
> | sort   | 0.114s | 0.074s | 0.074s | 0.078s | **0.080s** | unchanged ✅ |
> | fft    | 0.612s | 0.395s | 0.497s | 0.395s | **0.369s** | 0.35–0.45s ✅ |
>
> On bare-metal M4 every workload meets or beats its plan target. FFT
> in particular is **25 ms faster** under F-only than under the int
> default — the opposite of what the VM reported. The 25% FFT
> "regression" documented in the earlier version of §13 of this file
> was a virtualization artifact.
>
> Phase status:
>
> | Phase                                 | Status                  | Commit(s)            |
> |---------------------------------------|-------------------------|----------------------|
> | 1 — F-variant op scaffold             | ✅ landed               | `2be383d`            |
> | 2 — `emit_float` helper               | ✅ landed, then deleted | `4a366ae`, `b818fd1` |
> | 3.1 — Direct f32 op sites             | ✅ landed               | `9d2e0bc`            |
> | 3.2 — Type-dependent sites + CLI      | ✅ landed               | `f9e27a8`            |
> | 3.3 — Memory/slice/assign-temp sites  | ✅ landed               | `cf44a00`            |
> | 4 — Flip default on                   | ✅ landed via #6        | `b818fd1`            |
> | 5 — Float-aware fusion patterns       | ✅ landed, integrated   | `4b6f00e`, `f2a9c95` |
> | 6 — Remove legacy FW path             | ✅ landed               | `b818fd1`            |
> | 7 — Float hot local cache             | ⏸ skipped              |                      |
> | **`fsp` register-arg promotion**      | ✅ unplanned            | `21f2949`            |
> | **Float-typed window**                | ✅ unplanned            | `c67ed9e`            |
> | **x86-64 `fsp` in ctx + min/max fix** | ✅ unplanned            | `821af8d`            |
> | **`saved_sp` / `saved_fsp` removal**  | ✅ unplanned            | `bc44a11`, `b999162` |
> | **Direct f32 stores + tee fusions**   | ✅ unplanned            | `f2a9c95`            |
> | **Pre-shifted f32 local indices**     | ✅ unplanned            | `b40ecc1`            |
>
> Every phase is either landed or explicitly skipped. F variants are
> the only path for f32 arithmetic; the `--fp-window` / `stack_fp_window`
> flag is gone because there's no alternative to pick anymore. The FFT
> gap that §13 was created to diagnose was a VM artifact and does not
> exist on bare metal — §13 now documents the post-mortem rather than
> an open investigation.

## 1. Background

### Current state (commit 5b8c48f)

The TOS window has four slots, `t0..t3`, declared as `uint64_t` handler
parameters. `preserve_none` pins them to specific GPRs across the entire
musttail-dispatched handler chain. Integer values ride in GPRs end-to-end.
Float values are stored as their IEEE-754 bit pattern in the same `uint64_t`
slots and reinterpreted per-op (`as_f32`, `from_f32` memcpy helpers).

A float TOS window was added alongside the integer one: `f0..f3` as `double`
handler parameters living in the FP register file (v0-v3 on aarch64, xmm0-xmm3
on x86-64). A separate `ctx->float_stack` backs spills. The infrastructure
works and a narrow peephole pass (`float_window_rewrite`) rewrites the biquad
FMA chain into FW variants, giving a measured 15% speedup on biquad.

### Problem

Every float arithmetic op that consumes a TOS value pays ~6 cycles of
GPR↔FP crossings per op:

```
fmov s_tmp, w26          ; ~3 cycles (t0 GPR → FP)
fmul s_result, s_a, s_tmp ; ~4 cycles (the actual work)
fmov w26, s_result        ; ~3 cycles (FP → GPR)
```

The peephole workaround only catches hand-matched patterns. LLVM's compiled
code, by contrast, tracks types at codegen time and emits FP instructions
directly — so scalar f32 values never visit a GPR. The Stack VM should do
the same: at codegen time, when translating an expression of type `f32`,
emit FP-window ops directly.

### Previous attempts

A first attempt (unsuccessful, reverted) tried to flip the codegen in a
single pass. It compiled but crashed at runtime because handlers that
consume f32 values (comparisons, `F32ToI32`, `SinF32`, `Drop`, `LocalTee`)
still expected their operands in the integer window, leading to mismatches
like `f32.mul_f` writing to `f0` followed by `f32.sin` reading from `t0`.

The lesson: this has to be done incrementally, one op-class at a time,
with a feature flag so broken intermediate states don't break the whole
test suite.

## 2. Target architecture

### Invariants

1. At every point in the IR, every stack slot has a statically-known type
   (int or float). The codegen tracks this via `expr_type` on every
   subexpression.
2. Every op knows statically which window its operands come from and which
   window its result goes to. There is no runtime type check.
3. Crossings between windows happen only at explicit conversion ops
   (`F32ToI32`, `I32ToF32`, float comparisons producing bool, function
   call/return boundaries).
4. Balanced stack discipline holds per-window: within any straight-line
   region, `sum(int_pushes) == sum(int_pops)` and likewise for floats.
5. Jump targets have the same per-window depth on all incoming edges
   (already a property of Lyte IR for the unified depth; extends naturally).

### Op taxonomy

**Int-only ops** — unchanged. `IAdd`, `ILt`, `Load32` when the loaded type
is int, `LocalGet` for int locals, etc. Operate on `t0..t3` in GPRs.

**Float-only ops** — new F variants. `FAddF`, `FSubF`, `FMulF`, `FDivF`,
`SinF32F`, `F32ConstF`, `LocalGetF` for f32 locals, `LoadF32Off`, etc.
Operate on `f0..f3` in FP registers.

**Crossing ops** — bridge between windows. `F32ToI32F` reads `f0`, writes
`t0` as signed integer cast. `FLtF` reads `f0, f1`, writes `t0` as bool
(0/1). `PrintF32F` reads `f0`. A dedicated `FToBits` op for the case where
you need the raw f32 bit pattern in a GPR (e.g., before a function call
whose arg-copy path expects ints).

**Window-agnostic ops** — don't touch either TOS window. `Nop`, `Halt`,
`Jump`, `LocalAddr` (pushes a pointer as int), `Call`/`Return`,
`I64Const` (pushes int). Fused memory-to-memory ops like
`FusedAddrLoad32OffSet` already bypass both windows.

### Window-independent fused ops

The existing 3-address fused ops (`FusedGetGetFAddSet`, etc.) read both
operands directly from `locals[]` into FP registers, do the op, and write
back to `locals[]`. They touch neither TOS window, so they need **no**
float-window variant — they work equally well with the new design. This
is a significant portion of the hot-path arith in biquad and FFT already.

### Hot local cache

Option A (minimal): Keep `l0..l2` as `uint64_t` GPR args. Float-typed hot
locals store their bit pattern in the GPR and pay one GPR↔FP crossing on
read/write. Simpler; accepts a small cost for float hot locals.

Option B (full): Add `fl0, fl1, fl2` as `double` handler args. Separate
hot-local cache for float locals. Analysis picks top-3 int locals *and*
top-3 float locals independently. Three more FP register args; no GPR
budget impact.

Recommendation: **start with Option A**. Measure. Only move to Option B
if profiling shows hot float local access dominates the remaining cost.

## 3. Op inventory

Complete list of new ops needed. Grouped by phase.

### 3.1 Primitive ops (Phase 1)

Push/pop/tee of float-typed locals and constants:

- `LocalGetF(u16)`, `LocalSetF(u16)`, `LocalTeeF(u16)`
- `LocalGetL0F`, `LocalGetL1F`, `LocalGetL2F`
- `LocalSetL0F`, `LocalSetL1F`, `LocalSetL2F`
- `F32ConstF(f32)`
- `F64ConstF(f64)` *(if we decide to cover f64; see §6 Open Questions)*
- `DropF` — pop from float window

### 3.2 Float arithmetic (Phase 1)

Operands/result in float window:

- `FAddF`, `FSubF`, `FMulF`, `FDivF`, `FNegF`
- `FPowF` *(backed by `powf`)*
- `DAddF`, `DSubF`, `DMulF`, `DDivF`, `DNegF` *(if f64 covered)*

### 3.3 Math intrinsics (Phase 3)

All consume `f0`, write `f0`:

- `SinF32F`, `CosF32F`, `TanF32F`, `AsinF32F`, `AcosF32F`, `AtanF32F`
- `SinhF32F`, `CoshF32F`, `TanhF32F`, `AsinhF32F`, `AcoshF32F`, `AtanhF32F`
- `LnF32F`, `ExpF32F`, `Exp2F32F`, `Log10F32F`, `Log2F32F`
- `SqrtF32F`, `AbsF32F`, `FloorF32F`, `CeilF32F`
- `Atan2F32F` *(binary — reads f0, f1)*
- `IsnanF32F`, `IsinfF32F` *(return int — cross-window)*
- Same pattern for the f64 variants if f64 is in scope

### 3.4 Crossing ops (Phase 4)

These bridge the two windows explicitly. Codegen inserts them where types
transition:

- `F32ToI32F` — pop `f0` (as float), push `t0` (as signed i32 cast)
- `I32ToF32F` — pop `t0` (as signed i32), push `f0`
- `F32ToF64F`, `F64ToF32F` — in-window, both operate on float window
- `FToBitsF` — pop `f0`, push `t0` as the f32 bit pattern (no conversion).
  Used at function call boundaries where the arg-copy path expects an int.
- `BitsToFF` — the inverse, used on return values

### 3.5 Float comparisons (Phase 4)

Each reads two values from `f0/f1`, pushes `0` or `1` to the **int**
window (`t0`). These are the float analogues of `FEq`, `FLt`, etc.:

- `FEqF`, `FNeF`, `FLtF`, `FLeF`, `FGtF`, `FGeF`
- `DEqF`, `DLtF`, `DLeF` *(if f64)*

### 3.6 Float memory ops (Phase 2)

For struct field and slice access:

- `LoadF32Off(i32)` — pop address from int window, push f32 to `f0`
- `StoreF32Off(i32)` — pop f32 from `f0`, pop address from int window
- `FusedAddrLoad32OffF(u16, i32)` — load struct field into `f0`
- `FusedAddrGetSliceLoad32F(u16, u16)` — slice[idx] f32 into `f0`
- `FusedAddrGetSliceStore32F(u16, u16)` — pop `f0`, store to slice[idx]
- `FusedTeeSliceStore32F(u16, u16, u16)`
- `FusedLocalArrayLoad32F(u16, u16)` — local fixed-size array load
- `FusedLocalArrayStore32F(u16, u16)`

### 3.7 Float-window fused arithmetic (Phase 5)

Float-window variants of the existing stack-based fusion patterns. These
are what the peephole will emit after it grows float-aware patterns:

- `FusedGetGetFAddF(u16, u16)`, `FusedGetGetFSubF(u16, u16)`,
  `FusedGetGetFMulF(u16, u16)` — load both operands from locals, push to `f0`
- `FusedGetFAddF(u16)`, `FusedGetFSubF(u16)`, `FusedGetFMulF(u16)` —
  `f0 = f0 <op> locals[a]`
- `FusedFMulFAddF`, `FusedFMulFSubF` — 3-operand FMA on f-window
- `FusedGetAddrFMulFAddF(u16, u16, i32)`,
  `FusedGetAddrFMulFSubF(u16, u16, i32)` — the biquad FMA term
- `FusedF32ConstFGtJumpIfZeroF(f32, i32)` — float const compare + branch
  (crosses to int for the jump condition)

### 3.8 Print/assert (Phase 4)

- `PrintF32F` — pop `f0`, printf

Count: ~55-70 new ops total. Most are mechanical (single-line handler
bodies mirroring their int-window counterparts).

## 4. Phased implementation

Each phase ends in a commit that builds, passes the golden test suite,
and runs the benchmarks. No phase leaves the tree in a broken state.

### Phase 0: Infrastructure sanity (already done)

- Float TOS window arguments in handler signature ✓
- `ctx->float_stack` spill buffer ✓
- `FPUSH`/`FPOP`/`FDROP1`/`FBINOP_SHIFT` macros ✓
- Narrow FW peephole (to be removed in Phase 7) ✓

### Phase 1: Add ops and handlers without wiring codegen

Goal: every op in §3.1 and §3.2 exists with a C handler, bridge
registration, and correct entries in `stack_depth`/`stack_hot_locals`/
`stack_rebase_lm`. Nothing emits them yet, so tests stay green.

Concretely:

1. Add StackOp variants (enum + Display + encoding in bridge)
2. Write C handlers (`op_local_get_f`, `op_f32_add_f`, …)
3. Register handler function pointers in bridge
4. Update `stack_depth::stack_delta` with correct per-op deltas (split:
   integer delta and float delta; see §5 below)
5. Update `stack_hot_locals::{local_indices_read, local_indices_written,
   rewrite_local_indices, lower}` to route the F variants through the
   existing remap + lowering logic
6. Update `stack_rebase_lm::rebase` for any F op that takes a memory slot

Acceptance: `cargo test --test cli` passes. No change to benchmark
numbers.

### Phase 2: Feature flag on StackFunction

Add `use_fp_window: bool` to `StackFunction`. Default `false` for
everything. Codegen methods check this flag when emitting float
expressions; when false, emit the current int-window ops; when true,
emit F variants.

Add a helper `StackCodegen::emit_float_op(op_if_int, op_if_float)` or
equivalent to keep the conditional short at call sites.

Acceptance: build succeeds, all tests still pass with the flag default
`false`. Manually setting the flag for a single test function and
feeding it a trivial `f32` expression (e.g., `var x: f32 = 1.5`) runs
correctly.

### Phase 3: Per-pattern flip with micro-tests

For each source-level pattern below, add a golden test case exercising
it in isolation, flip the codegen for that pattern, verify against VM
and JIT output. One commit per pattern; each leaves all tests green.

1. Float literal: `var x: f32 = 1.5; print(x as i32)`
2. Float variable read: `fn f(x: f32) -> i32 { x as i32 }`
3. Float variable write: `var x: f32 = 1.5; x = 2.5`
4. Binary f32 add: `var y = x + 1.0`
5. f32 sub, mul, div
6. Chained arith: `var y = a * x + b` (expect FusedGetGetFMulF + FAddF,
   or the eventual 3-address fusion in Phase 5)
7. Call with f32 args: `f(x, y)` — introduces `FToBitsF` boundary op
8. Return f32 from function
9. f32 comparison in `if`: `if x > 1.0 { ... }`
10. f32 field read: `s.freq`
11. f32 field write: `s.freq = v`
12. f32 slice read: `a[i]`
13. f32 slice write: `a[i] = v`
14. f32 local fixed-size array read/write
15. Math functions: `sin(theta)`, `cos(theta)`, `sqrt(x)`
16. f32 ↔ i32 conversions: `(x as i32)`, `(i as f32)`
17. `f32x4` / SIMD types — out of scope for this pass; defer

Each step adds the crossing / unary / binary ops from §3 as needed.

Acceptance after each step: golden test for that pattern passes on all
backends (VM, JIT, Stack VM with flag on). Existing tests still pass.

### Phase 4: Enable by default

Flip `use_fp_window: true` for every `StackFunction`. Run the full
golden test suite. Fix any regressions that show up.

Run the benchmark suite. Expected results:

- **biquad**: roughly matches the current FW peephole result (0.22s) or
  slightly better, because the chain is still captured
- **fft**: improves materially because its hot loop uses the simpler
  `LocalGet + FMul` pattern that the FW peephole misses
- **sort**: unchanged (integer-heavy)

### Phase 5: Float-aware fusion

Add peephole patterns that mirror the existing int-window fusions:

1. `LocalGetF + LocalGetF + FAddF` → `FusedGetGetFAddF`
2. `LocalGetF + LocalGetF + FAddF + LocalSetF` → `FusedGetGetFAddSet`
   *(can reuse the existing 3-address op — it doesn't touch either TOS
   window, so int/float doesn't matter at the fusion result)*
3. `FusedGetGetFMulF + LocalGetF + LocalGetF + FMulF + FSubF` →
   `FusedFMulFSubF` *(FFT butterfly)*
4. `LocalGetF + FMulF` → `FusedGetFMulF`
5. `LocalGetF + FMulF + FAddF` → `FusedFMulFAddF` + helpers
6. `FusedAddrLoad32OffF + FusedFMulFAddF` → `FusedGetAddrFMulFAddF`
7. `F32ConstF + FGtF + JumpIfZero` → `FusedF32ConstFGtJumpIfZeroF`
8. `LocalGetF + LocalGetF + FLtF + JumpIfZero` → `FusedGetGetFLtJumpIfZeroF`

Measure after each pattern. Stop when returns diminish.

### Phase 6: Clean up legacy paths

Remove the things that are no longer needed:

1. `float_window_rewrite` pass (the narrow FW peephole)
2. `FusedGetGetFMulFW`, `FusedGetAddrFMulFAddFW`, `FusedGetAddrFMulFSubFW`,
   `LocalSetL{0,1,2}FW` and their handlers
3. Original int-window float ops (`FAdd`, `FSub`, `FMul`, `FDiv` etc.) —
   they should have zero emissions now. Verify with a `cargo build`
   warning for unused enum variants, then delete.
4. `from_f32` / `as_f32` helper calls in any handler that's no longer
   reached. Keep the helpers themselves; they still get used by loads
   and bit-cast boundaries.

### Phase 7: (Optional) Float hot local cache

If profiling shows `LocalGetL{0,1,2}F` still has measurable GPR↔FP
crossings, add `fl0`, `fl1`, `fl2` as `double` handler args and repeat
the hot local analysis separately for float-typed locals. Otherwise
skip.

### Phase 8: Measurement and write-up

Update `benchmark/run.sh` to record timings. Document results in the
commit message and in `Silverfir-nano/docs/` comparison.

## 5. `stack_depth` changes

Currently `stack_delta(op)` returns a single `i32` for the unified
stack depth. With two windows we need two deltas: int and float.

Change `stack_delta` to return `(i32, i32)` = (int_delta, float_delta).
Update the jump-target depth tracking to propagate both deltas
independently. Merge points (jump targets) still need both depths to
match across incoming edges — fall back to conservative "force deep on
both" as today.

Alternatively, keep `stack_delta` returning a single int and add a
sibling `float_stack_delta`. Either is fine; the first is cleaner.

## 6. Open questions

### 6.1 `double` vs `float` for f0..f3

Current design uses `double`. That widens f32 through arith, which risks
double-rounding: `(f32)(double_a + double_b)` isn't always the same as
`(f32)a + (f32)b`. For correctness, either:

- Use `float` (single-precision) args. Requires separate `f64` window
  if f64 is supported, or a type union trick.
- Keep `double` and explicit `(float)` casts on every f32 arith op so
  each op rounds to f32 before the next one runs. This is what the
  current FW handlers do:
  ```c
  f0 = (double)((float)f1 * (float)f0);
  ```

Recommendation: **stick with `double` and explicit `(float)` casts**.
Compilers emit this as scalar single-precision ops on aarch64 (s-regs
overlap d-regs). Allows the same window to hold f64 values too without
a separate register bank.

### 6.2 f64 scope

The only f64 ops in the Stack VM are there for completeness (Lyte
supports `f64` but the hot-path benchmarks don't exercise it). Option:

- **A**: Cover f64 with F variants too. More ops, but consistent.
- **B**: Leave f64 on the int TOS path. f64 is rare in hot code; the
  crossing cost is tolerable.

Recommendation: **Option B** for now. Add f64 F variants later if real
f64 workloads emerge.

### 6.3 Function call arg passing

When calling a function with mixed int and float args, how are they
packed into `new_locals[]`?

Option A: All args pass through the int window. At the call site, float
args are explicitly bit-cast (`FToBitsF`) into `t0..t3`. The callee
reads them with `LocalGetF(n)` which reinterprets the stored bit pattern
as f32. Simple; two crossings per call (one each side).

Option B: Per-arg typed routing. Call op reads float args from `f0..f3`
and int args from `t0..t3`. Requires the call op to know per-arg types.
Fast (no crossings) but complex.

Recommendation: **Option A for v1**. Function calls on the hot path are
rare (biquad and FFT inline most things). Revisit if profiling shows
call-heavy float code.

### 6.4 Block value window

A block expression's value can be int or float. `translate_expr_inner`
already tracks `expr_type`; the result window follows from the type.
When a block is in `void_ctx`, the value is dropped — use `Drop` for
int-typed blocks and `DropF` for float-typed blocks. `translate_void`
already dispatches on the last expression; add the type check there.

### 6.5 Float-typed function results

Return values currently flow back through `t0` (see `op_return`). For
float returns, add a conversion at the return site: callee has the
value in `f0`, needs to move it to `t0` as the f32 bit pattern before
`op_return`, then the caller reads `t0` via `BitsToFF` if the caller's
context expects the value in `f0`.

Or: add `op_return_float` that copies from `f0` on return. Cleaner but
doubles the return op count.

Recommendation: **go through `t0`** (bit-cast at boundary). Call-heavy
float code is rare enough that the two extra crossings per call don't
dominate.

### 6.6 `Drop` windowing

Existing `Drop` pops from int window. Add `DropF` for float window.
Codegen knows the type being dropped; emits the right one. Simple.

`LocalTee` same — add `LocalTeeF`.

## 7. Risks and mitigations

- **Runtime window mismatch (SIGSEGV)**. Mitigation: feature flag
  (Phase 2) + one pattern at a time (Phase 3) + bridge ops at every
  type transition. Each phase commit is independently revertable.

- **Fusion coverage regression.** If Phase 4 flips the codegen before
  Phase 5 adds float-aware fusions, biquad and FFT will regress from
  losing the int-window fused ops. Mitigation: measure after Phase 4
  and Phase 5 separately; the float-aware peephole patterns are
  mechanical mirrors of the existing int ones. Expect Phase 5 to take
  a day of work.

- **Handler signature too large.** Every op gets 4 more `double` args
  (16 total per handler). On aarch64 that's fine (plenty of FP
  registers). On x86-64 it uses xmm0-xmm3, independent of the GPR
  budget. Verified already during the current float-window
  infrastructure work.

- **Precision differences vs current code.** The widening-to-double
  then cast-to-float behavior can round differently than chained f32
  ops. Mitigation: keep explicit `(float)` casts on every op and
  compare golden-test outputs against the JIT/VM backends (which do
  native f32 arith). Any divergence is a codegen bug.

- **Time sink.** The refactor is ~2-3 days of focused work. Mitigation:
  Phase 3 patterns can be shipped one at a time, so the refactor can be
  paused between phases without leaving the tree broken.

## 8. Measurement plan

Baselines from when the plan was written (3-run averages, aarch64 M-series):

| Workload | Stack VM (commit 5b8c48f) | Lyte LLVM AOT | Silverfir μJIT | Silverfir interp |
|---|---|---|---|---|
| biquad | 0.218s | 0.034s | 0.072s | 0.641s |
| sort | 0.114s | 0.027s | 0.023s | 0.068s |
| fft | 0.612s | 0.043s | 0.054s | 0.656s |

Actual results after the full work sequence (5-run samples via
`benchmark/run.sh 5`, aarch64 M-series inside a VM):

| Workload | 5b8c48f | +`fsp` | +float win | +tee fusions | +pre-shift | Int-path today (`b40ecc1`) | F-only today (`b818fd1`) | Plan target |
|---|---|---|---|---|---|---|---|---|
| biquad | 0.218s | 0.125s | 0.104s | 0.110s | 0.100s | **0.100s** | **0.102s** | 0.15–0.18s ✅ |
| sort | 0.114s | 0.074s | 0.075s | 0.074s | 0.074s | 0.074s | **0.074s** | unchanged ✅ |
| fft | 0.612s | 0.405s | 0.399s | 0.395s | 0.395s | **0.395s** | **0.497s** | 0.35–0.45s ❌ |

The final F-only state beats every plan target on biquad and sort but
is 10% over the FFT target. The ~100ms FFT regression from the
deletion of the int f32 path is a known issue — the BTB-aliasing
hypothesis that motivated the deletion turned out to be wrong, and
the real cause hasn't been diagnosed yet because the development VM
doesn't expose hardware counters. See §13 for the investigation log
and plan.

Silverfir pure-interp is the honest ceiling for an interpreter with
this architecture. We currently beat it by ~6× on biquad, ~1.3× on
FFT, and match it on sort.

## 9. Rollback strategy

- Phase 0-1: additions only, no behavior change. Revertable by deleting
  commits.
- Phase 2: feature flag default `false`. Revertable by flipping the
  default.
- Phase 3: per-pattern commits, each independently revertable.
- Phase 4: enable by default. If regressions appear, flip the flag back
  to `false` and bisect which pattern is wrong.
- Phase 5-6: cleanups and fusions — pure wins, easy to revert.

At any point, the Stack VM can fall back to the current FW peephole
approach (commit 5b8c48f) by reverting to that point in history.

## 10. Effort estimate

- Phase 0: done
- Phase 1: 1 day (mechanical additions)
- Phase 2: 0.5 days (feature flag plumbing)
- Phase 3: 2 days (17 patterns × ~1h each including tests)
- Phase 4: 0.5 days (flip + debug any stragglers)
- Phase 5: 1 day (mirror peephole patterns, benchmark between)
- Phase 6: 0.5 days (cleanup)
- Phase 7: 0.5-1 day (optional)
- Phase 8: 0.5 days (measurement + docs)

**Total: 6-8 focused days.** Can ship Phase 1-4 in isolation if only
raw-ops coverage is needed, then add Phase 5 for fusion coverage later.

## 11. Success criteria

- Biquad Stack VM result ≤ 0.18s (matches or beats Silverfir interp by
  a large margin; within 5× of LLVM AOT)
- FFT Stack VM result ≤ 0.45s (within 10× of LLVM AOT; beats Silverfir
  interp by 30%+)
- No regression on sort
- No regression on any golden test
- No new unsafe code in the Rust side
- Legacy FW peephole path removed (no dual codepaths)

## 12. Post-implementation findings

### What actually happened

Phases 1–3 landed cleanly. Flipping the default in phase 4 regressed
biquad from 0.150s to 0.270s (−80%) and FFT from 0.395s to 1.036s
(−163%). Tracing the regression turned up two independent costs the
plan hadn't anticipated: the float spill pointer chasing memory on
every push/pop, and a latent fcvt tax from the `double`-typed float
window. Both got fixed, and both ended up helping the *default* int
path more than the `--fp-window` path the plan was supposed to
optimize — because the narrow `float_window_rewrite` peephole uses
the same `FPUSH`/`FDROP` macros and feeds into the same FMA-chain
handlers.

### Root cause 1 (fixed): `fsp` lived in memory, `sp` lived in a register

The integer `PUSH`/`DROP1` macros compile to `*sp++ = t3` — one store
with post-increment, with `sp` pinned in a GPR by `preserve_none`
across the whole handler chain. The float `FPUSH`/`FDROP1` macros
expanded to `ctx->float_stack[ctx->float_sp_off++] = f3`, which on
aarch64 became four memory ops per call: load `float_stack` from
ctx, load `float_sp_off` from ctx, store `f3` at the computed index,
write `float_sp_off` back to ctx. The narrow peephole only paid this
cost once per biquad iteration (for the FMA chain); uniform F-emission
paid it 4–5× per iteration.

**Fix** (commit `21f2949`): promote the float spill pointer to a
handler argument next to `sp`, under the name `fsp`. `FPUSH` becomes
`*fsp++ = f3`. `CallFrame` grows a `saved_fsp` field, saved on
`op_call`/`op_call_closure`/`op_call_indirect` and restored on
`op_return`/`op_return_void` exactly like `saved_sp`. Handler
signature widens by one GPR arg — still within `preserve_none`'s
aarch64 budget; x86-64 would stash `fsp` in `ctx->current_fsp` the
same way `locals` already is.

Impact (3-run averages, aarch64 M-series):

| Workload | Default, pre-fsp | Default, post-fsp | `--fp-window`, pre-fsp | `--fp-window`, post-fsp |
|---|---|---|---|---|
| Biquad | 0.150s | **0.125s** (−17%) | 0.270s | **0.133s** (−51%) |
| Sort | 0.075s | 0.074s | 0.074s | 0.080s |
| FFT | 0.395s | 0.405s | 1.036s | **0.500s** (−52%) |

Note that the **default int-window path also benefited** from this
fix — the FW peephole chain uses the same `FPUSH`/`FDROP` macros.
This is the biggest stack-VM perf improvement since hot local
registers landed, and it was hiding in plain sight.

### Root cause 2 (fixed): the `double`-typed float window cost an fcvt tax per op

§6.1 of the original plan typed the float TOS window as `double` so
one set of FP-register slots could hold both f32 (widened) and f64
values. Every f32 handler body looked like:

```c
f0 = (double)((float)f0 + coeff * state);
```

The intent was that the `(float)` narrow + `(double)` widen round-trip
would disappear under optimization since f0 had just come from an f32
op. It didn't. LLVM couldn't prove the round-trip away because `f0`
is a handler argument: its `double` type survives across the
`musttail` dispatch boundary, and the next handler (chosen by
indirect branch) might interpret it differently. So every op in the
biquad FMA chain compiled to:

```asm
fcvt  s0, d0                 ; ~3 cycles, narrow on entry
fmadd s0, s4, s5, s0         ; ~3 cycles, the actual work
fcvt  d0, s0                 ; ~3 cycles, widen on exit
```

Plus — separately — `as_f32(locals[i])` on the coeff side compiled to
`ldr x_, [x_addr]; fmov s_, w_` because `locals` is typed as
`uint64_t*`, forcing a GPR load followed by a GPR→FP move. That's an
extra ~3-cycle crossing per op.

**Fix** (commit `c67ed9e`): flip the window to `float`.

- `HANDLER_ARGS`, `Handler` typedef, `CallFrame.saved_fsp`, and
  `Ctx.float_stack` all change from `double` / `double*` to `float` /
  `float*`. The spill buffer halves in size. `FPUSH` pushes a `float`
  directly.
- Every float-window handler body loses its `(double)` / `(float)f0`
  casts. `f0 = f1 + f0;` is literal source code now.
- Every `as_f32(locals[pc->imm[i]])` in a fused f-window handler is
  rewritten as `*(float*)(locals + pc->imm[i])` so the compiler emits
  `ldr s_, [x_, y_]` — a direct single-precision load, no crossing.
- f64 is already on the int-window path per §6.2 Option B; dropping
  double-precision window support costs nothing on our hot workloads.

Resulting handler disassembly for the biquad FMA term:

```asm
_op_fused_get_addr_fmul_fadd_fw:
  ldp   x8, x9, [x21, #0x8]
  lsl   x8, x8, #3
  ldr   s4, [x24, x8]           ; direct f32 load, no crossing
  add   x8, x24, x9, lsl #3
  ldrsw x9, [x21, #0x18]
  ldr   s5, [x8, x9]            ; direct f32 load
  fmadd s0, s4, s5, s0          ; single-cycle FMA
  ldr   x3, [x21, #0x40]
  add   x21, x21, #0x20
  br    x4
```

Critical path on the `f0` accumulator: one `fmadd` per op (3 cycles),
down from `fcvt` + `fmadd` + `fcvt` (9 cycles). The two `ldr s` loads
are off-critical so the scheduler can hide them under the data
dependency.

Impact (3-run averages, aarch64 M-series):

| Workload | Pre (`double` window) | Post (`float` window) | Delta |
|---|---|---|---|
| biquad default | 0.125s | **0.104s** | −17% |
| biquad `--fp-window` | 0.133s | **0.100s** | −25% |
| sort default | 0.074s | 0.075s | ~0 |
| fft default | 0.405s | 0.399s | ~0 |
| fft `--fp-window` | 0.500s | 0.490s | −2% |

Biquad's FMA chain is the tightest dependency chain in the suite, so
it gets the biggest win. FFT has more control-flow overhead per FP
op so the fcvt savings are overlapped with other work.

### What's left: the float window still has a small per-iteration tax

After both fixes, `--fp-window` on biquad is within the noise of the
default path (0.100 vs 0.104). On FFT it's still ~23% slower, likely
because of the same reason Phase 5 fusion couldn't fully close: the
F codegen uses the float window for more values per iteration than
the narrow FW peephole, and the few extra FPUSH/FDROPs still cost
(albeit much less than before) on workloads with dense, short-lived
f32 values.

The fundamental tradeoff is unchanged: the F window wins when a
value is used across many ops (amortizing FPUSH/FDROP); the int
window wins when a value only lives for one op, because GPR shuffles
in the int TOS window don't touch memory while the window isn't full.
Phase 5 fusion mitigates this partially by collapsing chains, but
doesn't help single-use values. A smarter codegen that only emits F
variants for values provably used in multi-op chains would close the
remaining FFT gap — but the ROI is small at this point.

### What this means for the original success criteria

| Criterion | Status (VM) | Status (M4 bare metal) |
|---|---|---|
| Biquad ≤ 0.18s | ✅ 0.102s | ✅ **0.104s** |
| FFT ≤ 0.45s | ❌ 0.497s | ✅ **0.369s** |
| No regression on sort | ✅ 0.074s | ✅ 0.080s |
| No regression on any golden test | ✅ | ✅ |
| No new unsafe code | ✅ | ✅ |
| Legacy FW peephole path removed | ✅ done in `b818fd1` | ✅ |

All six criteria met on real hardware.

All six targets met — but only once the measurements were rerun
on bare metal. The FFT "regression" that made the earlier version
of this table mark the target as failed was a virtualization
artifact (see §13). On Apple M4 bare metal, F-only FFT is 0.369s
vs int-path 0.395s — a ~6.5% improvement that exactly matches
the per-handler instruction-count analysis.

## 13. Post-mortem: the FFT "regression" was a VM artifact

The earlier version of this section treated the post-deletion FFT
gap as an open performance investigation. It wasn't: everything
that looked like a 25% regression inside the development VM
disappears — or reverses into a small win — on bare-metal Apple
Silicon. §13.1–§13.3 below record the original investigation log
(mostly to preserve the reasoning trail); §13.4 is the actual
resolution.

### 13.1 What the VM reported

Running `benchmark/run.sh` inside an aarch64 Linux VM on Apple
Silicon:

|        | int default (`b40ecc1`) | F only (`b818fd1`) | Δ |
|--------|-------------------------|---------------------|---|
| biquad | 0.100s | 0.102s | +2% |
| sort   | 0.074s | 0.074s | 0  |
| fft    | 0.395s | **0.497s** | **+25%** |

Biquad and sort were flat; FFT was dramatically slower under F-only.
Nothing about the deletion commit itself could plausibly cause the
gap — the handlers being used after deletion are the same F
handlers that `--fp-window` was already invoking in the pre-deletion
build, and the per-handler disassembly shows them strictly shorter
than their int counterparts. So the gap had to be
microarchitectural, and since it wasn't obviously code-driven, we
burned a lot of time chasing BTB aliasing, NEON forwarding
latency, and store-buffer pressure as theories.

### 13.2 Failed hypothesis: BTB aliasing (pre-deletion)

With both paths still in the tree, Instruments Time Profiler
(`xcrun xctrace record --template 'Time Profiler'`) on the FFT
benchmark in each mode counted samples per top-of-stack handler:

|                          | int default | `--fp-window` | Δ     |
|--------------------------|-------------|---------------|-------|
| f32-related handlers     | 233         | 220           | −13   |
| **int-only handlers**    | **154**     | **290**       | **+136** |
| sin / dylib / setup      | 45          | 48            | +3    |
| **Total**                | **432**     | **558**       | **+126** |

The F handlers were collectively *faster* than their int counterparts
(−13 samples) — matching the disassembly evidence. But handlers that
are **identical C code at identical addresses in both paths**
(`op_fused_get_get_ilt_jiz`, `op_jump`, `op_i64_const`, …) racked
up 136 extra samples when they ran interleaved with F handlers.

Since the handlers couldn't physically run slower, something
external had to be slowing them down. The theory was BTB aliasing
on the tail-call dispatch BR: the int path's "target universe" was
just int handlers, while the F path alternated int and F handlers
interleaved, so the BTB had to track more distinct targets for the
same source BR and started mispredicting. Deleting the int-window
f32 handlers should remove the aliasing.

**It didn't.** Post-deletion FFT ran at exactly the same 0.497s the
`--fp-window` path had been reporting. The BTB aliasing theory was
falsified by the deletion itself.

### 13.3 Things verified directly before the bare-metal test

- **Op counts are equal.** The butterfly inner loop is 38 ops per
  iteration in both paths (see diff of `/tmp/sir_int.txt` vs
  `/tmp/sir_f.txt`). No missing F fusion patterns on the hot path.
- **Per-op F handlers are ≤ int handlers in instruction count.**
  On HEAD:
  - `op_fmul`: **9 insns F vs 12 int** (removes 3 fmov GPR↔FP
    crossings; see disassembly in 13.4 below).
  - `op_fused_get_addr_fmul_fadd`: **10 insns F vs 13 int** (removes
    3 fmov crossings; the critical FMA is on already-FP-resident
    operands).
  - `op_local_get_l0`: 11 F vs 11 int (parity after `b40ecc1`).
- **Every missing fusion pattern that could be spotted got added.**
  `FusedTeeSliceStore32F`, `LocalSet+LocalGet→LocalTee` F peephole,
  direct-store optimization.
- **`fsp` is in a register.** `FPUSH` compiles to `str s, [x23], #0x4`
  — equivalent to int `PUSH`'s `str x, [x22], #0x8`.
- **Extra `lsl` instructions on F handlers are gone** (`b40ecc1`).

Nothing in the static analysis suggested the F path should be
slower. It just *measured* as slower in the VM.

### 13.4 Resolution: measurement on bare metal

Rerunning the A/B comparison on the bare-metal host — Apple M4
(`Mac16,13`, macOS 26.4) — collapses the regression entirely. Same
two binaries, built the same way:

```bash
# pre-deletion (int default, --fp-window opt-in):
git worktree add /tmp/lyte-wt-int 67eb53c
(cd /tmp/lyte-wt-int && CARGO_TARGET_DIR=/tmp/lyte-target-int \
  cargo build --release -p lyte-cli)
cp /tmp/lyte-target-int/release/lyte /tmp/lyte-int.bin

# HEAD (F-only):
CARGO_TARGET_DIR=/tmp/lyte-target-f cargo build --release -p lyte-cli
cp /tmp/lyte-target-f/release/lyte /tmp/lyte-f.bin
```

12-run median wall time via `subprocess.run` + `time.perf_counter`
(first run dropped as warm-up):

| Config | FFT median | stdev |
|---|---|---|
| `lyte-int.bin` default (int path)  | **395 ms** | 1.5 ms |
| `lyte-int.bin --fp-window` (F path) | **369 ms** | 2.0 ms |
| `lyte-f.bin` (F-only, HEAD)         | **369 ms** | 2.0 ms |

On M4 bare metal the F path is **26 ms (6.5%) faster** than the int
path, not slower. The two F configurations (`--fp-window` inside
the pre-deletion build and F-only at HEAD) match to within
measurement noise, confirming they're running the same code.

The stack IR confirms that `--fp-window` on the pre-deletion binary
and default on HEAD emit the **exact same IR** for FFT
(`md5 /tmp/sir_int_fp.txt /tmp/sir_f.txt` match), so any performance
difference between them would have to be environmental. There
isn't one.

The handler disassembly on both binaries shows the predicted win
concretely. The biquad-FMA-shaped hot op for the FFT butterfly:

```asm
; int path, _op_fused_get_addr_fmul_fadd (13 insns)
mov    x4, x3
ldp    x8, x9, [x21, #0x8]
ldr    x8, [x24, x8, lsl #3]
fmov   s4, w8                ; GPR→FP crossing
add    x8, x24, x9, lsl #3
ldrsw  x9, [x21, #0x18]
ldr    s5, [x8, x9]
fmov   s6, w28                ; GPR→FP crossing
fmadd  s4, s4, s5, s6
fmov   w28, s4                ; FP→GPR crossing
ldr    x3, [x21, #0x40]
add    x21, x21, #0x20
br     x4

; F path, _op_fused_get_addr_fmul_fadd_f (10 insns)
mov    x4, x3
ldp    x8, x9, [x21, #0x8]
ldr    s4, [x24, x8]          ; direct f32 load
add    x8, x24, x9, lsl #3
ldrsw  x9, [x21, #0x18]
ldr    s5, [x8, x9]
fmadd  s0, s4, s5, s0          ; FP→FP, no crossings
ldr    x3, [x21, #0x40]
add    x21, x21, #0x20
br     x4
```

Three `fmov` GPR↔FP crossings deleted per op. On M4 that's ~9
cycles of crossing latency saved, which is exactly in the
neighborhood of the 6.5% FFT improvement: 26 ms / 395 ms × mean
op latency implies ~4–5 cycles saved per hot op, which is the
right order of magnitude given that many ops in the butterfly
aren't the FMA and therefore don't save the full 9 cycles.

### 13.5 Why the VM reported the opposite

Not fully diagnosed, and probably not worth diagnosing. The
development VM (aarch64 Linux guest on Apple Silicon) is the only
environment where the "regression" appeared. Plausible candidates
for what the VM was doing differently:

- **Trapped FP ops.** Some VM configurations trap on advanced
  floating-point instructions (especially if running under
  translation) and the trap overhead dwarfs the GPR↔FP crossing
  savings. The FFT butterfly uses `fmadd` much more densely than
  biquad or sort, which would explain why only FFT regressed in
  the VM.
- **Different FP register pressure in the hypervisor's save/restore
  path.** Interpreter-shaped code with heavy `preserve_none` FP
  arg traffic can trigger host-side lazy-FP behavior.
- **Scheduler moving the workload.** The VM may migrate between
  virtual P/E cores more aggressively than macOS does on bare
  metal; each migration loses cache state for the 10M-iteration
  butterfly.

None of these are fixable from inside Lyte, and all of them stop
mattering the moment you run on real hardware. The Stack VM's
target is the Audulus runtime on iOS, which is also bare metal —
so the bare-metal result is the one that matches production.

### 13.6 Decision

The `b818fd1` deletion stands. No revert, no smart-codegen cost
model, no further investigation. §13.1–§13.3 are preserved for
anyone who hits a similar situation and wants to see the full
reasoning trail; §13.4 is the actual resolution.

If the FFT gap reappears in production-shaped measurements later
— on Audulus on an iPad, for example — the investigation should
start with Instruments CPU Counters on the iPad itself, not on a
VM. The concrete steps (build A/B binaries, `xcrun xctrace record
--template 'CPU Counters'`, compare counter deltas in
`Instruments.app`) are still valid; we just no longer need to run
them on the M4.
