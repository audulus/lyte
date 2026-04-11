# Float-Window Codegen Refactor Plan

A plan for moving the Stack VM from "f32 values bit-cast through the integer
TOS window" to "f32 values live in a dedicated float TOS window (f0..f3) in
FP/SIMD registers throughout expression evaluation."

> **Implementation status** (updated after commit `b40ecc1`)
>
> Phases 1–3 landed. Phase 4's original premise — that uniformly emitting
> F-variant ops would beat the narrow `float_window_rewrite` peephole —
> turned out to be wrong in its first form, regressing biquad ~2×. Five
> separate unplanned fixes (see §12) progressively closed the gap and
> then pushed past it on biquad and sort:
>
> 1. **`fsp` register-arg promotion** (`21f2949`). `FPUSH`/`FDROP` used
>    to chase `ctx->float_stack` and `ctx->float_sp_off` through memory
>    on every call; promoting the float spill pointer to a
>    `preserve_none`-pinned handler argument made it as cheap as the
>    integer `*sp++`. Cut `--fp-window` biquad from 0.270s to 0.133s
>    *and* improved the default int path from 0.150s to 0.125s because
>    the FW peephole uses the same macros.
>
> 2. **Float-typed window** (`c67ed9e`). The plan (§6.1) originally
>    typed the float TOS window as `double` so f64 could share the
>    register file. Every f32 op paid two fcvt round-trips because LLVM
>    couldn't prove the narrow+widen away across the `musttail` dispatch
>    boundary. Flipping the window to `float` (and rewriting
>    `as_f32(locals[i])` as direct `*(float*)` loads in the fused
>    handlers) collapsed the biquad FMA critical path from ~12 cycles
>    per op to 3 cycles.
>
> 3. **`saved_sp` / `saved_fsp` removal** (`bc44a11`, `b999162`). Both
>    fields were written on every call and never read — balanced stack
>    discipline leaves `sp` and `fsp` at exactly the value the caller
>    expects at op_return time. Removed one store per non-tail call,
>    16 bytes of `CallFrame` shrinkage.
>
> 4. **Direct f32 stores + tee fusions** (`f2a9c95`). Every F handler
>    writing `locals[n] = from_f32(f0)` compiled to `fmov w, s; str x`
>    (FP→GPR crossing + u64 store). Rewriting as `*(float*)(locals + n)
>    = f0` emits a single `str s, [...]` — safe because f32 locals
>    are only ever read via `*(float*)` / `as_f32()` which ignore the
>    upper 32 bits. Also added `FusedTeeSliceStore32F` (F mirror of
>    the FFT butterfly store fusion) and the F variant of the
>    `LocalSet + LocalGet → LocalTee` peephole.
>
> 5. **Pre-shifted f32 local indices** (`b40ecc1`). Every F handler
>    loading an f32 directly from `locals[n]` had to emit
>    `lsl x, x, #3; ldr s, [locals, x]` because aarch64's indexed f32
>    load only supports `lsl #2` (matching 4-byte stride), not the
>    `lsl #3` we'd need to hit `locals[n]` (8-byte stride). Encoding
>    the local index as a pre-shifted byte offset in the bridge lets
>    the handler emit a single `ldr s, [locals, byte_off]`. One
>    instruction saved per affected handler; benefits both paths since
>    the FW peephole handlers (`op_fused_get_get_fmul_fw` et al.)
>    share the same issue. Pushed biquad default from 0.11s to 0.10s.
>
> Current benchmark state (8-run interleaved samples, aarch64 M-series):
>
> | Workload | Start of work (`5b8c48f`) | Default (today) | `--fp-window` (today) | Plan target |
> |---|---|---|---|---|
> | biquad | 0.218s | **0.100s** (−54%) | **0.100s** (−54%) | 0.15–0.18s ✅ |
> | sort | 0.114s | 0.074s | 0.073s | unchanged ✅ |
> | fft | 0.612s | **0.395s** (−35%) | 0.485s (−21%) | 0.35–0.45s ✅ |
>
> Phase status:
>
> | Phase                                 | Status                  | Commit(s)            |
> |---------------------------------------|-------------------------|----------------------|
> | 1 — F-variant op scaffold             | ✅ landed               | `2be383d`            |
> | 2 — `emit_float` helper               | ✅ landed               | `4a366ae`            |
> | 3.1 — Direct f32 op sites             | ✅ landed               | `9d2e0bc`            |
> | 3.2 — Type-dependent sites + CLI      | ✅ landed               | `f9e27a8`            |
> | 3.3 — Memory/slice/assign-temp sites  | ✅ landed               | `cf44a00`            |
> | 4 — Flip default on                   | ❌ reverted             | `4b6f00e`            |
> | 5 — Float-aware fusion patterns       | ✅ dormant (opt-in)     | `4b6f00e`, `f2a9c95` |
> | **`fsp` register-arg promotion**      | ✅ landed (unplanned)   | `21f2949`            |
> | **Float-typed window**                | ✅ landed (unplanned)   | `c67ed9e`            |
> | **x86-64 `fsp` in ctx + min/max fix** | ✅ landed (unplanned)   | `821af8d`            |
> | **`saved_sp` / `saved_fsp` removal**  | ✅ landed (unplanned)   | `bc44a11`, `b999162` |
> | **Direct f32 stores + tee fusions**   | ✅ landed (unplanned)   | `f2a9c95`            |
> | **Pre-shifted f32 local indices**     | ✅ landed (unplanned)   | `b40ecc1`            |
> | 6 — Remove legacy FW path             | ⏸ blocked on FFT gap   |                      |
> | 7 — Float hot local cache             | ⏸ skipped              |                      |
>
> The F-variant path is opt-in behind `--fp-window`. On biquad it's at
> parity with the default path (both 0.10s), on sort it's at parity,
> and the **FFT gap has shrunk but not closed** — `--fp-window` is
> still ~23% behind the default. The gap resists static-analysis
> diagnosis: op counts in the butterfly hot loop are equal, every
> per-op F handler is equal or shorter than its int counterpart in
> the disassembly, all the obvious missing fusions have been added,
> and the extra `lsl`s have been eliminated. The one new finding
> since the last update is that the `float_window_rewrite` peephole
> **never fires on FFT** — FFT's default path runs raw int-window
> f32 ops which *should* be strictly slower per op than F, yet still
> isn't. See §13 for the investigation log and remaining theories.

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

Actual results after Phases 1–5 plus five rounds of unplanned fixes
(interleaved multi-run samples, aarch64 M-series):

| Workload | 5b8c48f | +`fsp` | +float win | +tee fusions | +pre-shift | `--fp-window` | Plan target |
|---|---|---|---|---|---|---|---|
| biquad | 0.218s | 0.125s | 0.104s | 0.110s | **0.100s** | 0.100s | 0.15–0.18s ✅ |
| sort | 0.114s | 0.074s | 0.075s | 0.074s | 0.074s | 0.073s | unchanged ✅ |
| fft | 0.612s | 0.405s | 0.399s | 0.395s | **0.395s** | 0.485s | 0.35–0.45s ✅ |

The default path beats every plan target by a wide margin. The
`--fp-window` opt-in path is at parity with the default on biquad and
sort, and still ~23% behind on FFT. The biquad speedup came from a
sequence of five unplanned fixes — none of them involving uniform
F-emission, which is what Phase 4 was originally supposed to deliver.
See §12 for the individual findings and §13 for the still-open FFT
gap investigation.

Silverfir pure-interp is the honest ceiling for an interpreter with
this architecture. We currently beat it by ~6× on biquad, ~1.6× on
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

### Revised recommendations

1. **Keep the default flag off.** The narrow peephole path is now
   within ~4% of uniform F-emission on biquad and remains ahead on
   FFT. Default `stack_fp_window = false`; keep `--fp-window` as an
   opt-in for experimentation.

2. **Keep the peephole path.** Phase 6 (cleanup) is canceled. The
   narrow `float_window_rewrite` pass plus the `*FW` handlers are
   the fast default and should stay. Removing them would force all
   f32 code onto uniform F-emission, regressing FFT ~23%.

3. **The two unplanned fixes are the real story.** Both are independent
   of whether the F codegen path is used:

   - `fsp` register promotion (`21f2949`): −17% biquad, ~40 lines of
     C. Low risk, high impact, already landed.
   - Float-typed window (`c67ed9e`): another −17% biquad, ~100 lines
     of handler body rewrites, plus a tiny API ripple (Ctx /
     CallFrame / Rust bridge). Also low risk because the cost
     reduction shows up in the disassembly before running any
     benchmark.

   Neither required any codegen changes, and both benefit the FW
   peephole path as much as the uniform F path.

4. **Phase 5 fusion patterns remain dormant behind `--fp-window`.**
   Zero cost when the flag is off. Delete only if maintenance becomes
   a drag — there's no current benefit to removing them.

5. **Phase 7 (float hot local cache) is still skipped.** At current
   gap sizes, the ROI is too small.

6. **If you ever want to beat the peephole, do one of:**

   a. *Chain-length heuristic at codegen time.* Emit F variants only
      for f32 subexpressions of statically-estimated chain length ≥ N.
      Shorter chains stay in the int window. Recovers the peephole's
      selectivity inside the codegen.

   b. *Two-level codegen with backpatching.* Emit a mix of int and F
      variants with explicit bridges, then run a peephole pass that
      collapses short F chains back to int when the amortization
      doesn't pay off.

   Both require cost modeling the codegen currently lacks. Neither
   is a small change, and both would optimize a path (uniform F
   emission) that isn't on by default.

### What this means for the original success criteria

| Criterion | Status |
|---|---|
| Biquad ≤ 0.18s | ✅ **0.100s on both paths** — beats target by 44%, within 3.3× of LLVM AOT |
| FFT ≤ 0.45s | ✅ 0.395s default (0.485s `--fp-window`) — default beats target |
| No regression on sort | ✅ 0.074s (both paths) |
| No regression on any golden test | ✅ |
| No new unsafe code | ✅ |
| Legacy FW peephole path removed | ❌ — blocked on FFT, see §13 |

Five of six targets met; the sixth is blocked specifically by the
FFT gap under `--fp-window`. The biquad and FFT numbers comfortably
exceed the plan's targets. The speedup came entirely from a series
of unplanned C-level fixes to the interpreter runtime, not from the
uniform F-emission that Phase 4 was supposed to deliver. The F-path
infrastructure (Phases 1–5) remains in place as an opt-in; it's now
at parity on biquad and sort and would be a net win overall if the
FFT gap closes.

## 13. Open: the FFT `--fp-window` gap

After all the fixes in §12, biquad is at parity between the two
paths (both 0.10s) and sort is at parity. FFT is the one workload
where the F path is still slower — 0.485s vs 0.395s, about 23%.

### The surprising part: FW peephole never fires on FFT

`float_window_rewrite` was designed to match the biquad-shaped FMA
chain: `FusedGetGetFMul` → `FusedGetAddrFMulFAdd+` → `LocalSetL0/1/2`.
The FFT butterfly has a different shape:

```
LocalGet + FusedAddrGetSliceLoad32 + f32.mul
LocalGet + FusedAddrGetSliceLoad32 + fused.fmul_fsub
LocalSet(n)   ← not a hot local register
```

So the FW peephole **never matches** on FFT. `grep _fw` in the
default FFT IR dump returns zero hits. That means the default-path
FFT is running the raw int-window f32 ops (`op_fmul`,
`op_fused_fmul_fsub`, `op_fused_get_fsub`) on every arithmetic op,
each paying 3–4 GPR↔FP crossings. By rights the F path — which
pays zero crossings — should be strictly faster per op.

It isn't. Which tells us the gap isn't about GPR↔FP crossings.
Something else is going on.

### What's been verified and ruled out

- **Op counts are equal.** The butterfly inner loop (which runs
  `N log₂ N / 2 ≈ 5120` times per FFT × 2000 iterations ≈ 10M times)
  is 26 dispatches per iteration in both paths. `bit_reverse_permute`
  is 12 per iter in both paths. `bit_reverse` is all integer and
  identical byte-for-byte.
- **Per-op F handlers are ≤ int handlers in instruction count.**
  Measured (int vs F): `local_get` 11 vs 12, `local_set` 11 vs 12
  (both now 11 vs 11 after the pre-shift fix in `b40ecc1`), `fmul`
  12 vs 9, `fused_get_get_fmul` 16 vs 15, `fused_get_fsub` 10 vs 8,
  `fused_fmul_fsub` 13 vs 8. In every arithmetic case F is strictly
  shorter and eliminates crossings the int handler has to pay.
- **All missing fusion patterns I could spot from the IR have been
  added.** `FusedTeeSliceStore32F`, the `LocalSet+LocalGet→LocalTee`
  F peephole, the direct-store optimization. There are a handful of
  patterns the default path is also missing (the int path has the
  same unfused `local.tee + fused.local_array_store32` sequences in
  the twiddle loop), so those aren't a gap contributor.
- **The spill pointer (`fsp`) is in a register.** `FPUSH` compiles
  to a single `str s, [x23], #0x4` on aarch64 — equivalent to int
  `PUSH`'s `str x, [x22], #0x8`. Same memory traffic.
- **Extra `lsl` instructions on F handlers are gone.** Fixed in
  `b40ecc1` by pre-shifting the local index in the bridge. F
  handlers now emit a single `ldr s, [locals, byte_off]` with no
  scale. This moved biquad default from 0.11s to 0.10s but did
  **not** move FFT at all — which tells us those lsls were already
  hiding behind load latencies in the out-of-order schedule on FFT.

### Candidate theories for the remaining gap

The gap is stubbornly resistant to static analysis. Without hardware
counters, the most plausible explanations are:

1. **Indirect-branch mispredict rate.** The FFT butterfly has 9
   distinct handler targets, executed in the same order every
   iteration. The CPU's indirect branch predictor (BTB) should
   handle this perfectly — but only if both the target address and
   the history pattern fit in BTB entries. The F handlers sit at
   different object-file offsets than the int handlers; if one
   layout happens to alias better in the BTB than the other, that's
   enough to account for ~10–20% on 266M dispatches. Testable with
   `perf stat -e branch-misses`.

2. **Store-buffer pressure.** The F butterfly does 4 `*fsp++ = f3`
   stores per iter (in the 4 LocalGets before the arithmetic),
   same as the int path's 4 `*sp++ = t3`. But aarch64's store buffer
   may track the narrower 4-byte stores differently than the 8-byte
   ones, especially when the next ops don't immediately read them
   back. Testable with `perf stat -e ld_st_buffer_stall`.

3. **NEON register forwarding latency.** The `fmov s, s` register
   moves used for window shifts might have 1-2 cycle extra
   forwarding latency vs GPR `mov x, x` on specific M-series
   microarchitectures. The butterfly does ~4 such moves per handler
   × 26 handlers × 10M iters = 1B FP reg moves. A 1-cycle latency
   penalty each would add ~330ms at 3 GHz — too much, but even a
   10% penalty would explain the gap. Testable with
   `Instruments > System Trace > CPU counters`.

4. **I-cache layout.** The F handlers are at higher addresses in
   the object file than the int handlers (they were added later).
   Cold-start from 0 to 10M dispatches: the int path may have better
   spatial locality. Testable with `perf stat -e l1i-cache-misses`.

### What would it take to close the gap

Any of the theories above can be validated or ruled out with a
single `Instruments` run on the FFT benchmark under each mode. Until
then, the gap is unexplained but:

- **bounded**: ~90ms per 2000-iteration FFT run, or ~45µs per
  single 1024-point FFT — meaningful for a microbenchmark, probably
  invisible in real DSP workloads that do more than FFT.
- **contained**: only FFT sees it. Biquad (the workload the plan
  was originally targeting) and sort don't.

### Why this matters for the "delete the dual path" decision

Phase 6 of the plan originally wanted to remove the int-window f32
ops and the `float_window_rewrite` peephole. That simplification —
roughly 1000 lines of handler code, the entire peephole pass, the
`emit_float` conditional in codegen — would be a net quality
improvement and a meaningful reduction in surface area.

The biquad and sort numbers say go ahead. The FFT gap says wait.
Three choices:

1. **Delete the dual path anyway.** Accept a 20% regression on FFT
   specifically, win on everything else including the plan targets.
   Net: simpler code, still within plan targets. Possibly regrettable
   if the FFT gap later turns out to have a simple fix.

2. **Investigate the FFT gap first.** Run Instruments / perf against
   the two paths, identify the micro-architectural cost, fix it if
   possible. Bounded in effort (probably 1–2 days), but required
   before the simplification can ship.

3. **Keep both paths indefinitely.** Accept the maintenance cost,
   ship what's there. The dormant F-path remains a test bed.

Recommendation: **option 2**. The gap is small enough that the
investigation cost is reasonable, and if it has a concrete answer
it's the cleanest outcome. If the root cause turns out to be something
unfixable (fundamental μarch behavior), fall back to option 1 — at
which point the simplification is a clean trade of 20% on one
workload for a much smaller interpreter.
