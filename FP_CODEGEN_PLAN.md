# Float-Window Codegen Refactor Plan

A plan for moving the Stack VM from "f32 values bit-cast through the integer
TOS window" to "f32 values live in a dedicated float TOS window (f0..f3) in
FP/SIMD registers throughout expression evaluation."

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

Baselines to beat (3-run averages, aarch64 M-series):

| Workload | Stack VM (current, commit 5b8c48f) | Lyte LLVM AOT | Silverfir μJIT | Silverfir interp |
|---|---|---|---|---|
| biquad | 0.218s | 0.034s | 0.072s | 0.641s |
| sort | 0.114s | 0.027s | 0.023s | 0.068s |
| fft | 0.612s | 0.043s | 0.054s | 0.656s |

Target after Phase 5 (float-aware fusion):

| Workload | Stack VM target | Rationale |
|---|---|---|
| biquad | 0.15-0.18s | Chain already in FW; Phase 5 adds simpler patterns |
| sort | unchanged | integer-heavy |
| fft | 0.35-0.45s | Currently loses on `LocalGet + FMul` patterns; this is the biggest win |

Silverfir pure-interp is the honest ceiling for an interpreter with this
architecture. We should approach 0.3-0.4x of it on FFT.

Add a floating-point-heavy micro-benchmark to `benchmark/` that exercises
Phase 3 patterns in isolation (e.g., a simple dot-product kernel). Track
per-pattern speedup.

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
