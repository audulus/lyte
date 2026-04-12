# Stack VM Hot Locals: Design Space Exploration

A record of what was tried in the branch `register-only-hot-locals`
(commit `ec96409`), what worked, what didn't, and what's worth
coming back to. This document lives on `main`; the working code
for the approaches discussed below is on the branch.

## The starting point

The stack VM's "hot local cache" was, on paper, three preserve_none
GPR arguments `l0`, `l1`, `l2` that cached the top-3 most-accessed
local slots per function. After analysis picked the hottest slots,
`stack_hot_locals::remap` physically swapped them into frame
positions 0/1/2, and `lower()` rewrote `LocalGet(0/1/2)` /
`LocalSet(0/1/2)` to dedicated `LocalGetL0..L2` / `LocalSetL0..L2`
handlers.

### What the cache was actually doing

The baseline handlers:

```c
HANDLER(op_local_get_l0) { l0 = locals[0]; PUSH(l0); NEXT(); }
HANDLER(op_local_set_l0) { POP(l0); locals[0] = l0; NEXT(); }
```

`op_local_get_l0` loads `locals[0]` from memory into `l0`, then
pushes `l0` onto the TOS window. The `l0 = locals[0]` line is
equivalent to `t0 = locals[0]` — no downstream handler ever reads
`l0` as an input (only `op_local_set_l0` writes to it, and then
`op_local_get_l0` overwrites it on the very next read). The
preserve_none arg slot flowed through the entire dispatch chain
carrying nothing useful.

**The cache was a no-op.** Every benchmark on main that references
biquad's ~0.098s Stack VM number was measuring a VM where
`l0/l1/l2` were, as far as data flow goes, dead registers.

That observation is the premise for everything below.

## What we tried

### Attempt 1: FP hot local cache (`fl0/fl1/fl2`)

Extend the handler signature with three additional FP register
args for float hot locals, split the analysis into independent
int and float top-3 pools, and place them in frame slots 0..2 and
3..5 respectively.

- **Correctness:** full test suite green, including an instructive
  off-by-one fix when bumping `local_count` to 6 after the rebase
  pass had already shifted memory-slot references using the
  pre-bump value.
- **Code size:** handler signature grew from 17 to 20 preserve_none
  args on aarch64 (12 GPR + 7 FP), comfortably within the
  preserve_none budget on both aarch64 and x86-64.
- **Performance:** biquad −4% (0.098 → 0.094s), sort flat, FFT flat
  — but only because the split pulled one more float hot local
  (`phase`) into the cache, saving one instruction per access on a
  handful of `LocalGetL*F` calls. The int side of the cache was
  still doing nothing.
- **Verdict:** architecturally interesting, performance marginal.
  Stashed and then abandoned in favor of solving the underlying
  no-op cache problem.

### Attempt 2: register-only hot locals

Make `l0/l1/l2` the *sole* storage for hot local values. No memory
mirror. Get handlers just push from the register; set handlers
just pop into it. Call and return boundaries save/restore through
new `saved_l0/l1/l2` fields in `CallFrame`.

The coherence problem: under the baseline design, any fused op
that wrote `locals[0..2]` from memory didn't need to update the
register, because the next `LocalGetL0` would reload from memory
anyway. Under register-only, that reload is gone — so *every*
fused op whose slot args might be hot has to route reads and
writes through the register.

We explored three ways to handle this:

#### 2a. Skip fusion when slot args are hot

If any slot arg of a candidate fusion is a hot slot, refuse to
fuse. The raw `LocalGet + ... + LocalSet` sequence remains and
gets lowered to L-handlers, which are register-only and therefore
correct.

- **Cost:** every fused-op dispatch that would have been
  eliminated becomes 3–4 separate dispatches. For biquad's
  `phase = phase + phase_inc`, the fused `FusedGetGetFAddSet(0,3,0)`
  became `LocalGetL0F + FusedGetFAddF(3) + LocalSetL0F` —
  2 extra dispatches × 10M samples ≈ 20ms of regression.
- **Verdict:** correct, trivial to implement, but strictly slower
  than the baseline no-op cache because unfused sequences on hot
  slots are more expensive than the fused memory-backed sequences
  they replace.

#### 2b. Register-specialized fused variants

For each existing fused op, emit a specialized variant whose
slot args are hardcoded to a specific l-register. E.g. change
`FusedGetGetFAddSet(a, b, d)` into `FusedL0FAddMemL0(mem_off)`
when `a == d` is hot and `b` isn't.

The problem is combinatorial. Per op-arity:

| arity | combinations per op | variant count (rough) |
|---|---|---|
| 1 slot | 4 (cold, L0, L1, L2) | 3 per op × 17 ops ≈ **51** |
| 2 slot | 16 (4²), cut by commutativity | 9–15 per op × 12 ops ≈ **132** |
| 3 slot | 64 (4³), cut by commutativity | 35–63 per op × 7 ops ≈ **329** |

≈ **512 hand-written handlers** to cover every combination for the
current fused op inventory. Each needs a C handler, a StackOp enum
entry, a Display impl, bridge handler-pointer wiring, bridge
imm encoding, a `stack_depth` entry, and a fusion rule update.
Roughly 15,000 lines of mechanical code. Impractical to write by
hand.

A scoped version — only the specific biquad / sort / FFT hot-loop
patterns — would need on the order of 15–25 variants and is the
most pragmatic path if we return to this problem. The branch ships
with 9 such variants as a partial proof-of-concept
(`FusedConstSetL0/1/2`, `FusedF32ConstSetL0/1/2`,
`FusedAddrImmL0/1/2Store32`, `FusedGetL0/1/2FMulF`) — they were
subsequently reverted in favor of approach 2c.

#### 2c. Runtime dispatch via magic slot indices

Keep exactly one C handler per fused op. Reserve three slot
indices at the top of the `u16` space — `HOT_L0 = 0xFFFC`,
`HOT_L1 = 0xFFFD`, `HOT_L2 = 0xFFFE` — that can't collide with any
legitimate slot. A new `rewrite_hot` pass runs after fusion,
replacing scalar-local slot refs in every op with the corresponding
magic value when the slot is a hot local. Dispatch macros
`READ_I / WRITE_I / READ_F_OFF / WRITE_F_OFF` check for the magic
values and route to `l0/l1/l2` or to `locals[imm]`.

Raw `LocalGet` / `LocalSet` / `LocalTee` on hot slots still get
lowered to the specialized `LocalGetL0..L2` / `LocalSetL0..L2` /
`LocalTeeL0..L2` handlers (also added: float `LocalTeeL0F..L2F`) —
those are the fastest path for the common case where a hot local
is read or written as a standalone op with no surrounding fusion.

The bridge's float encoding now calls a `shift_f_slot` helper that
pre-shifts normal slot indices by 8 for the aarch64 `ldr s, [base,
imm]` fast path but passes magic values through unshifted, so the
C macros can match against the same `HOT_L*` constants across both
windows.

- **Correctness:** full test suite green (329 unit + 4 golden
  backends + 21 LSP). Several bugs fell out of this path that were
  latent under the no-op cache design:

  - `FusedConstSet` init writing memory but not register (the
    `var i: i32` pattern in `while.lyte`).
  - Call / return boundaries reloading `l0/l1/l2` from stale
    `locals[0..2]` instead of saving through the call frame
    (biquad `iir_filter.lyte`).
  - `LocalTee` wasn't in the lowering pass and silently wrote
    memory without updating the register (`type_cast_chain.lyte`,
    `lambda_closure.lyte`).
  - A pre-existing bug in `remap()`'s swap logic — it computed
    `position_of(target_idx as value)` instead of `target_idx as
    position`. Invisible under the old memory-mirror design,
    surfaced immediately under register-only (`const_array_size.lyte`).
  - The runtime dispatch collided with parameter slots 0/1/2 on
    functions with `param_count > 0` (`quicksort$i32`). The magic
    slot indices were introduced specifically to fix this.
  - The bridge pre-shifting magic values by 8 for float ops, so
    the handler's `READ_F_OFF` check couldn't match (`biquad.lyte`
    example).

- **Performance (10-run avg, Apple M4, release):**

  | workload | main baseline | register-only | Δ |
  |---|---|---|---|
  | biquad | ~0.098s | **0.131s** | **+34%** |
  | sort | ~0.073s | **0.086s** | **+18%** |
  | FFT | ~0.475s | **0.487s** | **+3%** |

  All three regress. The branches added to every fused handler's
  dispatch macros cost more than the register cache delivers. For
  biquad's hot loop (~10 fused ops per sample, 3 branches per slot
  read), that's on the order of 300M extra branch resolutions
  across the 10M-sample run, which matches the ~33ms delta on
  biquad.

- **Verdict:** architecturally correct, semantically the intended
  design, but a net loss because the runtime dispatch cost is
  amortized across *every* fused op — including the ones that
  don't touch hot slots at all.

## Why it didn't pay off

The fundamental tension: Lyte's Stack VM aggressively fuses local
operand patterns (inspired by the aggressive peephole fusion in
`stack_optimize.rs`), while Silverfir-nano — the dispatch-design
inspiration — is a WebAssembly interpreter and does essentially no
fusion. In Silverfir-nano, each local access is its own op, so a
3-variant hot local cache covers almost everything at zero
interaction cost with surrounding ops.

In Lyte, fused 3-address ops like `FusedGetGetFAddSet(a, b, d)`
combine a read, a read, and a write across three local slots in
one handler. Any slot in a fused op can be hot. Making the hot
local cache actually work requires every such handler to either:

1. be specialized per hotness combination (combinatorial
   explosion), or
2. branch on slot hotness at runtime (adds dispatch overhead
   everywhere, swamps the savings), or
3. simply not fuse when a slot is hot (trades dispatch count for
   dispatch overhead — loses).

The baseline's cache was a no-op specifically *because* it didn't
have to solve this problem: `l0/l1/l2` never had to be
authoritative, so fused ops could write memory freely and the
L-handlers reloaded from memory on every read.

## What the Stack VM should probably do

Three plausible next steps, none of which the branch currently
implements as the shipping answer:

### 1. Honestly delete the hot local cache

Remove `l0/l1/l2` from the handler signature. Delete
`stack_hot_locals`'s remap/lower pipeline. Delete the
`LocalGetL*` / `LocalSetL*` / `LocalTeeL*` ops (and their float
counterparts). Reclaim the 3 GPR arg slots for something that
actually uses them every dispatch:

- **Deeper TOS window.** Expand `t0..t3` to `t0..t6`, eliminating
  spill traffic for the FMA chain in biquad and the theta / wr /
  wi chain in FFT. Likely the biggest single win on our current
  workloads because spill depth ≥ 4 is exactly what those hot
  loops trip over.
- **x86-64 `locals` / `fsp` parity.** Today those two live in
  `ctx` on x86-64 because the preserve_none GPR budget is tight.
  Freeing three slots lets x86-64 promote them to real handler
  args and eliminate one field load per handler entry.
- **Globals pointer.** Every `GlobalAddr` op currently chases
  `ctx->globals`; a register for it reduces it to an `add`.

This is the simplest direction and the most honest one. It
accepts that the "hot local cache" as designed was never doing
anything and moves the register budget to work that compounds.

### 2. Scoped specialized fused variants

Pick the hottest fused op patterns from the current benchmarks
(biquad's `phase += phase_inc`, the FMA coefficient reads, the
state field stores; FFT's butterfly tee+slice-store) and add the
15–25 handler variants needed to cover them. Bridge picks the
specialized handler pointer at encode time based on each
instruction's slot args, so the op surface in Rust stays the
same — only the C handler count grows. This is the approach
Silverfir-nano implicitly takes (few but carefully chosen
specializations).

Probably ~2–4 hours of targeted work. Expected to put biquad /
sort / FFT ahead of baseline because the registers finally do
something *and* fusion keeps its 1-dispatch form on the patterns
that matter.

### 3. Leave it alone

Baseline's no-op cache is fast enough. Biquad at 0.098s is already
within 2.5–3× of `-O3` C. Document that the hot local cache is a
vestigial design and move the optimization budget elsewhere.

## References

- Branch: `register-only-hot-locals` (`ec96409`).
- Related doc: `docs/Stack_VM.md` — architecture of the Stack VM,
  including the fusion layer that made register-only hot locals
  hard.
- Related doc: `docs/FP_CODEGEN_PLAN.md` — §Phase 7 "Float hot
  local cache" was also discussed during this exploration and
  shipped briefly on the stashed `fp-split` branch.
