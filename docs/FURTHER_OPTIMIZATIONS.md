# Further Stack VM Optimizations

This note captures plausible next optimizations for the Stack VM backend
after the current `preserve_none` + `musttail` + fused-handler design.
The goal is to focus effort on the remaining high-leverage areas rather
than revisit ideas that have already been measured and rejected.

## Summary

The current backend is already well past the point where small dispatch
micro-optimizations are likely to dominate. The most promising remaining
work is:

1. Expand the fusion surface more systematically.
2. Remove unnecessary result materialization in void context.
3. Eliminate remaining f32 window crossings in generic slice paths.
4. Reduce call overhead, especially for float-heavy helpers.
5. Broaden inlining for pure leaf functions.
6. Fix and optimize composite store-with-offset lowering.

## Prioritized Worklist

### 1. Systematic fusion expansion

**Expected payoff:** High

**Why it looks promising**

- The current peephole optimizer is still a hand-maintained list of
  patterns in `src/stack_optimize.rs`.
- The codebase already has the start of an automatic discovery tool in
  `src/stack_discover.rs`.
- `docs/HOT_LOCALS.md` already points toward two scalable directions:
  scoped specialized fused variants and profile-guided handler/codegen.

**Concrete next steps**

- Run fusion discovery on the real hot programs (`biquad`, `sort`,
  `fft`, golden tests) and rank patterns by dispatches saved.
- Add a small batch of targeted fused handlers for the top missing
  patterns rather than continuing purely ad hoc growth.
- If this backend remains strategically important on iOS, consider
  moving from hand-written fusion rules to generated fusion metadata.

**Relevant files**

- `src/stack_optimize.rs`
- `src/stack_discover.rs`
- `docs/HOT_LOCALS.md`

### 2. Stop materializing unused assignment results

**Expected payoff:** Medium to High

**Why it looks promising**

- `translate_void` explicitly leaves a TODO about assignments whose
  result is truly unused.
- In void context, the backend still often emits value-preserving forms
  (`LocalTee`, temp locals, reloads) only to drop the result later.
- This should matter in loop-heavy code where assignments are used only
  for state updates.

**Concrete next steps**

- Teach assignment lowering to distinguish value-producing and
  statement-only contexts.
- Prefer `LocalSet` / direct store forms over `LocalTee` when the value
  is dead.
- Re-measure `fft` and `biquad`, since both contain hot state-update
  patterns.

**Relevant files**

- `src/stack_codegen.rs`

### 3. Add generic f32 slice load/store ops

**Expected payoff:** Medium

**Why it looks promising**

- The fused float slice handlers already exist.
- Generic f32 slice loads currently fall back to `SliceLoad32` plus
  `BitsToFF`.
- Generic f32 slice stores currently fall back to `FToBitsF` plus
  `SliceStore32`.
- Those extra int/float window crossings are avoidable when the access
  pattern does not match an existing fusion rule.

**Concrete next steps**

- Add `SliceLoad32F` / `SliceStore32F` style ops to `StackOp`,
  stack-depth accounting, the bridge, and the C interpreter.
- Switch generic f32 slice lowering to those ops.
- Keep the existing fused variants for the local-slot fast path.

**Relevant files**

- `src/stack_codegen.rs`
- `src/stack_ir.rs`
- `src/stack_depth.rs`
- `src/stack_interp_bridge.rs`
- `src/stack_interp.c`

### 4. Specialize calls instead of tuning dispatch further

**Expected payoff:** Medium

**Why it looks promising**

- `op_call` still does a runtime `switch(nargs)` and copies arguments
  generically.
- Float arguments still travel through the integer path at call
  boundaries.
- `Call.preserve` is computed and encoded, but the current call handler
  no longer appears to use it for specialization.

**Concrete next steps**

- Audit whether `preserve` is now vestigial; if so, remove it or reuse
  it for call-site specialization.
- Consider arity-specialized call handlers for the common small-arity
  cases.
- Explore a float-argument fast path for pure f32 helper calls if those
  show up in profiles.

**Relevant files**

- `src/stack_interp.c`
- `src/stack_interp_bridge.rs`
- `src/stack_rebase_lm.rs`
- `docs/Stack_VM.md`

### 5. Broaden trivial inlining

**Expected payoff:** Medium

**Why it looks promising**

- The trivial inliner only accepts a very narrow function shape:
  `LocalGet(0..N-1)` followed by stack-only ops and `Return`.
- The allowed op set already includes many pure F-window operations, but
  the entry-shape restriction is still tight.
- This likely leaves easy wins on the table for small helper functions.

**Concrete next steps**

- Relax the matcher to cover more pure leaf bodies.
- Allow more harmless setup ops when they do not change semantics at the
  call site.
- Re-check code size growth so the inliner does not bloat cold paths.

**Relevant files**

- `src/stack_inline.rs`

### 6. Repair and optimize composite store-with-offset lowering

**Expected payoff:** Medium

**Why it looks promising**

- The pointer-type `emit_store_offset` path still contains correctness
  caveats and fallback logic.
- Even where it is correct in practice, it is not generating a strong
  fast path for nested aggregate construction.
- Fixing this improves both code quality and future optimization
  headroom.

**Concrete next steps**

- Make callers explicitly compute `base + offset` before `MemCopy` when
  storing composite fields.
- Remove the current fallback assumptions.
- After correctness is solid, look for common field-copy patterns that
  can fuse.

**Relevant files**

- `src/stack_codegen.rs`

## Lower-Priority Ideas

### 7. Revisit hot locals only with specialization

**Expected payoff:** Unclear without substantial machinery

The current documentation already explains why the old hot-local cache
did not pay for itself. If this is revisited, it should be through
encode-time specialized fused handlers or generated handler families,
not by adding runtime branches back into the dispatch path.

**Relevant files**

- `docs/HOT_LOCALS.md`

### 8. Generated interpreter metadata

**Expected payoff:** Long-term scalability, not a quick win

The hand-written fused-op surface is still manageable, but only barely.
If the Stack VM becomes a primary long-term backend, generating the op
inventory, bridge mapping, stack-depth tables, and maybe handler bodies
from shared metadata would reduce maintenance cost and make larger-scale
specialization practical.

**Relevant files**

- `src/stack_ir.rs`
- `src/stack_interp_bridge.rs`
- `src/stack_depth.rs`
- `src/stack_interp.c`
- `src/stack_discover.rs`

## Not Recommended

- More generic dispatch micro-tuning without first expanding fusion.
- Reintroducing the old hot-local cache design as a runtime mechanism.
- Chasing f64-specific speedups before the remaining f32/codegen issues.

## Suggested Order

If we want the best effort-to-payoff ratio, a reasonable implementation
order is:

1. Unused assignment-result elimination.
2. Generic f32 slice load/store ops.
3. A targeted batch of new fused handlers discovered from profiles.
4. Broader trivial inlining.
5. Call specialization.
6. Larger generated-fusion / generated-handler work, only if the Stack
   VM is a strategic long-term priority.
