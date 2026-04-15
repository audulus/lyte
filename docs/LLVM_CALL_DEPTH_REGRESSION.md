# LLVM Call-Depth Check Regression (0.22 → 0.23)

## Symptom

A user reported that reverb DSP code running through the LLVM backend on
macOS regressed by ~2x between Lyte 0.22 and 0.23 when embedded in the
`CLyte.xcframework`.

## Root Cause

Commit `df0377a` ("Propagate runtime traps gracefully across all
backends", Apr 9 2026) landed between 0.22 and 0.23. It added a
per-function call-depth counter to every backend — including the LLVM
JIT — so that runaway recursion can unwind back to the host via
`longjmp` instead of aborting the process.

In `src/llvm_jit.rs` this shows up as two new emitters that run on
**every** compiled function:

- `emit_call_depth_check` (prologue): load
  `globals[CALL_DEPTH_OFFSET]`, subtract 1, store back, compare `< 0`,
  conditional branch to a trap block that calls
  `llvm_lyte_stack_overflow_trap`.
- `emit_call_depth_release` (epilogue, before every return): load
  `globals[CALL_DEPTH_OFFSET]`, add 1, store back.

### Why it is expensive for DSP

Reverb and similar DSP graphs are typically long chains of small
functions — biquads, allpasses, comb filters — each called once per
sample. The regression compounds because:

1. Every call site pays a load / sub / store / compare / branch on
   `globals[CALL_DEPTH_OFFSET]` in the callee prologue.
2. Every return pays another load / add / store.
3. LLVM cannot hoist the traffic out of the per-sample loop: the store
   is observable across calls, so the counter must live in memory, not
   a register.
4. Each function gains a trap basic block, bloating icache pressure
   on an already call-heavy hot loop.

For a DSP chain dominated by tiny leaf functions, doubling the per-call
cost is entirely consistent with a 2x wall-clock regression.

## Ruled-Out Suspects

Other changes between 0.22 and 0.23 touched LLVM-adjacent infrastructure
but not hot-path codegen:

- `6f06bf9` — adds an x86_64 slice to `CLyte.xcframework`. arm64 macOS,
  which is the affected path, is unchanged.
- `f090aba` — switches the **non-LLVM** iOS / x86_64 slices to the stack
  VM. LLVM path untouched.
- `bc72b1c` — pure `cargo fmt`.

## Proposed Fix: Emit the Check Only for Functions in Recursive Cycles

A non-recursive call graph is a DAG and therefore has bounded depth by
construction, so functions that are not in any cycle can never
contribute to unbounded recursion. The check only needs to exist for
functions that are actually part of a recursive cycle, direct or
indirect.

### Algorithm

1. Build the static call graph over the `DeclTable`.
2. Run Tarjan's SCC.
3. A function needs `emit_call_depth_check` / `emit_call_depth_release`
   iff it lies in an SCC with a self-loop or size > 1.
4. Everything else gets a clean prologue/epilogue.

### Handling indirect calls conservatively

Higher-order functions, function pointers, and closure invocations must
be modelled so the analysis stays sound:

- At an indirect call site, add call graph edges to every function
  whose address is taken (or, tighter, every function with a matching
  signature).
- Lyte's existing escape analysis on closures already bounds where
  closure values can flow, so the set of possible indirect targets is
  much smaller than in C.
- FFI / builtin calls (`assert`, `print`, `sincos`, etc.) are leaves
  from Lyte's perspective and can be ignored as call graph edges —
  they cannot re-enter Lyte recursively.

### Expected win

In typical DSP code — including the reverb that triggered this report —
`main` and the entire per-sample call chain form a DAG. Every hot
function on that path would skip the check entirely, recovering
essentially all of the regression. Programs that do use recursion
(`fib`, tree walks, etc.) keep the check only on the recursive SCC,
which is exactly where the trap is meaningful.

### Cheaper first cut

If threading a call graph pass through the existing codegen is
inconvenient, a simpler intermediate step is to skip the check for any
function with **no call instructions at all** (pure leaves). This
captures trivial math helpers and gets a large fraction of the win
without requiring SCC machinery, and the full cycle-aware version can
be layered on afterwards.

## Alternative Mitigations

These are lower-priority but worth noting:

- Gate the depth counter behind a runtime flag that xcframework
  embedders can leave off, since host apps often rely on OS stack
  guards anyway.
- Keep the counter but move it into a thread-local so LLVM can prove
  lack of aliasing across calls — still not free, but potentially
  register-promotable.
