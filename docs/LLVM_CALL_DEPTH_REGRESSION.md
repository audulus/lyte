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

## Fix Options Considered

Three approaches were discussed, in order of preference:

### 1. SCC-aware emission with DAG-tail reservation

Only emit the check for functions that participate in a recursive cycle,
but have each SCC member **reserve its worst-case DAG tail** on entry
rather than just decrementing by 1. This preserves soundness (a
recursive function cannot walk the counter close to zero and then push
actual stack depth past the limit via a chain of non-SCC leaves that
never touch the counter).

**Algorithm:**

1. Build the static call graph over the `DeclTable`.
2. Run Tarjan's SCC and condense the graph.
3. Compute, for every function `f`:

   ```
   subtree_max(f) = 1 + max over DAG successors of subtree_max(g)
   ```

   via a reverse-topological walk over the condensed graph. Each SCC is
   treated as a single node whose `subtree_max` is the max across its
   members.
4. Codegen rules:
   - **SCC members** (function in an SCC with size > 1 or a self-loop):
     on entry, decrement counter by `subtree_max(f)` and check; on
     return, add `subtree_max(f)` back.
   - **Non-SCC functions**: skip both the check and the release. Any
     enclosing SCC entry already reserved enough budget to cover their
     frames.
   - **Pure DAG programs**: no SCC members exist, so every function
     skips. A single static assertion
     `max_dag_depth_from(main) ≤ MAX_CALL_DEPTH` at compile time
     discharges the bound.

**Worked examples:**

| Program                                             | Behaviour                                                            |
| --------------------------------------------------- | -------------------------------------------------------------------- |
| `fib` self-recursion, no descendants                | `subtree_max(fib) = 1` — decrements by 1 per call, same cost as today |
| `walk_tree` ⟲ `walk_tree` → `process` → `hash` (tail depth 2) | `walk_tree` decrements by 3 on entry; `process` and `hash` skip |
| Reverb DSP chain, no recursion anywhere             | Every function skips; all regression recovered                       |

**Indirect calls** must be modelled conservatively: at an indirect call
site, add edges to every function whose address is taken (or, tighter,
every function with a matching signature). Lyte's existing escape
analysis on closures already bounds where closure values can flow, so
the set of possible indirect targets is much smaller than in C. FFI and
builtin calls (`assert`, `print`, `sincos`, etc.) are leaves from
Lyte's perspective and can be ignored as call graph edges.

**False positives:** reserving `subtree_max > 1` per recursive call
means a recursion that previously succeeded at depth `D` now traps at
roughly `D / subtree_max`. In practice `subtree_max` for typical tails
is 5–10, and `MAX_CALL_DEPTH` can be raised to compensate since the
accounting is now precise. For `fib`-shaped pure recursion the
reservation is 1, unchanged.

### 2. Disallow recursion entirely

Since Lyte already forbids recursive data structures
("no recursive data structures, use arrays with indices instead" from
`CLAUDE.md`), a natural further step is to forbid recursive *functions*
as well. DSP code — the dominant Lyte workload — essentially never uses
recursion.

Gains:

- **Every backend gets simpler.** `emit_call_depth_check` /
  `emit_call_depth_release` disappear from LLVM, Cranelift, the VMs,
  and the ARM64 ASM path. No more `globals[CALL_DEPTH_OFFSET]`
  load/store/branch per call.
- **longjmp/setjmp machinery for stack overflow disappears too** —
  the only trap kinds left are assert-fail and cancel, which already
  need longjmp for other reasons.
- **Fully bounded stack usage at compile time.** Since the call graph
  is a DAG, `max_dag_depth_from(main)` is a compile-time constant and
  can be statically checked against any target's stack budget. Free
  stack-overflow safety, zero runtime cost.
- **Inlining, escape analysis, and monomorphization all get simpler**
  when the call graph has no cycles.
- **Zero regression on reverb and every other DSP program**, because
  DSP never uses recursion in practice.

**Cost:** a handful of existing tests use recursion. As of
`317cb1a`, the recursive tests in the suite are:

| File | Functions | Kind |
| --- | --- | --- |
| `tests/cases/recursion.lyte` | `fact` | direct |
| `tests/cases/recursion_advanced.lyte` | `fib`, `is_even` ↔ `is_odd` | direct + mutual |
| `tests/cases/examples/functions.lyte` | `fact` | direct |
| `tests/cases/traps/stack_overflow.lyte` | `recurse` | intentional infinite |

The stdlib's recursive `quicksort` was already rewritten as iterative
in commits `2b854ad` and `317cb1a`, using two explicit `[i32; 64]`
stacks with the "push larger half, loop on smaller" discipline that
caps stack depth at ⌈log2 a.len⌉ ≤ 31. This also fixed a latent O(n)
recursion-depth bug on sorted / reverse-sorted / all-equal inputs.

A global ban would require rewriting the three first tests iteratively
(trivial: `fib`, `fact`, and `is_even`/`is_odd` → `n % 2`) and deleting
the stack-overflow trap test — the trap machinery it exercises would
no longer exist.

### 3. Opt-in `--no-recursion` flag (recommended)

Rather than globally banning recursion, expose it as a compile-time
opt-in flag that mirrors Lyte's existing `--allow-assume` pattern. This
is the preferred design because it preserves backward compatibility,
keeps the existing test suite running under the default mode, and lets
the xcframework build opt in without disturbing anyone else.

**Flag semantics (`--no-recursion`):**

1. **Safety checker:** build the static call graph, run Tarjan's SCC,
   reject any cycle with a clear error pointing at the offending
   function(s). Indirect calls handled conservatively as in option 1.
2. **LLVM / Cranelift codegen:** skip `emit_call_depth_check` and
   `emit_call_depth_release` entirely. Zero prologue/epilogue overhead,
   full regression recovered.
3. **Trap machinery:** stays compiled in for default mode, so
   `tests/cases/traps/stack_overflow.lyte` continues to pass. Under
   `--no-recursion`, the counter simply isn't decremented, so the trap
   is unreachable.
4. **Static guarantee:** with the flag set, emit
   `max_dag_depth_from(main) ≤ MAX_CALL_DEPTH` as a compile-time
   assertion. "Probably won't overflow the stack" becomes a proved
   property.

**Test suite:** nothing needs to change. Recursive tests run under the
default mode. The `recursion*.lyte` and `examples/functions.lyte` tests
would fail under `--no-recursion`, which is exactly the expected
behaviour — they could optionally gain a `// skip-flag: --no-recursion`
directive if the flag ever gets tested in CI.

**Rollout plan:**

1. Land the SCC-based safety-checker pass behind `--no-recursion`.
2. Thread the flag through to the LLVM and Cranelift codegens so they
   skip the call-depth emission.
3. Update `build-xcframework.sh` to pass `--no-recursion`, recovering
   the reverb regression without touching any user code or tests.
4. Optionally: expose the flag in the Audulus host's compile call and
   document it in `docs/lyte-dsp-quickstart.md` for DSP authors.

**Naming note:** `--no-recursion` is precise but narrow. A broader name
like `--realtime` could eventually gate other realtime-hostile features
(allocation, unbounded loops, non-deterministic work) under one
umbrella. Pick the broader name now if more such constraints are
expected; otherwise stay specific and add `--realtime` later as a
meta-flag that implies `--no-recursion`.

## Alternative Mitigations

Lower-priority fallbacks that don't require a call-graph pass:

- Gate the depth counter behind a runtime flag that xcframework
  embedders can leave off, since host apps often rely on OS stack
  guards anyway.
- Keep the counter but move it into a thread-local so LLVM can prove
  lack of aliasing across calls — still not free, but potentially
  register-promotable.
