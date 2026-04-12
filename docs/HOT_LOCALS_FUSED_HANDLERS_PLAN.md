# Hot-Local Fused Handlers: Plan

Follow-up to `docs/HOT_LOCALS.md`. That document concluded that the
register-only hot local cache regresses biquad/sort/fft by 3–34%
because every fused handler's `READ_I / WRITE_I / READ_F_OFF /
WRITE_F_OFF` macro adds 3 conditional dispatches on the slot-read
critical path. This plan implements option 2-scoped from that doc:
specialized fused handlers baked for the exact (op, hot-slot)
patterns that appear in the three benchmark hot loops.

Branch: `register-only-hot-locals` (base commit `ec96409`).

## Goal

Eliminate the runtime magic-value dispatch from the three benchmark
hot loops by emitting specialized C handlers whose slot args are
hardcoded to `l0`/`l1`/`l2` (for hot slots) or direct `locals[s]`
access (for cold slots). No branches on the read critical path
inside a specialized handler.

## Observed hot-loop patterns

Enumerated from `cargo run -p lyte-cli --bin lyte -- benchmark/<bench>.lyte --stack-ir`
after `stack_hot_locals::rewrite_hot` has substituted magic values
(`HOT_L0 = 0xFFFC`, `HOT_L1 = 0xFFFD`, `HOT_L2 = 0xFFFE`). Every
(op, hot-pattern) combination that appears:

| StackOp | Hot patterns | Source loops |
|---|---|---|
| `FusedGetGetFMulF(a, b)` | `(L0, _)`, `(_, L1)` | biquad main |
| `FusedGetGetFAddSet(a, b, d)` | `(L0, _, L0)` | biquad main |
| `FusedAddrImmGetStore32(_, off, v)` | `v=L1`, `v=L2` | biquad main |
| `FusedGetGetILtJumpIfZero(a, b, _)` | `(L0, _)`, `(L1, _)`, `(L2, _)` | sort lcg, quicksort, fft |
| `FusedGetAddImmSet(s, _, d)` (s==d) | `L0`, `L1`, `L2` | sort, fft |
| `FusedAddrGetSliceLoad32(_, i)` | `i=L0`, `i=L1` | quicksort |
| `FusedTeeSliceStore32(n, _, i)` | `(L2, _, L1)`, `(_, _, L0)`, `(_, _, L1)` | quicksort |
| `FusedGetSet(a, _)` | `(L1, _)` | quicksort |
| `FusedGetGetIAddSet(a, b, d)` | `(_, L2, L1)`, `(L1, _, L0)` | fft |
| `FusedGetGetILt(a, b)` | `(L0, _)`, `(L1, _)`, `(L0, L1)` | fft |
| `FusedLocalArrayLoad32F(_, i)` | `i=L2` | fft |
| `FusedLocalArrayStore32(_, i)` | `i=L1` | sort |
| `FusedLocalArrayStore32F(_, i)` | `i=L0` | fft |
| `FusedGetFMulF(a)` | `a=L1` | fft |
| `FusedAddrGetSliceLoad32F(_, i)` | `i=L0`, `i=L1` | fft |
| `FusedTeeSliceStore32F(n, _, i)` | `(L2, _, L0)`, `(_, _, L0)`, `(_, _, L1)` | fft |

**Total: 31 specialized handlers.**

Notes:

- For three-arg ops with a `dst` slot, only the combinations actually
  observed appear above. In particular, `FusedGetAddImmSet` only
  shows up as an in-place increment (`s == d`), so we only need
  three handlers for it, not nine.
- `FusedAddrGetSliceLoad32(s, i)`, `FusedTeeSliceStore32(n, s, i)`,
  and their F variants take a *slot* `s` argument that is the slice
  fat-pointer slot, not a scalar local. `stack_hot_locals::
  rewrite_fused_hot_refs` only rewrites the index local `i` (and
  the tee destination `n`), so `s` is never magic for these ops.

## Mechanism — Approach A (bridge-only specialization)

No changes to `StackOp`, fusion rules, hot-local rewrite, stack
depths, or codegen. All changes are confined to:

1. **`src/stack_interp.c`** — add 4 new macros and 31 handler
   functions.
2. **`src/stack_interp_bridge.rs`** — add `extern "C"` decls and
   update `handler_for(op)` to dispatch specialized handlers based
   on the slot args of each StackOp variant.

### 1. New cold-direct macros

```c
#define READ_I_COLD(s)         (locals[s])
#define WRITE_I_COLD(s, v)     (locals[s] = (v))
#define READ_F_OFF_COLD(off)   (*(float*)((uint8_t*)locals + (off)))
#define WRITE_F_OFF_COLD(off, v) (*(float*)((uint8_t*)locals + (off)) = (v))
```

Specialized handlers use `READ_I_COLD` / `WRITE_I_COLD` /
`READ_F_OFF_COLD` / `WRITE_F_OFF_COLD` for cold slots and
`l0`/`l1`/`l2` / `f0`-window-appropriate access for hot slots. No
branches anywhere in a specialized handler.

### 2. Specialized handler naming

`op_<generic>_l<hot-slot>_at<arg-position>` — arg positions named
by the field letters used in the StackOp variant. Examples:

- `op_fused_get_get_fmul_f_l0a` — `FusedGetGetFMulF` with `a = L0`
- `op_fused_get_get_fmul_f_l1b` — `FusedGetGetFMulF` with `b = L1`
- `op_fused_get_get_fadd_set_l0a_l0d` — `FusedGetGetFAddSet`
  with `a = d = L0`
- `op_fused_addr_imm_get_store32_l1v` — `FusedAddrImmGetStore32`
  with `v = L1`
- `op_fused_get_get_ilt_jiz_l0a` — `FusedGetGetILtJumpIfZero` with
  `a = L0`
- `op_fused_get_addimm_set_l0sd` — `FusedGetAddImmSet` with
  `s = d = L0` (in-place increment)
- `op_fused_addr_get_sload32_l0i` — `FusedAddrGetSliceLoad32` with
  `i = L0`
- `op_fused_tee_sstore32_l2n_l1i` — `FusedTeeSliceStore32` with
  `n = L2, i = L1`
- `op_fused_get_get_ilt_l0a_l1b` — `FusedGetGetILt` with
  `a = L0, b = L1`

### 3. Bridge dispatch

In `handler_for(op)`, expand the existing single-variant match to
a pattern match that inspects the slot args:

```rust
StackOp::FusedGetGetFMulF(a, b) => match (*a, *b) {
    (HOT_L0, _) if *b != HOT_L0 && *b != HOT_L1 && *b != HOT_L2
        => op_fused_get_get_fmul_f_l0a as *const (),
    (_, HOT_L1) if *a != HOT_L0 && *a != HOT_L1 && *a != HOT_L2
        => op_fused_get_get_fmul_f_l1b as *const (),
    _ => op_fused_get_get_fmul_f as *const (),
},
```

Same idea for the other 15 fused ops. The dispatch is per-op
compilation only; runtime cost is zero.

Helper: add a `fn slot_hot(s: u16) -> Option<usize>` that returns
`Some(0|1|2)` for magic values, `None` otherwise, to keep the
match arms readable.

### 4. Generic handlers (unchanged — safety net)

Keep the existing generic handlers with `READ_I`/`WRITE_I` dispatch
in place. They still fire on:

- Any (op, pattern) combo we didn't specialize (e.g., patterns from
  non-benchmark code that happen to assign hot slots to fused ops).
- Ops whose slot args happen to be all-cold in a given program —
  they pay the fall-through branch cost, but this is the same cost
  as baseline `main` plus the magic-check prefix. Phase 2 below
  addresses this.

This keeps the change safe: anything we don't specialize still
runs correctly through the generic path.

## Tests / validation

1. `cargo build --workspace` — the 31 new handlers and the bridge
   dispatch must compile.
2. `cargo test --workspace` — full golden suite (329 unit + 4
   golden backends + 21 LSP) must stay green. The hot-local rewrite
   is unchanged, so any regression here would indicate a handler
   body bug.
3. `./benchmark/run.sh 5` — biquad / sort / fft, 5-run average.
   Compare to two baselines:
   - **main** (no hot local cache effectively — the no-op cache):
     biquad ~0.098s, sort ~0.073s, fft ~0.475s.
   - **register-only-hot-locals HEAD** (this branch before the
     patch): biquad 0.131s, sort 0.086s, fft 0.487s.
   Target: at or below main baselines on all three.
4. Spot-check the hot loops after the change with `--stack-ir` to
   confirm no unintended IR differences.

## Scope, cost, risk

- **LOC**: ~250 lines of C (31 × ~8-line handler bodies) + ~150
  lines of Rust in `handler_for`. One new helper (`slot_hot`).
  One change to `stack_interp.c` headers for the cold-direct
  macros.
- **Duration**: 2–4 hours of mechanical work per the HOT_LOCALS.md
  estimate.
- **Risk**: each handler is a 1:1 copy of the generic body with
  magic-dispatch resolved. The main failure mode is a typo in one
  handler; golden tests catch those quickly. The bridge dispatch is
  constrained: it must only fire on exact magic matches, never on
  cold slots that happen to equal `HOT_L0..L2` numerically — but
  `0xFFFC..0xFFFE` are unreachable as real slot indices (slot count
  capped well below that), so any magic match is unambiguous.

## Phase 2 (only if benchmarks don't fully recover)

Specialized handlers only help when the op actually references a
hot slot. In biquad's hot loop, roughly half the ops are all-cold
but still go through the generic handler and still eat the
3-branch magic prefix on every slot read. If Phase 1 doesn't
restore biquad to ≤0.098s, the remaining regression is on those
all-cold paths.

To fix: make the generic handlers branchless by replacing
`READ_I`/`WRITE_I`/`READ_F_OFF`/`WRITE_F_OFF` with the cold-direct
macros, and guarantee that no magic value ever reaches a generic
handler. Either:

- **2a.** Add specialized handlers for every (op, pattern) combo
  that the rewrite pass can produce — not just the benchmark subset.
  Combinatorial, but bounded: the rewrite pass walks ~15 op
  variants with ≤3 slot args, so ≤15 × 64 = 960 combinations in
  the worst case, most of which won't occur in practice.

- **2b.** Make `stack_hot_locals::rewrite_fused_hot_refs` skip the
  rewrite if no specialized variant exists for the resulting
  pattern, and un-fuse the op back to base ops so it can run
  through the L-handlers.

Phase 2 is deferred until Phase 1 benchmark numbers prove it's
needed.

## Non-goals

- No new StackOp variants. Everything lives in C handlers + bridge
  dispatch.
- No changes to the fusion pass or hot-local selection.
- No changes to the L-handlers (`LocalGetL0..L2`, `LocalSetL0..L2`,
  `LocalTeeL0..L2` and their F variants) — those are already the
  fastest path for raw `local.get`/`set`/`tee` of hot slots.
- Not trying to restore the original pre-`register-only-hot-locals`
  behavior. We keep the register-only semantics (`l0/l1/l2` are
  authoritative, no memory mirror) and make the fused path
  cooperate with it.

## References

- `docs/HOT_LOCALS.md` — context, regression numbers, options 1/2/3/4.
- `docs/Stack_VM.md` — stack VM architecture.
- `src/stack_interp.c` lines 210–235 — current `READ_I / WRITE_I /
  READ_F_OFF / WRITE_F_OFF` macro definitions.
- `src/stack_hot_locals.rs` — hot-local selection + rewrite pass.
- `src/stack_interp_bridge.rs` — `handler_for` and `encode_imm`;
  the `shift_f_slot` helper at ~L458-465 for float slot encoding.
