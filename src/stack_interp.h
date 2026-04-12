// Stack VM interpreter using preserve_none + musttail dispatch.
//
// Following the Silverfir-nano design (INTERPRETER_DESIGN.md):
// - Each handler is a preserve_none function
// - Hot state passed through function arguments (registers)
// - musttail guarantees zero-overhead dispatch
// - Direct-threaded: instructions contain handler pointers

#ifndef STACK_INTERP_H
#define STACK_INTERP_H

#include <stdint.h>
#include <stddef.h>
#include <string.h>

// Instruction encoding: 32 bytes (4 x u64).
// [0] handler function pointer
// [1] imm0 (local index, constant, jump offset, etc.)
// [2] imm1 (second operand)
// [3] imm2 (third operand)
typedef struct Instruction {
    void*    handler;
    uint64_t imm[3];
} Instruction;

// Call frame saved on the call stack.
typedef struct CallFrame {
    Instruction* return_pc;    // instruction to resume at after return
    uint64_t*    saved_locals; // caller's locals pointer (= caller's fp)
    uint32_t     func_idx;    // caller's function index (for looking up metadata)
    size_t       saved_frame_size; // frame_stack_size to restore on return
    // Hot local registers are register-only (no memory mirror). The
    // caller's l0/l1/l2 values are preserved here across the call
    // because the callee will overwrite them with its own hot locals.
    // Saved on op_call entry, restored on op_return.
    uint64_t     saved_l0;
    uint64_t     saved_l1;
    uint64_t     saved_l2;
} CallFrame;

// Per-function metadata (set up by Rust, read by C).
typedef struct FuncMeta {
    Instruction* code;         // instruction array for this function
    uint32_t     local_count;  // number of scalar locals (includes params)
    uint32_t     local_memory; // bytes of memory-backed locals (after scalars in frame)
    uint32_t     param_count;  // number of parameters
} FuncMeta;

// Execution context (cold state, accessed through pointer).
typedef struct Ctx {
    // Call stack
    CallFrame*   call_stack;
    uint32_t     call_depth;
    uint32_t     call_stack_cap;

    // Function table
    FuncMeta*    functions;
    uint32_t     func_count;

    // Global variables
    uint8_t*     globals;

    // Unified frame stack (bump-allocated, one buffer for all frames).
    // Each call allocates: max(local_count, 3) u64 slots for scalar
    // locals, followed by ceil(local_memory/8) u64 slots for memory
    // locals (structs/arrays accessed by byte offset). fp points at the
    // start of the scalar locals, and lm = (uint8_t*)(fp + local_count).
    uint64_t*    frame_stack;
    size_t       frame_stack_size;  // current top (in u64 slots)
    size_t       frame_stack_cap;

    // Operand stack base (for bounds checking if needed)
    uint64_t*    stack_base;

    // Float spill stack: backing store for the float TOS window when
    // its depth exceeds 4. Each slot is a float (single precision) —
    // the f-window is typed as `float` so f32 arithmetic never pays
    // double↔float fcvt round-trips. The live "top" pointer lives in
    // the `fsp` handler argument (kept in a register by preserve_none);
    // this field is just the base for bounds checks and the initial
    // value passed to the entry handler. Separate from stack_base so
    // int and float spills don't interleave.
    float*       float_stack;
    size_t       float_stack_cap;

    // Current frame pointer: scalar locals start here, local memory follows.
    // Stored in the context (rather than passed through the handler chain)
    // to keep the handler argument count within preserve_none's register
    // budget on x86-64. Updated on call/return.
    uint64_t*    current_locals;

    // Current float spill pointer. On aarch64 this lives in a GPR as a
    // handler argument (`fsp`) — preserve_none has budget for it. On
    // x86-64 we're out of GPRs, so it's stashed here and handlers access
    // it via a `#define fsp (ctx->current_fsp)` macro, the same trick
    // used for `locals`. Updated on call/return.
    float*       current_fsp;

    // Closure pointer (set by call_closure, read by handlers)
    uint64_t     closure_ptr;

    // Return value
    int64_t      result;

    // Flag: set when we should exit the interpreter
    int          done;

    // Error message set by handlers on a trap (stack overflow, assertion
    // failed, etc.). NULL means "no error" — a normal exit through
    // op_return/op_halt leaves this unchanged. Statically-allocated string
    // pointer; not owned by the context.
    const char*  error;
} Ctx;

// Handler function signature.
// Hot state is passed as arguments so it stays in registers:
//   ctx     - execution context (cold state; also holds current_locals)
//   pc      - current instruction pointer
//   sp      - operand stack pointer (grows upward, points BELOW TOS window)
//   locals  - frame pointer (aarch64 only — see below)
//   l0-l2   - hot local register cache (top 3 locals by access weight)
//   t0-t3   - TOS register window (t0 = top, t3 = deepest in window)
//
// On aarch64, preserve_none exposes enough argument registers that we
// can pass `locals` as a dedicated parameter — fastest possible access.
// On x86-64, the preserve_none GPR budget (~12) is tight, so `locals`
// is stashed in ctx->current_locals instead and read via a macro in
// the handler body. LLVM CSEs the field load so each handler pays at
// most one extra ldr at entry.
//
// With preserve_none, all these arguments stay in hardware registers
// across the entire handler chain — zero memory traffic for values in
// the TOS window.
#define PRESERVE_NONE __attribute__((preserve_none))

// Handler type uses void* for the nh parameter since C can't have
// self-referential function pointer typedefs.
//
// The f0..f3 parameters form a parallel "float TOS window" living in
// FP/SIMD registers (v0-v3 on aarch64, xmm0-xmm3 on x86-64). Float
// handlers read/write these directly, avoiding the GPR↔FP crossings
// that the u64 TOS window would force (3+ cycles each way). Int and
// float values coexist on the logical stack; static types at each
// position tell the codegen which window to use.
//
// The window is typed as `float` (not `double`) so f32 arithmetic
// compiles to direct single-precision FMA/fadd/... instructions
// without the fcvt round-trips that a double-typed window forces on
// every op. f64 values — rare in our hot workloads — still travel
// through the integer window paying GPR↔FP crossings.
#if defined(__aarch64__)
typedef PRESERVE_NONE void (*Handler)(
    Ctx*          ctx,
    Instruction*  pc,
    uint64_t*     sp,
    float*        fsp,    // float spill pointer (lives in a GPR via preserve_none)
    uint64_t*     locals, // frame pointer: scalars, then local memory contiguously
    uint64_t      l0,     // hot local register 0
    uint64_t      l1,     // hot local register 1
    uint64_t      l2,     // hot local register 2
    uint64_t      t0,     // int TOS window (GPRs)
    uint64_t      t1,
    uint64_t      t2,
    uint64_t      t3,
    float         f0,     // float TOS window (FP regs)
    float         f1,
    float         f2,
    float         f3,
    void*         nh      // preloaded handler for the NEXT instruction (cast to Handler)
);
#else
// x86-64 preserve_none has only ~12 GPR arg slots. `locals` and `fsp`
// both live in ctx on this platform; handlers reach them via macros.
typedef PRESERVE_NONE void (*Handler)(
    Ctx*          ctx,
    Instruction*  pc,
    uint64_t*     sp,
    uint64_t      l0,     // hot local register 0
    uint64_t      l1,     // hot local register 1
    uint64_t      l2,     // hot local register 2
    uint64_t      t0,     // int TOS window (GPRs)
    uint64_t      t1,
    uint64_t      t2,
    uint64_t      t3,
    float         f0,     // float TOS window (xmm regs)
    float         f1,
    float         f2,
    float         f3,
    void*         nh      // preloaded handler for the NEXT instruction (cast to Handler)
);
#endif

// ============================================================================
// f32/f64 bit helpers
// ============================================================================

static inline float    as_f32(uint64_t v) { uint32_t b = (uint32_t)v; float f; memcpy(&f, &b, 4); return f; }
static inline uint64_t from_f32(float f)  { uint32_t b; memcpy(&b, &f, 4); return (uint64_t)b; }
static inline double   as_f64(uint64_t v) { double d; memcpy(&d, &v, 8); return d; }
static inline uint64_t from_f64(double d) { uint64_t v; memcpy(&v, &d, 8); return v; }

// ============================================================================
// TOS window macros (4-register window: t0=top, t3=deepest)
//
// All variants spill/fill through memory. sp = stack_base + depth is
// authoritative for current depth; balanced push/pop pairs naturally
// round-trip the caller's t0..t3 through memory across a call.
// ============================================================================

#define PUSH(val) do { *sp++ = t3; t3 = t2; t2 = t1; t1 = t0; t0 = (val); } while(0)
#define POP(dst) do { (dst) = t0; t0 = t1; t1 = t2; t2 = t3; t3 = *--sp; } while(0)
#define DROP1() do { t0 = t1; t1 = t2; t2 = t3; t3 = *--sp; } while(0)
#define BINOP_SHIFT() do { t1 = t2; t2 = t3; t3 = *--sp; } while(0)

// Drop 2 values (t0 and t1 consumed, e.g. stores).
#define DROP2() do { t0 = t2; t1 = t3; t2 = *--sp; t3 = *--sp; } while(0)

// Drop 3 values (t0, t1, t2 consumed, e.g. slice_store32).
#define DROP3() do { t0 = t3; t1 = *--sp; t2 = *--sp; t3 = *--sp; } while(0)

// ============================================================================
// Handler signature and dispatch macros
//
// The frame pointer `locals` is passed as a handler argument on aarch64
// (enough preserve_none arg regs) but stashed in ctx->current_locals on
// other targets (tighter budget on x86-64). Handler bodies read/write
// `locals` the same way on both paths: on aarch64 it's a parameter; on
// x86-64 it's a macro that expands to (ctx->current_locals).
//
// The float TOS window (f0..f3) is a separate 4-slot register window
// living in FP/SIMD registers, independent of the integer window
// (t0..t3) in GPRs. Float ops read/write f0..f3 directly and never
// cross between GPR and FP register files, dodging the ~3-cycle
// fmov/movq penalty that the old "f32 bit-pattern in u64" design
// paid on every float arithmetic op.
// ============================================================================

#if defined(__aarch64__)
#define HANDLER_ARGS Ctx* ctx, Instruction* pc, uint64_t* sp, float* fsp, uint64_t* locals, uint64_t l0, uint64_t l1, uint64_t l2, uint64_t t0, uint64_t t1, uint64_t t2, uint64_t t3, float f0, float f1, float f2, float f3, void* _nh_raw
#else
#define HANDLER_ARGS Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t l0, uint64_t l1, uint64_t l2, uint64_t t0, uint64_t t1, uint64_t t2, uint64_t t3, float f0, float f1, float f2, float f3, void* _nh_raw
// Reads expand to a load of ctx->current_locals / current_fsp; writes
// (locals = X, fsp = X) store back to the same field. LLVM's TBAA rules
// out aliasing with stores through sp, so the field read is CSEd per
// handler. The macros keep the source code identical between aarch64
// and x86-64 — the only difference is where these values live.
#define locals (ctx->current_locals)
#define fsp (ctx->current_fsp)
#endif

#define HANDLER(name) PRESERVE_NONE void name(HANDLER_ARGS)
// Cast nh from void* for use in NEXT macro.
#define nh ((Handler)_nh_raw)

// Dispatch macros. Parameterless — they reference the handler's in-scope
// ctx/pc/sp/l0-l2/t0-t3/f0-f3/_nh_raw arguments directly (plus the
// `locals` parameter on aarch64) and must only be used inside a
// HANDLER() body.
//
// NEXT: linear fall-through. Branch to the preloaded nh and preload the
// handler for the instruction after next.
#if defined(__aarch64__)
#define NEXT() \
    do { \
        Instruction* _next = pc + 1; \
        void* _new_nh = (_next + 1)->handler; \
        __attribute__((musttail)) return ((Handler)_nh_raw)(ctx, _next, sp, fsp, locals, l0, l1, l2, t0, t1, t2, t3, f0, f1, f2, f3, _new_nh); \
    } while(0)

#define DISPATCH() \
    do { \
        Handler _target_h = (Handler)pc->handler; \
        void* _new_nh = (pc + 1)->handler; \
        __attribute__((musttail)) return _target_h(ctx, pc, sp, fsp, locals, l0, l1, l2, t0, t1, t2, t3, f0, f1, f2, f3, _new_nh); \
    } while(0)
#else
#define NEXT() \
    do { \
        Instruction* _next = pc + 1; \
        void* _new_nh = (_next + 1)->handler; \
        __attribute__((musttail)) return ((Handler)_nh_raw)(ctx, _next, sp, l0, l1, l2, t0, t1, t2, t3, f0, f1, f2, f3, _new_nh); \
    } while(0)

#define DISPATCH() \
    do { \
        Handler _target_h = (Handler)pc->handler; \
        void* _new_nh = (pc + 1)->handler; \
        __attribute__((musttail)) return _target_h(ctx, pc, sp, l0, l1, l2, t0, t1, t2, t3, f0, f1, f2, f3, _new_nh); \
    } while(0)
#endif

// Float TOS window push/pop. Spills/refills through `fsp`, a handler
// argument pinned to a GPR by preserve_none — analogous to the integer
// `sp`. Each op pays a single register-indirect store/load (one cycle
// on aarch64) instead of chasing ctx->float_stack and ctx->float_sp_off
// through memory. Separate from stack_base so int and float spills
// don't interleave.
#define FPUSH(val) do { \
    *fsp++ = f3; \
    f3 = f2; f2 = f1; f1 = f0; f0 = (val); \
} while(0)

#define FPOP(dst) do { \
    (dst) = f0; f0 = f1; f1 = f2; f2 = f3; f3 = *--fsp; \
} while(0)

#define FDROP1() do { f0 = f1; f1 = f2; f2 = f3; f3 = *--fsp; } while(0)
#define FBINOP_SHIFT() do { f1 = f2; f2 = f3; f3 = *--fsp; } while(0)

// ============================================================================
// Hot local register dispatch macros
//
// Hot local slots live exclusively in the preserve_none register args
// l0/l1/l2 — there is no memory mirror. A fused handler discovers "this
// operand is hot" by inspecting the slot-index immediate and matching it
// against three reserved magic values:
//
//   HOT_L0 = 0xFFFC  →  l0
//   HOT_L1 = 0xFFFD  →  l1
//   HOT_L2 = 0xFFFE  →  l2
//
// The lowering pass (stack_hot_locals::lower) rewrites slot references
// in fused ops to these magic values whenever `func.hot_locals[n]` is
// populated. Legitimate slot indices are < func.local_count, which is
// bounded by the u16 slot-index space — these magic values sit at the
// very top of u16 and never collide with a real slot.
//
// Two coordinate systems use the same magic-value scheme:
//   * Integer handlers treat pc->imm[k] as a u64 slot index. READ_I
//     compares against the raw magic (0xFFFC/D/E).
//   * Float handlers treat pc->imm[k] as a pre-shifted byte offset
//     (slot * 8), but the bridge leaves magic values unshifted, so
//     READ_F_OFF compares against the same raw magic values.
//
// The slot-index immediate is a per-instruction constant, so every
// branch is perfectly predicted after the first few executions (one
// path taken every time a given instruction fires) — roughly 0–1
// cycles of overhead per dispatch.
//
// The l0/l1/l2 GPRs hold the full u64 for int values. For floats, the
// low 32 bits hold the f32 bit pattern (set via `from_f32(f0)`).
// as_f32 / from_f32 compile to `fmov s, w` / `fmov w, s` — one cycle
// each on aarch64.
// ============================================================================

#define HOT_L0_MAGIC 0xFFFCULL
#define HOT_L1_MAGIC 0xFFFDULL
#define HOT_L2_MAGIC 0xFFFEULL

#define READ_I(s) \
    ((s) == HOT_L0_MAGIC ? l0 : (s) == HOT_L1_MAGIC ? l1 : (s) == HOT_L2_MAGIC ? l2 : locals[s])

#define WRITE_I(s, v) do { \
    uint64_t _wv = (v); \
    if ((s) == HOT_L0_MAGIC) l0 = _wv; \
    else if ((s) == HOT_L1_MAGIC) l1 = _wv; \
    else if ((s) == HOT_L2_MAGIC) l2 = _wv; \
    else locals[s] = _wv; \
} while(0)

#define READ_F_OFF(off) \
    ((off) == HOT_L0_MAGIC ? as_f32(l0) : (off) == HOT_L1_MAGIC ? as_f32(l1) : (off) == HOT_L2_MAGIC ? as_f32(l2) \
        : *(float*)((uint8_t*)locals + (off)))

#define WRITE_F_OFF(off, v) do { \
    float _wf = (v); \
    if ((off) == HOT_L0_MAGIC) l0 = from_f32(_wf); \
    else if ((off) == HOT_L1_MAGIC) l1 = from_f32(_wf); \
    else if ((off) == HOT_L2_MAGIC) l2 = from_f32(_wf); \
    else *(float*)((uint8_t*)locals + (off)) = _wf; \
} while(0)

// Entry point: called from Rust via FFI.
//
// This function performs no heap allocation and is safe to call from
// a realtime audio thread. The caller must pre-populate all runtime
// buffers on the context before the first call: call_stack /
// call_stack_cap, frame_stack / frame_stack_cap, and stack_base.
// A single context can be reused across invocations — done, result,
// error, frame_stack_size, and call_depth are reset on entry.
int64_t stack_interp_run(Ctx* ctx, uint32_t entry_func);

#endif // STACK_INTERP_H
