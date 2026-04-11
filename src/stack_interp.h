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
    uint64_t*    saved_sp;    // caller's stack pointer (for truncating on return)
    float*       saved_fsp;   // caller's float spill pointer
    uint32_t     func_idx;    // caller's function index (for looking up metadata)
    size_t       saved_frame_size; // frame_stack_size to restore on return
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
typedef PRESERVE_NONE void (*Handler)(
    Ctx*          ctx,
    Instruction*  pc,
    uint64_t*     sp,
    float*        fsp,    // float spill pointer (lives in a GPR via preserve_none)
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
