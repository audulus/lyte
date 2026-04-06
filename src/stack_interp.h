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
    uint64_t*    saved_locals; // caller's locals pointer
    uint8_t*     saved_lm;    // caller's local memory base
    uint64_t*    saved_sp;    // caller's stack pointer (for truncating on return)
    uint32_t     func_idx;    // caller's function index (for looking up metadata)
    size_t       saved_lm_size; // local_memory_size to restore on return
} CallFrame;

// Per-function metadata (set up by Rust, read by C).
typedef struct FuncMeta {
    Instruction* code;         // instruction array for this function
    uint32_t     local_count;  // number of scalar locals (includes params)
    uint32_t     local_memory; // bytes of memory-backed locals
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

    // Local memory pool (all frames share one growable buffer)
    uint8_t*     local_memory;
    size_t       local_memory_size;
    size_t       local_memory_cap;

    // Operand stack base (for bounds checking if needed)
    uint64_t*    stack_base;

    // Closure pointer (set by call_closure, read by handlers)
    uint64_t     closure_ptr;

    // Return value
    int64_t      result;

    // Flag: set when we should exit the interpreter
    int          done;
} Ctx;

// Handler function signature.
// Hot state is passed as arguments so it stays in registers:
//   ctx     - execution context (cold state)
//   pc      - current instruction pointer
//   sp      - operand stack pointer (grows upward, points BELOW TOS window)
//   locals  - current frame's scalar locals array
//   lm      - current frame's local memory base pointer
//   t0-t3   - TOS register window (t0 = top, t3 = deepest in window)
//
// With preserve_none, all arguments stay in hardware registers across
// the entire handler chain — zero memory traffic for values in the window.
#define PRESERVE_NONE __attribute__((preserve_none))

// Handler type uses void* for the nh parameter since C can't have
// self-referential function pointer typedefs.
typedef PRESERVE_NONE void (*Handler)(
    Ctx*          ctx,
    Instruction*  pc,
    uint64_t*     sp,
    uint64_t*     locals,
    uint8_t*      lm,
    uint64_t      t0,
    uint64_t      t1,
    uint64_t      t2,
    uint64_t      t3,
    void*         nh      // preloaded handler for the NEXT instruction (cast to Handler)
);

// Linear dispatch: branch to preloaded nh, preload handler for instruction after next.
// nh is available as a macro in the handler (cast from _nh_raw).
#define NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3) \
    do { \
        Instruction* _next = (pc) + 1; \
        void* _new_nh = (_next + 1)->handler; \
        __attribute__((musttail)) return ((Handler)_nh_raw)(ctx, _next, sp, locals, lm, t0, t1, t2, t3, _new_nh); \
    } while(0)

// Non-linear dispatch: reload handler from target, preload the one after.
// Used by jumps, calls, returns.
#define DISPATCH(ctx, pc, sp, locals, lm, t0, t1, t2, t3) \
    do { \
        Handler _target_h = (Handler)(pc)->handler; \
        void* _new_nh = ((pc) + 1)->handler; \
        __attribute__((musttail)) return _target_h(ctx, pc, sp, locals, lm, t0, t1, t2, t3, _new_nh); \
    } while(0)

// Entry point: called from Rust via FFI.
int64_t stack_interp_run(Ctx* ctx, uint32_t entry_func);

#endif // STACK_INTERP_H
