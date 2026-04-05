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
//   sp      - operand stack pointer (grows upward)
//   locals  - current frame's scalar locals array
//   lm      - current frame's local memory base pointer
#define PRESERVE_NONE __attribute__((preserve_none))

typedef PRESERVE_NONE void (*Handler)(
    Ctx*          ctx,
    Instruction*  pc,
    uint64_t*     sp,
    uint64_t*     locals,
    uint8_t*      lm
);

// Dispatch to next instruction.
#define DISPATCH(ctx, pc, sp, locals, lm) \
    __attribute__((musttail)) return ((Handler)(pc)->handler)(ctx, pc, sp, locals, lm)

// Dispatch to next sequential instruction.
#define NEXT(ctx, pc, sp, locals, lm) \
    do { Instruction* _next = (pc) + 1; DISPATCH(ctx, _next, sp, locals, lm); } while(0)

// Entry point: called from Rust via FFI.
int64_t stack_interp_run(Ctx* ctx, uint32_t entry_func);

#endif // STACK_INTERP_H
