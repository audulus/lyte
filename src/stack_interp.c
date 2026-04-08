// Stack VM interpreter using preserve_none + musttail dispatch.
// See INTERPRETER_DESIGN.md for the design rationale.

#include "stack_interp.h"
#include <math.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>

// ============================================================================
// Helper: enter a function (allocate locals + local memory)
// ============================================================================

static void enter_function(
    Ctx* ctx,
    uint32_t func_idx,
    uint64_t* args,
    uint32_t arg_count,
    uint64_t** out_locals,
    uint8_t** out_lm
) {
    FuncMeta* meta = &ctx->functions[func_idx];

    // Bump-allocate scalar locals from the pre-allocated locals stack.
    // Always allocate at least 3 for hot local register spill/fill.
    uint32_t alloc_count = meta->local_count < 3 ? 3 : meta->local_count;
    size_t ls_base = ctx->locals_stack_size;
    size_t ls_needed = ls_base + alloc_count;
    if (ls_needed > ctx->locals_stack_cap) {
        fprintf(stderr, "stack_interp: locals stack overflow (%zu slots needed, %zu cap)\n",
                ls_needed, ctx->locals_stack_cap);
        exit(1);
    }
    uint64_t* new_locals = ctx->locals_stack + ls_base;
    ctx->locals_stack_size = ls_needed;

    // Copy arguments into locals 0..arg_count-1.
    for (uint32_t i = 0; i < arg_count && i < meta->param_count; i++) {
        new_locals[i] = args[i];
    }

    // Zero non-param locals that the function declares. Slots beyond
    // local_count (the hot-local padding) are not touched by any instruction
    // when the function has no hot-local ops, so we don't need to clear them.
    // Param slots are overwritten by the arg copy above.
    if (meta->local_count > meta->param_count) {
        memset(new_locals + meta->param_count, 0,
               (meta->local_count - meta->param_count) * sizeof(uint64_t));
    }

    // Allocate local memory from a pre-allocated fixed buffer.
    // We must NOT realloc because callers may hold raw pointers into this buffer
    // (e.g., output pointers for struct returns pushed before the call).
    size_t lm_base = ctx->local_memory_size;
    size_t needed = lm_base + meta->local_memory;
    if (needed > ctx->local_memory_cap) {
        fprintf(stderr, "stack_interp: local memory overflow (%zu bytes needed, %zu cap)\n",
                needed, ctx->local_memory_cap);
        exit(1);
    }
    memset(ctx->local_memory + lm_base, 0, meta->local_memory);
    ctx->local_memory_size = needed;

    *out_locals = new_locals;
    *out_lm = ctx->local_memory + lm_base;
}

// ============================================================================
// f32/f64 bit helpers
// ============================================================================

static inline float    as_f32(uint64_t v) { uint32_t b = (uint32_t)v; float f; memcpy(&f, &b, 4); return f; }
static inline uint64_t from_f32(float f)  { uint32_t b; memcpy(&b, &f, 4); return (uint64_t)b; }
static inline double   as_f64(uint64_t v) { double d; memcpy(&d, &v, 8); return d; }
static inline uint64_t from_f64(double d) { uint64_t v; memcpy(&v, &d, 8); return v; }

// ============================================================================
// Integer power
// ============================================================================

static int64_t ipow(int64_t base, uint32_t exp) {
    int64_t result = 1;
    while (exp > 0) {
        if (exp & 1) result *= base;
        base *= base;
        exp >>= 1;
    }
    return result;
}

// ============================================================================
// TOS window macros (4-register window: t0=top, t3=deepest)
// ============================================================================

// Deep variants (depth >= 4): spill/fill through memory.
#define PUSH(val) do { *sp++ = t3; t3 = t2; t2 = t1; t1 = t0; t0 = (val); } while(0)
#define POP(dst) do { (dst) = t0; t0 = t1; t1 = t2; t2 = t3; t3 = *--sp; } while(0)
#define DROP1() do { t0 = t1; t1 = t2; t2 = t3; t3 = *--sp; } while(0)
#define BINOP_SHIFT() do { t1 = t2; t2 = t3; t3 = *--sp; } while(0)

// Shallow variants (depth < 4): pure register, no memory access.
#define PUSH_S(val) do { t3 = t2; t2 = t1; t1 = t0; t0 = (val); } while(0)
#define POP_S(dst) do { (dst) = t0; t0 = t1; t1 = t2; t2 = t3; } while(0)
#define DROP1_S() do { t0 = t1; t1 = t2; t2 = t3; } while(0)
#define BINOP_SHIFT_S() do { t1 = t2; t2 = t3; } while(0)

// Drop 2 values (t0 and t1 consumed, e.g. stores).
#define DROP2() do { t0 = t2; t1 = t3; t2 = *--sp; t3 = *--sp; } while(0)
#define DROP2_S() do { t0 = t2; t1 = t3; } while(0)

// Drop 3 values (t0, t1, t2 consumed, e.g. slice_store32).
#define DROP3() do { t0 = t3; t1 = *--sp; t2 = *--sp; t3 = *--sp; } while(0)
#define DROP3_S() do { t0 = t3; } while(0)

// Convenience macros for handlers (pass all 12 args).
// Note: NEXT uses preloaded nh. DISPATCH reloads from target.
#define NEXT_ALL() NEXT(ctx, pc, sp, locals, lm, l0, l1, l2, t0, t1, t2, t3)
#define DISPATCH_ALL() DISPATCH(ctx, pc, sp, locals, lm, l0, l1, l2, t0, t1, t2, t3)

// Spill all 4 TOS registers to memory (for calls).
#define SPILL_ALL() do { *sp++ = t3; *sp++ = t2; *sp++ = t1; *sp++ = t0; } while(0)

// Fill all 4 TOS registers from memory (for return void).
#define FILL_ALL() do { t0 = *--sp; t1 = *--sp; t2 = *--sp; t3 = *--sp; } while(0)

// Fill t1-t3 from memory (for return with value, t0 = result).
#define FILL_BELOW() do { t1 = *--sp; t2 = *--sp; t3 = *--sp; } while(0)

// Handler signature shorthand.
#define HANDLER_ARGS Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t l0, uint64_t l1, uint64_t l2, uint64_t t0, uint64_t t1, uint64_t t2, uint64_t t3, void* _nh_raw
#define HANDLER(name) PRESERVE_NONE void name(HANDLER_ARGS)
// Cast nh from void* for use in NEXT macro.
#define nh ((Handler)_nh_raw)

// ============================================================================
// Handlers
// ============================================================================

// --- Constants (push) ---

HANDLER(op_i64_const) {
    PUSH(pc->imm[0]);
    NEXT_ALL();
}

HANDLER(op_f32_const) {
    PUSH(pc->imm[0]); // already encoded as f32 bits in u64
    NEXT_ALL();
}

HANDLER(op_f64_const) {
    PUSH(pc->imm[0]);
    NEXT_ALL();
}

// --- Local variables ---

HANDLER(op_local_get) {
    PUSH(locals[pc->imm[0]]);
    NEXT_ALL();
}

HANDLER(op_local_set) {
    uint64_t val; POP(val);
    locals[pc->imm[0]] = val;
    NEXT_ALL();
}

HANDLER(op_local_tee) {
    locals[pc->imm[0]] = t0; // peek, don't pop
    NEXT_ALL();
}

HANDLER(op_local_addr) {
    PUSH((uint64_t)(lm + pc->imm[0] * 8));
    NEXT_ALL();
}

// --- Hot local registers (l0/l1/l2) ---

// Get handlers reload from locals[] to stay in sync with fused ops that
// write to locals[0/1/2] via memory. The reload also refreshes the register
// for subsequent accesses.
HANDLER(op_local_get_l0) { l0 = locals[0]; PUSH(l0); NEXT_ALL(); }
HANDLER(op_local_get_l1) { l1 = locals[1]; PUSH(l1); NEXT_ALL(); }
HANDLER(op_local_get_l2) { l2 = locals[2]; PUSH(l2); NEXT_ALL(); }
// Set handlers write to both register and locals[] to keep them in sync.
// Fused ops that write to locals[0/1/2] via memory still work correctly
// because the next LocalGetL0 will read from the register, which is also updated.
HANDLER(op_local_set_l0) { POP(l0); locals[0] = l0; NEXT_ALL(); }
HANDLER(op_local_set_l1) { POP(l1); locals[1] = l1; NEXT_ALL(); }
HANDLER(op_local_set_l2) { POP(l2); locals[2] = l2; NEXT_ALL(); }

// Shallow variants (depth < 4).
HANDLER(op_local_get_l0_s) { l0 = locals[0]; PUSH_S(l0); NEXT_ALL(); }
HANDLER(op_local_get_l1_s) { l1 = locals[1]; PUSH_S(l1); NEXT_ALL(); }
HANDLER(op_local_get_l2_s) { l2 = locals[2]; PUSH_S(l2); NEXT_ALL(); }
HANDLER(op_local_set_l0_s) { POP_S(l0); locals[0] = l0; NEXT_ALL(); }
HANDLER(op_local_set_l1_s) { POP_S(l1); locals[1] = l1; NEXT_ALL(); }
HANDLER(op_local_set_l2_s) { POP_S(l2); locals[2] = l2; NEXT_ALL(); }

// --- Global variables ---

HANDLER(op_global_addr) {
    PUSH((uint64_t)(ctx->globals + (int32_t)pc->imm[0]));
    NEXT_ALL();
}

// --- Integer arithmetic (binary: consume t0 and t1, push result) ---

HANDLER(op_iadd) {
    t0 = (uint64_t)((int64_t)t1 + (int64_t)t0);
    BINOP_SHIFT();
    NEXT_ALL();
}

HANDLER(op_isub) {
    t0 = (uint64_t)((int64_t)t1 - (int64_t)t0);
    BINOP_SHIFT();
    NEXT_ALL();
}

HANDLER(op_imul) {
    t0 = (uint64_t)((int64_t)t1 * (int64_t)t0);
    BINOP_SHIFT();
    NEXT_ALL();
}

HANDLER(op_idiv) {
    int64_t b = (int64_t)t0;
    int64_t a = (int64_t)t1;
    t0 = (uint64_t)(b != 0 ? a / b : 0);
    BINOP_SHIFT();
    NEXT_ALL();
}

HANDLER(op_udiv) {
    uint64_t b = t0;
    uint64_t a = t1;
    t0 = b != 0 ? a / b : 0;
    BINOP_SHIFT();
    NEXT_ALL();
}

HANDLER(op_irem) {
    int64_t b = (int64_t)t0;
    int64_t a = (int64_t)t1;
    t0 = (uint64_t)(b != 0 ? a % b : 0);
    BINOP_SHIFT();
    NEXT_ALL();
}

HANDLER(op_ipow) {
    uint32_t exp = (uint32_t)t0;
    int64_t base = (int64_t)t1;
    t0 = (uint64_t)ipow(base, exp);
    BINOP_SHIFT();
    NEXT_ALL();
}

// Unary integer ops
HANDLER(op_ineg) {
    t0 = (uint64_t)(-(int64_t)t0);
    NEXT_ALL();
}

HANDLER(op_iadd_imm) {
    t0 = (uint64_t)((int64_t)t0 + (int64_t)pc->imm[0]);
    NEXT_ALL();
}

// --- Float32 arithmetic (binary) ---

HANDLER(op_fadd) {
    t0 = from_f32(as_f32(t1) + as_f32(t0));
    BINOP_SHIFT();
    NEXT_ALL();
}

HANDLER(op_fsub) {
    t0 = from_f32(as_f32(t1) - as_f32(t0));
    BINOP_SHIFT();
    NEXT_ALL();
}

HANDLER(op_fmul) {
    t0 = from_f32(as_f32(t1) * as_f32(t0));
    BINOP_SHIFT();
    NEXT_ALL();
}

HANDLER(op_fdiv) {
    t0 = from_f32(as_f32(t1) / as_f32(t0));
    BINOP_SHIFT();
    NEXT_ALL();
}

HANDLER(op_fpow) {
    t0 = from_f32(powf(as_f32(t1), as_f32(t0)));
    BINOP_SHIFT();
    NEXT_ALL();
}

// Unary f32
HANDLER(op_fneg) {
    t0 = from_f32(-as_f32(t0));
    NEXT_ALL();
}

// --- Float64 arithmetic (binary) ---

HANDLER(op_dadd) {
    t0 = from_f64(as_f64(t1) + as_f64(t0));
    BINOP_SHIFT();
    NEXT_ALL();
}

HANDLER(op_dsub) {
    t0 = from_f64(as_f64(t1) - as_f64(t0));
    BINOP_SHIFT();
    NEXT_ALL();
}

HANDLER(op_dmul) {
    t0 = from_f64(as_f64(t1) * as_f64(t0));
    BINOP_SHIFT();
    NEXT_ALL();
}

HANDLER(op_ddiv) {
    t0 = from_f64(as_f64(t1) / as_f64(t0));
    BINOP_SHIFT();
    NEXT_ALL();
}

HANDLER(op_dpow) {
    t0 = from_f64(pow(as_f64(t1), as_f64(t0)));
    BINOP_SHIFT();
    NEXT_ALL();
}

// Unary f64
HANDLER(op_dneg) {
    t0 = from_f64(-as_f64(t0));
    NEXT_ALL();
}

// --- Comparisons (binary: consume t0 and t1, push result) ---

#define CMP_OP(name, type, cast, op) \
HANDLER(name) { \
    type b = cast(t0); type a = cast(t1); \
    t0 = (a op b) ? 1 : 0; \
    BINOP_SHIFT(); \
    NEXT_ALL(); \
}

CMP_OP(op_ieq, int64_t, (int64_t), ==)
CMP_OP(op_ine, int64_t, (int64_t), !=)
CMP_OP(op_ilt, int64_t, (int64_t), <)
CMP_OP(op_ile, int64_t, (int64_t), <=)
CMP_OP(op_igt, int64_t, (int64_t), >)
CMP_OP(op_ige, int64_t, (int64_t), >=)
CMP_OP(op_ult, uint64_t, (uint64_t), <)
CMP_OP(op_ugt, uint64_t, (uint64_t), >)
CMP_OP(op_feq, float, as_f32, ==)
CMP_OP(op_fne, float, as_f32, !=)
CMP_OP(op_flt, float, as_f32, <)
CMP_OP(op_fle, float, as_f32, <=)
CMP_OP(op_fgt, float, as_f32, >)
CMP_OP(op_fge, float, as_f32, >=)
CMP_OP(op_deq, double, as_f64, ==)
CMP_OP(op_dlt, double, as_f64, <)
CMP_OP(op_dle, double, as_f64, <=)

// --- Bitwise (binary) ---

HANDLER(op_and) {
    t0 = t1 & t0; BINOP_SHIFT(); NEXT(ctx, pc, sp, locals, lm, l0, l1, l2, t0, t1, t2, t3);
}
HANDLER(op_or) {
    t0 = t1 | t0; BINOP_SHIFT(); NEXT(ctx, pc, sp, locals, lm, l0, l1, l2, t0, t1, t2, t3);
}
HANDLER(op_xor) {
    t0 = t1 ^ t0; BINOP_SHIFT(); NEXT(ctx, pc, sp, locals, lm, l0, l1, l2, t0, t1, t2, t3);
}
HANDLER(op_not) {
    t0 = ~t0; NEXT(ctx, pc, sp, locals, lm, l0, l1, l2, t0, t1, t2, t3);
}
HANDLER(op_shl) {
    t0 = t1 << (t0 & 63); BINOP_SHIFT(); NEXT(ctx, pc, sp, locals, lm, l0, l1, l2, t0, t1, t2, t3);
}
HANDLER(op_shr) {
    t0 = (uint64_t)((int64_t)t1 >> (t0 & 63)); BINOP_SHIFT(); NEXT(ctx, pc, sp, locals, lm, l0, l1, l2, t0, t1, t2, t3);
}
HANDLER(op_ushr) {
    t0 = t1 >> (t0 & 63); BINOP_SHIFT(); NEXT(ctx, pc, sp, locals, lm, l0, l1, l2, t0, t1, t2, t3);
}

// --- Type conversions (unary) ---

HANDLER(op_i32_to_f32) {
    t0 = from_f32((float)(int32_t)t0); NEXT(ctx, pc, sp, locals, lm, l0, l1, l2, t0, t1, t2, t3);
}
HANDLER(op_f32_to_i32) {
    t0 = (uint64_t)(int64_t)(int32_t)as_f32(t0); NEXT(ctx, pc, sp, locals, lm, l0, l1, l2, t0, t1, t2, t3);
}
HANDLER(op_i32_to_f64) {
    t0 = from_f64((double)(int32_t)t0); NEXT(ctx, pc, sp, locals, lm, l0, l1, l2, t0, t1, t2, t3);
}
HANDLER(op_f64_to_i32) {
    t0 = (uint64_t)(int64_t)(int32_t)as_f64(t0); NEXT(ctx, pc, sp, locals, lm, l0, l1, l2, t0, t1, t2, t3);
}
HANDLER(op_f32_to_f64) {
    t0 = from_f64((double)as_f32(t0)); NEXT(ctx, pc, sp, locals, lm, l0, l1, l2, t0, t1, t2, t3);
}
HANDLER(op_f64_to_f32) {
    t0 = from_f32((float)as_f64(t0)); NEXT(ctx, pc, sp, locals, lm, l0, l1, l2, t0, t1, t2, t3);
}
HANDLER(op_i32_to_i8) {
    t0 = (uint64_t)(int64_t)(int8_t)(int32_t)t0; NEXT(ctx, pc, sp, locals, lm, l0, l1, l2, t0, t1, t2, t3);
}
HANDLER(op_i8_to_i32) {
    t0 = (uint64_t)(int64_t)(int32_t)(int8_t)t0; NEXT(ctx, pc, sp, locals, lm, l0, l1, l2, t0, t1, t2, t3);
}
HANDLER(op_i64_to_u32) {
    t0 = t0 & 0xFFFFFFFF; NEXT(ctx, pc, sp, locals, lm, l0, l1, l2, t0, t1, t2, t3);
}

// --- Memory loads (unary: transform t0) ---

HANDLER(op_load8) {
    t0 = (uint64_t)(int64_t)(int8_t)*(uint8_t*)t0;
    NEXT_ALL();
}

HANDLER(op_load32) {
    t0 = (uint64_t)(int64_t)*(int32_t*)t0;
    NEXT_ALL();
}

HANDLER(op_load64) {
    t0 = *(uint64_t*)t0;
    NEXT_ALL();
}

HANDLER(op_load32_off) {
    uint8_t* base = (uint8_t*)t0;
    int32_t off = (int32_t)pc->imm[0];
    t0 = (uint64_t)(int64_t)*(int32_t*)(base + off);
    NEXT_ALL();
}

HANDLER(op_load64_off) {
    uint8_t* base = (uint8_t*)t0;
    int32_t off = (int32_t)pc->imm[0];
    t0 = *(uint64_t*)(base + off);
    NEXT_ALL();
}

// --- Memory stores (pop 2: val=t0, addr=t1) ---

HANDLER(op_store8) {
    *(uint8_t*)t1 = (uint8_t)t0;
    DROP2();
    NEXT_ALL();
}

HANDLER(op_store32) {
    *(int32_t*)t1 = (int32_t)t0;
    DROP2();
    NEXT_ALL();
}

HANDLER(op_store64) {
    *(uint64_t*)t1 = t0;
    DROP2();
    NEXT_ALL();
}

HANDLER(op_store8_off) {
    *((uint8_t*)t1 + (int32_t)pc->imm[0]) = (uint8_t)t0;
    DROP2();
    NEXT_ALL();
}

HANDLER(op_store32_off) {
    *(int32_t*)((uint8_t*)t1 + (int32_t)pc->imm[0]) = (int32_t)t0;
    DROP2();
    NEXT_ALL();
}

HANDLER(op_store64_off) {
    *(uint64_t*)((uint8_t*)t1 + (int32_t)pc->imm[0]) = t0;
    DROP2();
    NEXT_ALL();
}

// --- Bulk memory ---

HANDLER(op_memcopy) {
    // pop src=t0, pop dst=t1
    memmove((uint8_t*)t1, (uint8_t*)t0, (size_t)pc->imm[0]);
    DROP2();
    NEXT_ALL();
}

HANDLER(op_memzero) {
    // pop dst=t0
    memset((uint8_t*)t0, 0, (size_t)pc->imm[0]);
    DROP1();
    NEXT_ALL();
}

HANDLER(op_memeq) {
    // binary: pop b=t0, pop a=t1, push result
    uint8_t* b = (uint8_t*)t0;
    uint8_t* a = (uint8_t*)t1;
    t0 = memcmp(a, b, (size_t)pc->imm[0]) == 0 ? 1 : 0;
    BINOP_SHIFT();
    NEXT_ALL();
}

HANDLER(op_memne) {
    uint8_t* b = (uint8_t*)t0;
    uint8_t* a = (uint8_t*)t1;
    t0 = memcmp(a, b, (size_t)pc->imm[0]) != 0 ? 1 : 0;
    BINOP_SHIFT();
    NEXT_ALL();
}

// --- Slice operations ---

HANDLER(op_slice_eq) {
    // binary: pop b=t0, pop a=t1, push result
    uint8_t* fat_b = (uint8_t*)t0;
    uint8_t* fat_a = (uint8_t*)t1;
    uint32_t elem_size = (uint32_t)pc->imm[0];
    uint32_t len_a = *(uint32_t*)(fat_a + 8);
    uint32_t len_b = *(uint32_t*)(fat_b + 8);
    if (len_a != len_b) {
        t0 = 0;
        BINOP_SHIFT();
        NEXT_ALL();
    }
    uint8_t* data_a = *(uint8_t**)fat_a;
    uint8_t* data_b = *(uint8_t**)fat_b;
    t0 = memcmp(data_a, data_b, (size_t)len_a * elem_size) == 0 ? 1 : 0;
    BINOP_SHIFT();
    NEXT_ALL();
}

HANDLER(op_slice_ne) {
    uint8_t* fat_b = (uint8_t*)t0;
    uint8_t* fat_a = (uint8_t*)t1;
    uint32_t elem_size = (uint32_t)pc->imm[0];
    uint32_t len_a = *(uint32_t*)(fat_a + 8);
    uint32_t len_b = *(uint32_t*)(fat_b + 8);
    if (len_a != len_b) {
        t0 = 1;
        BINOP_SHIFT();
        NEXT_ALL();
    }
    uint8_t* data_a = *(uint8_t**)fat_a;
    uint8_t* data_b = *(uint8_t**)fat_b;
    t0 = memcmp(data_a, data_b, (size_t)len_a * elem_size) != 0 ? 1 : 0;
    BINOP_SHIFT();
    NEXT_ALL();
}

HANDLER(op_slice_load32) {
    // binary: pop idx=t0, pop fat=t1, push result
    int64_t idx = (int64_t)t0;
    uint8_t* fat = (uint8_t*)t1;
    uint8_t* data = *(uint8_t**)fat;
    t0 = (uint64_t)(int64_t)*(int32_t*)(data + idx * 4);
    BINOP_SHIFT();
    NEXT_ALL();
}

HANDLER(op_slice_store32) {
    // pop val=t0, pop idx=t1, pop fat=t2
    int32_t val = (int32_t)t0;
    int64_t idx = (int64_t)t1;
    uint8_t* fat = (uint8_t*)t2;
    uint8_t* data = *(uint8_t**)fat;
    *(int32_t*)(data + idx * 4) = val;
    DROP3();
    NEXT_ALL();
}

// --- Control flow ---

HANDLER(op_jump) {
    int64_t off = (int64_t)pc->imm[0];
    pc = pc + 1 + off;
    DISPATCH_ALL();
}

HANDLER(op_jump_if_zero) {
    uint64_t cond = t0;
    t0 = t1; t1 = t2; t2 = t3; t3 = *--sp;
    if (cond == 0) {
        int64_t off = (int64_t)pc->imm[0];
        pc = pc + 1 + off;
        DISPATCH_ALL();
    }
    NEXT_ALL();
}

HANDLER(op_jump_if_not_zero) {
    uint64_t cond = t0;
    t0 = t1; t1 = t2; t2 = t3; t3 = *--sp;
    if (cond != 0) {
        int64_t off = (int64_t)pc->imm[0];
        pc = pc + 1 + off;
        DISPATCH_ALL();
    }
    NEXT_ALL();
}

// --- Function calls ---

HANDLER(op_call) {
    // Spill entire TOS window to memory.
    SPILL_ALL();
    // Note: do NOT spill l0/l1/l2 here. locals[0/1/2] are already up-to-date
    // because LocalSetL* writes both register and memory, and fused ops write
    // to memory directly. Spilling would overwrite correct values with stale
    // register contents when fused ops have written to locals[0/1/2].

    uint32_t target = (uint32_t)pc->imm[0];
    uint32_t nargs = (uint32_t)pc->imm[1];

    // Save call frame.
    CallFrame* frame = &ctx->call_stack[ctx->call_depth++];
    frame->return_pc = pc + 1;
    frame->saved_locals = locals;
    frame->saved_lm = lm;
    frame->saved_sp = sp - nargs;
    frame->func_idx = (uint32_t)pc->imm[2];
    frame->saved_lm_size = ctx->local_memory_size;
    frame->saved_locals_size = ctx->locals_stack_size;

    // Pop args from memory.
    uint64_t args[256];
    for (uint32_t i = 0; i < nargs; i++) {
        args[nargs - 1 - i] = *--sp;
    }

    // Enter callee.
    uint64_t* new_locals;
    uint8_t* new_lm;
    enter_function(ctx, target, args, nargs, &new_locals, &new_lm);

    Instruction* entry = ctx->functions[target].code;
    // Callee starts with l0/l1/l2 loaded from its own locals[0/1/2].
    DISPATCH(ctx, entry, frame->saved_sp, new_locals, new_lm, new_locals[0], new_locals[1], new_locals[2], 0, 0, 0, 0);
}

HANDLER(op_call_closure) {
    // t0 is the fat_ptr address (last thing pushed before call_closure)
    uint8_t* fat_ptr = (uint8_t*)t0;
    uint32_t target = (uint32_t)*(int64_t*)fat_ptr;
    ctx->closure_ptr = *(uint64_t*)(fat_ptr + 8);
    uint32_t nargs = (uint32_t)pc->imm[0];

    // Spill remaining TOS (t1, t2, t3) -- t0 was consumed as fat_ptr
    *sp++ = t3; *sp++ = t2; *sp++ = t1;

    CallFrame* frame = &ctx->call_stack[ctx->call_depth++];
    frame->return_pc = pc + 1;
    frame->saved_locals = locals;
    frame->saved_lm = lm;
    frame->saved_sp = sp - nargs;
    frame->func_idx = (uint32_t)pc->imm[1];
    frame->saved_lm_size = ctx->local_memory_size;
    frame->saved_locals_size = ctx->locals_stack_size;

    uint64_t args[256];
    for (uint32_t i = 0; i < nargs; i++) {
        args[nargs - 1 - i] = *--sp;
    }

    uint64_t* new_locals;
    uint8_t* new_lm;
    enter_function(ctx, target, args, nargs, &new_locals, &new_lm);

    Instruction* entry = ctx->functions[target].code;
    DISPATCH(ctx, entry, frame->saved_sp, new_locals, new_lm, new_locals[0], new_locals[1], new_locals[2], 0, 0, 0, 0);
}

HANDLER(op_call_indirect) {
    uint32_t target = (uint32_t)(int64_t)t0; // t0 consumed as func_idx
    uint32_t nargs = (uint32_t)pc->imm[0];

    // Spill remaining TOS (t1, t2, t3) -- t0 was consumed
    *sp++ = t3; *sp++ = t2; *sp++ = t1;

    CallFrame* frame = &ctx->call_stack[ctx->call_depth++];
    frame->return_pc = pc + 1;
    frame->saved_locals = locals;
    frame->saved_lm = lm;
    frame->saved_sp = sp - nargs;
    frame->func_idx = (uint32_t)pc->imm[1];
    frame->saved_lm_size = ctx->local_memory_size;
    frame->saved_locals_size = ctx->locals_stack_size;

    uint64_t args[256];
    for (uint32_t i = 0; i < nargs; i++) {
        args[nargs - 1 - i] = *--sp;
    }

    uint64_t* new_locals;
    uint8_t* new_lm;
    enter_function(ctx, target, args, nargs, &new_locals, &new_lm);

    Instruction* entry = ctx->functions[target].code;
    DISPATCH(ctx, entry, frame->saved_sp, new_locals, new_lm, new_locals[0], new_locals[1], new_locals[2], 0, 0, 0, 0);
}

HANDLER(op_return) {
    uint64_t result = t0;

    if (ctx->call_depth == 0) {
        ctx->result = (int64_t)result;
        ctx->done = 1;
        return; // Exit interpreter.
    }

    CallFrame* frame = &ctx->call_stack[--ctx->call_depth];
    ctx->local_memory_size = frame->saved_lm_size;
    ctx->locals_stack_size = frame->saved_locals_size;
    sp = frame->saved_sp;
    locals = frame->saved_locals;
    // Fill hot locals from restored caller's locals.
    l0 = locals[0]; l1 = locals[1]; l2 = locals[2];
    t0 = result;
    FILL_BELOW();
    DISPATCH(ctx, frame->return_pc, sp, locals, frame->saved_lm, l0, l1, l2, t0, t1, t2, t3);
}

HANDLER(op_return_void) {
    if (ctx->call_depth == 0) {
        ctx->result = 0;
        ctx->done = 1;
        return;
    }

    CallFrame* frame = &ctx->call_stack[--ctx->call_depth];
    ctx->local_memory_size = frame->saved_lm_size;
    ctx->locals_stack_size = frame->saved_locals_size;
    sp = frame->saved_sp;
    locals = frame->saved_locals;
    // Fill hot locals from restored caller's locals.
    l0 = locals[0]; l1 = locals[1]; l2 = locals[2];
    FILL_ALL();
    DISPATCH(ctx, frame->return_pc, sp, locals, frame->saved_lm, l0, l1, l2, t0, t1, t2, t3);
}

// --- Stack manipulation ---

HANDLER(op_drop) {
    DROP1();
    NEXT_ALL();
}

// --- Math builtins (f32) ---

#define F32_UNARY(name, func) \
HANDLER(name) { \
    t0 = from_f32(func(as_f32(t0))); \
    NEXT_ALL(); \
}

F32_UNARY(op_sin_f32,   sinf)
F32_UNARY(op_cos_f32,   cosf)
F32_UNARY(op_tan_f32,   tanf)
F32_UNARY(op_asin_f32,  asinf)
F32_UNARY(op_acos_f32,  acosf)
F32_UNARY(op_atan_f32,  atanf)
F32_UNARY(op_sinh_f32,  sinhf)
F32_UNARY(op_cosh_f32,  coshf)
F32_UNARY(op_tanh_f32,  tanhf)
F32_UNARY(op_asinh_f32, asinhf)
F32_UNARY(op_acosh_f32, acoshf)
F32_UNARY(op_atanh_f32, atanhf)
F32_UNARY(op_ln_f32,    logf)
F32_UNARY(op_exp_f32,   expf)
F32_UNARY(op_exp2_f32,  exp2f)
F32_UNARY(op_log10_f32, log10f)
F32_UNARY(op_log2_f32,  log2f)
F32_UNARY(op_sqrt_f32,  sqrtf)
F32_UNARY(op_abs_f32,   fabsf)
F32_UNARY(op_floor_f32, floorf)
F32_UNARY(op_ceil_f32,  ceilf)

// --- Math builtins (f64) ---

#define F64_UNARY(name, func) \
HANDLER(name) { \
    t0 = from_f64(func(as_f64(t0))); \
    NEXT_ALL(); \
}

F64_UNARY(op_sin_f64,   sin)
F64_UNARY(op_cos_f64,   cos)
F64_UNARY(op_tan_f64,   tan)
F64_UNARY(op_asin_f64,  asin)
F64_UNARY(op_acos_f64,  acos)
F64_UNARY(op_atan_f64,  atan)
F64_UNARY(op_sinh_f64,  sinh)
F64_UNARY(op_cosh_f64,  cosh)
F64_UNARY(op_tanh_f64,  tanh)
F64_UNARY(op_asinh_f64, asinh)
F64_UNARY(op_acosh_f64, acosh)
F64_UNARY(op_atanh_f64, atanh)
F64_UNARY(op_ln_f64,    log)
F64_UNARY(op_exp_f64,   exp)
F64_UNARY(op_exp2_f64,  exp2)
F64_UNARY(op_log10_f64, log10)
F64_UNARY(op_log2_f64,  log2)
F64_UNARY(op_sqrt_f64,  sqrt)
F64_UNARY(op_abs_f64,   fabs)
F64_UNARY(op_floor_f64, floor)
F64_UNARY(op_ceil_f64,  ceil)

// Unary predicates

HANDLER(op_isnan_f32) {
    t0 = isnan(as_f32(t0)) ? 1 : 0;
    NEXT_ALL();
}
HANDLER(op_isnan_f64) {
    t0 = isnan(as_f64(t0)) ? 1 : 0;
    NEXT_ALL();
}
HANDLER(op_isinf_f32) {
    t0 = isinf(as_f32(t0)) ? 1 : 0;
    NEXT_ALL();
}
HANDLER(op_isinf_f64) {
    t0 = isinf(as_f64(t0)) ? 1 : 0;
    NEXT_ALL();
}

// Binary math (atan2)

HANDLER(op_atan2_f32) {
    t0 = from_f32(atan2f(as_f32(t1), as_f32(t0)));
    BINOP_SHIFT();
    NEXT_ALL();
}

HANDLER(op_atan2_f64) {
    t0 = from_f64(atan2(as_f64(t1), as_f64(t0)));
    BINOP_SHIFT();
    NEXT_ALL();
}

// --- Debug/IO (pop 1) ---

HANDLER(op_print_i32) {
    int32_t val = (int32_t)t0;
    DROP1();
    printf("%d\n", val);
    NEXT_ALL();
}

HANDLER(op_print_f32) {
    float val = as_f32(t0);
    DROP1();
    if (val == floorf(val) && fabsf(val) < 1e15f) {
        printf("%.1f\n", val);
    } else {
        printf("%g\n", val);
    }
    NEXT_ALL();
}

HANDLER(op_putc) {
    char c = (char)(int32_t)t0;
    DROP1();
    putchar(c);
    NEXT_ALL();
}

HANDLER(op_assert) {
    uint64_t val = t0;
    DROP1();
    printf("assert(%s)\n", val != 0 ? "true" : "false");
    fflush(stdout);
    if (val == 0) {
        fprintf(stderr, "Assertion failed\n");
        fflush(stderr);
        exit(1);
    }
    NEXT_ALL();
}

HANDLER(op_get_closure_ptr) {
    PUSH(ctx->closure_ptr);
    NEXT_ALL();
}

// ============================================================================
// Fused superinstructions
// ============================================================================

// locals[a] * locals[b] (f32) -- push
HANDLER(op_fused_get_get_fmul) {
    PUSH(from_f32(as_f32(locals[pc->imm[0]]) * as_f32(locals[pc->imm[1]])));
    NEXT_ALL();
}

// locals[a] + locals[b] (f32) -- push
HANDLER(op_fused_get_get_fadd) {
    PUSH(from_f32(as_f32(locals[pc->imm[0]]) + as_f32(locals[pc->imm[1]])));
    NEXT_ALL();
}

// locals[a] - locals[b] (f32) -- push
HANDLER(op_fused_get_get_fsub) {
    PUSH(from_f32(as_f32(locals[pc->imm[0]]) - as_f32(locals[pc->imm[1]])));
    NEXT_ALL();
}

// locals[a] + locals[b] (i64) -- push
HANDLER(op_fused_get_get_iadd) {
    PUSH((uint64_t)((int64_t)locals[pc->imm[0]] + (int64_t)locals[pc->imm[1]]));
    NEXT_ALL();
}

// locals[a] < locals[b] (i64 signed) -- push
HANDLER(op_fused_get_get_ilt) {
    PUSH(((int64_t)locals[pc->imm[0]] < (int64_t)locals[pc->imm[1]]) ? 1 : 0);
    NEXT_ALL();
}

// TOS * locals[a] (f32) -- unary (replaces TOS)
HANDLER(op_fused_get_fmul) {
    t0 = from_f32(as_f32(t0) * as_f32(locals[pc->imm[0]]));
    NEXT_ALL();
}

// TOS + locals[a] (f32) -- unary (replaces TOS)
HANDLER(op_fused_get_fadd) {
    t0 = from_f32(as_f32(t0) + as_f32(locals[pc->imm[0]]));
    NEXT_ALL();
}

// TOS - locals[a] (f32) -- unary (replaces TOS)
HANDLER(op_fused_get_fsub) {
    t0 = from_f32(as_f32(t0) - as_f32(locals[pc->imm[0]]));
    NEXT_ALL();
}

// Fused multiply-accumulate: t0=b, t1=a, t2=c, push c + a*b (f32)
// Consumes 3, pushes 1 => net drop 2
HANDLER(op_fused_fmul_fadd) {
    t0 = from_f32(as_f32(t2) + as_f32(t1) * as_f32(t0));
    // consumed t0, t1, t2
    t1 = t3; t2 = *--sp; t3 = *--sp;
    NEXT_ALL();
}

// Fused multiply-subtract: t0=b, t1=a, t2=c, push c - a*b (f32)
HANDLER(op_fused_fmul_fsub) {
    t0 = from_f32(as_f32(t2) - as_f32(t1) * as_f32(t0));
    t1 = t3; t2 = *--sp; t3 = *--sp;
    NEXT_ALL();
}

// Load i32 from lm + slot*8 + offset -- push
HANDLER(op_fused_addr_load32off) {
    PUSH((uint64_t)(int64_t)*(int32_t*)(lm + pc->imm[0] * 8 + (int32_t)pc->imm[1]));
    NEXT_ALL();
}

// locals[dst] = locals[src] + imm -- no stack change
HANDLER(op_fused_get_addimm_set) {
    locals[pc->imm[2]] = (uint64_t)((int64_t)locals[pc->imm[0]] + (int64_t)pc->imm[1]);
    NEXT_ALL();
}

// if !(locals[a] < locals[b]) jump -- no stack change
HANDLER(op_fused_get_get_ilt_jiz) {
    if ((int64_t)locals[pc->imm[0]] >= (int64_t)locals[pc->imm[1]]) {
        int64_t off = (int64_t)pc->imm[2];
        pc = pc + 1 + off;
        DISPATCH_ALL();
    }
    NEXT_ALL();
}

// locals[n] = i64 constant -- no stack change
HANDLER(op_fused_const_set) {
    locals[pc->imm[1]] = pc->imm[0];
    NEXT_ALL();
}

// locals[n] = f32 constant (bits in imm[0]) -- no stack change
HANDLER(op_fused_f32const_set) {
    locals[pc->imm[1]] = pc->imm[0];
    NEXT_ALL();
}

// Push slice_data[locals[idx_local] * 4] from slice at lm + slot*8 -- push
HANDLER(op_fused_addr_get_sload32) {
    uint8_t* fat = lm + pc->imm[0] * 8;
    int64_t idx = (int64_t)locals[pc->imm[1]];
    uint8_t* data = *(uint8_t**)fat;
    PUSH((uint64_t)(int64_t)*(int32_t*)(data + idx * 4));
    NEXT_ALL();
}

HANDLER(op_halt) {
    ctx->result = (int64_t)t0;
    ctx->done = 1;
    free(locals);
    return;
}

HANDLER(op_nop) {
    NEXT_ALL();
}

// ============================================================================
// Entry point
// ============================================================================

// ============================================================================
// Shallow handler variants (depth < 4: no memory spill/fill)
//
// These are selected at compile time based on static stack depth analysis.
// The only difference from the deep variants is that TOS window shifts
// skip the memory access (no *sp++ or *--sp).
// ============================================================================

// --- Push ops (shallow: no t3 spill) ---

#define SHALLOW_PUSH_HANDLER(name, deep_name, val_expr) \
HANDLER(name) { \
    PUSH_S(val_expr); \
    NEXT_ALL(); \
}

SHALLOW_PUSH_HANDLER(op_i64_const_s, op_i64_const, pc->imm[0])
SHALLOW_PUSH_HANDLER(op_f32_const_s, op_f32_const, pc->imm[0])
SHALLOW_PUSH_HANDLER(op_f64_const_s, op_f64_const, pc->imm[0])
SHALLOW_PUSH_HANDLER(op_local_get_s, op_local_get, locals[pc->imm[0]])
SHALLOW_PUSH_HANDLER(op_local_addr_s, op_local_addr, (uint64_t)(lm + pc->imm[0] * 8))
SHALLOW_PUSH_HANDLER(op_global_addr_s, op_global_addr, (uint64_t)(ctx->globals + (int32_t)pc->imm[0]))
SHALLOW_PUSH_HANDLER(op_get_closure_ptr_s, op_get_closure_ptr, ctx->closure_ptr)

HANDLER(op_fused_get_get_fmul_s) {
    PUSH_S(from_f32(as_f32(locals[pc->imm[0]]) * as_f32(locals[pc->imm[1]])));
    NEXT_ALL();
}
HANDLER(op_fused_get_get_fadd_s) {
    PUSH_S(from_f32(as_f32(locals[pc->imm[0]]) + as_f32(locals[pc->imm[1]])));
    NEXT_ALL();
}
HANDLER(op_fused_get_get_fsub_s) {
    PUSH_S(from_f32(as_f32(locals[pc->imm[0]]) - as_f32(locals[pc->imm[1]])));
    NEXT_ALL();
}
HANDLER(op_fused_get_get_iadd_s) {
    PUSH_S((uint64_t)((int64_t)locals[pc->imm[0]] + (int64_t)locals[pc->imm[1]]));
    NEXT_ALL();
}
HANDLER(op_fused_get_get_ilt_s) {
    PUSH_S(((int64_t)locals[pc->imm[0]] < (int64_t)locals[pc->imm[1]]) ? 1 : 0);
    NEXT_ALL();
}
HANDLER(op_fused_addr_load32off_s) {
    PUSH_S((uint64_t)(int64_t)*(int32_t*)(lm + pc->imm[0] * 8 + (int32_t)pc->imm[1]));
    NEXT_ALL();
}
HANDLER(op_fused_addr_get_sload32_s) {
    uint8_t* fat = lm + pc->imm[0] * 8;
    int64_t idx = (int64_t)locals[pc->imm[1]];
    uint8_t* data = *(uint8_t**)fat;
    PUSH_S((uint64_t)(int64_t)*(int32_t*)(data + idx * 4));
    NEXT_ALL();
}

// --- Pop ops (shallow: no t3 fill) ---

HANDLER(op_local_set_s) {
    uint64_t val; POP_S(val);
    locals[pc->imm[0]] = val;
    NEXT_ALL();
}

HANDLER(op_drop_s) {
    DROP1_S();
    NEXT_ALL();
}

HANDLER(op_jump_if_zero_s) {
    uint64_t cond; POP_S(cond);
    if (cond == 0) {
        int64_t off = (int64_t)pc->imm[0];
        pc = pc + 1 + off;
        DISPATCH_ALL();
    }
    NEXT_ALL();
}

HANDLER(op_jump_if_not_zero_s) {
    uint64_t cond; POP_S(cond);
    if (cond != 0) {
        int64_t off = (int64_t)pc->imm[0];
        pc = pc + 1 + off;
        DISPATCH_ALL();
    }
    NEXT_ALL();
}

HANDLER(op_print_i32_s) {
    uint64_t val; POP_S(val);
    printf("%d\n", (int32_t)val);
    NEXT_ALL();
}
HANDLER(op_print_f32_s) {
    uint64_t val; POP_S(val);
    float f = as_f32(val);
    if (f == floorf(f) && fabsf(f) < 1e15f) { printf("%.1f\n", f); }
    else { printf("%g\n", f); }
    NEXT_ALL();
}
HANDLER(op_putc_s) {
    uint64_t val; POP_S(val);
    putchar((char)(int32_t)val);
    NEXT_ALL();
}
HANDLER(op_assert_s) {
    uint64_t val; POP_S(val);
    printf("assert(%s)\n", val != 0 ? "true" : "false");
    fflush(stdout);
    if (val == 0) { fprintf(stderr, "Assertion failed\n"); fflush(stderr); exit(1); }
    NEXT_ALL();
}
HANDLER(op_memzero_s) {
    uint8_t* dst = (uint8_t*)t0; DROP1_S();
    memset(dst, 0, (size_t)pc->imm[0]);
    NEXT_ALL();
}

// --- Binary ops (shallow: no t3 fill after shift) ---

#define SHALLOW_BINOP_I(name, op) \
HANDLER(name) { \
    t0 = (uint64_t)((int64_t)t1 op (int64_t)t0); \
    BINOP_SHIFT_S(); \
    NEXT_ALL(); \
}

#define SHALLOW_BINOP_F32(name, op) \
HANDLER(name) { \
    t0 = from_f32(as_f32(t1) op as_f32(t0)); \
    BINOP_SHIFT_S(); \
    NEXT_ALL(); \
}

#define SHALLOW_BINOP_F64(name, op) \
HANDLER(name) { \
    t0 = from_f64(as_f64(t1) op as_f64(t0)); \
    BINOP_SHIFT_S(); \
    NEXT_ALL(); \
}

SHALLOW_BINOP_I(op_iadd_s, +)
SHALLOW_BINOP_I(op_isub_s, -)
SHALLOW_BINOP_I(op_imul_s, *)
SHALLOW_BINOP_F32(op_fadd_s, +)
SHALLOW_BINOP_F32(op_fsub_s, -)
SHALLOW_BINOP_F32(op_fmul_s, *)
SHALLOW_BINOP_F32(op_fdiv_s, /)
SHALLOW_BINOP_F64(op_dadd_s, +)
SHALLOW_BINOP_F64(op_dsub_s, -)
SHALLOW_BINOP_F64(op_dmul_s, *)
SHALLOW_BINOP_F64(op_ddiv_s, /)

#define SHALLOW_CMP_OP(name, type, cast, op) \
HANDLER(name) { \
    type b = cast(t0); type a = cast(t1); \
    t0 = (a op b) ? 1 : 0; \
    BINOP_SHIFT_S(); \
    NEXT_ALL(); \
}

SHALLOW_CMP_OP(op_ieq_s, int64_t, (int64_t), ==)
SHALLOW_CMP_OP(op_ine_s, int64_t, (int64_t), !=)
SHALLOW_CMP_OP(op_ilt_s, int64_t, (int64_t), <)
SHALLOW_CMP_OP(op_ile_s, int64_t, (int64_t), <=)
SHALLOW_CMP_OP(op_igt_s, int64_t, (int64_t), >)
SHALLOW_CMP_OP(op_ige_s, int64_t, (int64_t), >=)
SHALLOW_CMP_OP(op_ult_s, uint64_t, (uint64_t), <)
SHALLOW_CMP_OP(op_ugt_s, uint64_t, (uint64_t), >)
SHALLOW_CMP_OP(op_feq_s, float, as_f32, ==)
SHALLOW_CMP_OP(op_fne_s, float, as_f32, !=)
SHALLOW_CMP_OP(op_flt_s, float, as_f32, <)
SHALLOW_CMP_OP(op_fle_s, float, as_f32, <=)
SHALLOW_CMP_OP(op_fgt_s, float, as_f32, >)
SHALLOW_CMP_OP(op_fge_s, float, as_f32, >=)
SHALLOW_CMP_OP(op_deq_s, double, as_f64, ==)
SHALLOW_CMP_OP(op_dlt_s, double, as_f64, <)
SHALLOW_CMP_OP(op_dle_s, double, as_f64, <=)

HANDLER(op_and_s) { t0 = t1 & t0; BINOP_SHIFT_S(); NEXT_ALL(); }
HANDLER(op_or_s) { t0 = t1 | t0; BINOP_SHIFT_S(); NEXT_ALL(); }
HANDLER(op_xor_s) { t0 = t1 ^ t0; BINOP_SHIFT_S(); NEXT_ALL(); }
HANDLER(op_shl_s) { t0 = t1 << (t0 & 63); BINOP_SHIFT_S(); NEXT_ALL(); }
HANDLER(op_shr_s) { t0 = (uint64_t)((int64_t)t1 >> (t0 & 63)); BINOP_SHIFT_S(); NEXT_ALL(); }
HANDLER(op_ushr_s) { t0 = t1 >> (t0 & 63); BINOP_SHIFT_S(); NEXT_ALL(); }

// --- Store ops (shallow: no fill after dropping 2) ---

HANDLER(op_store32_s) {
    *(int32_t*)t1 = (int32_t)t0;
    DROP2_S();
    NEXT_ALL();
}
HANDLER(op_store64_s) {
    *(uint64_t*)t1 = t0;
    DROP2_S();
    NEXT_ALL();
}
HANDLER(op_store8_s) {
    *(uint8_t*)t1 = (uint8_t)t0;
    DROP2_S();
    NEXT_ALL();
}
HANDLER(op_store32_off_s) {
    *(int32_t*)((uint8_t*)t1 + (int32_t)pc->imm[0]) = (int32_t)t0;
    DROP2_S();
    NEXT_ALL();
}
HANDLER(op_store64_off_s) {
    *(uint64_t*)((uint8_t*)t1 + (int32_t)pc->imm[0]) = t0;
    DROP2_S();
    NEXT_ALL();
}
HANDLER(op_store8_off_s) {
    *((uint8_t*)t1 + (int32_t)pc->imm[0]) = (uint8_t)t0;
    DROP2_S();
    NEXT_ALL();
}

// --- FusedFMulFAdd/FSub shallow (consumes t0,t1,t2 — no memory fill) ---

HANDLER(op_fused_fmul_fadd_s) {
    t0 = from_f32(as_f32(t2) + as_f32(t1) * as_f32(t0));
    t1 = t3;
    NEXT_ALL();
}
HANDLER(op_fused_fmul_fsub_s) {
    t0 = from_f32(as_f32(t2) - as_f32(t1) * as_f32(t0));
    t1 = t3;
    NEXT_ALL();
}

// --- Fused f32.const + f32.gt + jump_if_zero: if !(t0 > const) jump. Pop 1. ---
HANDLER(op_fused_f32const_fgt_jiz) {
    float val = as_f32(t0);
    float limit = as_f32(pc->imm[0]);
    DROP1();
    if (!(val > limit)) {
        int64_t off = (int64_t)pc->imm[1];
        pc = pc + 1 + off;
        DISPATCH_ALL();
    }
    NEXT_ALL();
}

HANDLER(op_fused_f32const_fgt_jiz_s) {
    float val = as_f32(t0);
    float limit = as_f32(pc->imm[0]);
    DROP1_S();
    if (!(val > limit)) {
        int64_t off = (int64_t)pc->imm[1];
        pc = pc + 1 + off;
        DISPATCH_ALL();
    }
    NEXT_ALL();
}

// --- locals[dst] = locals[a] + locals[b] (f32). No stack change. ---
HANDLER(op_fused_get_get_fadd_set) {
    locals[pc->imm[2]] = from_f32(as_f32(locals[pc->imm[0]]) + as_f32(locals[pc->imm[1]]));
    NEXT_ALL();
}

// --- Copy struct field: dst_field = src_field. No stack change. ---
HANDLER(op_fused_field_copy32) {
    int32_t src_off = (int32_t)pc->imm[1];
    int32_t dst_off = (int32_t)pc->imm[2];
    uint8_t* base = lm + pc->imm[0] * 8;
    *(int32_t*)(base + dst_off) = *(int32_t*)(base + src_off);
    NEXT_ALL();
}

// --- Slice store with fused address: *(data + locals[idx]*4) = t0. Pop 1. ---
HANDLER(op_fused_addr_get_sstore32) {
    uint8_t* fat = lm + pc->imm[0] * 8;
    int64_t idx = (int64_t)locals[pc->imm[1]];
    uint8_t* data = *(uint8_t**)fat;
    *(int32_t*)(data + idx * 4) = (int32_t)t0;
    DROP1();
    NEXT_ALL();
}

HANDLER(op_fused_addr_get_sstore32_s) {
    uint8_t* fat = lm + pc->imm[0] * 8;
    int64_t idx = (int64_t)locals[pc->imm[1]];
    uint8_t* data = *(uint8_t**)fat;
    *(int32_t*)(data + idx * 4) = (int32_t)t0;
    DROP1_S();
    NEXT_ALL();
}

// --- Variable move: locals[b] = locals[a]. No stack change. ---
HANDLER(op_fused_get_set) {
    locals[pc->imm[1]] = locals[pc->imm[0]];
    NEXT_ALL();
}

// --- FMA term: accum += locals[a] * load(slot,off). Pure register on t0. ---
HANDLER(op_fused_get_addr_fmul_fadd) {
    float coeff = as_f32(locals[pc->imm[0]]);
    float state = as_f32((uint64_t)(int64_t)*(int32_t*)(lm + pc->imm[1] * 8 + (int32_t)pc->imm[2]));
    t0 = from_f32(as_f32(t0) + coeff * state);
    NEXT_ALL();
}

// --- FMA term: accum -= locals[a] * load(slot,off). ---
HANDLER(op_fused_get_addr_fmul_fsub) {
    float coeff = as_f32(locals[pc->imm[0]]);
    float state = as_f32((uint64_t)(int64_t)*(int32_t*)(lm + pc->imm[1] * 8 + (int32_t)pc->imm[2]));
    t0 = from_f32(as_f32(t0) - coeff * state);
    NEXT_ALL();
}

// --- Load struct field into local: locals[dst] = load(slot,off). No stack change. ---
HANDLER(op_fused_addr_load32off_set) {
    locals[pc->imm[2]] = (uint64_t)(int64_t)*(int32_t*)(lm + pc->imm[0] * 8 + (int32_t)pc->imm[1]);
    NEXT_ALL();
}

// --- Store local into struct field: *(i32*)(lm + slot*8 + off) = locals[src]. No stack change. ---
HANDLER(op_fused_addr_imm_get_store32) {
    *(int32_t*)(lm + pc->imm[0] * 8 + (int32_t)pc->imm[1]) = (int32_t)locals[pc->imm[2]];
    NEXT_ALL();
}

// --- MemCopy shallow (pop 2, no fill) ---
HANDLER(op_memcopy_s) {
    memmove((uint8_t*)t1, (uint8_t*)t0, (size_t)pc->imm[0]);
    DROP2_S();
    NEXT_ALL();
}

// ============================================================================
// Entry point
// ============================================================================

int64_t stack_interp_run(Ctx* ctx, uint32_t entry_func) {
    // Allocate operand stack.
    uint64_t* stack = (uint64_t*)calloc(64 * 1024, sizeof(uint64_t));
    ctx->stack_base = stack;
    ctx->done = 0;
    ctx->result = 0;

    // Pre-allocate local memory so it never needs realloc.
    // Raw pointers into this buffer are held on the operand stack
    // (e.g., output pointers for struct returns), so the buffer must not move.
    if (ctx->local_memory == NULL) {
        ctx->local_memory_cap = 4 * 1024 * 1024; // 4 MB
        ctx->local_memory = (uint8_t*)calloc(1, ctx->local_memory_cap);
    }

    // Pre-allocate scalar locals stack (bump allocator).
    // Each call allocates max(local_count, 3) u64 slots; returns pop them.
    // Sized for deep recursion with moderate local counts.
    if (ctx->locals_stack == NULL) {
        ctx->locals_stack_cap = 256 * 1024; // 2 MB worth of u64 slots
        ctx->locals_stack = (uint64_t*)calloc(ctx->locals_stack_cap, sizeof(uint64_t));
        ctx->locals_stack_size = 0;
    }

    // Enter entry function.
    uint64_t* locals;
    uint8_t* lm;
    enter_function(ctx, entry_func, NULL, 0, &locals, &lm);

    // Start dispatch with hot locals loaded and TOS registers zeroed.
    // Preload the handler for the second instruction as nh.
    Instruction* pc = ctx->functions[entry_func].code;
    Handler initial_nh = (Handler)(pc + 1)->handler;
    ((Handler)pc->handler)(ctx, pc, stack, locals, lm, locals[0], locals[1], locals[2], 0, 0, 0, 0, initial_nh);

    free(stack);
    return ctx->result;
}
