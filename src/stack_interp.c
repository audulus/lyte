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

    // Allocate scalar locals (calloc for zero-init).
    uint64_t* new_locals = (uint64_t*)calloc(meta->local_count, sizeof(uint64_t));

    // Copy arguments into locals 0..arg_count-1.
    for (uint32_t i = 0; i < arg_count && i < meta->param_count; i++) {
        new_locals[i] = args[i];
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

// Convenience macros for shallow handlers (pass all 9 args).
#define NEXT_ALL() NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3)
#define DISPATCH_ALL() DISPATCH(ctx, pc, sp, locals, lm, t0, t1, t2, t3)

// Spill all 4 TOS registers to memory (for calls).
#define SPILL_ALL() do { *sp++ = t3; *sp++ = t2; *sp++ = t1; *sp++ = t0; } while(0)

// Fill all 4 TOS registers from memory (for return void).
#define FILL_ALL() do { t0 = *--sp; t1 = *--sp; t2 = *--sp; t3 = *--sp; } while(0)

// Fill t1-t3 from memory (for return with value, t0 = result).
#define FILL_BELOW() do { t1 = *--sp; t2 = *--sp; t3 = *--sp; } while(0)

// Handler signature shorthand.
#define HANDLER_ARGS Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0, uint64_t t1, uint64_t t2, uint64_t t3

// ============================================================================
// Handlers
// ============================================================================

// --- Constants (push) ---

PRESERVE_NONE void op_i64_const(HANDLER_ARGS) {
    PUSH(pc->imm[0]);
    NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
}

PRESERVE_NONE void op_f32_const(HANDLER_ARGS) {
    PUSH(pc->imm[0]); // already encoded as f32 bits in u64
    NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
}

PRESERVE_NONE void op_f64_const(HANDLER_ARGS) {
    PUSH(pc->imm[0]);
    NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
}

// --- Local variables ---

PRESERVE_NONE void op_local_get(HANDLER_ARGS) {
    PUSH(locals[pc->imm[0]]);
    NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
}

PRESERVE_NONE void op_local_set(HANDLER_ARGS) {
    uint64_t val; POP(val);
    locals[pc->imm[0]] = val;
    NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
}

PRESERVE_NONE void op_local_tee(HANDLER_ARGS) {
    locals[pc->imm[0]] = t0; // peek, don't pop
    NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
}

PRESERVE_NONE void op_local_addr(HANDLER_ARGS) {
    PUSH((uint64_t)(lm + pc->imm[0] * 8));
    NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
}

// --- Global variables ---

PRESERVE_NONE void op_global_addr(HANDLER_ARGS) {
    PUSH((uint64_t)(ctx->globals + (int32_t)pc->imm[0]));
    NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
}

// --- Integer arithmetic (binary: consume t0 and t1, push result) ---

PRESERVE_NONE void op_iadd(HANDLER_ARGS) {
    t0 = (uint64_t)((int64_t)t1 + (int64_t)t0);
    BINOP_SHIFT();
    NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
}

PRESERVE_NONE void op_isub(HANDLER_ARGS) {
    t0 = (uint64_t)((int64_t)t1 - (int64_t)t0);
    BINOP_SHIFT();
    NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
}

PRESERVE_NONE void op_imul(HANDLER_ARGS) {
    t0 = (uint64_t)((int64_t)t1 * (int64_t)t0);
    BINOP_SHIFT();
    NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
}

PRESERVE_NONE void op_idiv(HANDLER_ARGS) {
    int64_t b = (int64_t)t0;
    int64_t a = (int64_t)t1;
    t0 = (uint64_t)(b != 0 ? a / b : 0);
    BINOP_SHIFT();
    NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
}

PRESERVE_NONE void op_udiv(HANDLER_ARGS) {
    uint64_t b = t0;
    uint64_t a = t1;
    t0 = b != 0 ? a / b : 0;
    BINOP_SHIFT();
    NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
}

PRESERVE_NONE void op_irem(HANDLER_ARGS) {
    int64_t b = (int64_t)t0;
    int64_t a = (int64_t)t1;
    t0 = (uint64_t)(b != 0 ? a % b : 0);
    BINOP_SHIFT();
    NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
}

PRESERVE_NONE void op_ipow(HANDLER_ARGS) {
    uint32_t exp = (uint32_t)t0;
    int64_t base = (int64_t)t1;
    t0 = (uint64_t)ipow(base, exp);
    BINOP_SHIFT();
    NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
}

// Unary integer ops
PRESERVE_NONE void op_ineg(HANDLER_ARGS) {
    t0 = (uint64_t)(-(int64_t)t0);
    NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
}

PRESERVE_NONE void op_iadd_imm(HANDLER_ARGS) {
    t0 = (uint64_t)((int64_t)t0 + (int64_t)pc->imm[0]);
    NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
}

// --- Float32 arithmetic (binary) ---

PRESERVE_NONE void op_fadd(HANDLER_ARGS) {
    t0 = from_f32(as_f32(t1) + as_f32(t0));
    BINOP_SHIFT();
    NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
}

PRESERVE_NONE void op_fsub(HANDLER_ARGS) {
    t0 = from_f32(as_f32(t1) - as_f32(t0));
    BINOP_SHIFT();
    NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
}

PRESERVE_NONE void op_fmul(HANDLER_ARGS) {
    t0 = from_f32(as_f32(t1) * as_f32(t0));
    BINOP_SHIFT();
    NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
}

PRESERVE_NONE void op_fdiv(HANDLER_ARGS) {
    t0 = from_f32(as_f32(t1) / as_f32(t0));
    BINOP_SHIFT();
    NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
}

PRESERVE_NONE void op_fpow(HANDLER_ARGS) {
    t0 = from_f32(powf(as_f32(t1), as_f32(t0)));
    BINOP_SHIFT();
    NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
}

// Unary f32
PRESERVE_NONE void op_fneg(HANDLER_ARGS) {
    t0 = from_f32(-as_f32(t0));
    NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
}

// --- Float64 arithmetic (binary) ---

PRESERVE_NONE void op_dadd(HANDLER_ARGS) {
    t0 = from_f64(as_f64(t1) + as_f64(t0));
    BINOP_SHIFT();
    NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
}

PRESERVE_NONE void op_dsub(HANDLER_ARGS) {
    t0 = from_f64(as_f64(t1) - as_f64(t0));
    BINOP_SHIFT();
    NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
}

PRESERVE_NONE void op_dmul(HANDLER_ARGS) {
    t0 = from_f64(as_f64(t1) * as_f64(t0));
    BINOP_SHIFT();
    NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
}

PRESERVE_NONE void op_ddiv(HANDLER_ARGS) {
    t0 = from_f64(as_f64(t1) / as_f64(t0));
    BINOP_SHIFT();
    NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
}

PRESERVE_NONE void op_dpow(HANDLER_ARGS) {
    t0 = from_f64(pow(as_f64(t1), as_f64(t0)));
    BINOP_SHIFT();
    NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
}

// Unary f64
PRESERVE_NONE void op_dneg(HANDLER_ARGS) {
    t0 = from_f64(-as_f64(t0));
    NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
}

// --- Comparisons (binary: consume t0 and t1, push result) ---

#define CMP_OP(name, type, cast, op) \
PRESERVE_NONE void name(HANDLER_ARGS) { \
    type b = cast(t0); type a = cast(t1); \
    t0 = (a op b) ? 1 : 0; \
    BINOP_SHIFT(); \
    NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3); \
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

PRESERVE_NONE void op_and(HANDLER_ARGS) {
    t0 = t1 & t0; BINOP_SHIFT(); NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
}
PRESERVE_NONE void op_or(HANDLER_ARGS) {
    t0 = t1 | t0; BINOP_SHIFT(); NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
}
PRESERVE_NONE void op_xor(HANDLER_ARGS) {
    t0 = t1 ^ t0; BINOP_SHIFT(); NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
}
PRESERVE_NONE void op_not(HANDLER_ARGS) {
    t0 = ~t0; NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
}
PRESERVE_NONE void op_shl(HANDLER_ARGS) {
    t0 = t1 << (t0 & 63); BINOP_SHIFT(); NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
}
PRESERVE_NONE void op_shr(HANDLER_ARGS) {
    t0 = (uint64_t)((int64_t)t1 >> (t0 & 63)); BINOP_SHIFT(); NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
}
PRESERVE_NONE void op_ushr(HANDLER_ARGS) {
    t0 = t1 >> (t0 & 63); BINOP_SHIFT(); NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
}

// --- Type conversions (unary) ---

PRESERVE_NONE void op_i32_to_f32(HANDLER_ARGS) {
    t0 = from_f32((float)(int32_t)t0); NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
}
PRESERVE_NONE void op_f32_to_i32(HANDLER_ARGS) {
    t0 = (uint64_t)(int64_t)(int32_t)as_f32(t0); NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
}
PRESERVE_NONE void op_i32_to_f64(HANDLER_ARGS) {
    t0 = from_f64((double)(int32_t)t0); NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
}
PRESERVE_NONE void op_f64_to_i32(HANDLER_ARGS) {
    t0 = (uint64_t)(int64_t)(int32_t)as_f64(t0); NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
}
PRESERVE_NONE void op_f32_to_f64(HANDLER_ARGS) {
    t0 = from_f64((double)as_f32(t0)); NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
}
PRESERVE_NONE void op_f64_to_f32(HANDLER_ARGS) {
    t0 = from_f32((float)as_f64(t0)); NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
}
PRESERVE_NONE void op_i32_to_i8(HANDLER_ARGS) {
    t0 = (uint64_t)(int64_t)(int8_t)(int32_t)t0; NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
}
PRESERVE_NONE void op_i8_to_i32(HANDLER_ARGS) {
    t0 = (uint64_t)(int64_t)(int32_t)(int8_t)t0; NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
}
PRESERVE_NONE void op_i64_to_u32(HANDLER_ARGS) {
    t0 = t0 & 0xFFFFFFFF; NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
}

// --- Memory loads (unary: transform t0) ---

PRESERVE_NONE void op_load8(HANDLER_ARGS) {
    t0 = (uint64_t)(int64_t)(int8_t)*(uint8_t*)t0;
    NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
}

PRESERVE_NONE void op_load32(HANDLER_ARGS) {
    t0 = (uint64_t)(int64_t)*(int32_t*)t0;
    NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
}

PRESERVE_NONE void op_load64(HANDLER_ARGS) {
    t0 = *(uint64_t*)t0;
    NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
}

PRESERVE_NONE void op_load32_off(HANDLER_ARGS) {
    uint8_t* base = (uint8_t*)t0;
    int32_t off = (int32_t)pc->imm[0];
    t0 = (uint64_t)(int64_t)*(int32_t*)(base + off);
    NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
}

PRESERVE_NONE void op_load64_off(HANDLER_ARGS) {
    uint8_t* base = (uint8_t*)t0;
    int32_t off = (int32_t)pc->imm[0];
    t0 = *(uint64_t*)(base + off);
    NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
}

// --- Memory stores (pop 2: val=t0, addr=t1) ---

PRESERVE_NONE void op_store8(HANDLER_ARGS) {
    *(uint8_t*)t1 = (uint8_t)t0;
    DROP2();
    NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
}

PRESERVE_NONE void op_store32(HANDLER_ARGS) {
    *(int32_t*)t1 = (int32_t)t0;
    DROP2();
    NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
}

PRESERVE_NONE void op_store64(HANDLER_ARGS) {
    *(uint64_t*)t1 = t0;
    DROP2();
    NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
}

PRESERVE_NONE void op_store8_off(HANDLER_ARGS) {
    *((uint8_t*)t1 + (int32_t)pc->imm[0]) = (uint8_t)t0;
    DROP2();
    NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
}

PRESERVE_NONE void op_store32_off(HANDLER_ARGS) {
    *(int32_t*)((uint8_t*)t1 + (int32_t)pc->imm[0]) = (int32_t)t0;
    DROP2();
    NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
}

PRESERVE_NONE void op_store64_off(HANDLER_ARGS) {
    *(uint64_t*)((uint8_t*)t1 + (int32_t)pc->imm[0]) = t0;
    DROP2();
    NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
}

// --- Bulk memory ---

PRESERVE_NONE void op_memcopy(HANDLER_ARGS) {
    // pop src=t0, pop dst=t1
    memmove((uint8_t*)t1, (uint8_t*)t0, (size_t)pc->imm[0]);
    DROP2();
    NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
}

PRESERVE_NONE void op_memzero(HANDLER_ARGS) {
    // pop dst=t0
    memset((uint8_t*)t0, 0, (size_t)pc->imm[0]);
    DROP1();
    NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
}

PRESERVE_NONE void op_memeq(HANDLER_ARGS) {
    // binary: pop b=t0, pop a=t1, push result
    uint8_t* b = (uint8_t*)t0;
    uint8_t* a = (uint8_t*)t1;
    t0 = memcmp(a, b, (size_t)pc->imm[0]) == 0 ? 1 : 0;
    BINOP_SHIFT();
    NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
}

PRESERVE_NONE void op_memne(HANDLER_ARGS) {
    uint8_t* b = (uint8_t*)t0;
    uint8_t* a = (uint8_t*)t1;
    t0 = memcmp(a, b, (size_t)pc->imm[0]) != 0 ? 1 : 0;
    BINOP_SHIFT();
    NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
}

// --- Slice operations ---

PRESERVE_NONE void op_slice_eq(HANDLER_ARGS) {
    // binary: pop b=t0, pop a=t1, push result
    uint8_t* fat_b = (uint8_t*)t0;
    uint8_t* fat_a = (uint8_t*)t1;
    uint32_t elem_size = (uint32_t)pc->imm[0];
    uint32_t len_a = *(uint32_t*)(fat_a + 8);
    uint32_t len_b = *(uint32_t*)(fat_b + 8);
    if (len_a != len_b) {
        t0 = 0;
        BINOP_SHIFT();
        NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
    }
    uint8_t* data_a = *(uint8_t**)fat_a;
    uint8_t* data_b = *(uint8_t**)fat_b;
    t0 = memcmp(data_a, data_b, (size_t)len_a * elem_size) == 0 ? 1 : 0;
    BINOP_SHIFT();
    NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
}

PRESERVE_NONE void op_slice_ne(HANDLER_ARGS) {
    uint8_t* fat_b = (uint8_t*)t0;
    uint8_t* fat_a = (uint8_t*)t1;
    uint32_t elem_size = (uint32_t)pc->imm[0];
    uint32_t len_a = *(uint32_t*)(fat_a + 8);
    uint32_t len_b = *(uint32_t*)(fat_b + 8);
    if (len_a != len_b) {
        t0 = 1;
        BINOP_SHIFT();
        NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
    }
    uint8_t* data_a = *(uint8_t**)fat_a;
    uint8_t* data_b = *(uint8_t**)fat_b;
    t0 = memcmp(data_a, data_b, (size_t)len_a * elem_size) != 0 ? 1 : 0;
    BINOP_SHIFT();
    NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
}

PRESERVE_NONE void op_slice_load32(HANDLER_ARGS) {
    // binary: pop idx=t0, pop fat=t1, push result
    int64_t idx = (int64_t)t0;
    uint8_t* fat = (uint8_t*)t1;
    uint8_t* data = *(uint8_t**)fat;
    t0 = (uint64_t)(int64_t)*(int32_t*)(data + idx * 4);
    BINOP_SHIFT();
    NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
}

PRESERVE_NONE void op_slice_store32(HANDLER_ARGS) {
    // pop val=t0, pop idx=t1, pop fat=t2
    int32_t val = (int32_t)t0;
    int64_t idx = (int64_t)t1;
    uint8_t* fat = (uint8_t*)t2;
    uint8_t* data = *(uint8_t**)fat;
    *(int32_t*)(data + idx * 4) = val;
    DROP3();
    NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
}

// --- Control flow ---

PRESERVE_NONE void op_jump(HANDLER_ARGS) {
    int64_t off = (int64_t)pc->imm[0];
    pc = pc + 1 + off;
    DISPATCH(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
}

PRESERVE_NONE void op_jump_if_zero(HANDLER_ARGS) {
    uint64_t cond = t0;
    t0 = t1; t1 = t2; t2 = t3; t3 = *--sp;
    if (cond == 0) {
        int64_t off = (int64_t)pc->imm[0];
        pc = pc + 1 + off;
        DISPATCH(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
    }
    NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
}

PRESERVE_NONE void op_jump_if_not_zero(HANDLER_ARGS) {
    uint64_t cond = t0;
    t0 = t1; t1 = t2; t2 = t3; t3 = *--sp;
    if (cond != 0) {
        int64_t off = (int64_t)pc->imm[0];
        pc = pc + 1 + off;
        DISPATCH(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
    }
    NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
}

// --- Function calls ---

PRESERVE_NONE void op_call(HANDLER_ARGS) {
    // Spill entire TOS window to memory.
    SPILL_ALL();

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
    DISPATCH(ctx, entry, frame->saved_sp, new_locals, new_lm, 0, 0, 0, 0);
}

PRESERVE_NONE void op_call_closure(HANDLER_ARGS) {
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

    uint64_t args[256];
    for (uint32_t i = 0; i < nargs; i++) {
        args[nargs - 1 - i] = *--sp;
    }

    uint64_t* new_locals;
    uint8_t* new_lm;
    enter_function(ctx, target, args, nargs, &new_locals, &new_lm);

    Instruction* entry = ctx->functions[target].code;
    DISPATCH(ctx, entry, frame->saved_sp, new_locals, new_lm, 0, 0, 0, 0);
}

PRESERVE_NONE void op_call_indirect(HANDLER_ARGS) {
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

    uint64_t args[256];
    for (uint32_t i = 0; i < nargs; i++) {
        args[nargs - 1 - i] = *--sp;
    }

    uint64_t* new_locals;
    uint8_t* new_lm;
    enter_function(ctx, target, args, nargs, &new_locals, &new_lm);

    Instruction* entry = ctx->functions[target].code;
    DISPATCH(ctx, entry, frame->saved_sp, new_locals, new_lm, 0, 0, 0, 0);
}

PRESERVE_NONE void op_return(HANDLER_ARGS) {
    uint64_t result = t0;
    free(locals);

    if (ctx->call_depth == 0) {
        ctx->result = (int64_t)result;
        ctx->done = 1;
        return; // Exit interpreter.
    }

    CallFrame* frame = &ctx->call_stack[--ctx->call_depth];
    ctx->local_memory_size = frame->saved_lm_size;
    sp = frame->saved_sp;
    t0 = result;
    FILL_BELOW();
    DISPATCH(ctx, frame->return_pc, sp, frame->saved_locals, frame->saved_lm, t0, t1, t2, t3);
}

PRESERVE_NONE void op_return_void(HANDLER_ARGS) {
    free(locals);

    if (ctx->call_depth == 0) {
        ctx->result = 0;
        ctx->done = 1;
        return;
    }

    CallFrame* frame = &ctx->call_stack[--ctx->call_depth];
    ctx->local_memory_size = frame->saved_lm_size;
    sp = frame->saved_sp;
    FILL_ALL();
    DISPATCH(ctx, frame->return_pc, sp, frame->saved_locals, frame->saved_lm, t0, t1, t2, t3);
}

// --- Stack manipulation ---

PRESERVE_NONE void op_drop(HANDLER_ARGS) {
    DROP1();
    NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
}

// --- Math builtins (f32) ---

#define F32_UNARY(name, func) \
PRESERVE_NONE void name(HANDLER_ARGS) { \
    t0 = from_f32(func(as_f32(t0))); \
    NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3); \
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
PRESERVE_NONE void name(HANDLER_ARGS) { \
    t0 = from_f64(func(as_f64(t0))); \
    NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3); \
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

PRESERVE_NONE void op_isnan_f32(HANDLER_ARGS) {
    t0 = isnan(as_f32(t0)) ? 1 : 0;
    NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
}
PRESERVE_NONE void op_isnan_f64(HANDLER_ARGS) {
    t0 = isnan(as_f64(t0)) ? 1 : 0;
    NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
}
PRESERVE_NONE void op_isinf_f32(HANDLER_ARGS) {
    t0 = isinf(as_f32(t0)) ? 1 : 0;
    NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
}
PRESERVE_NONE void op_isinf_f64(HANDLER_ARGS) {
    t0 = isinf(as_f64(t0)) ? 1 : 0;
    NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
}

// Binary math (atan2)

PRESERVE_NONE void op_atan2_f32(HANDLER_ARGS) {
    t0 = from_f32(atan2f(as_f32(t1), as_f32(t0)));
    BINOP_SHIFT();
    NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
}

PRESERVE_NONE void op_atan2_f64(HANDLER_ARGS) {
    t0 = from_f64(atan2(as_f64(t1), as_f64(t0)));
    BINOP_SHIFT();
    NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
}

// --- Debug/IO (pop 1) ---

PRESERVE_NONE void op_print_i32(HANDLER_ARGS) {
    int32_t val = (int32_t)t0;
    DROP1();
    printf("%d\n", val);
    NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
}

PRESERVE_NONE void op_print_f32(HANDLER_ARGS) {
    float val = as_f32(t0);
    DROP1();
    if (val == floorf(val) && fabsf(val) < 1e15f) {
        printf("%.1f\n", val);
    } else {
        printf("%g\n", val);
    }
    NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
}

PRESERVE_NONE void op_putc(HANDLER_ARGS) {
    char c = (char)(int32_t)t0;
    DROP1();
    putchar(c);
    NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
}

PRESERVE_NONE void op_assert(HANDLER_ARGS) {
    uint64_t val = t0;
    DROP1();
    printf("assert(%s)\n", val != 0 ? "true" : "false");
    fflush(stdout);
    if (val == 0) {
        fprintf(stderr, "Assertion failed\n");
        fflush(stderr);
        exit(1);
    }
    NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
}

PRESERVE_NONE void op_get_closure_ptr(HANDLER_ARGS) {
    PUSH(ctx->closure_ptr);
    NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
}

// ============================================================================
// Fused superinstructions
// ============================================================================

// locals[a] * locals[b] (f32) -- push
PRESERVE_NONE void op_fused_get_get_fmul(HANDLER_ARGS) {
    PUSH(from_f32(as_f32(locals[pc->imm[0]]) * as_f32(locals[pc->imm[1]])));
    NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
}

// locals[a] + locals[b] (f32) -- push
PRESERVE_NONE void op_fused_get_get_fadd(HANDLER_ARGS) {
    PUSH(from_f32(as_f32(locals[pc->imm[0]]) + as_f32(locals[pc->imm[1]])));
    NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
}

// locals[a] - locals[b] (f32) -- push
PRESERVE_NONE void op_fused_get_get_fsub(HANDLER_ARGS) {
    PUSH(from_f32(as_f32(locals[pc->imm[0]]) - as_f32(locals[pc->imm[1]])));
    NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
}

// locals[a] + locals[b] (i64) -- push
PRESERVE_NONE void op_fused_get_get_iadd(HANDLER_ARGS) {
    PUSH((uint64_t)((int64_t)locals[pc->imm[0]] + (int64_t)locals[pc->imm[1]]));
    NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
}

// locals[a] < locals[b] (i64 signed) -- push
PRESERVE_NONE void op_fused_get_get_ilt(HANDLER_ARGS) {
    PUSH(((int64_t)locals[pc->imm[0]] < (int64_t)locals[pc->imm[1]]) ? 1 : 0);
    NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
}

// TOS * locals[a] (f32) -- unary (replaces TOS)
PRESERVE_NONE void op_fused_get_fmul(HANDLER_ARGS) {
    t0 = from_f32(as_f32(t0) * as_f32(locals[pc->imm[0]]));
    NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
}

// TOS + locals[a] (f32) -- unary (replaces TOS)
PRESERVE_NONE void op_fused_get_fadd(HANDLER_ARGS) {
    t0 = from_f32(as_f32(t0) + as_f32(locals[pc->imm[0]]));
    NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
}

// TOS - locals[a] (f32) -- unary (replaces TOS)
PRESERVE_NONE void op_fused_get_fsub(HANDLER_ARGS) {
    t0 = from_f32(as_f32(t0) - as_f32(locals[pc->imm[0]]));
    NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
}

// Fused multiply-accumulate: t0=b, t1=a, t2=c, push c + a*b (f32)
// Consumes 3, pushes 1 => net drop 2
PRESERVE_NONE void op_fused_fmul_fadd(HANDLER_ARGS) {
    t0 = from_f32(as_f32(t2) + as_f32(t1) * as_f32(t0));
    // consumed t0, t1, t2
    t1 = t3; t2 = *--sp; t3 = *--sp;
    NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
}

// Fused multiply-subtract: t0=b, t1=a, t2=c, push c - a*b (f32)
PRESERVE_NONE void op_fused_fmul_fsub(HANDLER_ARGS) {
    t0 = from_f32(as_f32(t2) - as_f32(t1) * as_f32(t0));
    t1 = t3; t2 = *--sp; t3 = *--sp;
    NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
}

// Load i32 from lm + slot*8 + offset -- push
PRESERVE_NONE void op_fused_addr_load32off(HANDLER_ARGS) {
    PUSH((uint64_t)(int64_t)*(int32_t*)(lm + pc->imm[0] * 8 + (int32_t)pc->imm[1]));
    NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
}

// locals[dst] = locals[src] + imm -- no stack change
PRESERVE_NONE void op_fused_get_addimm_set(HANDLER_ARGS) {
    locals[pc->imm[2]] = (uint64_t)((int64_t)locals[pc->imm[0]] + (int64_t)pc->imm[1]);
    NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
}

// if !(locals[a] < locals[b]) jump -- no stack change
PRESERVE_NONE void op_fused_get_get_ilt_jiz(HANDLER_ARGS) {
    if ((int64_t)locals[pc->imm[0]] >= (int64_t)locals[pc->imm[1]]) {
        int64_t off = (int64_t)pc->imm[2];
        pc = pc + 1 + off;
        DISPATCH(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
    }
    NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
}

// locals[n] = i64 constant -- no stack change
PRESERVE_NONE void op_fused_const_set(HANDLER_ARGS) {
    locals[pc->imm[1]] = pc->imm[0];
    NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
}

// locals[n] = f32 constant (bits in imm[0]) -- no stack change
PRESERVE_NONE void op_fused_f32const_set(HANDLER_ARGS) {
    locals[pc->imm[1]] = pc->imm[0];
    NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
}

// Push slice_data[locals[idx_local] * 4] from slice at lm + slot*8 -- push
PRESERVE_NONE void op_fused_addr_get_sload32(HANDLER_ARGS) {
    uint8_t* fat = lm + pc->imm[0] * 8;
    int64_t idx = (int64_t)locals[pc->imm[1]];
    uint8_t* data = *(uint8_t**)fat;
    PUSH((uint64_t)(int64_t)*(int32_t*)(data + idx * 4));
    NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
}

PRESERVE_NONE void op_halt(HANDLER_ARGS) {
    ctx->result = (int64_t)t0;
    ctx->done = 1;
    free(locals);
    return;
}

PRESERVE_NONE void op_nop(HANDLER_ARGS) {
    NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
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
PRESERVE_NONE void name(HANDLER_ARGS) { \
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

PRESERVE_NONE void op_fused_get_get_fmul_s(HANDLER_ARGS) {
    PUSH_S(from_f32(as_f32(locals[pc->imm[0]]) * as_f32(locals[pc->imm[1]])));
    NEXT_ALL();
}
PRESERVE_NONE void op_fused_get_get_fadd_s(HANDLER_ARGS) {
    PUSH_S(from_f32(as_f32(locals[pc->imm[0]]) + as_f32(locals[pc->imm[1]])));
    NEXT_ALL();
}
PRESERVE_NONE void op_fused_get_get_fsub_s(HANDLER_ARGS) {
    PUSH_S(from_f32(as_f32(locals[pc->imm[0]]) - as_f32(locals[pc->imm[1]])));
    NEXT_ALL();
}
PRESERVE_NONE void op_fused_get_get_iadd_s(HANDLER_ARGS) {
    PUSH_S((uint64_t)((int64_t)locals[pc->imm[0]] + (int64_t)locals[pc->imm[1]]));
    NEXT_ALL();
}
PRESERVE_NONE void op_fused_get_get_ilt_s(HANDLER_ARGS) {
    PUSH_S(((int64_t)locals[pc->imm[0]] < (int64_t)locals[pc->imm[1]]) ? 1 : 0);
    NEXT_ALL();
}
PRESERVE_NONE void op_fused_addr_load32off_s(HANDLER_ARGS) {
    PUSH_S((uint64_t)(int64_t)*(int32_t*)(lm + pc->imm[0] * 8 + (int32_t)pc->imm[1]));
    NEXT_ALL();
}
PRESERVE_NONE void op_fused_addr_get_sload32_s(HANDLER_ARGS) {
    uint8_t* fat = lm + pc->imm[0] * 8;
    int64_t idx = (int64_t)locals[pc->imm[1]];
    uint8_t* data = *(uint8_t**)fat;
    PUSH_S((uint64_t)(int64_t)*(int32_t*)(data + idx * 4));
    NEXT_ALL();
}

// --- Pop ops (shallow: no t3 fill) ---

PRESERVE_NONE void op_local_set_s(HANDLER_ARGS) {
    uint64_t val; POP_S(val);
    locals[pc->imm[0]] = val;
    NEXT_ALL();
}

PRESERVE_NONE void op_drop_s(HANDLER_ARGS) {
    DROP1_S();
    NEXT_ALL();
}

PRESERVE_NONE void op_jump_if_zero_s(HANDLER_ARGS) {
    uint64_t cond; POP_S(cond);
    if (cond == 0) {
        int64_t off = (int64_t)pc->imm[0];
        pc = pc + 1 + off;
        DISPATCH_ALL();
    }
    NEXT_ALL();
}

PRESERVE_NONE void op_jump_if_not_zero_s(HANDLER_ARGS) {
    uint64_t cond; POP_S(cond);
    if (cond != 0) {
        int64_t off = (int64_t)pc->imm[0];
        pc = pc + 1 + off;
        DISPATCH_ALL();
    }
    NEXT_ALL();
}

PRESERVE_NONE void op_print_i32_s(HANDLER_ARGS) {
    uint64_t val; POP_S(val);
    printf("%d\n", (int32_t)val);
    NEXT_ALL();
}
PRESERVE_NONE void op_print_f32_s(HANDLER_ARGS) {
    uint64_t val; POP_S(val);
    float f = as_f32(val);
    if (f == floorf(f) && fabsf(f) < 1e15f) { printf("%.1f\n", f); }
    else { printf("%g\n", f); }
    NEXT_ALL();
}
PRESERVE_NONE void op_putc_s(HANDLER_ARGS) {
    uint64_t val; POP_S(val);
    putchar((char)(int32_t)val);
    NEXT_ALL();
}
PRESERVE_NONE void op_assert_s(HANDLER_ARGS) {
    uint64_t val; POP_S(val);
    printf("assert(%s)\n", val != 0 ? "true" : "false");
    fflush(stdout);
    if (val == 0) { fprintf(stderr, "Assertion failed\n"); fflush(stderr); exit(1); }
    NEXT_ALL();
}
PRESERVE_NONE void op_memzero_s(HANDLER_ARGS) {
    uint8_t* dst = (uint8_t*)t0; DROP1_S();
    memset(dst, 0, (size_t)pc->imm[0]);
    NEXT_ALL();
}

// --- Binary ops (shallow: no t3 fill after shift) ---

#define SHALLOW_BINOP_I(name, op) \
PRESERVE_NONE void name(HANDLER_ARGS) { \
    t0 = (uint64_t)((int64_t)t1 op (int64_t)t0); \
    BINOP_SHIFT_S(); \
    NEXT_ALL(); \
}

#define SHALLOW_BINOP_F32(name, op) \
PRESERVE_NONE void name(HANDLER_ARGS) { \
    t0 = from_f32(as_f32(t1) op as_f32(t0)); \
    BINOP_SHIFT_S(); \
    NEXT_ALL(); \
}

#define SHALLOW_BINOP_F64(name, op) \
PRESERVE_NONE void name(HANDLER_ARGS) { \
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
PRESERVE_NONE void name(HANDLER_ARGS) { \
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

PRESERVE_NONE void op_and_s(HANDLER_ARGS) { t0 = t1 & t0; BINOP_SHIFT_S(); NEXT_ALL(); }
PRESERVE_NONE void op_or_s(HANDLER_ARGS) { t0 = t1 | t0; BINOP_SHIFT_S(); NEXT_ALL(); }
PRESERVE_NONE void op_xor_s(HANDLER_ARGS) { t0 = t1 ^ t0; BINOP_SHIFT_S(); NEXT_ALL(); }
PRESERVE_NONE void op_shl_s(HANDLER_ARGS) { t0 = t1 << (t0 & 63); BINOP_SHIFT_S(); NEXT_ALL(); }
PRESERVE_NONE void op_shr_s(HANDLER_ARGS) { t0 = (uint64_t)((int64_t)t1 >> (t0 & 63)); BINOP_SHIFT_S(); NEXT_ALL(); }
PRESERVE_NONE void op_ushr_s(HANDLER_ARGS) { t0 = t1 >> (t0 & 63); BINOP_SHIFT_S(); NEXT_ALL(); }

// --- Store ops (shallow: no fill after dropping 2) ---

PRESERVE_NONE void op_store32_s(HANDLER_ARGS) {
    *(int32_t*)t1 = (int32_t)t0;
    DROP2_S();
    NEXT_ALL();
}
PRESERVE_NONE void op_store64_s(HANDLER_ARGS) {
    *(uint64_t*)t1 = t0;
    DROP2_S();
    NEXT_ALL();
}
PRESERVE_NONE void op_store8_s(HANDLER_ARGS) {
    *(uint8_t*)t1 = (uint8_t)t0;
    DROP2_S();
    NEXT_ALL();
}
PRESERVE_NONE void op_store32_off_s(HANDLER_ARGS) {
    *(int32_t*)((uint8_t*)t1 + (int32_t)pc->imm[0]) = (int32_t)t0;
    DROP2_S();
    NEXT_ALL();
}
PRESERVE_NONE void op_store64_off_s(HANDLER_ARGS) {
    *(uint64_t*)((uint8_t*)t1 + (int32_t)pc->imm[0]) = t0;
    DROP2_S();
    NEXT_ALL();
}
PRESERVE_NONE void op_store8_off_s(HANDLER_ARGS) {
    *((uint8_t*)t1 + (int32_t)pc->imm[0]) = (uint8_t)t0;
    DROP2_S();
    NEXT_ALL();
}

// --- FusedFMulFAdd/FSub shallow (consumes t0,t1,t2 — no memory fill) ---

PRESERVE_NONE void op_fused_fmul_fadd_s(HANDLER_ARGS) {
    t0 = from_f32(as_f32(t2) + as_f32(t1) * as_f32(t0));
    t1 = t3;
    NEXT_ALL();
}
PRESERVE_NONE void op_fused_fmul_fsub_s(HANDLER_ARGS) {
    t0 = from_f32(as_f32(t2) - as_f32(t1) * as_f32(t0));
    t1 = t3;
    NEXT_ALL();
}

// --- FMA term: accum += locals[a] * load(slot,off). Pure register on t0. ---
PRESERVE_NONE void op_fused_get_addr_fmul_fadd(HANDLER_ARGS) {
    float coeff = as_f32(locals[pc->imm[0]]);
    float state = as_f32((uint64_t)(int64_t)*(int32_t*)(lm + pc->imm[1] * 8 + (int32_t)pc->imm[2]));
    t0 = from_f32(as_f32(t0) + coeff * state);
    NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
}

// --- FMA term: accum -= locals[a] * load(slot,off). ---
PRESERVE_NONE void op_fused_get_addr_fmul_fsub(HANDLER_ARGS) {
    float coeff = as_f32(locals[pc->imm[0]]);
    float state = as_f32((uint64_t)(int64_t)*(int32_t*)(lm + pc->imm[1] * 8 + (int32_t)pc->imm[2]));
    t0 = from_f32(as_f32(t0) - coeff * state);
    NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
}

// --- Load struct field into local: locals[dst] = load(slot,off). No stack change. ---
PRESERVE_NONE void op_fused_addr_load32off_set(HANDLER_ARGS) {
    locals[pc->imm[2]] = (uint64_t)(int64_t)*(int32_t*)(lm + pc->imm[0] * 8 + (int32_t)pc->imm[1]);
    NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
}

// --- Store local into struct field: *(i32*)(lm + slot*8 + off) = locals[src]. No stack change. ---
PRESERVE_NONE void op_fused_addr_imm_get_store32(HANDLER_ARGS) {
    *(int32_t*)(lm + pc->imm[0] * 8 + (int32_t)pc->imm[1]) = (int32_t)locals[pc->imm[2]];
    NEXT(ctx, pc, sp, locals, lm, t0, t1, t2, t3);
}

// --- MemCopy shallow (pop 2, no fill) ---
PRESERVE_NONE void op_memcopy_s(HANDLER_ARGS) {
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

    // Enter entry function.
    uint64_t* locals;
    uint8_t* lm;
    enter_function(ctx, entry_func, NULL, 0, &locals, &lm);

    // Start dispatch with all 4 TOS registers zeroed.
    Instruction* pc = ctx->functions[entry_func].code;
    ((Handler)pc->handler)(ctx, pc, stack, locals, lm, 0, 0, 0, 0);

    free(stack);
    return ctx->result;
}
