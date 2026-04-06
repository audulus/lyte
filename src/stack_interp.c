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
// Handlers
// ============================================================================

// --- Constants (push: spill t0, load new value) ---

PRESERVE_NONE void op_i64_const(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    *sp++ = t0;
    t0 = pc->imm[0];
    NEXT(ctx, pc, sp, locals, lm, t0);
}

PRESERVE_NONE void op_f32_const(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    *sp++ = t0;
    t0 = pc->imm[0]; // already encoded as f32 bits in u64
    NEXT(ctx, pc, sp, locals, lm, t0);
}

PRESERVE_NONE void op_f64_const(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    *sp++ = t0;
    t0 = pc->imm[0];
    NEXT(ctx, pc, sp, locals, lm, t0);
}

// --- Local variables ---

PRESERVE_NONE void op_local_get(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    *sp++ = t0;
    t0 = locals[pc->imm[0]];
    NEXT(ctx, pc, sp, locals, lm, t0);
}

PRESERVE_NONE void op_local_set(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    locals[pc->imm[0]] = t0;
    t0 = *--sp;
    NEXT(ctx, pc, sp, locals, lm, t0);
}

PRESERVE_NONE void op_local_tee(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    locals[pc->imm[0]] = t0; // peek, don't pop
    NEXT(ctx, pc, sp, locals, lm, t0);
}

PRESERVE_NONE void op_local_addr(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    *sp++ = t0;
    t0 = (uint64_t)(lm + pc->imm[0] * 8);
    NEXT(ctx, pc, sp, locals, lm, t0);
}

// --- Global variables ---

PRESERVE_NONE void op_global_addr(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    *sp++ = t0;
    t0 = (uint64_t)(ctx->globals + (int32_t)pc->imm[0]);
    NEXT(ctx, pc, sp, locals, lm, t0);
}

// --- Integer arithmetic (binary: pop 2, push 1) ---

PRESERVE_NONE void op_iadd(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    uint64_t b = t0;
    t0 = (uint64_t)((int64_t)*--sp + (int64_t)b);
    NEXT(ctx, pc, sp, locals, lm, t0);
}

PRESERVE_NONE void op_isub(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    uint64_t b = t0;
    t0 = (uint64_t)((int64_t)*--sp - (int64_t)b);
    NEXT(ctx, pc, sp, locals, lm, t0);
}

PRESERVE_NONE void op_imul(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    uint64_t b = t0;
    t0 = (uint64_t)((int64_t)*--sp * (int64_t)b);
    NEXT(ctx, pc, sp, locals, lm, t0);
}

PRESERVE_NONE void op_idiv(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    int64_t b = (int64_t)t0;
    int64_t a = (int64_t)*--sp;
    t0 = (uint64_t)(b != 0 ? a / b : 0);
    NEXT(ctx, pc, sp, locals, lm, t0);
}

PRESERVE_NONE void op_udiv(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    uint64_t b = t0;
    uint64_t a = *--sp;
    t0 = b != 0 ? a / b : 0;
    NEXT(ctx, pc, sp, locals, lm, t0);
}

PRESERVE_NONE void op_irem(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    int64_t b = (int64_t)t0;
    int64_t a = (int64_t)*--sp;
    t0 = (uint64_t)(b != 0 ? a % b : 0);
    NEXT(ctx, pc, sp, locals, lm, t0);
}

PRESERVE_NONE void op_ipow(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    uint32_t exp = (uint32_t)t0;
    int64_t base = (int64_t)*--sp;
    t0 = (uint64_t)ipow(base, exp);
    NEXT(ctx, pc, sp, locals, lm, t0);
}

// Unary integer ops
PRESERVE_NONE void op_ineg(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    t0 = (uint64_t)(-(int64_t)t0);
    NEXT(ctx, pc, sp, locals, lm, t0);
}

PRESERVE_NONE void op_iadd_imm(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    t0 = (uint64_t)((int64_t)t0 + (int64_t)pc->imm[0]);
    NEXT(ctx, pc, sp, locals, lm, t0);
}

// --- Float32 arithmetic (binary: pop 2, push 1) ---

PRESERVE_NONE void op_fadd(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    float b = as_f32(t0);
    t0 = from_f32(as_f32(*--sp) + b);
    NEXT(ctx, pc, sp, locals, lm, t0);
}

PRESERVE_NONE void op_fsub(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    float b = as_f32(t0);
    t0 = from_f32(as_f32(*--sp) - b);
    NEXT(ctx, pc, sp, locals, lm, t0);
}

PRESERVE_NONE void op_fmul(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    float b = as_f32(t0);
    t0 = from_f32(as_f32(*--sp) * b);
    NEXT(ctx, pc, sp, locals, lm, t0);
}

PRESERVE_NONE void op_fdiv(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    float b = as_f32(t0);
    t0 = from_f32(as_f32(*--sp) / b);
    NEXT(ctx, pc, sp, locals, lm, t0);
}

PRESERVE_NONE void op_fpow(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    float b = as_f32(t0);
    t0 = from_f32(powf(as_f32(*--sp), b));
    NEXT(ctx, pc, sp, locals, lm, t0);
}

// Unary f32
PRESERVE_NONE void op_fneg(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    t0 = from_f32(-as_f32(t0));
    NEXT(ctx, pc, sp, locals, lm, t0);
}

// --- Float64 arithmetic (binary: pop 2, push 1) ---

PRESERVE_NONE void op_dadd(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    double b = as_f64(t0);
    t0 = from_f64(as_f64(*--sp) + b);
    NEXT(ctx, pc, sp, locals, lm, t0);
}

PRESERVE_NONE void op_dsub(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    double b = as_f64(t0);
    t0 = from_f64(as_f64(*--sp) - b);
    NEXT(ctx, pc, sp, locals, lm, t0);
}

PRESERVE_NONE void op_dmul(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    double b = as_f64(t0);
    t0 = from_f64(as_f64(*--sp) * b);
    NEXT(ctx, pc, sp, locals, lm, t0);
}

PRESERVE_NONE void op_ddiv(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    double b = as_f64(t0);
    t0 = from_f64(as_f64(*--sp) / b);
    NEXT(ctx, pc, sp, locals, lm, t0);
}

PRESERVE_NONE void op_dpow(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    double b = as_f64(t0);
    t0 = from_f64(pow(as_f64(*--sp), b));
    NEXT(ctx, pc, sp, locals, lm, t0);
}

// Unary f64
PRESERVE_NONE void op_dneg(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    t0 = from_f64(-as_f64(t0));
    NEXT(ctx, pc, sp, locals, lm, t0);
}

// --- Comparisons (binary: pop 2, push 1) ---

#define CMP_OP(name, type, cast, op) \
PRESERVE_NONE void name(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) { \
    type b = cast(t0); type a = cast(*--sp); \
    t0 = (a op b) ? 1 : 0; \
    NEXT(ctx, pc, sp, locals, lm, t0); \
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

// --- Bitwise (binary: pop 2, push 1) ---

PRESERVE_NONE void op_and(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    uint64_t b = t0; t0 = *--sp & b; NEXT(ctx, pc, sp, locals, lm, t0);
}
PRESERVE_NONE void op_or(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    uint64_t b = t0; t0 = *--sp | b; NEXT(ctx, pc, sp, locals, lm, t0);
}
PRESERVE_NONE void op_xor(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    uint64_t b = t0; t0 = *--sp ^ b; NEXT(ctx, pc, sp, locals, lm, t0);
}
PRESERVE_NONE void op_not(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    t0 = ~t0; NEXT(ctx, pc, sp, locals, lm, t0);
}
PRESERVE_NONE void op_shl(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    uint64_t b = t0; t0 = *--sp << (b & 63); NEXT(ctx, pc, sp, locals, lm, t0);
}
PRESERVE_NONE void op_shr(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    uint64_t b = t0; t0 = (uint64_t)((int64_t)*--sp >> (b & 63)); NEXT(ctx, pc, sp, locals, lm, t0);
}
PRESERVE_NONE void op_ushr(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    uint64_t b = t0; t0 = *--sp >> (b & 63); NEXT(ctx, pc, sp, locals, lm, t0);
}

// --- Type conversions (unary: pop 1, push 1) ---

PRESERVE_NONE void op_i32_to_f32(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    t0 = from_f32((float)(int32_t)t0); NEXT(ctx, pc, sp, locals, lm, t0);
}
PRESERVE_NONE void op_f32_to_i32(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    t0 = (uint64_t)(int64_t)(int32_t)as_f32(t0); NEXT(ctx, pc, sp, locals, lm, t0);
}
PRESERVE_NONE void op_i32_to_f64(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    t0 = from_f64((double)(int32_t)t0); NEXT(ctx, pc, sp, locals, lm, t0);
}
PRESERVE_NONE void op_f64_to_i32(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    t0 = (uint64_t)(int64_t)(int32_t)as_f64(t0); NEXT(ctx, pc, sp, locals, lm, t0);
}
PRESERVE_NONE void op_f32_to_f64(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    t0 = from_f64((double)as_f32(t0)); NEXT(ctx, pc, sp, locals, lm, t0);
}
PRESERVE_NONE void op_f64_to_f32(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    t0 = from_f32((float)as_f64(t0)); NEXT(ctx, pc, sp, locals, lm, t0);
}
PRESERVE_NONE void op_i32_to_i8(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    t0 = (uint64_t)(int64_t)(int8_t)(int32_t)t0; NEXT(ctx, pc, sp, locals, lm, t0);
}
PRESERVE_NONE void op_i8_to_i32(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    t0 = (uint64_t)(int64_t)(int32_t)(int8_t)t0; NEXT(ctx, pc, sp, locals, lm, t0);
}
PRESERVE_NONE void op_i64_to_u32(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    t0 = t0 & 0xFFFFFFFF; NEXT(ctx, pc, sp, locals, lm, t0);
}

// --- Memory loads (unary: pop 1 addr, push 1 value) ---

PRESERVE_NONE void op_load8(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    t0 = (uint64_t)(int64_t)(int8_t)*(uint8_t*)t0;
    NEXT(ctx, pc, sp, locals, lm, t0);
}

PRESERVE_NONE void op_load32(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    t0 = (uint64_t)(int64_t)*(int32_t*)t0;
    NEXT(ctx, pc, sp, locals, lm, t0);
}

PRESERVE_NONE void op_load64(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    t0 = *(uint64_t*)t0;
    NEXT(ctx, pc, sp, locals, lm, t0);
}

PRESERVE_NONE void op_load32_off(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    uint8_t* base = (uint8_t*)t0;
    int32_t off = (int32_t)pc->imm[0];
    t0 = (uint64_t)(int64_t)*(int32_t*)(base + off);
    NEXT(ctx, pc, sp, locals, lm, t0);
}

PRESERVE_NONE void op_load64_off(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    uint8_t* base = (uint8_t*)t0;
    int32_t off = (int32_t)pc->imm[0];
    t0 = *(uint64_t*)(base + off);
    NEXT(ctx, pc, sp, locals, lm, t0);
}

// --- Memory stores (pop 2, push 0) ---

PRESERVE_NONE void op_store8(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    uint64_t val = t0;
    uint64_t addr_v = *--sp;
    t0 = *--sp;
    *(uint8_t*)addr_v = (uint8_t)val;
    NEXT(ctx, pc, sp, locals, lm, t0);
}

PRESERVE_NONE void op_store32(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    uint64_t val = t0;
    uint64_t addr_v = *--sp;
    t0 = *--sp;
    *(int32_t*)addr_v = (int32_t)val;
    NEXT(ctx, pc, sp, locals, lm, t0);
}

PRESERVE_NONE void op_store64(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    uint64_t val = t0;
    uint64_t addr_v = *--sp;
    t0 = *--sp;
    *(uint64_t*)addr_v = val;
    NEXT(ctx, pc, sp, locals, lm, t0);
}

PRESERVE_NONE void op_store8_off(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    uint64_t val = t0;
    uint64_t base = *--sp;
    t0 = *--sp;
    *((uint8_t*)base + (int32_t)pc->imm[0]) = (uint8_t)val;
    NEXT(ctx, pc, sp, locals, lm, t0);
}

PRESERVE_NONE void op_store32_off(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    uint64_t val = t0;
    uint64_t base = *--sp;
    t0 = *--sp;
    *(int32_t*)((uint8_t*)base + (int32_t)pc->imm[0]) = (int32_t)val;
    NEXT(ctx, pc, sp, locals, lm, t0);
}

PRESERVE_NONE void op_store64_off(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    uint64_t val = t0;
    uint64_t base = *--sp;
    t0 = *--sp;
    *(uint64_t*)((uint8_t*)base + (int32_t)pc->imm[0]) = val;
    NEXT(ctx, pc, sp, locals, lm, t0);
}

// --- Bulk memory ---

PRESERVE_NONE void op_memcopy(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    uint8_t* src = (uint8_t*)t0;
    uint8_t* dst = (uint8_t*)*--sp;
    t0 = *--sp;
    memmove(dst, src, (size_t)pc->imm[0]);
    NEXT(ctx, pc, sp, locals, lm, t0);
}

PRESERVE_NONE void op_memzero(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    uint8_t* dst = (uint8_t*)t0;
    t0 = *--sp;
    memset(dst, 0, (size_t)pc->imm[0]);
    NEXT(ctx, pc, sp, locals, lm, t0);
}

PRESERVE_NONE void op_memeq(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    uint8_t* b = (uint8_t*)t0;
    uint8_t* a = (uint8_t*)*--sp;
    t0 = memcmp(a, b, (size_t)pc->imm[0]) == 0 ? 1 : 0;
    NEXT(ctx, pc, sp, locals, lm, t0);
}

PRESERVE_NONE void op_memne(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    uint8_t* b = (uint8_t*)t0;
    uint8_t* a = (uint8_t*)*--sp;
    t0 = memcmp(a, b, (size_t)pc->imm[0]) != 0 ? 1 : 0;
    NEXT(ctx, pc, sp, locals, lm, t0);
}

// --- Slice operations ---

PRESERVE_NONE void op_slice_eq(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    uint8_t* fat_b = (uint8_t*)t0;
    uint8_t* fat_a = (uint8_t*)*--sp;
    uint32_t elem_size = (uint32_t)pc->imm[0];
    uint32_t len_a = *(uint32_t*)(fat_a + 8);
    uint32_t len_b = *(uint32_t*)(fat_b + 8);
    if (len_a != len_b) { t0 = 0; NEXT(ctx, pc, sp, locals, lm, t0); }
    uint8_t* data_a = *(uint8_t**)fat_a;
    uint8_t* data_b = *(uint8_t**)fat_b;
    t0 = memcmp(data_a, data_b, (size_t)len_a * elem_size) == 0 ? 1 : 0;
    NEXT(ctx, pc, sp, locals, lm, t0);
}

PRESERVE_NONE void op_slice_ne(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    uint8_t* fat_b = (uint8_t*)t0;
    uint8_t* fat_a = (uint8_t*)*--sp;
    uint32_t elem_size = (uint32_t)pc->imm[0];
    uint32_t len_a = *(uint32_t*)(fat_a + 8);
    uint32_t len_b = *(uint32_t*)(fat_b + 8);
    if (len_a != len_b) { t0 = 1; NEXT(ctx, pc, sp, locals, lm, t0); }
    uint8_t* data_a = *(uint8_t**)fat_a;
    uint8_t* data_b = *(uint8_t**)fat_b;
    t0 = memcmp(data_a, data_b, (size_t)len_a * elem_size) != 0 ? 1 : 0;
    NEXT(ctx, pc, sp, locals, lm, t0);
}

PRESERVE_NONE void op_slice_load32(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    int64_t idx = (int64_t)t0;
    uint8_t* fat = (uint8_t*)*--sp;
    uint8_t* data = *(uint8_t**)fat;
    t0 = (uint64_t)(int64_t)*(int32_t*)(data + idx * 4);
    NEXT(ctx, pc, sp, locals, lm, t0);
}

PRESERVE_NONE void op_slice_store32(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    int32_t val = (int32_t)t0;
    int64_t idx = (int64_t)*--sp;
    uint8_t* fat = (uint8_t*)*--sp;
    t0 = *--sp;
    uint8_t* data = *(uint8_t**)fat;
    *(int32_t*)(data + idx * 4) = val;
    NEXT(ctx, pc, sp, locals, lm, t0);
}

// --- Control flow ---

PRESERVE_NONE void op_jump(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    int64_t off = (int64_t)pc->imm[0];
    pc = pc + 1 + off;
    DISPATCH(ctx, pc, sp, locals, lm, t0);
}

PRESERVE_NONE void op_jump_if_zero(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    uint64_t cond = t0;
    t0 = *--sp;
    if (cond == 0) {
        int64_t off = (int64_t)pc->imm[0];
        pc = pc + 1 + off;
        DISPATCH(ctx, pc, sp, locals, lm, t0);
    }
    NEXT(ctx, pc, sp, locals, lm, t0);
}

PRESERVE_NONE void op_jump_if_not_zero(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    uint64_t cond = t0;
    t0 = *--sp;
    if (cond != 0) {
        int64_t off = (int64_t)pc->imm[0];
        pc = pc + 1 + off;
        DISPATCH(ctx, pc, sp, locals, lm, t0);
    }
    NEXT(ctx, pc, sp, locals, lm, t0);
}

// --- Function calls ---

PRESERVE_NONE void op_call(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    // Spill t0 to make all values in memory
    *sp++ = t0;

    uint32_t target = (uint32_t)pc->imm[0];
    uint32_t nargs = (uint32_t)pc->imm[1];

    // Save call frame.
    CallFrame* frame = &ctx->call_stack[ctx->call_depth++];
    frame->return_pc = pc + 1;
    frame->saved_locals = locals;
    frame->saved_lm = lm;
    frame->saved_sp = sp - nargs; // caller's sp after args removed (but with spilled t0)
    frame->func_idx = (uint32_t)pc->imm[2]; // current function index (stored by linker)
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
    DISPATCH(ctx, entry, frame->saved_sp, new_locals, new_lm, 0); // t0=0 (callee starts with empty TOS)
}

PRESERVE_NONE void op_call_closure(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    // t0 is the fat_ptr address (last thing pushed before call_closure)
    uint8_t* fat_ptr = (uint8_t*)t0;
    uint32_t target = (uint32_t)*(int64_t*)fat_ptr;
    ctx->closure_ptr = *(uint64_t*)(fat_ptr + 8);
    uint32_t nargs = (uint32_t)pc->imm[0];

    // DON'T spill t0 (it was the fat_ptr, which is consumed, not an arg)
    // The args are in memory at sp[-nargs..sp]

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
    DISPATCH(ctx, entry, frame->saved_sp, new_locals, new_lm, 0);
}

PRESERVE_NONE void op_call_indirect(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    uint32_t target = (uint32_t)(int64_t)t0; // t0 is the func_idx
    uint32_t nargs = (uint32_t)pc->imm[0];

    // Args are in memory
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
    DISPATCH(ctx, entry, frame->saved_sp, new_locals, new_lm, 0);
}

PRESERVE_NONE void op_return(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    // t0 has the return value
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
    // Return value becomes the new t0
    DISPATCH(ctx, frame->return_pc, sp, frame->saved_locals, frame->saved_lm, result);
}

PRESERVE_NONE void op_return_void(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    free(locals);

    if (ctx->call_depth == 0) {
        ctx->result = 0;
        ctx->done = 1;
        return;
    }

    CallFrame* frame = &ctx->call_stack[--ctx->call_depth];
    ctx->local_memory_size = frame->saved_lm_size;
    sp = frame->saved_sp;
    // Reload caller's t0 from memory (it was spilled before the call)
    t0 = *--sp;
    DISPATCH(ctx, frame->return_pc, sp, frame->saved_locals, frame->saved_lm, t0);
}

// --- Stack manipulation ---

PRESERVE_NONE void op_drop(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    t0 = *--sp;
    NEXT(ctx, pc, sp, locals, lm, t0);
}

// --- Math builtins (f32) ---

#define F32_UNARY(name, func) \
PRESERVE_NONE void name(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) { \
    t0 = from_f32(func(as_f32(t0))); \
    NEXT(ctx, pc, sp, locals, lm, t0); \
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
PRESERVE_NONE void name(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) { \
    t0 = from_f64(func(as_f64(t0))); \
    NEXT(ctx, pc, sp, locals, lm, t0); \
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

// Binary math (unary predicates)

PRESERVE_NONE void op_isnan_f32(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    t0 = isnan(as_f32(t0)) ? 1 : 0;
    NEXT(ctx, pc, sp, locals, lm, t0);
}
PRESERVE_NONE void op_isnan_f64(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    t0 = isnan(as_f64(t0)) ? 1 : 0;
    NEXT(ctx, pc, sp, locals, lm, t0);
}
PRESERVE_NONE void op_isinf_f32(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    t0 = isinf(as_f32(t0)) ? 1 : 0;
    NEXT(ctx, pc, sp, locals, lm, t0);
}
PRESERVE_NONE void op_isinf_f64(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    t0 = isinf(as_f64(t0)) ? 1 : 0;
    NEXT(ctx, pc, sp, locals, lm, t0);
}

// Binary math (atan2)

PRESERVE_NONE void op_atan2_f32(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    float b = as_f32(t0);
    t0 = from_f32(atan2f(as_f32(*--sp), b));
    NEXT(ctx, pc, sp, locals, lm, t0);
}

PRESERVE_NONE void op_atan2_f64(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    double b = as_f64(t0);
    t0 = from_f64(atan2(as_f64(*--sp), b));
    NEXT(ctx, pc, sp, locals, lm, t0);
}

// --- Debug/IO (pop 1, push 0) ---

PRESERVE_NONE void op_print_i32(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    int32_t val = (int32_t)t0;
    t0 = *--sp;
    printf("%d\n", val);
    NEXT(ctx, pc, sp, locals, lm, t0);
}

PRESERVE_NONE void op_print_f32(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    float val = as_f32(t0);
    t0 = *--sp;
    if (val == floorf(val) && fabsf(val) < 1e15f) {
        printf("%.1f\n", val);
    } else {
        printf("%g\n", val);
    }
    NEXT(ctx, pc, sp, locals, lm, t0);
}

PRESERVE_NONE void op_putc(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    char c = (char)(int32_t)t0;
    t0 = *--sp;
    putchar(c);
    NEXT(ctx, pc, sp, locals, lm, t0);
}

PRESERVE_NONE void op_assert(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    uint64_t val = t0;
    t0 = *--sp;
    printf("assert(%s)\n", val != 0 ? "true" : "false");
    fflush(stdout);
    if (val == 0) {
        fprintf(stderr, "Assertion failed\n");
        fflush(stderr);
        exit(1);
    }
    NEXT(ctx, pc, sp, locals, lm, t0);
}

PRESERVE_NONE void op_get_closure_ptr(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    *sp++ = t0;
    t0 = ctx->closure_ptr;
    NEXT(ctx, pc, sp, locals, lm, t0);
}

// ============================================================================
// Fused superinstructions
// ============================================================================

// locals[a] * locals[b] (f32) -- push
PRESERVE_NONE void op_fused_get_get_fmul(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    *sp++ = t0;
    t0 = from_f32(as_f32(locals[pc->imm[0]]) * as_f32(locals[pc->imm[1]]));
    NEXT(ctx, pc, sp, locals, lm, t0);
}

// locals[a] + locals[b] (f32) -- push
PRESERVE_NONE void op_fused_get_get_fadd(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    *sp++ = t0;
    t0 = from_f32(as_f32(locals[pc->imm[0]]) + as_f32(locals[pc->imm[1]]));
    NEXT(ctx, pc, sp, locals, lm, t0);
}

// locals[a] - locals[b] (f32) -- push
PRESERVE_NONE void op_fused_get_get_fsub(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    *sp++ = t0;
    t0 = from_f32(as_f32(locals[pc->imm[0]]) - as_f32(locals[pc->imm[1]]));
    NEXT(ctx, pc, sp, locals, lm, t0);
}

// locals[a] + locals[b] (i64) -- push
PRESERVE_NONE void op_fused_get_get_iadd(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    *sp++ = t0;
    t0 = (uint64_t)((int64_t)locals[pc->imm[0]] + (int64_t)locals[pc->imm[1]]);
    NEXT(ctx, pc, sp, locals, lm, t0);
}

// locals[a] < locals[b] (i64 signed) -- push
PRESERVE_NONE void op_fused_get_get_ilt(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    *sp++ = t0;
    t0 = ((int64_t)locals[pc->imm[0]] < (int64_t)locals[pc->imm[1]]) ? 1 : 0;
    NEXT(ctx, pc, sp, locals, lm, t0);
}

// TOS * locals[a] (f32) -- unary (replaces TOS)
PRESERVE_NONE void op_fused_get_fmul(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    t0 = from_f32(as_f32(t0) * as_f32(locals[pc->imm[0]]));
    NEXT(ctx, pc, sp, locals, lm, t0);
}

// TOS + locals[a] (f32) -- unary (replaces TOS)
PRESERVE_NONE void op_fused_get_fadd(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    t0 = from_f32(as_f32(t0) + as_f32(locals[pc->imm[0]]));
    NEXT(ctx, pc, sp, locals, lm, t0);
}

// TOS - locals[a] (f32) -- unary (replaces TOS)
PRESERVE_NONE void op_fused_get_fsub(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    t0 = from_f32(as_f32(t0) - as_f32(locals[pc->imm[0]]));
    NEXT(ctx, pc, sp, locals, lm, t0);
}

// Fused multiply-accumulate: t0=b, pop a, pop c, push c + a*b (f32)
PRESERVE_NONE void op_fused_fmul_fadd(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    float b = as_f32(t0);
    float a = as_f32(*--sp);
    float c = as_f32(*--sp);
    t0 = from_f32(c + a * b);
    NEXT(ctx, pc, sp, locals, lm, t0);
}

// Fused multiply-subtract: t0=b, pop a, pop c, push c - a*b (f32)
PRESERVE_NONE void op_fused_fmul_fsub(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    float b = as_f32(t0);
    float a = as_f32(*--sp);
    float c = as_f32(*--sp);
    t0 = from_f32(c - a * b);
    NEXT(ctx, pc, sp, locals, lm, t0);
}

// Load i32 from lm + slot*8 + offset -- push
PRESERVE_NONE void op_fused_addr_load32off(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    *sp++ = t0;
    uint8_t* base = lm + pc->imm[0] * 8;
    t0 = (uint64_t)(int64_t)*(int32_t*)(base + (int32_t)pc->imm[1]);
    NEXT(ctx, pc, sp, locals, lm, t0);
}

// locals[dst] = locals[src] + imm -- pass t0 through
PRESERVE_NONE void op_fused_get_addimm_set(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    locals[pc->imm[2]] = (uint64_t)((int64_t)locals[pc->imm[0]] + (int64_t)pc->imm[1]);
    NEXT(ctx, pc, sp, locals, lm, t0);
}

// if !(locals[a] < locals[b]) jump -- pass t0 through
PRESERVE_NONE void op_fused_get_get_ilt_jiz(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    if ((int64_t)locals[pc->imm[0]] >= (int64_t)locals[pc->imm[1]]) {
        int64_t off = (int64_t)pc->imm[2];
        pc = pc + 1 + off;
        DISPATCH(ctx, pc, sp, locals, lm, t0);
    }
    NEXT(ctx, pc, sp, locals, lm, t0);
}

// locals[n] = i64 constant -- pass t0 through
PRESERVE_NONE void op_fused_const_set(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    locals[pc->imm[1]] = pc->imm[0];
    NEXT(ctx, pc, sp, locals, lm, t0);
}

// locals[n] = f32 constant (bits in imm[0]) -- pass t0 through
PRESERVE_NONE void op_fused_f32const_set(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    locals[pc->imm[1]] = pc->imm[0];
    NEXT(ctx, pc, sp, locals, lm, t0);
}

// Push slice_data[locals[idx_local] * 4] from slice at lm + slot*8 -- push
PRESERVE_NONE void op_fused_addr_get_sload32(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    *sp++ = t0;
    uint8_t* fat = lm + pc->imm[0] * 8;
    int64_t idx = (int64_t)locals[pc->imm[1]];
    uint8_t* data = *(uint8_t**)fat;
    t0 = (uint64_t)(int64_t)*(int32_t*)(data + idx * 4);
    NEXT(ctx, pc, sp, locals, lm, t0);
}

PRESERVE_NONE void op_halt(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    ctx->result = (int64_t)t0;
    ctx->done = 1;
    free(locals);
    return;
}

PRESERVE_NONE void op_nop(Ctx* ctx, Instruction* pc, uint64_t* sp, uint64_t* locals, uint8_t* lm, uint64_t t0) {
    NEXT(ctx, pc, sp, locals, lm, t0);
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

    // Start dispatch.
    Instruction* pc = ctx->functions[entry_func].code;
    ((Handler)pc->handler)(ctx, pc, stack, locals, lm, 0); // t0=0 initial

    free(stack);
    return ctx->result;
}
