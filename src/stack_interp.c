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

// Returns 1 on success, 0 on overflow (ctx->error is set and ctx->done is
// raised so the caller can break out of the tail-call chain).
// Bump-allocate one contiguous frame from the frame stack for `func_idx`.
// Layout:
//   [scalar locals (local_count u64 slots, min 3)] [local memory (ceil/8 u64 slots)]
// fp = locals, and lm = (uint8_t*)(locals + local_count).
//
// Arguments are NOT copied here — callers (op_call et al.) pop them
// straight from the TOS window into new_locals[0..nargs]. stack_interp_run
// enters the entry function with no arguments. Non-param scalars and
// local memory are left uninitialized: the codegen emits explicit
// LocalSet(I64Const(0)) / MemZero for `var` declarations without
// initializers, so the frame does not need to be zeroed here.
static int enter_function(Ctx* ctx, uint32_t func_idx, uint64_t** out_locals) {
    FuncMeta* meta = &ctx->functions[func_idx];

    uint32_t scalar_slots = meta->local_count < 3 ? 3 : meta->local_count;
    uint32_t mem_slots = (meta->local_memory + 7) / 8;
    uint32_t total_slots = scalar_slots + mem_slots;

    size_t fs_base = ctx->frame_stack_size;
    size_t fs_needed = fs_base + total_slots;
    if (fs_needed > ctx->frame_stack_cap) {
        ctx->error = "stack overflow";
        ctx->done = 1;
        *out_locals = NULL;
        return 0;
    }
    *out_locals = ctx->frame_stack + fs_base;
    ctx->frame_stack_size = fs_needed;
    return 1;
}

// ============================================================================
// f32/f64 bit helpers
// ============================================================================

static inline float    as_f32(uint64_t v) { uint32_t b = (uint32_t)v; float f; memcpy(&f, &b, 4); return f; }
static inline uint64_t from_f32(float f)  { uint32_t b; memcpy(&b, &f, 4); return (uint64_t)b; }
static inline double   as_f64(uint64_t v) { double d; memcpy(&d, &v, 8); return d; }
static inline uint64_t from_f64(double d) { uint64_t v; memcpy(&v, &d, 8); return v; }

static inline int32_t load_i32_unaligned(const void* p) {
    int32_t v;
    memcpy(&v, p, sizeof(v));
    return v;
}

static inline uint32_t load_u32_unaligned(const void* p) {
    uint32_t v;
    memcpy(&v, p, sizeof(v));
    return v;
}

static inline uint64_t load_u64_unaligned(const void* p) {
    uint64_t v;
    memcpy(&v, p, sizeof(v));
    return v;
}

static inline float load_f32_unaligned(const void* p) {
    float v;
    memcpy(&v, p, sizeof(v));
    return v;
}

static inline uint8_t* load_ptr_unaligned(const void* p) {
    uint8_t* v;
    memcpy(&v, p, sizeof(v));
    return v;
}

static inline void store_i32_unaligned(void* p, int32_t v) {
    memcpy(p, &v, sizeof(v));
}

static inline void store_u64_unaligned(void* p, uint64_t v) {
    memcpy(p, &v, sizeof(v));
}

static inline void store_f32_unaligned(void* p, float v) {
    memcpy(p, &v, sizeof(v));
}

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

// Handler signature shorthand. `locals` and `fsp` are both handler
// arguments pinned to GPRs by preserve_none on both aarch64 and
// x86-64 — the hot-local cache removal (docs/HOT_LOCALS.md) freed up
// enough GPR arg slots on x86-64 to carry them in registers too.
//
// The float TOS window (f0..f3) is a separate 4-slot register window
// living in FP/SIMD registers, independent of the integer window
// (t0..t3) in GPRs. Float ops read/write f0..f3 directly and never
// cross between GPR and FP register files, dodging the ~3-cycle
// fmov/movq penalty that the old "f32 bit-pattern in u64" design
// paid on every float arithmetic op.
#define HANDLER_ARGS Ctx* ctx, Instruction* pc, uint64_t* sp, float* fsp, uint64_t* locals, uint64_t t0, uint64_t t1, uint64_t t2, uint64_t t3, float f0, float f1, float f2, float f3, void* _nh_raw

#define HANDLER(name) PRESERVE_NONE void name(HANDLER_ARGS)
// Cast nh from void* for use in NEXT macro.
#define nh ((Handler)_nh_raw)

// Dispatch macros. Parameterless — they reference the handler's in-scope
// ctx/pc/sp/fsp/locals/t0-t3/f0-f3/_nh_raw arguments directly and must
// only be used inside a HANDLER() body.
//
// NEXT: linear fall-through. Branch to the preloaded nh and preload the
// handler for the instruction after next.
#define NEXT() \
    do { \
        Instruction* _next = pc + 1; \
        void* _new_nh = (_next + 1)->handler; \
        __attribute__((musttail)) return ((Handler)_nh_raw)(ctx, _next, sp, fsp, locals, t0, t1, t2, t3, f0, f1, f2, f3, _new_nh); \
    } while(0)

#define DISPATCH() \
    do { \
        Handler _target_h = (Handler)pc->handler; \
        void* _new_nh = (pc + 1)->handler; \
        __attribute__((musttail)) return _target_h(ctx, pc, sp, fsp, locals, t0, t1, t2, t3, f0, f1, f2, f3, _new_nh); \
    } while(0)

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
// Handlers
// ============================================================================

// --- Constants (push) ---

HANDLER(op_i64_const) {
    PUSH(pc->imm[0]);
    NEXT();
}

HANDLER(op_f32_const) {
    PUSH(pc->imm[0]); // already encoded as f32 bits in u64
    NEXT();
}

HANDLER(op_f64_const) {
    PUSH(pc->imm[0]);
    NEXT();
}

// --- Local variables ---

HANDLER(op_local_get) {
    PUSH(locals[pc->imm[0]]);
    NEXT();
}

HANDLER(op_local_set) {
    uint64_t val; POP(val);
    locals[pc->imm[0]] = val;
    NEXT();
}

HANDLER(op_local_tee) {
    locals[pc->imm[0]] = t0; // peek, don't pop
    NEXT();
}

HANDLER(op_local_addr) {
    // imm[0] is a u64-slot index into the frame. After the rebase pass,
    // memory slots start at local_count, so this skips the scalar locals.
    PUSH((uint64_t)(locals + pc->imm[0]));
    NEXT();
}

// --- Global variables ---

HANDLER(op_global_addr) {
    PUSH((uint64_t)(ctx->globals + (int32_t)pc->imm[0]));
    NEXT();
}

// --- Integer arithmetic (binary: consume t0 and t1, push result) ---

HANDLER(op_iadd) {
    t0 = (uint64_t)((int64_t)t1 + (int64_t)t0);
    BINOP_SHIFT();
    NEXT();
}

HANDLER(op_isub) {
    t0 = (uint64_t)((int64_t)t1 - (int64_t)t0);
    BINOP_SHIFT();
    NEXT();
}

HANDLER(op_imul) {
    t0 = (uint64_t)((int64_t)t1 * (int64_t)t0);
    BINOP_SHIFT();
    NEXT();
}

HANDLER(op_idiv) {
    int64_t b = (int64_t)t0;
    int64_t a = (int64_t)t1;
    t0 = (uint64_t)(b != 0 ? a / b : 0);
    BINOP_SHIFT();
    NEXT();
}

HANDLER(op_udiv) {
    uint64_t b = t0;
    uint64_t a = t1;
    t0 = b != 0 ? a / b : 0;
    BINOP_SHIFT();
    NEXT();
}

HANDLER(op_irem) {
    int64_t b = (int64_t)t0;
    int64_t a = (int64_t)t1;
    t0 = (uint64_t)(b != 0 ? a % b : 0);
    BINOP_SHIFT();
    NEXT();
}

HANDLER(op_ipow) {
    uint32_t exp = (uint32_t)t0;
    int64_t base = (int64_t)t1;
    t0 = (uint64_t)ipow(base, exp);
    BINOP_SHIFT();
    NEXT();
}

// Unary integer ops
HANDLER(op_ineg) {
    t0 = (uint64_t)(-(int64_t)t0);
    NEXT();
}

HANDLER(op_iadd_imm) {
    t0 = (uint64_t)((int64_t)t0 + (int64_t)pc->imm[0]);
    NEXT();
}

// --- Float64 arithmetic (binary) ---
// (f32 arithmetic lives in the F-window handlers below — the int-window
// f32 ops were removed after Instruments profiling showed they polluted
// the indirect-branch predictor on the shared dispatch BR.)

HANDLER(op_dadd) {
    t0 = from_f64(as_f64(t1) + as_f64(t0));
    BINOP_SHIFT();
    NEXT();
}

HANDLER(op_dsub) {
    t0 = from_f64(as_f64(t1) - as_f64(t0));
    BINOP_SHIFT();
    NEXT();
}

HANDLER(op_dmul) {
    t0 = from_f64(as_f64(t1) * as_f64(t0));
    BINOP_SHIFT();
    NEXT();
}

HANDLER(op_ddiv) {
    t0 = from_f64(as_f64(t1) / as_f64(t0));
    BINOP_SHIFT();
    NEXT();
}

HANDLER(op_dpow) {
    t0 = from_f64(pow(as_f64(t1), as_f64(t0)));
    BINOP_SHIFT();
    NEXT();
}

// Unary f64
HANDLER(op_dneg) {
    t0 = from_f64(-as_f64(t0));
    NEXT();
}

// --- Comparisons (binary: consume t0 and t1, push result) ---

#define CMP_OP(name, type, cast, op) \
HANDLER(name) { \
    type b = cast(t0); type a = cast(t1); \
    t0 = (a op b) ? 1 : 0; \
    BINOP_SHIFT(); \
    NEXT(); \
}

CMP_OP(op_ieq, int64_t, (int64_t), ==)
CMP_OP(op_ine, int64_t, (int64_t), !=)
CMP_OP(op_ilt, int64_t, (int64_t), <)
CMP_OP(op_ile, int64_t, (int64_t), <=)
CMP_OP(op_igt, int64_t, (int64_t), >)
CMP_OP(op_ige, int64_t, (int64_t), >=)
CMP_OP(op_ult, uint64_t, (uint64_t), <)
CMP_OP(op_ugt, uint64_t, (uint64_t), >)
// (f32 comparisons are F-window only — see the FW_CMP macro below.)
CMP_OP(op_deq, double, as_f64, ==)
CMP_OP(op_dlt, double, as_f64, <)
CMP_OP(op_dle, double, as_f64, <=)

// --- Bitwise (binary) ---

HANDLER(op_and) {
    t0 = t1 & t0; BINOP_SHIFT(); NEXT();
}
HANDLER(op_or) {
    t0 = t1 | t0; BINOP_SHIFT(); NEXT();
}
HANDLER(op_xor) {
    t0 = t1 ^ t0; BINOP_SHIFT(); NEXT();
}
HANDLER(op_not) {
    t0 = ~t0; NEXT();
}
HANDLER(op_shl) {
    t0 = t1 << (t0 & 63); BINOP_SHIFT(); NEXT();
}
HANDLER(op_shr) {
    t0 = (uint64_t)((int64_t)t1 >> (t0 & 63)); BINOP_SHIFT(); NEXT();
}
HANDLER(op_ushr) {
    t0 = t1 >> (t0 & 63); BINOP_SHIFT(); NEXT();
}

// --- Type conversions (unary) ---

HANDLER(op_i32_to_f32) {
    t0 = from_f32((float)(int32_t)t0); NEXT();
}
HANDLER(op_f32_to_i32) {
    t0 = (uint64_t)(int64_t)(int32_t)as_f32(t0); NEXT();
}
HANDLER(op_i32_to_f64) {
    t0 = from_f64((double)(int32_t)t0); NEXT();
}
HANDLER(op_f64_to_i32) {
    t0 = (uint64_t)(int64_t)(int32_t)as_f64(t0); NEXT();
}
HANDLER(op_f32_to_f64) {
    // Pop f0 (f32 from float window), push f64 bits to int window.
    double v = (double)f0;
    FDROP1();
    PUSH(from_f64(v));
    NEXT();
}
HANDLER(op_f64_to_f32) {
    // Pop f64 bits from int window, push f32 to float window.
    float v = (float)as_f64(t0);
    DROP1();
    FPUSH(v);
    NEXT();
}
HANDLER(op_i32_to_i8) {
    t0 = (uint64_t)(int64_t)(int8_t)(int32_t)t0; NEXT();
}
HANDLER(op_i8_to_i32) {
    t0 = (uint64_t)(int64_t)(int32_t)(int8_t)t0; NEXT();
}
HANDLER(op_i64_to_u32) {
    t0 = t0 & 0xFFFFFFFF; NEXT();
}

// --- Memory loads (unary: transform t0) ---

HANDLER(op_load8) {
    t0 = (uint64_t)(int64_t)(int8_t)*(uint8_t*)t0;
    NEXT();
}

HANDLER(op_load32) {
    t0 = (uint64_t)(int64_t)load_i32_unaligned((const void*)t0);
    NEXT();
}

HANDLER(op_load64) {
    t0 = load_u64_unaligned((const void*)t0);
    NEXT();
}

HANDLER(op_load32_off) {
    uint8_t* base = (uint8_t*)t0;
    int32_t off = (int32_t)pc->imm[0];
    t0 = (uint64_t)(int64_t)load_i32_unaligned(base + off);
    NEXT();
}

HANDLER(op_load64_off) {
    uint8_t* base = (uint8_t*)t0;
    int32_t off = (int32_t)pc->imm[0];
    t0 = load_u64_unaligned(base + off);
    NEXT();
}

// --- Memory stores (pop 2: val=t0, addr=t1) ---

HANDLER(op_store8) {
    *(uint8_t*)t1 = (uint8_t)t0;
    DROP2();
    NEXT();
}

HANDLER(op_store32) {
    store_i32_unaligned((void*)t1, (int32_t)t0);
    DROP2();
    NEXT();
}

HANDLER(op_store64) {
    store_u64_unaligned((void*)t1, t0);
    DROP2();
    NEXT();
}

HANDLER(op_store8_off) {
    *((uint8_t*)t1 + (int32_t)pc->imm[0]) = (uint8_t)t0;
    DROP2();
    NEXT();
}

HANDLER(op_store32_off) {
    store_i32_unaligned((uint8_t*)t1 + (int32_t)pc->imm[0], (int32_t)t0);
    DROP2();
    NEXT();
}

HANDLER(op_store64_off) {
    store_u64_unaligned((uint8_t*)t1 + (int32_t)pc->imm[0], t0);
    DROP2();
    NEXT();
}

// --- Bulk memory ---

HANDLER(op_memcopy) {
    // pop src=t0, pop dst=t1
    memmove((uint8_t*)t1, (uint8_t*)t0, (size_t)pc->imm[0]);
    DROP2();
    NEXT();
}

HANDLER(op_memzero) {
    // pop dst=t0
    memset((uint8_t*)t0, 0, (size_t)pc->imm[0]);
    DROP1();
    NEXT();
}

HANDLER(op_memeq) {
    // binary: pop b=t0, pop a=t1, push result
    uint8_t* b = (uint8_t*)t0;
    uint8_t* a = (uint8_t*)t1;
    t0 = memcmp(a, b, (size_t)pc->imm[0]) == 0 ? 1 : 0;
    BINOP_SHIFT();
    NEXT();
}

HANDLER(op_memne) {
    uint8_t* b = (uint8_t*)t0;
    uint8_t* a = (uint8_t*)t1;
    t0 = memcmp(a, b, (size_t)pc->imm[0]) != 0 ? 1 : 0;
    BINOP_SHIFT();
    NEXT();
}

// --- Slice operations ---

HANDLER(op_slice_eq) {
    // binary: pop b=t0, pop a=t1, push result
    uint8_t* fat_b = (uint8_t*)t0;
    uint8_t* fat_a = (uint8_t*)t1;
    uint32_t elem_size = (uint32_t)pc->imm[0];
    uint32_t len_a = load_u32_unaligned(fat_a + 8);
    uint32_t len_b = load_u32_unaligned(fat_b + 8);
    if (len_a != len_b) {
        t0 = 0;
        BINOP_SHIFT();
        NEXT();
    }
    uint8_t* data_a = load_ptr_unaligned(fat_a);
    uint8_t* data_b = load_ptr_unaligned(fat_b);
    t0 = memcmp(data_a, data_b, (size_t)len_a * elem_size) == 0 ? 1 : 0;
    BINOP_SHIFT();
    NEXT();
}

HANDLER(op_slice_ne) {
    uint8_t* fat_b = (uint8_t*)t0;
    uint8_t* fat_a = (uint8_t*)t1;
    uint32_t elem_size = (uint32_t)pc->imm[0];
    uint32_t len_a = load_u32_unaligned(fat_a + 8);
    uint32_t len_b = load_u32_unaligned(fat_b + 8);
    if (len_a != len_b) {
        t0 = 1;
        BINOP_SHIFT();
        NEXT();
    }
    uint8_t* data_a = load_ptr_unaligned(fat_a);
    uint8_t* data_b = load_ptr_unaligned(fat_b);
    t0 = memcmp(data_a, data_b, (size_t)len_a * elem_size) != 0 ? 1 : 0;
    BINOP_SHIFT();
    NEXT();
}

HANDLER(op_slice_load32) {
    // binary: pop idx=t0, pop fat=t1, push result
    int64_t idx = (int64_t)t0;
    uint8_t* fat = (uint8_t*)t1;
    uint8_t* data = load_ptr_unaligned(fat);
    t0 = (uint64_t)(int64_t)load_i32_unaligned(data + idx * 4);
    BINOP_SHIFT();
    NEXT();
}

HANDLER(op_slice_store32) {
    // pop val=t0, pop idx=t1, pop fat=t2
    int32_t val = (int32_t)t0;
    int64_t idx = (int64_t)t1;
    uint8_t* fat = (uint8_t*)t2;
    uint8_t* data = load_ptr_unaligned(fat);
    store_i32_unaligned(data + idx * 4, val);
    DROP3();
    NEXT();
}

HANDLER(op_slice_load32_f) {
    // pop idx=t0, pop fat=t1, push f32 to float window
    int64_t idx = (int64_t)t0;
    uint8_t* fat = (uint8_t*)t1;
    uint8_t* data = load_ptr_unaligned(fat);
    float v = load_f32_unaligned(data + idx * 4);
    DROP2();
    FPUSH(v);
    NEXT();
}

HANDLER(op_slice_store32_f) {
    // pop f0 (value), pop idx=t0, pop fat=t1
    int64_t idx = (int64_t)t0;
    uint8_t* fat = (uint8_t*)t1;
    uint8_t* data = load_ptr_unaligned(fat);
    store_f32_unaligned(data + idx * 4, f0);
    DROP2();
    FDROP1();
    NEXT();
}

// --- Control flow ---

HANDLER(op_jump) {
    int64_t off = (int64_t)pc->imm[0];
    pc = pc + 1 + off;
    DISPATCH();
}

HANDLER(op_jump_if_zero) {
    uint64_t cond = t0;
    t0 = t1; t1 = t2; t2 = t3; t3 = *--sp;
    if (cond == 0) {
        int64_t off = (int64_t)pc->imm[0];
        pc = pc + 1 + off;
        DISPATCH();
    }
    NEXT();
}

HANDLER(op_jump_if_not_zero) {
    uint64_t cond = t0;
    t0 = t1; t1 = t2; t2 = t3; t3 = *--sp;
    if (cond != 0) {
        int64_t off = (int64_t)pc->imm[0];
        pc = pc + 1 + off;
        DISPATCH();
    }
    NEXT();
}

// --- Function calls ---

HANDLER(op_call) {
    // No-spill call: the caller's non-arg t1..t3 values get round-tripped
    // through memory naturally by the callee's deep PUSH/POP ops as its
    // own depth grows and shrinks. We don't spill anything here; we just
    // copy the args into the callee's locals and pop them off the TOS
    // window. The caller's t0..t3 (post-pop) is handed to the callee
    // through DISPATCH.

    uint32_t target = (uint32_t)pc->imm[0];
    uint32_t nargs  = (uint32_t)(pc->imm[1] & 0xFF);

    if (ctx->call_depth >= ctx->call_stack_cap) {
        ctx->error = "call stack overflow";
        ctx->done = 1;
        return;
    }
    CallFrame* frame = &ctx->call_stack[ctx->call_depth++];
    frame->return_pc = pc + 1;
    frame->saved_locals = locals;
    frame->func_idx = (uint32_t)pc->imm[2];
    frame->saved_frame_size = ctx->frame_stack_size;

    uint64_t* new_locals;
    if (!enter_function(ctx, target, &new_locals)) {
        return; // Stack overflow: ctx->error / ctx->done already set.
    }

    // Transfer args from the TOS window into the callee's param slots,
    // and simultaneously pop them from the caller's window. After the
    // pops, t0..t3 holds the caller's stack state minus the args, with
    // any deeper values pulled in from memory by the deep-pop refill.
    //
    // Layout: t0 = arg(nargs-1) (last pushed), t1 = arg(nargs-2), ...,
    // t(nargs-1) = arg0 (first pushed). For nargs > 4 the bottom args
    // sit in memory just below sp (placed there by earlier deep pushes
    // displacing them out of the window).
    switch (nargs) {
        case 0:
            break;
        case 1:
            new_locals[0] = t0;
            t0 = t1; t1 = t2; t2 = t3; t3 = *--sp;
            break;
        case 2:
            new_locals[0] = t1;
            new_locals[1] = t0;
            t0 = t2; t1 = t3; t2 = *--sp; t3 = *--sp;
            break;
        case 3:
            new_locals[0] = t2;
            new_locals[1] = t1;
            new_locals[2] = t0;
            t0 = t3; t1 = *--sp; t2 = *--sp; t3 = *--sp;
            break;
        case 4:
            new_locals[0] = t3;
            new_locals[1] = t2;
            new_locals[2] = t1;
            new_locals[3] = t0;
            t0 = *--sp; t1 = *--sp; t2 = *--sp; t3 = *--sp;
            break;
        default: {
            // nargs > 4: 4 args in registers, (nargs - 4) in memory just
            // below sp. Read memory args first (before sp moves), then
            // register args, then pop: drop sp by (nargs - 4) to skip
            // the memory args, then four *--sp pops refill the window
            // with the caller's spilled t0..t3.
            uint32_t mem_args = nargs - 4;
            uint64_t* src = sp - mem_args;
            for (uint32_t i = 0; i < mem_args; i++) {
                new_locals[i] = src[i];
            }
            new_locals[mem_args + 0] = t3;
            new_locals[mem_args + 1] = t2;
            new_locals[mem_args + 2] = t1;
            new_locals[mem_args + 3] = t0;
            sp -= mem_args;
            t0 = *--sp;
            t1 = *--sp;
            t2 = *--sp;
            t3 = *--sp;
            break;
        }
    }

    // Callee inherits the caller's TOS window (post-pop). The window
    // holds garbage at depths < 4 — that's fine because well-formed
    // callee code never reads t-registers it hasn't pushed first.
    pc = ctx->functions[target].code;
    locals = new_locals;
    DISPATCH();
}

// Shared body for op_call_closure and op_call_indirect: consume t0 (the
// fat_ptr or func_idx), then consume nargs args. The call site differs
// only in how `target` is computed before this runs.
//
// The TOS layout at entry is: t0 = fat_ptr / func_idx (consumed),
// t1 = arg(nargs-1), t2 = arg(nargs-2), t3 = arg(nargs-3), and any
// further args in memory just below sp.
//
// We do (nargs + 1) deep pops total: 1 for the consumed t0, then nargs
// more for the args. After this, t0..t3 holds the caller's stack state
// minus the consumed values.
HANDLER(op_call_closure) {
    // t0 is the fat_ptr address (last thing pushed before call_closure)
    uint8_t* fat_ptr = (uint8_t*)t0;
    uint32_t target = (uint32_t)load_u64_unaligned(fat_ptr);
    ctx->closure_ptr = load_u64_unaligned(fat_ptr + 8);
    uint32_t nargs = (uint32_t)pc->imm[0];

    if (ctx->call_depth >= ctx->call_stack_cap) {
        ctx->error = "call stack overflow";
        ctx->done = 1;
        return;
    }
    CallFrame* frame = &ctx->call_stack[ctx->call_depth++];
    frame->return_pc = pc + 1;
    frame->saved_locals = locals;
    frame->func_idx = (uint32_t)pc->imm[1];
    frame->saved_frame_size = ctx->frame_stack_size;

    uint64_t* new_locals;
    if (!enter_function(ctx, target, &new_locals)) {
        return; // Stack overflow.
    }

    // Args are below the consumed fat_ptr in t0:
    //   t1 = arg(nargs-1), t2 = arg(nargs-2), t3 = arg(nargs-3),
    //   memory just below sp = arg(nargs-4), ..., arg0 deepest.
    // Copy args to new_locals, then pop (nargs + 1) values total.
    switch (nargs) {
        case 0:
            // Only the fat_ptr is consumed. Pop 1.
            t0 = t1; t1 = t2; t2 = t3; t3 = *--sp;
            break;
        case 1:
            new_locals[0] = t1;
            // Pop 2 (fat_ptr + 1 arg).
            t0 = t2; t1 = t3; t2 = *--sp; t3 = *--sp;
            break;
        case 2:
            new_locals[0] = t2;
            new_locals[1] = t1;
            // Pop 3.
            t0 = t3; t1 = *--sp; t2 = *--sp; t3 = *--sp;
            break;
        case 3:
            new_locals[0] = t3;
            new_locals[1] = t2;
            new_locals[2] = t1;
            // Pop 4.
            t0 = *--sp; t1 = *--sp; t2 = *--sp; t3 = *--sp;
            break;
        default: {
            // nargs >= 4: 3 args in registers (t1..t3), the rest in memory
            // just below sp. arg0 is deepest, arg(nargs-1) is just below
            // the fat_ptr in t1. Pop (nargs + 1) values total: skip the
            // mem_args memory args plus the shadow slot holding the
            // first-pushed value, then four *--sp pops refill the window.
            uint32_t mem_args = nargs - 3;
            uint64_t* src = sp - mem_args;
            for (uint32_t i = 0; i < mem_args; i++) {
                new_locals[i] = src[i];
            }
            new_locals[mem_args + 0] = t3;
            new_locals[mem_args + 1] = t2;
            new_locals[mem_args + 2] = t1;
            sp -= mem_args;
            t0 = *--sp;
            t1 = *--sp;
            t2 = *--sp;
            t3 = *--sp;
            break;
        }
    }

    pc = ctx->functions[target].code;
    locals = new_locals;
    DISPATCH();
}

HANDLER(op_call_indirect) {
    uint32_t target = (uint32_t)(int64_t)t0; // t0 consumed as func_idx
    uint32_t nargs = (uint32_t)pc->imm[0];

    if (ctx->call_depth >= ctx->call_stack_cap) {
        ctx->error = "call stack overflow";
        ctx->done = 1;
        return;
    }
    CallFrame* frame = &ctx->call_stack[ctx->call_depth++];
    frame->return_pc = pc + 1;
    frame->saved_locals = locals;
    frame->func_idx = (uint32_t)pc->imm[1];
    frame->saved_frame_size = ctx->frame_stack_size;

    uint64_t* new_locals;
    if (!enter_function(ctx, target, &new_locals)) {
        return; // Stack overflow.
    }

    // Same shape as op_call_closure: t0 was consumed (as func_idx), so
    // args are at t1..t3 + memory below sp. Pop (nargs + 1) total.
    switch (nargs) {
        case 0:
            t0 = t1; t1 = t2; t2 = t3; t3 = *--sp;
            break;
        case 1:
            new_locals[0] = t1;
            t0 = t2; t1 = t3; t2 = *--sp; t3 = *--sp;
            break;
        case 2:
            new_locals[0] = t2;
            new_locals[1] = t1;
            t0 = t3; t1 = *--sp; t2 = *--sp; t3 = *--sp;
            break;
        case 3:
            new_locals[0] = t3;
            new_locals[1] = t2;
            new_locals[2] = t1;
            t0 = *--sp; t1 = *--sp; t2 = *--sp; t3 = *--sp;
            break;
        default: {
            // nargs >= 4: same shape as op_call_closure. Pop (nargs + 1)
            // total; mem_args of those are already-copied memory args,
            // the remaining 4 *--sp pops refill the caller's window.
            uint32_t mem_args = nargs - 3;
            uint64_t* src = sp - mem_args;
            for (uint32_t i = 0; i < mem_args; i++) {
                new_locals[i] = src[i];
            }
            new_locals[mem_args + 0] = t3;
            new_locals[mem_args + 1] = t2;
            new_locals[mem_args + 2] = t1;
            sp -= mem_args;
            t0 = *--sp;
            t1 = *--sp;
            t2 = *--sp;
            t3 = *--sp;
            break;
        }
    }

    pc = ctx->functions[target].code;
    locals = new_locals;
    DISPATCH();
}

HANDLER(op_return) {
    // The callee leaves its return value in t0 with t1..t3 already
    // restored by its own balanced deep PUSH/POP traffic. Neither sp
    // nor fsp needs an explicit restore from the call frame: under
    // balanced stack discipline the callee's sp at op_return time is
    // already `caller_sp + arity`, which is exactly what the caller
    // expects for its own depth bookkeeping, and the f-window is
    // empty by the same argument (f32 return values cross to t0 via
    // FToBitsF before Return). Rewinding to a saved value would drop
    // the return-value slot.
    if (ctx->call_depth == 0) {
        ctx->result = (int64_t)t0;
        ctx->done = 1;
        return; // Exit interpreter.
    }

    CallFrame* frame = &ctx->call_stack[--ctx->call_depth];
    ctx->frame_stack_size = frame->saved_frame_size;
    locals = frame->saved_locals;
    pc = frame->return_pc;
    DISPATCH();
}

HANDLER(op_return_void) {
    // Same no-rewind invariant as op_return, arity = 0.
    if (ctx->call_depth == 0) {
        ctx->result = 0;
        ctx->done = 1;
        return;
    }

    CallFrame* frame = &ctx->call_stack[--ctx->call_depth];
    ctx->frame_stack_size = frame->saved_frame_size;
    locals = frame->saved_locals;
    pc = frame->return_pc;
    DISPATCH();
}

// --- Stack manipulation ---

HANDLER(op_drop) {
    DROP1();
    NEXT();
}

// --- Math builtins (f64) ---
// (f32 math intrinsics are F-window only — see FW_F32_UNARY below.)

#define F64_UNARY(name, func) \
HANDLER(name) { \
    t0 = from_f64(func(as_f64(t0))); \
    NEXT(); \
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

// Unary predicates (f64 only — f32 variants live in the F-window section)

HANDLER(op_isnan_f64) {
    t0 = isnan(as_f64(t0)) ? 1 : 0;
    NEXT();
}
HANDLER(op_isinf_f64) {
    t0 = isinf(as_f64(t0)) ? 1 : 0;
    NEXT();
}

// Binary math (atan2, f64 only)

HANDLER(op_atan2_f64) {
    t0 = from_f64(atan2(as_f64(t1), as_f64(t0)));
    BINOP_SHIFT();
    NEXT();
}

// --- Debug/IO (pop 1) ---

HANDLER(op_print_i32) {
    int32_t val = (int32_t)t0;
    DROP1();
    printf("%d\n", val);
    NEXT();
}

// (op_print_f32 replaced by op_print_f32_f — f32 print lives in the
// F-window section.)

HANDLER(op_putc) {
    char c = (char)(int32_t)t0;
    DROP1();
    putchar(c);
    NEXT();
}

HANDLER(op_assert) {
    uint64_t val = t0;
    DROP1();
    printf("assert(%s)\n", val != 0 ? "true" : "false");
    fflush(stdout);
    if (val == 0) {
        ctx->error = "assertion failed";
        ctx->done = 1;
        return; // Break the tail-call chain back to stack_interp_run.
    }
    NEXT();
}

HANDLER(op_get_closure_ptr) {
    PUSH(ctx->closure_ptr);
    NEXT();
}

// ============================================================================
// Fused superinstructions
// ============================================================================
//
// The int-window f32 fused ops (op_fused_get_get_fmul, op_fused_fmul_fadd,
// etc.) and the FW-peephole output ops (op_fused_get_get_fmul_fw,
// op_fused_get_addr_fmul_fadd_fw, op_local_set_l0_fw, …) were deleted
// after Instruments profiling showed their co-existence with the
// F-variant handlers polluted the indirect-branch predictor on the
// shared dispatch BR. F handlers are now the only path for f32
// arithmetic; see the float-window section further down.

// locals[a] + locals[b] (i64) -- push
HANDLER(op_fused_get_get_iadd) {
    PUSH((uint64_t)((int64_t)locals[pc->imm[0]] + (int64_t)locals[pc->imm[1]]));
    NEXT();
}

// locals[a] < locals[b] (i64 signed) -- push
HANDLER(op_fused_get_get_ilt) {
    PUSH(((int64_t)locals[pc->imm[0]] < (int64_t)locals[pc->imm[1]]) ? 1 : 0);
    NEXT();
}

// Load i32 from frame slot*8 + offset -- push
HANDLER(op_fused_addr_load32off) {
    PUSH((uint64_t)(int64_t)load_i32_unaligned((uint8_t*)locals + pc->imm[0] * 8 + (int32_t)pc->imm[1]));
    NEXT();
}

// locals[dst] = locals[src] + imm -- no stack change
HANDLER(op_fused_get_addimm_set) {
    locals[pc->imm[2]] = (uint64_t)((int64_t)locals[pc->imm[0]] + (int64_t)pc->imm[1]);
    NEXT();
}

// if !(locals[a] < locals[b]) jump -- no stack change
HANDLER(op_fused_get_get_ilt_jiz) {
    if ((int64_t)locals[pc->imm[0]] >= (int64_t)locals[pc->imm[1]]) {
        int64_t off = (int64_t)pc->imm[2];
        pc = pc + 1 + off;
        DISPATCH();
    }
    NEXT();
}

static inline uint8_t imm_u8(const Instruction* pc, int idx);

#define BOUNDS_CHECK_HANDLER(name, PAIRS) \
HANDLER(name) { \
    for (int i = 0; i < (PAIRS); i++) { \
        uint8_t idx = imm_u8(pc, i * 2); \
        uint8_t len = imm_u8(pc, i * 2 + 1); \
        if ((int64_t)locals[idx] >= (int64_t)locals[len]) { \
            int64_t off = (int64_t)pc->imm[2]; \
            pc = pc + 1 + off; \
            DISPATCH(); \
        } \
    } \
    NEXT(); \
}

BOUNDS_CHECK_HANDLER(op_fused_bounds_check1_jiz, 1)
BOUNDS_CHECK_HANDLER(op_fused_bounds_check2_jiz, 2)
BOUNDS_CHECK_HANDLER(op_fused_bounds_check3_jiz, 3)
BOUNDS_CHECK_HANDLER(op_fused_bounds_check4_jiz, 4)
BOUNDS_CHECK_HANDLER(op_fused_bounds_check5_jiz, 5)
BOUNDS_CHECK_HANDLER(op_fused_bounds_check6_jiz, 6)
BOUNDS_CHECK_HANDLER(op_fused_bounds_check7_jiz, 7)
BOUNDS_CHECK_HANDLER(op_fused_bounds_check8_jiz, 8)
#undef BOUNDS_CHECK_HANDLER

// locals[n] = i64 constant -- no stack change
HANDLER(op_fused_const_set) {
    locals[pc->imm[1]] = pc->imm[0];
    NEXT();
}

// locals[n] = f32 constant (bits in imm[0]) -- no stack change
HANDLER(op_fused_f32const_set) {
    locals[pc->imm[1]] = pc->imm[0];
    NEXT();
}

// Push slice_data[locals[idx_local] * 4] from slice at frame slot -- push
HANDLER(op_fused_addr_get_sload32) {
    uint8_t* fat = (uint8_t*)(locals + pc->imm[0]);
    int64_t idx = (int64_t)locals[pc->imm[1]];
    uint8_t* data = load_ptr_unaligned(fat);
    PUSH((uint64_t)(int64_t)load_i32_unaligned(data + idx * 4));
    NEXT();
}

HANDLER(op_halt) {
    ctx->result = (int64_t)t0;
    ctx->done = 1;
    return;
}

HANDLER(op_nop) {
    NEXT();
}

// ============================================================================
// Fused handlers (continued)
// ============================================================================

// (op_fused_f32const_fgt_jiz replaced by op_fused_f32const_fgt_jiz_f —
// the f32 phase-wrap check in biquad now uses the F-window variant.)

// --- 3-address register-form arithmetic. locals[dst] = locals[a] <op> locals[b]. ---
// No stack change; these replace LocalGet+LocalGet+<op>+LocalSet sequences
// emitted directly by codegen for `x = a OP b` patterns when x, a, b are
// simple scalar locals.
HANDLER(op_fused_get_get_fadd_set) {
    locals[pc->imm[2]] = from_f32(as_f32(locals[pc->imm[0]]) + as_f32(locals[pc->imm[1]]));
    NEXT();
}
HANDLER(op_fused_get_get_fsub_set) {
    locals[pc->imm[2]] = from_f32(as_f32(locals[pc->imm[0]]) - as_f32(locals[pc->imm[1]]));
    NEXT();
}
HANDLER(op_fused_get_get_fmul_set) {
    locals[pc->imm[2]] = from_f32(as_f32(locals[pc->imm[0]]) * as_f32(locals[pc->imm[1]]));
    NEXT();
}
HANDLER(op_fused_get_get_fdiv_set) {
    locals[pc->imm[2]] = from_f32(as_f32(locals[pc->imm[0]]) / as_f32(locals[pc->imm[1]]));
    NEXT();
}
HANDLER(op_fused_get_get_iadd_set) {
    locals[pc->imm[2]] = (uint64_t)((int64_t)locals[pc->imm[0]] + (int64_t)locals[pc->imm[1]]);
    NEXT();
}
HANDLER(op_fused_get_get_isub_set) {
    locals[pc->imm[2]] = (uint64_t)((int64_t)locals[pc->imm[0]] - (int64_t)locals[pc->imm[1]]);
    NEXT();
}
HANDLER(op_fused_get_get_imul_set) {
    locals[pc->imm[2]] = (uint64_t)((int64_t)locals[pc->imm[0]] * (int64_t)locals[pc->imm[1]]);
    NEXT();
}

// --- Copy struct field: dst_field = src_field. No stack change. ---
HANDLER(op_fused_field_copy32) {
    int32_t src_off = (int32_t)pc->imm[1];
    int32_t dst_off = (int32_t)pc->imm[2];
    uint8_t* base = (uint8_t*)(locals + pc->imm[0]);
    store_i32_unaligned(base + dst_off, load_i32_unaligned(base + src_off));
    NEXT();
}

// --- Local fixed-size array load: push *(i32*)((u8*)(locals + slot) + locals[idx]*4). ---
// Used for `arr[k]` where arr is a local [T; N] scalar array (inline in
// the frame, not behind a fat pointer).
HANDLER(op_fused_local_array_load32) {
    uint8_t* base = (uint8_t*)(locals + pc->imm[0]);
    int64_t idx = (int64_t)locals[pc->imm[1]];
    PUSH((uint64_t)(int64_t)*(int32_t*)(base + idx * 4));
    NEXT();
}

// --- Local fixed-size array store: *(i32*)((u8*)(locals + slot) + locals[idx]*4) = t0. ---
HANDLER(op_fused_local_array_store32) {
    uint8_t* base = (uint8_t*)(locals + pc->imm[0]);
    int64_t idx = (int64_t)locals[pc->imm[1]];
    *(int32_t*)(base + idx * 4) = (int32_t)t0;
    DROP1();
    NEXT();
}

// --- Slice store with fused address: *(data + locals[idx]*4) = t0. Pop 1. ---
HANDLER(op_fused_addr_get_sstore32) {
    uint8_t* fat = (uint8_t*)(locals + pc->imm[0]);
    int64_t idx = (int64_t)locals[pc->imm[1]];
    uint8_t* data = load_ptr_unaligned(fat);
    store_i32_unaligned(data + idx * 4, (int32_t)t0);
    DROP1();
    NEXT();
}

// --- Tee + slice store: locals[n] = TOS; slice[locals[idx]*4] = TOS; pop. ---
HANDLER(op_fused_tee_sstore32) {
    locals[pc->imm[0]] = t0;
    uint8_t* fat = (uint8_t*)(locals + pc->imm[1]);
    int64_t idx = (int64_t)locals[pc->imm[2]];
    uint8_t* data = load_ptr_unaligned(fat);
    store_i32_unaligned(data + idx * 4, (int32_t)t0);
    DROP1();
    NEXT();
}

// (op_fused_tee_sincos_set was a 6-op fusion for the FFT twiddle
// factor — it matched int-window CosF32/SinF32. With the F codegen
// now the only path, those are CosF32F/SinF32F and the fusion no
// longer applies; the handler was removed along with the int f32
// path.)

// --- Variable move: locals[b] = locals[a]. No stack change. ---
HANDLER(op_fused_get_set) {
    locals[pc->imm[1]] = locals[pc->imm[0]];
    NEXT();
}

static inline uint8_t imm_u8(const Instruction* pc, int idx) {
    return (uint8_t)(pc->imm[idx / 8] >> ((idx % 8) * 8));
}

#define COPY_F_PAIR(N) do { \
    uint8_t src = imm_u8(pc, (N) * 2); \
    uint8_t dst = imm_u8(pc, (N) * 2 + 1); \
    *(float*)((uint8_t*)locals + (size_t)dst * 8) = *(float*)((uint8_t*)locals + (size_t)src * 8); \
} while (0)

HANDLER(op_fused_get_set_f) {
    COPY_F_PAIR(0);
    NEXT();
}
HANDLER(op_fused_get_set2_f) {
    COPY_F_PAIR(0);
    COPY_F_PAIR(1);
    NEXT();
}
HANDLER(op_fused_get_set3_f) {
    COPY_F_PAIR(0);
    COPY_F_PAIR(1);
    COPY_F_PAIR(2);
    NEXT();
}
HANDLER(op_fused_get_set4_f) {
    COPY_F_PAIR(0);
    COPY_F_PAIR(1);
    COPY_F_PAIR(2);
    COPY_F_PAIR(3);
    NEXT();
}
HANDLER(op_fused_get_set5_f) {
    COPY_F_PAIR(0);
    COPY_F_PAIR(1);
    COPY_F_PAIR(2);
    COPY_F_PAIR(3);
    COPY_F_PAIR(4);
    NEXT();
}
HANDLER(op_fused_get_set6_f) {
    COPY_F_PAIR(0);
    COPY_F_PAIR(1);
    COPY_F_PAIR(2);
    COPY_F_PAIR(3);
    COPY_F_PAIR(4);
    COPY_F_PAIR(5);
    NEXT();
}
HANDLER(op_fused_get_set7_f) {
    COPY_F_PAIR(0);
    COPY_F_PAIR(1);
    COPY_F_PAIR(2);
    COPY_F_PAIR(3);
    COPY_F_PAIR(4);
    COPY_F_PAIR(5);
    COPY_F_PAIR(6);
    NEXT();
}
HANDLER(op_fused_get_set8_f) {
    COPY_F_PAIR(0);
    COPY_F_PAIR(1);
    COPY_F_PAIR(2);
    COPY_F_PAIR(3);
    COPY_F_PAIR(4);
    COPY_F_PAIR(5);
    COPY_F_PAIR(6);
    COPY_F_PAIR(7);
    NEXT();
}

#undef COPY_F_PAIR

// (op_fused_get_addr_fmul_fadd / _fsub and their _fw variants —
// int-window FMA terms and the narrow float_window_rewrite peephole
// outputs — were removed along with the rest of the int f32 path.
// The F-variant FusedGetAddrFMulF{Add,Sub}F lives in the float-window
// section below.)

// (op_local_set_l0_fw / _l1_fw / _l2_fw were also FW peephole outputs
// and have been removed.)

// --- Load struct field into local: locals[dst] = load(slot,off). No stack change. ---
HANDLER(op_fused_addr_load32off_set) {
    locals[pc->imm[2]] = (uint64_t)(int64_t)*(int32_t*)((uint8_t*)locals + pc->imm[0] * 8 + (int32_t)pc->imm[1]);
    NEXT();
}

// --- Store local into struct field: *(i32*)(fp + slot*8 + off) = locals[src]. No stack change. ---
HANDLER(op_fused_addr_imm_get_store32) {
    *(int32_t*)((uint8_t*)locals + pc->imm[0] * 8 + (int32_t)pc->imm[1]) = (int32_t)locals[pc->imm[2]];
    NEXT();
}

// ============================================================================
// Float-window (FP register) handlers — the only path for f32 arithmetic
// ============================================================================
//
// f32 values live in f0..f3, the FP TOS window (v0-v3 on aarch64,
// xmm0-xmm3 on x86-64). Every handler reads and writes f0..f3 directly,
// so f32 arithmetic never visits a GPR and never pays a GPR↔FP
// crossing. f32 locals are stored in the shared locals[] array as
// their bit pattern in the low 32 bits of a u64 slot — readers use
// `*(float*)` casts that only touch the low 32 bits, so the high
// half is always irrelevant.
//
// The original design had parallel int-window and F-window versions
// of every f32 op, gated by a compile-time flag. Profiling with
// Instruments showed that the coexistence of the two variant sets
// polluted the indirect-branch predictor on the shared dispatch BR,
// slowing down the shared integer handlers the two paths called
// through. Deleting the int path entirely fixed that.

// --- Constants and locals (float window) ---

HANDLER(op_f32_const_f) {
    // imm[0] is the f32 bit pattern. Reinterpret as float and push.
    uint32_t bits = (uint32_t)pc->imm[0];
    float v;
    memcpy(&v, &bits, 4);
    FPUSH(v);
    NEXT();
}

// NOTE: the imm[0] field for F LocalGet/Set/Tee holds a *byte offset*
// (pre-shifted by the bridge — see encode_imm in stack_interp_bridge.rs)
// so the handler can emit a single `ldr s, [locals, imm0]` with no
// shift. aarch64 register-indexed f32 loads only allow SCALE=2 in the
// [base, idx, lsl #SCALE] form, which doesn't match the 8-byte stride
// of `locals[]`. Pre-shifting saves one instruction per handler call.
HANDLER(op_local_get_f) {
    FPUSH(*(float*)((uint8_t*)locals + pc->imm[0]));
    NEXT();
}

// Direct f32 store to the low 32 bits of locals[n]. We deliberately
// leave the upper 32 bits undefined: f32 locals are only ever read via
// `as_f32(locals[n])` or `*(float*)(locals + n)`, both of which ignore
// the high bits. This saves an fmov (FP→GPR crossing) per handler vs
// the old `locals[n] = from_f32(f0)` form.
HANDLER(op_local_set_f) {
    *(float*)((uint8_t*)locals + pc->imm[0]) = f0;
    FDROP1();
    NEXT();
}

HANDLER(op_local_tee_f) {
    *(float*)((uint8_t*)locals + pc->imm[0]) = f0;
    NEXT();
}

HANDLER(op_drop_f) {
    FDROP1();
    NEXT();
}

// --- Float arithmetic (binary): pop b=f0, pop a=f1, push a OP b ---

HANDLER(op_fadd_f) {
    f0 = f1 + f0;
    FBINOP_SHIFT();
    NEXT();
}
HANDLER(op_fsub_f) {
    f0 = f1 - f0;
    FBINOP_SHIFT();
    NEXT();
}
HANDLER(op_fmul_f) {
    f0 = f1 * f0;
    FBINOP_SHIFT();
    NEXT();
}
HANDLER(op_fdiv_f) {
    f0 = f1 / f0;
    FBINOP_SHIFT();
    NEXT();
}
HANDLER(op_fpow_f) {
    f0 = powf(f1, f0);
    FBINOP_SHIFT();
    NEXT();
}
HANDLER(op_fneg_f) {
    f0 = -f0;
    NEXT();
}

// --- Comparisons: pop 2 from f-window, push 0/1 to int window ---

#define FW_CMP(name, op) \
HANDLER(name) { \
    PUSH((f1 op f0) ? 1ULL : 0ULL); \
    /* drop both floats */ \
    f0 = f2; f1 = f3; f2 = *--fsp; f3 = *--fsp; \
    NEXT(); \
}
FW_CMP(op_feq_f, ==)
FW_CMP(op_fne_f, !=)
FW_CMP(op_flt_f, <)
FW_CMP(op_fle_f, <=)
FW_CMP(op_fgt_f, >)
FW_CMP(op_fge_f, >=)

// --- Conversions / window crossings ---

HANDLER(op_f32_to_i32_f) {
    // Pop f0, push int t0 = (int32)f0
    int64_t v = (int64_t)(int32_t)f0;
    PUSH((uint64_t)v);
    FDROP1();
    NEXT();
}
HANDLER(op_i32_to_f32_f) {
    // Pop t0 (int32), push f0 = (float)i
    float v = (float)(int32_t)t0;
    DROP1();
    FPUSH(v);
    NEXT();
}
HANDLER(op_to_bits_f) {
    // Pop f0, push int t0 = bit pattern of f0
    uint64_t bits = from_f32(f0);
    PUSH(bits);
    FDROP1();
    NEXT();
}
HANDLER(op_from_bits_f) {
    // Pop t0 (bit pattern), push f0 = float
    float v = as_f32(t0);
    DROP1();
    FPUSH(v);
    NEXT();
}

// --- Float memory loads ---

HANDLER(op_load_f32_f) {
    // Pop addr from int window, push f32 to float window.
    float v = load_f32_unaligned((const void*)t0);
    DROP1();
    FPUSH(v);
    NEXT();
}
HANDLER(op_load_f32_off_f) {
    int32_t off = (int32_t)pc->imm[0];
    float v = load_f32_unaligned((uint8_t*)t0 + off);
    DROP1();
    FPUSH(v);
    NEXT();
}

// --- Float memory stores: pop f0 (value), pop t0 (addr) ---

HANDLER(op_store_f32_f) {
    store_f32_unaligned((void*)t0, f0);
    DROP1();
    FDROP1();
    NEXT();
}
HANDLER(op_store_f32_off_f) {
    int32_t off = (int32_t)pc->imm[0];
    store_f32_unaligned((uint8_t*)t0 + off, f0);
    DROP1();
    FDROP1();
    NEXT();
}

// --- Math intrinsics (float window) ---

#define FW_F32_UNARY(name, func) \
HANDLER(name) { \
    f0 = func(f0); \
    NEXT(); \
}
FW_F32_UNARY(op_sin_f32_f,   sinf)
FW_F32_UNARY(op_cos_f32_f,   cosf)
FW_F32_UNARY(op_tan_f32_f,   tanf)
FW_F32_UNARY(op_asin_f32_f,  asinf)
FW_F32_UNARY(op_acos_f32_f,  acosf)
FW_F32_UNARY(op_atan_f32_f,  atanf)
FW_F32_UNARY(op_sinh_f32_f,  sinhf)
FW_F32_UNARY(op_cosh_f32_f,  coshf)
FW_F32_UNARY(op_tanh_f32_f,  tanhf)
FW_F32_UNARY(op_asinh_f32_f, asinhf)
FW_F32_UNARY(op_acosh_f32_f, acoshf)
FW_F32_UNARY(op_atanh_f32_f, atanhf)
FW_F32_UNARY(op_ln_f32_f,    logf)
FW_F32_UNARY(op_exp_f32_f,   expf)
FW_F32_UNARY(op_exp2_f32_f,  exp2f)
FW_F32_UNARY(op_log10_f32_f, log10f)
FW_F32_UNARY(op_log2_f32_f,  log2f)
FW_F32_UNARY(op_sqrt_f32_f,  sqrtf)
FW_F32_UNARY(op_abs_f32_f,   fabsf)
FW_F32_UNARY(op_floor_f32_f, floorf)
FW_F32_UNARY(op_ceil_f32_f,  ceilf)

HANDLER(op_atan2_f32_f) {
    // Binary in f-window: pop b=f0, a=f1, push atan2f(a, b).
    f0 = atan2f(f1, f0);
    FBINOP_SHIFT();
    NEXT();
}

HANDLER(op_isnan_f32_f) {
    int v = isnan(f0) ? 1 : 0;
    PUSH((uint64_t)v);
    FDROP1();
    NEXT();
}
HANDLER(op_isinf_f32_f) {
    int v = isinf(f0) ? 1 : 0;
    PUSH((uint64_t)v);
    FDROP1();
    NEXT();
}

// --- Debug ---

HANDLER(op_print_f32_f) {
    float val = f0;
    FDROP1();
    if (val == floorf(val) && fabsf(val) < 1e15f) {
        printf("%.1f\n", val);
    } else {
        printf("%g\n", val);
    }
    NEXT();
}

// --- Float-window fused superinstructions (Phase 5) ---

// NOTE: imm[0] and imm[1] on these handlers are pre-shifted byte offsets
// into locals[], encoded that way by the bridge so the `ldr s` can be
// emitted without an extra lsl. See the comment in
// stack_interp_bridge.rs::encode_imm and op_local_get_f above.
HANDLER(op_fused_get_get_fadd_f) {
    float a = *(float*)((uint8_t*)locals + pc->imm[0]);
    float b = *(float*)((uint8_t*)locals + pc->imm[1]);
    FPUSH(a + b);
    NEXT();
}
HANDLER(op_fused_get_get_fsub_f) {
    float a = *(float*)((uint8_t*)locals + pc->imm[0]);
    float b = *(float*)((uint8_t*)locals + pc->imm[1]);
    FPUSH(a - b);
    NEXT();
}
HANDLER(op_fused_get_get_fmul_f) {
    float a = *(float*)((uint8_t*)locals + pc->imm[0]);
    float b = *(float*)((uint8_t*)locals + pc->imm[1]);
    FPUSH(a * b);
    NEXT();
}
HANDLER(op_fused_get_fmul_f) {
    f0 = f0 * *(float*)((uint8_t*)locals + pc->imm[0]);
    NEXT();
}
HANDLER(op_fused_get_fadd_f) {
    f0 = f0 + *(float*)((uint8_t*)locals + pc->imm[0]);
    NEXT();
}
HANDLER(op_fused_get_fsub_f) {
    f0 = f0 - *(float*)((uint8_t*)locals + pc->imm[0]);
    NEXT();
}
// Pop f0=b, f1=a, f2=c (in f-window), push c + a*b. 3→1.
HANDLER(op_fused_fmul_fadd_f) {
    f0 = f2 + f1 * f0;
    f1 = f3;
    f2 = *--fsp;
    f3 = *--fsp;
    NEXT();
}
HANDLER(op_fused_fmul_fsub_f) {
    f0 = f2 - f1 * f0;
    f1 = f3;
    f2 = *--fsp;
    f3 = *--fsp;
    NEXT();
}
HANDLER(op_fused_fmul_fadd_set_f) {
    *(float*)((uint8_t*)locals + pc->imm[0]) = f2 + f1 * f0;
    f0 = f3;
    f1 = *--fsp;
    f2 = *--fsp;
    f3 = *--fsp;
    NEXT();
}
HANDLER(op_fused_fmul_fsub_set_f) {
    *(float*)((uint8_t*)locals + pc->imm[0]) = f2 - f1 * f0;
    f0 = f3;
    f1 = *--fsp;
    f2 = *--fsp;
    f3 = *--fsp;
    NEXT();
}
HANDLER(op_fused_get_get_fmul_fadd_f) {
    float a = *(float*)((uint8_t*)locals + pc->imm[0]);
    float b = *(float*)((uint8_t*)locals + pc->imm[1]);
    f0 = f0 + a * b;
    NEXT();
}
HANDLER(op_fused_get_get_fmul_fsub_f) {
    float a = *(float*)((uint8_t*)locals + pc->imm[0]);
    float b = *(float*)((uint8_t*)locals + pc->imm[1]);
    f0 = f0 - a * b;
    NEXT();
}
#define FMUL_SUM_HANDLER(name, TERMS) \
HANDLER(name) { \
    uint8_t sub_mask = (uint8_t)pc->imm[2]; \
    float acc = 0.0f; \
    for (int i = 0; i < (TERMS); i++) { \
        uint8_t a_idx = imm_u8(pc, i * 2); \
        uint8_t b_idx = imm_u8(pc, i * 2 + 1); \
        float a = *(float*)((uint8_t*)locals + (size_t)a_idx * 8); \
        float b = *(float*)((uint8_t*)locals + (size_t)b_idx * 8); \
        float prod = a * b; \
        acc = (sub_mask & (1u << i)) ? (acc - prod) : (acc + prod); \
    } \
    FPUSH(acc); \
    NEXT(); \
}
FMUL_SUM_HANDLER(op_fused_get_get_fmul_sum2_f, 2)
FMUL_SUM_HANDLER(op_fused_get_get_fmul_sum3_f, 3)
FMUL_SUM_HANDLER(op_fused_get_get_fmul_sum4_f, 4)
FMUL_SUM_HANDLER(op_fused_get_get_fmul_sum5_f, 5)
FMUL_SUM_HANDLER(op_fused_get_get_fmul_sum6_f, 6)
FMUL_SUM_HANDLER(op_fused_get_get_fmul_sum7_f, 7)
FMUL_SUM_HANDLER(op_fused_get_get_fmul_sum8_f, 8)
#undef FMUL_SUM_HANDLER
HANDLER(op_fused_get_addr_fmul_fadd_f) {
    float coeff = *(float*)((uint8_t*)locals + pc->imm[0]);
    float state = *(float*)((uint8_t*)locals + pc->imm[1] * 8 + (int32_t)pc->imm[2]);
    f0 = f0 + coeff * state;
    NEXT();
}
HANDLER(op_fused_get_addr_fmul_fsub_f) {
    float coeff = *(float*)((uint8_t*)locals + pc->imm[0]);
    float state = *(float*)((uint8_t*)locals + pc->imm[1] * 8 + (int32_t)pc->imm[2]);
    f0 = f0 - coeff * state;
    NEXT();
}
HANDLER(op_fused_addr_load32off_f) {
    float v = *(float*)((uint8_t*)locals + pc->imm[0] * 8 + (int32_t)pc->imm[1]);
    FPUSH(v);
    NEXT();
}
HANDLER(op_fused_addr_get_sload32_f) {
    uint8_t* fat = (uint8_t*)(locals + pc->imm[0]);
    int64_t idx = (int64_t)locals[pc->imm[1]];
    uint8_t* data = load_ptr_unaligned(fat);
    float v = load_f32_unaligned(data + idx * 4);
    FPUSH(v);
    NEXT();
}
HANDLER(op_fused_addr_get_sstore32_f) {
    uint8_t* fat = (uint8_t*)(locals + pc->imm[0]);
    int64_t idx = (int64_t)locals[pc->imm[1]];
    uint8_t* data = load_ptr_unaligned(fat);
    store_f32_unaligned(data + idx * 4, f0);
    FDROP1();
    NEXT();
}
// Tee + slice store (float window, FFT butterfly hot op):
//   imm[0] = *byte offset* into locals[] for tee target (pre-shifted),
//   imm[1] = slot of the slice's fat pointer (u64 index),
//   imm[2] = local holding the element index (u64 index).
// Stores f0 to locals[tee] (low 32 bits) and to slice[idx], then
// pops f0. Both stores go through `str s, [...]` with no FP→GPR
// crossing. See encode_imm in the bridge for the byte-offset rationale.
HANDLER(op_fused_tee_sstore32_f) {
    *(float*)((uint8_t*)locals + pc->imm[0]) = f0;
    uint8_t* fat = (uint8_t*)(locals + pc->imm[1]);
    int64_t idx = (int64_t)locals[pc->imm[2]];
    uint8_t* data = load_ptr_unaligned(fat);
    store_f32_unaligned(data + idx * 4, f0);
    FDROP1();
    NEXT();
}
HANDLER(op_fused_local_array_load32_f) {
    uint8_t* base = (uint8_t*)(locals + pc->imm[0]);
    int64_t idx = (int64_t)locals[pc->imm[1]];
    float v = *(float*)(base + idx * 4);
    FPUSH(v);
    NEXT();
}
HANDLER(op_fused_local_array_store32_f) {
    uint8_t* base = (uint8_t*)(locals + pc->imm[0]);
    int64_t idx = (int64_t)locals[pc->imm[1]];
    *(float*)(base + idx * 4) = f0;
    FDROP1();
    NEXT();
}
HANDLER(op_fused_f32const_fgt_jiz_f) {
    float val = f0;
    uint32_t lim_bits = (uint32_t)pc->imm[0];
    float limit;
    memcpy(&limit, &lim_bits, 4);
    FDROP1();
    if (!(val > limit)) {
        int64_t off = (int64_t)pc->imm[1];
        pc = pc + 1 + off;
        DISPATCH();
    }
    NEXT();
}
HANDLER(op_fused_get_f32const_fgt_jiz_f) {
    float val = *(float*)((uint8_t*)locals + pc->imm[0]);
    uint32_t lim_bits = (uint32_t)pc->imm[1];
    float limit;
    memcpy(&limit, &lim_bits, 4);
    if (!(val > limit)) {
        int64_t off = (int64_t)pc->imm[2];
        pc = pc + 1 + off;
        DISPATCH();
    }
    NEXT();
}

// ============================================================================
// Entry point
// ============================================================================

// Realtime-safe entry point: performs no allocation. The caller must
// pre-populate ctx->stack_base (operand stack), ctx->frame_stack /
// frame_stack_cap (unified locals+local-memory bump buffer), and
// ctx->call_stack / call_stack_cap before calling. The frame stack
// size is reset here; done/result/error are cleared so a single ctx
// can be reused across invocations.
int64_t stack_interp_run(Ctx* ctx, uint32_t entry_func) {
    ctx->done = 0;
    ctx->result = 0;
    ctx->error = NULL;
    ctx->frame_stack_size = 0;
    ctx->call_depth = 0;

    // Enter entry function. enter_function writes the new frame pointer
    // into entry_locals via the out-parameter.
    uint64_t* entry_locals;
    if (!enter_function(ctx, entry_func, &entry_locals)) {
        return ctx->result; // ctx->error already set.
    }

    // Start dispatch with both TOS windows zeroed. Preload the handler
    // for the second instruction as nh. `locals` and `fsp` are both
    // handler arguments pinned to GPRs by preserve_none on both aarch64
    // and x86-64.
    Instruction* pc = ctx->functions[entry_func].code;
    Handler initial_nh = (Handler)(pc + 1)->handler;
    ((Handler)pc->handler)(ctx, pc, ctx->stack_base, ctx->float_stack, entry_locals, 0, 0, 0, 0, 0.0f, 0.0f, 0.0f, 0.0f, initial_nh);

    return ctx->result;
}
