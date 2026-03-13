// =============================================================================
// ARM64 assembly VM interpreter for Lyte
// =============================================================================
//
// Register pinning (callee-saved, survive calls to libm/helpers):
//   x19 = ops base pointer (PackedOp array, each 4 bytes)
//   x20 = regs base pointer (u64[256])
//   x21 = ip (instruction index into ops array)
//   x22 = locals_base (byte offset)
//   x23 = locals_ptr (base of locals memory)
//   x24 = ctx (AsmContext pointer)
//   x25 = jump table base address
//   x26 = globals_ptr
//   x27 = scratch (callee-saved, survives calls)
//   x28 = scratch (callee-saved, survives calls)
//
// Packed instruction format (4 bytes, little-endian):
//   ABC: [tag:8][A:8][B:8][C:8]
//   AD:  [tag:8][A:8][D:i16]
//
// After dispatch loads instruction into w16:
//   tag = w16 & 0xFF
//   A   = (w16 >> 8) & 0xFF
//   B   = (w16 >> 16) & 0xFF
//   C   = (w16 >> 24) & 0xFF
//   D   = (w16 arithmetic >> 16) as i16
//   Du  = (w16 >> 16) as u16

// === AsmContext field offsets (must match Rust #[repr(C)] struct) ===
.set CTX_OPS,             0
.set CTX_REGS,            8
.set CTX_IP,              16
.set CTX_LOCALS_BASE,     24
.set CTX_LOCALS_PTR,      32
.set CTX_GLOBALS_PTR,     40
.set CTX_FUNC_OFFSETS,    48
.set CTX_FUNC_LOCALS,     56
.set CTX_WIDE_I64,        64
.set CTX_WIDE_F64,        72
.set CTX_F32_POOL,        80
.set CTX_CONSTANTS,       88
.set CTX_CANCELLED_PTR,   96
.set CTX_CLOSURE_PTR,     104
.set CTX_CALL_STACK,      112
.set CTX_CALL_STACK_LEN,  120
.set CTX_CURRENT_FUNC,    128
.set CTX_LOCALS_CAP,      136
.set CTX_FN_PRINT_I32,    144
.set CTX_FN_PRINT_F32,    152
.set CTX_FN_ASSERT,       160
.set CTX_FN_PUTC,         168
.set CTX_FN_GROW_LOCALS,  176

// AsmCallFrame: 24 bytes each
.set FRAME_SIZE,          24
.set FRAME_FUNC_IDX,      0
.set FRAME_IP,            8
.set FRAME_LOCALS_BASE,   16

// =============================================================================
// Macros for common opcode patterns
// =============================================================================

// Extract A, B, C fields from instruction in w16
.macro extract_a dst
    ubfx    \dst, w16, #8, #8
.endm
.macro extract_b dst
    ubfx    \dst, w16, #16, #8
.endm
.macro extract_c dst
    lsr     \dst, w16, #24
.endm
// D field as signed i16 (sign-extended to 64-bit)
.macro extract_d dst
    sbfx    \dst, x16, #16, #16
.endm
// D field as unsigned u16
.macro extract_du dst
    ubfx    \dst, w16, #16, #16
.endm

// Integer ABC arithmetic: regs[A] = regs[B] <op> regs[C]
.macro int_abc_op op
    extract_a w0
    extract_b w1
    extract_c w2
    ldr     x3, [x20, x1, lsl #3]
    ldr     x4, [x20, x2, lsl #3]
    \op     x3, x3, x4
    str     x3, [x20, x0, lsl #3]
    b       Ldispatch
.endm

// Float32 ABC arithmetic: regs[A] = f32(regs[B]) <op> f32(regs[C])
// ARM64 only allows lsl #2 for 32-bit loads, but regs are 8-byte slots,
// so we compute the address manually.
.macro f32_abc_op op
    extract_a w0
    extract_b w1
    extract_c w2
    add     x3, x20, x1, lsl #3
    ldr     s0, [x3]
    add     x3, x20, x2, lsl #3
    ldr     s1, [x3]
    \op     s0, s0, s1
    fmov    w3, s0
    str     x3, [x20, x0, lsl #3]
    b       Ldispatch
.endm

// Float64 ABC arithmetic: regs[A] = f64(regs[B]) <op> f64(regs[C])
.macro f64_abc_op op
    extract_a w0
    extract_b w1
    extract_c w2
    ldr     d0, [x20, x1, lsl #3]
    ldr     d1, [x20, x2, lsl #3]
    \op     d0, d0, d1
    str     d0, [x20, x0, lsl #3]
    b       Ldispatch
.endm

// Integer ABC comparison: regs[A] = (regs[B] <cond> regs[C]) ? 1 : 0
.macro int_cmp_abc cond
    extract_a w0
    extract_b w1
    extract_c w2
    ldr     x3, [x20, x1, lsl #3]
    ldr     x4, [x20, x2, lsl #3]
    cmp     x3, x4
    cset    x3, \cond
    str     x3, [x20, x0, lsl #3]
    b       Ldispatch
.endm

// Float32 ABC comparison: regs[A] = (f32(regs[B]) <cond> f32(regs[C])) ? 1 : 0
.macro f32_cmp_abc cond
    extract_a w0
    extract_b w1
    extract_c w2
    add     x3, x20, x1, lsl #3
    ldr     s0, [x3]
    add     x3, x20, x2, lsl #3
    ldr     s1, [x3]
    fcmp    s0, s1
    cset    x3, \cond
    str     x3, [x20, x0, lsl #3]
    b       Ldispatch
.endm

// Float64 ABC comparison
.macro f64_cmp_abc cond
    extract_a w0
    extract_b w1
    extract_c w2
    ldr     d0, [x20, x1, lsl #3]
    ldr     d1, [x20, x2, lsl #3]
    fcmp    d0, d1
    cset    x3, \cond
    str     x3, [x20, x0, lsl #3]
    b       Ldispatch
.endm

// Float32 unary math (calls libm): regs[A] = func(f32(regs[B]))
// Uses x27 (callee-saved) to hold A across the call.
.macro f32_unary_math func
    extract_a w27
    extract_b w1
    add     x2, x20, x1, lsl #3
    ldr     s0, [x2]
    bl      \func
    fmov    w0, s0
    str     x0, [x20, x27, lsl #3]
    b       Ldispatch
.endm

// Float64 unary math: regs[A] = func(f64(regs[B]))
.macro f64_unary_math func
    extract_a w27
    extract_b w1
    ldr     d0, [x20, x1, lsl #3]
    bl      \func
    str     d0, [x20, x27, lsl #3]
    b       Ldispatch
.endm

// Float32 binary math: regs[A] = func(f32(regs[B]), f32(regs[C]))
.macro f32_binary_math func
    extract_a w27
    extract_b w1
    extract_c w2
    add     x3, x20, x1, lsl #3
    ldr     s0, [x3]
    add     x3, x20, x2, lsl #3
    ldr     s1, [x3]
    bl      \func
    fmov    w0, s0
    str     x0, [x20, x27, lsl #3]
    b       Ldispatch
.endm

// Float64 binary math: regs[A] = func(f64(regs[B]), f64(regs[C]))
.macro f64_binary_math func
    extract_a w27
    extract_b w1
    extract_c w2
    ldr     d0, [x20, x1, lsl #3]
    ldr     d1, [x20, x2, lsl #3]
    bl      \func
    str     d0, [x20, x27, lsl #3]
    b       Ldispatch
.endm

// =============================================================================
// Entry point
// =============================================================================

.text
.p2align 4
.globl _vm_arm64_enter
_vm_arm64_enter:
    // Save callee-saved registers (96 bytes frame)
    stp     x29, x30, [sp, #-96]!
    mov     x29, sp
    stp     x19, x20, [sp, #16]
    stp     x21, x22, [sp, #32]
    stp     x23, x24, [sp, #48]
    stp     x25, x26, [sp, #64]
    stp     x27, x28, [sp, #80]

    // x0 = AsmContext pointer
    mov     x24, x0

    // Load pinned registers from context
    ldr     x19, [x24, #CTX_OPS]
    ldr     x20, [x24, #CTX_REGS]
    ldr     x21, [x24, #CTX_IP]
    ldr     x22, [x24, #CTX_LOCALS_BASE]
    ldr     x23, [x24, #CTX_LOCALS_PTR]
    ldr     x26, [x24, #CTX_GLOBALS_PTR]

    // Load jump table address
    adrp    x25, Ljump_table@PAGE
    add     x25, x25, Ljump_table@PAGEOFF

    // Fall through to dispatch

// =============================================================================
// Dispatch loop — the hot path
// =============================================================================
.p2align 4
Ldispatch:
    ldr     w16, [x19, x21, lsl #2]    // w16 = ops[ip] (4-byte packed instruction)
    add     x21, x21, #1               // ip++
    and     w17, w16, #0xff            // w17 = tag
    ldr     x17, [x25, x17, lsl #3]   // x17 = jump_table[tag]
    br      x17                        // branch to handler

// =============================================================================
// Exit path
// =============================================================================
Lexit:
    // x0 has return value
    ldp     x19, x20, [sp, #16]
    ldp     x21, x22, [sp, #32]
    ldp     x23, x24, [sp, #48]
    ldp     x25, x26, [sp, #64]
    ldp     x27, x28, [sp, #80]
    ldp     x29, x30, [sp], #96
    ret

Lcancelled:
    mov     x0, #0
    b       Lexit

// =============================================================================
// 0: NOP
// =============================================================================
Lop_nop:
    b       Ldispatch

// =============================================================================
// 1: HALT — return regs[0]
// =============================================================================
Lop_halt:
    ldr     x0, [x20]
    b       Lexit

// =============================================================================
// 2: MOVE — regs[A] = regs[B]
// =============================================================================
Lop_move:
    extract_a w0
    extract_b w1
    ldr     x2, [x20, x1, lsl #3]
    str     x2, [x20, x0, lsl #3]
    b       Ldispatch

// =============================================================================
// 3: LOAD_IMM — regs[A] = sign_extend(D)
// =============================================================================
Lop_load_imm:
    extract_a w0
    extract_d x1
    str     x1, [x20, x0, lsl #3]
    b       Ldispatch

// =============================================================================
// 4: LOAD_F32 — regs[A] = f32_pool[Du]
// =============================================================================
Lop_load_f32:
    extract_a w0
    extract_du w1
    ldr     x2, [x24, #CTX_F32_POOL]
    ldr     s0, [x2, x1, lsl #2]       // f32_pool[Du]
    fmov    w3, s0
    str     x3, [x20, x0, lsl #3]      // zero-extended
    b       Ldispatch

// =============================================================================
// 5: LOAD_CONST — regs[A] = constants[Du]
// =============================================================================
Lop_load_const:
    extract_a w0
    extract_du w1
    ldr     x2, [x24, #CTX_CONSTANTS]
    ldr     x3, [x2, x1, lsl #3]
    str     x3, [x20, x0, lsl #3]
    b       Ldispatch

// =============================================================================
// 6-13: Integer arithmetic
// =============================================================================
Lop_iadd:
    int_abc_op add
Lop_isub:
    int_abc_op sub
Lop_imul:
    int_abc_op mul

Lop_idiv:
    extract_a w0
    extract_b w1
    extract_c w2
    ldr     x3, [x20, x1, lsl #3]
    ldr     x4, [x20, x2, lsl #3]
    cbz     x4, Ldiv_by_zero
    sdiv    x3, x3, x4
    str     x3, [x20, x0, lsl #3]
    b       Ldispatch

Lop_udiv:
    extract_a w0
    extract_b w1
    extract_c w2
    ldr     x3, [x20, x1, lsl #3]
    ldr     x4, [x20, x2, lsl #3]
    cbz     x4, Ldiv_by_zero
    udiv    x3, x3, x4
    str     x3, [x20, x0, lsl #3]
    b       Ldispatch

Lop_irem:
    extract_a w0
    extract_b w1
    extract_c w2
    ldr     x3, [x20, x1, lsl #3]
    ldr     x4, [x20, x2, lsl #3]
    cbz     x4, Ldiv_by_zero
    sdiv    x5, x3, x4
    msub    x3, x5, x4, x3             // rem = a - (a/b)*b
    str     x3, [x20, x0, lsl #3]
    b       Ldispatch

Lop_ipow:
    extract_a w0
    extract_b w1
    extract_c w2
    ldr     x3, [x20, x1, lsl #3]      // base
    ldr     x4, [x20, x2, lsl #3]      // exp (as u32)
    mov     w4, w4                      // truncate to u32
    mov     x5, #1                      // result = 1
    cbz     w4, Lipow_done
Lipow_loop:
    tst     w4, #1
    b.eq    Lipow_skip
    mul     x5, x5, x3
Lipow_skip:
    mul     x3, x3, x3
    lsr     w4, w4, #1
    cbnz    w4, Lipow_loop
Lipow_done:
    str     x5, [x20, x0, lsl #3]
    b       Ldispatch

Lop_ineg:
    extract_a w0
    extract_b w1
    ldr     x2, [x20, x1, lsl #3]
    neg     x2, x2
    str     x2, [x20, x0, lsl #3]
    b       Ldispatch

// 14: IADD_IMM — regs[A] = regs[B] + sign_extend(C as i8)
Lop_iadd_imm:
    extract_a w0
    extract_b w1
    sbfx    x2, x16, #24, #8           // C as signed i8, sign-extended
    ldr     x3, [x20, x1, lsl #3]
    add     x3, x3, x2
    str     x3, [x20, x0, lsl #3]
    b       Ldispatch

Ldiv_by_zero:
    brk     #1                          // trap: division by zero

// =============================================================================
// 15-20: Float32 arithmetic
// =============================================================================
Lop_fadd:
    f32_abc_op fadd
Lop_fsub:
    f32_abc_op fsub
Lop_fmul:
    f32_abc_op fmul
Lop_fdiv:
    f32_abc_op fdiv

Lop_fneg:
    extract_a w0
    extract_b w1
    add     x2, x20, x1, lsl #3
    ldr     s0, [x2]
    fneg    s0, s0
    fmov    w3, s0
    str     x3, [x20, x0, lsl #3]
    b       Ldispatch

Lop_fpow:
    f32_binary_math _powf

// =============================================================================
// 21-26: Float64 arithmetic
// =============================================================================
Lop_dadd:
    f64_abc_op fadd
Lop_dsub:
    f64_abc_op fsub
Lop_dmul:
    f64_abc_op fmul
Lop_ddiv:
    f64_abc_op fdiv

Lop_dneg:
    extract_a w0
    extract_b w1
    ldr     d0, [x20, x1, lsl #3]
    fneg    d0, d0
    str     d0, [x20, x0, lsl #3]
    b       Ldispatch

Lop_dpow:
    f64_binary_math _pow

// =============================================================================
// 27-33: Bitwise operations
// =============================================================================
Lop_and:
    int_abc_op and
Lop_or:
    int_abc_op orr
Lop_xor:
    int_abc_op eor

Lop_not:
    extract_a w0
    extract_b w1
    ldr     x2, [x20, x1, lsl #3]
    mvn     x2, x2
    str     x2, [x20, x0, lsl #3]
    b       Ldispatch

Lop_shl:
    extract_a w0
    extract_b w1
    extract_c w2
    ldr     x3, [x20, x1, lsl #3]
    ldr     x4, [x20, x2, lsl #3]
    and     x4, x4, #63
    lsl     x3, x3, x4
    str     x3, [x20, x0, lsl #3]
    b       Ldispatch

Lop_shr:
    extract_a w0
    extract_b w1
    extract_c w2
    ldr     x3, [x20, x1, lsl #3]
    ldr     x4, [x20, x2, lsl #3]
    and     x4, x4, #63
    asr     x3, x3, x4                 // arithmetic shift (signed)
    str     x3, [x20, x0, lsl #3]
    b       Ldispatch

Lop_ushr:
    extract_a w0
    extract_b w1
    extract_c w2
    ldr     x3, [x20, x1, lsl #3]
    ldr     x4, [x20, x2, lsl #3]
    and     x4, x4, #63
    lsr     x3, x3, x4                 // logical shift (unsigned)
    str     x3, [x20, x0, lsl #3]
    b       Ldispatch

// =============================================================================
// 34-38: Integer comparisons
// =============================================================================
Lop_ieq:
    int_cmp_abc eq
Lop_ine:
    int_cmp_abc ne
Lop_ilt:
    int_cmp_abc lt                      // signed less-than
Lop_ile:
    int_cmp_abc le                      // signed less-or-equal
Lop_ult:
    int_cmp_abc lo                      // unsigned less-than

// =============================================================================
// 39-42: Float32 comparisons
// =============================================================================
Lop_feq:
    f32_cmp_abc eq
Lop_fne:
    f32_cmp_abc ne
Lop_flt:
    f32_cmp_abc mi                      // ordered less-than
Lop_fle:
    f32_cmp_abc ls                      // ordered less-or-equal

// =============================================================================
// 43-44: Memory comparisons (extended: ABC + data word for size)
// =============================================================================
Lop_mem_eq:
    extract_a w0
    extract_b w1
    extract_c w2
    ldr     x3, [x20, x1, lsl #3]      // ptr_a
    ldr     x4, [x20, x2, lsl #3]      // ptr_b
    ldr     w5, [x19, x21, lsl #2]     // size from next word
    add     x21, x21, #1
    cbz     w5, Lmem_eq_true
    mov     w6, #0
Lmem_eq_loop:
    ldrb    w7, [x3, x6]
    ldrb    w8, [x4, x6]
    cmp     w7, w8
    b.ne    Lmem_eq_false
    add     w6, w6, #1
    cmp     w6, w5
    b.lt    Lmem_eq_loop
Lmem_eq_true:
    mov     x3, #1
    str     x3, [x20, x0, lsl #3]
    b       Ldispatch
Lmem_eq_false:
    str     xzr, [x20, x0, lsl #3]
    b       Ldispatch

Lop_mem_ne:
    extract_a w0
    extract_b w1
    extract_c w2
    ldr     x3, [x20, x1, lsl #3]
    ldr     x4, [x20, x2, lsl #3]
    ldr     w5, [x19, x21, lsl #2]
    add     x21, x21, #1
    cbz     w5, Lmem_ne_equal
    mov     w6, #0
Lmem_ne_loop:
    ldrb    w7, [x3, x6]
    ldrb    w8, [x4, x6]
    cmp     w7, w8
    b.ne    Lmem_ne_diff
    add     w6, w6, #1
    cmp     w6, w5
    b.lt    Lmem_ne_loop
Lmem_ne_equal:
    str     xzr, [x20, x0, lsl #3]
    b       Ldispatch
Lmem_ne_diff:
    mov     x3, #1
    str     x3, [x20, x0, lsl #3]
    b       Ldispatch

// =============================================================================
// 45-47: Float64 comparisons
// =============================================================================
Lop_deq:
    f64_cmp_abc eq
Lop_dlt:
    f64_cmp_abc mi
Lop_dle:
    f64_cmp_abc ls

// =============================================================================
// 48-53: Type conversions
// =============================================================================
Lop_i32_to_f32:
    extract_a w0
    extract_b w1
    ldr     x2, [x20, x1, lsl #3]
    scvtf   s0, w2                      // i32 -> f32
    fmov    w3, s0
    str     x3, [x20, x0, lsl #3]
    b       Ldispatch

Lop_f32_to_i32:
    extract_a w0
    extract_b w1
    add     x3, x20, x1, lsl #3
    ldr     s0, [x3]
    fcvtzs  w2, s0                      // f32 -> i32 (truncate)
    sxtw    x2, w2                      // sign-extend to i64
    str     x2, [x20, x0, lsl #3]
    b       Ldispatch

Lop_i32_to_f64:
    extract_a w0
    extract_b w1
    ldr     x2, [x20, x1, lsl #3]
    scvtf   d0, w2                      // i32 -> f64
    str     d0, [x20, x0, lsl #3]
    b       Ldispatch

Lop_f64_to_i32:
    extract_a w0
    extract_b w1
    ldr     d0, [x20, x1, lsl #3]
    fcvtzs  w2, d0                      // f64 -> i32 (truncate)
    sxtw    x2, w2                      // sign-extend to i64
    str     x2, [x20, x0, lsl #3]
    b       Ldispatch

Lop_f32_to_f64:
    extract_a w0
    extract_b w1
    add     x3, x20, x1, lsl #3
    ldr     s0, [x3]
    fcvt    d0, s0                      // f32 -> f64
    str     d0, [x20, x0, lsl #3]
    b       Ldispatch

Lop_f64_to_f32:
    extract_a w0
    extract_b w1
    ldr     d0, [x20, x1, lsl #3]
    fcvt    s0, d0                      // f64 -> f32
    fmov    w3, s0
    str     x3, [x20, x0, lsl #3]
    b       Ldispatch

// =============================================================================
// 54-58: Memory loads
// =============================================================================
Lop_load8:
    extract_a w0
    extract_b w1
    ldr     x2, [x20, x1, lsl #3]      // ptr
    ldrb    w3, [x2]                    // load byte (zero-extends to x3)
    str     x3, [x20, x0, lsl #3]
    b       Ldispatch

Lop_load32:
    extract_a w0
    extract_b w1
    ldr     x2, [x20, x1, lsl #3]
    ldrsw   x3, [x2]                   // load i32, sign-extend to i64
    str     x3, [x20, x0, lsl #3]
    b       Ldispatch

Lop_load64:
    extract_a w0
    extract_b w1
    ldr     x2, [x20, x1, lsl #3]
    ldr     x3, [x2]
    str     x3, [x20, x0, lsl #3]
    b       Ldispatch

Lop_load32_off:
    extract_a w0
    extract_b w1
    extract_c w2
    ldr     x3, [x20, x1, lsl #3]      // base ptr
    add     x3, x3, x2                 // base + offset (u8)
    ldrsw   x4, [x3]
    str     x4, [x20, x0, lsl #3]
    b       Ldispatch

Lop_load64_off:
    extract_a w0
    extract_b w1
    extract_c w2
    ldr     x3, [x20, x1, lsl #3]
    add     x3, x3, x2
    ldr     x4, [x3]
    str     x4, [x20, x0, lsl #3]
    b       Ldispatch

// =============================================================================
// 59-64: Memory stores
// =============================================================================
Lop_store8:
    extract_a w0
    extract_b w1
    ldr     x2, [x20, x0, lsl #3]      // ptr (in A for stores)
    ldr     x3, [x20, x1, lsl #3]      // value
    strb    w3, [x2]
    b       Ldispatch

Lop_store32:
    extract_a w0
    extract_b w1
    ldr     x2, [x20, x0, lsl #3]
    ldr     x3, [x20, x1, lsl #3]
    str     w3, [x2]
    b       Ldispatch

Lop_store64:
    extract_a w0
    extract_b w1
    ldr     x2, [x20, x0, lsl #3]
    ldr     x3, [x20, x1, lsl #3]
    str     x3, [x2]
    b       Ldispatch

Lop_store8_off:
    extract_a w0
    extract_b w1
    extract_c w2
    ldr     x3, [x20, x0, lsl #3]      // base ptr
    ldr     x4, [x20, x1, lsl #3]      // value
    strb    w4, [x3, x2]
    b       Ldispatch

Lop_store32_off:
    extract_a w0
    extract_b w1
    extract_c w2
    ldr     x3, [x20, x0, lsl #3]
    ldr     x4, [x20, x1, lsl #3]
    str     w4, [x3, x2]
    b       Ldispatch

Lop_store64_off:
    extract_a w0
    extract_b w1
    extract_c w2
    ldr     x3, [x20, x0, lsl #3]
    ldr     x4, [x20, x1, lsl #3]
    str     x4, [x3, x2]
    b       Ldispatch

// =============================================================================
// 65: LOCAL_ADDR — regs[A] = locals_ptr + locals_base + Du*8
// =============================================================================
Lop_local_addr:
    extract_a w0
    extract_du w1
    add     x2, x23, x22               // locals_ptr + locals_base
    add     x2, x2, x1, lsl #3         // + slot * 8
    str     x2, [x20, x0, lsl #3]
    b       Ldispatch

// =============================================================================
// 66: GLOBAL_ADDR — regs[A] = globals_ptr + Du
// =============================================================================
Lop_global_addr:
    extract_a w0
    extract_du w1
    add     x2, x26, x1                // globals_ptr + offset
    str     x2, [x20, x0, lsl #3]
    b       Ldispatch

// =============================================================================
// 67-71: Control flow (jumps)
// =============================================================================
Lop_jump:
    extract_d x0                        // signed offset
    add     x21, x21, x0               // ip += offset
    tbnz    x0, #63, Lcheck_cancel     // backward jump? check cancel
    b       Ldispatch

Lop_jump_if_zero:
    extract_a w0
    ldr     x1, [x20, x0, lsl #3]
    cbnz    x1, Ldispatch               // not zero -> skip jump
    extract_d x0
    add     x21, x21, x0
    tbnz    x0, #63, Lcheck_cancel
    b       Ldispatch

Lop_jump_if_not_zero:
    extract_a w0
    ldr     x1, [x20, x0, lsl #3]
    cbz     x1, Ldispatch               // zero -> skip jump
    extract_d x0
    add     x21, x21, x0
    tbnz    x0, #63, Lcheck_cancel
    b       Ldispatch

// ILT_JUMP: jump if regs[A] >= regs[B] (i.e., NOT less-than)
Lop_ilt_jump:
    extract_a w0
    extract_b w1
    ldr     x2, [x20, x0, lsl #3]
    ldr     x3, [x20, x1, lsl #3]
    cmp     x2, x3
    b.lt    Ldispatch                   // if a < b, don't jump
    sbfx    x0, x16, #24, #8           // C as signed i8 offset
    add     x21, x21, x0
    tbnz    x0, #63, Lcheck_cancel
    b       Ldispatch

// FLT_JUMP: jump if f32(regs[A]) >= f32(regs[B])
Lop_flt_jump:
    extract_a w0
    extract_b w1
    add     x2, x20, x0, lsl #3
    ldr     s0, [x2]
    add     x2, x20, x1, lsl #3
    ldr     s1, [x2]
    fcmp    s0, s1
    b.mi    Ldispatch                   // if a < b (ordered), don't jump
    sbfx    x0, x16, #24, #8           // C as signed i8 offset
    add     x21, x21, x0
    tbnz    x0, #63, Lcheck_cancel
    b       Ldispatch

Lcheck_cancel:
    ldr     x1, [x24, #CTX_CANCELLED_PTR]
    ldrb    w2, [x1]
    cbnz    w2, Lcancelled
    b       Ldispatch

// =============================================================================
// 72: CALL — A=args_start, B=arg_count, C=func_idx
// =============================================================================
Lop_call:
    extract_a w0                        // args_start
    extract_b w1                        // arg_count
    extract_c w2                        // func_idx

    // Push call frame
    ldr     x3, [x24, #CTX_CALL_STACK]
    ldr     x4, [x24, #CTX_CALL_STACK_LEN]
    mov     x5, #FRAME_SIZE
    madd    x6, x4, x5, x3             // &call_stack[len]
    ldr     x7, [x24, #CTX_CURRENT_FUNC]
    str     x7, [x6, #FRAME_FUNC_IDX]
    str     x21, [x6, #FRAME_IP]
    str     x22, [x6, #FRAME_LOCALS_BASE]
    add     x4, x4, #1
    str     x4, [x24, #CTX_CALL_STACK_LEN]

    // Copy arguments: regs[i] = regs[args_start + i]
    cbz     w1, Lcall_args_done
    cbz     w0, Lcall_args_done         // args_start == 0 means already in place
    mov     w8, #0
Lcall_copy_args:
    add     w9, w0, w8
    ldr     x10, [x20, x9, lsl #3]
    str     x10, [x20, x8, lsl #3]
    add     w8, w8, #1
    cmp     w8, w1
    b.lt    Lcall_copy_args
Lcall_args_done:

    // locals_base += func_locals[current_func]
    ldr     x5, [x24, #CTX_FUNC_LOCALS]
    ldr     w6, [x5, x7, lsl #2]       // func_locals[old current_func]
    add     x22, x22, x6

    // current_func = func_idx
    str     x2, [x24, #CTX_CURRENT_FUNC]

    // ip = func_offsets[func_idx]
    ldr     x3, [x24, #CTX_FUNC_OFFSETS]
    ldr     x21, [x3, x2, lsl #3]

    // Check if locals need growing
    ldr     w7, [x5, x2, lsl #2]       // func_locals[new func]
    add     x8, x22, x7
    ldr     x9, [x24, #CTX_LOCALS_CAP]
    cmp     x8, x9
    b.hi    Lcall_grow
    b       Ldispatch

Lcall_grow:
    str     x21, [x24, #CTX_IP]
    str     x22, [x24, #CTX_LOCALS_BASE]
    mov     x0, x24
    mov     x1, x8
    ldr     x2, [x24, #CTX_FN_GROW_LOCALS]
    blr     x2
    ldr     x23, [x24, #CTX_LOCALS_PTR]
    ldr     x9, [x24, #CTX_LOCALS_CAP]
    b       Ldispatch

// =============================================================================
// 73: CALL_INDIRECT — A=func_reg, B=args_start, C=arg_count
// =============================================================================
Lop_call_indirect:
    extract_a w0                        // func_reg
    extract_b w1                        // args_start
    extract_c w2                        // arg_count
    ldr     x0, [x20, x0, lsl #3]      // func_idx = regs[func_reg]

    // Push call frame
    ldr     x3, [x24, #CTX_CALL_STACK]
    ldr     x4, [x24, #CTX_CALL_STACK_LEN]
    mov     x5, #FRAME_SIZE
    madd    x6, x4, x5, x3
    ldr     x7, [x24, #CTX_CURRENT_FUNC]
    str     x7, [x6, #FRAME_FUNC_IDX]
    str     x21, [x6, #FRAME_IP]
    str     x22, [x6, #FRAME_LOCALS_BASE]
    add     x4, x4, #1
    str     x4, [x24, #CTX_CALL_STACK_LEN]

    // Copy arguments
    cbz     w2, Lcalli_args_done
    cbz     w1, Lcalli_args_done
    mov     w8, #0
Lcalli_copy:
    add     w9, w1, w8
    ldr     x10, [x20, x9, lsl #3]
    str     x10, [x20, x8, lsl #3]
    add     w8, w8, #1
    cmp     w8, w2
    b.lt    Lcalli_copy
Lcalli_args_done:

    // locals_base += func_locals[current_func]
    ldr     x5, [x24, #CTX_FUNC_LOCALS]
    ldr     w6, [x5, x7, lsl #2]
    add     x22, x22, x6

    // current_func = func_idx; ip = func_offsets[func_idx]
    str     x0, [x24, #CTX_CURRENT_FUNC]
    ldr     x3, [x24, #CTX_FUNC_OFFSETS]
    ldr     x21, [x3, x0, lsl #3]
    b       Ldispatch

// =============================================================================
// 74: RETURN
// =============================================================================
Lop_return:
    ldr     x0, [x24, #CTX_CALL_STACK_LEN]
    cbz     x0, Lreturn_exit
    sub     x0, x0, #1
    str     x0, [x24, #CTX_CALL_STACK_LEN]
    ldr     x1, [x24, #CTX_CALL_STACK]
    mov     x2, #FRAME_SIZE
    madd    x1, x0, x2, x1
    ldr     x3, [x1, #FRAME_FUNC_IDX]
    ldr     x21, [x1, #FRAME_IP]
    ldr     x22, [x1, #FRAME_LOCALS_BASE]
    str     x3, [x24, #CTX_CURRENT_FUNC]
    b       Ldispatch

Lreturn_exit:
    ldr     x0, [x20]                  // regs[0]
    b       Lexit

// =============================================================================
// 75: RETURN_REG — regs[0] = regs[A], then return
// =============================================================================
Lop_return_reg:
    extract_a w1
    ldr     x2, [x20, x1, lsl #3]
    str     x2, [x20]                  // regs[0] = regs[A]
    ldr     x0, [x24, #CTX_CALL_STACK_LEN]
    cbz     x0, Lreturn_exit
    sub     x0, x0, #1
    str     x0, [x24, #CTX_CALL_STACK_LEN]
    ldr     x1, [x24, #CTX_CALL_STACK]
    mov     x2, #FRAME_SIZE
    madd    x1, x0, x2, x1
    ldr     x3, [x1, #FRAME_FUNC_IDX]
    ldr     x21, [x1, #FRAME_IP]
    ldr     x22, [x1, #FRAME_LOCALS_BASE]
    str     x3, [x24, #CTX_CURRENT_FUNC]
    b       Ldispatch

// =============================================================================
// 76: ALLOC_LOCALS — zero locals[locals_base..locals_base+Du]
// =============================================================================
Lop_alloc_locals:
    extract_du w1                       // size in bytes
    add     x2, x22, x1                // needed = locals_base + size
    ldr     x3, [x24, #CTX_LOCALS_CAP]
    cmp     x2, x3
    b.hi    Lalloc_grow
    // Zero the memory
    add     x4, x23, x22               // dest = locals_ptr + locals_base
    cbz     w1, Ldispatch
Lalloc_zero16:
    cmp     w1, #16
    b.lt    Lalloc_zero8
    stp     xzr, xzr, [x4], #16
    sub     w1, w1, #16
    b       Lalloc_zero16
Lalloc_zero8:
    cmp     w1, #8
    b.lt    Lalloc_zero1
    str     xzr, [x4], #8
    sub     w1, w1, #8
Lalloc_zero1:
    cbz     w1, Ldispatch
Lalloc_byte:
    strb    wzr, [x4], #1
    subs    w1, w1, #1
    b.ne    Lalloc_byte
    b       Ldispatch

Lalloc_grow:
    str     x21, [x24, #CTX_IP]
    str     x22, [x24, #CTX_LOCALS_BASE]
    mov     x0, x24
    mov     x1, x2
    ldr     x2, [x24, #CTX_FN_GROW_LOCALS]
    blr     x2
    ldr     x23, [x24, #CTX_LOCALS_PTR]
    // Re-read size and redo the zero
    sub     x21, x21, #1               // back up ip to re-execute
    b       Ldispatch

// =============================================================================
// 77: MEM_COPY — copy C bytes from regs[B] to regs[A]
// =============================================================================
Lop_mem_copy:
    extract_a w0
    extract_b w1
    extract_c w2                        // size
    ldr     x3, [x20, x0, lsl #3]      // dst
    ldr     x4, [x20, x1, lsl #3]      // src
    cbz     w2, Ldispatch
Lmemcpy_loop:
    ldrb    w5, [x4], #1
    strb    w5, [x3], #1
    subs    w2, w2, #1
    b.ne    Lmemcpy_loop
    b       Ldispatch

// =============================================================================
// 78: MEM_ZERO — zero Du bytes at regs[A]
// =============================================================================
Lop_mem_zero:
    extract_a w0
    extract_du w1                       // size
    ldr     x2, [x20, x0, lsl #3]      // ptr
    cbz     w1, Ldispatch
Lmemzero_loop:
    strb    wzr, [x2], #1
    subs    w1, w1, #1
    b.ne    Lmemzero_loop
    b       Ldispatch

// =============================================================================
// 79: SAVE_REGS — save regs[A..A+B) to locals at base=locals_base+C*8
// =============================================================================
Lop_save_regs:
    extract_a w0                        // start_reg
    extract_b w1                        // count
    extract_c w2                        // slot/8
    add     x3, x23, x22               // locals_ptr + locals_base
    add     x3, x3, x2, lsl #3         // + C*8
    cbz     w1, Ldispatch
    mov     w4, #0
Lsave_loop:
    add     w5, w0, w4
    ldr     x6, [x20, x5, lsl #3]
    str     x6, [x3, x4, lsl #3]
    add     w4, w4, #1
    cmp     w4, w1
    b.lt    Lsave_loop
    b       Ldispatch

// =============================================================================
// 80: RESTORE_REGS — restore regs[A..A+B) from locals at base=locals_base+C*8
// =============================================================================
Lop_restore_regs:
    extract_a w0
    extract_b w1
    extract_c w2
    add     x3, x23, x22
    add     x3, x3, x2, lsl #3
    cbz     w1, Ldispatch
    mov     w4, #0
Lrestore_loop:
    ldr     x6, [x3, x4, lsl #3]
    add     w5, w0, w4
    str     x6, [x20, x5, lsl #3]
    add     w4, w4, #1
    cmp     w4, w1
    b.lt    Lrestore_loop
    b       Ldispatch

// =============================================================================
// 81: PRINT_I32
// =============================================================================
Lop_print_i32:
    extract_a w0
    ldr     x0, [x20, x0, lsl #3]      // val as i64
    ldr     x1, [x24, #CTX_FN_PRINT_I32]
    blr     x1
    b       Ldispatch

// =============================================================================
// 82: PRINT_F32
// =============================================================================
Lop_print_f32:
    extract_a w0
    add     x1, x20, x0, lsl #3
    ldr     w0, [x1]                    // f32 bits (low 32)
    ldr     x1, [x24, #CTX_FN_PRINT_F32]
    blr     x1
    b       Ldispatch

// =============================================================================
// 83: ASSERT
// =============================================================================
Lop_assert:
    extract_a w0
    ldr     x0, [x20, x0, lsl #3]      // val
    sub     x1, x21, #1                 // ip - 1
    ldr     x2, [x24, #CTX_FN_ASSERT]
    blr     x2
    cbnz    x0, Lassert_fail
    b       Ldispatch
Lassert_fail:
    mov     x0, #1
    b       Lexit

// =============================================================================
// 84: PUTC
// =============================================================================
Lop_putc:
    extract_a w0
    ldr     x0, [x20, x0, lsl #3]
    ldr     x1, [x24, #CTX_FN_PUTC]
    blr     x1
    b       Ldispatch

// =============================================================================
// 85-105: Float32 unary math builtins
// =============================================================================
Lop_sin_f32:
    f32_unary_math _sinf
Lop_cos_f32:
    f32_unary_math _cosf
Lop_tan_f32:
    f32_unary_math _tanf
Lop_asin_f32:
    f32_unary_math _asinf
Lop_acos_f32:
    f32_unary_math _acosf
Lop_atan_f32:
    f32_unary_math _atanf
Lop_sinh_f32:
    f32_unary_math _sinhf
Lop_cosh_f32:
    f32_unary_math _coshf
Lop_tanh_f32:
    f32_unary_math _tanhf
Lop_asinh_f32:
    f32_unary_math _asinhf
Lop_acosh_f32:
    f32_unary_math _acoshf
Lop_atanh_f32:
    f32_unary_math _atanhf
Lop_ln_f32:
    f32_unary_math _logf
Lop_exp_f32:
    f32_unary_math _expf
Lop_exp2_f32:
    f32_unary_math _exp2f
Lop_log10_f32:
    f32_unary_math _log10f
Lop_log2_f32:
    f32_unary_math _log2f

Lop_sqrt_f32:
    extract_a w0
    extract_b w1
    add     x2, x20, x1, lsl #3
    ldr     s0, [x2]
    fsqrt   s0, s0
    fmov    w3, s0
    str     x3, [x20, x0, lsl #3]
    b       Ldispatch

Lop_abs_f32:
    extract_a w0
    extract_b w1
    add     x2, x20, x1, lsl #3
    ldr     s0, [x2]
    fabs    s0, s0
    fmov    w3, s0
    str     x3, [x20, x0, lsl #3]
    b       Ldispatch

Lop_floor_f32:
    extract_a w0
    extract_b w1
    add     x2, x20, x1, lsl #3
    ldr     s0, [x2]
    frintm  s0, s0
    fmov    w3, s0
    str     x3, [x20, x0, lsl #3]
    b       Ldispatch

Lop_ceil_f32:
    extract_a w0
    extract_b w1
    add     x2, x20, x1, lsl #3
    ldr     s0, [x2]
    frintp  s0, s0
    fmov    w3, s0
    str     x3, [x20, x0, lsl #3]
    b       Ldispatch

// =============================================================================
// 106-126: Float64 unary math builtins
// =============================================================================
Lop_sin_f64:
    f64_unary_math _sin
Lop_cos_f64:
    f64_unary_math _cos
Lop_tan_f64:
    f64_unary_math _tan
Lop_asin_f64:
    f64_unary_math _asin
Lop_acos_f64:
    f64_unary_math _acos
Lop_atan_f64:
    f64_unary_math _atan
Lop_sinh_f64:
    f64_unary_math _sinh
Lop_cosh_f64:
    f64_unary_math _cosh
Lop_tanh_f64:
    f64_unary_math _tanh
Lop_asinh_f64:
    f64_unary_math _asinh
Lop_acosh_f64:
    f64_unary_math _acosh
Lop_atanh_f64:
    f64_unary_math _atanh
Lop_ln_f64:
    f64_unary_math _log
Lop_exp_f64:
    f64_unary_math _exp
Lop_exp2_f64:
    f64_unary_math _exp2
Lop_log10_f64:
    f64_unary_math _log10
Lop_log2_f64:
    f64_unary_math _log2

Lop_sqrt_f64:
    extract_a w0
    extract_b w1
    ldr     d0, [x20, x1, lsl #3]
    fsqrt   d0, d0
    str     d0, [x20, x0, lsl #3]
    b       Ldispatch

Lop_abs_f64:
    extract_a w0
    extract_b w1
    ldr     d0, [x20, x1, lsl #3]
    fabs    d0, d0
    str     d0, [x20, x0, lsl #3]
    b       Ldispatch

Lop_floor_f64:
    extract_a w0
    extract_b w1
    ldr     d0, [x20, x1, lsl #3]
    frintm  d0, d0
    str     d0, [x20, x0, lsl #3]
    b       Ldispatch

Lop_ceil_f64:
    extract_a w0
    extract_b w1
    ldr     d0, [x20, x1, lsl #3]
    frintp  d0, d0
    str     d0, [x20, x0, lsl #3]
    b       Ldispatch

// =============================================================================
// 127-130: Float predicates
// =============================================================================
Lop_isinf_f32:
    extract_a w0
    extract_b w1
    add     x3, x20, x1, lsl #3
    ldr     w2, [x3]
    and     w2, w2, #0x7FFFFFFF         // clear sign bit
    mov     w3, #0x7F800000             // +inf
    cmp     w2, w3
    cset    x4, eq
    str     x4, [x20, x0, lsl #3]
    b       Ldispatch

Lop_isinf_f64:
    extract_a w0
    extract_b w1
    ldr     x2, [x20, x1, lsl #3]
    mov     x3, #0x7FF0000000000000
    and     x2, x2, #0x7FFFFFFFFFFFFFFF
    cmp     x2, x3
    cset    x4, eq
    str     x4, [x20, x0, lsl #3]
    b       Ldispatch

Lop_isnan_f32:
    extract_a w0
    extract_b w1
    add     x2, x20, x1, lsl #3
    ldr     s0, [x2]
    fcmp    s0, s0                      // NaN != NaN
    cset    x4, ne                      // ne if unordered (NaN)
    str     x4, [x20, x0, lsl #3]
    b       Ldispatch

Lop_isnan_f64:
    extract_a w0
    extract_b w1
    ldr     d0, [x20, x1, lsl #3]
    fcmp    d0, d0
    cset    x4, ne
    str     x4, [x20, x0, lsl #3]
    b       Ldispatch

// =============================================================================
// 131-134: Float32 binary math
// =============================================================================
Lop_pow_f32:
    f32_binary_math _powf
Lop_atan2_f32:
    f32_binary_math _atan2f

Lop_min_f32:
    extract_a w0
    extract_b w1
    extract_c w2
    add     x3, x20, x1, lsl #3
    ldr     s0, [x3]
    add     x3, x20, x2, lsl #3
    ldr     s1, [x3]
    fmin    s0, s0, s1
    fmov    w3, s0
    str     x3, [x20, x0, lsl #3]
    b       Ldispatch

Lop_max_f32:
    extract_a w0
    extract_b w1
    extract_c w2
    add     x3, x20, x1, lsl #3
    ldr     s0, [x3]
    add     x3, x20, x2, lsl #3
    ldr     s1, [x3]
    fmax    s0, s0, s1
    fmov    w3, s0
    str     x3, [x20, x0, lsl #3]
    b       Ldispatch

// =============================================================================
// 135-138: Float64 binary math
// =============================================================================
Lop_pow_f64:
    f64_binary_math _pow
Lop_atan2_f64:
    f64_binary_math _atan2

Lop_min_f64:
    extract_a w0
    extract_b w1
    extract_c w2
    ldr     d0, [x20, x1, lsl #3]
    ldr     d1, [x20, x2, lsl #3]
    fmin    d0, d0, d1
    str     d0, [x20, x0, lsl #3]
    b       Ldispatch

Lop_max_f64:
    extract_a w0
    extract_b w1
    extract_c w2
    ldr     d0, [x20, x1, lsl #3]
    ldr     d1, [x20, x2, lsl #3]
    fmax    d0, d0, d1
    str     d0, [x20, x0, lsl #3]
    b       Ldispatch

// =============================================================================
// 139: LOAD_SLOT32 — fused LocalAddr + Load32
// =============================================================================
Lop_load_slot32:
    extract_a w0
    extract_du w1
    add     x2, x23, x22               // locals_ptr + locals_base
    add     x2, x2, x1, lsl #3         // + slot * 8
    ldrsw   x3, [x2]                   // load i32, sign-extend
    str     x3, [x20, x0, lsl #3]
    b       Ldispatch

// =============================================================================
// 140: STORE_SLOT32 — fused LocalAddr + Store32
// =============================================================================
Lop_store_slot32:
    extract_a w0
    extract_du w1
    add     x2, x23, x22
    add     x2, x2, x1, lsl #3
    ldr     x3, [x20, x0, lsl #3]
    str     w3, [x2]                   // store as i32
    b       Ldispatch

// =============================================================================
// 141: LOAD_IMM_WIDE — regs[A] = wide_i64[Du]
// =============================================================================
Lop_load_imm_wide:
    extract_a w0
    extract_du w1
    ldr     x2, [x24, #CTX_WIDE_I64]
    ldr     x3, [x2, x1, lsl #3]
    str     x3, [x20, x0, lsl #3]
    b       Ldispatch

// =============================================================================
// 142: LOAD_F64_WIDE — regs[A] = wide_f64[Du]
// =============================================================================
Lop_load_f64_wide:
    extract_a w0
    extract_du w1
    ldr     x2, [x24, #CTX_WIDE_F64]
    ldr     d0, [x2, x1, lsl #3]
    str     d0, [x20, x0, lsl #3]
    b       Ldispatch

// =============================================================================
// 143-144: Wide offset loads (AB + data word for offset)
// =============================================================================
Lop_load32_off_wide:
    extract_a w0
    extract_b w1
    ldr     w2, [x19, x21, lsl #2]     // offset from next word (as i32/i64)
    add     x21, x21, #1
    ldr     x3, [x20, x1, lsl #3]      // base ptr
    // The Rust code uses: (r!(op.b()) as i64 + off) as u64
    sxtw    x2, w2                      // sign-extend offset
    add     x3, x3, x2
    ldrsw   x4, [x3]
    str     x4, [x20, x0, lsl #3]
    b       Ldispatch

Lop_load64_off_wide:
    extract_a w0
    extract_b w1
    ldr     w2, [x19, x21, lsl #2]
    add     x21, x21, #1
    ldr     x3, [x20, x1, lsl #3]
    sxtw    x2, w2
    add     x3, x3, x2
    ldr     x4, [x3]
    str     x4, [x20, x0, lsl #3]
    b       Ldispatch

// =============================================================================
// 145-147: Wide offset stores
// =============================================================================
Lop_store8_off_wide:
    extract_a w0
    extract_b w1
    ldr     w2, [x19, x21, lsl #2]
    add     x21, x21, #1
    ldr     x3, [x20, x0, lsl #3]      // base ptr
    ldr     x4, [x20, x1, lsl #3]      // value
    sxtw    x2, w2
    add     x3, x3, x2
    strb    w4, [x3]
    b       Ldispatch

Lop_store32_off_wide:
    extract_a w0
    extract_b w1
    ldr     w2, [x19, x21, lsl #2]
    add     x21, x21, #1
    ldr     x3, [x20, x0, lsl #3]
    ldr     x4, [x20, x1, lsl #3]
    sxtw    x2, w2
    add     x3, x3, x2
    str     w4, [x3]
    b       Ldispatch

Lop_store64_off_wide:
    extract_a w0
    extract_b w1
    ldr     w2, [x19, x21, lsl #2]
    add     x21, x21, #1
    ldr     x3, [x20, x0, lsl #3]
    ldr     x4, [x20, x1, lsl #3]
    sxtw    x2, w2
    add     x3, x3, x2
    str     x4, [x3]
    b       Ldispatch

// =============================================================================
// 148: MEM_COPY_WIDE — copy next_word bytes from regs[B] to regs[A]
// =============================================================================
Lop_mem_copy_wide:
    extract_a w0
    extract_b w1
    ldr     w2, [x19, x21, lsl #2]     // size from next word
    add     x21, x21, #1
    ldr     x3, [x20, x0, lsl #3]      // dst
    ldr     x4, [x20, x1, lsl #3]      // src
    cbz     w2, Ldispatch
Lmcw_loop:
    ldrb    w5, [x4], #1
    strb    w5, [x3], #1
    subs    w2, w2, #1
    b.ne    Lmcw_loop
    b       Ldispatch

// =============================================================================
// 149-150: SAVE_REGS_WIDE / RESTORE_REGS_WIDE
// =============================================================================
Lop_save_regs_wide:
    extract_a w0
    extract_b w1
    ldr     w2, [x19, x21, lsl #2]     // raw byte offset from next word
    add     x21, x21, #1
    add     x3, x23, x22               // locals_ptr + locals_base
    add     x3, x3, x2                 // + raw offset
    cbz     w1, Ldispatch
    mov     w4, #0
Lsrw_loop:
    add     w5, w0, w4
    ldr     x6, [x20, x5, lsl #3]
    str     x6, [x3, x4, lsl #3]
    add     w4, w4, #1
    cmp     w4, w1
    b.lt    Lsrw_loop
    b       Ldispatch

Lop_restore_regs_wide:
    extract_a w0
    extract_b w1
    ldr     w2, [x19, x21, lsl #2]
    add     x21, x21, #1
    add     x3, x23, x22
    add     x3, x3, x2
    cbz     w1, Ldispatch
    mov     w4, #0
Lrrw_loop:
    ldr     x6, [x3, x4, lsl #3]
    add     w5, w0, w4
    str     x6, [x20, x5, lsl #3]
    add     w4, w4, #1
    cmp     w4, w1
    b.lt    Lrrw_loop
    b       Ldispatch

// =============================================================================
// 151-153: More type conversions
// =============================================================================
Lop_i32_to_i8:
    extract_a w0
    extract_b w1
    ldr     x2, [x20, x1, lsl #3]
    sxtb    x2, w2                      // sign-extend low 8 bits to 64
    str     x2, [x20, x0, lsl #3]
    b       Ldispatch

Lop_i8_to_i32:
    extract_a w0
    extract_b w1
    ldr     x2, [x20, x1, lsl #3]
    sxtb    w2, w2                      // sign-extend i8 to i32
    sxtw    x2, w2                      // sign-extend i32 to i64
    str     x2, [x20, x0, lsl #3]
    b       Ldispatch

Lop_i64_to_u32:
    extract_a w0
    extract_b w1
    ldr     x2, [x20, x1, lsl #3]
    mov     w2, w2                      // truncate to u32, zero-extend to u64
    // need sign-extend like Rust: (val as u32) as i64
    // Actually Rust: set_i64!(op.a(), (get_i64!(op.b()) as u32) as i64)
    // (val as u32) as i64: truncate to u32, then sign-extend u32 to i64? No.
    // In Rust: u32 as i64 just zero-extends (u32 fits in i64 without sign)
    // But wait: the Rust code uses set_i64 which does `($val) as u64`
    // So: (get_i64 as u32) as i64 as u64
    // get_i64 = r as i64, then as u32 truncates, then as i64 zero-extends (u32 -> i64 is always positive)
    // then as u64 is bitwise same
    // So result is just the low 32 bits, zero-extended.
    str     x2, [x20, x0, lsl #3]
    b       Ldispatch

// =============================================================================
// 154: CALL_CLOSURE — A=fat_ptr_reg, B=args_start, C=arg_count
// =============================================================================
Lop_call_closure:
    extract_a w0                        // fat_ptr_reg
    extract_b w1                        // args_start
    extract_c w2                        // arg_count
    ldr     x3, [x20, x0, lsl #3]      // fat_ptr = regs[A]
    ldr     x4, [x3]                   // func_idx = *fat_ptr
    ldr     x5, [x3, #8]              // closure_ptr = *(fat_ptr+1)
    str     x5, [x24, #CTX_CLOSURE_PTR]

    // Push call frame
    ldr     x6, [x24, #CTX_CALL_STACK]
    ldr     x7, [x24, #CTX_CALL_STACK_LEN]
    mov     x8, #FRAME_SIZE
    madd    x9, x7, x8, x6
    ldr     x10, [x24, #CTX_CURRENT_FUNC]
    str     x10, [x9, #FRAME_FUNC_IDX]
    str     x21, [x9, #FRAME_IP]
    str     x22, [x9, #FRAME_LOCALS_BASE]
    add     x7, x7, #1
    str     x7, [x24, #CTX_CALL_STACK_LEN]

    // Copy arguments
    cbz     w2, Lcloscall_args_done
    cbz     w1, Lcloscall_args_done
    mov     w8, #0
Lcloscall_copy:
    add     w9, w1, w8
    ldr     x11, [x20, x9, lsl #3]
    str     x11, [x20, x8, lsl #3]
    add     w8, w8, #1
    cmp     w8, w2
    b.lt    Lcloscall_copy
Lcloscall_args_done:

    // locals_base += func_locals[current_func]
    ldr     x5, [x24, #CTX_FUNC_LOCALS]
    ldr     w6, [x5, x10, lsl #2]
    add     x22, x22, x6

    // current_func = func_idx; ip = func_offsets[func_idx]
    str     x4, [x24, #CTX_CURRENT_FUNC]
    ldr     x3, [x24, #CTX_FUNC_OFFSETS]
    ldr     x21, [x3, x4, lsl #3]

    // Check locals capacity
    ldr     w7, [x5, x4, lsl #2]
    add     x8, x22, x7
    ldr     x9, [x24, #CTX_LOCALS_CAP]
    cmp     x8, x9
    b.hi    Lcall_grow
    b       Ldispatch

// =============================================================================
// 155: GET_CLOSURE_PTR — regs[A] = closure_ptr
// =============================================================================
Lop_get_closure_ptr:
    extract_a w0
    ldr     x1, [x24, #CTX_CLOSURE_PTR]
    str     x1, [x20, x0, lsl #3]
    b       Ldispatch

// =============================================================================
// Invalid opcode handler
// =============================================================================
Lop_invalid:
    brk     #2                          // trap: unknown opcode

// =============================================================================
// Jump table — 256 entries (tags 0-155 are valid, rest point to invalid)
// =============================================================================
.section __DATA,__const
.p2align 3
Ljump_table:
    .quad Lop_nop               // 0:  NOP
    .quad Lop_halt              // 1:  HALT
    .quad Lop_move              // 2:  MOVE
    .quad Lop_load_imm          // 3:  LOAD_IMM
    .quad Lop_load_f32          // 4:  LOAD_F32
    .quad Lop_load_const        // 5:  LOAD_CONST
    .quad Lop_iadd              // 6:  IADD
    .quad Lop_isub              // 7:  ISUB
    .quad Lop_imul              // 8:  IMUL
    .quad Lop_idiv              // 9:  IDIV
    .quad Lop_udiv              // 10: UDIV
    .quad Lop_irem              // 11: IREM
    .quad Lop_ipow              // 12: IPOW
    .quad Lop_ineg              // 13: INEG
    .quad Lop_iadd_imm          // 14: IADD_IMM
    .quad Lop_fadd              // 15: FADD
    .quad Lop_fsub              // 16: FSUB
    .quad Lop_fmul              // 17: FMUL
    .quad Lop_fdiv              // 18: FDIV
    .quad Lop_fneg              // 19: FNEG
    .quad Lop_fpow              // 20: FPOW
    .quad Lop_dadd              // 21: DADD
    .quad Lop_dsub              // 22: DSUB
    .quad Lop_dmul              // 23: DMUL
    .quad Lop_ddiv              // 24: DDIV
    .quad Lop_dneg              // 25: DNEG
    .quad Lop_dpow              // 26: DPOW
    .quad Lop_and               // 27: AND
    .quad Lop_or                // 28: OR
    .quad Lop_xor               // 29: XOR
    .quad Lop_not               // 30: NOT
    .quad Lop_shl               // 31: SHL
    .quad Lop_shr               // 32: SHR
    .quad Lop_ushr              // 33: USHR
    .quad Lop_ieq               // 34: IEQ
    .quad Lop_ine               // 35: INE
    .quad Lop_ilt               // 36: ILT
    .quad Lop_ile               // 37: ILE
    .quad Lop_ult               // 38: ULT
    .quad Lop_feq               // 39: FEQ
    .quad Lop_fne               // 40: FNE
    .quad Lop_flt               // 41: FLT
    .quad Lop_fle               // 42: FLE
    .quad Lop_mem_eq            // 43: MEM_EQ
    .quad Lop_mem_ne            // 44: MEM_NE
    .quad Lop_deq               // 45: DEQ
    .quad Lop_dlt               // 46: DLT
    .quad Lop_dle               // 47: DLE
    .quad Lop_i32_to_f32        // 48: I32_TO_F32
    .quad Lop_f32_to_i32        // 49: F32_TO_I32
    .quad Lop_i32_to_f64        // 50: I32_TO_F64
    .quad Lop_f64_to_i32        // 51: F64_TO_I32
    .quad Lop_f32_to_f64        // 52: F32_TO_F64
    .quad Lop_f64_to_f32        // 53: F64_TO_F32
    .quad Lop_load8             // 54: LOAD8
    .quad Lop_load32            // 55: LOAD32
    .quad Lop_load64            // 56: LOAD64
    .quad Lop_load32_off        // 57: LOAD32_OFF
    .quad Lop_load64_off        // 58: LOAD64_OFF
    .quad Lop_store8            // 59: STORE8
    .quad Lop_store32           // 60: STORE32
    .quad Lop_store64           // 61: STORE64
    .quad Lop_store8_off        // 62: STORE8_OFF
    .quad Lop_store32_off       // 63: STORE32_OFF
    .quad Lop_store64_off       // 64: STORE64_OFF
    .quad Lop_local_addr        // 65: LOCAL_ADDR
    .quad Lop_global_addr       // 66: GLOBAL_ADDR
    .quad Lop_jump              // 67: JUMP
    .quad Lop_jump_if_zero      // 68: JUMP_IF_ZERO
    .quad Lop_jump_if_not_zero  // 69: JUMP_IF_NOT_ZERO
    .quad Lop_ilt_jump          // 70: ILT_JUMP
    .quad Lop_flt_jump          // 71: FLT_JUMP
    .quad Lop_call              // 72: CALL
    .quad Lop_call_indirect     // 73: CALL_INDIRECT
    .quad Lop_return            // 74: RETURN
    .quad Lop_return_reg        // 75: RETURN_REG
    .quad Lop_alloc_locals      // 76: ALLOC_LOCALS
    .quad Lop_mem_copy          // 77: MEM_COPY
    .quad Lop_mem_zero          // 78: MEM_ZERO
    .quad Lop_save_regs         // 79: SAVE_REGS
    .quad Lop_restore_regs      // 80: RESTORE_REGS
    .quad Lop_print_i32         // 81: PRINT_I32
    .quad Lop_print_f32         // 82: PRINT_F32
    .quad Lop_assert            // 83: ASSERT
    .quad Lop_putc              // 84: PUTC
    .quad Lop_sin_f32           // 85: SIN_F32
    .quad Lop_cos_f32           // 86: COS_F32
    .quad Lop_tan_f32           // 87: TAN_F32
    .quad Lop_asin_f32          // 88: ASIN_F32
    .quad Lop_acos_f32          // 89: ACOS_F32
    .quad Lop_atan_f32          // 90: ATAN_F32
    .quad Lop_sinh_f32          // 91: SINH_F32
    .quad Lop_cosh_f32          // 92: COSH_F32
    .quad Lop_tanh_f32          // 93: TANH_F32
    .quad Lop_asinh_f32         // 94: ASINH_F32
    .quad Lop_acosh_f32         // 95: ACOSH_F32
    .quad Lop_atanh_f32         // 96: ATANH_F32
    .quad Lop_ln_f32            // 97: LN_F32
    .quad Lop_exp_f32           // 98: EXP_F32
    .quad Lop_exp2_f32          // 99: EXP2_F32
    .quad Lop_log10_f32         // 100: LOG10_F32
    .quad Lop_log2_f32          // 101: LOG2_F32
    .quad Lop_sqrt_f32          // 102: SQRT_F32
    .quad Lop_abs_f32           // 103: ABS_F32
    .quad Lop_floor_f32         // 104: FLOOR_F32
    .quad Lop_ceil_f32          // 105: CEIL_F32
    .quad Lop_sin_f64           // 106: SIN_F64
    .quad Lop_cos_f64           // 107: COS_F64
    .quad Lop_tan_f64           // 108: TAN_F64
    .quad Lop_asin_f64          // 109: ASIN_F64
    .quad Lop_acos_f64          // 110: ACOS_F64
    .quad Lop_atan_f64          // 111: ATAN_F64
    .quad Lop_sinh_f64          // 112: SINH_F64
    .quad Lop_cosh_f64          // 113: COSH_F64
    .quad Lop_tanh_f64          // 114: TANH_F64
    .quad Lop_asinh_f64         // 115: ASINH_F64
    .quad Lop_acosh_f64         // 116: ACOSH_F64
    .quad Lop_atanh_f64         // 117: ATANH_F64
    .quad Lop_ln_f64            // 118: LN_F64
    .quad Lop_exp_f64           // 119: EXP_F64
    .quad Lop_exp2_f64          // 120: EXP2_F64
    .quad Lop_log10_f64         // 121: LOG10_F64
    .quad Lop_log2_f64          // 122: LOG2_F64
    .quad Lop_sqrt_f64          // 123: SQRT_F64
    .quad Lop_abs_f64           // 124: ABS_F64
    .quad Lop_floor_f64         // 125: FLOOR_F64
    .quad Lop_ceil_f64          // 126: CEIL_F64
    .quad Lop_isinf_f32         // 127: ISINF_F32
    .quad Lop_isinf_f64         // 128: ISINF_F64
    .quad Lop_isnan_f32         // 129: ISNAN_F32
    .quad Lop_isnan_f64         // 130: ISNAN_F64
    .quad Lop_pow_f32           // 131: POW_F32
    .quad Lop_atan2_f32         // 132: ATAN2_F32
    .quad Lop_min_f32           // 133: MIN_F32
    .quad Lop_max_f32           // 134: MAX_F32
    .quad Lop_pow_f64           // 135: POW_F64
    .quad Lop_atan2_f64         // 136: ATAN2_F64
    .quad Lop_min_f64           // 137: MIN_F64
    .quad Lop_max_f64           // 138: MAX_F64
    .quad Lop_load_slot32       // 139: LOAD_SLOT32
    .quad Lop_store_slot32      // 140: STORE_SLOT32
    .quad Lop_load_imm_wide     // 141: LOAD_IMM_WIDE
    .quad Lop_load_f64_wide     // 142: LOAD_F64_WIDE
    .quad Lop_load32_off_wide   // 143: LOAD32_OFF_WIDE
    .quad Lop_load64_off_wide   // 144: LOAD64_OFF_WIDE
    .quad Lop_store8_off_wide   // 145: STORE8_OFF_WIDE
    .quad Lop_store32_off_wide  // 146: STORE32_OFF_WIDE
    .quad Lop_store64_off_wide  // 147: STORE64_OFF_WIDE
    .quad Lop_mem_copy_wide     // 148: MEM_COPY_WIDE
    .quad Lop_save_regs_wide    // 149: SAVE_REGS_WIDE
    .quad Lop_restore_regs_wide // 150: RESTORE_REGS_WIDE
    .quad Lop_i32_to_i8         // 151: I32_TO_I8
    .quad Lop_i8_to_i32         // 152: I8_TO_I32
    .quad Lop_i64_to_u32        // 153: I64_TO_U32
    .quad Lop_call_closure      // 154: CALL_CLOSURE
    .quad Lop_get_closure_ptr   // 155: GET_CLOSURE_PTR
    // 156-255: invalid
    .fill 100, 8, 0            // will be Lop_invalid pointers
    // Actually we need the address of Lop_invalid, but .fill with a symbol
    // isn't possible. We handle this by checking tag < 156 in dispatch,
    // or just let the zero address fault (which is fine for invalid opcodes).
