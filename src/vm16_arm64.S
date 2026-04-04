// =============================================================================
// ARM64 assembly VM16 interpreter — 65536-handler threaded dispatch
// =============================================================================
//
// 16-bit instruction format: [opcode:8][ra:4][rb:4]
// Each of the 65536 possible instruction words has its own handler at a fixed
// stride in the handler table. Dispatch is: handler_base + word * STRIDE.
//
// VM registers r0–r15 are pinned to ARM64 registers:
//   r0  = x3     r4  = x7     r8  = x11    r12 = x15
//   r1  = x4     r5  = x8     r9  = x12    r13 = x27
//   r2  = x5     r6  = x9     r10 = x13    r14 = x28
//   r3  = x6     r7  = x10    r11 = x14    r15 = x25
//
// Dispatch state:
//   x19 = bytecode pointer (points at next u16, post-increment)
//   x20 = handler base address
//   x21 = locals_ptr + locals_base (effective local base)
//   x22 = globals_ptr
//   x23 = context (Asm16Context pointer)
//   x24 = scratch (callee-saved, survives helper calls)
//   x26 = scratch (callee-saved)
//
// Each handler is HANDLER_STRIDE bytes, padded with brk. Hot ops fit in
// 5 instructions (op + dispatch); heavy ops trampoline to shared code.
//
// Threaded dispatch (inline at end of every handler):
//   ldrh  w0, [x19], #2           // load next instruction, advance IP
//   add   x1, x20, x0, lsl #5    // handler = base + word * 32
//   br    x1                      // jump to handler
//
// =============================================================================

.set HANDLER_STRIDE_SHIFT, 5   // 1 << 5 = 32 bytes per handler
.set HANDLER_STRIDE, 32

// --- Asm16Context field offsets (must match Rust #[repr(C)] struct) ---
.set CTX_OPS,             0
.set CTX_REGS,            8
.set CTX_IP,              16
.set CTX_LOCALS_BASE,     24
.set CTX_LOCALS_PTR,      32
.set CTX_GLOBALS_PTR,     40
.set CTX_FUNC_OFFSETS,    48
.set CTX_FUNC_LOCALS,     56
.set CTX_F32_POOL,        64
.set CTX_F64_POOL,        72
.set CTX_I64_POOL,        80
.set CTX_CLOSURE_PTR,     88
.set CTX_CALL_STACK,      96
.set CTX_CALL_STACK_LEN,  104
.set CTX_CURRENT_FUNC,    112
.set CTX_LOCALS_CAP,      120
.set CTX_FN_PRINT_I32,    128
.set CTX_FN_PRINT_F32,    136
.set CTX_FN_ASSERT,       144
.set CTX_FN_PUTC,         152
.set CTX_EXTERN_FUNCS_PTR, 160
.set CTX_EXTERN_FUNCS_LEN, 168
.set CTX_FN_CALL_EXTERN,  176

// Asm16CallFrame: func_idx(8) + ip(8) + locals_base(8) + saved_regs(128) = 152 bytes
.set FRAME_SIZE, 152

// --- VM register mapping: VM index (0-15) -> ARM64 register number ---
// The .irp loops iterate over ARM register numbers in VM-index order.
// VM r0=x3, r1=x4, ..., r12=x15, r13=x27, r14=x28, r15=x25
// IMPORTANT: The irp iteration order MUST match VM register indices 0-15.
// The instruction word encodes VM indices, so handler N in the irp corresponds
// to VM register index N.
.set VMREGS, 3  // Start of VM register range

// We iterate over ARM register numbers in VM-register order:
// Index:  0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15
// ARM:    3  4  5  6  7  8  9 10 11 12 13 14 15 27 28 25
// This list is used by all .irp loops below.

// =============================================================================
// Macros
// =============================================================================

// Threaded dispatch: load next u16 instruction and jump to its handler.
.macro next16
    ldrh    w0, [x19], #2
    add     x1, x20, x0, lsl #HANDLER_STRIDE_SHIFT
    br      x1
.endm

// Pad handler to exactly HANDLER_STRIDE bytes from label \start.
.macro pad16_handler start
    // Current position minus start should be <= HANDLER_STRIDE.
    // Fill remaining with brk #0xF000 (debug trap).
    .if (. - \start) < HANDLER_STRIDE
        .space HANDLER_STRIDE - (. - \start), 0x00
    .endif
.endm

// Read trailing u16 into \dst (sign-extended to 64-bit).
.macro trail16_i16 dst
    ldrsh   \dst, [x19], #2
.endm

// Read trailing u16 into \dst (zero-extended).
.macro trail16_u16 dst
    ldrh    \dst, [x19], #2
.endm

// Save all 16 VM registers to the context's regs array.
.macro save_vm16_regs
    ldr     x0, [x23, #CTX_REGS]
    stp     x3,  x4,  [x0, #0]
    stp     x5,  x6,  [x0, #16]
    stp     x7,  x8,  [x0, #32]
    stp     x9,  x10, [x0, #48]
    stp     x11, x12, [x0, #64]
    stp     x13, x14, [x0, #80]
    stp     x15, x27, [x0, #96]
    stp     x28, x25, [x0, #112]
.endm

// Load all 16 VM registers from the context's regs array.
.macro load_vm16_regs
    ldr     x0, [x23, #CTX_REGS]
    ldp     x3,  x4,  [x0, #0]
    ldp     x5,  x6,  [x0, #16]
    ldp     x7,  x8,  [x0, #32]
    ldp     x9,  x10, [x0, #48]
    ldp     x11, x12, [x0, #64]
    ldp     x13, x14, [x0, #80]
    ldp     x15, x27, [x0, #96]
    ldp     x28, x25, [x0, #112]
.endm

// =============================================================================
// Entry point
// =============================================================================

.text
.p2align 4
.globl _vm16_arm64_enter
_vm16_arm64_enter:
    // Save callee-saved registers.
    stp     x29, x30, [sp, #-112]!
    mov     x29, sp
    stp     x19, x20, [sp, #16]
    stp     x21, x22, [sp, #32]
    stp     x23, x24, [sp, #48]
    stp     x25, x26, [sp, #64]
    stp     x27, x28, [sp, #80]
    // Also save d8-d15 (callee-saved FP/SIMD) if needed for f32 ops.
    stp     d8, d9, [sp, #96]

    // x0 = Asm16Context pointer
    mov     x23, x0

    // Load pinned state from context.
    ldr     x19, [x23, #CTX_OPS]
    ldr     x22, [x23, #CTX_GLOBALS_PTR]

    // Compute effective locals base: locals_ptr + locals_base.
    ldr     x21, [x23, #CTX_LOCALS_PTR]
    ldr     x0,  [x23, #CTX_LOCALS_BASE]
    add     x21, x21, x0

    // Load IP (word index) and convert to byte pointer.
    ldr     x0,  [x23, #CTX_IP]
    add     x19, x19, x0, lsl #1   // ops + ip * 2

    // Load VM registers from context.
    load_vm16_regs

    // Compute handler base address.
    adrp    x20, _vm16_handler_table@PAGE
    add     x20, x20, _vm16_handler_table@PAGEOFF

    // Start dispatch.
    next16

// =============================================================================
// Exit: return to Rust
// =============================================================================
_vm16_exit:
    // Save VM registers back to context.
    save_vm16_regs

    // Return value is in x3 (VM r0).
    mov     x0, x3

    // Restore callee-saved registers.
    ldp     d8, d9, [sp, #96]
    ldp     x27, x28, [sp, #80]
    ldp     x25, x26, [sp, #64]
    ldp     x23, x24, [sp, #48]
    ldp     x21, x22, [sp, #32]
    ldp     x19, x20, [sp, #16]
    ldp     x29, x30, [sp], #112
    ret

// =============================================================================
// Shared trampoline for unimplemented opcodes (halt/return to Rust)
// =============================================================================
_vm16_halt:
    // Store the instruction word that triggered halt (still in w0 from dispatch).
    str     x0, [x23, #CTX_CLOSURE_PTR]   // abuse closure_ptr to pass debug info
    b       _vm16_exit

// =============================================================================
// Handler table: 65536 entries at HANDLER_STRIDE bytes each.
// =============================================================================
//
// Layout: for each possible 16-bit instruction word W = (opcode << 8) | (ra << 4) | rb,
// the handler is at _vm16_handler_table + W * HANDLER_STRIDE.
//
// We generate all handlers using nested .irp macros over opcodes and register
// combinations. For the MVP, only a subset of opcodes have real implementations;
// the rest jump to _vm16_halt.
//
// The register mapping uses the ARM64 register numbers directly. Since the .irp
// iteration is over ARM register numbers (not VM register indices), we iterate
// the 16 ARM registers in VM-register order.

.p2align HANDLER_STRIDE_SHIFT
_vm16_handler_table:

// --- Generate all 65536 handlers ---
// For each opcode (0x00-0xFF), generate 256 (ra, rb) combinations.
// Each handler is HANDLER_STRIDE bytes.

// Helper: iterate over all 256 (ra, rb) pairs for a given opcode.
// \body is a macro name that takes two ARM64 register numbers as arguments.

// The 16 ARM registers corresponding to VM r0-r15, in order:
// 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 27, 28, 25

// For each opcode, we emit 256 handlers (16 ra × 16 rb).
// Default: jump to halt for unimplemented opcodes.

// --- Opcode 0x00: NOP ---
.irp ra, 3,4,5,6,7,8,9,10,11,12,13,14,15,27,28,25
.irp rb, 3,4,5,6,7,8,9,10,11,12,13,14,15,27,28,25
    next16
    .space HANDLER_STRIDE - 12
.endr
.endr

// --- Opcode 0x01: HALT ---
.irp ra, 3,4,5,6,7,8,9,10,11,12,13,14,15,27,28,25
.irp rb, 3,4,5,6,7,8,9,10,11,12,13,14,15,27,28,25
    b       _vm16_exit
    .space HANDLER_STRIDE - 4
.endr
.endr

// --- Opcode 0x02: RETURN ---
// Pop call frame, restore registers, continue in caller.
.irp ra, 3,4,5,6,7,8,9,10,11,12,13,14,15,27,28,25
.irp rb, 3,4,5,6,7,8,9,10,11,12,13,14,15,27,28,25
    b       _vm16_do_return
    .space HANDLER_STRIDE - 4
.endr
.endr

// --- Opcode 0x03: RETURN_REG (move src to r0 then return) ---
.irp ra, 3,4,5,6,7,8,9,10,11,12,13,14,15,27,28,25
.irp rb, 3,4,5,6,7,8,9,10,11,12,13,14,15,27,28,25
    mov     x3, x\ra
    b       _vm16_do_return
    .space HANDLER_STRIDE - 8
.endr
.endr

// --- Opcode 0x04: JUMP (+trail i16 offset) ---
.irp ra, 3,4,5,6,7,8,9,10,11,12,13,14,15,27,28,25
.irp rb, 3,4,5,6,7,8,9,10,11,12,13,14,15,27,28,25
    ldrsh   x0, [x19]          // read i16 offset (don't advance yet)
    add     x19, x19, x0, lsl #1  // IP += offset * 2 (word offset to byte offset)
    add     x19, x19, #2       // skip past the trail word itself
    next16
    .space HANDLER_STRIDE - 24
.endr
.endr

// --- Opcode 0x05: JUMP_IF_ZERO ra (+trail i16 offset) ---
.irp ra, 3,4,5,6,7,8,9,10,11,12,13,14,15,27,28,25
.irp rb, 3,4,5,6,7,8,9,10,11,12,13,14,15,27,28,25
    ldrsh   x0, [x19]
    tst     w\ra, w\ra
    // If zero: IP = IP + offset * 2 + 2; else IP += 2 (skip trail)
    b.ne    1f
    add     x19, x19, x0, lsl #1
1:  add     x19, x19, #2
    next16
    .space HANDLER_STRIDE - 32
.endr
.endr

// --- Opcode 0x06: JUMP_IF_NOT_ZERO ra (+trail i16 offset) ---
.irp ra, 3,4,5,6,7,8,9,10,11,12,13,14,15,27,28,25
.irp rb, 3,4,5,6,7,8,9,10,11,12,13,14,15,27,28,25
    ldrsh   x0, [x19]
    tst     w\ra, w\ra
    b.eq    1f
    add     x19, x19, x0, lsl #1
1:  add     x19, x19, #2
    next16
    .space HANDLER_STRIDE - 32
.endr
.endr

// --- Opcode 0x07: reserved ---
.rept 256
    b       _vm16_halt
    .space HANDLER_STRIDE - 4
.endr

// --- Opcode 0x08: MOVE ra, rb ---
.irp ra, 3,4,5,6,7,8,9,10,11,12,13,14,15,27,28,25
.irp rb, 3,4,5,6,7,8,9,10,11,12,13,14,15,27,28,25
    mov     x\ra, x\rb
    next16
    .space HANDLER_STRIDE - 16
.endr
.endr

// --- Opcode 0x09: LOAD_IMM ra (+trail i16) ---
.irp ra, 3,4,5,6,7,8,9,10,11,12,13,14,15,27,28,25
.irp rb, 3,4,5,6,7,8,9,10,11,12,13,14,15,27,28,25
    ldrsh   x\ra, [x19], #2    // sign-extend i16 trail into ra
    next16
    .space HANDLER_STRIDE - 16
.endr
.endr

// --- Opcode 0x0A: LOAD_IMM_WIDE ra (+trail u16 pool index) ---
.irp ra, 3,4,5,6,7,8,9,10,11,12,13,14,15,27,28,25
.irp rb, 3,4,5,6,7,8,9,10,11,12,13,14,15,27,28,25
    ldrh    w0, [x19], #2          // pool index
    ldr     x1, [x23, #CTX_I64_POOL]
    ldr     x\ra, [x1, x0, lsl #3] // load i64 from pool
    next16
    .space HANDLER_STRIDE - 24
.endr
.endr

// --- Opcode 0x0B: LOAD_F32 ra (+trail u16 f32 pool index) ---
.irp ra, 3,4,5,6,7,8,9,10,11,12,13,14,15,27,28,25
.irp rb, 3,4,5,6,7,8,9,10,11,12,13,14,15,27,28,25
    ldrh    w0, [x19], #2
    ldr     x1, [x23, #CTX_F32_POOL]
    ldr     w\ra, [x1, x0, lsl #2] // load f32 bits
    next16
    .space HANDLER_STRIDE - 24
.endr
.endr

// --- Opcode 0x0C: LOAD_F64 ra (+trail u16 f64 pool index) ---
.irp ra, 3,4,5,6,7,8,9,10,11,12,13,14,15,27,28,25
.irp rb, 3,4,5,6,7,8,9,10,11,12,13,14,15,27,28,25
    ldrh    w0, [x19], #2
    ldr     x1, [x23, #CTX_F64_POOL]
    ldr     x\ra, [x1, x0, lsl #3]
    next16
    .space HANDLER_STRIDE - 24
.endr
.endr

// --- Opcode 0x0D: LOAD_CONST ra (+trail u16 const index) --- stub
.rept 256
    add     x19, x19, #2       // skip trail
    next16
    .space HANDLER_STRIDE - 16
.endr

// --- Opcode 0x0E: LOCAL_ADDR ra (+trail u16 slot) ---
.irp ra, 3,4,5,6,7,8,9,10,11,12,13,14,15,27,28,25
.irp rb, 3,4,5,6,7,8,9,10,11,12,13,14,15,27,28,25
    ldrh    w0, [x19], #2
    add     x\ra, x21, x0, lsl #3  // locals_base + slot * 8
    next16
    .space HANDLER_STRIDE - 20
.endr
.endr

// --- Opcode 0x0F: GLOBAL_ADDR ra (+trail u16 offset) ---
.irp ra, 3,4,5,6,7,8,9,10,11,12,13,14,15,27,28,25
.irp rb, 3,4,5,6,7,8,9,10,11,12,13,14,15,27,28,25
    ldrh    w0, [x19], #2
    add     x\ra, x22, x0      // globals_ptr + offset
    next16
    .space HANDLER_STRIDE - 20
.endr
.endr

// --- Opcode 0x10: GET_CLOSURE_PTR ra ---
.irp ra, 3,4,5,6,7,8,9,10,11,12,13,14,15,27,28,25
.irp rb, 3,4,5,6,7,8,9,10,11,12,13,14,15,27,28,25
    ldr     x\ra, [x23, #CTX_CLOSURE_PTR]
    next16
    .space HANDLER_STRIDE - 16
.endr
.endr

// --- Opcode 0x11: IADD ra, rb (destructive: ra += rb) ---
.irp ra, 3,4,5,6,7,8,9,10,11,12,13,14,15,27,28,25
.irp rb, 3,4,5,6,7,8,9,10,11,12,13,14,15,27,28,25
    add     x\ra, x\ra, x\rb
    next16
    .space HANDLER_STRIDE - 16
.endr
.endr

// --- Opcode 0x12: ISUB ra, rb ---
.irp ra, 3,4,5,6,7,8,9,10,11,12,13,14,15,27,28,25
.irp rb, 3,4,5,6,7,8,9,10,11,12,13,14,15,27,28,25
    sub     x\ra, x\ra, x\rb
    next16
    .space HANDLER_STRIDE - 16
.endr
.endr

// --- Opcode 0x13: IMUL ra, rb ---
.irp ra, 3,4,5,6,7,8,9,10,11,12,13,14,15,27,28,25
.irp rb, 3,4,5,6,7,8,9,10,11,12,13,14,15,27,28,25
    mul     x\ra, x\ra, x\rb
    next16
    .space HANDLER_STRIDE - 16
.endr
.endr

// --- Opcode 0x14: IDIV ra, rb ---
.irp ra, 3,4,5,6,7,8,9,10,11,12,13,14,15,27,28,25
.irp rb, 3,4,5,6,7,8,9,10,11,12,13,14,15,27,28,25
    sdiv    x\ra, x\ra, x\rb
    next16
    .space HANDLER_STRIDE - 16
.endr
.endr

// --- Opcode 0x15: UDIV ra, rb ---
.irp ra, 3,4,5,6,7,8,9,10,11,12,13,14,15,27,28,25
.irp rb, 3,4,5,6,7,8,9,10,11,12,13,14,15,27,28,25
    udiv    w\ra, w\ra, w\rb
    next16
    .space HANDLER_STRIDE - 16
.endr
.endr

// --- Opcode 0x16: IREM ra, rb ---
.irp ra, 3,4,5,6,7,8,9,10,11,12,13,14,15,27,28,25
.irp rb, 3,4,5,6,7,8,9,10,11,12,13,14,15,27,28,25
    sdiv    x0, x\ra, x\rb
    msub    x\ra, x0, x\rb, x\ra   // ra = ra - (ra/rb)*rb
    next16
    .space HANDLER_STRIDE - 20
.endr
.endr

// --- Opcode 0x17: IPOW --- trampoline (complex)
.rept 256
    b       _vm16_halt  // TODO
    .space HANDLER_STRIDE - 4
.endr

// --- Opcode 0x18: INEG ra ---
.irp ra, 3,4,5,6,7,8,9,10,11,12,13,14,15,27,28,25
.irp rb, 3,4,5,6,7,8,9,10,11,12,13,14,15,27,28,25
    neg     x\ra, x\ra
    next16
    .space HANDLER_STRIDE - 16
.endr
.endr

// --- Opcode 0x19: IADD_IMM ra (+trail i16) ---
.irp ra, 3,4,5,6,7,8,9,10,11,12,13,14,15,27,28,25
.irp rb, 3,4,5,6,7,8,9,10,11,12,13,14,15,27,28,25
    ldrsh   x0, [x19], #2
    add     x\ra, x\ra, x0
    next16
    .space HANDLER_STRIDE - 20
.endr
.endr

// --- Opcode 0x1A: FADD ra, rb (f32 destructive: ra += rb) ---
.irp ra, 3,4,5,6,7,8,9,10,11,12,13,14,15,27,28,25
.irp rb, 3,4,5,6,7,8,9,10,11,12,13,14,15,27,28,25
    fmov    s0, w\ra
    fmov    s1, w\rb
    fadd    s0, s0, s1
    fmov    w\ra, s0
    next16
    .space HANDLER_STRIDE - 28
.endr
.endr

// --- Opcode 0x1B: FSUB ra, rb ---
.irp ra, 3,4,5,6,7,8,9,10,11,12,13,14,15,27,28,25
.irp rb, 3,4,5,6,7,8,9,10,11,12,13,14,15,27,28,25
    fmov    s0, w\ra
    fmov    s1, w\rb
    fsub    s0, s0, s1
    fmov    w\ra, s0
    next16
    .space HANDLER_STRIDE - 28
.endr
.endr

// --- Opcode 0x1C: FMUL ra, rb ---
.irp ra, 3,4,5,6,7,8,9,10,11,12,13,14,15,27,28,25
.irp rb, 3,4,5,6,7,8,9,10,11,12,13,14,15,27,28,25
    fmov    s0, w\ra
    fmov    s1, w\rb
    fmul    s0, s0, s1
    fmov    w\ra, s0
    next16
    .space HANDLER_STRIDE - 28
.endr
.endr

// --- Opcode 0x1D: FDIV ra, rb ---
.irp ra, 3,4,5,6,7,8,9,10,11,12,13,14,15,27,28,25
.irp rb, 3,4,5,6,7,8,9,10,11,12,13,14,15,27,28,25
    fmov    s0, w\ra
    fmov    s1, w\rb
    fdiv    s0, s0, s1
    fmov    w\ra, s0
    next16
    .space HANDLER_STRIDE - 28
.endr
.endr

// --- Opcode 0x1E: FNEG ra ---
.irp ra, 3,4,5,6,7,8,9,10,11,12,13,14,15,27,28,25
.irp rb, 3,4,5,6,7,8,9,10,11,12,13,14,15,27,28,25
    fmov    s0, w\ra
    fneg    s0, s0
    fmov    w\ra, s0
    next16
    .space HANDLER_STRIDE - 24
.endr
.endr

// --- Opcode 0x1F: FPOW — stub ---
.rept 256
    b       _vm16_halt
    .space HANDLER_STRIDE - 4
.endr

// --- Opcode 0x20: FMUL_ADD ra, rb (+trail: rc in low 4 bits) ---
// ra = ra * rb + rc
.irp ra, 3,4,5,6,7,8,9,10,11,12,13,14,15,27,28,25
.irp rb, 3,4,5,6,7,8,9,10,11,12,13,14,15,27,28,25
    b       _vm16_fmuladd_trampoline
    .space HANDLER_STRIDE - 4
.endr
.endr

// --- Opcode 0x21: FMUL_SUB — stub ---
.rept 256
    b       _vm16_halt
    .space HANDLER_STRIDE - 4
.endr

// --- Opcode 0x22: FNMUL_ADD ra, rb (+trail: rc) ---
// ra = rc - ra * rb
.irp ra, 3,4,5,6,7,8,9,10,11,12,13,14,15,27,28,25
.irp rb, 3,4,5,6,7,8,9,10,11,12,13,14,15,27,28,25
    b       _vm16_fnmuladd_trampoline
    .space HANDLER_STRIDE - 4
.endr
.endr

// --- Opcodes 0x23-0x3F: f64 ops, bitwise, comparisons — stub ---
.rept (0x40 - 0x23) * 256
    b       _vm16_halt
    .space HANDLER_STRIDE - 4
.endr

// --- Opcode 0x40: ILT_JUMP ra, rb (+trail i16 offset) ---
// if !(ra < rb) then IP += offset
.irp ra, 3,4,5,6,7,8,9,10,11,12,13,14,15,27,28,25
.irp rb, 3,4,5,6,7,8,9,10,11,12,13,14,15,27,28,25
    ldrsh   x0, [x19]          // read offset
    cmp     w\ra, w\rb
    b.lt    1f                  // if ra < rb, skip the jump (continue)
    add     x19, x19, x0, lsl #1  // IP += offset * 2
1:  add     x19, x19, #2       // skip trail word
    next16
    .space HANDLER_STRIDE - 32
.endr
.endr

// --- Opcode 0x41: FLT_JUMP ra, rb (+trail i16 offset) ---
.irp ra, 3,4,5,6,7,8,9,10,11,12,13,14,15,27,28,25
.irp rb, 3,4,5,6,7,8,9,10,11,12,13,14,15,27,28,25
    b       _vm16_halt  // TODO: f32 compare + jump
    .space HANDLER_STRIDE - 4
.endr
.endr

// --- Opcodes 0x42-0x44: ILE_JUMP, FLE_JUMP, I32_TO_F32 — stub ---
.rept (0x45 - 0x42) * 256
    b       _vm16_halt
    .space HANDLER_STRIDE - 4
.endr

// --- Opcode 0x45: F32_TO_I32 ra ---
.irp ra, 3,4,5,6,7,8,9,10,11,12,13,14,15,27,28,25
.irp rb, 3,4,5,6,7,8,9,10,11,12,13,14,15,27,28,25
    fmov    s0, w\ra
    fcvtzs  w\ra, s0
    next16
    .space HANDLER_STRIDE - 20
.endr
.endr

// --- Opcodes 0x46-0x4E: remaining conversions — stub ---
.rept (0x4F - 0x46) * 256
    b       _vm16_halt
    .space HANDLER_STRIDE - 4
.endr

// --- Opcode 0x4F: LOAD8 ra, [rb] ---
.irp ra, 3,4,5,6,7,8,9,10,11,12,13,14,15,27,28,25
.irp rb, 3,4,5,6,7,8,9,10,11,12,13,14,15,27,28,25
    ldrsb   x\ra, [x\rb]
    next16
    .space HANDLER_STRIDE - 16
.endr
.endr

// --- Opcode 0x50: LOAD32 ra, [rb] ---
.irp ra, 3,4,5,6,7,8,9,10,11,12,13,14,15,27,28,25
.irp rb, 3,4,5,6,7,8,9,10,11,12,13,14,15,27,28,25
    ldr     w\ra, [x\rb]
    next16
    .space HANDLER_STRIDE - 16
.endr
.endr

// --- Opcode 0x51: LOAD64 ra, [rb] ---
.irp ra, 3,4,5,6,7,8,9,10,11,12,13,14,15,27,28,25
.irp rb, 3,4,5,6,7,8,9,10,11,12,13,14,15,27,28,25
    ldr     x\ra, [x\rb]
    next16
    .space HANDLER_STRIDE - 16
.endr
.endr

// --- Opcode 0x52: STORE8 [ra], rb ---
.irp ra, 3,4,5,6,7,8,9,10,11,12,13,14,15,27,28,25
.irp rb, 3,4,5,6,7,8,9,10,11,12,13,14,15,27,28,25
    strb    w\rb, [x\ra]
    next16
    .space HANDLER_STRIDE - 16
.endr
.endr

// --- Opcode 0x53: STORE32 [ra], rb ---
.irp ra, 3,4,5,6,7,8,9,10,11,12,13,14,15,27,28,25
.irp rb, 3,4,5,6,7,8,9,10,11,12,13,14,15,27,28,25
    str     w\rb, [x\ra]
    next16
    .space HANDLER_STRIDE - 16
.endr
.endr

// --- Opcode 0x54: STORE64 [ra], rb ---
.irp ra, 3,4,5,6,7,8,9,10,11,12,13,14,15,27,28,25
.irp rb, 3,4,5,6,7,8,9,10,11,12,13,14,15,27,28,25
    str     x\rb, [x\ra]
    next16
    .space HANDLER_STRIDE - 16
.endr
.endr

// --- Opcodes 0x55-0x87: Load/Store offsets, slot access, calls — stub ---
.rept (0x88 - 0x55) * 256
    b       _vm16_halt
    .space HANDLER_STRIDE - 4
.endr

// --- Opcode 0x88: ALLOC_LOCALS (+trail u16 size) — skip trail, no-op for now ---
.rept 256
    add     x19, x19, #2   // skip trail
    next16
    .space HANDLER_STRIDE - 16
.endr

// --- Opcode 0x89: SAVE_REGS (+trail) — no-op (CallFrame handles save) ---
.rept 256
    add     x19, x19, #2   // skip trail
    next16
    .space HANDLER_STRIDE - 16
.endr

// --- Opcode 0x8A: RESTORE_REGS (+trail) — no-op ---
.rept 256
    add     x19, x19, #2   // skip trail
    next16
    .space HANDLER_STRIDE - 16
.endr

// --- Opcodes 0x8B-0x8F: MEM_COPY, MEM_ZERO, SPILL, RELOAD — stub ---
.rept (0x90 - 0x8B) * 256
    b       _vm16_halt
    .space HANDLER_STRIDE - 4
.endr

// --- Opcode 0x90: PRINT_I32 ra ---
// Calls helper_print_i32. Must save caller-saved VM regs.
.irp ra, 3,4,5,6,7,8,9,10,11,12,13,14,15,27,28,25
.irp rb, 3,4,5,6,7,8,9,10,11,12,13,14,15,27,28,25
    sxtw    x0, w\ra
    b       _vm16_print_i32_trampoline
    .space HANDLER_STRIDE - 8
.endr
.endr

// --- Opcode 0x91: PRINT_F32 ra ---
.irp ra, 3,4,5,6,7,8,9,10,11,12,13,14,15,27,28,25
.irp rb, 3,4,5,6,7,8,9,10,11,12,13,14,15,27,28,25
    mov     w0, w\ra
    b       _vm16_print_f32_trampoline
    .space HANDLER_STRIDE - 8
.endr
.endr

// --- Opcode 0x92: ASSERT ra ---
.irp ra, 3,4,5,6,7,8,9,10,11,12,13,14,15,27,28,25
.irp rb, 3,4,5,6,7,8,9,10,11,12,13,14,15,27,28,25
    mov     x0, x\ra
    b       _vm16_assert_trampoline
    .space HANDLER_STRIDE - 8
.endr
.endr

// --- Opcode 0x93: PUTC ra ---
.irp ra, 3,4,5,6,7,8,9,10,11,12,13,14,15,27,28,25
.irp rb, 3,4,5,6,7,8,9,10,11,12,13,14,15,27,28,25
    mov     x0, x\ra
    b       _vm16_putc_trampoline
    .space HANDLER_STRIDE - 8
.endr
.endr

// --- Opcodes 0x94-0x9F: reserved — stub ---
.rept (0xA0 - 0x94) * 256
    b       _vm16_halt
    .space HANDLER_STRIDE - 4
.endr

// --- Opcode 0xA0: SIN_F32 ra ---
// Calls sinf. Must save/restore caller-saved VM regs.
.irp ra, 3,4,5,6,7,8,9,10,11,12,13,14,15,27,28,25
.irp rb, 3,4,5,6,7,8,9,10,11,12,13,14,15,27,28,25
    fmov    s0, w\ra            // arg: f32 in s0
    mov     x24, #\ra           // save ARM reg number for result
    b       _vm16_sinf32_trampoline
    .space HANDLER_STRIDE - 12
.endr
.endr

// --- Opcodes 0xA1-0xFF: remaining math builtins, SIMD — stub ---
.rept (0x100 - 0xA1) * 256
    b       _vm16_halt
    .space HANDLER_STRIDE - 4
.endr

// =============================================================================
// Shared out-of-line code (reached via trampoline from handlers)
// =============================================================================

.p2align 4
// --- Print i32 trampoline: x0 = value, call helper, restore VM regs ---
_vm16_print_i32_trampoline:
    // Save caller-saved VM registers (x3-x15) on the stack.
    stp     x3,  x4,  [sp, #-128]!
    stp     x5,  x6,  [sp, #16]
    stp     x7,  x8,  [sp, #32]
    stp     x9,  x10, [sp, #48]
    stp     x11, x12, [sp, #64]
    stp     x13, x14, [sp, #80]
    str     x15,       [sp, #96]
    // x0 already has the value.
    ldr     x1, [x23, #CTX_FN_PRINT_I32]
    blr     x1
    // Restore caller-saved VM registers.
    ldp     x3,  x4,  [sp]
    ldp     x5,  x6,  [sp, #16]
    ldp     x7,  x8,  [sp, #32]
    ldp     x9,  x10, [sp, #48]
    ldp     x11, x12, [sp, #64]
    ldp     x13, x14, [sp, #80]
    ldr     x15,       [sp, #96]
    add     sp, sp, #128
    next16

_vm16_print_f32_trampoline:
    stp     x3,  x4,  [sp, #-128]!
    stp     x5,  x6,  [sp, #16]
    stp     x7,  x8,  [sp, #32]
    stp     x9,  x10, [sp, #48]
    stp     x11, x12, [sp, #64]
    stp     x13, x14, [sp, #80]
    str     x15,       [sp, #96]
    ldr     x1, [x23, #CTX_FN_PRINT_F32]
    blr     x1
    ldp     x3,  x4,  [sp]
    ldp     x5,  x6,  [sp, #16]
    ldp     x7,  x8,  [sp, #32]
    ldp     x9,  x10, [sp, #48]
    ldp     x11, x12, [sp, #64]
    ldp     x13, x14, [sp, #80]
    ldr     x15,       [sp, #96]
    add     sp, sp, #128
    next16

_vm16_assert_trampoline:
    stp     x3,  x4,  [sp, #-128]!
    stp     x5,  x6,  [sp, #16]
    stp     x7,  x8,  [sp, #32]
    stp     x9,  x10, [sp, #48]
    stp     x11, x12, [sp, #64]
    stp     x13, x14, [sp, #80]
    str     x15,       [sp, #96]
    ldr     x1, [x23, #CTX_FN_ASSERT]
    blr     x1
    // x0 = 0 if ok, 1 if assertion failed
    cbnz    w0, 1f
    ldp     x3,  x4,  [sp]
    ldp     x5,  x6,  [sp, #16]
    ldp     x7,  x8,  [sp, #32]
    ldp     x9,  x10, [sp, #48]
    ldp     x11, x12, [sp, #64]
    ldp     x13, x14, [sp, #80]
    ldr     x15,       [sp, #96]
    add     sp, sp, #128
    next16
1:  // Assertion failed — exit.
    ldp     x3,  x4,  [sp]
    ldp     x5,  x6,  [sp, #16]
    ldp     x7,  x8,  [sp, #32]
    ldp     x9,  x10, [sp, #48]
    ldp     x11, x12, [sp, #64]
    ldp     x13, x14, [sp, #80]
    ldr     x15,       [sp, #96]
    add     sp, sp, #128
    b       _vm16_exit

_vm16_putc_trampoline:
    stp     x3,  x4,  [sp, #-128]!
    stp     x5,  x6,  [sp, #16]
    stp     x7,  x8,  [sp, #32]
    stp     x9,  x10, [sp, #48]
    stp     x11, x12, [sp, #64]
    stp     x13, x14, [sp, #80]
    str     x15,       [sp, #96]
    ldr     x1, [x23, #CTX_FN_PUTC]
    blr     x1
    ldp     x3,  x4,  [sp]
    ldp     x5,  x6,  [sp, #16]
    ldp     x7,  x8,  [sp, #32]
    ldp     x9,  x10, [sp, #48]
    ldp     x11, x12, [sp, #64]
    ldp     x13, x14, [sp, #80]
    ldr     x15,       [sp, #96]
    add     sp, sp, #128
    next16

// --- SinF32 trampoline ---
// On entry: s0 has the f32 arg, x24 has the ARM reg number for result.
_vm16_sinf32_trampoline:
    // Save caller-saved VM registers.
    stp     x3,  x4,  [sp, #-128]!
    stp     x5,  x6,  [sp, #16]
    stp     x7,  x8,  [sp, #32]
    stp     x9,  x10, [sp, #48]
    stp     x11, x12, [sp, #64]
    stp     x13, x14, [sp, #80]
    str     x15,       [sp, #96]
    // s0 already has the arg. Call sinf (takes float, returns float).
    bl      _sinf
    // Result in s0. Convert to w-reg bits.
    fmov    w26, s0
    // Restore caller-saved VM registers.
    ldp     x3,  x4,  [sp]
    ldp     x5,  x6,  [sp, #16]
    ldp     x7,  x8,  [sp, #32]
    ldp     x9,  x10, [sp, #48]
    ldp     x11, x12, [sp, #64]
    ldp     x13, x14, [sp, #80]
    ldr     x15,       [sp, #96]
    add     sp, sp, #128
    // Store result into the correct VM register via the regs array.
    // x24 has the ARM register number. Use the context regs array.
    // Simpler: save result to regs array, then reload all VM regs.
    save_vm16_regs
    ldr     x1, [x23, #CTX_REGS]
    // Map ARM reg number to VM index: 3->0, 4->1, ..., 15->12, 27->13, 28->14, 25->15
    // For x3-x15: vm_idx = arm_reg - 3
    // For x25: vm_idx = 15, x27: vm_idx = 13, x28: vm_idx = 14
    cmp     x24, #16
    b.ge    2f
    sub     x0, x24, #3        // vm_idx = arm_reg - 3 (for x3-x15)
    b       3f
2:  // Handle x25, x27, x28
    cmp     x24, #25
    mov     x0, #15
    b.eq    3f
    cmp     x24, #27
    mov     x0, #13
    b.eq    3f
    mov     x0, #14            // must be x28
3:  add     x1, x1, x0, lsl #3  // &regs[vm_idx]
    fmov    w26, s0
    str     x26, [x1]          // store f32 result (zero-extended)
    load_vm16_regs
    next16

// --- FMulAdd trampoline (opcode 0x20) ---
// ra = ra * rb + rc. Trail word has rc index in low 4 bits.
// Strategy: save all VM regs FIRST, then decode instruction, compute, load back.
_vm16_fmuladd_trampoline:
    // Save VM regs BEFORE clobbering x0-x3 for decode.
    save_vm16_regs
    // Now decode: read trail and instruction word.
    ldrh    w0, [x19], #2      // read trail word (rc VM index in low 4 bits)
    and     w0, w0, #0xf       // rc VM index
    ldrh    w1, [x19, #-4]     // reload instruction word
    ubfx    w2, w1, #4, #4     // ra VM index
    and     w3, w1, #0xf       // rb VM index
    ldr     x1, [x23, #CTX_REGS]
    add     x4, x1, x2, lsl #3
    ldr     s0, [x4]               // ra value (f32)
    add     x4, x1, x3, lsl #3
    ldr     s1, [x4]               // rb value
    add     x4, x1, x0, lsl #3
    ldr     s2, [x4]               // rc value
    fmadd   s0, s0, s1, s2         // result = ra * rb + rc
    // Store result back to regs[ra].
    fmov    w0, s0
    add     x4, x1, x2, lsl #3
    str     x0, [x4]               // store as 64-bit (zero-extended)
    // Load all VM regs back (including the modified one).
    load_vm16_regs
    next16

// --- FNMulAdd trampoline (opcode 0x22) ---
// ra = rc - ra * rb. ARM64 fnmsub: d = -(a*b) + c = c - a*b.
_vm16_fnmuladd_trampoline:
    save_vm16_regs
    ldrh    w0, [x19], #2
    and     w0, w0, #0xf
    ldrh    w1, [x19, #-4]
    ubfx    w2, w1, #4, #4
    and     w3, w1, #0xf
    ldr     x1, [x23, #CTX_REGS]
    add     x4, x1, x2, lsl #3
    ldr     s0, [x4]               // ra
    add     x4, x1, x3, lsl #3
    ldr     s1, [x4]               // rb
    add     x4, x1, x0, lsl #3
    ldr     s2, [x4]               // rc
    fnmsub  s0, s0, s1, s2         // result = -(ra*rb) + rc = rc - ra*rb
    fmov    w0, s0
    add     x4, x1, x2, lsl #3
    str     x0, [x4]
    load_vm16_regs
    next16

_vm16_do_return:
    // Check if call stack is empty.
    ldr     x0, [x23, #CTX_CALL_STACK_LEN]
    cbnz    x0, 1f
    b       _vm16_exit
1:
    // Pop frame. TODO: implement full return with register restore.
    b       _vm16_exit  // For now, just exit.
