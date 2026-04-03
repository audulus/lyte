// Register-based virtual machine for when we can't JIT,
// like on iOS.

pub use crate::opcode::*;

use std::convert::TryInto;
use std::fmt;

// ============ Print callback ============
//
// Thread-local print callback used by the VM interpreter, ARM64 assembly
// helpers, and LLVM helpers. When set, print/putc output is routed to the
// callback instead of stdout.

/// Print callback signature: (text pointer, text length, user data).
pub type PrintCallbackFn = unsafe extern "C" fn(*const u8, usize, *mut u8);

#[derive(Clone, Copy)]
struct PrintCallback {
    func: PrintCallbackFn,
    user_data: *mut u8,
}

thread_local! {
    static PRINT_CALLBACK: std::cell::Cell<Option<PrintCallback>> = const { std::cell::Cell::new(None) };
}

/// Set the thread-local print callback. Pass `None` to restore default stdout behavior.
pub fn set_print_callback(
    callback: Option<PrintCallbackFn>,
    user_data: *mut u8,
) {
    PRINT_CALLBACK.with(|cb| {
        cb.set(callback.map(|func| PrintCallback { func, user_data }));
    });
}

/// Write text through the print callback if set, otherwise to stdout.
#[inline]
pub fn print_output(text: &str) {
    PRINT_CALLBACK.with(|cb| {
        // Safety: Cell<Option<PrintCallback>> is Copy (PrintCallback contains a fn ptr + raw ptr).
        // We take a snapshot so the borrow on the Cell is released before calling out.
        let snapshot = cb.get();
        if let Some(ref pcb) = snapshot {
            unsafe { (pcb.func)(text.as_ptr(), text.len(), pcb.user_data) };
        } else {
            print!("{}", text);
        }
    });
}

/// Write text + newline through the print callback if set, otherwise to stdout.
#[inline]
pub fn println_output(text: &str) {
    PRINT_CALLBACK.with(|cb| {
        let snapshot = cb.get();
        if let Some(ref pcb) = snapshot {
            let with_newline = format!("{}\n", text);
            unsafe { (pcb.func)(with_newline.as_ptr(), with_newline.len(), pcb.user_data) };
        } else {
            println!("{}", text);
        }
    });
}

// ============ Packed Bytecode Format ============
//
// Each instruction is 4 bytes, matching LuaJIT's format:
//
//   ABC: [tag:8][A:8][B:8][C:8]  — 3 register operands
//   AD:  [tag:8][A:8][D:i16]     — 1 register + 16-bit immediate
//
// The `tag` field is a raw u8 discriminant, dispatched via a match that LLVM
// compiles to a jump table — the Rust equivalent of computed goto.

#[derive(Clone, Copy)]
#[repr(transparent)]
pub struct PackedOp(pub(crate) u32);

impl PackedOp {
    /// ABC format: 3 register operands (u8)
    #[inline(always)]
    fn abc(tag: u8, a: u8, b: u8, c: u8) -> Self {
        Self(tag as u32 | (a as u32) << 8 | (b as u32) << 16 | (c as u32) << 24)
    }
    /// ABC format accepting Reg (u16) — truncates to u8 (valid post-register-allocation)
    #[inline(always)]
    fn abc_r(tag: u8, a: Reg, b: Reg, c: Reg) -> Self {
        Self::abc(tag, a as u8, b as u8, c as u8)
    }
    /// AD format: 1 register + 16-bit signed immediate
    #[inline(always)]
    fn ad(tag: u8, a: u8, d: i16) -> Self {
        Self(tag as u32 | (a as u32) << 8 | (d as u16 as u32) << 16)
    }
    /// AD format accepting Reg (u16) — truncates to u8 (valid post-register-allocation)
    #[inline(always)]
    fn ad_r(tag: u8, a: Reg, d: i16) -> Self {
        Self::ad(tag, a as u8, d)
    }
    /// Raw data word (for extended instructions like MemEq)
    #[inline(always)]
    fn data(val: u32) -> Self {
        Self(val)
    }
    #[inline(always)]
    fn tag(self) -> u8 {
        self.0 as u8
    }
    #[inline(always)]
    fn a(self) -> u8 {
        (self.0 >> 8) as u8
    }
    #[inline(always)]
    fn b(self) -> u8 {
        (self.0 >> 16) as u8
    }
    #[inline(always)]
    fn c(self) -> u8 {
        (self.0 >> 24) as u8
    }
    #[inline(always)]
    fn c_i8(self) -> i8 {
        self.c() as i8
    }
    #[inline(always)]
    fn d(self) -> i16 {
        (self.0 >> 16) as i16
    }
    #[inline(always)]
    fn d_u16(self) -> u16 {
        (self.0 >> 16) as u16
    }
}

// Tag constants — one per opcode variant
pub(crate) mod tags {
    pub const NOP: u8 = 0;
    pub const HALT: u8 = 1;
    pub const MOVE: u8 = 2;
    pub const LOAD_IMM: u8 = 3; // AD: A=dst, D=i16 value
    pub const LOAD_F32: u8 = 4; // AD: A=dst, D=f32_pool index
    pub const LOAD_CONST: u8 = 5; // AD: A=dst, D=const_pool index
    pub const IADD: u8 = 6;
    pub const ISUB: u8 = 7;
    pub const IMUL: u8 = 8;
    pub const IDIV: u8 = 9;
    pub const UDIV: u8 = 10;
    pub const IREM: u8 = 11;
    pub const IPOW: u8 = 12;
    pub const INEG: u8 = 13;
    pub const IADD_IMM: u8 = 14; // ABC: A=dst, B=src, C=imm (i8)
    pub const FADD: u8 = 15;
    pub const FSUB: u8 = 16;
    pub const FMUL: u8 = 17;
    pub const FDIV: u8 = 18;
    pub const FNEG: u8 = 19;
    pub const FPOW: u8 = 20;
    pub const DADD: u8 = 21;
    pub const DSUB: u8 = 22;
    pub const DMUL: u8 = 23;
    pub const DDIV: u8 = 24;
    pub const DNEG: u8 = 25;
    pub const DPOW: u8 = 26;
    pub const AND: u8 = 27;
    pub const OR: u8 = 28;
    pub const XOR: u8 = 29;
    pub const NOT: u8 = 30;
    pub const SHL: u8 = 31;
    pub const SHR: u8 = 32;
    pub const USHR: u8 = 33;
    pub const IEQ: u8 = 34;
    pub const INE: u8 = 35;
    pub const ILT: u8 = 36;
    pub const ILE: u8 = 37;
    pub const ULT: u8 = 38;
    pub const FEQ: u8 = 39;
    pub const FNE: u8 = 40;
    pub const FLT: u8 = 41;
    pub const FLE: u8 = 42;
    pub const MEM_EQ: u8 = 43; // ABC+data: A=dst, B=a, C=b, next word=size
    pub const MEM_NE: u8 = 44; // ABC+data: A=dst, B=a, C=b, next word=size
    pub const DEQ: u8 = 45;
    pub const DLT: u8 = 46;
    pub const DLE: u8 = 47;
    pub const I32_TO_F32: u8 = 48;
    pub const F32_TO_I32: u8 = 49;
    pub const I32_TO_F64: u8 = 50;
    pub const F64_TO_I32: u8 = 51;
    pub const F32_TO_F64: u8 = 52;
    pub const F64_TO_F32: u8 = 53;
    pub const LOAD8: u8 = 54;
    pub const LOAD32: u8 = 55;
    pub const LOAD64: u8 = 56;
    pub const LOAD32_OFF: u8 = 57; // ABC: A=dst, B=base, C=offset (u8 bytes)
    pub const LOAD64_OFF: u8 = 58; // ABC: A=dst, B=base, C=offset (u8 bytes)
    pub const STORE8: u8 = 59;
    pub const STORE32: u8 = 60;
    pub const STORE64: u8 = 61;
    pub const STORE8_OFF: u8 = 62; // ABC: A=base, B=src, C=offset (u8 bytes)
    pub const STORE32_OFF: u8 = 63; // ABC: A=base, B=src, C=offset (u8 bytes)
    pub const STORE64_OFF: u8 = 64; // ABC: A=base, B=src, C=offset (u8 bytes)
    pub const LOCAL_ADDR: u8 = 65; // AD: A=dst, D=slot (u16)
    pub const GLOBAL_ADDR: u8 = 66; // AD: A=dst, D=offset (u16)
    pub const JUMP: u8 = 67; // AD: D=offset (i16)
    pub const JUMP_IF_ZERO: u8 = 68; // AD: A=cond, D=offset (i16)
    pub const JUMP_IF_NOT_ZERO: u8 = 69; // AD: A=cond, D=offset (i16)
    pub const ILT_JUMP: u8 = 70; // ABC: A=a, B=b, C=offset (i8)
    pub const FLT_JUMP: u8 = 71; // ABC: A=a, B=b, C=offset (i8)
    pub const CALL: u8 = 72; // ABC: A=args_start, B=arg_count, C=func_idx
    pub const CALL_INDIRECT: u8 = 73; // ABC: A=func_reg, B=args_start, C=arg_count
    pub const RETURN: u8 = 74;
    pub const RETURN_REG: u8 = 75;
    pub const ALLOC_LOCALS: u8 = 76; // AD: D=size (u16)
    pub const MEM_COPY: u8 = 77; // ABC: A=dst, B=src, C=size (u8)
    pub const MEM_ZERO: u8 = 78; // AD: A=dst, D=size (u16)
    pub const SAVE_REGS: u8 = 79; // ABC: A=start_reg, B=count, C=slot/8
    pub const RESTORE_REGS: u8 = 80; // ABC: A=start_reg, B=count, C=slot/8
    pub const PRINT_I32: u8 = 81;
    pub const PRINT_F32: u8 = 82;
    pub const ASSERT: u8 = 83;
    pub const PUTC: u8 = 84;
    // Unary f32 math
    pub const SIN_F32: u8 = 85;
    pub const COS_F32: u8 = 86;
    pub const TAN_F32: u8 = 87;
    pub const ASIN_F32: u8 = 88;
    pub const ACOS_F32: u8 = 89;
    pub const ATAN_F32: u8 = 90;
    pub const SINH_F32: u8 = 91;
    pub const COSH_F32: u8 = 92;
    pub const TANH_F32: u8 = 93;
    pub const ASINH_F32: u8 = 94;
    pub const ACOSH_F32: u8 = 95;
    pub const ATANH_F32: u8 = 96;
    pub const LN_F32: u8 = 97;
    pub const EXP_F32: u8 = 98;
    pub const EXP2_F32: u8 = 99;
    pub const LOG10_F32: u8 = 100;
    pub const LOG2_F32: u8 = 101;
    pub const SQRT_F32: u8 = 102;
    pub const ABS_F32: u8 = 103;
    pub const FLOOR_F32: u8 = 104;
    pub const CEIL_F32: u8 = 105;
    // Unary f64 math
    pub const SIN_F64: u8 = 106;
    pub const COS_F64: u8 = 107;
    pub const TAN_F64: u8 = 108;
    pub const ASIN_F64: u8 = 109;
    pub const ACOS_F64: u8 = 110;
    pub const ATAN_F64: u8 = 111;
    pub const SINH_F64: u8 = 112;
    pub const COSH_F64: u8 = 113;
    pub const TANH_F64: u8 = 114;
    pub const ASINH_F64: u8 = 115;
    pub const ACOSH_F64: u8 = 116;
    pub const ATANH_F64: u8 = 117;
    pub const LN_F64: u8 = 118;
    pub const EXP_F64: u8 = 119;
    pub const EXP2_F64: u8 = 120;
    pub const LOG10_F64: u8 = 121;
    pub const LOG2_F64: u8 = 122;
    pub const SQRT_F64: u8 = 123;
    pub const ABS_F64: u8 = 124;
    pub const FLOOR_F64: u8 = 125;
    pub const CEIL_F64: u8 = 126;
    // Predicates
    pub const ISINF_F32: u8 = 127;
    pub const ISINF_F64: u8 = 128;
    pub const ISNAN_F32: u8 = 129;
    pub const ISNAN_F64: u8 = 130;
    // Binary f32 math
    pub const POW_F32: u8 = 131;
    pub const ATAN2_F32: u8 = 132;
    pub const MIN_F32: u8 = 133;
    pub const MAX_F32: u8 = 134;
    // Binary f64 math
    pub const POW_F64: u8 = 135;
    pub const ATAN2_F64: u8 = 136;
    pub const MIN_F64: u8 = 137;
    pub const MAX_F64: u8 = 138;
    // Superinstructions: fused LocalAddr+Load/Store
    pub const LOAD_SLOT32: u8 = 139; // AD: A=dst, D=slot (u16)
    pub const STORE_SLOT32: u8 = 140; // AD: A=src, D=slot (u16)
                                      // For values that don't fit inline
    pub const LOAD_IMM_WIDE: u8 = 141; // AD: A=dst, D=wide_i64 pool index
    pub const LOAD_F64_WIDE: u8 = 142; // AD: A=dst, D=wide_f64 pool index
                                       // Extended instructions (AB + data word) for large offsets
    pub const LOAD32_OFF_WIDE: u8 = 143; // AB+data: A=dst, B=base, next word=offset
    pub const LOAD64_OFF_WIDE: u8 = 144; // AB+data: A=dst, B=base, next word=offset
    pub const STORE8_OFF_WIDE: u8 = 145; // AB+data: A=base, B=src, next word=offset
    pub const STORE32_OFF_WIDE: u8 = 146; // AB+data: A=base, B=src, next word=offset
    pub const STORE64_OFF_WIDE: u8 = 147; // AB+data: A=base, B=src, next word=offset
                                          // Extended instructions (ABC + data word) for large operands
    pub const MEM_COPY_WIDE: u8 = 148; // ABC+data: A=dst, B=src, next word=size
    pub const SAVE_REGS_WIDE: u8 = 149; // ABC+data: A=start_reg, B=count, next word=slot
    pub const RESTORE_REGS_WIDE: u8 = 150; // ABC+data: A=start_reg, B=count, next word=slot

    pub const I32_TO_I8: u8 = 151;
    pub const I8_TO_I32: u8 = 152;
    pub const I64_TO_U32: u8 = 153;
    pub const CALL_CLOSURE: u8 = 154;
    pub const GET_CLOSURE_PTR: u8 = 155;
    pub const SLICE_EQ: u8 = 156; // ABC+data: A=dst, B=a, C=b, next word=elem_size
    pub const SLICE_NE: u8 = 157; // ABC+data: A=dst, B=a, C=b, next word=elem_size
    pub const SLICE_LOAD32: u8 = 158; // ABC: A=dst, B=slice, C=index
    pub const SLICE_STORE32: u8 = 159; // ABC: A=src, B=slice, C=index
    pub const ILT_JUMP_WIDE: u8 = 161; // AB+data: A=a, B=b, next word=offset (i32)
    pub const FLT_JUMP_WIDE: u8 = 162; // AB+data: A=a, B=b, next word=offset (i32)
    pub const FMUL_ADD: u8 = 163; // ABC+data: A=dst, B=a, C=b, next word=c (dst = a*b + c)
    pub const FMUL_SUB: u8 = 164; // ABC+data: A=dst, B=a, C=b, next word=c (dst = a*b - c)
    pub const DMUL_ADD: u8 = 165; // ABC+data: A=dst, B=a, C=b, next word=c (dst = a*b + c)
    pub const DMUL_SUB: u8 = 166; // ABC+data: A=dst, B=a, C=b, next word=c (dst = a*b - c)
    pub const FNMUL_ADD: u8 = 167; // ABC+data: A=dst, B=a, C=b, next word=c (dst = c - a*b)
    pub const DNMUL_ADD: u8 = 168; // ABC+data: A=dst, B=a, C=b, next word=c (dst = c - a*b)
    pub const F32X4_ADD: u8 = 170; // ABC: A=dst, B=a, C=b (ptr-represented, lane-wise)
    pub const F32X4_SUB: u8 = 171;
    pub const F32X4_MUL: u8 = 172;
    pub const F32X4_DIV: u8 = 173;
    pub const F32X4_NEG: u8 = 174; // AB: A=dst, B=src
    pub const CALL_EXTERN: u8 = 160; // AD: A=args_start|arg_count, D=extern_index
}

/// Maximum number of C-level arguments for an extern function (including context).
const MAX_EXTERN_ARGS: usize = 16;

/// Call an extern function pointer using libffi's low-level API (no heap allocation).
///
/// Supports any combination of types (i32, f32, f64, ptr) up to MAX_EXTERN_ARGS - 1 params.
/// The function is called as `fn_ptr(context, args...)` where context is a `*mut u8`.
///
/// Returns the result as a u64 (reinterpreted from the actual return type).
pub(crate) unsafe fn call_extern_fn(
    fn_ptr: usize,
    context: *mut u8,
    registers: &[u64],
    args_start: usize,
    param_types: &[ExternType],
    ret_type: ExternType,
) -> u64 {
    use core::ffi::c_void;
    use libffi::low::{self, ffi_cif, ffi_type, CodePtr, types};
    use libffi::raw::ffi_abi_FFI_DEFAULT_ABI;

    let n = param_types.len();
    assert!(n + 1 <= MAX_EXTERN_ARGS, "extern function has too many parameters (max {})", MAX_EXTERN_ARGS - 1);

    // Build the ffi_type pointer array on the stack: [context, params...]
    let mut arg_types: [*mut ffi_type; MAX_EXTERN_ARGS] = [std::ptr::null_mut(); MAX_EXTERN_ARGS];
    arg_types[0] = &mut types::pointer;
    for (i, pt) in param_types.iter().enumerate() {
        arg_types[i + 1] = extern_type_to_ffi_ptr(*pt);
    }

    // Prepare the CIF on the stack.
    let mut cif: ffi_cif = Default::default();
    low::prep_cif(
        &mut cif,
        ffi_abi_FFI_DEFAULT_ABI,
        n + 1,
        extern_type_to_ffi_ptr(ret_type),
        arg_types.as_mut_ptr(),
    ).expect("ffi_prep_cif failed");

    // Store argument values on the stack as u64. On little-endian (ARM64/x86_64),
    // a pointer to the u64 works as a pointer to the i32/f32 value in its low bytes,
    // which is what libffi expects — it reads the correct number of bytes based on ffi_type.
    let mut vals: [u64; MAX_EXTERN_ARGS] = [0; MAX_EXTERN_ARGS];
    vals[0] = context as u64;
    for (i, pt) in param_types.iter().enumerate() {
        vals[i + 1] = match pt {
            ExternType::I32 => registers[args_start + i] as i32 as u32 as u64,
            ExternType::F32 => (registers[args_start + i] as u32) as u64,
            ExternType::F64 | ExternType::Ptr => registers[args_start + i],
            ExternType::Void => 0,
        };
    }

    // Build the args pointer array: each entry points to the corresponding value.
    let mut arg_ptrs: [*mut c_void; MAX_EXTERN_ARGS] = [std::ptr::null_mut(); MAX_EXTERN_ARGS];
    for i in 0..n + 1 {
        arg_ptrs[i] = &mut vals[i] as *mut u64 as *mut c_void;
    }

    let code = CodePtr(fn_ptr as *mut c_void);

    // Call and convert result to u64.
    match ret_type {
        ExternType::Void => { low::call::<()>(&mut cif, code, arg_ptrs.as_mut_ptr()); 0 }
        ExternType::I32 => low::call::<i32>(&mut cif, code, arg_ptrs.as_mut_ptr()) as u64,
        ExternType::F32 => low::call::<f32>(&mut cif, code, arg_ptrs.as_mut_ptr()).to_bits() as u64,
        ExternType::F64 => low::call::<f64>(&mut cif, code, arg_ptrs.as_mut_ptr()).to_bits(),
        ExternType::Ptr => low::call::<*mut u8>(&mut cif, code, arg_ptrs.as_mut_ptr()) as u64,
    }
}

/// Get a pointer to the static ffi_type for an ExternType.
unsafe fn extern_type_to_ffi_ptr(t: ExternType) -> *mut libffi::low::ffi_type {
    use libffi::low::types;
    match t {
        ExternType::Void => &raw mut types::void,
        ExternType::I32 => &raw mut types::sint32,
        ExternType::F32 => &raw mut types::float,
        ExternType::F64 => &raw mut types::double,
        ExternType::Ptr => &raw mut types::pointer,
    }
}

/// Linked program: all function code flattened into packed bytecode
pub struct LinkedProgram {
    pub ops: Vec<PackedOp>,
    pub(crate) func_offsets: Vec<usize>,
    pub(crate) func_locals: Vec<u32>,
    /// Pool for i64 values that don't fit in 16-bit immediate
    pub(crate) wide_i64: Vec<i64>,
    /// Pool for f64 values
    pub(crate) wide_f64: Vec<f64>,
    /// Pool for f32 values (can't fit 32-bit f32 in 16-bit D field)
    pub(crate) f32_pool: Vec<f32>,
}

impl LinkedProgram {
    pub fn from_program(program: &VMProgram) -> Self {
        let mut ops = Vec::new();
        let mut func_offsets = Vec::with_capacity(program.functions.len());
        let mut func_locals = Vec::with_capacity(program.functions.len());
        let mut wide_i64 = Vec::new();
        let mut wide_f64 = Vec::new();
        let mut f32_pool = Vec::new();

        for func in &program.functions {
            func_offsets.push(ops.len());
            func_locals.push(func.locals_size);

            // Build mapping from opcode index → packed index (for jump fixups)
            // MemEq/MemNe/SliceEq/SliceNe emit 2 words, all others emit 1.
            let mut opcode_to_packed: Vec<usize> = Vec::with_capacity(func.code.len());
            for op in &func.code {
                opcode_to_packed.push(ops.len());
                Self::pack_opcode(op, &mut ops, &mut wide_i64, &mut wide_f64, &mut f32_pool);
            }
            // Sentinel for "one past the last instruction"
            opcode_to_packed.push(ops.len());

            // Fix up jump offsets if any instructions expanded (MemEq/MemNe/SliceEq/SliceNe → 2 words)
            let _func_start = func_offsets[func_offsets.len() - 1];
            for (opcode_idx, op) in func.code.iter().enumerate() {
                let packed_idx = opcode_to_packed[opcode_idx];
                match *op {
                    Opcode::Jump { offset } => {
                        let target_opcode = (opcode_idx as i32 + 1 + offset) as usize;
                        let target_packed = opcode_to_packed[target_opcode];
                        let new_offset = target_packed as i32 - packed_idx as i32 - 1;
                        ops[packed_idx] = PackedOp::ad(tags::JUMP, 0, new_offset as i16);
                    }
                    Opcode::JumpIfZero { cond, offset } => {
                        let target_opcode = (opcode_idx as i32 + 1 + offset) as usize;
                        let target_packed = opcode_to_packed[target_opcode];
                        let new_offset = target_packed as i32 - packed_idx as i32 - 1;
                        ops[packed_idx] =
                            PackedOp::ad(tags::JUMP_IF_ZERO, cond as u8, new_offset as i16);
                    }
                    Opcode::JumpIfNotZero { cond, offset } => {
                        let target_opcode = (opcode_idx as i32 + 1 + offset) as usize;
                        let target_packed = opcode_to_packed[target_opcode];
                        let new_offset = target_packed as i32 - packed_idx as i32 - 1;
                        ops[packed_idx] =
                            PackedOp::ad(tags::JUMP_IF_NOT_ZERO, cond as u8, new_offset as i16);
                    }
                    Opcode::ILtJump { a, b, offset } => {
                        let target_opcode = (opcode_idx as i32 + 1 + offset) as usize;
                        let target_packed = opcode_to_packed[target_opcode];
                        // Wide format: 2 packed words. ip advances past both, so offset
                        // is relative to packed_idx + 2 (the word after the data word).
                        let new_offset = target_packed as i32 - (packed_idx as i32 + 2);
                        ops[packed_idx] =
                            PackedOp::abc(tags::ILT_JUMP_WIDE, a as u8, b as u8, 0);
                        ops[packed_idx + 1] = PackedOp::data(new_offset as u32);
                    }
                    Opcode::FLtJump { a, b, offset } => {
                        let target_opcode = (opcode_idx as i32 + 1 + offset) as usize;
                        let target_packed = opcode_to_packed[target_opcode];
                        let new_offset = target_packed as i32 - (packed_idx as i32 + 2);
                        ops[packed_idx] =
                            PackedOp::abc(tags::FLT_JUMP_WIDE, a as u8, b as u8, 0);
                        ops[packed_idx + 1] = PackedOp::data(new_offset as u32);
                    }
                    _ => {}
                }
            }

            // Insert implicit return if needed
            match func.code.last() {
                Some(Opcode::Return) | Some(Opcode::ReturnReg { .. }) | Some(Opcode::Halt) => {}
                _ => ops.push(PackedOp::abc(tags::RETURN, 0, 0, 0)),
            }
        }

        LinkedProgram {
            ops,
            func_offsets,
            func_locals,
            wide_i64,
            wide_f64,
            f32_pool,
        }
    }

    fn pack_opcode(
        op: &Opcode,
        ops: &mut Vec<PackedOp>,
        wide_i64: &mut Vec<i64>,
        wide_f64: &mut Vec<f64>,
        f32_pool: &mut Vec<f32>,
    ) {
        // Helper: truncate Reg (u16) to u8 for packing (valid post-register-allocation).
        let r = |reg: Reg| -> u8 { reg as u8 };
        let packed = match *op {
            Opcode::Nop => PackedOp::abc(tags::NOP, 0, 0, 0),
            Opcode::Halt => PackedOp::abc(tags::HALT, 0, 0, 0),
            Opcode::Move { dst, src } => PackedOp::abc(tags::MOVE, r(dst), r(src), 0),
            Opcode::LoadImm { dst, value } => {
                if value >= i16::MIN as i64 && value <= i16::MAX as i64 {
                    PackedOp::ad(tags::LOAD_IMM, r(dst), value as i16)
                } else {
                    let idx = wide_i64.len();
                    wide_i64.push(value);
                    PackedOp::ad(tags::LOAD_IMM_WIDE, r(dst), idx as i16)
                }
            }
            Opcode::LoadF32 { dst, value } => {
                let idx = f32_pool.len();
                f32_pool.push(value);
                PackedOp::ad(tags::LOAD_F32, r(dst), idx as i16)
            }
            Opcode::LoadF64 { dst, value } => {
                let idx = wide_f64.len();
                wide_f64.push(value);
                PackedOp::ad(tags::LOAD_F64_WIDE, r(dst), idx as i16)
            }
            Opcode::LoadConst { dst, idx } => PackedOp::ad(tags::LOAD_CONST, r(dst), idx as i16),
            // Integer arithmetic — ABC
            Opcode::IAdd { dst, a, b } => PackedOp::abc(tags::IADD, r(dst), r(a), r(b)),
            Opcode::ISub { dst, a, b } => PackedOp::abc(tags::ISUB, r(dst), r(a), r(b)),
            Opcode::IMul { dst, a, b } => PackedOp::abc(tags::IMUL, r(dst), r(a), r(b)),
            Opcode::IDiv { dst, a, b } => PackedOp::abc(tags::IDIV, r(dst), r(a), r(b)),
            Opcode::UDiv { dst, a, b } => PackedOp::abc(tags::UDIV, r(dst), r(a), r(b)),
            Opcode::IRem { dst, a, b } => PackedOp::abc(tags::IREM, r(dst), r(a), r(b)),
            Opcode::IPow { dst, a, b } => PackedOp::abc(tags::IPOW, r(dst), r(a), r(b)),
            Opcode::INeg { dst, src } => PackedOp::abc(tags::INEG, r(dst), r(src), 0),
            Opcode::IAddImm { dst, src, imm } => {
                PackedOp::abc(tags::IADD_IMM, r(dst), r(src), imm as i8 as u8)
            }
            // Float32 arithmetic — ABC
            Opcode::FAdd { dst, a, b } => PackedOp::abc(tags::FADD, r(dst), r(a), r(b)),
            Opcode::FSub { dst, a, b } => PackedOp::abc(tags::FSUB, r(dst), r(a), r(b)),
            Opcode::FMul { dst, a, b } => PackedOp::abc(tags::FMUL, r(dst), r(a), r(b)),
            Opcode::FDiv { dst, a, b } => PackedOp::abc(tags::FDIV, r(dst), r(a), r(b)),
            Opcode::FNeg { dst, src } => PackedOp::abc(tags::FNEG, r(dst), r(src), 0),
            Opcode::FPow { dst, a, b } => PackedOp::abc(tags::FPOW, r(dst), r(a), r(b)),
            Opcode::FMulAdd { dst, a, b, c } => {
                ops.push(PackedOp::abc(tags::FMUL_ADD, r(dst), r(a), r(b)));
                ops.push(PackedOp::data(r(c) as u32));
                return;
            }
            Opcode::FMulSub { dst, a, b, c } => {
                ops.push(PackedOp::abc(tags::FMUL_SUB, r(dst), r(a), r(b)));
                ops.push(PackedOp::data(r(c) as u32));
                return;
            }
            Opcode::FNMulAdd { dst, a, b, c } => {
                ops.push(PackedOp::abc(tags::FNMUL_ADD, r(dst), r(a), r(b)));
                ops.push(PackedOp::data(r(c) as u32));
                return;
            }
            // Float64 arithmetic — ABC
            Opcode::DAdd { dst, a, b } => PackedOp::abc(tags::DADD, r(dst), r(a), r(b)),
            Opcode::DSub { dst, a, b } => PackedOp::abc(tags::DSUB, r(dst), r(a), r(b)),
            Opcode::DMul { dst, a, b } => PackedOp::abc(tags::DMUL, r(dst), r(a), r(b)),
            Opcode::DDiv { dst, a, b } => PackedOp::abc(tags::DDIV, r(dst), r(a), r(b)),
            Opcode::DNeg { dst, src } => PackedOp::abc(tags::DNEG, r(dst), r(src), 0),
            Opcode::DPow { dst, a, b } => PackedOp::abc(tags::DPOW, r(dst), r(a), r(b)),
            Opcode::DMulAdd { dst, a, b, c } => {
                ops.push(PackedOp::abc(tags::DMUL_ADD, r(dst), r(a), r(b)));
                ops.push(PackedOp::data(r(c) as u32));
                return;
            }
            Opcode::DMulSub { dst, a, b, c } => {
                ops.push(PackedOp::abc(tags::DMUL_SUB, r(dst), r(a), r(b)));
                ops.push(PackedOp::data(r(c) as u32));
                return;
            }
            Opcode::DNMulAdd { dst, a, b, c } => {
                ops.push(PackedOp::abc(tags::DNMUL_ADD, r(dst), r(a), r(b)));
                ops.push(PackedOp::data(r(c) as u32));
                return;
            }
            // SIMD f32x4 — ABC (ptr-represented)
            Opcode::F32x4Add { dst, a, b } => PackedOp::abc(tags::F32X4_ADD, r(dst), r(a), r(b)),
            Opcode::F32x4Sub { dst, a, b } => PackedOp::abc(tags::F32X4_SUB, r(dst), r(a), r(b)),
            Opcode::F32x4Mul { dst, a, b } => PackedOp::abc(tags::F32X4_MUL, r(dst), r(a), r(b)),
            Opcode::F32x4Div { dst, a, b } => PackedOp::abc(tags::F32X4_DIV, r(dst), r(a), r(b)),
            Opcode::F32x4Neg { dst, src } => PackedOp::abc(tags::F32X4_NEG, r(dst), r(src), 0),
            // Bitwise — ABC
            Opcode::And { dst, a, b } => PackedOp::abc(tags::AND, r(dst), r(a), r(b)),
            Opcode::Or { dst, a, b } => PackedOp::abc(tags::OR, r(dst), r(a), r(b)),
            Opcode::Xor { dst, a, b } => PackedOp::abc(tags::XOR, r(dst), r(a), r(b)),
            Opcode::Not { dst, src } => PackedOp::abc(tags::NOT, r(dst), r(src), 0),
            Opcode::Shl { dst, a, b } => PackedOp::abc(tags::SHL, r(dst), r(a), r(b)),
            Opcode::Shr { dst, a, b } => PackedOp::abc(tags::SHR, r(dst), r(a), r(b)),
            Opcode::UShr { dst, a, b } => PackedOp::abc(tags::USHR, r(dst), r(a), r(b)),
            // Comparisons — ABC
            Opcode::IEq { dst, a, b } => PackedOp::abc(tags::IEQ, r(dst), r(a), r(b)),
            Opcode::INe { dst, a, b } => PackedOp::abc(tags::INE, r(dst), r(a), r(b)),
            Opcode::ILt { dst, a, b } => PackedOp::abc(tags::ILT, r(dst), r(a), r(b)),
            Opcode::ILe { dst, a, b } => PackedOp::abc(tags::ILE, r(dst), r(a), r(b)),
            Opcode::ULt { dst, a, b } => PackedOp::abc(tags::ULT, r(dst), r(a), r(b)),
            Opcode::FEq { dst, a, b } => PackedOp::abc(tags::FEQ, r(dst), r(a), r(b)),
            Opcode::FNe { dst, a, b } => PackedOp::abc(tags::FNE, r(dst), r(a), r(b)),
            Opcode::FLt { dst, a, b } => PackedOp::abc(tags::FLT, r(dst), r(a), r(b)),
            Opcode::FLe { dst, a, b } => PackedOp::abc(tags::FLE, r(dst), r(a), r(b)),
            // MemEq/MemNe — extended: ABC + data word for size
            Opcode::MemEq { dst, a, b, size } => {
                ops.push(PackedOp::abc(tags::MEM_EQ, r(dst), r(a), r(b)));
                ops.push(PackedOp::data(size));
                return;
            }
            Opcode::MemNe { dst, a, b, size } => {
                ops.push(PackedOp::abc(tags::MEM_NE, r(dst), r(a), r(b)));
                ops.push(PackedOp::data(size));
                return;
            }
            Opcode::SliceEq {
                dst,
                a,
                b,
                elem_size,
            } => {
                ops.push(PackedOp::abc(tags::SLICE_EQ, r(dst), r(a), r(b)));
                ops.push(PackedOp::data(elem_size));
                return;
            }
            Opcode::SliceNe {
                dst,
                a,
                b,
                elem_size,
            } => {
                ops.push(PackedOp::abc(tags::SLICE_NE, r(dst), r(a), r(b)));
                ops.push(PackedOp::data(elem_size));
                return;
            }
            Opcode::SliceLoad32 { dst, slice, index } => {
                PackedOp::abc(tags::SLICE_LOAD32, r(dst), r(slice), r(index))
            }
            Opcode::SliceStore32 { slice, index, src } => {
                PackedOp::abc(tags::SLICE_STORE32, r(src), r(slice), r(index))
            }
            Opcode::DEq { dst, a, b } => PackedOp::abc(tags::DEQ, r(dst), r(a), r(b)),
            Opcode::DLt { dst, a, b } => PackedOp::abc(tags::DLT, r(dst), r(a), r(b)),
            Opcode::DLe { dst, a, b } => PackedOp::abc(tags::DLE, r(dst), r(a), r(b)),
            // Type conversions — AB
            Opcode::I32ToF32 { dst, src } => PackedOp::abc(tags::I32_TO_F32, r(dst), r(src), 0),
            Opcode::F32ToI32 { dst, src } => PackedOp::abc(tags::F32_TO_I32, r(dst), r(src), 0),
            Opcode::I32ToF64 { dst, src } => PackedOp::abc(tags::I32_TO_F64, r(dst), r(src), 0),
            Opcode::F64ToI32 { dst, src } => PackedOp::abc(tags::F64_TO_I32, r(dst), r(src), 0),
            Opcode::F32ToF64 { dst, src } => PackedOp::abc(tags::F32_TO_F64, r(dst), r(src), 0),
            Opcode::F64ToF32 { dst, src } => PackedOp::abc(tags::F64_TO_F32, r(dst), r(src), 0),
            Opcode::I32ToI8 { dst, src } => PackedOp::abc(tags::I32_TO_I8, r(dst), r(src), 0),
            Opcode::I8ToI32 { dst, src } => PackedOp::abc(tags::I8_TO_I32, r(dst), r(src), 0),
            Opcode::I64ToU32 { dst, src } => PackedOp::abc(tags::I64_TO_U32, r(dst), r(src), 0),
            // Memory — AB or ABC
            Opcode::Load8 { dst, addr } => PackedOp::abc(tags::LOAD8, r(dst), r(addr), 0),
            Opcode::Load32 { dst, addr } => PackedOp::abc(tags::LOAD32, r(dst), r(addr), 0),
            Opcode::Load64 { dst, addr } => PackedOp::abc(tags::LOAD64, r(dst), r(addr), 0),
            Opcode::Load32Off { dst, base, offset } => {
                if offset >= 0 && offset <= 255 {
                    PackedOp::abc(tags::LOAD32_OFF, r(dst), r(base), offset as u8)
                } else {
                    ops.push(PackedOp::abc(tags::LOAD32_OFF_WIDE, r(dst), r(base), 0));
                    ops.push(PackedOp::data(offset as u32));
                    return;
                }
            }
            Opcode::Load64Off { dst, base, offset } => {
                if offset >= 0 && offset <= 255 {
                    PackedOp::abc(tags::LOAD64_OFF, r(dst), r(base), offset as u8)
                } else {
                    ops.push(PackedOp::abc(tags::LOAD64_OFF_WIDE, r(dst), r(base), 0));
                    ops.push(PackedOp::data(offset as u32));
                    return;
                }
            }
            Opcode::Store8 { addr, src } => PackedOp::abc(tags::STORE8, r(addr), r(src), 0),
            Opcode::Store32 { addr, src } => PackedOp::abc(tags::STORE32, r(addr), r(src), 0),
            Opcode::Store64 { addr, src } => PackedOp::abc(tags::STORE64, r(addr), r(src), 0),
            Opcode::Store8Off { base, offset, src } => {
                if offset >= 0 && offset <= 255 {
                    PackedOp::abc(tags::STORE8_OFF, r(base), r(src), offset as u8)
                } else {
                    ops.push(PackedOp::abc(tags::STORE8_OFF_WIDE, r(base), r(src), 0));
                    ops.push(PackedOp::data(offset as u32));
                    return;
                }
            }
            Opcode::Store32Off { base, offset, src } => {
                if offset >= 0 && offset <= 255 {
                    PackedOp::abc(tags::STORE32_OFF, r(base), r(src), offset as u8)
                } else {
                    ops.push(PackedOp::abc(tags::STORE32_OFF_WIDE, r(base), r(src), 0));
                    ops.push(PackedOp::data(offset as u32));
                    return;
                }
            }
            Opcode::Store64Off { base, offset, src } => {
                if offset >= 0 && offset <= 255 {
                    PackedOp::abc(tags::STORE64_OFF, r(base), r(src), offset as u8)
                } else {
                    ops.push(PackedOp::abc(tags::STORE64_OFF_WIDE, r(base), r(src), 0));
                    ops.push(PackedOp::data(offset as u32));
                    return;
                }
            }
            // Addressing — AD
            Opcode::LocalAddr { dst, slot } => PackedOp::ad(tags::LOCAL_ADDR, r(dst), slot as i16),
            Opcode::GlobalAddr { dst, offset } => {
                PackedOp::ad(tags::GLOBAL_ADDR, r(dst), offset as i16)
            }
            // Control flow — jump offsets are placeholders, fixed up after packing
            Opcode::Jump { offset } => PackedOp::ad(tags::JUMP, 0, offset as i16),
            Opcode::JumpIfZero { cond, offset } => {
                PackedOp::ad(tags::JUMP_IF_ZERO, r(cond), offset as i16)
            }
            Opcode::JumpIfNotZero { cond, offset } => {
                PackedOp::ad(tags::JUMP_IF_NOT_ZERO, r(cond), offset as i16)
            }
            Opcode::ILtJump { a, b, offset } => {
                // Always emit wide (2-word) format; fixup will set the correct offset.
                ops.push(PackedOp::abc(tags::ILT_JUMP_WIDE, r(a), r(b), 0));
                ops.push(PackedOp::data(offset as u32));
                return;
            }
            Opcode::FLtJump { a, b, offset } => {
                ops.push(PackedOp::abc(tags::FLT_JUMP_WIDE, r(a), r(b), 0));
                ops.push(PackedOp::data(offset as u32));
                return;
            }
            // Superinstructions — AD
            Opcode::LoadSlot32 { dst, slot } => {
                PackedOp::ad(tags::LOAD_SLOT32, r(dst), slot as i16)
            }
            Opcode::StoreSlot32 { slot, src } => {
                PackedOp::ad(tags::STORE_SLOT32, r(src), slot as i16)
            }
            // Calls — ABC
            Opcode::Call {
                func,
                args_start,
                arg_count,
            } => {
                assert!(func <= 255, "Call func index {} out of u8 range", func);
                PackedOp::abc(tags::CALL, r(args_start), arg_count, func as u8)
            }
            Opcode::CallIndirect {
                func_reg,
                args_start,
                arg_count,
            } => PackedOp::abc(tags::CALL_INDIRECT, r(func_reg), r(args_start), arg_count),
            Opcode::CallClosure {
                fat_ptr,
                args_start,
                arg_count,
            } => PackedOp::abc(tags::CALL_CLOSURE, r(fat_ptr), r(args_start), arg_count),
            Opcode::CallExtern {
                args_start,
                arg_count,
                globals_offset,
            } => {
                ops.push(PackedOp::abc(tags::CALL_EXTERN, r(args_start), arg_count, 0));
                ops.push(PackedOp::data(globals_offset as u32));
                return;
            }
            Opcode::GetClosurePtr { dst } => PackedOp::abc(tags::GET_CLOSURE_PTR, r(dst), 0, 0),
            Opcode::Return => PackedOp::abc(tags::RETURN, 0, 0, 0),
            Opcode::ReturnReg { src } => PackedOp::abc(tags::RETURN_REG, r(src), 0, 0),
            // Stack frame — AD or ABC
            Opcode::AllocLocals { size } => {
                assert!(
                    size <= u16::MAX as u32,
                    "AllocLocals size {} out of u16 range",
                    size
                );
                PackedOp::ad(tags::ALLOC_LOCALS, 0, size as u16 as i16)
            }
            Opcode::MemCopy { dst, src, size } => {
                if size <= 255 {
                    PackedOp::abc(tags::MEM_COPY, r(dst), r(src), size as u8)
                } else {
                    ops.push(PackedOp::abc(tags::MEM_COPY_WIDE, r(dst), r(src), 0));
                    ops.push(PackedOp::data(size));
                    return;
                }
            }
            Opcode::MemZero { dst, size } => {
                assert!(
                    size <= u16::MAX as u32,
                    "MemZero size {} out of u16 range",
                    size
                );
                PackedOp::ad(tags::MEM_ZERO, r(dst), size as u16 as i16)
            }
            Opcode::SaveRegs {
                start_reg,
                count,
                slot,
            } => {
                if slot % 8 == 0 && slot / 8 <= 255 {
                    PackedOp::abc(tags::SAVE_REGS, r(start_reg), count, (slot / 8) as u8)
                } else {
                    ops.push(PackedOp::abc(tags::SAVE_REGS_WIDE, r(start_reg), count, 0));
                    ops.push(PackedOp::data(slot));
                    return;
                }
            }
            Opcode::RestoreRegs {
                start_reg,
                count,
                slot,
            } => {
                if slot % 8 == 0 && slot / 8 <= 255 {
                    PackedOp::abc(tags::RESTORE_REGS, r(start_reg), count, (slot / 8) as u8)
                } else {
                    ops.push(PackedOp::abc(
                        tags::RESTORE_REGS_WIDE,
                        r(start_reg),
                        count,
                        0,
                    ));
                    ops.push(PackedOp::data(slot));
                    return;
                }
            }
            // Debugging
            Opcode::PrintI32 { src } => PackedOp::abc(tags::PRINT_I32, r(src), 0, 0),
            Opcode::PrintF32 { src } => PackedOp::abc(tags::PRINT_F32, r(src), 0, 0),
            Opcode::Assert { src } => PackedOp::abc(tags::ASSERT, r(src), 0, 0),
            Opcode::Putc { src } => PackedOp::abc(tags::PUTC, r(src), 0, 0),
            // Math builtins — unary f32
            Opcode::SinF32 { dst, src } => PackedOp::abc(tags::SIN_F32, r(dst), r(src), 0),
            Opcode::CosF32 { dst, src } => PackedOp::abc(tags::COS_F32, r(dst), r(src), 0),
            Opcode::TanF32 { dst, src } => PackedOp::abc(tags::TAN_F32, r(dst), r(src), 0),
            Opcode::AsinF32 { dst, src } => PackedOp::abc(tags::ASIN_F32, r(dst), r(src), 0),
            Opcode::AcosF32 { dst, src } => PackedOp::abc(tags::ACOS_F32, r(dst), r(src), 0),
            Opcode::AtanF32 { dst, src } => PackedOp::abc(tags::ATAN_F32, r(dst), r(src), 0),
            Opcode::SinhF32 { dst, src } => PackedOp::abc(tags::SINH_F32, r(dst), r(src), 0),
            Opcode::CoshF32 { dst, src } => PackedOp::abc(tags::COSH_F32, r(dst), r(src), 0),
            Opcode::TanhF32 { dst, src } => PackedOp::abc(tags::TANH_F32, r(dst), r(src), 0),
            Opcode::AsinhF32 { dst, src } => PackedOp::abc(tags::ASINH_F32, r(dst), r(src), 0),
            Opcode::AcoshF32 { dst, src } => PackedOp::abc(tags::ACOSH_F32, r(dst), r(src), 0),
            Opcode::AtanhF32 { dst, src } => PackedOp::abc(tags::ATANH_F32, r(dst), r(src), 0),
            Opcode::LnF32 { dst, src } => PackedOp::abc(tags::LN_F32, r(dst), r(src), 0),
            Opcode::ExpF32 { dst, src } => PackedOp::abc(tags::EXP_F32, r(dst), r(src), 0),
            Opcode::Exp2F32 { dst, src } => PackedOp::abc(tags::EXP2_F32, r(dst), r(src), 0),
            Opcode::Log10F32 { dst, src } => PackedOp::abc(tags::LOG10_F32, r(dst), r(src), 0),
            Opcode::Log2F32 { dst, src } => PackedOp::abc(tags::LOG2_F32, r(dst), r(src), 0),
            Opcode::SqrtF32 { dst, src } => PackedOp::abc(tags::SQRT_F32, r(dst), r(src), 0),
            Opcode::AbsF32 { dst, src } => PackedOp::abc(tags::ABS_F32, r(dst), r(src), 0),
            Opcode::FloorF32 { dst, src } => PackedOp::abc(tags::FLOOR_F32, r(dst), r(src), 0),
            Opcode::CeilF32 { dst, src } => PackedOp::abc(tags::CEIL_F32, r(dst), r(src), 0),
            // Math builtins — unary f64
            Opcode::SinF64 { dst, src } => PackedOp::abc(tags::SIN_F64, r(dst), r(src), 0),
            Opcode::CosF64 { dst, src } => PackedOp::abc(tags::COS_F64, r(dst), r(src), 0),
            Opcode::TanF64 { dst, src } => PackedOp::abc(tags::TAN_F64, r(dst), r(src), 0),
            Opcode::AsinF64 { dst, src } => PackedOp::abc(tags::ASIN_F64, r(dst), r(src), 0),
            Opcode::AcosF64 { dst, src } => PackedOp::abc(tags::ACOS_F64, r(dst), r(src), 0),
            Opcode::AtanF64 { dst, src } => PackedOp::abc(tags::ATAN_F64, r(dst), r(src), 0),
            Opcode::SinhF64 { dst, src } => PackedOp::abc(tags::SINH_F64, r(dst), r(src), 0),
            Opcode::CoshF64 { dst, src } => PackedOp::abc(tags::COSH_F64, r(dst), r(src), 0),
            Opcode::TanhF64 { dst, src } => PackedOp::abc(tags::TANH_F64, r(dst), r(src), 0),
            Opcode::AsinhF64 { dst, src } => PackedOp::abc(tags::ASINH_F64, r(dst), r(src), 0),
            Opcode::AcoshF64 { dst, src } => PackedOp::abc(tags::ACOSH_F64, r(dst), r(src), 0),
            Opcode::AtanhF64 { dst, src } => PackedOp::abc(tags::ATANH_F64, r(dst), r(src), 0),
            Opcode::LnF64 { dst, src } => PackedOp::abc(tags::LN_F64, r(dst), r(src), 0),
            Opcode::ExpF64 { dst, src } => PackedOp::abc(tags::EXP_F64, r(dst), r(src), 0),
            Opcode::Exp2F64 { dst, src } => PackedOp::abc(tags::EXP2_F64, r(dst), r(src), 0),
            Opcode::Log10F64 { dst, src } => PackedOp::abc(tags::LOG10_F64, r(dst), r(src), 0),
            Opcode::Log2F64 { dst, src } => PackedOp::abc(tags::LOG2_F64, r(dst), r(src), 0),
            Opcode::SqrtF64 { dst, src } => PackedOp::abc(tags::SQRT_F64, r(dst), r(src), 0),
            Opcode::AbsF64 { dst, src } => PackedOp::abc(tags::ABS_F64, r(dst), r(src), 0),
            Opcode::FloorF64 { dst, src } => PackedOp::abc(tags::FLOOR_F64, r(dst), r(src), 0),
            Opcode::CeilF64 { dst, src } => PackedOp::abc(tags::CEIL_F64, r(dst), r(src), 0),
            // Math builtins — predicates
            Opcode::IsinfF32 { dst, src } => PackedOp::abc(tags::ISINF_F32, r(dst), r(src), 0),
            Opcode::IsinfF64 { dst, src } => PackedOp::abc(tags::ISINF_F64, r(dst), r(src), 0),
            Opcode::IsnanF32 { dst, src } => PackedOp::abc(tags::ISNAN_F32, r(dst), r(src), 0),
            Opcode::IsnanF64 { dst, src } => PackedOp::abc(tags::ISNAN_F64, r(dst), r(src), 0),
            // Math builtins — binary f32
            Opcode::PowF32 { dst, a, b } => PackedOp::abc(tags::POW_F32, r(dst), r(a), r(b)),
            Opcode::Atan2F32 { dst, a, b } => PackedOp::abc(tags::ATAN2_F32, r(dst), r(a), r(b)),
            Opcode::MinF32 { dst, a, b } => PackedOp::abc(tags::MIN_F32, r(dst), r(a), r(b)),
            Opcode::MaxF32 { dst, a, b } => PackedOp::abc(tags::MAX_F32, r(dst), r(a), r(b)),
            // Math builtins — binary f64
            Opcode::PowF64 { dst, a, b } => PackedOp::abc(tags::POW_F64, r(dst), r(a), r(b)),
            Opcode::Atan2F64 { dst, a, b } => PackedOp::abc(tags::ATAN2_F64, r(dst), r(a), r(b)),
            Opcode::MinF64 { dst, a, b } => PackedOp::abc(tags::MIN_F64, r(dst), r(a), r(b)),
            Opcode::MaxF64 { dst, a, b } => PackedOp::abc(tags::MAX_F64, r(dst), r(a), r(b)),
        };
        ops.push(packed);
    }
}

/// A compiled function for the VM
#[derive(Clone, Debug)]
pub struct VMFunction {
    /// Function name for debugging
    pub name: String,

    /// Number of parameters
    pub param_count: u8,

    /// Number of local variable slots (in 8-byte units)
    pub local_slots: u16,

    /// Size of locals area in bytes
    pub locals_size: u32,

    /// The instruction stream
    pub code: Vec<Opcode>,

    /// Debug info: register → variable name (for register-promoted scalars)
    pub reg_names: Vec<(Reg, String)>,

    /// Debug info: local slot → variable name (for slot-based variables)
    pub slot_names: Vec<(u16, String)>,
}

impl VMFunction {
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            param_count: 0,
            local_slots: 0,
            locals_size: 0,
            code: Vec::new(),
            reg_names: Vec::new(),
            slot_names: Vec::new(),
        }
    }

    /// Add an instruction and return its index
    pub fn emit(&mut self, op: Opcode) -> usize {
        let idx = self.code.len();
        self.code.push(op);
        idx
    }

    /// Patch a jump instruction at `idx` to jump to the current position
    pub fn patch_jump(&mut self, idx: usize) {
        let current = self.code.len() as i32;
        let target = idx as i32;
        let offset = current - target - 1;

        match &mut self.code[idx] {
            Opcode::Jump { offset: off }
            | Opcode::JumpIfZero { offset: off, .. }
            | Opcode::JumpIfNotZero { offset: off, .. }
            | Opcode::ILtJump { offset: off, .. }
            | Opcode::FLtJump { offset: off, .. } => {
                *off = offset;
            }
            _ => panic!(
                "patch_jump called on non-jump instruction at index {}: {:?}",
                idx, &self.code[idx]
            ),
        }
    }
}

/// Types that can cross the extern function boundary.
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum ExternType {
    Void,
    I32,
    F32,
    F64,
    /// Pointer (used for slice data pointers and other by-reference args).
    Ptr,
}

/// Descriptor for an extern function.
#[derive(Clone, Debug)]
pub struct ExternFuncInfo {
    /// Byte offset of the {fn_ptr, context} pair in the globals buffer.
    pub globals_offset: i32,
    /// Parameter types (determines how to marshal registers).
    pub param_types: Vec<ExternType>,
    /// Return type.
    pub ret_type: ExternType,
}

/// A program ready to run on the VM
#[derive(Clone, Debug, Default)]
pub struct VMProgram {
    /// All functions in the program
    pub functions: Vec<VMFunction>,

    /// Constant pool (for large constants)
    pub constants: Vec<u64>,

    /// Entry point function index
    pub entry: FuncIdx,

    /// Size of global memory needed (in bytes)
    pub globals_size: usize,

    /// Named entry points (for multi-entry-point programs)
    pub entry_points: std::collections::HashMap<crate::Name, FuncIdx>,

    /// Extern function descriptors, indexed by extern function index.
    pub extern_funcs: Vec<ExternFuncInfo>,
}

impl VMProgram {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn add_function(&mut self, func: VMFunction) -> FuncIdx {
        let idx = self.functions.len() as FuncIdx;
        self.functions.push(func);
        idx
    }
}

/// Call frame for function execution
#[derive(Clone, Debug)]
struct CallFrame {
    /// Function being executed
    func_idx: FuncIdx,

    /// Instruction pointer within the function
    ip: usize,

    /// Base pointer into locals memory
    locals_base: usize,

    /// Register snapshot (for return value handling)
    return_reg: Reg,
}

/// The virtual machine state
pub struct VM {
    /// 256 general-purpose registers (64-bit each)
    pub(crate) registers: [u64; 256],

    /// Memory for local variables (stack-allocated per frame)
    pub(crate) locals: Vec<u8>,

    /// Call stack
    call_stack: Vec<CallFrame>,

    /// Heap memory for dynamic allocations
    heap: Vec<u8>,

    /// Global variable memory (zero-initialized)
    pub(crate) globals: Vec<u8>,

    /// Current instruction pointer
    ip: usize,

    /// Current function index
    pub(crate) current_func: FuncIdx,

    /// Base pointer for current frame's locals
    locals_base: usize,

    /// Current function name for debug diagnostics
    debug_func_name: String,

    /// Set to true if execution was cancelled
    pub cancelled: bool,

    /// Closure pointer set by CallClosure, read by GetClosurePtr.
    closure_ptr: u64,

    /// Cancel callback, called periodically at backward jumps.
    /// Returns true to cancel execution.
    cancel_callback: Option<unsafe extern "C" fn(*mut u8) -> bool>,

    /// User data passed to cancel callback.
    cancel_userdata: *mut u8,
}

impl Default for VM {
    fn default() -> Self {
        Self::new()
    }
}

impl VM {
    pub fn new() -> Self {
        Self {
            registers: [0; 256],
            locals: vec![0; 1024 * 1024], // 1MB stack
            call_stack: Vec::with_capacity(1024),
            heap: vec![0; 1024 * 1024], // 1MB heap
            globals: Vec::new(),        // Allocated when program runs
            ip: 0,
            current_func: 0,
            locals_base: 0,
            debug_func_name: String::new(),
            cancelled: false,
            closure_ptr: 0,
            cancel_callback: None,
            cancel_userdata: std::ptr::null_mut(),
        }
    }

    /// Set a cancel callback that is called periodically at backward jumps.
    /// If it returns true, execution is cancelled.
    pub fn set_cancel_callback(
        &mut self,
        callback: Option<unsafe extern "C" fn(*mut u8) -> bool>,
        user_data: *mut u8,
    ) {
        self.cancel_callback = callback;
        self.cancel_userdata = user_data;
    }

    /// Get register value as i64
    #[inline(always)]
    fn get_i64(&self, r: Reg) -> i64 {
        self.registers[r as usize] as i64
    }

    /// Get register value as u64
    #[inline(always)]
    fn get_u64(&self, r: Reg) -> u64 {
        self.registers[r as usize]
    }

    /// Get register value as f32
    #[inline(always)]
    fn get_f32(&self, r: Reg) -> f32 {
        f32::from_bits(self.registers[r as usize] as u32)
    }

    /// Get register value as f64
    #[inline(always)]
    fn get_f64(&self, r: Reg) -> f64 {
        f64::from_bits(self.registers[r as usize])
    }

    /// Set register value from i64
    #[inline(always)]
    fn set_i64(&mut self, r: Reg, v: i64) {
        self.registers[r as usize] = v as u64;
    }

    /// Set register value from u64
    #[inline(always)]
    fn set_u64(&mut self, r: Reg, v: u64) {
        self.registers[r as usize] = v;
    }

    /// Set register value from f32
    #[inline(always)]
    fn set_f32(&mut self, r: Reg, v: f32) {
        self.registers[r as usize] = v.to_bits() as u64;
    }

    /// Set register value from f64
    #[inline(always)]
    fn set_f64(&mut self, r: Reg, v: f64) {
        self.registers[r as usize] = v.to_bits();
    }

    /// Set register to boolean (0 or 1)
    #[inline(always)]
    fn set_bool(&mut self, r: Reg, v: bool) {
        self.registers[r as usize] = v as u64;
    }

    /// Get pointer to locals memory
    #[inline(always)]
    fn local_ptr(&self, slot: u16) -> *const u8 {
        let offset = self.locals_base + (slot as usize) * 8;
        debug_assert!(
            offset < self.locals.len(),
            "local slot {slot} out of bounds (offset {offset}, locals size {})",
            self.locals.len()
        );
        unsafe { self.locals.as_ptr().add(offset) }
    }

    /// Get mutable pointer to locals memory
    #[inline(always)]
    fn local_ptr_mut(&mut self, slot: u16) -> *mut u8 {
        let offset = self.locals_base + (slot as usize) * 8;
        debug_assert!(
            offset < self.locals.len(),
            "local slot {slot} out of bounds (offset {offset}, locals size {})",
            self.locals.len()
        );
        unsafe { self.locals.as_mut_ptr().add(offset) }
    }

    /// In debug builds, panic if [ptr, ptr+size) is not within locals or globals.
    #[inline(always)]
    #[allow(unused_variables)]
    fn check_ptr(&self, ptr: u64, size: usize) {
        #[cfg(debug_assertions)]
        {
            let start = ptr as usize;
            let end = start + size;

            let locals_start = self.locals.as_ptr() as usize;
            let locals_end = locals_start + self.locals.len();

            let globals_start = self.globals.as_ptr() as usize;
            let globals_end = globals_start + self.globals.len();

            let heap_start = self.heap.as_ptr() as usize;
            let heap_end = heap_start + self.heap.len();

            let in_locals = start >= locals_start && end <= locals_end;
            let in_globals =
                !self.globals.is_empty() && start >= globals_start && end <= globals_end;
            let in_heap = !self.heap.is_empty() && start >= heap_start && end <= heap_end;

            if !in_locals && !in_globals && !in_heap {
                let func = &self.debug_func_name;
                panic!(
                    "VM: out-of-bounds memory access at {:#x}, size {size} (ip={}, func={func})\n  \
                     locals: {locals_start:#x}..{locals_end:#x}\n  \
                     globals: {globals_start:#x}..{globals_end:#x}\n  \
                     heap: {heap_start:#x}..{heap_end:#x}",
                    ptr, self.ip - 1,
                );
            }
        }
    }

    /// Run the program and return the result.
    /// Globals are always re-zeroed.
    pub fn run(&mut self, program: &VMProgram) -> i64 {
        // Always reinitialize globals for run().
        self.globals = vec![0u8; program.globals_size];
        self.run_inner(program, program.entry, &[])
    }

    /// Run a specific function entry point with arguments.
    /// Globals are allocated on first call and preserved on subsequent calls.
    fn run_inner(&mut self, program: &VMProgram, func_idx: FuncIdx, args: &[i64]) -> i64 {
        let linked = LinkedProgram::from_program(program);
        // Allocate globals if not yet allocated.
        if self.globals.is_empty() && program.globals_size > 0 {
            self.globals = vec![0u8; program.globals_size];
        }
        self.run_linked(&linked, program, func_idx, args)
    }

    /// Run a function using a pre-linked program. This is the fast path —
    /// no re-linking, suitable for repeated calls.
    pub fn run_linked(
        &mut self,
        linked: &LinkedProgram,
        program: &VMProgram,
        func_idx: FuncIdx,
        args: &[i64],
    ) -> i64 {
        // === Initialize VM state ===
        self.current_func = func_idx;
        self.registers = [0; 256];

        // Set argument registers.
        for (i, &arg) in args.iter().enumerate() {
            if i < 256 {
                self.registers[i] = arg as u64;
            }
        }

        self.cancelled = false;

        #[cfg(debug_assertions)]
        {
            self.debug_func_name = program.functions[func_idx as usize].name.clone();
        }

        // === Dispatch loop: match on raw u8 tag → LLVM compiles to jump table ===
        // PackedOp is 4 bytes (matching LuaJIT), quartering cache footprint vs Opcode enum.
        // Raw pointer access to registers avoids &mut self borrow conflicts.
        //
        // Hot state kept in locals so LLVM can register-allocate them (like LuaJIT's
        // PC=x21, BASE=x19). Going through `self.field` forces a pointer load each time.
        let ops = linked.ops.as_ptr();
        let regs = self.registers.as_mut_ptr();
        let mut ip: usize = linked.func_offsets[func_idx as usize];
        let mut locals_base: usize = 0;
        let mut cancel_counter: i32 = crate::cancel::CANCEL_CHECK_INTERVAL;

        loop {
            let op = unsafe { *ops.add(ip) };
            ip += 1;

            macro_rules! r {
                ($idx:expr) => {
                    *regs.add($idx as usize)
                };
            }
            macro_rules! r_set {
                ($idx:expr, $val:expr) => {
                    *regs.add($idx as usize) = $val
                };
            }
            macro_rules! r_f32 {
                ($idx:expr) => {
                    f32::from_bits(r!($idx) as u32)
                };
            }
            macro_rules! r_f64 {
                ($idx:expr) => {
                    f64::from_bits(r!($idx))
                };
            }
            macro_rules! set_f32 {
                ($idx:expr, $val:expr) => {
                    r_set!($idx, ($val).to_bits() as u64)
                };
            }
            macro_rules! set_f64 {
                ($idx:expr, $val:expr) => {
                    r_set!($idx, ($val).to_bits())
                };
            }
            macro_rules! set_i64 {
                ($idx:expr, $val:expr) => {
                    r_set!($idx, ($val) as u64)
                };
            }
            macro_rules! get_i64 {
                ($idx:expr) => {
                    r!($idx) as i64
                };
            }

            unsafe {
                match op.tag() {
                    tags::NOP => {}

                    tags::HALT => {
                        return get_i64!(0u8);
                    }

                    tags::MOVE => {
                        r_set!(op.a(), r!(op.b()));
                    }

                    tags::LOAD_IMM => {
                        set_i64!(op.a(), op.d() as i64);
                    }

                    tags::LOAD_IMM_WIDE => {
                        set_i64!(op.a(), linked.wide_i64[op.d_u16() as usize]);
                    }

                    tags::LOAD_F32 => {
                        let val = linked.f32_pool[op.d_u16() as usize];
                        r_set!(op.a(), val.to_bits() as u64);
                    }

                    tags::LOAD_F64_WIDE => {
                        set_f64!(op.a(), linked.wide_f64[op.d_u16() as usize]);
                    }

                    tags::LOAD_CONST => {
                        r_set!(op.a(), program.constants[op.d_u16() as usize]);
                    }

                    // Integer arithmetic — ABC
                    tags::IADD => {
                        set_i64!(op.a(), get_i64!(op.b()).wrapping_add(get_i64!(op.c())));
                    }
                    tags::ISUB => {
                        set_i64!(op.a(), get_i64!(op.b()).wrapping_sub(get_i64!(op.c())));
                    }
                    tags::IMUL => {
                        set_i64!(op.a(), get_i64!(op.b()).wrapping_mul(get_i64!(op.c())));
                    }
                    tags::IDIV => {
                        let b = get_i64!(op.c());
                        if b == 0 {
                            panic!("Division by zero");
                        }
                        set_i64!(op.a(), get_i64!(op.b()) / b);
                    }
                    tags::UDIV => {
                        let b = r!(op.c());
                        if b == 0 {
                            panic!("Division by zero");
                        }
                        r_set!(op.a(), r!(op.b()) / b);
                    }
                    tags::IREM => {
                        let b = get_i64!(op.c());
                        if b == 0 {
                            panic!("Division by zero");
                        }
                        set_i64!(op.a(), get_i64!(op.b()) % b);
                    }
                    tags::IPOW => {
                        set_i64!(
                            op.a(),
                            get_i64!(op.b()).wrapping_pow(get_i64!(op.c()) as u32)
                        );
                    }
                    tags::INEG => {
                        set_i64!(op.a(), -get_i64!(op.b()));
                    }
                    tags::IADD_IMM => {
                        set_i64!(op.a(), get_i64!(op.b()).wrapping_add(op.c_i8() as i64));
                    }

                    // Float32 arithmetic — ABC
                    tags::FADD => {
                        set_f32!(op.a(), r_f32!(op.b()) + r_f32!(op.c()));
                    }
                    tags::FSUB => {
                        set_f32!(op.a(), r_f32!(op.b()) - r_f32!(op.c()));
                    }
                    tags::FMUL => {
                        set_f32!(op.a(), r_f32!(op.b()) * r_f32!(op.c()));
                    }
                    tags::FDIV => {
                        set_f32!(op.a(), r_f32!(op.b()) / r_f32!(op.c()));
                    }
                    tags::FNEG => {
                        set_f32!(op.a(), -r_f32!(op.b()));
                    }
                    tags::FPOW => {
                        set_f32!(op.a(), r_f32!(op.b()).powf(r_f32!(op.c())));
                    }

                    // Float64 arithmetic — ABC
                    tags::DADD => {
                        set_f64!(op.a(), r_f64!(op.b()) + r_f64!(op.c()));
                    }
                    tags::DSUB => {
                        set_f64!(op.a(), r_f64!(op.b()) - r_f64!(op.c()));
                    }
                    tags::DMUL => {
                        set_f64!(op.a(), r_f64!(op.b()) * r_f64!(op.c()));
                    }
                    tags::DDIV => {
                        set_f64!(op.a(), r_f64!(op.b()) / r_f64!(op.c()));
                    }
                    tags::DNEG => {
                        set_f64!(op.a(), -r_f64!(op.b()));
                    }
                    tags::DPOW => {
                        set_f64!(op.a(), r_f64!(op.b()).powf(r_f64!(op.c())));
                    }

                    // SIMD f32x4 — ptr-represented, lane-wise ops
                    tags::F32X4_ADD | tags::F32X4_SUB | tags::F32X4_MUL | tags::F32X4_DIV => {
                        let dst_ptr = r!(op.a()) as *mut f32;
                        let a_ptr = r!(op.b()) as *const f32;
                        let b_ptr = r!(op.c()) as *const f32;
                        for lane in 0..4 {
                            let a_val = *a_ptr.add(lane);
                            let b_val = *b_ptr.add(lane);
                            let result = match op.tag() {
                                tags::F32X4_ADD => a_val + b_val,
                                tags::F32X4_SUB => a_val - b_val,
                                tags::F32X4_MUL => a_val * b_val,
                                tags::F32X4_DIV => a_val / b_val,
                                _ => unreachable!(),
                            };
                            *dst_ptr.add(lane) = result;
                        }
                    }
                    tags::F32X4_NEG => {
                        let dst_ptr = r!(op.a()) as *mut f32;
                        let src_ptr = r!(op.b()) as *const f32;
                        for lane in 0..4 {
                            *dst_ptr.add(lane) = -*src_ptr.add(lane);
                        }
                    }

                    // Bitwise — ABC
                    tags::AND => {
                        r_set!(op.a(), r!(op.b()) & r!(op.c()));
                    }
                    tags::OR => {
                        r_set!(op.a(), r!(op.b()) | r!(op.c()));
                    }
                    tags::XOR => {
                        r_set!(op.a(), r!(op.b()) ^ r!(op.c()));
                    }
                    tags::NOT => {
                        r_set!(op.a(), !r!(op.b()));
                    }
                    tags::SHL => {
                        r_set!(op.a(), r!(op.b()) << (r!(op.c()) & 63));
                    }
                    tags::SHR => {
                        set_i64!(op.a(), get_i64!(op.b()) >> (r!(op.c()) & 63));
                    }
                    tags::USHR => {
                        r_set!(op.a(), r!(op.b()) >> (r!(op.c()) & 63));
                    }

                    // Comparisons — ABC
                    tags::IEQ => {
                        r_set!(op.a(), (get_i64!(op.b()) == get_i64!(op.c())) as u64);
                    }
                    tags::INE => {
                        r_set!(op.a(), (get_i64!(op.b()) != get_i64!(op.c())) as u64);
                    }
                    tags::ILT => {
                        r_set!(op.a(), (get_i64!(op.b()) < get_i64!(op.c())) as u64);
                    }
                    tags::ILE => {
                        r_set!(op.a(), (get_i64!(op.b()) <= get_i64!(op.c())) as u64);
                    }
                    tags::ULT => {
                        r_set!(op.a(), (r!(op.b()) < r!(op.c())) as u64);
                    }
                    tags::FEQ => {
                        r_set!(op.a(), (r_f32!(op.b()) == r_f32!(op.c())) as u64);
                    }
                    tags::FNE => {
                        r_set!(op.a(), (r_f32!(op.b()) != r_f32!(op.c())) as u64);
                    }
                    tags::FLT => {
                        r_set!(op.a(), (r_f32!(op.b()) < r_f32!(op.c())) as u64);
                    }
                    tags::FLE => {
                        r_set!(op.a(), (r_f32!(op.b()) <= r_f32!(op.c())) as u64);
                    }
                    tags::DEQ => {
                        r_set!(op.a(), (r_f64!(op.b()) == r_f64!(op.c())) as u64);
                    }
                    tags::DLT => {
                        r_set!(op.a(), (r_f64!(op.b()) < r_f64!(op.c())) as u64);
                    }
                    tags::DLE => {
                        r_set!(op.a(), (r_f64!(op.b()) <= r_f64!(op.c())) as u64);
                    }

                    // MemEq/MemNe — extended: ABC + next word is size
                    tags::MEM_EQ => {
                        let pa = r!(op.b()) as *const u8;
                        let pb = r!(op.c()) as *const u8;
                        let size = (*ops.add(ip)).0 as usize;
                        ip += 1;
                        let eq = std::slice::from_raw_parts(pa, size)
                            == std::slice::from_raw_parts(pb, size);
                        r_set!(op.a(), eq as u64);
                    }

                    tags::MEM_NE => {
                        let pa = r!(op.b()) as *const u8;
                        let pb = r!(op.c()) as *const u8;
                        let size = (*ops.add(ip)).0 as usize;
                        ip += 1;
                        let ne = std::slice::from_raw_parts(pa, size)
                            != std::slice::from_raw_parts(pb, size);
                        r_set!(op.a(), ne as u64);
                    }

                    // SliceEq/SliceNe — compare slice contents by value
                    // Fat pointer layout: data_ptr (8 bytes) + len (4 bytes)
                    tags::SLICE_EQ => {
                        let fat_a = r!(op.b()) as *const u8;
                        let fat_b = r!(op.c()) as *const u8;
                        let elem_size = (*ops.add(ip)).0 as usize;
                        ip += 1;
                        let ptr_a = *(fat_a as *const u64) as *const u8;
                        let len_a = *(fat_a.add(8) as *const u32) as usize;
                        let ptr_b = *(fat_b as *const u64) as *const u8;
                        let len_b = *(fat_b.add(8) as *const u32) as usize;
                        let eq = len_a == len_b
                            && std::slice::from_raw_parts(ptr_a, len_a * elem_size)
                                == std::slice::from_raw_parts(ptr_b, len_b * elem_size);
                        r_set!(op.a(), eq as u64);
                    }

                    tags::SLICE_NE => {
                        let fat_a = r!(op.b()) as *const u8;
                        let fat_b = r!(op.c()) as *const u8;
                        let elem_size = (*ops.add(ip)).0 as usize;
                        ip += 1;
                        let ptr_a = *(fat_a as *const u64) as *const u8;
                        let len_a = *(fat_a.add(8) as *const u32) as usize;
                        let ptr_b = *(fat_b as *const u64) as *const u8;
                        let len_b = *(fat_b.add(8) as *const u32) as usize;
                        let ne = len_a != len_b
                            || std::slice::from_raw_parts(ptr_a, len_a * elem_size)
                                != std::slice::from_raw_parts(ptr_b, len_b * elem_size);
                        r_set!(op.a(), ne as u64);
                    }

                    // Slice element access — ABC
                    tags::SLICE_LOAD32 => {
                        // B = slice fat pointer, C = index
                        let fat_ptr = r!(op.b()) as *const u8;
                        let data_ptr = *(fat_ptr as *const *const u8);
                        let idx = r!(op.c()) as usize;
                        let elem = *(data_ptr.add(idx * 4) as *const i32);
                        r_set!(op.a(), elem as i64 as u64);
                    }
                    tags::SLICE_STORE32 => {
                        // A = src value, B = slice fat pointer, C = index
                        let fat_ptr = r!(op.b()) as *const u8;
                        let data_ptr = *(fat_ptr as *const *mut u8);
                        let idx = r!(op.c()) as usize;
                        *(data_ptr.add(idx * 4) as *mut i32) = r!(op.a()) as i32;
                    }

                    // Type conversions — AB
                    tags::I32_TO_F32 => {
                        set_f32!(op.a(), (get_i64!(op.b()) as i32) as f32);
                    }
                    tags::F32_TO_I32 => {
                        set_i64!(op.a(), r_f32!(op.b()) as i32 as i64);
                    }
                    tags::I32_TO_F64 => {
                        set_f64!(op.a(), (get_i64!(op.b()) as i32) as f64);
                    }
                    tags::F64_TO_I32 => {
                        set_i64!(op.a(), r_f64!(op.b()) as i32 as i64);
                    }
                    tags::F32_TO_F64 => {
                        set_f64!(op.a(), r_f32!(op.b()) as f64);
                    }
                    tags::F64_TO_F32 => {
                        set_f32!(op.a(), r_f64!(op.b()) as f32);
                    }
                    tags::I32_TO_I8 => {
                        set_i64!(op.a(), (get_i64!(op.b()) as i8) as i64);
                    }
                    tags::I8_TO_I32 => {
                        set_i64!(op.a(), (get_i64!(op.b()) as i8) as i32 as i64);
                    }
                    tags::I64_TO_U32 => {
                        set_i64!(op.a(), (get_i64!(op.b()) as u32) as i64);
                    }

                    // Memory operations
                    tags::LOAD8 => {
                        let ptr = r!(op.b());
                        self.check_ptr(ptr, 1);
                        set_i64!(op.a(), *(ptr as *const u8) as i64);
                    }
                    tags::LOAD32 => {
                        let ptr = r!(op.b());
                        self.check_ptr(ptr, 4);
                        set_i64!(op.a(), *(ptr as *const i32) as i64);
                    }
                    tags::LOAD64 => {
                        let ptr = r!(op.b());
                        self.check_ptr(ptr, 8);
                        set_i64!(op.a(), *(ptr as *const i64));
                    }
                    tags::LOAD32_OFF => {
                        let ptr = r!(op.b()).wrapping_add(op.c() as u64);
                        self.check_ptr(ptr, 4);
                        set_i64!(op.a(), *(ptr as *const i32) as i64);
                    }
                    tags::LOAD32_OFF_WIDE => {
                        let off = (*ops.add(ip)).0 as i64;
                        ip += 1;
                        let ptr = (r!(op.b()) as i64 + off) as u64;
                        self.check_ptr(ptr, 4);
                        set_i64!(op.a(), *(ptr as *const i32) as i64);
                    }
                    tags::LOAD64_OFF => {
                        let ptr = r!(op.b()).wrapping_add(op.c() as u64);
                        self.check_ptr(ptr, 8);
                        set_i64!(op.a(), *(ptr as *const i64));
                    }
                    tags::LOAD64_OFF_WIDE => {
                        let off = (*ops.add(ip)).0 as i64;
                        ip += 1;
                        let ptr = (r!(op.b()) as i64 + off) as u64;
                        self.check_ptr(ptr, 8);
                        set_i64!(op.a(), *(ptr as *const i64));
                    }
                    tags::STORE8 => {
                        let ptr = r!(op.a());
                        self.check_ptr(ptr, 1);
                        *(ptr as *mut u8) = r!(op.b()) as u8;
                    }
                    tags::STORE32 => {
                        let ptr = r!(op.a());
                        self.check_ptr(ptr, 4);
                        *(ptr as *mut i32) = get_i64!(op.b()) as i32;
                    }
                    tags::STORE64 => {
                        let ptr = r!(op.a());
                        self.check_ptr(ptr, 8);
                        *(ptr as *mut i64) = get_i64!(op.b());
                    }
                    tags::STORE8_OFF => {
                        let ptr = r!(op.a()).wrapping_add(op.c() as u64);
                        self.check_ptr(ptr, 1);
                        *(ptr as *mut u8) = r!(op.b()) as u8;
                    }
                    tags::STORE8_OFF_WIDE => {
                        let off = (*ops.add(ip)).0 as i64;
                        ip += 1;
                        let ptr = (r!(op.a()) as i64 + off) as u64;
                        self.check_ptr(ptr, 1);
                        *(ptr as *mut u8) = r!(op.b()) as u8;
                    }
                    tags::STORE32_OFF => {
                        let ptr = r!(op.a()).wrapping_add(op.c() as u64);
                        self.check_ptr(ptr, 4);
                        *(ptr as *mut i32) = get_i64!(op.b()) as i32;
                    }
                    tags::STORE32_OFF_WIDE => {
                        let off = (*ops.add(ip)).0 as i64;
                        ip += 1;
                        let ptr = (r!(op.a()) as i64 + off) as u64;
                        self.check_ptr(ptr, 4);
                        *(ptr as *mut i32) = get_i64!(op.b()) as i32;
                    }
                    tags::STORE64_OFF => {
                        let ptr = r!(op.a()).wrapping_add(op.c() as u64);
                        self.check_ptr(ptr, 8);
                        *(ptr as *mut i64) = get_i64!(op.b());
                    }
                    tags::STORE64_OFF_WIDE => {
                        let off = (*ops.add(ip)).0 as i64;
                        ip += 1;
                        let ptr = (r!(op.a()) as i64 + off) as u64;
                        self.check_ptr(ptr, 8);
                        *(ptr as *mut i64) = get_i64!(op.b());
                    }

                    tags::LOCAL_ADDR => {
                        let offset = locals_base + (op.d_u16() as usize) * 8;
                        r_set!(op.a(), self.locals.as_ptr().add(offset) as u64);
                    }
                    tags::LOAD_SLOT32 => {
                        let offset = locals_base + (op.d_u16() as usize) * 8;
                        let ptr = self.locals.as_ptr().add(offset);
                        set_i64!(op.a(), *(ptr as *const i32) as i64);
                    }
                    tags::STORE_SLOT32 => {
                        let offset = locals_base + (op.d_u16() as usize) * 8;
                        let ptr = self.locals.as_mut_ptr().add(offset);
                        *(ptr as *mut i32) = get_i64!(op.a()) as i32;
                    }

                    tags::GLOBAL_ADDR => {
                        r_set!(
                            op.a(),
                            self.globals.as_ptr().add(op.d_u16() as usize) as u64
                        );
                    }

                    // Control flow — AD format (i16 offset)
                    tags::JUMP => {
                        let off = op.d();
                        ip = (ip as i32 + off as i32) as usize;
                        if off < 0 {
                            cancel_counter -= 1;
                            if cancel_counter <= 0 {
                                cancel_counter = crate::cancel::CANCEL_CHECK_INTERVAL;
                                if let Some(cb) = self.cancel_callback {
                                    if cb(self.cancel_userdata) {
                                        self.cancelled = true;
                                        return 0;
                                    }
                                }
                            }
                        }
                    }

                    tags::JUMP_IF_ZERO => {
                        if r!(op.a()) == 0 {
                            let off = op.d();
                            ip = (ip as i32 + off as i32) as usize;
                            if off < 0 {
                                cancel_counter -= 1;
                                if cancel_counter <= 0 {
                                    cancel_counter = crate::cancel::CANCEL_CHECK_INTERVAL;
                                    if let Some(cb) = self.cancel_callback {
                                        if cb(self.cancel_userdata) {
                                            self.cancelled = true;
                                            return 0;
                                        }
                                    }
                                }
                            }
                        }
                    }

                    tags::JUMP_IF_NOT_ZERO => {
                        if r!(op.a()) != 0 {
                            let off = op.d();
                            ip = (ip as i32 + off as i32) as usize;
                            if off < 0 {
                                cancel_counter -= 1;
                                if cancel_counter <= 0 {
                                    cancel_counter = crate::cancel::CANCEL_CHECK_INTERVAL;
                                    if let Some(cb) = self.cancel_callback {
                                        if cb(self.cancel_userdata) {
                                            self.cancelled = true;
                                            return 0;
                                        }
                                    }
                                }
                            }
                        }
                    }

                    // ILtJump/FLtJump — ABC format (i8 offset in C)
                    tags::ILT_JUMP => {
                        if get_i64!(op.a()) >= get_i64!(op.b()) {
                            let off = op.c_i8();
                            ip = (ip as i32 + off as i32) as usize;
                            if off < 0 {
                                cancel_counter -= 1;
                                if cancel_counter <= 0 {
                                    cancel_counter = crate::cancel::CANCEL_CHECK_INTERVAL;
                                    if let Some(cb) = self.cancel_callback {
                                        if cb(self.cancel_userdata) {
                                            self.cancelled = true;
                                            return 0;
                                        }
                                    }
                                }
                            }
                        }
                    }

                    tags::FLT_JUMP => {
                        if r_f32!(op.a()) >= r_f32!(op.b()) {
                            let off = op.c_i8();
                            ip = (ip as i32 + off as i32) as usize;
                            if off < 0 {
                                cancel_counter -= 1;
                                if cancel_counter <= 0 {
                                    cancel_counter = crate::cancel::CANCEL_CHECK_INTERVAL;
                                    if let Some(cb) = self.cancel_callback {
                                        if cb(self.cancel_userdata) {
                                            self.cancelled = true;
                                            return 0;
                                        }
                                    }
                                }
                            }
                        }
                    }

                    // ILtJump/FLtJump wide — AB+data: next word is i32 offset
                    tags::ILT_JUMP_WIDE => {
                        let off = (*ops.add(ip)).0 as i32;
                        ip += 1;
                        if get_i64!(op.a()) >= get_i64!(op.b()) {
                            ip = (ip as i32 + off) as usize;
                            if off < 0 {
                                cancel_counter -= 1;
                                if cancel_counter <= 0 {
                                    cancel_counter = crate::cancel::CANCEL_CHECK_INTERVAL;
                                    if let Some(cb) = self.cancel_callback {
                                        if cb(self.cancel_userdata) {
                                            self.cancelled = true;
                                            return 0;
                                        }
                                    }
                                }
                            }
                        }
                    }

                    tags::FLT_JUMP_WIDE => {
                        let off = (*ops.add(ip)).0 as i32;
                        ip += 1;
                        if r_f32!(op.a()) >= r_f32!(op.b()) {
                            ip = (ip as i32 + off) as usize;
                            if off < 0 {
                                cancel_counter -= 1;
                                if cancel_counter <= 0 {
                                    cancel_counter = crate::cancel::CANCEL_CHECK_INTERVAL;
                                    if let Some(cb) = self.cancel_callback {
                                        if cb(self.cancel_userdata) {
                                            self.cancelled = true;
                                            return 0;
                                        }
                                    }
                                }
                            }
                        }
                    }

                    // FMulAdd/FMulSub — ABC+data: dst=A, a=B, b=C, c=data; dst = a*b +/- c
                    tags::FMUL_ADD => {
                        let c_reg = (*ops.add(ip)).0 as usize;
                        ip += 1;
                        set_f32!(op.a(), r_f32!(op.b()) * r_f32!(op.c()) + r_f32!(c_reg));
                    }
                    tags::FMUL_SUB => {
                        let c_reg = (*ops.add(ip)).0 as usize;
                        ip += 1;
                        set_f32!(op.a(), r_f32!(op.b()) * r_f32!(op.c()) - r_f32!(c_reg));
                    }
                    tags::DMUL_ADD => {
                        let c_reg = (*ops.add(ip)).0 as usize;
                        ip += 1;
                        set_f64!(op.a(), r_f64!(op.b()) * r_f64!(op.c()) + r_f64!(c_reg));
                    }
                    tags::DMUL_SUB => {
                        let c_reg = (*ops.add(ip)).0 as usize;
                        ip += 1;
                        set_f64!(op.a(), r_f64!(op.b()) * r_f64!(op.c()) - r_f64!(c_reg));
                    }
                    tags::FNMUL_ADD => {
                        let c_reg = (*ops.add(ip)).0 as usize;
                        ip += 1;
                        set_f32!(op.a(), r_f32!(c_reg) - r_f32!(op.b()) * r_f32!(op.c()));
                    }
                    tags::DNMUL_ADD => {
                        let c_reg = (*ops.add(ip)).0 as usize;
                        ip += 1;
                        set_f64!(op.a(), r_f64!(c_reg) - r_f64!(op.b()) * r_f64!(op.c()));
                    }

                    // Call — ABC: A=args_start, B=arg_count, C=func_idx
                    tags::CALL => {
                        let func = op.c() as u32;
                        let args_start = op.a() as usize;
                        let arg_count = op.b() as usize;

                        let frame = CallFrame {
                            func_idx: self.current_func,
                            ip: ip,
                            locals_base: locals_base,
                            return_reg: 0,
                        };
                        self.call_stack.push(frame);

                        for i in 0..arg_count {
                            if i != args_start + i {
                                self.registers[i] = self.registers[args_start + i];
                            }
                        }

                        locals_base += linked.func_locals[self.current_func as usize] as usize;
                        self.current_func = func;
                        ip = linked.func_offsets[func as usize];

                        #[cfg(debug_assertions)]
                        {
                            self.debug_func_name = program.functions[func as usize].name.clone();
                        }

                        let needed = locals_base + linked.func_locals[func as usize] as usize;
                        if needed > self.locals.len() {
                            self.locals.resize(needed * 2, 0);
                        }
                    }

                    // CallIndirect — ABC: A=func_reg, B=args_start, C=arg_count
                    tags::CALL_INDIRECT => {
                        let func_idx = r!(op.a()) as FuncIdx;
                        let args_start = op.b() as usize;
                        let arg_count = op.c() as usize;

                        let frame = CallFrame {
                            func_idx: self.current_func,
                            ip: ip,
                            locals_base: locals_base,
                            return_reg: 0,
                        };
                        self.call_stack.push(frame);

                        for i in 0..arg_count {
                            if i != args_start + i {
                                self.registers[i] = self.registers[args_start + i];
                            }
                        }

                        locals_base += linked.func_locals[self.current_func as usize] as usize;
                        self.current_func = func_idx;
                        ip = linked.func_offsets[func_idx as usize];

                        #[cfg(debug_assertions)]
                        {
                            self.debug_func_name =
                                program.functions[func_idx as usize].name.clone();
                        }
                    }

                    // CallClosure — ABC: A=fat_ptr_reg, B=args_start, C=arg_count
                    // fat_ptr points to {func_idx: i64, closure_ptr: i64}
                    tags::CALL_CLOSURE => {
                        let fat_ptr = r!(op.a()) as *const u64;
                        let func_idx = *fat_ptr as FuncIdx;
                        let closure_ptr_val = *fat_ptr.add(1);
                        let args_start = op.b() as usize;
                        let arg_count = op.c() as usize;

                        let frame = CallFrame {
                            func_idx: self.current_func,
                            ip: ip,
                            locals_base: locals_base,
                            return_reg: 0,
                        };
                        self.call_stack.push(frame);

                        for i in 0..arg_count {
                            if i != args_start + i {
                                self.registers[i] = self.registers[args_start + i];
                            }
                        }

                        self.closure_ptr = closure_ptr_val;

                        locals_base += linked.func_locals[self.current_func as usize] as usize;
                        self.current_func = func_idx;
                        ip = linked.func_offsets[func_idx as usize];

                        #[cfg(debug_assertions)]
                        {
                            self.debug_func_name =
                                program.functions[func_idx as usize].name.clone();
                        }

                        let needed = locals_base + linked.func_locals[func_idx as usize] as usize;
                        if needed > self.locals.len() {
                            self.locals.resize(needed * 2, 0);
                        }
                    }

                    // CallExtern — ABC+data: A=args_start, B=arg_count, data=globals_offset
                    // Reads {fn_ptr, context} from globals buffer, calls fn_ptr(context, args...)
                    tags::CALL_EXTERN => {
                        let args_start = op.a() as usize;
                        let _arg_count = op.b() as usize;
                        let data_word = linked.ops[ip];
                        ip += 1;
                        let globals_offset = data_word.0 as i32;

                        // Read fn_ptr and context from globals buffer.
                        let slot = self.globals.as_ptr().add(globals_offset as usize) as *const u64;
                        let fn_ptr = *slot as usize;
                        let context = *slot.add(1) as *mut u8;

                        if fn_ptr == 0 {
                            panic!("called unbound extern function (globals offset {})", globals_offset);
                        }

                        // Collect argument values as u64 from registers.
                        // Find the ExternFuncInfo for this globals_offset.
                        let info = program.extern_funcs.iter().find(|e| e.globals_offset == globals_offset)
                            .expect("no ExternFuncInfo for extern call");
                        let param_types = &info.param_types;
                        let ret_type = info.ret_type;

                        // Call the extern function using type-dispatched trampolines.
                        // Context is always the first argument.
                        let result: u64 = call_extern_fn(
                            fn_ptr,
                            context,
                            &self.registers,
                            args_start,
                            param_types,
                            ret_type,
                        );

                        // Store result in r0.
                        self.registers[0] = result;
                    }

                    // GetClosurePtr — A: dst register
                    tags::GET_CLOSURE_PTR => {
                        set_i64!(op.a(), self.closure_ptr as i64);
                    }

                    tags::RETURN => {
                        if self.call_stack.is_empty() {
                            return get_i64!(0u8);
                        }
                        let frame = self.call_stack.pop().unwrap();
                        self.current_func = frame.func_idx;
                        ip = frame.ip;
                        locals_base = frame.locals_base;

                        #[cfg(debug_assertions)]
                        {
                            self.debug_func_name =
                                program.functions[frame.func_idx as usize].name.clone();
                        }
                    }

                    tags::RETURN_REG => {
                        self.registers[0] = self.registers[op.a() as usize];
                        if self.call_stack.is_empty() {
                            return get_i64!(0u8);
                        }
                        let frame = self.call_stack.pop().unwrap();
                        self.current_func = frame.func_idx;
                        ip = frame.ip;
                        locals_base = frame.locals_base;

                        #[cfg(debug_assertions)]
                        {
                            self.debug_func_name =
                                program.functions[frame.func_idx as usize].name.clone();
                        }
                    }

                    tags::ALLOC_LOCALS => {
                        let size = op.d_u16() as usize;
                        let needed = locals_base + size;
                        if needed > self.locals.len() {
                            self.locals.resize(needed * 2, 0);
                        }
                        for i in 0..size {
                            self.locals[locals_base + i] = 0;
                        }
                    }

                    tags::MEM_COPY => {
                        let dst_ptr = r!(op.a());
                        let src_ptr = r!(op.b());
                        let size = op.c() as usize;
                        self.check_ptr(dst_ptr, size);
                        self.check_ptr(src_ptr, size);
                        std::ptr::copy_nonoverlapping(
                            src_ptr as *const u8,
                            dst_ptr as *mut u8,
                            size,
                        );
                    }

                    tags::MEM_COPY_WIDE => {
                        let dst_ptr = r!(op.a());
                        let src_ptr = r!(op.b());
                        let size = (*ops.add(ip)).0 as usize;
                        ip += 1;
                        self.check_ptr(dst_ptr, size);
                        self.check_ptr(src_ptr, size);
                        std::ptr::copy_nonoverlapping(
                            src_ptr as *const u8,
                            dst_ptr as *mut u8,
                            size,
                        );
                    }

                    tags::MEM_ZERO => {
                        let ptr = r!(op.a());
                        let size = op.d_u16() as usize;
                        self.check_ptr(ptr, size);
                        std::ptr::write_bytes(ptr as *mut u8, 0, size);
                    }

                    tags::SAVE_REGS => {
                        let base = locals_base + (op.c() as usize) * 8;
                        for i in 0..op.b() as usize {
                            let reg_val = self.registers[op.a() as usize + i];
                            let offset = base + i * 8;
                            self.locals[offset..offset + 8].copy_from_slice(&reg_val.to_le_bytes());
                        }
                    }

                    tags::SAVE_REGS_WIDE => {
                        let base = locals_base + (*ops.add(ip)).0 as usize;
                        ip += 1;
                        for i in 0..op.b() as usize {
                            let reg_val = self.registers[op.a() as usize + i];
                            let offset = base + i * 8;
                            self.locals[offset..offset + 8].copy_from_slice(&reg_val.to_le_bytes());
                        }
                    }

                    tags::RESTORE_REGS => {
                        let base = locals_base + (op.c() as usize) * 8;
                        for i in 0..op.b() as usize {
                            let offset = base + i * 8;
                            let bytes: [u8; 8] =
                                self.locals[offset..offset + 8].try_into().unwrap();
                            self.registers[op.a() as usize + i] = u64::from_le_bytes(bytes);
                        }
                    }

                    tags::RESTORE_REGS_WIDE => {
                        let base = locals_base + (*ops.add(ip)).0 as usize;
                        ip += 1;
                        for i in 0..op.b() as usize {
                            let offset = base + i * 8;
                            let bytes: [u8; 8] =
                                self.locals[offset..offset + 8].try_into().unwrap();
                            self.registers[op.a() as usize + i] = u64::from_le_bytes(bytes);
                        }
                    }

                    // Debugging
                    tags::PRINT_I32 => {
                        println_output(&format!("{}", get_i64!(op.a()) as i32));
                    }
                    tags::PRINT_F32 => {
                        println_output(&format!("{}", r_f32!(op.a())));
                    }
                    tags::ASSERT => {
                        let val = r!(op.a()) != 0;
                        println_output(&format!("assert({})", val));
                        if !val {
                            panic!(
                                "Assertion failed at {}:{}",
                                program.functions[self.current_func as usize].name,
                                ip - 1
                            );
                        }
                    }
                    tags::PUTC => {
                        if let Some(c) = char::from_u32(get_i64!(op.a()) as u32) {
                            print_output(&format!("{}", c));
                        }
                    }

                    // Math builtins — f32 unary
                    tags::SIN_F32 => {
                        set_f32!(op.a(), r_f32!(op.b()).sin());
                    }
                    tags::COS_F32 => {
                        set_f32!(op.a(), r_f32!(op.b()).cos());
                    }
                    tags::TAN_F32 => {
                        set_f32!(op.a(), r_f32!(op.b()).tan());
                    }
                    tags::ASIN_F32 => {
                        set_f32!(op.a(), r_f32!(op.b()).asin());
                    }
                    tags::ACOS_F32 => {
                        set_f32!(op.a(), r_f32!(op.b()).acos());
                    }
                    tags::ATAN_F32 => {
                        set_f32!(op.a(), r_f32!(op.b()).atan());
                    }
                    tags::SINH_F32 => {
                        set_f32!(op.a(), r_f32!(op.b()).sinh());
                    }
                    tags::COSH_F32 => {
                        set_f32!(op.a(), r_f32!(op.b()).cosh());
                    }
                    tags::TANH_F32 => {
                        set_f32!(op.a(), r_f32!(op.b()).tanh());
                    }
                    tags::ASINH_F32 => {
                        set_f32!(op.a(), r_f32!(op.b()).asinh());
                    }
                    tags::ACOSH_F32 => {
                        set_f32!(op.a(), r_f32!(op.b()).acosh());
                    }
                    tags::ATANH_F32 => {
                        set_f32!(op.a(), r_f32!(op.b()).atanh());
                    }
                    tags::LN_F32 => {
                        set_f32!(op.a(), r_f32!(op.b()).ln());
                    }
                    tags::EXP_F32 => {
                        set_f32!(op.a(), r_f32!(op.b()).exp());
                    }
                    tags::EXP2_F32 => {
                        set_f32!(op.a(), r_f32!(op.b()).exp2());
                    }
                    tags::LOG10_F32 => {
                        set_f32!(op.a(), r_f32!(op.b()).log10());
                    }
                    tags::LOG2_F32 => {
                        set_f32!(op.a(), r_f32!(op.b()).log2());
                    }
                    tags::SQRT_F32 => {
                        set_f32!(op.a(), r_f32!(op.b()).sqrt());
                    }
                    tags::ABS_F32 => {
                        set_f32!(op.a(), r_f32!(op.b()).abs());
                    }
                    tags::FLOOR_F32 => {
                        set_f32!(op.a(), r_f32!(op.b()).floor());
                    }
                    tags::CEIL_F32 => {
                        set_f32!(op.a(), r_f32!(op.b()).ceil());
                    }

                    // Math builtins — f64 unary
                    tags::SIN_F64 => {
                        set_f64!(op.a(), r_f64!(op.b()).sin());
                    }
                    tags::COS_F64 => {
                        set_f64!(op.a(), r_f64!(op.b()).cos());
                    }
                    tags::TAN_F64 => {
                        set_f64!(op.a(), r_f64!(op.b()).tan());
                    }
                    tags::ASIN_F64 => {
                        set_f64!(op.a(), r_f64!(op.b()).asin());
                    }
                    tags::ACOS_F64 => {
                        set_f64!(op.a(), r_f64!(op.b()).acos());
                    }
                    tags::ATAN_F64 => {
                        set_f64!(op.a(), r_f64!(op.b()).atan());
                    }
                    tags::SINH_F64 => {
                        set_f64!(op.a(), r_f64!(op.b()).sinh());
                    }
                    tags::COSH_F64 => {
                        set_f64!(op.a(), r_f64!(op.b()).cosh());
                    }
                    tags::TANH_F64 => {
                        set_f64!(op.a(), r_f64!(op.b()).tanh());
                    }
                    tags::ASINH_F64 => {
                        set_f64!(op.a(), r_f64!(op.b()).asinh());
                    }
                    tags::ACOSH_F64 => {
                        set_f64!(op.a(), r_f64!(op.b()).acosh());
                    }
                    tags::ATANH_F64 => {
                        set_f64!(op.a(), r_f64!(op.b()).atanh());
                    }
                    tags::LN_F64 => {
                        set_f64!(op.a(), r_f64!(op.b()).ln());
                    }
                    tags::EXP_F64 => {
                        set_f64!(op.a(), r_f64!(op.b()).exp());
                    }
                    tags::EXP2_F64 => {
                        set_f64!(op.a(), r_f64!(op.b()).exp2());
                    }
                    tags::LOG10_F64 => {
                        set_f64!(op.a(), r_f64!(op.b()).log10());
                    }
                    tags::LOG2_F64 => {
                        set_f64!(op.a(), r_f64!(op.b()).log2());
                    }
                    tags::SQRT_F64 => {
                        set_f64!(op.a(), r_f64!(op.b()).sqrt());
                    }
                    tags::ABS_F64 => {
                        set_f64!(op.a(), r_f64!(op.b()).abs());
                    }
                    tags::FLOOR_F64 => {
                        set_f64!(op.a(), r_f64!(op.b()).floor());
                    }
                    tags::CEIL_F64 => {
                        set_f64!(op.a(), r_f64!(op.b()).ceil());
                    }

                    // Math builtins — predicates
                    tags::ISINF_F32 => {
                        set_i64!(op.a(), r_f32!(op.b()).is_infinite() as i64);
                    }
                    tags::ISINF_F64 => {
                        set_i64!(op.a(), r_f64!(op.b()).is_infinite() as i64);
                    }
                    tags::ISNAN_F32 => {
                        set_i64!(op.a(), r_f32!(op.b()).is_nan() as i64);
                    }
                    tags::ISNAN_F64 => {
                        set_i64!(op.a(), r_f64!(op.b()).is_nan() as i64);
                    }

                    // Math builtins — binary f32
                    tags::POW_F32 => {
                        set_f32!(op.a(), r_f32!(op.b()).powf(r_f32!(op.c())));
                    }
                    tags::ATAN2_F32 => {
                        set_f32!(op.a(), r_f32!(op.b()).atan2(r_f32!(op.c())));
                    }
                    tags::MIN_F32 => {
                        set_f32!(op.a(), r_f32!(op.b()).min(r_f32!(op.c())));
                    }
                    tags::MAX_F32 => {
                        set_f32!(op.a(), r_f32!(op.b()).max(r_f32!(op.c())));
                    }

                    // Math builtins — binary f64
                    tags::POW_F64 => {
                        set_f64!(op.a(), r_f64!(op.b()).powf(r_f64!(op.c())));
                    }
                    tags::ATAN2_F64 => {
                        set_f64!(op.a(), r_f64!(op.b()).atan2(r_f64!(op.c())));
                    }
                    tags::MIN_F64 => {
                        set_f64!(op.a(), r_f64!(op.b()).min(r_f64!(op.c())));
                    }
                    tags::MAX_F64 => {
                        set_f64!(op.a(), r_f64!(op.b()).max(r_f64!(op.c())));
                    }

                    _ => {
                        #[cfg(debug_assertions)]
                        panic!("VM: unknown opcode tag {}", op.tag());
                        #[cfg(not(debug_assertions))]
                        core::hint::unreachable_unchecked();
                    }
                }
            }
        }
    }
    /// Only valid after `run` has been called.
    pub fn globals_ptr(&mut self) -> *mut u8 {
        self.globals.as_mut_ptr()
    }

    /// Call a named entry point function with i64 arguments.
    /// Globals are allocated on first call and preserved on subsequent calls.
    pub fn call(
        &mut self,
        program: &VMProgram,
        name: crate::Name,
        args: &[i64],
    ) -> Result<i64, String> {
        let func_idx = program
            .entry_points
            .get(&name)
            .copied()
            .ok_or_else(|| format!("entry point '{}' not found", name))?;

        Ok(self.run_inner(program, func_idx, args))
    }

    /// Call a function using a pre-linked program with an external globals buffer.
    /// Copies globals in before execution, copies back out after.
    /// This is the fast path used by the FFI.
    pub unsafe fn call_with_external_globals(
        &mut self,
        linked: &LinkedProgram,
        program: &VMProgram,
        func_idx: FuncIdx,
        external_globals: *mut u8,
        globals_size: usize,
    ) -> i64 {
        // Ensure internal buffer is the right size.
        if self.globals.len() != globals_size {
            self.globals = vec![0u8; globals_size];
        }
        // Copy in.
        if globals_size > 0 {
            std::ptr::copy_nonoverlapping(
                external_globals,
                self.globals.as_mut_ptr(),
                globals_size,
            );
        }
        let result = self.run_linked(linked, program, func_idx, &[]);
        // Copy out.
        if globals_size > 0 {
            std::ptr::copy_nonoverlapping(self.globals.as_ptr(), external_globals, globals_size);
        }
        result
    }

    /// Get the size of the globals buffer in bytes.
    pub fn globals_size(&self) -> usize {
        self.globals.len()
    }

    /// Run the program and return f32 result
    pub fn run_f32(&mut self, program: &VMProgram) -> f32 {
        self.run(program);
        self.get_f32(0)
    }

    /// Run the program and return f64 result
    pub fn run_f64(&mut self, program: &VMProgram) -> f64 {
        self.run(program);
        self.get_f64(0)
    }
}

/// Disassemble a function to a string
impl fmt::Display for VMFunction {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(
            f,
            "fn {} (params: {}, locals: {} bytes):",
            self.name, self.param_count, self.locals_size
        )?;

        // Collect jump targets.
        let mut targets = std::collections::HashSet::new();
        for (i, op) in self.code.iter().enumerate() {
            let offset = match op {
                Opcode::Jump { offset }
                | Opcode::JumpIfZero { offset, .. }
                | Opcode::JumpIfNotZero { offset, .. }
                | Opcode::ILtJump { offset, .. }
                | Opcode::FLtJump { offset, .. } => Some(*offset),
                _ => None,
            };
            if let Some(off) = offset {
                targets.insert((i as i32 + 1 + off) as usize);
            }
        }

        // Build slot → name map.
        let mut slot_name: std::collections::HashMap<u16, &str> = std::collections::HashMap::new();
        for (slot, name) in &self.slot_names {
            slot_name.insert(*slot, name);
        }

        for (i, op) in self.code.iter().enumerate() {
            let marker = if targets.contains(&i) { "=>" } else { "  " };
            let comment = match op {
                Opcode::LocalAddr { slot, .. } => slot_name.get(&(*slot as u16)).copied(),
                _ => None,
            };
            match comment {
                Some(name) => writeln!(f, "{}{:4}: {:?}  ; {}", marker, i, op, name)?,
                None => writeln!(f, "{}{:4}: {:?}", marker, i, op)?,
            }
        }

        // Print locals table (only registers that appear in the code).
        if !self.slot_names.is_empty() || !self.reg_names.is_empty() {
            let mut used_regs = std::collections::HashSet::new();
            for op in &self.code {
                if let Some(dst) = op.get_dst() {
                    used_regs.insert(dst);
                }
            }

            let mut entries: Vec<(String, &str)> = Vec::new();
            for (slot, name) in &self.slot_names {
                entries.push((format!("slot {}", slot), name));
            }
            for (reg, name) in &self.reg_names {
                if used_regs.contains(reg) {
                    entries.push((format!("r{}", reg), name));
                }
            }
            entries.sort();
            if !entries.is_empty() {
                writeln!(f, "locals:")?;
                for (loc, name) in &entries {
                    writeln!(f, "  {}\t{}", loc, name)?;
                }
            }
        }

        Ok(())
    }
}

// ============ Biquad Filter Benchmark ============

/// Create a biquad filter program in VM assembly.
///
/// A biquad filter computes:
///   y[n] = b0*x[n] + b1*x[n-1] + b2*x[n-2] - a1*y[n-1] - a2*y[n-2]
///
/// State: [x1, x2, y1, y2] (previous samples)
/// Coefficients: [b0, b1, b2, a1, a2]
pub fn create_biquad_program() -> VMProgram {
    let mut program = VMProgram::new();

    // Create the biquad process function
    // Processes a single sample
    // r0 = input sample (f32)
    // r1 = pointer to state [x1, x2, y1, y2] (4 * f32 = 16 bytes)
    // r2 = pointer to coefficients [b0, b1, b2, a1, a2] (5 * f32 = 20 bytes)
    // Returns: output sample in r0
    let mut biquad = VMFunction::new("biquad_process");
    biquad.param_count = 3;

    // Input is in r0 (x0)
    // State pointer in r1
    // Coefficients pointer in r2

    // Load state: x1, x2, y1, y2
    biquad.emit(Opcode::Load32Off {
        dst: 3,
        base: 1,
        offset: 0,
    }); // r3 = x1
    biquad.emit(Opcode::Load32Off {
        dst: 4,
        base: 1,
        offset: 4,
    }); // r4 = x2
    biquad.emit(Opcode::Load32Off {
        dst: 5,
        base: 1,
        offset: 8,
    }); // r5 = y1
    biquad.emit(Opcode::Load32Off {
        dst: 6,
        base: 1,
        offset: 12,
    }); // r6 = y2

    // Load coefficients: b0, b1, b2, a1, a2
    biquad.emit(Opcode::Load32Off {
        dst: 10,
        base: 2,
        offset: 0,
    }); // r10 = b0
    biquad.emit(Opcode::Load32Off {
        dst: 11,
        base: 2,
        offset: 4,
    }); // r11 = b1
    biquad.emit(Opcode::Load32Off {
        dst: 12,
        base: 2,
        offset: 8,
    }); // r12 = b2
    biquad.emit(Opcode::Load32Off {
        dst: 13,
        base: 2,
        offset: 12,
    }); // r13 = a1
    biquad.emit(Opcode::Load32Off {
        dst: 14,
        base: 2,
        offset: 16,
    }); // r14 = a2

    // Compute output: y0 = b0*x0 + b1*x1 + b2*x2 - a1*y1 - a2*y2

    // r20 = b0 * x0
    biquad.emit(Opcode::FMul {
        dst: 20,
        a: 10,
        b: 0,
    });

    // r20 += b1 * x1
    biquad.emit(Opcode::FMul {
        dst: 21,
        a: 11,
        b: 3,
    });
    biquad.emit(Opcode::FAdd {
        dst: 20,
        a: 20,
        b: 21,
    });

    // r20 += b2 * x2
    biquad.emit(Opcode::FMul {
        dst: 21,
        a: 12,
        b: 4,
    });
    biquad.emit(Opcode::FAdd {
        dst: 20,
        a: 20,
        b: 21,
    });

    // r20 -= a1 * y1
    biquad.emit(Opcode::FMul {
        dst: 21,
        a: 13,
        b: 5,
    });
    biquad.emit(Opcode::FSub {
        dst: 20,
        a: 20,
        b: 21,
    });

    // r20 -= a2 * y2
    biquad.emit(Opcode::FMul {
        dst: 21,
        a: 14,
        b: 6,
    });
    biquad.emit(Opcode::FSub {
        dst: 20,
        a: 20,
        b: 21,
    });

    // Update state:
    // x2 = x1, x1 = x0, y2 = y1, y1 = y0
    biquad.emit(Opcode::Store32Off {
        base: 1,
        offset: 4,
        src: 3,
    }); // x2 = x1
    biquad.emit(Opcode::Store32Off {
        base: 1,
        offset: 0,
        src: 0,
    }); // x1 = x0
    biquad.emit(Opcode::Store32Off {
        base: 1,
        offset: 12,
        src: 5,
    }); // y2 = y1
    biquad.emit(Opcode::Store32Off {
        base: 1,
        offset: 8,
        src: 20,
    }); // y1 = y0

    // Return y0
    biquad.emit(Opcode::ReturnReg { src: 20 });

    let biquad_idx = program.add_function(biquad);

    // Create main function that runs the biquad filter N times
    let mut main = VMFunction::new("main");
    main.locals_size = 64; // Space for state and coefficients

    // Allocate locals
    main.emit(Opcode::AllocLocals { size: 64 });

    // Get pointers to state and coefficients
    main.emit(Opcode::LocalAddr { dst: 10, slot: 0 }); // r10 = &state (slots 0-1)
    main.emit(Opcode::LocalAddr { dst: 11, slot: 2 }); // r11 = &coeffs (slots 2-4)

    // Initialize state to zero (already done by AllocLocals)

    // Initialize coefficients for a simple lowpass filter
    // b0 = 0.0675, b1 = 0.135, b2 = 0.0675
    // a1 = -1.143, a2 = 0.413
    main.emit(Opcode::LoadF32 {
        dst: 0,
        value: 0.0675,
    });
    main.emit(Opcode::Store32Off {
        base: 11,
        offset: 0,
        src: 0,
    }); // b0
    main.emit(Opcode::LoadF32 {
        dst: 0,
        value: 0.135,
    });
    main.emit(Opcode::Store32Off {
        base: 11,
        offset: 4,
        src: 0,
    }); // b1
    main.emit(Opcode::LoadF32 {
        dst: 0,
        value: 0.0675,
    });
    main.emit(Opcode::Store32Off {
        base: 11,
        offset: 8,
        src: 0,
    }); // b2
    main.emit(Opcode::LoadF32 {
        dst: 0,
        value: -1.143,
    });
    main.emit(Opcode::Store32Off {
        base: 11,
        offset: 12,
        src: 0,
    }); // a1
    main.emit(Opcode::LoadF32 {
        dst: 0,
        value: 0.413,
    });
    main.emit(Opcode::Store32Off {
        base: 11,
        offset: 16,
        src: 0,
    }); // a2

    // Loop counter
    main.emit(Opcode::LoadImm { dst: 20, value: 0 }); // r20 = i = 0
    main.emit(Opcode::LoadImm {
        dst: 21,
        value: 10000,
    }); // r21 = N = 10000

    // Accumulator for output
    main.emit(Opcode::LoadF32 {
        dst: 30,
        value: 0.0,
    }); // r30 = sum = 0.0

    // Loop start (instruction index for jump target)
    let loop_start = main.code.len();

    // Check i < N using superinstruction (jumps if NOT i < N)
    let jump_end = main.emit(Opcode::ILtJump {
        a: 20,
        b: 21,
        offset: 0,
    }); // patched later

    // Generate input: simple sawtooth wave
    // input = (i % 100) / 100.0 - 0.5
    main.emit(Opcode::LoadImm {
        dst: 23,
        value: 100,
    });
    main.emit(Opcode::IRem {
        dst: 24,
        a: 20,
        b: 23,
    }); // r24 = i % 100
    main.emit(Opcode::I32ToF32 { dst: 25, src: 24 }); // r25 = (float)(i % 100)
    main.emit(Opcode::LoadF32 {
        dst: 26,
        value: 100.0,
    });
    main.emit(Opcode::FDiv {
        dst: 0,
        a: 25,
        b: 26,
    }); // r0 = r25 / 100.0
    main.emit(Opcode::LoadF32 {
        dst: 26,
        value: 0.5,
    });
    main.emit(Opcode::FSub {
        dst: 0,
        a: 0,
        b: 26,
    }); // r0 = r0 - 0.5 (input)

    // Call biquad_process(input, &state, &coeffs)
    main.emit(Opcode::Move { dst: 1, src: 10 }); // r1 = &state
    main.emit(Opcode::Move { dst: 2, src: 11 }); // r2 = &coeffs
    main.emit(Opcode::Call {
        func: biquad_idx,
        args_start: 0,
        arg_count: 3,
    });

    // Accumulate output
    main.emit(Opcode::FAdd {
        dst: 30,
        a: 30,
        b: 0,
    }); // sum += output

    // i++ using superinstruction
    main.emit(Opcode::IAddImm {
        dst: 20,
        src: 20,
        imm: 1,
    });

    // Jump back to loop start
    let loop_end = main.code.len();
    main.emit(Opcode::Jump {
        offset: (loop_start as i32) - (loop_end as i32) - 1,
    });

    // Patch the conditional jump
    main.patch_jump(jump_end);

    // Return the sum (for verification)
    main.emit(Opcode::Move { dst: 0, src: 30 });
    main.emit(Opcode::Return);

    let main_idx = program.add_function(main);
    program.entry = main_idx;

    program
}

/// Run the biquad benchmark and return execution statistics
pub fn run_biquad_benchmark(iterations: u32) -> BenchmarkResult {
    let program = create_biquad_program();
    let mut vm = VM::new();

    let start = std::time::Instant::now();

    for _ in 0..iterations {
        vm.run(&program);
    }

    let elapsed = start.elapsed();
    let total_samples = 10000 * iterations;

    BenchmarkResult {
        total_time: elapsed,
        iterations,
        samples_per_iteration: 10000,
        samples_per_second: (total_samples as f64) / elapsed.as_secs_f64(),
    }
}

#[derive(Debug)]
pub struct BenchmarkResult {
    pub total_time: std::time::Duration,
    pub iterations: u32,
    pub samples_per_iteration: u32,
    pub samples_per_second: f64,
}

impl fmt::Display for BenchmarkResult {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Biquad Filter Benchmark Results:")?;
        writeln!(f, "  Iterations: {}", self.iterations)?;
        writeln!(f, "  Samples per iteration: {}", self.samples_per_iteration)?;
        writeln!(f, "  Total time: {:?}", self.total_time)?;
        writeln!(f, "  Samples/second: {:.2}", self.samples_per_second)?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_basic_arithmetic() {
        let mut func = VMFunction::new("test");
        func.emit(Opcode::LoadImm { dst: 0, value: 10 });
        func.emit(Opcode::LoadImm { dst: 1, value: 20 });
        func.emit(Opcode::IAdd { dst: 0, a: 0, b: 1 });
        func.emit(Opcode::Return);

        let mut program = VMProgram::new();
        program.entry = program.add_function(func);

        let mut vm = VM::new();
        let result = vm.run(&program);
        assert_eq!(result, 30);
    }

    #[test]
    fn test_float_arithmetic() {
        let mut func = VMFunction::new("test");
        func.emit(Opcode::LoadF32 { dst: 0, value: 1.5 });
        func.emit(Opcode::LoadF32 { dst: 1, value: 2.5 });
        func.emit(Opcode::FMul { dst: 0, a: 0, b: 1 });
        func.emit(Opcode::Return);

        let mut program = VMProgram::new();
        program.entry = program.add_function(func);

        let mut vm = VM::new();
        let result = vm.run_f32(&program);
        assert!((result - 3.75).abs() < 0.0001);
    }

    #[test]
    fn test_conditionals() {
        let mut func = VMFunction::new("test");
        func.emit(Opcode::LoadImm { dst: 0, value: 5 });
        func.emit(Opcode::LoadImm { dst: 1, value: 10 });
        func.emit(Opcode::ILt { dst: 2, a: 0, b: 1 }); // 5 < 10 = true
        func.emit(Opcode::JumpIfZero { cond: 2, offset: 2 });
        func.emit(Opcode::LoadImm { dst: 0, value: 100 }); // if true
        func.emit(Opcode::Jump { offset: 1 });
        func.emit(Opcode::LoadImm { dst: 0, value: 200 }); // if false
        func.emit(Opcode::Return);

        let mut program = VMProgram::new();
        program.entry = program.add_function(func);

        let mut vm = VM::new();
        let result = vm.run(&program);
        assert_eq!(result, 100);
    }

    #[test]
    fn test_loop() {
        // Sum 1..10
        let mut func = VMFunction::new("test");
        func.emit(Opcode::LoadImm { dst: 0, value: 0 }); // sum = 0
        func.emit(Opcode::LoadImm { dst: 1, value: 1 }); // i = 1
        func.emit(Opcode::LoadImm { dst: 2, value: 11 }); // limit = 11

        // Loop start (index 3)
        func.emit(Opcode::ILt { dst: 3, a: 1, b: 2 }); // i < 11?
        func.emit(Opcode::JumpIfZero { cond: 3, offset: 4 }); // exit if false
        func.emit(Opcode::IAdd { dst: 0, a: 0, b: 1 }); // sum += i
        func.emit(Opcode::LoadImm { dst: 4, value: 1 });
        func.emit(Opcode::IAdd { dst: 1, a: 1, b: 4 }); // i++
        func.emit(Opcode::Jump { offset: -6 }); // back to loop start

        func.emit(Opcode::Return);

        let mut program = VMProgram::new();
        program.entry = program.add_function(func);

        let mut vm = VM::new();
        let result = vm.run(&program);
        assert_eq!(result, 55); // 1+2+3+...+10 = 55
    }

    #[test]
    fn test_function_call() {
        // Create a function that doubles its input
        let mut double_fn = VMFunction::new("double");
        double_fn.param_count = 1;
        double_fn.emit(Opcode::IAdd { dst: 0, a: 0, b: 0 });
        double_fn.emit(Opcode::Return);

        // Main function
        let mut main = VMFunction::new("main");
        main.emit(Opcode::LoadImm { dst: 0, value: 21 });
        main.emit(Opcode::Call {
            func: 0,
            args_start: 0,
            arg_count: 1,
        });
        main.emit(Opcode::Return);

        let mut program = VMProgram::new();
        let double_idx = program.add_function(double_fn);
        program.entry = program.add_function(main);

        // Fix the call instruction to use correct function index
        if let Opcode::Call { func, .. } = &mut program.functions[program.entry as usize].code[1] {
            *func = double_idx;
        }

        let mut vm = VM::new();
        let result = vm.run(&program);
        assert_eq!(result, 42);
    }

    #[test]
    fn test_memory_operations() {
        let mut func = VMFunction::new("test");
        func.locals_size = 32;

        func.emit(Opcode::AllocLocals { size: 32 });
        func.emit(Opcode::LocalAddr { dst: 1, slot: 0 });
        func.emit(Opcode::LoadImm {
            dst: 0,
            value: 12345,
        });
        func.emit(Opcode::Store32 { addr: 1, src: 0 });
        func.emit(Opcode::LoadImm { dst: 0, value: 0 }); // Clear r0
        func.emit(Opcode::Load32 { dst: 0, addr: 1 });
        func.emit(Opcode::Return);

        let mut program = VMProgram::new();
        program.entry = program.add_function(func);

        let mut vm = VM::new();
        let result = vm.run(&program);
        assert_eq!(result, 12345);
    }

    #[test]
    fn test_biquad_program_runs() {
        let program = create_biquad_program();
        let mut vm = VM::new();
        let result = vm.run_f32(&program);

        // The exact value depends on the filter, but it should be non-zero
        // and within a reasonable range for filtered audio
        assert!(result.abs() < 10000.0);
        assert!(result != 0.0);
    }

    #[test]
    fn test_disassembly() {
        let program = create_biquad_program();
        let output = format!("{}", program.functions[0]);
        assert!(output.contains("biquad_process"));
        assert!(output.contains("FMul"));
    }

    #[test]
    fn test_bitwise_operations() {
        let mut func = VMFunction::new("test");
        func.emit(Opcode::LoadImm {
            dst: 0,
            value: 0b1010,
        });
        func.emit(Opcode::LoadImm {
            dst: 1,
            value: 0b1100,
        });
        func.emit(Opcode::And { dst: 2, a: 0, b: 1 }); // 0b1000 = 8
        func.emit(Opcode::Or { dst: 3, a: 0, b: 1 }); // 0b1110 = 14
        func.emit(Opcode::Xor { dst: 4, a: 0, b: 1 }); // 0b0110 = 6
                                                       // Return AND result
        func.emit(Opcode::Move { dst: 0, src: 2 });
        func.emit(Opcode::Return);

        let mut program = VMProgram::new();
        program.entry = program.add_function(func);

        let mut vm = VM::new();
        let result = vm.run(&program);
        assert_eq!(result, 8);
    }

    #[test]
    fn test_type_conversions() {
        let mut func = VMFunction::new("test");
        func.emit(Opcode::LoadImm { dst: 0, value: 42 });
        func.emit(Opcode::I32ToF32 { dst: 1, src: 0 });
        func.emit(Opcode::LoadF32 { dst: 2, value: 0.5 });
        func.emit(Opcode::FAdd { dst: 1, a: 1, b: 2 }); // 42.5
        func.emit(Opcode::F32ToI32 { dst: 0, src: 1 }); // truncates to 42
        func.emit(Opcode::Return);

        let mut program = VMProgram::new();
        program.entry = program.add_function(func);

        let mut vm = VM::new();
        let result = vm.run(&program);
        assert_eq!(result, 42);
    }

    #[test]
    fn test_fmul_add() {
        let mut func = VMFunction::new("test");
        func.emit(Opcode::LoadF32 { dst: 0, value: 2.0 });
        func.emit(Opcode::LoadF32 { dst: 1, value: 3.0 });
        func.emit(Opcode::LoadF32 { dst: 2, value: 4.0 });
        // dst = 2.0 * 3.0 + 4.0 = 10.0
        func.emit(Opcode::FMul { dst: 3, a: 0, b: 1 });
        func.emit(Opcode::FAdd { dst: 0, a: 3, b: 2 });
        func.emit(Opcode::Return);

        let mut program = VMProgram::new();
        program.entry = program.add_function(func);

        let mut vm = VM::new();
        let result = vm.run_f32(&program);
        assert!((result - 10.0).abs() < 0.0001);
    }

    #[test]
    fn test_fmul_sub() {
        let mut func = VMFunction::new("test");
        func.emit(Opcode::LoadF32 { dst: 0, value: 2.0 });
        func.emit(Opcode::LoadF32 { dst: 1, value: 3.0 });
        func.emit(Opcode::LoadF32 { dst: 2, value: 4.0 });
        // dst = 2.0 * 3.0 - 4.0 = 2.0
        func.emit(Opcode::FMul { dst: 3, a: 0, b: 1 });
        func.emit(Opcode::FSub { dst: 0, a: 3, b: 2 });
        func.emit(Opcode::Return);

        let mut program = VMProgram::new();
        program.entry = program.add_function(func);

        let mut vm = VM::new();
        let result = vm.run_f32(&program);
        assert!((result - 2.0).abs() < 0.0001);
    }

    #[test]
    fn test_iadd_imm() {
        let mut func = VMFunction::new("test");
        func.emit(Opcode::LoadImm { dst: 0, value: 40 });
        func.emit(Opcode::IAddImm {
            dst: 0,
            src: 0,
            imm: 2,
        });
        func.emit(Opcode::Return);

        let mut program = VMProgram::new();
        program.entry = program.add_function(func);

        let mut vm = VM::new();
        let result = vm.run(&program);
        assert_eq!(result, 42);
    }

    #[test]
    fn test_ilt_jump() {
        // Test the fused compare-and-branch: ILtJump jumps if NOT (a < b)
        let mut func = VMFunction::new("test");
        func.emit(Opcode::LoadImm { dst: 0, value: 5 });
        func.emit(Opcode::LoadImm { dst: 1, value: 10 });
        // 5 < 10 is true, so we should NOT jump
        func.emit(Opcode::ILtJump {
            a: 0,
            b: 1,
            offset: 2,
        });
        func.emit(Opcode::LoadImm { dst: 0, value: 100 }); // should execute
        func.emit(Opcode::Jump { offset: 1 });
        func.emit(Opcode::LoadImm { dst: 0, value: 200 }); // should skip
        func.emit(Opcode::Return);

        let mut program = VMProgram::new();
        program.entry = program.add_function(func);

        let mut vm = VM::new();
        let result = vm.run(&program);
        assert_eq!(result, 100);
    }

    #[test]
    fn test_ilt_jump_taken() {
        // Test when jump IS taken: a >= b
        let mut func = VMFunction::new("test");
        func.emit(Opcode::LoadImm { dst: 0, value: 10 });
        func.emit(Opcode::LoadImm { dst: 1, value: 5 });
        // 10 < 5 is false, so we SHOULD jump
        func.emit(Opcode::ILtJump {
            a: 0,
            b: 1,
            offset: 2,
        });
        func.emit(Opcode::LoadImm { dst: 0, value: 100 }); // should skip
        func.emit(Opcode::Jump { offset: 1 });
        func.emit(Opcode::LoadImm { dst: 0, value: 200 }); // should execute
        func.emit(Opcode::Return);

        let mut program = VMProgram::new();
        program.entry = program.add_function(func);

        let mut vm = VM::new();
        let result = vm.run(&program);
        assert_eq!(result, 200);
    }

    #[test]
    fn test_loop_with_superinstructions() {
        // Sum 1..10 using IAddImm and ILtJump
        let mut func = VMFunction::new("test");
        func.emit(Opcode::LoadImm { dst: 0, value: 0 }); // sum = 0
        func.emit(Opcode::LoadImm { dst: 1, value: 1 }); // i = 1
        func.emit(Opcode::LoadImm { dst: 2, value: 11 }); // limit = 11

        // Loop start (index 3)
        // ILtJump: if !(i < 11) jump to end
        func.emit(Opcode::ILtJump {
            a: 1,
            b: 2,
            offset: 3,
        });
        func.emit(Opcode::IAdd { dst: 0, a: 0, b: 1 }); // sum += i
        func.emit(Opcode::IAddImm {
            dst: 1,
            src: 1,
            imm: 1,
        }); // i++
        func.emit(Opcode::Jump { offset: -4 }); // back to loop start

        func.emit(Opcode::Return);

        let mut program = VMProgram::new();
        program.entry = program.add_function(func);

        let mut vm = VM::new();
        let result = vm.run(&program);
        assert_eq!(result, 55); // 1+2+3+...+10 = 55
    }

    #[test]
    fn test_infinite_loop_cancels() {
        // Infinite loop: Jump { offset: -1 } forever.
        let mut func = VMFunction::new("test");
        func.emit(Opcode::Jump { offset: -1 });

        let mut program = VMProgram::new();
        program.globals_size = 0;
        program.entry = program.add_function(func);

        let mut vm = VM::new();

        // Cancel callback that always cancels on first invocation.
        unsafe extern "C" fn always_cancel(_user_data: *mut u8) -> bool {
            true
        }
        vm.set_cancel_callback(Some(always_cancel), std::ptr::null_mut());

        vm.run(&program);
        assert!(vm.cancelled, "expected the infinite loop to be cancelled");
    }
}
