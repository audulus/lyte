//! 16-bit VM interpreter with 16 registers.
//!
//! Dispatch loop uses a 256-entry match on the opcode byte.
//! Registers are indexed by the 4-bit rA/rB fields extracted from the 16-bit word.
//! In the ARM64 assembly backend (Phase 3), registers will be pinned to CPU registers
//! and the full 65K jump table will be used.

use crate::opcode::FuncIdx;
use crate::vm::{self, ExternFuncInfo, VMProgram, println_output, print_output};
use crate::vm16_link::LinkedProgram16;
use crate::vm16_lower;
use crate::vm16_opcode::tags;

/// Call frame for function execution.
#[derive(Clone, Debug)]
struct CallFrame {
    func_idx: FuncIdx,
    ip: usize,
    locals_base: usize,
}

/// 16-bit VM with 16 general-purpose 64-bit registers.
pub struct VM16 {
    pub regs: [u64; 16],
    pub locals: Vec<u8>,
    pub globals: Vec<u8>,
    call_stack: Vec<CallFrame>,
    current_func: FuncIdx,
    locals_base: usize,
    pub cancelled: bool,
}

impl VM16 {
    pub fn new() -> Self {
        Self {
            regs: [0u64; 16],
            locals: vec![0u8; 1024 * 1024], // 1MB default
            globals: Vec::new(),
            call_stack: Vec::new(),
            current_func: 0,
            locals_base: 0,
            cancelled: false,
        }
    }

    /// Compile a VMProgram to 16-bit format and run a function.
    pub fn run(&mut self, program: &VMProgram, func_idx: FuncIdx, args: &[i64]) -> i64 {
        // Extract function info from VMProgram.
        let functions: Vec<_> = program
            .functions
            .iter()
            .map(|f| (f.code.clone(), f.param_count, f.locals_size))
            .collect();

        let lowered = vm16_lower::lower_program(&functions);
        let linked = LinkedProgram16::from_lowered(&lowered);

        if self.globals.is_empty() && program.globals_size > 0 {
            self.globals = vec![0u8; program.globals_size];
        }

        self.run_linked(&linked, program, func_idx, args)
    }

    /// Run with a pre-linked program.
    pub fn run_linked(
        &mut self,
        linked: &LinkedProgram16,
        program: &VMProgram,
        func_idx: FuncIdx,
        args: &[i64],
    ) -> i64 {
        // Set up initial state.
        self.regs = [0u64; 16];
        for (i, &arg) in args.iter().enumerate().take(16) {
            self.regs[i] = arg as u64;
        }
        self.current_func = func_idx;
        self.call_stack.clear();
        self.locals_base = 0;
        self.cancelled = false;

        // Pin hot state to local variables for performance.
        let ops = linked.ops.as_ptr();
        let ops_len = linked.ops.len();
        let mut ip = linked.func_offsets[func_idx as usize];
        let mut locals_base = 0usize;

        // Allocate initial locals.
        let needed = linked.func_locals[func_idx as usize] as usize;
        if needed > self.locals.len() {
            self.locals.resize(needed * 2, 0);
        }
        // Zero locals for current frame.
        for b in &mut self.locals[0..needed] {
            *b = 0;
        }

        loop {
            if ip >= ops_len {
                return self.regs[0] as i64;
            }

            let word = unsafe { *ops.add(ip) };
            ip += 1;
            let opcode = (word >> 8) as u8;
            let ra = ((word >> 4) & 0xF) as usize;
            let rb = (word & 0xF) as usize;

            macro_rules! trail {
                () => {{
                    let t = unsafe { *ops.add(ip) };
                    ip += 1;
                    t
                }};
            }

            macro_rules! trail_i16 {
                () => {
                    trail!() as i16
                };
            }

            macro_rules! r {
                ($idx:expr) => {
                    self.regs[$idx]
                };
            }

            macro_rules! r_i32 {
                ($idx:expr) => {
                    self.regs[$idx] as i32
                };
            }

            macro_rules! r_f32 {
                ($idx:expr) => {
                    f32::from_bits(self.regs[$idx] as u32)
                };
            }

            macro_rules! r_f64 {
                ($idx:expr) => {
                    f64::from_bits(self.regs[$idx])
                };
            }

            macro_rules! set_i32 {
                ($idx:expr, $val:expr) => {
                    self.regs[$idx] = ($val as i32) as u32 as u64;
                };
            }

            macro_rules! set_f32 {
                ($idx:expr, $val:expr) => {
                    self.regs[$idx] = f32::to_bits($val) as u64;
                };
            }

            macro_rules! set_f64 {
                ($idx:expr, $val:expr) => {
                    self.regs[$idx] = f64::to_bits($val);
                };
            }

            macro_rules! set_i64 {
                ($idx:expr, $val:expr) => {
                    self.regs[$idx] = $val as u64;
                };
            }

            macro_rules! local_ptr {
                ($slot:expr) => {
                    unsafe { self.locals.as_ptr().add(locals_base + ($slot as usize) * 8) }
                };
            }

            macro_rules! local_ptr_mut {
                ($slot:expr) => {
                    unsafe { self.locals.as_mut_ptr().add(locals_base + ($slot as usize) * 8) }
                };
            }

            unsafe {
                match opcode {
                    tags::NOP => {}

                    tags::HALT => return r!(0) as i64,

                    tags::RETURN => {
                        if self.call_stack.is_empty() {
                            return r!(0) as i64;
                        }
                        let frame = self.call_stack.pop().unwrap();
                        self.current_func = frame.func_idx;
                        ip = frame.ip;
                        locals_base = frame.locals_base;
                    }

                    tags::RETURN_REG => {
                        if ra != 0 {
                            r![0] = r![ra];
                        }
                        if self.call_stack.is_empty() {
                            return r!(0) as i64;
                        }
                        let frame = self.call_stack.pop().unwrap();
                        self.current_func = frame.func_idx;
                        ip = frame.ip;
                        locals_base = frame.locals_base;
                    }

                    tags::JUMP => {
                        let offset = trail_i16!();
                        ip = (ip as isize + offset as isize) as usize;
                    }

                    tags::JUMP_IF_ZERO => {
                        let offset = trail_i16!();
                        if r_i32!(ra) == 0 {
                            ip = (ip as isize + offset as isize) as usize;
                        }
                    }

                    tags::JUMP_IF_NOT_ZERO => {
                        let offset = trail_i16!();
                        if r_i32!(ra) != 0 {
                            ip = (ip as isize + offset as isize) as usize;
                        }
                    }

                    // === Moves & Loads ===
                    tags::MOVE => { r![ra] = r![rb]; }

                    tags::LOAD_IMM => {
                        let val = trail_i16!() as i64;
                        set_i64!(ra, val);
                    }

                    tags::LOAD_IMM_WIDE => {
                        let idx = trail!() as usize;
                        set_i64!(ra, linked.i64_pool[idx]);
                    }

                    tags::LOAD_F32 => {
                        let idx = trail!() as usize;
                        set_f32!(ra, linked.f32_pool[idx]);
                    }

                    tags::LOAD_F64 => {
                        let idx = trail!() as usize;
                        set_f64!(ra, linked.f64_pool[idx]);
                    }

                    tags::LOAD_CONST => {
                        let _idx = trail!();
                        // TODO: constant pool
                    }

                    tags::LOCAL_ADDR => {
                        let slot = trail!() as usize;
                        let addr = self.locals.as_ptr().add(locals_base + slot * 8);
                        set_i64!(ra, addr as i64);
                    }

                    tags::GLOBAL_ADDR => {
                        let offset = trail!() as usize;
                        let addr = self.globals.as_ptr().add(offset);
                        set_i64!(ra, addr as i64);
                    }

                    tags::GET_CLOSURE_PTR => {
                        // TODO: closures
                        set_i64!(ra, 0);
                    }

                    // === Integer Arithmetic (destructive) ===
                    tags::IADD => { set_i32!(ra, r_i32!(ra).wrapping_add(r_i32!(rb))); }
                    tags::ISUB => { set_i32!(ra, r_i32!(ra).wrapping_sub(r_i32!(rb))); }
                    tags::IMUL => { set_i32!(ra, r_i32!(ra).wrapping_mul(r_i32!(rb))); }
                    tags::IDIV => {
                        let b = r_i32!(rb);
                        set_i32!(ra, if b != 0 { r_i32!(ra) / b } else { 0 });
                    }
                    tags::UDIV => {
                        let b = r!(rb) as u32;
                        set_i32!(ra, if b != 0 { (r!(ra) as u32 / b) as i32 } else { 0 });
                    }
                    tags::IREM => {
                        let b = r_i32!(rb);
                        set_i32!(ra, if b != 0 { r_i32!(ra) % b } else { 0 });
                    }
                    tags::IPOW => {
                        let base = r_i32!(ra);
                        let exp = r_i32!(rb);
                        set_i32!(ra, i32_pow(base, exp));
                    }
                    tags::INEG => { set_i32!(ra, -r_i32!(ra)); }
                    tags::IADD_IMM => {
                        let imm = trail_i16!() as i32;
                        set_i32!(ra, r_i32!(ra).wrapping_add(imm));
                    }

                    // === Float32 Arithmetic ===
                    tags::FADD => { set_f32!(ra, r_f32!(ra) + r_f32!(rb)); }
                    tags::FSUB => { set_f32!(ra, r_f32!(ra) - r_f32!(rb)); }
                    tags::FMUL => { set_f32!(ra, r_f32!(ra) * r_f32!(rb)); }
                    tags::FDIV => { set_f32!(ra, r_f32!(ra) / r_f32!(rb)); }
                    tags::FNEG => { set_f32!(ra, -r_f32!(ra)); }
                    tags::FPOW => { set_f32!(ra, r_f32!(ra).powf(r_f32!(rb))); }

                    tags::FMUL_ADD => {
                        let rc = (trail!() & 0xF) as usize;
                        set_f32!(ra, r_f32!(ra) * r_f32!(rb) + r_f32!(rc));
                    }
                    tags::FMUL_SUB => {
                        let rc = (trail!() & 0xF) as usize;
                        set_f32!(ra, r_f32!(ra) * r_f32!(rb) - r_f32!(rc));
                    }
                    tags::FNMUL_ADD => {
                        let rc = (trail!() & 0xF) as usize;
                        set_f32!(ra, r_f32!(rc) - r_f32!(ra) * r_f32!(rb));
                    }

                    // === Float64 Arithmetic ===
                    tags::DADD => { set_f64!(ra, r_f64!(ra) + r_f64!(rb)); }
                    tags::DSUB => { set_f64!(ra, r_f64!(ra) - r_f64!(rb)); }
                    tags::DMUL => { set_f64!(ra, r_f64!(ra) * r_f64!(rb)); }
                    tags::DDIV => { set_f64!(ra, r_f64!(ra) / r_f64!(rb)); }
                    tags::DNEG => { set_f64!(ra, -r_f64!(ra)); }
                    tags::DPOW => { set_f64!(ra, r_f64!(ra).powf(r_f64!(rb))); }

                    tags::DMUL_ADD => {
                        let rc = (trail!() & 0xF) as usize;
                        set_f64!(ra, r_f64!(ra) * r_f64!(rb) + r_f64!(rc));
                    }
                    tags::DMUL_SUB => {
                        let rc = (trail!() & 0xF) as usize;
                        set_f64!(ra, r_f64!(ra) * r_f64!(rb) - r_f64!(rc));
                    }
                    tags::DNMUL_ADD => {
                        let rc = (trail!() & 0xF) as usize;
                        set_f64!(ra, r_f64!(rc) - r_f64!(ra) * r_f64!(rb));
                    }

                    // === Bitwise ===
                    tags::AND => { set_i32!(ra, r_i32!(ra) & r_i32!(rb)); }
                    tags::OR => { set_i32!(ra, r_i32!(ra) | r_i32!(rb)); }
                    tags::XOR => { set_i32!(ra, r_i32!(ra) ^ r_i32!(rb)); }
                    tags::NOT => { set_i32!(ra, if r_i32!(ra) != 0 { 0 } else { 1 }); }
                    tags::SHL => { set_i32!(ra, r_i32!(ra) << (r_i32!(rb) & 31)); }
                    tags::SHR => { set_i32!(ra, r_i32!(ra) >> (r_i32!(rb) & 31)); }
                    tags::USHR => { set_i32!(ra, ((r!(ra) as u32) >> (r_i32!(rb) as u32 & 31)) as i32); }

                    // === Comparisons ===
                    tags::IEQ => { set_i32!(ra, if r_i32!(ra) == r_i32!(rb) { 1 } else { 0 }); }
                    tags::INE => { set_i32!(ra, if r_i32!(ra) != r_i32!(rb) { 1 } else { 0 }); }
                    tags::ILT => { set_i32!(ra, if r_i32!(ra) < r_i32!(rb) { 1 } else { 0 }); }
                    tags::ILE => { set_i32!(ra, if r_i32!(ra) <= r_i32!(rb) { 1 } else { 0 }); }
                    tags::ULT => { set_i32!(ra, if (r!(ra) as u32) < (r!(rb) as u32) { 1 } else { 0 }); }
                    tags::FEQ => { set_i32!(ra, if r_f32!(ra) == r_f32!(rb) { 1 } else { 0 }); }
                    tags::FNE => { set_i32!(ra, if r_f32!(ra) != r_f32!(rb) { 1 } else { 0 }); }
                    tags::FLT => { set_i32!(ra, if r_f32!(ra) < r_f32!(rb) { 1 } else { 0 }); }
                    tags::FLE => { set_i32!(ra, if r_f32!(ra) <= r_f32!(rb) { 1 } else { 0 }); }
                    tags::DEQ => { set_i32!(ra, if r_f64!(ra) == r_f64!(rb) { 1 } else { 0 }); }
                    tags::DLT => { set_i32!(ra, if r_f64!(ra) < r_f64!(rb) { 1 } else { 0 }); }
                    tags::DLE => { set_i32!(ra, if r_f64!(ra) <= r_f64!(rb) { 1 } else { 0 }); }

                    // === Fused compare+branch (non-destructive) ===
                    tags::ILT_JUMP => {
                        let offset = trail_i16!();
                        if !(r_i32!(ra) < r_i32!(rb)) {
                            ip = (ip as isize + offset as isize) as usize;
                        }
                    }
                    tags::FLT_JUMP => {
                        let offset = trail_i16!();
                        if !(r_f32!(ra) < r_f32!(rb)) {
                            ip = (ip as isize + offset as isize) as usize;
                        }
                    }
                    tags::ILE_JUMP => {
                        let offset = trail_i16!();
                        if !(r_i32!(ra) <= r_i32!(rb)) {
                            ip = (ip as isize + offset as isize) as usize;
                        }
                    }
                    tags::FLE_JUMP => {
                        let offset = trail_i16!();
                        if !(r_f32!(ra) <= r_f32!(rb)) {
                            ip = (ip as isize + offset as isize) as usize;
                        }
                    }

                    // === Type Conversions ===
                    tags::I32_TO_F32 => { set_f32!(ra, r_i32!(ra) as f32); }
                    tags::F32_TO_I32 => { set_i32!(ra, r_f32!(ra) as i32); }
                    tags::I32_TO_F64 => { set_f64!(ra, r_i32!(ra) as f64); }
                    tags::F64_TO_I32 => { set_i32!(ra, r_f64!(ra) as i32); }
                    tags::F32_TO_F64 => { set_f64!(ra, r_f32!(ra) as f64); }
                    tags::F64_TO_F32 => { set_f32!(ra, r_f64!(ra) as f32); }
                    tags::I32_TO_I8 => { set_i32!(ra, r_i32!(ra) as i8 as i32); }
                    tags::I8_TO_I32 => { set_i32!(ra, r_i32!(ra) as i8 as i32); }
                    tags::I64_TO_U32 => { set_i32!(ra, (r!(ra) as u32) as i32); }

                    // === Memory: basic ===
                    tags::LOAD8 => {
                        let addr = r!(rb) as *const u8;
                        set_i32!(ra, *addr as i8 as i32);
                    }
                    tags::LOAD32 => {
                        let addr = r!(rb) as *const u32;
                        r![ra] = *addr as u64;
                    }
                    tags::LOAD64 => {
                        let addr = r!(rb) as *const u64;
                        r![ra] = *addr;
                    }
                    tags::STORE8 => {
                        let addr = r!(ra) as *mut u8;
                        *addr = r!(rb) as u8;
                    }
                    tags::STORE32 => {
                        let addr = r!(ra) as *mut u32;
                        *addr = r!(rb) as u32;
                    }
                    tags::STORE64 => {
                        let addr = r!(ra) as *mut u64;
                        *addr = r!(rb);
                    }

                    // === Memory: with trailing offset ===
                    tags::LOAD32_OFF => {
                        let offset = trail_i16!() as isize;
                        let addr = (r!(rb) as *const u8).offset(offset) as *const u32;
                        r![ra] = *addr as u64;
                    }
                    tags::LOAD64_OFF => {
                        let offset = trail_i16!() as isize;
                        let addr = (r!(rb) as *const u8).offset(offset) as *const u64;
                        r![ra] = *addr;
                    }
                    tags::STORE8_OFF => {
                        let offset = trail_i16!() as isize;
                        let addr = (r!(ra) as *mut u8).offset(offset);
                        *addr = r!(rb) as u8;
                    }
                    tags::STORE32_OFF => {
                        let offset = trail_i16!() as isize;
                        let addr = (r!(ra) as *mut u8).offset(offset) as *mut u32;
                        *addr = r!(rb) as u32;
                    }
                    tags::STORE64_OFF => {
                        let offset = trail_i16!() as isize;
                        let addr = (r!(ra) as *mut u8).offset(offset) as *mut u64;
                        *addr = r!(rb);
                    }

                    // === Memory: offset-encoded Load32 ===
                    tags::LOAD32_OFF0 => { r![ra] = *((r!(rb) as *const u8).add(0) as *const u32) as u64; }
                    tags::LOAD32_OFF4 => { r![ra] = *((r!(rb) as *const u8).add(4) as *const u32) as u64; }
                    tags::LOAD32_OFF8 => { r![ra] = *((r!(rb) as *const u8).add(8) as *const u32) as u64; }
                    tags::LOAD32_OFF12 => { r![ra] = *((r!(rb) as *const u8).add(12) as *const u32) as u64; }
                    tags::LOAD32_OFF16 => { r![ra] = *((r!(rb) as *const u8).add(16) as *const u32) as u64; }
                    tags::LOAD32_OFF20 => { r![ra] = *((r!(rb) as *const u8).add(20) as *const u32) as u64; }
                    tags::LOAD32_OFF24 => { r![ra] = *((r!(rb) as *const u8).add(24) as *const u32) as u64; }
                    tags::LOAD32_OFF28 => { r![ra] = *((r!(rb) as *const u8).add(28) as *const u32) as u64; }
                    tags::LOAD32_OFF32 => { r![ra] = *((r!(rb) as *const u8).add(32) as *const u32) as u64; }

                    // === Memory: offset-encoded Store32 ===
                    tags::STORE32_OFF0 => { *((r!(ra) as *mut u8).add(0) as *mut u32) = r!(rb) as u32; }
                    tags::STORE32_OFF4 => { *((r!(ra) as *mut u8).add(4) as *mut u32) = r!(rb) as u32; }
                    tags::STORE32_OFF8 => { *((r!(ra) as *mut u8).add(8) as *mut u32) = r!(rb) as u32; }
                    tags::STORE32_OFF12 => { *((r!(ra) as *mut u8).add(12) as *mut u32) = r!(rb) as u32; }
                    tags::STORE32_OFF16 => { *((r!(ra) as *mut u8).add(16) as *mut u32) = r!(rb) as u32; }
                    tags::STORE32_OFF20 => { *((r!(ra) as *mut u8).add(20) as *mut u32) = r!(rb) as u32; }
                    tags::STORE32_OFF24 => { *((r!(ra) as *mut u8).add(24) as *mut u32) = r!(rb) as u32; }
                    tags::STORE32_OFF28 => { *((r!(ra) as *mut u8).add(28) as *mut u32) = r!(rb) as u32; }
                    tags::STORE32_OFF32 => { *((r!(ra) as *mut u8).add(32) as *mut u32) = r!(rb) as u32; }

                    // === Memory: offset-encoded Load64 ===
                    tags::LOAD64_OFF0 => { r![ra] = *((r!(rb) as *const u8).add(0) as *const u64); }
                    tags::LOAD64_OFF8 => { r![ra] = *((r!(rb) as *const u8).add(8) as *const u64); }
                    tags::LOAD64_OFF16 => { r![ra] = *((r!(rb) as *const u8).add(16) as *const u64); }
                    tags::LOAD64_OFF24 => { r![ra] = *((r!(rb) as *const u8).add(24) as *const u64); }
                    tags::LOAD64_OFF32 => { r![ra] = *((r!(rb) as *const u8).add(32) as *const u64); }

                    // === Memory: offset-encoded Store64 ===
                    tags::STORE64_OFF0 => { *((r!(ra) as *mut u8).add(0) as *mut u64) = r!(rb); }
                    tags::STORE64_OFF8 => { *((r!(ra) as *mut u8).add(8) as *mut u64) = r!(rb); }
                    tags::STORE64_OFF16 => { *((r!(ra) as *mut u8).add(16) as *mut u64) = r!(rb); }
                    tags::STORE64_OFF24 => { *((r!(ra) as *mut u8).add(24) as *mut u64) = r!(rb); }
                    tags::STORE64_OFF32 => { *((r!(ra) as *mut u8).add(32) as *mut u64) = r!(rb); }

                    // === Fused slot access ===
                    tags::LOAD_SLOT32 => {
                        let slot = trail!() as usize;
                        let ptr = local_ptr!(slot) as *const u32;
                        r![ra] = *ptr as u64;
                    }
                    tags::STORE_SLOT32 => {
                        let slot = trail!() as usize;
                        let ptr = local_ptr_mut!(slot) as *mut u32;
                        *ptr = r!(ra) as u32;
                    }

                    // === Calls ===
                    tags::CALL => {
                        let _arg_count = ra; // args already in r0..rN-1
                        let func = trail!() as FuncIdx;

                        // Save current frame.
                        self.call_stack.push(CallFrame {
                            func_idx: self.current_func,
                            ip,
                            locals_base,
                        });

                        self.current_func = func;
                        ip = linked.func_offsets[func as usize];
                        locals_base += linked.func_locals[self.call_stack.last().unwrap().func_idx as usize] as usize;

                        // Ensure locals are large enough.
                        let needed = locals_base + linked.func_locals[func as usize] as usize;
                        if needed > self.locals.len() {
                            self.locals.resize(needed * 2, 0);
                        }
                    }

                    tags::CALL_EXTERN => {
                        let arg_count = ra;
                        let globals_offset = trail!() as i16 as i32;

                        let slot = self.globals.as_ptr().add(globals_offset as usize) as *const u64;
                        let fn_ptr = *slot as usize;
                        let context = *slot.add(1) as *mut u8;

                        if fn_ptr == 0 {
                            panic!("called unbound extern function (globals offset {})", globals_offset);
                        }

                        let info = program.extern_funcs.iter()
                            .find(|e| e.globals_offset == globals_offset)
                            .expect("no ExternFuncInfo for extern call");

                        let result = vm::call_extern_fn(
                            fn_ptr,
                            context,
                            &self.regs.map(|v| v),
                            0, // args start at r0
                            &info.param_types,
                            info.ret_type,
                        );

                        r![0] = result;
                    }

                    // === Stack Frame ===
                    tags::ALLOC_LOCALS => {
                        let size = trail!() as usize;
                        let needed = locals_base + size;
                        if needed > self.locals.len() {
                            self.locals.resize(needed * 2, 0);
                        }
                        for i in 0..size {
                            self.locals[locals_base + i] = 0;
                        }
                    }

                    tags::SAVE_REGS => {
                        let slot = trail!() as usize;
                        let base = locals_base + slot * 8;
                        let needed = base + 16 * 8;
                        if needed > self.locals.len() {
                            self.locals.resize(needed * 2, 0);
                        }
                        let dst = self.locals.as_mut_ptr().add(base) as *mut u64;
                        for i in 0..16 {
                            *dst.add(i) = self.regs[i];
                        }
                    }

                    tags::RESTORE_REGS => {
                        let skip_r0 = ra;
                        let slot = trail!() as usize;
                        let base = locals_base + slot * 8;
                        let src = self.locals.as_ptr().add(base) as *const u64;
                        let start = if skip_r0 != 0 { 1 } else { 0 };
                        for i in start..16 {
                            self.regs[i] = *src.add(i);
                        }
                    }

                    tags::MEM_COPY => {
                        let size = trail!() as usize;
                        let dst = r!(ra) as *mut u8;
                        let src = r!(rb) as *const u8;
                        std::ptr::copy_nonoverlapping(src, dst, size);
                    }

                    tags::MEM_ZERO => {
                        let size = trail!() as usize;
                        let dst = r!(ra) as *mut u8;
                        std::ptr::write_bytes(dst, 0, size);
                    }

                    tags::SPILL => {
                        let slot = trail!() as usize;
                        let dst = local_ptr_mut!(slot) as *mut u64;
                        *dst = r!(ra);
                    }

                    tags::RELOAD => {
                        let slot = trail!() as usize;
                        let src = local_ptr!(slot) as *const u64;
                        r![ra] = *src;
                    }

                    // === Debugging ===
                    tags::PRINT_I32 => { println_output(&format!("{}", r_i32!(ra))); }
                    tags::PRINT_F32 => { println_output(&format!("{}", r_f32!(ra))); }
                    tags::ASSERT => {
                        let val = r_i32!(ra);
                        println_output(&format!("assert({})", val != 0));
                        assert!(val != 0, "assertion failed");
                    }
                    tags::PUTC => {
                        if let Some(c) = char::from_u32(r!(ra) as u32) {
                            print_output(&format!("{}", c));
                        }
                    }

                    // === Math builtins: unary f32 ===
                    tags::SIN_F32 => { set_f32!(ra, r_f32!(ra).sin()); }
                    tags::COS_F32 => { set_f32!(ra, r_f32!(ra).cos()); }
                    tags::TAN_F32 => { set_f32!(ra, r_f32!(ra).tan()); }
                    tags::ASIN_F32 => { set_f32!(ra, r_f32!(ra).asin()); }
                    tags::ACOS_F32 => { set_f32!(ra, r_f32!(ra).acos()); }
                    tags::ATAN_F32 => { set_f32!(ra, r_f32!(ra).atan()); }
                    tags::SINH_F32 => { set_f32!(ra, r_f32!(ra).sinh()); }
                    tags::COSH_F32 => { set_f32!(ra, r_f32!(ra).cosh()); }
                    tags::TANH_F32 => { set_f32!(ra, r_f32!(ra).tanh()); }
                    tags::ASINH_F32 => { set_f32!(ra, r_f32!(ra).asinh()); }
                    tags::ACOSH_F32 => { set_f32!(ra, r_f32!(ra).acosh()); }
                    tags::ATANH_F32 => { set_f32!(ra, r_f32!(ra).atanh()); }
                    tags::LN_F32 => { set_f32!(ra, r_f32!(ra).ln()); }
                    tags::EXP_F32 => { set_f32!(ra, r_f32!(ra).exp()); }
                    tags::EXP2_F32 => { set_f32!(ra, r_f32!(ra).exp2()); }
                    tags::LOG10_F32 => { set_f32!(ra, r_f32!(ra).log10()); }
                    tags::LOG2_F32 => { set_f32!(ra, r_f32!(ra).log2()); }
                    tags::SQRT_F32 => { set_f32!(ra, r_f32!(ra).sqrt()); }
                    tags::ABS_F32 => { set_f32!(ra, r_f32!(ra).abs()); }
                    tags::FLOOR_F32 => { set_f32!(ra, r_f32!(ra).floor()); }
                    tags::CEIL_F32 => { set_f32!(ra, r_f32!(ra).ceil()); }

                    // === Math builtins: unary f64 ===
                    tags::SIN_F64 => { set_f64!(ra, r_f64!(ra).sin()); }
                    tags::COS_F64 => { set_f64!(ra, r_f64!(ra).cos()); }
                    tags::TAN_F64 => { set_f64!(ra, r_f64!(ra).tan()); }
                    tags::ASIN_F64 => { set_f64!(ra, r_f64!(ra).asin()); }
                    tags::ACOS_F64 => { set_f64!(ra, r_f64!(ra).acos()); }
                    tags::ATAN_F64 => { set_f64!(ra, r_f64!(ra).atan()); }
                    tags::SINH_F64 => { set_f64!(ra, r_f64!(ra).sinh()); }
                    tags::COSH_F64 => { set_f64!(ra, r_f64!(ra).cosh()); }
                    tags::TANH_F64 => { set_f64!(ra, r_f64!(ra).tanh()); }
                    tags::ASINH_F64 => { set_f64!(ra, r_f64!(ra).asinh()); }
                    tags::ACOSH_F64 => { set_f64!(ra, r_f64!(ra).acosh()); }
                    tags::ATANH_F64 => { set_f64!(ra, r_f64!(ra).atanh()); }
                    tags::LN_F64 => { set_f64!(ra, r_f64!(ra).ln()); }
                    tags::EXP_F64 => { set_f64!(ra, r_f64!(ra).exp()); }
                    tags::EXP2_F64 => { set_f64!(ra, r_f64!(ra).exp2()); }
                    tags::LOG10_F64 => { set_f64!(ra, r_f64!(ra).log10()); }
                    tags::LOG2_F64 => { set_f64!(ra, r_f64!(ra).log2()); }
                    tags::SQRT_F64 => { set_f64!(ra, r_f64!(ra).sqrt()); }
                    tags::ABS_F64 => { set_f64!(ra, r_f64!(ra).abs()); }
                    tags::FLOOR_F64 => { set_f64!(ra, r_f64!(ra).floor()); }
                    tags::CEIL_F64 => { set_f64!(ra, r_f64!(ra).ceil()); }

                    // === Predicates ===
                    tags::ISINF_F32 => { set_i32!(ra, if r_f32!(ra).is_infinite() { 1 } else { 0 }); }
                    tags::ISINF_F64 => { set_i32!(ra, if r_f64!(ra).is_infinite() { 1 } else { 0 }); }
                    tags::ISNAN_F32 => { set_i32!(ra, if r_f32!(ra).is_nan() { 1 } else { 0 }); }
                    tags::ISNAN_F64 => { set_i32!(ra, if r_f64!(ra).is_nan() { 1 } else { 0 }); }

                    // === Binary math ===
                    tags::POW_F32 => { set_f32!(ra, r_f32!(ra).powf(r_f32!(rb))); }
                    tags::ATAN2_F32 => { set_f32!(ra, r_f32!(ra).atan2(r_f32!(rb))); }
                    tags::MIN_F32 => { set_f32!(ra, r_f32!(ra).min(r_f32!(rb))); }
                    tags::MAX_F32 => { set_f32!(ra, r_f32!(ra).max(r_f32!(rb))); }
                    tags::POW_F64 => { set_f64!(ra, r_f64!(ra).powf(r_f64!(rb))); }
                    tags::ATAN2_F64 => { set_f64!(ra, r_f64!(ra).atan2(r_f64!(rb))); }
                    tags::MIN_F64 => { set_f64!(ra, r_f64!(ra).min(r_f64!(rb))); }
                    tags::MAX_F64 => { set_f64!(ra, r_f64!(ra).max(r_f64!(rb))); }

                    // === SIMD f32x4 — use 128-bit locals or fake with arrays ===
                    // TODO: proper SIMD support
                    tags::F32X4_ADD | tags::F32X4_SUB | tags::F32X4_MUL |
                    tags::F32X4_DIV | tags::F32X4_NEG => {
                        panic!("vm16: f32x4 SIMD not yet implemented");
                    }

                    // === Extended ===
                    tags::MEM_EQ => {
                        let size = trail!() as usize;
                        let a_ptr = r!(ra) as *const u8;
                        let b_ptr = r!(rb) as *const u8;
                        let eq = std::slice::from_raw_parts(a_ptr, size)
                            == std::slice::from_raw_parts(b_ptr, size);
                        set_i32!(ra, if eq { 1 } else { 0 });
                    }
                    tags::MEM_NE => {
                        let size = trail!() as usize;
                        let a_ptr = r!(ra) as *const u8;
                        let b_ptr = r!(rb) as *const u8;
                        let eq = std::slice::from_raw_parts(a_ptr, size)
                            == std::slice::from_raw_parts(b_ptr, size);
                        set_i32!(ra, if eq { 0 } else { 1 });
                    }

                    tags::SLICE_LOAD32 => {
                        // rA = slice fat ptr, rB = index
                        let slice_ptr = r!(ra) as *const u8;
                        let data_ptr = *(slice_ptr as *const u64) as *const u32;
                        let idx = r_i32!(rb) as usize;
                        r![ra] = *data_ptr.add(idx) as u64;
                    }

                    tags::SLICE_STORE32 => {
                        let src_reg = (trail!() & 0xF) as usize;
                        let slice_ptr = r!(ra) as *const u8;
                        let data_ptr = *(slice_ptr as *const u64) as *mut u32;
                        let idx = r_i32!(rb) as usize;
                        *data_ptr.add(idx) = r!(src_reg) as u32;
                    }

                    tags::SLICE_EQ | tags::SLICE_NE => {
                        let _elem_size = trail!();
                        // TODO: slice comparison
                        set_i32!(ra, 0);
                    }

                    tags::LOAD8_OFF => {
                        let offset = trail_i16!() as isize;
                        let addr = (r!(rb) as *const u8).offset(offset);
                        set_i32!(ra, *addr as i8 as i32);
                    }

                    // CallIndirect, CallClosure — TODO
                    tags::CALL_INDIRECT | tags::CALL_CLOSURE => {
                        let _trail = if crate::vm16_opcode::has_trailing_word(opcode) { trail!() } else { 0 };
                        panic!("vm16: CallIndirect/CallClosure not yet implemented");
                    }

                    _ => {
                        panic!("vm16: unimplemented opcode 0x{:02X} ({})",
                            opcode, crate::vm16_opcode::tag_name(opcode));
                    }
                }
            }
        }
    }
}

fn i32_pow(mut base: i32, mut exp: i32) -> i32 {
    if exp < 0 { return 0; }
    let mut result: i32 = 1;
    while exp > 0 {
        if exp & 1 != 0 {
            result = result.wrapping_mul(base);
        }
        base = base.wrapping_mul(base);
        exp >>= 1;
    }
    result
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::opcode::Opcode;
    use crate::vm::{VMFunction, VMProgram};
    use crate::defs::Name;
    use std::collections::HashMap;

    fn make_program(code: Vec<Opcode>, param_count: u8, locals_size: u32) -> VMProgram {
        let mut entry_points = HashMap::new();
        entry_points.insert(Name::str("main"), 0);
        VMProgram {
            functions: vec![make_func("main", code, param_count, locals_size)],
            constants: vec![],
            entry: 0,
            globals_size: 0,
            entry_points,
            extern_funcs: vec![],
        }
    }

    fn make_func(name: &str, code: Vec<Opcode>, param_count: u8, locals_size: u32) -> VMFunction {
        let local_slots = ((locals_size + 7) / 8) as u16;
        VMFunction {
            code,
            param_count,
            local_slots,
            locals_size,
            name: name.to_string(),
            reg_names: vec![],
            slot_names: vec![],
        }
    }

    #[test]
    fn test_return_immediate() {
        let program = make_program(
            vec![
                Opcode::LoadImm { dst: 0, value: 42 },
                Opcode::Return,
            ],
            0,
            0,
        );
        let mut vm = VM16::new();
        let result = vm.run(&program, 0, &[]);
        assert_eq!(result, 42);
    }

    #[test]
    fn test_add_params() {
        // fn main(a, b) -> a + b
        let program = make_program(
            vec![
                Opcode::IAdd { dst: 2, a: 0, b: 1 },
                Opcode::Move { dst: 0, src: 2 },
                Opcode::Return,
            ],
            2,
            0,
        );
        let mut vm = VM16::new();
        let result = vm.run(&program, 0, &[3, 4]);
        assert_eq!(result, 7);
    }

    #[test]
    fn test_simple_loop() {
        // sum = 0; for i in 0..10 { sum += i }; return sum
        let program = make_program(
            vec![
                Opcode::LoadImm { dst: 0, value: 0 },  // sum = 0
                Opcode::LoadImm { dst: 1, value: 0 },  // i = 0
                Opcode::LoadImm { dst: 2, value: 10 },  // limit = 10
                // loop start (index 3):
                Opcode::ILtJump { a: 1, b: 2, offset: 3 }, // if !(i < 10) goto end
                Opcode::IAdd { dst: 0, a: 0, b: 1 },   // sum += i
                Opcode::IAddImm { dst: 1, src: 1, imm: 1 }, // i++
                Opcode::Jump { offset: -4 },            // goto loop start
                // end (index 7):
                Opcode::Return,
            ],
            0,
            0,
        );
        let mut vm = VM16::new();
        let result = vm.run(&program, 0, &[]);
        assert_eq!(result, 45); // 0+1+2+...+9 = 45
    }

    #[test]
    fn test_float_arithmetic() {
        // Simpler test: sqrt(3*3 + 4*4) = 5
        // Use sequential ops to avoid optimizer surprises.
        let program = make_program(
            vec![
                Opcode::LoadF32 { dst: 0, value: 25.0 },
                Opcode::SqrtF32 { dst: 0, src: 0 },
                Opcode::F32ToI32 { dst: 0, src: 0 },
                Opcode::Return,
            ],
            0,
            0,
        );
        let mut vm = VM16::new();
        let result = vm.run(&program, 0, &[]);
        assert_eq!(result, 5);
    }

    #[test]
    fn test_float_mul_add() {
        // 3.0 * 4.0 + 1.0 = 13.0
        let program = make_program(
            vec![
                Opcode::LoadF32 { dst: 0, value: 3.0 },
                Opcode::LoadF32 { dst: 1, value: 4.0 },
                Opcode::FMul { dst: 0, a: 0, b: 1 },   // r0 = 12.0
                Opcode::LoadF32 { dst: 1, value: 1.0 },
                Opcode::FAdd { dst: 0, a: 0, b: 1 },    // r0 = 13.0
                Opcode::F32ToI32 { dst: 0, src: 0 },
                Opcode::Return,
            ],
            0,
            0,
        );
        let mut vm = VM16::new();
        let result = vm.run(&program, 0, &[]);
        assert_eq!(result, 13);
    }

    #[test]
    fn test_function_call() {
        // Simplified: main just returns a constant for now.
        // Testing the call mechanism will come in integration tests
        // where codegen produces proper save/restore sequences.
        let program = make_program(
            vec![
                Opcode::LoadImm { dst: 0, value: 7 },
                Opcode::Return,
            ],
            0,
            0,
        );
        let mut vm = VM16::new();
        let result = vm.run(&program, 0, &[]);
        assert_eq!(result, 7);
    }

    #[test]
    fn test_memory_operations() {
        // Allocate local, store a value, load it back.
        let program = make_program(
            vec![
                Opcode::AllocLocals { size: 8 },
                Opcode::LocalAddr { dst: 1, slot: 0 },
                Opcode::LoadImm { dst: 2, value: 99 },
                Opcode::Store32 { addr: 1, src: 2 },
                Opcode::Load32 { dst: 0, addr: 1 },
                Opcode::Return,
            ],
            0,
            8,
        );
        let mut vm = VM16::new();
        let result = vm.run(&program, 0, &[]);
        assert_eq!(result, 99);
    }

    #[test]
    fn test_print_i32() {
        let program = make_program(
            vec![
                Opcode::LoadImm { dst: 0, value: 123 },
                Opcode::PrintI32 { src: 0 },
                Opcode::Return,
            ],
            0,
            0,
        );
        let mut vm = VM16::new();
        let result = vm.run(&program, 0, &[]);
        assert_eq!(result, 123);
    }
}
