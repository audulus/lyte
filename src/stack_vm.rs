//! Stack-based virtual machine interpreter for StackProgram bytecode.

use crate::stack_ir::{StackOp, StackProgram};
use crate::vm::{print_output, println_output};

struct CallFrame {
    func_idx: u32,
    ip: usize,
    locals: Vec<u64>,
    local_memory_base: usize,
    operand_stack_base: usize,
}

pub struct StackVM {
    operand_stack: Vec<u64>,
    call_stack: Vec<CallFrame>,
    globals: Vec<u8>,
    local_memory: Vec<u8>,
    closure_ptr: u64,
}

fn ipow(mut base: i64, mut exp: u32) -> i64 {
    let mut result: i64 = 1;
    while exp > 0 {
        if exp & 1 == 1 {
            result = result.wrapping_mul(base);
        }
        base = base.wrapping_mul(base);
        exp >>= 1;
    }
    result
}

impl StackVM {
    pub fn new() -> Self {
        Self {
            operand_stack: Vec::with_capacity(1024),
            call_stack: Vec::with_capacity(256),
            globals: Vec::new(),
            local_memory: Vec::with_capacity(64 * 1024),
            closure_ptr: 0,
        }
    }

    #[inline(always)]
    fn push(&mut self, val: u64) {
        self.operand_stack.push(val);
    }

    #[inline(always)]
    fn pop(&mut self) -> u64 {
        self.operand_stack.pop().expect("stack underflow")
    }

    #[inline(always)]
    fn peek(&self) -> u64 {
        *self.operand_stack.last().expect("stack underflow")
    }

    fn enter_function(
        &mut self,
        program: &StackProgram,
        func_idx: u32,
        args: &[u64],
    ) -> (Vec<u64>, usize) {
        let func = &program.functions[func_idx as usize];
        let mut new_locals = vec![0u64; func.local_count as usize];
        for (i, &arg) in args.iter().enumerate() {
            new_locals[i] = arg;
        }
        let lm_base = self.local_memory.len();
        self.local_memory
            .resize(lm_base + func.local_memory as usize, 0);
        (new_locals, lm_base)
    }

    pub fn run(&mut self, program: &StackProgram) -> i64 {
        self.globals.resize(program.globals_size, 0);

        let mut func_idx = program.entry;
        let (mut locals, mut lm_base) = self.enter_function(program, func_idx, &[]);
        let mut ip: usize = 0;
        let mut os_base = self.operand_stack.len();

        loop {
            let func = &program.functions[func_idx as usize];
            if ip >= func.ops.len() {
                // Implicit return void at end of function.
                if let Some(frame) = self.call_stack.pop() {
                    self.local_memory.truncate(lm_base);
                    self.operand_stack.truncate(frame.operand_stack_base);
                    func_idx = frame.func_idx;
                    ip = frame.ip;
                    locals = frame.locals;
                    lm_base = frame.local_memory_base;
                    os_base = if let Some(f) = self.call_stack.last() {
                        f.operand_stack_base
                    } else {
                        0
                    };
                    continue;
                } else {
                    break;
                }
            }

            // Clone the op to avoid borrowing program across the match.
            let op = func.ops[ip].clone();
            ip += 1;

            match op {
                // === Constants ===
                StackOp::I64Const(v) => self.push(v as u64),
                StackOp::F32Const(v) => self.push(f32::to_bits(v) as u64),
                StackOp::F64Const(v) => self.push(f64::to_bits(v)),

                // === Local variables ===
                StackOp::LocalGet(n) => self.push(locals[n as usize]),
                StackOp::LocalSet(n) => {
                    locals[n as usize] = self.pop();
                }
                StackOp::LocalTee(n) => {
                    locals[n as usize] = self.peek();
                }
                StackOp::LocalAddr(slot) => {
                    let addr =
                        self.local_memory[lm_base + slot as usize * 8..].as_ptr() as u64;
                    self.push(addr);
                }

                // === Global variables ===
                StackOp::GlobalAddr(off) => {
                    let addr = self.globals[off as usize..].as_ptr() as u64;
                    self.push(addr);
                }

                // === Integer arithmetic ===
                StackOp::IAdd => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push((a as i64).wrapping_add(b as i64) as u64);
                }
                StackOp::ISub => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push((a as i64).wrapping_sub(b as i64) as u64);
                }
                StackOp::IMul => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push((a as i64).wrapping_mul(b as i64) as u64);
                }
                StackOp::IDiv => {
                    let b = self.pop() as i64;
                    let a = self.pop() as i64;
                    if b == 0 {
                        panic!("division by zero");
                    }
                    self.push(a.wrapping_div(b) as u64);
                }
                StackOp::UDiv => {
                    let b = self.pop();
                    let a = self.pop();
                    if b == 0 {
                        panic!("division by zero");
                    }
                    self.push(a / b);
                }
                StackOp::IRem => {
                    let b = self.pop() as i64;
                    let a = self.pop() as i64;
                    if b == 0 {
                        panic!("division by zero");
                    }
                    self.push(a.wrapping_rem(b) as u64);
                }
                StackOp::IPow => {
                    let b = self.pop() as u32;
                    let a = self.pop() as i64;
                    self.push(ipow(a, b) as u64);
                }
                StackOp::INeg => {
                    let a = self.pop() as i64;
                    self.push(a.wrapping_neg() as u64);
                }
                StackOp::IAddImm(imm) => {
                    let a = self.pop() as i64;
                    self.push(a.wrapping_add(imm as i64) as u64);
                }

                // === Float32 arithmetic ===
                StackOp::FAdd => {
                    let b = f32::from_bits(self.pop() as u32);
                    let a = f32::from_bits(self.pop() as u32);
                    self.push(f32::to_bits(a + b) as u64);
                }
                StackOp::FSub => {
                    let b = f32::from_bits(self.pop() as u32);
                    let a = f32::from_bits(self.pop() as u32);
                    self.push(f32::to_bits(a - b) as u64);
                }
                StackOp::FMul => {
                    let b = f32::from_bits(self.pop() as u32);
                    let a = f32::from_bits(self.pop() as u32);
                    self.push(f32::to_bits(a * b) as u64);
                }
                StackOp::FDiv => {
                    let b = f32::from_bits(self.pop() as u32);
                    let a = f32::from_bits(self.pop() as u32);
                    self.push(f32::to_bits(a / b) as u64);
                }
                StackOp::FPow => {
                    let b = f32::from_bits(self.pop() as u32);
                    let a = f32::from_bits(self.pop() as u32);
                    self.push(f32::to_bits(a.powf(b)) as u64);
                }
                StackOp::FNeg => {
                    let a = f32::from_bits(self.pop() as u32);
                    self.push(f32::to_bits(-a) as u64);
                }

                // === Float64 arithmetic ===
                StackOp::DAdd => {
                    let b = f64::from_bits(self.pop());
                    let a = f64::from_bits(self.pop());
                    self.push(f64::to_bits(a + b));
                }
                StackOp::DSub => {
                    let b = f64::from_bits(self.pop());
                    let a = f64::from_bits(self.pop());
                    self.push(f64::to_bits(a - b));
                }
                StackOp::DMul => {
                    let b = f64::from_bits(self.pop());
                    let a = f64::from_bits(self.pop());
                    self.push(f64::to_bits(a * b));
                }
                StackOp::DDiv => {
                    let b = f64::from_bits(self.pop());
                    let a = f64::from_bits(self.pop());
                    self.push(f64::to_bits(a / b));
                }
                StackOp::DPow => {
                    let b = f64::from_bits(self.pop());
                    let a = f64::from_bits(self.pop());
                    self.push(f64::to_bits(a.powf(b)));
                }
                StackOp::DNeg => {
                    let a = f64::from_bits(self.pop());
                    self.push(f64::to_bits(-a));
                }

                // === Comparisons ===
                StackOp::IEq => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(if a == b { 1 } else { 0 });
                }
                StackOp::INe => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(if a != b { 1 } else { 0 });
                }
                StackOp::ILt => {
                    let b = self.pop() as i64;
                    let a = self.pop() as i64;
                    self.push(if a < b { 1 } else { 0 });
                }
                StackOp::ILe => {
                    let b = self.pop() as i64;
                    let a = self.pop() as i64;
                    self.push(if a <= b { 1 } else { 0 });
                }
                StackOp::IGt => {
                    let b = self.pop() as i64;
                    let a = self.pop() as i64;
                    self.push(if a > b { 1 } else { 0 });
                }
                StackOp::IGe => {
                    let b = self.pop() as i64;
                    let a = self.pop() as i64;
                    self.push(if a >= b { 1 } else { 0 });
                }
                StackOp::ULt => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(if a < b { 1 } else { 0 });
                }
                StackOp::UGt => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(if a > b { 1 } else { 0 });
                }
                StackOp::FEq => {
                    let b = f32::from_bits(self.pop() as u32);
                    let a = f32::from_bits(self.pop() as u32);
                    self.push(if a == b { 1 } else { 0 });
                }
                StackOp::FNe => {
                    let b = f32::from_bits(self.pop() as u32);
                    let a = f32::from_bits(self.pop() as u32);
                    self.push(if a != b { 1 } else { 0 });
                }
                StackOp::FLt => {
                    let b = f32::from_bits(self.pop() as u32);
                    let a = f32::from_bits(self.pop() as u32);
                    self.push(if a < b { 1 } else { 0 });
                }
                StackOp::FLe => {
                    let b = f32::from_bits(self.pop() as u32);
                    let a = f32::from_bits(self.pop() as u32);
                    self.push(if a <= b { 1 } else { 0 });
                }
                StackOp::FGt => {
                    let b = f32::from_bits(self.pop() as u32);
                    let a = f32::from_bits(self.pop() as u32);
                    self.push(if a > b { 1 } else { 0 });
                }
                StackOp::FGe => {
                    let b = f32::from_bits(self.pop() as u32);
                    let a = f32::from_bits(self.pop() as u32);
                    self.push(if a >= b { 1 } else { 0 });
                }
                StackOp::DEq => {
                    let b = f64::from_bits(self.pop());
                    let a = f64::from_bits(self.pop());
                    self.push(if a == b { 1 } else { 0 });
                }
                StackOp::DLt => {
                    let b = f64::from_bits(self.pop());
                    let a = f64::from_bits(self.pop());
                    self.push(if a < b { 1 } else { 0 });
                }
                StackOp::DLe => {
                    let b = f64::from_bits(self.pop());
                    let a = f64::from_bits(self.pop());
                    self.push(if a <= b { 1 } else { 0 });
                }

                // === Bitwise ===
                StackOp::And => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(a & b);
                }
                StackOp::Or => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(a | b);
                }
                StackOp::Xor => {
                    let b = self.pop();
                    let a = self.pop();
                    self.push(a ^ b);
                }
                StackOp::Not => {
                    let a = self.pop();
                    self.push(!a);
                }
                StackOp::Shl => {
                    let b = self.pop() as u32;
                    let a = self.pop() as i64;
                    self.push((a << b) as u64);
                }
                StackOp::Shr => {
                    let b = self.pop() as u32;
                    let a = self.pop() as i64;
                    self.push((a >> b) as u64);
                }
                StackOp::UShr => {
                    let b = self.pop() as u32;
                    let a = self.pop();
                    self.push(a >> b);
                }

                // === Type conversions ===
                StackOp::I32ToF32 => {
                    let v = self.pop() as i32;
                    self.push(f32::to_bits(v as f32) as u64);
                }
                StackOp::F32ToI32 => {
                    let v = f32::from_bits(self.pop() as u32);
                    self.push(v as i32 as i64 as u64);
                }
                StackOp::I32ToF64 => {
                    let v = self.pop() as i32;
                    self.push(f64::to_bits(v as f64));
                }
                StackOp::F64ToI32 => {
                    let v = f64::from_bits(self.pop());
                    self.push(v as i32 as i64 as u64);
                }
                StackOp::F32ToF64 => {
                    let v = f32::from_bits(self.pop() as u32);
                    self.push(f64::to_bits(v as f64));
                }
                StackOp::F64ToF32 => {
                    let v = f64::from_bits(self.pop());
                    self.push(f32::to_bits(v as f32) as u64);
                }
                StackOp::I32ToI8 => {
                    let v = self.pop() as i32 as i8;
                    self.push(v as i64 as u64);
                }
                StackOp::I8ToI32 => {
                    let v = self.pop() as i8;
                    self.push(v as i32 as i64 as u64);
                }
                StackOp::I64ToU32 => {
                    let v = self.pop() as u32;
                    self.push(v as u64);
                }

                // === Memory loads ===
                StackOp::Load8 => {
                    let addr = self.pop() as *const i8;
                    let val = unsafe { *addr } as i64;
                    self.push(val as u64);
                }
                StackOp::Load32 => {
                    let addr = self.pop() as *const i32;
                    let val = unsafe { *addr } as i64;
                    self.push(val as u64);
                }
                StackOp::Load64 => {
                    let addr = self.pop() as *const u64;
                    let val = unsafe { *addr };
                    self.push(val);
                }
                StackOp::Load32Off(off) => {
                    let base = self.pop() as *const u8;
                    let addr = unsafe { base.offset(off as isize) } as *const i32;
                    let val = unsafe { *addr } as i64;
                    self.push(val as u64);
                }
                StackOp::Load64Off(off) => {
                    let base = self.pop() as *const u8;
                    let addr = unsafe { base.offset(off as isize) } as *const u64;
                    let val = unsafe { *addr };
                    self.push(val);
                }

                // === Memory stores ===
                StackOp::Store8 => {
                    let val = self.pop() as i8;
                    let addr = self.pop() as *mut i8;
                    unsafe { *addr = val };
                }
                StackOp::Store32 => {
                    let val = self.pop() as i32;
                    let addr = self.pop() as *mut i32;
                    unsafe { *addr = val };
                }
                StackOp::Store64 => {
                    let val = self.pop();
                    let addr = self.pop() as *mut u64;
                    unsafe { *addr = val };
                }
                StackOp::Store8Off(off) => {
                    let val = self.pop() as i8;
                    let base = self.pop() as *const u8;
                    let addr = unsafe { base.offset(off as isize) } as *mut i8;
                    unsafe { *addr = val };
                }
                StackOp::Store32Off(off) => {
                    let val = self.pop() as i32;
                    let base = self.pop() as *const u8;
                    let addr = unsafe { base.offset(off as isize) } as *mut i32;
                    unsafe { *addr = val };
                }
                StackOp::Store64Off(off) => {
                    let val = self.pop();
                    let base = self.pop() as *const u8;
                    let addr = unsafe { base.offset(off as isize) } as *mut u64;
                    unsafe { *addr = val };
                }

                // === Bulk memory ===
                StackOp::MemCopy(n) => {
                    let src = self.pop() as *const u8;
                    let dst = self.pop() as *mut u8;
                    unsafe { std::ptr::copy_nonoverlapping(src, dst, n as usize) };
                }
                StackOp::MemZero(n) => {
                    let dst = self.pop() as *mut u8;
                    unsafe { std::ptr::write_bytes(dst, 0, n as usize) };
                }
                StackOp::MemEq(n) => {
                    let b = self.pop() as *const u8;
                    let a = self.pop() as *const u8;
                    let eq = unsafe {
                        std::slice::from_raw_parts(a, n as usize)
                            == std::slice::from_raw_parts(b, n as usize)
                    };
                    self.push(if eq { 1 } else { 0 });
                }
                StackOp::MemNe(n) => {
                    let b = self.pop() as *const u8;
                    let a = self.pop() as *const u8;
                    let eq = unsafe {
                        std::slice::from_raw_parts(a, n as usize)
                            == std::slice::from_raw_parts(b, n as usize)
                    };
                    self.push(if eq { 0 } else { 1 });
                }

                // === Slice operations ===
                StackOp::SliceEq(elem_size) => {
                    let fat_b = self.pop() as *const u8;
                    let fat_a = self.pop() as *const u8;
                    unsafe {
                        let data_a = *(fat_a as *const *const u8);
                        let len_a = *(fat_a.add(8) as *const i32);
                        let data_b = *(fat_b as *const *const u8);
                        let len_b = *(fat_b.add(8) as *const i32);
                        if len_a != len_b {
                            self.push(0);
                        } else {
                            let total = len_a as usize * elem_size as usize;
                            let eq = std::slice::from_raw_parts(data_a, total)
                                == std::slice::from_raw_parts(data_b, total);
                            self.push(if eq { 1 } else { 0 });
                        }
                    }
                }
                StackOp::SliceNe(elem_size) => {
                    let fat_b = self.pop() as *const u8;
                    let fat_a = self.pop() as *const u8;
                    unsafe {
                        let data_a = *(fat_a as *const *const u8);
                        let len_a = *(fat_a.add(8) as *const i32);
                        let data_b = *(fat_b as *const *const u8);
                        let len_b = *(fat_b.add(8) as *const i32);
                        if len_a != len_b {
                            self.push(1);
                        } else {
                            let total = len_a as usize * elem_size as usize;
                            let eq = std::slice::from_raw_parts(data_a, total)
                                == std::slice::from_raw_parts(data_b, total);
                            self.push(if eq { 0 } else { 1 });
                        }
                    }
                }
                StackOp::SliceLoad32 => {
                    let index = self.pop() as i64;
                    let fat_ptr = self.pop() as *const u8;
                    unsafe {
                        let data_ptr = *(fat_ptr as *const *const u8);
                        let addr = data_ptr.offset(index as isize * 4) as *const i32;
                        self.push((*addr) as i64 as u64);
                    }
                }
                StackOp::SliceStore32 => {
                    let val = self.pop() as i32;
                    let index = self.pop() as i64;
                    let fat_ptr = self.pop() as *const u8;
                    unsafe {
                        let data_ptr = *(fat_ptr as *const *const u8);
                        let addr = data_ptr.offset(index as isize * 4) as *mut i32;
                        *addr = val;
                    }
                }

                // === Control flow ===
                StackOp::Jump(off) => {
                    ip = (ip as i64 + off as i64) as usize;
                }
                StackOp::JumpIfZero(off) => {
                    let cond = self.pop();
                    if cond == 0 {
                        ip = (ip as i64 + off as i64) as usize;
                    }
                }
                StackOp::JumpIfNotZero(off) => {
                    let cond = self.pop();
                    if cond != 0 {
                        ip = (ip as i64 + off as i64) as usize;
                    }
                }

                // === Function calls ===
                StackOp::Call { func, args } => {
                    let n = args as usize;
                    let stack_len = self.operand_stack.len();
                    let arg_values: Vec<u64> =
                        self.operand_stack[stack_len - n..].to_vec();
                    self.operand_stack.truncate(stack_len - n);

                    self.call_stack.push(CallFrame {
                        func_idx,
                        ip,
                        locals,
                        local_memory_base: lm_base,
                        operand_stack_base: os_base,
                    });

                    func_idx = func;
                    ip = 0;
                    os_base = self.operand_stack.len();
                    let (new_locals, new_lm_base) =
                        self.enter_function(program, func_idx, &arg_values);
                    locals = new_locals;
                    lm_base = new_lm_base;
                }
                StackOp::CallIndirect { args } => {
                    let target = self.pop() as u32;
                    let n = args as usize;
                    let stack_len = self.operand_stack.len();
                    let arg_values: Vec<u64> =
                        self.operand_stack[stack_len - n..].to_vec();
                    self.operand_stack.truncate(stack_len - n);

                    self.call_stack.push(CallFrame {
                        func_idx,
                        ip,
                        locals,
                        local_memory_base: lm_base,
                        operand_stack_base: os_base,
                    });

                    func_idx = target;
                    ip = 0;
                    os_base = self.operand_stack.len();
                    let (new_locals, new_lm_base) =
                        self.enter_function(program, func_idx, &arg_values);
                    locals = new_locals;
                    lm_base = new_lm_base;
                }
                StackOp::CallClosure { args } => {
                    let fat_ptr = self.pop() as *const u8;
                    let (target, closure_ptr) = unsafe {
                        let fid = *(fat_ptr as *const i64) as u32;
                        let cptr = *(fat_ptr.add(8) as *const u64);
                        (fid, cptr)
                    };

                    let n = args as usize;
                    let stack_len = self.operand_stack.len();
                    let arg_values: Vec<u64> =
                        self.operand_stack[stack_len - n..].to_vec();
                    self.operand_stack.truncate(stack_len - n);

                    self.call_stack.push(CallFrame {
                        func_idx,
                        ip,
                        locals,
                        local_memory_base: lm_base,
                        operand_stack_base: os_base,
                    });

                    self.closure_ptr = closure_ptr;
                    func_idx = target;
                    ip = 0;
                    os_base = self.operand_stack.len();
                    let (new_locals, new_lm_base) =
                        self.enter_function(program, func_idx, &arg_values);
                    locals = new_locals;
                    lm_base = new_lm_base;
                }

                StackOp::Return => {
                    let retval = self.pop();
                    if let Some(frame) = self.call_stack.pop() {
                        self.local_memory.truncate(lm_base);
                        self.operand_stack.truncate(frame.operand_stack_base);
                        func_idx = frame.func_idx;
                        ip = frame.ip;
                        locals = frame.locals;
                        lm_base = frame.local_memory_base;
                        os_base = if let Some(f) = self.call_stack.last() {
                            f.operand_stack_base
                        } else {
                            0
                        };
                        self.push(retval);
                    } else {
                        // Returning from entry function.
                        self.push(retval);
                        return retval as i64;
                    }
                }
                StackOp::ReturnVoid => {
                    if let Some(frame) = self.call_stack.pop() {
                        self.local_memory.truncate(lm_base);
                        self.operand_stack.truncate(frame.operand_stack_base);
                        func_idx = frame.func_idx;
                        ip = frame.ip;
                        locals = frame.locals;
                        lm_base = frame.local_memory_base;
                        os_base = if let Some(f) = self.call_stack.last() {
                            f.operand_stack_base
                        } else {
                            0
                        };
                    } else {
                        return 0;
                    }
                }

                // === Stack manipulation ===
                StackOp::Drop => {
                    self.pop();
                }

                // === Math builtins: f32 unary ===
                StackOp::SinF32 => { self.f32_unary(f32::sin); }
                StackOp::CosF32 => { self.f32_unary(f32::cos); }
                StackOp::TanF32 => { self.f32_unary(f32::tan); }
                StackOp::AsinF32 => { self.f32_unary(f32::asin); }
                StackOp::AcosF32 => { self.f32_unary(f32::acos); }
                StackOp::AtanF32 => { self.f32_unary(f32::atan); }
                StackOp::SinhF32 => { self.f32_unary(f32::sinh); }
                StackOp::CoshF32 => { self.f32_unary(f32::cosh); }
                StackOp::TanhF32 => { self.f32_unary(f32::tanh); }
                StackOp::AsinhF32 => { self.f32_unary(f32::asinh); }
                StackOp::AcoshF32 => { self.f32_unary(f32::acosh); }
                StackOp::AtanhF32 => { self.f32_unary(f32::atanh); }
                StackOp::LnF32 => { self.f32_unary(f32::ln); }
                StackOp::ExpF32 => { self.f32_unary(f32::exp); }
                StackOp::Exp2F32 => { self.f32_unary(f32::exp2); }
                StackOp::Log10F32 => { self.f32_unary(f32::log10); }
                StackOp::Log2F32 => { self.f32_unary(f32::log2); }
                StackOp::SqrtF32 => { self.f32_unary(f32::sqrt); }
                StackOp::AbsF32 => { self.f32_unary(f32::abs); }
                StackOp::FloorF32 => { self.f32_unary(f32::floor); }
                StackOp::CeilF32 => { self.f32_unary(f32::ceil); }

                // === Math builtins: f64 unary ===
                StackOp::SinF64 => { self.f64_unary(f64::sin); }
                StackOp::CosF64 => { self.f64_unary(f64::cos); }
                StackOp::TanF64 => { self.f64_unary(f64::tan); }
                StackOp::AsinF64 => { self.f64_unary(f64::asin); }
                StackOp::AcosF64 => { self.f64_unary(f64::acos); }
                StackOp::AtanF64 => { self.f64_unary(f64::atan); }
                StackOp::SinhF64 => { self.f64_unary(f64::sinh); }
                StackOp::CoshF64 => { self.f64_unary(f64::cosh); }
                StackOp::TanhF64 => { self.f64_unary(f64::tanh); }
                StackOp::AsinhF64 => { self.f64_unary(f64::asinh); }
                StackOp::AcoshF64 => { self.f64_unary(f64::acosh); }
                StackOp::AtanhF64 => { self.f64_unary(f64::atanh); }
                StackOp::LnF64 => { self.f64_unary(f64::ln); }
                StackOp::ExpF64 => { self.f64_unary(f64::exp); }
                StackOp::Exp2F64 => { self.f64_unary(f64::exp2); }
                StackOp::Log10F64 => { self.f64_unary(f64::log10); }
                StackOp::Log2F64 => { self.f64_unary(f64::log2); }
                StackOp::SqrtF64 => { self.f64_unary(f64::sqrt); }
                StackOp::AbsF64 => { self.f64_unary(f64::abs); }
                StackOp::FloorF64 => { self.f64_unary(f64::floor); }
                StackOp::CeilF64 => { self.f64_unary(f64::ceil); }

                // === Math builtins: binary ===
                StackOp::Atan2F32 => {
                    let b = f32::from_bits(self.pop() as u32);
                    let a = f32::from_bits(self.pop() as u32);
                    self.push(f32::to_bits(a.atan2(b)) as u64);
                }
                StackOp::Atan2F64 => {
                    let b = f64::from_bits(self.pop());
                    let a = f64::from_bits(self.pop());
                    self.push(f64::to_bits(a.atan2(b)));
                }

                // === Debug / IO ===
                StackOp::PrintI32 => {
                    let val = self.pop() as i32;
                    println_output(&format!("{}", val));
                }
                StackOp::PrintF32 => {
                    let val = f32::from_bits(self.pop() as u32);
                    println_output(&format!("{}", val));
                }
                StackOp::Putc => {
                    let val = self.pop() as u32;
                    if let Some(c) = char::from_u32(val) {
                        print_output(&format!("{}", c));
                    }
                }
                StackOp::Assert => {
                    let val = self.pop();
                    let ok = val != 0;
                    println_output(&format!("assert({})", ok));
                    if !ok {
                        panic!(
                            "Assertion failed at {}:{}",
                            program.functions[func_idx as usize].name,
                            ip - 1
                        );
                    }
                }

                StackOp::Halt => {
                    if self.operand_stack.is_empty() {
                        return 0;
                    } else {
                        return self.pop() as i64;
                    }
                }

                StackOp::Nop => {}
            }
        }

        // Entry function fell through without explicit return.
        if self.operand_stack.is_empty() {
            0
        } else {
            self.pop() as i64
        }
    }

    #[inline(always)]
    fn f32_unary(&mut self, f: fn(f32) -> f32) {
        let v = f32::from_bits(self.pop() as u32);
        self.push(f32::to_bits(f(v)) as u64);
    }

    #[inline(always)]
    fn f64_unary(&mut self, f: fn(f64) -> f64) {
        let v = f64::from_bits(self.pop());
        self.push(f64::to_bits(f(v)));
    }
}
