// Register-based virtual machine for when we can't JIT,
// like on iOS.

use std::convert::TryInto;
use std::fmt;

/// Register index (0-255)
pub type Reg = u8;

/// Instruction pointer offset for jumps
pub type Offset = i32;

/// Function index in the function table
pub type FuncIdx = u32;

/// Constant pool index
pub type ConstIdx = u32;

/// VM instructions for a register-based machine.
///
/// All values are stored as 64-bit in registers.
/// Memory operations handle different sizes based on type info
/// stored separately (not in the instruction stream).
#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Opcode {
    /// No operation
    Nop,

    /// Halt execution
    Halt,

    // ============ Register Operations ============

    /// Move register to register: dst = src
    Move { dst: Reg, src: Reg },

    /// Load immediate i64 into register
    LoadImm { dst: Reg, value: i64 },

    /// Load f32 immediate (stored as bits in i64)
    LoadF32 { dst: Reg, value: f32 },

    /// Load f64 immediate
    LoadF64 { dst: Reg, value: f64 },

    /// Load from constant pool
    LoadConst { dst: Reg, idx: ConstIdx },

    // ============ Integer Arithmetic ============

    /// Integer add: dst = a + b
    IAdd { dst: Reg, a: Reg, b: Reg },

    /// Integer subtract: dst = a - b
    ISub { dst: Reg, a: Reg, b: Reg },

    /// Integer multiply: dst = a * b
    IMul { dst: Reg, a: Reg, b: Reg },

    /// Signed integer divide: dst = a / b
    IDiv { dst: Reg, a: Reg, b: Reg },

    /// Unsigned integer divide: dst = a / b
    UDiv { dst: Reg, a: Reg, b: Reg },

    /// Integer remainder: dst = a % b
    IRem { dst: Reg, a: Reg, b: Reg },

    /// Integer power: dst = a ^ b
    IPow { dst: Reg, a: Reg, b: Reg },

    /// Integer negate: dst = -src
    INeg { dst: Reg, src: Reg },

    /// Integer add immediate: dst = src + imm
    IAddImm { dst: Reg, src: Reg, imm: i32 },

    // ============ Floating Point Arithmetic ============

    /// Float32 add: dst = a + b
    FAdd { dst: Reg, a: Reg, b: Reg },

    /// Float32 subtract: dst = a - b
    FSub { dst: Reg, a: Reg, b: Reg },

    /// Float32 multiply: dst = a * b
    FMul { dst: Reg, a: Reg, b: Reg },

    /// Float32 divide: dst = a / b
    FDiv { dst: Reg, a: Reg, b: Reg },

    /// Float32 negate: dst = -src
    FNeg { dst: Reg, src: Reg },

    /// Float32 power: dst = a ^ b
    FPow { dst: Reg, a: Reg, b: Reg },

    /// Float32 fused multiply-add: dst = a * b + c
    FMulAdd { dst: Reg, a: Reg, b: Reg, c: Reg },

    /// Float32 fused multiply-subtract: dst = a * b - c
    FMulSub { dst: Reg, a: Reg, b: Reg, c: Reg },

    // ============ Float64 Arithmetic ============

    /// Float64 add: dst = a + b
    DAdd { dst: Reg, a: Reg, b: Reg },

    /// Float64 subtract: dst = a - b
    DSub { dst: Reg, a: Reg, b: Reg },

    /// Float64 multiply: dst = a * b
    DMul { dst: Reg, a: Reg, b: Reg },

    /// Float64 divide: dst = a / b
    DDiv { dst: Reg, a: Reg, b: Reg },

    /// Float64 negate: dst = -src
    DNeg { dst: Reg, src: Reg },

    /// Float64 power: dst = a ^ b
    DPow { dst: Reg, a: Reg, b: Reg },

    /// Float64 fused multiply-add: dst = a * b + c
    DMulAdd { dst: Reg, a: Reg, b: Reg, c: Reg },

    /// Float64 fused multiply-subtract: dst = a * b - c
    DMulSub { dst: Reg, a: Reg, b: Reg, c: Reg },

    // ============ Bitwise Operations ============

    /// Bitwise AND: dst = a & b
    And { dst: Reg, a: Reg, b: Reg },

    /// Bitwise OR: dst = a | b
    Or { dst: Reg, a: Reg, b: Reg },

    /// Bitwise XOR: dst = a ^ b
    Xor { dst: Reg, a: Reg, b: Reg },

    /// Bitwise NOT: dst = !src
    Not { dst: Reg, src: Reg },

    /// Shift left: dst = a << b
    Shl { dst: Reg, a: Reg, b: Reg },

    /// Arithmetic shift right: dst = a >> b (signed)
    Shr { dst: Reg, a: Reg, b: Reg },

    /// Logical shift right: dst = a >>> b (unsigned)
    UShr { dst: Reg, a: Reg, b: Reg },

    // ============ Comparisons (result is 0 or 1) ============

    /// Integer equal: dst = (a == b)
    IEq { dst: Reg, a: Reg, b: Reg },

    /// Integer not equal: dst = (a != b)
    INe { dst: Reg, a: Reg, b: Reg },

    /// Signed less than: dst = (a < b)
    ILt { dst: Reg, a: Reg, b: Reg },

    /// Signed less or equal: dst = (a <= b)
    ILe { dst: Reg, a: Reg, b: Reg },

    /// Unsigned less than
    ULt { dst: Reg, a: Reg, b: Reg },

    /// Float32 equal
    FEq { dst: Reg, a: Reg, b: Reg },

    /// Float32 not equal
    FNe { dst: Reg, a: Reg, b: Reg },

    /// Float32 less than
    FLt { dst: Reg, a: Reg, b: Reg },

    /// Float32 less or equal
    FLe { dst: Reg, a: Reg, b: Reg },

    /// Memory equal: compare `size` bytes at pointers in a and b
    MemEq { dst: Reg, a: Reg, b: Reg, size: u32 },

    /// Memory not equal: compare `size` bytes at pointers in a and b
    MemNe { dst: Reg, a: Reg, b: Reg, size: u32 },

    /// Float64 equal
    DEq { dst: Reg, a: Reg, b: Reg },

    /// Float64 less than
    DLt { dst: Reg, a: Reg, b: Reg },

    /// Float64 less or equal
    DLe { dst: Reg, a: Reg, b: Reg },

    // ============ Type Conversions ============

    /// Convert i32 to f32
    I32ToF32 { dst: Reg, src: Reg },

    /// Convert f32 to i32
    F32ToI32 { dst: Reg, src: Reg },

    /// Convert i32 to f64
    I32ToF64 { dst: Reg, src: Reg },

    /// Convert f64 to i32
    F64ToI32 { dst: Reg, src: Reg },

    /// Convert f32 to f64
    F32ToF64 { dst: Reg, src: Reg },

    /// Convert f64 to f32
    F64ToF32 { dst: Reg, src: Reg },

    // ============ Memory Operations ============

    /// Load 8-bit value from memory: dst = mem[addr]
    Load8 { dst: Reg, addr: Reg },

    /// Load 32-bit value from memory
    Load32 { dst: Reg, addr: Reg },

    /// Load 64-bit value from memory
    Load64 { dst: Reg, addr: Reg },

    /// Load with offset: dst = mem[base + offset]
    Load32Off { dst: Reg, base: Reg, offset: i32 },

    /// Load 64-bit with offset
    Load64Off { dst: Reg, base: Reg, offset: i32 },

    /// Store 8-bit value to memory: mem[addr] = src
    Store8 { addr: Reg, src: Reg },

    /// Store 32-bit value to memory
    Store32 { addr: Reg, src: Reg },

    /// Store 64-bit value to memory
    Store64 { addr: Reg, src: Reg },

    /// Store 8-bit with offset: mem[base + offset] = src
    Store8Off { base: Reg, offset: i32, src: Reg },

    /// Store with offset: mem[base + offset] = src
    Store32Off { base: Reg, offset: i32, src: Reg },

    /// Store 64-bit with offset
    Store64Off { base: Reg, offset: i32, src: Reg },

    /// Get address of local variable slot
    LocalAddr { dst: Reg, slot: u16 },

    /// Get address of global variable at offset
    GlobalAddr { dst: Reg, offset: i32 },

    // ============ Control Flow ============

    /// Unconditional jump
    Jump { offset: Offset },

    /// Jump if register is zero (false)
    JumpIfZero { cond: Reg, offset: Offset },

    /// Jump if register is non-zero (true)
    JumpIfNotZero { cond: Reg, offset: Offset },

    // ============ Superinstructions: Compare and Branch ============

    /// Jump if a < b (signed): if !(a < b) jump
    ILtJump { a: Reg, b: Reg, offset: Offset },

    /// Call function by index, args in registers starting at `args_start`
    /// Result (if any) goes in register 0
    Call { func: FuncIdx, args_start: Reg, arg_count: u8 },

    /// Call function pointer in register
    CallIndirect { func_reg: Reg, args_start: Reg, arg_count: u8 },

    /// Return from function (return value in register 0)
    Return,

    /// Return with specific register
    ReturnReg { src: Reg },

    // ============ Stack Frame Operations ============

    /// Allocate stack space for locals (in bytes)
    AllocLocals { size: u32 },

    /// Copy bytes from src to dst
    MemCopy { dst: Reg, src: Reg, size: u32 },

    /// Zero memory region
    MemZero { dst: Reg, size: u32 },

    // ============ Register Save/Restore ============

    /// Save registers [start_reg..start_reg+count] to locals at slot offset.
    /// Each register is 8 bytes.
    SaveRegs { start_reg: Reg, count: u8, slot: u32 },

    /// Restore registers [start_reg..start_reg+count] from locals at slot offset.
    RestoreRegs { start_reg: Reg, count: u8, slot: u32 },

    // ============ Debugging ============

    /// Print integer value (for debugging)
    PrintI32 { src: Reg },

    /// Print float value
    PrintF32 { src: Reg },

    /// Assert value is non-zero
    Assert { src: Reg },

    /// Print a character
    Putc { src: Reg },

    // ============ Math Builtins ============

    /// Unary f32 math: dst = op(src)
    SinF32 { dst: Reg, src: Reg },
    CosF32 { dst: Reg, src: Reg },
    TanF32 { dst: Reg, src: Reg },
    LnF32 { dst: Reg, src: Reg },
    ExpF32 { dst: Reg, src: Reg },
    SqrtF32 { dst: Reg, src: Reg },
    AbsF32 { dst: Reg, src: Reg },
    FloorF32 { dst: Reg, src: Reg },
    CeilF32 { dst: Reg, src: Reg },

    /// Unary f64 math: dst = op(src)
    SinF64 { dst: Reg, src: Reg },
    CosF64 { dst: Reg, src: Reg },
    TanF64 { dst: Reg, src: Reg },
    LnF64 { dst: Reg, src: Reg },
    ExpF64 { dst: Reg, src: Reg },
    SqrtF64 { dst: Reg, src: Reg },
    AbsF64 { dst: Reg, src: Reg },
    FloorF64 { dst: Reg, src: Reg },
    CeilF64 { dst: Reg, src: Reg },

    /// Binary f32 math: dst = op(a, b)
    PowF32 { dst: Reg, a: Reg, b: Reg },
    Atan2F32 { dst: Reg, a: Reg, b: Reg },

    /// Binary f64 math: dst = op(a, b)
    PowF64 { dst: Reg, a: Reg, b: Reg },
    Atan2F64 { dst: Reg, a: Reg, b: Reg },
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
}

impl VMFunction {
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            param_count: 0,
            local_slots: 0,
            locals_size: 0,
            code: Vec::new(),
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
            Opcode::Jump { offset: off } |
            Opcode::JumpIfZero { offset: off, .. } |
            Opcode::JumpIfNotZero { offset: off, .. } |
            Opcode::ILtJump { offset: off, .. } => {
                *off = offset;
            }
            _ => panic!("patch_jump called on non-jump instruction at index {}: {:?}", idx, &self.code[idx]),
        }
    }
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
    registers: [u64; 256],

    /// Memory for local variables (stack-allocated per frame)
    locals: Vec<u8>,

    /// Call stack
    call_stack: Vec<CallFrame>,

    /// Heap memory for dynamic allocations
    heap: Vec<u8>,

    /// Global variable memory (zero-initialized)
    globals: Vec<u8>,

    /// Current instruction pointer
    ip: usize,

    /// Current function index
    current_func: FuncIdx,

    /// Base pointer for current frame's locals
    locals_base: usize,

    /// Current function name for debug diagnostics
    debug_func_name: String,

    /// Set to true if execution was cancelled via cancel_ptr()
    pub cancelled: bool,
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
            globals: Vec::new(), // Allocated when program runs
            ip: 0,
            current_func: 0,
            locals_base: 0,
            debug_func_name: String::new(),
            cancelled: false,
        }
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
        debug_assert!(offset < self.locals.len(), "local slot {slot} out of bounds (offset {offset}, locals size {})", self.locals.len());
        unsafe {
            self.locals.as_ptr().add(offset)
        }
    }

    /// Get mutable pointer to locals memory
    #[inline(always)]
    fn local_ptr_mut(&mut self, slot: u16) -> *mut u8 {
        let offset = self.locals_base + (slot as usize) * 8;
        debug_assert!(offset < self.locals.len(), "local slot {slot} out of bounds (offset {offset}, locals size {})", self.locals.len());
        unsafe {
            self.locals.as_mut_ptr().add(offset)
        }
    }

    /// In debug builds, panic if [ptr, ptr+size) is not within locals or globals.
    #[inline(always)]
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
            let in_globals = !self.globals.is_empty() && start >= globals_start && end <= globals_end;
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

    /// Run the program and return the result
    pub fn run(&mut self, program: &VMProgram) -> i64 {
        self.current_func = program.entry;
        self.ip = 0;
        self.locals_base = 0;
        self.debug_func_name = program.functions[program.entry as usize].name.clone();

        // Clear registers
        self.registers = [0; 256];

        // Allocate zero-initialized global memory
        self.globals = vec![0u8; program.globals_size];
        self.cancelled = false;

        loop {
            let func = &program.functions[self.current_func as usize];

            if self.ip >= func.code.len() {
                // End of function without return
                if self.call_stack.is_empty() {
                    return self.get_i64(0);
                }
                // Implicit return
                let frame = self.call_stack.pop().unwrap();
                self.current_func = frame.func_idx;
                self.ip = frame.ip;
                self.locals_base = frame.locals_base;
                continue;
            }

            let op = func.code[self.ip];
            self.ip += 1;

            match op {
                Opcode::Nop => {}

                Opcode::Halt => {
                    return self.get_i64(0);
                }

                // Register operations
                Opcode::Move { dst, src } => {
                    self.registers[dst as usize] = self.registers[src as usize];
                }

                Opcode::LoadImm { dst, value } => {
                    self.set_i64(dst, value);
                }

                Opcode::LoadF32 { dst, value } => {
                    self.set_f32(dst, value);
                }

                Opcode::LoadF64 { dst, value } => {
                    self.set_f64(dst, value);
                }

                Opcode::LoadConst { dst, idx } => {
                    self.set_u64(dst, program.constants[idx as usize]);
                }

                // Integer arithmetic
                Opcode::IAdd { dst, a, b } => {
                    let result = self.get_i64(a).wrapping_add(self.get_i64(b));
                    self.set_i64(dst, result);
                }

                Opcode::ISub { dst, a, b } => {
                    let result = self.get_i64(a).wrapping_sub(self.get_i64(b));
                    self.set_i64(dst, result);
                }

                Opcode::IMul { dst, a, b } => {
                    let result = self.get_i64(a).wrapping_mul(self.get_i64(b));
                    self.set_i64(dst, result);
                }

                Opcode::IDiv { dst, a, b } => {
                    let b_val = self.get_i64(b);
                    if b_val == 0 {
                        panic!("Division by zero");
                    }
                    self.set_i64(dst, self.get_i64(a) / b_val);
                }

                Opcode::UDiv { dst, a, b } => {
                    let b_val = self.get_u64(b);
                    if b_val == 0 {
                        panic!("Division by zero");
                    }
                    self.set_u64(dst, self.get_u64(a) / b_val);
                }

                Opcode::IRem { dst, a, b } => {
                    let b_val = self.get_i64(b);
                    if b_val == 0 {
                        panic!("Division by zero");
                    }
                    self.set_i64(dst, self.get_i64(a) % b_val);
                }

                Opcode::IPow { dst, a, b } => {
                    let base = self.get_i64(a);
                    let exp = self.get_i64(b) as u32;
                    self.set_i64(dst, base.wrapping_pow(exp));
                }

                Opcode::INeg { dst, src } => {
                    self.set_i64(dst, -self.get_i64(src));
                }

                Opcode::IAddImm { dst, src, imm } => {
                    self.set_i64(dst, self.get_i64(src).wrapping_add(imm as i64));
                }

                // Float32 arithmetic
                Opcode::FAdd { dst, a, b } => {
                    self.set_f32(dst, self.get_f32(a) + self.get_f32(b));
                }

                Opcode::FSub { dst, a, b } => {
                    self.set_f32(dst, self.get_f32(a) - self.get_f32(b));
                }

                Opcode::FMul { dst, a, b } => {
                    self.set_f32(dst, self.get_f32(a) * self.get_f32(b));
                }

                Opcode::FDiv { dst, a, b } => {
                    self.set_f32(dst, self.get_f32(a) / self.get_f32(b));
                }

                Opcode::FNeg { dst, src } => {
                    self.set_f32(dst, -self.get_f32(src));
                }

                Opcode::FPow { dst, a, b } => {
                    self.set_f32(dst, self.get_f32(a).powf(self.get_f32(b)));
                }

                Opcode::FMulAdd { dst, a, b, c } => {
                    self.set_f32(dst, self.get_f32(a).mul_add(self.get_f32(b), self.get_f32(c)));
                }

                Opcode::FMulSub { dst, a, b, c } => {
                    self.set_f32(dst, self.get_f32(a).mul_add(self.get_f32(b), -self.get_f32(c)));
                }

                // Float64 arithmetic
                Opcode::DAdd { dst, a, b } => {
                    self.set_f64(dst, self.get_f64(a) + self.get_f64(b));
                }

                Opcode::DSub { dst, a, b } => {
                    self.set_f64(dst, self.get_f64(a) - self.get_f64(b));
                }

                Opcode::DMul { dst, a, b } => {
                    self.set_f64(dst, self.get_f64(a) * self.get_f64(b));
                }

                Opcode::DDiv { dst, a, b } => {
                    self.set_f64(dst, self.get_f64(a) / self.get_f64(b));
                }

                Opcode::DNeg { dst, src } => {
                    self.set_f64(dst, -self.get_f64(src));
                }

                Opcode::DPow { dst, a, b } => {
                    self.set_f64(dst, self.get_f64(a).powf(self.get_f64(b)));
                }

                Opcode::DMulAdd { dst, a, b, c } => {
                    self.set_f64(dst, self.get_f64(a).mul_add(self.get_f64(b), self.get_f64(c)));
                }

                Opcode::DMulSub { dst, a, b, c } => {
                    self.set_f64(dst, self.get_f64(a).mul_add(self.get_f64(b), -self.get_f64(c)));
                }

                // Bitwise operations
                Opcode::And { dst, a, b } => {
                    self.set_u64(dst, self.get_u64(a) & self.get_u64(b));
                }

                Opcode::Or { dst, a, b } => {
                    self.set_u64(dst, self.get_u64(a) | self.get_u64(b));
                }

                Opcode::Xor { dst, a, b } => {
                    self.set_u64(dst, self.get_u64(a) ^ self.get_u64(b));
                }

                Opcode::Not { dst, src } => {
                    self.set_u64(dst, !self.get_u64(src));
                }

                Opcode::Shl { dst, a, b } => {
                    let shift = (self.get_u64(b) & 63) as u32;
                    self.set_u64(dst, self.get_u64(a) << shift);
                }

                Opcode::Shr { dst, a, b } => {
                    let shift = (self.get_u64(b) & 63) as u32;
                    self.set_i64(dst, self.get_i64(a) >> shift);
                }

                Opcode::UShr { dst, a, b } => {
                    let shift = (self.get_u64(b) & 63) as u32;
                    self.set_u64(dst, self.get_u64(a) >> shift);
                }

                // Integer comparisons
                Opcode::IEq { dst, a, b } => {
                    self.set_bool(dst, self.get_i64(a) == self.get_i64(b));
                }

                Opcode::INe { dst, a, b } => {
                    self.set_bool(dst, self.get_i64(a) != self.get_i64(b));
                }

                Opcode::ILt { dst, a, b } => {
                    self.set_bool(dst, self.get_i64(a) < self.get_i64(b));
                }

                Opcode::ILe { dst, a, b } => {
                    self.set_bool(dst, self.get_i64(a) <= self.get_i64(b));
                }

                Opcode::ULt { dst, a, b } => {
                    self.set_bool(dst, self.get_u64(a) < self.get_u64(b));
                }

                // Float32 comparisons
                Opcode::FEq { dst, a, b } => {
                    self.set_bool(dst, self.get_f32(a) == self.get_f32(b));
                }

                Opcode::FNe { dst, a, b } => {
                    self.set_bool(dst, self.get_f32(a) != self.get_f32(b));
                }

                Opcode::FLt { dst, a, b } => {
                    self.set_bool(dst, self.get_f32(a) < self.get_f32(b));
                }

                Opcode::FLe { dst, a, b } => {
                    self.set_bool(dst, self.get_f32(a) <= self.get_f32(b));
                }

                // Memory comparisons (for structs, tuples, arrays)
                Opcode::MemEq { dst, a, b, size } => {
                    let pa = self.get_u64(a);
                    let pb = self.get_u64(b);
                    self.check_ptr(pa, size as usize);
                    self.check_ptr(pb, size as usize);
                    let eq = unsafe {
                        std::slice::from_raw_parts(pa as *const u8, size as usize)
                            == std::slice::from_raw_parts(pb as *const u8, size as usize)
                    };
                    self.set_bool(dst, eq);
                }

                Opcode::MemNe { dst, a, b, size } => {
                    let pa = self.get_u64(a);
                    let pb = self.get_u64(b);
                    self.check_ptr(pa, size as usize);
                    self.check_ptr(pb, size as usize);
                    let ne = unsafe {
                        std::slice::from_raw_parts(pa as *const u8, size as usize)
                            != std::slice::from_raw_parts(pb as *const u8, size as usize)
                    };
                    self.set_bool(dst, ne);
                }

                // Float64 comparisons
                Opcode::DEq { dst, a, b } => {
                    self.set_bool(dst, self.get_f64(a) == self.get_f64(b));
                }

                Opcode::DLt { dst, a, b } => {
                    self.set_bool(dst, self.get_f64(a) < self.get_f64(b));
                }

                Opcode::DLe { dst, a, b } => {
                    self.set_bool(dst, self.get_f64(a) <= self.get_f64(b));
                }

                // Type conversions
                Opcode::I32ToF32 { dst, src } => {
                    self.set_f32(dst, (self.get_i64(src) as i32) as f32);
                }

                Opcode::F32ToI32 { dst, src } => {
                    self.set_i64(dst, self.get_f32(src) as i32 as i64);
                }

                Opcode::I32ToF64 { dst, src } => {
                    self.set_f64(dst, (self.get_i64(src) as i32) as f64);
                }

                Opcode::F64ToI32 { dst, src } => {
                    self.set_i64(dst, self.get_f64(src) as i32 as i64);
                }

                Opcode::F32ToF64 { dst, src } => {
                    self.set_f64(dst, self.get_f32(src) as f64);
                }

                Opcode::F64ToF32 { dst, src } => {
                    self.set_f32(dst, self.get_f64(src) as f32);
                }

                // Memory operations
                Opcode::Load8 { dst, addr } => {
                    let ptr = self.get_u64(addr);
                    self.check_ptr(ptr, 1);
                    unsafe { self.set_i64(dst, *(ptr as *const u8) as i64); }
                }

                Opcode::Load32 { dst, addr } => {
                    let ptr = self.get_u64(addr);
                    self.check_ptr(ptr, 4);
                    unsafe { self.set_i64(dst, *(ptr as *const i32) as i64); }
                }

                Opcode::Load64 { dst, addr } => {
                    let ptr = self.get_u64(addr);
                    self.check_ptr(ptr, 8);
                    unsafe { self.set_i64(dst, *(ptr as *const i64)); }
                }

                Opcode::Load32Off { dst, base, offset } => {
                    let ptr = (self.get_u64(base) as i64 + offset as i64) as u64;
                    self.check_ptr(ptr, 4);
                    unsafe { self.set_i64(dst, *(ptr as *const i32) as i64); }
                }

                Opcode::Load64Off { dst, base, offset } => {
                    let ptr = (self.get_u64(base) as i64 + offset as i64) as u64;
                    self.check_ptr(ptr, 8);
                    unsafe { self.set_i64(dst, *(ptr as *const i64)); }
                }

                Opcode::Store8 { addr, src } => {
                    let ptr = self.get_u64(addr);
                    self.check_ptr(ptr, 1);
                    unsafe { *(ptr as *mut u8) = self.get_u64(src) as u8; }
                }

                Opcode::Store32 { addr, src } => {
                    let ptr = self.get_u64(addr);
                    self.check_ptr(ptr, 4);
                    unsafe { *(ptr as *mut i32) = self.get_i64(src) as i32; }
                }

                Opcode::Store64 { addr, src } => {
                    let ptr = self.get_u64(addr);
                    self.check_ptr(ptr, 8);
                    unsafe { *(ptr as *mut i64) = self.get_i64(src); }
                }

                Opcode::Store8Off { base, offset, src } => {
                    let ptr = (self.get_u64(base) as i64 + offset as i64) as u64;
                    self.check_ptr(ptr, 1);
                    unsafe { *(ptr as *mut u8) = self.get_u64(src) as u8; }
                }

                Opcode::Store32Off { base, offset, src } => {
                    let ptr = (self.get_u64(base) as i64 + offset as i64) as u64;
                    self.check_ptr(ptr, 4);
                    unsafe { *(ptr as *mut i32) = self.get_i64(src) as i32; }
                }

                Opcode::Store64Off { base, offset, src } => {
                    let ptr = (self.get_u64(base) as i64 + offset as i64) as u64;
                    self.check_ptr(ptr, 8);
                    unsafe { *(ptr as *mut i64) = self.get_i64(src); }
                }

                Opcode::LocalAddr { dst, slot } => {
                    self.set_u64(dst, self.local_ptr(slot) as u64);
                }

                Opcode::GlobalAddr { dst, offset } => {
                    let addr = unsafe { self.globals.as_ptr().add(offset as usize) };
                    self.set_u64(dst, addr as u64);
                }

                // Control flow
                Opcode::Jump { offset } => {
                    self.ip = (self.ip as i32 + offset) as usize;
                    if offset < 0 && self.cancel_flag() {
                        self.cancelled = true;
                        return 0;
                    }
                }

                Opcode::JumpIfZero { cond, offset } => {
                    if self.get_u64(cond) == 0 {
                        self.ip = (self.ip as i32 + offset) as usize;
                        if offset < 0 && self.cancel_flag() {
                            self.cancelled = true;
                            return 0;
                        }
                    }
                }

                Opcode::JumpIfNotZero { cond, offset } => {
                    if self.get_u64(cond) != 0 {
                        self.ip = (self.ip as i32 + offset) as usize;
                        if offset < 0 && self.cancel_flag() {
                            self.cancelled = true;
                            return 0;
                        }
                    }
                }

                // Superinstructions: compare and branch
                Opcode::ILtJump { a, b, offset } => {
                    if self.get_i64(a) >= self.get_i64(b) {
                        self.ip = (self.ip as i32 + offset) as usize;
                        if offset < 0 && self.cancel_flag() {
                            self.cancelled = true;
                            return 0;
                        }
                    }
                }

                Opcode::Call { func, args_start, arg_count } => {
                    debug_assert!((func as usize) < program.functions.len(),
                        "VM: call to invalid function index {func} (have {} functions)",
                        program.functions.len());

                    // Save current frame
                    let frame = CallFrame {
                        func_idx: self.current_func,
                        ip: self.ip,
                        locals_base: self.locals_base,
                        return_reg: 0,
                    };
                    self.call_stack.push(frame);

                    // Copy arguments to registers 0..arg_count
                    let arg_start = args_start as usize;
                    for i in 0..arg_count as usize {
                        if i != arg_start + i {
                            self.registers[i] = self.registers[arg_start + i];
                        }
                    }

                    // Set up new frame
                    // Move locals_base past the current function's locals
                    let current_func = &program.functions[self.current_func as usize];
                    self.locals_base += current_func.locals_size as usize;

                    let new_func = &program.functions[func as usize];
                    self.current_func = func;
                    self.ip = 0;
                    self.debug_func_name = new_func.name.clone();

                    // Ensure we have enough locals space
                    let needed = self.locals_base + new_func.locals_size as usize;
                    if needed > self.locals.len() {
                        self.locals.resize(needed * 2, 0);
                    }
                }

                Opcode::CallIndirect { func_reg, args_start, arg_count } => {
                    let func_idx = self.get_u64(func_reg) as FuncIdx;

                    debug_assert!((func_idx as usize) < program.functions.len(),
                        "VM: indirect call to invalid function index {func_idx} (have {} functions)",
                        program.functions.len());

                    // Save current frame
                    let frame = CallFrame {
                        func_idx: self.current_func,
                        ip: self.ip,
                        locals_base: self.locals_base,
                        return_reg: 0,
                    };
                    self.call_stack.push(frame);

                    // Copy arguments
                    let arg_start = args_start as usize;
                    for i in 0..arg_count as usize {
                        if i != arg_start + i {
                            self.registers[i] = self.registers[arg_start + i];
                        }
                    }

                    // Set up new frame
                    // Move locals_base past the current function's locals
                    let current_func = &program.functions[self.current_func as usize];
                    self.locals_base += current_func.locals_size as usize;

                    self.debug_func_name = program.functions[func_idx as usize].name.clone();
                    self.current_func = func_idx;
                    self.ip = 0;
                }

                Opcode::Return => {
                    if self.call_stack.is_empty() {
                        return self.get_i64(0);
                    }
                    let frame = self.call_stack.pop().unwrap();
                    self.current_func = frame.func_idx;
                    self.ip = frame.ip;
                    self.locals_base = frame.locals_base;
                    self.debug_func_name = program.functions[frame.func_idx as usize].name.clone();
                }

                Opcode::ReturnReg { src } => {
                    // Move return value to r0
                    self.registers[0] = self.registers[src as usize];

                    if self.call_stack.is_empty() {
                        return self.get_i64(0);
                    }
                    let frame = self.call_stack.pop().unwrap();
                    self.current_func = frame.func_idx;
                    self.ip = frame.ip;
                    self.locals_base = frame.locals_base;
                    self.debug_func_name = program.functions[frame.func_idx as usize].name.clone();
                }

                Opcode::AllocLocals { size } => {
                    let needed = self.locals_base + size as usize;
                    if needed > self.locals.len() {
                        self.locals.resize(needed * 2, 0);
                    }
                    // Zero out the locals
                    for i in 0..size as usize {
                        self.locals[self.locals_base + i] = 0;
                    }
                }

                Opcode::MemCopy { dst, src, size } => {
                    let dst_ptr = self.get_u64(dst);
                    let src_ptr = self.get_u64(src);
                    self.check_ptr(dst_ptr, size as usize);
                    self.check_ptr(src_ptr, size as usize);
                    unsafe {
                        std::ptr::copy_nonoverlapping(src_ptr as *const u8, dst_ptr as *mut u8, size as usize);
                    }
                }

                Opcode::MemZero { dst, size } => {
                    let ptr = self.get_u64(dst);
                    self.check_ptr(ptr, size as usize);
                    unsafe {
                        std::ptr::write_bytes(ptr as *mut u8, 0, size as usize);
                    }
                }

                Opcode::SaveRegs { start_reg, count, slot } => {
                    let base = self.locals_base + slot as usize;
                    for i in 0..count as usize {
                        let reg_val = self.registers[start_reg as usize + i];
                        let offset = base + i * 8;
                        self.locals[offset..offset + 8].copy_from_slice(&reg_val.to_le_bytes());
                    }
                }

                Opcode::RestoreRegs { start_reg, count, slot } => {
                    let base = self.locals_base + slot as usize;
                    for i in 0..count as usize {
                        let offset = base + i * 8;
                        let bytes: [u8; 8] = self.locals[offset..offset + 8].try_into().unwrap();
                        self.registers[start_reg as usize + i] = u64::from_le_bytes(bytes);
                    }
                }

                // Debugging
                Opcode::PrintI32 { src } => {
                    println!("{}", self.get_i64(src) as i32);
                }

                Opcode::PrintF32 { src } => {
                    println!("{}", self.get_f32(src));
                }

                Opcode::Assert { src } => {
                    let val = self.get_u64(src) != 0;
                    println!("assert({})", val);
                    if !val {
                        panic!("Assertion failed at {}:{}",
                            program.functions[self.current_func as usize].name,
                            self.ip - 1);
                    }
                }

                Opcode::Putc { src } => {
                    let val = self.get_i64(src) as u32;
                    if let Some(c) = char::from_u32(val) {
                        print!("{}", c);
                    }
                }

                // Math builtins — f32 unary
                Opcode::SinF32 { dst, src } => { self.set_f32(dst, self.get_f32(src).sin()); }
                Opcode::CosF32 { dst, src } => { self.set_f32(dst, self.get_f32(src).cos()); }
                Opcode::TanF32 { dst, src } => { self.set_f32(dst, self.get_f32(src).tan()); }
                Opcode::LnF32 { dst, src } => { self.set_f32(dst, self.get_f32(src).ln()); }
                Opcode::ExpF32 { dst, src } => { self.set_f32(dst, self.get_f32(src).exp()); }
                Opcode::SqrtF32 { dst, src } => { self.set_f32(dst, self.get_f32(src).sqrt()); }
                Opcode::AbsF32 { dst, src } => { self.set_f32(dst, self.get_f32(src).abs()); }
                Opcode::FloorF32 { dst, src } => { self.set_f32(dst, self.get_f32(src).floor()); }
                Opcode::CeilF32 { dst, src } => { self.set_f32(dst, self.get_f32(src).ceil()); }

                // Math builtins — f64 unary
                Opcode::SinF64 { dst, src } => { self.set_f64(dst, self.get_f64(src).sin()); }
                Opcode::CosF64 { dst, src } => { self.set_f64(dst, self.get_f64(src).cos()); }
                Opcode::TanF64 { dst, src } => { self.set_f64(dst, self.get_f64(src).tan()); }
                Opcode::LnF64 { dst, src } => { self.set_f64(dst, self.get_f64(src).ln()); }
                Opcode::ExpF64 { dst, src } => { self.set_f64(dst, self.get_f64(src).exp()); }
                Opcode::SqrtF64 { dst, src } => { self.set_f64(dst, self.get_f64(src).sqrt()); }
                Opcode::AbsF64 { dst, src } => { self.set_f64(dst, self.get_f64(src).abs()); }
                Opcode::FloorF64 { dst, src } => { self.set_f64(dst, self.get_f64(src).floor()); }
                Opcode::CeilF64 { dst, src } => { self.set_f64(dst, self.get_f64(src).ceil()); }

                // Math builtins — f32 binary
                Opcode::PowF32 { dst, a, b } => { self.set_f32(dst, self.get_f32(a).powf(self.get_f32(b))); }
                Opcode::Atan2F32 { dst, a, b } => { self.set_f32(dst, self.get_f32(a).atan2(self.get_f32(b))); }

                // Math builtins — f64 binary
                Opcode::PowF64 { dst, a, b } => { self.set_f64(dst, self.get_f64(a).powf(self.get_f64(b))); }
                Opcode::Atan2F64 { dst, a, b } => { self.set_f64(dst, self.get_f64(a).atan2(self.get_f64(b))); }
            }
        }
    }

    /// Get a mutable pointer to the globals buffer.
    /// Only valid after `run` has been called.
    pub fn globals_ptr(&mut self) -> *mut u8 {
        self.globals.as_mut_ptr()
    }

    /// Returns a pointer to the cancel flag byte.
    /// Write a non-zero value from another thread to stop execution at the
    /// next backward jump.  The pointer is valid for the lifetime of this VM.
    pub fn cancel_ptr(&mut self) -> *mut u8 {
        &mut self.cancelled as *mut bool as *mut u8
    }

    #[inline(always)]
    fn cancel_flag(&self) -> bool {
        self.cancelled
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
        writeln!(f, "fn {} (params: {}, locals: {} bytes):",
            self.name, self.param_count, self.locals_size)?;

        for (i, op) in self.code.iter().enumerate() {
            writeln!(f, "  {:4}: {:?}", i, op)?;
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
    biquad.emit(Opcode::Load32Off { dst: 3, base: 1, offset: 0 });   // r3 = x1
    biquad.emit(Opcode::Load32Off { dst: 4, base: 1, offset: 4 });   // r4 = x2
    biquad.emit(Opcode::Load32Off { dst: 5, base: 1, offset: 8 });   // r5 = y1
    biquad.emit(Opcode::Load32Off { dst: 6, base: 1, offset: 12 });  // r6 = y2

    // Load coefficients: b0, b1, b2, a1, a2
    biquad.emit(Opcode::Load32Off { dst: 10, base: 2, offset: 0 });  // r10 = b0
    biquad.emit(Opcode::Load32Off { dst: 11, base: 2, offset: 4 });  // r11 = b1
    biquad.emit(Opcode::Load32Off { dst: 12, base: 2, offset: 8 });  // r12 = b2
    biquad.emit(Opcode::Load32Off { dst: 13, base: 2, offset: 12 }); // r13 = a1
    biquad.emit(Opcode::Load32Off { dst: 14, base: 2, offset: 16 }); // r14 = a2

    // Compute output using FMulAdd/FMulSub superinstructions:
    // y0 = b0*x0 + b1*x1 + b2*x2 - a1*y1 - a2*y2

    // r20 = b0 * x0
    biquad.emit(Opcode::FMul { dst: 20, a: 10, b: 0 });

    // r20 = b1 * x1 + r20  (b0*x0 + b1*x1)
    biquad.emit(Opcode::FMulAdd { dst: 20, a: 11, b: 3, c: 20 });

    // r20 = b2 * x2 + r20  (b0*x0 + b1*x1 + b2*x2)
    biquad.emit(Opcode::FMulAdd { dst: 20, a: 12, b: 4, c: 20 });

    // r20 = r20 - a1 * y1  (using FMulSub: r20 = a1*y1 - r20, then negate... or use different approach)
    // Actually FMulSub is: dst = a * b - c, so we need: r20 - a1*y1
    // Let's compute a1*y1 first, then subtract
    biquad.emit(Opcode::FMul { dst: 21, a: 13, b: 5 });   // r21 = a1 * y1
    biquad.emit(Opcode::FSub { dst: 20, a: 20, b: 21 }); // r20 = r20 - a1*y1

    // r20 = r20 - a2 * y2
    biquad.emit(Opcode::FMul { dst: 21, a: 14, b: 6 });   // r21 = a2 * y2
    biquad.emit(Opcode::FSub { dst: 20, a: 20, b: 21 }); // r20 = r20 - a2*y2 -> this is y0

    // Update state:
    // x2 = x1, x1 = x0, y2 = y1, y1 = y0
    biquad.emit(Opcode::Store32Off { base: 1, offset: 4, src: 3 });  // x2 = x1
    biquad.emit(Opcode::Store32Off { base: 1, offset: 0, src: 0 });  // x1 = x0
    biquad.emit(Opcode::Store32Off { base: 1, offset: 12, src: 5 }); // y2 = y1
    biquad.emit(Opcode::Store32Off { base: 1, offset: 8, src: 20 }); // y1 = y0

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
    main.emit(Opcode::LoadF32 { dst: 0, value: 0.0675 });
    main.emit(Opcode::Store32Off { base: 11, offset: 0, src: 0 });  // b0
    main.emit(Opcode::LoadF32 { dst: 0, value: 0.135 });
    main.emit(Opcode::Store32Off { base: 11, offset: 4, src: 0 });  // b1
    main.emit(Opcode::LoadF32 { dst: 0, value: 0.0675 });
    main.emit(Opcode::Store32Off { base: 11, offset: 8, src: 0 });  // b2
    main.emit(Opcode::LoadF32 { dst: 0, value: -1.143 });
    main.emit(Opcode::Store32Off { base: 11, offset: 12, src: 0 }); // a1
    main.emit(Opcode::LoadF32 { dst: 0, value: 0.413 });
    main.emit(Opcode::Store32Off { base: 11, offset: 16, src: 0 }); // a2

    // Loop counter
    main.emit(Opcode::LoadImm { dst: 20, value: 0 });        // r20 = i = 0
    main.emit(Opcode::LoadImm { dst: 21, value: 10000 });    // r21 = N = 10000

    // Accumulator for output
    main.emit(Opcode::LoadF32 { dst: 30, value: 0.0 });      // r30 = sum = 0.0

    // Loop start (instruction index for jump target)
    let loop_start = main.code.len();

    // Check i < N using superinstruction (jumps if NOT i < N)
    let jump_end = main.emit(Opcode::ILtJump { a: 20, b: 21, offset: 0 }); // patched later

    // Generate input: simple sawtooth wave
    // input = (i % 100) / 100.0 - 0.5
    main.emit(Opcode::LoadImm { dst: 23, value: 100 });
    main.emit(Opcode::IRem { dst: 24, a: 20, b: 23 });       // r24 = i % 100
    main.emit(Opcode::I32ToF32 { dst: 25, src: 24 });        // r25 = (float)(i % 100)
    main.emit(Opcode::LoadF32 { dst: 26, value: 100.0 });
    main.emit(Opcode::FDiv { dst: 0, a: 25, b: 26 });        // r0 = r25 / 100.0
    main.emit(Opcode::LoadF32 { dst: 26, value: 0.5 });
    main.emit(Opcode::FSub { dst: 0, a: 0, b: 26 });         // r0 = r0 - 0.5 (input)

    // Call biquad_process(input, &state, &coeffs)
    main.emit(Opcode::Move { dst: 1, src: 10 });             // r1 = &state
    main.emit(Opcode::Move { dst: 2, src: 11 });             // r2 = &coeffs
    main.emit(Opcode::Call { func: biquad_idx, args_start: 0, arg_count: 3 });

    // Accumulate output
    main.emit(Opcode::FAdd { dst: 30, a: 30, b: 0 });        // sum += output

    // i++ using superinstruction
    main.emit(Opcode::IAddImm { dst: 20, src: 20, imm: 1 });

    // Jump back to loop start
    let loop_end = main.code.len();
    main.emit(Opcode::Jump { offset: (loop_start as i32) - (loop_end as i32) - 1 });

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
        func.emit(Opcode::ILt { dst: 2, a: 0, b: 1 });  // 5 < 10 = true
        func.emit(Opcode::JumpIfZero { cond: 2, offset: 2 });
        func.emit(Opcode::LoadImm { dst: 0, value: 100 });  // if true
        func.emit(Opcode::Jump { offset: 1 });
        func.emit(Opcode::LoadImm { dst: 0, value: 200 });  // if false
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
        func.emit(Opcode::LoadImm { dst: 0, value: 0 });   // sum = 0
        func.emit(Opcode::LoadImm { dst: 1, value: 1 });   // i = 1
        func.emit(Opcode::LoadImm { dst: 2, value: 11 });  // limit = 11

        // Loop start (index 3)
        func.emit(Opcode::ILt { dst: 3, a: 1, b: 2 });     // i < 11?
        func.emit(Opcode::JumpIfZero { cond: 3, offset: 4 }); // exit if false
        func.emit(Opcode::IAdd { dst: 0, a: 0, b: 1 });    // sum += i
        func.emit(Opcode::LoadImm { dst: 4, value: 1 });
        func.emit(Opcode::IAdd { dst: 1, a: 1, b: 4 });    // i++
        func.emit(Opcode::Jump { offset: -6 });            // back to loop start

        func.emit(Opcode::Return);

        let mut program = VMProgram::new();
        program.entry = program.add_function(func);

        let mut vm = VM::new();
        let result = vm.run(&program);
        assert_eq!(result, 55);  // 1+2+3+...+10 = 55
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
        main.emit(Opcode::Call { func: 0, args_start: 0, arg_count: 1 });
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
        func.emit(Opcode::LoadImm { dst: 0, value: 12345 });
        func.emit(Opcode::Store32 { addr: 1, src: 0 });
        func.emit(Opcode::LoadImm { dst: 0, value: 0 });  // Clear r0
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
        func.emit(Opcode::LoadImm { dst: 0, value: 0b1010 });
        func.emit(Opcode::LoadImm { dst: 1, value: 0b1100 });
        func.emit(Opcode::And { dst: 2, a: 0, b: 1 });  // 0b1000 = 8
        func.emit(Opcode::Or { dst: 3, a: 0, b: 1 });   // 0b1110 = 14
        func.emit(Opcode::Xor { dst: 4, a: 0, b: 1 });  // 0b0110 = 6
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
        func.emit(Opcode::FAdd { dst: 1, a: 1, b: 2 });  // 42.5
        func.emit(Opcode::F32ToI32 { dst: 0, src: 1 });  // truncates to 42
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
        func.emit(Opcode::FMulAdd { dst: 0, a: 0, b: 1, c: 2 });
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
        func.emit(Opcode::FMulSub { dst: 0, a: 0, b: 1, c: 2 });
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
        func.emit(Opcode::IAddImm { dst: 0, src: 0, imm: 2 });
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
        func.emit(Opcode::ILtJump { a: 0, b: 1, offset: 2 });
        func.emit(Opcode::LoadImm { dst: 0, value: 100 });  // should execute
        func.emit(Opcode::Jump { offset: 1 });
        func.emit(Opcode::LoadImm { dst: 0, value: 200 });  // should skip
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
        func.emit(Opcode::ILtJump { a: 0, b: 1, offset: 2 });
        func.emit(Opcode::LoadImm { dst: 0, value: 100 });  // should skip
        func.emit(Opcode::Jump { offset: 1 });
        func.emit(Opcode::LoadImm { dst: 0, value: 200 });  // should execute
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
        func.emit(Opcode::LoadImm { dst: 0, value: 0 });   // sum = 0
        func.emit(Opcode::LoadImm { dst: 1, value: 1 });   // i = 1
        func.emit(Opcode::LoadImm { dst: 2, value: 11 });  // limit = 11

        // Loop start (index 3)
        // ILtJump: if !(i < 11) jump to end
        func.emit(Opcode::ILtJump { a: 1, b: 2, offset: 3 });
        func.emit(Opcode::IAdd { dst: 0, a: 0, b: 1 });    // sum += i
        func.emit(Opcode::IAddImm { dst: 1, src: 1, imm: 1 }); // i++
        func.emit(Opcode::Jump { offset: -4 });            // back to loop start

        func.emit(Opcode::Return);

        let mut program = VMProgram::new();
        program.entry = program.add_function(func);

        let mut vm = VM::new();
        let result = vm.run(&program);
        assert_eq!(result, 55);  // 1+2+3+...+10 = 55
    }

    #[test]
    fn test_infinite_loop_cancels() {
        // Infinite loop: Jump { offset: -1 } forever.
        let mut func = VMFunction::new("test");
        func.emit(Opcode::Jump { offset: -1 });

        let mut program = VMProgram::new();
        program.globals_size = 1; // room for the cancel flag
        program.entry = program.add_function(func);

        let mut vm = VM::new();

        struct CancelPtr(*mut u8);
        unsafe impl Send for CancelPtr {}
        let cancel = CancelPtr(vm.cancel_ptr());

        // After 50 ms, write 1 to the cancel flag.
        std::thread::spawn(move || {
            std::thread::sleep(std::time::Duration::from_millis(50));
            unsafe { *cancel.0 = 1; }
        });

        vm.run(&program);
        assert!(vm.cancelled, "expected the infinite loop to be cancelled");
    }
}
