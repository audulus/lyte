//! VM code generator.
//!
//! This module translates a DeclTable into a VMProgram that can be
//! executed by the register-based virtual machine.

use crate::decl::*;
use crate::defs::*;
use crate::expr::*;
use crate::types::*;
use crate::vm::*;
use crate::DeclTable;
use std::collections::{HashMap, HashSet};

/// A call that needs to be patched with the correct function index.
#[derive(Clone, Debug)]
struct PendingCall {
    /// Index of the function containing this call.
    func_idx: FuncIdx,
    /// Index of the Call instruction within that function.
    instr_idx: usize,
    /// Name of the function being called.
    callee: Name,
}

/// Code generator for the VM.
pub struct VMCodegen {
    /// The program being built.
    program: VMProgram,

    /// Map from function names to their indices in the program.
    func_indices: HashMap<Name, FuncIdx>,

    /// Functions that have been compiled.
    compiled_functions: HashSet<Name>,

    /// Functions that need to be compiled.
    pending_functions: Vec<Name>,

    /// Calls that need to be patched after all functions are compiled.
    pending_calls: Vec<PendingCall>,

    /// LoadImm instructions for function references that need patching.
    pending_func_loads: Vec<PendingCall>,

    /// Global variable offsets.
    globals: HashMap<Name, i32>,

    /// Counter for generating unique lambda names.
    lambda_counter: usize,
}

impl Default for VMCodegen {
    fn default() -> Self {
        Self::new()
    }
}

impl VMCodegen {
    pub fn new() -> Self {
        Self {
            program: VMProgram::new(),
            func_indices: HashMap::new(),
            compiled_functions: HashSet::new(),
            pending_functions: Vec::new(),
            pending_calls: Vec::new(),
            pending_func_loads: Vec::new(),
            globals: HashMap::new(),
            lambda_counter: 0,
        }
    }

    /// Collect global variables and compute their offsets.
    fn declare_globals(&mut self, decls: &DeclTable) {
        let mut offset: i32 = 0;
        for decl in &decls.decls {
            if let Decl::Global { name, ty } = decl {
                self.globals.insert(*name, offset);
                offset += ty.size(decls) as i32;
            }
        }
        self.program.globals_size = offset as usize;
    }

    /// Compile a DeclTable into a VMProgram.
    ///
    /// This looks for a "main" function and compiles it along with all
    /// functions it calls.
    pub fn compile(&mut self, decls: &DeclTable) -> Result<VMProgram, String> {
        // First, collect all global variables.
        self.declare_globals(decls);

        let main_name = Name::str("main");
        let main_decls = decls.find(main_name);

        if main_decls.is_empty() {
            return Err("no main function found".to_string());
        }

        let main_decl = match &main_decls[0] {
            Decl::Func(d) => d,
            _ => return Err("main is not a function".to_string()),
        };

        // Compile main function.
        self.compile_function(main_decl, decls)?;

        // Compile any pending functions (called by main or other functions).
        while let Some(name) = self.pending_functions.pop() {
            if self.compiled_functions.contains(&name) {
                continue;
            }

            let func_decls = decls.find(name);
            if func_decls.is_empty() {
                continue;
            }

            if let Decl::Func(func_decl) = &func_decls[0] {
                self.compile_function(func_decl, decls)?;
            }
        }

        // Set entry point to main.
        self.program.entry = *self.func_indices.get(&main_name).unwrap();

        // Patch all pending calls with the correct function indices.
        for pending in &self.pending_calls {
            if let Some(&callee_idx) = self.func_indices.get(&pending.callee) {
                let func = &mut self.program.functions[pending.func_idx as usize];
                if let Opcode::Call {
                    func: ref mut f, ..
                } = func.code[pending.instr_idx]
                {
                    *f = callee_idx;
                }
            }
        }

        // Patch all pending function reference loads with the correct function indices.
        for pending in &self.pending_func_loads {
            if let Some(&callee_idx) = self.func_indices.get(&pending.callee) {
                let func = &mut self.program.functions[pending.func_idx as usize];
                if let Opcode::LoadImm { ref mut value, .. } = func.code[pending.instr_idx] {
                    *value = callee_idx as i64;
                }
            }
        }

        // Phase 2: Strip NOPs now that all patching is done.
        for func in &mut self.program.functions {
            crate::vm_optimize::compact(&mut func.code);
        }

        Ok(std::mem::take(&mut self.program))
    }

    /// Compile a single function.
    fn compile_function(&mut self, decl: &FuncDecl, decls: &DeclTable) -> Result<FuncIdx, String> {
        let mut func = VMFunction::new(&*decl.name);
        func.param_count = decl.params.len() as u8;

        let mut translator = FunctionTranslator::new(
            decl,
            decls,
            &mut self.pending_functions,
            &mut self.lambda_counter,
            &self.globals,
        );
        translator.translate(&mut func);

        // Extract debug info: register and slot names for disassembly.
        for (&name, &reg) in &translator.variables {
            if translator.reg_promoted.contains(&name) {
                func.reg_names.push((reg, format!("{}", name)));
            }
        }
        for (&name, &slot) in &translator.local_slots {
            func.slot_names.push((slot, format!("{}", name)));
        }

        // Peephole optimize: eliminate redundant instructions + register allocation.
        if let Some((new_reg_count, mapping)) =
            crate::vm_optimize::optimize(&mut func.code, func.param_count as u8)
        {
            // Register allocation compacted the register numbering.
            // Update locals_size: slot area stays the same, register save area shrinks.
            func.locals_size = func.local_slots as u32 * 8 + new_reg_count as u32 * 8;
            // Update debug register names with the new physical register numbers.
            for entry in &mut func.reg_names {
                let preg = mapping[entry.0 as usize];
                if preg != Reg::MAX {
                    entry.0 = preg;
                }
            }
            // Remove entries where the register was optimized away.
            func.reg_names.retain(|&(reg, _)| reg != Reg::MAX);
        }

        let idx = self.program.add_function(func);
        self.func_indices.insert(decl.name, idx);
        self.compiled_functions.insert(decl.name);

        // Extract data from translator before it's dropped (it borrows self.pending_functions).
        let calls_to_patch = std::mem::take(&mut translator.calls_to_patch);
        let func_load_patches = std::mem::take(&mut translator.func_load_patches);
        let pending_lambdas = std::mem::take(&mut translator.pending_lambdas);
        let lambda_patches = std::mem::take(&mut translator.lambda_patches);
        drop(translator);

        // Collect pending calls.
        for call in calls_to_patch {
            self.pending_calls.push(PendingCall {
                func_idx: idx,
                instr_idx: call.instr_idx,
                callee: call.callee,
            });
        }

        // Collect pending function reference loads.
        for patch in func_load_patches {
            self.pending_func_loads.push(PendingCall {
                func_idx: idx,
                instr_idx: patch.instr_idx,
                callee: patch.callee,
            });
        }

        // Compile lambda functions and patch their indices into the parent function.
        for lambda_decl in pending_lambdas {
            let lambda_name = lambda_decl.name;
            let lambda_idx = self.compile_function(&lambda_decl, decls)?;
            // Patch every LoadImm placeholder for this lambda.
            for &(instr_idx, patch_name) in &lambda_patches {
                if patch_name == lambda_name {
                    if let Opcode::LoadImm { ref mut value, .. } =
                        self.program.functions[idx as usize].code[instr_idx]
                    {
                        *value = lambda_idx as i64;
                    }
                }
            }
        }

        Ok(idx)
    }

    /// Get or create a function index for a given name.
    fn get_or_create_func_index(&mut self, name: Name) -> FuncIdx {
        if let Some(&idx) = self.func_indices.get(&name) {
            idx
        } else {
            // Create a placeholder index - the function will be compiled later.
            let idx = self.program.functions.len() as FuncIdx;
            self.func_indices.insert(name, idx);
            self.pending_functions.push(name);
            idx
        }
    }
}

/// A call instruction that needs patching.
struct CallToPatch {
    /// Index of the Call instruction.
    instr_idx: usize,
    /// Name of the function being called.
    callee: Name,
}

/// Translator for a single function body.
struct FunctionTranslator<'a> {
    /// The function declaration being translated.
    decl: &'a FuncDecl,

    /// Declaration table for looking up types and functions.
    decls: &'a DeclTable,

    /// Map from variable names to their register numbers.
    /// For pointer types, the register holds the memory address.
    /// For register-promoted scalars, the register holds the value directly.
    variables: HashMap<Name, Reg>,

    /// Set of variable names that are register-promoted (value in register, not memory).
    reg_promoted: HashSet<Name>,

    /// Map from variable names to their local slot indices (for addressable vars).
    local_slots: HashMap<Name, u16>,

    /// Next available register.
    next_reg: Reg,

    /// Next available local slot.
    next_slot: u16,

    /// Total size of local variables in bytes.
    locals_size: u32,

    /// Functions that are called and need to be compiled.
    pending_functions: &'a mut Vec<Name>,

    /// Counter for generating unique lambda names.
    lambda_counter: &'a mut usize,

    /// Map from function name to expected function index.
    called_functions: HashMap<Name, FuncIdx>,

    /// Current next function index for placeholders.
    next_func_idx: FuncIdx,

    /// Calls that need patching.
    calls_to_patch: Vec<CallToPatch>,

    /// Lambda FuncDecls extracted from this function body, to be compiled afterward.
    pending_lambdas: Vec<FuncDecl>,

    /// LoadImm instructions that need to be patched with lambda function indices.
    lambda_patches: Vec<(usize, Name)>,

    /// LoadImm instructions that need to be patched with function indices (function references).
    func_load_patches: Vec<CallToPatch>,

    /// Global variable offsets.
    globals: &'a HashMap<Name, i32>,

    /// Output pointer register for functions returning pointer types.
    output_ptr: Option<Reg>,

    /// Local slot where the output pointer is saved (survives across calls).
    output_ptr_slot: Option<u16>,

    /// Tracks if a return has been emitted (so we don't emit epilogue).
    has_returned: bool,

    /// Byte offset in locals where registers are saved.
    save_regs_offset: u32,

    /// Variables captured from an enclosing scope (accessed via double indirection).
    captured_vars: HashSet<Name>,
}

/// Check if a type should be returned via output pointer.
fn returns_via_pointer(ty: TypeID) -> bool {
    matches!(
        &*ty,
        Type::Array(_, _) | Type::Slice(_) | Type::Name(_, _) | Type::Tuple(_)
    )
}

impl<'a> FunctionTranslator<'a> {
    fn new(
        decl: &'a FuncDecl,
        decls: &'a DeclTable,
        pending_functions: &'a mut Vec<Name>,
        lambda_counter: &'a mut usize,
        globals: &'a HashMap<Name, i32>,
    ) -> Self {
        Self {
            decl,
            decls,
            variables: HashMap::new(),
            reg_promoted: HashSet::new(),
            local_slots: HashMap::new(),
            next_reg: 0,
            next_slot: 0,
            locals_size: 0,
            pending_functions,
            lambda_counter,
            called_functions: HashMap::new(),
            next_func_idx: 0,
            calls_to_patch: Vec::new(),
            pending_lambdas: Vec::new(),
            lambda_patches: Vec::new(),
            func_load_patches: Vec::new(),
            globals,
            output_ptr: None,
            output_ptr_slot: None,
            has_returned: false,
            save_regs_offset: 0,
            captured_vars: HashSet::new(),
        }
    }

    /// Translate the function body.
    fn translate(&mut self, func: &mut VMFunction) {
        // If return type is a pointer type, first parameter is output pointer.
        if returns_via_pointer(self.decl.ret) {
            self.output_ptr = Some(self.alloc_reg());
            func.param_count += 1;
        }

        // Reserve registers for parameters (these are the incoming argument positions).
        let param_count = self.decl.params.len();
        for _ in 0..param_count {
            self.alloc_reg();
        }

        // Ensure r0 is reserved even for 0-parameter functions.
        // r0 is the return value register and is clobbered by every Call instruction.
        // Variables must never live in r0 across calls.
        if self.next_reg == 0 {
            self.alloc_reg();
        }

        // Emit SaveRegs with placeholders. Both count and slot will be patched
        // after translation when we know how many registers are used.
        // The save area is placed after all local variable slots to avoid
        // over-reserving space.
        func.emit(Opcode::SaveRegs {
            start_reg: 0,
            count: 0, // Placeholder - will be patched
            slot: 0,  // Placeholder - will be patched
        });

        // Save the output pointer to a local slot so it survives across calls.
        if let Some(out_reg) = self.output_ptr {
            let slot = self.alloc_local(8);
            self.output_ptr_slot = Some(slot);
            let addr = self.alloc_reg();
            func.emit(Opcode::LocalAddr { dst: addr, slot });
            func.emit(Opcode::Store64 { addr, src: out_reg });
        }

        // Handle parameters. Scalars stay in registers (SaveRegs preserves them
        // across calls). Pointer types are stored to local slots.
        let param_offset = if returns_via_pointer(self.decl.ret) {
            1u8
        } else {
            0u8
        };
        for (i, param) in self.decl.params.iter().enumerate() {
            let src_reg = i as Reg + param_offset as Reg;
            let ty = param.ty.expect("parameter must have type");

            if !self.is_ptr_type(&ty) {
                // Scalar parameter: copy to a dedicated register so the param
                // register can be reused. The copy will be eliminated by
                // move forwarding if possible.
                let reg = self.alloc_reg();
                func.emit(Opcode::Move {
                    dst: reg,
                    src: src_reg,
                });
                self.variables.insert(param.name, reg);
                self.reg_promoted.insert(param.name);
            } else {
                let size = self.vm_type_size(&ty);
                let slot = self.alloc_local(size);
                self.local_slots.insert(param.name, slot);
                let addr_reg = self.alloc_reg();
                func.emit(Opcode::LocalAddr {
                    dst: addr_reg,
                    slot,
                });
                self.emit_store(&ty, addr_reg, src_reg, func);
                self.variables.insert(param.name, addr_reg);
            }
        }

        // Set up captured closure variables.
        // The closure pointer was set by CallClosure before entering this function.
        if !self.decl.closure_vars.is_empty() {
            let closure_ptr_reg = self.alloc_reg();
            func.emit(Opcode::GetClosurePtr {
                dst: closure_ptr_reg,
            });
            for (i, cv) in self.decl.closure_vars.iter().enumerate() {
                // Load the address of the captured variable from closure_struct[i].
                let addr_reg = self.alloc_reg();
                func.emit(Opcode::Load64Off {
                    dst: addr_reg,
                    base: closure_ptr_reg,
                    offset: (i * 8) as i32,
                });
                // Store this address in a local slot so it survives across calls.
                let slot = self.alloc_local(8);
                self.local_slots.insert(cv.name, slot);
                let slot_addr = self.alloc_reg();
                func.emit(Opcode::LocalAddr {
                    dst: slot_addr,
                    slot,
                });
                func.emit(Opcode::Store64 {
                    addr: slot_addr,
                    src: addr_reg,
                });
                // The variable maps to the address of the captured storage (a pointer).
                // Access goes: load addr from local slot → load/store value through addr.
                self.variables.insert(cv.name, addr_reg);
                // Mark as a captured variable (accessed via double indirection).
                self.captured_vars.insert(cv.name);
            }
        }

        // Translate the body if present.
        if let Some(body) = self.decl.body {
            let result_reg = self.translate_expr(body, func);

            // Skip epilogue if we already emitted a return (e.g., explicit return statement).
            if !self.has_returned {
                // Return the result.
                if returns_via_pointer(self.decl.ret) {
                    // Reload output pointer from local slot (r0 may have been
                    // clobbered by subcalls).
                    let output = self.reload_output_ptr(func);
                    let size = self.decl.ret.size(self.decls) as u32;
                    func.emit(Opcode::MemCopy {
                        dst: output,
                        src: result_reg,
                        size,
                    });
                    // Restore all registers before return.
                    func.emit(Opcode::RestoreRegs {
                        start_reg: 0,
                        count: 0, // Placeholder - will be patched
                        slot: self.save_regs_offset,
                    });
                    func.emit(Opcode::Return);
                } else {
                    // Move result to r0, restore r1..N, return
                    if result_reg != 0 {
                        func.emit(Opcode::Move {
                            dst: 0,
                            src: result_reg,
                        });
                    }
                    func.emit(Opcode::RestoreRegs {
                        start_reg: 1, // Skip r0 which has the return value
                        count: 0,     // Placeholder - will be patched
                        slot: self.save_regs_offset + 8,
                    });
                    func.emit(Opcode::Return);
                }
            }
        } else {
            // No body - restore all registers and return
            func.emit(Opcode::RestoreRegs {
                start_reg: 0,
                count: 0, // Placeholder - will be patched
                slot: self.save_regs_offset,
            });
            func.emit(Opcode::Return);
        }

        // Now that translation is complete, place the save area after all variable
        // slots and patch SaveRegs/RestoreRegs with the actual count and offset.
        let reg_count = self.next_reg;
        self.save_regs_offset = self.next_slot as u32 * 8;
        self.locals_size = self.save_regs_offset + (reg_count as u32) * 8;

        for op in &mut func.code {
            match op {
                Opcode::SaveRegs { count, slot, .. } => {
                    *count = reg_count as u8;
                    *slot = self.save_regs_offset;
                }
                Opcode::RestoreRegs {
                    start_reg,
                    count,
                    slot,
                    ..
                } => {
                    if *start_reg == 1 {
                        *count = if reg_count > 1 { (reg_count - 1) as u8 } else { 0 };
                        *slot = self.save_regs_offset + 8;
                    } else {
                        *count = reg_count as u8;
                        *slot = self.save_regs_offset;
                    }
                }
                _ => {}
            }
        }

        // Set function metadata.
        func.locals_size = self.locals_size;
        func.local_slots = self.next_slot;
    }

    /// Allocate a new register.
    fn alloc_reg(&mut self) -> Reg {
        let reg = self.next_reg;
        self.next_reg += 1;
        reg
    }

    /// Allocate a local slot for a variable.
    fn alloc_local(&mut self, size: u32) -> u16 {
        let slot = self.next_slot;
        let slots_needed = ((size + 7) / 8) as u16; // Round up to 8-byte slots.
        self.next_slot += slots_needed;
        self.locals_size += slots_needed as u32 * 8;
        slot
    }

    /// Reload the output pointer from its local slot into a fresh register.
    fn reload_output_ptr(&mut self, func: &mut VMFunction) -> Reg {
        let slot = self.output_ptr_slot.expect("output_ptr_slot not set");
        let addr = self.alloc_reg();
        func.emit(Opcode::LocalAddr { dst: addr, slot });
        let ptr = self.alloc_reg();
        func.emit(Opcode::Load64 { dst: ptr, addr });
        ptr
    }

    /// Get the address of a variable's storage (for closure capture).
    /// For register-promoted vars, spills to a local slot first.
    fn get_var_address(&mut self, name: &Name, func: &mut VMFunction) -> Reg {
        if self.reg_promoted.contains(name) {
            // Register-promoted scalar: spill to a local slot so we have a stable address.
            let val_reg = *self.variables.get(name).unwrap();
            let slot = self.alloc_local(8);
            self.local_slots.insert(*name, slot);
            let addr = self.alloc_reg();
            func.emit(Opcode::LocalAddr { dst: addr, slot });
            func.emit(Opcode::Store64 { addr, src: val_reg });
            // Change variable from register-promoted to stack-allocated.
            self.reg_promoted.remove(name);
            self.variables.insert(*name, addr);
            addr
        } else if let Some(&slot) = self.local_slots.get(name) {
            // Already stack-allocated: return its address.
            let addr = self.alloc_reg();
            func.emit(Opcode::LocalAddr { dst: addr, slot });
            addr
        } else {
            // Should not happen for captured variables.
            panic!("get_var_address: variable {:?} has no storage", name);
        }
    }

    /// Get the type of an expression.
    fn expr_type(&self, expr: ExprID) -> TypeID {
        self.decl.types[expr]
    }

    /// Translate an expression and return the register containing the result.
    fn translate_expr(&mut self, expr: ExprID, func: &mut VMFunction) -> Reg {
        match &self.decl.arena.exprs[expr] {
            Expr::Int(n) => {
                let dst = self.alloc_reg();
                func.emit(Opcode::LoadImm { dst, value: *n });
                dst
            }

            Expr::UInt(n) => {
                let dst = self.alloc_reg();
                func.emit(Opcode::LoadImm {
                    dst,
                    value: *n as i64,
                });
                dst
            }

            Expr::Real(s) => {
                let dst = self.alloc_reg();
                let ty = self.expr_type(expr);
                match &*ty {
                    Type::Float32 => {
                        let value: f32 = s.parse().unwrap_or(0.0);
                        func.emit(Opcode::LoadF32 { dst, value });
                    }
                    Type::Float64 => {
                        let value: f64 = s.parse().unwrap_or(0.0);
                        func.emit(Opcode::LoadF64 { dst, value });
                    }
                    _ => {
                        // Default to f32.
                        let value: f32 = s.parse().unwrap_or(0.0);
                        func.emit(Opcode::LoadF32 { dst, value });
                    }
                }
                dst
            }

            Expr::True => {
                let dst = self.alloc_reg();
                func.emit(Opcode::LoadImm { dst, value: 1 });
                dst
            }

            Expr::False => {
                let dst = self.alloc_reg();
                func.emit(Opcode::LoadImm { dst, value: 0 });
                dst
            }

            Expr::Id(name) => {
                let ty = self.expr_type(expr);

                // Check if it's a captured closure variable (double indirection).
                if self.captured_vars.contains(name) {
                    // Load pointer-to-captured-storage from our local slot.
                    let slot = *self.local_slots.get(name).unwrap();
                    let slot_addr = self.alloc_reg();
                    func.emit(Opcode::LocalAddr {
                        dst: slot_addr,
                        slot,
                    });
                    let captured_addr = self.alloc_reg();
                    func.emit(Opcode::Load64 {
                        dst: captured_addr,
                        addr: slot_addr,
                    });
                    // Now load the value from the captured variable's storage.
                    let dst = self.alloc_reg();
                    self.emit_load(&ty, dst, captured_addr, func);
                    return dst;
                }

                // Check if it's a local variable.
                if let Some(&reg) = self.variables.get(name) {
                    if self.reg_promoted.contains(name) {
                        // Register-promoted scalar: value is already in the register.
                        reg
                    } else if self.is_ptr_type(&ty) {
                        // Pointer type: re-emit LocalAddr to ensure the register
                        // is correct after calls that may have clobbered it.
                        if let Some(&slot) = self.local_slots.get(name) {
                            func.emit(Opcode::LocalAddr { dst: reg, slot });
                        }
                        reg
                    } else if let Some(&slot) = self.local_slots.get(name) {
                        // Non-promoted scalar in local slot: load from memory.
                        let dst = self.alloc_reg();
                        func.emit(Opcode::LocalAddr { dst, slot });
                        let load_dst = self.alloc_reg();
                        self.emit_load(&ty, load_dst, dst, func);
                        load_dst
                    } else {
                        reg
                    }
                } else if let Some(&offset) = self.globals.get(name) {
                    // Global variable - load from globals memory.
                    let addr = self.alloc_reg();
                    func.emit(Opcode::GlobalAddr { dst: addr, offset });
                    let dst = self.alloc_reg();
                    self.emit_load(&ty, dst, addr, func);
                    dst
                } else {
                    // Check if it's a function.
                    if let Type::Func(_, _) = &*ty {
                        // Build a 16-byte fat pointer {func_idx, 0} on the stack.
                        let fat_slot = self.alloc_local(16);
                        let fat_addr = self.alloc_reg();
                        func.emit(Opcode::LocalAddr {
                            dst: fat_addr,
                            slot: fat_slot,
                        });
                        // Store func_idx (patched later).
                        let func_idx_reg = self.alloc_reg();
                        self.pending_functions.push(*name);
                        let instr_idx = func.emit(Opcode::LoadImm {
                            dst: func_idx_reg,
                            value: 0,
                        });
                        self.func_load_patches.push(CallToPatch {
                            instr_idx,
                            callee: *name,
                        });
                        func.emit(Opcode::Store64 {
                            addr: fat_addr,
                            src: func_idx_reg,
                        });
                        // Store closure_ptr = 0.
                        let zero_reg = self.alloc_reg();
                        func.emit(Opcode::LoadImm {
                            dst: zero_reg,
                            value: 0,
                        });
                        func.emit(Opcode::Store64Off {
                            base: fat_addr,
                            offset: 8,
                            src: zero_reg,
                        });
                        fat_addr
                    } else {
                        // Unknown identifier - this shouldn't happen after type checking.
                        let dst = self.alloc_reg();
                        func.emit(Opcode::LoadImm { dst, value: 0 });
                        dst
                    }
                }
            }

            Expr::Binop(op, lhs_id, rhs_id) => self.translate_binop(*op, *lhs_id, *rhs_id, func),

            Expr::Unop(op, arg_id) => self.translate_unop(*op, *arg_id, func),

            Expr::Call(fn_id, arg_ids) => self.translate_call(*fn_id, arg_ids, expr, func),

            Expr::Let(name, init, _) => {
                let init_reg = self.translate_expr(*init, func);
                let ty = self.expr_type(expr);

                if !self.is_ptr_type(&ty) {
                    // Scalar: keep value in a register (SaveRegs preserves it across calls).
                    let reg = self.alloc_reg();
                    func.emit(Opcode::Move {
                        dst: reg,
                        src: init_reg,
                    });
                    self.variables.insert(*name, reg);
                    self.reg_promoted.insert(*name);
                } else {
                    // Pointer type: store to local slot.
                    let size = self.vm_type_size(&ty);
                    let slot = self.alloc_local(size);
                    self.local_slots.insert(*name, slot);

                    let addr_reg = self.alloc_reg();
                    func.emit(Opcode::LocalAddr {
                        dst: addr_reg,
                        slot,
                    });
                    self.emit_store(&ty, addr_reg, init_reg, func);
                    self.variables.insert(*name, addr_reg);
                }
                init_reg
            }

            Expr::Var(name, init, _) => {
                let ty = self.expr_type(expr);

                if !self.is_ptr_type(&ty) {
                    // Scalar: keep value in a register.
                    let reg = self.alloc_reg();
                    if let Some(init_id) = init {
                        let init_reg = self.translate_expr(*init_id, func);
                        func.emit(Opcode::Move {
                            dst: reg,
                            src: init_reg,
                        });
                    } else {
                        func.emit(Opcode::LoadImm { dst: reg, value: 0 });
                    }
                    self.variables.insert(*name, reg);
                    self.reg_promoted.insert(*name);
                } else {
                    // Pointer type: store to local slot.
                    let size = self.vm_type_size(&ty);
                    let slot = self.alloc_local(size);
                    self.local_slots.insert(*name, slot);

                    let addr_reg = self.alloc_reg();
                    func.emit(Opcode::LocalAddr {
                        dst: addr_reg,
                        slot,
                    });
                    self.variables.insert(*name, addr_reg);

                    if let Some(init_id) = init {
                        let init_reg = self.translate_expr(*init_id, func);
                        func.emit(Opcode::LocalAddr {
                            dst: addr_reg,
                            slot,
                        });
                        self.emit_store(&ty, addr_reg, init_reg, func);
                    } else {
                        func.emit(Opcode::MemZero {
                            dst: addr_reg,
                            size,
                        });
                    }
                }

                // Return 0 for void-like expression.
                let result = self.alloc_reg();
                func.emit(Opcode::LoadImm {
                    dst: result,
                    value: 0,
                });
                result
            }

            Expr::Block(exprs) => {
                if exprs.is_empty() {
                    let dst = self.alloc_reg();
                    func.emit(Opcode::LoadImm { dst, value: 0 });
                    dst
                } else {
                    let mut result = 0;
                    for expr_id in exprs {
                        result = self.translate_expr(*expr_id, func);
                    }
                    result
                }
            }

            Expr::If(cond_id, then_id, else_id) => {
                self.translate_if(*cond_id, *then_id, *else_id, func)
            }

            Expr::While(cond_id, body_id) => self.translate_while(*cond_id, *body_id, func),

            Expr::For {
                var,
                start,
                end,
                body,
            } => self.translate_for(*var, *start, *end, *body, func),

            Expr::Return(expr_id) => {
                let result = self.translate_expr(*expr_id, func);
                let ret_ty = self.expr_type(*expr_id);

                if returns_via_pointer(ret_ty) {
                    // Reload output pointer from local slot (r0 may have been
                    // clobbered by subcalls).
                    let output = self.reload_output_ptr(func);
                    let size = ret_ty.size(self.decls) as u32;
                    func.emit(Opcode::MemCopy {
                        dst: output,
                        src: result,
                        size,
                    });
                    // Restore caller's registers before returning.
                    func.emit(Opcode::RestoreRegs {
                        start_reg: 0,
                        count: 0, // Will be patched
                        slot: self.save_regs_offset,
                    });
                    func.emit(Opcode::Return);
                } else {
                    // Move result to r0 before restoring (it will be preserved).
                    if result != 0 {
                        func.emit(Opcode::Move {
                            dst: 0,
                            src: result,
                        });
                    }
                    // Restore caller's registers, but skip r0 which has return value.
                    func.emit(Opcode::RestoreRegs {
                        start_reg: 1,                    // Skip r0
                        count: 0,                        // Will be patched to next_reg - 1
                        slot: self.save_regs_offset + 8, // Skip first 8 bytes (r0's slot)
                    });
                    func.emit(Opcode::Return);
                }
                self.has_returned = true;
                result
            }

            Expr::Field(lhs_id, name) => self.translate_field(*lhs_id, *name, func),

            Expr::ArrayIndex(arr_id, idx_id) => self.translate_array_index(*arr_id, *idx_id, func),

            Expr::ArrayLiteral(elements) => self.translate_array_literal(elements, expr, func),

            Expr::String(s) => {
                let bytes = s.as_bytes();
                let total_size = bytes.len() as u32 + 1;
                let slot = self.alloc_local(total_size);
                let addr_reg = self.alloc_reg();
                func.emit(Opcode::LocalAddr {
                    dst: addr_reg,
                    slot,
                });
                let int8_ty = mk_type(Type::Int8);
                for (i, &b) in bytes.iter().enumerate() {
                    let val_reg = self.alloc_reg();
                    func.emit(Opcode::LoadImm {
                        dst: val_reg,
                        value: b as i64,
                    });
                    self.emit_store_offset(&int8_ty, addr_reg, i as i32, val_reg, func);
                }
                let null_reg = self.alloc_reg();
                func.emit(Opcode::LoadImm {
                    dst: null_reg,
                    value: 0,
                });
                self.emit_store_offset(&int8_ty, addr_reg, bytes.len() as i32, null_reg, func);
                addr_reg
            }

            Expr::Tuple(elements) => self.translate_tuple(elements, expr, func),

            Expr::AsTy(expr_id, target_ty) => self.translate_cast(*expr_id, *target_ty, func),

            Expr::Lambda { params, body } => {
                let lambda_ty = self.expr_type(expr);
                if let Type::Func(dom, rng) = &*lambda_ty {
                    if let Type::Tuple(param_types) = &**dom {
                        let id = *self.lambda_counter;
                        *self.lambda_counter += 1;
                        let lambda_name = Name::new(format!("__lambda_{}", id));

                        let lambda_params: Vec<Param> = params
                            .iter()
                            .zip(param_types.iter())
                            .map(|(p, ty)| Param {
                                name: p.name,
                                ty: Some(*ty),
                            })
                            .collect();

                        // Compute free variables captured from the enclosing scope.
                        let param_names: std::collections::HashSet<String> =
                            params.iter().map(|p| p.name.to_string()).collect();
                        let free_vars = collect_free_var_names(
                            *body,
                            &self.decl.arena,
                            &param_names,
                            &self.variables,
                            &self.decl.types,
                        );

                        // Build closure struct if there are captures.
                        let closure_ptr_val = if !free_vars.is_empty() {
                            let n = free_vars.len();
                            let closure_slot = self.alloc_local((n * 8) as u32);
                            let closure_addr = self.alloc_reg();
                            func.emit(Opcode::LocalAddr {
                                dst: closure_addr,
                                slot: closure_slot,
                            });
                            for (i, (name, _ty)) in free_vars.iter().enumerate() {
                                let var_name = Name::new(name.clone());
                                let var_addr = self.get_var_address(&var_name, func);
                                func.emit(Opcode::Store64Off {
                                    base: closure_addr,
                                    offset: (i * 8) as i32,
                                    src: var_addr,
                                });
                            }
                            closure_addr
                        } else {
                            let zero = self.alloc_reg();
                            func.emit(Opcode::LoadImm {
                                dst: zero,
                                value: 0,
                            });
                            zero
                        };

                        let closure_vars: Vec<ClosureVar> = free_vars
                            .iter()
                            .map(|(name, ty)| ClosureVar {
                                name: Name::new(name.clone()),
                                ty: *ty,
                            })
                            .collect();

                        let lambda_decl = FuncDecl {
                            name: lambda_name,
                            typevars: vec![],
                            size_vars: vec![],
                            params: lambda_params,
                            body: Some(*body),
                            ret: *rng,
                            constraints: vec![],
                            loc: self.decl.loc,
                            arena: self.decl.arena.clone(),
                            types: self.decl.types.clone(),
                            closure_vars,
                        };

                        self.pending_lambdas.push(lambda_decl);

                        // Build a 16-byte fat pointer {func_idx, closure_ptr}.
                        let fat_slot = self.alloc_local(16);
                        let fat_addr = self.alloc_reg();
                        func.emit(Opcode::LocalAddr {
                            dst: fat_addr,
                            slot: fat_slot,
                        });
                        // Store func_idx (patched later).
                        let func_idx_reg = self.alloc_reg();
                        let instr_idx = func.emit(Opcode::LoadImm {
                            dst: func_idx_reg,
                            value: 0,
                        });
                        self.lambda_patches.push((instr_idx, lambda_name));
                        func.emit(Opcode::Store64 {
                            addr: fat_addr,
                            src: func_idx_reg,
                        });
                        // Store closure_ptr.
                        func.emit(Opcode::Store64Off {
                            base: fat_addr,
                            offset: 8,
                            src: closure_ptr_val,
                        });
                        fat_addr
                    } else {
                        panic!(
                            "VM codegen lambda: expected tuple domain type, got {:?}",
                            dom
                        );
                    }
                } else {
                    panic!(
                        "VM codegen lambda: expected function type, got {:?}",
                        lambda_ty
                    );
                }
            }

            Expr::Char(c) => {
                let dst = self.alloc_reg();
                func.emit(Opcode::LoadImm {
                    dst,
                    value: *c as i64,
                });
                dst
            }

            Expr::Enum(case_name) => {
                let index = if let crate::Type::Name(enum_name, _) = &*self.decl.types[expr] {
                    let enum_decls = self.decls.find(*enum_name);
                    if let Some(crate::Decl::Enum { cases, .. }) = enum_decls
                        .iter()
                        .find(|d| matches!(d, crate::Decl::Enum { .. }))
                    {
                        cases.iter().position(|c| c == case_name).unwrap_or(0) as i64
                    } else {
                        0
                    }
                } else {
                    0
                };
                // Enums are pointer types (Type::Name). Allocate a local
                // slot for the i32 discriminant and return its address.
                let slot = self.alloc_local(4);
                let addr = self.alloc_reg();
                func.emit(Opcode::LocalAddr { dst: addr, slot });
                let val = self.alloc_reg();
                func.emit(Opcode::LoadImm {
                    dst: val,
                    value: index,
                });
                func.emit(Opcode::Store32 { addr, src: val });
                addr
            }

            Expr::Arena(inner) => self.translate_expr(*inner, func),

            _ => {
                // Unimplemented expression - return 0.
                let dst = self.alloc_reg();
                func.emit(Opcode::LoadImm { dst, value: 0 });
                dst
            }
        }
    }

    /// Translate a binary operation.
    fn translate_binop(
        &mut self,
        op: Binop,
        lhs_id: ExprID,
        rhs_id: ExprID,
        func: &mut VMFunction,
    ) -> Reg {
        // Handle assignment specially.
        if op == Binop::Assign {
            return self.translate_assign(lhs_id, rhs_id, func);
        }

        let lhs = self.translate_expr(lhs_id, func);
        let rhs = self.translate_expr(rhs_id, func);
        let dst = self.alloc_reg();

        let ty = self.expr_type(lhs_id);

        match op {
            Binop::Plus => match &*ty {
                Type::Int32 | Type::UInt32 | Type::Int8 | Type::UInt8 => {
                    func.emit(Opcode::IAdd {
                        dst,
                        a: lhs,
                        b: rhs,
                    });
                }
                Type::Float32 => {
                    func.emit(Opcode::FAdd {
                        dst,
                        a: lhs,
                        b: rhs,
                    });
                }
                Type::Float64 => {
                    func.emit(Opcode::DAdd {
                        dst,
                        a: lhs,
                        b: rhs,
                    });
                }
                _ => {
                    func.emit(Opcode::IAdd {
                        dst,
                        a: lhs,
                        b: rhs,
                    });
                }
            },

            Binop::Minus => match &*ty {
                Type::Int32 | Type::UInt32 | Type::Int8 | Type::UInt8 => {
                    func.emit(Opcode::ISub {
                        dst,
                        a: lhs,
                        b: rhs,
                    });
                }
                Type::Float32 => {
                    func.emit(Opcode::FSub {
                        dst,
                        a: lhs,
                        b: rhs,
                    });
                }
                Type::Float64 => {
                    func.emit(Opcode::DSub {
                        dst,
                        a: lhs,
                        b: rhs,
                    });
                }
                _ => {
                    func.emit(Opcode::ISub {
                        dst,
                        a: lhs,
                        b: rhs,
                    });
                }
            },

            Binop::Mult => match &*ty {
                Type::Int32 | Type::UInt32 | Type::Int8 | Type::UInt8 => {
                    func.emit(Opcode::IMul {
                        dst,
                        a: lhs,
                        b: rhs,
                    });
                }
                Type::Float32 => {
                    func.emit(Opcode::FMul {
                        dst,
                        a: lhs,
                        b: rhs,
                    });
                }
                Type::Float64 => {
                    func.emit(Opcode::DMul {
                        dst,
                        a: lhs,
                        b: rhs,
                    });
                }
                _ => {
                    func.emit(Opcode::IMul {
                        dst,
                        a: lhs,
                        b: rhs,
                    });
                }
            },

            Binop::Div => match &*ty {
                Type::Int32 | Type::Int8 => {
                    func.emit(Opcode::IDiv {
                        dst,
                        a: lhs,
                        b: rhs,
                    });
                }
                Type::UInt32 | Type::UInt8 => {
                    func.emit(Opcode::UDiv {
                        dst,
                        a: lhs,
                        b: rhs,
                    });
                }
                Type::Float32 => {
                    func.emit(Opcode::FDiv {
                        dst,
                        a: lhs,
                        b: rhs,
                    });
                }
                Type::Float64 => {
                    func.emit(Opcode::DDiv {
                        dst,
                        a: lhs,
                        b: rhs,
                    });
                }
                _ => {
                    func.emit(Opcode::IDiv {
                        dst,
                        a: lhs,
                        b: rhs,
                    });
                }
            },

            Binop::Mod => {
                func.emit(Opcode::IRem {
                    dst,
                    a: lhs,
                    b: rhs,
                });
            }

            Binop::Equal => match &*ty {
                Type::Float32 => {
                    func.emit(Opcode::FEq {
                        dst,
                        a: lhs,
                        b: rhs,
                    });
                }
                Type::Float64 => {
                    func.emit(Opcode::DEq {
                        dst,
                        a: lhs,
                        b: rhs,
                    });
                }
                Type::Name(_, _) | Type::Tuple(_) | Type::Array(_, _) => {
                    let size = ty.size(self.decls) as u32;
                    func.emit(Opcode::MemEq {
                        dst,
                        a: lhs,
                        b: rhs,
                        size,
                    });
                }
                _ => {
                    func.emit(Opcode::IEq {
                        dst,
                        a: lhs,
                        b: rhs,
                    });
                }
            },

            Binop::NotEqual => match &*ty {
                Type::Float32 => {
                    func.emit(Opcode::FNe {
                        dst,
                        a: lhs,
                        b: rhs,
                    });
                }
                Type::Name(_, _) | Type::Tuple(_) | Type::Array(_, _) => {
                    let size = ty.size(self.decls) as u32;
                    func.emit(Opcode::MemNe {
                        dst,
                        a: lhs,
                        b: rhs,
                        size,
                    });
                }
                _ => {
                    func.emit(Opcode::INe {
                        dst,
                        a: lhs,
                        b: rhs,
                    });
                }
            },

            Binop::Less => match &*ty {
                Type::Float32 => {
                    func.emit(Opcode::FLt {
                        dst,
                        a: lhs,
                        b: rhs,
                    });
                }
                Type::Float64 => {
                    func.emit(Opcode::DLt {
                        dst,
                        a: lhs,
                        b: rhs,
                    });
                }
                Type::UInt32 | Type::UInt8 => {
                    func.emit(Opcode::ULt {
                        dst,
                        a: lhs,
                        b: rhs,
                    });
                }
                _ => {
                    func.emit(Opcode::ILt {
                        dst,
                        a: lhs,
                        b: rhs,
                    });
                }
            },

            Binop::Greater => match &*ty {
                Type::Float32 => {
                    // a > b ≡ b < a
                    func.emit(Opcode::FLt {
                        dst,
                        a: rhs,
                        b: lhs,
                    });
                }
                Type::Float64 => {
                    // a > b ≡ b < a
                    func.emit(Opcode::DLt {
                        dst,
                        a: rhs,
                        b: lhs,
                    });
                }
                Type::UInt32 | Type::UInt8 => {
                    // a > b ≡ b < a
                    func.emit(Opcode::ULt {
                        dst,
                        a: rhs,
                        b: lhs,
                    });
                }
                _ => {
                    // a > b ≡ b < a
                    func.emit(Opcode::ILt {
                        dst,
                        a: rhs,
                        b: lhs,
                    });
                }
            },

            Binop::Leq => match &*ty {
                Type::Float32 => {
                    func.emit(Opcode::FLe {
                        dst,
                        a: lhs,
                        b: rhs,
                    });
                }
                Type::Float64 => {
                    func.emit(Opcode::DLe {
                        dst,
                        a: lhs,
                        b: rhs,
                    });
                }
                _ => {
                    func.emit(Opcode::ILe {
                        dst,
                        a: lhs,
                        b: rhs,
                    });
                }
            },

            Binop::Geq => match &*ty {
                Type::Float32 => {
                    // a >= b ≡ b <= a
                    func.emit(Opcode::FLe {
                        dst,
                        a: rhs,
                        b: lhs,
                    });
                }
                Type::Float64 => {
                    // a >= b ≡ b <= a
                    func.emit(Opcode::DLe {
                        dst,
                        a: rhs,
                        b: lhs,
                    });
                }
                _ => {
                    // a >= b ≡ b <= a
                    func.emit(Opcode::ILe {
                        dst,
                        a: rhs,
                        b: lhs,
                    });
                }
            },

            Binop::And => {
                func.emit(Opcode::And {
                    dst,
                    a: lhs,
                    b: rhs,
                });
            }

            Binop::Or => {
                func.emit(Opcode::Or {
                    dst,
                    a: lhs,
                    b: rhs,
                });
            }

            Binop::Pow => match &*ty {
                Type::Float32 => {
                    func.emit(Opcode::FPow {
                        dst,
                        a: lhs,
                        b: rhs,
                    });
                }
                Type::Float64 => {
                    func.emit(Opcode::DPow {
                        dst,
                        a: lhs,
                        b: rhs,
                    });
                }
                _ => {
                    func.emit(Opcode::IPow {
                        dst,
                        a: lhs,
                        b: rhs,
                    });
                }
            },

            Binop::Assign => {
                // Handled above.
                unreachable!("Assign binop should be handled before translate_binop")
            }
        }

        dst
    }

    /// Translate an assignment expression.
    fn translate_assign(&mut self, lhs_id: ExprID, rhs_id: ExprID, func: &mut VMFunction) -> Reg {
        // Check for captured variable assignment (double indirection).
        if let Expr::Id(name) = &self.decl.arena.exprs[lhs_id] {
            if self.captured_vars.contains(name) {
                let rhs = self.translate_expr(rhs_id, func);
                let slot = *self.local_slots.get(name).unwrap();
                let slot_addr = self.alloc_reg();
                func.emit(Opcode::LocalAddr {
                    dst: slot_addr,
                    slot,
                });
                let captured_addr = self.alloc_reg();
                func.emit(Opcode::Load64 {
                    dst: captured_addr,
                    addr: slot_addr,
                });
                let ty = self.expr_type(lhs_id);
                self.emit_store(&ty, captured_addr, rhs, func);
                return rhs;
            }
        }
        // Check for direct register-promoted scalar assignment (e.g., `x = expr`).
        if let Expr::Id(name) = &self.decl.arena.exprs[lhs_id] {
            if self.reg_promoted.contains(name) {
                let rhs = self.translate_expr(rhs_id, func);
                let reg = *self.variables.get(name).unwrap();
                if reg != rhs {
                    func.emit(Opcode::Move { dst: reg, src: rhs });
                }
                return rhs;
            }
        }
        let rhs = self.translate_expr(rhs_id, func);
        let lhs_addr = self.translate_lvalue(lhs_id, func);
        let ty = self.expr_type(lhs_id);
        // Struct field assignment of Func type: only copy 8 bytes (func_idx).
        // The struct field stores only the func_idx, not the full 16-byte fat pointer.
        if matches!(&*ty, Type::Func(_, _)) {
            if matches!(&self.decl.arena.exprs[lhs_id], Expr::Field(_, _)) {
                // rhs is a fat pointer address; load func_idx from it and store to struct field.
                let func_idx_reg = self.alloc_reg();
                func.emit(Opcode::Load64 {
                    dst: func_idx_reg,
                    addr: rhs,
                });
                func.emit(Opcode::Store64 {
                    addr: lhs_addr,
                    src: func_idx_reg,
                });
                return rhs;
            }
        }
        self.emit_store(&ty, lhs_addr, rhs, func);
        rhs
    }

    /// Translate an lvalue expression (returns address).
    fn translate_lvalue(&mut self, expr: ExprID, func: &mut VMFunction) -> Reg {
        match &self.decl.arena.exprs[expr] {
            Expr::Id(name) => {
                if let Some(&reg) = self.variables.get(name) {
                    reg
                } else if let Some(&offset) = self.globals.get(name) {
                    // Global variable - return its address.
                    let dst = self.alloc_reg();
                    func.emit(Opcode::GlobalAddr { dst, offset });
                    dst
                } else {
                    let dst = self.alloc_reg();
                    func.emit(Opcode::LoadImm { dst, value: 0 });
                    dst
                }
            }

            Expr::Field(lhs_id, name) => {
                let lhs_addr = self.translate_lvalue(*lhs_id, func);
                let lhs_ty = self.expr_type(*lhs_id);

                if let Type::Name(struct_name, type_args) = &*lhs_ty {
                    let struct_decl = self.decls.find(*struct_name);
                    if let Decl::Struct(s) = &struct_decl[0] {
                        let inst: crate::Instance = s
                            .typevars
                            .iter()
                            .zip(type_args.iter())
                            .map(|(tv, ty)| (crate::types::mk_type(crate::Type::Var(*tv)), *ty))
                            .collect();
                        let offset = s.field_offset(name, self.decls, &inst);
                        let dst = self.alloc_reg();
                        func.emit(Opcode::IAddImm {
                            dst,
                            src: lhs_addr,
                            imm: offset,
                        });
                        return dst;
                    }
                }
                lhs_addr
            }

            Expr::ArrayIndex(arr_id, idx_id) => {
                let arr_addr = self.translate_lvalue(*arr_id, func);
                let idx = self.translate_expr(*idx_id, func);
                let arr_ty = self.expr_type(*arr_id);

                let (elem_ty, is_slice) = match &*arr_ty {
                    Type::Array(elem_ty, _) => (*elem_ty, false),
                    Type::Slice(elem_ty) => (*elem_ty, true),
                    _ => return arr_addr,
                };

                // For slices, load the data pointer from the fat pointer.
                let base = if is_slice {
                    let data_ptr = self.alloc_reg();
                    func.emit(Opcode::Load64 {
                        dst: data_ptr,
                        addr: arr_addr,
                    });
                    data_ptr
                } else {
                    arr_addr
                };

                let elem_size = elem_ty.size(self.decls);
                let offset_reg = self.alloc_reg();

                // offset = idx * elem_size
                let size_reg = self.alloc_reg();
                func.emit(Opcode::LoadImm {
                    dst: size_reg,
                    value: elem_size as i64,
                });
                func.emit(Opcode::IMul {
                    dst: offset_reg,
                    a: idx,
                    b: size_reg,
                });

                // result = base + offset
                let dst = self.alloc_reg();
                func.emit(Opcode::IAdd {
                    dst,
                    a: base,
                    b: offset_reg,
                });
                dst
            }

            _ => {
                // For other expressions, translate and return result.
                self.translate_expr(expr, func)
            }
        }
    }

    /// Translate a unary operation.
    fn translate_unop(&mut self, op: Unop, arg_id: ExprID, func: &mut VMFunction) -> Reg {
        let arg = self.translate_expr(arg_id, func);
        let dst = self.alloc_reg();
        let ty = self.expr_type(arg_id);

        match op {
            Unop::Neg => match &*ty {
                Type::Float32 => {
                    func.emit(Opcode::FNeg { dst, src: arg });
                }
                Type::Float64 => {
                    func.emit(Opcode::DNeg { dst, src: arg });
                }
                _ => {
                    func.emit(Opcode::INeg { dst, src: arg });
                }
            },
            Unop::Not => {
                func.emit(Opcode::Not { dst, src: arg });
                // Mask to 1 bit for boolean.
                let one = self.alloc_reg();
                func.emit(Opcode::LoadImm { dst: one, value: 1 });
                func.emit(Opcode::And {
                    dst,
                    a: dst,
                    b: one,
                });
            }
        }

        dst
    }

    /// Translate a function call.
    fn translate_call(
        &mut self,
        fn_id: ExprID,
        arg_ids: &[ExprID],
        call_expr: ExprID,
        func: &mut VMFunction,
    ) -> Reg {
        // If fn_id is a local variable (lambda or function pointer), use CallClosure.
        // The variable holds a pointer to a fat pointer {func_idx, closure_ptr}.
        let is_local_var = if let Expr::Id(name) = &self.decl.arena.exprs[fn_id] {
            self.variables.contains_key(name)
        } else {
            false
        };

        if is_local_var {
            let fat_ptr_reg = self.translate_expr(fn_id, func);

            let mut arg_values = Vec::new();
            for arg_id in arg_ids {
                arg_values.push(self.translate_expr(*arg_id, func));
            }

            let args_start = self.next_reg;
            for (i, &arg_reg) in arg_values.iter().enumerate() {
                let target = args_start + i as Reg;
                let _ = self.alloc_reg();
                if arg_reg != target {
                    func.emit(Opcode::Move {
                        dst: target,
                        src: arg_reg,
                    });
                }
            }

            func.emit(Opcode::CallClosure {
                fat_ptr: fat_ptr_reg,
                args_start,
                arg_count: arg_ids.len() as u8,
            });
            let result_reg = self.alloc_reg();
            func.emit(Opcode::Move {
                dst: result_reg,
                src: 0,
            });
            return result_reg;
        }

        // Special handling for built-in functions.
        if let Expr::Id(name) = &self.decl.arena.exprs[fn_id] {
            if **name == "print" {
                // Print the first argument.
                if let Some(&arg_id) = arg_ids.first() {
                    let arg = self.translate_expr(arg_id, func);
                    let ty = self.expr_type(arg_id);
                    match &*ty {
                        Type::Float32 => {
                            func.emit(Opcode::PrintF32 { src: arg });
                        }
                        _ => {
                            func.emit(Opcode::PrintI32 { src: arg });
                        }
                    }
                }
                let dst = self.alloc_reg();
                func.emit(Opcode::LoadImm { dst, value: 0 });
                return dst;
            }

            if **name == "assert" {
                if let Some(&arg_id) = arg_ids.first() {
                    let arg = self.translate_expr(arg_id, func);
                    func.emit(Opcode::Assert { src: arg });
                }
                let dst = self.alloc_reg();
                func.emit(Opcode::LoadImm { dst, value: 0 });
                return dst;
            }

            if **name == "putc" {
                if let Some(&arg_id) = arg_ids.first() {
                    let arg = self.translate_expr(arg_id, func);
                    func.emit(Opcode::Putc { src: arg });
                }
                let dst = self.alloc_reg();
                func.emit(Opcode::LoadImm { dst, value: 0 });
                return dst;
            }

            // Unary math builtins (f32 and f64).
            let unary_math_f32: &[(&str, fn(Reg, Reg) -> Opcode)] = &[
                ("sin$f32", |dst, src| Opcode::SinF32 { dst, src }),
                ("cos$f32", |dst, src| Opcode::CosF32 { dst, src }),
                ("tan$f32", |dst, src| Opcode::TanF32 { dst, src }),
                ("asin$f32", |dst, src| Opcode::AsinF32 { dst, src }),
                ("acos$f32", |dst, src| Opcode::AcosF32 { dst, src }),
                ("atan$f32", |dst, src| Opcode::AtanF32 { dst, src }),
                ("sinh$f32", |dst, src| Opcode::SinhF32 { dst, src }),
                ("cosh$f32", |dst, src| Opcode::CoshF32 { dst, src }),
                ("tanh$f32", |dst, src| Opcode::TanhF32 { dst, src }),
                ("asinh$f32", |dst, src| Opcode::AsinhF32 { dst, src }),
                ("acosh$f32", |dst, src| Opcode::AcoshF32 { dst, src }),
                ("atanh$f32", |dst, src| Opcode::AtanhF32 { dst, src }),
                ("ln$f32", |dst, src| Opcode::LnF32 { dst, src }),
                ("exp$f32", |dst, src| Opcode::ExpF32 { dst, src }),
                ("exp2$f32", |dst, src| Opcode::Exp2F32 { dst, src }),
                ("log10$f32", |dst, src| Opcode::Log10F32 { dst, src }),
                ("log2$f32", |dst, src| Opcode::Log2F32 { dst, src }),
                ("sqrt$f32", |dst, src| Opcode::SqrtF32 { dst, src }),
                ("abs$f32", |dst, src| Opcode::AbsF32 { dst, src }),
                ("floor$f32", |dst, src| Opcode::FloorF32 { dst, src }),
                ("ceil$f32", |dst, src| Opcode::CeilF32 { dst, src }),
                ("isinf$f32", |dst, src| Opcode::IsinfF32 { dst, src }),
                ("isnan$f32", |dst, src| Opcode::IsnanF32 { dst, src }),
            ];
            let unary_math_f64: &[(&str, fn(Reg, Reg) -> Opcode)] = &[
                ("sin$f64", |dst, src| Opcode::SinF64 { dst, src }),
                ("cos$f64", |dst, src| Opcode::CosF64 { dst, src }),
                ("tan$f64", |dst, src| Opcode::TanF64 { dst, src }),
                ("asin$f64", |dst, src| Opcode::AsinF64 { dst, src }),
                ("acos$f64", |dst, src| Opcode::AcosF64 { dst, src }),
                ("atan$f64", |dst, src| Opcode::AtanF64 { dst, src }),
                ("sinh$f64", |dst, src| Opcode::SinhF64 { dst, src }),
                ("cosh$f64", |dst, src| Opcode::CoshF64 { dst, src }),
                ("tanh$f64", |dst, src| Opcode::TanhF64 { dst, src }),
                ("asinh$f64", |dst, src| Opcode::AsinhF64 { dst, src }),
                ("acosh$f64", |dst, src| Opcode::AcoshF64 { dst, src }),
                ("atanh$f64", |dst, src| Opcode::AtanhF64 { dst, src }),
                ("ln$f64", |dst, src| Opcode::LnF64 { dst, src }),
                ("exp$f64", |dst, src| Opcode::ExpF64 { dst, src }),
                ("exp2$f64", |dst, src| Opcode::Exp2F64 { dst, src }),
                ("log10$f64", |dst, src| Opcode::Log10F64 { dst, src }),
                ("log2$f64", |dst, src| Opcode::Log2F64 { dst, src }),
                ("sqrt$f64", |dst, src| Opcode::SqrtF64 { dst, src }),
                ("abs$f64", |dst, src| Opcode::AbsF64 { dst, src }),
                ("floor$f64", |dst, src| Opcode::FloorF64 { dst, src }),
                ("ceil$f64", |dst, src| Opcode::CeilF64 { dst, src }),
                ("isinf$f64", |dst, src| Opcode::IsinfF64 { dst, src }),
                ("isnan$f64", |dst, src| Opcode::IsnanF64 { dst, src }),
            ];
            for (n, mk_op) in unary_math_f32.iter().chain(unary_math_f64.iter()) {
                if **name == *n {
                    let src = self.translate_expr(arg_ids[0], func);
                    let dst = self.alloc_reg();
                    func.emit(mk_op(dst, src));
                    return dst;
                }
            }

            // Binary math builtins (f32 and f64).
            let binary_math: &[(&str, fn(Reg, Reg, Reg) -> Opcode)] = &[
                ("pow$f32$f32", |dst, a, b| Opcode::PowF32 { dst, a, b }),
                ("pow$f64$f64", |dst, a, b| Opcode::PowF64 { dst, a, b }),
                ("atan2$f32$f32", |dst, a, b| Opcode::Atan2F32 { dst, a, b }),
                ("atan2$f64$f64", |dst, a, b| Opcode::Atan2F64 { dst, a, b }),
                ("min$f32$f32", |dst, a, b| Opcode::MinF32 { dst, a, b }),
                ("min$f64$f64", |dst, a, b| Opcode::MinF64 { dst, a, b }),
                ("max$f32$f32", |dst, a, b| Opcode::MaxF32 { dst, a, b }),
                ("max$f64$f64", |dst, a, b| Opcode::MaxF64 { dst, a, b }),
            ];
            for (n, mk_op) in binary_math {
                if **name == *n {
                    let a = self.translate_expr(arg_ids[0], func);
                    let b = self.translate_expr(arg_ids[1], func);
                    let dst = self.alloc_reg();
                    func.emit(mk_op(dst, a, b));
                    return dst;
                }
            }

            // Get the return type of the call expression.
            let ret_ty = self.expr_type(call_expr);
            let returns_ptr = returns_via_pointer(ret_ty);

            // If returning a pointer type, allocate local storage for the result.
            let output_slot = if returns_ptr {
                let size = ret_ty.size(self.decls);
                Some(self.alloc_local(size as u32))
            } else {
                None
            };

            // Get the callee's parameter types to detect slice params.
            let fn_ty = self.expr_type(fn_id);
            let param_types: Vec<TypeID> = if let Type::Func(from, _) = &*fn_ty {
                if let Type::Tuple(pts) = &**from {
                    pts.clone()
                } else {
                    vec![]
                }
            } else {
                vec![]
            };

            // First, translate all arguments to get their values.
            // We need to do this before allocating the consecutive arg registers
            // because translation may allocate temporary registers.
            let mut arg_values = Vec::new();
            for (i, arg_id) in arg_ids.iter().enumerate() {
                let arg_reg = self.translate_expr(*arg_id, func);
                // If the callee expects a slice, wrap sized arrays in a fat pointer.
                if i < param_types.len() {
                    if matches!(&*param_types[i], Type::Slice(_)) {
                        let wrapped = self.wrap_as_slice(arg_reg, self.expr_type(*arg_id), func);
                        arg_values.push(wrapped);
                        continue;
                    }
                }
                arg_values.push(arg_reg);
            }

            // Now allocate consecutive registers for the call arguments.
            let args_start = self.next_reg;

            if let Some(slot) = output_slot {
                // Allocate register for output pointer.
                let addr_reg = self.alloc_reg();
                func.emit(Opcode::LocalAddr {
                    dst: addr_reg,
                    slot,
                });
            }

            // Move argument values to consecutive registers starting at args_start
            // (or args_start + 1 if we have an output pointer).
            let first_arg_reg = if output_slot.is_some() {
                args_start + 1
            } else {
                args_start
            };

            for (i, &arg_reg) in arg_values.iter().enumerate() {
                let target_reg = first_arg_reg + i as Reg;
                // Allocate the target register (to keep next_reg in sync).
                let _ = self.alloc_reg();
                if arg_reg != target_reg {
                    func.emit(Opcode::Move {
                        dst: target_reg,
                        src: arg_reg,
                    });
                }
            }

            // Add function to pending list.
            self.pending_functions.push(*name);

            // Calculate arg count (including output pointer if present).
            let arg_count = if output_slot.is_some() {
                arg_ids.len() as u8 + 1
            } else {
                arg_ids.len() as u8
            };

            // Emit the call with a placeholder function index.
            // Record the instruction index for later patching.
            let instr_idx = func.emit(Opcode::Call {
                func: 0, // Placeholder - needs relocation.
                args_start,
                arg_count,
            });

            // Record this call for patching.
            self.calls_to_patch.push(CallToPatch {
                instr_idx,
                callee: *name,
            });

            // If returning pointer type, return the address of the output storage.
            if let Some(slot) = output_slot {
                let addr_reg = self.alloc_reg();
                func.emit(Opcode::LocalAddr {
                    dst: addr_reg,
                    slot,
                });
                return addr_reg;
            }

            // Result is in register 0. Move it to a fresh register so it doesn't
            // get clobbered by subsequent operations that might use register 0.
            let result_reg = self.alloc_reg();
            func.emit(Opcode::Move {
                dst: result_reg,
                src: 0,
            });
            result_reg
        } else {
            // Indirect call via expression (e.g. lambda literal in call position).
            // The expression evaluates to a fat pointer {func_idx, closure_ptr}.
            let fat_ptr_reg = self.translate_expr(fn_id, func);

            let mut arg_values = Vec::new();
            for arg_id in arg_ids {
                arg_values.push(self.translate_expr(*arg_id, func));
            }

            let args_start = self.next_reg;
            for (i, &arg_reg) in arg_values.iter().enumerate() {
                let target = args_start + i as Reg;
                let _ = self.alloc_reg();
                if arg_reg != target {
                    func.emit(Opcode::Move {
                        dst: target,
                        src: arg_reg,
                    });
                }
            }

            func.emit(Opcode::CallClosure {
                fat_ptr: fat_ptr_reg,
                args_start,
                arg_count: arg_ids.len() as u8,
            });

            let result_reg = self.alloc_reg();
            func.emit(Opcode::Move {
                dst: result_reg,
                src: 0,
            });
            result_reg
        }
    }

    /// Translate an if expression.
    fn translate_if(
        &mut self,
        cond_id: ExprID,
        then_id: ExprID,
        else_id: Option<ExprID>,
        func: &mut VMFunction,
    ) -> Reg {
        let cond = self.translate_expr(cond_id, func);
        let result_reg = self.alloc_reg();

        // Jump to else branch if condition is false.
        let jump_to_else = func.emit(Opcode::JumpIfZero { cond, offset: 0 });

        // Save has_returned before branches — a return inside one branch
        // should not suppress the epilogue for the other branch.
        let saved_has_returned = self.has_returned;

        // Then branch.
        let then_result = self.translate_expr(then_id, func);
        func.emit(Opcode::Move {
            dst: result_reg,
            src: then_result,
        });
        let then_returned = self.has_returned;

        if let Some(else_expr_id) = else_id {
            // Jump over else branch.
            let jump_to_end = func.emit(Opcode::Jump { offset: 0 });

            // Patch jump to else (which is now here, after the then branch's jump to end).
            func.patch_jump(jump_to_else);

            // Else branch.
            self.has_returned = saved_has_returned;
            let else_result = self.translate_expr(else_expr_id, func);
            func.emit(Opcode::Move {
                dst: result_reg,
                src: else_result,
            });
            let else_returned = self.has_returned;

            // Patch jump to end.
            func.patch_jump(jump_to_end);

            // Only mark as returned if BOTH branches returned.
            self.has_returned = then_returned && else_returned;
        } else {
            // No else branch — patch jump to here directly.
            func.patch_jump(jump_to_else);
            // Single-branch if can never guarantee a return.
            self.has_returned = saved_has_returned;
        }

        result_reg
    }

    /// Translate a while loop.
    fn translate_while(&mut self, cond_id: ExprID, body_id: ExprID, func: &mut VMFunction) -> Reg {
        let loop_start = func.code.len();

        // Evaluate condition.
        let cond = self.translate_expr(cond_id, func);

        // Jump to end if condition is false.
        let jump_to_end = func.emit(Opcode::JumpIfZero { cond, offset: 0 });

        // Execute body.
        self.translate_expr(body_id, func);

        // Jump back to loop start.
        let loop_end = func.code.len();
        func.emit(Opcode::Jump {
            offset: (loop_start as i32) - (loop_end as i32) - 1,
        });

        // Patch jump to end.
        func.patch_jump(jump_to_end);

        // While loops return 0.
        let result = self.alloc_reg();
        func.emit(Opcode::LoadImm {
            dst: result,
            value: 0,
        });
        result
    }

    /// Translate a for loop.
    fn translate_for(
        &mut self,
        var: Name,
        start_id: ExprID,
        end_id: ExprID,
        body_id: ExprID,
        func: &mut VMFunction,
    ) -> Reg {
        // Initialize loop variable.
        let start = self.translate_expr(start_id, func);
        let end = self.translate_expr(end_id, func);

        let loop_var = self.alloc_reg();
        func.emit(Opcode::Move {
            dst: loop_var,
            src: start,
        });
        self.variables.insert(var, loop_var);

        let loop_start = func.code.len();

        // Check if loop_var < end.
        let cond = self.alloc_reg();
        func.emit(Opcode::ILt {
            dst: cond,
            a: loop_var,
            b: end,
        });

        // Jump to end if condition is false.
        let jump_to_end = func.emit(Opcode::JumpIfZero { cond, offset: 0 });

        // Execute body.
        self.translate_expr(body_id, func);

        // Increment loop variable.
        func.emit(Opcode::IAddImm {
            dst: loop_var,
            src: loop_var,
            imm: 1,
        });

        // Jump back to loop start.
        let loop_end = func.code.len();
        func.emit(Opcode::Jump {
            offset: (loop_start as i32) - (loop_end as i32) - 1,
        });

        // Patch jump to end.
        func.patch_jump(jump_to_end);

        // For loops return 0.
        let result = self.alloc_reg();
        func.emit(Opcode::LoadImm {
            dst: result,
            value: 0,
        });
        result
    }

    /// Translate a field access.
    fn translate_field(&mut self, lhs_id: ExprID, name: Name, func: &mut VMFunction) -> Reg {
        let lhs_ty = self.expr_type(lhs_id);

        // Handle array.len / slice.len.
        if *name == "len" {
            match &*lhs_ty {
                Type::Slice(_) => {
                    // Slice: load length from fat pointer at offset 8.
                    let lhs = self.translate_expr(lhs_id, func);
                    let dst = self.alloc_reg();
                    func.emit(Opcode::Load32Off {
                        dst,
                        base: lhs,
                        offset: 8,
                    });
                    return dst;
                }
                Type::Array(_, len) => {
                    let dst = self.alloc_reg();
                    func.emit(Opcode::LoadImm {
                        dst,
                        value: len.known() as i64,
                    });
                    return dst;
                }
                _ => {}
            }
        }

        let lhs = self.translate_expr(lhs_id, func);

        if let Type::Name(struct_name, type_args) = &*lhs_ty {
            let struct_decl = self.decls.find(*struct_name);
            if let Decl::Struct(s) = &struct_decl[0] {
                let inst: crate::Instance = s
                    .typevars
                    .iter()
                    .zip(type_args.iter())
                    .map(|(tv, ty)| (crate::types::mk_type(crate::Type::Var(*tv)), *ty))
                    .collect();
                let offset = s.field_offset(&name, self.decls, &inst);

                // Find field type and substitute type variables.
                let field = s.find_field(&name);
                if let Some(field) = field {
                    let field_ty = field.ty.subst(&inst);
                    // Func fields in structs are 8 bytes (func_idx only).
                    // Construct a 16-byte fat pointer locally for indirect calls.
                    if matches!(&*field_ty, Type::Func(_, _)) {
                        let func_idx_reg = self.alloc_reg();
                        self.emit_load_offset(&mk_type(Type::Int32), func_idx_reg, lhs, offset, func);
                        // Actually load as 64-bit since func_idx is i64:
                        let func_idx_reg2 = self.alloc_reg();
                        func.emit(Opcode::Load64Off {
                            dst: func_idx_reg2,
                            base: lhs,
                            offset,
                        });
                        let fat_slot = self.alloc_local(16);
                        let fat_addr = self.alloc_reg();
                        func.emit(Opcode::LocalAddr {
                            dst: fat_addr,
                            slot: fat_slot,
                        });
                        func.emit(Opcode::Store64 {
                            addr: fat_addr,
                            src: func_idx_reg2,
                        });
                        let zero = self.alloc_reg();
                        func.emit(Opcode::LoadImm {
                            dst: zero,
                            value: 0,
                        });
                        func.emit(Opcode::Store64Off {
                            base: fat_addr,
                            offset: 8,
                            src: zero,
                        });
                        return fat_addr;
                    }
                    // Arrays and other pointer types are stored inline,
                    // so return the address of the field instead of loading.
                    if self.is_ptr_type(&field_ty) {
                        let dst = self.alloc_reg();
                        func.emit(Opcode::IAddImm {
                            dst,
                            src: lhs,
                            imm: offset,
                        });
                        return dst;
                    } else {
                        let dst = self.alloc_reg();
                        self.emit_load_offset(&field_ty, dst, lhs, offset, func);
                        return dst;
                    }
                }
            }
        } else if let Type::Tuple(elem_types) = &*lhs_ty {
            // Tuple field access: x.0, x.1, etc.
            let index: usize = name.parse().expect("tuple field should be numeric");
            let mut offset = 0i32;
            for i in 0..index {
                offset += elem_types[i].size(self.decls) as i32;
            }
            let elem_ty = &elem_types[index];
            if self.is_ptr_type(elem_ty) {
                let dst = self.alloc_reg();
                func.emit(Opcode::IAddImm {
                    dst,
                    src: lhs,
                    imm: offset,
                });
                return dst;
            } else {
                let dst = self.alloc_reg();
                self.emit_load_offset(elem_ty, dst, lhs, offset, func);
                return dst;
            }
        }

        // Fallback - return lhs.
        lhs
    }

    /// Translate an array index.
    fn translate_array_index(
        &mut self,
        arr_id: ExprID,
        idx_id: ExprID,
        func: &mut VMFunction,
    ) -> Reg {
        let arr = self.translate_expr(arr_id, func);
        let idx = self.translate_expr(idx_id, func);
        let arr_ty = self.expr_type(arr_id);

        let (elem_ty, is_slice) = match &*arr_ty {
            Type::Array(elem_ty, _) => (*elem_ty, false),
            Type::Slice(elem_ty) => (*elem_ty, true),
            _ => return arr, // Fallback
        };

        // For slices, load the data pointer from the fat pointer.
        let base = if is_slice {
            let data_ptr = self.alloc_reg();
            func.emit(Opcode::Load64 {
                dst: data_ptr,
                addr: arr,
            });
            data_ptr
        } else {
            arr
        };

        let elem_size = elem_ty.size(self.decls);

        // Calculate offset.
        let size_reg = self.alloc_reg();
        func.emit(Opcode::LoadImm {
            dst: size_reg,
            value: elem_size as i64,
        });

        let offset_reg = self.alloc_reg();
        func.emit(Opcode::IMul {
            dst: offset_reg,
            a: idx,
            b: size_reg,
        });

        let addr_reg = self.alloc_reg();
        func.emit(Opcode::IAdd {
            dst: addr_reg,
            a: base,
            b: offset_reg,
        });

        // Composite types (arrays, structs, tuples) are represented as
        // pointers — return the address directly instead of loading.
        if self.is_ptr_type(&elem_ty) {
            addr_reg
        } else {
            let dst = self.alloc_reg();
            self.emit_load(&elem_ty, dst, addr_reg, func);
            dst
        }
    }

    /// Translate an array literal.
    fn translate_array_literal(
        &mut self,
        elements: &[ExprID],
        expr: ExprID,
        func: &mut VMFunction,
    ) -> Reg {
        let ty = self.expr_type(expr);
        let size = ty.size(self.decls);

        // Allocate local storage.
        let slot = self.alloc_local(size as u32);
        let addr_reg = self.alloc_reg();
        func.emit(Opcode::LocalAddr {
            dst: addr_reg,
            slot,
        });

        if let Type::Array(elem_ty, _) = &*ty {
            let elem_size = elem_ty.size(self.decls);

            // Store each element.
            for (i, &elem_id) in elements.iter().enumerate() {
                let elem_val = self.translate_expr(elem_id, func);
                let offset = (i as i32) * elem_size;
                self.emit_store_offset(elem_ty, addr_reg, offset, elem_val, func);
            }
        }

        addr_reg
    }

    /// Translate a tuple literal.
    fn translate_tuple(&mut self, elements: &[ExprID], expr: ExprID, func: &mut VMFunction) -> Reg {
        let ty = self.expr_type(expr);
        let size = ty.size(self.decls);

        // Allocate local storage.
        let slot = self.alloc_local(size as u32);
        let addr_reg = self.alloc_reg();
        func.emit(Opcode::LocalAddr {
            dst: addr_reg,
            slot,
        });

        if let Type::Tuple(elem_types) = &*ty {
            let mut offset = 0;
            for (i, &elem_id) in elements.iter().enumerate() {
                let elem_val = self.translate_expr(elem_id, func);
                let elem_ty = &elem_types[i];
                self.emit_store_offset(elem_ty, addr_reg, offset, elem_val, func);
                offset += elem_ty.size(self.decls);
            }
        }

        addr_reg
    }

    /// Translate a type cast.
    fn translate_cast(&mut self, expr_id: ExprID, target_ty: TypeID, func: &mut VMFunction) -> Reg {
        let src = self.translate_expr(expr_id, func);
        let src_ty = self.expr_type(expr_id);
        let dst = self.alloc_reg();

        match (&*src_ty, &*target_ty) {
            (Type::Int32, Type::Float32) => {
                func.emit(Opcode::I32ToF32 { dst, src });
            }
            (Type::Float32, Type::Int32) => {
                func.emit(Opcode::F32ToI32 { dst, src });
            }
            (Type::Int32, Type::Float64) => {
                func.emit(Opcode::I32ToF64 { dst, src });
            }
            (Type::Float64, Type::Int32) => {
                func.emit(Opcode::F64ToI32 { dst, src });
            }
            (Type::Float32, Type::Float64) => {
                func.emit(Opcode::F32ToF64 { dst, src });
            }
            (Type::Float64, Type::Float32) => {
                func.emit(Opcode::F64ToF32 { dst, src });
            }
            (Type::Int32, Type::Int8) | (Type::UInt32, Type::Int8) => {
                func.emit(Opcode::I32ToI8 { dst, src });
            }
            (Type::Int8, Type::Int32) => {
                func.emit(Opcode::I8ToI32 { dst, src });
            }
            (Type::Int32, Type::UInt32) | (Type::UInt32, Type::Int32) => {
                // Same size, just reinterpret — mask to u32.
                func.emit(Opcode::I64ToU32 { dst, src });
            }
            _ => {
                // No conversion needed or unsupported - just move.
                func.emit(Opcode::Move { dst, src });
            }
        }

        dst
    }

    /// Wraps a sized array value in a slice fat pointer {data_ptr: i64, len: i32} on the stack.
    /// If the value is already a slice, returns it as-is.
    fn wrap_as_slice(&mut self, val: Reg, actual_ty: TypeID, func: &mut VMFunction) -> Reg {
        match &*actual_ty {
            Type::Slice(_) => val,
            Type::Array(_, sz) => {
                // Allocate 12 bytes for fat pointer {data_ptr: i64, len: i32}.
                let slot = self.alloc_local(12);
                let fat_addr = self.alloc_reg();
                func.emit(Opcode::LocalAddr {
                    dst: fat_addr,
                    slot,
                });
                // Store data_ptr at offset 0.
                func.emit(Opcode::Store64 {
                    addr: fat_addr,
                    src: val,
                });
                // Store len at offset 8.
                let len_reg = self.alloc_reg();
                func.emit(Opcode::LoadImm {
                    dst: len_reg,
                    value: sz.known() as i64,
                });
                func.emit(Opcode::Store32Off {
                    base: fat_addr,
                    offset: 8,
                    src: len_reg,
                });
                fat_addr
            }
            _ => panic!(
                "VM codegen wrap_as_slice: expected array type, got {:?}",
                actual_ty
            ),
        }
    }

    /// Check if a type is represented as a pointer.
    fn is_ptr_type(&self, ty: &TypeID) -> bool {
        matches!(
            &**ty,
            Type::Name(_, _) | Type::Tuple(_) | Type::Array(_, _) | Type::Slice(_) | Type::Func(_, _)
        )
    }

    /// Size of a type in the VM. Function types use 16-byte fat pointers
    /// {func_idx, closure_ptr} rather than the language-level 8 bytes.
    fn vm_type_size(&self, ty: &TypeID) -> u32 {
        if matches!(&**ty, Type::Func(_, _)) {
            16
        } else {
            ty.size(self.decls) as u32
        }
    }

    /// Emit a load instruction based on type.
    fn emit_load(&self, ty: &TypeID, dst: Reg, addr: Reg, func: &mut VMFunction) {
        match &**ty {
            Type::Bool | Type::Int8 | Type::UInt8 => {
                func.emit(Opcode::Load8 { dst, addr });
            }
            Type::Int32 | Type::UInt32 | Type::Float32 => {
                func.emit(Opcode::Load32 { dst, addr });
            }
            Type::Float64 => {
                func.emit(Opcode::Load64 { dst, addr });
            }
            _ => {
                func.emit(Opcode::Load64 { dst, addr });
            }
        }
    }

    /// Emit a load instruction with offset based on type.
    fn emit_load_offset(
        &mut self,
        ty: &TypeID,
        dst: Reg,
        base: Reg,
        offset: i32,
        func: &mut VMFunction,
    ) {
        match &**ty {
            Type::Bool | Type::Int8 | Type::UInt8 => {
                let addr = self.alloc_reg();
                func.emit(Opcode::IAddImm {
                    dst: addr,
                    src: base,
                    imm: offset,
                });
                func.emit(Opcode::Load8 { dst, addr });
            }
            Type::Int32 | Type::UInt32 | Type::Float32 => {
                func.emit(Opcode::Load32Off { dst, base, offset });
            }
            Type::Float64 => {
                func.emit(Opcode::Load64Off { dst, base, offset });
            }
            _ => {
                func.emit(Opcode::Load64Off { dst, base, offset });
            }
        }
    }

    /// Emit a store instruction based on type.
    fn emit_store(&self, ty: &TypeID, addr: Reg, src: Reg, func: &mut VMFunction) {
        if self.is_ptr_type(ty) {
            let size = self.vm_type_size(ty);
            func.emit(Opcode::MemCopy {
                dst: addr,
                src,
                size,
            });
        } else {
            match &**ty {
                Type::Bool | Type::Int8 | Type::UInt8 => {
                    func.emit(Opcode::Store8 { addr, src });
                }
                Type::Int32 | Type::UInt32 | Type::Float32 => {
                    func.emit(Opcode::Store32 { addr, src });
                }
                Type::Float64 => {
                    func.emit(Opcode::Store64 { addr, src });
                }
                _ => {
                    func.emit(Opcode::Store64 { addr, src });
                }
            }
        }
    }

    /// Emit a store instruction with offset.
    fn emit_store_offset(
        &self,
        ty: &TypeID,
        base: Reg,
        offset: i32,
        src: Reg,
        func: &mut VMFunction,
    ) {
        match &**ty {
            Type::Bool | Type::Int8 | Type::UInt8 => {
                func.emit(Opcode::Store8Off { base, offset, src });
            }
            Type::Int32 | Type::UInt32 | Type::Float32 => {
                func.emit(Opcode::Store32Off { base, offset, src });
            }
            Type::Float64 => {
                func.emit(Opcode::Store64Off { base, offset, src });
            }
            _ => {
                func.emit(Opcode::Store32Off { base, offset, src });
            }
        }
    }
}

/// Collect free variable names referenced in a lambda body that come from the enclosing scope.
fn collect_free_var_names(
    body: crate::ExprID,
    arena: &crate::ExprArena,
    exclude: &std::collections::HashSet<String>,
    local_vars: &HashMap<Name, Reg>,
    types: &[crate::TypeID],
) -> Vec<(String, crate::TypeID)> {
    let mut result = Vec::new();
    let mut seen = std::collections::HashSet::new();
    collect_free_vars_rec(body, arena, exclude, local_vars, types, &mut result, &mut seen);
    result
}

fn collect_free_vars_rec(
    expr: crate::ExprID,
    arena: &crate::ExprArena,
    exclude: &std::collections::HashSet<String>,
    local_vars: &HashMap<Name, Reg>,
    types: &[crate::TypeID],
    result: &mut Vec<(String, crate::TypeID)>,
    seen: &mut std::collections::HashSet<String>,
) {
    match &arena[expr] {
        Expr::Id(name) => {
            let s = name.to_string();
            if local_vars.contains_key(name) && !exclude.contains(&s) && !seen.contains(&s) {
                result.push((s.clone(), types[expr]));
                seen.insert(s);
            }
        }
        Expr::Call(fn_id, args) => {
            collect_free_vars_rec(*fn_id, arena, exclude, local_vars, types, result, seen);
            for a in args {
                collect_free_vars_rec(*a, arena, exclude, local_vars, types, result, seen);
            }
        }
        Expr::Binop(_, lhs, rhs) => {
            collect_free_vars_rec(*lhs, arena, exclude, local_vars, types, result, seen);
            collect_free_vars_rec(*rhs, arena, exclude, local_vars, types, result, seen);
        }
        Expr::Unop(_, arg) => {
            collect_free_vars_rec(*arg, arena, exclude, local_vars, types, result, seen);
        }
        Expr::Let(_, init, _) => {
            collect_free_vars_rec(*init, arena, exclude, local_vars, types, result, seen);
        }
        Expr::Var(_, init, _) => {
            if let Some(init_id) = init {
                collect_free_vars_rec(*init_id, arena, exclude, local_vars, types, result, seen);
            }
        }
        Expr::If(cond, then, else_) => {
            collect_free_vars_rec(*cond, arena, exclude, local_vars, types, result, seen);
            collect_free_vars_rec(*then, arena, exclude, local_vars, types, result, seen);
            if let Some(e) = else_ {
                collect_free_vars_rec(*e, arena, exclude, local_vars, types, result, seen);
            }
        }
        Expr::While(cond, body) => {
            collect_free_vars_rec(*cond, arena, exclude, local_vars, types, result, seen);
            collect_free_vars_rec(*body, arena, exclude, local_vars, types, result, seen);
        }
        Expr::For {
            start, end, body, ..
        } => {
            collect_free_vars_rec(*start, arena, exclude, local_vars, types, result, seen);
            collect_free_vars_rec(*end, arena, exclude, local_vars, types, result, seen);
            collect_free_vars_rec(*body, arena, exclude, local_vars, types, result, seen);
        }
        Expr::Block(exprs) => {
            for e in exprs {
                collect_free_vars_rec(*e, arena, exclude, local_vars, types, result, seen);
            }
        }
        Expr::Return(e) => {
            collect_free_vars_rec(*e, arena, exclude, local_vars, types, result, seen);
        }
        Expr::Field(e, _) => {
            collect_free_vars_rec(*e, arena, exclude, local_vars, types, result, seen);
        }
        Expr::ArrayIndex(arr, idx) => {
            collect_free_vars_rec(*arr, arena, exclude, local_vars, types, result, seen);
            collect_free_vars_rec(*idx, arena, exclude, local_vars, types, result, seen);
        }
        Expr::ArrayLiteral(elems) => {
            for e in elems {
                collect_free_vars_rec(*e, arena, exclude, local_vars, types, result, seen);
            }
        }
        Expr::Tuple(elems) => {
            for e in elems {
                collect_free_vars_rec(*e, arena, exclude, local_vars, types, result, seen);
            }
        }
        Expr::AsTy(e, _) => {
            collect_free_vars_rec(*e, arena, exclude, local_vars, types, result, seen);
        }
        Expr::Arena(e) => {
            collect_free_vars_rec(*e, arena, exclude, local_vars, types, result, seen);
        }
        Expr::Array(ty_expr, size_expr) => {
            collect_free_vars_rec(*ty_expr, arena, exclude, local_vars, types, result, seen);
            collect_free_vars_rec(*size_expr, arena, exclude, local_vars, types, result, seen);
        }
        Expr::Lambda { params, body } => {
            let mut inner_exclude = exclude.clone();
            for p in params {
                inner_exclude.insert(p.name.to_string());
            }
            collect_free_vars_rec(*body, arena, &inner_exclude, local_vars, types, result, seen);
        }
        Expr::Macro(_, args) => {
            for a in args {
                collect_free_vars_rec(*a, arena, exclude, local_vars, types, result, seen);
            }
        }
        Expr::Int(_)
        | Expr::UInt(_)
        | Expr::Real(_)
        | Expr::String(_)
        | Expr::Char(_)
        | Expr::True
        | Expr::False
        | Expr::Enum(_)
        | Expr::Error => {}
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vm::VM;

    /// Helper to create a simple function for testing.
    fn make_simple_decl_table(body: Expr, ret_ty: TypeID) -> DeclTable {
        let mut arena = ExprArena::new();
        let body_id = arena.add(body, crate::test_loc());

        let func = FuncDecl {
            name: Name::str("main"),
            typevars: vec![],
            size_vars: vec![],
            params: vec![],
            body: Some(body_id),
            ret: ret_ty,
            constraints: vec![],
            loc: crate::test_loc(),
            arena,
            types: vec![ret_ty], // Simplified - just use return type.
            closure_vars: vec![],
        };

        DeclTable::new(vec![Decl::Func(func)])
    }

    #[test]
    fn test_compile_simple_int() {
        let mut arena = ExprArena::new();
        let expr = arena.add(Expr::Int(42), crate::test_loc());

        let func = FuncDecl {
            name: Name::str("main"),
            typevars: vec![],
            size_vars: vec![],
            params: vec![],
            body: Some(expr),
            ret: mk_type(Type::Int32),
            constraints: vec![],
            loc: crate::test_loc(),
            arena,
            types: vec![mk_type(Type::Int32)],
            closure_vars: vec![],
        };

        let decls = DeclTable::new(vec![Decl::Func(func)]);

        let mut codegen = VMCodegen::new();
        let program = codegen.compile(&decls).unwrap();

        let mut vm = VM::new();
        let result = vm.run(&program);
        assert_eq!(result, 42);
    }

    #[test]
    fn test_compile_addition() {
        let mut arena = ExprArena::new();
        let lhs = arena.add(Expr::Int(10), crate::test_loc());
        let rhs = arena.add(Expr::Int(32), crate::test_loc());
        let add = arena.add(Expr::Binop(Binop::Plus, lhs, rhs), crate::test_loc());

        let int32 = mk_type(Type::Int32);
        let func = FuncDecl {
            name: Name::str("main"),
            typevars: vec![],
            size_vars: vec![],
            params: vec![],
            body: Some(add),
            ret: int32,
            constraints: vec![],
            loc: crate::test_loc(),
            arena,
            types: vec![int32, int32, int32],
            closure_vars: vec![],
        };

        let decls = DeclTable::new(vec![Decl::Func(func)]);

        let mut codegen = VMCodegen::new();
        let program = codegen.compile(&decls).unwrap();

        let mut vm = VM::new();
        let result = vm.run(&program);
        assert_eq!(result, 42);
    }

    #[test]
    fn test_compile_float_arithmetic() {
        let mut arena = ExprArena::new();
        let lhs = arena.add(Expr::Real("1.5".to_string()), crate::test_loc());
        let rhs = arena.add(Expr::Real("2.5".to_string()), crate::test_loc());
        let mul = arena.add(Expr::Binop(Binop::Mult, lhs, rhs), crate::test_loc());

        let f32_ty = mk_type(Type::Float32);
        let func = FuncDecl {
            name: Name::str("main"),
            typevars: vec![],
            size_vars: vec![],
            params: vec![],
            body: Some(mul),
            ret: f32_ty,
            constraints: vec![],
            loc: crate::test_loc(),
            arena,
            types: vec![f32_ty, f32_ty, f32_ty],
            closure_vars: vec![],
        };

        let decls = DeclTable::new(vec![Decl::Func(func)]);

        let mut codegen = VMCodegen::new();
        let program = codegen.compile(&decls).unwrap();

        let mut vm = VM::new();
        let result = vm.run_f32(&program);
        assert!((result - 3.75).abs() < 0.0001);
    }

    #[test]
    fn test_compile_if_else() {
        let mut arena = ExprArena::new();
        let cond = arena.add(Expr::True, crate::test_loc());
        let then_val = arena.add(Expr::Int(100), crate::test_loc());
        let else_val = arena.add(Expr::Int(200), crate::test_loc());
        let if_expr = arena.add(Expr::If(cond, then_val, Some(else_val)), crate::test_loc());

        let int32 = mk_type(Type::Int32);
        let bool_ty = mk_type(Type::Bool);
        let func = FuncDecl {
            name: Name::str("main"),
            typevars: vec![],
            size_vars: vec![],
            params: vec![],
            body: Some(if_expr),
            ret: int32,
            constraints: vec![],
            loc: crate::test_loc(),
            arena,
            types: vec![bool_ty, int32, int32, int32],
            closure_vars: vec![],
        };

        let decls = DeclTable::new(vec![Decl::Func(func)]);

        let mut codegen = VMCodegen::new();
        let program = codegen.compile(&decls).unwrap();

        let mut vm = VM::new();
        let result = vm.run(&program);
        assert_eq!(result, 100);
    }

    #[test]
    fn test_compile_while_loop() {
        // while (false) { 1 }
        let mut arena = ExprArena::new();
        let cond = arena.add(Expr::False, crate::test_loc());
        let body = arena.add(Expr::Int(1), crate::test_loc());
        let while_expr = arena.add(Expr::While(cond, body), crate::test_loc());
        let result = arena.add(Expr::Int(42), crate::test_loc());
        let block = arena.add(Expr::Block(vec![while_expr, result]), crate::test_loc());

        let int32 = mk_type(Type::Int32);
        let bool_ty = mk_type(Type::Bool);
        let func = FuncDecl {
            name: Name::str("main"),
            typevars: vec![],
            size_vars: vec![],
            params: vec![],
            body: Some(block),
            ret: int32,
            constraints: vec![],
            loc: crate::test_loc(),
            arena,
            types: vec![bool_ty, int32, int32, int32, int32],
            closure_vars: vec![],
        };

        let decls = DeclTable::new(vec![Decl::Func(func)]);

        let mut codegen = VMCodegen::new();
        let program = codegen.compile(&decls).unwrap();

        let mut vm = VM::new();
        let result = vm.run(&program);
        assert_eq!(result, 42);
    }
}
