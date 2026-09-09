//! VM code generator.
//!
//! This module translates a SpecializedProgram into a VMProgram that can be
//! executed by the register-based virtual machine.

use crate::checked::{
    CheckedBody, CheckedExpr as Expr, CheckedFunction, InstanceId, LocalId, Reference,
    SpecializedProgram,
};
use crate::decl::Decl;
use crate::defs::*;
use crate::types::*;
use crate::vm::*;
use std::collections::{HashMap, HashSet};

/// Loop context for break/continue support.
struct LoopContext {
    /// Code position to jump to for continue. For while loops, this is set
    /// before the body. For for loops, this is set after the body (the increment).
    continue_target: usize,
    /// Continue jumps that need patching (for For loops where target isn't known yet).
    continue_patches: Vec<usize>,
    /// Break jumps that need patching to point past the loop.
    break_patches: Vec<usize>,
}

/// A call that needs to be patched with the correct function index.
#[derive(Clone, Debug)]
struct PendingCall {
    /// Index of the function containing this call.
    func_idx: FuncIdx,
    /// Index of the Call instruction within that function.
    instr_idx: usize,
    /// Concrete function instance being called.
    callee: InstanceId,
}

/// Code generator for the VM.
pub struct VMCodegen {
    /// The program being built.
    program: VMProgram,

    /// Map from function instances to their indices in the program.
    func_indices: HashMap<InstanceId, FuncIdx>,

    /// Functions that have been compiled.
    compiled_functions: HashSet<InstanceId>,

    /// Functions that need to be compiled.
    pending_functions: Vec<InstanceId>,

    /// Calls that need to be patched after all functions are compiled.
    pending_calls: Vec<PendingCall>,

    /// LoadImm instructions for function references that need patching.
    pending_func_loads: Vec<PendingCall>,

    /// Global variable offsets.
    globals: HashMap<InstanceId, i32>,

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
    fn declare_globals(&mut self, decls: &SpecializedProgram) {
        let mut offset: i32 = 0;
        for (instance, decl) in decls.storage_instances() {
            let size = match decl {
                Decl::Global { ty, .. } => ty.size(decls) as i32,
                Decl::Func(f) if f.is_extern => 16,
                _ => continue,
            };
            self.globals.insert(instance, offset);
            offset += size;
        }
        self.program.globals_size = offset as usize;
    }

    /// Compile a SpecializedProgram into a VMProgram.
    ///
    /// This looks for a "main" function and compiles it along with all
    /// functions it calls.
    pub fn compile(&mut self, decls: &SpecializedProgram) -> Result<VMProgram, String> {
        let main_name = Name::str("main");
        self.compile_multi(decls, &[main_name])
    }

    /// Compile multiple entry points into a VMProgram.
    ///
    /// Entry points that aren't defined are skipped: only the ones that were
    /// found show up in `program.entry_points`.
    pub fn compile_multi(
        &mut self,
        decls: &SpecializedProgram,
        entry_points: &[Name],
    ) -> Result<VMProgram, String> {
        // First, collect all global variables.
        self.declare_globals(decls);

        // Compile each entry point root.
        for &ep_name in entry_points {
            let Some(instance) = decls.instance_for_entry(ep_name) else {
                continue;
            };
            if self.compiled_functions.contains(&instance) {
                continue;
            }
            let ep_decl = decls
                .function_instance(instance)
                .expect("entry is a function");
            self.compile_function(ep_decl, decls, Some(instance))?;

            // Compile any pending functions (called by this entry point or transitively).
            while let Some(name) = self.pending_functions.pop() {
                if self.compiled_functions.contains(&name) {
                    continue;
                }
                let func_decl = decls
                    .function_instance(name)
                    .expect("call target is a function");
                self.compile_function(func_decl, decls, Some(name))?;
            }
        }

        // Populate entry_points map, and set program.entry to the first entry
        // point that was actually found (for backward compat). If none were
        // found, program.entry stays at its default and the map is empty.
        let mut entry_set = false;
        for &ep_name in entry_points {
            if let Some(&idx) = decls
                .instance_for_entry(ep_name)
                .and_then(|id| self.func_indices.get(&id))
            {
                self.program.entry_points.insert(ep_name, idx);
                if !entry_set {
                    self.program.entry = idx;
                    entry_set = true;
                }
            }
        }

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
    fn compile_function(
        &mut self,
        decl: &CheckedFunction,
        decls: &SpecializedProgram,
        instance: Option<InstanceId>,
    ) -> Result<FuncIdx, String> {
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
        for (&name, &reg) in &translator.body.variables {
            if translator.body.reg_promoted.contains(&name) {
                func.reg_names
                    .push((reg, format!("{}", decl.arena.local(name).name)));
            }
        }
        for (&name, &slot) in &translator.body.local_slots {
            func.slot_names
                .push((slot, format!("{}", decl.arena.local(name).name)));
        }

        // Peephole optimize: eliminate redundant instructions + register allocation.
        if let Some((new_reg_count, mapping)) =
            crate::vm_optimize::optimize(&mut func.code, func.param_count as u8)
        {
            // Register allocation compacted the register numbering.
            // Update locals_size: slot area stays the same, register save area shrinks.
            func.locals_size = func.local_slots as u32 * 8 + new_reg_count as u32 * 8;
            // Debug names follow only registers the allocator retained.
            // An eliminated virtual register must never label a reused physical one.
            for (register, _) in &mut func.reg_names {
                *register = mapping.get(*register as usize).copied().unwrap_or(Reg::MAX);
            }
            func.reg_names.retain(|&(register, _)| register != Reg::MAX);
            func.reg_names.sort();
            func.reg_names.dedup();
        }

        let idx = self.program.add_function(func);
        if let Some(instance) = instance {
            self.func_indices.insert(instance, idx);
            self.compiled_functions.insert(instance);
        }

        // Extract data from translator before it's dropped (it borrows self.pending_functions).
        let calls_to_patch = std::mem::take(&mut translator.calls_to_patch);
        let func_load_patches = std::mem::take(&mut translator.func_load_patches);
        let pending_lambdas = std::mem::take(&mut translator.pending_lambdas);
        let lambda_patches = std::mem::take(&mut translator.lambda_patches);
        let extern_funcs = std::mem::take(&mut translator.extern_funcs);
        drop(translator);

        // Collect extern function descriptors.
        self.program.extern_funcs.extend(extern_funcs);

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
            let lambda_idx = self.compile_function(&lambda_decl, decls, None)?;
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
}

/// Check if an expression is simple enough to inline: no blocks, control flow,
/// let/var bindings, or nested calls. Only arithmetic, comparisons, literals,
/// identifiers, type casts, and field access are allowed.
fn is_inline_expr(id: ExprID, arena: &CheckedBody) -> bool {
    match &arena[id] {
        Expr::Int(_, _) | Expr::Real(_, _) | Expr::Id(_) | Expr::True | Expr::False => true,
        Expr::Binop(op, lhs, rhs) => {
            !matches!(op, Binop::Assign)
                && is_inline_expr(*lhs, arena)
                && is_inline_expr(*rhs, arena)
        }
        Expr::Unop(_, operand) => is_inline_expr(*operand, arena),
        Expr::AsTy(inner, _) => is_inline_expr(*inner, arena),
        Expr::Field(base, _) => is_inline_expr(*base, arena),
        // A block with a single expression (e.g. `{ lhs - rhs }`) is inlineable.
        Expr::Block(exprs) if exprs.len() == 1 => is_inline_expr(exprs[0], arena),
        // If-else with simple branches (e.g. `if x < y { -1 } else { 1 }`)
        Expr::If(cond, then_expr, Some(else_expr)) => {
            is_inline_expr(*cond, arena)
                && is_inline_expr(*then_expr, arena)
                && is_inline_expr(*else_expr, arena)
        }
        _ => false,
    }
}

/// A call instruction that needs patching.
struct CallToPatch {
    /// Index of the Call instruction.
    instr_idx: usize,
    /// Concrete function instance being called.
    callee: InstanceId,
}

/// Storage and derived facts whose IDs belong to one checked body. Inlining
/// switches this context as a unit; lexical scopes need no storage snapshots.
struct BodyContext<'a> {
    decl: &'a CheckedFunction,
    variables: HashMap<LocalId, Reg>,
    reg_promoted: HashSet<LocalId>,
    lambda_referenced: HashSet<LocalId>,
    reference_vars: HashSet<LocalId>,
    local_slots: HashMap<LocalId, u16>,
    elidable_lets: HashSet<ExprID>,
    captured_vars: HashSet<LocalId>,
}

impl<'a> BodyContext<'a> {
    fn new(decl: &'a CheckedFunction) -> Self {
        Self {
            decl,
            variables: HashMap::new(),
            reg_promoted: HashSet::new(),
            lambda_referenced: decl.captured_locals(),
            reference_vars: HashSet::new(),
            local_slots: HashMap::new(),
            elidable_lets: crate::copy_elision::elidable_let_copies(decl),
            captured_vars: HashSet::new(),
        }
    }
}

/// Translator for a single function body.
struct FunctionTranslator<'a> {
    body: BodyContext<'a>,

    /// Declaration table for looking up types and functions.
    decls: &'a SpecializedProgram,

    /// Next available register.
    next_reg: Reg,

    /// Next available local slot.
    next_slot: u16,

    /// Total size of local variables in bytes.
    locals_size: u32,

    /// Functions that are called and need to be compiled.
    pending_functions: &'a mut Vec<InstanceId>,

    /// Counter for generating unique lambda names.
    lambda_counter: &'a mut usize,

    /// Calls that need patching.
    calls_to_patch: Vec<CallToPatch>,

    /// Lambda CheckedFunctions extracted from this function body, to be compiled afterward.
    pending_lambdas: Vec<CheckedFunction>,

    /// LoadImm instructions that need to be patched with lambda function indices.
    lambda_patches: Vec<(usize, Name)>,

    /// LoadImm instructions that need to be patched with function indices (function references).
    func_load_patches: Vec<CallToPatch>,

    /// Global variable offsets.
    globals: &'a HashMap<InstanceId, i32>,

    /// Output pointer register for functions returning pointer types.
    output_ptr: Option<Reg>,

    /// Local slot where the output pointer is saved (survives across calls).
    output_ptr_slot: Option<u16>,

    /// Tracks if a return has been emitted (so we don't emit epilogue).
    has_returned: bool,

    /// Stack of loop context for nested loops.
    /// continue_target: code position to jump to for continue (0 = use continue_patches).
    /// continue_patches: positions of Jump instructions needing patching to continue target.
    /// break_patches: positions of Jump instructions needing patching to loop exit.
    loop_stack: Vec<LoopContext>,

    /// Byte offset in locals where registers are saved.
    save_regs_offset: u32,

    /// Extern function info collected during translation.
    extern_funcs: Vec<crate::vm::ExternFuncInfo>,
}

/// Convert a Lyte type to ExternTypes for FFI marshalling.
/// Slices expand to two arguments: (pointer, i32 length).
fn type_to_extern_types(ty: TypeID) -> Vec<crate::vm::ExternType> {
    match &*ty {
        Type::Void => vec![crate::vm::ExternType::Void],
        Type::Bool => vec![crate::vm::ExternType::Bool],
        Type::Int32 => vec![crate::vm::ExternType::I32],
        Type::Float32 => vec![crate::vm::ExternType::F32],
        Type::Float64 => vec![crate::vm::ExternType::F64],
        Type::Slice(_) => vec![crate::vm::ExternType::Ptr, crate::vm::ExternType::I32],
        Type::Reference(_) => vec![crate::vm::ExternType::Ptr],
        _ => panic!(
            "unsupported extern function parameter type: {}",
            ty.pretty_print()
        ),
    }
}

/// Check if a type should be returned via output pointer.
fn returns_via_pointer(ty: TypeID) -> bool {
    matches!(
        &*ty,
        Type::Array(_, _) | Type::Slice(_) | Type::Name(_, _) | Type::Tuple(_) | Type::Float32x4
    )
}

impl<'a> FunctionTranslator<'a> {
    fn new(
        decl: &'a CheckedFunction,
        decls: &'a SpecializedProgram,
        pending_functions: &'a mut Vec<InstanceId>,
        lambda_counter: &'a mut usize,
        globals: &'a HashMap<InstanceId, i32>,
    ) -> Self {
        Self {
            body: BodyContext::new(decl),
            decls,

            next_reg: 0,
            next_slot: 0,
            locals_size: 0,
            pending_functions,
            lambda_counter,
            calls_to_patch: Vec::new(),
            pending_lambdas: Vec::new(),
            lambda_patches: Vec::new(),
            func_load_patches: Vec::new(),
            globals,
            output_ptr: None,
            output_ptr_slot: None,
            has_returned: false,
            save_regs_offset: 0,
            loop_stack: Vec::new(),
            extern_funcs: Vec::new(),
        }
    }

    /// Translate the function body.
    fn translate(&mut self, func: &mut VMFunction) {
        // If return type is a pointer type, first parameter is output pointer.
        if returns_via_pointer(self.body.decl.ret) {
            self.output_ptr = Some(self.alloc_reg());
            func.param_count += 1;
        }

        // Reserve registers for parameters (these are the incoming argument positions).
        let param_count = self.body.decl.params.len();
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
        let param_offset = if returns_via_pointer(self.body.decl.ret) {
            1u8
        } else {
            0u8
        };
        for (i, param) in self.body.decl.params.iter().enumerate() {
            let src_reg = i as Reg + param_offset as Reg;
            let ty = self.body.decl.arena.local(param.local).ty;

            if let Type::Reference(_) = &*ty {
                let reg = self.alloc_reg();
                func.emit(Opcode::Move {
                    dst: reg,
                    src: src_reg,
                });
                self.body.variables.insert(param.local, reg);

                self.body.reference_vars.insert(param.local);
            } else if !self.is_ptr_type(&ty) && self.body.lambda_referenced.contains(&param.local) {
                // A conditional capture must not decide whether parameter storage exists.
                let addr = self.alloc_scalar_slot(param.local, ty, func);
                self.emit_store(&ty, addr, src_reg, func);
            } else if !self.is_ptr_type(&ty) {
                // Scalar parameter: copy to a dedicated register so the param
                // register can be reused. The copy will be eliminated by
                // move forwarding if possible.
                let reg = self.alloc_reg();
                func.emit(Opcode::Move {
                    dst: reg,
                    src: src_reg,
                });
                self.body.variables.insert(param.local, reg);

                self.body.reg_promoted.insert(param.local);
            } else {
                // Pointer-represented parameters are passed as addresses.
                // Copy them out of the call argument registers so scalar
                // returns in r0 cannot clobber long-lived pointer params.
                let reg = self.alloc_reg();
                func.emit(Opcode::Move {
                    dst: reg,
                    src: src_reg,
                });
                self.body.variables.insert(param.local, reg);
            }
        }

        // Set up captured closure variables.
        // The closure pointer was set by CallClosure before entering this function.
        if !self.body.decl.closure_vars.is_empty() {
            let closure_ptr_reg = self.alloc_reg();
            func.emit(Opcode::GetClosurePtr {
                dst: closure_ptr_reg,
            });
            for (i, cv) in self.body.decl.closure_vars.iter().enumerate() {
                // Load the address of the captured variable from closure_struct[i].
                let addr_reg = self.alloc_reg();
                func.emit(Opcode::Load64Off {
                    dst: addr_reg,
                    base: closure_ptr_reg,
                    offset: (i * 8) as i32,
                });
                // Store this address in a local slot so it survives across calls.
                let slot = self.alloc_local(8);
                self.body.local_slots.insert(*cv, slot);
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
                self.body.variables.insert(*cv, addr_reg);

                // Mark as a captured variable (accessed via double indirection).
                self.body.captured_vars.insert(*cv);
            }
        }

        // Translate the body if present.
        if let Some(body) = self.body.decl.body {
            let result_reg = self.translate_expr(body, func);

            // Skip epilogue if we already emitted a return (e.g., explicit return statement).
            if !self.has_returned {
                // Return the result.
                if returns_via_pointer(self.body.decl.ret) {
                    // Reload output pointer from local slot (r0 may have been
                    // clobbered by subcalls).
                    let output = self.reload_output_ptr(func);
                    let size = self.body.decl.ret.size(self.decls) as u32;
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
                        *count = if reg_count > 1 {
                            (reg_count - 1) as u8
                        } else {
                            0
                        };
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

    /// Give a scalar variable a local slot instead of a register, and return a
    /// register holding the slot's address.
    fn alloc_scalar_slot(&mut self, name: LocalId, ty: TypeID, func: &mut VMFunction) -> Reg {
        let slot = self.alloc_local(ty.size(self.decls) as u32);
        self.body.local_slots.insert(name, slot);
        let addr = self.alloc_reg();
        func.emit(Opcode::LocalAddr { dst: addr, slot });
        self.body.variables.insert(name, addr);

        self.body.reg_promoted.remove(&name);
        addr
    }

    /// Get the address of a variable's storage (for closure capture).
    /// For register-promoted vars, spills to a local slot first.
    fn get_var_address(&mut self, name: &LocalId, func: &mut VMFunction) -> Reg {
        if self.body.captured_vars.contains(name) {
            // This variable was itself captured from an enclosing scope.
            // Our local slot holds a *pointer* to the actual storage, so we
            // must follow the indirection to return the real address.
            let slot = *self.body.local_slots.get(name).unwrap();
            let slot_addr = self.alloc_reg();
            func.emit(Opcode::LocalAddr {
                dst: slot_addr,
                slot,
            });
            let actual_addr = self.alloc_reg();
            func.emit(Opcode::Load64 {
                dst: actual_addr,
                addr: slot_addr,
            });
            actual_addr
        } else if self.body.reg_promoted.contains(name) {
            // Register-promoted scalar: spill to a local slot so we have a stable address.
            let val_reg = *self.body.variables.get(name).unwrap();
            let slot = self.alloc_local(8);
            self.body.local_slots.insert(*name, slot);
            let addr = self.alloc_reg();
            func.emit(Opcode::LocalAddr { dst: addr, slot });
            func.emit(Opcode::Store64 { addr, src: val_reg });
            // Change variable from register-promoted to stack-allocated.
            self.body.reg_promoted.remove(name);
            self.body.variables.insert(*name, addr);
            addr
        } else if let Some(&slot) = self.body.local_slots.get(name) {
            // Already stack-allocated: return its address.
            let addr = self.alloc_reg();
            func.emit(Opcode::LocalAddr { dst: addr, slot });
            addr
        } else if self.is_ptr_type(&self.body.decl.arena.local(*name).ty) {
            // Borrowed and aggregate parameters carry the captured storage address.
            self.body.variables[name]
        } else {
            // Should not happen for captured variables.
            panic!("get_var_address: variable {:?} has no storage", name);
        }
    }

    /// Get the type of an expression.
    fn expr_type(&self, expr: ExprID) -> TypeID {
        self.body.decl.arena.ty(expr)
    }

    /// Get the type that determines how an expression is represented at runtime.
    ///
    /// Type solving may coerce an array expression to a slice at a call site, but
    /// codegen still receives an array address and must build the slice fat
    /// pointer itself.
    fn representation_type(&self, expr: ExprID) -> TypeID {
        match &self.body.decl.arena[expr] {
            Expr::Id(Reference::Local(local)) => {
                let ty = self.body.decl.arena.local(*local).ty;
                match &*ty {
                    Type::Reference(inner) => *inner,
                    _ => ty,
                }
            }
            Expr::Id(Reference::Instance(instance)) => self.decls.instance(*instance).ty(),
            Expr::ArrayIndex(arr_id, _) => match &*self.representation_type(*arr_id) {
                Type::Array(elem, _) | Type::Slice(elem) | Type::Reference(elem) => *elem,
                _ => self.expr_type(expr),
            },
            _ => self.expr_type(expr),
        }
    }

    /// Translate an expression and return the register containing the result.
    fn translate_expr(&mut self, expr: ExprID, func: &mut VMFunction) -> Reg {
        match &self.body.decl.arena[expr] {
            Expr::Int(n, _) => {
                let dst = self.alloc_reg();
                func.emit(Opcode::LoadImm { dst, value: *n });
                dst
            }

            Expr::Real(s, _) => {
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

            Expr::Id(Reference::Local(name)) => {
                let ty = self.expr_type(expr);

                // Check if it's a captured closure variable (double indirection).
                if self.body.captured_vars.contains(name) {
                    // Load pointer-to-captured-storage from our local slot.
                    let slot = *self.body.local_slots.get(name).unwrap();
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
                    // Aggregates and slices are represented by their address,
                    // and the captured pointer already is that address —
                    // dereferencing it would yield the first word of the value.
                    if self.is_ptr_type(&ty) {
                        return captured_addr;
                    }
                    // Now load the value from the captured variable's storage.
                    let dst = self.alloc_reg();
                    self.emit_load(&ty, dst, captured_addr, func);
                    return dst;
                }

                // Check if it's a local variable.
                if let Some(&reg) = self.body.variables.get(name) {
                    if self.body.reference_vars.contains(name) {
                        if self.is_ptr_type(&ty) {
                            return reg;
                        }
                        let dst = self.alloc_reg();
                        self.emit_load(&ty, dst, reg, func);
                        dst
                    } else if self.body.reg_promoted.contains(name) {
                        // Register-promoted scalar: value is already in the register.
                        reg
                    } else if self.is_ptr_type(&ty) {
                        // Pointer type: re-emit LocalAddr to ensure the register
                        // is correct after calls that may have clobbered it.
                        if let Some(&slot) = self.body.local_slots.get(name) {
                            func.emit(Opcode::LocalAddr { dst: reg, slot });
                        }
                        reg
                    } else if let Some(&slot) = self.body.local_slots.get(name) {
                        // Non-promoted scalar in local slot: load from memory.
                        let dst = self.alloc_reg();
                        func.emit(Opcode::LocalAddr { dst, slot });
                        let load_dst = self.alloc_reg();
                        self.emit_load(&ty, load_dst, dst, func);
                        load_dst
                    } else {
                        reg
                    }
                } else {
                    unreachable!("checked local must have storage")
                }
            }
            Expr::Id(Reference::Instance(name)) => {
                let ty = self.expr_type(expr);
                if let Some(&offset) = self.globals.get(name) {
                    // Global variable - load from globals memory.
                    let addr = self.alloc_reg();
                    func.emit(Opcode::GlobalAddr { dst: addr, offset });
                    // Composite types (arrays, structs) are pointer-represented:
                    // return the address, don't load the value.
                    if self.is_ptr_type(&ty) {
                        addr
                    } else {
                        let dst = self.alloc_reg();
                        self.emit_load(&ty, dst, addr, func);
                        dst
                    }
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
                        unreachable!("instance must name storage or a function")
                    }
                }
            }

            Expr::Id(_) => unreachable!("non-concrete reference in specialized body"),

            Expr::Binop(op, lhs_id, rhs_id) => self.translate_binop(*op, *lhs_id, *rhs_id, func),

            Expr::Unop(op, arg_id) => self.translate_unop(*op, *arg_id, func),

            Expr::Call(fn_id, arg_ids) => self.translate_call(*fn_id, arg_ids, expr, func),

            Expr::Let(name, init, _) => {
                let ty = self.body.decl.arena.local(*name).ty;
                let init_reg = self.translate_expr(*init, func);
                let init_reg = self.wrap_for_expected_slice(init_reg, ty, *init, func);

                if !self.is_ptr_type(&ty) && self.body.lambda_referenced.contains(name) {
                    // Captured by a lambda: must live in memory, not a register.
                    let addr = self.alloc_scalar_slot(*name, ty, func);
                    self.emit_store(&ty, addr, init_reg, func);
                } else if !self.is_ptr_type(&ty) {
                    // Scalar: keep value in a register (SaveRegs preserves it across calls).
                    let reg = self.alloc_reg();
                    func.emit(Opcode::Move {
                        dst: reg,
                        src: init_reg,
                    });

                    self.body.variables.insert(*name, reg);

                    self.body.reg_promoted.insert(*name);
                } else if crate::copy_elision::is_value_aggregate(&ty)
                    && !self.body.elidable_lets.contains(&expr)
                {
                    // `let` binds aggregates by value, so the initializer's
                    // storage has to be copied — otherwise a slice coerced from
                    // the binding writes back into the source. Same shape as
                    // `var`, which has always copied.
                    let size = self.vm_type_size(&ty);
                    let slot = self.alloc_local(size);

                    self.body.local_slots.insert(*name, slot);

                    let addr_reg = self.alloc_reg();
                    func.emit(Opcode::LocalAddr {
                        dst: addr_reg,
                        slot,
                    });
                    self.body.variables.insert(*name, addr_reg);

                    self.emit_store(&ty, addr_reg, init_reg, func);
                    return addr_reg;
                } else {
                    // Pointer-represented let bindings carry the address value.
                    let reg = self.alloc_reg();
                    func.emit(Opcode::Move {
                        dst: reg,
                        src: init_reg,
                    });

                    self.body.variables.insert(*name, reg);

                    return reg;
                }
                init_reg
            }

            Expr::Var(name, init, _) => {
                let ty = self.body.decl.arena.local(*name).ty;

                if !self.is_ptr_type(&ty) && self.body.lambda_referenced.contains(name) {
                    // Captured by a lambda: must live in memory, not a register.
                    let init_reg = if let Some(init_id) = init {
                        self.translate_expr(*init_id, func)
                    } else {
                        let zero = self.alloc_reg();
                        func.emit(Opcode::LoadImm {
                            dst: zero,
                            value: 0,
                        });
                        zero
                    };
                    let addr = self.alloc_scalar_slot(*name, ty, func);
                    self.emit_store(&ty, addr, init_reg, func);
                } else if !self.is_ptr_type(&ty) {
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

                    self.body.variables.insert(*name, reg);

                    self.body.reg_promoted.insert(*name);
                } else {
                    // Pointer type: store to local slot.
                    let size = self.vm_type_size(&ty);
                    let slot = self.alloc_local(size);

                    self.body.local_slots.insert(*name, slot);

                    let addr_reg = self.alloc_reg();
                    func.emit(Opcode::LocalAddr {
                        dst: addr_reg,
                        slot,
                    });
                    self.body.variables.insert(*name, addr_reg);

                    if let Some(init_id) = init {
                        let init_reg = self.translate_expr(*init_id, func);
                        let init_reg = self.wrap_for_expected_slice(init_reg, ty, *init_id, func);
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

            Expr::StructLit(struct_name, fields) => {
                let ty = self.expr_type(expr);
                let size = self.vm_type_size(&ty);
                let slot = self.alloc_local(size);

                let base_reg = self.alloc_reg();
                func.emit(Opcode::LocalAddr {
                    dst: base_reg,
                    slot,
                });
                func.emit(Opcode::MemZero {
                    dst: base_reg,
                    size,
                });

                if let Type::Name(_, type_args) = &*ty {
                    let struct_decl = self.decls.find(*struct_name);
                    if let Decl::Struct(s) = &struct_decl[0] {
                        let inst: crate::Instance = s
                            .typevars
                            .iter()
                            .map(|tv| crate::types::mk_type(crate::Type::Var(*tv)))
                            .zip(type_args.iter().copied())
                            .collect();
                        for (fname, fval) in fields {
                            let val_reg = self.translate_expr(*fval, func);
                            let offset = s.field_offset(fname, self.decls, &inst);
                            let field_ty = self.expr_type(*fval);
                            // Re-load base address after translate_expr
                            // (it may have allocated registers that alias base_reg).
                            let addr = self.alloc_reg();
                            func.emit(Opcode::LocalAddr { dst: addr, slot });
                            self.emit_store_offset(&field_ty, addr, offset, val_reg, func);
                        }
                    }
                }

                // Re-emit the address as the result.
                let result = self.alloc_reg();
                func.emit(Opcode::LocalAddr { dst: result, slot });
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

            Expr::Assume(_) => {
                // No-op: assume is only used by the safety checker.
                let result = self.alloc_reg();
                func.emit(Opcode::LoadImm {
                    dst: result,
                    value: 0,
                });
                result
            }
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

            Expr::Break => {
                let break_jump = func.emit(Opcode::Jump { offset: 0 });
                self.loop_stack
                    .last_mut()
                    .expect("break outside loop")
                    .break_patches
                    .push(break_jump);
                let result = self.alloc_reg();
                func.emit(Opcode::LoadImm {
                    dst: result,
                    value: 0,
                });
                result
            }

            Expr::Continue => {
                let ctx = self.loop_stack.last().expect("continue outside loop");
                let continue_target = ctx.continue_target;
                if continue_target == 0 {
                    // For-loop: target not known yet, emit patch.
                    let jump_pos = func.emit(Opcode::Jump { offset: 0 });
                    self.loop_stack
                        .last_mut()
                        .unwrap()
                        .continue_patches
                        .push(jump_pos);
                } else {
                    let pos = func.code.len();
                    func.emit(Opcode::Jump {
                        offset: (continue_target as i32) - (pos as i32) - 1,
                    });
                }
                let result = self.alloc_reg();
                func.emit(Opcode::LoadImm {
                    dst: result,
                    value: 0,
                });
                result
            }

            Expr::Field(lhs_id, name) => self.translate_field(*lhs_id, *name, func),

            Expr::ArrayIndex(arr_id, idx_id) => self.translate_array_index(*arr_id, *idx_id, func),

            Expr::ArrayLiteral(elements) => self.translate_array_literal(elements, expr, func),

            Expr::Array(value_expr, _size_expr) => {
                // Fill-array expression: [value; size], e.g. [0; 5]
                let fill_val = self.translate_expr(*value_expr, func);
                let ty = self.expr_type(expr);
                let size = ty.size(self.decls);

                let slot = self.alloc_local(size as u32);
                let addr_reg = self.alloc_reg();
                func.emit(Opcode::LocalAddr {
                    dst: addr_reg,
                    slot,
                });

                if let Type::Array(elem_ty, sz) = &*ty {
                    let count = sz.known();
                    let elem_size = elem_ty.size(self.decls);
                    let fill_val =
                        self.wrap_for_expected_slice(fill_val, *elem_ty, *value_expr, func);
                    for i in 0..count {
                        let offset = i * elem_size;
                        self.emit_store_offset(elem_ty, addr_reg, offset, fill_val, func);
                    }
                }

                addr_reg
            }

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

            Expr::Lambda { .. } => {
                let id = *self.lambda_counter;
                *self.lambda_counter += 1;
                let lambda_name = Name::new(format!("__lambda_{}", id));

                let lambda_decl = self.body.decl.extract_lambda(expr, lambda_name);
                let free_vars = &lambda_decl.closure_vars;

                // Build closure struct if there are captures.
                let closure_ptr_val = if !free_vars.is_empty() {
                    let n = free_vars.len();
                    let closure_slot = self.alloc_local((n * 8) as u32);
                    let closure_addr = self.alloc_reg();
                    func.emit(Opcode::LocalAddr {
                        dst: closure_addr,
                        slot: closure_slot,
                    });
                    for (i, var_name) in free_vars.iter().enumerate() {
                        let var_addr = self.get_var_address(var_name, func);
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
                let index = if let crate::Type::Name(enum_name, _) = &*self.body.decl.arena.ty(expr)
                {
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

            Expr::Macro(..) | Expr::TypeApp(..) | Expr::Error => {
                unreachable!("unresolved expression in specialized body")
            }
        }
    }

    /// Try to emit a fused multiply-add/subtract instruction.
    /// Returns Some(dst) if successful, None if the pattern doesn't match.
    fn try_emit_fma(
        &mut self,
        op: Binop,
        lhs_id: ExprID,
        rhs_id: ExprID,
        is_f64: bool,
        func: &mut VMFunction,
    ) -> Option<Reg> {
        let arena = &self.body.decl.arena;

        if op == Binop::Plus {
            // a + b*c → FMulAdd { dst, a: b, b: c, c: a }
            if let Expr::Binop(Binop::Mult, ma, mb) = arena[rhs_id] {
                let c = self.translate_expr(lhs_id, func);
                let a = self.translate_expr(ma, func);
                let b = self.translate_expr(mb, func);
                let dst = self.alloc_reg();
                func.emit(if is_f64 {
                    Opcode::DMulAdd { dst, a, b, c }
                } else {
                    Opcode::FMulAdd { dst, a, b, c }
                });
                return Some(dst);
            }
            // b*c + a → FMulAdd { dst, a: b, b: c, c: a }
            if let Expr::Binop(Binop::Mult, ma, mb) = arena[lhs_id] {
                let a = self.translate_expr(ma, func);
                let b = self.translate_expr(mb, func);
                let c = self.translate_expr(rhs_id, func);
                let dst = self.alloc_reg();
                func.emit(if is_f64 {
                    Opcode::DMulAdd { dst, a, b, c }
                } else {
                    Opcode::FMulAdd { dst, a, b, c }
                });
                return Some(dst);
            }
        } else if op == Binop::Minus {
            // b*c - a → FMulSub { dst, a: b, b: c, c: a }  (b*c - a)
            if let Expr::Binop(Binop::Mult, ma, mb) = arena[lhs_id] {
                let a = self.translate_expr(ma, func);
                let b = self.translate_expr(mb, func);
                let c = self.translate_expr(rhs_id, func);
                let dst = self.alloc_reg();
                func.emit(if is_f64 {
                    Opcode::DMulSub { dst, a, b, c }
                } else {
                    Opcode::FMulSub { dst, a, b, c }
                });
                return Some(dst);
            }
            // a - b*c → FNMulAdd { dst, a: b, b: c, c: a }  (a - b*c)
            if let Expr::Binop(Binop::Mult, ma, mb) = arena[rhs_id] {
                let c = self.translate_expr(lhs_id, func);
                let a = self.translate_expr(ma, func);
                let b = self.translate_expr(mb, func);
                let dst = self.alloc_reg();
                func.emit(if is_f64 {
                    Opcode::DNMulAdd { dst, a, b, c }
                } else {
                    Opcode::FNMulAdd { dst, a, b, c }
                });
                return Some(dst);
            }
        }
        None
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

        // FMA: emit FMulAdd/FMulSub directly from the expression tree.
        if op == Binop::Plus || op == Binop::Minus {
            let ty = self.expr_type(lhs_id);
            let is_f32 = matches!(&*ty, Type::Float32);
            let is_f64 = matches!(&*ty, Type::Float64);
            if is_f32 || is_f64 {
                if let Some(reg) = self.try_emit_fma(op, lhs_id, rhs_id, is_f64, func) {
                    return reg;
                }
            }
        }

        // f32x4: ptr-represented SIMD ops — allocate result slot and emit SIMD opcode
        {
            let ty = self.expr_type(lhs_id);
            if matches!(&*ty, Type::Float32x4) {
                let lhs = self.translate_expr(lhs_id, func);
                let rhs = self.translate_expr(rhs_id, func);
                let slot = self.alloc_local(16);
                let dst = self.alloc_reg();
                func.emit(Opcode::LocalAddr { dst, slot });
                match op {
                    Binop::Plus => {
                        func.emit(Opcode::F32x4Add {
                            dst,
                            a: lhs,
                            b: rhs,
                        });
                    }
                    Binop::Minus => {
                        func.emit(Opcode::F32x4Sub {
                            dst,
                            a: lhs,
                            b: rhs,
                        });
                    }
                    Binop::Mult => {
                        func.emit(Opcode::F32x4Mul {
                            dst,
                            a: lhs,
                            b: rhs,
                        });
                    }
                    Binop::Div => {
                        func.emit(Opcode::F32x4Div {
                            dst,
                            a: lhs,
                            b: rhs,
                        });
                    }
                    _ => panic!("unsupported f32x4 binop: {:?}", op),
                }
                return dst;
            }
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

            // Float `%` is lowered to a call to the stdlib's `__mod` before
            // codegen, so only integer operands reach here.
            Binop::Mod => match &*ty {
                Type::Float32 | Type::Float64 | Type::Float32x4 => {
                    unreachable!("type {:?} not supported for modulo", ty)
                }
                _ => {
                    func.emit(Opcode::IRem {
                        dst,
                        a: lhs,
                        b: rhs,
                    });
                }
            },

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
                Type::Slice(elem) => {
                    let elem_size = elem.size(self.decls) as u32;
                    func.emit(Opcode::SliceEq {
                        dst,
                        a: lhs,
                        b: rhs,
                        elem_size,
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
                Type::Slice(elem) => {
                    let elem_size = elem.size(self.decls) as u32;
                    func.emit(Opcode::SliceNe {
                        dst,
                        a: lhs,
                        b: rhs,
                        elem_size,
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
        if let Expr::Id(Reference::Local(name)) = &self.body.decl.arena[lhs_id] {
            if self.body.captured_vars.contains(name) {
                let rhs = self.translate_expr(rhs_id, func);
                let ty = self.representation_type(lhs_id);
                let rhs = self.wrap_for_expected_slice(rhs, ty, rhs_id, func);
                let slot = *self.body.local_slots.get(name).unwrap();
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
                self.emit_store(&ty, captured_addr, rhs, func);
                return rhs;
            }
        }
        // Check for direct register-promoted scalar assignment (e.g., `x = expr`).
        if let Expr::Id(Reference::Local(name)) = &self.body.decl.arena[lhs_id] {
            if self.body.reg_promoted.contains(name) {
                let rhs = self.translate_expr(rhs_id, func);
                let reg = *self.body.variables.get(name).unwrap();
                if reg != rhs {
                    func.emit(Opcode::Move { dst: reg, src: rhs });
                }
                return rhs;
            }
        }
        // Slice store superinstruction: a[i] = rhs where a is a slice of 32-bit elements.
        if let Expr::ArrayIndex(arr_id, idx_id) = &self.body.decl.arena[lhs_id] {
            let arr_ty = self.expr_type(*arr_id);
            if let Type::Slice(elem_ty) = &*arr_ty {
                let elem_size = elem_ty.size(self.decls);
                if !self.is_ptr_type(elem_ty) && elem_size == 4 {
                    let rhs = self.translate_expr(rhs_id, func);
                    let slice = self.translate_expr(*arr_id, func);
                    let idx = self.translate_expr(*idx_id, func);
                    func.emit(Opcode::SliceStore32 {
                        slice,
                        index: idx,
                        src: rhs,
                    });
                    return rhs;
                }
            }
        }

        let ty = self.representation_type(lhs_id);
        let rhs = self.translate_expr(rhs_id, func);
        let rhs = self.wrap_for_expected_slice(rhs, ty, rhs_id, func);
        let lhs_addr = self.translate_lvalue(lhs_id, func);
        // Struct field assignment of Func type: only copy 8 bytes (func_idx).
        // The struct field stores only the func_idx, not the full 16-byte fat pointer.
        if matches!(&*ty, Type::Func(_, _)) {
            if matches!(&self.body.decl.arena[lhs_id], Expr::Field(_, _)) {
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
        match &self.body.decl.arena[expr] {
            Expr::Id(Reference::Local(name)) => {
                if let Some(&reg) = self.body.variables.get(name) {
                    if self.body.reg_promoted.contains(name) {
                        let ty = self.body.decl.arena.local(*name).ty;
                        let slot = self.alloc_local(ty.size(self.decls) as u32);
                        let addr = self.alloc_reg();
                        func.emit(Opcode::LocalAddr { dst: addr, slot });
                        self.emit_store(&ty, addr, reg, func);
                        self.body.variables.insert(*name, addr);
                        self.body.local_slots.insert(*name, slot);
                        self.body.reg_promoted.remove(name);
                        addr
                    } else {
                        reg
                    }
                } else {
                    unreachable!("checked local must have storage")
                }
            }
            Expr::Id(Reference::Instance(instance)) => {
                let dst = self.alloc_reg();
                func.emit(Opcode::GlobalAddr {
                    dst,
                    offset: self.globals[instance],
                });
                dst
            }

            Expr::Field(lhs_id, name) => {
                let lhs_addr = self.translate_lvalue(*lhs_id, func);
                let lhs_ty = self.expr_type(*lhs_id);

                if matches!(&*lhs_ty, Type::Float32x4) {
                    let s: &str = name;
                    let offset: i32 = match s {
                        "x" | "r" => 0,
                        "y" | "g" => 4,
                        "z" | "b" => 8,
                        "w" | "a" => 12,
                        _ => panic!("invalid f32x4 field: {}", name),
                    };
                    return self.emit_offset_addr(lhs_addr, offset, func);
                }

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
                        return self.emit_offset_addr(lhs_addr, offset, func);
                    }
                }
                lhs_addr
            }

            Expr::ArrayIndex(arr_id, idx_id) => {
                let arr_addr = self.translate_lvalue(*arr_id, func);
                let idx = self.translate_expr(*idx_id, func);
                let arr_ty = self.representation_type(*arr_id);

                // f32x4 lane store: the lane address is base + idx * 4.
                // The lane index is in 0..4: the safety checker proves it.
                if matches!(&*arr_ty, Type::Float32x4) {
                    let size_reg = self.alloc_reg();
                    func.emit(Opcode::LoadImm {
                        dst: size_reg,
                        value: 4,
                    });
                    let offset = self.alloc_reg();
                    func.emit(Opcode::IMul {
                        dst: offset,
                        a: idx,
                        b: size_reg,
                    });
                    let dst = self.alloc_reg();
                    func.emit(Opcode::IAdd {
                        dst,
                        a: arr_addr,
                        b: offset,
                    });
                    return dst;
                }

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
        let ty = self.expr_type(arg_id);

        // f32x4 negation: ptr-represented
        if op == Unop::Neg && matches!(&*ty, Type::Float32x4) {
            let slot = self.alloc_local(16);
            let dst = self.alloc_reg();
            func.emit(Opcode::LocalAddr { dst, slot });
            func.emit(Opcode::F32x4Neg { dst, src: arg });
            return dst;
        }

        let dst = self.alloc_reg();

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
        if self.holds_fat_pointer(fn_id) {
            return self.translate_closure_call(fn_id, arg_ids, call_expr, func);
        }

        // Special handling for built-in functions.
        if let Expr::Id(Reference::Instance(instance)) = &self.body.decl.arena[fn_id] {
            let instance = *instance;
            let name = &self.decls.instance_name(instance);
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

            // f32x4 constructor: f32x4(x, y, z, w) → allocate 16-byte slot, store 4 f32s
            if **name == "f32x4" && arg_ids.len() == 4 {
                let x = self.translate_expr(arg_ids[0], func);
                let y = self.translate_expr(arg_ids[1], func);
                let z = self.translate_expr(arg_ids[2], func);
                let w = self.translate_expr(arg_ids[3], func);
                let slot = self.alloc_local(16);
                let dst = self.alloc_reg();
                func.emit(Opcode::LocalAddr { dst, slot });
                func.emit(Opcode::Store32Off {
                    base: dst,
                    offset: 0,
                    src: x,
                });
                func.emit(Opcode::Store32Off {
                    base: dst,
                    offset: 4,
                    src: y,
                });
                func.emit(Opcode::Store32Off {
                    base: dst,
                    offset: 8,
                    src: z,
                });
                func.emit(Opcode::Store32Off {
                    base: dst,
                    offset: 12,
                    src: w,
                });
                return dst;
            }

            // f32x4_splat: broadcast one f32 to all 4 lanes
            if **name == "f32x4_splat" && arg_ids.len() == 1 {
                let x = self.translate_expr(arg_ids[0], func);
                let slot = self.alloc_local(16);
                let dst = self.alloc_reg();
                func.emit(Opcode::LocalAddr { dst, slot });
                func.emit(Opcode::Store32Off {
                    base: dst,
                    offset: 0,
                    src: x,
                });
                func.emit(Opcode::Store32Off {
                    base: dst,
                    offset: 4,
                    src: x,
                });
                func.emit(Opcode::Store32Off {
                    base: dst,
                    offset: 8,
                    src: x,
                });
                func.emit(Opcode::Store32Off {
                    base: dst,
                    offset: 12,
                    src: x,
                });
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

            // Check for extern function calls — emit CallExtern instead of Call.
            {
                if let Some(f) = self.decls.function_instance(instance) {
                    if f.is_extern {
                        let globals_offset = *self
                            .globals
                            .get(&instance)
                            .expect("extern function not in globals");

                        // Build the C-level parameter types (slices expand to ptr + i32).
                        let mut c_param_types: Vec<crate::vm::ExternType> = Vec::new();
                        for p in &f.params {
                            c_param_types.extend(type_to_extern_types(f.arena.local(p.local).ty));
                        }
                        let ret_types = type_to_extern_types(f.ret);
                        let ret_type = ret_types[0];
                        self.extern_funcs.push(crate::vm::ExternFuncInfo {
                            globals_offset,
                            param_types: c_param_types.clone(),
                            ret_type,
                        });

                        // Translate arguments, wrapping arrays as slices where needed.
                        let mut arg_values = Vec::new();
                        for (i, arg_id) in arg_ids.iter().enumerate() {
                            let param_ty = f.arena.local(f.params[i].local).ty;
                            let arg_reg = if matches!(&*param_ty, Type::Reference(_)) {
                                self.translate_lvalue(*arg_id, func)
                            } else {
                                self.translate_expr(*arg_id, func)
                            };
                            if matches!(&*param_ty, Type::Slice(_)) {
                                // Coerce array → slice if needed.
                                let wrapped = self.wrap_as_slice(
                                    arg_reg,
                                    self.representation_type(*arg_id),
                                    func,
                                );
                                arg_values.push(wrapped);
                            } else {
                                arg_values.push(arg_reg);
                            }
                        }

                        // Stage arguments into consecutive registers.
                        // Slices expand: load data_ptr and len from the fat pointer.
                        let args_start = self.next_reg;
                        let mut c_arg_count: u8 = 0;
                        for (i, param) in f.params.iter().enumerate() {
                            let arg_reg = arg_values[i];
                            let param_ty = f.arena.local(param.local).ty;
                            if matches!(&*param_ty, Type::Slice(_)) {
                                // arg_reg points to fat pointer {data_ptr: i64, len: i32}.
                                let ptr_reg = self.alloc_reg();
                                func.emit(Opcode::Load64 {
                                    dst: ptr_reg,
                                    addr: arg_reg,
                                });
                                let len_reg = self.alloc_reg();
                                func.emit(Opcode::Load32Off {
                                    dst: len_reg,
                                    base: arg_reg,
                                    offset: 8,
                                });
                                c_arg_count += 2;
                            } else {
                                let target = self.alloc_reg();
                                if arg_reg != target {
                                    func.emit(Opcode::Move {
                                        dst: target,
                                        src: arg_reg,
                                    });
                                }
                                c_arg_count += 1;
                            }
                        }

                        func.emit(Opcode::CallExtern {
                            args_start,
                            arg_count: c_arg_count,
                            globals_offset,
                        });

                        let result_reg = self.alloc_reg();
                        func.emit(Opcode::Move {
                            dst: result_reg,
                            src: 0,
                        });
                        return result_reg;
                    }
                }
            }

            // Try to inline small leaf functions. This avoids Call overhead
            // for trivial functions like `cmp(lhs, rhs) { lhs - rhs }`.
            if let Some(callee) = self.decls.function_instance(instance) {
                if let Some(result) = self.try_inline(callee, arg_ids, func) {
                    return result;
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

            // Get the callee's declared parameter types to detect slice params.
            // We use the declaration (not the solved call-site type) because
            // the solver may retain Array types where the callee expects Slice.
            let param_types = self
                .decls
                .function_instance(instance)
                .expect("direct call target is a function")
                .param_types();

            // First, translate all arguments to get their values.
            // We need to do this before allocating the consecutive arg registers
            // because translation may allocate temporary registers.
            let mut arg_values = Vec::new();
            for (i, arg_id) in arg_ids.iter().enumerate() {
                let param_ty = param_types.get(i).copied();
                let arg_reg = if param_ty.is_some_and(|ty| matches!(&*ty, Type::Reference(_))) {
                    self.translate_lvalue(*arg_id, func)
                } else {
                    self.translate_expr(*arg_id, func)
                };
                // If the callee expects a slice, wrap sized arrays in a fat pointer.
                if param_ty.is_some_and(|ty| matches!(&*ty, Type::Slice(_))) {
                    let wrapped =
                        self.wrap_as_slice(arg_reg, self.representation_type(*arg_id), func);
                    arg_values.push(wrapped);
                    continue;
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
            self.pending_functions.push(instance);

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
                callee: instance,
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
            self.translate_closure_call(fn_id, arg_ids, call_expr, func)
        }
    }

    /// True when the callee expression names storage holding a fat pointer
    /// {func_idx, closure_ptr} — a local variable or a function-typed global —
    /// rather than naming a function declaration. Such calls go through
    /// `translate_closure_call` instead of the direct-call path.
    fn holds_fat_pointer(&self, fn_id: ExprID) -> bool {
        match &self.body.decl.arena[fn_id] {
            Expr::Id(Reference::Local(_)) => true,
            Expr::Id(Reference::Instance(id)) => {
                matches!(self.decls.instance(*id), Decl::Global { .. })
            }
            _ => false,
        }
    }

    /// Parameter types of a callee reached through a fat pointer, taken from
    /// the function expression's solved type.
    fn closure_param_types(&self, fn_id: ExprID) -> Vec<TypeID> {
        if let Type::Func(from, _) = &*self.expr_type(fn_id) {
            if let Type::Tuple(param_types) = &**from {
                return param_types.clone();
            }
        }
        vec![]
    }

    /// Translate a call through a fat pointer {func_idx, closure_ptr}. Mirrors
    /// the direct-call ABI: an sret output pointer is passed as the first
    /// argument, `Reference` params are passed by address, and sized arrays are
    /// wrapped as slices where the callee expects one.
    fn translate_closure_call(
        &mut self,
        fn_id: ExprID,
        arg_ids: &[ExprID],
        call_expr: ExprID,
        func: &mut VMFunction,
    ) -> Reg {
        let fat_ptr_reg = self.translate_expr(fn_id, func);

        let param_types = self.closure_param_types(fn_id);

        let ret_ty = self.expr_type(call_expr);
        let output_slot = if returns_via_pointer(ret_ty) {
            let size = ret_ty.size(self.decls);
            Some(self.alloc_local(size as u32))
        } else {
            None
        };

        let mut arg_values = Vec::new();
        for (i, arg_id) in arg_ids.iter().enumerate() {
            let param_ty = param_types.get(i).copied();
            let arg_reg = if param_ty.is_some_and(|ty| matches!(&*ty, Type::Reference(_))) {
                self.translate_lvalue(*arg_id, func)
            } else {
                self.translate_expr(*arg_id, func)
            };
            if param_ty.is_some_and(|ty| matches!(&*ty, Type::Slice(_))) {
                let wrapped = self.wrap_as_slice(arg_reg, self.representation_type(*arg_id), func);
                arg_values.push(wrapped);
                continue;
            }
            arg_values.push(arg_reg);
        }

        let args_start = self.next_reg;

        if let Some(slot) = output_slot {
            let addr_reg = self.alloc_reg();
            func.emit(Opcode::LocalAddr {
                dst: addr_reg,
                slot,
            });
        }

        let first_arg_reg = if output_slot.is_some() {
            args_start + 1
        } else {
            args_start
        };

        for (i, &arg_reg) in arg_values.iter().enumerate() {
            let target = first_arg_reg + i as Reg;
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
            arg_count: arg_ids.len() as u8 + u8::from(output_slot.is_some()),
        });

        // sret callees return void; the result is the address of the output
        // storage we passed in.
        if let Some(slot) = output_slot {
            let addr_reg = self.alloc_reg();
            func.emit(Opcode::LocalAddr {
                dst: addr_reg,
                slot,
            });
            return addr_reg;
        }

        let result_reg = self.alloc_reg();
        func.emit(Opcode::Move {
            dst: result_reg,
            src: 0,
        });
        result_reg
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

        // continue → loop_start (re-check condition), break patches collected below.
        self.loop_stack.push(LoopContext {
            continue_target: loop_start,
            continue_patches: Vec::new(),
            break_patches: Vec::new(),
        });

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

        // Patch jump to end and all break jumps.
        func.patch_jump(jump_to_end);
        let ctx = self.loop_stack.pop().unwrap();
        for bp in ctx.break_patches {
            func.patch_jump(bp);
        }

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
        var: LocalId,
        start_id: ExprID,
        end_id: ExprID,
        body_id: ExprID,
        func: &mut VMFunction,
    ) -> Reg {
        // Initialize loop variable. Both bounds are outside its scope, so
        // they still see any outer binding of the same name.
        let start = self.translate_expr(start_id, func);
        let end = self.translate_expr(end_id, func);

        let loop_var = self.alloc_reg();
        func.emit(Opcode::Move {
            dst: loop_var,
            src: start,
        });

        // The counter is a scalar bound to `var` for the duration of the loop.
        // Shadowing an outer binding has to forget the outer binding's
        // name-keyed state — otherwise reads of the name go to its slot
        // instead of the counter — and the snapshot brings that state back at
        // loop exit, where the loop variable is out of scope again.

        let int_ty = mk_type(Type::Int32);
        let counter_slot = if self.body.lambda_referenced.contains(&var) {
            // A lambda shares the counter by address, so it needs storage of
            // its own, allocated up front the way `let` and `var` do it.
            // Leaving it register-promoted and letting `get_var_address` spill
            // it lazily would put the store at the capture site, which can sit
            // on a conditionally-executed path — iterations that don't reach
            // it would then read an unwritten slot.
            self.alloc_scalar_slot(var, int_ty, func);
            self.body.local_slots.get(&var).copied()
        } else {
            self.body.variables.insert(var, loop_var);

            self.body.reg_promoted.insert(var);
            None
        };

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

        // Push loop stack: continue target will be set to the increment position.
        // Use 0 as placeholder; continue jumps will be collected as patches.
        self.loop_stack.push(LoopContext {
            continue_target: 0,
            continue_patches: Vec::new(),
            break_patches: Vec::new(),
        });

        // A counter that lives in a slot is refreshed from the register at
        // the top of every iteration. The loop variable is immutable, so
        // nothing ever writes back the other way.
        if let Some(slot) = counter_slot {
            let addr = self.alloc_reg();
            func.emit(Opcode::LocalAddr { dst: addr, slot });
            self.emit_store(&int_ty, addr, loop_var, func);
        }

        // Execute body.
        self.translate_expr(body_id, func);

        // Increment position — this is where continue jumps to.
        let increment_pos = func.code.len();

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

        // Patch jump to end, break jumps, and continue jumps.
        func.patch_jump(jump_to_end);
        let ctx = self.loop_stack.pop().unwrap();
        for bp in ctx.break_patches {
            func.patch_jump(bp);
        }
        // Patch continue jumps to point to the increment position.
        for cp in ctx.continue_patches {
            let jump_offset = (increment_pos as i32) - (cp as i32) - 1;
            if let Opcode::Jump { offset } = &mut func.code[cp] {
                *offset = jump_offset;
            }
        }

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

        // f32x4 swizzle fields: x/r=0, y/g=1, z/b=2, w/a=3
        if matches!(&*lhs_ty, Type::Float32x4) {
            let s: &str = &name;
            let lane: i32 = match s {
                "x" | "r" => 0,
                "y" | "g" => 1,
                "z" | "b" => 2,
                "w" | "a" => 3,
                _ => panic!("invalid f32x4 field: {}", name),
            };
            let lhs = self.translate_expr(lhs_id, func);
            let dst = self.alloc_reg();
            func.emit(Opcode::Load32Off {
                dst,
                base: lhs,
                offset: lane * 4,
            });
            return dst;
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
                        self.emit_load_offset(
                            &mk_type(Type::Int32),
                            func_idx_reg,
                            lhs,
                            offset,
                            func,
                        );
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
                        return self.emit_offset_addr(lhs, offset, func);
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
                return self.emit_offset_addr(lhs, offset, func);
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
        let arr_ty = self.representation_type(arr_id);

        // f32x4 element extraction: base[idx] where base is a ptr to 4 f32s
        if matches!(&*arr_ty, Type::Float32x4) {
            let size_reg = self.alloc_reg();
            func.emit(Opcode::LoadImm {
                dst: size_reg,
                value: 4,
            });
            let offset = self.alloc_reg();
            func.emit(Opcode::IMul {
                dst: offset,
                a: idx,
                b: size_reg,
            });
            let addr = self.alloc_reg();
            func.emit(Opcode::IAdd {
                dst: addr,
                a: arr,
                b: offset,
            });
            let dst = self.alloc_reg();
            func.emit(Opcode::Load32 { dst, addr });
            return dst;
        }

        let (elem_ty, is_slice) = match &*arr_ty {
            Type::Array(elem_ty, _) => (*elem_ty, false),
            Type::Slice(elem_ty) => (*elem_ty, true),
            _ => return arr, // Fallback
        };

        // Slice superinstruction: fuse data-pointer load, offset computation,
        // and element load/store into a single instruction.
        if is_slice {
            let elem_size = elem_ty.size(self.decls);
            if !self.is_ptr_type(&elem_ty) && elem_size == 4 {
                let dst = self.alloc_reg();
                func.emit(Opcode::SliceLoad32 {
                    dst,
                    slice: arr,
                    index: idx,
                });
                return dst;
            }
        }

        // General path: manual address computation.
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

        if self.is_ptr_type(&elem_ty) {
            addr_reg
        } else {
            let dst = self.alloc_reg();
            self.emit_load(&elem_ty, dst, addr_reg, func);
            dst
        }
    }

    /// Try to inline a callee function at the call site. Returns Some(result_reg)
    /// if inlining succeeded, None if the function is too complex to inline.
    ///
    /// Inlineable functions must:
    /// - Have a body that is a single expression (not a block)
    /// - Take only scalar (non-pointer) parameters
    /// - Return a scalar (non-pointer) type
    /// - Not be recursive
    ///
    /// The inlined body is translated using the callee's arena and types,
    /// with callee parameter identities bound in a fresh body context.
    fn try_inline(
        &mut self,
        callee: &'a CheckedFunction,
        arg_ids: &[ExprID],
        func: &mut VMFunction,
    ) -> Option<Reg> {
        let body = callee.body?;

        // Only inline functions with matching parameter count.
        if callee.params.len() != arg_ids.len() {
            return None;
        }

        // Only inline if the return type is scalar (not a pointer/struct/tuple).
        if returns_via_pointer(callee.ret) {
            return None;
        }

        // Only inline scalar parameters (no slices, structs, arrays).
        for param in &callee.params {
            let ty = callee.arena.local(param.local).ty;
            if ty.is_ptr() || matches!(&*ty, Type::Slice(_)) {
                return None;
            }
        }

        // Only inline simple expression bodies (no blocks, no control flow).
        if !is_inline_expr(body, &callee.arena) {
            return None;
        }

        // Evaluate arguments in the caller's context.
        let arg_regs: Vec<Reg> = arg_ids
            .iter()
            .map(|arg| self.translate_expr(*arg, func))
            .collect();

        // Local IDs are interpreted only in the body context that owns them.
        // Register/slot allocation and emitted instructions stay in the caller.
        let caller = std::mem::replace(&mut self.body, BodyContext::new(callee));
        for (param, &reg) in callee.params.iter().zip(arg_regs.iter()) {
            self.body.variables.insert(param.local, reg);
            self.body.reg_promoted.insert(param.local);
        }
        let result = self.translate_expr(body, func);
        self.body = caller;

        Some(result)
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
                let elem_val = self.wrap_for_expected_slice(elem_val, *elem_ty, elem_id, func);
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

    fn wrap_for_expected_slice(
        &mut self,
        val: Reg,
        expected_ty: TypeID,
        actual_expr: ExprID,
        func: &mut VMFunction,
    ) -> Reg {
        if matches!(&*expected_ty, Type::Slice(_)) {
            let actual_ty = self.representation_type(actual_expr);
            self.wrap_as_slice(val, actual_ty, func)
        } else {
            val
        }
    }

    /// Check if a type is represented as a pointer.
    fn is_ptr_type(&self, ty: &TypeID) -> bool {
        matches!(
            &**ty,
            Type::Name(_, _)
                | Type::Tuple(_)
                | Type::Array(_, _)
                | Type::Slice(_)
                | Type::Reference(_)
                | Type::Func(_, _)
                | Type::Float32x4
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

    /// Emit `base + offset` into a fresh register. `IAddImm` packs its
    /// immediate as an i8, so materialize the constant when the offset is
    /// out of range — aggregates larger than 128 bytes reach this.
    fn emit_offset_addr(&mut self, base: Reg, offset: i32, func: &mut VMFunction) -> Reg {
        let dst = self.alloc_reg();
        if (i8::MIN as i32..=i8::MAX as i32).contains(&offset) {
            func.emit(Opcode::IAddImm {
                dst,
                src: base,
                imm: offset,
            });
        } else {
            let off_reg = self.alloc_reg();
            func.emit(Opcode::LoadImm {
                dst: off_reg,
                value: offset as i64,
            });
            func.emit(Opcode::IAdd {
                dst,
                a: base,
                b: off_reg,
            });
        }
        dst
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
                let addr = self.emit_offset_addr(base, offset, func);
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
        &mut self,
        ty: &TypeID,
        base: Reg,
        offset: i32,
        src: Reg,
        func: &mut VMFunction,
    ) {
        if self.is_ptr_type(ty) {
            // Composite values (structs, tuples, arrays, slices, closures) are
            // represented by the address of their storage, so copy the bytes
            // into place rather than storing the pointer itself.
            let dst = self.emit_offset_addr(base, offset, func);
            let size = self.vm_type_size(ty);
            func.emit(Opcode::MemCopy { dst, src, size });
            return;
        }
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::checked::{DefId, InstanceRecord};
    use crate::vm::VM;

    fn program(expressions: Vec<(Expr, TypeID)>) -> VMProgram {
        let mut arena = CheckedBody::new();
        for (expr, ty) in expressions {
            arena.add(expr, ty, crate::test_loc());
        }
        compile_body(arena)
    }

    fn compile_body(arena: CheckedBody) -> VMProgram {
        let body = arena.len() - 1;
        let func = CheckedFunction {
            name: Name::str("main"),
            typevars: vec![],
            size_vars: vec![],
            params: vec![],
            body: Some(body),
            ret: arena.ty(body),

            requires: vec![],
            loc: crate::test_loc(),
            arena,
            closure_vars: vec![],
            is_extern: false,
        };
        let checked = SpecializedProgram::from_instances(
            vec![Decl::Func(func)],
            vec![InstanceRecord {
                definition: DefId(0),
                type_args: vec![],
                size_args: vec![],
                declaration: 0,
            }],
        );
        VMCodegen::new().compile(&checked).unwrap()
    }

    #[test]
    fn test_compile_simple_int() {
        let program = program(vec![(Expr::Int(42, None), mk_type(Type::Int32))]);
        assert_eq!(VM::new().run(&program), 42);
    }

    #[test]
    fn test_compile_addition() {
        let program = program(vec![
            (Expr::Int(10, None), mk_type(Type::Int32)),
            (Expr::Int(32, None), mk_type(Type::Int32)),
            (Expr::Binop(Binop::Plus, 0, 1), mk_type(Type::Int32)),
        ]);
        assert_eq!(VM::new().run(&program), 42);
    }

    #[test]
    fn test_compile_float_arithmetic() {
        let ty = mk_type(Type::Float32);
        let program = program(vec![
            (Expr::Real("1.5".into(), None), ty),
            (Expr::Real("2.5".into(), None), ty),
            (Expr::Binop(Binop::Mult, 0, 1), ty),
        ]);
        assert!((VM::new().run_f32(&program) - 3.75).abs() < 0.0001);
    }

    #[test]
    fn test_compile_if_else() {
        let program = program(vec![
            (Expr::True, mk_type(Type::Bool)),
            (Expr::Int(100, None), mk_type(Type::Int32)),
            (Expr::Int(200, None), mk_type(Type::Int32)),
            (Expr::If(0, 1, Some(2)), mk_type(Type::Int32)),
        ]);
        assert_eq!(VM::new().run(&program), 100);
    }

    #[test]
    fn test_compile_while_loop() {
        let ty = mk_type(Type::Int32);
        let void = mk_type(Type::Void);
        let boolean = mk_type(Type::Bool);
        let loc = crate::test_loc();
        for iterations in [0, 1, 4] {
            let mut arena = CheckedBody::new();
            let counter = arena.add_local(Name::str("counter"), ty, true);
            let zero = arena.add(Expr::Int(0, None), ty, loc);
            let binding = arena.add(Expr::Var(counter, Some(zero), None), void, loc);
            let condition = if iterations == 0 {
                arena.add(Expr::False, boolean, loc)
            } else {
                let read = arena.add(Expr::Id(Reference::Local(counter)), ty, loc);
                let limit = arena.add(Expr::Int(iterations, None), ty, loc);
                arena.add(Expr::Binop(Binop::Less, read, limit), boolean, loc)
            };
            let read = arena.add(Expr::Id(Reference::Local(counter)), ty, loc);
            let one = arena.add(Expr::Int(1, None), ty, loc);
            let next = arena.add(Expr::Binop(Binop::Plus, read, one), ty, loc);
            let increment = arena.add(Expr::Binop(Binop::Assign, read, next), ty, loc);
            let loop_expr = arena.add(Expr::While(condition, increment), void, loc);
            // The returned state exposes omitted, extra, or missing iterations.
            let result = arena.add(Expr::Id(Reference::Local(counter)), ty, loc);
            arena.add(Expr::Block(vec![binding, loop_expr, result]), ty, loc);

            assert_eq!(
                VM::new().run(&compile_body(arena)),
                iterations,
                "while loop with {} iterations",
                iterations
            );
        }
    }

    #[test]
    fn resolved_targets_and_body_local_ids_survive_lowering() {
        use crate::checked::CheckedParam;
        let ty = mk_type(Type::Int32);
        let loc = crate::test_loc();
        let make_function = |name, params, arena: CheckedBody| CheckedFunction {
            name,
            params,
            body: Some(arena.len() - 1),
            ret: ty,
            typevars: vec![],
            size_vars: vec![],

            requires: vec![],
            loc,
            arena,
            closure_vars: vec![],
            is_extern: false,
        };
        // Both callee definitions deliberately have the same diagnostic name.
        // The checked instance reference chooses the second definition.
        let mut wrong_body = CheckedBody::new();
        wrong_body.add(Expr::Int(900, None), ty, loc);
        let wrong = make_function(Name::str("step"), vec![], wrong_body);
        let mut callee_body = CheckedBody::new();
        let parameter = callee_body.add_local(Name::str("value"), ty, false);
        let value = callee_body.add(Expr::Id(Reference::Local(parameter)), ty, loc);
        let one = callee_body.add(Expr::Int(1, None), ty, loc);
        callee_body.add(Expr::Binop(Binop::Plus, value, one), ty, loc);
        let callee = make_function(
            Name::str("step"),
            vec![CheckedParam { local: parameter }],
            callee_body,
        );

        let mut main_body = CheckedBody::new();
        let outer = main_body.add_local(Name::str("value"), ty, false);
        assert_eq!(
            outer, parameter,
            "the two bodies intentionally reuse local index zero"
        );
        let forty = main_body.add(Expr::Int(40, None), ty, loc);
        let binding = main_body.add(Expr::Let(outer, forty, None), mk_type(Type::Void), loc);
        let target = main_body.add(
            Expr::Id(Reference::Instance(InstanceId(2))),
            callee.ty(),
            loc,
        );
        let arg = main_body.add(Expr::Int(0, None), ty, loc);
        let first_call = main_body.add(Expr::Call(target, vec![arg]), ty, loc);
        let target = main_body.add(
            Expr::Id(Reference::Instance(InstanceId(2))),
            callee.ty(),
            loc,
        );
        let arg = main_body.add(Expr::Int(0, None), ty, loc);
        let second_call = main_body.add(Expr::Call(target, vec![arg]), ty, loc);
        let call = main_body.add(Expr::Binop(Binop::Plus, first_call, second_call), ty, loc);
        let outer_read = main_body.add(Expr::Id(Reference::Local(outer)), ty, loc);
        let sum = main_body.add(Expr::Binop(Binop::Plus, call, outer_read), ty, loc);
        main_body.add(Expr::Block(vec![binding, sum]), ty, loc);
        let main = make_function(Name::str("main"), vec![], main_body);
        let checked = SpecializedProgram::from_instances(
            vec![Decl::Func(main), Decl::Func(wrong), Decl::Func(callee)],
            (0..3)
                .map(|i| InstanceRecord {
                    definition: DefId(i),
                    type_args: vec![],
                    size_args: vec![],
                    declaration: i as usize,
                })
                .collect(),
        );
        let vm = VMCodegen::new().compile(&checked).unwrap();
        assert_eq!(VM::new().run(&vm), 42);
        let mut stack = crate::stack_codegen::StackCodegen::new()
            .compile(&checked)
            .unwrap();
        for function in &mut stack.functions {
            crate::stack_rebase_lm::rebase(function);
            crate::stack_rebase_lm::patch_call_preserve(function);
        }
        assert_eq!(crate::stack_interp_bridge::run(&stack), 42);
    }

    fn check_capture_program(source: &str) {
        let mut compiler = crate::Compiler::new();
        compiler.quiet = true;
        compiler.parse(source, ".");
        assert!(
            compiler.check(),
            "capture regression must check successfully"
        );
        compiler.specialize().unwrap();
        let stack = compiler.compile_stack().unwrap();
        assert_eq!(
            crate::stack_interp_bridge::run(&stack),
            42,
            "Stack capture result"
        );
        let vm = compiler.compile_vm().unwrap();
        assert_eq!(VM::new().run(&vm), 42, "register VM capture result");
    }

    #[test]
    fn scalar_parameter_capture_has_storage_before_conditional() {
        check_capture_program(
            r#"
            capture(value: i32, create: bool) -> i32 {
                if create { let read = || { value }; }
                value
            }
            main() -> i32 { capture(42, false) }
        "#,
        );
    }

    #[test]
    fn aggregate_parameter_capture_keeps_the_aggregate_address() {
        check_capture_program(
            r#"
            capture(values: [i32; 2]) -> i32 {
                var total = 0
                for i in 0 .. 2 {
                    let read = || { values[0] }
                    total = total + read()
                }
                total
            }
            main() -> i32 { capture([21, 0]) }
        "#,
        );
    }

    #[test]
    fn borrowed_parameter_capture_keeps_the_borrowed_address() {
        check_capture_program(
            r#"
            capture(value: &i32) -> i32 {
                for i in 0 .. 2 {
                    let increment = || { value = value + 1 }
                    increment()
                }
                value
            }
            main() -> i32 { var value = 40; capture(value) }
        "#,
        );
    }
}
