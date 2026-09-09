//! Stack-based code generator.
//!
//! This module translates a SpecializedProgram into a StackProgram that can be
//! executed by a stack-based virtual machine. It mirrors the register-based
//! VM codegen but emits stack IR instructions instead.

use crate::checked::{CheckedFunction, InstanceId, LocalId, Reference, SpecializedProgram};
use crate::decl::Decl;
use crate::defs::*;
use crate::expr::Expr;
use crate::stack_ir::*;
use crate::types::*;
use std::collections::{HashMap, HashSet};
use std::convert::TryFrom;

/// A checked private input or exact literal awaiting native operand storage.
#[derive(Clone, Copy)]
enum LoopScalar {
    Local(u16),
    Constant(u64),
}

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
    func_idx: u32,
    /// Index of the instruction within that function.
    instr_idx: usize,
    /// Concrete function instance being called.
    callee: InstanceId,
}

/// How a local variable is stored.
#[derive(Clone, Copy, Debug)]
enum LocalKind {
    /// Scalar value in a numbered local slot.
    Scalar(u16),
    /// Memory-backed local; LocalAddr(slot) gives its address.
    Memory(u16),
    /// Thin reference stored as an address in a scalar local slot.
    Reference(u16),
}

/// Code generator for the stack-based VM.
pub struct StackCodegen {
    /// Opt in to the shared precompiled native-loop consumer.
    pub(crate) native_loops: bool,

    /// The program being built.
    program: StackProgram,

    /// Map from function instances to their indices in the program.
    func_indices: HashMap<InstanceId, u32>,

    /// Functions that have been compiled.
    compiled_functions: HashSet<InstanceId>,

    /// Functions that need to be compiled.
    pending_functions: Vec<InstanceId>,

    /// Calls that need to be patched after all functions are compiled.
    pending_calls: Vec<PendingCall>,

    /// I64Const instructions for function references that need patching.
    pending_func_loads: Vec<PendingCall>,

    /// Global variable offsets.
    globals: HashMap<InstanceId, i32>,

    /// Counter for generating unique lambda names.
    lambda_counter: usize,
}

impl Default for StackCodegen {
    fn default() -> Self {
        Self::new()
    }
}

impl StackCodegen {
    pub fn new() -> Self {
        Self {
            native_loops: false,
            program: StackProgram::new(),
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
    ///
    /// Reserves `CANCEL_FLAG_RESERVED` bytes at the start of the globals
    /// buffer for the cancel/trap header (cancel counter, callback,
    /// trap_reason, jmp_buf). The header layout matches the one used by
    /// the JIT/LLVM backends, which lets the FFI layer write the stack
    /// interp's structural trap reason to `TRAP_REASON_OFFSET` so hosts
    /// can call `read_trap_reason(globals)` uniformly across backends.
    fn declare_globals(&mut self, decls: &SpecializedProgram) {
        let mut offset: i32 = crate::cancel::CANCEL_FLAG_RESERVED;
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

    /// Compile a SpecializedProgram into a StackProgram.
    pub fn compile(&mut self, decls: &SpecializedProgram) -> Result<StackProgram, String> {
        let main_name = Name::str("main");
        self.compile_multi(decls, &[main_name])
    }

    /// Compile multiple entry points into a StackProgram.
    ///
    /// Entry points that aren't defined are skipped: only the ones that were
    /// found show up in `program.entry_points`.
    pub fn compile_multi(
        &mut self,
        decls: &SpecializedProgram,
        entry_points: &[Name],
    ) -> Result<StackProgram, String> {
        self.declare_globals(decls);

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
        // point that was actually found. If none were found, program.entry
        // stays at its default and the map is empty.
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

        // Patch pending calls.
        for pending in &self.pending_calls {
            if let Some(&callee_idx) = self.func_indices.get(&pending.callee) {
                let func = &mut self.program.functions[pending.func_idx as usize];
                if let StackOp::Call {
                    func: ref mut f, ..
                } = func.ops[pending.instr_idx]
                {
                    *f = callee_idx;
                }
            }
        }

        // Patch pending function reference loads.
        for pending in &self.pending_func_loads {
            if let Some(&callee_idx) = self.func_indices.get(&pending.callee) {
                let func = &mut self.program.functions[pending.func_idx as usize];
                if let StackOp::I64Const(ref mut value) = func.ops[pending.instr_idx] {
                    *value = callee_idx as i64;
                }
            }
        }

        Ok(std::mem::replace(&mut self.program, StackProgram::new()))
    }

    /// Compile a single function.
    fn compile_function(
        &mut self,
        decl: &CheckedFunction,
        decls: &SpecializedProgram,
        instance: Option<InstanceId>,
    ) -> Result<u32, String> {
        let mut func = StackFunction::new(&*decl.name);
        func.param_count = decl.params.len() as u8;

        let pending_function_count = self.pending_functions.len();
        let lambda_counter = self.lambda_counter;

        let mut translator = FunctionTranslator::new(
            decl,
            decls,
            &mut self.pending_functions,
            &mut self.lambda_counter,
            &self.globals,
            self.native_loops && instance.is_some(),
        );
        if !translator.translate(&mut func)? {
            // Native setup is optional. Discard the complete unpublished draft
            // and retry ordinary lowering if its extra slots exceed the encoding.
            drop(translator);
            self.pending_functions.truncate(pending_function_count);
            self.lambda_counter = lambda_counter;
            func = StackFunction::new(&*decl.name);
            func.param_count = decl.params.len() as u8;
            translator = FunctionTranslator::new(
                decl,
                decls,
                &mut self.pending_functions,
                &mut self.lambda_counter,
                &self.globals,
                false,
            );
            translator.translate(&mut func)?;
        }

        let idx = self.program.add_function(func);
        if let Some(instance) = instance {
            self.func_indices.insert(instance, idx);
            self.compiled_functions.insert(instance);
        }

        // Collect pending calls.
        let calls_to_patch = std::mem::take(&mut translator.calls_to_patch);
        let func_load_patches = std::mem::take(&mut translator.func_load_patches);
        let pending_lambdas = std::mem::take(&mut translator.pending_lambdas);
        let lambda_patches = std::mem::take(&mut translator.lambda_patches);
        drop(translator);

        for call in calls_to_patch {
            self.pending_calls.push(PendingCall {
                func_idx: idx,
                instr_idx: call.instr_idx,
                callee: call.callee,
            });
        }

        for patch in func_load_patches {
            self.pending_func_loads.push(PendingCall {
                func_idx: idx,
                instr_idx: patch.instr_idx,
                callee: patch.callee,
            });
        }

        // Compile lambda functions and patch their indices.
        for lambda_decl in pending_lambdas {
            let lambda_name = lambda_decl.name;
            let lambda_idx = self.compile_function(&lambda_decl, decls, None)?;
            for &(instr_idx, patch_name) in &lambda_patches {
                if patch_name == lambda_name {
                    if let StackOp::I64Const(ref mut value) =
                        self.program.functions[idx as usize].ops[instr_idx]
                    {
                        *value = lambda_idx as i64;
                    }
                }
            }
        }

        Ok(idx)
    }
}

/// A call instruction that needs patching.
struct CallToPatch {
    instr_idx: usize,
    callee: InstanceId,
}

/// Check if a type should be returned via output pointer (sret).
fn returns_via_pointer(ty: TypeID) -> bool {
    matches!(
        &*ty,
        Type::Array(_, _) | Type::Slice(_) | Type::Name(_, _) | Type::Tuple(_) | Type::Float32x4
    )
}

fn stack_extern_ret_type(ty: TypeID) -> StackExternRet {
    match &*ty {
        Type::Void => StackExternRet::Void,
        Type::Bool => StackExternRet::Bool,
        Type::Int32 => StackExternRet::I32,
        Type::Float32 => StackExternRet::F32,
        Type::Float64 => StackExternRet::F64,
        Type::Reference(_) => StackExternRet::Ptr,
        _ => panic!(
            "unsupported stack extern return type: {}",
            ty.pretty_print()
        ),
    }
}

/// Translator for a single function body.
struct FunctionTranslator<'a> {
    /// The function declaration being translated.
    decl: &'a CheckedFunction,

    /// Declaration table for looking up types and functions.
    decls: &'a SpecializedProgram,

    /// Map from local identities to their local storage kind.
    variables: HashMap<LocalId, LocalKind>,

    /// Next available scalar local slot.
    next_scalar: u32,

    /// Next available memory slot (in 8-byte units).
    next_memory_slot: u32,

    /// Highest allocated memory start, including zero-sized objects.
    max_memory_slot: Option<u32>,

    /// Analysis belongs to precisely this immutable checked body, after hoisting.
    loops: HashMap<ExprID, crate::value_loops::LoopRegion>,
    native_loop_count: usize,

    /// Functions that are called and need to be compiled.
    pending_functions: &'a mut Vec<InstanceId>,

    /// Counter for generating unique lambda names.
    lambda_counter: &'a mut usize,

    /// Calls that need patching.
    calls_to_patch: Vec<CallToPatch>,

    /// Lambda CheckedFunctions extracted from this function body, to be compiled afterward.
    pending_lambdas: Vec<CheckedFunction>,

    /// I64Const instructions that need to be patched with lambda function indices.
    lambda_patches: Vec<(usize, Name)>,

    /// I64Const instructions for function references that need patching.
    func_load_patches: Vec<CallToPatch>,

    /// Global variable offsets.
    globals: &'a HashMap<InstanceId, i32>,

    /// Memory slot for the sret output pointer (if returning ptr type).
    output_ptr_slot: Option<u16>,

    /// Whether a return has been emitted.
    has_returned: bool,

    /// Stack of loop contexts for nested loops.
    loop_stack: Vec<LoopContext>,

    /// Variables captured from an enclosing scope (double indirection).
    captured_vars: HashSet<LocalId>,

    /// Names a lambda in this function mentions. Shared with the closure by
    /// address, so they must be memory-backed from the start.
    lambda_referenced: HashSet<LocalId>,

    /// Memory slot indices for captured variables (stores pointer-to-storage).
    captured_slots: HashMap<LocalId, u16>,

    /// True when the current expression's result will be discarded.
    void_ctx: bool,

    /// `Expr::Let` ids whose value-copy is unobservable, so the binding can
    /// alias the initializer's storage instead. See `crate::copy_elision`.
    elidable_lets: HashSet<ExprID>,
}

impl<'a> FunctionTranslator<'a> {
    fn new(
        decl: &'a CheckedFunction,
        decls: &'a SpecializedProgram,
        pending_functions: &'a mut Vec<InstanceId>,
        lambda_counter: &'a mut usize,
        globals: &'a HashMap<InstanceId, i32>,
        native_loops: bool,
    ) -> Self {
        Self {
            decl,
            decls,
            variables: HashMap::new(),

            next_scalar: 0,
            next_memory_slot: 0,
            max_memory_slot: None,
            loops: if native_loops {
                crate::value_loops::analyze_function(decl)
            } else {
                HashMap::new()
            },
            native_loop_count: 0,
            pending_functions,
            lambda_counter,
            calls_to_patch: Vec::new(),
            pending_lambdas: Vec::new(),
            lambda_patches: Vec::new(),
            func_load_patches: Vec::new(),
            globals,
            output_ptr_slot: None,
            has_returned: false,
            loop_stack: Vec::new(),
            captured_vars: HashSet::new(),
            lambda_referenced: decl.captured_locals(),
            captured_slots: HashMap::new(),
            void_ctx: false,
            elidable_lets: crate::copy_elision::elidable_let_copies(decl),
        }
    }

    /// Allocate a scalar local slot.
    fn alloc_scalar(&mut self) -> u16 {
        let slot = self.next_scalar;
        self.next_scalar = self.next_scalar.saturating_add(1);
        // Draft operands are narrow, but a complete wide frame check runs before
        // any function is published. Overflowing drafts are discarded.
        slot as u16
    }

    /// Allocate a memory-backed local slot. Returns the memory slot index.
    /// size is in bytes; we round up to 8-byte units.
    fn alloc_memory(&mut self, size: u32) -> u16 {
        let slot = self.next_memory_slot;
        self.max_memory_slot = Some(self.max_memory_slot.map_or(slot, |last| last.max(slot)));
        let slots_needed = size / 8 + u32::from(size % 8 != 0);
        self.next_memory_slot = self.next_memory_slot.saturating_add(slots_needed);
        slot as u16
    }

    /// Get the type of an expression.
    fn expr_type(&self, expr: ExprID) -> TypeID {
        self.decl.arena.ty(expr)
    }

    /// Get the type that determines how an expression is represented at runtime.
    ///
    /// A call site can solve an array expression as a slice, while codegen still
    /// has an array address and must explicitly build the slice fat pointer.
    fn representation_type(&self, expr: ExprID) -> TypeID {
        match &self.decl.arena[expr] {
            Expr::Id(_) => match self.decl.arena.reference(expr) {
                Some(Reference::Local(local)) => {
                    let ty = self.decl.arena.local(*local).ty;
                    match &*ty {
                        Type::Reference(inner) => *inner,
                        _ => ty,
                    }
                }
                Some(Reference::Instance(instance)) => self.decls.instance(*instance).ty(),
                _ => self.expr_type(expr),
            },
            Expr::ArrayIndex(arr_id, _) => match &*self.representation_type(*arr_id) {
                Type::Array(elem, _) | Type::Slice(elem) | Type::Reference(elem) => *elem,
                _ => self.expr_type(expr),
            },
            _ => self.expr_type(expr),
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

    /// Size of a type in the VM. Function types use 16-byte fat pointers.
    fn vm_type_size(&self, ty: &TypeID) -> u32 {
        if matches!(&**ty, Type::Func(_, _)) {
            16
        } else {
            ty.size(self.decls) as u32
        }
    }

    /// Translate the function body.
    fn translate(&mut self, func: &mut StackFunction) -> Result<bool, String> {
        // If return type is a pointer type, first parameter is output pointer.
        let has_sret = returns_via_pointer(self.decl.ret);
        if has_sret {
            let sret_slot = self.alloc_scalar();
            self.output_ptr_slot = Some(sret_slot);
            func.param_count += 1;
            // The sret pointer arrives as param 0; store it in its scalar slot.
            // (It's already in local 0 by calling convention.)
        }

        // Reserve scalar locals for parameters.
        let param_offset = if has_sret { 1u16 } else { 0u16 };
        for (i, param) in self.decl.params.iter().enumerate() {
            let param_slot = param_offset + i as u16;
            let ty = self.decl.arena.local(param.local).ty;

            if let Type::Reference(_) = &*ty {
                while self.next_scalar <= u32::from(param_slot) {
                    self.alloc_scalar();
                }
                self.variables
                    .insert(param.local, LocalKind::Reference(param_slot));
            } else if !self.is_ptr_type(&ty) {
                // Scalar parameter: already in local slot param_slot by calling convention.
                // Just make sure our allocator accounts for it.
                while self.next_scalar <= u32::from(param_slot) {
                    self.alloc_scalar();
                }
                self.variables
                    .insert(param.local, LocalKind::Scalar(param_slot));
            } else {
                // Pointer-represented parameters are passed as addresses.
                // Keep the address value directly, matching the JIT/LLVM ABI.
                while self.next_scalar <= u32::from(param_slot) {
                    self.alloc_scalar();
                }
                self.variables
                    .insert(param.local, LocalKind::Scalar(param_slot));
            }
        }

        // Capture creation can occur on only one branch. Addressable scalar
        // parameters therefore need initialized storage before control flow splits.
        for (i, param) in self.decl.params.iter().enumerate() {
            let ty = self.decl.arena.local(param.local).ty;
            if self.lambda_referenced.contains(&param.local) && !self.is_ptr_type(&ty) {
                let mem_slot = self.alloc_memory(self.vm_type_size(&ty));
                func.emit(StackOp::LocalAddr(mem_slot));
                self.emit_local_get(&ty, param_offset + i as u16, func);
                self.emit_store_op(&ty, func);
                self.variables
                    .insert(param.local, LocalKind::Memory(mem_slot));
            }
        }

        // Set up captured closure variables.
        if !self.decl.closure_vars.is_empty() {
            // Get the closure pointer (set by CallClosure before entering this function).
            let closure_local = self.alloc_scalar();
            func.emit(StackOp::GetClosurePtr);
            func.emit(StackOp::LocalSet(closure_local));

            for (i, cv) in self.decl.closure_vars.iter().enumerate() {
                // Load address of captured variable from closure_struct[i].
                let _mem_slot = self.alloc_memory(8);
                // Push closure_local, load the address at offset i*8.
                func.emit(StackOp::LocalGet(closure_local));
                func.emit(StackOp::I64Const((i * 8) as i64));
                func.emit(StackOp::IAdd);
                func.emit(StackOp::Load64);
                // Store the captured variable address in a scalar local.
                let addr_local = self.alloc_scalar();
                func.emit(StackOp::LocalSet(addr_local));
                // Save for later access.
                self.captured_vars.insert(*cv);
                self.captured_slots.insert(*cv, addr_local);
                // Also register in variables so nested closures can find this capture.
                self.variables.insert(*cv, LocalKind::Scalar(addr_local));
            }
        }

        // Translate body. For void-returning functions, translate in
        // void context so the body's final expression is consumed by
        // the appropriate window-aware drop (Let → DropF/Drop, Var →
        // nothing, etc.) rather than dropped blindly here. For
        // value-returning functions, translate in non-void context so
        // the last expression's value is left on the stack for Return.
        let returns_void_no_sret = !has_sret && matches!(&*self.decl.ret, Type::Void);
        if let Some(body) = self.decl.body {
            // A body that is itself an f32x4 computation — a lambda's
            // expression body, say — writes into the sret buffer directly,
            // with no result temp and no 16-byte copy.
            let sret_vector_body = match self.output_ptr_slot {
                Some(slot) => self.f32x4_store_op(body).map(|op| (slot, op)),
                None => None,
            };
            if let Some((sret_slot, store_op)) = sret_vector_body {
                self.emit_f32x4_operands(body, func);
                func.emit(StackOp::LocalGet(sret_slot));
                func.emit(store_op);
                func.emit(StackOp::ReturnVoid);
                self.has_returned = true;
            } else if returns_void_no_sret {
                self.translate_void(body, func);
            } else {
                self.translate_expr(body, func);
            }

            if !self.has_returned {
                if has_sret {
                    // Copy result to output pointer.
                    let size = self.decl.ret.size(self.decls) as u32;
                    let sret_slot = self.output_ptr_slot.unwrap();
                    // Stack: [result_addr]
                    // Need: dst=sret_ptr, src=result_addr
                    let tmp = self.alloc_scalar();
                    func.emit(StackOp::LocalSet(tmp)); // save result addr
                    func.emit(StackOp::LocalGet(sret_slot)); // push sret ptr (dst)
                    func.emit(StackOp::LocalGet(tmp)); // push result addr (src)
                    func.emit(StackOp::MemCopy(size));
                    func.emit(StackOp::ReturnVoid);
                } else if matches!(&*self.decl.ret, Type::Void) {
                    // translate_void has already consumed the body's
                    // value (if any) through the window-aware drop
                    // path. Just emit the return.
                    func.emit(StackOp::ReturnVoid);
                } else {
                    // Fall-through return: bridge f32/f64 results back to t0.
                    if matches!(&*self.decl.ret, Type::Float32) {
                        func.emit(StackOp::FToBitsF);
                    } else if matches!(&*self.decl.ret, Type::Float64) {
                        func.emit(StackOp::DToBitsD);
                    }
                    func.emit(StackOp::Return);
                }
            }
        } else {
            func.emit(StackOp::ReturnVoid);
        }

        let memory_starts_fit = self
            .max_memory_slot
            .is_none_or(|slot| slot.saturating_add(self.next_scalar) <= u32::from(u16::MAX));
        if self.next_scalar > u32::from(u16::MAX)
            || !memory_starts_fit
            || self.next_memory_slot.checked_mul(8).is_none()
        {
            if self.native_loop_count != 0 {
                return Ok(false);
            }
            return Err(format!(
                "stack function {} exceeds frame slot limits ({} scalar slots, {} memory slots)",
                self.decl.name, self.next_scalar, self.next_memory_slot,
            ));
        }
        func.local_count = self.next_scalar as u16;
        func.local_memory = self.next_memory_slot * 8;
        func.has_return_value = !matches!(&*self.decl.ret, Type::Void) && !has_sret;
        Ok(true)
    }

    /// Translate an expression. Pushes exactly one value onto the stack
    /// (or an address for pointer types).
    /// Translate an expression in void context (result will be discarded).
    /// Only optimizes specific expression types known to be safe.
    fn translate_void(&mut self, expr: ExprID, func: &mut StackFunction) {
        match &self.decl.arena[expr].clone() {
            // Declarations have no language value. Lower their storage effects
            // directly, without materializing an operand-stack placeholder.
            Expr::Let(..) | Expr::Var(..) => {
                self.translate_expr_inner(expr, func, true);
            }
            // An f32x4 assignment in statement position: void context is
            // what lets translate_assign send the vector ops straight at
            // the destination instead of computing into a temp and copying
            // 16 bytes over. Nothing is left on the stack to drop.
            Expr::Binop(Binop::Assign, lhs_id, _)
                if matches!(&*self.expr_type(*lhs_id), Type::Float32x4) =>
            {
                self.translate_expr_inner(expr, func, true);
            }
            // Block: recurse with void context for every expression,
            // including the last. Using translate_void for the last
            // expression lets value-producing constructs (If, For, While,
            // calls to void functions) avoid emitting their placeholder
            // result + matching drop — important for the FFT inner loop
            // where an if-without-else previously cost 4 wasted ops per
            // iteration.
            Expr::Block(exprs) => {
                let exprs = exprs.clone();
                if !exprs.is_empty() {
                    for &expr_id in exprs.iter() {
                        self.translate_void(expr_id, func);
                    }
                }
            }
            // If in void context: no need to produce a value on both branches.
            Expr::If(cond_id, then_id, else_id) => {
                let cond_id = *cond_id;
                let then_id = *then_id;
                let else_id = *else_id;
                let saved_has_returned = self.has_returned;
                self.translate_expr(cond_id, func);
                let jump_to_else = func.pos();
                func.emit(StackOp::JumpIfZero(0));
                self.translate_void(then_id, func);
                let then_returned = self.has_returned;
                if let Some(else_expr_id) = else_id {
                    let jump_to_end = func.pos();
                    func.emit(StackOp::Jump(0));
                    func.patch_jump(jump_to_else);
                    self.has_returned = saved_has_returned;
                    self.translate_void(else_expr_id, func);
                    let else_returned = self.has_returned;
                    func.patch_jump(jump_to_end);
                    // Only mark as returned if BOTH branches returned.
                    self.has_returned = then_returned && else_returned;
                } else {
                    // No else: just patch the jump. No dead value needed.
                    func.patch_jump(jump_to_else);
                    // Single-branch if can't guarantee a return.
                    self.has_returned = saved_has_returned;
                }
            }
            // While/For: the body result is always discarded.
            Expr::While(..) | Expr::For { .. } => {
                self.translate_expr_inner(expr, func, true);
            }
            // Everything else: translate normally then drop.
            _ => {
                self.translate_expr(expr, func);
                let ty = self.expr_type(expr);
                if matches!(&*ty, Type::Float32) {
                    func.emit(StackOp::DropF);
                } else if matches!(&*ty, Type::Float64) {
                    func.emit(StackOp::DropD);
                } else {
                    func.emit(StackOp::Drop);
                }
            }
        }
    }

    fn translate_expr(&mut self, expr: ExprID, func: &mut StackFunction) {
        self.translate_expr_inner(expr, func, false);
    }

    fn translate_expr_inner(&mut self, expr: ExprID, func: &mut StackFunction, void_ctx: bool) {
        // Save/restore void_ctx so it reflects the current caller's
        // context at each translate_expr_inner entry. Without the restore,
        // a nested translate_expr call would clobber the flag and the
        // outer While/For handler would wrongly believe its result was
        // needed, emitting a spurious I64Const(0). That used to be
        // harmless because op_return_void did FILL_ALL and forcibly
        // restored the caller's TOS window from memory, but with the
        // no-spill op_call/op_return design any trailing value leaves
        // the callee's final depth unbalanced and leaks into the caller.
        let declaration = matches!(self.decl.arena[expr], Expr::Let(..) | Expr::Var(..))
            && matches!(&*self.expr_type(expr), Type::Void);
        let old_void_ctx = self.void_ctx;
        self.void_ctx = void_ctx || declaration;
        self.translate_expr_inner_body(expr, func);
        self.void_ctx = old_void_ctx;
        if declaration && !void_ctx {
            // Generic expression composition uses one inert value for void.
            func.emit(StackOp::I64Const(0));
        }
    }

    fn translate_expr_inner_body(&mut self, expr: ExprID, func: &mut StackFunction) {
        match &self.decl.arena[expr].clone() {
            Expr::Int(n, _) => {
                func.emit(StackOp::I64Const(*n));
            }

            Expr::Real(s, _) => {
                let ty = self.expr_type(expr);
                match &*ty {
                    Type::Float32 => {
                        let value: f32 = s.parse().unwrap_or(0.0);
                        func.emit(StackOp::F32ConstF(value));
                    }
                    Type::Float64 => {
                        let value: f64 = s.parse().unwrap_or(0.0);
                        func.emit(StackOp::F64ConstD(value));
                    }
                    _ => {
                        let value: f32 = s.parse().unwrap_or(0.0);
                        func.emit(StackOp::F32ConstF(value));
                    }
                }
            }

            Expr::True => {
                func.emit(StackOp::I64Const(1));
            }

            Expr::False => {
                func.emit(StackOp::I64Const(0));
            }

            Expr::Char(c) => {
                func.emit(StackOp::I64Const(*c as i64));
            }

            Expr::String(s) => {
                let bytes = s.as_bytes();
                let total_size = bytes.len() as u32 + 1;
                let mem_slot = self.alloc_memory(total_size);
                // Store each byte.
                for (i, &b) in bytes.iter().enumerate() {
                    func.emit(StackOp::LocalAddr(mem_slot));
                    func.emit(StackOp::I64Const(b as i64));
                    func.emit(StackOp::Store8Off(i as i32));
                }
                // Null terminator.
                func.emit(StackOp::LocalAddr(mem_slot));
                func.emit(StackOp::I64Const(0));
                func.emit(StackOp::Store8Off(bytes.len() as i32));
                // Push address as result.
                func.emit(StackOp::LocalAddr(mem_slot));
            }

            Expr::Id(_) => {
                let decl = self.decl;
                let reference = decl.arena.reference(expr).expect("checked reference");
                self.translate_id(reference, expr, func);
            }

            Expr::Enum(case_name) => {
                let case_name = *case_name;
                let index = if let Type::Name(enum_name, _) = &*self.decl.arena.ty(expr) {
                    let enum_decls = self.decls.find(*enum_name);
                    if let Some(Decl::Enum { cases, .. }) =
                        enum_decls.iter().find(|d| matches!(d, Decl::Enum { .. }))
                    {
                        cases.iter().position(|c| *c == case_name).unwrap_or(0) as i64
                    } else {
                        0
                    }
                } else {
                    0
                };
                // Enums are pointer types (Type::Name). Allocate memory for the discriminant.
                let mem_slot = self.alloc_memory(4);
                func.emit(StackOp::LocalAddr(mem_slot));
                func.emit(StackOp::I64Const(index));
                func.emit(StackOp::Store32);
                func.emit(StackOp::LocalAddr(mem_slot));
            }

            Expr::Binop(op, lhs_id, rhs_id) => {
                self.translate_binop(*op, *lhs_id, *rhs_id, expr, func);
            }

            Expr::Unop(op, arg_id) => {
                self.translate_unop(*op, *arg_id, func);
            }

            Expr::Call(fn_id, arg_ids) => {
                let arg_ids = arg_ids.clone();
                self.translate_call(*fn_id, &arg_ids, expr, func);
            }

            Expr::Let(_, init, _) => {
                let name = self.decl.arena.binder(expr);
                let init = *init;
                let ty = self.decl.arena.local(name).ty;

                if !self.is_ptr_type(&ty) && self.lambda_referenced.contains(&name) {
                    // Captured by a lambda: memory-backed from the start.
                    self.translate_expr(init, func);
                    let tmp = self.alloc_scalar();
                    self.emit_local_set(&ty, tmp, func);
                    let mem_slot = self.alloc_memory(self.vm_type_size(&ty));
                    func.emit(StackOp::LocalAddr(mem_slot));
                    self.emit_local_get(&ty, tmp, func);
                    self.emit_store_op(&ty, func);

                    self.variables.insert(name, LocalKind::Memory(mem_slot));

                    if !self.void_ctx {
                        self.emit_local_get(&ty, tmp, func);
                    }
                } else if !self.is_ptr_type(&ty) {
                    // Scalar: translate init, store in local.
                    self.translate_expr(init, func);
                    let local = self.alloc_scalar();
                    if self.void_ctx {
                        self.emit_local_set(&ty, local, func);
                    } else {
                        self.emit_local_tee(&ty, local, func);
                    }

                    self.variables.insert(name, LocalKind::Scalar(local));
                } else if crate::copy_elision::is_value_aggregate(&ty)
                    && !self.elidable_lets.contains(&expr)
                {
                    // `let` binds aggregates by value, so the initializer's
                    // storage has to be copied — otherwise a slice coerced from
                    // the binding writes back into the source. Same shape as
                    // `var`, which has always copied.
                    self.translate_expr(init, func);
                    self.emit_wrap_for_expected_slice(ty, init, func);
                    let size = self.vm_type_size(&ty);
                    let mem_slot = self.alloc_memory(size);
                    let tmp = self.alloc_scalar();
                    func.emit(StackOp::LocalSet(tmp));
                    func.emit(StackOp::LocalAddr(mem_slot));
                    func.emit(StackOp::LocalGet(tmp));
                    func.emit(StackOp::MemCopy(size));

                    self.variables.insert(name, LocalKind::Memory(mem_slot));

                    if !self.void_ctx {
                        func.emit(StackOp::LocalAddr(mem_slot));
                    }
                } else {
                    // Pointer-represented let bindings carry the address value.
                    self.translate_expr(init, func);
                    self.emit_wrap_for_expected_slice(ty, init, func);
                    let local = self.alloc_scalar();
                    if self.void_ctx {
                        func.emit(StackOp::LocalSet(local));
                    } else {
                        func.emit(StackOp::LocalTee(local));
                    }

                    self.variables.insert(name, LocalKind::Scalar(local));
                }
            }

            Expr::Var(_, init, _) => {
                let name = self.decl.arena.binder(expr);
                let init = *init;
                let ty = self.decl.arena.local(name).ty;

                if !self.is_ptr_type(&ty) && self.lambda_referenced.contains(&name) {
                    // Captured by a lambda: memory-backed from the start.
                    let size = self.vm_type_size(&ty);
                    let mem_slot = self.alloc_memory(size);
                    if let Some(init_id) = init {
                        self.translate_expr(init_id, func);
                        let tmp = self.alloc_scalar();
                        self.emit_local_set(&ty, tmp, func);
                        func.emit(StackOp::LocalAddr(mem_slot));
                        self.emit_local_get(&ty, tmp, func);
                        self.emit_store_op(&ty, func);
                    } else {
                        func.emit(StackOp::LocalAddr(mem_slot));
                        func.emit(StackOp::MemZero(size));
                    }

                    self.variables.insert(name, LocalKind::Memory(mem_slot));
                } else if !self.is_ptr_type(&ty) {
                    let local = self.alloc_scalar();
                    if let Some(init_id) = init {
                        if !self.try_emit_binop_set(local, init_id, func) {
                            self.translate_expr(init_id, func);
                            self.emit_local_set(&ty, local, func);
                        }
                    } else {
                        // Uninitialized local — zero the slot via the int window.
                        // (Float zero has the same bit pattern, so this works
                        // for f32 locals too even when use_fp_window is on.)
                        func.emit(StackOp::I64Const(0));
                        func.emit(StackOp::LocalSet(local));
                    }

                    self.variables.insert(name, LocalKind::Scalar(local));
                } else {
                    let size = self.vm_type_size(&ty);
                    let mem_slot = self.alloc_memory(size);
                    if let Some(init_id) = init {
                        // An f32x4 initializer computes into the variable's
                        // own storage — no temp, no 16-byte copy.
                        if matches!(&*ty, Type::Float32x4) && self.f32x4_slot_form(init_id) {
                            self.emit_f32x4_into_slot(init_id, mem_slot, func);

                            self.variables.insert(name, LocalKind::Memory(mem_slot));

                            if !self.void_ctx {
                                func.emit(StackOp::I64Const(0));
                            }
                            return;
                        }
                        if let Some(store_op) = self.f32x4_store_op(init_id) {
                            self.emit_f32x4_operands(init_id, func);
                            func.emit(StackOp::LocalAddr(mem_slot));
                            func.emit(store_op);

                            self.variables.insert(name, LocalKind::Memory(mem_slot));

                            if !self.void_ctx {
                                func.emit(StackOp::I64Const(0));
                            }
                            return;
                        }
                        self.translate_expr(init_id, func);
                        self.emit_wrap_for_expected_slice(ty, init_id, func);
                        let tmp = self.alloc_scalar();
                        func.emit(StackOp::LocalSet(tmp));
                        func.emit(StackOp::LocalAddr(mem_slot));
                        func.emit(StackOp::LocalGet(tmp));
                        func.emit(StackOp::MemCopy(size));
                    } else {
                        func.emit(StackOp::LocalAddr(mem_slot));
                        func.emit(StackOp::MemZero(size));
                    }

                    self.variables.insert(name, LocalKind::Memory(mem_slot));
                }
                // Var expressions produce void; push 0 only if result is needed.
                if !self.void_ctx {
                    func.emit(StackOp::I64Const(0));
                }
            }

            Expr::Block(exprs) => {
                let exprs = exprs.clone();
                if exprs.is_empty() {
                    if !self.void_ctx {
                        func.emit(StackOp::I64Const(0));
                    }
                } else {
                    for (i, &expr_id) in exprs.iter().enumerate() {
                        if i < exprs.len() - 1 {
                            // Intermediate expressions: void context.
                            self.translate_void(expr_id, func);
                        } else if self.void_ctx {
                            // Last expression in void block: also void.
                            self.translate_void(expr_id, func);
                        } else {
                            // Last expression: result needed.
                            self.translate_expr(expr_id, func);
                        }
                    }
                }
            }

            Expr::If(cond_id, then_id, else_id) => {
                self.translate_if(*cond_id, *then_id, *else_id, func);
            }

            Expr::While(cond_id, body_id) => {
                self.translate_while(*cond_id, *body_id, func);
                // translate_while no longer pushes a result; push 0 if needed.
                if !self.void_ctx {
                    func.emit(StackOp::I64Const(0));
                }
            }

            Expr::For {
                start, end, body, ..
            } => {
                let var = self.decl.arena.binder(expr);
                self.translate_for(expr, var, *start, *end, *body, func);
                if !self.void_ctx {
                    func.emit(StackOp::I64Const(0));
                }
            }

            Expr::Return(expr_id) => {
                let expr_id = *expr_id;
                let ret_ty = self.expr_type(expr_id);

                // An f32x4 result computes straight into the sret buffer.
                if let Some(sret_slot) = self.output_ptr_slot {
                    if let Some(store_op) = self.f32x4_store_op(expr_id) {
                        self.emit_f32x4_operands(expr_id, func);
                        func.emit(StackOp::LocalGet(sret_slot));
                        func.emit(store_op);
                        func.emit(StackOp::ReturnVoid);
                        self.has_returned = true;
                        return;
                    }
                }

                self.translate_expr(expr_id, func);

                if returns_via_pointer(ret_ty) {
                    let size = ret_ty.size(self.decls) as u32;
                    let sret_slot = self.output_ptr_slot.unwrap();
                    let tmp = self.alloc_scalar();
                    func.emit(StackOp::LocalSet(tmp));
                    func.emit(StackOp::LocalGet(sret_slot));
                    func.emit(StackOp::LocalGet(tmp));
                    func.emit(StackOp::MemCopy(size));
                    func.emit(StackOp::ReturnVoid);
                } else {
                    // f32/f64 return values travel through t0 (int window).
                    // If the preceding expression left the value in a FP
                    // window, bridge it back to the int window first.
                    if matches!(&*ret_ty, Type::Float32) {
                        func.emit(StackOp::FToBitsF);
                    } else if matches!(&*ret_ty, Type::Float64) {
                        func.emit(StackOp::DToBitsD);
                    }
                    func.emit(StackOp::Return);
                }
                self.has_returned = true;
            }

            Expr::Break => {
                let break_jump = func.pos();
                func.emit(StackOp::Jump(0)); // placeholder
                self.loop_stack
                    .last_mut()
                    .expect("break outside loop")
                    .break_patches
                    .push(break_jump);
                func.emit(StackOp::I64Const(0)); // unreachable but keeps stack balanced
            }

            Expr::Continue => {
                let ctx = self.loop_stack.last().expect("continue outside loop");
                let continue_target = ctx.continue_target;
                if continue_target == 0 {
                    let jump_pos = func.pos();
                    func.emit(StackOp::Jump(0));
                    self.loop_stack
                        .last_mut()
                        .unwrap()
                        .continue_patches
                        .push(jump_pos);
                } else {
                    let pos = func.pos();
                    func.emit(StackOp::Jump((continue_target as i32) - (pos as i32) - 1));
                }
                func.emit(StackOp::I64Const(0)); // stack balance
            }

            Expr::Field(lhs_id, name) => {
                self.translate_field(*lhs_id, *name, func);
            }

            Expr::ArrayIndex(arr_id, idx_id) => {
                self.translate_array_index(*arr_id, *idx_id, func);
            }

            Expr::ArrayLiteral(elements) => {
                let elements = elements.clone();
                self.translate_array_literal(&elements, expr, func);
            }

            Expr::Array(value_expr, _size_expr) => {
                let value_expr = *value_expr;
                let ty = self.expr_type(expr);
                let size = ty.size(self.decls) as u32;
                let mem_slot = self.alloc_memory(size);

                if let Type::Array(elem_ty, sz) = &*ty {
                    let count = sz.known();
                    let elem_size = elem_ty.size(self.decls);
                    let elem_ty = *elem_ty;
                    // Translate fill value once.
                    self.translate_expr(value_expr, func);
                    self.emit_wrap_for_expected_slice(elem_ty, value_expr, func);
                    let val_local = self.alloc_scalar();
                    func.emit(StackOp::LocalSet(val_local));
                    for i in 0..count {
                        let offset = i * elem_size;
                        self.emit_dest_addr(mem_slot, &elem_ty, offset, func);
                        func.emit(StackOp::LocalGet(val_local));
                        self.emit_store_offset(&elem_ty, offset, func);
                    }
                }

                func.emit(StackOp::LocalAddr(mem_slot));
            }

            Expr::StructLit(struct_name, fields) => {
                let struct_name = *struct_name;
                let fields = fields.clone();
                self.translate_struct_lit(struct_name, &fields, expr, func);
            }

            Expr::Tuple(elements) => {
                let elements = elements.clone();
                self.translate_tuple(&elements, expr, func);
            }

            Expr::AsTy(expr_id, target_ty) => {
                self.translate_cast(*expr_id, *target_ty, func);
            }

            Expr::Lambda { .. } => self.translate_lambda(expr, func),

            Expr::Assume(_) => {
                // No-op: assume is only used by the safety checker.
                func.emit(StackOp::I64Const(0));
            }

            Expr::Arena(inner) => {
                self.translate_expr(*inner, func);
            }

            Expr::TypeApp(_, _) | Expr::Macro(_, _) | Expr::Error => {
                unreachable!("unresolved expression in specialized body")
            }
        }
    }

    /// Translate an identifier reference.
    fn translate_id(&mut self, reference: &Reference, expr: ExprID, func: &mut StackFunction) {
        let ty = self.expr_type(expr);
        match reference {
            Reference::Local(local) => {
                if let Some(&addr_local) = self.captured_slots.get(local) {
                    func.emit(StackOp::LocalGet(addr_local));
                    if !self.is_ptr_type(&ty) {
                        self.emit_load(&ty, func);
                    }
                    return;
                }
                match self.variables[local] {
                    LocalKind::Scalar(slot) => self.emit_local_get(&ty, slot, func),
                    LocalKind::Reference(slot) => {
                        func.emit(StackOp::LocalGet(slot));
                        if !self.is_ptr_type(&ty) {
                            self.emit_load(&ty, func);
                        }
                    }
                    LocalKind::Memory(slot) => {
                        func.emit(StackOp::LocalAddr(slot));
                        if !self.is_ptr_type(&ty) {
                            self.emit_load(&ty, func);
                        }
                    }
                }
            }
            Reference::Instance(instance) => {
                if let Some(&offset) = self.globals.get(instance) {
                    func.emit(StackOp::GlobalAddr(offset));
                    if !self.is_ptr_type(&ty) {
                        self.emit_load(&ty, func);
                    }
                    return;
                }
                assert!(
                    self.decls.function_instance(*instance).is_some(),
                    "reference must name storage or a function"
                );
                let mem_slot = self.alloc_memory(16);
                func.emit(StackOp::LocalAddr(mem_slot));
                let instr_idx = func.pos();
                func.emit(StackOp::I64Const(0));
                self.pending_functions.push(*instance);
                self.func_load_patches.push(CallToPatch {
                    instr_idx,
                    callee: *instance,
                });
                func.emit(StackOp::Store64);
                func.emit(StackOp::LocalAddr(mem_slot));
                func.emit(StackOp::I64Const(0));
                func.emit(StackOp::Store64Off(8));
                func.emit(StackOp::LocalAddr(mem_slot));
            }
            _ => unreachable!("non-concrete reference in specialized body"),
        }
    }

    /// The store-form vector op for an f32x4-producing expression, or
    /// `None` if this isn't an expression the f32x4 ops can compute
    /// straight into a caller-supplied destination.
    ///
    /// Callers pair this with [`Self::emit_f32x4_operands`]: emit the
    /// operands, push the destination address, then emit this op. That
    /// writes the result into the destination directly, skipping the
    /// temporary frame slot and the 16-byte copy the generic
    /// pointer-represented path would otherwise need.
    fn f32x4_store_op(&self, expr: ExprID) -> Option<StackOp> {
        if !matches!(&*self.expr_type(expr), Type::Float32x4) {
            return None;
        }
        match &self.decl.arena[expr] {
            Expr::Binop(op, lhs_id, _) => {
                if !matches!(&*self.expr_type(*lhs_id), Type::Float32x4) {
                    return None;
                }
                match op {
                    Binop::Plus => Some(StackOp::F32x4AddStore),
                    Binop::Minus => Some(StackOp::F32x4SubStore),
                    Binop::Mult => Some(StackOp::F32x4MulStore),
                    Binop::Div => Some(StackOp::F32x4DivStore),
                    _ => None,
                }
            }
            Expr::Unop(Unop::Neg, arg_id) => {
                if matches!(&*self.expr_type(*arg_id), Type::Float32x4) {
                    Some(StackOp::F32x4NegStore)
                } else {
                    None
                }
            }
            Expr::Call(fn_id, arg_ids) => {
                if self.holds_fat_pointer(*fn_id) {
                    return None;
                }
                let Some(Reference::Instance(instance)) = self.decl.arena.reference(*fn_id) else {
                    return None;
                };
                let name = self.decls.instance_name(*instance);
                match (name.as_str(), arg_ids.len()) {
                    ("f32x4", 4) => Some(StackOp::F32x4BuildStore),
                    ("f32x4_splat", 1) => Some(StackOp::F32x4SplatStore),
                    _ => None,
                }
            }
            _ => None,
        }
    }

    /// True if `expr` is an f32x4 computation the three-address vector ops
    /// can evaluate entirely between frame slots: a memory-backed local, or
    /// an arithmetic node whose operands are themselves in that shape.
    ///
    /// Everything admitted here is a pure read of a frame slot, so the
    /// operands can be evaluated in any order and a destination that
    /// aliases an operand is safe — the final op is the only write.
    fn f32x4_slot_form(&self, expr: ExprID) -> bool {
        if !matches!(&*self.expr_type(expr), Type::Float32x4) {
            return false;
        }
        if self.get_memory_slot(expr).is_some() {
            return true;
        }
        match &self.decl.arena[expr] {
            Expr::Binop(op, lhs_id, rhs_id) => {
                matches!(op, Binop::Plus | Binop::Minus | Binop::Mult | Binop::Div)
                    && self.f32x4_slot_form(*lhs_id)
                    && self.f32x4_slot_form(*rhs_id)
            }
            Expr::Unop(Unop::Neg, arg_id) => self.f32x4_slot_form(*arg_id),
            _ => false,
        }
    }

    /// The operands of `expr` if it is an f32x4 multiplication.
    fn f32x4_mul_operands(&self, expr: ExprID) -> Option<(ExprID, ExprID)> {
        match &self.decl.arena[expr] {
            Expr::Binop(Binop::Mult, lhs_id, rhs_id)
                if matches!(&*self.expr_type(expr), Type::Float32x4) =>
            {
                Some((*lhs_id, *rhs_id))
            }
            _ => None,
        }
    }

    /// The frame slot holding `expr`, computing it into a fresh temp slot
    /// first when it isn't already a memory-backed local. Only valid when
    /// [`Self::f32x4_slot_form`] holds.
    fn f32x4_operand_slot(&mut self, expr: ExprID, func: &mut StackFunction) -> u16 {
        if let Some(slot) = self.get_memory_slot(expr) {
            return slot;
        }
        let tmp = self.alloc_memory(16);
        self.emit_f32x4_into_slot(expr, tmp, func);
        tmp
    }

    /// Emit `expr` computed into 16-byte frame slot `dst` using the
    /// three-address vector ops. Only valid when [`Self::f32x4_slot_form`]
    /// holds for `expr`.
    fn emit_f32x4_into_slot(&mut self, expr: ExprID, dst: u16, func: &mut StackFunction) {
        // A bare local: the caller wanted the value in `dst`, so copy it.
        if let Some(src) = self.get_memory_slot(expr) {
            if src != dst {
                func.emit(StackOp::LocalAddr(dst));
                func.emit(StackOp::LocalAddr(src));
                func.emit(StackOp::MemCopy(16));
            }
            return;
        }
        match &self.decl.arena[expr] {
            Expr::Binop(op, lhs_id, rhs_id) => {
                let (op, lhs_id, rhs_id) = (*op, *lhs_id, *rhs_id);
                // `a * b + c`, `c + a * b` and `a * b - c` each collapse to
                // one multiply-accumulate.
                let mul_add = match op {
                    Binop::Plus => self
                        .f32x4_mul_operands(lhs_id)
                        .map(|(a, b)| (a, b, rhs_id, false))
                        .or_else(|| {
                            self.f32x4_mul_operands(rhs_id)
                                .map(|(a, b)| (a, b, lhs_id, false))
                        }),
                    Binop::Minus => self
                        .f32x4_mul_operands(lhs_id)
                        .map(|(a, b)| (a, b, rhs_id, true)),
                    _ => None,
                };
                if let Some((a_id, b_id, c_id, is_sub)) = mul_add {
                    let a = self.f32x4_operand_slot(a_id, func);
                    let b = self.f32x4_operand_slot(b_id, func);
                    let c = self.f32x4_operand_slot(c_id, func);
                    func.emit(if is_sub {
                        StackOp::F32x4MulSubSet(a, b, c, dst)
                    } else {
                        StackOp::F32x4MulAddSet(a, b, c, dst)
                    });
                    return;
                }
                let a = self.f32x4_operand_slot(lhs_id, func);
                let b = self.f32x4_operand_slot(rhs_id, func);
                func.emit(match op {
                    Binop::Plus => StackOp::F32x4Add3(a, b, dst),
                    Binop::Minus => StackOp::F32x4Sub3(a, b, dst),
                    Binop::Mult => StackOp::F32x4Mul3(a, b, dst),
                    Binop::Div => StackOp::F32x4Div3(a, b, dst),
                    _ => unreachable!("f32x4_slot_form admitted a non-arithmetic binop"),
                });
            }
            Expr::Unop(Unop::Neg, arg_id) => {
                let arg_id = *arg_id;
                let a = self.f32x4_operand_slot(arg_id, func);
                func.emit(StackOp::F32x4Neg2(a, dst));
            }
            _ => unreachable!("emit_f32x4_into_slot on a non-slot-form expression"),
        }
    }

    /// Push the operands of an expression [`Self::f32x4_store_op`]
    /// accepted, leaving the destination address to the caller.
    fn emit_f32x4_operands(&mut self, expr: ExprID, func: &mut StackFunction) {
        match &self.decl.arena[expr] {
            Expr::Binop(_, lhs_id, rhs_id) => {
                let (lhs_id, rhs_id) = (*lhs_id, *rhs_id);
                self.translate_expr(lhs_id, func);
                self.translate_expr(rhs_id, func);
            }
            Expr::Unop(_, arg_id) => {
                let arg_id = *arg_id;
                self.translate_expr(arg_id, func);
            }
            Expr::Call(_, arg_ids) => {
                for arg_id in arg_ids.clone() {
                    self.translate_expr(arg_id, func);
                }
            }
            _ => unreachable!("emit_f32x4_operands on a non-vector expression"),
        }
    }

    /// Translate a binary operation.
    fn translate_binop(
        &mut self,
        op: Binop,
        lhs_id: ExprID,
        rhs_id: ExprID,
        _expr: ExprID,
        func: &mut StackFunction,
    ) {
        // Handle assignment specially.
        if op == Binop::Assign {
            self.translate_assign(lhs_id, rhs_id, func);
            return;
        }

        let ty = self.expr_type(lhs_id);

        // f32x4 SIMD ops — one vector instruction, result in a fresh
        // 16-byte frame slot whose address is left on the stack.
        if matches!(&*ty, Type::Float32x4) {
            self.translate_expr(lhs_id, func);
            self.translate_expr(rhs_id, func);
            let mem_slot = self.alloc_memory(16);
            func.emit(match op {
                Binop::Plus => StackOp::F32x4Add(mem_slot),
                Binop::Minus => StackOp::F32x4Sub(mem_slot),
                Binop::Mult => StackOp::F32x4Mul(mem_slot),
                Binop::Div => StackOp::F32x4Div(mem_slot),
                _ => panic!("unsupported f32x4 binop: {:?}", op),
            });
            return;
        }

        // Normal binary operation: post-order traversal.
        self.translate_expr(lhs_id, func);
        self.translate_expr(rhs_id, func);

        match op {
            Binop::Plus => match &*ty {
                Type::Float32 => func.emit(StackOp::FAddF),
                Type::Float64 => func.emit(StackOp::DAddD),
                _ => func.emit(StackOp::IAdd),
            },
            Binop::Minus => match &*ty {
                Type::Float32 => func.emit(StackOp::FSubF),
                Type::Float64 => func.emit(StackOp::DSubD),
                _ => func.emit(StackOp::ISub),
            },
            Binop::Mult => match &*ty {
                Type::Float32 => func.emit(StackOp::FMulF),
                Type::Float64 => func.emit(StackOp::DMulD),
                _ => func.emit(StackOp::IMul),
            },
            Binop::Div => match &*ty {
                Type::Float32 => func.emit(StackOp::FDivF),
                Type::Float64 => func.emit(StackOp::DDivD),
                Type::UInt32 | Type::UInt8 => func.emit(StackOp::UDiv),
                _ => func.emit(StackOp::IDiv),
            },
            // Float `%` is lowered to a call to the stdlib's `__mod` before
            // codegen, so only integer operands reach here.
            Binop::Mod => match &*ty {
                Type::Float32 | Type::Float64 | Type::Float32x4 => {
                    unreachable!("type {:?} not supported for modulo", ty)
                }
                _ => func.emit(StackOp::IRem),
            },
            Binop::Pow => match &*ty {
                Type::Float32 => func.emit(StackOp::FPowF),
                Type::Float64 => func.emit(StackOp::DPowD),
                _ => func.emit(StackOp::IPow),
            },
            Binop::Equal => match &*ty {
                Type::Float32 => func.emit(StackOp::FEqF),
                Type::Float64 => func.emit(StackOp::DEqD),
                Type::Name(_, _) | Type::Tuple(_) | Type::Array(_, _) => {
                    let size = ty.size(self.decls) as u32;
                    func.emit(StackOp::MemEq(size));
                }
                Type::Slice(elem) => {
                    let elem_size = elem.size(self.decls) as u32;
                    func.emit(StackOp::SliceEq(elem_size));
                }
                _ => func.emit(StackOp::IEq),
            },
            Binop::NotEqual => match &*ty {
                Type::Float32 => func.emit(StackOp::FNeF),
                Type::Float64 => func.emit(StackOp::DNeD),
                Type::Name(_, _) | Type::Tuple(_) | Type::Array(_, _) => {
                    let size = ty.size(self.decls) as u32;
                    func.emit(StackOp::MemNe(size));
                }
                Type::Slice(elem) => {
                    let elem_size = elem.size(self.decls) as u32;
                    func.emit(StackOp::SliceNe(elem_size));
                }
                _ => func.emit(StackOp::INe),
            },
            Binop::Less => match &*ty {
                Type::Float32 => func.emit(StackOp::FLtF),
                Type::Float64 => func.emit(StackOp::DLtD),
                Type::UInt32 | Type::UInt8 => func.emit(StackOp::ULt),
                _ => func.emit(StackOp::ILt),
            },
            Binop::Greater => match &*ty {
                Type::Float32 => func.emit(StackOp::FGtF),
                Type::Float64 => func.emit(StackOp::DGtD),
                Type::UInt32 | Type::UInt8 => func.emit(StackOp::UGt),
                _ => func.emit(StackOp::IGt),
            },
            Binop::Leq => match &*ty {
                Type::Float32 => func.emit(StackOp::FLeF),
                Type::Float64 => func.emit(StackOp::DLeD),
                _ => func.emit(StackOp::ILe),
            },
            Binop::Geq => {
                match &*ty {
                    Type::Float32 => func.emit(StackOp::FGeF),
                    Type::Float64 => func.emit(StackOp::DGeD),
                    Type::UInt32 | Type::UInt8 => func.emit(StackOp::IGe), // unsigned uses signed
                    _ => func.emit(StackOp::IGe),
                }
            }
            Binop::And => func.emit(StackOp::And),
            Binop::Or => func.emit(StackOp::Or),
            Binop::Assign => unreachable!(),
        }
    }

    /// Translate an assignment expression.
    fn translate_assign(&mut self, lhs_id: ExprID, rhs_id: ExprID, func: &mut StackFunction) {
        let lhs_ty = self.representation_type(lhs_id);

        // Check for captured variable assignment (double indirection).
        if let Some(Reference::Local(name)) = self.decl.arena.reference(lhs_id) {
            let name = *name;
            if self.captured_vars.contains(&name) {
                self.translate_expr(rhs_id, func);
                let val_local = self.alloc_scalar();
                let addr_local = *self.captured_slots.get(&name).unwrap();
                if self.void_ctx {
                    self.emit_local_set(&lhs_ty, val_local, func);
                } else {
                    self.emit_local_tee(&lhs_ty, val_local, func);
                }
                // Stack: [value]. Need to store through captured pointer.
                // Push addr, then value, then store.
                func.emit(StackOp::LocalGet(addr_local)); // push captured addr
                self.emit_local_get(&lhs_ty, val_local, func); // push value
                self.emit_store_op(&lhs_ty, func);
                if !self.void_ctx {
                    self.emit_local_get(&lhs_ty, val_local, func); // result value
                }
                return;
            }
        }

        // Direct scalar local assignment.
        if let Some(Reference::Local(name)) = self.decl.arena.reference(lhs_id) {
            let name = *name;
            if let Some(&LocalKind::Scalar(slot)) = self.variables.get(&name) {
                // Try to emit a register-form `locals[slot] = a OP b` op
                // directly. Skips the stack trip through LocalGet+LocalGet+
                // <op>+LocalTee for common arithmetic patterns.
                if self.void_ctx && self.try_emit_binop_set(slot, rhs_id, func) {
                    return;
                }
                self.translate_expr(rhs_id, func);
                if self.void_ctx {
                    self.emit_local_set(&lhs_ty, slot, func);
                } else {
                    self.emit_local_tee(&lhs_ty, slot, func);
                }
                return;
            }
        }

        // Slice store: a[i] = rhs where a is a slice of 32-bit elements.
        if let Expr::ArrayIndex(arr_id, idx_id) = &self.decl.arena[lhs_id] {
            let arr_id = *arr_id;
            let idx_id = *idx_id;
            let arr_ty = self.representation_type(arr_id);
            let (elem_ty, is_slice) = match &*arr_ty {
                Type::Slice(elem) => (Some(*elem), true),
                Type::Array(elem, _) => (Some(*elem), false),
                _ => (None, false),
            };
            if let Some(elem_ty) = elem_ty {
                let elem_size = elem_ty.size(self.decls);
                let elem_is_f32 = matches!(&*elem_ty, Type::Float32);
                if !self.is_ptr_type(&elem_ty) && elem_size == 4 {
                    // Try fused version if arr and idx are simple locals.
                    if let (Some(arr_slot), Some(idx_local)) =
                        (self.get_memory_slot(arr_id), self.get_scalar_local(idx_id))
                    {
                        // Fused store: value is on TOS from translate_expr(rhs).
                        let store_op_int = if is_slice {
                            StackOp::FusedAddrGetSliceStore32(arr_slot, idx_local)
                        } else {
                            StackOp::FusedLocalArrayStore32(arr_slot, idx_local)
                        };
                        let store_op_float = if is_slice {
                            StackOp::FusedAddrGetSliceStore32F(arr_slot, idx_local)
                        } else {
                            StackOp::FusedLocalArrayStore32F(arr_slot, idx_local)
                        };
                        self.translate_expr(rhs_id, func);
                        if !self.void_ctx {
                            let val_local = self.alloc_scalar();
                            if elem_is_f32 {
                                func.emit(StackOp::LocalTeeF(val_local));
                                func.emit(store_op_float);
                                func.emit(StackOp::LocalGetF(val_local));
                            } else {
                                func.emit(StackOp::LocalTee(val_local));
                                func.emit(store_op_int);
                                func.emit(StackOp::LocalGet(val_local));
                            }
                        } else if elem_is_f32 {
                            func.emit(store_op_float);
                        } else {
                            func.emit(store_op_int);
                        }
                        return;
                    }
                    if is_slice {
                        // Fallback: generic slice store.
                        self.translate_expr(rhs_id, func);
                        let val_local = self.alloc_scalar();
                        if elem_is_f32 {
                            func.emit(StackOp::LocalSetF(val_local));
                        } else {
                            func.emit(StackOp::LocalSet(val_local));
                        }
                        self.translate_expr(arr_id, func); // push fat_ptr
                        self.translate_expr(idx_id, func); // push index
                        if elem_is_f32 {
                            func.emit(StackOp::LocalGetF(val_local));
                            func.emit(StackOp::SliceStore32F);
                        } else {
                            func.emit(StackOp::LocalGet(val_local));
                            func.emit(StackOp::SliceStore32);
                        }
                        if !self.void_ctx {
                            if elem_is_f32 {
                                func.emit(StackOp::LocalGetF(val_local));
                            } else {
                                func.emit(StackOp::LocalGet(val_local));
                            }
                        }
                        return;
                    }
                }
            }
        }

        // f32x4 assignment: compute the vector straight into the
        // destination, skipping the temp slot and the 16-byte copy the
        // generic path below would emit.
        if self.void_ctx {
            // Destination and operands all in frame slots: the three-address
            // ops compute between slots with nothing on the operand stack.
            if let Some(dst) = self.get_memory_slot(lhs_id) {
                if self.f32x4_slot_form(rhs_id) {
                    self.emit_f32x4_into_slot(rhs_id, dst, func);
                    return;
                }
            }
            if let Some(store_op) = self.f32x4_store_op(rhs_id) {
                self.emit_f32x4_operands(rhs_id, func);
                self.translate_lvalue(lhs_id, func);
                func.emit(store_op);
                return;
            }
        }

        // General assignment: compute rhs, compute lvalue address, store.
        // Optimization: if RHS is a simple local variable, reuse it directly
        // instead of creating a temp (avoids get_set + get pattern).
        let needs_slice_wrap = self.needs_slice_wrap(lhs_ty, rhs_id);
        let val_local = if !needs_slice_wrap {
            if let Some(rhs_local) = self.get_scalar_local(rhs_id) {
                rhs_local
            } else {
                self.translate_expr(rhs_id, func);
                self.emit_wrap_for_expected_slice(lhs_ty, rhs_id, func);
                let tmp = self.alloc_scalar();
                self.emit_local_set(&lhs_ty, tmp, func);
                tmp
            }
        } else {
            self.translate_expr(rhs_id, func);
            self.emit_wrap_for_expected_slice(lhs_ty, rhs_id, func);
            let tmp = self.alloc_scalar();
            self.emit_local_set(&lhs_ty, tmp, func);
            tmp
        };

        self.translate_lvalue(lhs_id, func); // pushes address
        self.emit_local_get(&lhs_ty, val_local, func);

        // For Func type field assignment, only copy func_idx (8 bytes).
        if matches!(&*lhs_ty, Type::Func(_, _)) {
            if matches!(&self.decl.arena[lhs_id], Expr::Field(_, _)) {
                // rhs is a fat pointer address; load func_idx and store.
                func.emit(StackOp::Load64); // load func_idx from value (which is fat ptr addr)
                func.emit(StackOp::Store64);
                if !self.void_ctx {
                    func.emit(StackOp::LocalGet(val_local));
                }
                return;
            }
        }

        self.emit_store_op(&lhs_ty, func);
        if !self.void_ctx {
            self.emit_local_get(&lhs_ty, val_local, func);
        }
    }

    /// If this expr is an Id that resolves to a memory-backed local, return the slot index.
    fn get_memory_slot(&self, expr: ExprID) -> Option<u16> {
        if let Some(Reference::Local(name)) = self.decl.arena.reference(expr) {
            if let Some(LocalKind::Memory(slot)) = self.variables.get(name) {
                return Some(*slot);
            }
        }
        None
    }

    /// If this expr is an Id that resolves to a scalar local, return the local index.
    fn get_scalar_local(&self, expr: ExprID) -> Option<u16> {
        if let Some(Reference::Local(name)) = self.decl.arena.reference(expr) {
            if let Some(LocalKind::Scalar(local)) = self.variables.get(name) {
                return Some(*local);
            }
        }
        None
    }

    /// Try to emit `locals[dst_slot] = <binop>` as a single register-form
    /// op when the binop's operands are both simple scalar locals. Covers
    /// Plus/Minus/Mult/Div for f32 and Plus/Minus/Mult for i32. Returns
    /// true on success; the caller is responsible for emitting the naive
    /// `translate_expr(rhs_id) + LocalSet(dst_slot)` sequence otherwise.
    fn try_emit_binop_set(
        &mut self,
        dst_slot: u16,
        rhs_id: ExprID,
        func: &mut StackFunction,
    ) -> bool {
        let (op, lhs, rhs) = match &self.decl.arena[rhs_id] {
            Expr::Binop(op, lhs, rhs) => (*op, *lhs, *rhs),
            _ => return false,
        };
        let Some(a_slot) = self.get_scalar_local(lhs) else {
            return false;
        };
        let Some(b_slot) = self.get_scalar_local(rhs) else {
            return false;
        };
        let ty = self.expr_type(rhs_id);
        let fused = match (op, &*ty) {
            (Binop::Plus, Type::Float32) => StackOp::FusedGetGetFAddSet(a_slot, b_slot, dst_slot),
            (Binop::Minus, Type::Float32) => StackOp::FusedGetGetFSubSet(a_slot, b_slot, dst_slot),
            (Binop::Mult, Type::Float32) => StackOp::FusedGetGetFMulSet(a_slot, b_slot, dst_slot),
            (Binop::Div, Type::Float32) => StackOp::FusedGetGetFDivSet(a_slot, b_slot, dst_slot),
            (Binop::Plus, _) if !matches!(&*ty, Type::Float64) => {
                StackOp::FusedGetGetIAddSet(a_slot, b_slot, dst_slot)
            }
            (Binop::Minus, _) if !matches!(&*ty, Type::Float64) => {
                StackOp::FusedGetGetISubSet(a_slot, b_slot, dst_slot)
            }
            (Binop::Mult, _) if !matches!(&*ty, Type::Float64) => {
                StackOp::FusedGetGetIMulSet(a_slot, b_slot, dst_slot)
            }
            _ => return false,
        };
        func.emit(fused);
        true
    }

    /// Translate an lvalue expression. Pushes the address onto the stack.
    fn translate_lvalue(&mut self, expr: ExprID, func: &mut StackFunction) {
        match &self.decl.arena[expr].clone() {
            Expr::Id(_) => match self.decl.arena.reference(expr) {
                Some(Reference::Local(name)) => {
                    let name = *name;
                    if let Some(&kind) = self.variables.get(&name) {
                        match kind {
                            LocalKind::Scalar(slot) => {
                                let ty = self.expr_type(expr);
                                if self.is_ptr_type(&ty) {
                                    func.emit(StackOp::LocalGet(slot));
                                } else {
                                    self.emit_var_address(&name, func);
                                }
                            }
                            LocalKind::Reference(slot) => {
                                func.emit(StackOp::LocalGet(slot));
                            }
                            LocalKind::Memory(slot) => {
                                func.emit(StackOp::LocalAddr(slot));
                            }
                        }
                    } else {
                        unreachable!("checked local must have storage");
                    }
                }
                Some(Reference::Instance(instance)) => {
                    func.emit(StackOp::GlobalAddr(self.globals[instance]));
                }
                reference => unreachable!("unresolved checked reference: {:?}", reference),
            },

            Expr::Field(lhs_id, name) => {
                let lhs_id = *lhs_id;
                let name = *name;
                self.translate_lvalue(lhs_id, func);
                let lhs_ty = self.expr_type(lhs_id);

                if matches!(&*lhs_ty, Type::Float32x4) {
                    let s: &str = &name;
                    let offset: i32 = match s {
                        "x" | "r" => 0,
                        "y" | "g" => 4,
                        "z" | "b" => 8,
                        "w" | "a" => 12,
                        _ => panic!("invalid f32x4 field: {}", name),
                    };
                    func.emit(StackOp::IAddImm(offset));
                    return;
                }

                if let Type::Name(struct_name, type_args) = &*lhs_ty {
                    let struct_decl = self.decls.find(*struct_name);
                    if let Decl::Struct(s) = &struct_decl[0] {
                        let inst: crate::Instance = s
                            .typevars
                            .iter()
                            .zip(type_args.iter())
                            .map(|(tv, ty)| (mk_type(Type::Var(*tv)), *ty))
                            .collect();
                        let offset = s.field_offset(&name, self.decls, &inst);
                        func.emit(StackOp::IAddImm(offset));
                    }
                }
            }

            Expr::ArrayIndex(arr_id, idx_id) => {
                let arr_id = *arr_id;
                let idx_id = *idx_id;
                self.translate_lvalue(arr_id, func);
                let arr_ty = self.representation_type(arr_id);

                // f32x4 lane store: the lane address is base + idx * 4.
                // The lane index is in 0..4: the safety checker proves it.
                if matches!(&*arr_ty, Type::Float32x4) {
                    self.translate_expr(idx_id, func);
                    func.emit(StackOp::I64Const(4));
                    func.emit(StackOp::IMul);
                    func.emit(StackOp::IAdd);
                    return;
                }

                let (elem_ty, is_slice) = match &*arr_ty {
                    Type::Array(elem_ty, _) => (*elem_ty, false),
                    Type::Slice(elem_ty) => (*elem_ty, true),
                    _ => return,
                };

                // For slices, load the data pointer from the fat pointer.
                if is_slice {
                    func.emit(StackOp::Load64); // load data_ptr
                }

                let elem_size = elem_ty.size(self.decls);
                // Compute offset: idx * elem_size
                self.translate_expr(idx_id, func);
                func.emit(StackOp::I64Const(elem_size as i64));
                func.emit(StackOp::IMul);
                func.emit(StackOp::IAdd); // base + offset
            }

            _ => {
                // For other expressions, translate normally (result should be an address).
                self.translate_expr(expr, func);
            }
        }
    }

    /// Translate a unary operation.
    fn translate_unop(&mut self, op: Unop, arg_id: ExprID, func: &mut StackFunction) {
        let ty = self.expr_type(arg_id);

        // f32x4 negation — one vector instruction.
        if op == Unop::Neg && matches!(&*ty, Type::Float32x4) {
            self.translate_expr(arg_id, func);
            let mem_slot = self.alloc_memory(16);
            func.emit(StackOp::F32x4Neg(mem_slot));
            return;
        }

        self.translate_expr(arg_id, func);

        match op {
            Unop::Neg => match &*ty {
                Type::Float32 => func.emit(StackOp::FNegF),
                Type::Float64 => func.emit(StackOp::DNegD),
                _ => func.emit(StackOp::INeg),
            },
            Unop::Not => {
                func.emit(StackOp::Not);
                // Mask to 1 bit for boolean.
                func.emit(StackOp::I64Const(1));
                func.emit(StackOp::And);
            }
        }
    }

    /// Translate a function call.
    fn translate_call(
        &mut self,
        fn_id: ExprID,
        arg_ids: &[ExprID],
        call_expr: ExprID,
        func: &mut StackFunction,
    ) {
        if self.holds_fat_pointer(fn_id) {
            self.translate_closure_call(fn_id, arg_ids, call_expr, func);
            return;
        }

        // Check for builtin functions.
        if let Some(Reference::Instance(instance)) = self.decl.arena.reference(fn_id) {
            let instance = *instance;
            let name = self.decls.instance_name(instance);

            if *name == "print" {
                if let Some(&arg_id) = arg_ids.first() {
                    self.translate_expr(arg_id, func);
                    let ty = self.expr_type(arg_id);
                    match &*ty {
                        Type::Float32 => func.emit(StackOp::PrintF32F),
                        Type::Float64 => func.emit(StackOp::PrintF64D),
                        _ => func.emit(StackOp::PrintI32),
                    }
                }
                func.emit(StackOp::I64Const(0));
                return;
            }

            if *name == "assert" {
                if let Some(&arg_id) = arg_ids.first() {
                    self.translate_expr(arg_id, func);
                    func.emit(StackOp::Assert);
                }
                func.emit(StackOp::I64Const(0));
                return;
            }

            if *name == "putc" {
                if let Some(&arg_id) = arg_ids.first() {
                    self.translate_expr(arg_id, func);
                    func.emit(StackOp::Putc);
                }
                func.emit(StackOp::I64Const(0));
                return;
            }

            // f32x4 constructor. The four lanes are pushed onto the float
            // window in order, and one op packs them into a frame slot.
            if *name == "f32x4" && arg_ids.len() == 4 {
                for arg_id in arg_ids.iter() {
                    self.translate_expr(*arg_id, func);
                }
                let mem_slot = self.alloc_memory(16);
                func.emit(StackOp::F32x4Build(mem_slot));
                return;
            }

            // f32x4_splat. Arg is an f32 in the float window.
            if *name == "f32x4_splat" && arg_ids.len() == 1 {
                self.translate_expr(arg_ids[0], func);
                let mem_slot = self.alloc_memory(16);
                func.emit(StackOp::F32x4Splat(mem_slot));
                return;
            }

            // Unary math builtins (f32) — F-window ops only.
            let unary_math_f32: &[(&str, StackOp)] = &[
                ("sin$f32", StackOp::SinF32F),
                ("cos$f32", StackOp::CosF32F),
                ("tan$f32", StackOp::TanF32F),
                ("asin$f32", StackOp::AsinF32F),
                ("acos$f32", StackOp::AcosF32F),
                ("atan$f32", StackOp::AtanF32F),
                ("sinh$f32", StackOp::SinhF32F),
                ("cosh$f32", StackOp::CoshF32F),
                ("tanh$f32", StackOp::TanhF32F),
                ("asinh$f32", StackOp::AsinhF32F),
                ("acosh$f32", StackOp::AcoshF32F),
                ("atanh$f32", StackOp::AtanhF32F),
                ("ln$f32", StackOp::LnF32F),
                ("exp$f32", StackOp::ExpF32F),
                ("exp2$f32", StackOp::Exp2F32F),
                ("log10$f32", StackOp::Log10F32F),
                ("log2$f32", StackOp::Log2F32F),
                ("sqrt$f32", StackOp::SqrtF32F),
                ("abs$f32", StackOp::AbsF32F),
                ("floor$f32", StackOp::FloorF32F),
                ("ceil$f32", StackOp::CeilF32F),
                ("isnan$f32", StackOp::IsnanF32F),
                ("isinf$f32", StackOp::IsinfF32F),
            ];
            let unary_math_f64: &[(&str, StackOp)] = &[
                ("sin$f64", StackOp::SinF64D),
                ("cos$f64", StackOp::CosF64D),
                ("tan$f64", StackOp::TanF64D),
                ("asin$f64", StackOp::AsinF64D),
                ("acos$f64", StackOp::AcosF64D),
                ("atan$f64", StackOp::AtanF64D),
                ("sinh$f64", StackOp::SinhF64D),
                ("cosh$f64", StackOp::CoshF64D),
                ("tanh$f64", StackOp::TanhF64D),
                ("asinh$f64", StackOp::AsinhF64D),
                ("acosh$f64", StackOp::AcoshF64D),
                ("atanh$f64", StackOp::AtanhF64D),
                ("ln$f64", StackOp::LnF64D),
                ("exp$f64", StackOp::ExpF64D),
                ("exp2$f64", StackOp::Exp2F64D),
                ("log10$f64", StackOp::Log10F64D),
                ("log2$f64", StackOp::Log2F64D),
                ("sqrt$f64", StackOp::SqrtF64D),
                ("abs$f64", StackOp::AbsF64D),
                ("floor$f64", StackOp::FloorF64D),
                ("ceil$f64", StackOp::CeilF64D),
                ("isnan$f64", StackOp::IsnanF64D),
                ("isinf$f64", StackOp::IsinfF64D),
            ];
            for (n, op) in unary_math_f32.iter() {
                if *name == *n {
                    self.translate_expr(arg_ids[0], func);
                    func.emit(op.clone());
                    return;
                }
            }
            for (n, op) in unary_math_f64.iter() {
                if *name == *n {
                    self.translate_expr(arg_ids[0], func);
                    func.emit(op.clone());
                    return;
                }
            }

            // Binary math builtins.
            if *name == "atan2$f32$f32" {
                self.translate_expr(arg_ids[0], func);
                self.translate_expr(arg_ids[1], func);
                func.emit(StackOp::Atan2F32F);
                return;
            }
            if *name == "atan2$f64$f64" {
                self.translate_expr(arg_ids[0], func);
                self.translate_expr(arg_ids[1], func);
                func.emit(StackOp::Atan2F64D);
                return;
            }

            // pow builtins: map to FPow/DPow.
            if *name == "pow$f32$f32" {
                self.translate_expr(arg_ids[0], func);
                self.translate_expr(arg_ids[1], func);
                func.emit(StackOp::FPowF);
                return;
            }
            if *name == "pow$f64$f64" {
                self.translate_expr(arg_ids[0], func);
                self.translate_expr(arg_ids[1], func);
                func.emit(StackOp::DPowD);
                return;
            }

            // min/max builtins: emit comparison + select.
            if *name == "min$f32$f32"
                || *name == "max$f32$f32"
                || *name == "min$f64$f64"
                || *name == "max$f64$f64"
            {
                let is_f64 = name.contains("f64");
                let is_min = name.contains("min");
                // Local set/get for the a/b temps: f32 goes through the
                // f32 lives in the float window (LocalSetF/LocalGetF),
                // f64 in the double window (LocalSetD/LocalGetD).
                let local_set = |slot: u16| {
                    if is_f64 {
                        StackOp::LocalSetD(slot)
                    } else {
                        StackOp::LocalSetF(slot)
                    }
                };
                let local_get = |slot: u16| {
                    if is_f64 {
                        StackOp::LocalGetD(slot)
                    } else {
                        StackOp::LocalGetF(slot)
                    }
                };
                let cmp_lt = if is_f64 { StackOp::DLtD } else { StackOp::FLtF };
                self.translate_expr(arg_ids[0], func);
                let a_local = self.alloc_scalar();
                func.emit(local_set(a_local));
                self.translate_expr(arg_ids[1], func);
                let b_local = self.alloc_scalar();
                func.emit(local_set(b_local));
                // Emit: if a < b then a else b (for min), if a > b then a else b (for max).
                // For max we flip the operand order on the comparison:
                // push (a, b) for min and (b, a) for max.
                let (first, second) = if is_min {
                    (a_local, b_local)
                } else {
                    (b_local, a_local)
                };
                func.emit(local_get(first));
                func.emit(local_get(second));
                func.emit(cmp_lt);
                let jump_if_false = func.pos();
                func.emit(StackOp::JumpIfZero(0));
                func.emit(local_get(a_local));
                let jump_end = func.pos();
                func.emit(StackOp::Jump(0));
                func.patch_jump(jump_if_false);
                func.emit(local_get(b_local));
                func.patch_jump(jump_end);
                return;
            }

            // Extern function calls.
            {
                if let Some(f) = self.decls.function_instance(instance) {
                    if f.is_extern {
                        let globals_offset = *self
                            .globals
                            .get(&instance)
                            .expect("extern function not in globals");

                        // For extern calls, push C-level args. Slices expand
                        // to (data_ptr, i32 len) at the host boundary.
                        let mut c_arg_count: u8 = 0;
                        for (i, arg_id) in arg_ids.iter().enumerate() {
                            let arg = *arg_id;
                            let param_ty = f.arena.local(f.params[i].local).ty;
                            if matches!(&*param_ty, Type::Slice(_)) {
                                self.translate_expr(arg, func);
                                match &*self.representation_type(arg) {
                                    Type::Slice(_) => {
                                        let slice_local = self.alloc_scalar();
                                        func.emit(StackOp::LocalSet(slice_local));
                                        func.emit(StackOp::LocalGet(slice_local));
                                        func.emit(StackOp::Load64);
                                        func.emit(StackOp::LocalGet(slice_local));
                                        func.emit(StackOp::Load32Off(8));
                                    }
                                    Type::Array(_, sz) => {
                                        func.emit(StackOp::I64Const(sz.known() as i64));
                                    }
                                    actual_ty => panic!(
                                        "stack extern call: expected slice or array, got {:?}",
                                        actual_ty
                                    ),
                                }
                                c_arg_count += 2;
                            } else {
                                if matches!(&*param_ty, Type::Reference(_)) {
                                    self.translate_lvalue(arg, func);
                                } else {
                                    self.translate_expr(arg, func);
                                }
                                let arg_ty = self.expr_type(arg);
                                if matches!(&*arg_ty, Type::Float32) {
                                    func.emit(StackOp::FToBitsF);
                                } else if matches!(&*arg_ty, Type::Float64) {
                                    func.emit(StackOp::DToBitsD);
                                }
                                c_arg_count += 1;
                            }
                        }
                        assert!(
                            c_arg_count <= 8,
                            "stack extern call has too many C-level arguments"
                        );
                        let ret_ty = self.expr_type(call_expr);
                        func.emit(StackOp::CallExtern {
                            globals_offset,
                            args: c_arg_count,
                            ret: stack_extern_ret_type(ret_ty),
                        });
                        // Bridge the return value back to the window the
                        // surrounding codegen expects. The extern handler
                        // pushes a zero placeholder for void returns so
                        // translate_call's +1 invariant still holds.
                        if matches!(&*ret_ty, Type::Float32) {
                            func.emit(StackOp::BitsToFF);
                        } else if matches!(&*ret_ty, Type::Float64) {
                            func.emit(StackOp::BitsToDD);
                        }
                        return;
                    }
                }
            }

            // Regular function call.
            let ret_ty = self.expr_type(call_expr);
            let returns_ptr = returns_via_pointer(ret_ty);
            let returns_void = matches!(&*ret_ty, Type::Void);

            // If returning a pointer type, allocate output storage.
            let output_slot = if returns_ptr {
                let size = ret_ty.size(self.decls) as u32;
                Some(self.alloc_memory(size))
            } else {
                None
            };

            // Get callee param types for slice coercion.
            let param_types: Vec<TypeID> = {
                if let Some(f) = self.decls.function_instance(instance) {
                    f.param_types()
                } else {
                    vec![]
                }
            };

            // Push output pointer as first arg if sret.
            if let Some(slot) = output_slot {
                func.emit(StackOp::LocalAddr(slot));
            }

            // Push arguments. op_call copies args from the int TOS window
            // into the callee's locals, so f32 args that rode through the
            // float window have to be bridged back to their bit pattern.
            for (i, arg_id) in arg_ids.iter().enumerate() {
                let param_ty = param_types.get(i).copied();
                if param_ty.is_some_and(|t| matches!(&*t, Type::Reference(_))) {
                    self.translate_lvalue(*arg_id, func);
                } else {
                    self.translate_expr(*arg_id, func);
                    let arg_ty = self.expr_type(*arg_id);
                    if matches!(&*arg_ty, Type::Float32) {
                        func.emit(StackOp::FToBitsF);
                    } else if matches!(&*arg_ty, Type::Float64) {
                        func.emit(StackOp::DToBitsD);
                    }
                }
                if param_ty.is_some_and(|t| matches!(&*t, Type::Slice(_))) {
                    let actual_ty = self.representation_type(*arg_id);
                    self.emit_wrap_as_slice(actual_ty, func);
                }
            }

            // Emit call.
            let arg_count = if output_slot.is_some() {
                arg_ids.len() as u8 + 1
            } else {
                arg_ids.len() as u8
            };

            self.pending_functions.push(instance);
            let instr_idx = func.pos();
            func.emit(StackOp::Call {
                func: 0,
                args: arg_count,
                preserve: 0, // patched post-codegen from static depth
            });
            self.calls_to_patch.push(CallToPatch {
                instr_idx,
                callee: instance,
            });

            // If sret, push the output address as the result.
            if let Some(slot) = output_slot {
                func.emit(StackOp::LocalAddr(slot));
            } else if returns_void {
                // Void calls leave no return value on the operand stack,
                // but translate_call must push exactly one value (Block and
                // other wrappers expect it). Push a placeholder zero; void
                // contexts drop it via translate_void's normal Drop path.
                func.emit(StackOp::I64Const(0));
            } else if matches!(&*ret_ty, Type::Float32) {
                // f32 return values come back through t0 (int window).
                // Bridge into the float window so the surrounding codegen
                // can consume them as f32 directly.
                func.emit(StackOp::BitsToFF);
            } else if matches!(&*ret_ty, Type::Float64) {
                // Same for f64: bridge t0 bits into the double window.
                func.emit(StackOp::BitsToDD);
            }
            // Otherwise the call already pushed its return value.

            return;
        }

        // Indirect call via expression.
        self.translate_closure_call(fn_id, arg_ids, call_expr, func);
    }

    /// Translate a call through a fat pointer (lambda, closure or function
    /// pointer). Mirrors the direct-call ABI: an sret output pointer is passed
    /// as the first argument, `Reference` params are passed by address, and
    /// sized arrays are wrapped as slices where the callee expects one.
    fn translate_closure_call(
        &mut self,
        fn_id: ExprID,
        arg_ids: &[ExprID],
        call_expr: ExprID,
        func: &mut StackFunction,
    ) {
        let ret_ty = self.expr_type(call_expr);
        let output_slot = if returns_via_pointer(ret_ty) {
            let size = ret_ty.size(self.decls) as u32;
            Some(self.alloc_memory(size))
        } else {
            None
        };

        // Push the output pointer as the first argument if sret.
        if let Some(slot) = output_slot {
            func.emit(StackOp::LocalAddr(slot));
        }

        // Push arguments, then the fat pointer address.
        self.push_closure_args(fn_id, arg_ids, func);
        self.translate_expr(fn_id, func); // pushes fat_ptr_addr

        let args = arg_ids.len() as u8 + u8::from(output_slot.is_some());
        func.emit(StackOp::CallClosure { args });

        if let Some(slot) = output_slot {
            // sret callees return void; the result is the output storage.
            func.emit(StackOp::LocalAddr(slot));
        } else {
            self.bridge_call_result(call_expr, func);
        }
    }

    /// True when the callee expression names storage holding a fat pointer
    /// {func_idx, closure_ptr} — a local variable or a function-typed global —
    /// rather than naming a function declaration. Such calls go through
    /// `translate_closure_call` instead of the direct-call path.
    fn holds_fat_pointer(&self, fn_id: ExprID) -> bool {
        match self.decl.arena.reference(fn_id) {
            Some(Reference::Local(_)) => true,
            Some(Reference::Instance(id)) => {
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

    /// Push the arguments for a `CallClosure`. Like `op_call`, `op_call_closure`
    /// copies args out of the int TOS window, so f32/f64 args that rode through
    /// the float window have to be bridged back to their bit pattern.
    fn push_closure_args(&mut self, fn_id: ExprID, arg_ids: &[ExprID], func: &mut StackFunction) {
        let param_types = self.closure_param_types(fn_id);
        for (i, arg_id) in arg_ids.iter().enumerate() {
            let param_ty = param_types.get(i).copied();
            if param_ty.is_some_and(|t| matches!(&*t, Type::Reference(_))) {
                self.translate_lvalue(*arg_id, func);
            } else {
                self.translate_expr(*arg_id, func);
                match &*self.expr_type(*arg_id) {
                    Type::Float32 => func.emit(StackOp::FToBitsF),
                    Type::Float64 => func.emit(StackOp::DToBitsD),
                    _ => {}
                }
            }
            if param_ty.is_some_and(|t| matches!(&*t, Type::Slice(_))) {
                let actual_ty = self.representation_type(*arg_id);
                self.emit_wrap_as_slice(actual_ty, func);
            }
        }
    }

    /// Fix up the result of a `CallClosure`. Void calls leave nothing on the
    /// operand stack, so push a placeholder to keep translate_call's +1
    /// invariant. Float returns come back through t0 (the int window) and must
    /// be bridged into the float/double window.
    fn bridge_call_result(&mut self, call_expr: ExprID, func: &mut StackFunction) {
        match &*self.expr_type(call_expr) {
            Type::Void => func.emit(StackOp::I64Const(0)),
            Type::Float32 => func.emit(StackOp::BitsToFF),
            Type::Float64 => func.emit(StackOp::BitsToDD),
            _ => {}
        }
    }

    /// Translate an if expression.
    fn translate_if(
        &mut self,
        cond_id: ExprID,
        then_id: ExprID,
        else_id: Option<ExprID>,
        func: &mut StackFunction,
    ) {
        self.translate_expr(cond_id, func);

        let saved_has_returned = self.has_returned;

        let jump_to_else = func.pos();
        func.emit(StackOp::JumpIfZero(0));

        // Then branch.
        self.translate_expr(then_id, func);
        let then_returned = self.has_returned;

        if let Some(else_expr_id) = else_id {
            let jump_to_end = func.pos();
            func.emit(StackOp::Jump(0));

            func.patch_jump(jump_to_else);

            // Else branch.
            self.has_returned = saved_has_returned;
            self.translate_expr(else_expr_id, func);
            let else_returned = self.has_returned;

            func.patch_jump(jump_to_end);

            self.has_returned = then_returned && else_returned;
        } else {
            func.patch_jump(jump_to_else);
            // No else: push 0 as default value when condition is false,
            // but then branch already pushed a value. We need the same
            // stack effect. The "then" value stays if taken; we need a
            // value if not taken. Use a jump-over pattern.
            // Actually, let's redo: we need to ensure both paths push one value.
            // Restructure:
            //   translate cond -> JumpIfZero(else_label)
            //   translate then -> Jump(end_label)
            //   else_label: push 0
            //   end_label:
            // Let me fix this properly:

            // We already emitted: cond, JumpIfZero, then_body
            // We need: if cond was false, skip then_body and push 0.
            // But we already patched jump_to_else to here. So at this point
            // we've already jumped past the then-body. Let's insert a
            // jump-over for the else default.

            // Actually simpler: re-emit with jump-over.
            // Let's just emit a zero after the then-body, and use
            // a different structure:
            //   cond, JumpIfZero(else), then, Jump(end), else: I64Const(0), end:
            let last_ops_len = func.ops.len();
            // We need to undo the patch and add the else path.
            // Since we already patched, let's add a Jump and else block.

            // Current state:
            //   ... cond JumpIfZero(here) then_body [we are here]
            // We need:
            //   ... cond JumpIfZero(else) then_body Jump(end) else: I64Const(0) end:

            // Re-patch: the JumpIfZero should point past the Jump we're about to emit.
            // But we already patched it to point here. Let's add a Jump(end) and I64Const(0).
            let jump_to_end2 = func.pos();
            func.emit(StackOp::Jump(0));
            // Re-patch the JumpIfZero to point here (after the Jump).
            func.patch_jump(jump_to_else);
            func.emit(StackOp::I64Const(0));
            func.patch_jump(jump_to_end2);

            self.has_returned = saved_has_returned;
            let _ = last_ops_len;
        }
    }

    /// Translate a while loop.
    fn translate_while(&mut self, cond_id: ExprID, body_id: ExprID, func: &mut StackFunction) {
        let loop_start = func.pos();

        self.loop_stack.push(LoopContext {
            continue_target: loop_start,
            continue_patches: Vec::new(),
            break_patches: Vec::new(),
        });

        // Evaluate condition.
        self.translate_expr(cond_id, func);

        let jump_to_end = func.pos();
        func.emit(StackOp::JumpIfZero(0));

        // Execute body in void context.
        self.translate_void(body_id, func);

        // Jump back to loop start.
        let pos = func.pos();
        func.emit(StackOp::Jump((loop_start as i32) - (pos as i32) - 1));

        // Patch jump to end and break jumps.
        func.patch_jump(jump_to_end);
        let ctx = self.loop_stack.pop().unwrap();
        for bp in ctx.break_patches {
            func.patch_jump(bp);
        }

        // Caller handles result push if needed.
    }

    /// Translate a for loop.
    fn translate_for(
        &mut self,
        expr: ExprID,
        var: LocalId,
        start_id: ExprID,
        end_id: ExprID,
        body_id: ExprID,
        func: &mut StackFunction,
    ) {
        // Initialize loop variable.
        self.translate_expr(start_id, func);
        let loop_var = self.alloc_scalar();
        func.emit(StackOp::LocalSet(loop_var));

        // Translate end expression and save it. Both bounds are outside the
        // loop variable's scope, so they still see any outer binding of the
        // same name.
        self.translate_expr(end_id, func);
        let end_local = self.alloc_scalar();
        func.emit(StackOp::LocalSet(end_local));

        // Bounds are evaluated once. The native path and scalar continuation
        // share the same counter/end slots and publication boundary.
        let kernel = self.try_emit_loop(expr, loop_var, end_local, func);

        // The checked loop binding has its own local identity.

        let int_ty = mk_type(Type::Int32);

        let counter_mem = if self.lambda_referenced.contains(&var) {
            // A lambda shares the counter by address, so it needs memory of
            // its own, allocated up front the way `let` and `var` do it.
            // Letting `emit_var_address` spill it lazily instead would put the
            // store at the capture site, which can sit on a conditionally-
            // executed path — iterations that don't reach it would then read
            // unwritten memory.
            let mem_slot = self.alloc_memory(self.vm_type_size(&int_ty));
            self.variables.insert(var, LocalKind::Memory(mem_slot));
            Some(mem_slot)
        } else {
            self.variables.insert(var, LocalKind::Scalar(loop_var));
            None
        };

        let loop_start = func.pos();

        // Check loop_var < end.
        func.emit(StackOp::LocalGet(loop_var));
        func.emit(StackOp::LocalGet(end_local));
        func.emit(StackOp::ILt);

        let jump_to_end = func.pos();
        func.emit(StackOp::JumpIfZero(0));

        // Push loop stack with placeholder continue target.
        self.loop_stack.push(LoopContext {
            continue_target: 0,
            continue_patches: Vec::new(),
            break_patches: Vec::new(),
        });

        // A counter that lives in memory is refreshed from the scalar local at
        // the top of every iteration. The loop variable is immutable, so
        // nothing ever writes back the other way.
        if let Some(mem_slot) = counter_mem {
            func.emit(StackOp::LocalAddr(mem_slot));
            self.emit_local_get(&int_ty, loop_var, func);
            self.emit_store_op(&int_ty, func);
        }

        // Execute body in void context.
        self.translate_void(body_id, func);

        // Increment position (continue target).
        let increment_pos = func.pos();

        // Increment loop variable.
        func.emit(StackOp::LocalGet(loop_var));
        func.emit(StackOp::IAddImm(1));
        func.emit(StackOp::LocalSet(loop_var));

        // Jump back to loop start.
        let pos = func.pos();
        func.emit(StackOp::Jump((loop_start as i32) - (pos as i32) - 1));

        // Patch jumps.
        func.patch_jump(jump_to_end);
        if let Some(index) = kernel {
            let done = func.pos() as i32 - index as i32 - 1;
            if let StackOp::NativeLoop(kernel) = &mut func.ops[index] {
                kernel.done = done;
            }
        }
        let ctx = self.loop_stack.pop().unwrap();
        for bp in ctx.break_patches {
            func.patch_jump(bp);
        }
        for cp in ctx.continue_patches {
            let jump_offset = (increment_pos as i32) - (cp as i32) - 1;
            if let StackOp::Jump(offset) = &mut func.ops[cp] {
                *offset = jump_offset;
            }
        }

        // Caller handles result push if needed.
    }

    /// Select a precompiled implementation from a loop's checked value graph.
    /// Unsupported value/effect shapes keep the existing loop lowering.
    fn try_emit_loop(
        &mut self,
        expr: ExprID,
        counter: u16,
        end: u16,
        func: &mut StackFunction,
    ) -> Option<usize> {
        use crate::value_loops::Scalar;
        let plan = self.loops.get(&expr)?.clone();
        // Contracts supply typed coefficient/state roles. Resolve every
        // scalar and span before emitting any native setup, then lower the
        // common stream domain independently of its arithmetic body.
        let (input, output, pending) = if let Some(map) = plan.pointwise() {
            // Only the recipe's active prefix is materialized. Padding names
            // slot zero but is never read by the generated implementation.
            let mut coefficients = std::array::from_fn(|_| LoopScalar::Local(0));
            for (slot, value) in coefficients.iter_mut().zip(map.coefficients) {
                *slot = self.loop_scalar(&plan, value)?;
            }
            (
                map.input,
                map.output,
                NativeStreamBody::Pointwise {
                    recipe: map.recipe,
                    coefficients,
                },
            )
        } else if let Some(pole) = plan.one_pole() {
            (
                pole.input,
                pole.output,
                NativeStreamBody::OnePole {
                    state: self.loop_scalar_local(plan.carries[pole.carry].binding)?,
                    feed: self.loop_scalar(&plan, pole.feed)?,
                    feedback: self.loop_scalar(&plan, pole.feedback)?,
                },
            )
        } else if let Some(biquad) = plan.biquad() {
            let [x1, x2, y1, y2] = biquad
                .carries
                .map(|carry| self.loop_scalar_local(plan.carries[carry].binding));
            let [b0, b1, b2, a1, a2] = biquad
                .coefficients
                .map(|value| self.loop_scalar(&plan, value));
            (
                biquad.input,
                biquad.output,
                NativeStreamBody::Biquad {
                    states: [x1?, x2?, y1?, y2?],
                    coefficients: [b0?, b1?, b2?, a1?, a2?],
                },
            )
        } else {
            return None;
        };
        let input_expr = plan.streams[input].expression;
        let output_expr = plan.streams[output].expression;
        let input_length = self.loop_span_length(input_expr)?;
        let output_length = self.loop_span_length(output_expr)?;
        let body = pending.map_coefficients(|value| self.materialize_loop_scalar(value, func));
        let (input_slot, input_len) = self.materialize_loop_span(input_expr, input_length, func);
        let (output_slot, output_len) = if input == output {
            (input_slot, input_len)
        } else {
            self.materialize_loop_span(output_expr, output_length, func)
        };
        let op = StackOp::NativeLoop(NativeLoopKernel {
            scalar: match plan.ty {
                Scalar::F32 => NativeScalar::F32,
                Scalar::F64 => NativeScalar::F64,
            },
            counter,
            end,
            done: 0,
            spans: NativeStreamSlots {
                input: input_slot,
                output: output_slot,
                input_len,
                output_len,
            },
            body,
        });
        let index = func.pos();
        func.emit(op);
        self.native_loop_count += 1;
        Some(index)
    }

    /// Only an owned numeric scalar slot can be a private native port. Captured
    /// storage and reference/pointer slots keep ordinary lowering.
    fn loop_scalar_local(&self, binding: LocalId) -> Option<u16> {
        if self.captured_vars.contains(&binding)
            || self.lambda_referenced.contains(&binding)
            || !matches!(
                &*self.decl.arena.local(binding).ty,
                Type::Float32 | Type::Float64
            )
        {
            return None;
        }
        match self.variables.get(&binding) {
            Some(LocalKind::Scalar(slot)) => Some(*slot),
            _ => None,
        }
    }

    /// Resolve invariant scalar ports while the outer lexical environment is live.
    fn loop_scalar(
        &self,
        plan: &crate::value_loops::LoopRegion,
        value: crate::value_loops::ValueId,
    ) -> Option<LoopScalar> {
        use crate::value_loops::{Operation, Scalar};
        match &plan.values.get(value.0)?.operation {
            Operation::Input(binding) => Some(LoopScalar::Local(self.loop_scalar_local(*binding)?)),
            Operation::Constant(text) => Some(LoopScalar::Constant(match plan.ty {
                Scalar::F32 => u64::from(text.parse::<f32>().unwrap_or(0.0).to_bits()),
                Scalar::F64 => text.parse::<f64>().unwrap_or(0.0).to_bits(),
            })),
            _ => None,
        }
    }

    fn materialize_loop_scalar(&mut self, value: LoopScalar, func: &mut StackFunction) -> u16 {
        match value {
            LoopScalar::Local(slot) => slot,
            LoopScalar::Constant(bits) => {
                let slot = self.alloc_scalar();
                func.emit(StackOp::FusedConstSet(bits as i64, slot));
                slot
            }
        }
    }

    /// Solved array-to-slice coercions do not change the original storage ABI.
    fn loop_span_length(&self, expression: ExprID) -> Option<crate::value_loops::SpanLength> {
        use crate::value_loops::SpanLength;
        match &*self.representation_type(expression) {
            Type::Array(_, ArraySize::Known(length)) => {
                Some(SpanLength::Fixed(u32::try_from(*length).ok()?))
            }
            Type::Slice(_) => Some(SpanLength::Slice),
            _ => None,
        }
    }

    fn materialize_loop_span(
        &mut self,
        expression: ExprID,
        length: crate::value_loops::SpanLength,
        func: &mut StackFunction,
    ) -> (u16, u16) {
        use crate::value_loops::SpanLength;
        self.translate_expr(expression, func);
        let pointer = self.alloc_scalar();
        let length_slot = self.alloc_scalar();
        func.emit(StackOp::LocalSet(pointer));
        match length {
            SpanLength::Fixed(length) => {
                func.emit(StackOp::FusedConstSet(i64::from(length), length_slot));
            }
            SpanLength::Slice => {
                // First retain the header's length, then replace its address with
                // the raw element pointer. Both are refreshed by ordinary lowering
                // if an actual callback makes the kernel resume bytecode.
                func.emit(StackOp::LocalGet(pointer));
                func.emit(StackOp::Load32Off(8));
                func.emit(StackOp::LocalSet(length_slot));
                func.emit(StackOp::LocalGet(pointer));
                func.emit(StackOp::Load64);
                func.emit(StackOp::LocalSet(pointer));
            }
        }
        (pointer, length_slot)
    }

    /// Translate a field access.
    fn translate_field(&mut self, lhs_id: ExprID, name: Name, func: &mut StackFunction) {
        let lhs_ty = self.expr_type(lhs_id);

        // Handle array.len / slice.len.
        if *name == "len" {
            match &*lhs_ty {
                Type::Slice(_) => {
                    self.translate_expr(lhs_id, func);
                    func.emit(StackOp::Load32Off(8));
                    return;
                }
                Type::Array(_, len) => {
                    func.emit(StackOp::I64Const(len.known() as i64));
                    return;
                }
                _ => {}
            }
        }

        // f32x4 swizzle fields. Result is an f32 pushed to the float window.
        if matches!(&*lhs_ty, Type::Float32x4) {
            let s: &str = &name;
            let lane: i32 = match s {
                "x" | "r" => 0,
                "y" | "g" => 1,
                "z" | "b" => 2,
                "w" | "a" => 3,
                _ => panic!("invalid f32x4 field: {}", name),
            };
            self.translate_expr(lhs_id, func);
            func.emit(StackOp::LoadF32OffF(lane * 4));
            return;
        }

        self.translate_expr(lhs_id, func);

        if let Type::Name(struct_name, type_args) = &*lhs_ty {
            let struct_decl = self.decls.find(*struct_name);
            if let Decl::Struct(s) = &struct_decl[0] {
                let inst: crate::Instance = s
                    .typevars
                    .iter()
                    .zip(type_args.iter())
                    .map(|(tv, ty)| (mk_type(Type::Var(*tv)), *ty))
                    .collect();
                let offset = s.field_offset(&name, self.decls, &inst);

                if let Some(field) = s.find_field(&name) {
                    let field_ty = field.ty.subst(&inst);

                    // Func fields: build a fat pointer.
                    if matches!(&*field_ty, Type::Func(_, _)) {
                        let base_local = self.alloc_scalar();
                        func.emit(StackOp::LocalTee(base_local));
                        // Load func_idx from struct field.
                        func.emit(StackOp::Load64Off(offset));
                        let func_idx_local = self.alloc_scalar();
                        func.emit(StackOp::LocalSet(func_idx_local));
                        // Build fat pointer.
                        let mem_slot = self.alloc_memory(16);
                        func.emit(StackOp::LocalAddr(mem_slot));
                        func.emit(StackOp::LocalGet(func_idx_local));
                        func.emit(StackOp::Store64);
                        func.emit(StackOp::LocalAddr(mem_slot));
                        func.emit(StackOp::I64Const(0));
                        func.emit(StackOp::Store64Off(8));
                        func.emit(StackOp::LocalAddr(mem_slot));
                        return;
                    }

                    // Pointer types: return address of field.
                    if self.is_ptr_type(&field_ty) {
                        func.emit(StackOp::IAddImm(offset));
                        return;
                    }

                    // Scalar field: load from offset.
                    self.emit_load_offset(&field_ty, offset, func);
                    return;
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
                func.emit(StackOp::IAddImm(offset));
            } else {
                self.emit_load_offset(elem_ty, offset, func);
            }
            return;
        }

        // Fallback: the lhs is already on the stack.
    }

    /// Translate an array index.
    fn translate_array_index(&mut self, arr_id: ExprID, idx_id: ExprID, func: &mut StackFunction) {
        let arr_ty = self.representation_type(arr_id);

        // f32x4 element extraction. Result is an f32 pushed to the float window.
        if matches!(&*arr_ty, Type::Float32x4) {
            self.translate_expr(arr_id, func);
            self.translate_expr(idx_id, func);
            func.emit(StackOp::I64Const(4));
            func.emit(StackOp::IMul);
            func.emit(StackOp::IAdd);
            func.emit(StackOp::LoadF32F);
            return;
        }

        let (elem_ty, is_slice) = match &*arr_ty {
            Type::Array(elem_ty, _) => (*elem_ty, false),
            Type::Slice(elem_ty) => (*elem_ty, true),
            _ => {
                self.translate_expr(arr_id, func);
                return;
            }
        };

        // Fused 32-bit scalar load for slices and inline local arrays.
        let elem_size = elem_ty.size(self.decls);
        let elem_is_f32 = matches!(&*elem_ty, Type::Float32);
        if !self.is_ptr_type(&elem_ty) && elem_size == 4 {
            if let (Some(arr_slot), Some(idx_local)) =
                (self.get_memory_slot(arr_id), self.get_scalar_local(idx_id))
            {
                if is_slice {
                    if elem_is_f32 {
                        func.emit(StackOp::FusedAddrGetSliceLoad32F(arr_slot, idx_local));
                    } else {
                        func.emit(StackOp::FusedAddrGetSliceLoad32(arr_slot, idx_local));
                    }
                } else if elem_is_f32 {
                    func.emit(StackOp::FusedLocalArrayLoad32F(arr_slot, idx_local));
                } else {
                    func.emit(StackOp::FusedLocalArrayLoad32(arr_slot, idx_local));
                }
                return;
            }
            if is_slice {
                // Fallback: generic slice load.
                self.translate_expr(arr_id, func);
                self.translate_expr(idx_id, func);
                if elem_is_f32 {
                    func.emit(StackOp::SliceLoad32F);
                } else {
                    func.emit(StackOp::SliceLoad32);
                }
                return;
            }
        }

        self.translate_expr(arr_id, func);

        // For slices, load data pointer from fat pointer.
        if is_slice {
            func.emit(StackOp::Load64);
        }

        // Compute element address: base + idx * elem_size.
        self.translate_expr(idx_id, func);
        let elem_size = elem_ty.size(self.decls);
        func.emit(StackOp::I64Const(elem_size as i64));
        func.emit(StackOp::IMul);
        func.emit(StackOp::IAdd);

        if self.is_ptr_type(&elem_ty) {
            // Return address.
        } else {
            self.emit_load(&elem_ty, func);
        }
    }

    /// Translate an array literal.
    fn translate_array_literal(
        &mut self,
        elements: &[ExprID],
        expr: ExprID,
        func: &mut StackFunction,
    ) {
        let ty = self.expr_type(expr);
        let size = ty.size(self.decls) as u32;
        let mem_slot = self.alloc_memory(size);

        if let Type::Array(elem_ty, _) = &*ty {
            let elem_size = elem_ty.size(self.decls);
            let elem_ty = *elem_ty;
            for (i, &elem_id) in elements.iter().enumerate() {
                let offset = (i as i32) * elem_size;
                self.emit_dest_addr(mem_slot, &elem_ty, offset, func);
                self.translate_expr(elem_id, func);
                self.emit_wrap_for_expected_slice(elem_ty, elem_id, func);
                self.emit_store_offset(&elem_ty, offset, func);
            }
        }

        func.emit(StackOp::LocalAddr(mem_slot));
    }

    /// Translate a struct literal.
    fn translate_struct_lit(
        &mut self,
        struct_name: Name,
        fields: &[(Name, ExprID)],
        expr: ExprID,
        func: &mut StackFunction,
    ) {
        let ty = self.expr_type(expr);
        let size = self.vm_type_size(&ty);
        let mem_slot = self.alloc_memory(size);

        // Zero-init.
        func.emit(StackOp::LocalAddr(mem_slot));
        func.emit(StackOp::MemZero(size));

        if let Type::Name(_, type_args) = &*ty {
            let struct_decl = self.decls.find(struct_name);
            if let Decl::Struct(s) = &struct_decl[0] {
                let inst: crate::Instance = s
                    .typevars
                    .iter()
                    .map(|tv| mk_type(Type::Var(*tv)))
                    .zip(type_args.iter().copied())
                    .collect();
                for (fname, fval) in fields {
                    let offset = s.field_offset(fname, self.decls, &inst);
                    let field_ty = self.expr_type(*fval);
                    self.emit_dest_addr(mem_slot, &field_ty, offset, func);
                    self.translate_expr(*fval, func);
                    self.emit_store_offset(&field_ty, offset, func);
                }
            }
        }

        func.emit(StackOp::LocalAddr(mem_slot));
    }

    /// Translate a tuple literal.
    fn translate_tuple(&mut self, elements: &[ExprID], expr: ExprID, func: &mut StackFunction) {
        let ty = self.expr_type(expr);
        let size = ty.size(self.decls) as u32;
        let mem_slot = self.alloc_memory(size);

        if let Type::Tuple(elem_types) = &*ty {
            let mut offset = 0;
            for (i, &elem_id) in elements.iter().enumerate() {
                let elem_ty = &elem_types[i];
                self.emit_dest_addr(mem_slot, elem_ty, offset, func);
                self.translate_expr(elem_id, func);
                self.emit_store_offset(elem_ty, offset, func);
                offset += elem_ty.size(self.decls);
            }
        }

        func.emit(StackOp::LocalAddr(mem_slot));
    }

    /// Translate a type cast.
    fn translate_cast(&mut self, expr_id: ExprID, target_ty: TypeID, func: &mut StackFunction) {
        self.translate_expr(expr_id, func);
        let src_ty = self.expr_type(expr_id);

        match (&*src_ty, &*target_ty) {
            (Type::Int32, Type::Float32) => func.emit(StackOp::I32ToF32F),
            (Type::Float32, Type::Int32) => func.emit(StackOp::F32ToI32F),
            (Type::Int32, Type::Float64) => func.emit(StackOp::I32ToF64D),
            (Type::Float64, Type::Int32) => func.emit(StackOp::F64ToI32D),
            (Type::Float32, Type::Float64) => func.emit(StackOp::F32ToF64D),
            (Type::Float64, Type::Float32) => func.emit(StackOp::F64ToF32D),
            (Type::Int32, Type::Int8) | (Type::UInt32, Type::Int8) => func.emit(StackOp::I32ToI8),
            (Type::Int8, Type::Int32) => func.emit(StackOp::I8ToI32),
            (Type::Int32, Type::UInt32) | (Type::UInt32, Type::Int32) => {
                func.emit(StackOp::I64ToU32)
            }
            _ => {
                // No conversion needed.
            }
        }
    }

    /// Translate a lambda expression.
    fn translate_lambda(&mut self, expr: ExprID, func: &mut StackFunction) {
        let id = *self.lambda_counter;
        *self.lambda_counter += 1;
        let lambda_name = Name::new(format!("__lambda_{}", id));

        let lambda_decl = self.decl.extract_lambda(expr, lambda_name);
        let free_vars = &lambda_decl.closure_vars;

        // Build closure struct if there are captures.
        let has_captures = !free_vars.is_empty();
        let closure_mem_slot = if has_captures {
            let n = free_vars.len();
            let slot = self.alloc_memory((n * 8) as u32);
            for (i, var_name) in free_vars.iter().enumerate() {
                func.emit(StackOp::LocalAddr(slot));
                self.emit_var_address(var_name, func);
                func.emit(StackOp::Store64Off((i * 8) as i32));
            }
            Some(slot)
        } else {
            None
        };

        self.pending_lambdas.push(lambda_decl);

        // Build fat pointer {func_idx, closure_ptr}.
        let fat_slot = self.alloc_memory(16);
        // Store func_idx.
        func.emit(StackOp::LocalAddr(fat_slot));
        let instr_idx = func.pos();
        func.emit(StackOp::I64Const(0)); // placeholder
        self.lambda_patches.push((instr_idx, lambda_name));
        func.emit(StackOp::Store64);
        // Store closure_ptr.
        func.emit(StackOp::LocalAddr(fat_slot));
        if let Some(closure_slot) = closure_mem_slot {
            func.emit(StackOp::LocalAddr(closure_slot));
        } else {
            func.emit(StackOp::I64Const(0));
        }
        func.emit(StackOp::Store64Off(8));
        // Push fat pointer address.
        func.emit(StackOp::LocalAddr(fat_slot));
    }

    /// Get the address of a variable for closure capture.
    fn emit_var_address(&mut self, name: &LocalId, func: &mut StackFunction) {
        if self.captured_vars.contains(name) {
            // Already captured from an enclosing scope: follow indirection.
            let addr_local = *self.captured_slots.get(name).unwrap();
            func.emit(StackOp::LocalGet(addr_local));
        } else if let Some(&kind) = self.variables.get(name) {
            match kind {
                LocalKind::Scalar(slot) if self.is_ptr_type(&self.decl.arena.local(*name).ty) => {
                    // Aggregate and fat-pointer values already hold their storage address.
                    func.emit(StackOp::LocalGet(slot));
                }
                LocalKind::Scalar(slot) => {
                    // Scalar: need to spill to memory so we have a stable address.
                    let mem_slot = self.alloc_memory(8);
                    func.emit(StackOp::LocalAddr(mem_slot));
                    func.emit(StackOp::LocalGet(slot));
                    func.emit(StackOp::Store64);
                    // Update variable to memory-backed.
                    self.variables.insert(*name, LocalKind::Memory(mem_slot));
                    func.emit(StackOp::LocalAddr(mem_slot));
                }
                LocalKind::Memory(slot) => {
                    func.emit(StackOp::LocalAddr(slot));
                }
                LocalKind::Reference(slot) => {
                    func.emit(StackOp::LocalGet(slot));
                }
            }
        } else {
            unreachable!("checked capture local {:?} has no storage", name);
        }
    }

    /// Emit the window-appropriate `local.get` for a scalar of `ty`:
    /// f32 → float window, f64 → double window, everything else → int.
    fn emit_local_get(&self, ty: &TypeID, slot: u16, func: &mut StackFunction) {
        match &**ty {
            Type::Float32 => func.emit(StackOp::LocalGetF(slot)),
            Type::Float64 => func.emit(StackOp::LocalGetD(slot)),
            _ => func.emit(StackOp::LocalGet(slot)),
        }
    }

    /// Window-appropriate `local.set` (see `emit_local_get`).
    fn emit_local_set(&self, ty: &TypeID, slot: u16, func: &mut StackFunction) {
        match &**ty {
            Type::Float32 => func.emit(StackOp::LocalSetF(slot)),
            Type::Float64 => func.emit(StackOp::LocalSetD(slot)),
            _ => func.emit(StackOp::LocalSet(slot)),
        }
    }

    /// Window-appropriate `local.tee` (see `emit_local_get`).
    fn emit_local_tee(&self, ty: &TypeID, slot: u16, func: &mut StackFunction) {
        match &**ty {
            Type::Float32 => func.emit(StackOp::LocalTeeF(slot)),
            Type::Float64 => func.emit(StackOp::LocalTeeD(slot)),
            _ => func.emit(StackOp::LocalTee(slot)),
        }
    }

    /// Emit a load instruction based on type. Pops address, pushes value.
    fn emit_load(&self, ty: &TypeID, func: &mut StackFunction) {
        match &**ty {
            Type::Bool | Type::Int8 | Type::UInt8 => func.emit(StackOp::Load8),
            Type::Float32 => func.emit(StackOp::LoadF32F),
            Type::Int32 | Type::UInt32 => func.emit(StackOp::Load32),
            Type::Float64 => func.emit(StackOp::LoadF64D),
            _ => func.emit(StackOp::Load64),
        }
    }

    /// Emit a load with offset. Stack: [base] -> [value].
    fn emit_load_offset(&self, ty: &TypeID, offset: i32, func: &mut StackFunction) {
        match &**ty {
            Type::Bool | Type::Int8 | Type::UInt8 => {
                func.emit(StackOp::IAddImm(offset));
                func.emit(StackOp::Load8);
            }
            Type::Float32 => {
                func.emit(StackOp::LoadF32OffF(offset));
            }
            Type::Int32 | Type::UInt32 => {
                func.emit(StackOp::Load32Off(offset));
            }
            Type::Float64 => {
                func.emit(StackOp::LoadF64OffD(offset));
            }
            _ => {
                func.emit(StackOp::Load64Off(offset));
            }
        }
    }

    /// Emit a store instruction. Stack: [addr, value] -> [].
    fn emit_store_op(&self, ty: &TypeID, func: &mut StackFunction) {
        if self.is_ptr_type(ty) {
            let size = self.vm_type_size(ty);
            func.emit(StackOp::MemCopy(size));
        } else {
            match &**ty {
                Type::Bool | Type::Int8 | Type::UInt8 => func.emit(StackOp::Store8),
                Type::Float32 => func.emit(StackOp::StoreF32F),
                Type::Int32 | Type::UInt32 => func.emit(StackOp::Store32),
                Type::Float64 => func.emit(StackOp::StoreF64D),
                _ => func.emit(StackOp::Store64),
            }
        }
    }

    /// Push the destination address for storing a value of type `ty` at
    /// `offset` within `slot`, ready for a following `emit_store_offset`.
    /// Composite types are stored with MemCopy, which has no offset operand,
    /// so the offset is folded into the address here.
    fn emit_dest_addr(&self, slot: u16, ty: &TypeID, offset: i32, func: &mut StackFunction) {
        func.emit(StackOp::LocalAddr(slot));
        if self.is_ptr_type(ty) && offset != 0 {
            func.emit(StackOp::IAddImm(offset));
        }
    }

    /// Emit a store with offset. Stack: [base, value] -> [].
    /// For composite types the base must already include the offset — see
    /// `emit_dest_addr`, which the aggregate-literal callers use.
    fn emit_store_offset(&self, ty: &TypeID, offset: i32, func: &mut StackFunction) {
        if self.is_ptr_type(ty) {
            // Composite values are represented by the address of their storage,
            // so copy the bytes into place. Stack is [dst_addr, src_addr] and
            // MemCopy takes no offset operand, hence the emit_dest_addr contract.
            func.emit(StackOp::MemCopy(self.vm_type_size(ty)));
        } else {
            match &**ty {
                Type::Bool | Type::Int8 | Type::UInt8 => {
                    func.emit(StackOp::Store8Off(offset));
                }
                Type::Float32 => {
                    func.emit(StackOp::StoreF32OffF(offset));
                }
                Type::Int32 | Type::UInt32 => {
                    func.emit(StackOp::Store32Off(offset));
                }
                Type::Float64 => {
                    func.emit(StackOp::StoreF64OffD(offset));
                }
                _ => {
                    func.emit(StackOp::Store32Off(offset));
                }
            }
        }
    }

    /// Wrap a sized array as a slice fat pointer if needed.
    /// Stack: [value] -> [fat_ptr_addr]
    fn emit_wrap_as_slice(&mut self, actual_ty: TypeID, func: &mut StackFunction) {
        match &*actual_ty {
            Type::Slice(_) => {
                // Already a slice, nothing to do.
            }
            Type::Array(_, sz) => {
                let val_local = self.alloc_scalar();
                func.emit(StackOp::LocalSet(val_local));
                let fat_slot = self.alloc_memory(12);
                // Store data_ptr at offset 0.
                func.emit(StackOp::LocalAddr(fat_slot));
                func.emit(StackOp::LocalGet(val_local));
                func.emit(StackOp::Store64);
                // Store len at offset 8.
                func.emit(StackOp::LocalAddr(fat_slot));
                func.emit(StackOp::I64Const(sz.known() as i64));
                func.emit(StackOp::Store32Off(8));
                // Push fat pointer address.
                func.emit(StackOp::LocalAddr(fat_slot));
            }
            _ => {
                // Not an array type; leave as-is.
            }
        }
    }

    fn needs_slice_wrap(&self, expected_ty: TypeID, actual_expr: ExprID) -> bool {
        matches!(&*expected_ty, Type::Slice(_))
            && matches!(&*self.representation_type(actual_expr), Type::Array(_, _))
    }

    fn emit_wrap_for_expected_slice(
        &mut self,
        expected_ty: TypeID,
        actual_expr: ExprID,
        func: &mut StackFunction,
    ) {
        if matches!(&*expected_ty, Type::Slice(_)) {
            let actual_ty = self.representation_type(actual_expr);
            self.emit_wrap_as_slice(actual_ty, func);
        }
    }
}
