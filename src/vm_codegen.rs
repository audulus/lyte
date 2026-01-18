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
        }
    }

    /// Compile a DeclTable into a VMProgram.
    ///
    /// This looks for a "main" function and compiles it along with all
    /// functions it calls.
    pub fn compile(&mut self, decls: &DeclTable) -> Result<VMProgram, String> {
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

        Ok(std::mem::take(&mut self.program))
    }

    /// Compile a single function.
    fn compile_function(&mut self, decl: &FuncDecl, decls: &DeclTable) -> Result<FuncIdx, String> {
        let mut func = VMFunction::new(&*decl.name);
        func.param_count = decl.params.len() as u8;

        let mut translator = FunctionTranslator::new(decl, decls, &mut self.pending_functions);
        translator.translate(&mut func);

        let idx = self.program.add_function(func);
        self.func_indices.insert(decl.name, idx);
        self.compiled_functions.insert(decl.name);

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

/// Translator for a single function body.
struct FunctionTranslator<'a> {
    /// The function declaration being translated.
    decl: &'a FuncDecl,

    /// Declaration table for looking up types and functions.
    decls: &'a DeclTable,

    /// Map from variable names to their register numbers.
    variables: HashMap<Name, Reg>,

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

    /// Map from function name to expected function index.
    called_functions: HashMap<Name, FuncIdx>,

    /// Current next function index for placeholders.
    next_func_idx: FuncIdx,
}

impl<'a> FunctionTranslator<'a> {
    fn new(
        decl: &'a FuncDecl,
        decls: &'a DeclTable,
        pending_functions: &'a mut Vec<Name>,
    ) -> Self {
        Self {
            decl,
            decls,
            variables: HashMap::new(),
            local_slots: HashMap::new(),
            next_reg: 0,
            next_slot: 0,
            locals_size: 0,
            pending_functions,
            called_functions: HashMap::new(),
            next_func_idx: 0,
        }
    }

    /// Translate the function body.
    fn translate(&mut self, func: &mut VMFunction) {
        // Reserve registers for parameters.
        for param in &self.decl.params {
            let reg = self.alloc_reg();
            self.variables.insert(param.name, reg);
        }

        // Translate the body if present.
        if let Some(body) = self.decl.body {
            let result_reg = self.translate_expr(body, func);

            // Return the result.
            if result_reg != 0 {
                func.emit(Opcode::ReturnReg { src: result_reg });
            } else {
                func.emit(Opcode::Return);
            }
        } else {
            func.emit(Opcode::Return);
        }

        // Set function metadata.
        func.locals_size = self.locals_size;
        func.local_slots = self.next_slot;
    }

    /// Allocate a new register.
    fn alloc_reg(&mut self) -> Reg {
        let reg = self.next_reg;
        self.next_reg += 1;
        if self.next_reg > 250 {
            panic!("too many registers used");
        }
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

                // Check if it's a local variable.
                if let Some(&reg) = self.variables.get(name) {
                    // For pointer types (structs, arrays), return the address.
                    if self.is_ptr_type(&ty) {
                        reg
                    } else if let Some(&slot) = self.local_slots.get(name) {
                        // Load from local slot.
                        let dst = self.alloc_reg();
                        func.emit(Opcode::LocalAddr { dst, slot });
                        let load_dst = self.alloc_reg();
                        self.emit_load(&ty, load_dst, dst, func);
                        load_dst
                    } else {
                        reg
                    }
                } else {
                    // Check if it's a function.
                    if let Type::Func(_, _) = &*ty {
                        // Return function index as a value.
                        let dst = self.alloc_reg();
                        self.pending_functions.push(*name);
                        // For now, emit 0 - in a real implementation we'd need to
                        // patch this later or use a function table.
                        func.emit(Opcode::LoadImm { dst, value: 0 });
                        dst
                    } else {
                        // Unknown identifier - this shouldn't happen after type checking.
                        let dst = self.alloc_reg();
                        func.emit(Opcode::LoadImm { dst, value: 0 });
                        dst
                    }
                }
            }

            Expr::Binop(op, lhs_id, rhs_id) => {
                self.translate_binop(*op, *lhs_id, *rhs_id, func)
            }

            Expr::Unop(op, arg_id) => self.translate_unop(*op, *arg_id, func),

            Expr::Call(fn_id, arg_ids) => self.translate_call(*fn_id, arg_ids, func),

            Expr::Let(name, init, _) => {
                let init_reg = self.translate_expr(*init, func);
                self.variables.insert(*name, init_reg);
                init_reg
            }

            Expr::Var(name, init, _) => {
                let ty = self.expr_type(expr);
                let size = ty.size(self.decls);
                let slot = self.alloc_local(size as u32);
                self.local_slots.insert(*name, slot);

                // Get address of local slot.
                let addr_reg = self.alloc_reg();
                func.emit(Opcode::LocalAddr {
                    dst: addr_reg,
                    slot,
                });
                self.variables.insert(*name, addr_reg);

                // Initialize if there's an initializer.
                if let Some(init_id) = init {
                    let init_reg = self.translate_expr(*init_id, func);
                    self.emit_store(&ty, addr_reg, init_reg, func);
                } else {
                    // Zero-initialize.
                    func.emit(Opcode::MemZero {
                        dst: addr_reg,
                        size: size as u32,
                    });
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
                func.emit(Opcode::ReturnReg { src: result });
                result
            }

            Expr::Field(lhs_id, name) => self.translate_field(*lhs_id, *name, func),

            Expr::ArrayIndex(arr_id, idx_id) => {
                self.translate_array_index(*arr_id, *idx_id, func)
            }

            Expr::ArrayLiteral(elements) => self.translate_array_literal(elements, expr, func),

            Expr::Tuple(elements) => self.translate_tuple(elements, expr, func),

            Expr::AsTy(expr_id, target_ty) => {
                self.translate_cast(*expr_id, *target_ty, func)
            }

            Expr::Assign(name, value_id) => {
                let value_reg = self.translate_expr(*value_id, func);
                if let Some(&addr_reg) = self.variables.get(name) {
                    if self.local_slots.contains_key(name) {
                        let ty = self.expr_type(*value_id);
                        self.emit_store(&ty, addr_reg, value_reg, func);
                    }
                }
                value_reg
            }

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
                    func.emit(Opcode::IAdd { dst, a: lhs, b: rhs });
                }
                Type::Float32 => {
                    func.emit(Opcode::FAdd { dst, a: lhs, b: rhs });
                }
                Type::Float64 => {
                    func.emit(Opcode::DAdd { dst, a: lhs, b: rhs });
                }
                _ => {
                    func.emit(Opcode::IAdd { dst, a: lhs, b: rhs });
                }
            },

            Binop::Minus => match &*ty {
                Type::Int32 | Type::UInt32 | Type::Int8 | Type::UInt8 => {
                    func.emit(Opcode::ISub { dst, a: lhs, b: rhs });
                }
                Type::Float32 => {
                    func.emit(Opcode::FSub { dst, a: lhs, b: rhs });
                }
                Type::Float64 => {
                    func.emit(Opcode::DSub { dst, a: lhs, b: rhs });
                }
                _ => {
                    func.emit(Opcode::ISub { dst, a: lhs, b: rhs });
                }
            },

            Binop::Mult => match &*ty {
                Type::Int32 | Type::UInt32 | Type::Int8 | Type::UInt8 => {
                    func.emit(Opcode::IMul { dst, a: lhs, b: rhs });
                }
                Type::Float32 => {
                    func.emit(Opcode::FMul { dst, a: lhs, b: rhs });
                }
                Type::Float64 => {
                    func.emit(Opcode::DMul { dst, a: lhs, b: rhs });
                }
                _ => {
                    func.emit(Opcode::IMul { dst, a: lhs, b: rhs });
                }
            },

            Binop::Div => match &*ty {
                Type::Int32 | Type::Int8 => {
                    func.emit(Opcode::IDiv { dst, a: lhs, b: rhs });
                }
                Type::UInt32 | Type::UInt8 => {
                    func.emit(Opcode::UDiv { dst, a: lhs, b: rhs });
                }
                Type::Float32 => {
                    func.emit(Opcode::FDiv { dst, a: lhs, b: rhs });
                }
                Type::Float64 => {
                    func.emit(Opcode::DDiv { dst, a: lhs, b: rhs });
                }
                _ => {
                    func.emit(Opcode::IDiv { dst, a: lhs, b: rhs });
                }
            },

            Binop::Equal => match &*ty {
                Type::Float32 => {
                    func.emit(Opcode::FEq { dst, a: lhs, b: rhs });
                }
                Type::Float64 => {
                    func.emit(Opcode::DEq { dst, a: lhs, b: rhs });
                }
                _ => {
                    func.emit(Opcode::IEq { dst, a: lhs, b: rhs });
                }
            },

            Binop::NotEqual => match &*ty {
                Type::Float32 => {
                    func.emit(Opcode::FNe { dst, a: lhs, b: rhs });
                }
                _ => {
                    func.emit(Opcode::INe { dst, a: lhs, b: rhs });
                }
            },

            Binop::Less => match &*ty {
                Type::Float32 => {
                    func.emit(Opcode::FLt { dst, a: lhs, b: rhs });
                }
                Type::Float64 => {
                    func.emit(Opcode::DLt { dst, a: lhs, b: rhs });
                }
                Type::UInt32 | Type::UInt8 => {
                    func.emit(Opcode::ULt { dst, a: lhs, b: rhs });
                }
                _ => {
                    func.emit(Opcode::ILt { dst, a: lhs, b: rhs });
                }
            },

            Binop::Greater => match &*ty {
                Type::Float32 => {
                    func.emit(Opcode::FGt { dst, a: lhs, b: rhs });
                }
                Type::UInt32 | Type::UInt8 => {
                    func.emit(Opcode::UGt { dst, a: lhs, b: rhs });
                }
                _ => {
                    func.emit(Opcode::IGt { dst, a: lhs, b: rhs });
                }
            },

            Binop::Leq => match &*ty {
                Type::Float32 => {
                    func.emit(Opcode::FLe { dst, a: lhs, b: rhs });
                }
                Type::Float64 => {
                    func.emit(Opcode::DLe { dst, a: lhs, b: rhs });
                }
                _ => {
                    func.emit(Opcode::ILe { dst, a: lhs, b: rhs });
                }
            },

            Binop::Geq => {
                func.emit(Opcode::IGe { dst, a: lhs, b: rhs });
            }

            Binop::And => {
                func.emit(Opcode::And { dst, a: lhs, b: rhs });
            }

            Binop::Or => {
                func.emit(Opcode::Or { dst, a: lhs, b: rhs });
            }

            Binop::Pow => {
                // Power is not directly supported - just multiply for now.
                func.emit(Opcode::IMul { dst, a: lhs, b: rhs });
            }

            Binop::Assign => {
                // Handled above.
                unreachable!()
            }
        }

        dst
    }

    /// Translate an assignment expression.
    fn translate_assign(
        &mut self,
        lhs_id: ExprID,
        rhs_id: ExprID,
        func: &mut VMFunction,
    ) -> Reg {
        let rhs = self.translate_expr(rhs_id, func);
        let lhs_addr = self.translate_lvalue(lhs_id, func);
        let ty = self.expr_type(lhs_id);
        self.emit_store(&ty, lhs_addr, rhs, func);
        rhs
    }

    /// Translate an lvalue expression (returns address).
    fn translate_lvalue(&mut self, expr: ExprID, func: &mut VMFunction) -> Reg {
        match &self.decl.arena.exprs[expr] {
            Expr::Id(name) => {
                if let Some(&reg) = self.variables.get(name) {
                    reg
                } else {
                    let dst = self.alloc_reg();
                    func.emit(Opcode::LoadImm { dst, value: 0 });
                    dst
                }
            }

            Expr::Field(lhs_id, name) => {
                let lhs_addr = self.translate_lvalue(*lhs_id, func);
                let lhs_ty = self.expr_type(*lhs_id);

                if let Type::Name(struct_name, _) = &*lhs_ty {
                    let struct_decl = self.decls.find(*struct_name);
                    if let Decl::Struct(s) = &struct_decl[0] {
                        let inst = crate::Instance::new();
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

                if let Type::Array(elem_ty, _) = &*arr_ty {
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

                    // result = arr_addr + offset
                    let dst = self.alloc_reg();
                    func.emit(Opcode::IAdd {
                        dst,
                        a: arr_addr,
                        b: offset_reg,
                    });
                    return dst;
                }
                arr_addr
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
                func.emit(Opcode::And { dst, a: dst, b: one });
            }
        }

        dst
    }

    /// Translate a function call.
    fn translate_call(
        &mut self,
        fn_id: ExprID,
        arg_ids: &[ExprID],
        func: &mut VMFunction,
    ) -> Reg {
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

            // Regular function call.
            // Evaluate arguments into consecutive registers.
            let args_start = self.next_reg;
            for arg_id in arg_ids {
                let arg_reg = self.translate_expr(*arg_id, func);
                // Ensure arguments are in consecutive registers.
                if arg_reg != self.next_reg - 1 {
                    let dst = self.alloc_reg();
                    func.emit(Opcode::Move { dst, src: arg_reg });
                }
            }

            // Add function to pending list.
            self.pending_functions.push(*name);

            // For now, use function index 0 - this needs to be fixed up later.
            // In a real implementation, we'd use a relocation table.
            func.emit(Opcode::Call {
                func: 0, // Placeholder - needs relocation.
                args_start,
                arg_count: arg_ids.len() as u8,
            });

            // Result is in register 0.
            0
        } else {
            // Indirect call.
            let fn_reg = self.translate_expr(fn_id, func);

            // Evaluate arguments.
            let args_start = self.next_reg;
            for arg_id in arg_ids {
                self.translate_expr(*arg_id, func);
            }

            func.emit(Opcode::CallIndirect {
                func_reg: fn_reg,
                args_start,
                arg_count: arg_ids.len() as u8,
            });

            0
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

        // Then branch.
        let then_result = self.translate_expr(then_id, func);
        func.emit(Opcode::Move {
            dst: result_reg,
            src: then_result,
        });

        if let Some(else_expr_id) = else_id {
            // Jump over else branch.
            let jump_to_end = func.emit(Opcode::Jump { offset: 0 });

            // Patch jump to else.
            func.patch_jump(jump_to_else);

            // Else branch.
            let else_result = self.translate_expr(else_expr_id, func);
            func.emit(Opcode::Move {
                dst: result_reg,
                src: else_result,
            });

            // Patch jump to end.
            func.patch_jump(jump_to_end);
        } else {
            // No else branch - patch jump.
            func.patch_jump(jump_to_else);
        }

        result_reg
    }

    /// Translate a while loop.
    fn translate_while(
        &mut self,
        cond_id: ExprID,
        body_id: ExprID,
        func: &mut VMFunction,
    ) -> Reg {
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
        let lhs = self.translate_expr(lhs_id, func);
        let lhs_ty = self.expr_type(lhs_id);
        let result_ty = self.expr_type(lhs_id); // Placeholder - should be field type.

        if let Type::Name(struct_name, _) = &*lhs_ty {
            let struct_decl = self.decls.find(*struct_name);
            if let Decl::Struct(s) = &struct_decl[0] {
                let inst = crate::Instance::new();
                let offset = s.field_offset(&name, self.decls, &inst);

                // Find field type.
                let field = s.find_field(&name);
                if let Some(field) = field {
                    let dst = self.alloc_reg();
                    self.emit_load_offset(&field.ty, dst, lhs, offset, func);
                    return dst;
                }
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

        if let Type::Array(elem_ty, _) = &*arr_ty {
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
                a: arr,
                b: offset_reg,
            });

            let dst = self.alloc_reg();
            self.emit_load(elem_ty, dst, addr_reg, func);
            return dst;
        }

        // Fallback.
        arr
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
    fn translate_tuple(
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
    fn translate_cast(
        &mut self,
        expr_id: ExprID,
        target_ty: TypeID,
        func: &mut VMFunction,
    ) -> Reg {
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
            _ => {
                // No conversion needed or unsupported - just move.
                func.emit(Opcode::Move { dst, src });
            }
        }

        dst
    }

    /// Check if a type is represented as a pointer.
    fn is_ptr_type(&self, ty: &TypeID) -> bool {
        matches!(&**ty, Type::Name(_, _) | Type::Tuple(_) | Type::Array(_, _))
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
        &self,
        ty: &TypeID,
        dst: Reg,
        base: Reg,
        offset: i32,
        func: &mut VMFunction,
    ) {
        match &**ty {
            Type::Bool | Type::Int8 | Type::UInt8 => {
                let addr = self.alloc_reg_func(func);
                func.emit(Opcode::IAddImm {
                    dst: addr,
                    src: base,
                    imm: offset,
                });
                func.emit(Opcode::Load8 { dst, addr });
            }
            Type::Int32 | Type::UInt32 | Type::Float32 => {
                func.emit(Opcode::Load32Off {
                    dst,
                    base,
                    offset,
                });
            }
            Type::Float64 => {
                func.emit(Opcode::Load64Off {
                    dst,
                    base,
                    offset,
                });
            }
            _ => {
                func.emit(Opcode::Load64Off {
                    dst,
                    base,
                    offset,
                });
            }
        }
    }

    /// Allocate a register (workaround for borrow issues).
    fn alloc_reg_func(&self, _func: &mut VMFunction) -> Reg {
        // This is a hack - we just use a high register.
        250
    }

    /// Emit a store instruction based on type.
    fn emit_store(&self, ty: &TypeID, addr: Reg, src: Reg, func: &mut VMFunction) {
        if self.is_ptr_type(ty) {
            let size = ty.size(self.decls) as u32;
            func.emit(Opcode::MemCopy { dst: addr, src, size });
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
    use crate::vm::VM;

    /// Helper to create a simple function for testing.
    fn make_simple_decl_table(body: Expr, ret_ty: TypeID) -> DeclTable {
        let mut arena = ExprArena::new();
        let body_id = arena.add(body, crate::test_loc());

        let func = FuncDecl {
            name: Name::str("main"),
            typevars: vec![],
            params: vec![],
            body: Some(body_id),
            ret: ret_ty,
            constraints: vec![],
            loc: crate::test_loc(),
            arena,
            types: vec![ret_ty], // Simplified - just use return type.
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
            params: vec![],
            body: Some(expr),
            ret: mk_type(Type::Int32),
            constraints: vec![],
            loc: crate::test_loc(),
            arena,
            types: vec![mk_type(Type::Int32)],
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
            params: vec![],
            body: Some(add),
            ret: int32,
            constraints: vec![],
            loc: crate::test_loc(),
            arena,
            types: vec![int32, int32, int32],
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
            params: vec![],
            body: Some(mul),
            ret: f32_ty,
            constraints: vec![],
            loc: crate::test_loc(),
            arena,
            types: vec![f32_ty, f32_ty, f32_ty],
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
        let if_expr = arena.add(
            Expr::If(cond, then_val, Some(else_val)),
            crate::test_loc(),
        );

        let int32 = mk_type(Type::Int32);
        let bool_ty = mk_type(Type::Bool);
        let func = FuncDecl {
            name: Name::str("main"),
            typevars: vec![],
            params: vec![],
            body: Some(if_expr),
            ret: int32,
            constraints: vec![],
            loc: crate::test_loc(),
            arena,
            types: vec![bool_ty, int32, int32, int32],
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
        let block = arena.add(
            Expr::Block(vec![while_expr, result]),
            crate::test_loc(),
        );

        let int32 = mk_type(Type::Int32);
        let bool_ty = mk_type(Type::Bool);
        let func = FuncDecl {
            name: Name::str("main"),
            typevars: vec![],
            params: vec![],
            body: Some(block),
            ret: int32,
            constraints: vec![],
            loc: crate::test_loc(),
            arena,
            types: vec![bool_ty, int32, int32, int32, int32],
        };

        let decls = DeclTable::new(vec![Decl::Func(func)]);

        let mut codegen = VMCodegen::new();
        let program = codegen.compile(&decls).unwrap();

        let mut vm = VM::new();
        let result = vm.run(&program);
        assert_eq!(result, 42);
    }
}
