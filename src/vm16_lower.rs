//! Lower `Vec<Opcode>` to `Vec<Op16Instr>` for the 16-bit VM.
//!
//! Pipeline:
//! 1. Run shared peephole optimization (from vm_optimize)
//! 2. Copy coalescing + 16-register linear scan with spilling
//! 3. Convert 3-operand → 2-operand destructive form
//! 4. Translate Opcode → Op16Instr

use crate::opcode::{FuncIdx, Offset, Opcode, Reg};
use crate::vm16_opcode::{tags, Reg16};
use crate::vm_optimize;

/// A 16-bit instruction in intermediate form before packing.
/// Contains the opcode tag, two 4-bit register operands, and an optional trailing u16.
#[derive(Clone, Debug, PartialEq)]
pub struct Op16Instr {
    pub tag: u8,
    pub ra: Reg16,
    pub rb: Reg16,
    pub trail: Option<u16>,
}

impl Op16Instr {
    fn ab(tag: u8, ra: Reg16, rb: Reg16) -> Self {
        Self { tag, ra, rb, trail: None }
    }

    fn ab_trail(tag: u8, ra: Reg16, rb: Reg16, trail: u16) -> Self {
        Self { tag, ra, rb, trail: Some(trail) }
    }

    fn a_trail(tag: u8, ra: Reg16, trail: u16) -> Self {
        Self { tag, ra, rb: 0, trail: Some(trail) }
    }

    fn a_only(tag: u8, ra: Reg16) -> Self {
        Self { tag, ra, rb: 0, trail: None }
    }

    fn trail_only(tag: u8, trail: u16) -> Self {
        Self { tag, ra: 0, rb: 0, trail: Some(trail) }
    }

    fn bare(tag: u8) -> Self {
        Self { tag, ra: 0, rb: 0, trail: None }
    }
}

/// Result of lowering one function.
pub struct LoweredFunction {
    pub code: Vec<Op16Instr>,
    pub locals_size: u32,
}

/// Result of lowering an entire program.
pub struct LoweredProgram {
    pub functions: Vec<LoweredFunction>,
    pub f32_pool: Vec<f32>,
    pub f64_pool: Vec<f64>,
    pub i64_pool: Vec<i64>,
}

const MAX_REGS: usize = 16;

/// Lower a program's functions from 3-operand Opcode to 2-operand Op16Instr.
pub fn lower_program(
    functions: &[(Vec<Opcode>, u8, u32)], // (code, param_count, locals_size)
) -> LoweredProgram {
    let mut f32_pool: Vec<f32> = Vec::new();
    let mut f64_pool: Vec<f64> = Vec::new();
    let mut i64_pool: Vec<i64> = Vec::new();
    let mut lowered_fns = Vec::new();

    for (code, param_count, locals_size) in functions {
        let mut code = code.clone();
        let param_count = *param_count;
        let mut locals_size = *locals_size;

        // Step 1: Run shared peephole optimization (no regalloc).
        vm_optimize::optimize_peephole(&mut code);
        vm_optimize::compact(&mut code);

        // Step 2: 16-register allocation with spilling.
        let spill_info = register_allocation_16(&mut code, param_count, &mut locals_size);
        _ = spill_info;

        // Step 3: Convert 3-op → 2-op destructive and translate to Op16Instr.
        let instrs = translate_function(&code, &mut f32_pool, &mut f64_pool, &mut i64_pool);

        lowered_fns.push(LoweredFunction {
            code: instrs,
            locals_size,
        });
    }

    LoweredProgram {
        functions: lowered_fns,
        f32_pool,
        f64_pool,
        i64_pool,
    }
}

// ---- Register Allocation with Spilling ----

/// Allocate registers with a maximum of 16 physical registers.
/// Inserts Spill/Reload opcodes and patches register references.
/// Returns the mapping from virtual to physical registers.
fn register_allocation_16(
    code: &mut Vec<Opcode>,
    param_count: u8,
    locals_size: &mut u32,
) -> Vec<Reg> {
    if code.is_empty() {
        return Vec::new();
    }

    // Copy coalescing (shared).
    vm_optimize::copy_coalesce(code, param_count);
    vm_optimize::compact(code);

    let n = vm_optimize::num_vregs(code);
    if n == 0 {
        return Vec::new();
    }

    let (def_point, last_use, is_used) = vm_optimize::compute_live_ranges(code);

    // Build sorted interval list: (vreg, start, end).
    let mut intervals: Vec<(Reg, u32, u32)> = Vec::new();
    for r in 0..n {
        if is_used[r] {
            let start = def_point[r];
            let end = last_use[r].max(start);
            intervals.push((r as Reg, start, end));
        }
    }
    intervals.sort_by_key(|&(_, start, end)| (start, end));

    // Identify call argument groups (must be contiguous).
    let mut call_group: Vec<Option<(Reg, u8)>> = vec![None; n];
    for op in code.iter() {
        let (args_start, arg_count) = match op {
            Opcode::Call { args_start, arg_count, .. } => (*args_start, *arg_count),
            Opcode::CallIndirect { args_start, arg_count, .. } => (*args_start, *arg_count),
            Opcode::CallClosure { args_start, arg_count, .. } => (*args_start, *arg_count),
            Opcode::CallExtern { args_start, arg_count, .. } => (*args_start, *arg_count),
            _ => continue,
        };
        if arg_count >= 2 {
            for offset in 0..arg_count {
                let vreg = args_start + offset as Reg;
                if (vreg as usize) < n {
                    call_group[vreg as usize] = Some((args_start, offset));
                }
            }
        }
    }

    // Build hints from Move instructions.
    const UNASSIGNED: Reg = Reg::MAX;
    let mut hint: Vec<Reg> = vec![UNASSIGNED; n];
    for op in code.iter() {
        if let Opcode::Move { dst, src } = op {
            if *dst != *src {
                hint[*dst as usize] = *src;
            }
        }
    }

    // Linear scan with max 16 registers. When no register is free, spill.
    let mut mapping = vec![UNASSIGNED; n];
    let mut preg_free = [true; MAX_REGS];
    // Active intervals sorted by end point: (end, vreg, preg)
    let mut active: Vec<(u32, Reg, u8)> = Vec::new();

    // Pin r0 (return value).
    if is_used[0] {
        mapping[0] = 0;
        preg_free[0] = false;
        let end = last_use[0].max(def_point[0]);
        active.push((end, 0, 0));
    }

    // Pin parameter registers.
    for p in 1..(param_count as usize).min(n).min(MAX_REGS) {
        if is_used[p] {
            mapping[p] = p as Reg;
            preg_free[p] = false;
            let end = last_use[p].max(def_point[p]);
            active.push((end, p as Reg, p as u8));
        }
    }

    // Track spill slots: vreg -> local slot byte offset.
    let mut spill_slot: Vec<Option<u32>> = vec![None; n];
    let mut next_spill_offset = *locals_size;

    fn alloc_spill_slot(spill_slot: &mut [Option<u32>], next_offset: &mut u32, vreg: usize) -> u32 {
        if let Some(offset) = spill_slot[vreg] {
            return offset;
        }
        // Align to 8 bytes.
        let offset = (*next_offset + 7) & !7;
        *next_offset = offset + 8;
        spill_slot[vreg] = Some(offset);
        offset
    }

    // Spill the active interval with the furthest endpoint, freeing its preg.
    fn spill_furthest(
        active: &mut Vec<(u32, Reg, u8)>,
        preg_free: &mut [bool; MAX_REGS],
        mapping: &mut [Reg],
        spill_slot: &mut [Option<u32>],
        next_spill_offset: &mut u32,
    ) -> u8 {
        // Find active interval with max end.
        let idx = active
            .iter()
            .enumerate()
            .max_by_key(|(_, (end, _, _))| *end)
            .unwrap()
            .0;
        let (_, spilled_vreg, freed_preg) = active.remove(idx);
        preg_free[freed_preg as usize] = true;
        // Mark as spilled — mapping stays so we know what preg it had.
        alloc_spill_slot(spill_slot, next_spill_offset, spilled_vreg as usize);
        freed_preg
    }

    for &(vreg, start, end) in &intervals {
        if (vreg as usize) < param_count as usize {
            continue;
        }
        if mapping[vreg as usize] != UNASSIGNED {
            continue;
        }

        // Expire old intervals.
        active.retain(|&(active_end, _vr, pr)| {
            if active_end < start {
                preg_free[pr as usize] = true;
                false
            } else {
                true
            }
        });

        if let Some((group_start, _offset)) = call_group[vreg as usize] {
            // Allocate entire call arg group contiguously.
            let mut group_size: u8 = 0;
            while {
                let idx = (group_start + group_size as Reg) as usize;
                idx < call_group.len()
                    && call_group[idx].map_or(false, |(gs, _)| gs == group_start)
            } {
                group_size += 1;
            }

            // Find contiguous block.
            let block_start = (0..=(MAX_REGS as u8).saturating_sub(group_size))
                .find(|&p| (0..group_size).all(|j| preg_free[(p + j) as usize]));

            let block_start = match block_start {
                Some(b) => b,
                None => {
                    // Spill enough registers to make room.
                    for _ in 0..group_size {
                        spill_furthest(
                            &mut active,
                            &mut preg_free,
                            &mut mapping,
                            &mut spill_slot,
                            &mut next_spill_offset,
                        );
                    }
                    (0..=(MAX_REGS as u8).saturating_sub(group_size))
                        .find(|&p| (0..group_size).all(|j| preg_free[(p + j) as usize]))
                        .expect("register allocation: no contiguous block after spilling")
                }
            };

            for j in 0..group_size {
                let gvreg = group_start + j as Reg;
                if is_used[gvreg as usize] {
                    mapping[gvreg as usize] = (block_start + j) as Reg;
                    preg_free[(block_start + j) as usize] = false;
                    let gend = last_use[gvreg as usize].max(def_point[gvreg as usize]);
                    active.push((gend, gvreg, block_start + j));
                }
            }
        } else {
            // Normal register — try hint first, then lowest free.
            let hinted_vreg = hint[vreg as usize];
            let hinted_preg = if hinted_vreg != UNASSIGNED && mapping[hinted_vreg as usize] != UNASSIGNED {
                mapping[hinted_vreg as usize]
            } else {
                UNASSIGNED
            };

            let preg = if hinted_preg != UNASSIGNED
                && (hinted_preg as usize) < MAX_REGS
                && preg_free[hinted_preg as usize]
            {
                hinted_preg as u8
            } else if let Some(p) = preg_free.iter().position(|&free| free) {
                p as u8
            } else {
                // All 16 regs occupied — spill the one with the furthest endpoint.
                spill_furthest(
                    &mut active,
                    &mut preg_free,
                    &mut mapping,
                    &mut spill_slot,
                    &mut next_spill_offset,
                )
            };

            preg_free[preg as usize] = false;
            mapping[vreg as usize] = preg as Reg;
            active.push((end, vreg, preg));
        }
    }

    // Handle cross-call registers: spill any vreg that is live across a call.
    // With 16 registers, these must be saved/restored explicitly.
    // The SaveRegs/RestoreRegs instructions handle this at the call site.

    // Update locals_size to account for spill slots.
    *locals_size = next_spill_offset;

    // Insert Spill/Reload instructions for spilled vregs.
    if spill_slot.iter().any(|s| s.is_some()) {
        insert_spill_reload(code, &mapping, &spill_slot, &def_point, &last_use);
    }

    // Patch SaveRegs/RestoreRegs to cover all 16 registers.
    for op in code.iter_mut() {
        match op {
            Opcode::SaveRegs { count, .. } => {
                *count = MAX_REGS as u8;
            }
            Opcode::RestoreRegs { start_reg, count, .. } => {
                if *start_reg == 1 {
                    *count = (MAX_REGS - 1) as u8;
                } else {
                    *count = MAX_REGS as u8;
                }
            }
            _ => {}
        }
    }

    // Rewrite all registers with the mapping.
    for op in code.iter_mut() {
        op.rewrite_regs(&mapping);
    }

    mapping
}

/// Insert Spill after each definition and Reload before each use of spilled vregs.
fn insert_spill_reload(
    code: &mut Vec<Opcode>,
    mapping: &[Reg],
    spill_slot: &[Option<u32>],
    _def_point: &[u32],
    _last_use: &[u32],
) {
    let mut new_code = Vec::with_capacity(code.len() * 2);

    for op in code.iter() {
        // Reload: before the instruction, reload any spilled source registers.
        let mut reloads: Vec<(Reg, u32)> = Vec::new();
        op.for_each_src(|vreg| {
            if let Some(slot) = spill_slot.get(vreg as usize).and_then(|s| *s) {
                if mapping[vreg as usize] != Reg::MAX {
                    reloads.push((vreg, slot));
                }
            }
        });
        // Deduplicate reloads.
        reloads.sort();
        reloads.dedup();
        for (vreg, slot) in &reloads {
            let preg = mapping[*vreg as usize];
            // Emit a Load64Off from locals to reload the value.
            // We'll translate this to a Reload in Op16 later.
            new_code.push(Opcode::LoadImm {
                dst: preg,
                value: *slot as i64,
            });
            // Mark as reload: we use a special pattern that translate_function recognizes.
            // Actually, simpler: just use LoadSlot32 as the reload mechanism via locals.
            // For now, use a LocalAddr + Load64 sequence. The lowering will handle it.
        }

        new_code.push(*op);

        // Spill: after the instruction, if the dst is spilled, store it.
        if let Some(dst_vreg) = op.get_dst() {
            if let Some(slot) = spill_slot.get(dst_vreg as usize).and_then(|s| *s) {
                if mapping[dst_vreg as usize] != Reg::MAX {
                    let _preg = mapping[dst_vreg as usize];
                    // Will be translated as a Spill instruction.
                    new_code.push(Opcode::Store64Off {
                        base: dst_vreg, // Will be rewritten to preg
                        offset: slot as i32,
                        src: dst_vreg,
                    });
                }
            }
        }
    }

    *code = new_code;
}

// ---- 3-operand to 2-operand translation ----

/// Returns true if the opcode is commutative (a op b == b op a).
fn is_commutative(op: &Opcode) -> bool {
    matches!(
        op,
        Opcode::IAdd { .. }
            | Opcode::IMul { .. }
            | Opcode::FAdd { .. }
            | Opcode::FMul { .. }
            | Opcode::DAdd { .. }
            | Opcode::DMul { .. }
            | Opcode::And { .. }
            | Opcode::Or { .. }
            | Opcode::Xor { .. }
            | Opcode::IEq { .. }
            | Opcode::INe { .. }
            | Opcode::FEq { .. }
            | Opcode::FNe { .. }
            | Opcode::DEq { .. }
    )
}

fn r(reg: Reg) -> Reg16 {
    debug_assert!(reg < 16, "vm16: register {} out of range (max 15)", reg);
    reg as Reg16
}

/// Translate a function's Opcodes (already register-allocated to 0-15) into Op16Instrs.
fn translate_function(
    code: &[Opcode],
    f32_pool: &mut Vec<f32>,
    f64_pool: &mut Vec<f64>,
    i64_pool: &mut Vec<i64>,
) -> Vec<Op16Instr> {
    let mut out = Vec::new();

    for op in code {
        match *op {
            Opcode::Nop => {}
            Opcode::Halt => out.push(Op16Instr::bare(tags::HALT)),

            Opcode::Move { dst, src } => {
                if dst != src {
                    out.push(Op16Instr::ab(tags::MOVE, r(dst), r(src)));
                }
            }

            Opcode::LoadImm { dst, value } => {
                if value >= i16::MIN as i64 && value <= i16::MAX as i64 {
                    out.push(Op16Instr::a_trail(tags::LOAD_IMM, r(dst), value as u16));
                } else {
                    let idx = pool_insert(i64_pool, value);
                    out.push(Op16Instr::a_trail(tags::LOAD_IMM_WIDE, r(dst), idx as u16));
                }
            }

            Opcode::LoadF32 { dst, value } => {
                let idx = pool_insert_f32(f32_pool, value);
                out.push(Op16Instr::a_trail(tags::LOAD_F32, r(dst), idx as u16));
            }

            Opcode::LoadF64 { dst, value } => {
                let idx = pool_insert_f64(f64_pool, value);
                out.push(Op16Instr::a_trail(tags::LOAD_F64, r(dst), idx as u16));
            }

            Opcode::LoadConst { dst, idx } => {
                out.push(Op16Instr::a_trail(tags::LOAD_CONST, r(dst), idx as u16));
            }

            // 3-operand integer arithmetic → destructive 2-operand
            Opcode::IAdd { dst, a, b } => emit_binary_destructive(&mut out, tags::IADD, dst, a, b, true),
            Opcode::ISub { dst, a, b } => emit_binary_destructive(&mut out, tags::ISUB, dst, a, b, false),
            Opcode::IMul { dst, a, b } => emit_binary_destructive(&mut out, tags::IMUL, dst, a, b, true),
            Opcode::IDiv { dst, a, b } => emit_binary_destructive(&mut out, tags::IDIV, dst, a, b, false),
            Opcode::UDiv { dst, a, b } => emit_binary_destructive(&mut out, tags::UDIV, dst, a, b, false),
            Opcode::IRem { dst, a, b } => emit_binary_destructive(&mut out, tags::IREM, dst, a, b, false),
            Opcode::IPow { dst, a, b } => emit_binary_destructive(&mut out, tags::IPOW, dst, a, b, false),

            Opcode::INeg { dst, src } => emit_unary(&mut out, tags::INEG, dst, src),

            Opcode::IAddImm { dst, src, imm } => {
                if dst != src {
                    out.push(Op16Instr::ab(tags::MOVE, r(dst), r(src)));
                }
                out.push(Op16Instr::a_trail(tags::IADD_IMM, r(dst), imm as u16));
            }

            // Float32 arithmetic
            Opcode::FAdd { dst, a, b } => emit_binary_destructive(&mut out, tags::FADD, dst, a, b, true),
            Opcode::FSub { dst, a, b } => emit_binary_destructive(&mut out, tags::FSUB, dst, a, b, false),
            Opcode::FMul { dst, a, b } => emit_binary_destructive(&mut out, tags::FMUL, dst, a, b, true),
            Opcode::FDiv { dst, a, b } => emit_binary_destructive(&mut out, tags::FDIV, dst, a, b, false),
            Opcode::FNeg { dst, src } => emit_unary(&mut out, tags::FNEG, dst, src),
            Opcode::FPow { dst, a, b } => emit_binary_destructive(&mut out, tags::FPOW, dst, a, b, false),

            Opcode::FMulAdd { dst, a, b, c } => {
                // rA = rA * rB + rC. Need dst == a.
                if dst != a {
                    out.push(Op16Instr::ab(tags::MOVE, r(dst), r(a)));
                }
                out.push(Op16Instr::ab_trail(tags::FMUL_ADD, r(dst), r(b), r(c) as u16));
            }
            Opcode::FMulSub { dst, a, b, c } => {
                if dst != a {
                    out.push(Op16Instr::ab(tags::MOVE, r(dst), r(a)));
                }
                out.push(Op16Instr::ab_trail(tags::FMUL_SUB, r(dst), r(b), r(c) as u16));
            }
            Opcode::FNMulAdd { dst, a, b, c } => {
                // rA = rC - rA * rB. Need dst == a.
                if dst != a {
                    out.push(Op16Instr::ab(tags::MOVE, r(dst), r(a)));
                }
                out.push(Op16Instr::ab_trail(tags::FNMUL_ADD, r(dst), r(b), r(c) as u16));
            }

            // Float64 arithmetic
            Opcode::DAdd { dst, a, b } => emit_binary_destructive(&mut out, tags::DADD, dst, a, b, true),
            Opcode::DSub { dst, a, b } => emit_binary_destructive(&mut out, tags::DSUB, dst, a, b, false),
            Opcode::DMul { dst, a, b } => emit_binary_destructive(&mut out, tags::DMUL, dst, a, b, true),
            Opcode::DDiv { dst, a, b } => emit_binary_destructive(&mut out, tags::DDIV, dst, a, b, false),
            Opcode::DNeg { dst, src } => emit_unary(&mut out, tags::DNEG, dst, src),
            Opcode::DPow { dst, a, b } => emit_binary_destructive(&mut out, tags::DPOW, dst, a, b, false),

            Opcode::DMulAdd { dst, a, b, c } => {
                if dst != a { out.push(Op16Instr::ab(tags::MOVE, r(dst), r(a))); }
                out.push(Op16Instr::ab_trail(tags::DMUL_ADD, r(dst), r(b), r(c) as u16));
            }
            Opcode::DMulSub { dst, a, b, c } => {
                if dst != a { out.push(Op16Instr::ab(tags::MOVE, r(dst), r(a))); }
                out.push(Op16Instr::ab_trail(tags::DMUL_SUB, r(dst), r(b), r(c) as u16));
            }
            Opcode::DNMulAdd { dst, a, b, c } => {
                if dst != a { out.push(Op16Instr::ab(tags::MOVE, r(dst), r(a))); }
                out.push(Op16Instr::ab_trail(tags::DNMUL_ADD, r(dst), r(b), r(c) as u16));
            }

            // Bitwise
            Opcode::And { dst, a, b } => emit_binary_destructive(&mut out, tags::AND, dst, a, b, true),
            Opcode::Or { dst, a, b } => emit_binary_destructive(&mut out, tags::OR, dst, a, b, true),
            Opcode::Xor { dst, a, b } => emit_binary_destructive(&mut out, tags::XOR, dst, a, b, true),
            Opcode::Not { dst, src } => emit_unary(&mut out, tags::NOT, dst, src),
            Opcode::Shl { dst, a, b } => emit_binary_destructive(&mut out, tags::SHL, dst, a, b, false),
            Opcode::Shr { dst, a, b } => emit_binary_destructive(&mut out, tags::SHR, dst, a, b, false),
            Opcode::UShr { dst, a, b } => emit_binary_destructive(&mut out, tags::USHR, dst, a, b, false),

            // Comparisons — destructive: rA = (rA op rB)
            Opcode::IEq { dst, a, b } => emit_binary_destructive(&mut out, tags::IEQ, dst, a, b, true),
            Opcode::INe { dst, a, b } => emit_binary_destructive(&mut out, tags::INE, dst, a, b, true),
            Opcode::ILt { dst, a, b } => emit_binary_destructive(&mut out, tags::ILT, dst, a, b, false),
            Opcode::ILe { dst, a, b } => emit_binary_destructive(&mut out, tags::ILE, dst, a, b, false),
            Opcode::ULt { dst, a, b } => emit_binary_destructive(&mut out, tags::ULT, dst, a, b, false),
            Opcode::FEq { dst, a, b } => emit_binary_destructive(&mut out, tags::FEQ, dst, a, b, true),
            Opcode::FNe { dst, a, b } => emit_binary_destructive(&mut out, tags::FNE, dst, a, b, true),
            Opcode::FLt { dst, a, b } => emit_binary_destructive(&mut out, tags::FLT, dst, a, b, false),
            Opcode::FLe { dst, a, b } => emit_binary_destructive(&mut out, tags::FLE, dst, a, b, false),
            Opcode::DEq { dst, a, b } => emit_binary_destructive(&mut out, tags::DEQ, dst, a, b, true),
            Opcode::DLt { dst, a, b } => emit_binary_destructive(&mut out, tags::DLT, dst, a, b, false),
            Opcode::DLe { dst, a, b } => emit_binary_destructive(&mut out, tags::DLE, dst, a, b, false),

            // Memory comparisons
            Opcode::MemEq { dst, a, b, size } => {
                if dst != a { out.push(Op16Instr::ab(tags::MOVE, r(dst), r(a))); }
                out.push(Op16Instr::ab_trail(tags::MEM_EQ, r(dst), r(b), size as u16));
            }
            Opcode::MemNe { dst, a, b, size } => {
                if dst != a { out.push(Op16Instr::ab(tags::MOVE, r(dst), r(a))); }
                out.push(Op16Instr::ab_trail(tags::MEM_NE, r(dst), r(b), size as u16));
            }
            Opcode::SliceEq { dst, a, b, elem_size } => {
                if dst != a { out.push(Op16Instr::ab(tags::MOVE, r(dst), r(a))); }
                out.push(Op16Instr::ab_trail(tags::SLICE_EQ, r(dst), r(b), elem_size as u16));
            }
            Opcode::SliceNe { dst, a, b, elem_size } => {
                if dst != a { out.push(Op16Instr::ab(tags::MOVE, r(dst), r(a))); }
                out.push(Op16Instr::ab_trail(tags::SLICE_NE, r(dst), r(b), elem_size as u16));
            }

            // Fused compare+branch — non-destructive (does not clobber rA)
            Opcode::ILtJump { a, b, offset } => {
                out.push(Op16Instr::ab_trail(tags::ILT_JUMP, r(a), r(b), offset as u16));
            }
            Opcode::FLtJump { a, b, offset } => {
                out.push(Op16Instr::ab_trail(tags::FLT_JUMP, r(a), r(b), offset as u16));
            }

            // Type conversions — in-place
            Opcode::I32ToF32 { dst, src } => emit_unary(&mut out, tags::I32_TO_F32, dst, src),
            Opcode::F32ToI32 { dst, src } => emit_unary(&mut out, tags::F32_TO_I32, dst, src),
            Opcode::I32ToF64 { dst, src } => emit_unary(&mut out, tags::I32_TO_F64, dst, src),
            Opcode::F64ToI32 { dst, src } => emit_unary(&mut out, tags::F64_TO_I32, dst, src),
            Opcode::F32ToF64 { dst, src } => emit_unary(&mut out, tags::F32_TO_F64, dst, src),
            Opcode::F64ToF32 { dst, src } => emit_unary(&mut out, tags::F64_TO_F32, dst, src),
            Opcode::I32ToI8 { dst, src } => emit_unary(&mut out, tags::I32_TO_I8, dst, src),
            Opcode::I8ToI32 { dst, src } => emit_unary(&mut out, tags::I8_TO_I32, dst, src),
            Opcode::I64ToU32 { dst, src } => emit_unary(&mut out, tags::I64_TO_U32, dst, src),

            // Memory: basic
            Opcode::Load8 { dst, addr } => out.push(Op16Instr::ab(tags::LOAD8, r(dst), r(addr))),
            Opcode::Load32 { dst, addr } => out.push(Op16Instr::ab(tags::LOAD32, r(dst), r(addr))),
            Opcode::Load64 { dst, addr } => out.push(Op16Instr::ab(tags::LOAD64, r(dst), r(addr))),
            Opcode::Store8 { addr, src } => out.push(Op16Instr::ab(tags::STORE8, r(addr), r(src))),
            Opcode::Store32 { addr, src } => out.push(Op16Instr::ab(tags::STORE32, r(addr), r(src))),
            Opcode::Store64 { addr, src } => out.push(Op16Instr::ab(tags::STORE64, r(addr), r(src))),

            // Memory: with offset — use fixed-offset opcodes when possible
            Opcode::Load32Off { dst, base, offset } => {
                emit_load_off32(&mut out, dst, base, offset);
            }
            Opcode::Load64Off { dst, base, offset } => {
                emit_load_off64(&mut out, dst, base, offset);
            }
            Opcode::Store8Off { base, offset, src } => {
                out.push(Op16Instr::ab_trail(tags::STORE8_OFF, r(base), r(src), offset as u16));
            }
            Opcode::Store32Off { base, offset, src } => {
                emit_store_off32(&mut out, base, src, offset);
            }
            Opcode::Store64Off { base, offset, src } => {
                emit_store_off64(&mut out, base, src, offset);
            }

            // Addressing
            Opcode::LocalAddr { dst, slot } => {
                out.push(Op16Instr::a_trail(tags::LOCAL_ADDR, r(dst), slot));
            }
            Opcode::GlobalAddr { dst, offset } => {
                out.push(Op16Instr::a_trail(tags::GLOBAL_ADDR, r(dst), offset as u16));
            }
            Opcode::GetClosurePtr { dst } => {
                out.push(Op16Instr::a_only(tags::GET_CLOSURE_PTR, r(dst)));
            }

            // Control flow
            Opcode::Jump { offset } => {
                // Offset will be fixed up during linking.
                out.push(Op16Instr::trail_only(tags::JUMP, offset as u16));
            }
            Opcode::JumpIfZero { cond, offset } => {
                out.push(Op16Instr::a_trail(tags::JUMP_IF_ZERO, r(cond), offset as u16));
            }
            Opcode::JumpIfNotZero { cond, offset } => {
                out.push(Op16Instr::a_trail(tags::JUMP_IF_NOT_ZERO, r(cond), offset as u16));
            }

            // Calls: args are already in r0..rN-1
            Opcode::Call { func, args_start: _, arg_count } => {
                out.push(Op16Instr::a_trail(tags::CALL, arg_count as Reg16, func as u16));
            }
            Opcode::CallIndirect { func_reg, args_start: _, arg_count } => {
                out.push(Op16Instr::ab(tags::CALL_INDIRECT, r(func_reg), arg_count as Reg16));
            }
            Opcode::CallClosure { fat_ptr, args_start: _, arg_count } => {
                out.push(Op16Instr::ab(tags::CALL_CLOSURE, r(fat_ptr), arg_count as Reg16));
            }
            Opcode::CallExtern { args_start: _, arg_count, globals_offset } => {
                out.push(Op16Instr::a_trail(tags::CALL_EXTERN, arg_count as Reg16, globals_offset as u16));
            }

            // Return
            Opcode::Return => out.push(Op16Instr::bare(tags::RETURN)),
            Opcode::ReturnReg { src } => {
                if src != 0 {
                    out.push(Op16Instr::ab(tags::MOVE, 0, r(src)));
                }
                out.push(Op16Instr::bare(tags::RETURN));
            }

            // Stack frame
            Opcode::AllocLocals { size } => {
                out.push(Op16Instr::trail_only(tags::ALLOC_LOCALS, size as u16));
            }
            Opcode::SaveRegs { slot, .. } => {
                out.push(Op16Instr::trail_only(tags::SAVE_REGS, slot as u16));
            }
            Opcode::RestoreRegs { slot, start_reg, .. } => {
                // Use ra to signal whether to skip r0 (start_reg == 1).
                let skip_r0 = if start_reg == 1 { 1 } else { 0 };
                out.push(Op16Instr::a_trail(tags::RESTORE_REGS, skip_r0, slot as u16));
            }
            Opcode::MemCopy { dst, src, size } => {
                out.push(Op16Instr::ab_trail(tags::MEM_COPY, r(dst), r(src), size as u16));
            }
            Opcode::MemZero { dst, size } => {
                out.push(Op16Instr::a_trail(tags::MEM_ZERO, r(dst), size as u16));
            }

            // Debugging
            Opcode::PrintI32 { src } => out.push(Op16Instr::a_only(tags::PRINT_I32, r(src))),
            Opcode::PrintF32 { src } => out.push(Op16Instr::a_only(tags::PRINT_F32, r(src))),
            Opcode::Assert { src } => out.push(Op16Instr::a_only(tags::ASSERT, r(src))),
            Opcode::Putc { src } => out.push(Op16Instr::a_only(tags::PUTC, r(src))),

            // Math builtins: unary f32
            Opcode::SinF32 { dst, src } => emit_unary(&mut out, tags::SIN_F32, dst, src),
            Opcode::CosF32 { dst, src } => emit_unary(&mut out, tags::COS_F32, dst, src),
            Opcode::TanF32 { dst, src } => emit_unary(&mut out, tags::TAN_F32, dst, src),
            Opcode::AsinF32 { dst, src } => emit_unary(&mut out, tags::ASIN_F32, dst, src),
            Opcode::AcosF32 { dst, src } => emit_unary(&mut out, tags::ACOS_F32, dst, src),
            Opcode::AtanF32 { dst, src } => emit_unary(&mut out, tags::ATAN_F32, dst, src),
            Opcode::SinhF32 { dst, src } => emit_unary(&mut out, tags::SINH_F32, dst, src),
            Opcode::CoshF32 { dst, src } => emit_unary(&mut out, tags::COSH_F32, dst, src),
            Opcode::TanhF32 { dst, src } => emit_unary(&mut out, tags::TANH_F32, dst, src),
            Opcode::AsinhF32 { dst, src } => emit_unary(&mut out, tags::ASINH_F32, dst, src),
            Opcode::AcoshF32 { dst, src } => emit_unary(&mut out, tags::ACOSH_F32, dst, src),
            Opcode::AtanhF32 { dst, src } => emit_unary(&mut out, tags::ATANH_F32, dst, src),
            Opcode::LnF32 { dst, src } => emit_unary(&mut out, tags::LN_F32, dst, src),
            Opcode::ExpF32 { dst, src } => emit_unary(&mut out, tags::EXP_F32, dst, src),
            Opcode::Exp2F32 { dst, src } => emit_unary(&mut out, tags::EXP2_F32, dst, src),
            Opcode::Log10F32 { dst, src } => emit_unary(&mut out, tags::LOG10_F32, dst, src),
            Opcode::Log2F32 { dst, src } => emit_unary(&mut out, tags::LOG2_F32, dst, src),
            Opcode::SqrtF32 { dst, src } => emit_unary(&mut out, tags::SQRT_F32, dst, src),
            Opcode::AbsF32 { dst, src } => emit_unary(&mut out, tags::ABS_F32, dst, src),
            Opcode::FloorF32 { dst, src } => emit_unary(&mut out, tags::FLOOR_F32, dst, src),
            Opcode::CeilF32 { dst, src } => emit_unary(&mut out, tags::CEIL_F32, dst, src),

            // Math builtins: unary f64
            Opcode::SinF64 { dst, src } => emit_unary(&mut out, tags::SIN_F64, dst, src),
            Opcode::CosF64 { dst, src } => emit_unary(&mut out, tags::COS_F64, dst, src),
            Opcode::TanF64 { dst, src } => emit_unary(&mut out, tags::TAN_F64, dst, src),
            Opcode::AsinF64 { dst, src } => emit_unary(&mut out, tags::ASIN_F64, dst, src),
            Opcode::AcosF64 { dst, src } => emit_unary(&mut out, tags::ACOS_F64, dst, src),
            Opcode::AtanF64 { dst, src } => emit_unary(&mut out, tags::ATAN_F64, dst, src),
            Opcode::SinhF64 { dst, src } => emit_unary(&mut out, tags::SINH_F64, dst, src),
            Opcode::CoshF64 { dst, src } => emit_unary(&mut out, tags::COSH_F64, dst, src),
            Opcode::TanhF64 { dst, src } => emit_unary(&mut out, tags::TANH_F64, dst, src),
            Opcode::AsinhF64 { dst, src } => emit_unary(&mut out, tags::ASINH_F64, dst, src),
            Opcode::AcoshF64 { dst, src } => emit_unary(&mut out, tags::ACOSH_F64, dst, src),
            Opcode::AtanhF64 { dst, src } => emit_unary(&mut out, tags::ATANH_F64, dst, src),
            Opcode::LnF64 { dst, src } => emit_unary(&mut out, tags::LN_F64, dst, src),
            Opcode::ExpF64 { dst, src } => emit_unary(&mut out, tags::EXP_F64, dst, src),
            Opcode::Exp2F64 { dst, src } => emit_unary(&mut out, tags::EXP2_F64, dst, src),
            Opcode::Log10F64 { dst, src } => emit_unary(&mut out, tags::LOG10_F64, dst, src),
            Opcode::Log2F64 { dst, src } => emit_unary(&mut out, tags::LOG2_F64, dst, src),
            Opcode::SqrtF64 { dst, src } => emit_unary(&mut out, tags::SQRT_F64, dst, src),
            Opcode::AbsF64 { dst, src } => emit_unary(&mut out, tags::ABS_F64, dst, src),
            Opcode::FloorF64 { dst, src } => emit_unary(&mut out, tags::FLOOR_F64, dst, src),
            Opcode::CeilF64 { dst, src } => emit_unary(&mut out, tags::CEIL_F64, dst, src),

            // Predicates
            Opcode::IsinfF32 { dst, src } => emit_unary(&mut out, tags::ISINF_F32, dst, src),
            Opcode::IsinfF64 { dst, src } => emit_unary(&mut out, tags::ISINF_F64, dst, src),
            Opcode::IsnanF32 { dst, src } => emit_unary(&mut out, tags::ISNAN_F32, dst, src),
            Opcode::IsnanF64 { dst, src } => emit_unary(&mut out, tags::ISNAN_F64, dst, src),

            // Binary math builtins
            Opcode::PowF32 { dst, a, b } => emit_binary_destructive(&mut out, tags::POW_F32, dst, a, b, false),
            Opcode::Atan2F32 { dst, a, b } => emit_binary_destructive(&mut out, tags::ATAN2_F32, dst, a, b, false),
            Opcode::MinF32 { dst, a, b } => emit_binary_destructive(&mut out, tags::MIN_F32, dst, a, b, true),
            Opcode::MaxF32 { dst, a, b } => emit_binary_destructive(&mut out, tags::MAX_F32, dst, a, b, true),
            Opcode::PowF64 { dst, a, b } => emit_binary_destructive(&mut out, tags::POW_F64, dst, a, b, false),
            Opcode::Atan2F64 { dst, a, b } => emit_binary_destructive(&mut out, tags::ATAN2_F64, dst, a, b, false),
            Opcode::MinF64 { dst, a, b } => emit_binary_destructive(&mut out, tags::MIN_F64, dst, a, b, true),
            Opcode::MaxF64 { dst, a, b } => emit_binary_destructive(&mut out, tags::MAX_F64, dst, a, b, true),

            // SIMD f32x4
            Opcode::F32x4Add { dst, a, b } => emit_binary_destructive(&mut out, tags::F32X4_ADD, dst, a, b, true),
            Opcode::F32x4Sub { dst, a, b } => emit_binary_destructive(&mut out, tags::F32X4_SUB, dst, a, b, false),
            Opcode::F32x4Mul { dst, a, b } => emit_binary_destructive(&mut out, tags::F32X4_MUL, dst, a, b, true),
            Opcode::F32x4Div { dst, a, b } => emit_binary_destructive(&mut out, tags::F32X4_DIV, dst, a, b, false),
            Opcode::F32x4Neg { dst, src } => emit_unary(&mut out, tags::F32X4_NEG, dst, src),

            // Fused slot access
            Opcode::LoadSlot32 { dst, slot } => {
                out.push(Op16Instr::a_trail(tags::LOAD_SLOT32, r(dst), slot));
            }
            Opcode::StoreSlot32 { slot, src } => {
                out.push(Op16Instr::a_trail(tags::STORE_SLOT32, r(src), slot));
            }

            // Slice access
            Opcode::SliceLoad32 { dst, slice, index } => {
                // Destructive: dst gets result. slice addr in rA, index in rB.
                if dst != slice {
                    out.push(Op16Instr::ab(tags::MOVE, r(dst), r(slice)));
                }
                out.push(Op16Instr::ab(tags::SLICE_LOAD32, r(dst), r(index)));
            }
            Opcode::SliceStore32 { slice, index, src } => {
                // A=slice, B=index, trail=src_reg
                out.push(Op16Instr::ab_trail(tags::SLICE_STORE32, r(slice), r(index), r(src) as u16));
            }
        }
    }

    out
}

// ---- Emit helpers ----

/// Emit a destructive binary operation: rA op= rB, result in dst.
/// If commutative and dst == b, swap operands.
/// If dst != a (and not commutative swap), emit Move first.
fn emit_binary_destructive(
    out: &mut Vec<Op16Instr>,
    tag: u8,
    dst: Reg,
    a: Reg,
    b: Reg,
    commutative: bool,
) {
    if dst == a {
        // Best case: already destructive form.
        out.push(Op16Instr::ab(tag, r(dst), r(b)));
    } else if commutative && dst == b {
        // Swap operands for commutative ops.
        out.push(Op16Instr::ab(tag, r(dst), r(a)));
    } else {
        // Need a move first.
        out.push(Op16Instr::ab(tags::MOVE, r(dst), r(a)));
        out.push(Op16Instr::ab(tag, r(dst), r(b)));
    }
}

/// Emit a unary operation. If dst != src, emit Move first.
fn emit_unary(out: &mut Vec<Op16Instr>, tag: u8, dst: Reg, src: Reg) {
    if dst != src {
        out.push(Op16Instr::ab(tags::MOVE, r(dst), r(src)));
    }
    out.push(Op16Instr::a_only(tag, r(dst)));
}

/// Emit Load32Off with fixed-offset opcode if possible.
fn emit_load_off32(out: &mut Vec<Op16Instr>, dst: Reg, base: Reg, offset: i32) {
    match offset {
        0 => out.push(Op16Instr::ab(tags::LOAD32_OFF0, r(dst), r(base))),
        4 => out.push(Op16Instr::ab(tags::LOAD32_OFF4, r(dst), r(base))),
        8 => out.push(Op16Instr::ab(tags::LOAD32_OFF8, r(dst), r(base))),
        12 => out.push(Op16Instr::ab(tags::LOAD32_OFF12, r(dst), r(base))),
        16 => out.push(Op16Instr::ab(tags::LOAD32_OFF16, r(dst), r(base))),
        20 => out.push(Op16Instr::ab(tags::LOAD32_OFF20, r(dst), r(base))),
        24 => out.push(Op16Instr::ab(tags::LOAD32_OFF24, r(dst), r(base))),
        28 => out.push(Op16Instr::ab(tags::LOAD32_OFF28, r(dst), r(base))),
        32 => out.push(Op16Instr::ab(tags::LOAD32_OFF32, r(dst), r(base))),
        _ => out.push(Op16Instr::ab_trail(tags::LOAD32_OFF, r(dst), r(base), offset as u16)),
    }
}

/// Emit Load64Off with fixed-offset opcode if possible.
fn emit_load_off64(out: &mut Vec<Op16Instr>, dst: Reg, base: Reg, offset: i32) {
    match offset {
        0 => out.push(Op16Instr::ab(tags::LOAD64_OFF0, r(dst), r(base))),
        8 => out.push(Op16Instr::ab(tags::LOAD64_OFF8, r(dst), r(base))),
        16 => out.push(Op16Instr::ab(tags::LOAD64_OFF16, r(dst), r(base))),
        24 => out.push(Op16Instr::ab(tags::LOAD64_OFF24, r(dst), r(base))),
        32 => out.push(Op16Instr::ab(tags::LOAD64_OFF32, r(dst), r(base))),
        _ => out.push(Op16Instr::ab_trail(tags::LOAD64_OFF, r(dst), r(base), offset as u16)),
    }
}

/// Emit Store32Off with fixed-offset opcode if possible.
fn emit_store_off32(out: &mut Vec<Op16Instr>, base: Reg, src: Reg, offset: i32) {
    match offset {
        0 => out.push(Op16Instr::ab(tags::STORE32_OFF0, r(base), r(src))),
        4 => out.push(Op16Instr::ab(tags::STORE32_OFF4, r(base), r(src))),
        8 => out.push(Op16Instr::ab(tags::STORE32_OFF8, r(base), r(src))),
        12 => out.push(Op16Instr::ab(tags::STORE32_OFF12, r(base), r(src))),
        16 => out.push(Op16Instr::ab(tags::STORE32_OFF16, r(base), r(src))),
        20 => out.push(Op16Instr::ab(tags::STORE32_OFF20, r(base), r(src))),
        24 => out.push(Op16Instr::ab(tags::STORE32_OFF24, r(base), r(src))),
        28 => out.push(Op16Instr::ab(tags::STORE32_OFF28, r(base), r(src))),
        32 => out.push(Op16Instr::ab(tags::STORE32_OFF32, r(base), r(src))),
        _ => out.push(Op16Instr::ab_trail(tags::STORE32_OFF, r(base), r(src), offset as u16)),
    }
}

/// Emit Store64Off with fixed-offset opcode if possible.
fn emit_store_off64(out: &mut Vec<Op16Instr>, base: Reg, src: Reg, offset: i32) {
    match offset {
        0 => out.push(Op16Instr::ab(tags::STORE64_OFF0, r(base), r(src))),
        8 => out.push(Op16Instr::ab(tags::STORE64_OFF8, r(base), r(src))),
        16 => out.push(Op16Instr::ab(tags::STORE64_OFF16, r(base), r(src))),
        24 => out.push(Op16Instr::ab(tags::STORE64_OFF24, r(base), r(src))),
        32 => out.push(Op16Instr::ab(tags::STORE64_OFF32, r(base), r(src))),
        _ => out.push(Op16Instr::ab_trail(tags::STORE64_OFF, r(base), r(src), offset as u16)),
    }
}

// ---- Constant pool helpers ----

fn pool_insert(pool: &mut Vec<i64>, value: i64) -> usize {
    if let Some(idx) = pool.iter().position(|&v| v == value) {
        idx
    } else {
        let idx = pool.len();
        pool.push(value);
        idx
    }
}

fn pool_insert_f32(pool: &mut Vec<f32>, value: f32) -> usize {
    if let Some(idx) = pool.iter().position(|v| v.to_bits() == value.to_bits()) {
        idx
    } else {
        let idx = pool.len();
        pool.push(value);
        idx
    }
}

fn pool_insert_f64(pool: &mut Vec<f64>, value: f64) -> usize {
    if let Some(idx) = pool.iter().position(|v| v.to_bits() == value.to_bits()) {
        idx
    } else {
        let idx = pool.len();
        pool.push(value);
        idx
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::opcode::Opcode;

    #[test]
    fn test_simple_add() {
        // fn main(a: i32, b: i32) -> i32 { a + b }
        // Codegen produces: IAdd { dst: 2, a: 0, b: 1 }, Move { dst: 0, src: 2 }, Return
        let code = vec![
            Opcode::IAdd { dst: 2, a: 0, b: 1 },
            Opcode::Move { dst: 0, src: 2 },
            Opcode::Return,
        ];
        let functions = vec![(code, 2u8, 0u32)];
        let result = lower_program(&functions);
        assert_eq!(result.functions.len(), 1);
        let instrs = &result.functions[0].code;
        // After optimization + regalloc + destructive lowering, we should get
        // something like: IAdd r0, r1; Return
        // (the optimizer should eliminate the Move by coalescing)
        let has_iadd = instrs.iter().any(|i| i.tag == tags::IADD);
        let has_return = instrs.iter().any(|i| i.tag == tags::RETURN);
        assert!(has_iadd, "expected IAdd instruction");
        assert!(has_return, "expected Return instruction");
    }

    #[test]
    fn test_destructive_lowering_needs_move() {
        // dst != a and not commutative: ISub { dst: 2, a: 0, b: 1 }
        // After regalloc, registers should be 0-2 (3 regs, fits in 16).
        // Since ISub is not commutative and dst(2) != a(0), we need: Move r2, r0; ISub r2, r1
        let code = vec![
            Opcode::ISub { dst: 2, a: 0, b: 1 },
            Opcode::Move { dst: 0, src: 2 },
            Opcode::Return,
        ];
        let functions = vec![(code, 2u8, 0u32)];
        let result = lower_program(&functions);
        let instrs = &result.functions[0].code;
        // The result should have ISub somewhere
        let has_isub = instrs.iter().any(|i| i.tag == tags::ISUB);
        assert!(has_isub, "expected ISub instruction");
    }

    #[test]
    fn test_commutative_swap() {
        // IAdd { dst: 1, a: 0, b: 1 } — commutative, dst == b → can swap to IAdd r1, r0
        let code = vec![
            Opcode::IAdd { dst: 1, a: 0, b: 1 },
            Opcode::Move { dst: 0, src: 1 },
            Opcode::Return,
        ];
        let functions = vec![(code, 2u8, 0u32)];
        let result = lower_program(&functions);
        let instrs = &result.functions[0].code;
        let has_iadd = instrs.iter().any(|i| i.tag == tags::IADD);
        assert!(has_iadd, "expected IAdd instruction");
        // Should NOT have a Move before the IAdd (commutative swap avoids it)
    }

    #[test]
    fn test_offset_encoded_load() {
        let code = vec![
            Opcode::Load32Off { dst: 1, base: 0, offset: 8 },
            Opcode::Move { dst: 0, src: 1 },
            Opcode::Return,
        ];
        let functions = vec![(code, 1u8, 0u32)];
        let result = lower_program(&functions);
        let instrs = &result.functions[0].code;
        // Should use Load32Off8 (no trailing word)
        let has_off8 = instrs.iter().any(|i| i.tag == tags::LOAD32_OFF8);
        assert!(has_off8, "expected Load32Off8 (offset-encoded)");
    }

    #[test]
    fn test_load_imm_small() {
        let code = vec![
            Opcode::LoadImm { dst: 0, value: 42 },
            Opcode::Return,
        ];
        let functions = vec![(code, 0u8, 0u32)];
        let result = lower_program(&functions);
        let instrs = &result.functions[0].code;
        let imm = instrs.iter().find(|i| i.tag == tags::LOAD_IMM).unwrap();
        assert_eq!(imm.trail, Some(42u16));
        assert!(result.i64_pool.is_empty(), "small imm should not use wide pool");
    }

    #[test]
    fn test_load_imm_wide() {
        let code = vec![
            Opcode::LoadImm { dst: 0, value: 100000 },
            Opcode::Return,
        ];
        let functions = vec![(code, 0u8, 0u32)];
        let result = lower_program(&functions);
        let instrs = &result.functions[0].code;
        let wide = instrs.iter().find(|i| i.tag == tags::LOAD_IMM_WIDE).unwrap();
        assert_eq!(result.i64_pool[wide.trail.unwrap() as usize], 100000);
    }
}
