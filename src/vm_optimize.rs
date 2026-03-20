//! Peephole optimizer for VM bytecode.
//!
//! Runs as a post-pass on `Vec<Opcode>` after codegen, before execution.
//! Uses NOP replacement (not removal) so jump offsets stay valid.

use crate::vm::{Opcode, Reg};
use std::collections::HashSet;

/// Sentinel value for unassigned registers in mapping vectors.
const UNASSIGNED: Reg = Reg::MAX;

/// Collect all jump target indices in the code.
fn jump_targets(code: &[Opcode]) -> HashSet<usize> {
    let mut targets = HashSet::new();
    for (i, op) in code.iter().enumerate() {
        let offset = match op {
            Opcode::Jump { offset }
            | Opcode::JumpIfZero { offset, .. }
            | Opcode::JumpIfNotZero { offset, .. }
            | Opcode::ILtJump { offset, .. }
            | Opcode::FLtJump { offset, .. } => Some(*offset),
            _ => None,
        };
        if let Some(off) = offset {
            let target = (i as i32 + 1 + off) as usize;
            targets.insert(target);
        }
    }
    targets
}

/// Find the number of virtual registers used in the code (max reg + 1).
fn num_vregs(code: &[Opcode]) -> usize {
    let mut max_r: Reg = 0;
    for op in code {
        if let Some(dst) = op.get_dst() {
            max_r = max_r.max(dst);
        }
        op.for_each_src(|r| {
            max_r = max_r.max(r);
        });
    }
    max_r as usize + 1
}

/// Phase 1: Peephole optimizations (NOP-safe, preserves instruction indices).
/// Returns (new_reg_count, vreg_to_preg_mapping) after register allocation.
pub fn optimize(code: &mut Vec<Opcode>, param_count: u8) -> Option<(u8, Vec<Reg>)> {
    if code.is_empty() {
        return None;
    }
    // Run optimization passes iteratively until no more changes.
    for _ in 0..3 {
        move_forwarding(code);
        copy_propagation(code);
        redundant_local_addr(code);
        dead_code_elimination(code);
    }
    // Fuse LocalAddr+Load/Store into superinstructions (after all other opts)
    fuse_local_access(code);
    // Fuse IAddImm+Load/Store into offset-addressing superinstructions
    fuse_offset_access(code);
    // Fuse compare+branch into single instructions
    fuse_compare_branch(code);
    // Register allocation: compact register numbering via linear scan
    let (count, mapping) = register_allocation(code, param_count);
    Some((count, mapping))
}

/// Dead code elimination: NOP any instruction that writes to a register
/// that is never read.
fn dead_code_elimination(code: &mut Vec<Opcode>) {
    let mut uses = compute_use_counts(code);
    // Register 0 is the implicit return value register — always considered live
    uses[0] = uses[0].saturating_add(1);
    for i in 0..code.len() {
        if let Some(dst) = code[i].get_dst() {
            if uses[dst as usize] == 0 {
                // Don't eliminate instructions with side effects
                match &code[i] {
                    Opcode::Call { .. }
                    | Opcode::CallIndirect { .. }
                    | Opcode::CallClosure { .. }
                    | Opcode::Store8 { .. }
                    | Opcode::Store32 { .. }
                    | Opcode::Store64 { .. }
                    | Opcode::Store8Off { .. }
                    | Opcode::Store32Off { .. }
                    | Opcode::Store64Off { .. }
                    | Opcode::MemCopy { .. }
                    | Opcode::MemZero { .. }
                    | Opcode::PrintI32 { .. }
                    | Opcode::PrintF32 { .. }
                    | Opcode::Assert { .. }
                    | Opcode::Putc { .. } => {}
                    _ => {
                        code[i] = Opcode::Nop;
                    }
                }
            }
        }
    }
}

/// Phase 2: Strip NOPs and adjust jump offsets. Call after all patching is done.
pub fn compact(code: &mut Vec<Opcode>) {
    strip_nops(code);
}

/// Pass 3: Remove all Nop instructions and adjust jump offsets.
fn strip_nops(code: &mut Vec<Opcode>) {
    let n = code.len();
    // Build a mapping from old index to new index
    let mut new_index = vec![0usize; n];
    let mut new_pos = 0usize;
    for i in 0..n {
        new_index[i] = new_pos;
        if !matches!(code[i], Opcode::Nop) {
            new_pos += 1;
        }
    }
    let new_len = new_pos;
    if new_len == n {
        return; // No Nops to strip
    }

    // Adjust jump offsets. Jump offset semantics: target = current_pos + 1 + offset
    for i in 0..n {
        if matches!(code[i], Opcode::Nop) {
            continue;
        }
        match &mut code[i] {
            Opcode::Jump { offset } => {
                let old_target = (i as i32 + 1 + *offset) as usize;
                let new_src = new_index[i] as i32;
                let new_tgt = new_index[old_target] as i32;
                *offset = new_tgt - new_src - 1;
            }
            Opcode::JumpIfZero { offset, .. }
            | Opcode::JumpIfNotZero { offset, .. }
            | Opcode::ILtJump { offset, .. }
            | Opcode::FLtJump { offset, .. } => {
                let old_target = (i as i32 + 1 + *offset) as usize;
                let new_src = new_index[i] as i32;
                let new_tgt = new_index[old_target] as i32;
                *offset = new_tgt - new_src - 1;
            }
            _ => {}
        }
    }

    // Compact: remove Nops
    let mut write = 0;
    for read in 0..n {
        if !matches!(code[read], Opcode::Nop) {
            code[write] = code[read].clone();
            write += 1;
        }
    }
    code.truncate(new_len);
}

/// Count how many times each register is read as a source operand.
fn compute_use_counts(code: &[Opcode]) -> Vec<u16> {
    let n = num_vregs(code);
    let mut counts = vec![0u16; n];
    for op in code {
        op.for_each_src(|r| {
            counts[r as usize] += 1;
        });
    }
    counts
}

/// Pass 1: Move forwarding.
///
/// Pattern: instruction writes to R, next instruction is `Move { dst: D, src: R }`,
/// and R is only used by that Move. Rewrite the instruction to write to D directly
/// and NOP the Move.
fn move_forwarding(code: &mut [Opcode]) {
    let uses = compute_use_counts(code);

    // Count how many times each register is defined (written to).
    // If move_dst is defined more than once (e.g. from both branches of
    // an if-expression), forwarding is unsafe: it would cause one branch's
    // def to be lost when DCE removes the now-unused intermediate register.
    let n = num_vregs(code);
    let mut defs = vec![0u32; n];
    for op in code.iter() {
        if let Some(dst) = op.get_dst() {
            if (dst as usize) < n {
                defs[dst as usize] += 1;
            }
        }
    }

    for i in 0..code.len().saturating_sub(1) {
        if let Opcode::Move {
            dst: move_dst,
            src: move_src,
        } = code[i + 1]
        {
            // Never forward into r0. It is the return value register and is
            // clobbered by every Call instruction, so no variable should live
            // there across calls.
            if move_dst == 0 {
                continue;
            }
            // Check: instruction at i writes to move_src
            if let Some(prev_dst) = code[i].get_dst() {
                if prev_dst == move_src {
                    // Check: move_src is only read by this one Move
                    if uses[move_src as usize] == 1 {
                        // Don't forward if move_src has multiple definitions
                        // (e.g. if-expression result register written by both
                        // branches). The Move is a phi-like merge and must be
                        // preserved so all defs flow to move_dst.
                        if defs[move_src as usize] > 1 {
                            continue;
                        }
                        // Rewrite: make instruction i write to move_dst directly.
                        // Only eliminate the Move if set_dst succeeds (e.g. Call
                        // has an implicit dst that can't be changed).
                        if code[i].set_dst(move_dst) {
                            code[i + 1] = Opcode::Nop;
                        }
                    }
                }
            }
        }
    }
}

/// Copy propagation: for `Move { dst: D, src: S }`, replace reads of D with S
/// in subsequent instructions, then NOP the Move when all uses are propagated.
///
/// Mechanically derived from `proofs/CopyPropagation.v`, Theorem
/// `copy_propagation_correct`. Each precondition check and rewrite step
/// corresponds to a named component in the proof:
///
///   (P1) D ≠ S                — self-moves are no-ops
///   (P2) D ≠ 0                — never clobber the return register
///   (P3) not_written_in D     — defs[D] == 1 (only the Move defines D)
///   (P4) not_written_in S     — stop propagation when S is clobbered
///   (P5) opaque_safe D        — stop before Call/SaveRegs/RestoreRegs
///                               (Opaque instructions whose args_start denotes
///                               a contiguous register range — substituting one
///                               register would shift the entire range)
///   [subst_src_instr D S]     — replace_src_reg(D, S) on each instruction
///   [Opaque unchanged]        — break before Call/SaveRegs/RestoreRegs
///   [Nop]                     — code[i] = Opcode::Nop
///
/// This catches cases move_forwarding misses (e.g., after Call, whose dst
/// can't be redirected).
fn copy_propagation(code: &mut [Opcode]) {
    let targets = jump_targets(code);
    let uses = compute_use_counts(code);

    // [P3] Count definitions per register. D must have exactly one def (the Move).
    let n = num_vregs(code);
    let mut defs = vec![0u32; n];
    for op in code.iter() {
        if let Some(dst) = op.get_dst() {
            if (dst as usize) < n {
                defs[dst as usize] += 1;
            }
        }
    }

    for i in 0..code.len() {
        if let Opcode::Move { dst: d, src: s } = code[i] {
            // [P1] D ≠ S
            if d == s {
                continue;
            }
            // [P2] D ≠ 0
            if d == 0 {
                continue;
            }
            // [P3] not_written_in D rest: D has exactly one definition (this Move)
            if defs[d as usize] != 1 {
                continue;
            }

            // [subst_src D S rest]: replace reads of D with S in subsequent
            // instructions within the basic block.
            let mut propagated = 0u16;
            for j in (i + 1)..code.len() {
                // Basic block boundary — the proof assumes sequential flow.
                if targets.contains(&j) {
                    break;
                }

                // [P5] opaque_safe D: stop before Opaque instructions.
                // Call/CallIndirect/CallClosure have args_start denoting a
                // contiguous register range — substituting one register would
                // shift the entire range. SaveRegs/RestoreRegs have similar
                // range semantics. The proof models these as Opaque.
                match code[j] {
                    Opcode::Call { .. }
                    | Opcode::CallIndirect { .. }
                    | Opcode::CallClosure { .. }
                    | Opcode::SaveRegs { .. }
                    | Opcode::RestoreRegs { .. } => break,
                    _ => {}
                }

                // Count source operand slots that reference D (before replacing).
                code[j].for_each_src(|r| {
                    if r == d {
                        propagated += 1;
                    }
                });

                // [subst_src_instr D S]: replace reads of D with S.
                code[j].replace_src_reg(d, s);

                // [P4] not_written_in S rest: stop if S is clobbered.
                // Also stop if D is somehow written (defensive; shouldn't
                // happen with defs[D]==1).
                if let Some(dst) = code[j].get_dst() {
                    if dst == s || dst == d {
                        break;
                    }
                }

                // Stop at control flow (branches leave the basic block).
                match code[j] {
                    Opcode::Jump { .. }
                    | Opcode::JumpIfZero { .. }
                    | Opcode::JumpIfNotZero { .. }
                    | Opcode::ILtJump { .. }
                    | Opcode::FLtJump { .. } => break,
                    _ => {}
                }
            }

            // [Nop]: only NOP the Move if ALL uses of D were propagated.
            // If some uses are beyond the basic block or after S is clobbered,
            // the Move must be preserved.
            if propagated >= uses[d as usize] {
                code[i] = Opcode::Nop;
            }
        }
    }
}

/// Pass 2: Redundant LocalAddr elimination.
///
/// Within a basic block, if the same slot is loaded into a different register,
/// rewrite downstream users to reference the original register and NOP the
/// duplicate LocalAddr.
fn redundant_local_addr(code: &mut [Opcode]) {
    let targets = jump_targets(code);

    // Track slot → register that holds its address
    // Key: slot, Value: register
    let mut slot_reg: std::collections::HashMap<u16, Reg> = std::collections::HashMap::new();

    for i in 0..code.len() {
        // Clear tracking at jump targets (control flow merge point)
        if targets.contains(&i) {
            slot_reg.clear();
        }

        match code[i] {
            Opcode::LocalAddr { dst, slot } => {
                if let Some(&existing_reg) = slot_reg.get(&slot) {
                    // Same slot already in existing_reg. Rewrite users of dst
                    // to use existing_reg instead, then NOP this instruction.
                    let old_reg = dst;
                    replace_reg_uses_until_clobber(code, i + 1, old_reg, existing_reg, &targets);
                    code[i] = Opcode::Nop;
                } else {
                    slot_reg.insert(slot, dst);
                }
            }
            // Instructions that can clobber registers: clear tracking
            Opcode::Call { .. }
            | Opcode::CallIndirect { .. }
            | Opcode::CallClosure { .. }
            | Opcode::SaveRegs { .. }
            | Opcode::RestoreRegs { .. } => {
                slot_reg.clear();
            }
            // Jumps: clear tracking (we're leaving this basic block)
            Opcode::Jump { .. }
            | Opcode::JumpIfZero { .. }
            | Opcode::JumpIfNotZero { .. }
            | Opcode::ILtJump { .. }
            | Opcode::FLtJump { .. } => {
                slot_reg.clear();
            }
            // If something else writes to a register that's tracked, invalidate
            _ => {
                if let Some(dst) = code[i].get_dst() {
                    slot_reg.retain(|_, &mut reg| reg != dst);
                }
            }
        }
    }
}

/// Replace reads of `old_reg` with `new_reg` in instructions starting from `start`,
/// stopping at control flow boundaries or when `new_reg` is clobbered.
fn replace_reg_uses_until_clobber(
    code: &mut [Opcode],
    start: usize,
    old_reg: Reg,
    new_reg: Reg,
    targets: &HashSet<usize>,
) {
    for i in start..code.len() {
        // Stop at control flow boundaries
        if targets.contains(&i) {
            break;
        }
        match &code[i] {
            Opcode::Jump { .. }
            | Opcode::JumpIfZero { .. }
            | Opcode::JumpIfNotZero { .. }
            | Opcode::ILtJump { .. }
            | Opcode::FLtJump { .. }
            | Opcode::SaveRegs { .. }
            | Opcode::RestoreRegs { .. } => break,
            // For Call/CallIndirect/CallClosure: replace in the args range, then stop.
            Opcode::Call { .. }
            | Opcode::CallIndirect { .. }
            | Opcode::CallClosure { .. } => {
                code[i].replace_src_reg(old_reg, new_reg);
                break;
            }
            _ => {}
        }
        // Replace reads of old_reg with new_reg
        code[i].replace_src_reg(old_reg, new_reg);
        // Stop if either register gets clobbered (the replacement is only
        // valid while old_reg still holds the LocalAddr value and new_reg
        // still holds the same address).
        if let Some(dst) = code[i].get_dst() {
            if dst == new_reg || dst == old_reg {
                break;
            }
        }
    }
}

/// Fuse LocalAddr + Load32 → LoadSlot32, LocalAddr + Store32 → StoreSlot32.
///
/// Pattern: `LocalAddr { dst: A, slot: S }` followed by `Load32 { dst: B, addr: A }`
/// where A is only used by the Load32. Fuse into `LoadSlot32 { dst: B, slot: S }`.
/// Similarly for Store32.
fn fuse_local_access(code: &mut Vec<Opcode>) {
    let uses = compute_use_counts(code);

    for i in 0..code.len().saturating_sub(1) {
        if let Opcode::LocalAddr {
            dst: addr_reg,
            slot,
        } = code[i]
        {
            // Only fuse if the address register is used exactly once (by the next instruction)
            if uses[addr_reg as usize] != 1 {
                continue;
            }
            match code[i + 1] {
                Opcode::Load32 { dst, addr } if addr == addr_reg => {
                    code[i] = Opcode::LoadSlot32 { dst, slot };
                    code[i + 1] = Opcode::Nop;
                }
                Opcode::Store32 { addr, src } if addr == addr_reg => {
                    code[i] = Opcode::StoreSlot32 { slot, src };
                    code[i + 1] = Opcode::Nop;
                }
                _ => {}
            }
        }
    }
}

/// Fuse IAddImm + Load32/Store32 into Load32Off/Store32Off.
///
/// Pattern: `IAddImm { dst: A, src: B, imm: N }` followed by `Store32 { addr: A, src: V }`
/// where A is only used by the Store32. Fuse into `Store32Off { base: B, offset: N, src: V }`.
/// Similarly for Load32, Load64, Store64, etc.
fn fuse_offset_access(code: &mut Vec<Opcode>) {
    let uses = compute_use_counts(code);

    for i in 0..code.len().saturating_sub(1) {
        if let Opcode::IAddImm {
            dst: addr_reg,
            src: base,
            imm,
        } = code[i]
        {
            // Only fuse if the computed address register is used exactly once
            if uses[addr_reg as usize] != 1 {
                continue;
            }
            let offset = imm as i32;
            match code[i + 1] {
                Opcode::Store32 { addr, src } if addr == addr_reg => {
                    code[i] = Opcode::Store32Off { base, offset, src };
                    code[i + 1] = Opcode::Nop;
                }
                Opcode::Store64 { addr, src } if addr == addr_reg => {
                    code[i] = Opcode::Store64Off { base, offset, src };
                    code[i + 1] = Opcode::Nop;
                }
                Opcode::Store8 { addr, src } if addr == addr_reg => {
                    code[i] = Opcode::Store8Off { base, offset, src };
                    code[i + 1] = Opcode::Nop;
                }
                Opcode::Load32 { dst, addr } if addr == addr_reg => {
                    code[i] = Opcode::Load32Off { dst, base, offset };
                    code[i + 1] = Opcode::Nop;
                }
                Opcode::Load64 { dst, addr } if addr == addr_reg => {
                    code[i] = Opcode::Load64Off { dst, base, offset };
                    code[i + 1] = Opcode::Nop;
                }
                _ => {}
            }
        }
    }
}

/// Fuse compare + branch into single superinstructions.
///
/// Patterns:
///   ILt { dst, a, b } + JumpIfZero { cond: dst, offset } → ILtJump { a, b, offset }
///   FLt { dst, a, b } + JumpIfZero { cond: dst, offset } → FLtJump { a, b, offset }
fn fuse_compare_branch(code: &mut Vec<Opcode>) {
    let uses = compute_use_counts(code);

    // Don't fuse if the JumpIfZero is a jump target, because another path
    // can reach it without executing the preceding compare.
    let targets = jump_targets(code);

    for i in 0..code.len().saturating_sub(1) {
        // Don't fuse if the JumpIfZero at i+1 is a jump target.
        if targets.contains(&(i + 1)) {
            continue;
        }
        match code[i] {
            Opcode::ILt { dst, a, b } => {
                if uses[dst as usize] == 1 {
                    if let Opcode::JumpIfZero { cond, offset } = code[i + 1] {
                        if cond == dst {
                            // offset was relative to JumpIfZero (i+1), now relative to ILt (i)
                            code[i] = Opcode::ILtJump {
                                a,
                                b,
                                offset: offset + 1,
                            };
                            code[i + 1] = Opcode::Nop;
                        }
                    }
                }
            }
            Opcode::FLt { dst, a, b } => {
                if uses[dst as usize] == 1 {
                    if let Opcode::JumpIfZero { cond, offset } = code[i + 1] {
                        if cond == dst {
                            code[i] = Opcode::FLtJump {
                                a,
                                b,
                                offset: offset + 1,
                            };
                            code[i + 1] = Opcode::Nop;
                        }
                    }
                }
            }
            _ => {}
        }
    }
}

/// Compute live ranges for all virtual registers.
/// Returns (def_point, last_use, is_used) vectors indexed by register number.
fn compute_live_ranges(code: &[Opcode]) -> (Vec<u32>, Vec<u32>, Vec<bool>) {
    let n = num_vregs(code);
    let mut def_point = vec![u32::MAX; n];
    let mut last_use = vec![0u32; n];
    let mut is_used = vec![false; n];

    for (i, op) in code.iter().enumerate() {
        let i = i as u32;
        if let Some(dst) = op.get_dst() {
            let d = dst as usize;
            if def_point[d] == u32::MAX {
                def_point[d] = i;
            }
            // Extend range to cover all definitions. A later write to this
            // register still occupies the physical register, so it must not
            // be reused by another virtual register before the last write.
            last_use[d] = last_use[d].max(i);
            is_used[d] = true;
        }
        op.for_each_src(|r| {
            let u = r as usize;
            last_use[u] = last_use[u].max(i);
            is_used[u] = true;
            if def_point[u] == u32::MAX {
                def_point[u] = 0;
            }
        });
    }

    // Extend live ranges through loops (backward jumps).
    for (i, op) in code.iter().enumerate() {
        let offset = match op {
            Opcode::Jump { offset }
            | Opcode::JumpIfZero { offset, .. }
            | Opcode::JumpIfNotZero { offset, .. }
            | Opcode::ILtJump { offset, .. }
            | Opcode::FLtJump { offset, .. } => Some(*offset),
            _ => None,
        };
        if let Some(off) = offset {
            let target = (i as i32 + 1 + off) as u32;
            let i = i as u32;
            if target <= i {
                let loop_start = target;
                let loop_end = i;
                for r in 0..n {
                    if !is_used[r] {
                        continue;
                    }
                    if def_point[r] < loop_start
                        && last_use[r] >= loop_start
                        && last_use[r] <= loop_end
                    {
                        last_use[r] = loop_end;
                    }
                    if def_point[r] >= loop_start
                        && def_point[r] <= loop_end
                        && last_use[r] >= loop_start
                        && last_use[r] <= loop_end
                    {
                        for j in loop_start..def_point[r] {
                            if code[j as usize].reads_reg(r as Reg) {
                                def_point[r] = loop_start;
                                last_use[r] = loop_end;
                                break;
                            }
                        }
                    }
                }
            }
        }
    }

    (def_point, last_use, is_used)
}

/// Copy coalescing: merge virtual registers connected by Move instructions
/// when their live ranges don't interfere. This eliminates moves and reduces
/// register pressure. Runs as a pre-pass before linear scan allocation.
fn copy_coalesce(code: &mut [Opcode], param_count: u8) {
    let n = num_vregs(code);
    // Union-find for register coalescing.
    let mut parent: Vec<Reg> = (0..n as Reg).collect();

    fn find(parent: &mut [Reg], mut x: Reg) -> Reg {
        while parent[x as usize] != x {
            parent[x as usize] = parent[parent[x as usize] as usize]; // path compression
            x = parent[x as usize];
        }
        x
    }

    fn union(parent: &mut [Reg], a: Reg, b: Reg) {
        // Prefer lower-numbered register as representative (keeps reg 0 as itself).
        let ra = find(parent, a);
        let rb = find(parent, b);
        if ra != rb {
            if ra < rb {
                parent[rb as usize] = ra;
            } else {
                parent[ra as usize] = rb;
            }
        }
    }

    // Don't coalesce registers used as call arguments (contiguity requirement).
    let mut call_arg_reg = vec![false; n];
    for op in code.iter() {
        let (args_start, arg_count) = match op {
            Opcode::Call {
                args_start,
                arg_count,
                ..
            } => (*args_start, *arg_count),
            Opcode::CallIndirect {
                args_start,
                arg_count,
                ..
            } => (*args_start, *arg_count),
            Opcode::CallClosure {
                args_start,
                arg_count,
                ..
            } => (*args_start, *arg_count),
            _ => continue,
        };
        for r in args_start..(args_start + arg_count as Reg) {
            call_arg_reg[r as usize] = true;
        }
    }

    // Iterate coalescing until no more merges.
    // Coalesce one pair per pass (recompute live ranges each time for safety).
    for _ in 0..64 {
        let (def_point, last_use, is_used) = compute_live_ranges(code);
        let mut coalesced = false;

        for i in 0..code.len() {
            if let Opcode::Move { dst, src } = code[i] {
                if dst == src {
                    code[i] = Opcode::Nop;
                    coalesced = true;
                    break;
                }

                let d = dst as usize;
                let s = src as usize;
                if !is_used[d] || !is_used[s] {
                    continue;
                }
                // Skip registers involved in call arguments.
                if call_arg_reg[d] || call_arg_reg[s] {
                    continue;
                }
                // Never coalesce into r0. It is the return value register
                // and is clobbered by every Call instruction.
                if dst == 0 || src == 0 {
                    continue;
                }
                // Don't coalesce parameter copies: Move { dst: D, src: param_reg }.
                // The src register is the physical parameter position at function entry.
                // Coalescing would eliminate the copy, leaving the value stranded in
                // the parameter register when regalloc maps the vreg elsewhere.
                if src < param_count as Reg {
                    continue;
                }

                let d_def = def_point[d];
                let s_end = last_use[s].max(def_point[s]);
                let move_point = i as u32;

                // Conservative: only coalesce when src dies at the move and
                // dst is first defined at the move. This guarantees no interference.
                let src_dies_here = s_end == move_point;
                let dst_born_here = d_def == move_point;
                let can_coalesce = src_dies_here && dst_born_here;

                if can_coalesce {
                    union(&mut parent, dst, src);
                    let rep = find(&mut parent, dst);
                    for op in code.iter_mut() {
                        rewrite_coalesced_reg(op, dst, rep);
                        rewrite_coalesced_reg(op, src, rep);
                    }
                    code[i] = Opcode::Nop;
                    coalesced = true;
                    break; // Restart with fresh live ranges
                }
            }
        }
        if !coalesced {
            break;
        }
    }
}

/// Rewrite all occurrences of `old` register to `new` in an instruction.
fn rewrite_coalesced_reg(op: &mut Opcode, old: Reg, new: Reg) {
    if old == new {
        return;
    }
    // Rewrite dst
    match op {
        Opcode::Move { dst, src } => {
            if *dst == old {
                *dst = new;
            }
            if *src == old {
                *src = new;
            }
        }
        _ => {
            if let Some(d) = op.get_dst() {
                if d == old {
                    op.set_dst(new);
                }
            }
            op.replace_src_reg(old, new);
        }
    }
}

/// Linear scan register allocation.
///
/// Maps virtual registers (0..N, potentially sparse and high-numbered) to
/// compacted physical registers (0..M where M << N). This reduces SaveRegs
/// count and improves cache utilization of the register file.
fn register_allocation(code: &mut Vec<Opcode>, param_count: u8) -> (u8, Vec<Reg>) {
    if code.is_empty() {
        return (0, Vec::new());
    }

    // Pre-pass: copy coalescing to eliminate moves.
    copy_coalesce(code, param_count);

    let n = num_vregs(code);

    // Step 1: Find live ranges.
    let (def_point, last_use, is_used) = compute_live_ranges(code);

    // Step 3: Build sorted interval list.
    let mut intervals: Vec<(Reg, u32, u32)> = Vec::new(); // (vreg, start, end)
    for r in 0..n {
        if is_used[r] {
            let start = def_point[r];
            let end = last_use[r].max(start);
            intervals.push((r as Reg, start, end));
        }
    }
    intervals.sort_by_key(|&(_, start, end)| (start, end));

    // Step 3.5a: Pin registers that are live across calls.
    // Calls clobber all low registers (callee uses its own register range from 0).
    // Registers that span calls must keep their original (high) virtual register numbers
    // to avoid conflicts with callee registers.
    let mut pinned = vec![false; n];
    for (i, op) in code.iter().enumerate() {
        let is_call = matches!(op, Opcode::Call { .. } | Opcode::CallIndirect { .. } | Opcode::CallClosure { .. });
        if !is_call {
            continue;
        }
        let i = i as u32;
        for r in 0..n {
            if is_used[r] && def_point[r] < i && last_use[r] > i {
                pinned[r] = true;
            }
        }
    }

    // Step 3.5b: Identify call argument groups that must be contiguous.
    // Map vreg → (group_start_vreg, offset_in_group).
    let mut call_group: Vec<Option<(Reg, u8)>> = vec![None; n];
    for op in code.iter() {
        let (args_start, arg_count) = match op {
            Opcode::Call {
                args_start,
                arg_count,
                ..
            } => (*args_start, *arg_count),
            Opcode::CallIndirect {
                args_start,
                arg_count,
                ..
            } => (*args_start, *arg_count),
            Opcode::CallClosure {
                args_start,
                arg_count,
                ..
            } => (*args_start, *arg_count),
            _ => continue,
        };
        if arg_count >= 2 {
            for offset in 0..arg_count {
                let vreg = args_start + offset as Reg;
                call_group[vreg as usize] = Some((args_start, offset));
            }
        }
    }

    // Step 3.5c: Build register hints from remaining Move instructions.
    // If Move { dst: D, src: S } exists, hint that D should use the same preg as S.
    let mut hint: Vec<Reg> = vec![UNASSIGNED; n]; // vreg -> hinted vreg
    for op in code.iter() {
        if let Opcode::Move { dst, src } = op {
            if *dst != *src && !pinned[*dst as usize] && !pinned[*src as usize] {
                hint[*dst as usize] = *src;
            }
        }
    }

    // Step 4: Linear scan — assign physical registers.
    let mut mapping = vec![UNASSIGNED; n]; // vreg -> preg (UNASSIGNED = not yet assigned)
    // Track which physical registers are free. Use a simple bitset.
    let mut preg_free = [true; 256];
    // Active intervals sorted by end point: (end, vreg, preg)
    let mut active: Vec<(u32, Reg, u8)> = Vec::new();

    // Pin virtual register 0 to physical register 0 (implicit return value).
    if is_used[0] {
        mapping[0] = 0;
        preg_free[0] = false;
        let end = last_use[0].max(def_point[0]);
        active.push((end, 0, 0));
    }

    // Pin remaining parameter registers to themselves. The calling convention
    // requires r0..param_count-1 to hold the function's parameters at entry.
    for p in 1..(param_count as usize).min(n) {
        if is_used[p] {
            mapping[p] = p as Reg;
            preg_free[p] = false;
            let end = last_use[p].max(def_point[p]);
            active.push((end, p as Reg, p as u8));
        }
    }

    // Cross-call registers will be assigned after the main allocation (see below).

    for &(vreg, start, end) in &intervals {
        // Skip parameter registers — already pinned
        if (vreg as usize) < param_count as usize {
            continue;
        }
        // Skip cross-call registers — assigned later above the compacted range
        if pinned[vreg as usize] {
            continue;
        }
        // Skip if already allocated (part of a group that was allocated earlier)
        if mapping[vreg as usize] != UNASSIGNED {
            continue;
        }

        // Expire intervals that ended before this one starts
        active.retain(|&(active_end, _vr, pr)| {
            if active_end < start {
                preg_free[pr as usize] = true;
                false
            } else {
                true
            }
        });

        if let Some((group_start, _offset)) = call_group[vreg as usize] {
            // This vreg is part of a call arg group. Allocate the entire group contiguously.
            // Find group size (excluding pinned regs in the group).
            let mut group_size: u8 = 0;
            while {
                let idx = (group_start + group_size as Reg) as usize;
                idx < call_group.len()
                    && call_group[idx].map_or(false, |(gs, _)| gs == group_start)
            } {
                group_size += 1;
            }

            // Find a contiguous block of `group_size` free physical registers.
            let block_start = (0..=(255 - group_size + 1))
                .find(|&p| (0..group_size).all(|j| preg_free[(p + j) as usize]))
                .expect("register allocation: no contiguous block for call args")
                as u8;

            // Assign the entire group.
            for j in 0..group_size {
                let gvreg = group_start + j as Reg;
                if is_used[gvreg as usize] && !pinned[gvreg as usize] {
                    mapping[gvreg as usize] = (block_start + j) as Reg;
                    preg_free[(block_start + j) as usize] = false;
                    let gend = last_use[gvreg as usize].max(def_point[gvreg as usize]);
                    active.push((gend, gvreg, block_start + j));
                }
            }
        } else {
            // Normal register — try hinted register first, then lowest free.
            let hinted_vreg = hint[vreg as usize];
            let hinted_preg = if hinted_vreg != UNASSIGNED {
                mapping[hinted_vreg as usize]
            } else {
                UNASSIGNED
            };
            let preg = if hinted_preg != UNASSIGNED && preg_free[hinted_preg as usize] {
                hinted_preg as u8
            } else {
                preg_free
                    .iter()
                    .position(|&free| free)
                    .expect("register allocation: ran out of physical registers")
                    as u8
            };
            preg_free[preg as usize] = false;
            mapping[vreg as usize] = preg as Reg;
            active.push((end, vreg, preg));
        }
    }

    // Step 4b: Assign cross-call registers above the compacted range.
    // Find max physical register assigned so far.
    let mut max_compact: u8 = 0;
    for r in 0..n {
        if mapping[r] != UNASSIGNED {
            max_compact = max_compact.max(mapping[r] as u8);
        }
    }
    let mut next_cross_call = max_compact + 1;
    for r in 0..n {
        if pinned[r] && is_used[r] && mapping[r] == UNASSIGNED {
            mapping[r] = next_cross_call as Reg;
            next_cross_call += 1;
        }
    }

    // Step 5: Find max physical register used.
    let mut max_preg: u8 = 0;
    for r in 0..n {
        if is_used[r] && mapping[r] != UNASSIGNED {
            max_preg = max_preg.max(mapping[r] as u8);
        }
    }
    let new_count = max_preg
        .checked_add(1)
        .expect("register allocation overflow: need >255 physical registers");

    // Validate: no two vregs with overlapping live ranges share a physical register.
    for a in 0..n {
        if !is_used[a] || mapping[a] == UNASSIGNED { continue; }
        let a_start = def_point[a];
        let a_end = last_use[a].max(a_start);
        for b in (a + 1)..n {
            if !is_used[b] || mapping[b] == UNASSIGNED { continue; }
            if mapping[a] != mapping[b] { continue; }
            let b_start = def_point[b];
            let b_end = last_use[b].max(b_start);
            if a_start <= b_end && b_start <= a_end {
                panic!(
                    "register allocation conflict: vreg {} and vreg {} both mapped to preg {} \
                     with overlapping live ranges [{},{}] and [{},{}]",
                    a, b, mapping[a], a_start, a_end, b_start, b_end
                );
            }
        }
    }

    // Step 6: Patch SaveRegs/RestoreRegs BEFORE rewriting (start_reg still has virtual numbers).
    for op in code.iter_mut() {
        match op {
            Opcode::SaveRegs { count, .. } => {
                *count = new_count;
            }
            Opcode::RestoreRegs {
                start_reg, count, ..
            } => {
                if *start_reg == 1 {
                    *count = if new_count > 1 { new_count - 1 } else { 0 };
                } else {
                    *count = new_count;
                }
            }
            _ => {}
        }
    }

    // Step 7: Rewrite all instructions with the new register mapping.
    for op in code.iter_mut() {
        op.rewrite_regs(&mapping);
    }

    (new_count, mapping)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vm::Opcode;

    // ========== move_forwarding tests ==========

    #[test]
    fn test_move_forwarding_basic() {
        let mut code = vec![
            Opcode::LoadImm { dst: 0, value: 42 },
            Opcode::Move { dst: 1, src: 0 },
        ];
        move_forwarding(&mut code);
        assert_eq!(code[0], Opcode::LoadImm { dst: 1, value: 42 });
        assert_eq!(code[1], Opcode::Nop);
    }

    #[test]
    fn test_move_forwarding_src_used_elsewhere() {
        // r0 is used by both the Move and the IAdd, so forwarding should NOT happen
        let mut code = vec![
            Opcode::LoadImm { dst: 0, value: 42 },
            Opcode::Move { dst: 1, src: 0 },
            Opcode::IAdd { dst: 2, a: 0, b: 1 },
        ];
        move_forwarding(&mut code);
        // Should remain unchanged because r0 has use count > 1
        assert_eq!(code[0], Opcode::LoadImm { dst: 0, value: 42 });
        assert_eq!(code[1], Opcode::Move { dst: 1, src: 0 });
    }

    #[test]
    fn test_move_forwarding_chain() {
        // Two independent move-forwarding opportunities
        let mut code = vec![
            Opcode::LoadImm { dst: 0, value: 10 },
            Opcode::Move { dst: 2, src: 0 },
            Opcode::LoadImm { dst: 1, value: 20 },
            Opcode::Move { dst: 3, src: 1 },
        ];
        move_forwarding(&mut code);
        assert_eq!(code[0], Opcode::LoadImm { dst: 2, value: 10 });
        assert_eq!(code[1], Opcode::Nop);
        assert_eq!(code[2], Opcode::LoadImm { dst: 3, value: 20 });
        assert_eq!(code[3], Opcode::Nop);
    }

    #[test]
    fn test_move_forwarding_arith() {
        // IAdd writes to r2, then Move r2 -> r5, and r2 only used by that move
        let mut code = vec![
            Opcode::LoadImm { dst: 0, value: 1 },
            Opcode::LoadImm { dst: 1, value: 2 },
            Opcode::IAdd { dst: 2, a: 0, b: 1 },
            Opcode::Move { dst: 5, src: 2 },
        ];
        move_forwarding(&mut code);
        assert_eq!(code[2], Opcode::IAdd { dst: 5, a: 0, b: 1 });
        assert_eq!(code[3], Opcode::Nop);
    }

    // ========== redundant_local_addr tests ==========

    #[test]
    fn test_redundant_local_addr_basic() {
        // Two LocalAddr for the same slot -> second should be eliminated
        let mut code = vec![
            Opcode::LocalAddr { dst: 0, slot: 0 },
            Opcode::Store32 { addr: 0, src: 5 },
            Opcode::LocalAddr { dst: 1, slot: 0 },
            Opcode::Load32 { dst: 2, addr: 1 },
        ];
        redundant_local_addr(&mut code);
        // Second LocalAddr should be NOPed, Load32 should use r0 instead of r1
        assert_eq!(code[2], Opcode::Nop);
        assert_eq!(code[3], Opcode::Load32 { dst: 2, addr: 0 });
    }

    #[test]
    fn test_redundant_local_addr_different_slots() {
        // Different slots -> no elimination
        let mut code = vec![
            Opcode::LocalAddr { dst: 0, slot: 0 },
            Opcode::Store32 { addr: 0, src: 5 },
            Opcode::LocalAddr { dst: 1, slot: 1 },
            Opcode::Load32 { dst: 2, addr: 1 },
        ];
        let original = code.clone();
        redundant_local_addr(&mut code);
        assert_eq!(code, original);
    }

    #[test]
    fn test_redundant_local_addr_clobbered() {
        // Original register gets clobbered before the duplicate -> no elimination
        let mut code = vec![
            Opcode::LocalAddr { dst: 0, slot: 0 },
            Opcode::Store32 { addr: 0, src: 5 },
            Opcode::LoadImm { dst: 0, value: 99 }, // clobbers r0
            Opcode::LocalAddr { dst: 1, slot: 0 },
            Opcode::Load32 { dst: 2, addr: 1 },
        ];
        redundant_local_addr(&mut code);
        // r0 was clobbered, so slot_reg entry for slot 0 was invalidated
        assert_eq!(code[3], Opcode::LocalAddr { dst: 1, slot: 0 });
    }

    #[test]
    fn test_redundant_local_addr_jump_target_clears() {
        // Jump target between the two LocalAddrs should prevent optimization
        let mut code = vec![
            Opcode::LocalAddr { dst: 0, slot: 0 },
            Opcode::Store32 { addr: 0, src: 5 },
            Opcode::Jump { offset: 0 }, // jumps to index 3
            Opcode::LocalAddr { dst: 1, slot: 0 },
            Opcode::Load32 { dst: 2, addr: 1 },
        ];
        redundant_local_addr(&mut code);
        // The jump clears slot_reg, then index 3 is a jump target which also clears.
        assert_eq!(code[3], Opcode::LocalAddr { dst: 1, slot: 0 });
    }

    // ========== dead_code_elimination tests ==========

    #[test]
    fn test_dce_basic() {
        let mut code = vec![
            Opcode::LoadImm { dst: 1, value: 42 }, // r1 never used -> dead
            Opcode::LoadImm { dst: 0, value: 10 }, // r0 is return reg -> live
        ];
        dead_code_elimination(&mut code);
        assert_eq!(code[0], Opcode::Nop);
        assert_eq!(code[1], Opcode::LoadImm { dst: 0, value: 10 });
    }

    #[test]
    fn test_dce_preserves_side_effects() {
        // PrintI32 has side effects; its source register should stay live
        let mut code = vec![
            Opcode::LoadImm { dst: 1, value: 42 },
            Opcode::PrintI32 { src: 1 },
        ];
        dead_code_elimination(&mut code);
        assert_eq!(code[0], Opcode::LoadImm { dst: 1, value: 42 });
        assert_eq!(code[1], Opcode::PrintI32 { src: 1 });
    }

    #[test]
    fn test_dce_keeps_used_registers() {
        let mut code = vec![
            Opcode::LoadImm { dst: 1, value: 5 },
            Opcode::LoadImm { dst: 2, value: 10 },
            Opcode::IAdd { dst: 0, a: 1, b: 2 }, // r0 is live (return)
        ];
        dead_code_elimination(&mut code);
        assert_eq!(code[0], Opcode::LoadImm { dst: 1, value: 5 });
        assert_eq!(code[1], Opcode::LoadImm { dst: 2, value: 10 });
        assert_eq!(code[2], Opcode::IAdd { dst: 0, a: 1, b: 2 });
    }

    #[test]
    fn test_dce_eliminates_unused_chain() {
        // r3 unused, so IAdd to r3 is dead
        let mut code = vec![
            Opcode::LoadImm { dst: 0, value: 99 }, // r0 is return -> live
            Opcode::LoadImm { dst: 1, value: 5 },
            Opcode::LoadImm { dst: 2, value: 10 },
            Opcode::IAdd { dst: 3, a: 1, b: 2 }, // r3 never used -> dead
        ];
        dead_code_elimination(&mut code);
        assert_eq!(code[3], Opcode::Nop);
    }

    // ========== fuse_local_access tests ==========

    #[test]
    fn test_fuse_local_load() {
        let mut code = vec![
            Opcode::LocalAddr { dst: 0, slot: 3 },
            Opcode::Load32 { dst: 1, addr: 0 },
        ];
        fuse_local_access(&mut code);
        assert_eq!(code[0], Opcode::LoadSlot32 { dst: 1, slot: 3 });
        assert_eq!(code[1], Opcode::Nop);
    }

    #[test]
    fn test_fuse_local_store() {
        let mut code = vec![
            Opcode::LocalAddr { dst: 0, slot: 5 },
            Opcode::Store32 { addr: 0, src: 2 },
        ];
        fuse_local_access(&mut code);
        assert_eq!(code[0], Opcode::StoreSlot32 { slot: 5, src: 2 });
        assert_eq!(code[1], Opcode::Nop);
    }

    #[test]
    fn test_fuse_local_addr_used_twice_no_fuse() {
        // addr register used by both Store32 and Load32 -> use count > 1 -> no fusion
        let mut code = vec![
            Opcode::LocalAddr { dst: 0, slot: 3 },
            Opcode::Store32 { addr: 0, src: 1 },
            Opcode::Load32 { dst: 2, addr: 0 },
        ];
        let original = code.clone();
        fuse_local_access(&mut code);
        assert_eq!(code, original);
    }

    // ========== fuse_offset_access tests ==========

    #[test]
    fn test_fuse_offset_store32() {
        let mut code = vec![
            Opcode::IAddImm {
                dst: 1,
                src: 0,
                imm: 8,
            },
            Opcode::Store32 { addr: 1, src: 2 },
        ];
        fuse_offset_access(&mut code);
        assert_eq!(
            code[0],
            Opcode::Store32Off {
                base: 0,
                offset: 8,
                src: 2
            }
        );
        assert_eq!(code[1], Opcode::Nop);
    }

    #[test]
    fn test_fuse_offset_load32() {
        let mut code = vec![
            Opcode::IAddImm {
                dst: 1,
                src: 0,
                imm: 4,
            },
            Opcode::Load32 { dst: 2, addr: 1 },
        ];
        fuse_offset_access(&mut code);
        assert_eq!(
            code[0],
            Opcode::Load32Off {
                dst: 2,
                base: 0,
                offset: 4
            }
        );
        assert_eq!(code[1], Opcode::Nop);
    }

    #[test]
    fn test_fuse_offset_load64() {
        let mut code = vec![
            Opcode::IAddImm {
                dst: 1,
                src: 0,
                imm: 16,
            },
            Opcode::Load64 { dst: 2, addr: 1 },
        ];
        fuse_offset_access(&mut code);
        assert_eq!(
            code[0],
            Opcode::Load64Off {
                dst: 2,
                base: 0,
                offset: 16
            }
        );
        assert_eq!(code[1], Opcode::Nop);
    }

    #[test]
    fn test_fuse_offset_addr_used_twice_no_fuse() {
        // addr reg used by both Store32 and Load32
        let mut code = vec![
            Opcode::IAddImm {
                dst: 1,
                src: 0,
                imm: 8,
            },
            Opcode::Store32 { addr: 1, src: 2 },
            Opcode::Load32 { dst: 3, addr: 1 },
        ];
        let original = code.clone();
        fuse_offset_access(&mut code);
        assert_eq!(code, original);
    }

    // ========== fuse_compare_branch tests ==========

    #[test]
    fn test_fuse_ilt_jump() {
        let mut code = vec![
            Opcode::ILt { dst: 2, a: 0, b: 1 },
            Opcode::JumpIfZero { cond: 2, offset: 5 },
        ];
        fuse_compare_branch(&mut code);
        // offset adjusts: was 5 relative to JumpIfZero (i+1), now 5+1=6 relative to ILt (i)
        assert_eq!(
            code[0],
            Opcode::ILtJump {
                a: 0,
                b: 1,
                offset: 6
            }
        );
        assert_eq!(code[1], Opcode::Nop);
    }

    #[test]
    fn test_fuse_flt_jump() {
        let mut code = vec![
            Opcode::FLt { dst: 3, a: 0, b: 1 },
            Opcode::JumpIfZero {
                cond: 3,
                offset: 10,
            },
        ];
        fuse_compare_branch(&mut code);
        assert_eq!(
            code[0],
            Opcode::FLtJump {
                a: 0,
                b: 1,
                offset: 11
            }
        );
        assert_eq!(code[1], Opcode::Nop);
    }

    #[test]
    fn test_fuse_compare_branch_cond_used_elsewhere() {
        // Compare result used by both JumpIfZero and later instruction -> no fusion
        let mut code = vec![
            Opcode::ILt { dst: 2, a: 0, b: 1 },
            Opcode::JumpIfZero { cond: 2, offset: 5 },
            Opcode::PrintI32 { src: 2 }, // extra use of r2
        ];
        let original = code.clone();
        fuse_compare_branch(&mut code);
        assert_eq!(code, original);
    }

    #[test]
    fn test_fuse_compare_branch_wrong_cond() {
        // JumpIfZero uses a different register than what ILt wrote to
        let mut code = vec![
            Opcode::ILt { dst: 2, a: 0, b: 1 },
            Opcode::JumpIfZero { cond: 3, offset: 5 },
        ];
        let original = code.clone();
        fuse_compare_branch(&mut code);
        assert_eq!(code, original);
    }

    // ========== strip_nops / compact tests ==========

    #[test]
    fn test_compact_strips_nops() {
        let mut code = vec![
            Opcode::LoadImm { dst: 0, value: 1 },
            Opcode::Nop,
            Opcode::LoadImm { dst: 1, value: 2 },
            Opcode::Nop,
            Opcode::IAdd { dst: 0, a: 0, b: 1 },
        ];
        compact(&mut code);
        assert_eq!(code.len(), 3);
        assert_eq!(code[0], Opcode::LoadImm { dst: 0, value: 1 });
        assert_eq!(code[1], Opcode::LoadImm { dst: 1, value: 2 });
        assert_eq!(code[2], Opcode::IAdd { dst: 0, a: 0, b: 1 });
    }

    #[test]
    fn test_compact_adjusts_jump_offsets() {
        // Jump over Nops: offset should shrink
        let mut code = vec![
            Opcode::Jump { offset: 2 }, // target = 0 + 1 + 2 = index 3
            Opcode::Nop,
            Opcode::Nop,
            Opcode::LoadImm { dst: 0, value: 42 },
        ];
        compact(&mut code);
        assert_eq!(code.len(), 2);
        // After compaction: Jump at 0, LoadImm at 1. offset = 1 - 0 - 1 = 0
        assert_eq!(code[0], Opcode::Jump { offset: 0 });
        assert_eq!(code[1], Opcode::LoadImm { dst: 0, value: 42 });
    }

    // ========== move_forwarding edge cases ==========

    #[test]
    fn test_move_forwarding_move_to_move() {
        // Move → Move chain: Move{dst:1, src:0} + Move{dst:2, src:1}
        // Should forward: Move{dst:2, src:0} + Nop
        let mut code = vec![
            Opcode::LoadImm { dst: 0, value: 7 },
            Opcode::Move { dst: 1, src: 0 },
            Opcode::Move { dst: 2, src: 1 },
        ];
        move_forwarding(&mut code);
        // r1 only used by second move, so first Move rewrites dst to 2
        // but r0 is used by both the LoadImm consumer and the first Move src,
        // so we need to check use counts carefully
        // Actually r0 has use_count=1 (only first Move reads it), r1 has use_count=1 (only second Move)
        // First pass: LoadImm→Move: r0 used once → forward LoadImm dst to 1, Nop
        // Then at i=1 (now Nop) + i=2: Move{dst:2,src:1} — src=1, but r1 was result of LoadImm
        // Let's just check the outcome
        assert_eq!(code[1], Opcode::Nop);
    }

    #[test]
    fn test_move_forwarding_self_move() {
        // Move { dst: 0, src: 0 } — trivial self-move, but dst is r0 so
        // move forwarding skips it (r0 is reserved for return values).
        let mut code = vec![
            Opcode::LoadImm { dst: 0, value: 1 },
            Opcode::Move { dst: 0, src: 0 },
        ];
        move_forwarding(&mut code);
        assert_eq!(code[0], Opcode::LoadImm { dst: 0, value: 1 });
        assert_eq!(code[1], Opcode::Move { dst: 0, src: 0 });
    }

    // ========== DCE edge cases ==========

    #[test]
    fn test_dce_preserves_calls() {
        // Calls have side effects even if their result is unused
        let mut code = vec![
            Opcode::LoadImm { dst: 1, value: 0 },
            Opcode::LoadImm { dst: 2, value: 1 },
            Opcode::Call {
                func: 0,
                args_start: 1,
                arg_count: 2,
            },
            Opcode::LoadImm { dst: 0, value: 99 },
        ];
        dead_code_elimination(&mut code);
        // Call must be preserved; its arg registers (1,2) must stay alive
        assert_eq!(code[0], Opcode::LoadImm { dst: 1, value: 0 });
        assert_eq!(code[1], Opcode::LoadImm { dst: 2, value: 1 });
        assert!(matches!(code[2], Opcode::Call { .. }));
    }

    #[test]
    fn test_dce_preserves_stores() {
        // Store instructions have side effects
        let mut code = vec![
            Opcode::LoadImm { dst: 1, value: 42 },
            Opcode::LocalAddr { dst: 2, slot: 0 },
            Opcode::Store32 { addr: 2, src: 1 },
            Opcode::LoadImm { dst: 0, value: 0 },
        ];
        dead_code_elimination(&mut code);
        // Store32 and its dependencies must survive
        assert_eq!(code[0], Opcode::LoadImm { dst: 1, value: 42 });
        assert_eq!(code[1], Opcode::LocalAddr { dst: 2, slot: 0 });
        assert!(matches!(code[2], Opcode::Store32 { .. }));
    }

    #[test]
    fn test_dce_iterative_dead_chain() {
        // r3 = r1 + r2, r4 = r3 + r1, both dead.
        // First DCE pass kills r4 (unused), second pass kills r3 (now unused).
        let mut code = vec![
            Opcode::LoadImm { dst: 0, value: 99 }, // r0 live (return)
            Opcode::LoadImm { dst: 1, value: 5 },
            Opcode::LoadImm { dst: 2, value: 10 },
            Opcode::IAdd { dst: 3, a: 1, b: 2 },
            Opcode::IAdd { dst: 4, a: 3, b: 1 }, // r4 unused
        ];
        // Single pass: r4 unused → NOP [4]; but r3 now still has use_count from [4]
        dead_code_elimination(&mut code);
        assert_eq!(code[4], Opcode::Nop);
        // After first pass, r3 may still appear used. Run again:
        dead_code_elimination(&mut code);
        assert_eq!(code[3], Opcode::Nop);
        // r1, r2 only fed into dead code, run third pass:
        dead_code_elimination(&mut code);
        assert_eq!(code[1], Opcode::Nop);
        assert_eq!(code[2], Opcode::Nop);
    }

    // ========== fuse_offset_access edge cases ==========

    #[test]
    fn test_fuse_offset_store8() {
        let mut code = vec![
            Opcode::IAddImm {
                dst: 1,
                src: 0,
                imm: 3,
            },
            Opcode::Store8 { addr: 1, src: 2 },
        ];
        fuse_offset_access(&mut code);
        assert_eq!(
            code[0],
            Opcode::Store8Off {
                base: 0,
                offset: 3,
                src: 2
            }
        );
        assert_eq!(code[1], Opcode::Nop);
    }

    #[test]
    fn test_fuse_offset_store64() {
        let mut code = vec![
            Opcode::IAddImm {
                dst: 1,
                src: 0,
                imm: 24,
            },
            Opcode::Store64 { addr: 1, src: 2 },
        ];
        fuse_offset_access(&mut code);
        assert_eq!(
            code[0],
            Opcode::Store64Off {
                base: 0,
                offset: 24,
                src: 2
            }
        );
        assert_eq!(code[1], Opcode::Nop);
    }

    // ========== fuse_compare_branch edge cases ==========

    #[test]
    fn test_fuse_ilt_jump_if_not_zero() {
        // ILt + JumpIfNotZero should NOT fuse (only JumpIfZero fuses)
        let mut code = vec![
            Opcode::ILt { dst: 2, a: 0, b: 1 },
            Opcode::JumpIfNotZero { cond: 2, offset: 5 },
        ];
        let original = code.clone();
        fuse_compare_branch(&mut code);
        assert_eq!(code, original);
    }

    // ========== compact edge cases ==========

    #[test]
    fn test_compact_backward_jump() {
        // Backward jump (loop): Jump at index 3 targets index 1
        // offset = target - (pos+1) = 1 - 4 = -3
        let mut code = vec![
            Opcode::LoadImm { dst: 0, value: 0 },
            Opcode::LoadImm { dst: 1, value: 1 }, // loop start
            Opcode::Nop,
            Opcode::IAdd { dst: 0, a: 0, b: 1 },
            Opcode::Nop,
            Opcode::Jump { offset: -5 }, // target = 5+1+(-5) = 1
        ];
        compact(&mut code);
        // After removing 2 Nops: [LoadImm, LoadImm, IAdd, Jump]
        assert_eq!(code.len(), 4);
        // Jump at new index 3, target should be new index 1
        // offset = 1 - 3 - 1 = -3
        assert_eq!(code[3], Opcode::Jump { offset: -3 });
    }

    #[test]
    fn test_compact_no_nops() {
        let mut code = vec![
            Opcode::LoadImm { dst: 0, value: 1 },
            Opcode::LoadImm { dst: 1, value: 2 },
        ];
        let original = code.clone();
        compact(&mut code);
        assert_eq!(code, original);
    }

    #[test]
    fn test_compact_all_nops() {
        let mut code = vec![Opcode::Nop, Opcode::Nop, Opcode::Nop];
        compact(&mut code);
        assert!(code.is_empty());
    }

    // ========== compute_live_ranges tests ==========

    #[test]
    fn test_live_ranges_basic() {
        let code = vec![
            Opcode::LoadImm { dst: 0, value: 1 }, // def r0 at 0
            Opcode::LoadImm { dst: 1, value: 2 }, // def r1 at 1
            Opcode::IAdd { dst: 2, a: 0, b: 1 },  // use r0,r1 at 2; def r2 at 2
        ];
        let (def_point, last_use, is_used) = compute_live_ranges(&code);
        assert!(is_used[0] && is_used[1] && is_used[2]);
        assert_eq!(def_point[0], 0);
        assert_eq!(def_point[1], 1);
        assert_eq!(def_point[2], 2);
        assert_eq!(last_use[0], 2); // r0 last used at IAdd
        assert_eq!(last_use[1], 2); // r1 last used at IAdd
    }

    #[test]
    fn test_live_ranges_loop_extension() {
        // Backward jump extends live ranges of registers used in the loop
        let code = vec![
            Opcode::LoadImm { dst: 0, value: 0 },  // 0: def r0
            Opcode::LoadImm { dst: 1, value: 10 }, // 1: def r1
            Opcode::IAdd { dst: 0, a: 0, b: 1 },   // 2: use r0,r1 (loop body)
            Opcode::Jump { offset: -2 },           // 3: jump to index 2 (offset = 2 - 3 - 1 = -2)
        ];
        let (def_point, last_use, _is_used) = compute_live_ranges(&code);
        // r1 is defined before loop (at 1), used in loop body (at 2).
        // Backward jump from 3 to 2 should extend r1's last_use to 3 (loop end).
        assert_eq!(def_point[1], 1);
        assert!(
            last_use[1] >= 3,
            "r1 last_use should be extended to loop end, got {}",
            last_use[1]
        );
    }

    #[test]
    fn test_live_ranges_unused_register() {
        let code = vec![
            Opcode::LoadImm { dst: 5, value: 42 }, // r5 defined but never used
        ];
        let (_def_point, _last_use, is_used) = compute_live_ranges(&code);
        // r5 is "used" because it has a def point (get_dst returns it)
        assert!(is_used[5]);
        // But registers 0-4, 6-255 should not be used
        assert!(!is_used[0]);
        assert!(!is_used[1]);
    }

    // ========== copy_coalesce tests ==========

    #[test]
    fn test_copy_coalesce_basic() {
        // r1 = 42, Move r1 → r2, use r2
        // r1 dies at the Move, r2 is born at the Move → can coalesce
        let mut code = vec![
            Opcode::LoadImm { dst: 1, value: 42 },
            Opcode::Move { dst: 2, src: 1 },
            Opcode::PrintI32 { src: 2 },
        ];
        copy_coalesce(&mut code, 0);
        // Move should be eliminated; r1 and r2 merged to same register
        assert!(matches!(code[1], Opcode::Nop));
        // The PrintI32 should use whichever register the coalescer chose (the lower one = 1)
        assert!(matches!(code[2], Opcode::PrintI32 { src: 1 }));
    }

    #[test]
    fn test_copy_coalesce_skip_call_args() {
        // Registers used as call args shouldn't be coalesced (contiguity requirement)
        let mut code = vec![
            Opcode::LoadImm { dst: 3, value: 1 },
            Opcode::Move { dst: 4, src: 3 },
            Opcode::Call {
                func: 0,
                args_start: 4,
                arg_count: 1,
            },
        ];
        copy_coalesce(&mut code, 0);
        // r4 is a call arg → should NOT be coalesced
        assert!(matches!(code[1], Opcode::Move { dst: 4, src: 3 }));
    }

    #[test]
    fn test_copy_coalesce_self_move() {
        // Move { dst: 1, src: 1 } → should be eliminated as Nop
        let mut code = vec![
            Opcode::LoadImm { dst: 1, value: 5 },
            Opcode::Move { dst: 1, src: 1 },
            Opcode::PrintI32 { src: 1 },
        ];
        copy_coalesce(&mut code, 0);
        assert_eq!(code[1], Opcode::Nop);
    }

    #[test]
    fn test_copy_coalesce_interference() {
        // r1 and r2 both live at the same time → cannot coalesce
        let mut code = vec![
            Opcode::LoadImm { dst: 1, value: 1 },
            Opcode::LoadImm { dst: 2, value: 2 },
            Opcode::Move { dst: 3, src: 1 }, // r1 still live (used below)
            Opcode::IAdd { dst: 0, a: 1, b: 2 },
        ];
        copy_coalesce(&mut code, 0);
        // r1 doesn't die at the Move (it's used at IAdd), so can't coalesce
        assert!(matches!(code[2], Opcode::Move { dst: 3, src: 1 }));
    }

    // ========== register_allocation tests ==========

    #[test]
    fn test_register_allocation_compacts() {
        // Uses r0, r5, r10 → should be compacted to r0, r1, r2
        let mut code = vec![
            Opcode::LoadImm { dst: 5, value: 1 },
            Opcode::LoadImm { dst: 10, value: 2 },
            Opcode::IAdd {
                dst: 0,
                a: 5,
                b: 10,
            },
        ];
        let (count, mapping) = register_allocation(&mut code, 0);
        // r0 pinned to preg 0
        assert_eq!(mapping[0], 0);
        // r5 and r10 should map to pregs 1 and 2 (or 2 and 1)
        assert!(mapping[5] < count as Reg);
        assert!(mapping[10] < count as Reg);
        assert_ne!(mapping[5], mapping[10]);
        assert!(count <= 3, "expected at most 3 pregs, got {}", count);
        // Verify instruction was rewritten
        if let Opcode::IAdd { dst, a, b } = code[2] {
            assert_eq!(dst, 0);
            assert_eq!(a, mapping[5]);
            assert_eq!(b, mapping[10]);
        } else {
            panic!("expected IAdd");
        }
    }

    #[test]
    fn test_register_allocation_reuses_expired() {
        // r1 used then dead, r2 used then dead → should reuse same preg
        let mut code = vec![
            Opcode::LoadImm { dst: 1, value: 1 },
            Opcode::Move { dst: 0, src: 1 }, // r1 dies here
            Opcode::LoadImm { dst: 2, value: 2 },
            Opcode::IAdd { dst: 0, a: 0, b: 2 }, // r2 dies here
        ];
        let (count, _mapping) = register_allocation(&mut code, 0);
        // r0 always needed; r1 and r2 don't overlap, so can share → 2 pregs total
        assert!(count <= 2, "expected ≤2 pregs, got {}", count);
    }

    #[test]
    fn test_register_allocation_preserves_call_arg_contiguity() {
        // Call args r3,r4 must remain contiguous after allocation
        let mut code = vec![
            Opcode::LoadImm { dst: 3, value: 10 },
            Opcode::LoadImm { dst: 4, value: 20 },
            Opcode::Call {
                func: 0,
                args_start: 3,
                arg_count: 2,
            },
            Opcode::LoadImm { dst: 0, value: 0 },
        ];
        let (_count, mapping) = register_allocation(&mut code, 0);
        // The mapped registers for r3 and r4 must be contiguous
        let p3 = mapping[3];
        let p4 = mapping[4];
        assert_eq!(
            p4,
            p3 + 1,
            "call args must be contiguous: r3→{}, r4→{}",
            p3,
            p4
        );
        // Verify the Call instruction was updated
        if let Opcode::Call {
            args_start,
            arg_count,
            ..
        } = code[2]
        {
            assert_eq!(args_start, p3);
            assert_eq!(arg_count, 2);
        } else {
            panic!("expected Call");
        }
    }

    #[test]
    fn test_register_allocation_empty_code() {
        let mut code: Vec<Opcode> = vec![];
        let result = optimize(&mut code, 0);
        assert!(result.is_none());
    }

    // ========== full optimize pipeline ==========

    #[test]
    fn test_optimize_pipeline_simple() {
        // LoadImm → Move → PrintI32
        // move_forwarding merges LoadImm+Move, DCE keeps PrintI32 alive
        let mut code = vec![
            Opcode::LoadImm { dst: 5, value: 42 },
            Opcode::Move { dst: 10, src: 5 },
            Opcode::PrintI32 { src: 10 },
        ];
        let result = optimize(&mut code, 0);
        assert!(result.is_some());
        // After optimize, code still has Nops; need compact to remove them
        compact(&mut code);
        // Should be: LoadImm + PrintI32 (move eliminated, regs compacted)
        assert_eq!(code.len(), 2);
        // First instruction loads a value, second prints it
        assert!(matches!(code[0], Opcode::LoadImm { .. }));
        assert!(matches!(code[1], Opcode::PrintI32 { .. }));
    }

    #[test]
    fn test_optimize_pipeline_fuse_and_compact() {
        // LocalAddr + Load32 should fuse to LoadSlot32, then Nop gets stripped
        let mut code = vec![
            Opcode::LocalAddr { dst: 3, slot: 8 },
            Opcode::Load32 { dst: 0, addr: 3 },
        ];
        let result = optimize(&mut code, 0);
        assert!(result.is_some());
        compact(&mut code);
        assert_eq!(code.len(), 1);
        assert!(matches!(code[0], Opcode::LoadSlot32 { .. }));
    }

    #[test]
    fn test_move_forwarding_then_dce() {
        // Move into r0 is not forwarded (r0 is reserved for return values).
        // The Move stays, and DCE preserves both since r0 is implicitly live.
        let mut code = vec![
            Opcode::LoadImm { dst: 2, value: 42 },
            Opcode::Move { dst: 0, src: 2 }, // r0 is return, r2 only used here
        ];
        move_forwarding(&mut code);
        assert_eq!(code[0], Opcode::LoadImm { dst: 2, value: 42 });
        assert_eq!(code[1], Opcode::Move { dst: 0, src: 2 });
        dead_code_elimination(&mut code);
        // Both survive: r2 is used by Move, r0 is implicitly live (return value).
        assert_eq!(code[0], Opcode::LoadImm { dst: 2, value: 42 });
        assert_eq!(code[1], Opcode::Move { dst: 0, src: 2 });
    }

    // ========== replace_reg_uses_until_clobber tests ==========

    #[test]
    fn test_replace_reg_uses_stops_at_clobber() {
        let targets = HashSet::new();
        let mut code = vec![
            Opcode::IAdd { dst: 2, a: 0, b: 1 },   // uses r1
            Opcode::LoadImm { dst: 0, value: 99 }, // clobbers r0 (the new_reg)
            Opcode::ISub { dst: 3, a: 0, b: 1 },   // should NOT be rewritten
        ];
        // Replace r1 with r0, starting from index 0
        replace_reg_uses_until_clobber(&mut code, 0, 1, 0, &targets);
        // First instruction: r1 replaced with r0
        assert_eq!(code[0], Opcode::IAdd { dst: 2, a: 0, b: 0 });
        // Third instruction: r0 was clobbered at [1], so replacement stopped
        assert_eq!(code[2], Opcode::ISub { dst: 3, a: 0, b: 1 });
    }

    #[test]
    fn test_replace_reg_uses_stops_at_jump_target() {
        let mut targets = HashSet::new();
        targets.insert(1usize); // index 1 is a jump target
        let mut code = vec![
            Opcode::IAdd { dst: 2, a: 0, b: 1 }, // uses r1
            Opcode::ISub { dst: 3, a: 0, b: 1 }, // at jump target → stop before this
        ];
        replace_reg_uses_until_clobber(&mut code, 0, 1, 0, &targets);
        // Replacement should stop at index 1 (jump target), but index 0 is before it
        assert_eq!(code[0], Opcode::IAdd { dst: 2, a: 0, b: 0 });
        // Index 1 NOT reached (replacement stopped before jump target)
        assert_eq!(code[1], Opcode::ISub { dst: 3, a: 0, b: 1 });
    }

    #[test]
    fn test_replace_reg_uses_stops_at_call() {
        let targets = HashSet::new();
        let mut code = vec![
            Opcode::IAdd { dst: 2, a: 0, b: 1 },
            Opcode::Call {
                func: 0,
                args_start: 0,
                arg_count: 1,
            },
            Opcode::ISub { dst: 3, a: 0, b: 1 },
        ];
        replace_reg_uses_until_clobber(&mut code, 0, 1, 0, &targets);
        assert_eq!(code[0], Opcode::IAdd { dst: 2, a: 0, b: 0 });
        // Call stops replacement; instruction after call untouched
        assert_eq!(code[2], Opcode::ISub { dst: 3, a: 0, b: 1 });
    }
}
