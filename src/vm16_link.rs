//! Linker for the 16-bit VM: packs `Op16Instr` into `Vec<u16>` with constant pools
//! and jump offset fixup.

use crate::vm16_lower::{LoweredProgram, Op16Instr};
use crate::vm16_opcode::{tags, PackedOp16, has_trailing_word};

/// A linked 16-bit VM program ready for execution.
pub struct LinkedProgram16 {
    /// Packed 16-bit instruction stream.
    pub ops: Vec<u16>,
    /// Function entry points in the ops stream (word index).
    pub func_offsets: Vec<usize>,
    /// Local variable size per function (in bytes).
    pub func_locals: Vec<u32>,
    /// f32 constant pool.
    pub f32_pool: Vec<f32>,
    /// f64 constant pool.
    pub f64_pool: Vec<f64>,
    /// i64 constant pool (for wide immediates).
    pub i64_pool: Vec<i64>,
}

impl LinkedProgram16 {
    /// Link a lowered program: pack instructions, fix up jump offsets.
    pub fn from_lowered(program: &LoweredProgram) -> Self {
        let mut ops = Vec::new();
        let mut func_offsets = Vec::new();
        let mut func_locals = Vec::new();

        for func in &program.functions {
            let func_start = ops.len();
            func_offsets.push(func_start);
            func_locals.push(func.locals_size);

            // First pass: compute the mapping from instruction index to word offset.
            // Each Op16Instr takes 1 word, plus 1 for the trailing word if present.
            let mut instr_to_word: Vec<usize> = Vec::with_capacity(func.code.len() + 1);
            let mut word_offset = 0usize;
            for instr in &func.code {
                instr_to_word.push(word_offset);
                word_offset += if instr.trail.is_some() { 2 } else { 1 };
            }
            // Sentinel: offset past the last instruction.
            instr_to_word.push(word_offset);

            // Second pass: emit packed words with fixed-up jump offsets.
            for (idx, instr) in func.code.iter().enumerate() {
                let packed = PackedOp16::ab(instr.tag, instr.ra, instr.rb);
                ops.push(packed.0);

                if let Some(trail) = instr.trail {
                    if is_jump_tag(instr.tag) {
                        // Fix up jump offset: convert from instruction-relative to word-relative.
                        // The original offset is in instruction units (from the old Opcode IR).
                        // In the packed stream, the jump is relative to the word AFTER the trail word.
                        let raw_offset = trail as i16 as i32;
                        let source_instr = idx as i32 + 1; // instruction after this one (Opcode convention)
                        let target_instr = source_instr + raw_offset;

                        // Clamp target to valid range.
                        let target_instr = target_instr.max(0).min(instr_to_word.len() as i32 - 1) as usize;
                        let target_word = func_start + instr_to_word[target_instr];
                        // Source word: the word AFTER the trailing word (ip after consuming both words).
                        let source_word = func_start + instr_to_word[idx] + 2; // op word + trail word
                        let word_offset = (target_word as i32) - (source_word as i32);
                        ops.push(word_offset as u16);
                    } else {
                        ops.push(trail);
                    }
                }
            }
        }

        LinkedProgram16 {
            ops,
            func_offsets,
            func_locals,
            f32_pool: program.f32_pool.clone(),
            f64_pool: program.f64_pool.clone(),
            i64_pool: program.i64_pool.clone(),
        }
    }

    /// Get the number of 16-bit words in a function.
    pub fn func_size(&self, func_idx: usize) -> usize {
        let start = self.func_offsets[func_idx];
        let end = if func_idx + 1 < self.func_offsets.len() {
            self.func_offsets[func_idx + 1]
        } else {
            self.ops.len()
        };
        end - start
    }
}

/// Returns true if the opcode is a jump that needs offset fixup.
fn is_jump_tag(tag: u8) -> bool {
    matches!(
        tag,
        tags::JUMP
            | tags::JUMP_IF_ZERO
            | tags::JUMP_IF_NOT_ZERO
            | tags::ILT_JUMP
            | tags::FLT_JUMP
            | tags::ILE_JUMP
            | tags::FLE_JUMP
    )
}

/// Disassemble a function from the linked program for debugging.
pub fn disassemble(linked: &LinkedProgram16, func_idx: usize) -> String {
    use crate::vm16_opcode::tag_name;
    let mut result = String::new();
    let start = linked.func_offsets[func_idx];
    let end = if func_idx + 1 < linked.func_offsets.len() {
        linked.func_offsets[func_idx + 1]
    } else {
        linked.ops.len()
    };

    let mut ip = start;
    while ip < end {
        let word = linked.ops[ip];
        let packed = PackedOp16(word);
        let opcode = packed.opcode();
        let ra = packed.ra();
        let rb = packed.rb();
        let name = tag_name(opcode);

        let offset = ip - start;
        if has_trailing_word(opcode) && ip + 1 < end {
            let trail = linked.ops[ip + 1];
            result.push_str(&format!(
                "  {:4}: {} r{}, r{} [trail={}]\n",
                offset, name, ra, rb, trail as i16
            ));
            ip += 2;
        } else {
            result.push_str(&format!("  {:4}: {} r{}, r{}\n", offset, name, ra, rb));
            ip += 1;
        }
    }

    result
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::vm16_lower::{LoweredFunction, LoweredProgram, Op16Instr};

    #[test]
    fn test_link_simple() {
        let func = LoweredFunction {
            code: vec![
                Op16Instr { tag: tags::LOAD_IMM, ra: 0, rb: 0, trail: Some(42) },
                Op16Instr { tag: tags::RETURN, ra: 0, rb: 0, trail: None },
            ],
            locals_size: 0,
        };
        let program = LoweredProgram {
            functions: vec![func],
            f32_pool: vec![],
            f64_pool: vec![],
            i64_pool: vec![],
        };
        let linked = LinkedProgram16::from_lowered(&program);
        assert_eq!(linked.ops.len(), 3); // LoadImm(1) + trail(1) + Return(1)
        assert_eq!(linked.func_offsets, vec![0]);

        // Verify LoadImm encoding
        let op0 = PackedOp16(linked.ops[0]);
        assert_eq!(op0.opcode(), tags::LOAD_IMM);
        assert_eq!(op0.ra(), 0);
        assert_eq!(linked.ops[1], 42); // trail

        // Verify Return encoding
        let op2 = PackedOp16(linked.ops[2]);
        assert_eq!(op2.opcode(), tags::RETURN);
    }

    #[test]
    fn test_jump_offset_fixup() {
        // A simple loop: JumpIfZero over one instruction, then Jump back.
        // Instruction layout:
        //   0: LoadImm r0, 10    (2 words)
        //   1: IAddImm r0, -1    (2 words)
        //   2: JumpIfNotZero r0, offset=-2  (jump back to instr 1)
        //   3: Return
        let func = LoweredFunction {
            code: vec![
                Op16Instr { tag: tags::LOAD_IMM, ra: 0, rb: 0, trail: Some(10) },
                Op16Instr { tag: tags::IADD_IMM, ra: 0, rb: 0, trail: Some((-1i16) as u16) },
                // Offset -2 in instruction units = jump back 2 instructions from instr 3.
                // Target is instr 1 (instr 3 + (-2) = instr 1).
                Op16Instr { tag: tags::JUMP_IF_NOT_ZERO, ra: 0, rb: 0, trail: Some((-2i16) as u16) },
                Op16Instr { tag: tags::RETURN, ra: 0, rb: 0, trail: None },
            ],
            locals_size: 0,
        };
        let program = LoweredProgram {
            functions: vec![func],
            f32_pool: vec![],
            f64_pool: vec![],
            i64_pool: vec![],
        };
        let linked = LinkedProgram16::from_lowered(&program);

        // Word layout:
        //   0: LoadImm word
        //   1: trail (10)
        //   2: IAddImm word
        //   3: trail (-1)
        //   4: JumpIfNotZero word
        //   5: trail (fixed-up offset)
        //   6: Return word
        assert_eq!(linked.ops.len(), 7);

        // The JumpIfNotZero's trail is at word 5.
        // Source word (ip after consuming op+trail) = 6
        // Target instr = 1, which maps to word 2.
        // Word offset = 2 - 6 = -4
        let jump_trail = linked.ops[5] as i16;
        assert_eq!(jump_trail, -4, "jump offset should be -4 words");
    }

    #[test]
    fn test_multiple_functions() {
        let func0 = LoweredFunction {
            code: vec![
                Op16Instr { tag: tags::LOAD_IMM, ra: 0, rb: 0, trail: Some(1) },
                Op16Instr { tag: tags::RETURN, ra: 0, rb: 0, trail: None },
            ],
            locals_size: 0,
        };
        let func1 = LoweredFunction {
            code: vec![
                Op16Instr { tag: tags::LOAD_IMM, ra: 0, rb: 0, trail: Some(2) },
                Op16Instr { tag: tags::RETURN, ra: 0, rb: 0, trail: None },
            ],
            locals_size: 8,
        };
        let program = LoweredProgram {
            functions: vec![func0, func1],
            f32_pool: vec![],
            f64_pool: vec![],
            i64_pool: vec![],
        };
        let linked = LinkedProgram16::from_lowered(&program);
        assert_eq!(linked.func_offsets, vec![0, 3]);
        assert_eq!(linked.func_locals, vec![0, 8]);
        assert_eq!(linked.func_size(0), 3);
        assert_eq!(linked.func_size(1), 3);
    }
}
