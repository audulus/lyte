//! Expand the checked recurrence recipes into statically specialized C loops.
//!
//! Carry nodes name iteration-entry values. The next-carry map is published only
//! after every node has been evaluated, so history shifts do not depend on the
//! order in which mutable C carry variables are assigned.

use crate::loop_recipe::{BinaryOp, Node};
use crate::recurrence;
use std::fmt::Write;
use std::path::Path;

pub fn generate(out_dir: &Path) {
    recurrence::validate().expect("invalid native recurrence recipe catalog");
    // Preserve the existing native boundary and biquad descriptor capacities.
    assert_eq!(recurrence::MAX_COEFFICIENTS, 5);
    assert_eq!(recurrence::MAX_CARRIES, 4);

    let mut catalog = String::from("// Generated from src/recurrence.rs; do not edit.\n");
    for (id, recipe) in recurrence::recipes() {
        writeln!(
            catalog,
            "RECURRENCE_RECIPE({}, {}, {}, {}, {})",
            id.index(),
            recipe.name.to_ascii_uppercase(),
            recipe.name,
            recipe.coefficient_count(),
            recipe.carry_count(),
        )
        .unwrap();
    }
    std::fs::write(out_dir.join("recurrence_catalog.inc"), catalog)
        .expect("failed to write native recurrence catalog");

    let mut chunks = String::from("// Generated from src/recurrence.rs; do not edit.\n");
    for (suffix, scalar, load) in [
        ("f32", "float", "load_f32_unaligned"),
        ("f64", "double", "load_f64_unaligned"),
    ] {
        writeln!(
            chunks,
            "static inline __attribute__((always_inline)) void native_recurrence_{suffix}(\n\
             NativeKernelKind kind, const uint8_t* input, uint8_t* output,\n\
             int64_t count, const {scalar}* coefficients, {scalar}* carried\n\
             ) {{\n    switch (kind) {{"
        )
        .unwrap();
        for (_, recipe) in recurrence::recipes() {
            writeln!(
                chunks,
                "        case NATIVE_{}: {{",
                recipe.name.to_ascii_uppercase()
            )
            .unwrap();
            for (index, node) in recipe.nodes.iter().enumerate() {
                if let Node::Parameter(parameter) = node {
                    writeln!(
                        chunks,
                        "            {scalar} v{index} = coefficients[{parameter}];"
                    )
                    .unwrap();
                }
            }
            for carry in 0..recipe.carry_count() {
                writeln!(chunks, "            {scalar} c{carry} = carried[{carry}];").unwrap();
            }
            writeln!(
                chunks,
                "            for (int64_t item = 0; item < count; ++item) {{"
            )
            .unwrap();
            writeln!(
                chunks,
                "                size_t offset = (size_t)item * sizeof({scalar});"
            )
            .unwrap();
            for (index, node) in recipe.nodes.iter().enumerate() {
                let expression = match node {
                    Node::Read => format!("{load}(input + offset)"),
                    Node::Parameter(_) => continue,
                    Node::Carry(carry) => format!("c{carry}"),
                    Node::Binary(operator, left, right) => {
                        let operator = match operator {
                            BinaryOp::Add => '+',
                            BinaryOp::Sub => '-',
                            BinaryOp::Mul => '*',
                        };
                        format!("v{left} {operator} v{right}")
                    }
                };
                writeln!(chunks, "                {scalar} v{index} = {expression};").unwrap();
            }
            // Every root is an SSA value from the completed iteration. Updating
            // c0 cannot change a later root which refers to the old c0 snapshot.
            for (carry, next) in recipe.next_carries.iter().enumerate() {
                writeln!(chunks, "                c{carry} = v{next};").unwrap();
            }
            writeln!(
                chunks,
                "                __builtin_memcpy(output + offset, &v{}, sizeof(v{}));",
                recipe.result, recipe.result,
            )
            .unwrap();
            chunks.push_str("            }\n");
            for carry in 0..recipe.carry_count() {
                writeln!(chunks, "            carried[{carry}] = c{carry};").unwrap();
            }
            chunks.push_str("            return;\n        }\n");
        }
        chunks.push_str("        default: __builtin_unreachable();\n    }\n}\n");
    }
    std::fs::write(out_dir.join("recurrence_chunks.inc"), chunks)
        .expect("failed to write native recurrence arithmetic");
}
