//! Build-time expansion of the shared pointwise recipes into native C bodies.
//!
//! Every recipe produces typed SSA statements in its declared order. Handler
//! selection happens while the backend is constructed; constant kernel kinds
//! remove the generated switch before the interpreter is compiled.

use crate::pointwise::{self, BinaryOp, Node, Recipe};
use std::fmt::Write;
use std::path::Path;

pub fn generate(out_dir: &Path) {
    pointwise::validate().expect("invalid native pointwise recipe catalog");
    // The compact native encoding has exactly four coefficient slots.
    assert_eq!(pointwise::MAX_COEFFICIENTS, 4);

    let mut catalog = String::from("// Generated from src/pointwise.rs; do not edit.\n");
    for (id, recipe) in pointwise::recipes() {
        writeln!(
            catalog,
            "POINTWISE_RECIPE({}, {}, {}, {})",
            id.index(),
            recipe.name.to_ascii_uppercase(),
            recipe.name,
            recipe.coefficient_count(),
        )
        .unwrap();
    }
    std::fs::write(out_dir.join("pointwise_catalog.inc"), catalog)
        .expect("failed to write native pointwise catalog");

    let mut chunks = String::from("// Generated from src/pointwise.rs; do not edit.\n");
    for (suffix, scalar, load) in [
        ("f32", "float", "load_f32_unaligned"),
        ("f64", "double", "load_f64_unaligned"),
    ] {
        writeln!(
            chunks,
            "static inline __attribute__((always_inline)) void native_pointwise_{suffix}(\n\
             NativeKernelKind kind, const uint8_t* input, uint8_t* output,\n\
             int64_t count, const {scalar}* coefficients\n\
             ) {{\n    switch (kind) {{"
        )
        .unwrap();
        for (_, recipe) in pointwise::recipes() {
            writeln!(
                chunks,
                "        case NATIVE_POINTWISE_{}: {{",
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
            // The one-pointer spelling lets LLVM prove exact in-place access
            // safe for SIMD without making a restrict promise about host spans.
            chunks.push_str("            if (input == output) {\n");
            emit_loop(&mut chunks, recipe, scalar, load, "output");
            chunks.push_str("            } else {\n");
            emit_loop(&mut chunks, recipe, scalar, load, "input");
            chunks.push_str("            }\n            return;\n        }\n");
        }
        chunks.push_str("        default: __builtin_unreachable();\n    }\n}\n");
    }
    std::fs::write(out_dir.join("pointwise_chunks.inc"), chunks)
        .expect("failed to write native pointwise arithmetic");
}

fn emit_loop(code: &mut String, recipe: &Recipe, scalar: &str, load: &str, input: &str) {
    writeln!(
        code,
        "                for (int64_t item = 0; item < count; ++item) {{"
    )
    .unwrap();
    writeln!(
        code,
        "                    size_t offset = (size_t)item * sizeof({scalar});"
    )
    .unwrap();
    for (index, node) in recipe.nodes.iter().enumerate() {
        let expression = match node {
            Node::Read => format!("{load}({input} + offset)"),
            Node::Parameter(_) => continue,
            Node::Carry(_) => unreachable!("validated pointwise recipes have no carries"),
            Node::Binary(operator, left, right) => {
                let operator = match operator {
                    BinaryOp::Add => '+',
                    BinaryOp::Sub => '-',
                    BinaryOp::Mul => '*',
                };
                format!("v{left} {operator} v{right}")
            }
        };
        writeln!(
            code,
            "                    {scalar} v{index} = {expression};"
        )
        .unwrap();
    }
    writeln!(
        code,
        "                    __builtin_memcpy(output + offset, &v{}, sizeof(v{}));",
        recipe.result, recipe.result,
    )
    .unwrap();
    code.push_str("                }\n");
}
