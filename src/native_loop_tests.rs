//! End-to-end consumers of checked identities and the optional native-loop path.
//! Raw alias/cancellation/frame-boundary contracts are covered by bridge tests.

use crate::stack_interp_bridge::StackBackend;
use crate::stack_ir::{NativeStreamBody, StackOp, StackProgram};
use crate::*;

fn checked(source: &str, entries: &[&str]) -> Compiler {
    let mut compiler = Compiler::new();
    compiler.quiet = true;
    compiler.set_entry_points(entries);
    assert!(
        compiler.parse(source, "native_loop_tests.lyte"),
        "{:?}",
        compiler.last_errors
    );
    assert!(compiler.check(), "{:?}", compiler.last_errors);
    compiler.specialize().unwrap();
    compiler
}

fn native_bodies(program: &StackProgram) -> Vec<&NativeStreamBody> {
    program
        .functions
        .iter()
        .flat_map(|function| &function.ops)
        .filter_map(|op| {
            if let StackOp::NativeLoop(kernel) = op {
                Some(&kernel.body)
            } else {
                None
            }
        })
        .collect()
}

fn variants(compiler: &Compiler) -> (StackProgram, StackProgram) {
    let ordinary = compiler.compile_stack().unwrap();
    assert!(
        native_bodies(&ordinary).is_empty(),
        "ordinary compilation must remain opt-out"
    );
    let selected = compiler.compile_stack_native_loops().unwrap();
    (ordinary, selected)
}

fn execute(program: &StackProgram, entries: &[&str]) -> Vec<(i64, Vec<u8>)> {
    let mut backend = Box::new(StackBackend::new(program));
    let mut globals = vec![0u8; program.globals_size.max(1)];
    entries
        .iter()
        .map(|entry| {
            let function = program.entry_points[&Name::str(entry)];
            let result = backend.call_entry(function, globals.as_mut_ptr());
            assert!(!backend.cancelled());
            assert_eq!(backend.trap_reason(), crate::cancel::TRAP_NONE);
            (result, globals.clone())
        })
        .collect()
}

// Replace the retained loop entry with a trap without changing jump coordinates.
// Valid, non-polling examples must execute native work to reach their results.
pub(crate) fn forbid_fallback(program: &mut StackProgram) {
    let mut gateways = 0;
    for function in &mut program.functions {
        for index in 0..function.ops.len() {
            if let StackOp::NativeLoop(kernel) = &function.ops[index] {
                assert!(kernel.done >= 2);
                function.ops[index + 1] = StackOp::I64Const(0);
                function.ops[index + 2] = StackOp::Assert;
                gateways += 1;
            }
        }
    }
    assert!(gateways > 0, "expected executable native work");
}

fn execute_native(mut program: StackProgram, entries: &[&str]) -> Vec<(i64, Vec<u8>)> {
    let expected = execute(&program, entries);
    forbid_fallback(&mut program);
    assert_eq!(execute(&program, entries), expected);
    expected
}

#[test]
fn pointwise_extensions_execute_natively_in_both_widths() {
    let inputs = [-0.0_f64, 0.0, 0.1, 0.25, 0.5, 0.75, 0.9, 1.0];
    for scalar in ["f32", "f64"] {
        let cases: [(String, fn(f32) -> f32, fn(f64) -> f64); 4] = [
            ("let y = x*gain".into(), |x| x * -1.25, |x| x * -1.25),
            ("let y = x+bias".into(), |x| x + 0.3, |x| x + 0.3),
            ("let y = x*x".into(), |x| x * x, |x| x * x),
            (
                format!(
                    "let cube = (x*x)*x
                     let curve = x*(x*6.0{scalar}-15.0{scalar})+10.0{scalar}
                     let y = cube*curve"
                ),
                |x| ((x * x) * x) * (x * (x * 6.0 - 15.0) + 10.0),
                |x| ((x * x) * x) * (x * (x * 6.0 - 15.0) + 10.0),
            ),
        ];
        let values = inputs
            .iter()
            .map(|x| format!("{x:?}{scalar}"))
            .collect::<Vec<_>>()
            .join(", ");
        for (body, reference_f32, reference_f64) in cases {
            // Independent scalar arithmetic, including separate rounding at each step.
            let expected: Vec<u8> = inputs
                .iter()
                .flat_map(|&x| {
                    if scalar == "f32" {
                        reference_f32(x as f32).to_le_bytes().to_vec()
                    } else {
                        reference_f64(x).to_le_bytes().to_vec()
                    }
                })
                .collect();
            for destination in ["output", "input"] {
                let compiler = checked(
                    &format!(
                        r#"
                    var input: [{scalar}; 8]
                    var output: [{scalar}; 8]
                    main() -> i32 {{
                        input = [{values}]
                        let gain = -1.25{scalar}
                        let bias = 0.3{scalar}
                        for i in 0 .. 8 {{
                            let x = input[i]
                            {body}
                            {destination}[i] = y
                        }}
                        0
                    }}
                    "#
                    ),
                    &["main"],
                );
                let (ordinary, native) = variants(&compiler);
                let results = execute(&ordinary, &["main"]);
                let offset = compiler
                    .globals_info_with_offset(crate::cancel::CANCEL_FLAG_RESERVED as usize)
                    .into_iter()
                    .find(|entry| entry.0 == destination)
                    .unwrap()
                    .1;
                assert_eq!(&results[0].1[offset..offset + expected.len()], &expected);
                assert_eq!(
                    execute_native(native, &["main"]),
                    results,
                    "{scalar}, {destination}, {body}"
                );
            }
        }
    }
}

#[test]
fn affine_slices_publish_output_and_reload_headers_after_callback() {
    struct Redirect {
        headers: [*mut u8; 2],
        replacements: [*const u8; 2],
        output: *const u8,
        size: usize,
        observed: Vec<u8>,
    }
    unsafe extern "C" fn redirect(data: *mut u8) -> bool {
        let probe = &mut *data.cast::<Redirect>();
        probe.observed = std::slice::from_raw_parts(probe.output, probe.size).to_vec();
        for (header, replacement) in probe.headers.iter().copied().zip(probe.replacements) {
            std::ptr::write_unaligned(header.cast::<*const u8>(), replacement);
        }
        false
    }

    for scalar in ["f32", "f64"] {
        let compiler = checked(
            &format!(
                "var input: [{scalar}]
                 var output: [{scalar}]
                 process() -> i32 {{
                     var prefix = 0
                     for j in 0 .. 1022 {{ prefix = prefix + 1 }}
                     if input.len >= 4 && output.len >= 4 {{
                         for i in 0 .. 4 {{ output[i] = input[i]*0.5{scalar}+1.0{scalar} }}
                     }}
                     prefix
                 }}"
            ),
            &["process"],
        );
        let info = compiler.globals_info_with_offset(crate::cancel::CANCEL_FLAG_RESERVED as usize);
        let offsets =
            ["input", "output"].map(|name| info.iter().find(|entry| entry.0 == name).unwrap().1);
        let bytes = |values: &[f64]| -> Vec<u8> {
            values
                .iter()
                .flat_map(|&value| {
                    if scalar == "f64" {
                        value.to_le_bytes().to_vec()
                    } else {
                        (value as f32).to_le_bytes().to_vec()
                    }
                })
                .collect()
        };
        for callback in [false, true] {
            let (ordinary, mut native) = variants(&compiler);
            if !callback {
                forbid_fallback(&mut native);
            }
            for program in [ordinary, native] {
                let input = bytes(&[2.0, 4.0, 6.0, 8.0]);
                let replacement = bytes(&[10.0, 12.0, 14.0, 16.0]);
                let mut output = bytes(&[0.0; 4]);
                let mut redirected = output.clone();
                let mut globals = vec![0u8; program.globals_size];
                let mut probe = Redirect {
                    headers: offsets.map(|offset| unsafe { globals.as_mut_ptr().add(offset) }),
                    replacements: [replacement.as_ptr(), redirected.as_mut_ptr()],
                    output: output.as_ptr(),
                    size: output.len(),
                    observed: Vec::new(),
                };
                for (offset, pointer) in offsets.iter().zip([input.as_ptr(), output.as_mut_ptr()]) {
                    globals[*offset..*offset + 8].copy_from_slice(&(pointer as u64).to_le_bytes());
                    globals[*offset + 8..*offset + 12].copy_from_slice(&4i32.to_le_bytes());
                }
                let mut backend = StackBackend::new(&program);
                if callback {
                    backend
                        .set_cancel_callback(Some(redirect), (&mut probe as *mut Redirect).cast());
                }
                assert_eq!(
                    backend.call_entry(program.entry, globals.as_mut_ptr()),
                    1022
                );
                assert_eq!(backend.trap_reason(), crate::cancel::TRAP_NONE);
                if callback {
                    assert_eq!(probe.observed, bytes(&[2.0, 3.0, 0.0, 0.0]));
                    assert_eq!(output, probe.observed);
                    assert_eq!(redirected, bytes(&[0.0, 0.0, 8.0, 9.0]));
                } else {
                    assert_eq!(output, bytes(&[2.0, 3.0, 4.0, 5.0]));
                }
            }
        }
    }
}

#[test]
fn captured_outer_and_uncaptured_shadow_select_affine_and_preserve_closure() {
    let compiler = checked(
        r#"
        kernel(input: &[f32; 4], output: &[f32; 4]) -> f32 {
            let gain = 0.25
            let read = || { gain }
            {
                let gain = 0.5
                for i in 0 .. 4 { output[i] = input[i]*gain+1.0 }
            }
            read()
        }
        main() -> i32 {
            var input = [2.0, 4.0, 6.0, 8.0]
            var output: [f32; 4]
            let captured = kernel(input, output)
            ((captured + output[0]+output[1]+output[2]+output[3])*100.0) as i32
        }
    "#,
        &["main"],
    );
    let (ordinary, selected) = variants(&compiler);
    assert!(matches!(
        native_bodies(&selected).as_slice(),
        [NativeStreamBody::Pointwise { .. }]
    ));
    let expected = execute(&ordinary, &["main"]);
    assert_eq!(expected[0].0, 1425);
    assert_eq!(execute_native(selected, &["main"]), expected);

    let mut renamed = compiler.specialized_program().unwrap().clone();
    for decl in &mut renamed.decls.decls {
        let Decl::Func(function) = decl else { continue };
        for local in &mut function.arena.locals {
            local.name = Name::str("same");
        }
        for id in function.arena.ids() {
            let mut expr = function.arena[id].clone();
            match &mut expr {
                Expr::Id(name)
                | Expr::TypeApp(name, _)
                | Expr::Let(name, ..)
                | Expr::Var(name, ..)
                | Expr::For { var: name, .. } => *name = Name::str("same"),
                Expr::Lambda { params, .. } => {
                    for param in params {
                        param.name = Name::str("same");
                    }
                }
                _ => {}
            }
            function.arena.replace(id, expr, function.arena.ty(id));
        }
    }
    renamed.validate().unwrap();
    let mut codegen = crate::stack_codegen::StackCodegen::new();
    codegen.native_loops = true;
    let mut program = codegen.compile(&renamed).unwrap();
    crate::stack_inline::inline_trivial(&mut program);
    for function in &mut program.functions {
        crate::stack_rebase_lm::rebase(function);
        crate::stack_optimize::optimize(function);
        crate::stack_rebase_lm::patch_call_preserve(function);
    }
    assert_eq!(execute_native(program, &["main"]), expected);
}

#[test]
fn one_pole_snapshots_and_state_survive_repeated_entry_calls() {
    for scalar in ["f32", "f64"] {
        let compiler = checked(
            &format!(
                r#"
            var input: [{scalar}; 4]
            var output: [{scalar}; 4]
            var history: {scalar}
            init() {{
                input[0] = 2.0{scalar}; input[1] = 4.0{scalar}
                input[2] = 6.0{scalar}; input[3] = 8.0{scalar}
                history = 1.0{scalar}
            }}
            process() -> i32 {{
                var state = history
                let gain = 0.5{scalar}
                let feedback = 0.25{scalar}
                for i in 0 .. 4 {{
                    let before = state
                    var x = input[i]
                    var feed = x*gain
                    let decay = before*feedback
                    state = feed+decay
                    output[i] = state
                }}
                history = state
                (state*1024.0{scalar}) as i32
            }}
        "#
            ),
            &["init", "process"],
        );
        let (ordinary, selected) = variants(&compiler);
        assert!(matches!(
            native_bodies(&selected).as_slice(),
            [NativeStreamBody::OnePole { .. }]
        ));
        let entries = ["init", "process", "process", "process", "init", "process"];
        let expected = execute(&ordinary, &entries);
        assert_eq!(expected[1].0, 5012);
        assert_ne!(expected[1].0, expected[2].0);
        assert_eq!(expected[1], expected[5]);
        assert_eq!(execute_native(selected, &entries), expected, "{scalar}");
    }
}

#[test]
fn biquad_staged_history_updates_preserve_every_carried_state() {
    for scalar in ["f32", "f64"] {
        let compiler = checked(
            &format!(
                r#"
            var input: [{scalar}; 4]
            var output: [{scalar}; 4]
            var history_x1: {scalar}
            var history_x2: {scalar}
            var history_y1: {scalar}
            var history_y2: {scalar}
            init() {{
                input[0] = 2.0{scalar}; input[1] = 4.0{scalar}
                input[2] = 6.0{scalar}; input[3] = 8.0{scalar}
                history_x1 = 0.5{scalar}; history_x2 = 1.0{scalar}
                history_y1 = 0.75{scalar}; history_y2 = 0.25{scalar}
            }}
            process() -> i32 {{
                var y2 = history_y2
                var x1 = history_x1
                var y1 = history_y1
                var x2 = history_x2
                let b0 = 0.5{scalar}; let b1 = 0.25{scalar}; let b2 = 0.125{scalar}
                let a1 = 0.0625{scalar}; let a2 = 0.03125{scalar}
                for i in 0 .. 4 {{
                    let x = input[i]
                    var y = b0*x
                    y = y+b1*x1
                    y = y+b2*x2
                    y = y-a1*y1
                    y = y-a2*y2
                    let old_input = x1
                    let old_output = y1
                    y1 = y; x1 = x; x2 = old_input; y2 = old_output
                    output[i] = y
                }}
                history_x1 = x1; history_x2 = x2
                history_y1 = y1; history_y2 = y2
                ((x1+x2+y1+y2)*1024.0{scalar}) as i32
            }}
        "#
            ),
            &["init", "process"],
        );
        let (ordinary, selected) = variants(&compiler);
        assert!(matches!(
            native_bodies(&selected).as_slice(),
            [NativeStreamBody::Biquad { .. }]
        ));
        let entries = ["init", "process", "process", "process", "init", "process"];
        let expected = execute(&ordinary, &entries);
        assert_ne!(expected[1].0, expected[2].0);
        assert_eq!(expected[1], expected[5]);
        assert_eq!(execute_native(selected, &entries), expected, "{scalar}");
    }
}

#[test]
fn same_typed_generic_globals_select_distinct_native_streams() {
    let compiler = checked(
        r#"
        var input<T>: [f32; 4]
        var output<T>: [f32; 4]
        main() -> i32 {
            input⟨i32⟩[0] = 2.0
            input⟨f32⟩[0] = 8.0
            for i in 0 .. 4 { output⟨i32⟩[i] = input⟨i32⟩[i]*0.5+1.0 }
            for i in 0 .. 4 { output⟨f32⟩[i] = input⟨f32⟩[i]*0.5+1.0 }
            (output⟨i32⟩[0]*100.0 + output⟨f32⟩[0]) as i32
        }
    "#,
        &["main"],
    );
    let program = compiler.specialized_program().unwrap();
    let inputs: Vec<_> = program
        .globals()
        .filter(|(id, _)| {
            let source = program.instances[id.index()].definition;
            program.globals().any(|(other, _)| {
                other != *id && program.instances[other.index()].definition == source
            })
        })
        .collect();
    assert_eq!(
        inputs.len(),
        4,
        "two concrete instances of each same-typed global"
    );
    let (ordinary, selected) = variants(&compiler);
    assert_eq!(native_bodies(&selected).len(), 2);
    assert!(native_bodies(&selected)
        .iter()
        .all(|body| matches!(body, NativeStreamBody::Pointwise { .. })));
    let expected = execute(&ordinary, &["main", "main"]);
    assert_eq!(expected[0].0, 205);
    assert_eq!(execute_native(selected, &["main", "main"]), expected);
}

#[test]
fn unsupported_indexing_calls_and_captured_inputs_remain_ordinary() {
    for (body, captured) in [
        ("output[i] = input[i+1]*gain+1.0", false),
        ("output[i] = twice(input[i])", false),
        ("output[i] = input[i]*gain+1.0", true),
    ] {
        let capture = if captured {
            "let read = || { gain }"
        } else {
            ""
        };
        let result = if captured { "read()" } else { "gain" };
        let compiler = checked(
            &format!(
                r#"
            twice(x: f32) -> f32 {{ x+x }}
            kernel(input: &[f32; 4], output: &[f32; 4]) -> f32 {{
                let gain = 0.5
                {capture}
                for i in 0 .. 3 {{ {body} }}
                {result}
            }}
            main() -> i32 {{
                var input = [2.0, 4.0, 6.0, 8.0]
                var output: [f32; 4]
                let value = kernel(input, output)
                ((value+output[0]+output[1]+output[2])*100.0) as i32
            }}
        "#
            ),
            &["main"],
        );
        let (ordinary, selected) = variants(&compiler);
        assert!(
            native_bodies(&selected).is_empty(),
            "unexpected native selection for {}",
            body
        );
        assert_eq!(execute(&selected, &["main"]), execute(&ordinary, &["main"]));
    }

    let compiler = checked(
        "main() -> i32 {
             var input = [2.0, 4.0, 6.0, 8.0]
             var output: [f32; 4]
             let map = |gain: f32| {
                 for i in 0 .. 4 { output[i] = input[i]*gain+1.0 }
             }
             map(0.5)
             (output[0]+output[1]+output[2]+output[3]) as i32
         }",
        &["main"],
    );
    let (ordinary, selected) = variants(&compiler);
    assert!(native_bodies(&selected).is_empty());
    let expected = execute(&ordinary, &["main"]);
    assert_eq!(expected[0].0, 14);
    assert_eq!(execute(&selected, &["main"]), expected);
}

#[test]
fn native_setup_frame_overflow_retries_the_exact_ordinary_function() {
    fn source(padding: usize, padding_first: bool) -> String {
        let allocation = format!(
            "var padding: [f32; {padding}]; padding[{}] = gain;\n\
             var marker: [f32; 1]; marker[0] = padding[{}];",
            padding - 1,
            padding - 1,
        );
        let map = "for i in 0 .. 4 { output[i] = input[i]*gain+bias }";
        let body = if padding_first {
            format!("{allocation}\n{map}")
        } else {
            format!("{map}\n{allocation}")
        };
        format!(
            r#"
            helper(x: f32) -> f32 {{ x }}
            kernel(input: &[f32; 4], output: &[f32; 4], gain: f32, bias: f32) -> f32 {{
                {body}
                let finish = |x| x
                helper(finish(marker[0]+output[3]))
            }}
            main() -> i32 {{
                var input: [f32; 4]
                var output: [f32; 4]
                kernel(input, output, 2.0, 0.5) as i32
            }}
        "#
        )
    }

    for padding_first in [true, false] {
        let small = checked(&source(16, padding_first), &["main"]);
        let (ordinary, selected) = variants(&small);
        assert_eq!(native_bodies(&selected).len(), 1);
        let kernel = ordinary
            .functions
            .iter()
            .find(|f| f.name == "kernel")
            .unwrap();
        let frame_slots = usize::from(kernel.local_count) + kernel.local_memory as usize / 8;
        // Add only array storage; the rest of this function's slot demand stays
        // unchanged. Its ordinary frame fits, but native setup needs extra slots.
        let padding = 16 + 2 * (usize::from(u16::MAX) - frame_slots);
        let compiler = checked(&source(padding, padding_first), &["main"]);
        let (ordinary, selected) = variants(&compiler);
        assert!(native_bodies(&selected).is_empty());
        assert_eq!(ordinary.entry_points, selected.entry_points);
        assert_eq!(ordinary.functions.len(), selected.functions.len());
        for (prior, retried) in ordinary.functions.iter().zip(&selected.functions) {
            assert_eq!(prior.name, retried.name);
            assert_eq!(prior.local_count, retried.local_count);
            assert_eq!(prior.local_memory, retried.local_memory);
            assert_eq!(prior.ops, retried.ops);
        }
        let expected = execute(&ordinary, &["main"]);
        assert_eq!(expected[0].0, 2);
        assert_eq!(execute(&selected, &["main"]), expected);
    }
}
