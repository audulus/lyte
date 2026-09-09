use super::*;

fn inspect(source: &str) -> Vec<LoopRegion> {
    inspect_with_prelude("", source)
}

fn inspect_with_prelude(prelude: &str, source: &str) -> Vec<LoopRegion> {
    let mut compiler = Compiler::new();
    compiler.quiet = true;
    assert!(
        compiler.parse(prelude, "<prelude>"),
        "{:?}",
        compiler.last_errors
    );
    assert!(
        compiler.parse(source, "value-loops.lyte"),
        "{:?}",
        compiler.last_errors
    );
    assert!(compiler.check(), "{:?}", compiler.last_errors);
    compiler.specialize().unwrap();
    compiler
        .specialized_program()
        .unwrap()
        .functions()
        .flat_map(|(_, function)| analyze_function(function).into_values())
        .collect()
}

fn loops(body: &str) -> Vec<LoopRegion> {
    inspect(&format!(
        "kernel(a: &[f32; 4], b: &[f32; 4], gain: f32, bias: f32) -> f32 {{
                var state = 1.0
                var coefficient = gain
                for i in 0 .. 4 {{ {body} }}
                state
            }}
            main {{
                var a: [f32; 4]
                var b: [f32; 4]
                print(kernel(a, b, 0.5, 0.25) as i32)
            }}"
    ))
}

#[test]
fn pointwise_contract_uses_invariants_and_immutable_local_versions() {
    for body in [
        "b[i] = a[i] * gain + bias",
        "let x = a[i]; var y = x * gain; y = y + bias; b[i] = y",
        "a[i] = a[i] * 0.5 + 1.0",
    ] {
        let regions = loops(body);
        assert_eq!(regions.len(), 1);
        assert_eq!(
            regions[0].pointwise().map(|kernel| kernel.recipe),
            Some(pointwise::AFFINE),
            "missing affine map for {}",
            body
        );
    }
    for body in [
        "let unused = a[i] * bias; b[i] = a[i] * gain + bias",
        "coefficient = coefficient + bias; b[i] = a[i] * coefficient + bias",
        "b[i] = a[i] * gain + bias; a[i] = b[i] * gain + bias",
    ] {
        let regions = loops(body);
        assert!(
            regions[0].pointwise().is_none(),
            "unexpected map for {}",
            body
        );
    }
    let regions = inspect(
        "map(a: &[f32; 4], b: &[f32; 4], gain: &f32) {
                 for i in 0 .. 4 { b[i] = a[i] * gain + 1.0 }
             }
             main {
                 var a: [f32; 4]
                 var b: [f32; 4]
                 var gain = 0.5
                 map(a, b, gain)
             }",
    );
    assert!(
        regions.is_empty(),
        "reference scalars cannot become invariants"
    );
}

fn pointwise_loop(body: &str, scalar: &str) -> LoopRegion {
    let mut regions = inspect(&format!(
            "kernel(input: &[{scalar}; 4], output: &[{scalar}; 4], a: {scalar}, b: {scalar}, c: {scalar}, d: {scalar}) {{
                 for i in 0 .. 4 {{ {body} }}
             }}
             main {{
                 var input: [{scalar}; 4]
                 var output: [{scalar}; 4]
                 kernel(input, output, 0.5{scalar}, 0.25{scalar}, 0.125{scalar}, 0.0625{scalar})
             }}"
        ));
    assert_eq!(regions.len(), 1, "discovery lost {}", body);
    regions.pop().unwrap()
}

#[test]
fn pointwise_recipes_match_ordered_reuse_and_coefficient_roles() {
    for scalar in ["f32", "f64"] {
        for (name, body, coefficient_count) in [
            ("offset_scale", "output[i] = (input[i] - b) * a", 2),
            (
                "cubic",
                "let x = input[i]; output[i] = x - a * ((x*x)*x)",
                1,
            ),
            (
                "cubic",
                "var x = input[i]; let original = x; var y = x*x;
                     y = y*x; y = a*y; x = original-y; output[i] = x",
                1,
            ),
            (
                "horner",
                "let x = input[i]; output[i] = ((a*x+b)*x+c)*x+d",
                4,
            ),
            (
                "horner",
                "let x = input[i]; var y = a*x; y = y+b; y = y*x;
                     y = y+c; y = y*x; y = y+d; output[i] = y",
                4,
            ),
        ] {
            let region = pointwise_loop(body, scalar);
            let kernel = region.pointwise().expect(body);
            assert_eq!(kernel.recipe.recipe().name, name, "{}", body);
            assert_eq!(kernel.coefficients.len(), coefficient_count);
            assert_ne!(kernel.input, kernel.output);
        }
        let region = pointwise_loop("let x = input[i]; input[i] = ((a*x+a)*x+a)*x+a", scalar);
        let kernel = region.pointwise().expect("aliased coefficient roles");
        assert_eq!(kernel.recipe.recipe().name, "horner");
        assert_eq!(kernel.input, kernel.output);
        assert_eq!(kernel.coefficients.len(), 4);
        assert!(kernel
            .coefficients
            .iter()
            .all(|value| *value == kernel.coefficients[0]));
    }
}

#[test]
fn pointwise_recipes_reject_changed_identity_order_and_effects() {
    for body in [
        // Repeated indexed reads are not reuse of one sampled value.
        "output[i] = input[i] - a * ((input[i]*input[i])*input[i])",
        // The read is shared, but changing association or operand order
        // changes the recipe's separately rounded arithmetic.
        "let x = input[i]; output[i] = x - a * (x*(x*x))",
        "let x = input[i]; output[i] = x - ((x*x)*x) * a",
        "let x = input[i]; output[i] = ((x*a+b)*x+c)*x+d",
        // An apparently harmless local update changes later value uses.
        "var x = input[i]; x = x*x; output[i] = x - a * ((x*x)*x)",
        "let x = input[i]; let unused = input[i]; output[i] = x - a*((x*x)*x)",
        "let x = input[i]; let unused = x*b; output[i] = x - a*((x*x)*x)",
        "let x = input[i]; output[i] = x - a*((x*x)*x); input[i] = x",
    ] {
        let region = pointwise_loop(body, "f32");
        assert!(
            region.pointwise().is_none(),
            "unexpected kernel for {}",
            body
        );
    }
}

fn biquad_loop(body: &str, scalar: &str) -> LoopRegion {
    let mut regions = inspect(&format!(
            "kernel(input: &[{scalar}; 4], output: &[{scalar}; 4], b0: {scalar}, b1: {scalar}, b2: {scalar}, a1: {scalar}, a2: {scalar}) -> {scalar} {{
                 var y2 = 0.25{scalar}
                 var x1 = 0.5{scalar}
                 var y1 = 0.75{scalar}
                 var x2 = 1.0{scalar}
                 for i in 0 .. 4 {{ {body} }}
                 x1 + x2 + y1 + y2
             }}
             main {{
                 var input: [{scalar}; 4]
                 var output: [{scalar}; 4]
                 print(kernel(input, output, 0.5{scalar}, 0.25{scalar}, 0.125{scalar}, 0.0625{scalar}, 0.03125{scalar}) as i32)
             }}"
        ));
    assert_eq!(regions.len(), 1);
    regions.pop().unwrap()
}

#[test]
fn biquad_contract_follows_four_state_roles_and_staged_values() {
    for scalar in ["f32", "f64"] {
        for body in [
            "let x = input[i]
                 let y = b0*x + b1*x1 + b2*x2 - a1*y1 - a2*y2
                 x2 = x1; x1 = x; y2 = y1; y1 = y
                 output[i] = y",
            "var x = input[i]
                 var y = b0*x
                 y = y + b1*x1
                 y = y + b2*x2
                 y = y - a1*y1
                 y = y - a2*y2
                 let old_input = x1
                 let old_output = y1
                 y1 = y; x1 = x; x2 = old_input; y2 = old_output
                 output[i] = y",
        ] {
            let region = biquad_loop(body, scalar);
            let kernel = region.biquad().expect("missing biquad");
            assert_eq!(region.carries.len(), 4);
            assert_eq!(region.inputs.len(), 5);
            let [x1, x2, y1, y2] = kernel.carries.map(|carry| &region.carries[carry]);
            assert_eq!(x2.next, x1.incoming);
            assert_eq!(y2.next, y1.incoming);
            assert!(matches!(
                region.operation(x1.next),
                Some(Operation::ReadElement { stream }) if *stream == kernel.input
            ));
            assert!(matches!(
                region.tape.last(),
                Some(LoopStep::StoreElement { stream, value })
                    if *stream == kernel.output && *value == y1.next
            ));
        }
    }
}

#[test]
fn biquad_contract_rejects_changed_arithmetic_state_and_effects() {
    let body = "let x = input[i]
                    let y = b0*x + b1*x1 + b2*x2 - a1*y1 - a2*y2
                    x2 = x1; x1 = x; y2 = y1; y1 = y
                    output[i] = y";
    for changed in [
        body.replace("x2 = x1; x1 = x", "x1 = x; x2 = x1"),
        body.replace("a2*y2", "a2*y1"),
        body.replace("b0*x", "x*b0"),
        body.replace("b0*x + b1*x1 + b2*x2", "b0*x + (b1*x1 + b2*x2)"),
        body.replace("let y =", "let unused = input[i]; let y ="),
        body.replace("output[i] = y", "output[i] = y; input[i] = x"),
    ] {
        let region = biquad_loop(&changed, "f32");
        assert!(
            region.biquad().is_none(),
            "unexpected biquad for {}",
            changed
        );
    }
}

#[test]
fn reference_call_exposure_remains_conservative_for_owned_scalar_locals() {
    let regions = inspect(
        "observe(value: &f32) { print(value as i32) }
             main {
                 var input: [f32; 4]
                 var output: [f32; 4]
                 var gain = 0.5
                 observe(gain)
                 for i in 0 .. 4 { output[i] = input[i]*gain+1.0 }
             }",
    );
    assert!(
        regions.is_empty(),
        "reference-exposed scalar cannot become an invariant"
    );
}
