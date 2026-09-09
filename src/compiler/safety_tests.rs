use super::*;

fn checked(source: &str) -> Compiler {
    let mut compiler = Compiler::new();
    compiler.quiet = true;
    assert!(compiler.parse(source, "concrete_safety.lyte"));
    assert!(compiler.check(), "{}\n{:?}", source, compiler.last_errors);
    compiler
}

fn assert_requirement_failure(compiler: &mut Compiler, clause: &str) {
    let error = compiler.specialize().unwrap_err();
    assert!(error.contains("safety check failed"), "{}", error);
    assert!(compiler.specialized_program().is_err());
    assert!(compiler.checked_program().is_some());
    assert_eq!(
        compiler.last_safety_errors.len(),
        1,
        "{:?}",
        compiler.last_errors
    );
    assert_eq!(compiler.last_errors.len(), 1, "{:?}", compiler.last_errors);
    assert!(compiler.last_safety_errors[0].message.contains(clause));
}

#[test]
fn generic_requirements_are_checked_in_ordinary_callers_and_type_applications() {
    for target in ["bounded", "bounded⟨bool⟩"] {
        for value in [-1, 0] {
            let mut compiler = checked(&format!(
                "bounded<T>(x: i32, value: T) require x >= 0 {{}}\n\
                 main {{ {target}({value}, true) }}"
            ));
            if value < 0 {
                assert_requirement_failure(&mut compiler, "`x >= 0`");
                assert_eq!(compiler.last_safety_errors[0].location.line, 2);
            } else {
                compiler.specialize().unwrap();
                assert!(compiler.last_errors.is_empty());
                compiler.specialized_program().unwrap().validate().unwrap();
            }
        }
    }
}

#[test]
fn ordinary_wrappers_must_establish_generic_call_requirements() {
    for body in ["bounded(-1, true)", "bounded(x, true)"] {
        let mut compiler = checked(&format!(
            "bounded<T>(x: i32, value: T) require x >= 0 {{}}\n\
             wrapper(x: i32) {{ {body} }}\n\
             main {{ wrapper(0) }}"
        ));
        assert_requirement_failure(&mut compiler, "`x >= 0`");
        assert_eq!(compiler.last_safety_errors[0].location.line, 2);
    }
    for wrapper in [
        "wrapper(x: i32) require x >= 0 { bounded(x, true) }",
        "wrapper(x: i32) { if x >= 0 { bounded(x, true) } }",
    ] {
        let mut compiler = checked(&format!(
            "bounded<T>(x: i32, value: T) require x >= 0 {{}}\n\
             {wrapper}\nmain {{ wrapper(0) }}"
        ));
        compiler.specialize().unwrap();
    }
}

#[test]
fn concrete_call_requirements_preserve_array_and_reference_coercions() {
    for (definition, call) in [
        (
            "bounded<T>(x: i32, values: [T]) require x >= 0 {}",
            "bounded(x, [1, 2])",
        ),
        (
            "bounded<T>(x: &i32, value: T) require x >= 0 {}",
            "bounded(x, true)",
        ),
        (
            "bounded<T>(x: i32, values: [T]) require x >= 0 require x < values.len {}",
            "bounded⟨i32⟩(x, [1, 2])",
        ),
    ] {
        for value in [-1, 0] {
            let mut compiler =
                checked(&format!("{definition}\nmain {{ var x = {value}; {call} }}"));
            if value < 0 {
                assert_requirement_failure(&mut compiler, "`x >= 0`");
            } else {
                compiler.specialize().unwrap();
            }
        }
    }
}

#[test]
fn selected_interface_implementations_and_externs_keep_their_requirements() {
    for prefix in ["", "extern fn "] {
        let body = if prefix.is_empty() { " {}" } else { "" };
        for value in [-1, 0] {
            let mut compiler = checked(&format!(
                "interface Bounded<T> {{ bounded(x: i32, value: T) }}\n\
                 {prefix}bounded(x: i32, value: bool) require x >= 0{body}\n\
                 wrapper<T>(x: i32, value: T) where Bounded<T> {{ bounded({value}, value) }}\n\
                 main {{ wrapper(0, true) }}"
            ));
            if value < 0 {
                assert_requirement_failure(&mut compiler, "`x >= 0`");
            } else {
                compiler.specialize().unwrap();
            }
        }
    }
}

#[test]
fn local_and_global_function_values_remain_indirect() {
    // This deliberately characterizes the existing proof limitation: a stored
    // function value does not transport its target's require clauses. These
    // calls must not be confused with the same-named direct functions.
    let mut compiler = checked(
        "bounded<T>(x: i32, value: T) require x >= 0 {}
         var callback: (i32, bool) -> void
         callback(x: i32, value: bool) require x >= 0 {}
         main {
             callback = bounded⟨bool⟩
             callback(-1, true)
             let bounded = bounded⟨bool⟩
             bounded(-1, true)
         }",
    );
    compiler.specialize().unwrap();
    let program = compiler.specialized_program().unwrap();
    let main = program
        .function_instance(program.instance_for_entry(Name::str("main")).unwrap())
        .unwrap();
    let mut local_calls = 0;
    let mut global_calls = 0;
    for node in main.arena.nodes() {
        if let CheckedExpr::Call(callee, _) = &node.kind {
            match &main.arena[*callee] {
                CheckedExpr::Id(Reference::Local(_)) => local_calls += 1,
                CheckedExpr::Id(Reference::Instance(target)) => {
                    assert!(matches!(
                        program.instance(*target),
                        CheckedDecl::Global { .. }
                    ));
                    global_calls += 1;
                }
                _ => panic!("unexpected call target"),
            }
        }
    }
    assert_eq!((local_calls, global_calls), (1, 1));
}

#[test]
fn equivalent_requirements_in_multiple_instances_report_once() {
    let mut compiler = checked(
        "bounded<U>(x: i32, value: U) require x >= 0 {}
         wrapper<T>(value: T) { bounded(-1, value) }
         main { wrapper(true); wrapper(1) }",
    );
    assert_requirement_failure(&mut compiler, "`x >= 0`");
    assert_eq!(compiler.last_safety_errors[0].location.line, 2);
    let diagnostics = compiler.last_errors.clone();
    assert_requirement_failure(&mut compiler, "`x >= 0`");
    assert_eq!(compiler.last_errors, diagnostics);
}

#[test]
fn separate_call_sites_keep_separate_requirement_diagnostics() {
    let mut compiler = checked(
        "bounded<T>(x: i32, value: T) require x >= 0 {}
         main { bounded(-1, true); bounded(-2, true) }",
    );
    assert!(compiler.specialize().is_err());
    assert_eq!(compiler.last_safety_errors.len(), 2);
    assert_ne!(
        compiler.last_safety_errors[0].location,
        compiler.last_safety_errors[1].location
    );
}

#[test]
fn different_concrete_requirements_at_one_call_site_remain_distinct() {
    let mut compiler = checked(
        "bounded<N>(xs: [i32; N], idx: i32) require idx < N {}
         wrapper<N>(xs: [i32; N]) { bounded(xs, 99) }
         main { wrapper([1, 2]); wrapper([1, 2, 3]) }",
    );
    assert!(compiler.specialize().is_err());
    assert_eq!(
        compiler.last_safety_errors.len(),
        2,
        "{:?}",
        compiler.last_errors
    );
    assert_eq!(
        compiler.last_safety_errors[0].location,
        compiler.last_safety_errors[1].location
    );
    for clause in ["`idx < 2`", "`idx < 3`"] {
        assert!(
            compiler
                .last_errors
                .iter()
                .any(|error| error.contains(clause)),
            "{:?}",
            compiler.last_errors
        );
    }
}

#[test]
fn unsupported_requirements_still_fail_conservatively_after_specialization() {
    // The call is valid, but equality is outside the existing call-site proof
    // grammar. Concrete integration must not silently accept an unproved clause
    // or expand the prover to make this witness pass.
    let mut compiler = checked(
        "bounded<T>(x: i32, value: T) require x == 0 {}
         main { bounded(0, true) }",
    );
    assert_requirement_failure(&mut compiler, "`x == 0`");
}

#[test]
fn template_safety_still_checks_unreachable_definitions_once() {
    let mut compiler = Compiler::new();
    compiler.quiet = true;
    assert!(compiler.parse(
        "bounded(x: i32) require x >= 0 {}
         unused { bounded(-1) }
         main {}",
        "unreachable.lyte",
    ));
    assert!(!compiler.check());
    let diagnostics = compiler.last_errors.clone();
    assert_eq!(diagnostics.len(), 1);
    assert!(compiler.specialize().is_err());
    assert_eq!(compiler.last_errors, diagnostics);
    assert_eq!(compiler.last_safety_errors.len(), 1);
}
