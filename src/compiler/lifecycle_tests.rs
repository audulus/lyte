use super::*;

fn parsed(source: &str) -> Compiler {
    let mut compiler = Compiler::new();
    compiler.quiet = true;
    assert!(
        compiler.parse(source, "lifecycle.lyte"),
        "{:?}",
        compiler.last_errors
    );
    compiler
}

fn assert_no_executable(compiler: &Compiler) {
    assert!(compiler.specialized_program().is_err());
    assert!(compiler.compile_vm().is_err());
    assert!(compiler.compile_stack().is_err());
    assert!(compiler.compile_stack_unfused().is_err());
    assert!(compiler.compile_program().is_err());
    #[cfg(feature = "cranelift")]
    {
        assert!(compiler.jit().is_err());
        assert!(compiler.jit_multi().is_err());
    }
}

#[test]
fn partial_editor_analysis_never_authorizes_execution() {
    for source in [
        "broken() -> i32 { true }",
        "broken() -> i32 { 1wat }",
        "broken() -> i32 { 1 / 0 }",
    ] {
        let mut compiler = parsed("good() -> i32 { let x = 42; x }");
        compiler.parse(source, "broken.lyte");
        assert!(!compiler.analyze());
        let snapshot = compiler.source_analysis().unwrap().clone();
        let definition = snapshot.declarations().named_ids(Name::str("good"))[0];
        assert!(snapshot.body(definition).is_some());
        compiler.set_entry_points(&["good"]);
        compiler.last_errors.clear();
        compiler.last_parse_errors.clear();
        compiler.last_type_errors.clear();
        compiler.last_safety_errors.clear();
        assert!(compiler.specialize().is_err());
        assert_no_executable(&compiler);
        assert!(!compiler.analyze());
        assert!(!compiler.last_errors.is_empty());
        assert!(compiler.specialize().is_err());
        assert_no_executable(&compiler);

        compiler.parse("another() {}", "another.lyte");
        assert!(compiler.source_analysis().is_none());
        // An explicitly cloned snapshot retains its own inventory; querying it
        // cannot attach it to new source or grant that compiler executable input.
        assert_eq!(
            snapshot.declarations().function(definition).unwrap().name,
            Name::str("good")
        );
        assert!(snapshot.body(definition).is_some());
        assert!(compiler.specialize().is_err());
        assert_no_executable(&compiler);
    }
}

#[test]
fn root_changes_and_backends_reuse_immutable_templates() {
    let mut compiler = parsed(
        "var count: i32
         identity<T>(value: T) -> T { value }
         main() -> i32 { identity(1) }
         other() -> i32 { identity(42) }",
    );
    assert!(compiler.check(), "{:?}", compiler.last_errors);
    let templates = compiler.checked_program().unwrap().clone();

    for (root, unused, expected) in [
        ("main", "other", 1),
        ("other", "main", 42),
        ("main", "other", 1),
    ] {
        compiler.set_entry_points(&[root]);
        assert_no_executable(&compiler);
        compiler.specialize().unwrap();
        let concrete = compiler.specialized_program().unwrap();
        concrete
            .validate_origins(compiler.checked_program().unwrap())
            .unwrap();
        assert!(concrete.decls.find(Name::str(unused)).is_empty());
        assert!(concrete.decls.find(Name::str("identity")).is_empty());
        assert!(!compiler
            .checked_program()
            .unwrap()
            .decls
            .find(Name::str(unused))
            .is_empty());
        assert!(std::ptr::eq(compiler.decls(), &concrete.decls));

        let vm = compiler.compile_vm().unwrap();
        assert_eq!(VM::new().call(&vm, Name::str(root), &[]).unwrap(), expected);
        assert_eq!(
            crate::stack_vm::StackVM::new().run(&compiler.compile_stack().unwrap()),
            expected
        );
        assert_eq!(
            crate::stack_vm::StackVM::new().run(&compiler.compile_stack_unfused().unwrap()),
            expected
        );
        let compiled = compiler.compile_program().unwrap();
        assert!(compiled.get_entry_point(Name::str(root)).is_some());
        #[cfg(feature = "cranelift")]
        {
            let (entries, _, jit) = compiler.jit_multi().unwrap();
            assert!(entries.contains_key(&Name::str(root)));
            jit.free_memory();
        }
        assert_eq!(
            compiler.globals_info_with_offset(0),
            vec![("count".into(), 0, 4, "i32".into(), false)]
        );
        assert_eq!(compiler.checked_program().unwrap().decls, templates.decls);
        assert!(compiler.last_errors.is_empty());
    }

    // An explicit check starts a new validation, even for unchanged source.
    assert!(compiler.check());
    assert_no_executable(&compiler);
    compiler.specialize().unwrap();
}

#[test]
fn equivalent_default_roots_preserve_specialization() {
    let mut compiler = parsed("main() -> i32 { 42 }");
    assert!(compiler.check());
    compiler.specialize().unwrap();
    let output = compiler.specialized_program().unwrap() as *const SpecializedProgram;
    for roots in [&["main"][..], &[][..], &["main"][..]] {
        compiler.set_entry_points(roots);
        assert_eq!(compiler.specialized_program().unwrap() as *const _, output);
        compiler.specialize().unwrap();
        assert_eq!(compiler.specialized_program().unwrap() as *const _, output);
    }
}

#[test]
fn failed_size_specialization_can_retry_the_same_or_different_roots() {
    let mut compiler = parsed(
        "get<N>(arr: [i32; N], idx: i32) -> i32 { arr[idx] }
         bad() -> i32 { get([10, 20, 30, 40], 99) }
         good() -> i32 { 42 }",
    );
    compiler.set_entry_points(&["bad"]);
    assert!(compiler.check(), "{:?}", compiler.last_errors);
    let templates = compiler.checked_program().unwrap().clone();

    for _ in 0..2 {
        compiler.set_entry_points(&["bad"]);
        let error = compiler.specialize().unwrap_err();
        assert!(error.contains("safety check failed"), "{}", error);
        let diagnostics = compiler.last_errors.clone();
        let safety_count = compiler.last_safety_errors.len();
        assert!(safety_count > 0);
        assert_no_executable(&compiler);
        assert_eq!(compiler.specialize().unwrap_err(), error);
        assert_eq!(compiler.last_errors, diagnostics);
        assert_eq!(compiler.last_safety_errors.len(), safety_count);

        compiler.set_entry_points(&["good"]);
        assert!(compiler.last_errors.is_empty());
        assert!(compiler.last_safety_errors.is_empty());
        assert_no_executable(&compiler);
        compiler.specialize().unwrap();
        let program = compiler.compile_vm().unwrap();
        assert_eq!(
            VM::new().call(&program, Name::str("good"), &[]).unwrap(),
            42
        );
        assert_eq!(compiler.checked_program().unwrap().decls, templates.decls);
    }

    compiler.set_entry_points(&["bad"]);
    assert!(compiler.specialize().is_err());
    assert!(compiler.check());
    assert!(compiler.last_errors.is_empty());
    assert!(compiler.last_safety_errors.is_empty());
    assert_no_executable(&compiler);
}

#[test]
fn failed_concrete_call_safety_retains_analysis_and_can_retry_valid_roots() {
    let mut compiler = parsed(
        "bounded<T>(x: i32, value: T) require x >= 0 {}
         bad { bounded(-1, true) }
         good() -> i32 { bounded⟨bool⟩(0, true); 42 }",
    );
    assert!(compiler.analyze(), "{:?}", compiler.last_errors);
    let templates = compiler.checked_program().unwrap().clone();
    let snapshot = compiler.source_analysis().unwrap() as *const SourceAnalysis;

    for _ in 0..2 {
        compiler.set_entry_points(&["bad"]);
        let failure = compiler.specialize().unwrap_err();
        assert!(failure.contains("safety check failed"));
        assert_eq!(compiler.last_safety_errors.len(), 1);
        let diagnostics = compiler.last_errors.clone();
        assert_eq!(diagnostics.len(), 1);
        assert_no_executable(&compiler);
        assert_eq!(compiler.checked_program().unwrap().decls, templates.decls);
        assert_eq!(compiler.source_analysis().unwrap() as *const _, snapshot);

        compiler.last_errors.clear();
        compiler.last_safety_errors.clear();
        assert_no_executable(&compiler);
        assert_eq!(compiler.specialize().unwrap_err(), failure);
        assert_eq!(compiler.last_errors, diagnostics);
        assert_eq!(compiler.last_safety_errors.len(), 1);

        compiler.set_entry_points(&["good"]);
        assert!(compiler.last_errors.is_empty());
        assert!(compiler.last_safety_errors.is_empty());
        compiler.specialize().unwrap();
        let program = compiler.compile_vm().unwrap();
        assert_eq!(
            VM::new().call(&program, Name::str("good"), &[]).unwrap(),
            42
        );
        assert_eq!(compiler.checked_program().unwrap().decls, templates.decls);
        assert_eq!(compiler.source_analysis().unwrap() as *const _, snapshot);
    }
}

#[test]
fn non_safety_specialization_errors_belong_to_the_current_roots() {
    let mut compiler = parsed(
        "main(x: i32) -> i32 { x }
         main(x: f32) -> f32 { x }
         good() -> i32 { 42 }",
    );
    assert!(compiler.check(), "{:?}", compiler.last_errors);
    let error = compiler.specialize().unwrap_err();
    assert!(error.contains("Multiple overloads"), "{}", error);
    assert_eq!(compiler.last_errors, vec![error]);
    assert!(compiler.last_safety_errors.is_empty());
    let diagnostics = compiler.last_errors.clone();
    compiler.set_entry_points(&["main"]);
    assert_eq!(compiler.last_errors, diagnostics);
    assert_no_executable(&compiler);
    compiler.set_entry_points(&["good"]);
    assert!(compiler.last_errors.is_empty());
    compiler.specialize().unwrap();
    assert!(compiler.compile_vm().is_ok());
}

#[test]
fn parsing_invalidates_checked_and_specialized_results() {
    let mut compiler = parsed("main() -> i32 { 42 }");
    assert_no_executable(&compiler);
    for source in ["first() -> i32 { 1 }", "second() -> i32 { 2 }"] {
        assert!(compiler.check());
        assert!(compiler.checked_program().is_some());
        if source.starts_with("second") {
            compiler.specialize().unwrap();
            assert!(compiler.compile_vm().is_ok());
        }
        assert!(compiler.parse(source, "extra.lyte"));
        assert!(compiler.checked_program().is_none());
        assert!(compiler.specialize().is_err());
        assert_no_executable(&compiler);
    }
    assert!(compiler.check());
    compiler.specialize().unwrap();
    assert!(compiler.parse("other() -> i32 { missing }", "other.lyte"));
    assert!(compiler.checked_program().is_none());
    assert_no_executable(&compiler);
    assert!(!compiler.check());
    assert!(compiler.checked_program().is_none());
    assert!(compiler.specialize().is_err());
    assert_no_executable(&compiler);
}

#[test]
fn parse_errors_invalidate_output_even_when_declarations_are_retained() {
    let mut compiler = parsed("main() -> i32 { 42 }");
    assert!(compiler.check());
    compiler.specialize().unwrap();
    assert!(!compiler.parse("var sink: [f32]\nassume sink.len == 1", "user.lyte"));
    assert!(compiler
        .last_parse_errors
        .iter()
        .any(|error| error.message.contains("assume is only allowed")));
    assert!(compiler
        .parsed_declarations()
        .any(|decl| matches!(decl, Decl::Global { name, .. } if *name == Name::str("sink"))));
    assert!(compiler.checked_program().is_none());
    assert_no_executable(&compiler);
    assert!(!compiler.check());
    assert!(compiler.specialize().is_err());
    assert_no_executable(&compiler);
    assert!(compiler.parse("other() -> i32 { 1 }", "other.lyte"));
    assert!(!compiler.check());
    assert_no_executable(&compiler);
}

#[test]
fn malformed_expressions_stop_at_parse_errors_with_either_check_all_setting() {
    for check_all in [false, true] {
        let mut compiler = Compiler::new();
        compiler.quiet = true;
        compiler.check_all = check_all;
        assert!(!compiler.parse("main() -> i32 { 1wat }", "malformed.lyte"));
        let parse_errors = compiler.last_parse_errors.clone();
        let messages = compiler.last_errors.clone();
        assert!(!parse_errors.is_empty());

        for _ in 0..2 {
            // Clearing public diagnostics must not hide the invalid syntax.
            compiler.last_parse_errors.clear();
            compiler.last_errors.clear();
            assert!(!compiler.check());
            assert_eq!(compiler.last_parse_errors, parse_errors);
            assert_eq!(compiler.last_errors, messages);
            assert!(compiler.last_type_errors.is_empty());
            assert!(compiler.last_safety_errors.is_empty());
            assert!(compiler.checked_program().is_none());
            assert!(compiler.specialize().is_err());
            assert_no_executable(&compiler);
        }

        assert!(compiler.parse("helper() -> i32 { 42 }", "helper.lyte"));
        assert!(!compiler.check());
        assert_eq!(compiler.last_parse_errors, parse_errors);
        assert_eq!(compiler.last_errors, messages);
        assert_no_executable(&compiler);
    }
}

#[test]
fn parse_errors_from_every_input_survive_parsing_and_rechecking() {
    for check_all in [false, true] {
        for reverse in [false, true] {
            let mut compiler = Compiler::new();
            compiler.quiet = true;
            compiler.check_all = check_all;
            let mut inputs = [
                ("first_bad.lyte", "fn broken( {", false),
                ("good.lyte", "main() -> i32 { 42 }", true),
                ("second_bad.lyte", "fn unfinished( {", false),
            ];
            if reverse {
                inputs.reverse();
            }
            for (path, source, valid) in inputs {
                let previous_errors = compiler.last_parse_errors.clone();
                assert_eq!(compiler.parse(source, path), valid);
                assert!(compiler.last_parse_errors.starts_with(&previous_errors));
                assert_no_executable(&compiler);
            }
            let parse_errors = compiler.last_parse_errors.clone();
            for path in ["first_bad.lyte", "second_bad.lyte"] {
                assert!(parse_errors
                    .iter()
                    .any(|error| error.location.file == Name::str(path)));
            }
            for _ in 0..2 {
                assert!(!compiler.check());
                assert_eq!(compiler.last_parse_errors, parse_errors);
                let messages = compiler.last_errors.clone();
                assert!(messages
                    .iter()
                    .any(|error| error.contains("first_bad.lyte")));
                assert!(messages
                    .iter()
                    .any(|error| error.contains("second_bad.lyte")));
                compiler.set_entry_points(&["good"]);
                assert_eq!(compiler.last_errors, messages);
                assert!(compiler.checked_program().is_none());
                assert!(compiler.specialize().is_err());
                assert_no_executable(&compiler);
                // Diagnostics are public views; input trees own parse failure.
                compiler.last_errors.clear();
                compiler.last_parse_errors.clear();
            }
        }
    }
}

#[test]
fn source_failures_survive_root_changes_and_require_rechecking_after_parse() {
    for (source, safety_failure) in [
        ("bad() -> i32 { missing }", false),
        ("bad() -> i32 { let values = [1, 2]; values[9] }", true),
    ] {
        let mut compiler = parsed(source);
        assert!(compiler.parse("good() -> i32 { 42 }", "good.lyte"));
        assert!(!compiler.check());
        assert_eq!(compiler.checked_program().is_some(), safety_failure);
        assert_eq!(!compiler.last_safety_errors.is_empty(), safety_failure);
        assert_eq!(!compiler.last_type_errors.is_empty(), !safety_failure);
        let errors = compiler.last_errors.clone();
        compiler.set_entry_points(&["good"]);
        assert_eq!(compiler.last_errors, errors);
        assert!(compiler.specialize().is_err());
        compiler.last_errors.clear();
        compiler.last_type_errors.clear();
        compiler.last_safety_errors.clear();
        assert!(compiler.specialize().is_err());
        assert_no_executable(&compiler);

        assert!(compiler.parse("helper() -> i32 { 1 }", "helper.lyte"));
        assert!(compiler.checked_program().is_none());
        assert!(compiler.last_errors.is_empty());
        assert!(compiler.last_type_errors.is_empty());
        assert!(compiler.last_safety_errors.is_empty());
        assert_no_executable(&compiler);
        assert!(!compiler.check());
        assert_eq!(compiler.last_errors, errors);
        assert!(compiler.specialize().is_err());
    }
}

#[test]
fn parsing_clears_specialization_diagnostics_and_requires_checking() {
    let mut compiler = parsed(
        "get<N>(arr: [i32; N], idx: i32) -> i32 { arr[idx] }
         main() -> i32 { get([1, 2], 9) }",
    );
    assert!(compiler.check());
    assert!(compiler.specialize().is_err());
    assert!(!compiler.last_safety_errors.is_empty());
    assert!(compiler.parse("good() -> i32 { 42 }", "good.lyte"));
    assert!(compiler.last_errors.is_empty());
    assert!(compiler.last_safety_errors.is_empty());
    assert!(compiler.checked_program().is_none());
    compiler.set_entry_points(&["good"]);
    assert!(compiler.specialize().is_err());
    assert_no_executable(&compiler);
    assert!(compiler.check());
    compiler.specialize().unwrap();
}

#[test]
fn validation_option_changes_require_checking_before_specialization_or_codegen() {
    for original in [false, true] {
        for specialized in [false, true] {
            let mut compiler = parsed("main() -> i32 { 42 }");
            compiler.no_recursion = original;
            assert!(compiler.check());
            if specialized {
                compiler.specialize().unwrap();
            }
            let templates = compiler.checked_program().unwrap().clone();
            compiler.no_recursion = !original;
            assert!(compiler.specialize().unwrap_err().contains("call check()"));
            assert_no_executable(&compiler);
            assert_eq!(compiler.checked_program().unwrap().decls, templates.decls);

            // Reuse is keyed by option values, not by writes to public fields.
            compiler.no_recursion = original;
            assert_eq!(compiler.specialized_program().is_ok(), specialized);
            compiler.no_recursion = !original;
            assert!(compiler.check());
            assert_no_executable(&compiler);
            compiler.specialize().unwrap();
            assert!(compiler.compile_vm().is_ok());
        }
    }
}

#[test]
fn enabling_no_recursion_rejects_previously_validated_recursion() {
    let mut compiler = parsed(
        "recurse(x: i32) -> i32 { if x == 0 { 0 } else { recurse(x - 1) } }
         main() -> i32 { recurse(2) }",
    );
    assert!(compiler.check());
    compiler.specialize().unwrap();
    compiler.no_recursion = true;
    assert_no_executable(&compiler);
    assert!(!compiler.check());
    assert!(compiler
        .last_errors
        .iter()
        .any(|error| error.contains("--no-recursion")));
    assert!(compiler.checked_program().is_some());
    assert!(compiler.specialize().is_err());
    assert_no_executable(&compiler);
    compiler.no_recursion = false;
    assert!(compiler.specialize().unwrap_err().contains("call check()"));
    assert!(compiler.check());
    assert!(compiler.last_safety_errors.is_empty());
    compiler.specialize().unwrap();
}

#[test]
fn ffi_malformed_expressions_report_parse_errors_without_an_ice() {
    use crate::ffi::*;
    use std::ffi::{CStr, CString};
    for check_first in [false, true] {
        unsafe {
            let compiler = lyte_compiler_new(std::ptr::null(), 0);
            let source = CString::new("main() -> i32 { 1wat }").unwrap();
            let path = CString::new("malformed.lyte").unwrap();
            assert!(!lyte_compiler_add_source(
                compiler,
                source.as_ptr(),
                path.as_ptr()
            ));
            let parse_error = CStr::from_ptr(lyte_compiler_get_error(compiler))
                .to_str()
                .unwrap()
                .to_string();
            assert!(parse_error.contains("malformed.lyte"), "{}", parse_error);

            if check_first {
                assert!(!lyte_compiler_check(compiler));
                assert!(!lyte_compiler_had_ice(compiler));
            }
            assert!(lyte_compiler_compile(compiler).is_null());
            assert!(!lyte_compiler_had_ice(compiler));
            assert_eq!(
                CStr::from_ptr(lyte_compiler_get_error(compiler))
                    .to_str()
                    .unwrap(),
                parse_error
            );
            assert!(!lyte_compiler_check(compiler));
            assert!(!lyte_compiler_had_ice(compiler));
            assert_eq!(
                CStr::from_ptr(lyte_compiler_get_error(compiler))
                    .to_str()
                    .unwrap(),
                parse_error
            );
            lyte_compiler_free(compiler);
        }
    }
}

#[test]
fn ffi_compile_rejects_parse_errors_in_an_earlier_input() {
    use crate::ffi::*;
    use std::ffi::{CStr, CString};
    unsafe {
        let compiler = lyte_compiler_new(std::ptr::null(), 0);
        let bad = CString::new("fn broken( {").unwrap();
        let good = CString::new("main() -> i32 { 42 }").unwrap();
        let bad_path = CString::new("bad.lyte").unwrap();
        let good_path = CString::new("good.lyte").unwrap();
        assert!(!lyte_compiler_add_source(
            compiler,
            bad.as_ptr(),
            bad_path.as_ptr()
        ));
        assert!(lyte_compiler_add_source(
            compiler,
            good.as_ptr(),
            good_path.as_ptr()
        ));
        assert!(!lyte_compiler_check(compiler));
        assert!(lyte_compiler_compile(compiler).is_null());
        assert!(!lyte_compiler_had_ice(compiler));
        let error = CStr::from_ptr(lyte_compiler_get_error(compiler))
            .to_str()
            .unwrap();
        assert!(error.contains("bad.lyte"), "{}", error);
        lyte_compiler_free(compiler);
    }
}
