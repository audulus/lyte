//! Exercise the real notification, request dispatch and diagnostic publication
//! paths. Every edit rebuilds the compiler, just as it does in a live session.
use super::*;
use serde_json::{json, Value};
use std::collections::HashMap;

struct Editor {
    state: analysis::AnalysisState,
    server: Connection,
    client: Connection,
    documents: HashMap<String, String>,
    diagnostics: HashMap<String, Vec<Diagnostic>>,
}

impl Editor {
    fn new() -> Self {
        let (server, client) = Connection::memory();
        Self {
            state: analysis::AnalysisState::new(),
            server,
            client,
            documents: HashMap::new(),
            diagnostics: HashMap::new(),
        }
    }

    fn edit(&mut self, file: &str, text: &str) {
        let uri = format!("file:///{}", file);
        let (method, params) = if self.documents.insert(file.into(), text.into()).is_some() {
            (
                "textDocument/didChange",
                json!({"textDocument": {"uri": uri, "version": 2}, "contentChanges": [{"text": text}]}),
            )
        } else {
            (
                "textDocument/didOpen",
                json!({"textDocument": {"uri": uri, "version": 1, "languageId": "lyte", "text": text}}),
            )
        };
        // Clear the observed messages to require a fresh publication for every
        // open file, including an empty list when old errors have disappeared.
        self.diagnostics.clear();
        handle_notification(
            &self.server,
            &mut self.state,
            Notification::new(method.into(), params),
        );
        while let Ok(message) = self.client.receiver.try_recv() {
            let Message::Notification(notification) = message else {
                panic!("expected diagnostics")
            };
            assert_eq!(notification.method, "textDocument/publishDiagnostics");
            let params: PublishDiagnosticsParams =
                serde_json::from_value(notification.params).unwrap();
            self.diagnostics
                .insert(params.uri.as_str().into(), params.diagnostics);
        }
        assert_eq!(self.diagnostics.len(), self.documents.len());
    }

    fn request(&self, file: &str, needle: &str, method: &str) -> Value {
        let source = &self.documents[file];
        let offset = source.find(needle).expect("query text in document");
        let prefix = &source[..offset];
        let line = prefix.bytes().filter(|&b| b == b'\n').count();
        let character = prefix.rsplit('\n').next().unwrap().encode_utf16().count();
        let request = Request::new(
            1.into(),
            method.into(),
            json!({
                "textDocument": {"uri": format!("file:///{}", file)},
                "position": {"line": line, "character": character}
            }),
        );
        handle_request(&self.server, &self.state, request);
        let Message::Response(response) = self.client.receiver.try_recv().unwrap() else {
            panic!("expected response")
        };
        assert!(response.error.is_none(), "{:?}", response.error);
        response.result.unwrap()
    }

    fn hover(&self, file: &str, needle: &str) -> Option<String> {
        let result = self.request(file, needle, "textDocument/hover");
        if result.is_null() {
            return None;
        }
        Some(result["contents"]["value"].as_str().unwrap().into())
    }

    fn definition(&self, file: &str, needle: &str) -> Option<Location> {
        serde_json::from_value(self.request(file, needle, "textDocument/definition")).unwrap()
    }

    fn assert_hover(&self, file: &str, needle: &str, text: &str) {
        assert_eq!(
            self.hover(file, needle).as_deref(),
            Some(format!("```lyte\n{}\n```", text).as_str())
        );
    }

    fn assert_definition(&self, file: &str, needle: &str, target_file: &str, line: u32) {
        let location = self
            .definition(file, needle)
            .expect("established reference");
        assert_eq!(location.uri.as_str(), format!("file:///{}", target_file));
        assert_eq!(location.range.start.line, line);
    }

    fn diagnostics(&self, file: &str) -> &[Diagnostic] {
        &self.diagnostics[&format!("file:///{}", file)]
    }

    fn assert_blocked(&self) {
        let compiler = self.state.compiler().unwrap();
        assert!(compiler.specialized_program().is_err());
        assert!(compiler.compile_vm().is_err());
        assert!(compiler.compile_stack().is_err());
    }
}

#[test]
fn valid_function_beside_type_invalid_function_in_same_or_other_file() {
    for separate in [false, true] {
        let mut editor = Editor::new();
        let good = "target(x: i32) -> i32 { x }\ngood() -> i32 {\n  let x = 1\n  target(x)\n}";
        let bad = "bad() -> i32 { true }";
        editor.edit(
            "good.lyte",
            &if separate {
                good.into()
            } else {
                format!("{}\n{}", good, bad)
            },
        );
        if separate {
            editor.edit("bad.lyte", bad);
        }
        assert!(editor.state.compiler().unwrap().checked_program().is_none());
        editor.assert_hover("good.lyte", "x)", "x: i32");
        editor.assert_definition("good.lyte", "target(x)", "good.lyte", 0);
        assert!(!editor
            .diagnostics(if separate { "bad.lyte" } else { "good.lyte" })
            .is_empty());
        editor.assert_blocked();
    }
}

#[test]
fn generic_types_and_local_references_survive_invalid_neighbors() {
    let mut editor = Editor::new();
    editor.edit(
        "generic.lyte",
        "identity<T>(x: T) -> T {\n  let value = x\n  value\n}",
    );
    editor.edit("bad.lyte", "bad() -> i32 { missing }");
    editor.assert_hover("generic.lyte", "x\n", "x: T");
    editor.assert_hover("generic.lyte", "value\n", "value: T");
    editor.assert_definition("generic.lyte", "value\n", "generic.lyte", 1);
    editor.assert_definition("generic.lyte", "x\n", "generic.lyte", 0);
}

#[test]
fn failing_body_keeps_identity_separate_from_failed_inference() {
    let mut editor = Editor::new();
    editor.edit("failing.lyte", "target(x: i32) -> i32 { x }\nbroken<T>(p: T) -> i32 {\n  var number: i32\n  let inferred = target(true)\n  number\n  p\n  inferred\n}");
    editor.assert_hover("failing.lyte", "number\n", "number: i32");
    editor.assert_hover("failing.lyte", "p\n", "p: T");
    // The solver can leave concrete types for these despite rejecting the call.
    assert!(editor.hover("failing.lyte", "target(true)").is_none());
    assert!(editor.hover("failing.lyte", "inferred\n").is_none());
    editor.assert_definition("failing.lyte", "target(true)", "failing.lyte", 0);
    editor.assert_definition("failing.lyte", "inferred\n", "failing.lyte", 3);
    editor.assert_definition("failing.lyte", "number\n", "failing.lyte", 2);
    editor.assert_hover("failing.lyte", "true", "bool");
    editor.assert_blocked();
}

#[test]
fn unavailable_types_and_unresolved_names_do_not_invent_answers() {
    let mut editor = Editor::new();
    editor.edit("unknown.lyte", "target() -> i32 { 1 }\nbroken(p: Unknown) {\n  let cast = true as i32\n  let target = missing\n  p\n  cast\n  target\n  42\n  999999999999i32\n}");
    for needle in [
        "p\n",
        "cast\n",
        "target\n",
        "missing",
        "42\n",
        "999999999999i32",
    ] {
        assert!(editor.hover("unknown.lyte", needle).is_none(), "{}", needle);
    }
    assert!(editor.definition("unknown.lyte", "missing").is_none());
    // The local shadows the top-level function, even with no established type.
    editor.assert_definition("unknown.lyte", "target\n", "unknown.lyte", 3);
    editor.assert_definition("unknown.lyte", "p\n", "unknown.lyte", 1);
}

#[test]
fn unresolved_explicit_calls_withhold_contextual_types_but_keep_bindings() {
    for call in [
        "missing⟨i32⟩(1)",
        "identity⟨i32, bool⟩(1)",
        "unused⟨i32⟩(1)",
    ] {
        let mut editor = Editor::new();
        editor.edit(
            "explicit.lyte",
            &format!(
                "bad() -> i32 {{ unknown }}\nprobe() -> i32 {{\n  let value = {}\n  true\n  value\n}}\nidentity<T>(x: T) -> T {{ x }}\nunused<T, U>(x: T) -> T {{ x }}",
                call
            ),
        );
        // The surrounding return constraint can solve `value` to i32 even
        // though the explicit application has no candidate. Neither the use
        // nor the binding may publish that inferred type.
        for needle in [call, "let value", "value\n"] {
            assert!(
                editor.hover("explicit.lyte", needle).is_none(),
                "{}: {}",
                call,
                needle
            );
        }
        assert!(
            editor.definition("explicit.lyte", call).is_none(),
            "{}",
            call
        );
        editor.assert_definition("explicit.lyte", "value\n", "explicit.lyte", 2);
        editor.assert_hover("explicit.lyte", "true\n", "bool");
        editor.assert_blocked();
    }
}

#[test]
fn builtin_arithmetic_keeps_types_without_named_overload_candidates() {
    let mut editor = Editor::new();
    editor.edit(
        "arithmetic.lyte",
        "probe() -> i32 {\n  let value = 1 + 2\n  value\n}\nbad() -> i32 { unknown }",
    );
    editor.assert_hover("arithmetic.lyte", "value\n", "value: i32");
    editor.assert_definition("arithmetic.lyte", "value\n", "arithmetic.lyte", 1);
    editor.assert_blocked();
}

#[test]
fn parse_recovery_preserves_other_files_without_publishing_placeholders() {
    for malformed in [
        "bad() -> i32 { 1wat }",
        "bad() -> [i32; 1] { [1wat] }",
        "bad() -> i32 { true + ) }",
        "bad() -> i32 { abs(1wat) }",
        "bad() { let x = 1; x. }",
        "fn broken( {",
    ] {
        let mut editor = Editor::new();
        editor.edit("good.lyte", "good() -> i32 {\n  let x = 1\n  x\n}");
        editor.edit("parse.lyte", malformed);
        assert!(
            !editor
                .state
                .compiler()
                .unwrap()
                .last_parse_errors
                .is_empty(),
            "{}",
            malformed
        );
        assert!(editor.state.compiler().unwrap().checked_program().is_none());
        editor.assert_hover("good.lyte", "x\n", "x: i32");
        editor.assert_definition("good.lyte", "x\n", "good.lyte", 1);
        assert!(!editor.diagnostics("parse.lyte").is_empty());
        editor.assert_blocked();
    }
}

#[test]
fn recovered_declaration_cannot_supply_a_confident_type_to_clean_caller() {
    let mut editor = Editor::new();
    // Recovery substitutes void for the malformed return annotation. Even a
    // fully solved clean caller must not expose that placeholder as a fact.
    editor.edit("damaged.lyte", "recovered() -> ?\n");
    editor.edit(
        "caller.lyte",
        "caller() {\n  recovered()\n}\nunrelated() -> i32 {\n  let value = 42\n  value\n}",
    );
    assert!(editor.state.compiler().unwrap().last_type_errors.is_empty());
    assert!(editor.hover("caller.lyte", "recovered()").is_none());
    editor.assert_definition("caller.lyte", "recovered()", "damaged.lyte", 0);
    editor.assert_hover("caller.lyte", "value\n", "value: i32");
    editor.assert_blocked();
}

#[test]
fn safety_diagnostics_preserve_types_and_references_but_block_execution() {
    let mut editor = Editor::new();
    editor.edit("safety.lyte", "broken() -> i32 {\n  let x = 1\n  x / 0\n}");
    assert!(editor.state.compiler().unwrap().checked_program().is_some());
    editor.assert_hover("safety.lyte", "x /", "x: i32");
    editor.assert_definition("safety.lyte", "x /", "safety.lyte", 1);
    assert!(editor
        .diagnostics("safety.lyte")
        .iter()
        .any(|d| d.severity == Some(DiagnosticSeverity::WARNING)));
    editor.assert_blocked();
}

#[test]
fn edits_replace_facts_and_publish_empty_diagnostics_after_repair() {
    let mut editor = Editor::new();
    editor.edit("edit.lyte", "main() -> i32 {\n  let old = 1\n  old\n}");
    editor.assert_hover("edit.lyte", "old\n", "old: i32");
    assert!(editor.diagnostics("edit.lyte").is_empty());
    editor.edit(
        "edit.lyte",
        "main() -> i32 {\n  let new = missing\n  old\n}",
    );
    assert!(editor.hover("edit.lyte", "old\n").is_none());
    assert!(editor.definition("edit.lyte", "old\n").is_none());
    assert!(!editor.diagnostics("edit.lyte").is_empty());
    editor.edit("edit.lyte", "main() -> bool {\n  let new = true\n  new\n}");
    editor.assert_hover("edit.lyte", "new\n", "new: bool");
    editor.assert_definition("edit.lyte", "new\n", "edit.lyte", 1);
    assert!(editor.diagnostics("edit.lyte").is_empty());
    assert!(editor.state.compiler().unwrap().checked_program().is_some());
}

#[test]
fn field_navigation_in_unaffected_body_survives_errors() {
    let mut editor = Editor::new();
    editor.edit(
        "field.lyte",
        "struct S {\n  value: i32\n}\nget(s: S) -> i32 {\n  s.value\n}\nbad() -> i32 { missing }",
    );
    editor.assert_hover("field.lyte", ".value", ".value: i32");
    editor.assert_definition("field.lyte", ".value", "field.lyte", 1);
}

#[test]
fn incomplete_parameter_and_generic_field_queries_do_not_panic() {
    for source in [
        "bad(x) -> i32 { x }\ncaller() -> i32 { bad(1) }",
        "struct Box<T> { value: T }\ncaller(x: Box) -> i32 { x.value }",
        "bad<T>(x) -> i32 { 1 }\ncaller() -> i32 { bad⟨i32⟩(1) }",
        "__add(x) -> i32 { 1 }\ncaller() -> i32 { 1 + 2 }",
        "interface I<T> { apply(x) -> T }\ncaller<T>(x: T) -> T where I<T> { apply(x) }",
        "interface I<T> { apply(x: T) -> T }\napply(x) -> i32 { 1 }\ncaller<T>(x: T) -> T where I<T> { apply(x) }\nmain() -> i32 { caller(1) }",
        "macro incomplete(x: i32)\ncaller() -> i32 { @incomplete(1) }",
        "macro incomplete(x: i32\ncaller() -> i32 { @incomplete(1) }",
    ] {
        let mut editor = Editor::new();
        editor.edit("incomplete.lyte", source);
        assert!(!editor.diagnostics("incomplete.lyte").is_empty());
        editor.assert_blocked();
    }
}

#[test]
fn recovered_body_keeps_bindings_and_never_hovers_error_nodes() {
    let mut editor = Editor::new();
    editor.edit(
        "recover.lyte",
        "broken() -> i32 {\n  let value = 1wat\n  value\n}",
    );
    assert!(editor.hover("recover.lyte", "1wat").is_none());
    assert!(editor.hover("recover.lyte", "value\n").is_none());
    editor.assert_definition("recover.lyte", "value\n", "recover.lyte", 1);
}

#[test]
fn recovered_struct_cannot_supply_field_types_to_clean_body() {
    let mut editor = Editor::new();
    editor.edit("damaged.lyte", "struct Damaged { field: ? }");
    editor.edit("caller.lyte", "caller(x: Damaged) {\n  x.field\n}");
    assert!(editor.hover("caller.lyte", "x.field").is_none());
    assert!(editor.hover("caller.lyte", ".field").is_none());
    assert!(editor.definition("caller.lyte", ".field").is_none());
    editor.assert_definition("caller.lyte", "x.field", "caller.lyte", 0);
}

#[test]
fn generic_interface_facts_keep_their_requirement_and_declaration_owners() {
    let mut editor = Editor::new();
    editor.edit("interface.lyte", "interface I<T> { apply(x: T) -> T }\ngood<T>(x: T) -> T where I<T> {\n  let result = apply(x)\n  result\n}\nbad<T>(x: T) -> T where Missing<T> I<T> {\n  apply(x)\n}");
    editor.assert_hover("interface.lyte", "result\n", "result: T");
    editor.assert_definition("interface.lyte", "apply(x)\n", "interface.lyte", 0);
    // Inspect ownership as well as sending requests: the failed first where
    // clause leaves a gap before the recorded requirement used by `apply`.
    let analysis = editor.state.compiler().unwrap().source_analysis().unwrap();
    let decls = analysis.declarations();
    for record in decls.records() {
        let Some(body) = analysis.body(record.definition) else {
            continue;
        };
        let Some(function) = decls.function(record.definition) else {
            continue;
        };
        for id in 0..function.arena.exprs.len() {
            if let Some(lyte::Reference::InterfaceMember {
                requirement,
                member,
            }) = &body.expression(id).unwrap().reference
            {
                let owner = body
                    .requirement_interface(*requirement)
                    .expect("requirement owner");
                assert!(decls.interface_members(owner).contains(member));
                assert!(decls.function(*member).is_some());
                assert!(decls.definition(owner).is_some());
            }
        }
    }
}

#[test]
fn successful_literal_types_and_invalid_annotations_are_distinguished() {
    let mut editor = Editor::new();
    editor.edit("annotations.lyte", "struct Box<T> { value: T }\nbroken(a: (i32, Unknown), b: Box, c: Box<i32>) {\n  a\n  b\n  c\n  1.0f64\n  7i32\n  missing\n}");
    for needle in ["a\n", "b\n"] {
        assert!(editor.hover("annotations.lyte", needle).is_none());
        editor.assert_definition("annotations.lyte", needle, "annotations.lyte", 1);
    }
    editor.assert_hover("annotations.lyte", "c\n", "c: Box<i32>");
    editor.assert_hover("annotations.lyte", "1.0f64", "f64");
    editor.assert_hover("annotations.lyte", "7i32", "i32");
}

#[test]
fn invalid_void_binding_keeps_identity_without_claiming_a_value_type() {
    let mut editor = Editor::new();
    editor.edit("void.lyte", "bad() {\n  var ghost: void\n  ghost\n}");
    assert!(editor.hover("void.lyte", "var ghost").is_none());
    assert!(editor.hover("void.lyte", "ghost\n").is_none());
    editor.assert_definition("void.lyte", "ghost\n", "void.lyte", 1);
}

#[test]
fn calls_with_deferred_size_parameters_preserve_successful_body_types() {
    let mut editor = Editor::new();
    editor.edit("sizes.lyte", "count<N>(values: [i32; N]) -> i32 { N }\ngood() -> i32 {\n  let values = [1, 2]\n  let total = count(values)\n  total\n}\nbad() -> i32 { missing }");
    editor.assert_hover("sizes.lyte", "total\n", "total: i32");
    editor.assert_definition("sizes.lyte", "count(values)", "sizes.lyte", 0);
}
