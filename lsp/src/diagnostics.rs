use crate::analysis::{self, AnalysisState};
use lsp_types::*;
use lyte::Loc;
use std::collections::HashMap;

pub fn collect_diagnostics(state: &AnalysisState) -> HashMap<Uri, Vec<Diagnostic>> {
    let mut diags: HashMap<Uri, Vec<Diagnostic>> = HashMap::new();

    let compiler = match state.compiler() {
        Some(c) => c,
        None => return diags,
    };

    for err in &compiler.last_parse_errors {
        let uri = analysis::path_to_uri(&err.location.file);
        diags.entry(uri).or_default().push(make_diagnostic(
            &err.location,
            &err.message,
            DiagnosticSeverity::ERROR,
        ));
    }

    for err in &compiler.last_type_errors {
        let uri = analysis::path_to_uri(&err.location.file);
        diags.entry(uri).or_default().push(make_diagnostic(
            &err.location,
            &err.message,
            DiagnosticSeverity::ERROR,
        ));
    }

    for err in &compiler.last_safety_errors {
        let uri = analysis::path_to_uri(&err.location.file);
        diags.entry(uri).or_default().push(make_diagnostic(
            &err.location,
            &err.message,
            DiagnosticSeverity::WARNING,
        ));
    }

    diags
}

fn make_diagnostic(loc: &Loc, message: &str, severity: DiagnosticSeverity) -> Diagnostic {
    // Loc is 1-indexed; LSP Position is 0-indexed.
    let line = loc.line.saturating_sub(1);
    let col = loc.col.saturating_sub(1);
    let pos = Position::new(line, col);

    Diagnostic {
        range: Range::new(pos, pos),
        severity: Some(severity),
        source: Some("lyte".to_string()),
        message: message.to_string(),
        ..Default::default()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_uri(path: &str) -> Uri {
        format!("file://{}", path).parse().unwrap()
    }

    #[test]
    fn no_diagnostics_for_valid_code() {
        let mut state = AnalysisState::new();
        let uri = test_uri("/test.lyte");
        state.update_document(uri.clone(), "fn foo() -> f32 { 1.0 }".to_string());
        let diags = collect_diagnostics(&state);
        let file_diags = diags.get(&uri).cloned().unwrap_or_default();
        assert!(
            file_diags.is_empty(),
            "expected no diagnostics, got: {:?}",
            file_diags
        );
    }

    #[test]
    fn parse_error_produces_diagnostic() {
        let mut state = AnalysisState::new();
        let uri = test_uri("/bad.lyte");
        state.update_document(uri.clone(), "fn foo( {".to_string());
        let diags = collect_diagnostics(&state);
        let file_diags = diags.get(&uri).cloned().unwrap_or_default();
        assert!(!file_diags.is_empty(), "expected parse error diagnostic");
        assert_eq!(file_diags[0].severity, Some(DiagnosticSeverity::ERROR));
        assert_eq!(file_diags[0].source, Some("lyte".to_string()));
    }

    #[test]
    fn type_error_produces_diagnostic() {
        let mut state = AnalysisState::new();
        let uri = test_uri("/type_err.lyte");
        // Assign a string to a float - should cause a type error.
        state.update_document(uri.clone(), "fn foo() -> Float { \"hello\" }".to_string());
        let diags = collect_diagnostics(&state);
        let file_diags = diags.get(&uri).cloned().unwrap_or_default();
        assert!(!file_diags.is_empty(), "expected type error diagnostic");
        assert_eq!(file_diags[0].severity, Some(DiagnosticSeverity::ERROR));
    }

    #[test]
    fn diagnostics_empty_after_document_removed() {
        let mut state = AnalysisState::new();
        let uri = test_uri("/bad2.lyte");
        state.update_document(uri.clone(), "fn foo( {".to_string());
        assert!(!collect_diagnostics(&state)
            .get(&uri)
            .cloned()
            .unwrap_or_default()
            .is_empty());
        state.remove_document(&uri);
        let diags = collect_diagnostics(&state);
        // No documents, no diagnostics.
        assert!(diags.is_empty());
    }

    #[test]
    fn fixing_error_clears_diagnostics() {
        let mut state = AnalysisState::new();
        let uri = test_uri("/fixme.lyte");
        state.update_document(uri.clone(), "fn foo( {".to_string());
        assert!(!collect_diagnostics(&state)
            .get(&uri)
            .cloned()
            .unwrap_or_default()
            .is_empty());
        // Fix the code.
        state.update_document(uri.clone(), "fn foo() -> f32 { 1.0 }".to_string());
        let diags = collect_diagnostics(&state);
        let file_diags = diags.get(&uri).cloned().unwrap_or_default();
        assert!(
            file_diags.is_empty(),
            "expected diagnostics to clear after fix, got: {:?}",
            file_diags
        );
    }
}
