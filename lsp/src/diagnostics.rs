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
        diags
            .entry(uri)
            .or_default()
            .push(make_diagnostic(&err.location, &err.message, DiagnosticSeverity::ERROR));
    }

    for err in &compiler.last_type_errors {
        let uri = analysis::path_to_uri(&err.location.file);
        diags
            .entry(uri)
            .or_default()
            .push(make_diagnostic(&err.location, &err.message, DiagnosticSeverity::ERROR));
    }

    for err in &compiler.last_safety_errors {
        let uri = analysis::path_to_uri(&err.location.file);
        diags
            .entry(uri)
            .or_default()
            .push(make_diagnostic(
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
