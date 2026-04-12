use crate::analysis::{self, AnalysisState};
use crate::hover::find_expr_at;
use lsp_types::*;
use lyte::{Decl, Expr, Loc, Name, Type};

pub fn handle_goto_definition(
    state: &AnalysisState,
    params: &GotoDefinitionParams,
) -> Option<GotoDefinitionResponse> {
    let compiler = state.compiler()?;
    let uri = &params.text_document_position_params.text_document.uri;
    let pos = params.text_document_position_params.position;
    let file = analysis::uri_to_path(uri);
    let file_name = Name::str(&file);

    let line = pos.line + 1;
    let col = pos.character + 1;

    let decls = compiler.decls();

    for decl in &decls.decls {
        if let Decl::Func(func) = decl {
            if func.loc.file != file_name {
                continue;
            }
            if let Some((id, _)) = find_expr_at(func, file_name, line, col) {
                match &func.arena.exprs[id] {
                    Expr::Id(name) => {
                        if let Some(loc) = find_decl_loc(decls, *name) {
                            return Some(loc_to_response(&loc));
                        }
                    }
                    Expr::Call(callee, _) => {
                        if let Expr::Id(name) = &func.arena.exprs[*callee] {
                            if let Some(loc) = find_decl_loc(decls, *name) {
                                return Some(loc_to_response(&loc));
                            }
                        }
                    }
                    Expr::Field(base_id, field_name) => {
                        // Try to resolve the base type and find the field declaration.
                        if let Some(&base_ty) = func.types.get(*base_id) {
                            if let Type::Name(struct_name, _) = &*base_ty {
                                let found = decls.find(*struct_name);
                                for d in found {
                                    if let Some(field) = d.find_field(field_name) {
                                        return Some(loc_to_response(&field.loc));
                                    }
                                }
                            }
                        }
                    }
                    _ => {}
                }
            }
        }
    }

    None
}

/// Find the source location of the first declaration with the given name.
/// Skips stdlib declarations (those in files starting with '<').
fn find_decl_loc(decls: &lyte::DeclTable, name: Name) -> Option<Loc> {
    let found = decls.find(name);
    for decl in found {
        match decl {
            Decl::Func(f) => {
                // Skip stdlib functions.
                if f.loc.file.starts_with('<') {
                    continue;
                }
                return Some(f.loc);
            }
            _ => {
                // Other decl types don't have loc yet; skip for now.
            }
        }
    }
    None
}

fn loc_to_response(loc: &Loc) -> GotoDefinitionResponse {
    let uri = analysis::path_to_uri(&loc.file);
    let line = loc.line.saturating_sub(1);
    let col = loc.col.saturating_sub(1);
    let pos = Position::new(line, col);
    GotoDefinitionResponse::Scalar(Location::new(uri, Range::new(pos, pos)))
}

#[cfg(test)]
mod tests {
    use super::*;
    use lsp_types::Uri;

    fn test_uri(path: &str) -> Uri {
        format!("file://{}", path).parse().unwrap()
    }

    fn make_goto_params(uri: &Uri, line: u32, character: u32) -> GotoDefinitionParams {
        GotoDefinitionParams {
            text_document_position_params: TextDocumentPositionParams {
                text_document: TextDocumentIdentifier { uri: uri.clone() },
                position: Position { line, character },
            },
            work_done_progress_params: Default::default(),
            partial_result_params: Default::default(),
        }
    }

    #[test]
    fn goto_definition_finds_function() {
        let mut state = AnalysisState::new();
        let uri = test_uri("/goto.lyte");
        let src = "fn bar() -> Float { 1.0 }\nfn foo() -> Float { bar() }";
        state.update_document(uri.clone(), src.to_string());
        // Cursor on `bar` in the call `bar()` — line 1, col 20 (0-indexed).
        let params = make_goto_params(&uri, 1, 20);
        let result = handle_goto_definition(&state, &params);
        assert!(
            result.is_some(),
            "expected goto definition result for function call"
        );
        if let Some(GotoDefinitionResponse::Scalar(loc)) = result {
            // Should point to `bar` definition at line 0.
            assert_eq!(loc.range.start.line, 0);
        }
    }

    #[test]
    fn goto_definition_no_compiler_returns_none() {
        let state = AnalysisState::new();
        let uri = test_uri("/nofile.lyte");
        let params = make_goto_params(&uri, 0, 0);
        assert!(handle_goto_definition(&state, &params).is_none());
    }

    #[test]
    fn goto_definition_unknown_position_returns_none() {
        let mut state = AnalysisState::new();
        let uri = test_uri("/goto2.lyte");
        state.update_document(uri.clone(), "fn foo() -> Float { 1.0 }".to_string());
        let params = make_goto_params(&uri, 50, 0);
        assert!(handle_goto_definition(&state, &params).is_none());
    }
}
