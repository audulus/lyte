use crate::analysis::{self, AnalysisState};
use crate::hover::find_expr_at;
use lsp_types::*;
use lyte::{BodyAnalysis, Decl, DeclTable, Expr, Loc, Name, Reference, Type, TypeID};

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

    let analysis = compiler.source_analysis()?;
    let decls = analysis.declarations();

    for (index, decl) in decls.decls.iter().enumerate() {
        let Decl::Func(func) = decl else { continue };
        if func.loc.file != file_name {
            continue;
        }
        let Some(body) = analysis.body(decls.id_at(index)) else {
            continue;
        };
        let Some((id, _)) = find_expr_at(func, file_name, line, col) else {
            continue;
        };
        let target = match &func.arena[id] {
            Expr::Call(callee, _) => *callee,
            Expr::Field(base, field_name) => {
                if let Some(Type::Name(struct_name, _)) =
                    body.expression(*base).and_then(|facts| facts.ty).as_deref()
                {
                    for decl in decls.find(*struct_name) {
                        if let Some(field) = decl.find_field(field_name) {
                            return Some(loc_to_response(&field.loc));
                        }
                    }
                }
                continue;
            }
            _ => id,
        };
        let Some(facts) = body.expression(target) else {
            continue;
        };
        if let Some(reference) = &facts.reference {
            if let Some(loc) = find_decl_loc(decls, body, reference, facts.ty) {
                return Some(loc_to_response(&loc));
            }
        }
    }
    None
}

/// Use recorded identities even when the expression has no established type.
/// Exact-type/first-candidate navigation is a presentation heuristic, not call
/// selection. Never fall back to spelling for an unresolved or shadowed use.
fn find_decl_loc(
    decls: &DeclTable,
    body: &BodyAnalysis,
    reference: &Reference,
    ty: Option<TypeID>,
) -> Option<Loc> {
    match reference {
        Reference::Local(local) | Reference::SizeParameter(local) => {
            body.local(*local).map(|local| local.loc)
        }
        Reference::Functions(candidates) => {
            let functions: Vec<_> = candidates
                .iter()
                .filter_map(|id| decls.function(*id))
                .filter(|f| !f.loc.file.starts_with('<'))
                .collect();
            functions
                .iter()
                .find(|f| ty.is_some() && f.annotated_ty() == ty)
                .or_else(|| functions.first())
                .map(|f| f.loc)
        }
        Reference::InterfaceMember { member, .. } => decls.function(*member).map(|f| f.loc),
        _ => None,
    }
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
