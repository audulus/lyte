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
