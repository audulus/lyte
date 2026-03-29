use crate::analysis::{self, AnalysisState};
use lsp_types::*;
use lyte::{Decl, Expr, ExprID, FuncDecl, Name};

pub fn handle_hover(
    state: &AnalysisState,
    params: &HoverParams,
) -> Option<Hover> {
    let compiler = state.compiler()?;
    let uri = &params.text_document_position_params.text_document.uri;
    let pos = params.text_document_position_params.position;
    let file = analysis::uri_to_path(uri);
    let file_name = Name::str(&file);

    // LSP position is 0-indexed; Loc is 1-indexed.
    let line = pos.line + 1;
    let col = pos.character + 1;

    let decls = compiler.decls();

    for decl in &decls.decls {
        if let Decl::Func(func) = decl {
            if func.loc.file != file_name {
                continue;
            }
            if let Some(info) = hover_in_func(func, file_name, line, col) {
                return Some(Hover {
                    contents: HoverContents::Markup(MarkupContent {
                        kind: MarkupKind::Markdown,
                        value: info,
                    }),
                    range: None,
                });
            }
        }
    }

    None
}

fn hover_in_func(func: &FuncDecl, file: Name, line: u32, col: u32) -> Option<String> {
    let (id, _) = find_expr_at(func, file, line, col)?;

    if let Some(&ty) = func.types.get(id) {
        let type_str = ty.pretty_print();
        let label = match &func.arena.exprs[id] {
            Expr::Id(name) => format!("{}", name),
            Expr::Field(_, name) => format!(".{}", name),
            _ => String::new(),
        };

        if label.is_empty() {
            Some(format!("```lyte\n{}\n```", type_str))
        } else {
            Some(format!("```lyte\n{}: {}\n```", label, type_str))
        }
    } else {
        None
    }
}

/// Find the expression in a function whose location best matches the cursor.
/// Returns (ExprID, Loc) of the best match.
pub fn find_expr_at(
    func: &FuncDecl,
    file: Name,
    line: u32,
    col: u32,
) -> Option<(ExprID, lyte::Loc)> {
    let mut best_id: Option<ExprID> = None;
    let mut best_col: u32 = 0;

    for (id, loc) in func.arena.locs.iter().enumerate() {
        if loc.file == file && loc.line == line && loc.col <= col && loc.col >= best_col {
            best_col = loc.col;
            best_id = Some(id);
        }
    }

    best_id.map(|id| (id, func.arena.locs[id]))
}
