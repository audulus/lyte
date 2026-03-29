use lsp_server::{Connection, Message, Notification, Request, Response};
use lsp_types::*;

mod analysis;
mod diagnostics;
mod goto_def;
mod hover;

fn main() {
    let (connection, io_threads) = Connection::stdio();

    let server_capabilities = serde_json::to_value(ServerCapabilities {
        text_document_sync: Some(TextDocumentSyncCapability::Kind(
            TextDocumentSyncKind::FULL,
        )),
        hover_provider: Some(HoverProviderCapability::Simple(true)),
        definition_provider: Some(OneOf::Left(true)),
        ..Default::default()
    })
    .unwrap();

    let _init_params = connection.initialize(server_capabilities).unwrap();

    let mut state = analysis::AnalysisState::new();
    main_loop(&connection, &mut state);
    io_threads.join().unwrap();
}

fn main_loop(connection: &Connection, state: &mut analysis::AnalysisState) {
    for msg in &connection.receiver {
        match msg {
            Message::Request(req) => {
                if connection.handle_shutdown(&req).unwrap() {
                    return;
                }
                handle_request(connection, state, req);
            }
            Message::Notification(not) => {
                handle_notification(connection, state, not);
            }
            Message::Response(_) => {}
        }
    }
}

fn handle_request(
    connection: &Connection,
    state: &mut analysis::AnalysisState,
    req: Request,
) {
    if let Some(params) = cast_request::<request::HoverRequest>(&req) {
        let result = hover::handle_hover(state, &params);
        let resp = Response::new_ok(req.id, serde_json::to_value(result).unwrap());
        connection.sender.send(Message::Response(resp)).unwrap();
    } else if let Some(params) = cast_request::<request::GotoDefinition>(&req) {
        let result = goto_def::handle_goto_definition(state, &params);
        let resp = Response::new_ok(req.id, serde_json::to_value(result).unwrap());
        connection.sender.send(Message::Response(resp)).unwrap();
    } else {
        let resp = Response::new_err(
            req.id,
            lsp_server::ErrorCode::MethodNotFound as i32,
            "method not found".to_string(),
        );
        connection.sender.send(Message::Response(resp)).unwrap();
    }
}

fn handle_notification(
    connection: &Connection,
    state: &mut analysis::AnalysisState,
    not: Notification,
) {
    match not.method.as_str() {
        "textDocument/didOpen" => {
            if let Ok(params) =
                serde_json::from_value::<DidOpenTextDocumentParams>(not.params)
            {
                let uri = params.text_document.uri;
                state.update_document(uri.clone(), params.text_document.text);
                publish_diagnostics(connection, state);
            }
        }
        "textDocument/didChange" => {
            if let Ok(params) =
                serde_json::from_value::<DidChangeTextDocumentParams>(not.params)
            {
                let uri = params.text_document.uri;
                if let Some(change) = params.content_changes.into_iter().last() {
                    state.update_document(uri.clone(), change.text);
                    publish_diagnostics(connection, state);
                }
            }
        }
        "textDocument/didClose" => {
            if let Ok(params) =
                serde_json::from_value::<DidCloseTextDocumentParams>(not.params)
            {
                let uri = params.text_document.uri.clone();
                state.remove_document(&params.text_document.uri);
                // Clear diagnostics for closed file.
                let params = PublishDiagnosticsParams::new(uri, vec![], None);
                let not = lsp_server::Notification::new(
                    "textDocument/publishDiagnostics".to_string(),
                    serde_json::to_value(params).unwrap(),
                );
                connection.sender.send(Message::Notification(not)).unwrap();
            }
        }
        _ => {}
    }
}

fn publish_diagnostics(connection: &Connection, state: &analysis::AnalysisState) {
    let all_diags = diagnostics::collect_diagnostics(state);

    // Publish diagnostics for each document (including empty ones to clear stale errors).
    for uri in state.document_uris() {
        let diags = all_diags.get(&uri).cloned().unwrap_or_default();
        let params = PublishDiagnosticsParams::new(uri, diags, None);
        let not = lsp_server::Notification::new(
            "textDocument/publishDiagnostics".to_string(),
            serde_json::to_value(params).unwrap(),
        );
        connection.sender.send(Message::Notification(not)).unwrap();
    }
}

fn cast_request<R: request::Request>(req: &Request) -> Option<R::Params> {
    if req.method == R::METHOD {
        serde_json::from_value(req.params.clone()).ok()
    } else {
        None
    }
}
