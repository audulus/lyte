use lsp_types::Uri;
use lyte::Compiler;
use std::collections::HashMap;

pub struct AnalysisState {
    /// URI -> source text for all open documents.
    documents: HashMap<Uri, String>,
    /// The compiler state after the last analysis run.
    compiler: Option<Compiler>,
}

impl AnalysisState {
    pub fn new() -> Self {
        Self {
            documents: HashMap::new(),
            compiler: None,
        }
    }

    pub fn update_document(&mut self, uri: Uri, text: String) {
        self.documents.insert(uri, text);
        self.analyze();
    }

    pub fn remove_document(&mut self, uri: &Uri) {
        self.documents.remove(uri);
        if !self.documents.is_empty() {
            self.analyze();
        } else {
            self.compiler = None;
        }
    }

    pub fn compiler(&self) -> Option<&Compiler> {
        self.compiler.as_ref()
    }

    pub fn document_uris(&self) -> Vec<Uri> {
        self.documents.keys().cloned().collect()
    }

    fn analyze(&mut self) {
        let mut compiler = Compiler::new();
        compiler.quiet = true;
        compiler.check_all = true;

        for (uri, text) in &self.documents {
            let path = uri_to_path(uri);
            compiler.parse(text, &path);
        }

        compiler.check();
        self.compiler = Some(compiler);
    }
}

/// Convert a file:// URI to a path string suitable for Loc.file.
pub fn uri_to_path(uri: &Uri) -> String {
    let s = uri.as_str();
    if let Some(path) = s.strip_prefix("file://") {
        // URL-decode percent-encoded characters.
        percent_decode(path)
    } else {
        s.to_string()
    }
}

/// Convert a Loc file path back to a URI.
pub fn path_to_uri(path: &str) -> Uri {
    // Construct a file:// URI from an absolute path.
    let uri_str = if path.starts_with('/') {
        format!("file://{}", path)
    } else {
        format!("file:///{}", path)
    };
    uri_str
        .parse()
        .unwrap_or_else(|_| "file:///unknown".parse().unwrap())
}

/// Simple percent-decoding for file paths.
fn percent_decode(s: &str) -> String {
    let mut result = String::with_capacity(s.len());
    let bytes = s.as_bytes();
    let mut i = 0;
    while i < bytes.len() {
        if bytes[i] == b'%' && i + 2 < bytes.len() {
            if let Ok(byte) =
                u8::from_str_radix(std::str::from_utf8(&bytes[i + 1..i + 3]).unwrap_or(""), 16)
            {
                result.push(byte as char);
                i += 3;
                continue;
            }
        }
        result.push(bytes[i] as char);
        i += 1;
    }
    result
}

#[cfg(test)]
mod tests {
    use super::*;

    fn test_uri(path: &str) -> Uri {
        format!("file://{}", path).parse().unwrap()
    }

    #[test]
    fn analysis_state_new_has_no_compiler() {
        let state = AnalysisState::new();
        assert!(state.compiler().is_none());
    }

    #[test]
    fn update_document_triggers_analysis() {
        let mut state = AnalysisState::new();
        let uri = test_uri("/test.lyte");
        state.update_document(uri, "fn foo() -> Float { 1.0 }".to_string());
        assert!(state.compiler().is_some());
    }

    #[test]
    fn remove_last_document_clears_compiler() {
        let mut state = AnalysisState::new();
        let uri = test_uri("/test.lyte");
        state.update_document(uri.clone(), "fn foo() -> Float { 1.0 }".to_string());
        assert!(state.compiler().is_some());
        state.remove_document(&uri);
        assert!(state.compiler().is_none());
    }

    #[test]
    fn document_uris_tracks_open_files() {
        let mut state = AnalysisState::new();
        let uri1 = test_uri("/a.lyte");
        let uri2 = test_uri("/b.lyte");
        state.update_document(uri1.clone(), "fn a() -> Float { 1.0 }".to_string());
        state.update_document(uri2.clone(), "fn b() -> Float { 2.0 }".to_string());
        let uris = state.document_uris();
        assert_eq!(uris.len(), 2);
        assert!(uris.contains(&uri1));
        assert!(uris.contains(&uri2));
    }

    #[test]
    fn uri_to_path_strips_file_scheme() {
        assert_eq!(uri_to_path(&test_uri("/foo/bar.lyte")), "/foo/bar.lyte");
    }

    #[test]
    fn uri_to_path_decodes_percent_encoding() {
        let uri: Uri = "file:///my%20folder/test.lyte".parse().unwrap();
        assert_eq!(uri_to_path(&uri), "/my folder/test.lyte");
    }

    #[test]
    fn path_to_uri_roundtrip() {
        let path = "/foo/bar.lyte";
        let uri = path_to_uri(path);
        assert_eq!(uri_to_path(&uri), path);
    }

    #[test]
    fn percent_decode_no_encoding() {
        assert_eq!(percent_decode("/simple/path"), "/simple/path");
    }

    #[test]
    fn percent_decode_space() {
        assert_eq!(percent_decode("/my%20path"), "/my path");
    }

    #[test]
    fn percent_decode_multiple() {
        assert_eq!(percent_decode("%41%42%43"), "ABC");
    }
}
