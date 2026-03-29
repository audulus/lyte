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
    uri_str.parse().unwrap_or_else(|_| "file:///unknown".parse().unwrap())
}

/// Simple percent-decoding for file paths.
fn percent_decode(s: &str) -> String {
    let mut result = String::with_capacity(s.len());
    let bytes = s.as_bytes();
    let mut i = 0;
    while i < bytes.len() {
        if bytes[i] == b'%' && i + 2 < bytes.len() {
            if let Ok(byte) = u8::from_str_radix(
                std::str::from_utf8(&bytes[i + 1..i + 3]).unwrap_or(""),
                16,
            ) {
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
