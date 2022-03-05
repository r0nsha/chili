use std::path::{Path, PathBuf};

use codespan_reporting::diagnostic::{Diagnostic, Label};
use common::compiler_info;
use path_absolutize::Absolutize;

use chili_error::DiagnosticResult;
use chili_span::Span;

pub fn resolve_relative_path(
    path: &str,
    relative_to: &str,
    span: Option<Span>,
) -> DiagnosticResult<String> {
    let path = Path::new(&path);

    let absolute_path = if relative_to.is_empty() {
        path.absolutize().unwrap()
    } else {
        path.absolutize_from(Path::new(relative_to)).unwrap()
    };

    if absolute_path.exists() {
        Ok(absolute_path.to_str().unwrap().to_string())
    } else {
        let diagnostic = Diagnostic::error()
            .with_message(format!("path `{}` doesn't exist", path.display()))
            .with_notes(vec![format!(
                "absolute path is: {}",
                absolute_path.display()
            )]);

        Err(match span {
            Some(span) => diagnostic.with_labels(vec![Label::primary(
                span.file_id,
                span.range().clone(),
            )]),
            None => diagnostic,
        })
    }
}
