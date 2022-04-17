use chili_error::{
    diagnostic::{Diagnostic, Label},
    DiagnosticResult,
};
use chili_span::Span;
use path_absolutize::Absolutize;
use std::path::Path;

pub fn resolve_relative_path(
    path: &Path,
    relative_to: &str,
    span: Option<Span>,
) -> DiagnosticResult<String> {
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
            .with_note(format!("absolute path is: {}", absolute_path.display()));

        Err(match span {
            Some(span) => diagnostic.with_label(Label::primary(span, "doesn't exist")),
            None => diagnostic,
        })
    }
}
