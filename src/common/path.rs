use crate::error::{
    diagnostic::{Diagnostic, Label},
    DiagnosticResult,
};
use crate::span::Span;
use path_absolutize::Absolutize;
use std::path::{Path, PathBuf};

pub enum RelativeTo<'a> {
    Path(&'a Path),
    Cwd,
}

pub fn try_resolve_relative_path<'a>(
    path: &'a Path,
    relative_to: &RelativeTo,
    span: Option<Span>,
) -> DiagnosticResult<PathBuf> {
    let absolute_path = get_absolute_path(relative_to, path);

    if absolute_path.exists() {
        Ok(absolute_path.to_path_buf())
    } else {
        let diagnostic = Diagnostic::error()
            .with_message(format!("path `{}` doesn't exist", path.display()))
            .maybe_with_label(span.map(|span| Label::primary(span, "doesn't exist")))
            .with_note(format!("absolute path is: {}", absolute_path.display()));

        Err(diagnostic)
    }
}

#[allow(unused)]
pub fn maybe_resolve_relative_path(path: &Path, relative_to: &RelativeTo) -> Option<PathBuf> {
    let absolute_path = get_absolute_path(relative_to, path);
    absolute_path.exists().then(|| absolute_path.to_path_buf())
}

#[allow(unused)]
pub fn resolve_relative_path(path: &Path, relative_to: &RelativeTo) -> PathBuf {
    get_absolute_path(relative_to, path).to_path_buf()
}

fn get_absolute_path<'a>(relative_to: &'a RelativeTo, path: &'a Path) -> std::borrow::Cow<'a, Path> {
    match relative_to {
        RelativeTo::Path(relative_to) => path.absolutize_from(relative_to).unwrap(),
        RelativeTo::Cwd => path.absolutize().unwrap(),
    }
}
