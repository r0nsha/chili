use std::path::{Path, PathBuf};

use codespan_reporting::diagnostic::{Diagnostic, Label};
use common::compiler_info;
use path_absolutize::Absolutize;

use chilic_error::DiagnosticResult;
use chilic_span::Span;

pub fn resolve_relative_path(
    path: &str,
    relative_to: &str,
    span: Option<&Span>,
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
                span.range.clone(),
            )]),
            None => diagnostic,
        })
    }
}

pub trait AsModuleName {
    fn as_module_name(&self, root_dir: &impl AsRef<str>) -> String;
}

impl AsModuleName for PathBuf {
    fn as_module_name(&self, root_dir: &impl AsRef<str>) -> String {
        self.to_str().unwrap().to_string().as_module_name(root_dir)
    }
}

impl AsModuleName for Path {
    fn as_module_name(&self, root_dir: &impl AsRef<str>) -> String {
        self.to_str().unwrap().to_string().as_module_name(root_dir)
    }
}

impl AsModuleName for String {
    fn as_module_name(&self, root_dir: &impl AsRef<str>) -> String {
        // TODO: this `std_root_dir` thing feels hacky. we should probably get
        // `std` from `root_dir`, and not do this ad-hoc.
        let mut std_root_dir = compiler_info::std_module_root_dir()
            .parent()
            .unwrap()
            .to_str()
            .unwrap()
            .to_string();

        std_root_dir.push(std::path::MAIN_SEPARATOR);

        self.replace(root_dir.as_ref(), "")
            .replace(&std_root_dir, "")
            .replace(std::path::MAIN_SEPARATOR, ".")
    }
}
