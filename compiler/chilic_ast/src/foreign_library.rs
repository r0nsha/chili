use std::path::Path;

use chilic_error::DiagnosticResult;
use chilic_span::Span;
use ustr::Ustr;

use crate::path::resolve_relative_path;

#[derive(Hash, Debug, Eq, PartialEq, Clone)]
pub enum ForeignLibrary {
    System(String),
    Path { lib_path: String, lib_name: String },
}

impl ForeignLibrary {
    pub fn from_str(
        string: &str,
        module_path: Ustr,
        span: Span,
    ) -> DiagnosticResult<Self> {
        const SYSTEM_PREFIX: &str = "system:";

        if string.starts_with(SYSTEM_PREFIX) {
            let split: Vec<&str> = string.split(SYSTEM_PREFIX).collect();
            Ok(ForeignLibrary::System(split[1].to_string()))
        } else {
            let relative_to = Path::new(module_path.as_str())
                .parent()
                .unwrap()
                .to_str()
                .unwrap();

            let path = resolve_relative_path(string, relative_to, Some(span))?;
            let path = Path::new(&path);

            Ok(ForeignLibrary::Path {
                lib_path: path.parent().unwrap().to_str().unwrap().to_string(),
                lib_name: path
                    .file_name()
                    .unwrap()
                    .to_str()
                    .unwrap()
                    .to_string(),
            })
        }
    }
}
