use std::fmt::Display;

use crate::{span::Span, workspace::Workspace};
use serde::{Deserialize, Serialize};

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct IdeSpan {
    pub file: String,
    pub start: usize,
    pub end: usize,
}

impl IdeSpan {
    pub fn from_span_and_file(span: Span, file: impl Into<String>) -> Self {
        Self {
            file: file.into(),
            start: span.start.index,
            end: span.end.index,
        }
    }

    pub fn from_span(span: Span, workspace: &Workspace) -> Self {
        let module_id = workspace.find_module_id_by_file_id(span.file_id).unwrap();
        Self::from_span_and_file(
            span,
            workspace.module_infos.get(module_id).unwrap().file_path.to_string(),
        )
    }
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct IdeDiagnostic {
    pub severity: IdeDiagnosticSeverity,
    pub span: IdeSpan,
    pub message: String,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub enum IdeDiagnosticSeverity {
    Error,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct Hint {
    pub span: IdeSpan,
    pub type_name: String,
    pub kind: String,
}

#[derive(Debug, Clone, Copy)]
pub enum HintKind {
    Binding,
    ReturnType,
    ImplicitParam,
}

impl Display for HintKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                HintKind::Binding => "Binding",
                HintKind::ReturnType => "ReturnType",
                HintKind::ImplicitParam => "ImplicitParam",
            }
        )
    }
}

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(tag = "type")]
pub enum IdeObject {
    Diagnostic(IdeDiagnostic),
    Hint(Hint),
}

#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct HoverInfo {
    pub contents: String,
}
