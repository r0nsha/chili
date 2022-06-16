use std::fmt::Display;

use crate::span::Span;
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
