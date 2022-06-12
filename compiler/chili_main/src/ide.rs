use chili_ast::workspace::Workspace;
use chili_error::diagnostic::DiagnosticSeverity;
use chili_span::Span;
use serde::{Deserialize, Serialize};

pub(crate) fn do_ide_check(workspace: &Workspace) {
    let diagnostics: Vec<IdeDiagnosticResponse> = workspace
        .diagnostics
        .items()
        .iter()
        .map(|diag| {
            let label = diag.labels.first().unwrap();
            let file = workspace.diagnostics.get_file(label.span.file_id).unwrap();

            let ide_diag = IdeDiagnostic {
                severity: match &diag.severity {
                    DiagnosticSeverity::Error => IdeDiagnosticSeverity::Error,
                },
                span: label.span.into(),
                message: diag.message.clone().unwrap(),
                source: file.name().to_string(),
            };

            IdeDiagnosticResponse::new(ide_diag)
        })
        .collect();

    let json = serde_json::to_string(&diagnostics).unwrap();

    println!("{}", json)
}

#[derive(Serialize, Deserialize, Clone, Copy)]
struct IdeSpan {
    start: usize,
    end: usize,
}

impl From<Span> for IdeSpan {
    fn from(s: Span) -> Self {
        IdeSpan {
            start: s.start.index,
            end: s.end.index,
        }
    }
}

#[derive(Serialize, Deserialize, Clone)]
struct IdeDiagnostic {
    severity: IdeDiagnosticSeverity,
    span: IdeSpan,
    message: String,
    source: String,
}

#[derive(Serialize, Deserialize, Clone)]
enum IdeDiagnosticSeverity {
    Error,
}

#[derive(Serialize, Deserialize, Clone)]
struct IdeDiagnosticResponse {
    r#type: String,
    diagnostic: IdeDiagnostic,
}

impl IdeDiagnosticResponse {
    fn new(diagnostic: IdeDiagnostic) -> Self {
        Self {
            r#type: "diagnostic".to_string(),
            diagnostic,
        }
    }
}
