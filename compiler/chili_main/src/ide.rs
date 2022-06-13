use chili_ast::workspace::{BindingInfo, Workspace};
use chili_error::diagnostic::DiagnosticSeverity;
use chili_infer::{display::DisplayTy, normalize::Normalize, ty_ctx::TyCtx};
use chili_span::Span;
use serde::{Deserialize, Serialize};

pub(crate) fn write_diagnostics(workspace: &Workspace) {
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

pub(crate) fn write_hover_info(workspace: &Workspace, tycx: Option<&TyCtx>, index: usize) {
    match (tycx, find_index_in_workspace(workspace, index)) {
        (Some(tycx), Some(binding_info)) => {
            let hover_info = HoverInfo {
                contents: binding_info.ty.display(tycx),
            };

            let json = serde_json::to_string(&hover_info).unwrap();

            println!("{}", json);
        }
        _ => print_null(),
    }
}

fn find_index_in_workspace(workspace: &Workspace, index: usize) -> Option<&BindingInfo> {
    workspace.binding_infos.iter().find(|binding_info| {
        binding_info.module_id == workspace.root_module_id
            && binding_info.span.range().contains(&index)
    })
}

#[inline]
fn print_null() {
    println!("null")
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

#[derive(Serialize, Deserialize, Clone)]
struct HoverInfo {
    contents: String,
}
