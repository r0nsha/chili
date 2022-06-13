use chili_ast::workspace::{BindingInfo, Workspace};
use chili_error::diagnostic::DiagnosticSeverity;
use chili_infer::{display::DisplayTy, ty_ctx::TyCtx};
use chili_span::Span;
use serde::{Deserialize, Serialize};

pub(crate) fn diagnostics(workspace: &Workspace) {
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

    write(&diagnostics);
}

pub(crate) fn hover_info(workspace: &Workspace, tycx: Option<&TyCtx>, offset: usize) {
    match (tycx, find_offset_in_root_module(workspace, offset)) {
        (Some(tycx), Some(binding_info)) => {
            write(&HoverInfo {
                contents: binding_info.ty.display(tycx),
            });
        }
        _ => write_null(),
    }
}

pub(crate) fn goto_definition(workspace: &Workspace, offset: usize) {
    for binding_info in workspace.binding_infos.iter() {
        if is_offset_in_span_and_root_module(workspace, offset, binding_info.span) {
            write(&SpanResponse {
                span: IdeSpan::from(binding_info.span),
            });
            return;
        }

        for &use_span in binding_info.uses.iter() {
            if is_offset_in_span_and_root_module(workspace, offset, use_span) {
                write(&SpanResponse {
                    span: IdeSpan::from(binding_info.span),
                });
                return;
            }
        }
    }

    write_null();
}

fn find_offset_in_root_module(workspace: &Workspace, offset: usize) -> Option<&BindingInfo> {
    workspace.binding_infos.iter().find(|binding_info| {
        binding_info.module_id == workspace.root_module_id && binding_info.span.contains(offset)
    })
}

fn is_offset_in_span_and_root_module(workspace: &Workspace, offset: usize, span: Span) -> bool {
    span.contains(offset)
        && workspace
            .find_module_id_by_file_id(span.file_id)
            .map_or(false, |module_id| module_id == workspace.root_module_id)
}

#[inline]
fn write<T>(value: &T)
where
    T: ?Sized + serde::Serialize,
{
    println!("{}", serde_json::to_string(value).unwrap())
}

#[inline]
fn write_null() {
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

#[derive(Serialize, Deserialize, Clone)]
struct SpanResponse {
    span: IdeSpan,
}
