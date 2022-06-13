use chili_ast::{
    ast::TypedAst,
    ty::TyKind,
    workspace::{BindingInfo, Workspace},
};
use chili_error::diagnostic::DiagnosticSeverity;
use chili_infer::{display::DisplayTy, normalize::Normalize, ty_ctx::TyCtx};
use chili_span::{EndPosition, Position, Span};
use serde::{Deserialize, Serialize};

pub(crate) fn diagnostics(
    workspace: &Workspace,
    tycx: Option<&TyCtx>,
    typed_ast: Option<&TypedAst>,
) {
    let mut objects: Vec<IdeObject> = vec![];

    objects.extend(
        workspace
            .diagnostics
            .items()
            .iter()
            .filter(|diag| !diag.labels.is_empty())
            .map(|diag| {
                let label = diag.labels.first().unwrap();
                let file = workspace.diagnostics.get_file(label.span.file_id).unwrap();

                IdeObject::Diagnostic(IdeDiagnostic {
                    severity: match &diag.severity {
                        DiagnosticSeverity::Error => IdeDiagnosticSeverity::Error,
                    },
                    span: IdeSpan::from_span_and_file(label.span, file.name()),
                    message: diag.message.clone().unwrap(),
                })
            }),
    );

    match (tycx, typed_ast) {
        (Some(tycx), Some(typed_ast)) => typed_ast.bindings.iter().for_each(|binding| {
            if binding.ty_expr.is_none() {
                let ty = binding.ty.normalize(tycx);

                match ty {
                    TyKind::Function(_) | TyKind::Module(_) | TyKind::Type(_) | TyKind::AnyType => {
                        ()
                    }
                    _ => {
                        let span = binding.pattern.span();
                        let file = workspace.diagnostics.get_file(span.file_id).unwrap();

                        objects.push(IdeObject::Hint(Hint {
                            span: IdeSpan::from_span_and_file(span, file.name()),
                            type_name: ty.to_string(),
                        }))
                    }
                }
            }
        }),
        _ => (),
    }

    write(&objects);
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

pub(crate) fn goto_definition(workspace: &Workspace, tycx: Option<&TyCtx>, offset: usize) {
    for binding_info in workspace.binding_infos.iter() {
        if is_offset_in_span_and_root_module(workspace, offset, binding_info.span) {
            if let Some(tycx) = tycx {
                match binding_info.ty.normalize(tycx) {
                    TyKind::Module(module_id) => {
                        let module_info = workspace.get_module_info(module_id).unwrap();

                        let span = Span {
                            file_id: module_info.file_id,
                            start: Position::initial(),
                            end: EndPosition::initial(),
                        };

                        write(&IdeSpan::from_span_and_file(
                            span,
                            module_info.file_path.to_string(),
                        ));

                        return;
                    }
                    _ => (),
                }
            }

            write(&IdeSpan::from_span_and_file(
                binding_info.span,
                workspace
                    .get_module_info(binding_info.module_id)
                    .unwrap()
                    .file_path
                    .to_string(),
            ));

            return;
        }

        for &use_span in binding_info.uses.iter() {
            if is_offset_in_span_and_root_module(workspace, offset, use_span) {
                write(&IdeSpan::from_span_and_file(
                    binding_info.span,
                    workspace
                        .get_module_info(binding_info.module_id)
                        .unwrap()
                        .file_path
                        .to_string(),
                ));

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

#[derive(Debug, Serialize, Deserialize, Clone)]
struct IdeSpan {
    file: String,
    start: usize,
    end: usize,
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
struct IdeDiagnostic {
    severity: IdeDiagnosticSeverity,
    span: IdeSpan,
    message: String,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
enum IdeDiagnosticSeverity {
    Error,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
struct Hint {
    span: IdeSpan,
    type_name: String,
}

#[derive(Debug, Serialize, Deserialize, Clone)]
#[serde(tag = "type")]
enum IdeObject {
    Diagnostic(IdeDiagnostic),
    Hint(Hint),
}

#[derive(Debug, Serialize, Deserialize, Clone)]
struct HoverInfo {
    contents: String,
}
