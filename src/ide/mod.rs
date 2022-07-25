mod hint;
mod types;
mod util;

use self::hint::{CollectHints, HintSess};
use crate::{
    error::diagnostic::DiagnosticSeverity,
    hir,
    infer::{normalize::Normalize, type_ctx::TypeCtx},
    span::{EndPosition, Position, Span},
    types::Type,
    workspace::Workspace,
};
use types::*;
use util::*;

pub fn diagnostics(workspace: &Workspace, tcx: Option<&TypeCtx>, typed_ast: Option<&hir::Cache>) {
    let mut objects: Vec<IdeObject> = vec![];

    objects.extend(
        workspace
            .diagnostics
            .items()
            .iter()
            .filter(|diag| !diag.labels.is_empty())
            .map(|diag| {
                diag.labels.first().map(|label| {
                    let file = workspace.diagnostics.get_file(label.span.file_id).unwrap();

                    IdeObject::Diagnostic(IdeDiagnostic {
                        severity: match &diag.severity {
                            DiagnosticSeverity::Error => IdeDiagnosticSeverity::Error,
                        },
                        span: IdeSpan::from_span_and_file(label.span, file.name()),
                        message: match &diag.message {
                            Some(message) => format!("{}\n{}", message, &label.message),
                            None => label.message.to_string(),
                        },
                    })
                })
            })
            .flatten(),
    );

    match (tcx, typed_ast) {
        (Some(tcx), Some(typed_ast)) => {
            let mut sess = HintSess {
                workspace,
                tcx,
                hints: vec![],
            };

            typed_ast
                .bindings
                .iter()
                .for_each(|(_, binding)| binding.collect_hints(&mut sess));

            typed_ast
                .functions
                .iter()
                .for_each(|(_, function)| function.collect_hints(&mut sess));

            objects.extend(sess.hints.into_iter().map(IdeObject::Hint));
        }
        _ => (),
    }

    write(&objects);
}

pub fn hover_info(workspace: &Workspace, tcx: Option<&TypeCtx>, offset: usize) {
    if let Some(tcx) = tcx {
        let searched_binding_info = workspace.binding_infos.iter().map(|(_, b)| b).find(|binding_info| {
            binding_info.module_id == workspace.root_module_id
                && binding_info.is_is_user_defined()
                && binding_info.span.contains(offset)
        });

        if let Some(binding_info) = searched_binding_info {
            write(&HoverInfo {
                contents: binding_info.ty.normalize(tcx).to_string(),
            });
        }
    } else {
        write_null();
    }
}

pub fn goto_definition(workspace: &Workspace, tcx: Option<&TypeCtx>, offset: usize) {
    for (_, binding_info) in workspace.binding_infos.iter() {
        if is_offset_in_span_and_root_module(workspace, offset, binding_info.span) {
            if let Some(tcx) = tcx {
                match binding_info.ty.normalize(tcx) {
                    Type::Module(module_id) => {
                        let module_info = workspace.module_infos.get(module_id).unwrap();

                        let span = Span {
                            file_id: module_info.file_id,
                            start: Position::initial(),
                            end: EndPosition::initial(),
                        };

                        write(&IdeSpan::from_span_and_file(span, module_info.file_path.to_string()));

                        return;
                    }
                    _ => (),
                }
            }

            write(&IdeSpan::from_span(binding_info.span, workspace));

            return;
        }

        for &use_span in binding_info.uses.iter() {
            if is_offset_in_span_and_root_module(workspace, offset, use_span) {
                write(&IdeSpan::from_span(binding_info.span, workspace));
                return;
            }
        }
    }

    write_null();
}
