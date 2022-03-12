use chili_ast::{
    ast,
    ty::TyKind,
    workspace::{BindingInfo, ModuleIdx, Workspace},
};
use chili_error::DiagnosticResult;
use chili_span::Span;
use codespan_reporting::diagnostic::{Diagnostic, Label};
use ustr::{ustr, Ustr};

use crate::sess::LintSess;

enum RefAccessErr {
    ImmutableReference {
        symbol: Ustr,
        ty_str: String,
    },
    ImmutableMemberAccess {
        root_symbol: Ustr,
        binding_span: Span,
        full_path: String,
    },
    Immutablebinding {
        symbol: Ustr,
        binding_span: Span,
    },
}

pub(super) fn check_expr_can_be_mutably_referenced(
    sess: &LintSess,
    expr: &ast::Expr,
) -> DiagnosticResult<()> {
    use RefAccessErr::*;

    check_expr_can_be_mutably_referenced_internal(sess, expr, true).map_err(|err| match err {
        ImmutableReference { symbol, ty_str } => Diagnostic::error()
            .with_message(format!(
                "cannot reference `{}`, because it is behind an immutable `{}`",
                symbol, ty_str
            ))
            .with_labels(vec![Label::primary(
                expr.span.file_id,
                expr.span.range().clone(),
            )
            .with_message("cannot reference")]),
        ImmutableMemberAccess {
            root_symbol,
            binding_span,
            full_path,
        } => Diagnostic::error()
            .with_message(format!(
                "cannot reference `{}`, as `{}` is not declared as mutable",
                full_path, root_symbol
            ))
            .with_labels(vec![
                Label::primary(expr.span.file_id, expr.span.range().clone())
                    .with_message("cannot reference"),
                Label::secondary(binding_span.file_id, binding_span.range()).with_message(format!(
                    "consider changing this to be mutable: `mut {}`",
                    root_symbol
                )),
            ]),
        Immutablebinding {
            symbol,
            binding_span,
        } => Diagnostic::error()
            .with_message(format!(
                "cannot reference `{}` as mutable, as it is not declared as mutable",
                symbol
            ))
            .with_labels(vec![
                Label::primary(expr.span.file_id, expr.span.range().clone())
                    .with_message("cannot reference immutable variable"),
                Label::secondary(binding_span.file_id, binding_span.range()).with_message(format!(
                    "consider changing this to be mutable: `mut {}`",
                    symbol
                )),
            ]),
    })
}

fn check_expr_can_be_mutably_referenced_internal(
    sess: &LintSess,
    expr: &ast::Expr,
    is_direct_ref: bool,
) -> Result<(), RefAccessErr> {
    use RefAccessErr::*;

    let ty = &expr.ty;

    match &expr.kind {
        ast::ExprKind::MemberAccess { expr, member } => {
            match check_expr_can_be_mutably_referenced_internal(sess, expr, true) {
                Ok(_) => match ty {
                    TyKind::Tuple(tys) => {
                        let index = member.parse::<usize>().unwrap();
                        let ty = &tys[index];

                        match ty {
                            TyKind::Slice(_, is_mutable)
                            | TyKind::MultiPointer(_, is_mutable)
                            | TyKind::Pointer(_, is_mutable)
                                if !is_mutable =>
                            {
                                Err(ImmutableReference {
                                    symbol: *member,
                                    ty_str: ty.to_string(),
                                })
                            }
                            _ => Ok(()),
                        }
                    }
                    TyKind::Struct(struct_ty) => {
                        let field_ty = struct_ty
                            .fields
                            .iter()
                            .find(|f| f.symbol == *member)
                            .map(|f| &f.ty)
                            .unwrap();

                        match field_ty {
                            TyKind::Slice(_, is_mutable)
                            | TyKind::MultiPointer(_, is_mutable)
                            | TyKind::Pointer(_, is_mutable)
                                if !is_mutable =>
                            {
                                Err(ImmutableReference {
                                    symbol: *member,
                                    ty_str: field_ty.to_string(),
                                })
                            }
                            _ => Ok(()),
                        }
                    }
                    TyKind::Module(module_idx) => {
                        let binding_info =
                            find_binding_info_in_module(sess.workspace, *module_idx, *member);

                        match &binding_info.ty {
                            TyKind::Slice(_, is_mutable)
                            | TyKind::MultiPointer(_, is_mutable)
                            | TyKind::Pointer(_, is_mutable)
                                if !is_mutable =>
                            {
                                Err(ImmutableReference {
                                    symbol: *member,
                                    ty_str: binding_info.ty.to_string(),
                                })
                            }
                            _ => {
                                if binding_info.is_mutable {
                                    Ok(())
                                } else {
                                    let module_info =
                                        sess.workspace.get_module_info(*module_idx).unwrap();

                                    Err(Immutablebinding {
                                        symbol: ustr(&format!("{}.{}", module_info.name, member)),
                                        binding_span: binding_info.span,
                                    })
                                }
                            }
                        }
                    }
                    _ => Ok(()),
                },
                Err(err) => Err(match err {
                    ImmutableMemberAccess {
                        root_symbol,
                        binding_span,
                        full_path,
                    } => ImmutableMemberAccess {
                        root_symbol,
                        binding_span,
                        full_path: format!("{}.{}", full_path, member),
                    },
                    ImmutableReference { symbol, ty_str } => ImmutableReference {
                        symbol: ustr(&format!("{}.{}", symbol, member)),
                        ty_str,
                    },
                    Immutablebinding {
                        symbol,
                        binding_span,
                    } => ImmutableMemberAccess {
                        root_symbol: symbol,
                        binding_span,
                        full_path: format!("{}.{}", symbol, member),
                    },
                }),
            }
        }
        ast::ExprKind::Id {
            symbol,
            is_mutable,
            binding_span,
            binding_info_idx: _,
        } => {
            match ty {
                TyKind::Slice(_, is_mutable)
                | TyKind::MultiPointer(_, is_mutable)
                | TyKind::Pointer(_, is_mutable) => {
                    if *is_mutable && is_direct_ref {
                        return Ok(());
                    } else {
                        return Err(ImmutableReference {
                            symbol: *symbol,
                            ty_str: ty.to_string(),
                        });
                    }
                }
                _ => (),
            }

            if *is_mutable {
                Ok(())
            } else {
                Err(Immutablebinding {
                    symbol: *symbol,
                    binding_span: *binding_span,
                })
            }
        }
        _ => Ok(()),
    }
}

fn find_binding_info_in_module(
    workspace: &Workspace,
    module_idx: ModuleIdx,
    symbol: Ustr,
) -> &BindingInfo {
    workspace
        .binding_infos
        .iter()
        .find(|b| b.module_idx == module_idx && b.symbol == symbol)
        .unwrap()
}
