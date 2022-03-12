use chili_ast::{ast, ty::TyKind};
use chili_error::DiagnosticResult;
use chili_span::{MaybeSpanned, Span};
use codespan_reporting::diagnostic::{Diagnostic, Label};
use ustr::Ustr;

pub(crate) enum LvalueAccessErr {
    ImmutableReference {
        symbol: String,
        binding_span: Option<Span>,
        ty_str: String,
    },
    ImmutableMemberAccess {
        root_symbol: String,
        binding_span: Span,
        full_path: String,
    },
    Immutablebinding {
        symbol: String,
        binding_span: Span,
    },
    InvalidLValue,
}

pub(crate) fn check_lvalue_access(expr: &ast::Expr, expr_span: Span) -> DiagnosticResult<()> {
    use LvalueAccessErr::*;

    check_lvalue_mutability_internal(expr, expr_span, true).map_err(|err| match err {
        ImmutableReference {
            symbol,
            binding_span,
            ty_str,
        } => {
            let mut labels =
                vec![Label::primary(expr_span.file_id, expr_span.range())
                    .with_message("cannot assign")];

            if let Some(binding_span) = binding_span {
                labels.push(
                    Label::secondary(binding_span.file_id, binding_span.range())
                        .with_message("consider referencing as mutable"),
                );
            }

            Diagnostic::error()
                .with_message(format!(
                    "cannot assign to `{}`, which is behind an immutable `{}`",
                    symbol, ty_str
                ))
                .with_labels(labels)
        }
        ImmutableMemberAccess {
            root_symbol,
            binding_span,
            full_path,
        } => Diagnostic::error()
            .with_message(format!(
                "cannot assign to `{}`, as `{}` is not declared as mutable",
                full_path, root_symbol
            ))
            .with_labels(vec![
                Label::primary(expr_span.file_id, expr_span.range()).with_message("cannot assign"),
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
                "cannot assign to `{}`, as it is not declared as mutable",
                symbol
            ))
            .with_labels(vec![
                Label::primary(expr_span.file_id, expr_span.range()).with_message("cannot assign"),
                Label::secondary(binding_span.file_id, binding_span.range()).with_message(format!(
                    "consider making this binding mutable: `mut {}`",
                    symbol
                )),
            ]),
        InvalidLValue => Diagnostic::error()
            .with_message("invalid left-hand side of assignment")
            .with_labels(vec![Label::primary(expr_span.file_id, expr_span.range())
                .with_message("cannot assign to this expression")]),
    })
}

fn check_lvalue_mutability_internal(
    expr: &ast::Expr,
    original_expr_span: Span,
    is_direct_assign: bool,
) -> Result<(), LvalueAccessErr> {
    use LvalueAccessErr::*;

    match &expr.kind {
        ast::ExprKind::Unary { op, lhs } => match op {
            ast::UnaryOp::Deref => check_deref(lhs),
            _ => Err(InvalidLValue),
        },
        ast::ExprKind::MemberAccess { expr, member } => {
            check_member_access(expr, *member, original_expr_span)
        }
        ast::ExprKind::Subscript { expr, .. } => check_subscript(expr, original_expr_span),
        ast::ExprKind::Id {
            symbol,
            is_mutable,
            binding_span,
            binding_info_idx: _,
        } => check_id(
            *symbol,
            *is_mutable,
            *binding_span,
            &expr.ty,
            is_direct_assign,
        ),
        _ => Err(InvalidLValue),
    }
}

fn check_deref(lhs: &ast::Expr) -> Result<(), LvalueAccessErr> {
    use LvalueAccessErr::*;

    if let TyKind::Pointer(_, is_mutable) = &lhs.ty {
        if *is_mutable {
            Ok(())
        } else {
            let MaybeSpanned { value, span } = lhs.display_name_and_binding_span();
            Err(ImmutableReference {
                symbol: value,
                binding_span: span,
                ty_str: lhs.ty.to_string(),
            })
        }
    } else {
        unreachable!("got {}", lhs.ty)
    }
}

fn check_member_access(
    expr: &ast::Expr,
    member: Ustr,
    original_expr_span: Span,
) -> Result<(), LvalueAccessErr> {
    use LvalueAccessErr::*;

    check_lvalue_mutability_internal(expr, original_expr_span, false).map_err(|err| match err {
        ImmutableMemberAccess {
            root_symbol,
            binding_span,
            full_path,
        } => ImmutableMemberAccess {
            root_symbol,
            binding_span,
            full_path: format!("{}.{}", full_path, member),
        },
        ImmutableReference {
            symbol,
            binding_span,
            ty_str,
        } => ImmutableReference {
            symbol: format!("{}.{}", symbol, member),
            binding_span,
            ty_str,
        },
        Immutablebinding {
            symbol,
            binding_span,
        } => {
            let full_path = format!("{}.{}", symbol, member);
            ImmutableMemberAccess {
                root_symbol: symbol,
                binding_span,
                full_path,
            }
        }
        InvalidLValue => err,
    })
}

fn check_subscript(expr: &ast::Expr, original_expr_span: Span) -> Result<(), LvalueAccessErr> {
    use LvalueAccessErr::*;

    match &expr.ty {
        TyKind::Slice(_, is_mutable)
        | TyKind::MultiPointer(_, is_mutable)
        | TyKind::Pointer(_, is_mutable) => {
            return if *is_mutable {
                Ok(())
            } else {
                let MaybeSpanned { value, span } = expr.display_name_and_binding_span();
                Err(ImmutableReference {
                    symbol: format!("{}[_]", value),
                    binding_span: span,
                    ty_str: expr.ty.to_string(),
                })
            };
        }
        _ => (),
    }

    check_lvalue_mutability_internal(expr, original_expr_span, false).map_err(|err| match err {
        ImmutableMemberAccess {
            root_symbol,
            binding_span,
            full_path,
        } => ImmutableMemberAccess {
            root_symbol,
            binding_span,
            full_path: format!("{}[_]", full_path),
        },
        ImmutableReference {
            symbol,
            binding_span,
            ty_str,
        } => ImmutableReference {
            symbol: format!("{}[_]", symbol),
            binding_span,
            ty_str,
        },
        Immutablebinding {
            symbol,
            binding_span,
        } => Immutablebinding {
            symbol: format!("{}[_]", symbol),
            binding_span,
        },
        InvalidLValue => err,
    })
}

fn check_id(
    symbol: Ustr,
    is_mutable: bool,
    binding_span: Span,
    ty: &TyKind,
    is_direct_assign: bool,
) -> Result<(), LvalueAccessErr> {
    use LvalueAccessErr::*;

    if !is_direct_assign {
        match ty {
            TyKind::Slice(_, is_mutable)
            | TyKind::MultiPointer(_, is_mutable)
            | TyKind::Pointer(_, is_mutable) => {
                return if *is_mutable {
                    Ok(())
                } else {
                    Err(ImmutableReference {
                        symbol: symbol.to_string(),
                        binding_span: Some(binding_span),
                        ty_str: ty.to_string(),
                    })
                }
            }
            _ => (),
        }
    }

    if is_mutable {
        Ok(())
    } else {
        Err(Immutablebinding {
            symbol: symbol.to_string(),
            binding_span: binding_span,
        })
    }
}
