use crate::{
    BindingInfo, CheckedExpr, {AnalysisContext, AnalysisFrame},
};
use chili_ast::ast::{Expr, ExprKind, UnaryOp};
use chili_error::DiagnosticResult;
use chili_span::{MaybeSpanned, Span};
use chili_ty::*;
use codespan_reporting::diagnostic::{Diagnostic, Label};
use ustr::Ustr;

impl<'a> AnalysisContext<'a> {
    pub(crate) fn check_assign_expr(
        &mut self,
        frame: &mut AnalysisFrame,
        lvalue: &Expr,
        rvalue: &Expr,
        span: Span,
    ) -> DiagnosticResult<CheckedExpr> {
        let lvalue = match &lvalue.kind {
            ExprKind::Id {
                symbol, is_mutable, ..
            } => {
                let (lvalue, is_init) =
                    self.check_id(frame, *symbol, lvalue.span, true)?;

                if is_init {
                    check_lvalue_is_mut(&lvalue.expr, lvalue.expr.span)?;
                } else {
                    // set binding as init in the current scope
                    frame.insert_binding_info(
                        *symbol,
                        BindingInfo {
                            ty: lvalue.ty.clone(),
                            const_value: None,
                            is_mutable: *is_mutable,
                            is_init: true,
                            span: lvalue.expr.span,
                        },
                    );
                }

                lvalue
            }
            _ => {
                let lvalue = self.check_expr(frame, lvalue, None)?;
                check_lvalue_is_mut(&lvalue.expr, lvalue.expr.span)?;
                lvalue
            }
        };

        let mut rvalue =
            self.check_expr(frame, rvalue, Some(lvalue.ty.clone()))?;

        let rvalue_span = rvalue.expr.span;
        self.infcx.unify_or_coerce_ty_expr(
            &lvalue.ty,
            &mut rvalue.expr,
            rvalue_span,
        )?;

        Ok(CheckedExpr::new(
            ExprKind::Assign {
                lvalue: Box::new(lvalue.expr),
                rvalue: Box::new(rvalue.expr),
            },
            Ty::Unit,
            None,
            span,
        ))
    }
}

enum MutabilityCheckErr {
    ImmutableReference {
        symbol: String,
        binding_span: Option<Span>,
        ty_str: String,
    },
    ImmutableFieldAccess {
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

fn check_lvalue_is_mut(expr: &Expr, expr_span: Span) -> DiagnosticResult<()> {
    use MutabilityCheckErr::*;

    check_lvalue_mutability_internal(expr, expr_span, true).map_err(|err| {
        match err {
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
                        Label::secondary(
                            binding_span.file_id,
                            binding_span.range(),
                        )
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
            ImmutableFieldAccess {
                root_symbol,
                binding_span,
                full_path,
            } => Diagnostic::error()
                .with_message(format!(
                    "cannot assign to `{}`, as `{}` is not declared as mutable",
                    full_path, root_symbol
                ))
                .with_labels(vec![
                    Label::primary(expr_span.file_id, expr_span.range())
                        .with_message("cannot assign"),
                    Label::secondary(
                        binding_span.file_id,
                        binding_span.range(),
                    )
                    .with_message(format!(
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
                    Label::primary(expr_span.file_id, expr_span.range())
                        .with_message("cannot assign"),
                    Label::secondary(
                        binding_span.file_id,
                        binding_span.range(),
                    )
                    .with_message(format!(
                        "consider making this binding mutable: `mut {}`",
                        symbol
                    )),
                ]),
            InvalidLValue => Diagnostic::error()
                .with_message("invalid left-hand side of assignment")
                .with_labels(vec![Label::primary(
                    expr_span.file_id,
                    expr_span.range(),
                )
                .with_message("cannot assign to this expression")]),
        }
    })
}

fn check_lvalue_mutability_internal(
    expr: &Expr,
    original_expr_span: Span,
    is_direct_assign: bool,
) -> Result<(), MutabilityCheckErr> {
    use MutabilityCheckErr::*;

    match &expr.kind {
        ExprKind::Unary { op, lhs } => match op {
            UnaryOp::Deref => check_deref(lhs),
            _ => Err(InvalidLValue),
        },
        ExprKind::MemberAccess { expr, member } => {
            check_member_access(expr, *member, original_expr_span)
        }
        ExprKind::Subscript { expr, .. } => {
            check_subscript(expr, original_expr_span)
        }
        ExprKind::Id {
            symbol,
            is_mutable,
            binding_span,
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

fn check_deref(lhs: &Expr) -> Result<(), MutabilityCheckErr> {
    use MutabilityCheckErr::*;

    if let Ty::Pointer(_, is_mutable) = &lhs.ty {
        if *is_mutable {
            Ok(())
        } else {
            let MaybeSpanned { value, span } =
                lhs.display_name_and_binding_span();
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
    expr: &Expr,
    member: Ustr,
    original_expr_span: Span,
) -> Result<(), MutabilityCheckErr> {
    use MutabilityCheckErr::*;

    check_lvalue_mutability_internal(expr, original_expr_span, false).map_err(
        |err| match err {
            ImmutableFieldAccess {
                root_symbol,
                binding_span,
                full_path,
            } => ImmutableFieldAccess {
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
                ImmutableFieldAccess {
                    root_symbol: symbol,
                    binding_span,
                    full_path,
                }
            }
            InvalidLValue => err,
        },
    )
}

fn check_subscript(
    expr: &Expr,
    original_expr_span: Span,
) -> Result<(), MutabilityCheckErr> {
    use MutabilityCheckErr::*;

    match &expr.ty {
        Ty::Slice(_, is_mutable)
        | Ty::MultiPointer(_, is_mutable)
        | Ty::Pointer(_, is_mutable) => {
            return if *is_mutable {
                Ok(())
            } else {
                let MaybeSpanned { value, span } =
                    expr.display_name_and_binding_span();
                Err(ImmutableReference {
                    symbol: format!("{}[_]", value),
                    binding_span: span,
                    ty_str: expr.ty.to_string(),
                })
            };
        }
        _ => (),
    }

    check_lvalue_mutability_internal(expr, original_expr_span, false).map_err(
        |err| match err {
            ImmutableFieldAccess {
                root_symbol,
                binding_span,
                full_path,
            } => ImmutableFieldAccess {
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
        },
    )
}

fn check_id(
    symbol: Ustr,
    is_mutable: bool,
    binding_span: Span,
    ty: &Ty,
    is_direct_assign: bool,
) -> Result<(), MutabilityCheckErr> {
    use MutabilityCheckErr::*;

    if !is_direct_assign {
        match ty {
            Ty::Slice(_, is_mutable)
            | Ty::MultiPointer(_, is_mutable)
            | Ty::Pointer(_, is_mutable) => {
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
