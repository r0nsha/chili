use crate::sess::LintSess;
use chili_ast::{ast, ty::TyKind, workspace::BindingInfoId};
use chili_check::normalize::NormalizeTy;
use chili_error::DiagnosticResult;
use chili_span::{MaybeSpanned, Span};
use codespan_reporting::diagnostic::{Diagnostic, Label};
use ustr::Ustr;

pub(crate) enum LvalueAccessErr {
    ImmutableReference { ty: TyKind, span: Span },
    ImmutableMemberAccess { member: Ustr, span: Span },
    ImmutableIdent { id: BindingInfoId, span: Span },
    InvalidLvalue { span: Span },
}

impl<'s> LintSess<'s> {
    pub(crate) fn check_lvalue_access(
        &self,
        expr: &ast::Expr,
        expr_span: Span,
    ) -> DiagnosticResult<()> {
        use LvalueAccessErr::*;

        self.check_lvalue_mutability_internal(expr, expr_span, true)
            .map_err(|err| -> Diagnostic<usize> {
                match err {
                    ImmutableReference { ty, span } => {
                        todo!()
                        // let mut labels = vec![Label::primary(expr_span.file_id, expr_span.range())
                        //     .with_message("cannot assign")];

                        // if let Some(binding_span) = binding_span {
                        //     labels.push(
                        //         Label::secondary(binding_span.file_id, binding_span.range())
                        //             .with_message("consider referencing as mutable"),
                        //     );
                        // }

                        // Diagnostic::error()
                        //     .with_message(format!(
                        //         "cannot assign to `{}`, which is behind an immutable `{}`",
                        //         symbol, ty_str
                        //     ))
                        //     .with_labels(labels)
                    }
                    ImmutableMemberAccess { member, span } => {
                        todo!()
                        // Diagnostic::error()
                        // .with_message(format!(
                        //     "cannot assign to `{}`, as `{}` is not declared as mutable",
                        //     full_path, root_symbol
                        // ))
                        // .with_labels(vec![
                        //     Label::primary(expr_span.file_id, expr_span.range())
                        //         .with_message("cannot assign"),
                        //     Label::secondary(binding_span.file_id, binding_span.range()).with_message(
                        //         format!(
                        //             "consider changing this to be mutable: `mut {}`",
                        //             root_symbol
                        //         ),
                        //     ),
                        // ])
                    }
                    ImmutableIdent { id, span } => {
                        todo!()
                        // Diagnostic::error()
                        // .with_message(format!(
                        //     "cannot assign to `{}`, as it is not declared as mutable",
                        //     symbol
                        // ))
                        // .with_labels(vec![
                        //     Label::primary(expr_span.file_id, expr_span.range())
                        //         .with_message("cannot assign"),
                        //     Label::secondary(binding_span.file_id, binding_span.range()).with_message(
                        //         format!("consider making this binding mutable: `mut {}`", symbol),
                        //     ),
                        // ])
                    }
                    InvalidLvalue => Diagnostic::error()
                        .with_message("invalid left-hand side of assignment")
                        .with_labels(vec![Label::primary(expr_span.file_id, expr_span.range())
                            .with_message("cannot assign to this expression")]),
                }
            })
    }

    fn check_lvalue_mutability_internal(
        &self,
        expr: &ast::Expr,
        original_expr_span: Span,
        is_direct_assign: bool,
    ) -> Result<(), LvalueAccessErr> {
        use LvalueAccessErr::*;

        match &expr.kind {
            ast::ExprKind::Unary(unary) => match &unary.op {
                ast::UnaryOp::Deref => self.check_deref(&unary.lhs),
                _ => Err(InvalidLvalue),
            },
            ast::ExprKind::MemberAccess(access) => {
                self.check_member_access(&access.expr, access.member, original_expr_span)
            }
            ast::ExprKind::Subscript(sub) => self.check_subscript(&sub.expr, original_expr_span),
            ast::ExprKind::Ident(ident) => {
                self.check_id(ident, expr.ty.normalize(self.tycx), is_direct_assign)
            }
            _ => Err(InvalidLvalue),
        }
    }

    fn check_deref(&self, lhs: &ast::Expr) -> Result<(), LvalueAccessErr> {
        use LvalueAccessErr::*;

        if let TyKind::Pointer(_, is_mutable) = &lhs.ty.normalize(self.tycx) {
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
        &self,
        expr: &ast::Expr,
        member: Ustr,
        original_expr_span: Span,
    ) -> Result<(), LvalueAccessErr> {
        use LvalueAccessErr::*;

        self.check_lvalue_mutability_internal(expr, original_expr_span, false)
            .map_err(|err| match err {
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
                ImmutableIdent {
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
                InvalidLvalue => err,
            })
    }

    fn check_subscript(
        &self,
        expr: &ast::Expr,
        original_expr_span: Span,
    ) -> Result<(), LvalueAccessErr> {
        use LvalueAccessErr::*;

        match expr.ty.normalize(self.tycx) {
            TyKind::Slice(_, is_mutable)
            | TyKind::MultiPointer(_, is_mutable)
            | TyKind::Pointer(_, is_mutable) => {
                return if is_mutable {
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

        self.check_lvalue_mutability_internal(expr, original_expr_span, false)
            .map_err(|err| match err {
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
                ImmutableIdent {
                    symbol,
                    binding_span,
                } => ImmutableIdent {
                    symbol: format!("{}[_]", symbol),
                    binding_span,
                },
                InvalidLvalue => err,
            })
    }

    fn check_id(
        &self,
        ident: &ast::Ident,
        kind: TyKind,
        is_direct_assign: bool,
    ) -> Result<(), LvalueAccessErr> {
        use LvalueAccessErr::*;

        let binding_info = self
            .workspace
            .get_binding_info(ident.binding_info_id)
            .unwrap();

        if !is_direct_assign {
            match kind {
                TyKind::Slice(_, is_mutable)
                | TyKind::MultiPointer(_, is_mutable)
                | TyKind::Pointer(_, is_mutable) => {
                    return if is_mutable {
                        Ok(())
                    } else {
                        Err(ImmutableReference {
                            symbol: ident.symbol.to_string(),
                            binding_span: Some(binding_info.binding_span),
                            ty_str: kind.to_string(),
                        })
                    }
                }
                _ => (),
            }
        }

        if is_mutable {
            Ok(())
        } else {
            Err(ImmutableIdent {
                symbol: symbol.to_string(),
                binding_span: binding_span,
            })
        }
    }
}
