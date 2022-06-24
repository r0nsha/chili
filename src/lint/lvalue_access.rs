use super::sess::LintSess;
use crate::ast::{self, ty::Type, workspace::BindingId};
use crate::error::diagnostic::{Diagnostic, Label};
use crate::infer::{display::DisplayTy, normalize::Normalize};
use crate::span::Span;

pub enum LvalueAccessErr {
    ImmutableReference { ty: Type, span: Span },
    ImmutableIdent { id: BindingId, span: Span },
    InvalidLvalue,
}

impl<'s> LintSess<'s> {
    pub fn check_lvalue_access(&mut self, expr: &ast::Ast, expr_span: Span) {
        use LvalueAccessErr::*;

        let result = self
            .check_lvalue_mutability_inner(expr)
            .map_err(|err| -> Diagnostic {
                match err {
                    ImmutableReference { ty, span } => Diagnostic::error()
                        .with_message(format!(
                            "cannot assignment to the value, it is behind an immutable `{}`",
                            ty.display(self.tycx)
                        ))
                        .with_label(Label::primary(span, "cannot assignment")),
                    ImmutableIdent { id, span } => {
                        let binding_info = self.workspace.binding_infos.get(id).unwrap();

                        Diagnostic::error()
                            .with_message(format!(
                                "cannot assignment to `{}`, as it is not declared as mutable",
                                binding_info.symbol
                            ))
                            .with_label(Label::primary(span, "cannot assignment"))
                            .with_label(Label::secondary(
                                binding_info.span,
                                format!(
                                    "consider making this binding mutable: `mut {}`",
                                    binding_info.symbol
                                ),
                            ))
                    }
                    InvalidLvalue => Diagnostic::error()
                        .with_message("invalid left-hand side of assignment")
                        .with_label(Label::primary(
                            expr_span,
                            "cannot assignment to this expression",
                        )),
                }
            });

        if let Err(diag) = result {
            self.workspace.diagnostics.push(diag);
        }
    }

    fn check_lvalue_mutability_inner(&self, expr: &ast::Ast) -> Result<(), LvalueAccessErr> {
        use LvalueAccessErr::*;

        match expr {
            ast::Ast::Unary(unary) => match &unary.op {
                ast::UnaryOp::Deref => {
                    let ty = unary.value.ty().normalize(self.tycx);

                    if let Type::Pointer(_, is_mutable) = ty {
                        if is_mutable {
                            Ok(())
                        } else {
                            Err(ImmutableReference {
                                ty,
                                span: unary.value.span(),
                            })
                        }
                    } else {
                        unreachable!("got {}", unary.value.ty())
                    }
                }
                _ => Err(InvalidLvalue),
            },
            ast::Ast::MemberAccess(access) => self.check_lvalue_mutability_inner(&access.expr),
            ast::Ast::Subscript(sub) => self.check_lvalue_mutability_inner(&sub.expr),
            ast::Ast::Ident(ident) => {
                let binding_info = self.workspace.binding_infos.get(ident.binding_id).unwrap();

                let ty = expr.ty().normalize(self.tycx);
                match ty {
                    Type::Pointer(_, is_mutable)
                    | Type::MultiPointer(_, is_mutable)
                    | Type::Slice(_, is_mutable) => {
                        if is_mutable {
                            Ok(())
                        } else {
                            Err(LvalueAccessErr::ImmutableReference {
                                ty,
                                span: expr.span(),
                            })
                        }
                    }
                    _ => {
                        if binding_info.is_mutable {
                            Ok(())
                        } else {
                            Err(LvalueAccessErr::ImmutableIdent {
                                id: ident.binding_id,
                                span: expr.span(),
                            })
                        }
                    }
                }
            }
            _ => Err(InvalidLvalue),
        }
    }
}
