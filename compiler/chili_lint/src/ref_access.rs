use crate::sess::LintSess;
use chili_ast::{
    ast,
    ty::TyKind,
    workspace::{BindingInfo, BindingInfoId, ModuleId},
};
use chili_check::{display::DisplayTy, normalize::NormalizeTy};
use chili_error::diagnostic::{Diagnostic, Label};
use chili_span::Span;
use ustr::Ustr;

enum RefAccessErr {
    ImmutableReference { ty: TyKind, span: Span },
    ImmutableBinding { id: BindingInfoId, span: Span },
}

impl<'s> LintSess<'s> {
    pub(super) fn check_expr_can_be_mutably_referenced(&mut self, expr: &ast::Expr) {
        use RefAccessErr::*;

        let result = self
            .check_expr_can_be_mutably_referenced_inner(expr, true)
            .map_err(|err| match err {
                ImmutableReference { ty, span } => Diagnostic::error()
                    .with_message(format!(
                        "cannot reference value, because it is behind an immutable `{}`",
                        ty.display(self.tycx)
                    ))
                    .with_label(Label::primary(span, "cannot reference")),
                ImmutableBinding { id, span } => {
                    let binding_info = self.workspace.get_binding_info(id).unwrap();

                    Diagnostic::error()
                        .with_message(format!(
                            "cannot reference `{}` as mutable, as it is not declared as mutable",
                            binding_info.symbol
                        ))
                        .with_label(Label::primary(span, "cannot reference immutable variable"))
                        .with_label(Label::secondary(
                            binding_info.span,
                            format!(
                                "consider changing this to be mutable: `mut {}`",
                                binding_info.symbol
                            ),
                        ))
                }
            });

        if let Err(diag) = result {
            self.workspace.diagnostics.add(diag);
        }
    }

    fn check_expr_can_be_mutably_referenced_inner(
        &self,
        expr: &ast::Expr,
        is_direct_ref: bool,
    ) -> Result<(), RefAccessErr> {
        use RefAccessErr::*;

        let ty = expr.ty.normalize(self.tycx);

        match &expr.kind {
            ast::ExprKind::MemberAccess(access) => {
                match self.check_expr_can_be_mutably_referenced_inner(expr, true) {
                    Ok(_) => match ty {
                        TyKind::Tuple(tys) => {
                            let index = access.member.parse::<usize>().unwrap();
                            let ty = tys[index].normalize(self.tycx);

                            match ty {
                                TyKind::Slice(_, is_mutable)
                                | TyKind::MultiPointer(_, is_mutable)
                                | TyKind::Pointer(_, is_mutable)
                                    if !is_mutable =>
                                {
                                    Err(ImmutableReference {
                                        ty,
                                        span: expr.span,
                                    })
                                }
                                _ => Ok(()),
                            }
                        }
                        TyKind::Struct(struct_ty) => {
                            let ty = struct_ty
                                .fields
                                .iter()
                                .find(|f| f.symbol == access.member)
                                .map(|f| f.ty.normalize(self.tycx))
                                .unwrap();

                            match ty {
                                TyKind::Slice(_, is_mutable)
                                | TyKind::MultiPointer(_, is_mutable)
                                | TyKind::Pointer(_, is_mutable)
                                    if !is_mutable =>
                                {
                                    Err(ImmutableReference {
                                        ty,
                                        span: expr.span,
                                    })
                                }
                                _ => Ok(()),
                            }
                        }
                        TyKind::Module(module_id) => {
                            let binding_info =
                                self.find_binding_info_in_module(module_id, access.member);
                            let ty = binding_info.ty.normalize(self.tycx);

                            match ty {
                                TyKind::Slice(_, is_mutable)
                                | TyKind::MultiPointer(_, is_mutable)
                                | TyKind::Pointer(_, is_mutable)
                                    if !is_mutable =>
                                {
                                    Err(ImmutableReference {
                                        ty,
                                        span: expr.span,
                                    })
                                }
                                _ => {
                                    if binding_info.is_mutable {
                                        Ok(())
                                    } else {
                                        Err(ImmutableBinding {
                                            id: binding_info.id,
                                            span: expr.span,
                                        })
                                    }
                                }
                            }
                        }
                        _ => Ok(()),
                    },
                    Err(err) => Err(err),
                }
            }
            ast::ExprKind::Ident(ident) => {
                match ty {
                    TyKind::Slice(_, is_mutable)
                    | TyKind::MultiPointer(_, is_mutable)
                    | TyKind::Pointer(_, is_mutable) => {
                        if is_mutable && is_direct_ref {
                            return Ok(());
                        } else {
                            return Err(ImmutableReference {
                                ty,
                                span: expr.span,
                            });
                        }
                    }
                    _ => (),
                }

                let binding_info = self
                    .workspace
                    .get_binding_info(ident.binding_info_id)
                    .unwrap();

                if binding_info.is_mutable {
                    Ok(())
                } else {
                    Err(ImmutableBinding {
                        id: ident.binding_info_id,
                        span: expr.span,
                    })
                }
            }
            _ => Ok(()),
        }
    }

    fn find_binding_info_in_module(&self, module_id: ModuleId, symbol: Ustr) -> &BindingInfo {
        self.workspace
            .binding_infos
            .iter()
            .find(|b| b.module_id == module_id && b.symbol == symbol)
            .unwrap()
    }
}
