use super::sess::LintSess;
use crate::ast::{
    self,
    ty::Type,
    workspace::{BindingId, BindingInfo, ModuleId},
};
use crate::error::diagnostic::{Diagnostic, Label};
use crate::infer::{display::DisplayTy, normalize::Normalize};
use crate::span::Span;
use ustr::Ustr;

enum RefAccessErr {
    ImmutableReference { ty: Type, span: Span },
    ImmutableBinding { id: BindingId, span: Span },
}

impl<'s> LintSess<'s> {
    pub fn check_expr_can_be_mutably_referenced(&mut self, expr: &ast::Ast) {
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
                    let binding_info = self.workspace.binding_infos.get(id).unwrap();

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
            self.workspace.diagnostics.push(diag);
        }
    }

    fn check_expr_can_be_mutably_referenced_inner(
        &self,
        expr: &ast::Ast,
        is_direct_ref: bool,
    ) -> Result<(), RefAccessErr> {
        use RefAccessErr::*;

        let ty = expr.ty().normalize(self.tycx);

        match expr {
            ast::Ast::MemberAccess(access) => {
                match self.check_expr_can_be_mutably_referenced_inner(expr, true) {
                    Ok(_) => match ty {
                        Type::Tuple(tys) => {
                            let index = access.member.parse::<usize>().unwrap();
                            let ty = tys[index].normalize(self.tycx);

                            match ty {
                                Type::Slice(_, is_mutable)
                                | Type::MultiPointer(_, is_mutable)
                                | Type::Pointer(_, is_mutable)
                                    if !is_mutable =>
                                {
                                    Err(ImmutableReference {
                                        ty,
                                        span: expr.span(),
                                    })
                                }
                                _ => Ok(()),
                            }
                        }
                        Type::Struct(struct_ty) => {
                            let ty = struct_ty
                                .fields
                                .iter()
                                .find(|f| f.symbol == access.member)
                                .map(|f| f.ty.normalize(self.tycx))
                                .unwrap();

                            match ty {
                                Type::Slice(_, is_mutable)
                                | Type::MultiPointer(_, is_mutable)
                                | Type::Pointer(_, is_mutable)
                                    if !is_mutable =>
                                {
                                    Err(ImmutableReference {
                                        ty,
                                        span: expr.span(),
                                    })
                                }
                                _ => Ok(()),
                            }
                        }
                        Type::Module(module_id) => {
                            let binding_info =
                                self.find_binding_info_in_module(module_id, access.member);
                            let ty = binding_info.ty.normalize(self.tycx);

                            match ty {
                                Type::Slice(_, is_mutable)
                                | Type::MultiPointer(_, is_mutable)
                                | Type::Pointer(_, is_mutable)
                                    if !is_mutable =>
                                {
                                    Err(ImmutableReference {
                                        ty,
                                        span: expr.span(),
                                    })
                                }
                                _ => {
                                    if binding_info.is_mutable {
                                        Ok(())
                                    } else {
                                        Err(ImmutableBinding {
                                            id: binding_info.id,
                                            span: expr.span(),
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
            ast::Ast::Ident(ident) => {
                match ty {
                    Type::Slice(_, is_mutable)
                    | Type::MultiPointer(_, is_mutable)
                    | Type::Pointer(_, is_mutable) => {
                        if is_mutable && is_direct_ref {
                            return Ok(());
                        } else {
                            return Err(ImmutableReference {
                                ty,
                                span: expr.span(),
                            });
                        }
                    }
                    _ => (),
                }

                let binding_info = self.workspace.binding_infos.get(ident.binding_id).unwrap();

                if binding_info.is_mutable {
                    Ok(())
                } else {
                    Err(ImmutableBinding {
                        id: ident.binding_id,
                        span: expr.span(),
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
            .map(|(_, b)| b)
            .find(|b| b.module_id == module_id && b.symbol == symbol)
            .unwrap()
    }
}
