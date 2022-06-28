use super::CheckSess;
use crate::{
    error::diagnostic::{Diagnostic, Label},
    hir,
    infer::{display::DisplayTy, normalize::Normalize},
    span::Span,
    types::Type,
    workspace::BindingId,
};

pub enum LvalueAccessErr {
    ImmutableReference { ty: Type, span: Span },
    ImmutableIdent { id: BindingId, span: Span },
    InvalidLvalue,
}

impl<'s> CheckSess<'s> {
    pub fn check_lvalue_access(&mut self, node: &hir::Node) {
        use LvalueAccessErr::*;

        let result = self
            .check_lvalue_access_inner(node)
            .map_err(|err| -> Diagnostic {
                match err {
                    ImmutableReference { ty, span } => Diagnostic::error()
                        .with_message(format!(
                            "cannot assign to the value, it is behind an immutable `{}`",
                            ty.display(&self.tycx)
                        ))
                        .with_label(Label::primary(span, "cannot assign")),
                    ImmutableIdent { id, span } => {
                        let binding_info = self.workspace.binding_infos.get(id).unwrap();

                        Diagnostic::error()
                            .with_message(format!(
                                "cannot assign to `{}`, as it is not declared as mutable",
                                binding_info.name
                            ))
                            .with_label(Label::primary(span, "cannot assignment"))
                            .with_label(Label::secondary(
                                binding_info.span,
                                format!(
                                    "consider making this binding mutable: `mut {}`",
                                    binding_info.name
                                ),
                            ))
                    }
                    InvalidLvalue => Diagnostic::error()
                        .with_message("invalid left-hand side of assignment")
                        .with_label(Label::primary(
                            node.span(),
                            "cannot assign to this expression",
                        )),
                }
            });

        if let Err(diag) = result {
            self.workspace.diagnostics.push(diag);
        }
    }

    fn check_lvalue_access_inner(&self, node: &hir::Node) -> Result<(), LvalueAccessErr> {
        use LvalueAccessErr::*;

        match node {
            hir::Node::Builtin(hir::Builtin::Deref(unary)) => {
                let ty = unary.value.ty().normalize(&self.tycx);

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
            hir::Node::Builtin(hir::Builtin::Offset(offset)) => {
                self.check_lvalue_access_inner(&offset.value)
            }
            hir::Node::MemberAccess(access) => self.check_lvalue_access_inner(&access.value),
            hir::Node::Id(id) => {
                let binding_info = self.workspace.binding_infos.get(id.id).unwrap();

                let ty = node.ty().normalize(&self.tycx);
                match ty {
                    Type::Pointer(_, is_mutable)
                    | Type::MultiPointer(_, is_mutable)
                    | Type::Slice(_, is_mutable) => {
                        if is_mutable {
                            Ok(())
                        } else {
                            Err(LvalueAccessErr::ImmutableReference {
                                ty,
                                span: node.span(),
                            })
                        }
                    }
                    _ => {
                        if binding_info.is_mutable {
                            Ok(())
                        } else {
                            Err(LvalueAccessErr::ImmutableIdent {
                                id: id.id,
                                span: node.span(),
                            })
                        }
                    }
                }
            }
            _ => Err(InvalidLvalue),
        }
    }
}
