use super::LintSess;
use crate::{
    error::diagnostic::{Diagnostic, Label},
    hir,
    infer::{display::DisplayType, normalize::Normalize},
    span::Span,
    types::Type,
    workspace::{BindingId, BindingInfo, ModuleId},
};
use ustr::Ustr;

enum RefAccessErr {
    ImmutableReference { ty: Type, span: Span },
    ImmutableBinding { id: BindingId, span: Span },
}

impl<'s> LintSess<'s> {
    pub fn check_node_can_be_mutably_referenced(&mut self, node: &hir::Node) {
        use RefAccessErr::*;

        let result = self
            .check_node_can_be_mutably_referenced_inner(node, true)
            .map_err(|err| match err {
                ImmutableReference { ty, span } => Diagnostic::error()
                    .with_message(format!(
                        "cannot reference value, because it is behind an immutable `{}`",
                        ty.display(self.tcx)
                    ))
                    .with_label(Label::primary(span, "cannot reference")),
                ImmutableBinding { id, span } => {
                    let binding_info = self.workspace.binding_infos.get(id).unwrap();

                    Diagnostic::error()
                        .with_message(format!(
                            "cannot reference `{}` as mutable, as it is not declared as mutable",
                            binding_info.name
                        ))
                        .with_label(Label::primary(span, "cannot reference immutable variable"))
                        .with_label(Label::secondary(
                            binding_info.span,
                            format!("consider changing this to be mutable: `mut {}`", binding_info.name),
                        ))
                }
            });

        if let Err(diag) = result {
            self.workspace.diagnostics.push(diag);
        }
    }

    fn check_node_can_be_mutably_referenced_inner(
        &self,
        node: &hir::Node,
        is_direct_ref: bool,
    ) -> Result<(), RefAccessErr> {
        use RefAccessErr::*;

        let ty = node.ty().normalize(self.tcx);

        match node {
            hir::Node::MemberAccess(access) => {
                match self.check_node_can_be_mutably_referenced_inner(&access.value, true) {
                    Ok(_) => match ty {
                        Type::Tuple(tys) => {
                            let index = access.member_name.parse::<usize>().unwrap();
                            let ty = tys[index].normalize(self.tcx);

                            match ty {
                                Type::Pointer(_, false) => Err(ImmutableReference { ty, span: node.span() }),
                                _ => Ok(()),
                            }
                        }
                        Type::Struct(struct_ty) => {
                            let ty = struct_ty
                                .fields
                                .iter()
                                .find(|f| f.name == access.member_name)
                                .map(|f| f.ty.normalize(self.tcx))
                                .unwrap();

                            match ty {
                                Type::Pointer(_, false) => Err(ImmutableReference { ty, span: node.span() }),
                                _ => Ok(()),
                            }
                        }
                        Type::Module(module_id) => {
                            let binding_info = self.find_binding_info_in_module(module_id, access.member_name);
                            let ty = binding_info.ty.normalize(self.tcx);

                            match ty {
                                Type::Pointer(_, false) => Err(ImmutableReference { ty, span: node.span() }),
                                _ => {
                                    if binding_info.is_mutable {
                                        Ok(())
                                    } else {
                                        Err(ImmutableBinding {
                                            id: binding_info.id,
                                            span: node.span(),
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
            hir::Node::Id(id) => {
                if let Type::Pointer(_, is_mutable) = ty {
                    if is_mutable && is_direct_ref {
                        return Ok(());
                    } else {
                        return Err(ImmutableReference { ty, span: node.span() });
                    }
                }

                let binding_info = self.workspace.binding_infos.get(id.id).unwrap();

                if binding_info.is_mutable {
                    Ok(())
                } else {
                    Err(ImmutableBinding {
                        id: id.id,
                        span: node.span(),
                    })
                }
            }
            _ => Ok(()),
        }
    }

    fn find_binding_info_in_module(&self, module_id: ModuleId, name: Ustr) -> &BindingInfo {
        self.workspace
            .binding_infos
            .iter()
            .map(|(_, b)| b)
            .find(|b| b.module_id == module_id && b.name == name)
            .unwrap()
    }
}
