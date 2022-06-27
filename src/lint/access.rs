use super::LintSess;
use crate::{
    error::diagnostic::{Diagnostic, Label},
    hir,
    workspace::BindingId,
};

impl<'s> LintSess<'s> {
    pub fn check_assign_lvalue_id_access(&mut self, lhs: &hir::Node, binding_id: BindingId) {
        let binding_info = self.workspace.binding_infos.get(binding_id).unwrap();

        if !binding_info.is_mutable {
            let msg = format!(
                "cannot assignment twice to immutable variable `{}`",
                binding_info.name
            );
            let binding_span = binding_info.span;

            self.workspace.diagnostics.push(
                Diagnostic::error()
                    .with_message(msg.clone())
                    .with_label(Label::primary(lhs.span(), msg))
                    .with_label(Label::secondary(binding_span, "defined here")),
            );
        }

        self.check_lvalue_access(lhs, lhs.span());
    }
}
