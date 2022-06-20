use super::sess::{InitState, LintSess};
use crate::ast::{ast, workspace::BindingId};
use crate::error::diagnostic::{Diagnostic, Label};
use crate::span::Span;

impl<'s> LintSess<'s> {
    pub fn check_id_access(&mut self, binding_id: BindingId, span: Span) {
        if let Some((_, state)) = self.init_scopes.get(binding_id) {
            if state.is_not_init() {
                let binding_info = self.workspace.get_binding_info(binding_id).unwrap();

                if binding_info.kind.is_extern() {
                    return;
                }

                let msg = format!(
                    "use of possibly uninitialized value `{}`",
                    binding_info.symbol
                );

                self.workspace.diagnostics.push(
                    Diagnostic::error()
                        .with_message(msg.clone())
                        .with_label(Label::primary(span, msg))
                        .with_label(Label::secondary(binding_info.span, "defined here")),
                );
            }
        }
    }

    pub fn check_assign_lvalue_id_access(&mut self, lvalue: &ast::Expr, binding_id: BindingId) {
        let binding_info = self.workspace.get_binding_info(binding_id).unwrap();
        let init_state = self.init_scopes.value(binding_id).unwrap();

        if init_state.is_init() && !binding_info.is_mutable {
            let msg = format!(
                "cannot assignment twice to immutable variable `{}`",
                binding_info.symbol
            );
            let binding_span = binding_info.span;

            self.workspace.diagnostics.push(
                Diagnostic::error()
                    .with_message(msg.clone())
                    .with_label(Label::primary(lvalue.span, msg))
                    .with_label(Label::secondary(binding_span, "defined here")),
            );
        }

        if init_state.is_init() {
            self.check_lvalue_access(lvalue, lvalue.span);
        } else {
            // set binding as init in the current scope
            *self.init_scopes.get_mut(binding_id).unwrap().1 = InitState::Init;
        }
    }
}
