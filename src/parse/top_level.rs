use super::*;
use crate::{
    ast,
    error::{diagnostic::Label, SyntaxError},
    span::FileId,
};

impl Parser {
    pub fn parse_module(&mut self, file_id: FileId) -> ParserResult {
        let mut module = ast::Module::new(file_id, self.module_info);

        while !self.eof() {
            if let Err(diag) = self.parse_top_level(&mut module) {
                self.cache.lock().diagnostics.push(diag);
                return ParserResult::ParserFailed;
                // TODO: Recovery
                // self.skip_until_recovery_point();
            }
        }

        ParserResult::NewModule(module)
    }

    pub fn parse_top_level(&mut self, module: &mut ast::Module) -> DiagnosticResult<()> {
        let attrs = self.parse_attrs()?;
        let has_attrs = !attrs.is_empty();

        let visibility = if eat!(self, Pub) {
            ast::Visibility::Public
        } else {
            ast::Visibility::Private
        };

        match self.try_parse_any_binding(attrs, visibility, true)? {
            Some(binding) => {
                module.bindings.push(binding?);
                Ok(())
            }
            None => {
                if !has_attrs {
                    if is!(self, Static) {
                        let static_eval = self.parse_static_eval()?;
                        module.static_blocks.push(static_eval);
                        Ok(())
                    } else if eat!(self, Semicolon | Newline) {
                        // Ignore
                        Ok(())
                    } else {
                        Err(SyntaxError::expected(
                            self.span(),
                            &format!("an item, got `{}`", self.peek().kind.lexeme()),
                        ))
                    }
                } else {
                    Err(Diagnostic::error()
                        .with_message(format!("expected a binding, got `{}`", self.peek().kind.lexeme()))
                        .with_label(Label::primary(self.span(), "unexpected token")))
                }
            }
        }
    }
}
