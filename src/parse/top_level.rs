use super::*;
use crate::{
    ast,
    error::{diagnostic::Label, SyntaxError},
    span::FileId,
};

impl Parser {
    pub fn parse_all_top_level(&mut self, file_id: FileId) -> ParserResult {
        let mut module = ast::Module::new(file_id, self.module_info);

        while !self.is_end() {
            match self.parse_top_level(&mut module) {
                Ok(_) => {
                    // Note (Ron 20/07/2022):
                    // This piece of code requires semicolons for top level items.
                    // This is not semantically required, but is placed for orthogonallity.
                    // I did experiment with optional semicolon, but they ended up
                    // add much more complexity then benefit.
                    // Especially since the language is expression-based.
                    if let Err(_) = require!(self, Semicolon, ";") {
                        let span = Parser::get_missing_delimiter_span(self.previous_span());
                        self.cache.lock().diagnostics.push(SyntaxError::expected(span, ";"));
                        self.skip_until_recovery_point();
                    }
                }
                Err(diag) => {
                    self.cache.lock().diagnostics.push(diag);
                    self.skip_until_recovery_point();
                }
            }

            self.skip_semicolons();
        }

        ParserResult::NewModule(module)
    }

    pub fn parse_top_level(&mut self, module: &mut ast::Module) -> DiagnosticResult<()> {
        let attrs = if is!(self, At) { self.parse_attrs()? } else { vec![] };

        let has_attrs = !attrs.is_empty();

        let visibility = if eat!(self, Pub) {
            ast::Visibility::Public
        } else {
            ast::Visibility::Private
        };

        match self.try_parse_any_binding(attrs, visibility)? {
            Some(binding) => {
                module.bindings.push(binding?);
                Ok(())
            }
            None => {
                if !has_attrs {
                    if is!(self, Static) {
                        let static_eval = self.parse_static_eval()?;
                        module.consts.push(static_eval);
                        Ok(())
                    } else {
                        Err(SyntaxError::expected(
                            self.span(),
                            &format!("an item, got `{}`", self.peek().lexeme),
                        ))
                    }
                } else {
                    Err(Diagnostic::error()
                        .with_message(format!("expected a binding, got `{}`", self.peek().lexeme))
                        .with_label(Label::primary(self.span(), "unexpected token")))
                }
            }
        }
    }
}
