use super::*;
use crate::{
    error::{
        diagnostic::{Diagnostic, Label},
        DiagnosticResult, SyntaxError,
    },
    sym,
};

impl Parser {
    pub fn parse_import(&mut self) -> DiagnosticResult<ast::Ast> {
        let start_span = require!(self, Use, "use")?.span;

        require!(self, OpenParen, "(")?;

        let id_token = self.require_ident()?;
        let name = id_token.name();

        require!(self, CloseParen, ")")?;

        let span = start_span.to(self.previous_span());

        self.search_import_name(name, span)
    }

    pub fn search_import_name(&self, name: Ustr, span: Span) -> DiagnosticResult<ast::Ast> {
        let mut search_notes = vec![];

        match name.as_str() {
            // Allow using self/super in imports
            sym::SELF | sym::SUPER => Ok(ast::Ast::Ident(ast::Ident { name, span })),
            _ => {
                // Search for {module}/foo.chl
                match self.search_for_child_module(name) {
                    Ok(module_path) => self.finish_parse_import(module_path, span),
                    Err(path) => {
                        search_notes.push(format!("searched path: {}", path.display()));

                        // Search for ./foo.chl
                        match self.search_for_neighbor_module(name) {
                            Ok(module_path) => self.finish_parse_import(module_path, span),
                            Err(path) => {
                                search_notes.push(format!("searched path: {}", path.display()));

                                // Search for a library named `foo`
                                match self.cache.lock().libraries.get(&name) {
                                    Some(library) => {
                                        let module_path =
                                            ModulePath::new(library.clone(), vec![ustr(library.root_file_stem())]);

                                        self.finish_parse_import(module_path, span)
                                    }
                                    None => {
                                        search_notes.push(format!("searched for a library named `{}`", name));

                                        Err(Diagnostic::error()
                                            .with_message(format!("could not find module or library `{}`", name))
                                            .with_label(Label::primary(span, "undefined module or library"))
                                            .with_notes(&search_notes))
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }
    }

    fn finish_parse_import(&self, module_path: ModulePath, span: Span) -> DiagnosticResult<ast::Ast> {
        let path = module_path.path();

        spawn_parser(
            self.thread_pool.clone(),
            self.tx.clone(),
            Arc::clone(&self.cache),
            module_path,
            Some(self.module_info.id),
        );

        Ok(ast::Ast::Import(ast::Import { path, span }))
    }

    fn search_for_child_module(&self, name: Ustr) -> Result<ModulePath, PathBuf> {
        let mut module_path = self.module_path.clone();
        module_path.push(name);

        let path = module_path.path();

        if path.exists() {
            Ok(module_path)
        } else {
            Err(path)
        }
    }

    fn search_for_neighbor_module(&self, name: Ustr) -> Result<ModulePath, PathBuf> {
        let mut module_path = self.module_path.clone();
        module_path.pop();
        module_path.push(name);

        let path = module_path.path();

        if path.exists() {
            Ok(module_path)
        } else {
            Err(path)
        }
    }
}
