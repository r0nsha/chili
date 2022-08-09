use super::*;
use crate::{
    error::{
        diagnostic::{Diagnostic, Label},
        DiagnosticResult, SyntaxError,
    },
    workspace::library::LIB_NAME_STD,
};

impl Parser {
    pub fn parse_import(&mut self) -> DiagnosticResult<ast::Ast> {
        let start_span = self.previous_span();

        let module_path = self.parse_import_path()?;
        let absolute_path = module_path.path();

        spawn_parser(
            self.thread_pool.clone(),
            self.tx.clone(),
            Arc::clone(&self.cache),
            module_path,
        );

        Ok(ast::Ast::Import(ast::Import {
            path: absolute_path,
            span: start_span.to(self.previous_span()),
        }))
    }

    fn parse_import_path(&mut self) -> DiagnosticResult<ModulePath> {
        let start_span = self.span();

        let mut module_path = self.parse_import_path_root()?;

        if !is!(self, FwSlash) {
            return Err(SyntaxError::expected(self.span(), "/"));
        }

        while eat!(self, FwSlash) {
            self.parse_import_path_component(&mut module_path)?;
        }

        let path = module_path.path();

        if path.exists() {
            Ok(module_path)
        } else {
            Err(Diagnostic::error()
                .with_message(format!("module `{}` doesn't exist", module_path.name()))
                .with_label(Label::primary(start_span.to(self.previous_span()), "doesn't exist"))
                .with_note(format!("the module's path is: {}", path.display())))
        }
    }

    fn parse_import_path_root(&mut self) -> DiagnosticResult<ModulePath> {
        let token = self.bump().clone();

        let cache = self.cache.lock();

        match &token.kind {
            // Lib & current dir
            Dot => {
                let library = self.module_path.library().clone();
                let mut components = self.module_path.components().to_vec();

                if let None = components.pop() {
                    Err(Parser::outside_of_library_root_err(&library, token.span))
                } else {
                    Ok(ModulePath::new(library, components))
                }
            }
            // Lib & parent dir
            DotDot => {
                let library = self.module_path.library().clone();
                let mut components = self.module_path.components().to_vec();

                if components.pop().and_then(|_| components.pop()).is_none() {
                    Err(Parser::outside_of_library_root_err(&library, token.span))
                } else {
                    Ok(ModulePath::new(library, components))
                }
            }
            Ident(ident) => match ident.as_str() {
                // Lib & Lib root dir
                "lib" => Ok(ModulePath::new(self.module_path.library().clone(), vec![])),
                // Std & Std root dir
                LIB_NAME_STD => Ok(ModulePath::new(cache.std_library.clone(), vec![])),
                _ => Err(SyntaxError::expected(token.span, "lib or std")),
            },
            _ => Err(SyntaxError::expected(token.span, "one of: ., .., lib or std")),
        }
    }

    fn parse_import_path_component(&mut self, module_path: &mut ModulePath) -> DiagnosticResult<()> {
        let token = self.bump().clone();

        match &token.kind {
            // Lib & parent dir
            DotDot => {
                if module_path.pop().is_some() {
                    Ok(())
                } else {
                    Err(Parser::outside_of_library_root_err(module_path.library(), token.span))
                }
            }
            Ident(ident) => {
                module_path.push(*ident);
                Ok(())
            }
            _ => Err(SyntaxError::expected(token.span, "an identifier or ..")),
        }
    }

    fn outside_of_library_root_err(library: &Library, span: Span) -> Diagnostic {
        Diagnostic::error()
            .with_message("cannot import modules outside of this library's root")
            .with_label(Label::primary(span, "module path is outside of library root"))
            .with_note(format!("library root is: {}", library.root_dir().display()))
    }
}
