use super::*;
use crate::{
    error::{
        diagnostic::{Diagnostic, Label},
        DiagnosticResult, SyntaxError,
    },
    workspace::{LibraryId, ModuleId},
};

impl Parser {
    pub fn parse_import(&mut self) -> DiagnosticResult<ast::Ast> {
        let start_span = require!(self, Import, "import")?.span;

        require!(self, OpenParen, "(")?;

        let id_token = require!(self, Ident(_), "an identifier")?;
        let name = id_token.name();

        require!(self, CloseParen, ")")?;

        let cache = self.cache.lock();

        // If a directory with the same name as the current module already exists - Look for the module in that directory.
        dbg!(self.module_path.path(), self.module_path.path().parent().unwrap());
        let nested_module_path = self.module_path.path();

        // TODO: Module (file) in the same directory

        // TODO: Look for a library with the same name.

        // TODO: Error :cannot find module `bar`
        // note: searched path: ./foo/bar.chl
        // note: searched path: ./bar.chl
        // note: searched for a library named `bar`
        // if let Some(library) = cache.libraries.get(&name) {
        //     Ok(ModulePath::new(library.clone(), vec![]))
        // } else {
        //     let library = self.module_path.library().clone();
        //     let mut components = self.module_path.components().to_vec();

        //     if components.pop().is_some() {
        //         components.push(name);
        //         Ok(ModulePath::new(library, components))
        //     } else {
        //         Err(Parser::outside_of_library_root_err(&library, id_token.span))
        //     }
        // }

        // let absolute_path = module_path.path();

        // spawn_parser(
        //     self.thread_pool.clone(),
        //     self.tx.clone(),
        //     Arc::clone(&self.cache),
        //     module_path,
        // );

        // Ok(ast::Ast::Import(ast::Import {
        //     path: absolute_path,
        //     span: start_span.to(self.previous_span()),
        // }))

        todo!();
    }

    // TODO: remove
    // fn parse_import_path(&mut self) -> DiagnosticResult<ModulePath> {
    //     let start_span = self.span();

    //     let mut module_path = self.parse_import_path_root()?;

    //     if is!(self, FwSlash) {
    //         while eat!(self, FwSlash) {
    //             self.parse_import_path_component(&mut module_path)?;
    //         }

    //         let path = module_path.path();

    //         if path.exists() {
    //             Ok(module_path)
    //         } else {
    //             if module_path.library().is_main {
    //                 let cache = self.cache.lock();

    //                 // TODO: This is a HACK, and should be removed!
    //                 // TODO: This is used for LSP integration, because it creates temp files that could be outside this library's root
    //                 for include_path in cache.include_paths.iter() {
    //                     let path = ModulePath::build_path(include_path, module_path.components());

    //                     if path.exists() {
    //                         let tmp_library = Library {
    //                             id: LibraryId::unknown(),
    //                             name: ustr("tmp"),
    //                             root_file: path,
    //                             root_module_id: ModuleId::unknown(),
    //                             is_main: false,
    //                         };

    //                         return Ok(tmp_library.as_module_path());
    //                     }
    //                 }
    //             }

    //             Err(Diagnostic::error()
    //                 .with_message(format!("module `{}` doesn't exist", module_path.name()))
    //                 .with_label(Label::primary(start_span.to(self.previous_span()), "doesn't exist"))
    //                 .with_note(format!("the module's path is: {}", path.display())))
    //         }
    //     } else {
    //         if module_path.components().is_empty() {
    //             // This is a library dir, so add its root file as a component
    //             module_path.push(ustr(module_path.library().root_file_name()));
    //         }

    //         Ok(module_path)
    //     }
    // }

    // fn parse_import_path_root(&mut self) -> DiagnosticResult<ModulePath> {
    //     let token = require!(self, Ident(_), "an identifier")?;
    //     let ident = token.name();

    //     let cache = self.cache.lock();

    //     if let Some(library) = cache.libraries.get(&ident) {
    //         Ok(ModulePath::new(library.clone(), vec![]))
    //     } else {
    //         let library = self.module_path.library().clone();
    //         let mut components = self.module_path.components().to_vec();

    //         if components.pop().is_some() {
    //             components.push(ident);
    //             Ok(ModulePath::new(library, components))
    //         } else {
    //             Err(Parser::outside_of_library_root_err(&library, token.span))
    //         }
    //     }
    // }

    // fn parse_import_path_component(&mut self, module_path: &mut ModulePath) -> DiagnosticResult<()> {
    //     let token = self.bump().clone();

    //     match &token.kind {
    //         // Lib & parent dir
    //         DotDotDot => {
    //             if module_path.pop().is_some() {
    //                 Ok(())
    //             } else {
    //                 Err(Parser::outside_of_library_root_err(module_path.library(), token.span))
    //             }
    //         }
    //         Ident(ident) => {
    //             module_path.push(*ident);
    //             Ok(())
    //         }
    //         _ => Err(SyntaxError::expected(token.span, "an identifier or ...")),
    //     }
    // }

    fn outside_of_library_root_err(library: &Library, span: Span) -> Diagnostic {
        Diagnostic::error()
            .with_message("cannot import modules outside of this library's root")
            .with_label(Label::primary(span, "module path is outside of library root"))
            .with_note(format!("library root is: {}", library.root_dir().display()))
    }
}
