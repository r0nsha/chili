use super::*;
use crate::{
    common::path::{maybe_resolve_relative_path, try_resolve_relative_path, RelativeTo},
    error::{
        diagnostic::{Diagnostic, Label},
        DiagnosticResult, SyntaxError,
    },
    workspace::library::LIB_NAME_STD,
};
use std::path::Path;
use ustr::ustr;

const STD_PREFIX_FW: &str = "std/";
const STD_PREFIX_BK: &str = "std\\";
const SOURCE_FILE_EXT: &str = "chl";

impl Parser {
    pub fn parse_import(&mut self) -> DiagnosticResult<ast::Ast> {
        let start_span = self.previous_span();

        require!(self, OpenParen, "(")?;

        let path_token = require!(self, Str(_), "string")?;
        let path = path_token.name().as_str();

        require!(self, CloseParen, ")")?;

        let cache = self.cache.lock();

        let absolute_import_path = if path == "~" {
            // import root file
            // example: import("~")
            cache.root_file.clone()
        } else if path.starts_with("~") {
            // import relative to the root dir
            // example: import("~/foo")
            let import_path = cache
                .root_dir
                .join(PathBuf::from(path.trim_start_matches("~/")))
                .with_extension(SOURCE_FILE_EXT);

            let import_path = try_resolve_relative_path(&import_path, &RelativeTo::Cwd, Some(path_token.span))?;

            self.check_import_path_is_under_root(&cache, &import_path, path_token.span)?;

            import_path
        } else if path == LIB_NAME_STD {
            // import std root file
            // example: import("std")
            try_resolve_relative_path(&cache.std_library.root_file, &RelativeTo::Cwd, Some(path_token.span))?
        } else if path.starts_with(STD_PREFIX_FW) || path.starts_with(STD_PREFIX_BK) {
            // import relative to std root dir
            // example: import("std/foo/bar")
            let trimmed_path = path.trim_start_matches(STD_PREFIX_FW).trim_start_matches(STD_PREFIX_BK);

            let full_std_import_path = &cache
                .std_library
                .root_dir
                .join(trimmed_path)
                .with_extension(SOURCE_FILE_EXT);

            try_resolve_relative_path(&full_std_import_path, &RelativeTo::Cwd, Some(path_token.span))?
        } else {
            // import relative to current dir
            // example: import("foo/bar")
            let import_path = PathBuf::from(path).with_extension(SOURCE_FILE_EXT);
            self.search_for_import_path(&cache, &import_path, path_token.span)?
        };

        let module_name = self.get_module_name_from_path(&cache, &absolute_import_path);

        let module_info = PartialModuleInfo::new(ustr(&module_name), ustr(absolute_import_path.to_str().unwrap()));

        spawn_parser(
            self.thread_pool.clone(),
            self.tx.clone(),
            Arc::clone(&self.cache),
            module_info,
        );

        Ok(ast::Ast::Import(ast::Import {
            path: absolute_import_path,
            span: start_span.to(self.previous_span()),
        }))
    }

    fn search_for_import_path(&self, cache: &ParserCacheGuard, path: &Path, span: Span) -> DiagnosticResult<PathBuf> {
        let current_dir = self.module_info.dir();

        if let Some(import_path) = maybe_resolve_relative_path(&path, &RelativeTo::Path(current_dir)) {
            self.check_import_path_is_under_root(&cache, &import_path, span)?;
            return Ok(import_path);
        } else {
            for include_path in cache.include_paths.iter() {
                if let Some(path) = maybe_resolve_relative_path(&path, &RelativeTo::Path(include_path)) {
                    return Ok(path);
                }
            }
        }

        let mut diagnostic = Diagnostic::error()
            .with_message(format!("path `{}` doesn't exist", path.display()))
            .with_label(Label::primary(span, "doesn't exist"))
            .with_note("searched paths:");

        diagnostic.add_note(current_dir.join(path).to_str().unwrap());

        for include_path in cache.include_paths.iter() {
            diagnostic.add_note(include_path.to_str().unwrap());
        }

        Err(diagnostic)
    }

    fn get_module_name_from_path(&self, cache: &ParserCacheGuard, path: &Path) -> String {
        // TODO: this `std_root_dir` thing is very hacky. we should probably get
        // TODO: `std` from `root_dir`, and not do this ad-hoc.

        let root_dir = cache.root_dir.to_str().unwrap();
        let std_root_dir = cache.std_library.root_dir.parent().unwrap().to_str().unwrap();

        let path_str = path.with_extension("").to_str().unwrap().to_string();

        const DOT: &str = ".";

        path_str
            .replace(root_dir, "")
            .replace(std_root_dir, "")
            .replace(std::path::MAIN_SEPARATOR, DOT)
            .trim_start_matches(DOT)
            .trim_end_matches(DOT)
            .to_string()
    }

    fn check_import_path_is_under_root(
        &self,
        cache: &ParserCacheGuard,
        import_path: &Path,
        span: Span,
    ) -> DiagnosticResult<()> {
        if import_path.starts_with(&cache.root_dir) || import_path.starts_with(&cache.std_library.root_dir) {
            Ok(())
        } else {
            Err(Diagnostic::error()
                .with_message("cannot use a file outside of the root module's directory")
                .with_label(Label::primary(span, "cannot use this file"))
                .with_note(format!("import path is {}", import_path.to_str().unwrap()))
                .with_note(format!("root directory is {}", cache.root_dir.display())))
        }
    }
}
