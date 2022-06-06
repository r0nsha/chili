use crate::*;
use chili_ast::{
    ast, compiler_info,
    path::{try_resolve_relative_path, RelativeTo},
    workspace::ModuleInfo,
};
use chili_error::{
    diagnostic::{Diagnostic, Label},
    DiagnosticResult, SyntaxError,
};
use std::path::Path;
use ustr::ustr;

impl Parser {
    pub(crate) fn parse_builtin_import(&mut self) -> DiagnosticResult<ast::BuiltinKind> {
        let token = require!(self, Str(_), "string")?;
        let path = token.symbol().as_str();

        let absolute_import_path = if path == "~" {
            self.cache.lock().root_file.clone()
        } else if path.starts_with("~") {
            let mut import_path = self
                .cache
                .lock()
                .root_dir
                .join(PathBuf::from(path.trim_start_matches("~/")));

            if import_path.extension().is_none() {
                import_path.set_extension(compiler_info::SOURCE_FILE_EXT);
            }

            try_resolve_relative_path(&import_path, RelativeTo::Cwd, Some(token.span))?
        } else if compiler_info::is_std_module_path(&path) {
            // example: @import("std")
            try_resolve_relative_path(
                &compiler_info::std_module_root_file(),
                RelativeTo::Cwd,
                Some(token.span),
            )?
        } else if compiler_info::is_std_module_path_start(&path) {
            // example: @import("std/foo/bar")
            let trimmed_path = path
                .trim_start_matches(compiler_info::STD_PREFIX_FW)
                .trim_start_matches(compiler_info::STD_PREFIX_BK);

            let mut full_std_import_path = compiler_info::std_module_root_dir().join(trimmed_path);

            if full_std_import_path.extension().is_none() {
                full_std_import_path.set_extension(compiler_info::SOURCE_FILE_EXT);
            }

            try_resolve_relative_path(&full_std_import_path, RelativeTo::Cwd, Some(token.span))?
        } else {
            // example: @import("foo/bar")
            let path = Path::new(path);

            let import_path = if path.extension().is_some() {
                path.to_path_buf()
            } else {
                Path::new(path).with_extension(compiler_info::SOURCE_FILE_EXT)
            };

            try_resolve_relative_path(
                &import_path,
                RelativeTo::Path(Path::new(&self.current_dir)),
                Some(token.span),
            )?
        };

        {
            let root_dir = &self.cache.lock().root_dir;

            // TODO: We specially handle the case of paths under std.
            // TODO: In the future, we'd like to treat std is a proper library, so we won't need this.
            if !absolute_import_path.starts_with(root_dir)
                && !absolute_import_path.starts_with(compiler_info::std_module_root_dir())
            {
                return Err(Diagnostic::error()
                    .with_message("cannot use a file outside of the root module's directory")
                    .with_label(Label::primary(token.span, "cannot use this file"))
                    .with_note(format!(
                        "import path is {}",
                        absolute_import_path.to_str().unwrap()
                    ))
                    .with_note(format!("root directory is {}", root_dir.to_str().unwrap())));
            }
        }

        let module_name = self.get_module_name_from_path(&absolute_import_path);

        let module_info = ModuleInfo::new(
            ustr(&module_name),
            ustr(absolute_import_path.to_str().unwrap()),
        );

        spawn_parser(self.tx.clone(), Arc::clone(&self.cache), module_info);

        Ok(ast::BuiltinKind::Import(absolute_import_path))
    }

    fn get_module_name_from_path(&self, path: &Path) -> String {
        // TODO: this `std_root_dir` thing is very hacky. we should probably get
        // TODO: `std` from `root_dir`, and not do this ad-hoc.
        let cache = self.cache.lock();
        let root_dir = cache.root_dir.to_str().unwrap();
        let std_root_dir = cache.std_dir.parent().unwrap().to_str().unwrap();

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
}
