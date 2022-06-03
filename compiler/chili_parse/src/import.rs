use crate::*;
use chili_ast::{
    ast::{Import, ImportPath, ImportPathNode, Visibility},
    compiler_info,
    workspace::ModuleInfo,
};
use chili_error::{
    diagnostic::{Diagnostic, Label},
    DiagnosticResult, SyntaxError,
};
use chili_span::{Span, Spanned};
use common::builtin::{MOD_FILE_NAME, SOURCE_FILE_EXT};
use std::path::{Path, PathBuf};
use ustr::{ustr, Ustr};

impl<'p> Parser<'p> {
    pub(crate) fn parse_import(&mut self, visibility: Visibility) -> DiagnosticResult<Vec<Import>> {
        let imports = self.parse_import_inner(visibility)?;
        imports.iter().for_each(|import| {
            self.used_modules.insert(import.target_module_info);
        });
        Ok(imports)
    }

    fn parse_import_inner(&mut self, visibility: Visibility) -> DiagnosticResult<Vec<Import>> {
        let id_token = require!(self, Ident(_), "identifier")?;
        let name = id_token.symbol().as_str();

        match name {
            compiler_info::STD => {
                let std_module_info = compiler_info::std_module_info();

                let path_buf = PathBuf::from(std_module_info.file_path.as_str());

                let module = std_module_info.name;
                let alias = std_module_info.name;

                self.parse_import_postfix(path_buf, module, alias, visibility, id_token.span)
            }
            _ => {
                let mut path_buf = self.current_dir.clone();
                path_buf.push(name);

                let module = ustr(&self.get_module_name_from_path(&path_buf));
                let alias = ustr(name);

                if path_buf.with_extension(SOURCE_FILE_EXT).is_file() {
                    path_buf.set_extension(SOURCE_FILE_EXT);
                    check_path_is_under_root_or_std(self.root_dir, &path_buf, id_token.span)?;

                    self.parse_import_postfix(path_buf, module, alias, visibility, id_token.span)
                } else if path_buf.is_dir() {
                    check_path_is_under_root_or_std(self.root_dir, &path_buf, id_token.span)?;

                    let mut mod_path = path_buf.clone();

                    mod_path.push(MOD_FILE_NAME);
                    mod_path.set_extension(SOURCE_FILE_EXT);

                    if mod_path.exists() && mod_path.is_file() {
                        self.parse_import_postfix(
                            mod_path,
                            module,
                            alias,
                            visibility,
                            id_token.span,
                        )
                    } else {
                        Err(module_not_found_err(&path_buf, &module, id_token.span))
                    }
                } else {
                    Err(module_not_found_err(&path_buf, &module, id_token.span))
                }
            }
        }
    }

    fn parse_import_postfix(
        &mut self,
        path_buf: PathBuf,
        module: Ustr,
        alias: Ustr,
        visibility: Visibility,
        module_name_span: Span,
    ) -> DiagnosticResult<Vec<Import>> {
        let mut import_path = vec![];

        let imports = self.parse_import_postfix_inner(
            ustr(path_buf.to_str().unwrap()),
            module,
            alias,
            visibility,
            module_name_span,
            &mut import_path,
        )?;

        Ok(imports)
    }

    fn parse_import_postfix_inner(
        &mut self,
        path: Ustr,
        module: Ustr,
        alias: Ustr,
        visibility: Visibility,
        span: Span,
        import_path: &mut ImportPath,
    ) -> DiagnosticResult<Vec<Import>> {
        if eat!(self, Dot) {
            if eat!(self, Ident(_)) {
                // single child, i.e: `use other.foo`

                let id_token = self.previous();
                let id_token_span = id_token.span;
                let alias = id_token.symbol();

                import_path.push(Spanned::new(ImportPathNode::Symbol(alias), id_token.span));

                self.parse_import_postfix_inner(
                    path,
                    module,
                    alias,
                    visibility,
                    id_token_span,
                    import_path,
                )
            } else if eat!(self, OpenCurly) {
                // multiple children, i.e: `use other.{foo, bar}`

                let mut imports = vec![];

                while !eat!(self, CloseCurly) {
                    let id_token = require!(self, Ident(_), "identifier")?;
                    let alias = id_token.symbol();

                    let mut local_import_path = import_path.clone();
                    local_import_path
                        .push(Spanned::new(ImportPathNode::Symbol(alias), id_token.span));

                    let import = self.parse_import_postfix_inner(
                        path,
                        module,
                        alias,
                        visibility,
                        id_token.span,
                        &mut local_import_path,
                    )?;

                    imports.extend(import);

                    if !eat!(self, Comma) {
                        require!(self, CloseCurly, "}")?;
                        break;
                    }
                }

                Ok(imports)
            } else if eat!(self, QuestionMark) {
                import_path.push(Spanned::new(ImportPathNode::Glob, self.previous_span()));
                Ok(vec![Import {
                    module_id: Default::default(),
                    target_binding_info_id: None,
                    target_module_id: Default::default(),
                    target_module_info: ModuleInfo::new(module, path),
                    alias: ustr(""),
                    import_path: import_path.clone(),
                    visibility,
                    span,
                    binding_info_id: Default::default(),
                }])
            } else {
                Err(SyntaxError::expected(self.span(), "an identifier, { or ?"))
            }
        } else {
            let alias = if eat!(self, As) {
                require!(self, Ident(_), "identifier")?.symbol()
            } else {
                alias
            };

            Ok(vec![Import {
                module_id: Default::default(),
                target_binding_info_id: None,
                target_module_id: Default::default(),
                target_module_info: ModuleInfo::new(module, path),
                alias,
                import_path: import_path.clone(),
                visibility,
                span,
                binding_info_id: Default::default(),
            }])
        }
    }

    fn get_module_name_from_path(&self, path: &Path) -> String {
        // TODO: this `std_root_dir` thing is very hacky. we should probably get
        // TODO: `std` from `root_dir`, and not do this ad-hoc.
        let root_dir = self.root_dir.to_str().unwrap();
        let std_root_dir = self.std_dir.parent().unwrap().to_str().unwrap();

        let path_str = path.to_str().unwrap().to_string();

        const DOT: &str = ".";
        let path_str = path_str
            .replace(root_dir, "")
            .replace(std_root_dir, "")
            .replace(std::path::MAIN_SEPARATOR, DOT)
            .trim_start_matches(DOT)
            .trim_end_matches(DOT)
            .to_string();

        path_str
    }
}

fn module_not_found_err(path_buf: &Path, module: &str, span: Span) -> Diagnostic {
    Diagnostic::error()
        .with_message(format!("couldn't find module `{}`", module))
        .with_label(Label::primary(span, "not found"))
        .with_note(format!(
            "tried to resolve this path: {}",
            path_buf.display()
        ))
}

fn check_path_is_under_root_or_std(
    root_path: &Path,
    path_buf: &Path,
    span: Span,
) -> DiagnosticResult<()> {
    if path_buf.starts_with(root_path) || path_buf.starts_with(compiler_info::std_module_root_dir())
    {
        Ok(())
    } else {
        Err(Diagnostic::error()
            .with_message("cannot use modules outside of the root module scope")
            .with_label(Label::primary(span, "cannot use")))
    }
}
