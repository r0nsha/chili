mod util;

use crate::util::add_std_import;
use chili_ast::{
    ast::{self},
    path::{try_resolve_relative_path, RelativeTo},
    workspace::{ModuleInfo, Workspace},
};
use chili_parse::Parser;
use chili_token::lexer::Lexer;
use std::{collections::HashSet, path::PathBuf};
use unindent::unindent;
use ustr::ustr;

#[derive(Debug, Clone, Copy)]
pub struct AstGenerationStats {
    pub total_lines: usize,
}

pub type AstGenerationResult = Option<(Vec<ast::Ast>, AstGenerationStats)>;

pub fn generate_ast(workspace: &mut Workspace) -> AstGenerationResult {
    AstGenerator::new(workspace).start()
}

pub struct AstGenerator<'a> {
    pub workspace: &'a mut Workspace,
    pub parsed_modules: HashSet<ModuleInfo>,
    pub(crate) total_lines: usize,
}

impl<'a> AstGenerator<'a> {
    pub fn new(workspace: &'a mut Workspace) -> Self {
        Self {
            workspace,
            parsed_modules: Default::default(),
            total_lines: Default::default(),
        }
    }

    pub fn start(&mut self) -> AstGenerationResult {
        let mut asts: Vec<ast::Ast> = vec![];

        let root_file_path = try_resolve_relative_path(
            &self.workspace.build_options.source_file,
            RelativeTo::Cwd,
            None,
        )
        .map_err(|diag| self.workspace.diagnostics.push(diag))
        .ok()?;

        let root_module_info = ModuleInfo::new(
            common::builtin::root_module(),
            ustr(&root_file_path.to_str().unwrap().to_string()),
        );

        self.add_source_file(&mut asts, root_module_info, true);

        Some((
            asts,
            AstGenerationStats {
                total_lines: self.total_lines,
            },
        ))
    }

    fn add_source_file(
        &mut self,
        asts: &mut Vec<ast::Ast>,
        module_info: ModuleInfo,
        is_root: bool,
    ) {
        if !self.parsed_modules.insert(module_info) {
            return;
        }

        let source = std::fs::read_to_string(module_info.file_path.as_str())
            .unwrap_or_else(|_| panic!("failed to read `{}`", module_info.file_path));

        self.total_lines += source.lines().count();

        let file_id = self
            .workspace
            .diagnostics
            .add_file(module_info.file_path.to_string(), unindent(&source));

        if is_root {
            self.workspace.root_file_id = file_id;
        }

        let tokens = match Lexer::new(file_id, &source).scan() {
            Ok(t) => t,
            Err(diag) => {
                self.workspace.diagnostics.push(diag);
                return;
            }
        };

        let mut parser = Parser::new(
            tokens,
            module_info,
            &self.workspace.root_dir,
            &self.workspace.std_dir,
            module_info.dir().to_path_buf(),
        );
        match parser.parse() {
            Ok(mut parse_result) => {
                // implicitly add `std` to every file we parse
                add_std_import(&mut parse_result.ast, &mut parse_result.imports);

                for u in parse_result.imports.iter() {
                    self.add_source_file(asts, *u, false);
                }

                asts.push(parse_result.ast);
            }
            Err(diag) => self.workspace.diagnostics.push(diag),
        }
    }
}
