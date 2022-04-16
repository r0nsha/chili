use chili_ast::{
    ast::Ast,
    path::resolve_relative_path,
    workspace::{ModuleInfo, Workspace},
};
use chili_error::DiagnosticResult;
use chili_parse::Parser;
use chili_token::{lexer::Lexer, TokenKind};
use crossbeam_utils::thread;
use dashmap::DashSet;
use std::{
    path::PathBuf,
    sync::{
        atomic::{AtomicUsize, Ordering},
        Mutex, RwLock,
    },
};
use unindent::unindent;
use ustr::ustr;

use crate::{util::insert_std_import, AstGenerationResult, AstGenerationStats};

pub struct AstGenerator<'a> {
    pub workspace: Mutex<&'a mut Workspace>,
    pub parsed_modules: DashSet<ModuleInfo>,
    pub(crate) total_lines: AtomicUsize,
}

impl<'a> AstGenerator<'a> {
    pub fn new(workspace: &'a mut Workspace) -> Self {
        Self {
            workspace: Mutex::new(workspace),
            parsed_modules: Default::default(),
            total_lines: Default::default(),
        }
    }

    pub fn start(&mut self) -> AstGenerationResult {
        let asts: RwLock<Vec<Ast>> = RwLock::default();

        let root_file_path = {
            let w = &self.workspace.lock().unwrap();
            w.build_options.source_file.clone()
        };

        let root_file_path =
            resolve_relative_path(&root_file_path, &common::builtin::root_module(), None)?;

        let root_module_info =
            ModuleInfo::new(common::builtin::root_module(), ustr(&root_file_path));

        self.add_source_file(&asts, root_module_info, true)?;

        Ok((
            asts.into_inner().unwrap(),
            AstGenerationStats {
                total_lines: self.total_lines.load(Ordering::SeqCst),
            },
        ))
    }

    fn add_source_file(
        &self,
        asts: &RwLock<Vec<Ast>>,
        module_info: ModuleInfo,
        is_root: bool,
    ) -> DiagnosticResult<()> {
        if !self.parsed_modules.insert(module_info) {
            return Ok(());
        }

        let source = std::fs::read_to_string(module_info.file_path.as_str())
            .expect(&format!("failed to read `{}`", module_info.file_path));

        // TODO: this should be behind a `verbose` or `debug` flag or something
        self.total_lines
            .fetch_add(source.lines().count(), Ordering::SeqCst);

        let file_id = {
            let mut workspace = self.workspace.lock().unwrap();

            let file_id = workspace
                .diagnostics
                .add_file(module_info.file_path.to_string(), unindent(&source));

            if is_root {
                workspace.root_file_id = file_id;
            }

            file_id
        };
        let tokens = Lexer::new(file_id, &source).scan()?;
        // println!(
        //     "{:?}",
        //     tokens
        //         .iter()
        //         .map(|t| t.kind.lexeme())
        //         .collect::<Vec<&str>>()
        // );

        if tokens.is_empty() || tokens.first().unwrap().kind == TokenKind::Eof {
            return Ok(());
        }

        let (root_dir, std_dir) = {
            let w = self.workspace.lock().unwrap();
            (w.root_dir.clone(), w.std_dir.clone())
        };

        let mut parser = Parser::new(
            tokens,
            module_info,
            &root_dir,
            &std_dir,
            PathBuf::from(module_info.file_path.as_str())
                .parent()
                .unwrap()
                .to_str()
                .unwrap()
                .to_string(),
        );

        let mut parse_result = parser.parse()?;

        // implicitly add `std` to every file we parse
        // insert_std_import(&mut parse_result.ast, &mut parse_result.imports);

        thread::scope(|scope| {
            let mut handles = vec![];

            for u in parse_result.imports.iter() {
                let h = scope.spawn(|_| {
                    // TODO: I ignore parser errors for now (which is bad...)
                    // TODO: This will be fixed when I make the parser
                    // TODO: recoverable
                    let _ = self.add_source_file(asts, *u, false);
                });

                handles.push(h);
            }

            for h in handles {
                h.join().unwrap();
            }
        })
        .unwrap();

        asts.write().unwrap().push(parse_result.ast);

        Ok(())
    }
}
