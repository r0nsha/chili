use chili_ast::{
    ast::{Ast, Import, ModuleInfo, Visibility},
    path::resolve_relative_path,
};
use chili_error::DiagnosticResult;
use chili_parse::Parser;
use chili_span::Span;
use chili_token::{lexer::Lexer, TokenKind};
use codespan_reporting::files::SimpleFiles;
use common::{
    compiler_info::{self, IntrinsticModuleInfo},
    Stopwatch,
};
use crossbeam_utils::thread;
use dashmap::DashSet;
use std::{
    collections::HashSet,
    path::PathBuf,
    sync::{
        atomic::{AtomicUsize, Ordering},
        Mutex, RwLock,
    },
};
use unindent::unindent;
use ustr::ustr;

pub struct AstGenerator<'a> {
    pub files: Mutex<&'a mut SimpleFiles<String, String>>,
    pub root_dir: String,
    pub root_file_id: AtomicUsize,
    pub parsed_modules: DashSet<ModuleInfo>,
}

impl<'a> AstGenerator<'a> {
    pub fn new(
        files: &'a mut SimpleFiles<String, String>,
        root_dir: String,
    ) -> Self {
        Self {
            files: Mutex::new(files),
            root_dir,
            root_file_id: Default::default(),
            parsed_modules: Default::default(),
        }
    }

    pub fn start(&mut self, file_path: String) -> DiagnosticResult<Vec<Ast>> {
        let asts: RwLock<Vec<Ast>> = RwLock::default();

        let sw = Stopwatch::start_new("parse");

        let root_file_path = resolve_relative_path(
            &file_path,
            &common::builtin::root_module(),
            None,
        )?;

        let root_module_info = ModuleInfo::new(
            common::builtin::root_module(),
            ustr(&root_file_path),
        );

        self.add_source_file(&asts, root_module_info, true)?;

        sw.print();

        Ok(asts.into_inner().unwrap())
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

        let file_id = self
            .files
            .lock()
            .unwrap()
            .add(module_info.file_path.to_string(), unindent(&source));

        if is_root {
            self.root_file_id.store(file_id, Ordering::SeqCst);
        }

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

        let mut parser = Parser::new(
            tokens,
            module_info,
            self.root_dir.clone(),
            PathBuf::from(module_info.file_path.as_str())
                .parent()
                .unwrap()
                .to_str()
                .unwrap()
                .to_string(),
        );

        let mut parse_result = parser.parse()?;

        // implicitly add `std` to every file we parse
        add_intrinsic_std_import(
            &mut parse_result.ast,
            &mut parse_result.imports,
        );

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

fn add_intrinsic_std_import(ast: &mut Ast, imports: &mut HashSet<ModuleInfo>) {
    add_intrinsic_module(ast, imports, compiler_info::std_module_info())
}

fn add_intrinsic_module(
    ast: &mut Ast,
    imports: &mut HashSet<ModuleInfo>,
    intrinsic_module_info: IntrinsticModuleInfo,
) {
    let intrinsic_module_info = ModuleInfo::new(
        intrinsic_module_info.name,
        intrinsic_module_info.file_path,
    );

    ast.imports.push(Import {
        module_info: intrinsic_module_info,
        alias: intrinsic_module_info.name,
        import_path: vec![],
        visibility: Visibility::Private,
        span: Span::unknown(),
    });

    imports.insert(intrinsic_module_info);
}
