mod util;

use chili_ast::{
    ast::{self},
    compiler_info,
    path::{try_resolve_relative_path, RelativeTo},
    workspace::{ModuleInfo, Workspace},
};
use chili_parse::{spawn_parser, ParserCache, ParserResult};
use std::{
    collections::HashSet,
    sync::{mpsc::channel, Arc, Mutex},
};
use ustr::ustr;

#[derive(Debug, Clone, Copy)]
pub struct AstGenerationStats {
    pub total_lines: u32,
}

pub type AstGenerationResult = Option<(Vec<ast::Ast>, AstGenerationStats)>;

pub fn generate_ast(workspace: &mut Workspace) -> AstGenerationResult {
    let mut asts: Vec<ast::Ast> = vec![];

    let root_file_path =
        try_resolve_relative_path(&workspace.build_options.source_file, RelativeTo::Cwd, None)
            .map_err(|diag| workspace.diagnostics.push(diag))
            .ok()?;

    let (tx, rx) = channel::<Box<ParserResult>>();

    let cache = Arc::new(Mutex::new(ParserCache {
        root_dir: workspace.root_dir.clone(),
        std_dir: workspace.std_dir.clone(),
        diagnostics: workspace.diagnostics.clone(),
        parsed_modules: HashSet::<ModuleInfo>::new(),
        total_lines: 0,
    }));

    let root_module_info = ModuleInfo::new(
        common::builtin::root_module(),
        ustr(&root_file_path.to_str().unwrap().to_string()),
    );

    spawn_parser(
        tx.clone(),
        Arc::clone(&cache),
        compiler_info::std_module_info(),
    );
    spawn_parser(tx, Arc::clone(&cache), root_module_info);

    for result in rx.iter() {
        match *result {
            ParserResult::NewAst(ast) => asts.push(ast),
            ParserResult::AlreadyParsed => (),
            ParserResult::Failed(diag) => cache.lock().unwrap().diagnostics.push(diag),
        }
    }

    let cache = Arc::try_unwrap(cache).unwrap().into_inner().unwrap();

    workspace.diagnostics = cache.diagnostics;

    Some((
        asts,
        AstGenerationStats {
            total_lines: cache.total_lines,
        },
    ))
}
