use crate::{
    ast,
    common::path::{resolve_relative_path, try_resolve_relative_path, RelativeTo},
    parse::{spawn_parser, ParserCache, ParserResult},
    workspace::{compiler_info, PartialModuleInfo, Workspace},
};
use parking_lot::Mutex;
use std::{
    collections::HashSet,
    path::PathBuf,
    sync::{mpsc::channel, Arc},
};
use threadpool::ThreadPool;

#[derive(Debug, Clone, Copy)]
pub struct AstGenerationStats {
    pub total_lines: u32,
}

pub type AstGenerationResult = Option<(Vec<ast::Module>, AstGenerationStats)>;

pub fn generate_ast(workspace: &mut Workspace) -> AstGenerationResult {
    let root_file_path =
        try_resolve_relative_path(&workspace.build_options.source_file, RelativeTo::Cwd, None)
            .map_err(|diag| workspace.diagnostics.push(diag))
            .ok()?;

    let (mut modules, stats) = generate_ast_inner(workspace, root_file_path.clone());

    // Add all module_infos to the workspace
    for module in modules.iter_mut() {
        module.id = workspace.module_infos.insert(module.info);

        if module.info.file_path.as_str() == root_file_path.to_str().unwrap() {
            workspace.root_module_id = module.id;
        }

        module.bindings.iter_mut().for_each(|binding| {
            binding.module_id = module.id;
        });
    }

    Some((modules, stats))
}

fn generate_ast_inner(
    workspace: &mut Workspace,
    root_file_path: PathBuf,
) -> (Vec<ast::Module>, AstGenerationStats) {
    let mut modules: Vec<ast::Module> = vec![];

    let cache = Arc::new(Mutex::new(ParserCache {
        root_file: resolve_relative_path(&workspace.build_options.source_file, RelativeTo::Cwd)
            .unwrap(),
        root_dir: workspace.root_dir.clone(),
        std_dir: workspace.std_dir.clone(),
        include_paths: workspace.build_options.include_paths.clone(),
        diagnostics: workspace.diagnostics.clone(),
        parsed_files: HashSet::new(),
        total_lines: 0,
    }));

    let root_module_info = PartialModuleInfo::from_path(&root_file_path);

    let thread_pool = ThreadPool::new(num_cpus::get());
    let (tx, rx) = channel::<Box<ParserResult>>();

    spawn_parser(
        thread_pool.clone(),
        tx.clone(),
        Arc::clone(&cache),
        compiler_info::std_module_info(),
    );

    spawn_parser(
        thread_pool.clone(),
        tx,
        Arc::clone(&cache),
        root_module_info,
    );

    for result in rx.iter() {
        match *result {
            ParserResult::NewAst(ast) => modules.push(ast),
            ParserResult::AlreadyParsed => (),
            ParserResult::Failed(diag) => cache.lock().diagnostics.push(diag),
        }
    }

    thread_pool.join();

    let cache = Arc::try_unwrap(cache).unwrap().into_inner();

    workspace.diagnostics = cache.diagnostics;

    (
        modules,
        AstGenerationStats {
            total_lines: cache.total_lines,
        },
    )
}
