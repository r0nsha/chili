use crate::{
    ast,
    common::path::{try_resolve_relative_path, RelativeTo},
    parse::{spawn_parser, ParserCache, ParserResult},
    workspace::{ModuleId, Workspace},
};
use parking_lot::Mutex;
use std::{
    collections::HashSet,
    sync::{mpsc::channel, Arc},
};
use threadpool::ThreadPool;

#[derive(Debug, Clone, Copy)]
pub struct AstGenerationStats {
    pub total_lines: u32,
}

pub type AstGenerationResult = Option<(Vec<ast::Module>, AstGenerationStats)>;

pub fn generate_ast(workspace: &mut Workspace) -> AstGenerationResult {
    let root_file_path = try_resolve_relative_path(&workspace.build_options.source_file, &RelativeTo::Cwd, None)
        .map_err(|diag| workspace.diagnostics.push(diag))
        .ok()?;

    let (mut modules, stats) = generate_ast_inner(workspace);

    // Add all module_infos to the workspace
    for module in modules.iter_mut() {
        module.id = workspace.module_infos.insert(module.info);

        if module.info.file_path.as_str() == root_file_path.to_str().unwrap() {
            workspace.root_module_id = module.id;
        }
    }

    // Set the root module id for all libraries
    for (_, library) in workspace.libraries.iter_mut() {
        let root_file = library.root_file.to_str().unwrap();

        library.root_module_id = workspace
            .module_infos
            .iter()
            .position(|(_, module)| module.file_path == root_file)
            .map(ModuleId::from)
            .unwrap();
    }

    Some((modules, stats))
}

fn generate_ast_inner(workspace: &mut Workspace) -> (Vec<ast::Module>, AstGenerationStats) {
    let mut modules: Vec<ast::Module> = vec![];

    let cache = Arc::new(Mutex::new(ParserCache {
        libraries: workspace.library_map(),
        include_paths: workspace.build_options.include_paths.clone(),
        diagnostics: workspace.diagnostics.clone(),
        parsed_files: HashSet::new(),
        total_lines: 0,
    }));

    let thread_pool = ThreadPool::new(num_cpus::get());
    let (tx, rx) = channel::<Box<ParserResult>>();

    spawn_parser(
        thread_pool.clone(),
        tx.clone(),
        Arc::clone(&cache),
        workspace.main_library().as_module_path(),
    );

    spawn_parser(
        thread_pool.clone(),
        tx,
        Arc::clone(&cache),
        workspace.std_library().as_module_path(),
    );

    for result in rx.iter() {
        match *result {
            ParserResult::NewModule(module) => modules.push(module),
            ParserResult::AlreadyParsed | ParserResult::ParserFailed => (),
            ParserResult::LexerFailed(module, diag) => {
                modules.push(module);
                cache.lock().diagnostics.push(diag);
            }
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
