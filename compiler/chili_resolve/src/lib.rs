pub mod builtin;
mod declare;
mod import;
mod mark_codegen;
mod resolve;
mod resolver;
mod scope;

use chili_ast::{ast::Ast, workspace::Workspace};
use import::{collect_module_exports, resolve_imports};
use resolver::Resolver;
use scope::Scope;

pub fn resolve(workspace: &mut Workspace, asts: &mut Vec<Ast>) {
    let mut resolver = Resolver::new();

    resolver.add_builtin_types(workspace);
    collect_module_exports(&asts, &mut workspace.exports);

    // Add all module_infos to the workspace
    for ast in asts.iter_mut() {
        ast.module_id = workspace.add_module_info(ast.module_info);
        resolver
            .global_scopes
            .insert(ast.module_id, Scope::new(ast.module_info.name));
    }

    // Assign module ids to all imports
    for ast in asts.iter_mut() {
        for import in ast.imports.iter_mut() {
            import.target_module_id = workspace
                .find_module_info(import.target_module_info)
                .unwrap();
        }
    }

    // Declare all global definitions
    for ast in asts.iter_mut() {
        resolve_imports(&mut ast.imports, &workspace.exports);

        ast.imports
            .iter_mut()
            .for_each(|import| import.module_id = ast.module_id);

        ast.bindings
            .iter_mut()
            .for_each(|binding| binding.module_id = ast.module_id);
    }
}
