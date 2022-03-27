pub mod builtin;
mod declare;
mod import;
mod mark_codegen;
mod resolve;
mod resolver;
mod scope;

use chili_ast::{
    ast::{Ast, ResolvedAst},
    workspace::Workspace,
};
use chili_error::DiagnosticResult;
use codespan_reporting::diagnostic::Diagnostic;
use declare::Declare;
use import::{collect_module_exports, expand_and_replace_glob_imports};
use resolve::Resolve;
use resolver::Resolver;
use scope::Scope;

pub fn resolve<'w>(workspace: &mut Workspace, mut asts: Vec<Ast>) -> DiagnosticResult<ResolvedAst> {
    let mut resolved_ast = ResolvedAst::new();
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
            import.module_id = workspace.find_module_info(import.module_info).unwrap();
        }
    }

    // Declare all global definitions
    for ast in asts.iter_mut() {
        resolver.module_id = ast.module_id;
        resolver.module_info = ast.module_info;
        expand_and_replace_glob_imports(&mut ast.imports, &workspace.exports);
        ast.declare(&mut resolver, workspace)?;
    }

    // Resolve all definitions, scopes, uses, etc...
    for ast in asts.iter_mut() {
        resolver.module_id = ast.module_id;
        resolver.module_info = ast.module_info;
        ast.resolve(&mut resolver, workspace)?;
    }

    for ast in asts {
        for import in ast.imports {
            resolved_ast.add_import(import);
        }
        for binding in ast.bindings {
            resolved_ast.add_binding(binding);
        }
    }

    // Check that an entry point function exists
    // Note (Ron): This won't be relevant for targets like WASM or libraries
    if workspace.entry_point_function_id.is_none() {
        return Err(Diagnostic::error()
            .with_message("entry point function `main` is not defined")
            .with_notes(vec![
                "define function `let main = fn() {}` in your entry file".to_string(),
            ]));
    }

    Ok(resolved_ast)
}
