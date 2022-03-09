mod builtin;
mod declare;
mod import;
mod resolve;
mod resolver;
mod scope;

use chili_ast::{ast::Ast, workspace::Workspace};
use chili_error::DiagnosticResult;
use declare::Declare;
use resolver::Resolver;
use scope::Scope;

pub fn resolve<'w>(
    workspace: &mut Workspace<'w>,
    mut asts: Vec<Ast>,
) -> DiagnosticResult<()> {
    let mut resolver = Resolver::new();

    resolver.add_builtin_types(workspace);

    for ast in asts.iter_mut() {
        ast.module_id = workspace.add_module_info(ast.module_info);
        resolver
            .global_scopes
            .insert(ast.module_id, Scope::new(ast.module_info.name));
    }

    for ast in asts.iter_mut() {
        resolver.module_id = ast.module_id;
        resolver.module_info = ast.module_info;
        ast.declare(&mut resolver, workspace)?;
    }

    for ast in asts.iter_mut() {
        resolver.module_id = ast.module_id;
        resolver.module_info = ast.module_info;
        workspace.add_module(ast.clone());
    }

    Ok(())
}
