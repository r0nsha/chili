mod builtin;
mod declare;
mod import;
mod resolve;
mod resolver;
mod scope;

use builtin::add_builtin_types;
use chili_ast::{
    ast::Ast,
    workspace::{ModuleId, Workspace},
};
use chili_error::DiagnosticResult;
use declare::Declare;
use resolve::Resolve;
use resolver::Resolver;

pub fn resolve<'w>(
    workspace: &mut Workspace<'w>,
    mut asts: Vec<Ast>,
) -> DiagnosticResult<()> {
    let mut resolver = Resolver::new();

    add_builtin_types(workspace, &mut resolver.global_scope);

    for ast in asts.iter_mut() {
        let module_id = workspace.add_module_info(ast.module_info);

        resolver.module_id = module_id;
        resolver.module_info = ast.module_info;

        ast.declare(&mut resolver, workspace)?;
    }

    for ast in asts.iter_mut() {
        let module_id = ModuleId(
            workspace
                .module_infos
                .iter()
                .position(|m| *m == ast.module_info)
                .unwrap(),
        );

        resolver.module_id = module_id;
        resolver.module_info = ast.module_info;

        ast.resolve(&mut resolver, workspace)?;

        workspace.add_module(ast.clone());
    }

    Ok(())
}
