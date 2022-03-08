mod declare;
mod import;
mod resolve;
mod resolver;
mod scope;

use chili_ast::{ast::Ast, workspace::Workspace};
use chili_error::DiagnosticResult;
use declare::Declare;
use resolve::Resolve;
use resolver::Resolver;

pub fn resolve<'w>(
    workspace: &mut Workspace<'w>,
    mut asts: Vec<Ast>,
) -> DiagnosticResult<()> {
    for ast in asts.iter_mut() {
        let mut resolver =
            Resolver::new(workspace.next_module_id(), ast.module_info);

        resolver.push_scope();

        ast.declare(&mut resolver, workspace)?;
        ast.resolve(&mut resolver, workspace)?;

        workspace.add_module(ast.clone());

        resolver.pop_scope();
    }

    Ok(())
}
