use chili_ast::{ast::Ast, workspace::Workspace};
use chili_error::DiagnosticResult;

pub mod cast;
pub mod coerce;
mod constraint;
mod infer;
pub mod normalize;
pub mod sess;
pub mod substitute;
mod ty;
pub mod unify;

pub fn infer<'w>(workspace: &mut Workspace<'w>, asts: &mut Vec<Ast>) -> DiagnosticResult<()> {
    // Assign type variables to all binding infos
    for binding_info in workspace.binding_infos.iter_mut() {}

    // Assign type variables to ast's and apply constraints
    for ast in asts.iter_mut() {}

    // Unify all type variables

    Ok(())
}
