use chili_ast::{ast::Ast, workspace::Workspace};
use chili_error::DiagnosticResult;

pub mod cast;
pub mod coerce;
mod infer;
pub mod normalize;
pub mod sess;
pub mod substitute;
pub mod unify;

pub fn infer<'w>(workspace: &mut Workspace<'w>, asts: &mut Vec<Ast>) -> DiagnosticResult<()> {
    Ok(())
}
