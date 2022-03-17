mod display;
mod infer;
mod normalize;
mod substitute;
pub mod tycx;
mod unify;
mod unpack_type;

use chili_ast::{ast, workspace::Workspace};
use chili_error::DiagnosticResult;
use infer::Infer;
// use substitute::{substitute_ty, Substitute};
use tycx::TyContext;

pub fn infer(workspace: &mut Workspace, asts: &mut Vec<ast::Ast>) -> DiagnosticResult<TyContext> {
    let mut tycx = TyContext::new();

    // assign type variables to all binding infos
    for binding_info in workspace.binding_infos.iter_mut() {
        // TODO: if this is a builtin type, find its appropriate type and assign it
        binding_info.ty = tycx.new_variable();
    }

    // infer all expressions
    for ast in asts.iter_mut() {
        ast.infer(&mut tycx, workspace)?;
    }

    // for binding_info in workspace.binding_infos.iter_mut() {
    //     binding_info.ty = substitute_ty(&binding_info.ty, &tycx);
    // }

    // for ast in asts.iter_mut() {
    //     ast.substitute(&tycx);
    // }

    tycx.print_type_bindings();

    Ok(tycx)
}
