mod builtin;
mod display;
mod infer;
mod normalize;
mod substitute;
pub mod tycx;
mod unify;
mod unpack_type;

use builtin::get_ty_for_builtin_type;
use chili_ast::{
    ast,
    workspace::{BindingInfoFlags, Workspace},
};
use chili_error::DiagnosticResult;
use infer::Infer;
// use substitute::{substitute_ty, Substitute};
use tycx::TyContext;

pub fn infer(workspace: &mut Workspace, asts: &mut Vec<ast::Ast>) -> DiagnosticResult<TyContext> {
    let mut tycx = TyContext::new();

    // assign type variables to all binding infos
    for binding_info in workspace.binding_infos.iter_mut() {
        binding_info.ty = if binding_info.flags.contains(BindingInfoFlags::BUILTIN_TYPE) {
            get_ty_for_builtin_type(binding_info.symbol, &mut tycx)
        } else {
            tycx.new_variable()
        };
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
