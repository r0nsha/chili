mod builtin;
mod cast;
pub mod display;
mod infer;
pub mod normalize;
mod substitute;
pub mod tycx;
pub mod unify;
mod unpack_type;

use builtin::get_ty_for_builtin_type;
use chili_ast::{
    ast,
    workspace::{BindingInfoFlags, Workspace},
};
use chili_error::DiagnosticResult;
use infer::{Infer, InferFrame};
use tycx::TyCtx;

pub fn infer(workspace: &mut Workspace, asts: &mut Vec<ast::Ast>) -> DiagnosticResult<TyCtx> {
    let mut tycx = TyCtx::new();

    // assign type variables to all binding infos
    for binding_info in workspace.binding_infos.iter_mut() {
        binding_info.ty = if binding_info.flags.contains(BindingInfoFlags::BUILTIN_TYPE) {
            get_ty_for_builtin_type(binding_info.symbol, &mut tycx)
        } else {
            tycx.new_variable()
        };
    }

    // infer all expressions, with shallow semantics -
    // without requiring specific types for most semantics
    for ast in asts.iter_mut() {
        if let Err(err) = ast.infer(InferFrame::default(), &mut tycx, workspace) {
            tycx.print_type_bindings();
            return Err(err);
        }
    }

    Ok(tycx)
}
