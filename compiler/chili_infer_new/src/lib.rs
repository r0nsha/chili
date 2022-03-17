mod builtin;
mod cast;
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
use infer::{Infer, InferFrame};
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

    // infer all expressions, with shallow semantics -
    // without requiring specific types for most semantics
    for ast in asts.iter_mut() {
        ast.infer(InferFrame::default(), &mut tycx, workspace)?;
    }

    // infer and typecheck all expressions, with deep semantics
    // this step requires specific types for semantics that have those requirements
    // for example: tuple/struct unpacking, function calls, etc.
    // TODO: typeck step

    tycx.print_type_bindings();

    Ok(tycx)
}
