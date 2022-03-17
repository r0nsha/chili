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

    // infer all expressions, with shallow semantics
    // this step doesn't require specific types for most semantics
    for ast in asts.iter_mut() {
        ast.infer(&mut tycx, workspace)?;
    }

    // for binding_info in workspace.binding_infos.iter_mut() {
    //     binding_info.ty = substitute_ty(&binding_info.ty, &tycx);
    // }

    // for ast in asts.iter_mut() {
    //     ast.substitute(&tycx);
    // }

    // infer and typecheck all expressions, with deep semantics
    // this step requires specific types for semantics that have those requirements
    // for example: tuple/struct unpacking, function calls, etc.
    // TODO: typeck step

    // TODO: remove unused type variables:
    // TODO: 1. substitute all ty kinds in the table, using the substitution phase
    // TODO: 2. while substituting, mark used ty id's used in expressions/bindings
    // TODO: 3. filter out all unused vars (should reduce mem use?)

    tycx.print_type_bindings();

    Ok(tycx)
}
