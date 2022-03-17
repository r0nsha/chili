mod display;
mod infer;
mod normalize;
mod substitute;
pub mod tyctx;
mod unify;
mod unpack_type;
mod utils;

use chili_ast::{ast, workspace::Workspace};
use chili_error::DiagnosticResult;
use infer::Infer;
use substitute::{substitute_ty, Substitute};
use tyctx::TyContext;

pub fn infer(workspace: &mut Workspace, asts: &mut Vec<ast::Ast>) -> DiagnosticResult<()> {
    let mut ctx = TyContext::new();

    for binding_info in workspace.binding_infos.iter_mut() {
        if binding_info.ty.is_unknown() {
            let var = ctx.new_variable();
            binding_info.ty = var.into();
        }
    }

    for ast in asts.iter_mut() {
        ast.infer(&mut ctx, workspace)?;
    }

    for binding_info in workspace.binding_infos.iter_mut() {
        binding_info.ty = substitute_ty(&binding_info.ty, &ctx);
    }

    for ast in asts.iter_mut() {
        ast.substitute(&ctx);
    }

    ctx.print_type_bindings();

    Ok(())
}
