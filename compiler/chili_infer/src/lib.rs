mod builtin;
mod cast;
pub mod display;
mod infer;
mod infer_top_level;
pub mod normalize;
mod substitute;
pub mod tycx;
pub mod unify;
mod unpack_type;

use chili_ast::{ast, workspace::Workspace};
use chili_error::DiagnosticResult;
use infer::InferSess;
use tycx::TyCtx;

pub fn infer(
    workspace: &mut Workspace,
    ast: ast::ResolvedAst,
) -> DiagnosticResult<(ast::ResolvedAst, TyCtx)> {
    let mut sess = InferSess::new(workspace, &ast);
    sess.start()?;
    Ok((sess.new_ast, sess.tycx))
    // let mut tycx = TyCtx::new();

    // workspace.binding_infos.annotate(&mut tycx);
    // for binding in bound_ast.bindings.iter_mut() {
    //     binding.annotate(&mut tycx);
    // }

    // if let Err(err) = bound_ast.infer(InferFrame::default(), &mut tycx, workspace) {
    //     tycx.print_type_bindings();
    //     return Err(err);
    // }

    // typeck_bound_ast
    // if let Err(err) = bound_ast.infer(InferFrame::default(), &mut tycx, workspace) {
    //     tycx.print_type_bindings();
    //     return Err(err);
    // }
}
