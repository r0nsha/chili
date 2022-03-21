mod typeck;

use chili_ast::{ast, workspace::Workspace};
use chili_error::DiagnosticResult;
use chili_infer::tycx::TyCtx;
use typeck::TypeCk;

pub fn typeck(
    workspace: &mut Workspace,
    tycx: &mut TyCtx,
    asts: &mut Vec<ast::Ast>,
) -> DiagnosticResult<()> {
    // infer and typecheck all expressions, with deep semantics
    // this step requires specific types for semantics that have those requirements
    // for example: function calls, tuple/struct unpacking, for loops, etc.

    for ast in asts.iter_mut() {
        if let Err(err) = ast.typeck(tycx, workspace) {
            tycx.print_type_bindings();
            return Err(err);
        }
    }

    Ok(())
}
