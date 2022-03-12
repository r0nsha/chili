mod infer;
mod sess;
mod ty_var;

use chili_ast::{ast, workspace::Workspace};
use chili_error::DiagnosticResult;
use infer::Infer;
use sess::InferSess;

pub fn infer(workspace: &mut Workspace, asts: &mut Vec<ast::Ast>) -> DiagnosticResult<()> {
    let mut sess = InferSess::new();

    for ast in asts.iter_mut() {
        ast.infer(&mut sess, workspace)?;
    }

    Ok(())
}
