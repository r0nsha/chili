mod infer;
mod ty_var;

use chili_ast::{ast, workspace::Workspace};
use chili_error::DiagnosticResult;
use infer::{Infer, InferSess};

pub fn infer(workspace: &mut Workspace, asts: &mut Vec<ast::Ast>) -> DiagnosticResult<()> {
    let mut sess = InferSess { workspace };

    for ast in asts.iter_mut() {
        ast.infer(&mut sess)?;
    }

    Ok(())
}
