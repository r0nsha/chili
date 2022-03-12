mod infer;
mod ty_var;

use chili_ast::{ast, workspace::Workspace};
use chili_error::DiagnosticResult;

pub fn infer<'w>(workspace: &Workspace<'w>, asts: &Vec<ast::Ast>) -> DiagnosticResult<()> {
    // let mut sess = Sess {
    //     workspace,
    //     init_scopes: Scopes::new(),
    // };

    // sess.init_scopes.push_scope();

    // for ast in asts.iter() {
    //     ast.lint(&mut sess)?;
    // }

    // sess.init_scopes.push_scope();

    Ok(())
}
