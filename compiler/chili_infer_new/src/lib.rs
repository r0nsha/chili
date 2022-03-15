mod display;
mod infer;
mod sess;
mod substitute;
mod unify;

use chili_ast::{ast, workspace::Workspace};
use chili_error::DiagnosticResult;
use infer::Infer;
use sess::InferSess;
use substitute::{substitute_ty, Substitute};

pub fn infer(workspace: &mut Workspace, asts: &mut Vec<ast::Ast>) -> DiagnosticResult<()> {
    let mut sess = InferSess::new();

    for binding_info in workspace.binding_infos.iter_mut() {
        if binding_info.ty.is_unknown() {
            let var = sess.new_variable();
            // println!("{} :: {}", binding_info.symbol, var);
            binding_info.ty = var.into();
        }
    }

    for ast in asts.iter_mut() {
        ast.infer(&mut sess, workspace)?;
    }

    for binding_info in workspace.binding_infos.iter_mut() {
        binding_info.ty = substitute_ty(&binding_info.ty, &sess);
    }

    for ast in asts.iter_mut() {
        ast.substitute(&sess);
    }

    // TODO: need to have a pass here where i unify type specific syntax
    // TODO: for example: struct and tuple unpacking

    // sess.print_type_bindings();

    Ok(())
}
