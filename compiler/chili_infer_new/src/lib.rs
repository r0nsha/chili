use chili_ast::{ast::Ast, workspace::Workspace};
use chili_error::DiagnosticResult;
use sess::InferSess;

pub mod cast;
pub mod coerce;
mod constraint;
mod infer;
pub mod normalize;
pub mod sess;
pub mod substitute;
mod ty;
pub mod unify;

pub fn infer<'w>(workspace: &mut Workspace<'w>, asts: &mut Vec<Ast>) -> DiagnosticResult<()> {
    let mut sess = InferSess::new(workspace.build_options.target_platform.metrics().word_size);

    // Bind new type variables to all top level bindings
    for ast in asts.iter_mut() {
        for binding in ast.bindings.iter_mut() {
            binding.ty = sess.new_variable().into();

            for symbol in binding.pattern.symbols_mut() {
                workspace
                    .get_binding_info_mut(symbol.binding_info_idx)
                    .unwrap()
                    .ty = sess.new_variable().into();
            }
        }
    }

    for ast in asts.iter_mut() {
        for binding in ast.bindings.iter_mut() {}
    }

    Ok(())
}
