use crate::{interp_expr, top_level::CheckTopLevel, Check, CheckResult, CheckSess, Res};
use chili_ast::{ast, ty::TyKind};
use chili_span::Span;
use std::path::Path;

#[inline]
pub(crate) fn check_import(sess: &mut CheckSess, import_path: &Path, span: Span) -> CheckResult {
    let path_str = import_path.to_str().unwrap();

    let ast = sess
        .old_asts
        .iter()
        .find(|a| a.module_info.file_path == path_str)
        .unwrap_or_else(|| panic!("couldn't find ast for module with path: {}", path_str));

    check_ast(sess, ast, Some(span))
}

pub(crate) fn check_ast(sess: &mut CheckSess, ast: &ast::Ast, span: Option<Span>) -> CheckResult {
    // TODO: Module Type should be Struct (Kind: Module)
    // TODO: Before checking a module, create a type, and a const struct ({a: Value::Binding(id), ...})
    // TODO: Need to add Visibility to StructTypeField
    // TODO: When accessing or destructing a struct field, where the kind is `Module` - Check its visibility - instead of in `check_top_level_binding`

    if let Some(module_ty) = sess.checked_modules.get(&ast.module_id) {
        Ok(Res::new(*module_ty))
    } else {
        let module_id = ast.module_id;

        let module_type = sess
            .tycx
            .bound_maybe_spanned(TyKind::Module(module_id), span);

        for binding in ast.bindings.iter() {
            let first_pat = binding.pattern.iter().next().unwrap();

            if let None = sess.get_global_symbol(module_id, first_pat.symbol) {
                binding.clone().check_top_level(sess, module_id)?;
            }
        }

        for expr in ast.run_exprs.iter() {
            let mut expr = expr.clone();
            sess.with_env(module_id, |sess, mut env| expr.check(sess, &mut env, None))?;
            interp_expr(&expr, sess, module_id).unwrap();
        }

        Ok(Res::new(module_type))
    }
}
