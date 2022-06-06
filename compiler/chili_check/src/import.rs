use crate::{interp_expr, top_level::CheckTopLevel, Check, CheckResult, CheckSess, Res};
use chili_ast::{ast, ty::TyKind};
use chili_span::{EndPosition, Position, Span};
use std::path::Path;

#[inline]
pub(crate) fn check_import(sess: &mut CheckSess, import_path: &Path) -> CheckResult {
    let path_str = import_path.to_str().unwrap();

    let ast = sess
        .old_asts
        .iter()
        .find(|a| a.module_info.file_path == path_str)
        .unwrap_or_else(|| panic!("couldn't find ast for module with path: {}", path_str));

    check_ast(sess, ast)
}

pub(crate) fn check_ast(sess: &mut CheckSess, ast: &ast::Ast) -> CheckResult {
    if let Some(module_ty) = sess.checked_modules.get(&ast.module_id) {
        Ok(Res::new(*module_ty))
    } else {
        let module_id = ast.module_id;

        let module_type = sess.tycx.bound(
            TyKind::Module(module_id),
            Span::new(ast.file_id, Position::initial(), EndPosition::initial()),
        );

        sess.checked_modules.insert(ast.module_id, module_type);

        for binding in ast.bindings.iter() {
            match binding.pattern.iter().next() {
                Some(first_pat) => {
                    if let None = sess.get_global_symbol(module_id, first_pat.symbol) {
                        binding.clone().check_top_level(sess, module_id)?;
                    }
                }
                None => {
                    // 6/6/2022: this is reached when only a wildcard symbol is used in a pattern
                    binding.clone().check_top_level(sess, module_id)?;
                }
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
