use super::{interp_expr, top_level::CheckTopLevel, Check, CheckResult, CheckSess, Res};
use crate::ast::{ast, ty::Type};
use crate::span::Span;
use std::path::Path;

#[inline]
pub fn check_import(sess: &mut CheckSess, import_path: &Path) -> CheckResult {
    let path_str = import_path.to_str().unwrap();

    let ast = sess
        .old_asts
        .iter()
        .find(|a| a.module_info.file_path == path_str)
        .unwrap_or_else(|| panic!("couldn't find ast for module with path: {}", path_str));

    check_ast(sess, ast)
}

pub fn check_ast(sess: &mut CheckSess, ast: &ast::Ast) -> CheckResult {
    if let Some(module_ty) = sess.checked_modules.get(&ast.module_id) {
        Ok(Res::new(*module_ty))
    } else {
        let module_id = ast.module_id;

        let module_type = sess
            .tycx
            .bound(Type::Module(module_id), Span::initial(ast.file_id));

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

            if !sess.workspace.build_options.check_mode {
                interp_expr(&expr, sess, module_id).unwrap();
            }
        }

        Ok(Res::new(module_type))
    }
}
