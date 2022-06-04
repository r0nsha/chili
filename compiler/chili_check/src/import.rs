use std::path::Path;

use crate::{env::Env, interp_expr, top_level::CheckTopLevel, Check, CheckResult, CheckSess, Res};
use chili_ast::{ast::ModuleStruct, const_value::ConstValue, workspace::ModuleExports};

#[inline]
pub(crate) fn check_import(
    sess: &mut CheckSess,
    _env: &mut Env,
    import_path: &Path,
) -> CheckResult {
    let path_str = import_path.to_str().unwrap();

    let ast = sess
        .old_asts
        .iter()
        .find(|a| a.module_info.file_path == path_str)
        .unwrap_or_else(|| panic!("couldn't find ast for module with path: {}", path_str));

    dbg!(path_str);

    if let Some(module) = sess.new_typed_ast.modules.get(&ast.module_id) {
        Ok(Res::from(module))
    } else {
        let module_id = ast.module_id;
        let exports = ModuleExports::new();

        for binding in ast.bindings.iter() {
            let pat = binding.pattern.iter().next().unwrap();

            if let None = sess.get_global_symbol(module_id, pat.symbol) {
                binding.clone().check_top_level(sess, module_id)?;
            }

            //     if binding.visibility.is_public() {
            //         binding.pattern.iter().for_each(|pat| {
            //             entry.insert(pat.symbol, pat.binding_info_id);
            //         });
            //     }
        }

        for expr in ast.run_exprs.iter() {
            let mut expr = expr.clone();
            sess.with_env(module_id, |sess, mut env| expr.check(sess, &mut env, None))?;
            interp_expr(&expr, sess, module_id).unwrap();
        }

        // TODO: cache struct in new_typed_ast

        todo!()
    }
}

impl From<&ModuleStruct> for Res {
    fn from(module: &ModuleStruct) -> Self {
        Res::new_const(module.ty, ConstValue::Struct(module.const_value.clone()))
    }
}
