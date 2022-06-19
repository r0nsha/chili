use super::{interp_expr, top_level::CheckTopLevel, Check, CheckResult, CheckSess, Res};
use crate::ast::{ast, ty::Type};
use crate::span::Span;
use std::path::Path;

impl<'s> CheckSess<'s> {
    #[inline]
    pub fn check_import(&mut self, import_path: &Path) -> CheckResult {
        let path_str = import_path.to_str().unwrap();

        let ast = self
            .old_asts
            .iter()
            .find(|a| a.module_info.file_path == path_str)
            .unwrap_or_else(|| panic!("couldn't find ast for module with path: {}", path_str));

        self.check_ast(ast)
    }

    pub fn check_ast(&mut self, ast: &ast::Ast) -> CheckResult {
        if let Some(module_ty) = self.checked_modules.get(&ast.module_id) {
            Ok(Res::new(*module_ty))
        } else {
            let module_id = ast.module_id;

            let module_type = self
                .tycx
                .bound(Type::Module(module_id), Span::initial(ast.file_id));

            self.checked_modules.insert(ast.module_id, module_type);

            for binding in ast.bindings.iter() {
                // 6/6/2022: a binding's pattern has a count of 0 only when a single wildcard symbol is used
                if binding.pattern.iter().count() == 0
                    || binding
                        .pattern
                        .iter()
                        .all(|pattern| self.get_global_symbol(module_id, pattern.symbol).is_none())
                {
                    binding.clone().check_top_level(self)?;
                }
            }

            for expr in ast.run_exprs.iter() {
                let mut expr = expr.clone();
                self.with_env(module_id, |sess, mut env| expr.check(sess, &mut env, None))?;

                if !self.workspace.build_options.check_mode {
                    interp_expr(&expr, self, module_id).unwrap();
                }
            }

            Ok(Res::new(module_type))
        }
    }
}
