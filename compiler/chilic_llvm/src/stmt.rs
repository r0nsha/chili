use chilic_ast::stmt::{Stmt, StmtKind};
use inkwell::{module::Linkage, values::BasicValueEnum};

use crate::codegen::CodegenDecl;

use super::codegen::{Codegen, CodegenState};

impl<'cg, 'ctx> Codegen<'cg, 'ctx> {
    pub(super) fn gen_stmt(
        &mut self,
        state: &mut CodegenState<'ctx>,
        stmt: &Stmt,
    ) -> BasicValueEnum<'ctx> {
        match &stmt.kind {
            StmtKind::UseDecl(use_) => {
                let decl = self.resolve_decl_from_use(use_);
                state.env.insert(use_.alias, decl);
            }
            StmtKind::Entity(entity) => {
                if entity.lib_name.is_some() {
                    let symbol = entity.pattern.into_single().symbol;
                    let ty = self.llvm_type(&entity.ty);
                    let global_value =
                        self.add_global_uninit(&symbol, ty, Linkage::External);
                    state.env.insert(symbol, CodegenDecl::Global(global_value));
                } else {
                    self.gen_entity_pattern_from_expr(
                        state,
                        &entity.pattern,
                        &entity.ty,
                        &entity.value,
                    );
                }
            }
            StmtKind::Defer(_) => (),
            StmtKind::Expr { expr, terminated } => {
                let value = self.gen_expr(state, expr, true);

                if !terminated {
                    return value;
                }
            }
        }

        self.gen_unit()
    }
}
