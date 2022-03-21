use chili_ast::{ast, ty::TyKind, workspace::Workspace};
use chili_error::DiagnosticResult;
use chili_infer::{display::map_unify_err, tycx::TyCtx, unify::UnifyTy};

pub(crate) trait TypeCk {
    fn typeck(&mut self, tycx: &mut TyCtx, workspace: &mut Workspace) -> DiagnosticResult<()>;
}

impl TypeCk for ast::Ast {
    fn typeck(&mut self, tycx: &mut TyCtx, workspace: &mut Workspace) -> DiagnosticResult<()> {
        for import in self.imports.iter_mut() {
            import.typeck(tycx, workspace)?;
        }

        for binding in self.bindings.iter_mut() {
            binding.typeck(tycx, workspace)?;
        }

        Ok(())
    }
}

impl TypeCk for ast::Import {
    fn typeck(&mut self, tycx: &mut TyCtx, workspace: &mut Workspace) -> DiagnosticResult<()> {
        // let mut ty_kind = TyKind::Module(self.module_id);

        // if !self.import_path.is_empty() {
        // go over the import_path, resolving the module path
        // let mut current_module_id = self.module_id;

        // for (index, symbol) in self.import_path.iter().enumerate() {
        // let binding_info = self.find_binding_info_in_module(
        //     current_module_id,
        //     symbol.value.as_symbol(),
        //     symbol.span,
        // )?;

        // ty_kind = binding_info.ty.clone();

        // match ty_kind {
        //     Ty::Module(id) => current_module_id = id,
        //     _ => {
        //         if index < self.import_path.len() - 1 {
        //             return Err(TypeError::type_mismatch(
        //                 symbol.span,
        //                 Ty::Module(Default::default()).to_string(),
        //                 ty_kind.to_string(),
        //             ));
        //         }
        //     }
        // }
        //     }
        // }

        // let import_ty = workspace.get_binding_info(self.binding_info_id).unwrap().ty;

        // import_ty
        //     .unify(&ty_kind, tycx, workspace)
        //     .map_err(|e| map_unify_err(e, import_ty, ty_kind, self.span(), tycx))?;

        Ok(())
    }
}

impl TypeCk for ast::Binding {
    fn typeck(&mut self, tycx: &mut TyCtx, workspace: &mut Workspace) -> DiagnosticResult<()> {
        Ok(())
    }
}
