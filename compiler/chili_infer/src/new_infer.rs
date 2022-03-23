use crate::{display::map_unify_err, normalize::NormalizeTy, tycx::TyCtx, unify::UnifyTy};
use chili_ast::{
    ast::{self, Expr},
    ty::*,
    value::Value,
    workspace::Workspace,
};
use chili_error::DiagnosticResult;
use codespan_reporting::diagnostic::{Diagnostic, Label};
use ustr::ustr;

pub(crate) struct InferSess<'s> {
    pub(crate) workspace: &'s mut Workspace,
    pub(crate) old_ast: ast::ResolvedAst,
    pub(crate) new_ast: ast::ResolvedAst,
    pub(crate) tycx: TyCtx,
    pub(crate) frames: Vec<InferFrame>,
}

pub(crate) type InferResult<T> = DiagnosticResult<(T, Option<Value>)>;

#[derive(Debug, Default, Clone, Copy)]
pub(crate) struct InferFrame {
    return_ty: Ty,
    self_ty: Option<Ty>,
}

impl<'s> InferSess<'s> {
    pub(crate) fn new(workspace: &'s mut Workspace, old_ast: ast::ResolvedAst) -> Self {
        Self {
            workspace,
            old_ast,
            new_ast: ast::ResolvedAst::new(),
            tycx: TyCtx::new(),
            frames: vec![],
        }
    }

    pub(crate) fn start(&mut self) -> DiagnosticResult<()> {
        Ok(())
    }

    pub(crate) fn with_frame<T, F: Fn(&mut Self, &mut Workspace) -> InferResult<T>>(
        &mut self,
        workspace: &mut Workspace,
        frame: InferFrame,
        f: F,
    ) -> InferResult<T> {
        self.frames.push(frame);
        let result = f(self, workspace);
        self.frames.pop();
        result
    }
}

pub(crate) trait Infer
where
    Self: Sized,
{
    fn infer(&mut self, sess: &mut InferSess) -> InferResult<Self>;
}

impl Infer for ast::Binding {
    fn infer(&mut self, sess: &mut InferSess) -> InferResult<Self> {
        // TODO: support other patterns
        // let pat = self.pattern.as_single_ref();
        // let binding_ty = workspace.get_binding_info(pat.binding_info_id).unwrap().ty;

        // if let Some(ty_expr) = &mut self.ty_expr {
        //     let ty = ty_expr.infer(frame, tycx, workspace)?;
        //     ty.unify(&binding_ty, tycx, workspace)
        //         .map_err(|e| map_unify_err(e, ty, binding_ty, ty_expr.span, tycx))?;
        // }

        // if let Some(expr) = &mut self.expr {
        //     expr.infer(frame, tycx, workspace)?;

        //     binding_ty
        //         .unify(&expr.ty, tycx, workspace)
        //         .map_err(|e| map_unify_err(e, binding_ty, expr.ty, expr.span, tycx))?;
        // }

        // TODO: should i follow the rule of locality and solve each binding's types locally?
        // let binding_info_mut = workspace.get_binding_info_mut(pat.binding_info_id).unwrap();
        // if binding_info_mut.scope_level.is_global() {
        //     binding_info_mut.ty = substitute_ty(&binding_info_mut.ty, &tycx);
        // }

        // Ok(sess.tycx.common_types.unit)

        Ok((self.clone(), None))
    }
}
