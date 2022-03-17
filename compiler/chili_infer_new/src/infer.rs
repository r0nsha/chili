use crate::{
    display::map_unify_err, normalize::NormalizeTy, tycx::TyContext, unify::Unify,
    unpack_type::try_unpack_type,
};
use chili_ast::{ast, ty::*, workspace::Workspace};
use chili_error::DiagnosticResult;

#[derive(Debug, Default, Clone, Copy)]
pub(crate) struct InferFrame {
    return_ty: Ty,
}

pub(crate) trait Infer {
    fn infer(
        &mut self,
        frame: InferFrame,
        tycx: &mut TyContext,
        workspace: &mut Workspace,
    ) -> DiagnosticResult<Ty>;
}

impl Infer for ast::Ast {
    fn infer(
        &mut self,
        frame: InferFrame,
        tycx: &mut TyContext,
        workspace: &mut Workspace,
    ) -> DiagnosticResult<Ty> {
        for import in self.imports.iter_mut() {
            import.infer(frame, tycx, workspace)?;
        }

        for binding in self.bindings.iter_mut() {
            binding.infer(frame, tycx, workspace)?;
        }

        Ok(tycx.primitive(TyKind::Unit))
    }
}

impl Infer for ast::Import {
    fn infer(
        &mut self,
        frame: InferFrame,
        tycx: &mut TyContext,
        workspace: &mut Workspace,
    ) -> DiagnosticResult<Ty> {
        // TODO:
        Ok(tycx.primitive(TyKind::Unit))
    }
}

impl Infer for ast::Binding {
    fn infer(
        &mut self,
        frame: InferFrame,
        tycx: &mut TyContext,
        workspace: &mut Workspace,
    ) -> DiagnosticResult<Ty> {
        // TODO: support other patterns
        let pat = self.pattern.as_single_ref();
        let binding_ty = workspace.get_binding_info(pat.binding_info_id).unwrap().ty;

        if let Some(ty_expr) = &mut self.ty_expr {
            let ty = ty_expr.infer(frame, tycx, workspace)?.normalize(tycx);
            let inner_type = try_unpack_type(&ty, tycx, ty_expr.span)?;

            inner_type
                .unify(&binding_ty, tycx, workspace, ty_expr.span)
                .map_err(map_unify_err)?;
        }

        if let Some(expr) = &mut self.expr {
            expr.infer(frame, tycx, workspace)?;
            binding_ty
                .unify(&expr.ty, tycx, workspace, expr.span)
                .map_err(map_unify_err)?;
        }

        // TODO: should i follow the rule of locality and solve each binding's types locally?
        // let binding_info_mut = workspace.get_binding_info_mut(pat.binding_info_id).unwrap();
        // if binding_info_mut.scope_level.is_global() {
        //     binding_info_mut.ty = substitute_ty(&binding_info_mut.ty, &tycx);
        // }

        Ok(tycx.primitive(TyKind::Unit))
    }
}

impl Infer for ast::Fn {
    fn infer(
        &mut self,
        frame: InferFrame,
        tycx: &mut TyContext,
        workspace: &mut Workspace,
    ) -> DiagnosticResult<Ty> {
        let proto_ty = self.proto.infer(frame, tycx, workspace)?;
        let fn_ty = proto_ty.normalize(tycx).into_fn();

        let return_ty = tycx.new_bound_variable(fn_ty.ret.as_ref().clone());

        let body_ty = self.body.infer(InferFrame { return_ty }, tycx, workspace)?;

        body_ty
            .unify(&return_ty, tycx, workspace, self.body.span)
            .map_err(map_unify_err)?;

        Ok(proto_ty)
    }
}

impl Infer for ast::Proto {
    fn infer(
        &mut self,
        frame: InferFrame,
        tycx: &mut TyContext,
        workspace: &mut Workspace,
    ) -> DiagnosticResult<Ty> {
        let mut params = vec![];

        for param in self.params.iter_mut() {
            // TODO: support other patterns
            let pat = param.pattern.as_single_ref();
            let binding_info = workspace.get_binding_info_mut(pat.binding_info_id).unwrap();

            // TODO: param type annotation
            binding_info.ty = tycx.new_variable().into();

            params.push(FnTyParam {
                symbol: pat.symbol,
                ty: binding_info.ty.into(),
            })
        }

        let ret = if let Some(ret) = &mut self.ret {
            let ty = ret.infer(frame, tycx, workspace)?.normalize(tycx);
            let inner_type = try_unpack_type(&ty, tycx, ret.span)?;
            tycx.new_bound_variable(inner_type).into()
        } else {
            tycx.new_variable().into()
        };

        let ty = tycx.new_bound_variable(TyKind::Fn(FnTy {
            params,
            ret: Box::new(ret),
            variadic: self.variadic,
            lib_name: self.lib_name,
        }));

        Ok(ty)
    }
}

impl Infer for ast::Block {
    fn infer(
        &mut self,
        frame: InferFrame,
        tycx: &mut TyContext,
        workspace: &mut Workspace,
    ) -> DiagnosticResult<Ty> {
        let mut result_ty = tycx.primitive(TyKind::Unit);

        for expr in self.exprs.iter_mut() {
            result_ty = expr.infer(frame, tycx, workspace)?;
        }

        for expr in self.deferred.iter_mut() {
            expr.infer(frame, tycx, workspace)?;
        }

        Ok(result_ty)
    }
}

impl Infer for ast::Expr {
    fn infer(
        &mut self,
        frame: InferFrame,
        tycx: &mut TyContext,
        workspace: &mut Workspace,
    ) -> DiagnosticResult<Ty> {
        self.ty = match &mut self.kind {
            ast::ExprKind::Import(imports) => {
                for import in imports.iter_mut() {
                    import.infer(frame, tycx, workspace)?;
                }
                tycx.primitive(TyKind::Unit)
            }
            ast::ExprKind::Foreign(bindings) => {
                for binding in bindings.iter_mut() {
                    binding.infer(frame, tycx, workspace)?;
                }
                tycx.primitive(TyKind::Unit)
            }
            ast::ExprKind::Binding(binding) => {
                binding.infer(frame, tycx, workspace)?;
                tycx.primitive(TyKind::Unit)
            }
            ast::ExprKind::Defer(_) => todo!(),
            ast::ExprKind::Assign { lvalue, rvalue } => {
                let lty = lvalue.infer(frame, tycx, workspace)?;
                let rty = rvalue.infer(frame, tycx, workspace)?;

                rty.unify(&lty, tycx, workspace, rvalue.span)
                    .map_err(map_unify_err)?;

                tycx.primitive(TyKind::Unit)
            }
            ast::ExprKind::Cast(cast) => cast.infer(frame, tycx, workspace)?,
            ast::ExprKind::Builtin(builtin) => match builtin {
                ast::Builtin::SizeOf(expr) | ast::Builtin::AlignOf(expr) => {
                    expr.infer(frame, tycx, workspace)?;
                    tycx.primitive(TyKind::UInt(UIntTy::Usize))
                }
                ast::Builtin::Panic(expr) => {
                    if let Some(expr) = expr {
                        expr.infer(frame, tycx, workspace)?;
                    }
                    tycx.primitive(TyKind::Unit)
                }
            },
            ast::ExprKind::Fn(f) => f.infer(frame, tycx, workspace)?,
            ast::ExprKind::While { cond, block } => {
                let cond_ty = cond.infer(frame, tycx, workspace)?;
                cond_ty
                    .unify(&TyKind::Bool, tycx, workspace, cond.span)
                    .map_err(map_unify_err)?;
                block.infer(frame, tycx, workspace)?;
                tycx.primitive(TyKind::Unit)
            }
            ast::ExprKind::For(_) => todo!(),
            ast::ExprKind::Break { deferred } | ast::ExprKind::Continue { deferred } => {
                for expr in deferred.iter_mut() {
                    expr.infer(frame, tycx, workspace)?;
                }
                tycx.primitive(TyKind::Never)
            }
            ast::ExprKind::Return { expr, deferred } => {
                if let Some(expr) = expr {
                    expr.infer(frame, tycx, workspace)?;
                    expr.ty
                        .unify(&frame.return_ty, tycx, workspace, expr.span)
                        .map_err(map_unify_err)?;
                } else {
                    TyKind::Unit
                        .unify(&frame.return_ty, tycx, workspace, self.span)
                        .map_err(map_unify_err)?;
                }

                for expr in deferred.iter_mut() {
                    expr.infer(frame, tycx, workspace)?;
                }

                tycx.primitive(TyKind::Never)
            }
            ast::ExprKind::If {
                cond,
                then_expr,
                else_expr,
            } => todo!(),
            ast::ExprKind::Block(_) => todo!(),
            ast::ExprKind::Binary { lhs, op, rhs } => {
                todo!("Hello Binary");
            }
            ast::ExprKind::Unary { op, lhs } => todo!(),
            ast::ExprKind::Subscript { expr, index } => todo!(),
            ast::ExprKind::Slice { expr, low, high } => todo!(),
            ast::ExprKind::Call(_) => todo!(),
            ast::ExprKind::MemberAccess { expr, member } => todo!(),
            // TODO: this is wrong, and will not work for more complex scenarios
            // TODO: i have two options:
            // TODO: 1) create a `typed ast`, and resolve the binding ad-hoc.
            // TODO:    this is the same solution from my previous check pass which worked really well!
            // TODO: 2) do the infer-unify-substitute loop until everything is covered.
            ast::ExprKind::Id {
                binding_info_id, ..
            } => workspace
                .get_binding_info(*binding_info_id)
                .unwrap()
                .ty
                .clone(),
            ast::ExprKind::ArrayLiteral(_) => todo!(),
            ast::ExprKind::TupleLiteral(_) => todo!(),
            ast::ExprKind::StructLiteral { type_expr, fields } => todo!(),
            ast::ExprKind::Literal(lit) => lit.infer(frame, tycx, workspace)?,
            ast::ExprKind::PointerType(_, _) => todo!(),
            ast::ExprKind::MultiPointerType(_, _) => todo!(),
            ast::ExprKind::ArrayType(_, _) => todo!(),
            ast::ExprKind::SliceType(_, _) => todo!(),
            ast::ExprKind::StructType(_) => todo!(),
            ast::ExprKind::FnType(_) => todo!(),
            ast::ExprKind::SelfType => todo!(),
            ast::ExprKind::NeverType => todo!(),
            ast::ExprKind::UnitType => todo!(),
            ast::ExprKind::PlaceholderType => todo!(),
            ast::ExprKind::Noop => todo!(),
        };

        Ok(self.ty.clone())
    }
}

impl Infer for ast::Cast {
    fn infer(
        &mut self,
        frame: InferFrame,
        tycx: &mut TyContext,
        workspace: &mut Workspace,
    ) -> DiagnosticResult<Ty> {
        self.expr.infer(frame, tycx, workspace)?;
        if let Some(ty_expr) = &mut self.ty_expr {
            let ty = ty_expr.infer(frame, tycx, workspace)?.normalize(tycx);
            let inner_type = try_unpack_type(&ty, tycx, ty_expr.span)?;
            let ty = tycx.new_bound_variable(inner_type);

            self.target_ty = ty;

            Ok(ty)
        } else {
            Ok(tycx.new_variable())
        }
    }
}

impl Infer for ast::Literal {
    fn infer(
        &mut self,
        frame: InferFrame,
        tycx: &mut TyContext,
        _: &mut Workspace,
    ) -> DiagnosticResult<Ty> {
        let ty = match self {
            ast::Literal::Unit => tycx.primitive(TyKind::Unit),
            ast::Literal::Nil => tycx.new_bound_variable(TyKind::raw_pointer(true)),
            ast::Literal::Bool(_) => tycx.primitive(TyKind::Bool),
            ast::Literal::Int(_) => {
                let var = tycx.new_variable();
                tycx.new_bound_variable(TyKind::AnyInt(var))
            }
            ast::Literal::Float(_) => {
                let var = tycx.new_variable();
                tycx.new_bound_variable(TyKind::AnyFloat(var))
            }
            ast::Literal::Str(_) => tycx.str_primitive(),
            ast::Literal::Char(_) => tycx.primitive(TyKind::char()),
        };
        Ok(ty)
    }
}
