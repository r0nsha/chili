use crate::{
    display::map_unify_err, normalize::NormalizeTy, tycx::TyCtx, unify::Unify,
    unpack_type::try_unpack_type,
};
use chili_ast::{ast, ty::*, workspace::Workspace};
use chili_error::DiagnosticResult;
use ustr::ustr;

#[derive(Debug, Default, Clone, Copy)]
pub(crate) struct InferFrame {
    return_ty: Ty,
}

pub(crate) trait Infer {
    fn infer(
        &mut self,
        frame: InferFrame,
        tycx: &mut TyCtx,
        workspace: &mut Workspace,
    ) -> DiagnosticResult<Ty>;
}

impl Infer for ast::Ast {
    fn infer(
        &mut self,
        frame: InferFrame,
        tycx: &mut TyCtx,
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
        tycx: &mut TyCtx,
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
        tycx: &mut TyCtx,
        workspace: &mut Workspace,
    ) -> DiagnosticResult<Ty> {
        // TODO: support other patterns
        let pat = self.pattern.as_single_ref();
        let binding_ty = workspace.get_binding_info(pat.binding_info_id).unwrap().ty;

        if let Some(ty_expr) = &mut self.ty_expr {
            let ty = ty_expr.infer(frame, tycx, workspace)?.normalize(tycx);
            let inner_type = try_unpack_type(&ty, tycx, ty_expr.span)?;

            inner_type
                .unify(&binding_ty, tycx, workspace)
                .map_err(|e| map_unify_err(e, inner_type, binding_ty, ty_expr.span, tycx))?;
        }

        if let Some(expr) = &mut self.expr {
            expr.infer(frame, tycx, workspace)?;
            binding_ty
                .unify(&expr.ty, tycx, workspace)
                .map_err(|e| map_unify_err(e, binding_ty, expr.ty, expr.span, tycx))?;
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
        tycx: &mut TyCtx,
        workspace: &mut Workspace,
    ) -> DiagnosticResult<Ty> {
        let proto_ty = self.proto.infer(frame, tycx, workspace)?;
        let fn_ty = proto_ty.normalize(tycx).into_fn();

        let return_ty = tycx.new_bound_variable(fn_ty.ret.as_ref().clone());

        let body_ty = self.body.infer(InferFrame { return_ty }, tycx, workspace)?;

        body_ty
            .unify(&return_ty, tycx, workspace)
            .map_err(|e| map_unify_err(e, return_ty, body_ty, self.body.span, tycx))?;

        Ok(proto_ty)
    }
}

impl Infer for ast::Proto {
    fn infer(
        &mut self,
        frame: InferFrame,
        tycx: &mut TyCtx,
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
        tycx: &mut TyCtx,
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
        tycx: &mut TyCtx,
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

                rty.unify(&lty, tycx, workspace)
                    .map_err(|e| map_unify_err(e, lty, rty, rvalue.span, tycx))?;

                tycx.primitive(TyKind::Unit)
            }
            ast::ExprKind::Cast(cast) => cast.infer(frame, tycx, workspace)?,
            ast::ExprKind::Builtin(builtin) => match builtin {
                ast::Builtin::SizeOf(expr) | ast::Builtin::AlignOf(expr) => {
                    let ty = expr.infer(frame, tycx, workspace)?;
                    try_unpack_type(&ty.into(), tycx, expr.span)?;
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
                    .unify(&TyKind::Bool, tycx, workspace)
                    .map_err(|e| map_unify_err(e, TyKind::Bool, cond_ty, cond.span, tycx))?;

                block.infer(frame, tycx, workspace)?;

                tycx.primitive(TyKind::Unit)
            }
            ast::ExprKind::For(for_) => {
                match &mut for_.iterator {
                    ast::ForIter::Range(start, end) => {
                        let start_ty = start.infer(frame, tycx, workspace)?;
                        let end_ty = end.infer(frame, tycx, workspace)?;

                        start_ty
                            .unify(&end_ty, tycx, workspace)
                            .map_err(|e| map_unify_err(e, start_ty, end_ty, end.span, tycx))?;
                    }
                    ast::ForIter::Value(value) => {
                        value.infer(frame, tycx, workspace)?;
                    }
                };

                workspace
                    .get_binding_info_mut(for_.iter_index_id)
                    .unwrap()
                    .ty = tycx.primitive(TyKind::UInt(UIntTy::Usize));

                for_.block.infer(frame, tycx, workspace)?;

                tycx.primitive(TyKind::Unit)
            }
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
                        .unify(&frame.return_ty, tycx, workspace)
                        .map_err(|e| map_unify_err(e, frame.return_ty, expr.ty, expr.span, tycx))?;
                } else {
                    TyKind::Unit
                        .unify(&frame.return_ty, tycx, workspace)
                        .map_err(|e| {
                            map_unify_err(e, TyKind::Unit, frame.return_ty, self.span, tycx)
                        })?;
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
            } => {
                let cond_ty = cond.infer(frame, tycx, workspace)?;

                cond_ty
                    .unify(&TyKind::Bool, tycx, workspace)
                    .map_err(|e| map_unify_err(e, TyKind::Bool, cond_ty, cond.span, tycx))?;

                let then_ty = then_expr.infer(frame, tycx, workspace)?;

                if let Some(else_expr) = else_expr {
                    let else_ty = else_expr.infer(frame, tycx, workspace)?;

                    else_ty
                        .unify(&then_ty, tycx, workspace)
                        .map_err(|e| map_unify_err(e, then_ty, else_ty, else_expr.span, tycx))?;

                    then_ty
                } else {
                    tycx.primitive(TyKind::Unit)
                }
            }
            ast::ExprKind::Block(block) => block.infer(frame, tycx, workspace)?,
            ast::ExprKind::Binary { lhs, op, rhs } => {
                let lty = lhs.infer(frame, tycx, workspace)?;
                let rty = rhs.infer(frame, tycx, workspace)?;

                lty.unify(&rty, tycx, workspace)
                    .map_err(|e| map_unify_err(e, lty, rty, rhs.span, tycx))?;

                match op {
                    ast::BinaryOp::Add
                    | ast::BinaryOp::Sub
                    | ast::BinaryOp::Mul
                    | ast::BinaryOp::Div
                    | ast::BinaryOp::Rem
                    | ast::BinaryOp::Shl
                    | ast::BinaryOp::Shr
                    | ast::BinaryOp::BitwiseAnd
                    | ast::BinaryOp::BitwiseOr
                    | ast::BinaryOp::BitwiseXor => lty,

                    ast::BinaryOp::Eq
                    | ast::BinaryOp::NEq
                    | ast::BinaryOp::Lt
                    | ast::BinaryOp::LtEq
                    | ast::BinaryOp::Gt
                    | ast::BinaryOp::GtEq
                    | ast::BinaryOp::And
                    | ast::BinaryOp::Or => tycx.primitive(TyKind::Bool),
                }
            }
            ast::ExprKind::Unary { op, lhs } => {
                let lty = lhs.infer(frame, tycx, workspace)?;

                match op {
                    ast::UnaryOp::Ref(is_mutable) => {
                        tycx.new_bound_variable(TyKind::Pointer(Box::new(lty.into()), *is_mutable))
                    }
                    ast::UnaryOp::Deref => tycx.new_variable(),
                    ast::UnaryOp::Not => tycx.primitive(TyKind::Bool),
                    ast::UnaryOp::Neg | ast::UnaryOp::Plus | ast::UnaryOp::BitwiseNot => lty,
                }
            }
            ast::ExprKind::Subscript { expr, index } => {
                expr.infer(frame, tycx, workspace)?;
                index.infer(frame, tycx, workspace)?;
                tycx.new_variable()
            }
            ast::ExprKind::Slice { expr, low, high } => {
                expr.infer(frame, tycx, workspace)?;

                let low_ty = if let Some(low) = low {
                    low.infer(frame, tycx, workspace)?
                } else {
                    tycx.new_variable()
                };

                if let Some(high) = high {
                    let high_ty = high.infer(frame, tycx, workspace)?;
                    low_ty
                        .unify(&high_ty, tycx, workspace)
                        .map_err(|e| map_unify_err(e, low_ty, high_ty, high.span, tycx))?;
                }

                tycx.new_variable()
            }
            ast::ExprKind::Call(call) => call.infer(frame, tycx, workspace)?,
            ast::ExprKind::MemberAccess { expr, member: _ } => {
                expr.infer(frame, tycx, workspace)?;
                tycx.new_variable()
            }
            ast::ExprKind::Id {
                binding_info_id, ..
            } => workspace
                .get_binding_info(*binding_info_id)
                .unwrap()
                .ty
                .clone(),
            ast::ExprKind::ArrayLiteral(lit) => match lit {
                ast::ArrayLiteralKind::List(elements) => {
                    let ty = tycx.new_variable();

                    for el in elements.iter_mut() {
                        let el_ty = el.infer(frame, tycx, workspace)?;
                        ty.unify(&el_ty, tycx, workspace)
                            .map_err(|e| map_unify_err(e, ty, el_ty, el.span, tycx))?;
                    }

                    tycx.new_bound_variable(TyKind::Array(Box::new(ty.into()), elements.len()))
                }
                ast::ArrayLiteralKind::Fill { len, expr } => {
                    len.infer(frame, tycx, workspace)?;
                    expr.infer(frame, tycx, workspace)?;
                    tycx.new_variable()
                }
            },
            ast::ExprKind::TupleLiteral(elements) => {
                let mut tys = vec![];

                for el in elements.iter_mut() {
                    let ty = el.infer(frame, tycx, workspace)?;
                    tys.push(ty.into());
                }

                let is_type = tys.iter().all(|ty: &TyKind| ty.normalize(tycx).is_type());

                tycx.new_bound_variable(if is_type {
                    TyKind::Tuple(tys).create_type()
                } else {
                    TyKind::Tuple(tys)
                })
            }
            ast::ExprKind::StructLiteral { type_expr, fields } => {
                if let Some(type_expr) = type_expr {
                    type_expr.infer(frame, tycx, workspace)?;
                }

                let mut ty_fields = vec![];

                for field in fields.iter_mut() {
                    let ty = field.value.infer(frame, tycx, workspace)?;
                    ty_fields.push(StructTyField {
                        symbol: field.symbol,
                        ty: ty.into(),
                        span: field.span,
                    });
                }

                tycx.new_bound_variable(TyKind::Struct(StructTy {
                    name: ustr(""),
                    qualified_name: ustr(""),
                    binding_info_id: Default::default(),
                    fields: ty_fields,
                    kind: StructTyKind::Struct,
                }))
            }
            ast::ExprKind::Literal(lit) => lit.infer(frame, tycx, workspace)?,
            ast::ExprKind::PointerType(inner, is_mutable) => {
                let ty = inner.infer(frame, tycx, workspace)?;
                try_unpack_type(&ty.into(), tycx, inner.span)?;
                tycx.new_bound_variable(
                    TyKind::Pointer(Box::new(ty.into()), *is_mutable).create_type(),
                )
            }
            ast::ExprKind::MultiPointerType(inner, is_mutable) => {
                let ty = inner.infer(frame, tycx, workspace)?;
                try_unpack_type(&ty.into(), tycx, inner.span)?;
                tycx.new_bound_variable(
                    TyKind::MultiPointer(Box::new(ty.into()), *is_mutable).create_type(),
                )
            }
            ast::ExprKind::ArrayType(inner, size) => {
                let ty = inner.infer(frame, tycx, workspace)?;
                try_unpack_type(&ty.into(), tycx, inner.span)?;
                size.infer(frame, tycx, workspace)?;
                let var = tycx.new_variable();
                tycx.new_bound_variable(TyKind::Var(var.into()).create_type())
                // TODO:
                // tycx.new_bound_variable(TyKind::Pointer(
                //     Box::new(ty.into()),
                //     size,
                // ).create_type())
            }
            ast::ExprKind::SliceType(inner, is_mutable) => {
                let ty = inner.infer(frame, tycx, workspace)?;
                try_unpack_type(&ty.into(), tycx, inner.span)?;
                tycx.new_bound_variable(
                    TyKind::Slice(Box::new(ty.into()), *is_mutable).create_type(),
                )
            }
            ast::ExprKind::StructType(st) => {
                let mut ty_fields = vec![];

                for field in st.fields.iter_mut() {
                    let ty = field.ty.infer(frame, tycx, workspace)?;
                    ty_fields.push(StructTyField {
                        symbol: field.name,
                        ty: ty.into(),
                        span: field.span,
                    });
                }

                tycx.new_bound_variable(
                    TyKind::Struct(StructTy {
                        name: st.name,
                        qualified_name: st.name,
                        binding_info_id: st.binding_info_id,
                        fields: ty_fields,
                        kind: st.kind,
                    })
                    .create_type(),
                )
            }
            ast::ExprKind::FnType(proto) => {
                let ty = proto.infer(frame, tycx, workspace)?;
                if proto.lib_name.is_some() {
                    ty
                } else {
                    tycx.new_bound_variable(TyKind::Type(Box::new(ty.into())))
                }
            }
            ast::ExprKind::SelfType => unimplemented!("Self type"),
            ast::ExprKind::NeverType => todo!(),
            ast::ExprKind::UnitType => tycx.new_bound_variable(TyKind::Unit.create_type()),
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
        tycx: &mut TyCtx,
        workspace: &mut Workspace,
    ) -> DiagnosticResult<Ty> {
        self.expr.infer(frame, tycx, workspace)?;

        self.target_ty = if let Some(ty_expr) = &mut self.ty_expr {
            let ty = ty_expr.infer(frame, tycx, workspace)?.normalize(tycx);
            let inner_type = try_unpack_type(&ty, tycx, ty_expr.span)?;
            tycx.new_bound_variable(inner_type)
        } else {
            tycx.new_variable()
        };

        Ok(self.target_ty)
    }
}

impl Infer for ast::Call {
    fn infer(
        &mut self,
        frame: InferFrame,
        tycx: &mut TyCtx,
        workspace: &mut Workspace,
    ) -> DiagnosticResult<Ty> {
        self.callee.infer(frame, tycx, workspace)?;
        for arg in self.args.iter_mut() {
            arg.value.infer(frame, tycx, workspace)?;
        }
        Ok(tycx.new_variable())
    }
}

impl Infer for ast::Literal {
    fn infer(
        &mut self,
        _: InferFrame,
        tycx: &mut TyCtx,
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
