use std::collections::HashMap;

use crate::{
    builtin::get_ty_for_builtin_type,
    display::{map_unify_err, DisplayTy},
    infer_top_level::InferTopLevel,
    normalize::NormalizeTy,
    tycx::TyCtx,
    unify::UnifyTy,
};
use chili_ast::{
    ast::{self, Expr, ExprKind},
    pattern::{Pattern, SymbolPattern},
    ty::*,
    value::Value,
    workspace::{BindingInfoFlags, BindingInfoId, Workspace},
};
use chili_error::{DiagnosticResult, TypeError};
use chili_span::Span;
use codespan_reporting::diagnostic::{Diagnostic, Label};
use common::builtin::{BUILTIN_FIELD_DATA, BUILTIN_FIELD_LEN};
use ustr::ustr;

pub struct InferSess<'s> {
    pub(crate) workspace: &'s mut Workspace,
    pub(crate) old_ast: &'s ast::ResolvedAst,
    pub(crate) new_ast: ast::ResolvedAst,
    pub(crate) tycx: TyCtx,
    pub(crate) const_bindings: HashMap<BindingInfoId, Value>,
    pub(crate) frames: Vec<InferFrame>,
}

pub(crate) type InferResult = DiagnosticResult<Res>;

pub(crate) struct Res {
    ty: Ty,
    const_value: Option<Value>,
}

impl Res {
    pub(crate) fn new(ty: Ty) -> Self {
        Self {
            ty,
            const_value: None,
        }
    }

    pub(crate) fn new_maybe_const(ty: Ty, const_value: Option<Value>) -> Self {
        Self { ty, const_value }
    }

    pub(crate) fn new_const(ty: Ty, const_value: Value) -> Self {
        Self {
            ty,
            const_value: Some(const_value),
        }
    }
}

#[derive(Debug, Default, Clone, Copy)]
pub(crate) struct InferFrame {
    return_ty: Ty,
    self_ty: Option<Ty>,
}

impl<'s> InferSess<'s> {
    pub(crate) fn new(workspace: &'s mut Workspace, old_ast: &'s ast::ResolvedAst) -> Self {
        Self {
            workspace,
            old_ast,
            new_ast: ast::ResolvedAst::new(),
            tycx: TyCtx::new(),
            const_bindings: HashMap::new(),
            frames: vec![],
        }
    }

    pub(crate) fn start(&mut self) -> DiagnosticResult<()> {
        // init builtin types
        for binding_info in self
            .workspace
            .binding_infos
            .iter_mut()
            .filter(|b| b.flags.contains(BindingInfoFlags::BUILTIN_TYPE))
        {
            let ty = get_ty_for_builtin_type(binding_info.symbol, &mut self.tycx);
            binding_info.ty = ty;
            self.const_bindings.insert(binding_info.id, Value::Type(ty));
        }

        for binding in self.old_ast.bindings.iter() {
            let first_symbol = binding.pattern.symbols().first().cloned().unwrap();

            let binding_info = self
                .workspace
                .get_binding_info(first_symbol.binding_info_id)
                .unwrap();

            if binding_info.ty != Ty::unknown() {
                continue;
            }

            binding.clone().infer_top_level(self)?;
        }

        Ok(())
    }

    pub(crate) fn with_frame<T, F: FnMut(&mut Self) -> T>(
        &mut self,
        frame: InferFrame,
        mut f: F,
    ) -> T {
        self.frames.push(frame);
        let result = f(self);
        self.frames.pop();
        result
    }

    pub(crate) fn frame(&self) -> Option<InferFrame> {
        self.frames.last().map(|&f| f)
    }

    pub(crate) fn expect_const_type(
        &self,
        value: Option<Value>,
        ty: Ty,
        span: Span,
    ) -> DiagnosticResult<Ty> {
        match value {
            Some(v) => {
                if let Value::Type(t) = v {
                    return Ok(t);
                }
            }
            None => (),
        }

        Err(TypeError::expected(
            span,
            ty.normalize(&self.tycx).display(&self.tycx),
            "a type",
        ))
    }
}

pub(crate) trait Infer
where
    Self: Sized,
{
    fn infer(&mut self, sess: &mut InferSess) -> InferResult;
}

impl Infer for ast::Import {
    fn infer(&mut self, sess: &mut InferSess) -> InferResult {
        todo!()
    }
}

impl Infer for ast::Binding {
    fn infer(&mut self, sess: &mut InferSess) -> InferResult {
        // TODO: support other patterns
        let pat = self.pattern.as_single_ref();
        let binding_ty = sess.tycx.var();

        sess.workspace
            .get_binding_info_mut(pat.binding_info_id)
            .unwrap()
            .ty = binding_ty;

        if let Some(ty_expr) = &mut self.ty_expr {
            let res = ty_expr.infer(sess)?;
            res.ty
                .unify(&binding_ty, sess)
                .map_err(|e| map_unify_err(e, res.ty, binding_ty, ty_expr.span, &sess.tycx))?;
        }

        let const_value = if let Some(expr) = &mut self.expr {
            let res = expr.infer(sess)?;
            res.ty
                .unify(&binding_ty, sess)
                .map_err(|e| map_unify_err(e, binding_ty, res.ty, expr.span, &sess.tycx))?;
            res.const_value
        } else {
            None
        };

        // don't allow const values with mutable bindings or patterns that are not `Single`
        let const_value = match &self.pattern {
            Pattern::Single(SymbolPattern { is_mutable, .. }) => {
                if *is_mutable {
                    None
                } else {
                    const_value
                }
            }
            Pattern::StructDestructor(_) | Pattern::TupleDestructor(_) => None,
        };

        if let Some(const_value) = const_value {
            sess.const_bindings.insert(pat.binding_info_id, const_value);
        }

        Ok(Res {
            ty: sess.tycx.common_types.unit,
            const_value: None,
        })
    }
}

impl Infer for ast::Fn {
    fn infer(&mut self, sess: &mut InferSess) -> InferResult {
        let res = self.proto.infer(sess)?;

        let fn_ty = sess.tycx.ty_kind(res.ty);
        let fn_ty = fn_ty.as_fn();

        let return_ty = sess.tycx.bound(fn_ty.ret.as_ref().clone());

        let body_res = sess.with_frame(
            InferFrame {
                return_ty,
                self_ty: sess.frame().map(|f| f.self_ty).flatten(),
            },
            |sess| self.body.infer(sess),
        )?;

        body_res
            .ty
            .unify(&return_ty, sess)
            .map_err(|e| map_unify_err(e, return_ty, body_res.ty, self.body.span, &sess.tycx))?;

        Ok(Res::new(res.ty))
    }
}

impl Infer for ast::Proto {
    fn infer(&mut self, sess: &mut InferSess) -> InferResult {
        let mut ty_params = vec![];

        for param in self.params.iter_mut() {
            // TODO: support other patterns
            let pat = param.pattern.as_single_ref();

            let ty = if let Some(ty_expr) = &mut param.ty {
                ty_expr.infer(sess)?.ty
            } else {
                sess.tycx.var()
            };

            sess.workspace
                .get_binding_info_mut(pat.binding_info_id)
                .unwrap()
                .ty = ty;

            ty_params.push(FnTyParam {
                symbol: pat.symbol,
                ty: ty.into(),
            });
        }

        let ret = if let Some(ret) = &mut self.ret {
            ret.infer(sess)?.ty
        } else {
            sess.tycx.var()
        };

        self.ty = sess.tycx.bound(TyKind::Fn(FnTy {
            params: ty_params,
            ret: Box::new(ret.into()),
            variadic: self.variadic,
            lib_name: self.lib_name,
        }));

        Ok(Res {
            ty: self.ty,
            const_value: None,
        })
    }
}

impl Infer for ast::Expr {
    fn infer(&mut self, sess: &mut InferSess) -> InferResult {
        let res = match &mut self.kind {
            ast::ExprKind::Import(_) => todo!(),
            ast::ExprKind::Foreign(_) => todo!(),
            ast::ExprKind::Binding(binding) => binding.infer(sess),
            ast::ExprKind::Defer(_) => todo!(),
            ast::ExprKind::Assign { lvalue, rvalue } => todo!(),
            ast::ExprKind::Cast(_) => todo!(),
            ast::ExprKind::Builtin(_) => todo!(),
            ast::ExprKind::Fn(f) => f.infer(sess),
            ast::ExprKind::While { cond, block } => todo!(),
            ast::ExprKind::For(_) => todo!(),
            ast::ExprKind::Break { deferred } => todo!(),
            ast::ExprKind::Continue { deferred } => todo!(),
            ast::ExprKind::Return { expr, deferred } => todo!(),
            ast::ExprKind::If {
                cond,
                then_expr,
                else_expr,
            } => todo!(),
            ast::ExprKind::Block(_) => todo!(),
            ast::ExprKind::Binary { lhs, op, rhs } => todo!(),
            ast::ExprKind::Unary { op, lhs } => todo!(),
            ast::ExprKind::Subscript { expr, index } => todo!(),
            ast::ExprKind::Slice { expr, low, high } => todo!(),
            ast::ExprKind::Call(call) => call.infer(sess),
            ast::ExprKind::MemberAccess { expr, member } => {
                let res = expr.infer(sess)?;
                let kind = res.ty.normalize(&sess.tycx);

                match &kind.maybe_deref_once() {
                    ty @ TyKind::Tuple(tys) => match member.as_str().parse::<i32>() {
                        Ok(index) => match tys.get(index as usize) {
                            Some(field_ty) => Ok(Res::new(sess.tycx.bound(field_ty.clone()))),
                            None => Err(TypeError::tuple_field_out_of_bounds(
                                expr.span,
                                &member,
                                ty.to_string(),
                                tys.len() - 1,
                            )),
                        },
                        Err(_) => Err(TypeError::non_numeric_tuple_field(
                            expr.span,
                            &member,
                            ty.to_string(),
                        )),
                    },
                    TyKind::Struct(ty) => match ty.fields.iter().find(|f| f.symbol == *member) {
                        Some(field) => Ok(Res::new(sess.tycx.bound(field.ty.clone()))),
                        None => Err(TypeError::invalid_struct_field(
                            expr.span,
                            *member,
                            ty.to_string(),
                        )),
                    },
                    TyKind::Array(_, size) if member.as_str() == BUILTIN_FIELD_LEN => Ok(
                        Res::new_const(sess.tycx.common_types.uint, Value::Int(*size as _)),
                    ),
                    TyKind::Slice(..) if member.as_str() == BUILTIN_FIELD_LEN => {
                        Ok(Res::new(sess.tycx.common_types.uint))
                    }
                    TyKind::Slice(inner, is_mutable) if member.as_str() == BUILTIN_FIELD_DATA => {
                        Ok(Res::new(
                            sess.tycx
                                .bound(TyKind::MultiPointer(inner.clone(), *is_mutable)),
                        ))
                    }
                    TyKind::Module(module_id) => {
                        let binding_info_id = {
                            let info = sess
                                .workspace
                                .find_binding_info_by_name(*module_id, *member);

                            match info {
                                Some(info) => info.id,
                                None => {
                                    let module_info =
                                        sess.workspace.get_module_info(*module_id).unwrap();

                                    return Err(Diagnostic::error()
                                        .with_message(format!(
                                            "cannot find value `{}` in module `{}`",
                                            member, module_info.name
                                        ))
                                        .with_labels(vec![Label::primary(
                                            self.span.file_id,
                                            self.span.range(),
                                        )
                                        .with_message(format!(
                                            "not found in `{}`",
                                            module_info.name
                                        ))]));
                                }
                            }
                        };

                        sess.infer_binding_by_id(binding_info_id)
                    }
                    ty => Err(TypeError::field_access_on_invalid_type(
                        expr.span,
                        ty.to_string(),
                    )),
                }
            }
            ast::ExprKind::Id {
                binding_info_id, ..
            } => sess.infer_binding_by_id(*binding_info_id),
            ast::ExprKind::ArrayLiteral(_) => todo!(),
            ast::ExprKind::TupleLiteral(_) => todo!(),
            ast::ExprKind::StructLiteral { type_expr, fields } => todo!(),
            ast::ExprKind::Literal(lit) => lit.infer(sess),
            ast::ExprKind::PointerType(inner, is_mutable) => {
                let res = inner.infer(sess)?;
                let inner_kind = sess.expect_const_type(res.const_value, res.ty, inner.span)?;
                let kind = TyKind::Pointer(Box::new(inner_kind.into()), *is_mutable);
                Ok(Res::new_const(
                    sess.tycx.bound(kind.clone().create_type()),
                    Value::Type(sess.tycx.bound(kind.clone())),
                ))
            }
            ast::ExprKind::MultiPointerType(inner, is_mutable) => {
                let res = inner.infer(sess)?;
                let inner_kind = sess.expect_const_type(res.const_value, res.ty, inner.span)?;
                let kind = TyKind::MultiPointer(Box::new(inner_kind.into()), *is_mutable);
                Ok(Res::new_const(
                    sess.tycx.bound(kind.clone().create_type()),
                    Value::Type(sess.tycx.bound(kind.clone())),
                ))
            }
            ast::ExprKind::ArrayType(_, _) => todo!(),
            ast::ExprKind::SliceType(inner, is_mutable) => {
                let res = inner.infer(sess)?;
                let inner_kind = sess.expect_const_type(res.const_value, res.ty, inner.span)?;
                let kind = TyKind::Slice(Box::new(inner_kind.into()), *is_mutable);
                Ok(Res::new_const(
                    sess.tycx.bound(kind.clone().create_type()),
                    Value::Type(sess.tycx.bound(kind.clone())),
                ))
            }
            ast::ExprKind::StructType(_) => todo!(),
            ast::ExprKind::FnType(proto) => proto.infer(sess),
            ast::ExprKind::SelfType => {
                let self_ty = sess.frame().map(|f| f.self_ty).flatten();
                match self_ty {
                    Some(ty) => Ok(Res::new_const(
                        sess.tycx.bound(TyKind::Var(ty).create_type()),
                        Value::Type(ty),
                    )),
                    None => Err(Diagnostic::error()
                        .with_message("`Self` is only available within struct definitions")
                        .with_labels(vec![Label::primary(
                            self.span.file_id,
                            self.span.range().clone(),
                        )])),
                }
            }
            ast::ExprKind::NeverType => {
                let ty = sess.tycx.common_types.never;
                Ok(Res::new_const(
                    sess.tycx.bound(TyKind::Var(ty).create_type()),
                    Value::Type(ty),
                ))
            }
            ast::ExprKind::UnitType => {
                let ty = sess.tycx.common_types.unit;
                Ok(Res::new_const(
                    sess.tycx.bound(TyKind::Var(ty).create_type()),
                    Value::Type(ty),
                ))
            }
            ast::ExprKind::PlaceholderType => {
                let ty = sess.tycx.var();
                Ok(Res::new_const(
                    sess.tycx.bound(TyKind::Var(ty).create_type()),
                    Value::Type(ty),
                ))
            }
            ast::ExprKind::Noop => Ok(Res::new(sess.tycx.var())),
        }?;

        self.ty = res.ty;

        Ok(res)
    }
}

impl Infer for ast::Call {
    fn infer(&mut self, sess: &mut InferSess) -> InferResult {
        for arg in self.args.iter_mut() {
            arg.expr.infer(sess)?;
        }

        let callee_res = self.callee.infer(sess)?;
        let return_ty = sess.tycx.var();

        let fn_kind = TyKind::Fn(FnTy {
            params: self
                .args
                .iter()
                .map(|arg| FnTyParam {
                    symbol: arg.symbol.as_ref().map_or(ustr(""), |s| s.value),
                    ty: arg.expr.ty.into(),
                })
                .collect(),
            ret: Box::new(return_ty.into()),
            variadic: false,
            lib_name: None,
        });

        callee_res
            .ty
            .unify(&fn_kind, sess)
            .map_err(|e| map_unify_err(e, fn_kind, callee_res.ty, self.callee.span, &sess.tycx))?;

        Ok(Res::new(return_ty))
    }
}

impl Infer for ast::Block {
    fn infer(&mut self, sess: &mut InferSess) -> InferResult {
        let mut ty = sess.tycx.common_types.unit;
        let mut const_value = None;

        for expr in self.exprs.iter_mut() {
            let res = expr.infer(sess)?;
            ty = res.ty;
            const_value = res.const_value;
        }

        for expr in self.deferred.iter_mut() {
            expr.infer(sess)?;
        }

        Ok(Res::new_maybe_const(ty, const_value))
    }
}

impl Infer for ast::Literal {
    fn infer(&mut self, sess: &mut InferSess) -> InferResult {
        let res = match self {
            ast::Literal::Unit => Res::new(sess.tycx.common_types.unit),
            ast::Literal::Nil => Res::new(sess.tycx.var()),
            ast::Literal::Bool(b) => Res::new_const(sess.tycx.common_types.bool, Value::Bool(*b)),
            ast::Literal::Int(i) => Res::new_const(sess.tycx.anyint(), Value::Int(*i)),
            ast::Literal::Float(f) => Res::new_const(sess.tycx.anyfloat(), Value::Float(*f)),
            ast::Literal::Str(_) => Res::new(sess.tycx.common_types.str),
            ast::Literal::Char(_) => Res::new(sess.tycx.common_types.u8),
        };
        Ok(res)
    }
}
