use crate::{
    coerce::{can_coerce_mut, Coerce, CoercionResult, TryCoerce},
    constraint::Constraint,
    sess::InferSess,
    ty::TyVar,
};
use chili_ast::ast::Expr;
use chili_ast::ty::*;
use chili_error::{DiagnosticResult, TypeError};
use chili_span::Span;
use codespan_reporting::diagnostic::Diagnostic;
use ena::unify::UnifyValue;

pub(crate) struct UnifyError(pub TyKind, pub TyKind);

impl UnifyValue for Constraint {
    type Error = UnifyError;

    fn unify_values(a: &Self, b: &Self) -> Result<Self, Self::Error> {
        use Constraint::*;
        match (a, b) {
            (Unbound, Unbound) => Ok(Unbound),
            (Unbound, t @ Bound(_)) | (t @ Bound(_), Unbound) => Ok(t.clone()),
            (Bound(t1), Bound(t2)) => {
                panic!("can't unify two bound variables {} and {}", t1, t2)
            }
        }
    }
}

impl InferSess {
    pub(crate) fn unify_or_coerce_expr_expr(
        &mut self,
        left_expr: &mut Expr,
        right_expr: &mut Expr,
        span: Span,
    ) -> DiagnosticResult<TyKind> {
        match self.unify_ty_ty(&left_expr.ty, &right_expr.ty, span) {
            Ok(ty) => Ok(ty),
            Err(_) => {
                let left_expr_ty = self.normalize_ty(&left_expr.ty);
                let right_expr_ty = self.normalize_ty(&right_expr.ty);

                match left_expr_ty.try_coerce(&right_expr_ty, self.word_size) {
                    CoercionResult::CoerceToLeft => {
                        *right_expr = right_expr.coerce(left_expr_ty.clone());
                        Ok(left_expr_ty)
                    }
                    CoercionResult::CoerceToRight => {
                        *left_expr = left_expr.coerce(right_expr_ty.clone());
                        Ok(right_expr_ty)
                    }
                    CoercionResult::NoCoercion => {
                        Err(self.map_err(UnifyError(left_expr_ty, right_expr_ty), span))
                    }
                }
            }
        }
    }

    pub(crate) fn unify_or_coerce_ty_expr(
        &mut self,
        ty: &TyKind,
        expr: &mut Expr,
    ) -> DiagnosticResult<TyKind> {
        match self.unify_ty_ty(ty, &expr.ty, expr.span) {
            Ok(ty) => Ok(ty),
            Err(_) => {
                let ty = self.normalize_ty(ty);
                let expr_ty = self.normalize_ty(&expr.ty);

                match expr_ty.try_coerce(&ty, self.word_size) {
                    CoercionResult::CoerceToRight => {
                        *expr = expr.coerce(ty.clone());
                        Ok(ty)
                    }
                    CoercionResult::CoerceToLeft | CoercionResult::NoCoercion => {
                        Err(self.map_err(UnifyError(ty, expr_ty), expr.span))
                    }
                }
            }
        }
    }

    pub(crate) fn unify(
        &mut self,
        expected: impl Into<TyKind>,
        actual: impl Into<TyKind>,
        span: Span,
    ) -> DiagnosticResult<TyKind> {
        let expected: TyKind = expected.into();
        let actual: TyKind = actual.into();

        self.unify_ty_ty(&expected, &actual, span)
            .map_err(|_| self.map_err(UnifyError(expected, actual), span))
    }

    pub(crate) fn unify_ty_ty(
        &mut self,
        expected: &TyKind,
        actual: &TyKind,
        span: Span,
    ) -> Result<TyKind, UnifyError> {
        match (expected, actual) {
            (TyKind::Unit, TyKind::Unit)
            | (TyKind::Bool, TyKind::Bool)
            | (TyKind::Int(IntTy::I8), TyKind::Int(IntTy::I8))
            | (TyKind::Int(IntTy::I16), TyKind::Int(IntTy::I16))
            | (TyKind::Int(IntTy::I32), TyKind::Int(IntTy::I32))
            | (TyKind::Int(IntTy::I64), TyKind::Int(IntTy::I64))
            | (TyKind::Int(IntTy::Isize), TyKind::Int(IntTy::Isize))
            | (TyKind::UInt(UIntTy::U8), TyKind::UInt(UIntTy::U8))
            | (TyKind::UInt(UIntTy::U16), TyKind::UInt(UIntTy::U16))
            | (TyKind::UInt(UIntTy::U32), TyKind::UInt(UIntTy::U32))
            | (TyKind::UInt(UIntTy::U64), TyKind::UInt(UIntTy::U64))
            | (TyKind::UInt(UIntTy::Usize), TyKind::UInt(UIntTy::Usize))
            | (TyKind::Float(FloatTy::F16), TyKind::Float(FloatTy::F16))
            | (TyKind::Float(FloatTy::F32), TyKind::Float(FloatTy::F32))
            | (TyKind::Float(FloatTy::F64), TyKind::Float(FloatTy::F64))
            | (TyKind::Float(FloatTy::Fsize), TyKind::Float(FloatTy::Fsize))
            | (TyKind::Never, TyKind::Never) => Ok(expected.clone()),

            (TyKind::Pointer(t1, m1), TyKind::Pointer(t2, m2)) => {
                if !can_coerce_mut(*m1, *m2) {
                    return Err(UnifyError(expected.clone(), actual.clone()));
                }

                let unified = self.unify_ty_ty(t1, t2, span)?;
                Ok(TyKind::Pointer(Box::new(unified), *m1))
            }

            (TyKind::MultiPointer(t1, m1), TyKind::MultiPointer(t2, m2)) => {
                if !can_coerce_mut(*m1, *m2) {
                    return Err(UnifyError(expected.clone(), actual.clone()));
                }

                let unified = self.unify_ty_ty(t1, t2, span)?;
                Ok(TyKind::MultiPointer(Box::new(unified), *m1))
            }

            (TyKind::Var(v1), TyKind::Var(v2)) => {
                let v1 = TyVar::from(*v1);
                let v2 = TyVar::from(*v2);

                match (self.value_of(v1), self.value_of(v2)) {
                    (Constraint::Bound(t1), Constraint::Bound(t2)) => {
                        self.unify_ty_ty(&t1, &t2, span)
                    }
                    _ => {
                        self.table.unify_var_var(v1, v2)?;
                        Ok(expected.clone())
                    }
                }
            }

            (TyKind::Var(var), ty) | (ty, TyKind::Var(var)) => {
                match self.value_of(TyVar::from(*var)) {
                    Constraint::Bound(expected) => self.unify_ty_ty(&expected, ty, span),
                    Constraint::Unbound => {
                        self.table
                            .unify_var_value(TyVar::from(*var), Constraint::Bound(ty.clone()))?;
                        Ok(ty.clone())
                    }
                }
            }

            (TyKind::Fn(fn_a), TyKind::Fn(fn_b)) => {
                if fn_a.variadic != fn_b.variadic || fn_a.params.len() != fn_b.params.len() {
                    return Err(UnifyError(expected.clone(), actual.clone()));
                }

                self.unify_ty_ty(&fn_a.ret, &fn_b.ret, span)?;

                for (p1, p2) in fn_a.params.iter().zip(fn_b.params.iter()) {
                    self.unify_ty_ty(&p1.ty, &p2.ty, span)?;
                }

                Ok(expected.clone())
            }

            (TyKind::Array(inner_a, len_a), TyKind::Array(inner_b, len_b)) => {
                if len_a != len_b {
                    return Err(UnifyError(expected.clone(), actual.clone()));
                }

                let unified = self.unify_ty_ty(inner_a, inner_b, span)?;

                Ok(TyKind::Array(Box::new(unified), *len_a))
            }

            (TyKind::Slice(inner_a, m1), TyKind::Slice(inner_b, m2)) => {
                if !can_coerce_mut(*m1, *m2) {
                    return Err(UnifyError(expected.clone(), actual.clone()));
                }

                let unified = self.unify_ty_ty(inner_a, inner_b, span)?;
                Ok(TyKind::Slice(Box::new(unified), *m1))
            }

            (TyKind::Tuple(tys_a), TyKind::Tuple(tys_b)) => {
                if tys_a.len() != tys_b.len() {
                    return Err(UnifyError(expected.clone(), actual.clone()));
                }

                let mut unified_tys = vec![];

                for (t1, t2) in tys_a.iter().zip(tys_b.iter()) {
                    let unified = self.unify_ty_ty(t1, t2, span)?;
                    unified_tys.push(unified);
                }

                Ok(TyKind::Tuple(unified_tys))
            }

            (TyKind::Struct(t1), TyKind::Struct(t2)) => {
                if t1.qualified_name == t2.qualified_name {
                    Ok(expected.clone())
                } else {
                    Err(UnifyError(expected.clone(), actual.clone()))
                }
            }

            (TyKind::Never, t) | (t, TyKind::Never) => Ok(t.clone()),

            // (Unbound, v @ AnyInt)
            // | (v @ AnyInt, Unbound)
            // | (Unbound, v @ Float)
            // | (v @ Float, Unbound)
            // | (Unbound, v @ Bound(_))
            // | (v @ Bound(_), Unbound) => Ok(v.clone()),

            // (AnyInt, AnyInt) => Ok(AnyInt),

            // (AnyInt, Float) | (Float, AnyInt) | (Float, Float) => Ok(Float),

            // (AnyInt, v @ Bound(TyKind::Int(_)))
            // | (AnyInt, v @ Bound(TyKind::UInt(_)))
            // | (AnyInt, v @ Bound(TyKind::Float(_)))
            // | (v @ Bound(TyKind::Int(_)), AnyInt)
            // | (v @ Bound(TyKind::UInt(_)), AnyInt)
            // | (v @ Bound(TyKind::Float(_)), AnyInt) => Ok(v.clone()),

            // (v @ Bound(TyKind::Pointer(..)), Pointer)
            // | (Pointer, v @ Bound(TyKind::Pointer(..)))
            // | (v @ Bound(TyKind::MultiPointer(..)), Pointer)
            // | (Pointer, v @ Bound(TyKind::MultiPointer(..))) => Ok(v.clone()),

            // (Float, v @ Bound(TyKind::Float(_))) | (v @ Bound(TyKind::Float(_)), Float) => {
            //     Ok(v.clone())
            // }
            (expected, actual) => Err(UnifyError(expected.clone(), actual.clone())),
        }
    }

    #[inline]
    pub(crate) fn map_err(
        &mut self,
        UnifyError(expected, actual): UnifyError,
        span: Span,
    ) -> Diagnostic<usize> {
        TypeError::type_mismatch(
            span,
            self.normalize_ty_and_untyped(&expected).to_string(),
            self.normalize_ty_and_untyped(&actual).to_string(),
        )
    }
}
