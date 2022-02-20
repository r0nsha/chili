use chilic_ast::expr::Expr;
use chilic_error::{DiagnosticResult, TypeError};
use chilic_span::Span;
use chilic_ty::*;
use codespan_reporting::diagnostic::Diagnostic;
use common::mut_eq;
use ena::unify::UnifyValue;

use crate::{
    coerce::{Coerce, CoercionResult, TryCoerce},
    infer::{InferenceContext, TyVar},
};

use super::infer::{InferenceValue, InferenceValue::*};

pub struct UnificationError(pub Ty, pub Ty);

impl UnifyValue for InferenceValue {
    type Error = UnificationError;

    fn unify_values(a: &Self, b: &Self) -> Result<Self, Self::Error> {
        match (a, b) {
            (Unbound, Unbound) => Ok(Unbound),

            (Unbound, v @ UntypedInt)
            | (v @ UntypedInt, Unbound)
            | (Unbound, v @ UntypedFloat)
            | (v @ UntypedFloat, Unbound)
            | (Unbound, v @ Bound(_))
            | (v @ Bound(_), Unbound) => Ok(v.clone()),

            (UntypedInt, UntypedInt) => Ok(UntypedInt),

            (UntypedInt, UntypedFloat)
            | (UntypedFloat, UntypedInt)
            | (UntypedFloat, UntypedFloat) => Ok(UntypedFloat),

            (UntypedInt, v @ Bound(Ty::Int(_)))
            | (UntypedInt, v @ Bound(Ty::UInt(_)))
            | (UntypedInt, v @ Bound(Ty::Float(_)))
            | (v @ Bound(Ty::Int(_)), UntypedInt)
            | (v @ Bound(Ty::UInt(_)), UntypedInt)
            | (v @ Bound(Ty::Float(_)), UntypedInt) => Ok(v.clone()),

            (v @ Bound(Ty::Pointer(..)), UntypedNil)
            | (UntypedNil, v @ Bound(Ty::Pointer(..)))
            | (v @ Bound(Ty::MultiPointer(..)), UntypedNil)
            | (UntypedNil, v @ Bound(Ty::MultiPointer(..))) => Ok(v.clone()),

            (UntypedFloat, v @ Bound(Ty::Float(_)))
            | (v @ Bound(Ty::Float(_)), UntypedFloat) => Ok(v.clone()),

            (Bound(t1), Bound(t2)) => {
                panic!("can't unify two bound variables {} and {}", t1, t2)
            }

            _ => Err(UnificationError(a.clone().into(), b.clone().into())),
        }
    }
}

impl InferenceContext {
    pub fn unify_or_coerce_expr_expr(
        &mut self,
        left_expr: &mut Expr,
        right_expr: &mut Expr,
        span: Span,
    ) -> DiagnosticResult<Ty> {
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
                    CoercionResult::NoCoercion => Err(self
                        .map_unification_error(
                            UnificationError(left_expr_ty, right_expr_ty),
                            span,
                        )),
                }
            }
        }
    }

    pub fn unify_or_coerce_ty_expr(
        &mut self,
        ty: &Ty,
        expr: &mut Expr,
        span: Span,
    ) -> DiagnosticResult<Ty> {
        match self.unify_ty_ty(ty, &expr.ty, span) {
            Ok(ty) => Ok(ty),
            Err(_) => {
                let ty = self.normalize_ty(ty);
                let expr_ty = self.normalize_ty(&expr.ty);

                match expr_ty.try_coerce(&ty, self.word_size) {
                    CoercionResult::CoerceToRight => {
                        *expr = expr.coerce(ty.clone());
                        Ok(ty)
                    }
                    CoercionResult::CoerceToLeft
                    | CoercionResult::NoCoercion => Err(self
                        .map_unification_error(
                            UnificationError(ty, expr_ty),
                            span,
                        )),
                }
            }
        }
    }

    pub fn unify(
        &mut self,
        expected: impl Into<Ty>,
        actual: impl Into<Ty>,
        span: Span,
    ) -> DiagnosticResult<Ty> {
        let expected: Ty = expected.into();
        let actual: Ty = actual.into();

        self.unify_ty_ty(&expected, &actual, span).map_err(|_| {
            self.map_unification_error(UnificationError(expected, actual), span)
        })
    }

    pub fn unify_ty_ty(
        &mut self,
        expected: &Ty,
        actual: &Ty,
        span: Span,
    ) -> Result<Ty, UnificationError> {
        match (expected, actual) {
            (Ty::Unit, Ty::Unit)
            | (Ty::Bool, Ty::Bool)
            | (Ty::Int(IntTy::I8), Ty::Int(IntTy::I8))
            | (Ty::Int(IntTy::I16), Ty::Int(IntTy::I16))
            | (Ty::Int(IntTy::I32), Ty::Int(IntTy::I32))
            | (Ty::Int(IntTy::I64), Ty::Int(IntTy::I64))
            | (Ty::Int(IntTy::ISize), Ty::Int(IntTy::ISize))
            | (Ty::UInt(UIntTy::U8), Ty::UInt(UIntTy::U8))
            | (Ty::UInt(UIntTy::U16), Ty::UInt(UIntTy::U16))
            | (Ty::UInt(UIntTy::U32), Ty::UInt(UIntTy::U32))
            | (Ty::UInt(UIntTy::U64), Ty::UInt(UIntTy::U64))
            | (Ty::UInt(UIntTy::USize), Ty::UInt(UIntTy::USize))
            | (Ty::Float(FloatTy::F16), Ty::Float(FloatTy::F16))
            | (Ty::Float(FloatTy::F32), Ty::Float(FloatTy::F32))
            | (Ty::Float(FloatTy::F64), Ty::Float(FloatTy::F64))
            | (Ty::Float(FloatTy::FSize), Ty::Float(FloatTy::FSize))
            | (Ty::Never, Ty::Never) => Ok(expected.clone()),

            (Ty::Pointer(t1, m1), Ty::Pointer(t2, m2)) => {
                if !mut_eq(*m1, *m2) {
                    return Err(UnificationError(
                        expected.clone(),
                        actual.clone(),
                    ));
                }

                let unified = self.unify_ty_ty(t1, t2, span)?;
                Ok(Ty::Pointer(Box::new(unified), *m1))
            }

            (Ty::MultiPointer(t1, m1), Ty::MultiPointer(t2, m2)) => {
                if !mut_eq(*m1, *m2) {
                    return Err(UnificationError(
                        expected.clone(),
                        actual.clone(),
                    ));
                }

                let unified = self.unify_ty_ty(t1, t2, span)?;
                Ok(Ty::MultiPointer(Box::new(unified), *m1))
            }

            (Ty::Var(v1), Ty::Var(v2)) => {
                let v1 = TyVar::from(*v1);
                let v2 = TyVar::from(*v2);

                match (self.value_of(v1), self.value_of(v2)) {
                    (InferenceValue::Bound(t1), InferenceValue::Bound(t2)) => {
                        self.unify_ty_ty(&t1, &t2, span)
                    }
                    _ => {
                        self.table.unify_var_var(v1, v2)?;
                        Ok(expected.clone())
                    }
                }
            }

            (Ty::Var(var), actual) => match self.value_of(TyVar::from(*var)) {
                InferenceValue::Bound(expected) => {
                    self.unify_ty_ty(&expected, actual, span)
                }
                InferenceValue::UntypedInt
                | InferenceValue::UntypedFloat
                | InferenceValue::UntypedNil
                | InferenceValue::Unbound => {
                    self.table.unify_var_value(
                        TyVar::from(*var),
                        InferenceValue::Bound(actual.clone()),
                    )?;
                    Ok(actual.clone())
                }
            },

            (expected, Ty::Var(var)) => {
                match self.value_of(TyVar::from(*var)) {
                    InferenceValue::Bound(actual) => {
                        self.unify_ty_ty(expected, &actual, span)
                    }
                    value @ InferenceValue::UntypedInt
                    | value @ InferenceValue::UntypedFloat
                    | value @ InferenceValue::UntypedNil
                    | value @ InferenceValue::Unbound => {
                        // We map the error so that the error message matches
                        // the types
                        self.table
                            .unify_var_value(
                                TyVar::from(*var),
                                InferenceValue::Bound(expected.clone()),
                            )
                            .map_err(|_| {
                                UnificationError(expected.clone(), value.into())
                            })?;

                        Ok(expected.clone())
                    }
                }
            }

            (Ty::Fn(fn_a), Ty::Fn(fn_b)) => {
                if fn_a.variadic != fn_b.variadic
                    || fn_a.params.len() != fn_b.params.len()
                {
                    return Err(UnificationError(
                        expected.clone(),
                        actual.clone(),
                    ));
                }

                self.unify_ty_ty(&fn_a.ret, &fn_b.ret, span)?;

                for (p1, p2) in fn_a.params.iter().zip(fn_b.params.iter()) {
                    self.unify_ty_ty(&p1.ty, &p2.ty, span)?;
                }

                Ok(expected.clone())
            }

            (Ty::Array(inner_a, len_a), Ty::Array(inner_b, len_b)) => {
                if len_a != len_b {
                    return Err(UnificationError(
                        expected.clone(),
                        actual.clone(),
                    ));
                }

                let unified_inner = self.unify_ty_ty(inner_a, inner_b, span)?;

                Ok(Ty::Array(Box::new(unified_inner), *len_a))
            }

            (Ty::Slice(inner_a, m1), Ty::Slice(inner_b, m2)) => {
                if !mut_eq(*m1, *m2) {
                    return Err(UnificationError(
                        expected.clone(),
                        actual.clone(),
                    ));
                }

                let unified = self.unify_ty_ty(inner_a, inner_b, span)?;
                Ok(Ty::Slice(Box::new(unified), *m1))
            }

            (Ty::Tuple(tys_a), Ty::Tuple(tys_b)) => {
                if tys_a.len() != tys_b.len() {
                    return Err(UnificationError(
                        expected.clone(),
                        actual.clone(),
                    ));
                }

                let mut unified_tys = vec![];

                for (t1, t2) in tys_a.iter().zip(tys_b.iter()) {
                    let unified = self.unify_ty_ty(t1, t2, span)?;
                    unified_tys.push(unified);
                }

                Ok(Ty::Tuple(unified_tys))
            }

            (Ty::Struct(t1), Ty::Struct(t2)) => {
                if t1.qualified_name == t2.qualified_name {
                    Ok(expected.clone())
                } else {
                    Err(UnificationError(expected.clone(), actual.clone()))
                }
            }

            (Ty::Never, t) | (t, Ty::Never) => Ok(t.clone()),

            (expected, actual) => {
                Err(UnificationError(expected.clone(), actual.clone()))
            }
        }
    }

    #[inline]
    pub fn map_unification_error(
        &mut self,
        UnificationError(expected, actual): UnificationError,
        span: Span,
    ) -> Diagnostic<usize> {
        TypeError::type_mismatch(
            span,
            &self.normalize_ty_and_untyped(&expected),
            &self.normalize_ty_and_untyped(&actual),
        )
    }
}
