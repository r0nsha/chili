use super::{
    normalize::Normalize,
    type_ctx::TypeCtx,
    unify::{can_coerce_mut, UnifyTypeResult},
};
use crate::{
    check::CheckSess,
    hir::{self, const_value::ConstValue},
    infer::unify::UnifyType,
    types::{size_of::SizeOf, *},
};

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum CoercionResult {
    CoerceToLeft(CoercionKind),
    CoerceToRight(CoercionKind),
    NoCoercion,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum CoercionKind {
    Cast,
    Slice,
}

trait Coerce {
    fn coerce(&self, to: &Type, tcx: &mut TypeCtx, word_size: usize) -> CoercionResult;
}

impl Coerce for Type {
    fn coerce(&self, to: &Type, tcx: &mut TypeCtx, word_size: usize) -> CoercionResult {
        use CoercionResult::*;

        match (self, to) {
            (Type::Infer(_, InferType::AnyInt), Type::Infer(_, InferType::AnyFloat)) => {
                CoerceToRight(CoercionKind::Cast)
            }
            (Type::Infer(_, InferType::AnyFloat), Type::Infer(_, InferType::AnyInt)) => {
                CoerceToLeft(CoercionKind::Cast)
            }

            // * int -> same or bigger int
            (Type::Int(left), Type::Int(right)) => {
                if left.size_of(word_size) <= right.size_of(word_size) {
                    CoerceToRight(CoercionKind::Cast)
                } else {
                    CoerceToLeft(CoercionKind::Cast)
                }
            }

            // * int -> same or bigger uint
            (Type::Int(left), Type::Uint(right)) => {
                if left.size_of(word_size) <= right.size_of(word_size) {
                    CoerceToRight(CoercionKind::Cast)
                } else {
                    CoerceToLeft(CoercionKind::Cast)
                }
            }

            // * uint -> same or bigger uint
            (Type::Uint(left), Type::Uint(right)) => {
                if left.size_of(word_size) <= right.size_of(word_size) {
                    CoerceToRight(CoercionKind::Cast)
                } else {
                    CoerceToLeft(CoercionKind::Cast)
                }
            }

            // * uint -> same or bigger int
            (Type::Uint(left), Type::Int(right)) => {
                if left.size_of(word_size) <= right.size_of(word_size) {
                    CoerceToRight(CoercionKind::Cast)
                } else {
                    CoerceToLeft(CoercionKind::Cast)
                }
            }

            // * float -> same or bigger float
            (Type::Float(left), Type::Float(right)) => {
                if left.size_of(word_size) <= right.size_of(word_size) {
                    CoerceToRight(CoercionKind::Cast)
                } else {
                    CoerceToLeft(CoercionKind::Cast)
                }
            }

            (Type::Pointer(left, lmut), Type::Pointer(right, rmut)) => {
                if can_coerce_mut(*lmut, *rmut) {
                    match (left.as_ref(), right.as_ref()) {
                        // *array[N] of T -> slice of T
                        (Type::Array(left, _), Type::Slice(right)) => left
                            .as_ref()
                            .unify(right.as_ref(), tcx)
                            // *array[N] of T -> pointer of T
                            .map_or(NoCoercion, |_| CoerceToRight(CoercionKind::Slice)),
                        (Type::Array(left, _), right) => left
                            .as_ref()
                            .unify(right, tcx)
                            .map_or(NoCoercion, |_| CoerceToRight(CoercionKind::Cast)),
                        // *T -> *U
                        _ => left
                            .as_ref()
                            .unify(right.as_ref(), tcx)
                            .map_or(NoCoercion, |_| CoerceToRight(CoercionKind::Cast)),
                    }
                } else {
                    NoCoercion
                }
            }

            _ => NoCoercion,
        }
    }
}

pub trait OrCoerce {
    fn or_coerce(
        self,
        left: &mut hir::Node,
        right: &mut hir::Node,
        tcx: &mut TypeCtx,
        word_size: usize,
    ) -> UnifyTypeResult;
}

impl OrCoerce for UnifyTypeResult {
    fn or_coerce(
        self,
        left: &mut hir::Node,
        right: &mut hir::Node,
        tcx: &mut TypeCtx,
        word_size: usize,
    ) -> UnifyTypeResult {
        match self {
            Ok(r) => Ok(r),
            Err(e) => {
                let (left_ty, right_ty) = (left.ty().normalize(tcx), right.ty().normalize(tcx));
                match left_ty.coerce(&right_ty, tcx, word_size) {
                    CoercionResult::CoerceToLeft(kind) => {
                        coerce_node(tcx, right, left_ty, kind);
                        Ok(())
                    }
                    CoercionResult::CoerceToRight(kind) => {
                        coerce_node(tcx, left, right_ty, kind);
                        Ok(())
                    }
                    CoercionResult::NoCoercion => Err(e),
                }
            }
        }
    }
}

pub trait OrCoerceIntoTy {
    fn or_coerce_into_ty(
        self,
        node: &mut hir::Node,
        ty: &impl Normalize,
        tcx: &mut TypeCtx,
        word_size: usize,
    ) -> UnifyTypeResult;
}

impl OrCoerceIntoTy for UnifyTypeResult {
    fn or_coerce_into_ty(
        self,
        node: &mut hir::Node,
        ty: &impl Normalize,
        tcx: &mut TypeCtx,
        word_size: usize,
    ) -> UnifyTypeResult {
        match self {
            Ok(r) => Ok(r),
            Err(e) => {
                let node_ty = node.ty().normalize(tcx);
                let ty = ty.normalize(tcx);

                match node_ty.coerce(&ty, tcx, word_size) {
                    CoercionResult::CoerceToRight(kind) => {
                        coerce_node(tcx, node, ty, kind);
                        Ok(())
                    }
                    CoercionResult::CoerceToLeft(_) | CoercionResult::NoCoercion => Err(e),
                }
            }
        }
    }
}

pub fn coerce_node(tcx: &mut TypeCtx, node: &mut hir::Node, to: Type, kind: CoercionKind) {
    *node = match kind {
        CoercionKind::Cast => hir::Node::Cast(hir::Cast {
            value: Box::new(node.clone()),
            ty: tcx.bound(to, node.span()),
            span: node.span(),
        }),
        CoercionKind::Slice => coerce_array_to_slice(tcx, node, to),
    }
}

pub fn coerce_array_to_slice(tcx: &mut TypeCtx, node: &hir::Node, to: Type) -> hir::Node {
    // Slice coercion from *[N]T to *[]T
    // Essentially doing
    // `&array.*[...]`
    //
    // Steps:
    // - Deref the array
    // - Slice the deref
    // - Ref the slice

    let span = node.span();

    let value_deref = hir::Node::Builtin(hir::Builtin::Deref(hir::Unary {
        value: Box::new(node.clone()),
        ty: tcx.bound(node.ty().normalize(tcx).maybe_deref_once(), span),
        span,
    }));

    let low = Box::new(hir::Node::Const(hir::Const {
        value: ConstValue::Int(0),
        ty: tcx.common_types.uint,
        span: node.span(),
    }));

    let high = Box::new(CheckSess::get_len_node(tcx, &value_deref).unwrap());

    let ty = tcx.bound(to.maybe_deref_once(), node.span());

    hir::Node::Builtin(hir::Builtin::Ref(hir::Ref {
        value: Box::new(hir::Node::Builtin(hir::Builtin::Slice(hir::Slice {
            value: Box::new(value_deref),
            low,
            high,
            ty,
            span,
        }))),
        is_mutable: false,
        ty: tcx.bound(to, node.span()),
        span,
    }))
}

// fn coerce_array_to_slice()
