use chili_ast::ty::{FloatTy, IntTy, TyKind};

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum Constraint {
    Bound(TyKind),
    Int,
    Float,
    Pointer,
    Unbound,
}

impl From<TyKind> for Constraint {
    fn from(ty: TyKind) -> Self {
        Constraint::Bound(ty)
    }
}

impl From<Constraint> for TyKind {
    fn from(value: Constraint) -> Self {
        match value {
            Constraint::Bound(ty) => ty,
            Constraint::Int => TyKind::Int(IntTy::default()),
            Constraint::Float => TyKind::Float(FloatTy::default()),
            Constraint::Pointer => TyKind::raw_pointer(true),
            Constraint::Unbound => panic!("expected type, found InferenceValue::Unbound"),
        }
    }
}
