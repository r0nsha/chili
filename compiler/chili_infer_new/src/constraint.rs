use chili_ast::ty::*;

#[derive(Clone, Debug, PartialEq)]
pub(crate) enum Constraint {
    Bound(TyKind),
    Unbound,
}
