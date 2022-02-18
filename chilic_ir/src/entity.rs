use std::fmt::Display;

use chilic_ty::Ty;
use ustr::Ustr;

use crate::{expr::Expr, pattern::Pattern, value::Value};

#[derive(Debug, PartialEq, Clone)]
pub struct Entity {
    pub visibility: Visibility,
    pub kind: EntityKind,
    pub pattern: Pattern,
    pub ty_expr: Option<Expr>,
    pub ty: Ty,
    pub value: Option<Expr>,
    pub const_value: Option<Value>,
    pub should_codegen: bool,
    pub lib_name: Option<Ustr>,
}

impl Entity {
    pub fn new(
        visibility: Visibility,
        kind: EntityKind,
        pattern: Pattern,
        ty_expr: Option<Expr>,
        value: Option<Expr>,
        lib_name: Option<Ustr>,
    ) -> Self {
        Self {
            visibility,
            kind,
            pattern,
            ty_expr,
            ty: Ty::Unknown,
            value,
            const_value: None,
            should_codegen: false,
            lib_name,
        }
    }

    pub fn into_type(&self) -> Ty {
        self.const_value.clone().unwrap().into_type()
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum EntityKind {
    Value,
    Type,
}

impl Display for EntityKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                EntityKind::Value => "value",
                EntityKind::Type => "type",
            }
        )
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Visibility {
    Private,
    Public,
}
