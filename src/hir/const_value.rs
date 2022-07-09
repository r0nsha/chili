use super::FunctionId;
use crate::{
    ast::{self, ExternLibrary},
    infer::{display::DisplayTy, ty_ctx::TyCtx},
    types::TypeId,
};
use enum_as_inner::EnumAsInner;
use indexmap::IndexMap;
use std::fmt::Debug;
use ustr::Ustr;

#[derive(Debug, PartialEq, Clone, EnumAsInner)]
pub enum ConstValue {
    Unit(()),
    Type(TypeId),
    Bool(bool),
    Int(i64),
    Uint(u64),
    Float(f64),
    Str(Ustr),
    Array(ConstArray),
    Tuple(Vec<ConstElement>),
    Struct(ConstStruct),
    Function(ConstFunction),
    ExternVariable(ConstExternVariable),
}

pub type ConstStruct = IndexMap<Ustr, ConstElement>;

#[derive(Debug, PartialEq, Clone)]
pub struct ConstElement {
    pub value: ConstValue,
    pub ty: TypeId,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ConstArray {
    pub values: Vec<ConstValue>,
    pub element_ty: TypeId,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ConstFunction {
    pub id: FunctionId,
    // Name is only used for display purposes
    pub name: Ustr,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ConstExternVariable {
    pub name: Ustr,
    pub lib: Option<ExternLibrary>,
    pub ty: TypeId,
}

impl From<ast::LiteralKind> for ConstValue {
    fn from(lit: ast::LiteralKind) -> Self {
        match lit {
            ast::LiteralKind::Nil => panic!("nil is deprecated"),
            ast::LiteralKind::Bool(v) => ConstValue::Bool(v),
            ast::LiteralKind::Int(v) => ConstValue::Int(v),
            ast::LiteralKind::Float(v) => ConstValue::Float(v),
            ast::LiteralKind::Str(v) => ConstValue::Str(v),
            ast::LiteralKind::Char(v) => ConstValue::Uint(v as u64),
        }
    }
}

impl ConstValue {
    pub fn eq(&self, other: &ConstValue) -> ConstValue {
        ConstValue::Bool(self == other)
    }

    pub fn ne(&self, other: &ConstValue) -> ConstValue {
        self.eq(other).not()
    }

    pub fn not(&self) -> ConstValue {
        match self {
            ConstValue::Bool(v) => ConstValue::Bool(!v),
            ConstValue::Int(v) => ConstValue::Int(!v),
            ConstValue::Uint(v) => ConstValue::Uint(!v),
            _ => panic!("got {:?}", self),
        }
    }

    pub fn neg(&self) -> ConstValue {
        match self {
            ConstValue::Int(i) => ConstValue::Int(-i),
            ConstValue::Float(f) => ConstValue::Float(-f),
            _ => panic!("got {:?}", self),
        }
    }

    pub fn add(&self, other: &ConstValue) -> Option<ConstValue> {
        match (self, other) {
            (ConstValue::Int(v1), ConstValue::Int(v2)) => v1.checked_add(*v2).map(ConstValue::Int),
            (ConstValue::Uint(v1), ConstValue::Uint(v2)) => {
                v1.checked_add(*v2).map(ConstValue::Uint)
            }

            (ConstValue::Int(v1), ConstValue::Uint(v2)) => {
                v1.checked_add(*v2 as i64).map(ConstValue::Int)
            }
            (ConstValue::Uint(v1), ConstValue::Int(v2)) => {
                (*v1 as i64).checked_add(*v2).map(ConstValue::Int)
            }

            (ConstValue::Int(v1), ConstValue::Float(v2)) => {
                Some(ConstValue::Float(*v1 as f64 + *v2))
            }
            (ConstValue::Float(v1), ConstValue::Int(v2)) => {
                Some(ConstValue::Float(*v1 + *v2 as f64))
            }

            (ConstValue::Uint(v1), ConstValue::Float(v2)) => {
                Some(ConstValue::Float(*v1 as f64 + *v2))
            }
            (ConstValue::Float(v1), ConstValue::Uint(v2)) => {
                Some(ConstValue::Float(*v1 + *v2 as f64))
            }

            (ConstValue::Float(v1), ConstValue::Float(v2)) => Some(ConstValue::Float(*v1 + *v2)),

            _ => panic!("got {:?}", self),
        }
    }

    pub fn sub(&self, other: &ConstValue) -> Option<ConstValue> {
        match (self, other) {
            (ConstValue::Int(v1), ConstValue::Int(v2)) => v1.checked_sub(*v2).map(ConstValue::Int),

            (ConstValue::Uint(v1), ConstValue::Uint(v2)) => {
                v1.checked_sub(*v2).map(ConstValue::Uint)
            }

            (ConstValue::Int(v1), ConstValue::Uint(v2)) => {
                v1.checked_sub(*v2 as i64).map(ConstValue::Int)
            }
            (ConstValue::Uint(v1), ConstValue::Int(v2)) => {
                (*v1 as i64).checked_sub(*v2).map(ConstValue::Int)
            }

            (ConstValue::Int(v1), ConstValue::Float(v2)) => {
                Some(ConstValue::Float(*v1 as f64 - *v2))
            }
            (ConstValue::Float(v1), ConstValue::Int(v2)) => {
                Some(ConstValue::Float(*v1 - *v2 as f64))
            }

            (ConstValue::Uint(v1), ConstValue::Float(v2)) => {
                Some(ConstValue::Float(*v1 as f64 - *v2))
            }
            (ConstValue::Float(v1), ConstValue::Uint(v2)) => {
                Some(ConstValue::Float(*v1 - *v2 as f64))
            }

            (ConstValue::Float(v1), ConstValue::Float(v2)) => Some(ConstValue::Float(*v1 - *v2)),

            _ => panic!("got {:?}", self),
        }
    }

    pub fn mul(&self, other: &ConstValue) -> Option<ConstValue> {
        match (self, other) {
            (ConstValue::Int(v1), ConstValue::Int(v2)) => v1.checked_mul(*v2).map(ConstValue::Int),

            (ConstValue::Uint(v1), ConstValue::Uint(v2)) => {
                v1.checked_mul(*v2).map(ConstValue::Uint)
            }

            (ConstValue::Int(v1), ConstValue::Uint(v2)) => {
                v1.checked_mul(*v2 as i64).map(ConstValue::Int)
            }
            (ConstValue::Uint(v1), ConstValue::Int(v2)) => {
                (*v1 as i64).checked_mul(*v2).map(ConstValue::Int)
            }

            (ConstValue::Int(v1), ConstValue::Float(v2)) => {
                Some(ConstValue::Float(*v1 as f64 + *v2))
            }
            (ConstValue::Float(v1), ConstValue::Int(v2)) => {
                Some(ConstValue::Float(*v1 + *v2 as f64))
            }

            (ConstValue::Uint(v1), ConstValue::Float(v2)) => {
                Some(ConstValue::Float(*v1 as f64 + *v2))
            }
            (ConstValue::Float(v1), ConstValue::Uint(v2)) => {
                Some(ConstValue::Float(*v1 + *v2 as f64))
            }

            (ConstValue::Float(v1), ConstValue::Float(v2)) => Some(ConstValue::Float(*v1 + *v2)),

            _ => panic!("got {:?}", self),
        }
    }

    pub fn div(&self, other: &ConstValue) -> Option<ConstValue> {
        match (self, other) {
            (ConstValue::Int(v1), ConstValue::Int(v2)) => v1.checked_div(*v2).map(ConstValue::Int),

            (ConstValue::Uint(v1), ConstValue::Uint(v2)) => {
                v1.checked_div(*v2).map(ConstValue::Uint)
            }

            (ConstValue::Int(v1), ConstValue::Uint(v2)) => {
                v1.checked_div(*v2 as i64).map(ConstValue::Int)
            }

            (ConstValue::Uint(v1), ConstValue::Int(v2)) => {
                (*v1 as i64).checked_div(*v2).map(ConstValue::Int)
            }

            (ConstValue::Int(v1), ConstValue::Float(v2)) => {
                Some(ConstValue::Float(*v1 as f64 / *v2))
            }
            (ConstValue::Float(v1), ConstValue::Int(v2)) => {
                Some(ConstValue::Float(*v1 / *v2 as f64))
            }

            (ConstValue::Uint(v1), ConstValue::Float(v2)) => {
                Some(ConstValue::Float(*v1 as f64 / *v2))
            }
            (ConstValue::Float(v1), ConstValue::Uint(v2)) => {
                Some(ConstValue::Float(*v1 / *v2 as f64))
            }

            (ConstValue::Float(v1), ConstValue::Float(v2)) => Some(ConstValue::Float(*v1 / *v2)),

            _ => panic!("got {:?}", self),
        }
    }

    pub fn rem(&self, other: &ConstValue) -> Option<ConstValue> {
        match (self, other) {
            (ConstValue::Int(v1), ConstValue::Int(v2)) => v1.checked_rem(*v2).map(ConstValue::Int),

            (ConstValue::Uint(v1), ConstValue::Uint(v2)) => {
                v1.checked_rem(*v2).map(ConstValue::Uint)
            }

            (ConstValue::Int(v1), ConstValue::Uint(v2)) => {
                v1.checked_rem(*v2 as i64).map(ConstValue::Int)
            }

            (ConstValue::Uint(v1), ConstValue::Int(v2)) => {
                (*v1 as i64).checked_rem(*v2).map(ConstValue::Int)
            }

            (ConstValue::Int(v1), ConstValue::Float(v2)) => {
                Some(ConstValue::Float(*v1 as f64 % *v2))
            }
            (ConstValue::Float(v1), ConstValue::Int(v2)) => {
                Some(ConstValue::Float(*v1 % *v2 as f64))
            }

            (ConstValue::Uint(v1), ConstValue::Float(v2)) => {
                Some(ConstValue::Float(*v1 as f64 % *v2))
            }
            (ConstValue::Float(v1), ConstValue::Uint(v2)) => {
                Some(ConstValue::Float(*v1 % *v2 as f64))
            }

            (ConstValue::Float(v1), ConstValue::Float(v2)) => Some(ConstValue::Float(*v1 % *v2)),

            _ => panic!("got {:?}", self),
        }
    }

    pub fn lt(&self, other: &ConstValue) -> ConstValue {
        match (self, other) {
            (ConstValue::Int(v1), ConstValue::Int(v2)) => ConstValue::Bool(*v1 < *v2),
            (ConstValue::Uint(v1), ConstValue::Uint(v2)) => ConstValue::Bool(*v1 < *v2),
            (ConstValue::Int(v1), ConstValue::Uint(v2)) => ConstValue::Bool(*v1 < *v2 as i64),
            (ConstValue::Uint(v2), ConstValue::Int(v1)) => ConstValue::Bool((*v2 as i64) < *v1),
            (ConstValue::Int(v1), ConstValue::Float(v2)) => ConstValue::Bool((*v1 as f64) < *v2),
            (ConstValue::Float(v1), ConstValue::Int(v2)) => ConstValue::Bool(*v1 < *v2 as f64),
            (ConstValue::Uint(v1), ConstValue::Float(v2)) => ConstValue::Bool((*v1 as f64) < *v2),
            (ConstValue::Float(v1), ConstValue::Uint(v2)) => ConstValue::Bool(*v1 < *v2 as f64),
            (ConstValue::Float(v1), ConstValue::Float(v2)) => ConstValue::Bool(*v1 < *v2),
            _ => panic!("got {:?}", self),
        }
    }

    pub fn le(&self, other: &ConstValue) -> ConstValue {
        match (self, other) {
            (ConstValue::Int(v1), ConstValue::Int(v2)) => ConstValue::Bool(*v1 <= *v2),
            (ConstValue::Uint(v1), ConstValue::Uint(v2)) => ConstValue::Bool(*v1 <= *v2),
            (ConstValue::Int(v1), ConstValue::Uint(v2)) => ConstValue::Bool(*v1 <= *v2 as i64),
            (ConstValue::Uint(v2), ConstValue::Int(v1)) => ConstValue::Bool((*v2 as i64) <= *v1),
            (ConstValue::Int(v1), ConstValue::Float(v2)) => ConstValue::Bool((*v1 as f64) <= *v2),
            (ConstValue::Float(v1), ConstValue::Int(v2)) => ConstValue::Bool(*v1 <= *v2 as f64),
            (ConstValue::Uint(v1), ConstValue::Float(v2)) => ConstValue::Bool((*v1 as f64) <= *v2),
            (ConstValue::Float(v1), ConstValue::Uint(v2)) => ConstValue::Bool(*v1 <= *v2 as f64),
            (ConstValue::Float(v1), ConstValue::Float(v2)) => ConstValue::Bool(*v1 <= *v2),
            _ => panic!("got {:?}", self),
        }
    }

    pub fn gt(&self, other: &ConstValue) -> ConstValue {
        match (self, other) {
            (ConstValue::Int(v1), ConstValue::Int(v2)) => ConstValue::Bool(*v1 > *v2),
            (ConstValue::Uint(v1), ConstValue::Uint(v2)) => ConstValue::Bool(*v1 > *v2),
            (ConstValue::Int(v1), ConstValue::Uint(v2)) => ConstValue::Bool(*v1 > *v2 as i64),
            (ConstValue::Uint(v2), ConstValue::Int(v1)) => ConstValue::Bool((*v2 as i64) > *v1),
            (ConstValue::Int(v1), ConstValue::Float(v2)) => ConstValue::Bool((*v1 as f64) > *v2),
            (ConstValue::Float(v1), ConstValue::Int(v2)) => ConstValue::Bool(*v1 > *v2 as f64),
            (ConstValue::Uint(v1), ConstValue::Float(v2)) => ConstValue::Bool((*v1 as f64) > *v2),
            (ConstValue::Float(v1), ConstValue::Uint(v2)) => ConstValue::Bool(*v1 > *v2 as f64),
            (ConstValue::Float(v1), ConstValue::Float(v2)) => ConstValue::Bool(*v1 > *v2),
            _ => panic!("got {:?}", self),
        }
    }

    pub fn ge(&self, other: &ConstValue) -> ConstValue {
        match (self, other) {
            (ConstValue::Int(v1), ConstValue::Int(v2)) => ConstValue::Bool(*v1 >= *v2),
            (ConstValue::Uint(v1), ConstValue::Uint(v2)) => ConstValue::Bool(*v1 >= *v2),
            (ConstValue::Int(v1), ConstValue::Uint(v2)) => ConstValue::Bool(*v1 >= *v2 as i64),
            (ConstValue::Uint(v2), ConstValue::Int(v1)) => ConstValue::Bool((*v2 as i64) >= *v1),
            (ConstValue::Int(v1), ConstValue::Float(v2)) => ConstValue::Bool((*v1 as f64) >= *v2),
            (ConstValue::Float(v1), ConstValue::Int(v2)) => ConstValue::Bool(*v1 >= *v2 as f64),
            (ConstValue::Uint(v1), ConstValue::Float(v2)) => ConstValue::Bool((*v1 as f64) >= *v2),
            (ConstValue::Float(v1), ConstValue::Uint(v2)) => ConstValue::Bool(*v1 >= *v2 as f64),
            (ConstValue::Float(v1), ConstValue::Float(v2)) => ConstValue::Bool(*v1 >= *v2),
            _ => panic!("got {:?}", self),
        }
    }

    pub fn and(&self, other: &ConstValue) -> ConstValue {
        match (self, other) {
            (ConstValue::Bool(v1), ConstValue::Bool(v2)) => ConstValue::Bool(*v1 && *v2),
            _ => panic!("got {:?}", self),
        }
    }

    pub fn or(&self, other: &ConstValue) -> ConstValue {
        match (self, other) {
            (ConstValue::Bool(v1), ConstValue::Bool(v2)) => ConstValue::Bool(*v1 || *v2),
            _ => panic!("got {:?}", self),
        }
    }

    pub fn shl(&self, other: &ConstValue) -> Option<ConstValue> {
        match (self, other) {
            (ConstValue::Int(v1), ConstValue::Int(v2)) => {
                v1.checked_shl(*v2 as _).map(ConstValue::Int)
            }

            (ConstValue::Uint(v1), ConstValue::Uint(v2)) => {
                v1.checked_shl(*v2 as _).map(ConstValue::Uint)
            }

            (ConstValue::Int(v1), ConstValue::Uint(v2)) => {
                v1.checked_shl(*v2 as _).map(ConstValue::Int)
            }

            (ConstValue::Uint(v1), ConstValue::Int(v2)) => {
                (*v1 as i64).checked_shl(*v2 as _).map(ConstValue::Int)
            }

            _ => panic!("got {:?}", self),
        }
    }

    pub fn shr(&self, other: &ConstValue) -> Option<ConstValue> {
        match (self, other) {
            (ConstValue::Int(v1), ConstValue::Int(v2)) => {
                v1.checked_shr(*v2 as _).map(ConstValue::Int)
            }

            (ConstValue::Uint(v1), ConstValue::Uint(v2)) => {
                v1.checked_shr(*v2 as _).map(ConstValue::Uint)
            }

            (ConstValue::Int(v1), ConstValue::Uint(v2)) => {
                v1.checked_shr(*v2 as _).map(ConstValue::Int)
            }

            (ConstValue::Uint(v1), ConstValue::Int(v2)) => {
                (*v1 as i64).checked_shr(*v2 as _).map(ConstValue::Int)
            }

            _ => panic!("got {:?}", self),
        }
    }

    pub fn bitand(&self, other: &ConstValue) -> ConstValue {
        match (self, other) {
            (ConstValue::Int(v1), ConstValue::Int(v2)) => ConstValue::Int(*v1 & *v2),
            (ConstValue::Uint(v1), ConstValue::Uint(v2)) => ConstValue::Uint(*v1 & *v2),
            (ConstValue::Int(v1), ConstValue::Uint(v2)) => ConstValue::Int(*v1 & *v2 as i64),
            (ConstValue::Uint(v2), ConstValue::Int(v1)) => ConstValue::Int((*v2 as i64) & *v1),
            _ => panic!("got {:?}", self),
        }
    }

    pub fn bitor(&self, other: &ConstValue) -> ConstValue {
        match (self, other) {
            (ConstValue::Int(v1), ConstValue::Int(v2)) => ConstValue::Int(*v1 | *v2),
            (ConstValue::Uint(v1), ConstValue::Uint(v2)) => ConstValue::Uint(*v1 | *v2),
            (ConstValue::Int(v1), ConstValue::Uint(v2)) => ConstValue::Int(*v1 | *v2 as i64),
            (ConstValue::Uint(v2), ConstValue::Int(v1)) => ConstValue::Int((*v2 as i64) | *v1),
            _ => panic!("got {:?}", self),
        }
    }

    pub fn bitxor(&self, other: &ConstValue) -> ConstValue {
        match (self, other) {
            (ConstValue::Int(v1), ConstValue::Int(v2)) => ConstValue::Int(*v1 ^ *v2),
            (ConstValue::Uint(v1), ConstValue::Uint(v2)) => ConstValue::Uint(*v1 ^ *v2),
            (ConstValue::Int(v1), ConstValue::Uint(v2)) => ConstValue::Int(*v1 ^ *v2 as i64),
            (ConstValue::Uint(v2), ConstValue::Int(v1)) => ConstValue::Int((*v2 as i64) ^ *v1),
            _ => panic!("got {:?}", self),
        }
    }

    #[allow(unused)]
    pub fn display(&self, tcx: &TyCtx) -> String {
        match self {
            ConstValue::Unit(_) => "()".to_string(),
            ConstValue::Type(t) => format!("type {}", t.display(tcx)),
            ConstValue::Bool(v) => format!("{}", v),
            ConstValue::Int(v) => format!("{}", v),
            ConstValue::Uint(v) => format!("{}", v),
            ConstValue::Float(v) => format!("{}", v),
            ConstValue::Str(v) => format!("\"{}\"", v),
            ConstValue::Array(array) => format!(
                "[{}]",
                array
                    .values
                    .iter()
                    .map(|v| v.display(tcx))
                    .collect::<Vec<String>>()
                    .join(", "),
            ),
            ConstValue::Tuple(elements) => format!(
                "({})",
                elements
                    .iter()
                    .map(|el| el.value.display(tcx))
                    .collect::<Vec<String>>()
                    .join(", "),
            ),
            ConstValue::Struct(fields) => format!(
                "{{ {} }}",
                fields
                    .iter()
                    .map(|(name, el)| format!("{}: {}", name, el.value.display(tcx)))
                    .collect::<Vec<String>>()
                    .join(", "),
            ),
            ConstValue::Function(f) => f.name.to_string(),
            ConstValue::ExternVariable(v) => v.name.to_string(),
        }
    }
}
