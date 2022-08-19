use super::FunctionId;
use crate::{
    ast::{self, ExternLibrary},
    common::target::{Arch, Os},
    infer::{display::DisplayType, type_ctx::TypeCtx},
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
    pub element_type: TypeId,
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
    pub dylib: Option<ExternLibrary>,
    pub ty: TypeId,
}

impl From<ast::LiteralKind> for ConstValue {
    fn from(lit: ast::LiteralKind) -> Self {
        match lit {
            ast::LiteralKind::Nil => panic!("nil is deprecated"),
            ast::LiteralKind::Bool(v) => ConstValue::Bool(v),
            ast::LiteralKind::Int(v) => ConstValue::Uint(v),
            ast::LiteralKind::Float(v) => ConstValue::Float(v),
            ast::LiteralKind::Str(v) => ConstValue::Str(v),
            ast::LiteralKind::Char(v) => ConstValue::Uint(v as u64),
        }
    }
}

impl ConstValue {
    pub fn eq(&self, other: &ConstValue) -> ConstValue {
        ConstValue::Bool(match (self, other) {
            (ConstValue::Unit(left), ConstValue::Unit(right)) => *left == *right,
            (ConstValue::Type(left), ConstValue::Type(right)) => *left == *right,
            (ConstValue::Bool(left), ConstValue::Bool(right)) => *left == *right,
            (ConstValue::Int(left), ConstValue::Int(right)) => *left == *right,
            (ConstValue::Int(left), ConstValue::Uint(right)) => *left == *right as i64,
            (ConstValue::Int(left), ConstValue::Float(right)) => *left as f64 == *right,
            (ConstValue::Uint(left), ConstValue::Int(right)) => *left as i64 == *right,
            (ConstValue::Uint(left), ConstValue::Uint(right)) => *left == *right,
            (ConstValue::Uint(left), ConstValue::Float(right)) => *left as f64 == *right,
            (ConstValue::Float(left), ConstValue::Int(right)) => *left == *right as f64,
            (ConstValue::Float(left), ConstValue::Uint(right)) => *left == *right as f64,
            (ConstValue::Float(left), ConstValue::Float(right)) => *left == *right,
            (ConstValue::Str(left), ConstValue::Str(right)) => *left == *right,
            (ConstValue::Array(left), ConstValue::Array(right)) => {
                left.values.len() == right.values.len()
                    && left
                        .values
                        .iter()
                        .zip(right.values.iter())
                        .all(|(left, right)| *left.eq(right).as_bool().unwrap())
            }
            (ConstValue::Tuple(left), ConstValue::Tuple(right)) => {
                left.len() == right.len()
                    && left
                        .iter()
                        .zip(right.iter())
                        .all(|(left, right)| *left.value.eq(&right.value).as_bool().unwrap())
            }
            (ConstValue::Struct(left), ConstValue::Struct(right)) => {
                left.len() == right.len()
                    && left.iter().zip(right.iter()).all(|((lname, left), (rname, right))| {
                        lname == rname && *left.value.eq(&right.value).as_bool().unwrap()
                    })
            }
            (ConstValue::Function(left), ConstValue::Function(right)) => left.id == right.id,
            _ => false,
        })
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
            (ConstValue::Uint(v1), ConstValue::Uint(v2)) => v1.checked_add(*v2).map(ConstValue::Uint),

            (ConstValue::Int(v1), ConstValue::Uint(v2)) => v1.checked_add(*v2 as i64).map(ConstValue::Int),
            (ConstValue::Uint(v1), ConstValue::Int(v2)) => (*v1 as i64).checked_add(*v2).map(ConstValue::Int),

            (ConstValue::Int(v1), ConstValue::Float(v2)) => Some(ConstValue::Float(*v1 as f64 + *v2)),
            (ConstValue::Float(v1), ConstValue::Int(v2)) => Some(ConstValue::Float(*v1 + *v2 as f64)),

            (ConstValue::Uint(v1), ConstValue::Float(v2)) => Some(ConstValue::Float(*v1 as f64 + *v2)),
            (ConstValue::Float(v1), ConstValue::Uint(v2)) => Some(ConstValue::Float(*v1 + *v2 as f64)),

            (ConstValue::Float(v1), ConstValue::Float(v2)) => Some(ConstValue::Float(*v1 + *v2)),

            _ => panic!("got {:?}", self),
        }
    }

    pub fn sub(&self, other: &ConstValue) -> Option<ConstValue> {
        match (self, other) {
            (ConstValue::Int(v1), ConstValue::Int(v2)) => v1.checked_sub(*v2).map(ConstValue::Int),

            (ConstValue::Uint(v1), ConstValue::Uint(v2)) => v1.checked_sub(*v2).map(ConstValue::Uint),

            (ConstValue::Int(v1), ConstValue::Uint(v2)) => v1.checked_sub(*v2 as i64).map(ConstValue::Int),
            (ConstValue::Uint(v1), ConstValue::Int(v2)) => (*v1 as i64).checked_sub(*v2).map(ConstValue::Int),

            (ConstValue::Int(v1), ConstValue::Float(v2)) => Some(ConstValue::Float(*v1 as f64 - *v2)),
            (ConstValue::Float(v1), ConstValue::Int(v2)) => Some(ConstValue::Float(*v1 - *v2 as f64)),

            (ConstValue::Uint(v1), ConstValue::Float(v2)) => Some(ConstValue::Float(*v1 as f64 - *v2)),
            (ConstValue::Float(v1), ConstValue::Uint(v2)) => Some(ConstValue::Float(*v1 - *v2 as f64)),

            (ConstValue::Float(v1), ConstValue::Float(v2)) => Some(ConstValue::Float(*v1 - *v2)),

            _ => panic!("got {:?}", self),
        }
    }

    pub fn mul(&self, other: &ConstValue) -> Option<ConstValue> {
        match (self, other) {
            (ConstValue::Int(v1), ConstValue::Int(v2)) => v1.checked_mul(*v2).map(ConstValue::Int),

            (ConstValue::Uint(v1), ConstValue::Uint(v2)) => v1.checked_mul(*v2).map(ConstValue::Uint),

            (ConstValue::Int(v1), ConstValue::Uint(v2)) => v1.checked_mul(*v2 as i64).map(ConstValue::Int),
            (ConstValue::Uint(v1), ConstValue::Int(v2)) => (*v1 as i64).checked_mul(*v2).map(ConstValue::Int),

            (ConstValue::Int(v1), ConstValue::Float(v2)) => Some(ConstValue::Float(*v1 as f64 + *v2)),
            (ConstValue::Float(v1), ConstValue::Int(v2)) => Some(ConstValue::Float(*v1 + *v2 as f64)),

            (ConstValue::Uint(v1), ConstValue::Float(v2)) => Some(ConstValue::Float(*v1 as f64 + *v2)),
            (ConstValue::Float(v1), ConstValue::Uint(v2)) => Some(ConstValue::Float(*v1 + *v2 as f64)),

            (ConstValue::Float(v1), ConstValue::Float(v2)) => Some(ConstValue::Float(*v1 + *v2)),

            _ => panic!("got {:?}", self),
        }
    }

    pub fn div(&self, other: &ConstValue) -> Option<ConstValue> {
        match (self, other) {
            (ConstValue::Int(v1), ConstValue::Int(v2)) => v1.checked_div(*v2).map(ConstValue::Int),

            (ConstValue::Uint(v1), ConstValue::Uint(v2)) => v1.checked_div(*v2).map(ConstValue::Uint),

            (ConstValue::Int(v1), ConstValue::Uint(v2)) => v1.checked_div(*v2 as i64).map(ConstValue::Int),

            (ConstValue::Uint(v1), ConstValue::Int(v2)) => (*v1 as i64).checked_div(*v2).map(ConstValue::Int),

            (ConstValue::Int(v1), ConstValue::Float(v2)) => Some(ConstValue::Float(*v1 as f64 / *v2)),
            (ConstValue::Float(v1), ConstValue::Int(v2)) => Some(ConstValue::Float(*v1 / *v2 as f64)),

            (ConstValue::Uint(v1), ConstValue::Float(v2)) => Some(ConstValue::Float(*v1 as f64 / *v2)),
            (ConstValue::Float(v1), ConstValue::Uint(v2)) => Some(ConstValue::Float(*v1 / *v2 as f64)),

            (ConstValue::Float(v1), ConstValue::Float(v2)) => Some(ConstValue::Float(*v1 / *v2)),

            _ => panic!("got {:?}", self),
        }
    }

    pub fn rem(&self, other: &ConstValue) -> Option<ConstValue> {
        match (self, other) {
            (ConstValue::Int(v1), ConstValue::Int(v2)) => v1.checked_rem(*v2).map(ConstValue::Int),

            (ConstValue::Uint(v1), ConstValue::Uint(v2)) => v1.checked_rem(*v2).map(ConstValue::Uint),

            (ConstValue::Int(v1), ConstValue::Uint(v2)) => v1.checked_rem(*v2 as i64).map(ConstValue::Int),

            (ConstValue::Uint(v1), ConstValue::Int(v2)) => (*v1 as i64).checked_rem(*v2).map(ConstValue::Int),

            (ConstValue::Int(v1), ConstValue::Float(v2)) => Some(ConstValue::Float(*v1 as f64 % *v2)),
            (ConstValue::Float(v1), ConstValue::Int(v2)) => Some(ConstValue::Float(*v1 % *v2 as f64)),

            (ConstValue::Uint(v1), ConstValue::Float(v2)) => Some(ConstValue::Float(*v1 as f64 % *v2)),
            (ConstValue::Float(v1), ConstValue::Uint(v2)) => Some(ConstValue::Float(*v1 % *v2 as f64)),

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
            (ConstValue::Int(v1), ConstValue::Int(v2)) => v1.checked_shl(*v2 as _).map(ConstValue::Int),

            (ConstValue::Uint(v1), ConstValue::Uint(v2)) => v1.checked_shl(*v2 as _).map(ConstValue::Uint),

            (ConstValue::Int(v1), ConstValue::Uint(v2)) => v1.checked_shl(*v2 as _).map(ConstValue::Int),

            (ConstValue::Uint(v1), ConstValue::Int(v2)) => (*v1 as i64).checked_shl(*v2 as _).map(ConstValue::Int),

            _ => panic!("got {:?}", self),
        }
    }

    pub fn shr(&self, other: &ConstValue) -> Option<ConstValue> {
        match (self, other) {
            (ConstValue::Int(v1), ConstValue::Int(v2)) => v1.checked_shr(*v2 as _).map(ConstValue::Int),

            (ConstValue::Uint(v1), ConstValue::Uint(v2)) => v1.checked_shr(*v2 as _).map(ConstValue::Uint),

            (ConstValue::Int(v1), ConstValue::Uint(v2)) => v1.checked_shr(*v2 as _).map(ConstValue::Int),

            (ConstValue::Uint(v1), ConstValue::Int(v2)) => (*v1 as i64).checked_shr(*v2 as _).map(ConstValue::Int),

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
    pub fn display(&self, tcx: &TypeCtx) -> String {
        match self {
            ConstValue::Unit(_) => "()".to_string(),
            ConstValue::Type(t) => t.display(tcx),
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

    pub fn from_os(os: Os) -> Self {
        Self::Uint(match os {
            Os::Linux => 0,
            Os::Windows => 1,
            Os::Darwin | Os::Essence | Os::FreeBSD | Os::Wasi | Os::Js | Os::Freestanding => {
                todo!("{}", os.name())
            }
        })
    }

    pub fn from_arch(arch: Arch) -> Self {
        Self::Uint(match arch {
            Arch::Amd64 => 0,
            Arch::_386 | Arch::Arm64 | Arch::Wasm32 | Arch::Wasm64 => todo!("{}", arch.name()),
        })
    }
}
