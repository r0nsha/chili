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
    Int(i128),
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

#[derive(Debug, PartialEq,Eq, Clone)]
pub struct ConstFunction {
    pub id: FunctionId,
    // Name is only used for display purposes
    pub name: Ustr,
}

#[derive(Debug, PartialEq,Eq, Clone)]
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
            ast::LiteralKind::Int(v) => ConstValue::Int(v),
            ast::LiteralKind::Float(v) => ConstValue::Float(v),
            ast::LiteralKind::Str(v) => ConstValue::Str(v),
            ast::LiteralKind::Char(v) => ConstValue::Int(v as i128),
        }
    }
}

impl ConstValue {
    pub fn eq(&self, other: &Self) -> Self {
        Self::Bool(match (self, other) {
            (Self::Unit(_), Self::Unit(_)) => true,
            (Self::Type(left), Self::Type(right)) => *left == *right,
            (Self::Bool(left), Self::Bool(right)) => *left == *right,
            (Self::Int(left), Self::Int(right)) => *left == *right,
            (Self::Int(left), Self::Float(right)) => *left as f64 == *right,
            (Self::Float(left), Self::Int(right)) => *left == *right as f64,
            (Self::Float(left), Self::Float(right)) => *left == *right,
            (Self::Str(left), Self::Str(right)) => *left == *right,
            (Self::Array(left), Self::Array(right)) => {
                left.values.len() == right.values.len()
                    && left
                        .values
                        .iter()
                        .zip(right.values.iter())
                        .all(|(left, right)| *left.eq(right).as_bool().unwrap())
            }
            (Self::Tuple(left), Self::Tuple(right)) => {
                left.len() == right.len()
                    && left
                        .iter()
                        .zip(right.iter())
                        .all(|(left, right)| *left.value.eq(&right.value).as_bool().unwrap())
            }
            (Self::Struct(left), Self::Struct(right)) => {
                left.len() == right.len()
                    && left.iter().zip(right.iter()).all(|((lname, left), (rname, right))| {
                        lname == rname && *left.value.eq(&right.value).as_bool().unwrap()
                    })
            }
            (Self::Function(left), Self::Function(right)) => left.id == right.id,
            _ => false,
        })
    }

    pub fn ne(&self, other: &Self) -> Self {
        self.eq(other).not()
    }

    pub fn not(&self) -> Self {
        match self {
            Self::Bool(v) => Self::Bool(!v),
            Self::Int(v) => Self::Int(!v),
            _ => panic!("got {:?}", self),
        }
    }

    pub fn neg(&self) -> Self {
        match self {
            Self::Int(i) => Self::Int(-i),
            Self::Float(f) => Self::Float(-f),
            _ => panic!("got {:?}", self),
        }
    }

    pub fn add(&self, other: &Self) -> Option<Self> {
        match (self, other) {
            (Self::Int(v1), Self::Int(v2)) => v1.checked_add(*v2).map(Self::Int),
            (Self::Int(v1), Self::Float(v2)) => Some(Self::Float(*v1 as f64 + *v2)),
            (Self::Float(v1), Self::Int(v2)) => Some(Self::Float(*v1 + *v2 as f64)),
            (Self::Float(v1), Self::Float(v2)) => Some(Self::Float(*v1 + *v2)),
            _ => panic!("got {:?}", self),
        }
    }

    pub fn sub(&self, other: &Self) -> Option<Self> {
        match (self, other) {
            (Self::Int(v1), Self::Int(v2)) => v1.checked_sub(*v2).map(Self::Int),
            (Self::Int(v1), Self::Float(v2)) => Some(Self::Float(*v1 as f64 - *v2)),
            (Self::Float(v1), Self::Int(v2)) => Some(Self::Float(*v1 - *v2 as f64)),
            (Self::Float(v1), Self::Float(v2)) => Some(Self::Float(*v1 - *v2)),
            _ => panic!("got {:?}", self),
        }
    }

    pub fn mul(&self, other: &Self) -> Option<Self> {
        match (self, other) {
            (Self::Int(v1), Self::Int(v2)) => v1.checked_mul(*v2).map(Self::Int),
            (Self::Int(v1), Self::Float(v2)) => Some(Self::Float(*v1 as f64 + *v2)),
            (Self::Float(v1), Self::Int(v2)) => Some(Self::Float(*v1 + *v2 as f64)),
            (Self::Float(v1), Self::Float(v2)) => Some(Self::Float(*v1 + *v2)),
            _ => panic!("got {:?}", self),
        }
    }

    pub fn div(&self, other: &Self) -> Option<Self> {
        match (self, other) {
            (Self::Int(v1), Self::Int(v2)) => v1.checked_div(*v2).map(Self::Int),
            (Self::Int(v1), Self::Float(v2)) => Some(Self::Float(*v1 as f64 / *v2)),
            (Self::Float(v1), Self::Int(v2)) => Some(Self::Float(*v1 / *v2 as f64)),
            (Self::Float(v1), Self::Float(v2)) => Some(Self::Float(*v1 / *v2)),
            _ => panic!("got {:?}", self),
        }
    }

    pub fn rem(&self, other: &Self) -> Option<Self> {
        match (self, other) {
            (Self::Int(v1), Self::Int(v2)) => v1.checked_rem(*v2).map(Self::Int),
            (Self::Int(v1), Self::Float(v2)) => Some(Self::Float(*v1 as f64 % *v2)),
            (Self::Float(v1), Self::Int(v2)) => Some(Self::Float(*v1 % *v2 as f64)),
            (Self::Float(v1), Self::Float(v2)) => Some(Self::Float(*v1 % *v2)),
            _ => panic!("got {:?}", self),
        }
    }

    pub fn lt(&self, other: &Self) -> Self {
        match (self, other) {
            (Self::Int(v1), Self::Int(v2)) => Self::Bool(*v1 < *v2),
            (Self::Int(v1), Self::Float(v2)) => Self::Bool((*v1 as f64) < *v2),
            (Self::Float(v1), Self::Int(v2)) => Self::Bool(*v1 < *v2 as f64),
            (Self::Float(v1), Self::Float(v2)) => Self::Bool(*v1 < *v2),
            _ => panic!("got {:?}", self),
        }
    }

    pub fn le(&self, other: &Self) -> Self {
        match (self, other) {
            (Self::Int(v1), Self::Int(v2)) => Self::Bool(*v1 <= *v2),
            (Self::Int(v1), Self::Float(v2)) => Self::Bool((*v1 as f64) <= *v2),
            (Self::Float(v1), Self::Int(v2)) => Self::Bool(*v1 <= *v2 as f64),
            (Self::Float(v1), Self::Float(v2)) => Self::Bool(*v1 <= *v2),
            _ => panic!("got {:?}", self),
        }
    }

    pub fn gt(&self, other: &Self) -> Self {
        match (self, other) {
            (Self::Int(v1), Self::Int(v2)) => Self::Bool(*v1 > *v2),
            (Self::Int(v1), Self::Float(v2)) => Self::Bool((*v1 as f64) > *v2),
            (Self::Float(v1), Self::Int(v2)) => Self::Bool(*v1 > *v2 as f64),
            (Self::Float(v1), Self::Float(v2)) => Self::Bool(*v1 > *v2),
            _ => panic!("got {:?}", self),
        }
    }

    pub fn ge(&self, other: &Self) -> Self {
        match (self, other) {
            (Self::Int(v1), Self::Int(v2)) => Self::Bool(*v1 >= *v2),
            (Self::Int(v1), Self::Float(v2)) => Self::Bool((*v1 as f64) >= *v2),
            (Self::Float(v1), Self::Int(v2)) => Self::Bool(*v1 >= *v2 as f64),
            (Self::Float(v1), Self::Float(v2)) => Self::Bool(*v1 >= *v2),
            _ => panic!("got {:?}", self),
        }
    }

    pub fn and(&self, other: &Self) -> Self {
        match (self, other) {
            (Self::Bool(v1), Self::Bool(v2)) => Self::Bool(*v1 && *v2),
            _ => panic!("got {:?}", self),
        }
    }

    pub fn or(&self, other: &Self) -> Self {
        match (self, other) {
            (Self::Bool(v1), Self::Bool(v2)) => Self::Bool(*v1 || *v2),
            _ => panic!("got {:?}", self),
        }
    }

    pub fn shl(&self, other: &Self) -> Option<Self> {
        match (self, other) {
            (Self::Int(v1), Self::Int(v2)) => v1.checked_shl(*v2 as _).map(Self::Int),
            _ => panic!("got {:?}", self),
        }
    }

    pub fn shr(&self, other: &Self) -> Option<Self> {
        match (self, other) {
            (Self::Int(v1), Self::Int(v2)) => v1.checked_shr(*v2 as _).map(Self::Int),
            _ => panic!("got {:?}", self),
        }
    }

    pub fn bitand(&self, other: &Self) -> Self {
        match (self, other) {
            (Self::Int(v1), Self::Int(v2)) => Self::Int(*v1 & *v2),
            _ => panic!("got {:?}", self),
        }
    }

    pub fn bitor(&self, other: &Self) -> Self {
        match (self, other) {
            (Self::Int(v1), Self::Int(v2)) => Self::Int(*v1 | *v2),
            _ => panic!("got {:?}", self),
        }
    }

    pub fn bitxor(&self, other: &Self) -> Self {
        match (self, other) {
            (Self::Int(v1), Self::Int(v2)) => Self::Int(*v1 ^ *v2),
            _ => panic!("got {:?}", self),
        }
    }

    #[allow(unused)]
    pub fn display(&self, tcx: &TypeCtx) -> String {
        match self {
            Self::Unit(_) => "()".to_string(),
            Self::Type(t) => t.display(tcx),
            Self::Bool(v) => format!("{}", v),
            Self::Int(v) => format!("{}", v),
            Self::Float(v) => format!("{}", v),
            Self::Str(v) => format!("\"{}\"", v),
            Self::Array(array) => format!(
                "[{}]",
                array
                    .values
                    .iter()
                    .map(|v| v.display(tcx))
                    .collect::<Vec<String>>()
                    .join(", "),
            ),
            Self::Tuple(elements) => format!(
                "({})",
                elements
                    .iter()
                    .map(|el| el.value.display(tcx))
                    .collect::<Vec<String>>()
                    .join(", "),
            ),
            Self::Struct(fields) => format!(
                "{{ {} }}",
                fields
                    .iter()
                    .map(|(name, el)| format!("{}: {}", name, el.value.display(tcx)))
                    .collect::<Vec<String>>()
                    .join(", "),
            ),
            Self::Function(f) => f.name.to_string(),
            Self::ExternVariable(v) => v.name.to_string(),
        }
    }

    pub fn from_os(os: Os) -> Self {
        Self::Int(match os {
            Os::Linux => 0,
            Os::Windows => 1,
            Os::Darwin | Os::Essence | Os::FreeBSD | Os::Wasi | Os::Js | Os::Freestanding => {
                todo!("{}", os.name())
            }
        })
    }

    pub fn from_arch(arch: Arch) -> Self {
        Self::Int(match arch {
            Arch::Amd64 => 0,
            Arch::_386 | Arch::Arm64 | Arch::Wasm32 | Arch::Wasm64 => todo!("{}", arch.name()),
        })
    }
}
