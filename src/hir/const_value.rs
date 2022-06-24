use crate::ast::{self, ty::TypeId};
use indexmap::IndexMap;
use paste::paste;
use std::fmt::{self, Display};
use ustr::Ustr;

use super::FunctionId;

macro_rules! impl_value {
    ($($variant:ident($ty:ty)) , + $(,)?) => {
        #[derive(Debug, PartialEq, Clone, Copy)]
        pub enum ConstValueKind {
            $(
                $variant
            ),+
        }

        impl fmt::Display for ConstValueKind {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                write!(f, "{}", match self {
                    $(
                        ConstValueKind::$variant => String::from(stringify!($variant))
                    ),+
                })
            }
        }

        #[derive(Debug, PartialEq, Clone)]
        pub enum ConstValue {
            $(
                $variant($ty)
            ),+
        }

        impl ConstValue {
            #[allow(dead_code)]
            pub fn kind(&self) -> ConstValueKind {
                match self {
                    $(
                        ConstValue::$variant(_) => ConstValueKind::$variant
                    ),+
                }
            }

            paste! {
                $(
                    pub fn [<is_ $variant:snake>](&self) -> bool {
                        match self {
                            Self::$variant(_) => true,
                            _ => false
                        }
                    }
                )+

                $(
                    pub fn [<into_ $variant:snake>](self) -> $ty {
                        match self {
                            Self::$variant(v) => v,
                            _ => panic!("got {}, expected {}", self.to_string(), stringify!($variant))
                        }
                    }
                )+

                $(
                    pub fn [<as_ $variant:snake>](&self) -> &$ty {
                        match self {
                            Self::$variant(v) => v,
                            _ => panic!("got {}, expected {}", self.to_string(), stringify!($variant))
                        }
                    }
                )+

                $(
                    pub fn [<as_ $variant:snake _mut>](&mut self) -> &mut $ty {
                        match self {
                            Self::$variant(v) => v,
                            _ => panic!("got {}, expected {}", self.to_string(), stringify!($variant))
                        }
                    }
                )+
            }
        }
    };
}

impl_value! {
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

impl Display for ConstValue {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                ConstValue::Unit(_) => "unit".to_string(),
                ConstValue::Type(t) => format!("ty {}", t),
                ConstValue::Bool(v) => format!("bool {}", v),
                ConstValue::Int(v) => format!("int {}", v),
                ConstValue::Uint(v) => format!("uint {}", v),
                ConstValue::Float(v) => format!("float {}", v),
                ConstValue::Str(v) => format!("str {}", v),
                ConstValue::Array(array) => format!(
                    "[{}]",
                    array
                        .values
                        .iter()
                        .map(|v| v.to_string())
                        .collect::<Vec<String>>()
                        .join(", "),
                ),
                ConstValue::Tuple(elements) => format!(
                    "({})",
                    elements
                        .iter()
                        .map(|el| el.value.to_string())
                        .collect::<Vec<String>>()
                        .join(", "),
                ),
                ConstValue::Struct(fields) => format!(
                    "{{ {} }}",
                    fields
                        .iter()
                        .map(|(name, el)| format!("{}: {}", name, el.value.to_string()))
                        .collect::<Vec<String>>()
                        .join(", "),
                ),
                ConstValue::Function(f) => format!("fn '{}'", f.name),
            }
        )
    }
}

impl ConstValue {
    pub fn not(&self) -> ConstValue {
        match self {
            ConstValue::Bool(v) => ConstValue::Bool(!v),
            ConstValue::Int(v) => ConstValue::Int(!v),
            ConstValue::Uint(v) => ConstValue::Uint(!v),
            _ => unreachable!("got {:?}", self),
        }
    }

    pub fn neg(&self) -> ConstValue {
        match self {
            ConstValue::Int(i) => ConstValue::Int(-i),
            ConstValue::Float(f) => ConstValue::Float(-f),
            _ => unreachable!("got {:?}", self),
        }
    }

    pub fn add(&self, other: &ConstValue) -> ConstValue {
        match (self, other) {
            (ConstValue::Int(v1), ConstValue::Int(v2)) => ConstValue::Int(*v1 + *v2),
            (ConstValue::Uint(v1), ConstValue::Uint(v2)) => ConstValue::Uint(*v1 + *v2),

            (ConstValue::Int(v1), ConstValue::Uint(v2)) => ConstValue::Int(*v1 + *v2 as i64),
            (ConstValue::Uint(v2), ConstValue::Int(v1)) => ConstValue::Int(*v1 + *v2 as i64),

            (ConstValue::Int(v1), ConstValue::Float(v2)) => ConstValue::Float(*v1 as f64 + *v2),
            (ConstValue::Float(v2), ConstValue::Int(v1)) => ConstValue::Float(*v1 as f64 + *v2),

            (ConstValue::Uint(v1), ConstValue::Float(v2)) => ConstValue::Float(*v1 as f64 + *v2),
            (ConstValue::Float(v2), ConstValue::Uint(v1)) => ConstValue::Float(*v1 as f64 + *v2),

            (ConstValue::Float(v1), ConstValue::Float(v2)) => ConstValue::Float(*v1 + *v2),
            _ => unreachable!("got {:?}", self),
        }
    }
}
