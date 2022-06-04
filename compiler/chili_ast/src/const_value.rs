use crate::{
    ast::{self, LiteralKind},
    ty::Ty,
    workspace::BindingInfoId,
};
use paste::paste;
use std::{collections::BTreeMap, fmt};
use ustr::Ustr;

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
    Type(Ty),
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

pub type ConstStruct = BTreeMap<Ustr, ConstElement>;

#[derive(Debug, PartialEq, Clone)]
pub struct ConstElement {
    pub value: ConstValue,
    pub ty: Ty,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ConstArray {
    pub values: Vec<ConstValue>,
    pub element_ty: Ty,
}

#[derive(Debug, PartialEq, Clone)]
pub struct ConstFunction {
    pub id: BindingInfoId,
    pub name: Ustr,
}

impl From<LiteralKind> for ConstValue {
    fn from(lit: LiteralKind) -> Self {
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

impl ToString for ConstValue {
    fn to_string(&self) -> String {
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
                "{{{}}}",
                fields
                    .iter()
                    .map(|(name, el)| format!("{}: {}", name, el.value.to_string()))
                    .collect::<Vec<String>>()
                    .join(", "),
            ),
            ConstValue::Function(f) => format!("fn '{}'", f.name),
        }
    }
}
