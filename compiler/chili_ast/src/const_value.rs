use crate::{
    ast::{self, LiteralKind},
    ty::Ty,
};
use paste::paste;
use std::fmt;
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
    // I8(i8),
    // I16(i16),
    // I32(i32),
    // I64(i64),
    // Int(isize),
    // U8(u8),
    // U16(u16),
    // U32(u32),
    // U64(u64),
    // F32(f32),
    // F64(f64),
    // Bool(bool),
    // Aggregate(Aggregate),
    // Array(Array),
    // Pointer(Pointer),
    // Func(Func),
    // ForeignFunc(ForeignFunc),
    // Type(TyKind),
}

impl From<LiteralKind> for ConstValue {
    fn from(lit: LiteralKind) -> Self {
        match lit {
            ast::LiteralKind::Unit => ConstValue::Unit(()),
            ast::LiteralKind::Nil => panic!("nil will soon be deprecated"),
            ast::LiteralKind::Bool(v) => ConstValue::Bool(v),
            ast::LiteralKind::Int(v) => ConstValue::Int(v),
            ast::LiteralKind::Float(v) => ConstValue::Float(v),
            ast::LiteralKind::Str(v) => ConstValue::Str(v),
            ast::LiteralKind::Char(v) => ConstValue::Int(v as i64), // TODO: to uint const value
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
        }
    }
}
