use crate::{ast::LiteralKind, ty::Ty};
use paste::paste;
use std::fmt;

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
                            _ => panic!("got {:?}, expected {}", self, stringify!($variant))
                        }
                    }
                )+

                $(
                    pub fn [<as_ $variant:snake>](&self) -> &$ty {
                        match self {
                            Self::$variant(v) => v,
                            _ => panic!("got {:?}, expected {}", self, stringify!($variant))
                        }
                    }
                )+

                $(
                    pub fn [<as_ $variant:snake _mut>](&mut self) -> &mut $ty {
                        match self {
                            Self::$variant(v) => v,
                            _ => panic!("got {:?}, expected {}", self, stringify!($variant))
                        }
                    }
                )+
            }
        }
    };
}

impl_value! {
    Type(Ty),
    Bool(bool),
    Int(i64),
    Float(f64),
    // I8(i8),
    // I16(i16),
    // I32(i32),
    // I64(i64),
    // Int(isize),
    // U8(u8),
    // U16(u16),
    // U32(u32),
    // U64(u64),
    // Uint(usize),
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

// I8(i8),
// I16(i16),
// I32(i32),
// I64(i64),
// Int(isize),
// U8(u8),
// U16(u16),
// U32(u32),
// U64(u64),
// Uint(usize),
// F32(f32),
// F64(f64),
// Bool(bool),
// Aggregate(Aggregate),
// Array(Array),
// Pointer(Pointer),
// Func(Func),
// ForeignFunc(ForeignFunc),
// Type(TyKind),

impl ConstValue {
    pub fn as_literal(&self) -> LiteralKind {
        match self {
            ConstValue::Type(_) => panic!("unexpected Value::Type"),
            ConstValue::Bool(v) => LiteralKind::Bool(*v),
            ConstValue::Int(v) => LiteralKind::Int(*v),
            ConstValue::Float(v) => LiteralKind::Float(*v),
        }
    }
}
