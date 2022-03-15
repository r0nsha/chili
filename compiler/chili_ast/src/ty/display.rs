use super::*;
use std::fmt::Display;

impl Display for Ty {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Ty::Unit => "()".to_string(),
                Ty::Bool => "bool".to_string(),
                Ty::Int(inner) => match inner {
                    IntTy::I8 => "i8",
                    IntTy::I16 => "i16",
                    IntTy::I32 => "i32",
                    IntTy::I64 => "i64",
                    IntTy::Isize => "int",
                }
                .to_string(),
                Ty::UInt(inner) => match inner {
                    UIntTy::U8 => "u8",
                    UIntTy::U16 => "u16",
                    UIntTy::U32 => "u32",
                    UIntTy::U64 => "u64",
                    UIntTy::Usize => "uint",
                }
                .to_string(),
                Ty::Float(inner) => match inner {
                    FloatTy::F16 => "f16",
                    FloatTy::F32 => "f32",
                    FloatTy::F64 => "f64",
                    FloatTy::Fsize => "float",
                }
                .to_string(),
                Ty::Pointer(ty, is_mutable) => format!(
                    "*{}{}",
                    if *is_mutable { "mut " } else { "" },
                    ty.to_string()
                ),
                Ty::MultiPointer(ty, is_mutable) => format!(
                    "[*{}]{}",
                    if *is_mutable { "mut" } else { "" },
                    ty.to_string()
                ),
                Ty::Fn(func) => func.to_string(),
                Ty::Array(inner, size) => format!("[{}]{}", size, inner,),
                Ty::Slice(inner, is_mutable) =>
                    format!("[]{}{}", if *is_mutable { "mut " } else { "" }, inner),
                Ty::Tuple(tys) => format!(
                    "{{ {} }}",
                    tys.iter()
                        .map(|t| t.to_string())
                        .collect::<Vec<String>>()
                        .join(", ")
                ),
                Ty::Struct(ty) => ty.to_string(),
                Ty::Module { .. } => "[module]".to_string(),
                Ty::Type(_) => "type".to_string(),
                Ty::Never => "!".to_string(),
                Ty::AnyInt => "[int]".to_string(),
                Ty::AnyFloat => "[float]".to_string(),
                Ty::Var(v) => format!("'{}", v),
                Ty::Unknown => "[unknown]".to_string(),
            }
        )
    }
}

impl Display for StructTy {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.name.is_empty() {
            write!(
                f,
                "{} {{ {} }}",
                match self.kind {
                    StructTyKind::Struct => "struct",
                    StructTyKind::PackedStruct => "struct(packed)",
                    StructTyKind::Union => "union",
                },
                self.fields
                    .iter()
                    .map(|f| format!("{}: {}", f.symbol, f.ty))
                    .collect::<Vec<String>>()
                    .join(", ")
            )
        } else {
            write!(f, "{}", self.name)
        }
    }
}

impl Display for FnTy {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "fn({}{}) -> {}",
            self.params
                .iter()
                .map(|p| format!(
                    "{}: {}",
                    if p.symbol.is_empty() { "_" } else { &p.symbol },
                    p.ty.to_string()
                ))
                .collect::<Vec<String>>()
                .join(", "),
            if self.variadic { ", .." } else { "" },
            self.ret.to_string(),
        )
    }
}
