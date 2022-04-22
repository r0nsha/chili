use super::*;
use std::fmt::Display;

impl fmt::Display for Ty {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[unknown]")
    }
}

impl Display for TyKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                TyKind::Unit => "()".to_string(),
                TyKind::Bool => "bool".to_string(),
                TyKind::Int(inner) => match inner {
                    IntTy::I8 => "i8",
                    IntTy::I16 => "i16",
                    IntTy::I32 => "i32",
                    IntTy::I64 => "i64",
                    IntTy::Int => "int",
                }
                .to_string(),
                TyKind::UInt(inner) => match inner {
                    UIntTy::U8 => "u8",
                    UIntTy::U16 => "u16",
                    UIntTy::U32 => "u32",
                    UIntTy::U64 => "u64",
                    UIntTy::UInt => "uint",
                }
                .to_string(),
                TyKind::Float(inner) => match inner {
                    FloatTy::F16 => "f16",
                    FloatTy::F32 => "f32",
                    FloatTy::F64 => "f64",
                    FloatTy::Float => "float",
                }
                .to_string(),
                TyKind::Pointer(ty, is_mutable) =>
                    format!("*{}{}", if *is_mutable { "mut " } else { "" }, ty),
                TyKind::MultiPointer(ty, is_mutable) =>
                    format!("[*{}]{}", if *is_mutable { "mut" } else { "" }, ty),
                TyKind::Fn(func) => func.to_string(),
                TyKind::Array(inner, size) => format!("[{}]{}", size, inner,),
                TyKind::Slice(inner, is_mutable) =>
                    format!("[]{}{}", if *is_mutable { "mut " } else { "" }, inner),
                TyKind::Tuple(tys) => format!(
                    "({})",
                    tys.iter()
                        .map(|t| t.to_string())
                        .collect::<Vec<String>>()
                        .join(", ")
                ),
                TyKind::Struct(ty) => ty.to_string(),
                TyKind::Type(inner) => inner.to_string(),
                TyKind::Module(_) => "[module]".to_string(),
                TyKind::Never => "!".to_string(),
                TyKind::Infer(_, InferTy::PartialStruct(ty)) => ty.to_string(),
                TyKind::Infer(_, InferTy::AnyInt) => "[anyint]".to_string(),
                TyKind::Infer(_, InferTy::AnyFloat) => "[anyfloat]".to_string(),
                TyKind::Var(v) => v.to_string(),
                TyKind::Unknown => "[unknown]".to_string(),
            }
        )
    }
}

impl Display for StructTy {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} {{ {} }}",
            if self.name.is_empty() {
                match self.kind {
                    StructTyKind::Struct => "struct",
                    StructTyKind::PackedStruct => "struct(packed)",
                    StructTyKind::Union => "union",
                }
            } else {
                self.name.as_str()
            },
            self.fields
                .iter()
                .map(|f| format!("{}: {}", f.symbol, f.ty))
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
}

impl Display for PartialStructTy {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "struct {{ {} }}",
            self.iter()
                .map(|(symbol, ty)| format!("{}: {}", symbol, ty))
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
}

impl Display for FnTy {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "fn({}{}) -> {}",
            self.params
                .iter()
                .map(|p| p.to_string())
                .collect::<Vec<String>>()
                .join(", "),
            if self.variadic { ", .." } else { "" },
            self.ret,
        )
    }
}
