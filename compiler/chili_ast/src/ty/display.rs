use super::*;
use std::fmt::{self, Display};

impl fmt::Display for Ty {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "[unknown]")
    }
}

impl Display for TyKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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
                TyKind::Uint(inner) => match inner {
                    UintTy::U8 => "u8",
                    UintTy::U16 => "u16",
                    UintTy::U32 => "u32",
                    UintTy::U64 => "u64",
                    UintTy::Uint => "uint",
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
                    format!("[{}; *{}]", ty, if *is_mutable { "mut" } else { "" },),
                TyKind::Function(func) => func.to_string(),
                TyKind::Array(inner, size) => format!("[{}; {}]", inner, size,),
                TyKind::Slice(inner, is_mutable) =>
                    format!("[{}{}]", if *is_mutable { "mut " } else { "" }, inner),
                TyKind::Tuple(tys) | TyKind::Infer(_, InferTy::PartialTuple(tys)) => format!(
                    "({})",
                    tys.iter()
                        .map(|t| t.to_string())
                        .collect::<Vec<String>>()
                        .join(", ")
                ),
                TyKind::Struct(ty) => ty.to_string(),
                TyKind::Type(_) | TyKind::AnyType => "type".to_string(),
                TyKind::Module(_) => "[module]".to_string(),
                TyKind::Never => "never".to_string(),
                TyKind::Infer(_, InferTy::PartialStruct(ty)) => ty.to_string(),
                TyKind::Infer(_, InferTy::AnyInt) => "[integer]".to_string(),
                TyKind::Infer(_, InferTy::AnyFloat) => "[float]".to_string(),
                TyKind::Var(v) => v.to_string(),
                TyKind::Unknown => "[unknown]".to_string(),
            }
        )
    }
}

impl Display for StructTy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.binding_info_id != BindingInfoId::unknown() {
            write!(f, "{}", self.name.as_str())
        } else {
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
        }
    }
}

impl Display for PartialStructTy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
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

impl Display for FunctionTy {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "fn({}{}) -> {}",
            self.params
                .iter()
                .map(|p| p.to_string())
                .collect::<Vec<String>>()
                .join(", "),
            match &self.varargs {
                Some(v) => match &v.ty {
                    Some(ty) => format!(", ..{}", ty),
                    None => ", ..".to_string(),
                },
                None => "".to_string(),
            },
            self.ret,
        )
    }
}
