use crate::*;
use std::fmt::Display;

impl Display for Ty {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        use FloatTy::*;
        use IntTy::*;
        use Ty::*;
        use UIntTy::*;

        write!(
            f,
            "{}",
            match self {
                Unit => "()".to_string(),
                Bool => "bool".to_string(),
                Int(inner) => match inner {
                    I8 => "i8",
                    I16 => "i16",
                    I32 => "i32",
                    I64 => "i64",
                    ISize => "int",
                }
                .to_string(),
                UInt(inner) => match inner {
                    U8 => "u8",
                    U16 => "u16",
                    U32 => "u32",
                    U64 => "u64",
                    USize => "uint",
                }
                .to_string(),
                Float(inner) => match inner {
                    F16 => "f16",
                    F32 => "f32",
                    F64 => "f64",
                    FSize => "float",
                }
                .to_string(),
                Pointer(ty, is_mutable) => format!(
                    "*{}{}",
                    if *is_mutable { "mut " } else { "" },
                    ty.to_string()
                ),
                MultiPointer(ty, is_mutable) => format!(
                    "[*{}]{}",
                    if *is_mutable { "mut" } else { "" },
                    ty.to_string()
                ),
                Fn(func) => func.to_string(),
                Array(inner, size) => format!("[{}]{}", size, inner,),
                Slice(inner, is_mutable) => format!(
                    "[]{}{}",
                    if *is_mutable { "mut " } else { "" },
                    inner
                ),
                Tuple(tys) => format!(
                    "{{ {} }}",
                    tys.iter()
                        .map(|t| t.to_string())
                        .collect::<Vec<String>>()
                        .join(", ")
                ),
                Struct(ty) => ty.to_string(),
                Module { .. } => "{module}".to_string(),
                Type(_) => "{type}".to_string(),
                Never => "!".to_string(),
                Unknown | Var(_) => "{unknown}".to_string(),
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
                    "{} {}",
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
