use super::*;
use std::fmt::{self, Display};

impl Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Type::Unit => "()".to_string(),
                Type::Bool => "bool".to_string(),
                Type::Int(inner) => match inner {
                    IntType::I8 => "i8",
                    IntType::I16 => "i16",
                    IntType::I32 => "i32",
                    IntType::I64 => "i64",
                    IntType::Int => "int",
                }
                .to_string(),
                Type::Uint(inner) => match inner {
                    UintType::U8 => "u8",
                    UintType::U16 => "u16",
                    UintType::U32 => "u32",
                    UintType::U64 => "u64",
                    UintType::Uint => "uint",
                }
                .to_string(),
                Type::Float(inner) => match inner {
                    FloatType::F16 => "f16",
                    FloatType::F32 => "f32",
                    FloatType::F64 => "f64",
                    FloatType::Float => "float",
                }
                .to_string(),
                Type::Pointer(ty, is_mutable) => format!("*{}{}", if *is_mutable { "mut " } else { "" }, ty),
                Type::Function(func) => func.to_string(),
                Type::Array(inner, size) => format!("[{}]{}", size, inner),
                Type::Slice(inner) => format!("[]{}", inner),
                Type::Tuple(tys) | Type::Infer(_, InferType::PartialTuple(tys)) => format!(
                    "({})",
                    tys.iter().map(|t| t.to_string()).collect::<Vec<String>>().join(", ")
                ),
                Type::Struct(ty) => ty.to_string(),
                Type::Type(_) | Type::AnyType => "type".to_string(),
                Type::Module(_) => "{module}".to_string(),
                Type::Never => "never".to_string(),
                Type::Infer(_, InferType::PartialStruct(ty)) => ty.to_string(),
                Type::Infer(_, InferType::AnyInt) => "{integer}".to_string(),
                Type::Infer(_, InferType::AnyFloat) => "{float}".to_string(),
                Type::Var(_) => "?".to_string(),
            }
        )
    }
}

impl Display for StructType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if self.binding_id != BindingId::unknown() {
            write!(f, "{}", self.name.as_str())
        } else {
            write!(
                f,
                "{} {{ {} }}",
                match self.kind {
                    StructTypeKind::Struct => "struct",
                    StructTypeKind::PackedStruct => "struct(packed)",
                    StructTypeKind::Union => "union",
                },
                self.fields
                    .iter()
                    .map(|f| format!("{}: {}", f.name, f.ty))
                    .collect::<Vec<String>>()
                    .join(", ")
            )
        }
    }
}

impl Display for PartialStructType {
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

impl Display for FunctionType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "fn({}{}) -> {}",
            self.params
                .iter()
                .map(|p| format!(
                    "{}: {}{}",
                    p.name,
                    p.ty,
                    // TODO: display default value as part of the type
                    "" //p.default_value.as_ref().map(|v| v.display(tcx)).unwrap_or_default()
                ))
                .collect::<Vec<String>>()
                .join(", "),
            match &self.varargs {
                Some(v) => match &v.ty {
                    Some(ty) => format!(", ..{}", ty),
                    None => ", ..".to_string(),
                },
                None => "".to_string(),
            },
            self.return_type,
        )
    }
}
