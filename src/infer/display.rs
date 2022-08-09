use super::{
    normalize::Normalize,
    type_ctx::TypeCtx,
    unify::{UnifyTypeErr, UnifyTypeResult},
};
use crate::{check::symbols, error::DiagnosticResult, types::*};
use crate::{span::Span, workspace::BindingId};

pub trait DisplayType {
    fn display(&self, tcx: &TypeCtx) -> String;
}

impl<T: Normalize> DisplayType for T {
    fn display(&self, tcx: &TypeCtx) -> String {
        display_type(&self.normalize(tcx), tcx)
    }
}

fn display_type(ty: &Type, tcx: &TypeCtx) -> String {
    match ty {
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
        Type::Pointer(ty, is_mutable) => format!("*{}{}", if *is_mutable { "mut " } else { "" }, display_type(ty, tcx)),
        Type::Function(ty) => ty.display(tcx),
        Type::Array(inner, size) => format!("[{}]{}", size, display_type(inner, tcx)),
        Type::Slice(inner) => format!("[]{}", display_type(inner, tcx)),
        Type::Str(_) => "str".to_string(),
        Type::Tuple(tys) | Type::Infer(_, InferType::PartialTuple(tys)) => format!(
            "({})",
            tys.iter()
                .map(|t| display_type(t, tcx))
                .collect::<Vec<String>>()
                .join(", ")
        ),
        Type::Struct(ty) => ty.display(tcx),
        Type::Type(_) | Type::AnyType => "type".to_string(),
        Type::Module(_) => "{module}".to_string(),
        Type::Never => "never".to_string(),
        Type::Infer(_, InferType::PartialStruct(ty)) => ty.display(tcx),
        Type::Infer(_, InferType::AnyInt) => "{integer}".to_string(),
        Type::Infer(_, InferType::AnyFloat) => "{float}".to_string(),
        Type::Var(_) => "?".to_string(),
    }
}

impl DisplayType for StructType {
    fn display(&self, tcx: &TypeCtx) -> String {
        if self.binding_id != BindingId::unknown() {
            self.name.to_string()
        } else {
            format!(
                "{} {{ {} }}",
                match self.kind {
                    StructTypeKind::Struct => "struct",
                    StructTypeKind::PackedStruct => "struct(packed)",
                    StructTypeKind::Union => "union",
                },
                self.fields
                    .iter()
                    .map(|f| format!("{}: {}", f.name, display_type(&f.ty, tcx)))
                    .collect::<Vec<String>>()
                    .join(", ")
            )
        }
    }
}

impl DisplayType for PartialStructType {
    fn display(&self, tcx: &TypeCtx) -> String {
        format!(
            "struct {{ {} }}",
            self.iter()
                .map(|(symbol, ty)| format!("{}: {}", symbol, display_type(ty, tcx)))
                .collect::<Vec<String>>()
                .join(", ")
        )
    }
}

impl DisplayType for FunctionType {
    fn display(&self, tcx: &TypeCtx) -> String {
        format!(
            "fn({}{}) -> {}",
            self.params
                .iter()
                .filter(|param| !symbols::is_implicitly_generated_param(&param.name))
                .map(|param| format!(
                    "{}: {}{}",
                    param.name,
                    display_type(&param.ty, tcx),
                    param
                        .default_value
                        .as_ref()
                        .map(|v| format!(" = {}", v.display(tcx)))
                        .unwrap_or_default(),
                ))
                .collect::<Vec<String>>()
                .join(", "),
            match &self.varargs {
                Some(varargs) => format!(
                    ", {}{}",
                    varargs.name,
                    match &varargs.ty {
                        Some(ty) => format!(": {}...", display_type(ty, tcx)),
                        None => "...".to_string(),
                    }
                ),
                None => "".to_string(),
            },
            display_type(&self.return_type, tcx)
        )
    }
}

pub trait OrReportErr {
    fn or_report_err(
        self,
        tcx: &TypeCtx,
        expected: &impl DisplayType,
        expected_span: Option<Span>,
        found: &impl DisplayType,
        found_span: Span,
    ) -> DiagnosticResult<()>;
}

impl OrReportErr for UnifyTypeResult {
    fn or_report_err(
        self,
        tcx: &TypeCtx,
        expected: &impl DisplayType,
        expected_span: Option<Span>,
        found: &impl DisplayType,
        found_span: Span,
    ) -> DiagnosticResult<()> {
        self.map_err(|e| UnifyTypeErr::into_diagnostic(e, tcx, expected, expected_span, found, found_span))
    }
}
