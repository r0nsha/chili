pub mod diagnostic;
pub mod emitter;

use chili_span::{FileId, Span};
use codespan_reporting::files::SimpleFiles;
use diagnostic::{Diagnostic, DiagnosticKind, Label};
use emitter::{ColorMode, DiagnosticEmitter};
use ustr::Ustr;

pub struct Diagnostics {
    files: SimpleFiles<String, String>,
    emitter: DiagnosticEmitter,
    diagnostics: Vec<Diagnostic>,
}

impl Diagnostics {
    pub fn new(color_mode: ColorMode) -> Self {
        Self {
            files: SimpleFiles::new(),
            emitter: DiagnosticEmitter::new(color_mode),
            diagnostics: vec![],
        }
    }

    pub fn add_file(&mut self, name: String, source: String) -> FileId {
        self.files.add(name, source)
    }

    pub fn errors(&self) -> &[Diagnostic] {
        &self.diagnostics
    }

    pub fn push(&mut self, diagnostic: Diagnostic) {
        self.diagnostics.push(diagnostic);
    }

    pub fn extend(&mut self, diagnostics: impl IntoIterator<Item = Diagnostic>) {
        self.diagnostics.extend(diagnostics);
    }

    pub fn count(&self) -> usize {
        self.diagnostics.len()
    }

    pub fn error_count(&self) -> usize {
        self.diagnostics
            .iter()
            .filter(|d| d.kind == DiagnosticKind::Error)
            .count()
    }

    pub fn has_errors(&self) -> bool {
        self.error_count() > 0
    }

    pub fn emit(&self) {
        self.emitter
            .emit_many(&self.files, self.diagnostics.clone());
    }
}

pub type DiagnosticResult<T> = Result<T, diagnostic::Diagnostic>;

#[derive(Clone, PartialEq, Eq)]
pub struct LexerError;

impl LexerError {
    pub fn integer_too_large(span: Span) -> Diagnostic {
        Diagnostic::error()
            .with_message("integer literal is too large")
            .with_label(Label::primary(span, ""))
    }
}

#[derive(Clone, PartialEq, Eq)]
pub struct SyntaxError;

impl SyntaxError {
    pub fn unknown_builtin_function(span: Span, builtin: String) -> Diagnostic {
        Diagnostic::error()
            .with_message(format!("unknown builtin function `{}`", builtin))
            .with_label(Label::primary(span, ""))
    }

    pub fn unknown_keyword(keyword: String, span: Span) -> Diagnostic {
        Diagnostic::error()
            .with_message(format!("unknown keyword `{}`", keyword))
            .with_label(Label::primary(span, ""))
    }

    pub fn expected(span: Span, expectation: &str) -> Diagnostic {
        Diagnostic::error()
            .with_message(format!("expected {}", expectation))
            .with_label(Label::primary(span, ""))
    }

    pub fn illegal_fn_call(span: Span) -> Diagnostic {
        Diagnostic::error()
            .with_message("illegal function call")
            .with_label(Label::primary(span, "not callable"))
    }

    pub fn unreachable(span: Span) -> Diagnostic {
        Diagnostic::error()
            .with_message("unreachable expression")
            .with_label(Label::primary(span, "unreachable"))
    }

    pub fn uninit_var(name: String, span: Span) -> Diagnostic {
        Diagnostic::error()
            .with_message(format!("variable `{}` must be initialized", name))
            .with_label(Label::primary(span, "not initialized"))
    }

    pub fn struct_field_specified_more_than_once(span: Span, name: String) -> Diagnostic {
        Diagnostic::error()
            .with_message(format!("field `{}` specified more than once", name))
            .with_label(Label::primary(span, ""))
    }

    pub fn duplicate_symbol(
        already_defined_span: Span,
        duplicate_span: Span,
        name: Ustr,
    ) -> Diagnostic {
        Diagnostic::error()
            .with_message(format!("duplicate definitions with name `{}`", name))
            .with_label(Label::primary(duplicate_span, "duplicate definition here"))
            .with_label(Label::secondary(
                already_defined_span,
                format!("previous definition of `{}` here", name),
            ))
    }

    pub fn duplicate_struct_field(
        defined_symbol_span: Span,
        symbol_span: Span,
        field_name: String,
    ) -> Diagnostic {
        Diagnostic::error()
            .with_message(format!(
                "struct field `{}` is defined more than once",
                field_name
            ))
            .with_label(Label::primary(symbol_span, "field defined more than once"))
            .with_label(Label::secondary(
                defined_symbol_span,
                format!("previous definition of `{}` here", field_name),
            ))
    }

    pub fn outside_of_loop(span: Span, word: &str) -> Diagnostic {
        let msg = format!("`{}` outside of loop", word);
        Diagnostic::error()
            .with_message(&msg)
            .with_label(Label::primary(span, msg))
    }

    pub fn outside_of_function(span: Span, word: &str) -> Diagnostic {
        Diagnostic::error()
            .with_message(format!("`{}` outside of function", word))
            .with_label(Label::primary(span, ""))
    }

    pub fn divide_by_zero(span: Span) -> Diagnostic {
        Diagnostic::error()
            .with_message("division by zero")
            .with_label(Label::primary(span, ""))
    }
}

#[derive(Clone, PartialEq, Eq)]
pub struct TypeError;

impl TypeError {
    pub fn negative_array_len(span: Span, len: i64) -> Diagnostic {
        Diagnostic::error()
            .with_message(format!("array length cannot be negative, found {}", len))
            .with_label(Label::primary(span, ""))
    }

    pub fn circular_type(span: Span, ty_name: &str) -> Diagnostic {
        Diagnostic::error()
            .with_message(format!("recursive type `{}` has infinite size", ty_name))
            .with_label(Label::primary(span, ""))
    }

    pub fn tuple_field_out_of_bounds(
        span: Span,
        field: &str,
        ty: String,
        max: usize,
    ) -> Diagnostic {
        Diagnostic::error()
            .with_message(format!(
                "no field `{}` on type `{}`, expected index between 0 and {}",
                field, ty, max
            ))
            .with_label(Label::primary(span, ""))
    }

    pub fn non_numeric_tuple_field(span: Span, field: &str, ty: String) -> Diagnostic {
        Diagnostic::error()
            .with_message(format!(
                "can only access tuple `{}` fields by its indices",
                ty
            ))
            .with_label(Label::primary(
                span,
                format!("invalid tuple member `{}`", field),
            ))
    }

    pub fn struct_unpack_on_invalid_type(span: Span, ty: String) -> Diagnostic {
        Diagnostic::error()
            .with_message(format!("cannot use struct unpack on type `{}`", ty))
            .with_label(Label::primary(span, ""))
    }

    pub fn tuple_unpack_on_invalid_type(span: Span, ty: String) -> Diagnostic {
        Diagnostic::error()
            .with_message(format!("cannot use tuple unpack on type `{}`", ty))
            .with_label(Label::primary(span, ""))
    }

    pub fn too_many_unpack_variables(
        span: Span,
        ty: String,
        expected_len: usize,
        actual_len: usize,
    ) -> Diagnostic {
        Diagnostic::error()
            .with_message(format!(
                "too many variables in unpack for type `{}`, expected {} got {}",
                ty, expected_len, actual_len
            ))
            .with_label(Label::primary(span, ""))
    }

    pub fn duplicate_unpack_field(span: Span, field: Ustr) -> Diagnostic {
        Diagnostic::error()
            .with_message(format!(
                "field `{}` is defined more than once in unpack",
                field
            ))
            .with_label(Label::primary(span, ""))
    }

    pub fn invalid_struct_field(span: Span, field: Ustr, ty: String) -> Diagnostic {
        Diagnostic::error()
            .with_message(format!("no field `{}` on type `{}`", field, ty))
            .with_label(Label::primary(span, ""))
    }

    pub fn member_access_on_invalid_type(span: Span, ty: String) -> Diagnostic {
        Diagnostic::error()
            .with_message(format!("type `{}` has no members", ty))
            .with_label(Label::primary(span, ""))
    }

    pub fn type_mismatch(span: Span, expected: String, actual: String) -> Diagnostic {
        Diagnostic::error()
            .with_message(format!(
                "mismatched types - expected `{}`, found `{}`",
                expected, actual
            ))
            .with_label(Label::primary(span, format!("expected {}", expected)))
    }

    pub fn invalid_cast(span: Span, from: String, to: String) -> Diagnostic {
        Diagnostic::error()
            .with_message(format!("cannot cast from `{}` to `{}`", from, to))
            .with_label(Label::primary(span, format!("invalid cast to `{}`", to)))
    }

    pub fn undefined_type(span: Span, name: Ustr) -> Diagnostic {
        Diagnostic::error()
            .with_message(format!("undefined type `{}`", name))
            .with_label(Label::primary(span, ""))
    }

    pub fn undefined_self_type(span: Span) -> Diagnostic {
        Diagnostic::error()
            .with_message("cannot find type `Self` in this scope. `Self` is only available in type definitions".to_string())
            .with_label(Label::primary(span, ""))
    }

    pub fn not_a_callable_type(span: Span) -> Diagnostic {
        Diagnostic::error()
            .with_message("expression is not callable")
            .with_label(Label::primary(span, "not callable"))
    }

    pub fn invalid_ty_in_condition(span: Span, ty: String) -> Diagnostic {
        Diagnostic::error()
            .with_message(format!("expected type `bool`, found `{}`", ty))
            .with_label(Label::primary(span, ""))
    }

    pub fn struct_call_arity_mismatch(
        span: Span,
        expected: usize,
        actual: usize,
        missing: Vec<Ustr>,
    ) -> Diagnostic {
        Diagnostic::error()
            .with_message(format!(
                "struct expects {} field{}, but {} {} supplied. missing: {}",
                expected,
                if expected > 1 { "s" } else { "" },
                actual,
                if actual > 1 { "were" } else { "was" },
                missing
                    .iter()
                    .map(|m| m.as_str())
                    .collect::<Vec<&str>>()
                    .join(", ")
            ))
            .with_label(Label::primary(
                span,
                format!(
                    "expected {} field{}, got {}",
                    expected,
                    if expected > 1 { "s" } else { "" },
                    actual
                ),
            ))
    }

    pub fn expected(span: Span, ty: String, expectation: &str) -> Diagnostic {
        Diagnostic::error()
            .with_message(format!("expected {}, found {}", expectation, ty))
            .with_label(Label::primary(span, format!("expected {}", expectation)))
    }
}
