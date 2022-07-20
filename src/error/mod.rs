pub mod diagnostic;
pub mod emitter;

use crate::span::{FileId, Span};
use codespan_reporting::files::{SimpleFile, SimpleFiles};
use diagnostic::{Diagnostic, DiagnosticSeverity, Label};
use emitter::{ColorMode, DiagnosticEmitter};
use ustr::Ustr;

pub fn emit_diagnostics(diagnostics: &Diagnostics, color_mode: ColorMode) {
    let emitter = DiagnosticEmitter::new(color_mode);
    emitter.emit_many(&diagnostics.files, diagnostics.items.clone());
}

#[derive(Debug, Clone)]
pub struct Diagnostics {
    files: SimpleFiles<String, String>,
    items: Vec<Diagnostic>,
}

impl Diagnostics {
    pub fn new() -> Self {
        Self {
            files: SimpleFiles::new(),
            items: vec![],
        }
    }

    pub fn add_file(&mut self, name: String, source: String) -> FileId {
        self.files.add(name, source)
    }

    pub fn get_file(&self, file_id: FileId) -> Option<&SimpleFile<String, String>> {
        self.files.get(file_id).ok()
    }

    pub fn items(&self) -> &[Diagnostic] {
        &self.items
    }

    pub fn push(&mut self, diagnostic: Diagnostic) {
        self.items.push(diagnostic);
    }

    pub fn extend(&mut self, diagnostics: impl IntoIterator<Item = Diagnostic>) {
        self.items.extend(diagnostics);
    }

    pub fn error_count(&self) -> usize {
        self.items
            .iter()
            .filter(|d| d.severity == DiagnosticSeverity::Error)
            .count()
    }

    pub fn has_errors(&self) -> bool {
        self.error_count() > 0
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
    pub fn expected(span: Span, expectation: &str) -> Diagnostic {
        Diagnostic::error()
            .with_message(format!("expected {}", expectation))
            .with_label(Label::primary(span, ""))
    }

    pub fn struct_field_specified_more_than_once(span: Span, name: String) -> Diagnostic {
        Diagnostic::error()
            .with_message(format!("field `{}` specified more than once", name))
            .with_label(Label::primary(span, ""))
    }

    pub fn duplicate_binding(
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
        defined_field_span: Span,
        field_span: Span,
        field_name: String,
    ) -> Diagnostic {
        Diagnostic::error()
            .with_message(format!(
                "struct field `{}` is defined more than once",
                field_name
            ))
            .with_label(Label::primary(field_span, "field defined more than once"))
            .with_label(Label::secondary(
                defined_field_span,
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
    pub fn type_is_unsized(ty: String, span: Span) -> Diagnostic {
        Diagnostic::error()
            .with_message(format!(
                "the size of type `{}` cannot be known at compile-time",
                ty
            ))
            .with_label(Label::primary(
                span,
                "doesn't have a size known at compile-time",
            ))
    }

    pub fn binding_is_unsized(name: &str, ty: String, span: Span) -> Diagnostic {
        Diagnostic::error()
            .with_message(format!(
                "the size of `{}`'s type `{}` cannot be known at compile-time",
                name, ty
            ))
            .with_label(Label::primary(
                span,
                "doesn't have a size known at compile-time",
            ))
    }

    pub fn negative_array_len(span: Span, len: i64) -> Diagnostic {
        Diagnostic::error()
            .with_message(format!("array length cannot be negative, found {}", len))
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

    pub fn invalid_struct_field(span: Span, field: Ustr, ty: String) -> Diagnostic {
        Diagnostic::error()
            .with_message(format!("no field `{}` on type `{}`", field, ty))
            .with_label(Label::primary(span, ""))
    }

    pub fn expected(span: Span, ty: String, expectation: &str) -> Diagnostic {
        Diagnostic::error()
            .with_message(format!("expected {}, found {}", expectation, ty))
            .with_label(Label::primary(span, format!("expected {}", expectation)))
    }
}
