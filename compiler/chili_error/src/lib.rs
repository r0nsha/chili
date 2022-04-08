use std::process::exit;

use codespan_reporting::{
    diagnostic::*,
    files::SimpleFiles,
    term::termcolor::{ColorChoice, StandardStream},
    term::*,
};
use ustr::Ustr;

use chili_span::Span;

pub type Diagnostics = Vec<Diagnostic<usize>>;
pub type DiagnosticResult<T> = Result<T, Diagnostic<usize>>;

pub fn emit_single_diagnostic(files: &SimpleFiles<String, String>, diagnostic: Diagnostic<usize>) {
    emit_diagnostics(&files, &mut vec![diagnostic])
}

pub fn emit_diagnostics(files: &SimpleFiles<String, String>, diagnostics: &mut Diagnostics) {
    diagnostics.push(Diagnostic::error().with_message(format!(
        "aborting due to {} previous error{}; {} warnings emitted",
        diagnostics.len(),
        if diagnostics.len() > 1 { "s" } else { "" },
        0
    )));

    println!();

    let writer = StandardStream::stderr(ColorChoice::Always);
    let config = Config {
        display_style: DisplayStyle::Rich,
        tab_width: 4,
        styles: Styles::default(),
        chars: Chars::box_drawing(),
        start_context_lines: 3,
        end_context_lines: 1,
    };

    for diagnostic in diagnostics {
        emit(&mut writer.lock(), &config, files, diagnostic).unwrap();
    }

    exit(1);
}

#[derive(Clone, PartialEq, Eq)]
pub struct LexerError;

impl LexerError {
    pub fn integer_too_large(span: Span) -> Diagnostic<usize> {
        Diagnostic::error()
            .with_message("integer literal is too large")
            .with_labels(vec![Label::primary(span.file_id, span.range())])
    }
}

#[derive(Clone, PartialEq, Eq)]
pub struct SyntaxError;

impl SyntaxError {
    pub fn unknown_builtin_function(span: Span, builtin: String) -> Diagnostic<usize> {
        Diagnostic::error()
            .with_message(format!("unknown builtin function `{}`", builtin))
            .with_labels(vec![Label::primary(span.file_id, span.range())])
    }

    pub fn unknown_keyword(keyword: String, span: Span) -> Diagnostic<usize> {
        Diagnostic::error()
            .with_message(format!("unknown keyword `{}`", keyword))
            .with_labels(vec![Label::primary(span.file_id, span.range())])
    }

    pub fn expected(span: Span, expectation: &str) -> Diagnostic<usize> {
        Diagnostic::error()
            .with_message(format!("expected {}", expectation))
            .with_labels(vec![Label::primary(span.file_id, span.range())])
    }

    pub fn illegal_fn_call(span: Span) -> Diagnostic<usize> {
        Diagnostic::error()
            .with_message("illegal function call")
            .with_labels(vec![
                Label::primary(span.file_id, span.range()).with_message("not callable")
            ])
    }

    pub fn unreachable(span: Span) -> Diagnostic<usize> {
        Diagnostic::error()
            .with_message("unreachable expression")
            .with_labels(vec![
                Label::primary(span.file_id, span.range()).with_message("unreachable")
            ])
    }

    pub fn uninit_var(name: String, span: Span) -> Diagnostic<usize> {
        Diagnostic::error()
            .with_message(format!("variable `{}` must be initialized", name))
            .with_labels(vec![
                Label::primary(span.file_id, span.range()).with_message("not initialized")
            ])
    }

    pub fn struct_field_specified_more_than_once(span: Span, name: String) -> Diagnostic<usize> {
        Diagnostic::error()
            .with_message(format!("field `{}` specified more than once", name))
            .with_labels(vec![Label::primary(span.file_id, span.range())])
    }

    pub fn duplicate_symbol(
        already_defined_span: Span,
        duplicate_span: Span,
        name: Ustr,
    ) -> Diagnostic<usize> {
        Diagnostic::error()
            .with_message(format!("duplicate definitions with name `{}`", name))
            .with_labels(vec![
                Label::primary(duplicate_span.file_id, duplicate_span.range())
                    .with_message("duplicate definition here"),
                Label::secondary(already_defined_span.file_id, already_defined_span.range())
                    .with_message(format!("previous definition of `{}` here", name)),
            ])
    }

    pub fn duplicate_struct_field(
        defined_symbol_span: Span,
        symbol_span: Span,
        field_name: String,
    ) -> Diagnostic<usize> {
        Diagnostic::error()
            .with_message(format!(
                "struct field `{}` is defined more than once",
                field_name
            ))
            .with_labels(vec![
                Label::primary(symbol_span.file_id, symbol_span.range())
                    .with_message("field defined more than once"),
                Label::secondary(defined_symbol_span.file_id, defined_symbol_span.range())
                    .with_message(format!("previous definition of `{}` here", field_name)),
            ])
    }

    pub fn outside_of_loop(span: Span, word: &str) -> Diagnostic<usize> {
        let msg = format!("`{}` outside of loop", word);

        Diagnostic::error()
            .with_message(&msg)
            .with_labels(vec![
                Label::primary(span.file_id, span.range()).with_message(msg)
            ])
    }

    pub fn outside_of_function(span: Span, word: &str) -> Diagnostic<usize> {
        Diagnostic::error()
            .with_message(format!("`{}` outside of function", word))
            .with_labels(vec![Label::primary(span.file_id, span.range())])
    }

    pub fn divide_by_zero(span: Span) -> Diagnostic<usize> {
        Diagnostic::error()
            .with_message("division by zero")
            .with_labels(vec![Label::primary(span.file_id, span.range())])
    }
}

#[derive(Clone, PartialEq, Eq)]
pub struct TypeError;

impl TypeError {
    pub fn negative_array_len(span: Span, len: i64) -> Diagnostic<usize> {
        Diagnostic::error()
            .with_message(format!("array length cannot be negative, found {}", len))
            .with_labels(vec![Label::primary(span.file_id, span.range())])
    }

    pub fn circular_type(span: Span, ty_name: &str) -> Diagnostic<usize> {
        Diagnostic::error()
            .with_message(format!("recursive type `{}` has infinite size", ty_name))
            .with_labels(vec![Label::primary(span.file_id, span.range())])
    }

    pub fn tuple_field_out_of_bounds(
        span: Span,
        field: &str,
        ty: String,
        max: usize,
    ) -> Diagnostic<usize> {
        Diagnostic::error()
            .with_message(format!(
                "no field `{}` on type `{}`, expected index between 0 and {}",
                field, ty, max
            ))
            .with_labels(vec![Label::primary(span.file_id, span.range())])
    }

    pub fn non_numeric_tuple_field(span: Span, field: &str, ty: String) -> Diagnostic<usize> {
        Diagnostic::error()
            .with_message(format!(
                "can only access tuple `{}` fields by its indices",
                ty
            ))
            .with_labels(vec![Label::primary(span.file_id, span.range())
                .with_message(format!("invalid tuple member `{}`", field))])
    }

    pub fn struct_unpack_on_invalid_type(span: Span, ty: String) -> Diagnostic<usize> {
        Diagnostic::error()
            .with_message(format!("cannot use struct unpack on type `{}`", ty))
            .with_labels(vec![Label::primary(span.file_id, span.range())])
    }

    pub fn tuple_unpack_on_invalid_type(span: Span, ty: String) -> Diagnostic<usize> {
        Diagnostic::error()
            .with_message(format!("cannot use tuple unpack on type `{}`", ty))
            .with_labels(vec![Label::primary(span.file_id, span.range())])
    }

    pub fn too_many_unpack_variables(
        span: Span,
        ty: String,
        expected_len: usize,
        actual_len: usize,
    ) -> Diagnostic<usize> {
        Diagnostic::error()
            .with_message(format!(
                "too many variables in unpack for type `{}`, expected {} got {}",
                ty, expected_len, actual_len
            ))
            .with_labels(vec![Label::primary(span.file_id, span.range())])
    }

    pub fn duplicate_unpack_field(span: Span, field: Ustr) -> Diagnostic<usize> {
        Diagnostic::error()
            .with_message(format!(
                "field `{}` is defined more than once in unpack",
                field
            ))
            .with_labels(vec![Label::primary(span.file_id, span.range())])
    }

    pub fn invalid_struct_field(span: Span, field: Ustr, ty: String) -> Diagnostic<usize> {
        Diagnostic::error()
            .with_message(format!("no field `{}` on type `{}`", field, ty))
            .with_labels(vec![Label::primary(span.file_id, span.range())])
    }

    pub fn member_access_on_invalid_type(span: Span, ty: String) -> Diagnostic<usize> {
        Diagnostic::error()
            .with_message(format!("type `{}` has no members", ty))
            .with_labels(vec![Label::primary(span.file_id, span.range())])
    }

    pub fn cant_solve_inference(span: Span) -> Diagnostic<usize> {
        Diagnostic::error()
            .with_message("can't infer the expression's type, try adding more type information")
            .with_labels(vec![
                Label::primary(span.file_id, span.range()).with_message("can't infer type")
            ])
    }

    pub fn type_mismatch(span: Span, expected: String, actual: String) -> Diagnostic<usize> {
        Diagnostic::error()
            .with_message(format!(
                "mismatched types - expected `{}`, but found `{}`",
                expected, actual
            ))
            .with_labels(vec![Label::primary(span.file_id, span.range())
                .with_message(format!("expected {}", expected))])
    }

    pub fn invalid_cast(span: Span, from: String, to: String) -> Diagnostic<usize> {
        Diagnostic::error()
            .with_message(format!("cannot cast from `{}` to `{}`", from, to))
            .with_labels(vec![Label::primary(span.file_id, span.range())
                .with_message(format!("invalid cast to `{}`", to))])
    }

    pub fn undefined_type(span: Span, name: Ustr) -> Diagnostic<usize> {
        Diagnostic::error()
            .with_message(format!("undefined type `{}`", name))
            .with_labels(vec![Label::primary(span.file_id, span.range())])
    }

    pub fn undefined_self_type(span: Span) -> Diagnostic<usize> {
        Diagnostic::error()
            .with_message(format!("cannot find type `Self` in this scope. `Self` is only available in type definitions"))
            .with_labels(vec![Label::primary(span.file_id, span.range())])
    }

    pub fn not_a_callable_type(span: Span) -> Diagnostic<usize> {
        Diagnostic::error()
            .with_message("expression is not callable")
            .with_labels(vec![
                Label::primary(span.file_id, span.range()).with_message("not callable")
            ])
    }

    pub fn invalid_ty_in_condition(span: Span, ty: String) -> Diagnostic<usize> {
        Diagnostic::error()
            .with_message(format!("expected type `bool`, but found `{}`", ty))
            .with_labels(vec![Label::primary(span.file_id, span.range())])
    }

    pub fn fn_call_arity_mismatch(span: Span, expected: usize, actual: usize) -> Diagnostic<usize> {
        Diagnostic::error()
            .with_message(format!(
                "function expects {} argument{}, but {} {} supplied",
                expected,
                if expected > 1 { "s" } else { "" },
                actual,
                if actual > 1 { "were" } else { "was" },
            ))
            .with_labels(vec![Label::primary(span.file_id, span.range())
                .with_message(format!(
                    "expected {} argument{}, got {}",
                    expected,
                    if expected > 1 { "s" } else { "" },
                    actual
                ))])
    }

    pub fn struct_call_arity_mismatch(
        span: Span,
        expected: usize,
        actual: usize,
        missing: Vec<Ustr>,
    ) -> Diagnostic<usize> {
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
            .with_labels(vec![Label::primary(span.file_id, span.range())
                .with_message(format!(
                    "expected {} field{}, got {}",
                    expected,
                    if expected > 1 { "s" } else { "" },
                    actual
                ))])
    }

    pub fn deref_non_pointer_ty(span: Span, ty: String) -> Diagnostic<usize> {
        Diagnostic::error()
            .with_message(format!("cannot dereference type `{}`", ty))
            .with_labels(vec![
                Label::primary(span.file_id, span.range()).with_message("not a pointer type")
            ])
    }

    pub fn expected(span: Span, ty: String, expectation: &str) -> Diagnostic<usize> {
        Diagnostic::error()
            .with_message(format!("expected {}, but found {}", expectation, ty))
            .with_labels(vec![Label::primary(span.file_id, span.range())
                .with_message(format!("expected {}", expectation))])
    }

    pub fn invalid_ty_in_unary(span: Span, action: &str, ty: String) -> Diagnostic<usize> {
        Diagnostic::error()
            .with_message(format!("can't `{}` on `{}`", action, ty,))
            .with_labels(vec![Label::primary(span.file_id, span.range())
                .with_message("invalid type in unary operation")])
    }

    pub fn invalid_expr_in_slice(span: Span, ty: String) -> Diagnostic<usize> {
        Diagnostic::error()
            .with_message(format!("cannot slice type `{}`", ty))
            .with_labels(vec![Label::primary(span.file_id, span.range())])
    }

    pub fn invalid_expr_in_subscript(span: Span, ty: String) -> Diagnostic<usize> {
        Diagnostic::error()
            .with_message(format!("cannot subscript type `{}`", ty))
            .with_labels(vec![Label::primary(span.file_id, span.range())])
    }

    pub fn invalid_ty_in_neg(span: Span, ty: String) -> Diagnostic<usize> {
        Diagnostic::error()
            .with_message(format!("cannot negate `{}`", ty))
            .with_labels(vec![
                Label::primary(span.file_id, span.range()).with_message("not a valid number")
            ])
    }
    pub fn type_annotations_needed(span: Span) -> Diagnostic<usize> {
        Diagnostic::error()
            .with_message("type annotations needed")
            .with_labels(vec![
                Label::primary(span.file_id, span.range()).with_message("cannot infer type")
            ])
    }
}
