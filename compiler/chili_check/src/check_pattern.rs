use chili_error::{DiagnosticResult, TypeError};
use chili_ty::*;
use codespan_reporting::diagnostic::{Diagnostic, Label};
use ustr::UstrSet;

use crate::{CheckContext, CheckFrame, BindingInfo};
use chili_ast::{
    pattern::{DestructorPattern, Pattern, SymbolPattern},
    value::Value,
};

impl<'a> CheckContext<'a> {
    pub(crate) fn check_binding_pattern(
        &mut self,
        frame: &mut CheckFrame,
        pattern: &Pattern,
        expected_ty: Ty,
        const_value: Option<Value>,
        is_init: bool,
    ) -> DiagnosticResult<()> {
        match pattern {
            Pattern::Single(SymbolPattern {
                binding_info_id: _,
                symbol,
                alias: _,
                span,
                is_mutable,
                ignore,
            }) => {
                if !ignore {
                    frame.insert_binding_info(
                        *symbol,
                        BindingInfo {
                            ty: expected_ty,
                            const_value: const_value.clone(),
                            is_mutable: *is_mutable,
                            is_init,
                            span: *span,
                        },
                    );
                }
            }
            Pattern::StructDestructor(pattern) => {
                let ty = self.infcx.normalize_ty(&expected_ty);
                self.check_struct_destructor(frame, &ty, pattern, is_init)?;
            }
            Pattern::TupleDestructor(pattern) => {
                let ty = self.infcx.normalize_ty(&expected_ty);
                self.check_tuple_destructor(frame, &ty, pattern, is_init)?;
            }
        }

        Ok(())
    }

    fn check_struct_destructor(
        &mut self,
        frame: &mut CheckFrame,
        expected_ty: &Ty,
        pattern: &DestructorPattern,
        is_init: bool,
    ) -> DiagnosticResult<()> {
        match expected_ty.maybe_deref_once() {
            Ty::Struct(ref struct_ty) => {
                if struct_ty.is_union() {
                    return Err(Diagnostic::error()
                        .with_message(format!(
                            "can't destruct `{}`",
                            expected_ty
                        ))
                        .with_labels(vec![Label::primary(
                            pattern.span.file_id,
                            pattern.span.range().clone(),
                        )]));
                }

                let mut field_set = UstrSet::default();

                for SymbolPattern {
                    binding_info_id: _,
                    symbol,
                    alias,
                    span,
                    is_mutable,
                    ignore,
                } in pattern.symbols.iter()
                {
                    if *ignore {
                        continue;
                    }

                    match struct_ty.fields.iter().find(|f| f.symbol == *symbol)
                    {
                        Some(field) => {
                            if !field_set.insert(*symbol) {
                                return Err(
                                    TypeError::duplicate_destructor_field(
                                        *span,
                                        field.symbol,
                                    ),
                                );
                            }

                            let symbol = alias.unwrap_or(*symbol);

                            frame.insert_binding_info(
                                symbol,
                                BindingInfo {
                                    ty: get_destructed_ty(
                                        expected_ty,
                                        &field.ty,
                                    ),
                                    const_value: None,
                                    is_mutable: *is_mutable,
                                    is_init,
                                    span: *span,
                                },
                            );
                        }
                        None => {
                            return Err(TypeError::invalid_struct_field(
                                *span,
                                *symbol,
                                &expected_ty,
                            ))
                        }
                    }
                }

                if pattern.exhaustive
                    && field_set.len() < struct_ty.fields.len()
                {
                    return Err(Diagnostic::error()
                        .with_message(format!(
                            "missing struct fields: {}",
                            struct_ty
                                .fields
                                .iter()
                                .filter(|f| field_set.get(&f.symbol).is_none())
                                .map(|f| f.symbol.as_str())
                                .collect::<Vec<&str>>()
                                .join(", ")
                        ))
                        .with_labels(vec![Label::primary(
                            pattern.span.file_id,
                            pattern.span.range().clone(),
                        )]));
                }

                Ok(())
            }
            ty => {
                return Err(TypeError::struct_destructor_on_invalid_type(
                    pattern.span,
                    &ty,
                ))
            }
        }
    }

    fn check_tuple_destructor(
        &mut self,
        frame: &mut CheckFrame,
        expected_ty: &Ty,
        pattern: &DestructorPattern,
        is_init: bool,
    ) -> DiagnosticResult<()> {
        match expected_ty.maybe_deref_once() {
            Ty::Tuple(tys) => {
                if pattern.symbols.len() > tys.len() {
                    return Err(TypeError::too_many_destructor_variables(
                        pattern.span,
                        expected_ty,
                        tys.len(),
                        pattern.symbols.len(),
                    ));
                }

                for i in 0..pattern.symbols.len() {
                    let SymbolPattern {
                        binding_info_id: _,
                        symbol,
                        alias: _,
                        span,
                        is_mutable,
                        ignore,
                    } = &pattern.symbols[i];

                    if *ignore {
                        continue;
                    }

                    frame.insert_binding_info(
                        *symbol,
                        BindingInfo {
                            ty: get_destructed_ty(expected_ty, &tys[i]),
                            const_value: None,
                            is_mutable: *is_mutable,
                            is_init,
                            span: *span,
                        },
                    );
                }

                Ok(())
            }
            ty => Err(TypeError::tuple_destructor_on_invalid_type(
                pattern.span,
                &ty,
            )),
        }
    }
}

fn get_destructed_ty(expected_ty: &Ty, ty: &Ty) -> Ty {
    match expected_ty {
        Ty::Pointer(_, is_mutable) => {
            Ty::Pointer(Box::new(ty.clone()), *is_mutable)
        }
        _ => ty.clone(),
    }
}
