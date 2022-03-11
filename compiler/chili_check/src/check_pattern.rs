use chili_ast::ty::*;
use chili_error::{DiagnosticResult, TypeError};
use codespan_reporting::diagnostic::{Diagnostic, Label};
use ustr::UstrSet;

use crate::{CheckFrame, CheckSess};
use chili_ast::{
    pattern::{DestructorPattern, Pattern, SymbolPattern},
    value::Value,
};

impl<'w, 'a> CheckSess<'w, 'a> {
    pub(crate) fn check_binding_pattern(
        &mut self,
        frame: &mut CheckFrame,
        pattern: &Pattern,
        expected_ty: Ty,
        const_value: Option<Value>,
        is_init: bool,
    ) -> DiagnosticResult<()> {
        match pattern {
            Pattern::Single(pat) => {
                self.update_symbol_pattern_ty(pat, expected_ty);
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
                        .with_message(format!("can't destruct `{}`", expected_ty))
                        .with_labels(vec![Label::primary(
                            pattern.span.file_id,
                            pattern.span.range().clone(),
                        )]));
                }

                let mut field_set = UstrSet::default();

                for pat in pattern.symbols.iter() {
                    if pat.ignore {
                        continue;
                    }

                    match struct_ty.fields.iter().find(|f| f.symbol == pat.symbol) {
                        Some(field) => {
                            if !field_set.insert(pat.symbol) {
                                return Err(TypeError::duplicate_destructor_field(
                                    pat.span,
                                    field.symbol,
                                ));
                            }

                            let symbol = pat.alias.unwrap_or(pat.symbol);
                            self.update_symbol_pattern_ty(
                                pat,
                                get_destructed_ty(expected_ty, &field.ty),
                            );
                        }
                        None => {
                            return Err(TypeError::invalid_struct_field(
                                pat.span,
                                pat.symbol,
                                expected_ty.to_string(),
                            ))
                        }
                    }
                }

                if pattern.exhaustive && field_set.len() < struct_ty.fields.len() {
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
                    ty.to_string(),
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
                        expected_ty.to_string(),
                        tys.len(),
                        pattern.symbols.len(),
                    ));
                }

                for i in 0..pattern.symbols.len() {
                    let pat = &pattern.symbols[i];

                    if pat.ignore {
                        continue;
                    }

                    self.update_symbol_pattern_ty(pat, get_destructed_ty(expected_ty, &tys[i]));
                }

                Ok(())
            }
            ty => Err(TypeError::tuple_destructor_on_invalid_type(
                pattern.span,
                ty.to_string(),
            )),
        }
    }

    fn update_symbol_pattern_ty(&mut self, pattern: &SymbolPattern, ty: Ty) {
        if !pattern.ignore {
            self.workspace
                .get_binding_info_mut(pattern.binding_info_idx)
                .unwrap()
                .ty = ty;
        }
    }
}

fn get_destructed_ty(expected_ty: &Ty, ty: &Ty) -> Ty {
    match expected_ty {
        Ty::Pointer(_, is_mutable) => Ty::Pointer(Box::new(ty.clone()), *is_mutable),
        _ => ty.clone(),
    }
}
