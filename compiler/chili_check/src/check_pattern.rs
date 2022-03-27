use chili_ast::ty::*;
use chili_error::{DiagnosticResult, TypeError};
use codespan_reporting::diagnostic::{Diagnostic, Label};
use ustr::UstrSet;

use crate::CheckSess;
use chili_ast::{
    pattern::{Pattern, SymbolPattern, UnpackPattern},
    value::Value,
};

impl<'c> CheckSess<'c> {
    pub(crate) fn check_binding_pattern(
        &mut self,
        pattern: &Pattern,
        expected_ty: Ty,
        const_value: Option<Value>,
    ) -> DiagnosticResult<()> {
        match pattern {
            Pattern::Single(pat) => {
                self.update_symbol_pattern_ty(pat, expected_ty);
                self.workspace
                    .get_binding_info_mut(pat.binding_info_id)
                    .unwrap()
                    .const_value = const_value;
            }
            Pattern::StructUnpack(pattern) => {
                let ty = self.infcx.normalize_ty(&expected_ty);
                self.check_struct_unpack(&ty, pattern)?;
            }
            Pattern::TupleUnpack(pattern) => {
                let ty = self.infcx.normalize_ty(&expected_ty);
                self.check_tuple_unpack(&ty, pattern)?;
            }
        }

        Ok(())
    }

    fn check_struct_unpack(
        &mut self,
        expected_ty: &Ty,
        pattern: &UnpackPattern,
    ) -> DiagnosticResult<()> {
        match expected_ty.maybe_deref_once() {
            Ty::Struct(ref struct_ty) => {
                if struct_ty.is_union() {
                    return Err(Diagnostic::error()
                        .with_message(format!("can't destruct `{}`", expected_ty))
                        .with_labels(vec![Label::primary(
                            pattern.span.file_id,
                            pattern.span.range(),
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
                                return Err(TypeError::duplicate_unpack_field(
                                    pat.span,
                                    field.symbol,
                                ));
                            }

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

                Ok(())
            }
            ty => {
                return Err(TypeError::struct_unpack_on_invalid_type(
                    pattern.span,
                    ty.to_string(),
                ))
            }
        }
    }

    fn check_tuple_unpack(
        &mut self,
        expected_ty: &Ty,
        pattern: &UnpackPattern,
    ) -> DiagnosticResult<()> {
        match expected_ty.maybe_deref_once() {
            Ty::Tuple(tys) => {
                if pattern.symbols.len() > tys.len() {
                    return Err(TypeError::too_many_unpack_variables(
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
            ty => Err(TypeError::tuple_unpack_on_invalid_type(
                pattern.span,
                ty.to_string(),
            )),
        }
    }

    fn update_symbol_pattern_ty(&mut self, pattern: &SymbolPattern, ty: Ty) {
        if !pattern.ignore {
            self.update_binding_info_ty(pattern.binding_info_id, ty);
        }
    }
}

fn get_destructed_ty(expected_ty: &Ty, ty: &Ty) -> Ty {
    match expected_ty {
        Ty::Pointer(_, is_mutable) => Ty::Pointer(Box::new(ty.clone()), *is_mutable),
        _ => ty.clone(),
    }
}
