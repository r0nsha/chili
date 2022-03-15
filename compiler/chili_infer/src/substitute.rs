use chili_ast::ast::{
    ArrayLiteralKind, Binding, Block, Builtin, Cast, Expr, ExprKind, Fn, ForIter, Ir, Proto,
};
use chili_ast::ty::*;
use chili_error::{DiagnosticResult, TypeError};
use chili_span::Span;
use ena::unify::InPlaceUnificationTable;

use super::sess::{InferValue, TyVar};

pub trait Substitute {
    fn substitute(&mut self, table: &mut InPlaceUnificationTable<TyVar>) -> DiagnosticResult<()>;
}

impl<T: Substitute> Substitute for Vec<T> {
    fn substitute(&mut self, table: &mut InPlaceUnificationTable<TyVar>) -> DiagnosticResult<()> {
        for element in self {
            element.substitute(table)?;
        }
        Ok(())
    }
}

impl<T: Substitute> Substitute for Option<T> {
    fn substitute(&mut self, table: &mut InPlaceUnificationTable<TyVar>) -> DiagnosticResult<()> {
        if let Some(e) = self {
            e.substitute(table)?;
        }
        Ok(())
    }
}

impl<T: Substitute> Substitute for Box<T> {
    fn substitute(&mut self, table: &mut InPlaceUnificationTable<TyVar>) -> DiagnosticResult<()> {
        self.as_mut().substitute(table)
    }
}

impl Substitute for Ir {
    fn substitute(&mut self, table: &mut InPlaceUnificationTable<TyVar>) -> DiagnosticResult<()> {
        for (_, module) in self.modules.iter_mut() {
            module.bindings.substitute(table)?;

            for binding in module.bindings.iter_mut() {
                binding.substitute(table)?;
            }
        }

        Ok(())
    }
}

impl Substitute for Block {
    fn substitute(&mut self, table: &mut InPlaceUnificationTable<TyVar>) -> DiagnosticResult<()> {
        self.exprs.substitute(table)?;
        self.deferred.substitute(table)?;
        Ok(())
    }
}

impl Substitute for Fn {
    fn substitute(&mut self, table: &mut InPlaceUnificationTable<TyVar>) -> DiagnosticResult<()> {
        self.proto.substitute(table)?;
        self.body.substitute(table)?;
        Ok(())
    }
}

impl Substitute for Proto {
    fn substitute(&mut self, table: &mut InPlaceUnificationTable<TyVar>) -> DiagnosticResult<()> {
        for param in self.params.iter_mut() {
            param.ty.substitute(table)?;
        }

        self.ret.substitute(table)?;

        self.ty = self.ty.substitute_ty(table, Span::unknown())?;

        Ok(())
    }
}

impl Substitute for Binding {
    fn substitute(&mut self, table: &mut InPlaceUnificationTable<TyVar>) -> DiagnosticResult<()> {
        let span = match &mut self.ty_expr {
            Some(e) => {
                e.substitute(table)?;
                e.span
            }
            None => Span::unknown(),
        };

        self.expr.substitute(table)?;

        self.ty = self.ty.substitute_ty(table, span)?;

        Ok(())
    }
}

impl Substitute for Cast {
    fn substitute(&mut self, table: &mut InPlaceUnificationTable<TyVar>) -> DiagnosticResult<()> {
        self.expr.substitute(table)?;

        self.type_expr.substitute(table)?;

        self.target_ty = self.target_ty.substitute_ty(table, self.expr.span)?;

        Ok(())
    }
}

impl Substitute for Expr {
    fn substitute(&mut self, table: &mut InPlaceUnificationTable<TyVar>) -> DiagnosticResult<()> {
        match &mut self.kind {
            ExprKind::Import(..) | ExprKind::Defer(_) => (),
            ExprKind::Foreign(bindings) => {
                bindings.substitute(table)?;
            }
            ExprKind::Binding(binding) => {
                binding.substitute(table)?;
            }
            ExprKind::Assign { lvalue, rvalue } => {
                lvalue.substitute(table)?;
                rvalue.substitute(table)?;
            }
            ExprKind::Cast(info) => info.substitute(table)?,
            ExprKind::Builtin(builtin) => match builtin {
                Builtin::SizeOf(expr) | Builtin::AlignOf(expr) => expr.substitute(table)?,
                Builtin::Panic(expr) => {
                    expr.substitute(table)?;
                }
            },
            ExprKind::Fn(func) => {
                func.substitute(table)?;
            }
            ExprKind::While { cond, expr } => {
                cond.substitute(table)?;
                expr.substitute(table)?;
            }
            ExprKind::For(for_) => {
                match &mut for_.iterator {
                    ForIter::Range(start, end) => {
                        start.substitute(table)?;
                        end.substitute(table)?;
                    }
                    ForIter::Value(value) => {
                        value.substitute(table)?;
                    }
                }

                for_.expr.substitute(table)?;
            }
            ExprKind::Break { deferred } | ExprKind::Continue { deferred } => {
                deferred.substitute(table)?;
            }
            ExprKind::Return { expr, deferred } => {
                deferred.substitute(table)?;
                expr.substitute(table)?;
            }
            ExprKind::If {
                cond,
                then_expr,
                else_expr,
            } => {
                cond.substitute(table)?;
                then_expr.substitute(table)?;
                else_expr.substitute(table)?;
            }
            ExprKind::Block(block) => {
                block.exprs.substitute(table)?;
                block.deferred.substitute(table)?;
            }
            ExprKind::Binary { lhs, op: _, rhs } => {
                lhs.substitute(table)?;
                rhs.substitute(table)?;
            }
            ExprKind::Unary { op: _, lhs } => {
                lhs.substitute(table)?;
            }
            ExprKind::Subscript { expr, index } => {
                expr.substitute(table)?;
                index.substitute(table)?;
            }
            ExprKind::Slice { expr, low, high } => {
                expr.substitute(table)?;
                low.substitute(table)?;
                high.substitute(table)?;
            }
            ExprKind::Call(call) => {
                call.callee.substitute(table)?;
                for arg in call.args.iter_mut() {
                    arg.value.substitute(table)?;
                }
            }
            ExprKind::MemberAccess { expr, .. } => {
                expr.substitute(table)?;
            }
            ExprKind::ArrayLiteral(kind) => match kind {
                ArrayLiteralKind::List(elements) => {
                    elements.substitute(table)?;
                }
                ArrayLiteralKind::Fill { expr, len } => {
                    len.substitute(table)?;
                    expr.substitute(table)?;
                }
            },
            ExprKind::TupleLiteral(elements) => {
                elements.substitute(table)?;
            }
            ExprKind::StructLiteral { type_expr, fields } => {
                type_expr.substitute(table)?;
                for f in fields {
                    f.value.substitute(table)?;
                }
            }

            ExprKind::PointerType(expr, ..)
            | ExprKind::MultiPointerType(expr, ..)
            | ExprKind::SliceType(expr, ..)
            | ExprKind::ArrayType(expr, ..) => expr.substitute(table)?,

            ExprKind::StructType(struct_type, ..) => {
                for f in struct_type.fields.iter_mut() {
                    f.ty.substitute(table)?;
                }
            }

            ExprKind::FnType(proto) => {
                proto.substitute(table)?;
            }

            // not used
            ExprKind::Id { .. }
            | ExprKind::Literal(_)
            | ExprKind::SelfType
            | ExprKind::NeverType
            | ExprKind::UnitType
            | ExprKind::PlaceholderType => (),

            ExprKind::Noop => return Ok(()), /* Noop is skipped */
        }

        self.ty = self.ty.substitute_ty(table, self.span)?;

        Ok(())
    }
}

pub trait SubstituteTy {
    fn substitute_ty(
        &self,
        table: &mut InPlaceUnificationTable<TyVar>,
        span: Span,
    ) -> DiagnosticResult<Ty>;
}

impl SubstituteTy for Ty {
    fn substitute_ty(
        &self,
        table: &mut InPlaceUnificationTable<TyVar>,
        span: Span,
    ) -> DiagnosticResult<Ty> {
        match self {
            Ty::Var(id) => {
                let tyval = table.probe_value(TyVar::from(*id));
                let new_ty = match tyval {
                    InferValue::Bound(ty) => ty.substitute_ty(table, span)?,
                    InferValue::UntypedInt => Ty::Int(IntTy::default()),
                    InferValue::UntypedFloat => Ty::Float(FloatTy::default()),
                    InferValue::UntypedNil => Ty::raw_pointer(true),
                    InferValue::Unbound => {
                        return Err(TypeError::type_annotations_needed(span));
                    }
                };

                match &new_ty {
                    Ty::Var(v2) => {
                        if *id == *v2 {
                            return Err(TypeError::cant_solve_inference(span));
                        }
                    }
                    _ => (),
                };

                Ok(new_ty)
            }
            Ty::Fn(func) => {
                let mut new_params = vec![];

                for param in &func.params {
                    new_params.push(FnTyParam {
                        symbol: param.symbol,
                        ty: param.ty.substitute_ty(table, span)?,
                    });
                }

                let new_ret = func.ret.substitute_ty(table, span)?;

                Ok(Ty::Fn(FnTy {
                    params: new_params,
                    ret: Box::new(new_ret),
                    variadic: func.variadic,
                    lib_name: func.lib_name,
                }))
            }
            Ty::Pointer(inner, is_mutable) => {
                let inner = inner.substitute_ty(table, span)?;
                Ok(Ty::Pointer(Box::new(inner), *is_mutable))
            }
            Ty::MultiPointer(inner, is_mutable) => {
                let inner = inner.substitute_ty(table, span)?;
                Ok(Ty::MultiPointer(Box::new(inner), *is_mutable))
            }
            Ty::Slice(inner, is_mutable) => {
                let inner = inner.substitute_ty(table, span)?;
                Ok(Ty::Slice(Box::new(inner), *is_mutable))
            }
            Ty::Array(inner, size) => {
                let inner = inner.substitute_ty(table, span)?;
                Ok(Ty::Array(Box::new(inner), *size))
            }
            Ty::Tuple(tys) => {
                let mut new_tys = vec![];

                for ty in tys {
                    new_tys.push(ty.substitute_ty(table, span)?);
                }

                Ok(Ty::Tuple(new_tys))
            }
            Ty::Struct(struct_ty) => {
                let mut fields = vec![];

                for field in &struct_ty.fields {
                    let ty = field.ty.substitute_ty(table, field.span)?;

                    fields.push(StructTyField {
                        symbol: field.symbol,
                        ty,
                        span: field.span,
                    });
                }

                Ok(Ty::Struct(StructTy {
                    name: struct_ty.name,
                    qualified_name: struct_ty.qualified_name,
                    binding_info_id: struct_ty.binding_info_id,
                    kind: struct_ty.kind,
                    fields,
                }))
            }
            Ty::Type(inner) => {
                let inner = inner.substitute_ty(table, span)?;
                Ok(Ty::Type(Box::new(inner)))
            }
            _ => Ok(self.clone()),
        }
    }
}
