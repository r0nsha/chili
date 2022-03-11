use chili_ast::ast::{
    ArrayLiteralKind, Binding, Block, Builtin, Cast, Expr, ExprKind, Fn, ForIter, Ir, Proto,
};
use chili_ast::ty::*;
use chili_error::{DiagnosticResult, TypeError};
use chili_span::Span;
use ena::unify::InPlaceUnificationTable;

use super::{constraint::Constraint, ty::TyVar};

pub(crate) trait Substitute {
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

        // TODO: put a real span here
        self.ty = substitute_ty(&self.ty, table, Span::unknown())?;

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

        self.value.substitute(table)?;

        self.ty = substitute_ty(&self.ty, table, span)?;

        Ok(())
    }
}

impl Substitute for Cast {
    fn substitute(&mut self, table: &mut InPlaceUnificationTable<TyVar>) -> DiagnosticResult<()> {
        self.expr.substitute(table)?;

        self.type_expr.substitute(table)?;

        self.target_ty = substitute_ty(&self.target_ty, table, self.expr.span)?;

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
            ExprKind::For {
                iter_name: _,
                iter_index_name: _,
                iterator,
                expr,
            } => {
                match iterator {
                    ForIter::Range(start, end) => {
                        start.substitute(table)?;
                        end.substitute(table)?;
                    }
                    ForIter::Value(value) => {
                        value.substitute(table)?;
                    }
                }

                expr.substitute(table)?;
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

        self.ty = substitute_ty(&self.ty, table, self.span)?;

        Ok(())
    }
}

pub(crate) fn substitute_ty(
    ty: &TyKind,
    table: &mut InPlaceUnificationTable<TyVar>,
    span: Span,
) -> DiagnosticResult<TyKind> {
    match ty {
        TyKind::Var(id) => {
            let tyval = table.probe_value(TyVar::from(*id));
            let new_ty = match tyval {
                Constraint::Bound(ty) => substitute_ty(&ty, table, span)?,
                Constraint::Unbound => {
                    return Err(TypeError::type_annotations_needed(span));
                }
            };

            match &new_ty {
                TyKind::Var(v2) => {
                    if *id == *v2 {
                        return Err(TypeError::cant_solve_inference(span));
                    }
                }
                _ => (),
            };

            Ok(new_ty)
        }
        TyKind::Fn(func) => {
            let mut new_params = vec![];

            for param in &func.params {
                new_params.push(FnTyParam {
                    symbol: param.symbol,
                    ty: substitute_ty(&param.ty, table, span)?,
                });
            }

            let new_ret = substitute_ty(&func.ret, table, span)?;

            Ok(TyKind::Fn(FnTy {
                params: new_params,
                ret: Box::new(new_ret),
                variadic: func.variadic,
                lib_name: func.lib_name,
            }))
        }
        TyKind::Pointer(inner, is_mutable) => {
            let inner = substitute_ty(inner, table, span)?;
            Ok(TyKind::Pointer(Box::new(inner), *is_mutable))
        }
        TyKind::MultiPointer(inner, is_mutable) => {
            let inner = substitute_ty(inner, table, span)?;
            Ok(TyKind::MultiPointer(Box::new(inner), *is_mutable))
        }
        TyKind::Slice(inner, is_mutable) => {
            let inner = substitute_ty(inner, table, span)?;
            Ok(TyKind::Slice(Box::new(inner), *is_mutable))
        }
        TyKind::Array(inner, size) => {
            let inner = substitute_ty(inner, table, span)?;
            Ok(TyKind::Array(Box::new(inner), *size))
        }
        TyKind::Tuple(tys) => {
            let mut new_tys = vec![];

            for ty in tys {
                new_tys.push(substitute_ty(ty, table, span)?);
            }

            Ok(TyKind::Tuple(new_tys))
        }
        TyKind::Struct(struct_ty) => {
            let mut fields = vec![];

            for field in &struct_ty.fields {
                let ty = substitute_ty(&field.ty, table, field.span)?;

                fields.push(StructTyField {
                    symbol: field.symbol,
                    ty,
                    span: field.span,
                });
            }

            Ok(TyKind::Struct(StructTy {
                name: struct_ty.name,
                qualified_name: struct_ty.qualified_name,
                kind: struct_ty.kind,
                fields,
            }))
        }
        TyKind::Type(inner) => {
            let inner = substitute_ty(inner, table, span)?;
            Ok(TyKind::Type(Box::new(inner)))
        }
        TyKind::AnyInt => Ok(TyKind::Int(IntTy::default())),
        TyKind::AnyFloat => Ok(TyKind::Float(FloatTy::default())),
        _ => Ok(ty.clone()),
    }
}
