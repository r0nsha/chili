use crate::{display::map_unify_err, sess::InferSess, unify::Unify};
use chili_ast::{
    ast,
    ty::{FnTy, FnTyParam, Ty},
    workspace::Workspace,
};
use chili_error::DiagnosticResult;
use chili_span::Span;

pub(crate) trait Infer {
    fn infer(&mut self, sess: &mut InferSess, workspace: &mut Workspace) -> DiagnosticResult<Ty>;
}

impl Infer for ast::Ast {
    fn infer(&mut self, sess: &mut InferSess, workspace: &mut Workspace) -> DiagnosticResult<Ty> {
        for import in self.imports.iter_mut() {
            import.infer(sess, workspace)?;
        }

        for binding in self.bindings.iter_mut() {
            binding.infer(sess, workspace)?;
        }

        Ok(Ty::Unit)
    }
}

impl Infer for ast::Import {
    fn infer(&mut self, sess: &mut InferSess, workspace: &mut Workspace) -> DiagnosticResult<Ty> {
        // TODO:
        Ok(Ty::Unit)
    }
}

impl Infer for ast::Binding {
    fn infer(&mut self, sess: &mut InferSess, workspace: &mut Workspace) -> DiagnosticResult<Ty> {
        // TODO: support other patterns
        let pat = self.pattern.as_single_ref();
        let binding_info_ty = workspace
            .get_binding_info(pat.binding_info_id)
            .unwrap()
            .ty
            .clone();

        // TODO: type annotation

        if let Some(expr) = &mut self.expr {
            expr.infer(sess, workspace)?;

            binding_info_ty
                .unify(&expr.ty, sess, workspace, expr.span)
                .map_err(|e| map_unify_err(e, expr.span))?;
        }

        Ok(Ty::Unit)
    }
}

impl Infer for ast::Fn {
    fn infer(&mut self, sess: &mut InferSess, workspace: &mut Workspace) -> DiagnosticResult<Ty> {
        let proto_ty = self.proto.infer(sess, workspace)?;
        let fn_ty = proto_ty.as_fn();
        let ty = self.body.infer(sess, workspace)?;

        fn_ty
            .ret
            .unify(
                &ty,
                sess,
                workspace,
                self.proto.ret.as_ref().map_or(Span::unknown(), |e| e.span),
            )
            .map_err(|e| map_unify_err(e, self.body.span))?;

        Ok(proto_ty)
    }
}

impl Infer for ast::Proto {
    fn infer(&mut self, sess: &mut InferSess, workspace: &mut Workspace) -> DiagnosticResult<Ty> {
        let mut params = vec![];

        for param in self.params.iter_mut() {
            // TODO: support other patterns
            let pat = param.pattern.as_single_ref();
            let binding_info = workspace.get_binding_info_mut(pat.binding_info_id).unwrap();

            // TODO: param type annotation
            binding_info.ty = sess.new_variable().into();

            params.push(FnTyParam {
                symbol: pat.symbol,
                ty: binding_info.ty.clone(),
            })
        }

        let ret = if let Some(ret) = &mut self.ret {
            // TODO: return type annotation
            sess.new_variable().into()
        } else {
            sess.new_variable().into()
        };

        let ty = Ty::Fn(FnTy {
            params,
            ret: Box::new(ret),
            variadic: self.variadic,
            lib_name: self.lib_name,
        });

        self.ty = ty.clone();

        Ok(ty)
    }
}

impl Infer for ast::Block {
    fn infer(&mut self, sess: &mut InferSess, workspace: &mut Workspace) -> DiagnosticResult<Ty> {
        let mut result_ty = Ty::Unit;

        for expr in self.exprs.iter_mut() {
            result_ty = expr.infer(sess, workspace)?;
        }

        for expr in self.deferred.iter_mut() {
            expr.infer(sess, workspace)?;
        }

        Ok(result_ty)
    }
}

impl Infer for ast::Expr {
    fn infer(&mut self, sess: &mut InferSess, workspace: &mut Workspace) -> DiagnosticResult<Ty> {
        self.ty = match &mut self.kind {
            ast::ExprKind::Import(imports) => {
                for import in imports.iter_mut() {
                    import.infer(sess, workspace)?;
                }
                Ty::Unit
            }
            ast::ExprKind::Foreign(bindings) => {
                for binding in bindings.iter_mut() {
                    binding.infer(sess, workspace)?;
                }
                Ty::Unit
            }
            ast::ExprKind::Binding(binding) => {
                binding.infer(sess, workspace)?;
                Ty::Unit
            }
            ast::ExprKind::Defer(_) => todo!(),
            ast::ExprKind::Assign { lvalue, rvalue } => todo!(),
            ast::ExprKind::Cast(_) => todo!(),
            ast::ExprKind::Builtin(_) => todo!(),
            ast::ExprKind::Fn(f) => f.infer(sess, workspace)?,
            ast::ExprKind::While { cond, expr } => todo!(),
            ast::ExprKind::For(_) => todo!(),
            ast::ExprKind::Break { deferred } | ast::ExprKind::Continue { deferred } => {
                for expr in deferred.iter_mut() {
                    expr.infer(sess, workspace)?;
                }
                Ty::Never
            }
            ast::ExprKind::Return { expr, deferred } => {
                if let Some(expr) = expr {
                    expr.infer(sess, workspace)?;
                    // TODO: unify expr.ty with sess.return_ty
                } else {
                    // TODO: unify expr.ty with Ty::Unit
                }

                for expr in deferred.iter_mut() {
                    expr.infer(sess, workspace)?;
                }

                Ty::Never
            }
            ast::ExprKind::If {
                cond,
                then_expr,
                else_expr,
            } => todo!(),
            ast::ExprKind::Block(_) => todo!(),
            ast::ExprKind::Binary { lhs, op, rhs } => {
                todo!("Hello Binary");
            }
            ast::ExprKind::Unary { op, lhs } => todo!(),
            ast::ExprKind::Subscript { expr, index } => todo!(),
            ast::ExprKind::Slice { expr, low, high } => todo!(),
            ast::ExprKind::Call(_) => todo!(),
            ast::ExprKind::MemberAccess { expr, member } => todo!(),
            // TODO: this is wrong, and will not work for more complex scenarios
            // TODO: i have two options:
            // TODO: 1) create a `typed ast`, and resolve the binding ad-hoc.
            // TODO:    this is the same solution from my previous check pass which worked really well!
            // TODO: 2) do the infer-unify-substitute loop until everything is covered.
            ast::ExprKind::Id {
                binding_info_id, ..
            } => workspace
                .get_binding_info(*binding_info_id)
                .unwrap()
                .ty
                .clone(),
            ast::ExprKind::ArrayLiteral(_) => todo!(),
            ast::ExprKind::TupleLiteral(_) => todo!(),
            ast::ExprKind::StructLiteral { type_expr, fields } => todo!(),
            ast::ExprKind::Literal(lit) => lit.infer(sess, workspace)?,
            ast::ExprKind::PointerType(_, _) => todo!(),
            ast::ExprKind::MultiPointerType(_, _) => todo!(),
            ast::ExprKind::ArrayType(_, _) => todo!(),
            ast::ExprKind::SliceType(_, _) => todo!(),
            ast::ExprKind::StructType(_) => todo!(),
            ast::ExprKind::FnType(_) => todo!(),
            ast::ExprKind::SelfType => todo!(),
            ast::ExprKind::NeverType => todo!(),
            ast::ExprKind::UnitType => todo!(),
            ast::ExprKind::PlaceholderType => todo!(),
            ast::ExprKind::Noop => todo!(),
        };

        Ok(self.ty.clone())
    }
}

impl Infer for ast::Literal {
    fn infer(&mut self, _: &mut InferSess, _: &mut Workspace) -> DiagnosticResult<Ty> {
        let ty = match self {
            ast::Literal::Unit => Ty::Unit,
            ast::Literal::Nil => Ty::raw_pointer(true),
            ast::Literal::Bool(_) => Ty::Bool,
            ast::Literal::Int(_) => Ty::AnyInt,
            ast::Literal::Float(_) => Ty::AnyFloat,
            ast::Literal::Str(_) => Ty::str(),
            ast::Literal::Char(_) => Ty::char(),
        };
        Ok(ty)
    }
}

// impl<'a> Inferable<'a> for ast::Literal<'a> {
//     fn infer_impl(&mut self, cache: &mut ModuleCache<'a>) -> (Type, TraitConstraints) {
//         use ast::LiteralKind::*;
//         match self.kind {
//             Integer(x, kind) => {
//                 if kind == IntegerKind::Unknown {
//                     // Mutate this unknown integer literal to an IntegerKind::Inferred(int_type).
//                     // Also add `Int int_type` constraint to restrict this type variable to one
//                     // of the native integer types.
//                     let int_type = next_type_variable_id(cache);
//                     let callsite = cache.push_trait_binding(self.location);
//                     let trait_impl =
//                         TraitConstraint::int_constraint(int_type.clone(), callsite, cache);
//                     self.kind = Integer(x, IntegerKind::Inferred(int_type));
//                     (Type::TypeVariable(int_type), vec![trait_impl])
//                 } else {
//                     (Type::Primitive(PrimitiveType::IntegerType(kind)), vec![])
//                 }
//             },
//             Float(_) => (Type::Primitive(PrimitiveType::FloatType), vec![]),
//             String(_) => (Type::UserDefinedType(STRING_TYPE), vec![]),
//             Char(_) => (Type::Primitive(PrimitiveType::CharType), vec![]),
//             Bool(_) => (Type::Primitive(PrimitiveType::BooleanType), vec![]),
//             Unit => (Type::Primitive(PrimitiveType::UnitType), vec![]),
//         }
//     }
// }
