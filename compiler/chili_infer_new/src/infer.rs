use crate::sess::InferSess;
use chili_ast::{ast, ty::Ty, workspace::Workspace};
use chili_error::DiagnosticResult;

pub fn infer(workspace: &mut Workspace, asts: &mut Vec<ast::Ast>) -> DiagnosticResult<()> {
    let mut sess = InferSess::new();

    for ast in asts.iter_mut() {
        ast.infer(&mut sess, workspace)?;
    }

    sess.print_type_bindings();

    Ok(())
}

pub(crate) trait Infer {
    fn infer(&mut self, sess: &mut InferSess, workspace: &mut Workspace) -> DiagnosticResult<()>;
}

impl Infer for ast::Ast {
    fn infer(&mut self, sess: &mut InferSess, workspace: &mut Workspace) -> DiagnosticResult<()> {
        for import in self.imports.iter_mut() {
            import.infer(sess, workspace)?;
        }

        for binding in self.bindings.iter_mut() {
            binding.infer(sess, workspace)?;
        }

        Ok(())
    }
}

impl Infer for ast::Import {
    fn infer(&mut self, sess: &mut InferSess, workspace: &mut Workspace) -> DiagnosticResult<()> {
        // todo!("import")
        Ok(())
    }
}

impl Infer for ast::Binding {
    fn infer(&mut self, sess: &mut InferSess, workspace: &mut Workspace) -> DiagnosticResult<()> {
        let pat = self.pattern.as_single_ref();
        let binding_info = workspace
            .get_binding_info_mut(pat.binding_info_idx)
            .unwrap();

        // TODO: type annotation
        binding_info.ty = sess.new_variable().into();

        if let Some(value) = &self.expr {}

        // TODO: type annotation

        Ok(())
        // todo!("binding")
    }
}

impl Infer for ast::Expr {
    fn infer(&mut self, sess: &mut InferSess, workspace: &mut Workspace) -> DiagnosticResult<()> {
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
            ast::ExprKind::Fn(f) => {
                todo!("Hello Fn");
            }
            ast::ExprKind::While { cond, expr } => todo!(),
            ast::ExprKind::For(_) => todo!(),
            ast::ExprKind::Break { deferred } => todo!(),
            ast::ExprKind::Continue { deferred } => todo!(),
            ast::ExprKind::Return { expr, deferred } => {
                todo!("Hello Return");
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
            ast::ExprKind::Id {
                symbol,
                is_mutable,
                binding_span,
                binding_info_idx,
            } => {
                todo!("Hello Id");
            }
            ast::ExprKind::ArrayLiteral(_) => todo!(),
            ast::ExprKind::TupleLiteral(_) => todo!(),
            ast::ExprKind::StructLiteral { type_expr, fields } => todo!(),
            ast::ExprKind::Literal(_) => {
                todo!("Hello Literal");
            }
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
        Ok(())
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
