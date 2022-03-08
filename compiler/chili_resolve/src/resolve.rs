use crate::Resolver;
use chili_ast::{
    ast,
    workspace::{ModuleId, Workspace},
};
use chili_error::DiagnosticResult;

// Trait for resolving binding/import uses
pub(crate) trait Resolve<'w> {
    fn resolve(
        &mut self,
        resolver: &mut Resolver,
        workspace: &mut Workspace<'w>,
    ) -> DiagnosticResult<()>;
}

impl<'w, T: Resolve<'w>> Resolve<'w> for Vec<T> {
    fn resolve(
        &mut self,
        resolver: &mut Resolver,
        workspace: &mut Workspace<'w>,
    ) -> DiagnosticResult<()> {
        for element in self {
            element.resolve(resolver, workspace)?;
        }
        Ok(())
    }
}

impl<'w, T: Resolve<'w>> Resolve<'w> for Option<T> {
    fn resolve(
        &mut self,
        resolver: &mut Resolver,
        workspace: &mut Workspace<'w>,
    ) -> DiagnosticResult<()> {
        if let Some(e) = self {
            e.resolve(resolver, workspace)?;
        }
        Ok(())
    }
}

impl<'w, T: Resolve<'w>> Resolve<'w> for Box<T> {
    fn resolve(
        &mut self,
        resolver: &mut Resolver,
        workspace: &mut Workspace<'w>,
    ) -> DiagnosticResult<()> {
        self.as_mut().resolve(resolver, workspace)
    }
}

impl<'w> Resolve<'w> for ast::Ast {
    fn resolve(
        &mut self,
        resolver: &mut Resolver,
        workspace: &mut Workspace<'w>,
    ) -> DiagnosticResult<()> {
        self.bindings.resolve(resolver, workspace)?;
        Ok(())
    }
}

impl<'w> Resolve<'w> for ast::Import {
    fn resolve(
        &mut self,
        resolver: &mut Resolver,
        workspace: &mut Workspace<'w>,
    ) -> DiagnosticResult<()> {
        self.module_id = ModuleId(
            workspace
                .modules
                .iter()
                .position(|m| m.module_info == self.module_info)
                .unwrap(),
        );

        if !resolver.in_global_scope() {
            // TODO: add self to current scope
        }

        Ok(())
    }
}

impl<'w> Resolve<'w> for ast::Binding {
    fn resolve(
        &mut self,
        resolver: &mut Resolver,
        workspace: &mut Workspace<'w>,
    ) -> DiagnosticResult<()> {
        self.ty_expr.resolve(resolver, workspace)?;
        self.value.resolve(resolver, workspace)?;

        if !resolver.in_global_scope() {
            // TODO: add self to current scope
        }

        Ok(())
    }
}

impl<'w> Resolve<'w> for ast::Expr {
    fn resolve(
        &mut self,
        resolver: &mut Resolver,
        workspace: &mut Workspace<'w>,
    ) -> DiagnosticResult<()> {
        match &mut self.kind {
            ast::ExprKind::Import(imports) => {
                for import in imports {
                    import.resolve(resolver, workspace)?;
                }
            }
            ast::ExprKind::Foreign(bindings) => {
                for binding in bindings {
                    binding.resolve(resolver, workspace)?;
                }
            }
            ast::ExprKind::Binding(binding) => {
                binding.resolve(resolver, workspace)?;
            }
            ast::ExprKind::Defer(expr) => {
                expr.resolve(resolver, workspace)?;
            }
            ast::ExprKind::Assign { lvalue, rvalue } => {
                lvalue.resolve(resolver, workspace)?;
                rvalue.resolve(resolver, workspace)?;
            }
            ast::ExprKind::Cast(cast) => {
                cast.expr.resolve(resolver, workspace)?;
                cast.type_expr.resolve(resolver, workspace)?;
            }
            ast::ExprKind::Builtin(builtin) => match builtin {
                ast::Builtin::SizeOf(expr) | ast::Builtin::AlignOf(expr) => {
                    expr.resolve(resolver, workspace)?;
                }
                ast::Builtin::Panic(msg) => {
                    msg.resolve(resolver, workspace)?;
                }
            },
            ast::ExprKind::Fn(f) => {
                f.resolve(resolver, workspace)?;
            }
            ast::ExprKind::While { cond, expr } => {
                cond.resolve(resolver, workspace)?;
                expr.resolve(resolver, workspace)?;
            }
            ast::ExprKind::For {
                iter_name,
                iter_index_name,
                iterator,
                expr,
            } => {
                // TODO: add iter_name
                // TODO: add iter_index_name

                match iterator {
                    ast::ForIter::Range(start, end) => {
                        start.resolve(resolver, workspace)?;
                        end.resolve(resolver, workspace)?;
                    }
                    ast::ForIter::Value(value) => {
                        value.resolve(resolver, workspace)?;
                    }
                }

                expr.resolve(resolver, workspace)?;
            }
            ast::ExprKind::Break { deferred } => {
                deferred.resolve(resolver, workspace)?;
            }
            ast::ExprKind::Continue { deferred } => {
                deferred.resolve(resolver, workspace)?;
            }
            ast::ExprKind::Return { expr, deferred } => {
                expr.resolve(resolver, workspace)?;
                deferred.resolve(resolver, workspace)?;
            }
            ast::ExprKind::If {
                cond,
                then_expr,
                else_expr,
            } => {
                cond.resolve(resolver, workspace)?;
                then_expr.resolve(resolver, workspace)?;
                else_expr.resolve(resolver, workspace)?;
            }
            ast::ExprKind::Block(block) => {
                block.resolve(resolver, workspace)?;
            }
            ast::ExprKind::Binary { lhs, op: _, rhs } => {
                lhs.resolve(resolver, workspace)?;
                rhs.resolve(resolver, workspace)?;
            }
            ast::ExprKind::Unary { op: _, lhs } => {
                lhs.resolve(resolver, workspace)?;
            }
            ast::ExprKind::Subscript { expr, index } => {
                expr.resolve(resolver, workspace)?;
                index.resolve(resolver, workspace)?;
            }
            ast::ExprKind::Slice { expr, low, high } => {
                expr.resolve(resolver, workspace)?;
                low.resolve(resolver, workspace)?;
                high.resolve(resolver, workspace)?;
            }
            ast::ExprKind::Call(call) => {
                call.resolve(resolver, workspace)?;
            }
            ast::ExprKind::MemberAccess { expr, member: _ } => {
                expr.resolve(resolver, workspace)?;
            }
            ast::ExprKind::Id {
                symbol,
                is_mutable: _,
                binding_span: _,
                binding_info_id,
            } => {
                // TODO: resolve here!
            }
            ast::ExprKind::ArrayLiteral(kind) => match kind {
                ast::ArrayLiteralKind::List(list) => {
                    list.resolve(resolver, workspace)?;
                }
                ast::ArrayLiteralKind::Fill { len, expr } => {
                    len.resolve(resolver, workspace)?;
                    expr.resolve(resolver, workspace)?;
                }
            },
            ast::ExprKind::TupleLiteral(lit) => {
                lit.resolve(resolver, workspace)?;
            }
            ast::ExprKind::StructLiteral { type_expr, fields } => {
                type_expr.resolve(resolver, workspace)?;

                // TODO: check there are no duplicate fields
                for field in fields {
                    field.value.resolve(resolver, workspace)?;
                }
            }
            ast::ExprKind::PointerType(inner, _) => {
                inner.resolve(resolver, workspace)?;
            }
            ast::ExprKind::MultiPointerType(inner, _) => {
                inner.resolve(resolver, workspace)?;
            }
            ast::ExprKind::ArrayType(inner, _) => {
                inner.resolve(resolver, workspace)?;
            }
            ast::ExprKind::SliceType(inner, _) => {
                inner.resolve(resolver, workspace)?;
            }
            ast::ExprKind::StructType(typ) => {
                for field in typ.fields.iter_mut() {
                    field.ty.resolve(resolver, workspace)?;
                }
            }
            ast::ExprKind::FnType(proto) => {
                for param in proto.params.iter_mut() {
                    param.ty.resolve(resolver, workspace)?;
                }

                proto.ret.resolve(resolver, workspace)?;
            }
            ast::ExprKind::Literal(_)
            | ast::ExprKind::SelfType
            | ast::ExprKind::NeverType
            | ast::ExprKind::UnitType
            | ast::ExprKind::PlaceholderType
            | ast::ExprKind::Noop => (),
        }

        Ok(())
    }
}

impl<'w> Resolve<'w> for ast::Block {
    fn resolve(
        &mut self,
        resolver: &mut Resolver,
        workspace: &mut Workspace<'w>,
    ) -> DiagnosticResult<()> {
        resolver.push_scope();

        self.exprs.resolve(resolver, workspace)?;
        self.deferred.resolve(resolver, workspace)?;

        resolver.pop_scope();

        Ok(())
    }
}

impl<'w> Resolve<'w> for ast::Fn {
    fn resolve(
        &mut self,
        resolver: &mut Resolver,
        workspace: &mut Workspace<'w>,
    ) -> DiagnosticResult<()> {
        let old_scope = resolver.function_scope;

        // TODO: add parameters to current scope
        // TODO: check duplicate parameters
        // TODO: support destructor patterns
        for param in self.proto.params.iter_mut() {
            param.ty.resolve(resolver, workspace)?;
        }

        self.proto.ret.resolve(resolver, workspace)?;

        resolver.push_named_scope(self.proto.name);
        resolver.function_scope = resolver.current_binding_level().0;

        self.body.resolve(resolver, workspace)?;

        resolver.pop_scope();
        resolver.function_scope = old_scope;

        Ok(())
    }
}

impl<'w> Resolve<'w> for ast::Call {
    fn resolve(
        &mut self,
        resolver: &mut Resolver,
        workspace: &mut Workspace<'w>,
    ) -> DiagnosticResult<()> {
        self.callee.resolve(resolver, workspace)?;

        // TODO: check there are no duplicate arguments
        for arg in self.args.iter_mut() {
            arg.value.resolve(resolver, workspace)?;
        }

        Ok(())
    }
}
