use crate::Resolver;
use chili_ast::{
    ast::{self, BindingKind, ForeignLibrary, Visibility},
    workspace::Workspace,
};
use chili_error::{DiagnosticResult, SyntaxError};
use codespan_reporting::diagnostic::{Diagnostic, Label};
use ustr::UstrMap;

// Trait for resolving binding/import uses
pub(crate) trait Resolve {
    fn resolve(
        &mut self,
        resolver: &mut Resolver,
        workspace: &mut Workspace,
    ) -> DiagnosticResult<()>;
}

impl<T: Resolve> Resolve for Vec<T> {
    fn resolve(
        &mut self,
        resolver: &mut Resolver,
        workspace: &mut Workspace,
    ) -> DiagnosticResult<()> {
        for element in self {
            element.resolve(resolver, workspace)?;
        }
        Ok(())
    }
}

impl<T: Resolve> Resolve for Option<T> {
    fn resolve(
        &mut self,
        resolver: &mut Resolver,
        workspace: &mut Workspace,
    ) -> DiagnosticResult<()> {
        if let Some(e) = self {
            e.resolve(resolver, workspace)?;
        }
        Ok(())
    }
}

impl<T: Resolve> Resolve for Box<T> {
    fn resolve(
        &mut self,
        resolver: &mut Resolver,
        workspace: &mut Workspace,
    ) -> DiagnosticResult<()> {
        self.as_mut().resolve(resolver, workspace)
    }
}

impl Resolve for ast::Ast {
    fn resolve(
        &mut self,
        resolver: &mut Resolver,
        workspace: &mut Workspace,
    ) -> DiagnosticResult<()> {
        self.imports.resolve(resolver, workspace)?;
        self.bindings.resolve(resolver, workspace)?;
        Ok(())
    }
}

impl Resolve for ast::Import {
    fn resolve(
        &mut self,
        resolver: &mut Resolver,
        workspace: &mut Workspace,
    ) -> DiagnosticResult<()> {
        if !resolver.scope_level.is_global() {
            self.module_id = workspace.find_module_info(self.module_info).unwrap();

            self.binding_info_id = resolver.add_binding(
                workspace,
                self.alias,
                self.visibility,
                false,
                BindingKind::Import,
                self.span,
                true,
            );
        }

        Ok(())
    }
}

impl Resolve for ast::Binding {
    fn resolve(
        &mut self,
        resolver: &mut Resolver,
        workspace: &mut Workspace,
    ) -> DiagnosticResult<()> {
        self.module_id = resolver.module_id;

        self.ty_expr.resolve(resolver, workspace)?;
        self.expr.resolve(resolver, workspace)?;

        // Collect foreign libraries to be linked later
        if let Some(lib) = self.lib_name {
            workspace.foreign_libraries.insert(ForeignLibrary::from_str(
                &lib,
                resolver.module_info.file_path,
                self.pattern.span(),
            )?);
        }

        if !resolver.scope_level.is_global() {
            resolver.add_binding_with_pattern(
                workspace,
                &mut self.pattern,
                self.visibility,
                self.kind,
                false,
            );
        }

        Ok(())
    }
}

impl Resolve for ast::Expr {
    fn resolve(
        &mut self,
        resolver: &mut Resolver,
        workspace: &mut Workspace,
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
                cast.ty_expr.resolve(resolver, workspace)?;
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
            ast::ExprKind::While { cond, block } => {
                cond.resolve(resolver, workspace)?;
                block.resolve(resolver, workspace)?;
            }
            ast::ExprKind::For(for_) => {
                match &mut for_.iterator {
                    ast::ForIter::Range(start, end) => {
                        start.resolve(resolver, workspace)?;
                        end.resolve(resolver, workspace)?;
                    }
                    ast::ForIter::Value(value) => {
                        value.resolve(resolver, workspace)?;
                    }
                }

                for_.iter_id = resolver.add_binding(
                    workspace,
                    for_.iter_name,
                    Visibility::Private,
                    false,
                    BindingKind::Let,
                    self.span,
                    true,
                );

                for_.iter_index_id = resolver.add_binding(
                    workspace,
                    for_.iter_index_name,
                    Visibility::Private,
                    false,
                    BindingKind::Let,
                    self.span,
                    true,
                );

                for_.block.resolve(resolver, workspace)?;
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
            ast::ExprKind::FnCall(call) => {
                call.resolve(resolver, workspace)?;
            }
            ast::ExprKind::MemberAccess { expr, member: _ } => {
                expr.resolve(resolver, workspace)?;
            }
            ast::ExprKind::Ident {
                symbol,
                is_mutable: _,
                binding_span: _,
                binding_info_id,
            } => match resolver.lookup_binding(workspace, *symbol) {
                Some(id) => {
                    let binding_info = workspace.get_binding_info(id).unwrap();

                    if !binding_info.kind.is_type()
                        && !binding_info.scope_level.is_global()
                        && binding_info.scope_level < resolver.function_scope_level
                    {
                        return Err(Diagnostic::error()
                            .with_message("can't capture dynamic environment yet - not implemented")
                            .with_labels(vec![Label::primary(
                                self.span.file_id,
                                self.span.range(),
                            )]));
                    }

                    *binding_info_id = id
                }
                None => {
                    return Err(Diagnostic::error()
                        .with_message(format!("cannot find symbol `{}` in this scope", symbol))
                        .with_labels(vec![Label::primary(self.span.file_id, self.span.range())
                            .with_message("not found in this scope")]))
                }
            },
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

                let mut field_map = UstrMap::default();

                for field in fields {
                    field.value.resolve(resolver, workspace)?;

                    if let Some(already_defined_span) = field_map.insert(field.symbol, field.span) {
                        return Err(SyntaxError::duplicate_symbol(
                            already_defined_span,
                            field.span,
                            field.symbol,
                        ));
                    }
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
            ast::ExprKind::StructType(st) => {
                st.binding_info_id = resolver.add_binding(
                    workspace,
                    st.name,
                    Visibility::Private,
                    false,
                    BindingKind::Type,
                    self.span,
                    true,
                );

                let mut field_map = UstrMap::default();

                for field in st.fields.iter_mut() {
                    field.ty.resolve(resolver, workspace)?;

                    if let Some(already_defined_span) = field_map.insert(field.name, field.span) {
                        return Err(SyntaxError::duplicate_symbol(
                            already_defined_span,
                            field.span,
                            field.name,
                        ));
                    }
                }
            }
            ast::ExprKind::FnType(sig) => {
                for param in sig.params.iter_mut() {
                    param.ty.resolve(resolver, workspace)?;
                }

                sig.ret.resolve(resolver, workspace)?;
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

impl Resolve for ast::Block {
    fn resolve(
        &mut self,
        resolver: &mut Resolver,
        workspace: &mut Workspace,
    ) -> DiagnosticResult<()> {
        resolver.push_scope();

        self.exprs.resolve(resolver, workspace)?;
        self.deferred.resolve(resolver, workspace)?;

        resolver.pop_scope();

        Ok(())
    }
}

impl Resolve for ast::Fn {
    fn resolve(
        &mut self,
        resolver: &mut Resolver,
        workspace: &mut Workspace,
    ) -> DiagnosticResult<()> {
        let old_scope_level = resolver.function_scope_level;

        let mut param_map = UstrMap::default();

        for param in self.sig.params.iter_mut() {
            param.ty.resolve(resolver, workspace)?;

            resolver.add_binding_with_pattern(
                workspace,
                &mut param.pattern,
                Visibility::Private,
                BindingKind::Let,
                false,
            );

            for pat in param.pattern.symbols() {
                if let Some(already_defined_span) = param_map.insert(pat.symbol, pat.span) {
                    return Err(SyntaxError::duplicate_symbol(
                        already_defined_span,
                        pat.span,
                        pat.symbol,
                    ));
                }
            }
        }

        self.sig.ret.resolve(resolver, workspace)?;

        resolver.push_named_scope(self.sig.name);
        resolver.function_scope_level = resolver.scope_level;

        self.body.resolve(resolver, workspace)?;

        resolver.pop_scope();
        resolver.function_scope_level = old_scope_level;

        // TODO: need to somehow allow recursive functions (maybe keep a `binding_info_id` in resolver?)
        // if !self.sig.name.is_empty() {
        //     resolver.add_binding(
        //         workspace,
        //         self.sig.name,
        //         Visibility::Private,
        //         false,
        //         BindingKind::Let,
        //         Span::unknown(),
        //         true,
        //     );
        // }

        Ok(())
    }
}

impl Resolve for ast::FnCall {
    fn resolve(
        &mut self,
        resolver: &mut Resolver,
        workspace: &mut Workspace,
    ) -> DiagnosticResult<()> {
        self.callee.resolve(resolver, workspace)?;

        for arg in self.args.iter_mut() {
            arg.expr.resolve(resolver, workspace)?;
        }

        Ok(())
    }
}
