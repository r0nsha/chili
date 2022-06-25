use super::{
    env::{Env, Scope, ScopeKind},
    top_level::{CallerInfo, CheckTopLevel},
    CheckSess,
};
use crate::{
    ast::{
        self,
        pattern::{NamePattern, Pattern, UnpackPattern, UnpackPatternKind},
        ty::{InferTy, PartialStructType, Type, TypeId},
        workspace::{BindingId, ModuleId, PartialBindingInfo},
    },
    error::{
        diagnostic::{Diagnostic, Label},
        DiagnosticResult, SyntaxError,
    },
    hir,
    infer::{display::OrReportErr, normalize::Normalize, unify::UnifyTy},
    span::Span,
};
use indexmap::IndexMap;
use ustr::{ustr, Ustr};

impl<'s> CheckSess<'s> {
    pub fn get_global_binding_id(&self, module_id: ModuleId, name: Ustr) -> Option<BindingId> {
        self.global_scopes
            .get(&module_id)
            .and_then(|module| module.bindings.get(&name).cloned())
    }

    pub fn insert_global_binding_id(&mut self, module_id: ModuleId, name: Ustr, id: BindingId) {
        self.global_scopes
            .entry(module_id)
            .or_insert(Scope::new(
                self.workspace.module_infos.get(module_id).unwrap().name,
                ScopeKind::Global,
            ))
            .bindings
            .insert(name, id);
    }

    pub fn get_binding_id(&self, env: &Env, name: Ustr) -> Option<BindingId> {
        env.find_binding(name)
            .or_else(|| self.get_global_binding_id(env.module_id(), name))
    }

    pub fn bind_name(
        &mut self,
        env: &mut Env,
        name: Ustr,
        visibility: ast::Visibility,
        ty: TypeId,
        value: hir::Node,
        is_mutable: bool,
        kind: ast::BindingKind,
        span: Span,
    ) -> DiagnosticResult<hir::Binding> {
        let module_id = env.module_id();
        let scope_level = env.scope_level();
        let is_global = scope_level.is_global();

        if is_global {
            // check if there's already a binding with this symbol
            if let Some(id) = self.get_global_binding_id(module_id, name) {
                let already_defined = self.workspace.binding_infos.get(id).unwrap();
                return Err(SyntaxError::duplicate_binding(
                    already_defined.span,
                    span,
                    already_defined.name,
                ));
            }
        }

        let partial_binding_info = PartialBindingInfo {
            module_id,
            name,
            visibility,
            ty,
            const_value: if is_mutable {
                None
            } else {
                value.as_const_value().cloned()
            },
            is_mutable,
            kind,
            scope_level,
            scope_name: env.scope_name(),
            span,
        };

        let id = self
            .workspace
            .binding_infos
            .insert_with_id(partial_binding_info.into_binding_info());

        if is_global {
            // insert the symbol into its module's global scope
            self.insert_global_binding_id(module_id, name, id);
        } else {
            // insert the symbol into local scope
            env.insert_binding(name, id);
        }

        Ok(hir::Binding {
            module_id,
            id,
            name,
            value: Box::new(value),
            ty: self.tycx.common_types.unit,
            span,
        })
    }

    pub fn bind_name_pattern(
        &mut self,
        env: &mut Env,
        pattern: &NamePattern,
        visibility: ast::Visibility,
        ty: TypeId,
        value: hir::Node,
        kind: &ast::BindingKind,
    ) -> DiagnosticResult<hir::Node> {
        let binding = self.bind_name(
            env,
            pattern.alias.unwrap_or(pattern.name),
            visibility,
            ty,
            value,
            pattern.is_mutable,
            kind.clone(),
            pattern.span,
        )?;

        Ok(hir::Node::Binding(binding))
    }

    pub fn bind_pattern(
        &mut self,
        env: &mut Env,
        pattern: &Pattern,
        visibility: ast::Visibility,
        ty: TypeId,
        value: hir::Node,
        kind: &ast::BindingKind,
        ty_origin_span: Span,
    ) -> DiagnosticResult<hir::Node> {
        // Note (Ron 25/06/2022): There is a lot of duplicate code here. This should be organized better...
        match pattern {
            Pattern::Name(pattern) => {
                self.bind_name_pattern(env, pattern, visibility, ty, value, kind)
            }
            Pattern::StructUnpack(pattern) => {
                let mut statements = vec![];

                let value_span = value.span();

                let name = self.generate_name("v");
                let name_node = self.bind_name(
                    env,
                    name,
                    visibility,
                    ty,
                    value,
                    false,
                    kind.clone(),
                    pattern.span,
                )?;

                let id = name_node.id;
                let ty = self.workspace.binding_infos.get(id).unwrap().ty;

                let name_id_node = hir::Node::Id(hir::Id {
                    id,
                    ty,
                    span: value_span,
                });

                self.bind_struct_unpack_pattern(
                    &mut statements,
                    env,
                    pattern,
                    visibility,
                    ty,
                    name_id_node,
                    kind,
                    ty_origin_span,
                )?;

                Ok(hir::Node::Sequence(hir::Sequence {
                    statements,
                    ty: self.tycx.common_types.unit,
                    span: pattern.span,
                }))
            }
            Pattern::TupleUnpack(pattern) => {
                let mut statements = vec![];

                let value_span = value.span();

                let name = self.generate_name("v");
                let name_node = self.bind_name(
                    env,
                    name,
                    visibility,
                    ty,
                    value,
                    false,
                    kind.clone(),
                    pattern.span,
                )?;

                let id = name_node.id;
                let ty = self.workspace.binding_infos.get(id).unwrap().ty;

                let name_id_node = hir::Node::Id(hir::Id {
                    id,
                    ty,
                    span: value_span,
                });

                self.bind_tuple_unpack_pattern(
                    &mut statements,
                    env,
                    pattern,
                    visibility,
                    ty,
                    name_id_node,
                    kind,
                    ty_origin_span,
                )?;

                Ok(hir::Node::Sequence(hir::Sequence {
                    statements,
                    ty: self.tycx.common_types.unit,
                    span: pattern.span,
                }))
            }
            Pattern::Hybrid(pattern) => {
                let mut statements = vec![];

                let name_node = self.bind_name_pattern(
                    env,
                    &pattern.name_pattern,
                    visibility,
                    ty,
                    value.clone(),
                    kind,
                )?;

                let id = name_node.as_binding().unwrap().id;
                let ty = self.workspace.binding_infos.get(id).unwrap().ty;

                let name_id_node = hir::Node::Id(hir::Id {
                    id,
                    ty,
                    span: pattern.name_pattern.span,
                });

                match &pattern.unpack_pattern {
                    UnpackPatternKind::Struct(pattern) => self.bind_struct_unpack_pattern(
                        &mut statements,
                        env,
                        pattern,
                        visibility,
                        ty,
                        name_id_node,
                        kind,
                        ty_origin_span,
                    )?,
                    UnpackPatternKind::Tuple(pattern) => self.bind_tuple_unpack_pattern(
                        &mut statements,
                        env,
                        pattern,
                        visibility,
                        ty,
                        name_id_node,
                        kind,
                        ty_origin_span,
                    )?,
                }

                Ok(hir::Node::Sequence(hir::Sequence {
                    statements,
                    ty: self.tycx.common_types.unit,
                    span: pattern.span,
                }))
            }
        }
    }

    fn bind_struct_unpack_pattern(
        &mut self,
        statements: &mut Vec<hir::Node>,
        env: &mut Env,
        unpack_pattern: &UnpackPattern,
        visibility: ast::Visibility,
        ty: TypeId,
        value: hir::Node,
        kind: &ast::BindingKind,
        ty_origin_span: Span,
    ) -> DiagnosticResult<()> {
        let mut statements = Vec::<hir::Node>::new();

        match ty.normalize(&self.tycx).maybe_deref_once() {
            Type::Module(module_id) => {
                for pattern in unpack_pattern.symbols.iter() {
                    let top_level_binding_id = self.check_top_level_binding(
                        CallerInfo {
                            module_id: env.module_id(),
                            span: pattern.span,
                        },
                        module_id,
                        pattern.name,
                    )?;

                    // Note (Ron 19/06/2022):
                    // This is a HACK, to avoid binding the same symbol twice.
                    // When two mutually recursive modules are checking each other,
                    // the original symbol that started the check cycle is bound twice - causing a false `duplicate symbol error`
                    if env.scope_level().is_global()
                        && self
                            .get_global_binding_id(env.module_id(), pattern.name)
                            .is_some()
                    {
                        continue;
                    }

                    let top_level_binding_info = self
                        .workspace
                        .binding_infos
                        .get(top_level_binding_id)
                        .unwrap();

                    let binding = self.bind_name_pattern(
                        env,
                        pattern,
                        visibility,
                        top_level_binding_info.ty,
                        hir::Node::Id(hir::Id {
                            id: top_level_binding_info.id,
                            ty: top_level_binding_info.ty,
                            span: pattern.span,
                        }),
                        kind,
                    )?;

                    statements.push(binding);
                }

                if let Some(wildcard_symbol_span) = unpack_pattern.wildcard_symbol {
                    let module = self
                        .modules
                        .iter()
                        .find(|m| m.module_id == module_id)
                        .unwrap_or_else(|| panic!("couldn't find {:?}", module_id));

                    for binding in &module.bindings {
                        if binding.visibility.is_private() {
                            continue;
                        }

                        let bound_ids = match binding.pattern.iter().next() {
                            Some(pattern) => {
                                // check if the binding has already been checked
                                match self.get_global_binding_id(module_id, pattern.name) {
                                    Some(id) => binding.pattern.ids(),
                                    None => {
                                        let bound_names = binding.check_top_level(self)?;
                                        bound_names.values().cloned().collect::<Vec<BindingId>>()
                                    }
                                }
                            }
                            None => {
                                let bound_names = binding.check_top_level(self)?;
                                bound_names.values().cloned().collect::<Vec<BindingId>>()
                            }
                        };

                        for id in bound_ids.into_iter() {
                            let binding_info = self.workspace.binding_infos.get(id).unwrap();

                            let binding = self.bind_name(
                                env,
                                binding_info.name,
                                visibility,
                                binding_info.ty,
                                hir::Node::Id(hir::Id {
                                    id: binding_info.id,
                                    ty: binding_info.ty,
                                    span: wildcard_symbol_span,
                                }),
                                binding_info.is_mutable,
                                binding_info.kind,
                                binding_info.span,
                            )?;

                            statements.push(hir::Node::Binding(binding));
                        }
                    }
                }
            }
            ty_kind => {
                let partial_struct = PartialStructType(IndexMap::from_iter(
                    unpack_pattern
                        .symbols
                        .iter()
                        .map(|symbol| (symbol.name, self.tycx.var(symbol.span).as_kind())),
                ));

                let partial_struct_ty = self
                    .tycx
                    .partial_struct(partial_struct.clone(), unpack_pattern.span);

                ty.unify(&partial_struct_ty, &mut self.tycx).or_report_err(
                    &self.tycx,
                    partial_struct_ty,
                    Some(unpack_pattern.span),
                    ty,
                    ty_origin_span,
                )?;

                for (index, pattern) in unpack_pattern.symbols.iter().enumerate() {
                    let ty = self
                        .tycx
                        .bound(partial_struct[&pattern.name].clone(), pattern.span);

                    let field_value = match value.as_const_value() {
                        Some(const_value) if pattern.is_mutable => hir::Node::Const(hir::Const {
                            value: const_value
                                .as_struct()
                                .get(&pattern.name)
                                .unwrap()
                                .clone()
                                .value,
                            ty: value.ty(),
                            span: pattern.span,
                        }),
                        _ => hir::Node::MemberAccess(hir::MemberAccess {
                            value: Box::new(value),
                            member: pattern.name,
                            index: index as _,
                            ty,
                            span: pattern.span,
                        }),
                    };

                    let binding =
                        self.bind_name_pattern(env, pattern, visibility, ty, field_value, kind)?;

                    statements.push(binding);
                }

                if let Some(wildcard_symbol_span) = unpack_pattern.wildcard_symbol {
                    match ty_kind {
                        Type::Struct(struct_ty) => {
                            for (index, field) in struct_ty.fields.iter().enumerate() {
                                if unpack_pattern
                                    .symbols
                                    .iter()
                                    .find(|p| field.name == p.name)
                                    .is_some()
                                {
                                    // skip explicitly unpacked fields
                                    continue;
                                }

                                let field_value = match value.as_const_value() {
                                    Some(const_value) => hir::Node::Const(hir::Const {
                                        value: const_value
                                            .as_struct()
                                            .get(&field.name)
                                            .unwrap()
                                            .clone()
                                            .value,
                                        ty: value.ty(),
                                        span: field.span,
                                    }),
                                    None => hir::Node::MemberAccess(hir::MemberAccess {
                                        value: Box::new(value),
                                        member: field.name,
                                        index: index as _,
                                        ty,
                                        span: field.span,
                                    }),
                                };

                                let ty = self.tycx.bound(field.ty.clone(), field.span);

                                let binding = self.bind_name(
                                    env,
                                    field.name,
                                    visibility,
                                    ty,
                                    field_value,
                                    false,
                                    kind.clone(),
                                    wildcard_symbol_span,
                                )?;

                                statements.push(hir::Node::Binding(binding));
                            }
                        }
                        Type::Infer(_, InferTy::PartialStruct(_)) => {
                            return Err(Diagnostic::error()
                                .with_message(format!(
                                    "cannot use wildcard unpack on partial struct type - {}",
                                    ty_kind
                                ))
                                .with_label(Label::primary(
                                    wildcard_symbol_span,
                                    "illegal wildcard unpack",
                                )))
                        }
                        _ => {
                            return Err(Diagnostic::error()
                                .with_message(format!(
                                    "cannot use wildcard unpack on type {}",
                                    ty_kind
                                ))
                                .with_label(Label::primary(
                                    wildcard_symbol_span,
                                    "illegal wildcard unpack",
                                )))
                        }
                    }
                }
            }
        }

        Ok(())
    }

    fn bind_tuple_unpack_pattern(
        &mut self,
        statements: &mut Vec<hir::Node>,
        env: &mut Env,
        pattern: &UnpackPattern,
        visibility: ast::Visibility,
        ty: TypeId,
        value: hir::Node,
        kind: &ast::BindingKind,
        ty_origin_span: Span,
    ) -> DiagnosticResult<()> {
        let elements = pattern
            .symbols
            .iter()
            .map(|symbol| self.tycx.var(symbol.span).as_kind())
            .collect::<Vec<Type>>();

        let partial_tuple = self.tycx.partial_tuple(elements.clone(), pattern.span);

        ty.unify(&partial_tuple, &mut self.tycx).or_report_err(
            &self.tycx,
            partial_tuple,
            Some(pattern.span),
            ty,
            ty_origin_span,
        )?;

        for (index, pattern) in pattern.symbols.iter().enumerate() {
            let ty = self.tycx.bound(elements[index].clone(), pattern.span);

            let element_value = match value.as_const_value() {
                Some(const_value) => hir::Node::Const(hir::Const {
                    value: const_value.as_tuple()[index].clone().value,
                    ty: value.ty(),
                    span: value.span(),
                }),
                None => hir::Node::MemberAccess(hir::MemberAccess {
                    value: Box::new(value),
                    member: ustr(&index.to_string()),
                    index: index as _,
                    ty,
                    span: value.span(),
                }),
            };

            self.bind_name_pattern(env, pattern, visibility, ty, element_value, kind)?;
        }

        Ok(())
    }
}
