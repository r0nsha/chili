use super::{
    env::{Env, Scope, ScopeKind},
    top_level::CallerInfo,
    CheckSess,
};
use crate::{
    ast::{
        self,
        pattern::{NamePattern, Pattern, UnpackPattern, UnpackPatternKind},
    },
    error::{
        diagnostic::{Diagnostic, Label},
        DiagnosticResult, SyntaxError,
    },
    hir,
    infer::{display::OrReportErr, normalize::Normalize, unify::UnifyType},
    span::Span,
    types::{InferType, PartialStructType, Type, TypeId},
    workspace::{
        BindingId, BindingInfoFlags, BindingInfoKind, ModuleId, PartialBindingInfo, ScopeLevel,
    },
};
use indexmap::IndexMap;
use ustr::{ustr, Ustr};

impl<'s> CheckSess<'s> {
    pub fn get_global_binding_id(&self, module_id: ModuleId, name: Ustr) -> Option<BindingId> {
        self.global_scopes
            .get(&module_id)
            .and_then(|global_scope| global_scope.bindings.get(&name).cloned())
    }

    pub fn insert_global_binding_id(&mut self, module_id: ModuleId, name: Ustr, id: BindingId) {
        self.global_scopes
            .entry(module_id)
            .or_insert({
                let module_name = self.workspace.module_infos.get(module_id).unwrap().name;
                Scope::new(module_name, ScopeKind::Global)
            })
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
        value: Option<hir::Node>,
        is_mutable: bool,
        kind: BindingInfoKind,
        span: Span,
        flags: BindingInfoFlags,
    ) -> DiagnosticResult<(BindingId, hir::Node)> {
        let module_id = env.module_id();
        let scope_level = env.scope_level();

        let partial_binding_info = PartialBindingInfo {
            module_id,
            name,
            visibility,
            ty,
            const_value: if is_mutable || flags.contains(BindingInfoFlags::NO_CONST_FOLD) {
                None
            } else {
                value
                    .as_ref()
                    .map(|v| v.as_const_value().cloned())
                    .flatten()
            },
            is_mutable,
            kind,
            scope_level,
            qualified_name: get_qualified_name(env.scope_name(), name),
            span,
            flags,
        };

        let id = self
            .workspace
            .binding_infos
            .insert_with_id(partial_binding_info.clone().into_binding_info());

        match scope_level {
            // check if there's already a binding with this symbol
            ScopeLevel::Global => {
                if let Some(defined_id) = self.get_global_binding_id(module_id, name) {
                    let defined_binding_info =
                        self.workspace.binding_infos.get(defined_id).unwrap();

                    if flags.contains(BindingInfoFlags::SHADOWABLE) {
                        // Do nothing, this is shadowed by the already defined binding
                    } else if defined_binding_info.is_shadowable() {
                        // The defined binding can be shadowed, so this is Ok
                        self.insert_global_binding_id(module_id, name, id);
                    } else if defined_binding_info.span != span {
                        return Err(SyntaxError::duplicate_binding(
                            defined_binding_info.span,
                            span,
                            defined_binding_info.name,
                        ));
                    }
                } else {
                    // insert the symbol into its module's global scope
                    self.insert_global_binding_id(module_id, name, id);
                }
            }
            ScopeLevel::Scope(_) => {
                // insert the symbol into local scope
                env.insert_binding(name, id);
            }
        }

        let node = if let Some(value) = value {
            hir::Node::Binding(hir::Binding {
                module_id,
                id,
                name,
                value: Box::new(value),
                ty: self.tcx.common_types.unit,
                span,
            })
        } else {
            hir::Node::Id(hir::Id { id, ty, span })
        };

        Ok((id, node))
    }

    pub fn bind_name_pattern(
        &mut self,
        env: &mut Env,
        pattern: &NamePattern,
        visibility: ast::Visibility,
        ty: TypeId,
        value: Option<hir::Node>,
        kind: BindingInfoKind,
        flags: BindingInfoFlags,
    ) -> DiagnosticResult<(BindingId, hir::Node)> {
        self.bind_name(
            env,
            pattern.alias(),
            visibility,
            ty,
            value,
            pattern.is_mutable,
            kind,
            pattern.span,
            flags,
        )
    }

    pub fn bind_pattern(
        &mut self,
        env: &mut Env,
        pattern: &Pattern,
        visibility: ast::Visibility,
        ty: TypeId,
        value: Option<hir::Node>,
        kind: BindingInfoKind,
        ty_origin_span: Span,
        flags: BindingInfoFlags,
    ) -> DiagnosticResult<(BindingId, hir::Node)> {
        match pattern {
            Pattern::Name(pattern) => {
                self.bind_name_pattern(env, pattern, visibility, ty, value, kind, flags)
            }
            Pattern::StructUnpack(pattern) => {
                let mut statements = vec![];

                let name = self.generate_name("v");
                let (id, id_node) = self.bind_temp_name_for_unpack_pattern(
                    env,
                    name,
                    visibility,
                    ty,
                    value,
                    kind,
                    pattern,
                    &mut statements,
                    flags,
                )?;

                self.bind_struct_unpack_pattern(
                    &mut statements,
                    env,
                    pattern,
                    visibility,
                    ty,
                    id_node,
                    kind,
                    ty_origin_span,
                    flags,
                )?;

                Ok((
                    id,
                    hir::Node::Sequence(hir::Sequence {
                        statements,
                        ty: self.tcx.common_types.unit,
                        span: pattern.span,
                        is_block: false,
                    }),
                ))
            }
            Pattern::TupleUnpack(pattern) => {
                let mut statements = vec![];

                let name = self.generate_name("v");
                let (id, id_node) = self.bind_temp_name_for_unpack_pattern(
                    env,
                    name,
                    visibility,
                    ty,
                    value,
                    kind,
                    pattern,
                    &mut statements,
                    flags,
                )?;

                self.bind_tuple_unpack_pattern(
                    &mut statements,
                    env,
                    pattern,
                    visibility,
                    ty,
                    id_node,
                    kind,
                    ty_origin_span,
                    flags,
                )?;

                Ok((
                    id,
                    hir::Node::Sequence(hir::Sequence {
                        statements,
                        ty: self.tcx.common_types.unit,
                        span: pattern.span,
                        is_block: false,
                    }),
                ))
            }
            Pattern::Hybrid(pattern) => {
                let mut statements = vec![];

                let (id, bound_node) = self.bind_name_pattern(
                    env,
                    &pattern.name_pattern,
                    visibility,
                    ty,
                    value.clone(),
                    kind,
                    flags,
                )?;

                let id_node = self.get_id_node_for_unpack_pattern(bound_node, &mut statements);

                match &pattern.unpack_pattern {
                    UnpackPatternKind::Struct(pattern) => self.bind_struct_unpack_pattern(
                        &mut statements,
                        env,
                        pattern,
                        visibility,
                        ty,
                        id_node,
                        kind,
                        ty_origin_span,
                        flags,
                    )?,
                    UnpackPatternKind::Tuple(pattern) => self.bind_tuple_unpack_pattern(
                        &mut statements,
                        env,
                        pattern,
                        visibility,
                        ty,
                        id_node,
                        kind,
                        ty_origin_span,
                        flags,
                    )?,
                }

                Ok((
                    id,
                    hir::Node::Sequence(hir::Sequence {
                        statements,
                        ty: self.tcx.common_types.unit,
                        span: pattern.span,
                        is_block: false,
                    }),
                ))
            }
        }
    }

    fn bind_temp_name_for_unpack_pattern(
        &mut self,
        env: &mut Env,
        name: Ustr,
        visibility: ast::Visibility,
        ty: TypeId,
        value: Option<hir::Node>,
        kind: BindingInfoKind,
        pattern: &UnpackPattern,
        statements: &mut Vec<hir::Node>,
        flags: BindingInfoFlags,
    ) -> Result<(BindingId, hir::Node), Diagnostic> {
        let (id, bound_node) = self.bind_name(
            env,
            name,
            visibility,
            ty,
            value,
            false,
            kind,
            pattern.span,
            flags - BindingInfoFlags::IS_USER_DEFINED,
        )?;

        let id_node = self.get_id_node_for_unpack_pattern(bound_node, statements);

        Ok((id, id_node))
    }

    fn get_id_node_for_unpack_pattern(
        &mut self,
        bound_node: hir::Node,
        statements: &mut Vec<hir::Node>,
    ) -> hir::Node {
        match bound_node {
            hir::Node::Binding(ref binding) => {
                let id_node = self.id_or_const_by_id(binding.id, binding.span);
                statements.push(bound_node);
                id_node
            }
            hir::Node::Id(_) => bound_node,
            _ => panic!("got {:?}", bound_node),
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
        kind: BindingInfoKind,
        ty_origin_span: Span,
        flags: BindingInfoFlags,
    ) -> DiagnosticResult<()> {
        match ty.normalize(&self.tcx).maybe_deref_once() {
            Type::Module(module_id) => {
                // TODO: This could cause bugs, need to check
                //       that there is no way that the last statement isn't a binding
                statements.pop();

                self.check_module_by_id(module_id)?;

                let module_bindings = self.global_scopes.get(&module_id).unwrap().bindings.clone();

                for pattern in unpack_pattern.symbols.iter() {
                    let caller_info = CallerInfo {
                        module_id: env.module_id(),
                        span: pattern.span,
                    };

                    let id = match module_bindings.get(&pattern.name) {
                        Some(id) => *id,
                        None => {
                            return Err(self.name_not_found_error(
                                module_id,
                                pattern.name,
                                caller_info,
                            ))
                        }
                    };

                    self.validate_can_access_item(id, caller_info)?;

                    let binding_info = self.workspace.binding_infos.get(id).unwrap();

                    let (_, binding) = self.bind_name_pattern(
                        env,
                        pattern,
                        visibility,
                        binding_info.ty,
                        Some(self.id_or_const(binding_info, pattern.span)),
                        kind,
                        flags | BindingInfoFlags::TYPE_WAS_INFERRED,
                    )?;

                    statements.push(binding);
                }

                if let Some(_) = &unpack_pattern.wildcard {
                    for (_, &id) in module_bindings.iter() {
                        let binding_info = self.workspace.binding_infos.get(id).unwrap();

                        if binding_info.visibility.is_private() {
                            continue;
                        }

                        let (_, binding) = self.bind_name(
                            env,
                            binding_info.name,
                            visibility,
                            binding_info.ty,
                            Some(self.id_or_const(binding_info, binding_info.span)),
                            binding_info.is_mutable,
                            binding_info.kind,
                            binding_info.span,
                            flags - BindingInfoFlags::IS_USER_DEFINED,
                        )?;

                        statements.push(binding);
                    }
                }
            }
            type_kind => {
                let partial_struct = PartialStructType(IndexMap::from_iter(
                    unpack_pattern
                        .symbols
                        .iter()
                        .map(|pattern| (pattern.name, self.tcx.var(pattern.span).as_kind())),
                ));

                let partial_struct_ty = self
                    .tcx
                    .partial_struct(partial_struct.clone(), unpack_pattern.span);

                ty.unify(&partial_struct_ty, &mut self.tcx).or_report_err(
                    &self.tcx,
                    partial_struct_ty,
                    Some(unpack_pattern.span),
                    ty,
                    ty_origin_span,
                )?;

                for (index, pattern) in unpack_pattern.symbols.iter().enumerate() {
                    let ty = self
                        .tcx
                        .bound(partial_struct[&pattern.name].clone(), pattern.span);

                    let field_value = match value.as_const_value() {
                        Some(const_value) if !pattern.is_mutable => hir::Node::Const(hir::Const {
                            value: const_value
                                .as_struct()
                                .unwrap()
                                .get(&pattern.name)
                                .unwrap()
                                .clone()
                                .value,
                            ty,
                            span: pattern.span,
                        }),
                        _ => hir::Node::MemberAccess(hir::MemberAccess {
                            value: Box::new(value.clone()),
                            member_name: pattern.name,
                            member_index: index as _,
                            ty,
                            span: pattern.span,
                        }),
                    };

                    let (_, bound_node) = self.bind_name_pattern(
                        env,
                        pattern,
                        visibility,
                        ty,
                        Some(field_value),
                        kind,
                        flags | BindingInfoFlags::TYPE_WAS_INFERRED,
                    )?;

                    statements.push(bound_node);
                }

                if let Some(wildcard) = &unpack_pattern.wildcard {
                    match type_kind {
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

                                let ty = self.tcx.bound(field.ty.clone(), field.span);

                                let field_value = match value.as_const_value() {
                                    Some(const_value) => hir::Node::Const(hir::Const {
                                        value: const_value
                                            .as_struct()
                                            .unwrap()
                                            .get(&field.name)
                                            .unwrap()
                                            .value
                                            .clone(),
                                        ty,
                                        span: field.span,
                                    }),
                                    None => hir::Node::MemberAccess(hir::MemberAccess {
                                        value: Box::new(value.clone()),
                                        member_name: field.name,
                                        member_index: index as _,
                                        ty,
                                        span: field.span,
                                    }),
                                };

                                let (_, bound_node) = self.bind_name(
                                    env,
                                    field.name,
                                    visibility,
                                    ty,
                                    Some(field_value),
                                    false,
                                    kind,
                                    wildcard.span,
                                    flags - BindingInfoFlags::IS_USER_DEFINED,
                                )?;

                                statements.push(bound_node);
                            }
                        }
                        Type::Infer(_, InferType::PartialStruct(_)) => {
                            return Err(Diagnostic::error()
                                .with_message(format!(
                                    "cannot use wildcard unpack on partial struct type - {}",
                                    type_kind
                                ))
                                .with_label(Label::primary(
                                    wildcard.span,
                                    "illegal wildcard unpack",
                                )))
                        }
                        _ => {
                            return Err(Diagnostic::error()
                                .with_message(format!(
                                    "cannot use wildcard unpack on partial tuple type - {}",
                                    type_kind
                                ))
                                .with_label(Label::primary(
                                    wildcard.span,
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
        kind: BindingInfoKind,
        ty_origin_span: Span,
        flags: BindingInfoFlags,
    ) -> DiagnosticResult<()> {
        let elements = pattern
            .symbols
            .iter()
            .map(|symbol| self.tcx.var(symbol.span).as_kind())
            .collect::<Vec<Type>>();

        let partial_tuple = self.tcx.partial_tuple(elements.clone(), pattern.span);

        ty.unify(&partial_tuple, &mut self.tcx).or_report_err(
            &self.tcx,
            partial_tuple,
            Some(pattern.span),
            ty,
            ty_origin_span,
        )?;

        for (index, pattern) in pattern.symbols.iter().enumerate() {
            let ty = self.tcx.bound(elements[index].clone(), pattern.span);

            let element_value = match value.as_const_value() {
                Some(const_value) if !pattern.is_mutable => hir::Node::Const(hir::Const {
                    value: const_value.as_tuple().unwrap()[index].value.clone(),
                    ty,
                    span: value.span(),
                }),
                _ => hir::Node::MemberAccess(hir::MemberAccess {
                    value: Box::new(value.clone()),
                    member_name: ustr(&index.to_string()),
                    member_index: index as _,
                    ty,
                    span: value.span(),
                }),
            };

            let (_, bound_node) = self.bind_name_pattern(
                env,
                pattern,
                visibility,
                ty,
                Some(element_value),
                kind,
                flags | BindingInfoFlags::TYPE_WAS_INFERRED,
            )?;

            statements.push(bound_node);
        }

        Ok(())
    }
}

pub(super) fn get_qualified_name(scope_name: Ustr, name: Ustr) -> Ustr {
    if scope_name.is_empty() {
        name
    } else {
        ustr(&format!("{}.{}", scope_name, name))
    }
}
