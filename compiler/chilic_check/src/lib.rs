mod builtin;
mod check_assign;
mod check_binary;
mod check_call;
mod check_entity;
mod check_expr;
mod check_fn;
mod check_pattern;
mod check_unary;
mod lints;

use builtin::get_builtin_types;
use chilic_ast::{
    ast::{Entity, Expr, ExprKind, Ir, ModuleInfo},
    pattern::SymbolPattern,
    value::Value,
};
use chilic_error::{DiagnosticResult, SyntaxError, TypeError};
use chilic_span::Span;
use chilic_ty::Ty;
use codespan_reporting::{
    diagnostic::{Diagnostic, Label},
    files::SimpleFiles,
};
use common::{
    build_options::BuildOptions,
    env::{Env, Scope},
};
use lints::Lint;
use ustr::{Ustr, UstrMap};

use chilic_infer::infer::InferenceContext;

pub fn check_ir(
    files: &SimpleFiles<String, String>,
    build_options: &BuildOptions,
    ir: Ir,
) -> DiagnosticResult<Ir> {
    // * check for duplicate symbols in the same module
    for (_, module) in &ir.modules {
        let mut module_decls = UstrMap::default();

        for use_ in &module.uses {
            let span = use_.span();

            if let Some(defined_span) = module_decls.insert(use_.alias, span) {
                return Err(SyntaxError::duplicate_symbol(
                    defined_span,
                    span,
                    use_.alias,
                ));
            }
        }

        for entity in &module.entities {
            let SymbolPattern {
                symbol,
                span,
                ignore,
                ..
            } = entity.pattern.into_single();

            if ignore {
                continue;
            }

            if let Some(defined_span) = module_decls.insert(symbol, span) {
                return Err(SyntaxError::duplicate_symbol(
                    defined_span,
                    span,
                    symbol,
                ));
            }
        }
    }

    let mut new_ir = Ir::new(ir.files.clone());

    let target_metrics = build_options.target_platform.metrics();
    let mut infcx = InferenceContext::new(target_metrics.word_size);
    let mut ancx = AnalysisContext::new(&mut infcx, &ir, &mut new_ir, files);

    let root_module = ir.root_module();

    let mut startup_entity = None;

    for entity in &root_module.entities {
        match &entity.value {
            Some(expr) => match &expr.kind {
                ExprKind::Fn(func) => {
                    // TODO: check the function's @(entry) attribute, instead of
                    // its name
                    if func.proto.name == "main"
                        && func.proto.lib_name.is_none()
                    {
                        let mut entity = entity.clone();
                        let mut func = func.clone();

                        func.is_startup = true;

                        entity.value = Some(Expr::typed(
                            ExprKind::Fn(func),
                            expr.ty.clone(),
                            expr.span,
                        ));

                        startup_entity = Some(entity);
                    }
                }
                _ => (),
            },
            None => (),
        }
    }

    match startup_entity {
        Some(startup_entity) => {
            ancx.in_main_path = true;

            ancx.check_top_level_entity_internal(
                root_module.info,
                &startup_entity,
                root_module.info.name,
                Span::unknown(),
            )?;

            ancx.in_main_path = false;

            for (module_name, module) in ir.modules.iter() {
                for use_ in module.uses.iter() {
                    ancx.check_use(*module_name, use_)?;
                }

                for entity in module.entities.iter() {
                    ancx.check_top_level_entity_internal(
                        module.info,
                        entity,
                        *module_name,
                        Span::unknown(),
                    )?;
                }
            }

            new_ir.foreign_libraries = ir.foreign_libraries;

            new_ir.lint()?;

            Ok(new_ir)
        }
        None => Err(Diagnostic::error()
            .with_message("entry point function `main` is not defined")
            .with_notes(vec![
                "define function `let main = fn() {}` in your entry file"
                    .to_string(),
            ])),
    }
}

pub(crate) struct CheckedExpr {
    expr: Expr,
    ty: Ty,
    value: Option<Value>,
}

impl CheckedExpr {
    pub(crate) fn new(
        expr: ExprKind,
        ty: Ty,
        value: Option<Value>,
        span: Span,
    ) -> Self {
        Self {
            expr: Expr::typed(expr, ty.clone(), span),
            ty,
            value,
        }
    }
}

pub(crate) struct ProcessedItem {
    pub(crate) module_name: Ustr,
    pub(crate) symbol: Ustr,
}

pub(crate) struct AnalysisContext<'a> {
    pub(crate) infcx: &'a mut InferenceContext,
    pub(crate) old_ir: &'a Ir,
    pub(crate) new_ir: &'a mut Ir,
    pub(crate) builtin_types: UstrMap<Ty>,
    pub(crate) files: &'a SimpleFiles<String, String>,
    pub(crate) processed_items_stack: Vec<ProcessedItem>,
    pub(crate) in_main_path: bool,
}

pub(crate) struct AnalysisFrame {
    pub(crate) module_info: ModuleInfo,
    pub(crate) env: AnalysisEnv,
    pub(crate) expected_return_ty: Option<Ty>,
    pub(crate) loop_depth: usize,
    pub(crate) self_types: Vec<Ty>,
    pub(crate) min_env_depth: usize,
}

pub(crate) type AnalysisEnv = Env<EntityInfo>;

impl AnalysisFrame {
    pub(crate) fn insert_entity(
        &mut self,
        name: Ustr,
        ty: Ty,
        span: Span,
        is_init: bool,
    ) {
        self.env.insert(
            name,
            EntityInfo {
                ty,
                const_value: None,
                is_mutable: false,
                is_init,
                span,
            },
        );
    }

    pub(crate) fn insert_const_entity(
        &mut self,
        name: Ustr,
        ty: Ty,
        const_value: Value,
        span: Span,
    ) {
        self.env.insert(
            name,
            EntityInfo {
                ty,
                const_value: Some(const_value),
                is_mutable: false,
                is_init: true,
                span,
            },
        );
    }

    pub(crate) fn insert_entity_info(
        &mut self,
        name: Ustr,
        entity_info: EntityInfo,
    ) {
        self.env.insert(name, entity_info);
    }

    pub(crate) fn push_scope(&mut self) {
        self.env.push_scope();
    }

    #[allow(unused)]
    pub(crate) fn push_named_scope(&mut self, name: Ustr) {
        self.env.push_named_scope(name);
    }

    pub(crate) fn pop_scope(&mut self) -> Scope<EntityInfo> {
        self.env.pop_scope().unwrap()
    }
}

impl AnalysisFrame {
    pub(crate) fn new(
        module_info: ModuleInfo,
        expected_return_ty: Option<Ty>,
        mut env: AnalysisEnv,
    ) -> Self {
        env.push_named_scope(module_info.name);
        let min_env_depth = env.depth();

        Self {
            module_info,
            env,
            expected_return_ty,
            loop_depth: 0,
            self_types: vec![],
            min_env_depth,
        }
    }
}

impl Drop for AnalysisFrame {
    fn drop(&mut self) {
        self.env.clear();
    }
}

#[derive(Debug, Clone)]
pub(crate) struct EntityInfo {
    pub(crate) ty: Ty,
    pub(crate) const_value: Option<Value>,
    pub(crate) is_mutable: bool,
    pub(crate) is_init: bool,
    pub(crate) span: Span,
}

impl EntityInfo {
    pub(crate) fn from_entity(entity: &Entity) -> Self {
        let SymbolPattern {
            span, is_mutable, ..
        } = entity.pattern.into_single();

        Self {
            ty: entity.ty.clone(),
            const_value: entity.const_value.clone(),
            is_mutable,
            is_init: entity.value.is_some() || entity.const_value.is_some(),
            span: span,
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub(crate) enum TopLevelLookupKind {
    CurrentModule,
    OtherModule,
}

impl<'a> AnalysisContext<'a> {
    pub(crate) fn new(
        infcx: &'a mut InferenceContext,
        old_ir: &'a Ir,
        new_ir: &'a mut Ir,
        files: &'a SimpleFiles<String, String>,
    ) -> Self {
        Self {
            infcx,
            old_ir,
            new_ir,
            builtin_types: get_builtin_types(),
            files,
            processed_items_stack: vec![],
            in_main_path: false,
        }
    }

    pub(crate) fn expect_value_is_int(
        &mut self,
        value: Option<Value>,
        ty: &Ty,
        span: Span,
    ) -> DiagnosticResult<i64> {
        match &value {
            Some(value) => {
                if !value.is_int() {
                    Err(TypeError::expected(
                        span,
                        &self.infcx.normalize_ty_and_untyped(ty),
                        "compile-time known integer",
                    ))
                } else {
                    Ok(value.clone().into_int())
                }
            }
            None => Err(TypeError::expected(
                span,
                &self.infcx.normalize_ty_and_untyped(ty),
                "compile-time known integer",
            )),
        }
    }

    pub(crate) fn find_symbol(
        &mut self,
        frame: &mut AnalysisFrame,
        symbol: Ustr,
        span: Span,
    ) -> DiagnosticResult<EntityInfo> {
        match frame.env.get_with_depth(&symbol) {
            Some((entity_info, depth)) => {
                if depth < frame.min_env_depth {
                    return Err(Diagnostic::error()
                        .with_message(
                            "can't capture dynamic environment in a fn",
                        )
                        .with_labels(vec![Label::primary(
                            span.file_id,
                            span.range().clone(),
                        )]));
                }

                Ok(entity_info.clone())
            }
            None => self.check_top_level_entity(
                frame.module_info,
                frame.module_info.name,
                symbol,
                span,
                TopLevelLookupKind::CurrentModule,
            ),
        }
    }
}
