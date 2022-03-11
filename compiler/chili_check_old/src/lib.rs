mod builtin;
mod check_assign;
mod check_binary;
mod check_binding;
mod check_call;
mod check_expr;
mod check_fn;
mod check_pattern;
mod check_unary;
mod lints;

use builtin::get_builtin_types;
use chili_ast::{
    ast::{Binding, Expr, ExprKind, Ir, ModuleInfo},
    pattern::SymbolPattern,
    value::Value,
};
use chili_error::{DiagnosticResult, SyntaxError, TypeError};
use chili_infer::infer::InferenceContext;
use chili_span::Span;
use chili_ast::ty::Ty;
use codespan_reporting::diagnostic::{Diagnostic, Label};
use common::{
    build_options::BuildOptions,
    env::{Env, Scope},
};
use lints::Lint;
use ustr::{Ustr, UstrMap};

pub fn check_ir(build_options: &BuildOptions, ir: Ir) -> DiagnosticResult<Ir> {
    // * check for duplicate symbols in the same module
    for (_, module) in &ir.modules {
        let mut module_decls = UstrMap::default();

        for import in &module.imports {
            let span = import.span();

            if let Some(defined_span) = module_decls.insert(import.alias, span)
            {
                return Err(SyntaxError::duplicate_symbol(
                    defined_span,
                    span,
                    import.alias,
                ));
            }
        }

        for binding in &module.bindings {
            let SymbolPattern {
                symbol,
                span,
                ignore,
                ..
            } = binding.pattern.into_single();

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
    let mut ancx = CheckContext::new(&mut infcx, &ir, &mut new_ir);

    let root_module = ir.root_module();

    let mut startup_binding = None;

    for binding in &root_module.bindings {
        match &binding.value {
            Some(expr) => match &expr.kind {
                ExprKind::Fn(func) => {
                    // TODO: check the function's @(entry) attribute, instead of
                    // its name
                    if func.proto.name == "main"
                        && func.proto.lib_name.is_none()
                    {
                        let mut binding = binding.clone();
                        let mut func = func.clone();

                        func.is_startup = true;

                        binding.value = Some(Expr::typed(
                            ExprKind::Fn(func),
                            expr.ty.clone(),
                            expr.span,
                        ));

                        startup_binding = Some(binding);
                    }
                }
                _ => (),
            },
            None => (),
        }
    }

    match startup_binding {
        Some(startup_binding) => {
            ancx.in_main_path = true;

            ancx.check_top_level_binding_internal(
                root_module.info,
                &startup_binding,
                root_module.info.name,
                Span::unknown(),
            )?;

            ancx.in_main_path = false;

            for (module_name, module) in ir.modules.iter() {
                for import in module.imports.iter() {
                    ancx.check_import(*module_name, import)?;
                }

                for binding in module.bindings.iter() {
                    ancx.check_top_level_binding_internal(
                        module.info,
                        binding,
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

pub(crate) struct CheckContext<'a> {
    pub(crate) infcx: &'a mut InferenceContext,
    pub(crate) old_ir: &'a Ir,
    pub(crate) new_ir: &'a mut Ir,
    pub(crate) builtin_types: UstrMap<Ty>,
    pub(crate) processed_items_stack: Vec<ProcessedItem>,
    pub(crate) in_main_path: bool,
}

pub(crate) struct CheckFrame {
    pub(crate) module_info: ModuleInfo,
    pub(crate) env: CheckEnv,
    pub(crate) expected_return_ty: Option<Ty>,
    pub(crate) loop_depth: usize,
    pub(crate) self_types: Vec<Ty>,
    pub(crate) min_env_depth: usize,
}

pub(crate) type CheckEnv = Env<BindingInfo>;

impl CheckFrame {
    pub(crate) fn insert_binding(
        &mut self,
        name: Ustr,
        ty: Ty,
        span: Span,
        is_init: bool,
    ) {
        self.env.insert(
            name,
            BindingInfo {
                ty,
                const_value: None,
                is_mutable: false,
                is_init,
                span,
            },
        );
    }

    pub(crate) fn insert_const_binding(
        &mut self,
        name: Ustr,
        ty: Ty,
        const_value: Value,
        span: Span,
    ) {
        self.env.insert(
            name,
            BindingInfo {
                ty,
                const_value: Some(const_value),
                is_mutable: false,
                is_init: true,
                span,
            },
        );
    }

    pub(crate) fn insert_binding_info(
        &mut self,
        name: Ustr,
        binding_info: BindingInfo,
    ) {
        self.env.insert(name, binding_info);
    }

    pub(crate) fn push_scope(&mut self) {
        self.env.push_scope();
    }

    #[allow(unused)]
    pub(crate) fn push_named_scope(&mut self, name: Ustr) {
        self.env.push_named_scope(name);
    }

    pub(crate) fn pop_scope(&mut self) -> Scope<BindingInfo> {
        self.env.pop_scope().unwrap()
    }
}

impl CheckFrame {
    pub(crate) fn new(
        module_info: ModuleInfo,
        expected_return_ty: Option<Ty>,
        mut env: CheckEnv,
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

impl Drop for CheckFrame {
    fn drop(&mut self) {
        self.env.clear();
    }
}

#[derive(Debug, Clone)]
pub(crate) struct BindingInfo {
    pub(crate) ty: Ty,
    pub(crate) const_value: Option<Value>,
    pub(crate) is_mutable: bool,
    pub(crate) is_init: bool,
    pub(crate) span: Span,
}

impl BindingInfo {
    pub(crate) fn from_binding(binding: &Binding) -> Self {
        let SymbolPattern {
            span, is_mutable, ..
        } = binding.pattern.into_single();

        Self {
            ty: binding.ty.clone(),
            const_value: binding.const_value.clone(),
            is_mutable,
            is_init: binding.value.is_some() || binding.const_value.is_some(),
            span: span,
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub(crate) enum TopLevelLookupKind {
    CurrentModule,
    OtherModule,
}

impl<'a> CheckContext<'a> {
    pub(crate) fn new(
        infcx: &'a mut InferenceContext,
        old_ir: &'a Ir,
        new_ir: &'a mut Ir,
    ) -> Self {
        Self {
            infcx,
            old_ir,
            new_ir,
            builtin_types: get_builtin_types(),
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
        frame: &mut CheckFrame,
        symbol: Ustr,
        span: Span,
    ) -> DiagnosticResult<BindingInfo> {
        match frame.env.get_with_depth(&symbol) {
            Some((binding_info, depth)) => {
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

                Ok(binding_info.clone())
            }
            None => self.check_top_level_binding(
                frame.module_info,
                frame.module_info.name,
                symbol,
                span,
                TopLevelLookupKind::CurrentModule,
            ),
        }
    }
}
