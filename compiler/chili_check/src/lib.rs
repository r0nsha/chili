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
    ast::{Ast, Binding, Expr, ExprKind, ModuleInfo},
    pattern::SymbolPattern,
    value::Value,
    workspace::Workspace,
};
use chili_error::{DiagnosticResult, TypeError};
use chili_infer::infer::InferenceContext;
use chili_span::Span;
use chili_ty::Ty;
use codespan_reporting::diagnostic::{Diagnostic, Label};
use common::env::{Env, Scope};
use ustr::{Ustr, UstrMap};

pub fn check<'w>(
    workspace: &mut Workspace<'w>,
    asts: &mut Vec<Ast>,
) -> DiagnosticResult<()> {
    let target_metrics = workspace.build_options.target_platform.metrics();
    let mut infcx = InferenceContext::new(target_metrics.word_size);
    let mut ancx = CheckContext::new(&mut infcx);

    for ast in asts {
        for import in ast.imports.iter() {
            ancx.check_import(ast.module_info, import)?;
        }
        for binding in ast.bindings.iter() {
            ancx.check_top_level_binding_internal(
                ast.module_info,
                binding,
                ast.module_info,
                Span::unknown(),
            )?;
        }
    }

    Ok(())
}

pub(crate) struct CheckData {
    ty: Ty,
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
    pub(crate) builtin_types: UstrMap<Ty>,
    pub(crate) processed_items_stack: Vec<ProcessedItem>,
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
    pub(crate) fn new(infcx: &'a mut InferenceContext) -> Self {
        Self {
            infcx,
            builtin_types: get_builtin_types(),
            processed_items_stack: vec![],
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
                frame.module_info,
                symbol,
                span,
                TopLevelLookupKind::CurrentModule,
            ),
        }
    }
}
