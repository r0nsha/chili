use chili_ast::{ast, compiler_info, workspace::ModuleInfo};
use chili_span::Span;
use std::collections::HashSet;

pub(crate) fn add_std_import(ast: &mut ast::Ast, imports: &mut HashSet<ModuleInfo>) {
    let module_info = compiler_info::std_module_info();
    add_intrinsic_module(ast, imports, module_info)
}

pub(crate) fn add_intrinsic_module(
    ast: &mut ast::Ast,
    imports: &mut HashSet<ModuleInfo>,
    module_info: ModuleInfo,
) {
    ast.imports.push(ast::Import {
        module_id: Default::default(),
        binding_info_id: Default::default(),
        target_module_id: Default::default(),
        target_module_info: module_info,
        target_binding_info_id: None,
        alias: module_info.name,
        import_path: vec![],
        visibility: ast::Visibility::Private,
        span: Span::unknown(),
    });

    imports.insert(module_info);
}
