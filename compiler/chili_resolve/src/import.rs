use chili_ast::{
    ast::{Ast, Import, ImportPathNode},
    workspace::ModuleExports,
};
use chili_span::Spanned;

pub(crate) fn collect_module_exports(asts: &Vec<Ast>, exports: &mut ModuleExports) {
    for ast in asts.iter() {
        let entry = exports.entry(ast.module_id).or_default();

        for import in ast.imports.iter() {
            if import.visibility.is_public() {
                entry.insert(import.alias, import.binding_info_id);
            }
        }

        for binding in ast.bindings.iter() {
            if binding.visibility.is_public() {
                let pat = binding.pattern.into_single();
                entry.insert(pat.symbol, pat.binding_info_id);
            }
        }
    }
}

pub(crate) fn resolve_imports(imports: &mut Vec<Import>, exports: &ModuleExports) {
    let mut to_remove: Vec<usize> = vec![];
    let mut to_add: Vec<Import> = vec![];

    for (index, import) in imports.iter().enumerate() {
        if import.is_glob() {
            let expanded_imports = expand_glob_import(import.clone(), exports);
            to_remove.push(index);
            to_add.extend(expanded_imports);
        }
    }

    let mut removed = 0;
    for index in to_remove {
        imports.remove(index - removed);
        removed += 1;
    }

    imports.extend(to_add);
}

fn expand_glob_import(import: Import, exports: &ModuleExports) -> Vec<Import> {
    // for a given module `foo` with symbols: A, B, C.
    // with a given glob import of: `use foo.*`.
    // this function will expand this use to:
    //      `use foo.A`
    //      `use foo.B`
    //      `use foo.C`
    //
    let exports = exports.get(&import.target_module_id).unwrap();
    exports
        .iter()
        .map(|(symbol, _)| {
            let mut import_path = import.import_path.clone();
            import_path.pop();
            import_path.push(Spanned::new(
                ImportPathNode::Symbol(*symbol),
                import.path_span().clone(),
            ));
            Import {
                module_id: import.module_id,
                target_binding_info_id: import.target_binding_info_id,
                target_module_id: import.target_module_id,
                target_module_info: import.target_module_info,
                alias: *symbol,
                import_path,
                visibility: import.visibility,
                span: import.path_span().clone(),
                binding_info_id: import.binding_info_id,
            }
        })
        .collect()
}
