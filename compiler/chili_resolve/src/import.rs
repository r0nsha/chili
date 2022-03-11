use std::collections::HashMap;

use chili_ast::{
    ast::{Ast, Import, ImportPathNode},
    workspace::ModuleIdx,
};
use chili_span::Spanned;
use ustr::Ustr;

pub(crate) type ModuleExports = HashMap<ModuleIdx, Vec<Ustr>>;

pub(crate) fn collect_module_exports(asts: &Vec<Ast>) -> ModuleExports {
    let mut exports: ModuleExports = Default::default();

    for ast in asts.iter() {
        let entry = exports.entry(ast.module_idx).or_default();

        for import in ast.imports.iter() {
            if import.visibility.is_public() {
                entry.push(import.alias);
            }
        }

        for binding in ast.bindings.iter() {
            if binding.visibility.is_public() {
                // TODO: support destructor patterns
                entry.push(binding.pattern.into_single().symbol);
            }
        }
    }

    exports
}

pub(crate) fn expand_and_replace_glob_imports(imports: &mut Vec<Import>, exports: &ModuleExports) {
    let mut to_remove: Vec<usize> = vec![];
    let mut to_add: Vec<Import> = vec![];

    for (index, import) in imports.iter().enumerate() {
        if !import.is_glob() {
            continue;
        }

        let expanded_imports = expand_glob_import(import.clone(), exports);

        to_remove.push(index);
        to_add.extend(expanded_imports);
    }

    let mut removed = 0;
    for index in to_remove {
        imports.remove(index - removed);
        removed += 1;
    }

    imports.extend(to_add);
}

fn expand_glob_import(import: Import, exports: &ModuleExports) -> Vec<Import> {
    // for a given use: `use foo.*`;
    // with symbols: A, B, C;
    // this function will expand this use to:
    //      `use foo.A`
    //      `use foo.B`
    //      `use foo.C`
    //

    let exports = exports.get(&import.module_idx).unwrap();
    exports
        .iter()
        .map(|symbol| {
            let mut import_path = import.import_path.clone();
            import_path.pop();
            import_path.push(Spanned::new(
                ImportPathNode::Symbol(*symbol),
                import.span().clone(),
            ));
            Import {
                module_idx: import.module_idx,
                module_info: import.module_info,
                alias: *symbol,
                import_path,
                visibility: import.visibility,
                span: import.span().clone(),
                binding_info_idx: import.binding_info_idx,
            }
        })
        .collect()
}
