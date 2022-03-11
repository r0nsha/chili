use chili_ast::ast::{Import, ImportPathNode, Ir, Visibility};
use chili_error::DiagnosticResult;
use chili_span::Spanned;
use codespan_reporting::diagnostic::{Diagnostic, Label};
use ustr::{ustr, Ustr, UstrMap};

pub(super) fn expand_glob_imports(ir: &mut Ir) -> DiagnosticResult<()> {
    let mut glob_imports = UstrMap::default();

    for (module_name, module) in ir.modules.iter() {
        let glob_imports = glob_imports.entry(*module_name).or_insert(vec![]);
        for import in module.imports.iter() {
            if import.is_glob() {
                glob_imports.extend(expand_glob_import(
                    ir,
                    *module_name,
                    import.clone(),
                )?);
            }
        }
    }

    for (module_name, module) in ir.modules.iter_mut() {
        module.imports.retain(|import| !import.is_glob());
        let expanded_imports = glob_imports.get(module_name).unwrap().clone();
        module.imports.extend(expanded_imports);
    }

    // TODO: I also need to expand use expressions

    Ok(())
}

fn expand_glob_import(
    ir: &Ir,
    module_name: Ustr,
    mut import: Import,
) -> DiagnosticResult<Vec<Import>> {
    // for a given use: `use foo.*`;
    // with symbols: A, B, C;
    // this function will expand this use to:
    //      `use foo.A`
    //      `use foo.B`
    //      `use foo.C`
    //

    import.import_path.pop();
    let path = ustr(&import.import_path_str());
    match ir.modules.get(&path) {
        Some(module) => {
            let mut symbols_to_import = vec![];

            for import in module.imports.iter() {
                if !import.is_glob() && import.visibility != Visibility::Private
                {
                    symbols_to_import.push((import.alias, import.visibility));
                }
            }

            for binding in module.bindings.iter() {
                if binding.visibility != Visibility::Private {
                    symbols_to_import.push((
                        binding.pattern.into_single().symbol,
                        binding.visibility,
                    ));
                }
            }

            Ok(symbols_to_import
                .iter()
                .map(|(symbol, visibility)| {
                    let mut import_path = import.import_path.clone();
                    import_path.push(Spanned::new(
                        ImportPathNode::Symbol(*symbol),
                        import.span().clone(),
                    ));
                    Import {
                        module_idx: Default::default(),
                        module_info: import.module_info,
                        alias: *symbol,
                        import_path,
                        visibility: *visibility,
                        span: import.span().clone(),
                        binding_info_idx: import.binding_info_idx,
                    }
                })
                .collect())
        }
        None => Err(Diagnostic::error()
            .with_message(format!(
                "cannot find path `{}` in module `{}`",
                path, module_name,
            ))
            .with_labels(vec![Label::primary(
                import.span.file_id,
                import.span.range().clone(),
            )
            .with_message(format!("not found in `{}`", module_name))])),
    }
}
