use chilic_ast::{
    entity::Visibility,
    ir::Ir,
    r#use::{Use, UsePathNode},
};
use chilic_error::DiagnosticResult;
use chilic_span::Spanned;
use codespan_reporting::diagnostic::{Diagnostic, Label};
use ustr::{ustr, Ustr, UstrMap};

pub(super) fn expand_use_wildcard(ir: &mut Ir) -> DiagnosticResult<()> {
    let mut wildcard_uses = UstrMap::default();

    for (module_name, module) in ir.modules.iter() {
        let wildcard_uses = wildcard_uses.entry(*module_name).or_insert(vec![]);
        for use_ in module.uses.iter() {
            if use_.is_wildcard() {
                wildcard_uses.extend(expand_wildcard_use(
                    ir,
                    *module_name,
                    use_.clone(),
                )?);
            }
        }
    }

    for (module_name, module) in ir.modules.iter_mut() {
        module.uses.retain(|use_| !use_.is_wildcard());
        let expanded_uses = wildcard_uses.get(module_name).unwrap().clone();
        module.uses.extend(expanded_uses);
    }

    // TODO: I also need to expand use expressions

    Ok(())
}

fn expand_wildcard_use(
    ir: &Ir,
    module_name: Ustr,
    mut use_: Use,
) -> DiagnosticResult<Vec<Use>> {
    // for a given use: `use foo.*`;
    // with symbols: A, B, C;
    // this function will expand this use to:
    //      `use foo.A`
    //      `use foo.B`
    //      `use foo.C`
    //

    use_.use_path.pop();
    let path = ustr(&use_.use_path_str());
    match ir.modules.get(&path) {
        Some(module) => {
            let mut symbols_to_use = vec![];

            for use_ in module.uses.iter() {
                if !use_.is_wildcard() && use_.visibility != Visibility::Private
                {
                    symbols_to_use.push((use_.alias, use_.visibility));
                }
            }

            for entity in module.entities.iter() {
                if entity.visibility != Visibility::Private {
                    symbols_to_use.push((
                        entity.pattern.into_single().symbol,
                        entity.visibility,
                    ));
                }
            }

            Ok(symbols_to_use
                .iter()
                .map(|(symbol, visibility)| {
                    let mut use_path = use_.use_path.clone();
                    use_path.push(Spanned::new(
                        UsePathNode::Symbol(*symbol),
                        use_.span().clone(),
                    ));
                    Use {
                        module_info: use_.module_info,
                        alias: *symbol,
                        use_path,
                        visibility: *visibility,
                        span: use_.span().clone(),
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
                use_.span.file_id,
                use_.span.range().clone(),
            )
            .with_message(format!("not found in `{}`", module_name))])),
    }
}
