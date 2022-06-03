mod import;

use chili_ast::{ast::Ast, workspace::Workspace};
use chili_error::SyntaxError;
use chili_span::Span;
use import::{collect_module_exports, resolve_imports};
use ustr::{Ustr, UstrMap};

pub fn resolve(workspace: &mut Workspace, asts: &mut [Ast]) {
    println!(
        "{:#?}",
        asts.iter()
            .map(|a| a.module_info.file_path)
            .collect::<Vec<Ustr>>()
    );

    // Add all module_infos to the workspace
    for ast in asts.iter_mut() {
        ast.module_id = workspace.add_module_info(ast.module_info);

        // the root module should always have an empty name
        if ast.module_info.name.is_empty() {
            workspace.root_module_id = ast.module_id;
        }
    }

    collect_module_exports(asts, &mut workspace.exports);

    // Assign module ids to all imports
    for ast in asts.iter_mut() {
        for import in ast.imports.iter_mut() {
            println!("{:?}", import.target_module_info);
            import.target_module_id = workspace
                .find_module_info(import.target_module_info)
                .unwrap();
        }
    }

    // Resolve all imports and apply module ids to top level expressions
    for ast in asts.iter_mut() {
        let mut defined_symbols = UstrMap::<Span>::default();

        resolve_imports(&mut ast.imports, &workspace.exports);

        ast.imports.iter_mut().for_each(|import| {
            import.module_id = ast.module_id;
            check_duplicate_global_symbol(
                workspace,
                &mut defined_symbols,
                import.alias,
                import.span,
            );
        });

        ast.bindings.iter_mut().for_each(|binding| {
            binding.module_id = ast.module_id;
            for pat in binding.pattern.iter() {
                check_duplicate_global_symbol(
                    workspace,
                    &mut defined_symbols,
                    pat.symbol,
                    pat.span,
                );
            }
        });
    }
}

fn check_duplicate_global_symbol(
    workspace: &mut Workspace,
    defined_symbols: &mut UstrMap<Span>,
    symbol: Ustr,
    span: Span,
) {
    if let Some(already_defined_span) = defined_symbols.get(&symbol) {
        workspace.diagnostics.push(SyntaxError::duplicate_symbol(
            *already_defined_span,
            span,
            symbol,
        ));
    } else {
        defined_symbols.insert(symbol, span);
    }
}
