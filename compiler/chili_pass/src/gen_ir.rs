use crate::{
    expand_glob_imports::expand_glob_imports,
    solve_defer::{DeferContext, SolveDefer},
};
use chili_ast::ast::{Ast, Ir, Module};
use chili_error::DiagnosticResult;
use codespan_reporting::files::SimpleFiles;

pub fn gen_ir(
    asts: Vec<Ast>,
    files: SimpleFiles<String, String>,
) -> DiagnosticResult<Ir> {
    let mut ir = Ir::new(files);

    for ast in asts {
        let mut module = Module::new(ast.module_info);

        module.imports.extend(ast.imports);
        module.bindings.extend(ast.bindings);

        module.bindings.solve_defer(&mut DeferContext::new());

        ir.modules.insert(ast.module_info.name, module);
        ir.foreign_libraries.extend(ast.foreign_libraries);
    }

    expand_glob_imports(&mut ir)?;

    Ok(ir)
}
