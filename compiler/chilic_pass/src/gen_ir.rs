use crate::{
    expand_use_wildcard::expand_use_wildcard,
    solve_defer::{DeferContext, SolveDefer},
};
use chilic_ast::{ir::Ir, module::Module, Ast};
use chilic_error::DiagnosticResult;
use codespan_reporting::files::SimpleFiles;

pub fn gen_ir(
    asts: Vec<Ast>,
    files: SimpleFiles<String, String>,
) -> DiagnosticResult<Ir> {
    let mut ir = Ir::new(files);

    for ast in asts {
        let mut module = Module::new(ast.module_info);

        module.uses.extend(ast.uses);
        module.entities.extend(ast.entities);

        module.entities.solve_defer(&mut DeferContext::new());

        ir.modules.insert(ast.module_info.name, module);
        ir.foreign_libraries.extend(ast.foreign_libraries);
    }

    expand_use_wildcard(&mut ir)?;

    Ok(ir)
}
