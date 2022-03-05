use chili_ast::workspace::Workspace;
use chili_astgen::AstGenerator;
use chili_llvm::codegen;
use codespan_reporting::{diagnostic::Diagnostic, files::SimpleFiles};
use colored::Colorize;

use chili_error::emit_single_diagnostic;
use common::{build_options::BuildOptions, Stopwatch};
use num_format::{Locale, ToFormattedString};
use path_absolutize::*;

pub fn do_build(build_options: BuildOptions) {
    println!();

    let mut all_sw = Stopwatch::start_new("time");

    let source_path = build_options.source_path();
    let absolute_path = source_path.absolutize().unwrap();

    let root_dir = absolute_path.parent().unwrap();
    let std_dir = common::compiler_info::std_module_root_dir();

    let mut workspace = Workspace::new(root_dir, &std_dir);

    if !source_path.exists() {
        emit_single_diagnostic(
            &workspace.files,
            Diagnostic::error().with_message(format!(
                "file `{}` doesn't exist",
                source_path.display()
            )),
        );
        return;
    }

    {
        let mut ast_generator = AstGenerator::new(&mut workspace);

        if let Err(diagnostic) =
            ast_generator.start(build_options.source_file.clone())
        {
            emit_single_diagnostic(
                &ast_generator.workspace.lock().unwrap().files,
                diagnostic,
            );
            return;
        }
    }

    for (i, a) in workspace.parsed_trees.iter() {
        println!("{:?} -> {:?}", i, a.module_info);
    }

    // let sw = Stopwatch::start_new("lower");

    // let ir = match chili_pass::gen_ir(asts, workspace.files.clone()) {
    //     Ok(ir) => ir,
    //     Err(diagnostic) => {
    //         emit_single_diagnostic(&workspace.files, diagnostic);
    //         return;
    //     }
    // };

    // sw.print();

    // // ir.print();

    // let sw = Stopwatch::start_new("analyze");

    // let ir = match chili_check::check_ir(&build_options, ir) {
    //     Ok(ir) => ir,
    //     Err(diagnostic) => {
    //         emit_single_diagnostic(&workspace.files, diagnostic);
    //         return;
    //     }
    // };

    // sw.print();

    // // ir.print();

    // codegen(&build_options, &ir);

    all_sw.stop();

    print_stats(
        &workspace.files,
        workspace.root_file_id,
        all_sw.elapsed().as_millis(),
    );
}

fn print_stats(
    files: &SimpleFiles<String, String>,
    root_file_id: usize,
    elapsed_ms: u128,
) {
    let file_line_count =
        files.get(root_file_id).unwrap().source().lines().count();
    println!();
    println!(
        "{}\t{}",
        "span:".cyan().bold(),
        file_line_count.to_formatted_string(&Locale::en)
    );
    println!("{}\t{}m", "time:".cyan().bold(), elapsed_ms);
    println!();
}
