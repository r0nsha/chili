use chili_ast::workspace::Workspace;
use chili_astgen::{AstGenerationStats, AstGenerator};
use chili_error::emit_single_diagnostic;
use codespan_reporting::diagnostic::Diagnostic;
use colored::Colorize;
use common::{build_options::BuildOptions, time, Stopwatch};
use num_format::{Locale, ToFormattedString};
use path_absolutize::*;

pub fn do_build(build_options: BuildOptions) {
    println!();

    let mut all_sw = Stopwatch::start_new("time");

    // Set up workspace

    let source_path = build_options.source_path();
    let absolute_path = source_path.absolutize().unwrap();

    let root_dir = absolute_path.parent().unwrap().to_path_buf();
    let std_dir = common::compiler_info::std_module_root_dir();

    let mut workspace = Workspace::new(build_options.clone(), root_dir, std_dir);

    // Check that root file exists

    if !source_path.exists() {
        emit_single_diagnostic(
            &workspace.files,
            Diagnostic::error()
                .with_message(format!("file `{}` doesn't exist", source_path.display())),
        );
        return;
    }

    // Parse all source files into ast's

    let (mut asts, stats) = time! { "parse", {
            let mut generator = AstGenerator::new(&mut workspace);

            match generator.start(build_options.source_file.clone()) {
                Ok(result) => result,
                Err(diagnostic) => {
                    emit_single_diagnostic(&generator.workspace.lock().unwrap().files, diagnostic);
                    return;
                }
            }
        }
    };

    // Resolve ast definition/binding information

    time! { "resolve",
        if let Err(diagnostic) = chili_resolve::resolve(&mut workspace, &mut asts) {
            emit_single_diagnostic(&workspace.files, diagnostic);
            return;
        }
    }

    time! { "infer",
        if let Err(diagnostic) = chili_infer_new::infer(&mut workspace, &mut asts) {
            emit_single_diagnostic(&workspace.files, diagnostic);
            return;
        }
    }

    // // Check - does type inference, type checking and const folding

    // time! { "check",
    //     if let Err(diagnostic) = chili_check::check(&mut workspace, &mut asts) {
    //         emit_single_diagnostic(&workspace.files, diagnostic);
    //         return;
    //     }
    // }

    // // Lint - does auxillary checks which are not _required_ for type inference

    // time! { "lint",
    //     if let Err(diagnostic) = chili_lint::lint(&mut workspace, &mut asts) {
    //         emit_single_diagnostic(&workspace.files, diagnostic);
    //         return;
    //     }
    // }

    for ast in asts.iter() {
        ast.print();
    }

    // time! { "codegen(llvm)",
    //     chili_llvm::codegen(&workspace, &asts)
    // }

    all_sw.stop();

    print_stats(stats, all_sw.elapsed().as_millis());
}

fn print_stats(stats: AstGenerationStats, elapsed_ms: u128) {
    println!();
    println!(
        "{}\t{}",
        "lines:".cyan().bold(),
        stats.total_lines.to_formatted_string(&Locale::en)
    );
    println!("{}\t{}m", "time:".cyan().bold(), elapsed_ms);
    println!();
}
