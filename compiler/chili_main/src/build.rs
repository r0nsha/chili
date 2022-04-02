use chili_ast::workspace::Workspace;
use chili_astgen::{AstGenerationMode, AstGenerationStats};
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
            // TODO: pass `AstGenerationMode` from a `-mt` flag
            match chili_astgen::generate_ast(&mut workspace, AstGenerationMode::SingleThreaded) {
                Ok(result) => result,
                Err(diagnostic) => {
                    emit_single_diagnostic(&workspace.files, diagnostic);
                    return;
                }
            }
        }
    };

    // General pre-check transforms, such as glob import expansion
    time! { "resolve",
        chili_resolve::resolve(&mut workspace, &mut asts)
    };

    // Infer, type inference, static analysis, const folding, etc..
    let (mut typed_ast, tycx) = time! { "check",
        match chili_check::check(&mut workspace, asts) {
            Ok(result) => result,
            Err(diagnostic) => {
                emit_single_diagnostic(&workspace.files, diagnostic);
                return;
            }
        }
    };

    // Lint - does auxillary checks which are not required for type inference
    time! { "lint",
        if let Err(diagnostic) = chili_lint::lint(&mut workspace, &tycx, &typed_ast) {
            emit_single_diagnostic(&workspace.files, diagnostic);
            return;
        }
    }

    // Defer - resolve all `defer` statements
    time! { "defer",
        chili_defer::solve_defers(&mut typed_ast)
    }

    // time! { "codegen(llvm)",
    //     chili_llvm::codegen(&workspace, &asts)
    // }

    all_sw.stop();
    print_stats(stats, all_sw.elapsed().as_millis());

    // chili_pretty_print::print_typed_ast(&typed_ast, &workspace, &tycx);
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
