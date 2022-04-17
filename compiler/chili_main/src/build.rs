use chili_ast::workspace::Workspace;
use chili_astgen::{AstGenerationMode, AstGenerationStats};
use chili_error::diagnostic::Diagnostic;
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
    let std_dir = chili_ast::compiler_info::std_module_root_dir();

    let mut workspace = Workspace::new(build_options.clone(), root_dir, std_dir);

    // Check that root file exists
    if !source_path.exists() {
        workspace.diagnostics.add(
            Diagnostic::error()
                .with_message(format!("file `{}` doesn't exist", source_path.display())),
        );
        workspace.diagnostics.emit_and_exit();
    }

    // Parse all source files into ast's
    let (mut asts, stats) = time! { "parse", {
            match chili_astgen::generate_ast(&mut workspace, AstGenerationMode::SingleThreaded) {
                Ok(r) => r,
                Err(_) => workspace.diagnostics.emit_and_exit()
            }
        }
    };

    if workspace.diagnostics.has_errors() {
        workspace.diagnostics.emit_and_exit();
    }

    // General pre-check transforms, such as glob import expansion
    time! { "resolve",
        chili_resolve::resolve(&mut workspace, &mut asts)
    };

    // Infer, type inference, static analysis, const folding, etc..
    let (mut typed_ast, tycx) = time! { "check",
        match chili_check::check(&mut workspace, asts) {
            Ok(result) => result,
            Err(diagnostic) => {
                workspace.diagnostics.add(diagnostic);
                workspace.diagnostics.emit_and_exit();
            }
        }
    };

    if workspace.diagnostics.has_errors() {
        workspace.diagnostics.emit_and_exit();
    }

    // Lint - does auxillary checks which are not required for type inference
    time! { "lint",
        chili_lint::lint(&mut workspace, &tycx, &typed_ast)
    }

    if workspace.diagnostics.has_errors() {
        workspace.diagnostics.emit_and_exit();
    }

    // Defer - resolve all `defer` statements
    time! { "defer",
        chili_defer::solve_defers(&mut typed_ast)
    }

    // chili_pretty_print::print_typed_ast(&typed_ast, &workspace, &tycx);

    // Code generation
    chili_backend_llvm::codegen(&workspace, &tycx, &typed_ast);

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
