use std::process::Command;

use chili_ast::{ast::TypedAst, workspace::Workspace};
use chili_astgen::AstGenerationStats;
use chili_error::diagnostic::Diagnostic;
use chili_infer::ty_ctx::TyCtx;
use colored::Colorize;
use common::{
    build_options::{BuildOptions, CodegenOptions},
    time, Stopwatch,
};
use num_format::{Locale, ToFormattedString};
use path_absolutize::*;

pub type StartWorkspaceResult = (Workspace, Option<TyCtx>, Option<TypedAst>);

pub fn start_workspace(name: String, build_options: BuildOptions) -> StartWorkspaceResult {
    // Note (Ron): This is a workaround for a stack overflow that we're getting during the `Check` phase.
    // Note (Ron): Are we allocating too much in the stack frame while recursing?
    std::thread::Builder::new()
        .name("main".to_string())
        .stack_size(8 * 1024 * 1024) // 8 MB
        .spawn(|| start_workspace_inner(name, build_options))
        .unwrap()
        .join()
        .unwrap()
}

fn start_workspace_inner(name: String, build_options: BuildOptions) -> StartWorkspaceResult {
    // Set up workspace
    let source_path = build_options.source_path();
    let absolute_path = source_path.absolutize().unwrap();

    let root_dir = absolute_path.parent().unwrap().to_path_buf();
    let std_dir = chili_ast::compiler_info::std_module_root_dir();

    let mut workspace = Workspace::new(name, build_options.clone(), root_dir, std_dir);

    let all_sw = if workspace.build_options.verbose {
        Some(Stopwatch::start_new("time"))
    } else {
        None
    };

    // Check that root file exists
    if !source_path.exists() {
        workspace.diagnostics.push(
            Diagnostic::error()
                .with_message(format!("file `{}` doesn't exist", source_path.display())),
        );

        workspace.emit_diagnostics();

        return (workspace, None, None);
    }

    // Parse all source files into ast's
    let (asts, stats) = time! { workspace.build_options.verbose, "parse", {
            match chili_astgen::generate_ast(&mut workspace) {
                Some(result) => result,
                None => {
                    workspace.emit_diagnostics();
                    return (workspace, None,None);
                }
            }
        }
    };

    if workspace.diagnostics.has_errors() {
        workspace.emit_diagnostics();
        return (workspace, None, None);
    }

    // Type inference, type checking, static analysis, const folding, etc..
    let (typed_ast, tycx) = time! { workspace.build_options.verbose, "check",
        match chili_check::check(&mut workspace, asts) {
            Ok(result) => result,
            Err((tycx, typed_ast, diagnostic)) => {
                workspace.diagnostics.push(diagnostic);
                workspace.emit_diagnostics();
                return (workspace, Some(tycx), Some(typed_ast));
            }
        }
    };

    if workspace.diagnostics.has_errors() {
        workspace.emit_diagnostics();
        return (workspace, Some(tycx), Some(typed_ast));
    }

    // Lint - does auxillary checks which are not required for compilation
    time! { workspace.build_options.verbose, "lint",
        chili_lint::lint(&mut workspace, &tycx, &typed_ast)
    }

    if workspace.diagnostics.has_errors() {
        workspace.emit_diagnostics();
        return (workspace, Some(tycx), Some(typed_ast));
    }

    // chili_pretty_print::print_typed_ast(&typed_ast, &workspace, &tycx);

    // Code generation
    match &workspace.build_options.codegen_options {
        CodegenOptions::Codegen(codegen_options) => {
            let executable_path =
                chili_backend_llvm::codegen(&workspace, &tycx, &typed_ast, codegen_options);

            if workspace.build_options.verbose {
                print_stats(stats, all_sw.unwrap().elapsed().as_millis());
            }

            if codegen_options.run_when_done {
                Command::new(&executable_path)
                    .spawn()
                    .ok()
                    .unwrap_or_else(|| panic!("{}", executable_path));
            }
        }
        _ => {
            if workspace.build_options.verbose {
                print_stats(stats, all_sw.unwrap().elapsed().as_millis());
            }
        }
    }

    (workspace, Some(tycx), Some(typed_ast))
}

fn print_stats(stats: AstGenerationStats, elapsed_ms: u128) {
    println!("------------------------");
    println!(
        "{}\t{}",
        "lines:".cyan().bold(),
        stats.total_lines.to_formatted_string(&Locale::en)
    );
    println!("{}\t{}m", "time:".cyan().bold(), elapsed_ms);
}
