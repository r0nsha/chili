use crate::ast::{ast::TypedAst, workspace::Workspace};
use crate::astgen::AstGenerationStats;
use crate::error::diagnostic::Diagnostic;
use crate::infer::ty_ctx::TyCtx;
use crate::{
    common::{
        build_options::{BuildOptions, CodegenOptions},
        Stopwatch,
    },
    time,
};
use colored::Colorize;
use num_format::{Locale, ToFormattedString};
use std::process::Command;

pub struct StartWorkspaceResult {
    pub workspace: Workspace,
    pub tycx: Option<TyCtx>,
    pub typed_ast: Option<TypedAst>,
}

impl StartWorkspaceResult {
    pub fn new(workspace: Workspace, tycx: Option<TyCtx>, typed_ast: Option<TypedAst>) -> Self {
        Self {
            workspace,
            tycx,
            typed_ast,
        }
    }
}

pub fn start_workspace(name: String, build_options: BuildOptions) -> StartWorkspaceResult {
    // Set up workspace
    let source_file = &build_options.source_file;

    let root_dir = source_file.parent().unwrap().to_path_buf();
    let std_dir = crate::ast::compiler_info::std_module_root_dir();

    let mut workspace = Workspace::new(name, build_options.clone(), root_dir, std_dir);

    let all_sw = if workspace.build_options.verbose {
        Some(Stopwatch::start_new("time"))
    } else {
        None
    };

    // Check that root file exists
    if !source_file.exists() {
        workspace.diagnostics.push(
            Diagnostic::error()
                .with_message(format!("file `{}` doesn't exist", source_file.display())),
        );

        workspace.emit_diagnostics();

        return StartWorkspaceResult::new(workspace, None, None);
    }

    // Parse all source files into ast's
    let (asts, stats) = time! { workspace.build_options.verbose, "parse", {
            match crate::astgen::generate_ast(&mut workspace) {
                Some(result) => result,
                None => {
                    workspace.emit_diagnostics();
                    return  StartWorkspaceResult::new(workspace, None, None);
                }
            }
        }
    };

    if workspace.diagnostics.has_errors() {
        workspace.emit_diagnostics();
        return StartWorkspaceResult::new(workspace, None, None);
    }

    // Type inference, type checking, static analysis, const folding, etc..
    let (typed_ast, tycx) = time! { workspace.build_options.verbose, "check",
        match crate::check::check(&mut workspace, asts) {
            Ok(result) => result,
            Err((tycx, typed_ast, diagnostic)) => {
                workspace.diagnostics.push(diagnostic);
                workspace.emit_diagnostics();
                return  StartWorkspaceResult::new(workspace, Some(tycx), Some(typed_ast));
            }
        }
    };

    if workspace.diagnostics.has_errors() {
        workspace.emit_diagnostics();
        return StartWorkspaceResult::new(workspace, Some(tycx), Some(typed_ast));
    }

    // Lint - does auxillary checks which are not required for compilation
    time! { workspace.build_options.verbose, "lint",
        crate::lint::lint(&mut workspace, &tycx, &typed_ast)
    }

    if workspace.diagnostics.has_errors() {
        workspace.emit_diagnostics();
        return StartWorkspaceResult::new(workspace, Some(tycx), Some(typed_ast));
    }

    // chili_pretty_print::print_typed_ast(&typed_ast, &workspace, &tycx);

    // Code generation
    match &workspace.build_options.codegen_options {
        CodegenOptions::Codegen(codegen_options) => {
            let executable_path =
                crate::backend_llvm::codegen(&workspace, &tycx, &typed_ast, codegen_options);

            if workspace.build_options.verbose {
                print_stats(stats, all_sw.unwrap().elapsed().as_millis());
            }

            if codegen_options.run_executable {
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

    StartWorkspaceResult::new(workspace, Some(tycx), Some(typed_ast))
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
