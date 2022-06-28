use crate::{
    astgen::AstGenerationStats,
    common::{
        build_options::{BuildOptions, CodegenOptions},
        Stopwatch,
    },
    error::diagnostic::Diagnostic,
    hir,
    infer::ty_ctx::TyCtx,
    time,
    workspace::{compiler_info, Workspace},
};
use colored::Colorize;
use num_format::{Locale, ToFormattedString};
use std::process::Command;

pub struct StartWorkspaceResult {
    pub workspace: Workspace,
    pub tycx: Option<TyCtx>,
    pub cache: Option<hir::Cache>,
}

impl StartWorkspaceResult {
    pub fn new(workspace: Workspace, tycx: Option<TyCtx>, cache: Option<hir::Cache>) -> Self {
        Self {
            workspace,
            tycx,
            cache,
        }
    }
}

pub fn start_workspace(name: String, build_options: BuildOptions) -> StartWorkspaceResult {
    // Set up workspace
    let source_file = &build_options.source_file;

    let root_dir = source_file.parent().unwrap().to_path_buf();
    let std_dir = compiler_info::std_module_root_dir();

    let mut workspace = Workspace::new(name, build_options.clone(), root_dir, std_dir);

    let all_sw = if workspace.build_options.emit_times {
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
    let (modules, stats) = time! { workspace.build_options.emit_times, "parse", {
            match crate::astgen::generate_ast(&mut workspace) {
                Some(result) => result,
                None => {
                    workspace.emit_diagnostics();
                    return  StartWorkspaceResult::new(workspace, None, None);
                }
            }
        }
    };

    // Type inference, type checking, static analysis, const folding, etc..
    let (cache, tycx) = time! { workspace.build_options.emit_times, "check", {
        crate::check::check(&mut workspace, modules)
    }};

    if workspace.diagnostics.has_errors() {
        workspace.emit_diagnostics();
        return StartWorkspaceResult::new(workspace, Some(tycx), Some(cache));
    } else if workspace.build_options.emit_hir {
        hir::pretty::print(&cache, &workspace, &tycx);
    }

    // Lint - does auxillary checks which are not required for compilation
    time! { workspace.build_options.emit_times, "lint",
        crate::lint::lint(&mut workspace, &tycx, &cache)
    }

    if workspace.diagnostics.has_errors() {
        workspace.emit_diagnostics();
        return StartWorkspaceResult::new(workspace, Some(tycx), Some(cache));
    }

    // Code generation
    // todo!("codegen");
    // match &workspace.build_options.codegen_options {
    //     CodegenOptions::Codegen(codegen_options) => {
    //         let executable_path =
    //             crate::backend::llvm::codegen(&workspace, &tycx, &typed_ast, codegen_options);

    //         if workspace.build_options.emit_times {
    //             print_stats(stats, all_sw.unwrap().elapsed().as_millis());
    //         }

    //         if codegen_options.run_executable {
    //             Command::new(&executable_path)
    //                 .spawn()
    //                 .ok()
    //                 .unwrap_or_else(|| panic!("{}", executable_path));
    //         }
    //     }
    //     _ => {
    //         if workspace.build_options.emit_times {
    //             print_stats(stats, all_sw.unwrap().elapsed().as_millis());
    //         }
    //     }
    // }

    StartWorkspaceResult::new(workspace, Some(tycx), Some(cache))
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
