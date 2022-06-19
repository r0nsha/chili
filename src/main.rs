mod ast;
mod astgen;
mod backend_llvm;
mod check;
mod common;
mod driver;
mod error;
mod ide;
mod infer;
mod interp;
mod lint;
mod parse;
mod pretty_print;
mod span;
mod token;

use crate::common::{
    build_options::{BuildOptions, CodegenOptions, DiagnosticOptions, OptLevel},
    target::TargetPlatform,
};
use clap::*;
use colored::Colorize;
use common::build_options::EnabledCodegenOptions;
use path_absolutize::Absolutize;
use std::path::{Path, PathBuf};

#[derive(Parser, Debug)]
#[clap(
    name = "chili",
    author,
    version,
    about,
    long_about = "Compiler for the Chili programming language"
)]
struct Args {
    /// The main action the compiler should take.
    input: String,

    /// Print trace information verbosely.
    #[clap(long)]
    verbose: bool,

    /// Omit colors from output.
    #[clap(long)]
    no_color: bool,

    /// Emit LLVM IR file.
    #[clap(long)]
    emit_llvm_ir: bool,

    /// Additional include paths, separated by ;.
    #[clap(long)]
    include_paths: Option<String>,

    /// Enables Run mode - which runs the input file using the default options for the current platform.
    #[clap(long)]
    run: bool,

    /// Enables Check mode - which only checks the input file, skipping code generation.
    /// Check mode also enables additionals flags for language support.
    #[clap(long)]
    check: bool,

    /// Only available in Check mode.
    /// Return diagnostics of the input file, and all files imported by it - recursively.
    #[clap(long)]
    diagnostics: bool,

    /// Only available in Check mode.
    /// Return the hover info for a given index, in the given input file.
    #[clap(long)]
    hover_info: Option<usize>,

    /// Only available in Check mode.
    /// Return the hover info for a given index, in the given input file.
    #[clap(long)]
    goto_def: Option<usize>,
}

fn main() {
    std::thread::Builder::new()
        .name("main".to_string())
        .stack_size(8 * 1024 * 1024) // 8 MB
        .spawn(cli)
        .unwrap()
        .join()
        .unwrap();
}

fn cli() {
    let args = Args::parse();

    match get_file_path(&args.input) {
        Ok(source_file) => {
            let name = get_workspace_name(&source_file);

            if args.run {
                let build_options = BuildOptions {
                    source_file,
                    output_file: None,
                    target_platform: current_target_platform(),
                    opt_level: OptLevel::Debug,
                    verbose: args.verbose,
                    diagnostic_options: DiagnosticOptions::Emit {
                        no_color: args.no_color,
                    },
                    codegen_options: CodegenOptions::Codegen(EnabledCodegenOptions {
                        emit_llvm_ir: args.emit_llvm_ir,
                        run_executable: true,
                    }),
                    include_paths: get_include_paths(&args.include_paths),
                    check_mode: false,
                };

                driver::start_workspace(name, build_options);
            } else if args.check {
                let build_options = BuildOptions {
                    source_file,
                    output_file: None,
                    target_platform: current_target_platform(),
                    opt_level: OptLevel::Debug,
                    verbose: false,
                    diagnostic_options: DiagnosticOptions::DontEmit,
                    codegen_options: CodegenOptions::Skip,
                    include_paths: get_include_paths(&args.include_paths),
                    check_mode: true,
                };

                let result = driver::start_workspace(name, build_options);

                if args.diagnostics {
                    ide::diagnostics(
                        &result.workspace,
                        result.tycx.as_ref(),
                        result.typed_ast.as_ref(),
                    );
                } else if let Some(offset) = args.hover_info {
                    ide::hover_info(&result.workspace, result.tycx.as_ref(), offset);
                } else if let Some(offset) = args.goto_def {
                    ide::goto_definition(&result.workspace, result.tycx.as_ref(), offset);
                }
            } else {
                let build_options = BuildOptions {
                    source_file,
                    output_file: None,
                    target_platform: current_target_platform(),
                    opt_level: OptLevel::Debug,
                    verbose: args.verbose,
                    diagnostic_options: DiagnosticOptions::Emit {
                        no_color: args.no_color,
                    },
                    codegen_options: CodegenOptions::Skip,
                    include_paths: get_include_paths(&args.include_paths),
                    check_mode: false,
                };

                driver::start_workspace(name, build_options);
            }
        }
        Err(e) => print_err(&e),
    }
}

fn get_workspace_name(source_file: &Path) -> String {
    source_file
        .file_stem()
        .map_or("root", |p| p.to_str().unwrap())
        .to_string()
}

fn get_file_path(input_file: &str) -> Result<PathBuf, String> {
    let path = Path::new(input_file).absolutize().unwrap();

    if !path.exists() {
        Err(format!("input file `{}` doesn't exist", input_file))
    } else if !path.is_file() {
        Err(format!("`{}` is not a file", input_file))
    } else {
        Ok(path.to_path_buf())
    }
}

fn current_target_platform() -> TargetPlatform {
    match TargetPlatform::current() {
        Ok(t) => t,
        Err(os) => {
            print_err(&format!("targeting unsupported platform: {}", os));
            std::process::exit(1);
        }
    }
}

fn print_err(msg: &str) {
    println!("\n{} {}\n", "error:".red().bold(), msg.bold());
}

fn get_include_paths(include_paths: &Option<String>) -> Vec<PathBuf> {
    include_paths.as_ref().map_or_else(
        || vec![],
        |i| i.split(';').map(|s| PathBuf::from(s)).collect(),
    )
}