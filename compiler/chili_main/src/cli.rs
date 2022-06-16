use crate::{build::start_workspace, ide};
use clap::*;
use colored::Colorize;
use common::{
    build_options::{BuildOptions, CodegenOptions, DiagnosticOptions, OptLevel},
    target::TargetPlatform,
};
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
    /// The main action the compiler should take
    #[clap(subcommand)]
    action: Action,
}

#[derive(clap::Subcommand, Debug, PartialEq, Eq)]
enum Action {
    /// Compile a chili source file, as an executable
    Build(BuildArgs),
    /// Same as `build`, but also runs the compiled executable
    Run(BuildArgs),
    /// Checks the source file, providing additional flags - mainly for LSP usage
    Check(CheckArgs),
}

#[derive(Args, Debug, PartialEq, Eq)]
struct BuildArgs {
    /// The main action the compiler should take
    input: String,

    /// Change the build mode to release, disabling runtime safety and enabling optimizations
    #[clap(long)]
    release: bool,

    /// Print trace information verbosely
    #[clap(long)]
    verbose: bool,

    /// Emit LLVM IR file
    #[clap(long)]
    emit_llvm_ir: bool,

    /// Skip the code generation phase
    #[clap(long)]
    no_codegen: bool,

    /// Omit colors from output
    #[clap(long)]
    no_color: bool,

    /// Specify the target platform. Important: This flag is a placeholder until workspace-based configuration is implemented
    #[clap(arg_enum, default_value_t = Target::Current)]
    target: Target,

    /// Additional include paths, separated by ;
    #[clap(long)]
    include_paths: Option<String>,
}

#[derive(clap::ArgEnum, Debug, PartialEq, Eq, Clone, Copy)]
enum Target {
    Current,
    Windows,
    Linux,
}

#[derive(Args, Debug, PartialEq, Eq)]
struct CheckArgs {
    /// The main action the compiler should take
    input: String,

    /// Additional include paths, separated by ;
    #[clap(long)]
    include_paths: Option<String>,

    /// Return diagnostics of the input file, and all files imported by it - recursively
    #[clap(long)]
    diagnostics: bool,

    /// Return the hover info for a given index, in the given input file
    #[clap(long)]
    hover_info: Option<usize>,

    /// Return the hover info for a given index, in the given input file
    #[clap(long)]
    goto_def: Option<usize>,
}

pub fn start_cli() {
    let args = Args::parse();

    match args.action {
        Action::Build(args) | Action::Run(args) => match get_file_path(&args.input) {
            Ok(source_file) => {
                let build_options = BuildOptions {
                    source_file,
                    target_platform: current_target_platform(),
                    opt_level: OptLevel::Debug,
                    verbose: args.verbose,
                    diagnostic_options: DiagnosticOptions::Emit {
                        no_color: args.no_color,
                    },
                    codegen_options: CodegenOptions::Skip,
                    include_paths: get_include_paths(&args.include_paths),
                };

                start_workspace("__TEST__".to_string(), build_options);
            }
            Err(e) => print_err(&e),
        },
        Action::Check(args) => run_check(args),
    };

    // let (run, args) = match cli.action {
    //     Action::Build(args) => (false, args),
    //     Action::Run(args) => (true, args),
    //     Action::Check(args) => {
    //         run_check(args);
    //         return;
    //     }
    // };

    // match get_file_path(&args.input) {
    //     Ok(source_file) => {
    //         let diagnostic_options = DiagnosticOptions::Emit {
    //             no_color: args.no_color,
    //         };

    //         let build_options = BuildOptions {
    //             source_file,
    //             target_platform: current_target_platform(),
    //             opt_level: if args.release {
    //                 OptLevel::Release
    //             } else {
    //                 OptLevel::Debug
    //             },
    //             verbose: args.verbose,
    //             diagnostic_options,
    //             codegen_options: CodegenOptions::Skip,
    //             include_paths: get_include_paths(&args.include_paths),
    //         };

    //         start_workspace("__TEST__".to_string(), build_options);
    //     }
    //     Err(e) => print_err(&e),
    // }
}

fn run_check(args: CheckArgs) {
    match get_file_path(&args.input) {
        Ok(source_file) => {
            let build_options = BuildOptions {
                source_file,
                target_platform: current_target_platform(),
                opt_level: OptLevel::Debug,
                verbose: false,
                diagnostic_options: DiagnosticOptions::DontEmit,
                codegen_options: CodegenOptions::Skip,
                include_paths: get_include_paths(&args.include_paths),
            };

            let (workspace, tycx, typed_ast) =
                start_workspace("__TEST__".to_string(), build_options);

            if args.diagnostics {
                ide::diagnostics(&workspace, tycx.as_ref(), typed_ast.as_ref());
            } else if let Some(offset) = args.hover_info {
                ide::hover_info(&workspace, tycx.as_ref(), offset);
            } else if let Some(offset) = args.goto_def {
                ide::goto_definition(&workspace, tycx.as_ref(), offset);
            }
        }
        Err(e) => print_err(&e),
    }
}

fn get_file_path(input_file: &str) -> Result<PathBuf, String> {
    let path = Path::new(input_file);

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
