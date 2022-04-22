use crate::build::start_workspace;
use clap::*;
use colored::Colorize;
use common::{
    build_options::{BuildMode, BuildOptions},
    target::TargetPlatform,
};
use std::path::Path;

#[derive(Parser, Debug)]
#[clap(
    author,
    version,
    about,
    long_about = "Compiler for the Chili programming language"
)]
struct Cli {
    /// The main action the compiler should take
    #[clap(subcommand)]
    action: Action,
}

#[derive(clap::Subcommand, Debug, PartialEq, Eq)]
enum Action {
    /// Compile a chili source file, as an executable
    Build(Args),
    /// Same as `build`, but also runs the compiled executable
    Run(Args),
}

#[derive(Args, Debug, PartialEq, Eq)]
struct Args {
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
    emit_llvm: bool,

    /// Skip the code generation phase
    #[clap(long)]
    no_codegen: bool,
}

pub fn start_cli() {
    let cli = Cli::parse();

    let (run, args) = match cli.action {
        Action::Build(args) => (false, args),
        Action::Run(args) => (true, args),
    };

    match get_file_path(&args.input) {
        Ok(file) => {
            let build_options = BuildOptions {
                source_file: file.to_string(),
                target_platform: TargetPlatform::WindowsAmd64,
                build_mode: if args.release {
                    BuildMode::Release
                } else {
                    BuildMode::Debug
                },
                run,
                verbose: args.verbose,
                emit_llvm_ir: args.emit_llvm,
                no_codegen: args.no_codegen,
            };

            start_workspace(build_options);
        }
        Err(e) => print_err(&e),
    }
}

fn get_file_path(input_file: &str) -> Result<&str, String> {
    let path = Path::new(input_file);

    if !path.exists() {
        Err(format!("input file `{}` doesn't exist", input_file))
    } else if !path.is_file() {
        Err(format!("`{}` is not a file", input_file))
    } else {
        Ok(input_file)
    }
}

fn print_err(msg: &str) {
    println!("\n{} {}\n", "error:".red().bold(), msg.bold());
}
