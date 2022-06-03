use crate::build::start_workspace;
use clap::*;
use colored::Colorize;
use common::{
    build_options::{BuildMode, BuildOptions},
    target::TargetPlatform,
};
use std::{
    env,
    path::{Path, PathBuf},
};

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

    /// Skip the code generation phase
    #[clap(long)]
    no_color: bool,

    /// Specify the target platform. Important: This flag is a placeholder until workspace-based configuration is implemented
    #[clap(arg_enum, default_value_t = Target::Current)]
    target: Target,
}

#[derive(clap::ArgEnum, Debug, PartialEq, Eq, Clone, Copy)]
enum Target {
    Current,
    Windows,
    Linux,
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
                source_file: PathBuf::from(file),
                target_platform: match args.target {
                    Target::Current => get_current_target_platform(),
                    Target::Windows => TargetPlatform::WindowsAmd64,
                    Target::Linux => TargetPlatform::LinuxAmd64,
                },
                build_mode: if args.release {
                    BuildMode::Release
                } else {
                    BuildMode::Debug
                },
                run,
                verbose: args.verbose,
                emit_llvm_ir: args.emit_llvm,
                no_codegen: args.no_codegen,
                no_color: args.no_color,
            };

            start_workspace(build_options);
        }
        Err(e) => print_err(&e),
    }
}

fn get_current_target_platform() -> TargetPlatform {
    match env::consts::OS {
        "linux" => TargetPlatform::LinuxAmd64,
        "windows" => TargetPlatform::WindowsAmd64,
        os => {
            print_err(&format!("targeting unsupported platform: {}", os));
            std::process::exit(1);
        }
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
