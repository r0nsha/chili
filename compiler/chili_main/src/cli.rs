use crate::build::start_workspace;
use clap::*;
use colored::Colorize;
use common::{build_options::BuildOptions, target::TargetPlatform};
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

    /// Print trace information verbosely
    #[clap(short, long)]
    verbose: bool,
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
                run,
                verbose: args.verbose,
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
