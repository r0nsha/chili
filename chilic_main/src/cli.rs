use std::path::Path;

use clap::clap_app;
use colored::Colorize;
use common::{build_options::BuildOptions, target::TargetPlatform};

use crate::build::do_build;

pub fn start_cli() {
    let mut app = clap_app!(compiler =>
        (version: "0.0.1")
        (author: "Ron Shavit <r0nsh4v1t@gmail.com>")
        (@subcommand build =>
            (about: "compile a chili source file, as an executable.")
            (@arg input: +required "sets the input file to use.")
        )
        (@subcommand run =>
            (about: "same as the 'build' command, but also runs the compiled executable.")
            (@arg input: +required "sets the input file to use")
        )
    );

    let matches = app.clone().get_matches();

    let (run, subcommand_matches) = match matches.subcommand() {
        ("build", Some(matches)) => (false, matches),
        ("run", Some(matches)) => (true, matches),
        _ => {
            app.print_long_help().expect("failed printing help message");
            std::process::exit(0);
        }
    };

    let input_file = subcommand_matches.value_of("input").unwrap_or_else(|| {
        app.print_long_help().expect("failed printing help message");
        std::process::exit(0);
    });

    match get_file_path(input_file) {
        Ok(file) => do_build(BuildOptions {
            source_file: file.to_string(),
            target_platform: TargetPlatform::WindowsAmd64,
            run,
        }),
        Err(why) => print_err(&why),
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
