use chilic_ir_gen::ir_gen::IrGen;
use chilic_llvm::codegen;
use codespan_reporting::{diagnostic::Diagnostic, files::SimpleFiles};
use colored::Colorize;

use chilic_error::emit_single_diagnostic;
use common::{build_options::BuildOptions, Stopwatch};
use num_format::{Locale, ToFormattedString};
use path_absolutize::*;
use ustr::UstrSet;

pub fn do_build(build_options: BuildOptions) {
    println!();

    let mut all_sw = Stopwatch::start_new("time");
    let mut files = SimpleFiles::<String, String>::new();

    let source_path = build_options.source_path();
    if !source_path.exists() {
        emit_single_diagnostic(
            &files,
            Diagnostic::error().with_message(format!(
                "file `{}` doesn't exist",
                source_path.display()
            )),
        );
        return;
    }

    let absolute_path = source_path.absolutize().unwrap();
    let root_dir = absolute_path.parent().unwrap();

    let root_dir =
        format!("{}{}", root_dir.display(), std::path::MAIN_SEPARATOR);

    let (items, root_file_id) = {
        let mut ir_gen = IrGen {
            files: &mut files,
            root_dir: root_dir.clone(),
            root_file_id: 0,
            already_parsed_modules: UstrSet::default(),
        };

        let items = match ir_gen.gen(build_options.source_file.clone()) {
            Ok(items) => items,
            Err(diagnostic) => {
                emit_single_diagnostic(&ir_gen.files, diagnostic);
                return;
            }
        };

        (items, ir_gen.root_file_id)
    };

    let sw = Stopwatch::start_new("lower");

    let ir = match chilic_ir_gen::gen_structured_ir(&items, files.clone()) {
        Ok(ir) => ir,
        Err(diagnostic) => {
            emit_single_diagnostic(&files, diagnostic);
            return;
        }
    };

    sw.print();

    // ir.print();

    let sw = Stopwatch::start_new("analyze");

    let ir = match chilic_check::check_ir(&files, &build_options, ir) {
        Ok(ir) => ir,
        Err(diagnostic) => {
            emit_single_diagnostic(&files, diagnostic);
            return;
        }
    };

    sw.print();

    // ir.print();

    codegen(&build_options, &ir);

    all_sw.stop();

    print_stats(&files, root_file_id, all_sw.elapsed().as_millis());
}

fn print_stats(
    files: &SimpleFiles<String, String>,
    root_file_id: usize,
    elapsed_ms: u128,
) {
    let file_line_count =
        files.get(root_file_id).unwrap().source().lines().count();
    println!();
    println!(
        "{}\t{}",
        "span:".cyan().bold(),
        file_line_count.to_formatted_string(&Locale::en)
    );
    println!("{}\t{}m", "time:".cyan().bold(), elapsed_ms);
    println!();
}
