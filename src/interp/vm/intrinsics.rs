use std::path::PathBuf;

use path_absolutize::Absolutize;

use super::{value::IntrinsicFunction, VM};
use crate::common::{
    build_options::{BuildOptions, CodegenOptions, EnabledCodegenOptions, OptLevel},
    target::TargetPlatform,
};

impl<'vm> VM<'vm> {
    pub fn dispatch_intrinsic(&mut self, intrinsic: IntrinsicFunction) {
        match intrinsic {
            IntrinsicFunction::StartWorkspace => {
                // TODO
                let source_file = PathBuf::from("src/main.chili")
                    .absolutize_from(self.interp.build_options.root_dir())
                    .unwrap()
                    .to_path_buf();

                let build_options = BuildOptions {
                    source_file,
                    target_platform: TargetPlatform::current().unwrap(), // TODO
                    opt_level: OptLevel::Debug,                          // TODO
                    verbose: self.interp.build_options.verbose,
                    diagnostic_options: self.interp.build_options.diagnostic_options.clone(),
                    codegen_options: CodegenOptions::Codegen(EnabledCodegenOptions {
                        emit_llvm_ir: false,
                        run_when_done: false, // TODO
                    }),
                    include_paths: self.interp.build_options.include_paths.clone(),
                };

                println!("{}", self.interp.build_options.source_file.display());
                println!("{}", build_options.source_file.display());

                crate::driver::start_workspace(
                    "__TEST__".to_string(), // TODO
                    build_options,
                );
            }
        }
    }
}
