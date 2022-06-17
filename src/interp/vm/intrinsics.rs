use std::path::PathBuf;

use path_absolutize::Absolutize;

use super::{value::IntrinsicFunction, VM};
use crate::{
    common::{
        build_options::{BuildOptions, CodegenOptions, EnabledCodegenOptions, OptLevel},
        target::TargetPlatform,
    },
    interp::{vm::value::Value, workspace::WorkspaceValue},
};

impl<'vm> VM<'vm> {
    pub fn dispatch_intrinsic(&mut self, intrinsic: IntrinsicFunction) {
        match intrinsic {
            IntrinsicFunction::StartWorkspace => {
                let value = self.stack.pop();
                let workspace = WorkspaceValue::from(&value);

                dbg!(&workspace);

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
                        run_when_done: true, // TODO
                    }),
                    include_paths: vec![],
                };

                crate::driver::start_workspace(
                    "__TEST_____".to_string(), // TODO
                    build_options,
                );

                self.stack.push(Value::unit());
                self.next();
            }
        }
    }
}
