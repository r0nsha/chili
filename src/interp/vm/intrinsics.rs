use std::path::PathBuf;

use path_absolutize::Absolutize;

use super::{value::IntrinsicFunction, VM};
use crate::{
    common::{
        build_options::{BuildOptions, CodegenOptions, EnabledCodegenOptions, OptLevel},
        target::TargetPlatform,
    },
    interp::{
        vm::value::Value,
        workspace::{BuildTargetValue, OptLevelValue, WorkspaceValue},
    },
};

impl<'vm> VM<'vm> {
    pub fn dispatch_intrinsic(&mut self, intrinsic: IntrinsicFunction) {
        match intrinsic {
            IntrinsicFunction::StartWorkspace => {
                let value = self.stack.pop();
                let workspace = WorkspaceValue::from(&value);

                let source_file = PathBuf::from(workspace.build_options.input_file)
                    .absolutize_from(self.interp.build_options.root_dir())
                    .unwrap()
                    .to_path_buf();

                let output_file = PathBuf::from(workspace.build_options.output_file)
                    .absolutize_from(self.interp.build_options.root_dir())
                    .unwrap()
                    .to_path_buf();

                let build_options = BuildOptions {
                    source_file,
                    output_file: Some(output_file),
                    target_platform: match &workspace.build_options.target {
                        BuildTargetValue::Auto => TargetPlatform::current().unwrap(),
                        BuildTargetValue::Linux => TargetPlatform::LinuxAmd64,
                        BuildTargetValue::Windows => TargetPlatform::WindowsAmd64,
                    },
                    opt_level: match &workspace.build_options.opt_level {
                        OptLevelValue::Debug => OptLevel::Debug,
                        OptLevelValue::Release => OptLevel::Release,
                    },
                    verbose: self.interp.build_options.verbose,
                    diagnostic_options: self.interp.build_options.diagnostic_options.clone(),
                    codegen_options: CodegenOptions::Codegen(EnabledCodegenOptions {
                        emit_llvm_ir: match &self.interp.build_options.codegen_options {
                            CodegenOptions::Codegen(o) => o.emit_llvm_ir,
                            CodegenOptions::Skip => false,
                        },
                        run_executable: workspace.build_options.run_executable,
                    }),
                    include_paths: vec![],
                    check_mode: false,
                };

                crate::driver::start_workspace(workspace.name.to_string(), build_options);

                self.stack.push(Value::unit());
                self.next();
            }
        }
    }
}
