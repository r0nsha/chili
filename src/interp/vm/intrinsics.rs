use std::path::PathBuf;

use path_absolutize::Absolutize;
use ustr::ustr;

use super::{
    value::{Aggregate, IntrinsicFunction},
    VM,
};
use crate::{
    common::{
        build_options::{BuildOptions, CodegenOptions, OptimizationLevel},
        target::TargetPlatform,
    },
    interp::{
        vm::value::Value,
        workspace::{BuildTargetValue, OptimizationLevelValue, WorkspaceValue},
    },
    types::Type,
};

impl<'vm> VM<'vm> {
    pub fn dispatch_intrinsic(&mut self, intrinsic: IntrinsicFunction) {
        match intrinsic {
            IntrinsicFunction::StartWorkspace => {
                let value = self.stack.pop();
                let workspace_value = WorkspaceValue::from(&value);

                let source_file = PathBuf::from(workspace_value.build_options.input_file)
                    .absolutize_from(self.interp.build_options.root_dir())
                    .unwrap()
                    .to_path_buf();

                let output_file = PathBuf::from(workspace_value.build_options.output_file)
                    .absolutize_from(self.interp.build_options.root_dir())
                    .unwrap()
                    .to_path_buf();

                let build_options = BuildOptions {
                    source_file,
                    output_file: Some(output_file),
                    target_platform: match &workspace_value.build_options.target {
                        BuildTargetValue::Auto => TargetPlatform::current().unwrap(),
                        BuildTargetValue::Linux => TargetPlatform::LinuxAmd64,
                        BuildTargetValue::Windows => TargetPlatform::WindowsAmd64,
                    },
                    optimization_level: match &workspace_value.build_options.optimization_level {
                        OptimizationLevelValue::Debug => OptimizationLevel::Debug,
                        OptimizationLevelValue::Release => OptimizationLevel::Release,
                    },
                    emit_times: self.interp.build_options.emit_times,
                    emit_hir: self.interp.build_options.emit_hir,
                    emit_bytecode: self.interp.build_options.emit_bytecode,
                    diagnostic_options: self.interp.build_options.diagnostic_options.clone(),
                    codegen_options: CodegenOptions::Codegen {
                        emit_llvm_ir: self.interp.build_options.codegen_options.emit_llvm_ir(),
                    },
                    include_paths: vec![],
                    check_mode: false,
                };

                let result =
                    crate::driver::start_workspace(workspace_value.name.to_string(), build_options);

                let (output_file_str, ok) = if let Some(output_file) = &result.output_file {
                    (ustr(output_file.to_str().unwrap()), true)
                } else {
                    (ustr(""), false)
                };

                self.stack.push(Value::Aggregate(Aggregate {
                    elements: vec![Value::from(output_file_str), Value::Bool(ok)],
                    ty: Type::Tuple(vec![Type::str(), Type::Bool]),
                }));

                self.next();
            }
        }
    }
}
