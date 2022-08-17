use super::{value::IntrinsicFunction, VM};
use crate::{
    common::{
        build_options::{BuildOptions, CodegenOptions, OptimizationLevel},
        target::TargetPlatform,
    },
    interp::{
        vm::value::{Buffer, Value},
        workspace::{BuildTargetValue, OptimizationLevelValue, WorkspaceValue},
    },
    types::Type,
};
use path_absolutize::Absolutize;
use std::{
    ffi::{CStr, CString},
    path::PathBuf,
};

impl<'vm> VM<'vm> {
    pub fn dispatch_intrinsic(&mut self, intrinsic: IntrinsicFunction) {
        match intrinsic {
            IntrinsicFunction::StartWorkspace => {
                let value = self.stack.pop();
                let workspace_value = WorkspaceValue::from(&value);

                let source_file = PathBuf::from(workspace_value.build_options.input_file.as_str())
                    .absolutize_from(self.interp.build_options.root_dir())
                    .unwrap()
                    .to_path_buf();

                let output_file = PathBuf::from(workspace_value.build_options.output_file.as_str())
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

                let result = crate::driver::start_workspace(workspace_value.name.to_string(), build_options);

                let (output_file_bytes, ok) = if let Some(output_file) = &result.output_file {
                    // TODO: Remove null terminator after implementing printing/formatting:
                    // CString::new(output_file.to_str().unwrap())
                    //     .unwrap()
                    //     .into_bytes_with_nul();
                    // let output_file_with_nul = output_file.to_str().unwrap().to_string();
                    // output_file_with_nul.push('\0');
                    // output_file_with_nul.push('\0');
                    (
                        self.bump.alloc_slice_copy(
                            &CString::new(output_file.to_str().unwrap())
                                .unwrap()
                                .into_bytes_with_nul(),
                        ),
                        true,
                    )
                } else {
                    (self.bump.alloc_slice_copy(b"\0"), false)
                };

                let result_type = Type::Tuple(vec![Type::str_pointer(), Type::Bool]);

                let result_value = Value::Buffer(Buffer::from_values(
                    [
                        Value::Buffer(Buffer::from_str_bytes(output_file_bytes)),
                        Value::Bool(ok),
                    ],
                    result_type,
                ));

                self.stack.push(result_value);
            }
        }
    }
}
