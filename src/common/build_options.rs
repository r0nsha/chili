use super::target::TargetPlatform;
use std::path::{Path, PathBuf};

#[derive(Debug, Clone)]
pub struct BuildOptions {
    /// The root source file input from the user
    pub source_file: PathBuf,

    /// Optional output file path
    pub output_file: Option<PathBuf>,

    /// The target platform, including os and arch.
    /// If this is None, the target platform will be the current one, and codegen is skipped
    pub target_platform: TargetPlatform,

    /// The overall codegen optimization level
    pub optimization_level: OptimizationLevel,

    pub diagnostic_options: DiagnosticOptions,
    pub codegen_options: CodegenOptions,

    /// Additional search paths for imports
    pub include_paths: Vec<PathBuf>,

    /// Print timing information of every compiler pass
    pub emit_times: bool,

    /// Print the Hir into $CWD/hir.pretty.chl
    pub emit_hir: bool,

    /// Print the bytecode into $CWD/vm.out
    pub emit_bytecode: bool,

    /// Whether the workspace is running in check mode
    pub check_mode: bool,
}

impl BuildOptions {
    pub fn root_dir(&self) -> &Path {
        self.source_file.parent().unwrap()
    }

    pub fn need_entry_point_function(&self) -> bool {
        matches!(self.codegen_options, CodegenOptions::Codegen { .. })
            && matches!(
                self.target_platform,
                TargetPlatform::Windows386
                    | TargetPlatform::WindowsAmd64
                    | TargetPlatform::Linux386
                    | TargetPlatform::LinuxAmd64
                    | TargetPlatform::LinuxArm64
                    | TargetPlatform::DarwinAmd64
                    | TargetPlatform::DarwinArm64
                    | TargetPlatform::FreeBSD386
                    | TargetPlatform::FreeBSDAmd64
                    | TargetPlatform::EssenceAmd64
            )
    }

    pub fn start_function_name(&self) -> Option<&'static str> {
        self.need_entry_point_function()
            .then(|| match &self.target_platform {
                TargetPlatform::Windows386
                | TargetPlatform::WindowsAmd64
                | TargetPlatform::Linux386
                | TargetPlatform::LinuxAmd64
                | TargetPlatform::LinuxArm64
                | TargetPlatform::DarwinAmd64
                | TargetPlatform::DarwinArm64
                | TargetPlatform::FreeBSD386
                | TargetPlatform::FreeBSDAmd64
                | TargetPlatform::EssenceAmd64 => "main",

                p => panic!("unexpected TargetPlatform::{:?}", p),
            })
    }
}

#[derive(Debug, Clone, Copy)]
pub enum OptimizationLevel {
    Debug,
    Release,
}

impl OptimizationLevel {
    #[allow(unused)]
    pub fn is_debug(&self) -> bool {
        matches!(self, OptimizationLevel::Debug)
    }

    #[allow(unused)]
    pub fn is_release(&self) -> bool {
        matches!(self, OptimizationLevel::Release)
    }
}

#[derive(Debug, Clone)]
pub enum DiagnosticOptions {
    Emit { no_color: bool },
    DontEmit,
}

#[derive(Debug, Clone)]
pub enum CodegenOptions {
    Codegen { emit_llvm_ir: bool },
    Skip { emit_llvm_ir: bool },
}

impl CodegenOptions {
    pub(crate) fn emit_llvm_ir(&self) -> bool {
        match self {
            CodegenOptions::Codegen { emit_llvm_ir } => *emit_llvm_ir,
            CodegenOptions::Skip { emit_llvm_ir } => *emit_llvm_ir,
        }
    }
}
