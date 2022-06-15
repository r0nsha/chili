use crate::target::TargetPlatform;
use std::path::{Path, PathBuf};

#[derive(Debug, Clone)]
pub struct BuildOptions {
    /// The root source file input from the user
    pub source_file: PathBuf,

    /// The target platform, including os and arch
    pub target_platform: TargetPlatform,

    /// The overall codegen optimization level
    pub opt_level: OptLevel,

    /// Prints information verbosely, for example: timings
    pub verbose: bool,

    pub diagnostic_options: DiagnosticOptions,

    pub codegen_options: CodegenOptions,

    /// Additional search paths for imports
    pub include_paths: Vec<PathBuf>,
}

impl BuildOptions {
    pub fn source_path(&self) -> &Path {
        Path::new(&self.source_file)
    }
}

#[derive(Debug, Clone)]
pub enum OptLevel {
    Debug,
    Release,
}

impl OptLevel {
    pub fn is_debug(&self) -> bool {
        matches!(self, OptLevel::Debug)
    }

    pub fn is_release(&self) -> bool {
        matches!(self, OptLevel::Release)
    }
}

#[derive(Debug, Clone)]
pub enum DiagnosticOptions {
    Emit { no_color: bool },
    DontEmit,
}

#[derive(Debug, Clone)]
pub enum CodegenOptions {
    Enabled(EnabledCodegenOptions),
    Skip,
}

#[derive(Debug, Clone)]
pub struct EnabledCodegenOptions {
    pub emit_llvm_ir: bool,
    pub run_when_done: bool,
}
