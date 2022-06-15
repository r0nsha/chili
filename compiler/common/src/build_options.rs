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

    /// Whether to run the compiled file after codegen
    pub run: bool,

    /// Prints information verbosely, for example: timings
    pub verbose: bool,

    /// Whether to emit LLVM IR after codegen
    pub emit_llvm_ir: bool,

    /// Whether to emit collected diagnostics
    pub diagnostic_options: DiagnosticOptions,

    /// Disables the codegen phase
    pub no_codegen: bool,

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
    Enabled { no_color: bool },
    Disabled,
}
