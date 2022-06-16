use crate::target::TargetPlatform;
use std::path::{Path, PathBuf};

#[derive(Debug, Clone)]
pub struct BuildOptions {
    /// The root source file input from the user
    pub source_file: PathBuf,

    /// The target platform, including os and arch.
    /// If this is None, the target platform will be the current one, and codegen is skipped
    pub target_platform: Option<TargetPlatform>,

    /// The overall codegen optimization level
    pub opt_level: OptLevel,

    pub diagnostic_options: DiagnosticOptions,
    pub codegen_options: CodegenOptions,

    /// Additional search paths for imports
    pub include_paths: Vec<PathBuf>,

    /// Prints information verbosely, for example: timings
    pub verbose: bool,
}

impl BuildOptions {
    pub fn source_path(&self) -> &Path {
        Path::new(&self.source_file)
    }

    pub fn need_entry_point_function(&self) -> bool {
        self.target_platform.as_ref().map_or(false, |t| match t {
            TargetPlatform::Windows386
            | TargetPlatform::WindowsAmd64
            | TargetPlatform::Linux386
            | TargetPlatform::LinuxAmd64
            | TargetPlatform::LinuxArm64
            | TargetPlatform::DarwinAmd64
            | TargetPlatform::DarwinArm64
            | TargetPlatform::FreeBSD386
            | TargetPlatform::FreeBSDAmd64
            | TargetPlatform::EssenceAmd64 => true,

            TargetPlatform::FreestandingWasm32
            | TargetPlatform::JsWasm32
            | TargetPlatform::WasiWasm32 => false,
        })
    }

    pub fn entry_point_function_name(&self) -> Option<&'static str> {
        self.need_entry_point_function()
            .then(|| match self.target_platform.as_ref().unwrap() {
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
