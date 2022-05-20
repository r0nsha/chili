use crate::target::TargetPlatform;
use std::path::Path;

#[derive(Debug, Clone)]
pub struct BuildOptions {
    pub source_file: String,
    pub target_platform: TargetPlatform,
    pub build_mode: BuildMode,
    pub run: bool,
    pub verbose: bool,
    pub emit_llvm_ir: bool,
    pub no_codegen: bool,
}

impl BuildOptions {
    pub fn source_path(&self) -> &Path {
        Path::new(&self.source_file)
    }
}

#[derive(Debug, Clone)]
pub enum BuildMode {
    Debug,
    Release,
}

impl BuildMode {
    pub fn is_debug(&self) -> bool {
        matches!(self, BuildMode::Debug)
    }

    pub fn is_release(&self) -> bool {
        matches!(self, BuildMode::Release)
    }
}
