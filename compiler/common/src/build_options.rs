use crate::target::TargetPlatform;
use std::path::Path;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BuildOptions {
    pub source_file: String,
    pub target_platform: TargetPlatform,
    pub run: bool,
}

impl BuildOptions {
    pub fn source_path(&self) -> &Path {
        Path::new(&self.source_file)
    }
}
