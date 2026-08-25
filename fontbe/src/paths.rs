//! Where to emit BE work when written to filesystem

use std::path::{Path, PathBuf};

#[derive(Debug, Clone)]
pub struct Paths;

impl Paths {
    pub fn debug_dir(dir: &Path) -> PathBuf {
        dir.join("debug")
    }
}
