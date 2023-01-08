//! Where to emit BE work when written to filesystem

use std::path::{Path, PathBuf};

#[derive(Debug, Clone)]
pub struct Paths {
    build_dir: PathBuf,
}

impl Paths {
    pub fn new(build_dir: &Path) -> Paths {
        let build_dir = build_dir.to_path_buf();
        Paths { build_dir }
    }

    pub fn build_dir(&self) -> &Path {
        &self.build_dir
    }
}
