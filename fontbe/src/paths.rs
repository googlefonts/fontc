//! Where to emit BE work when written to filesystem

use std::path::{Path, PathBuf};

use fontdrasil::paths::glyph_file;

use crate::orchestration::WorkIdentifier;

#[derive(Debug, Clone)]
pub struct Paths {
    build_dir: PathBuf,
    glyph_dir: PathBuf,
}

impl Paths {
    pub fn new(build_dir: &Path) -> Paths {
        let glyph_dir = build_dir.join("glyphs");
        let build_dir = build_dir.to_path_buf();
        Paths {
            build_dir,
            glyph_dir,
        }
    }

    pub fn build_dir(&self) -> &Path {
        &self.build_dir
    }

    fn glyph_file(&self, name: &str) -> PathBuf {
        self.glyph_dir.join(glyph_file(name, ".ttf"))
    }

    pub fn target_file(&self, id: &WorkIdentifier) -> PathBuf {
        match id {
            WorkIdentifier::Features => self.build_dir.join("features.ttf"),
            WorkIdentifier::Glyph(name) => self.glyph_file(name.as_str()),
            WorkIdentifier::GlyphMerge => self.build_dir.join("all_glyphs.ttf"),
            WorkIdentifier::FinalMerge => self.build_dir.join("font.ttf"),
        }
    }
}
