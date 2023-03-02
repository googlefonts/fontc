//! Where to emit BE work when written to filesystem

use std::path::{Path, PathBuf};

use fontdrasil::paths::glyph_file;

use crate::orchestration::WorkId;

#[derive(Debug, Clone)]
pub struct Paths {
    build_dir: PathBuf,
    glyph_dir: PathBuf,
    debug_dir: PathBuf,
}

impl Paths {
    pub fn new(build_dir: &Path) -> Paths {
        let glyph_dir = build_dir.join("glyphs");
        let debug_dir = build_dir.join("debug");
        let build_dir = build_dir.to_path_buf();
        Paths {
            build_dir,
            glyph_dir,
            debug_dir,
        }
    }

    pub fn build_dir(&self) -> &Path {
        &self.build_dir
    }

    pub fn debug_dir(&self) -> &Path {
        &self.debug_dir
    }

    fn glyph_file(&self, name: &str) -> PathBuf {
        self.glyph_dir.join(glyph_file(name, ".ttf"))
    }

    pub fn target_file(&self, id: &WorkId) -> PathBuf {
        match id {
            WorkId::Features => self.build_dir.join("features.ttf"),
            WorkId::Glyph(name) => self.glyph_file(name.as_str()),
            WorkId::GlyphMerge => self.build_dir.join("all_glyphs.ttf"),
            WorkId::FinalMerge => self.build_dir.join("font.ttf"),
        }
    }
}
