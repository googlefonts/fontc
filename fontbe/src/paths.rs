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

    pub fn glyph_dir(&self) -> &Path {
        &self.glyph_dir
    }

    fn glyph_glyf_file(&self, name: &str) -> PathBuf {
        self.glyph_dir.join(glyph_file(name, ".glyf"))
    }

    fn glyph_gvar_file(&self, name: &str) -> PathBuf {
        self.glyph_dir.join(glyph_file(name, ".gvar"))
    }

    pub fn target_file(&self, id: &WorkId) -> PathBuf {
        match id {
            WorkId::Features => self.build_dir.join("features.ttf"),
            WorkId::GlyfFragment(name) => self.glyph_glyf_file(name.as_str()),
            WorkId::GvarFragment(name) => self.glyph_gvar_file(name.as_str()),
            WorkId::Avar => self.build_dir.join("avar.table"),
            WorkId::Glyf => self.build_dir.join("glyf.table"),
            WorkId::Gvar => self.build_dir.join("gvar.table"),
            WorkId::Loca => self.build_dir.join("loca.table"),
            WorkId::Cmap => self.build_dir.join("cmap.table"),
            WorkId::Fvar => self.build_dir.join("fvar.table"),
            WorkId::Head => self.build_dir.join("head.table"),
            WorkId::Hhea => self.build_dir.join("hhea.table"),
            WorkId::Hmtx => self.build_dir.join("hmtx.table"),
            WorkId::Maxp => self.build_dir.join("maxp.table"),
            WorkId::Name => self.build_dir.join("name.table"),
            WorkId::Os2 => self.build_dir.join("os2.table"),
            WorkId::Post => self.build_dir.join("post.table"),
            WorkId::Font => self.build_dir.join("font.ttf"),
        }
    }
}
