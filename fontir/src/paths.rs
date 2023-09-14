//! Where does IR go if we elect to write it to the filesystem?

use std::path::{Path, PathBuf};

use fontdrasil::paths::glyph_file;

use crate::orchestration::WorkId;

#[derive(Debug, Clone)]
pub struct Paths {
    build_dir: PathBuf,
    glyph_ir_dir: PathBuf,
    ir_input_file: PathBuf,
}

impl Paths {
    pub fn new(build_dir: &Path) -> Paths {
        let build_dir = build_dir.to_path_buf();
        let glyph_ir_dir = build_dir.join("glyph_ir");
        let ir_input_file = build_dir.join("irinput.yml");
        Paths {
            build_dir,
            glyph_ir_dir,
            ir_input_file,
        }
    }

    pub fn build_dir(&self) -> &Path {
        &self.build_dir
    }

    pub fn glyph_ir_dir(&self) -> &Path {
        &self.glyph_ir_dir
    }

    pub fn ir_input_file(&self) -> &Path {
        &self.ir_input_file
    }

    fn glyph_ir_file(&self, name: &str) -> PathBuf {
        self.glyph_ir_dir.join(glyph_file(name, ".yml"))
    }

    pub fn target_file(&self, id: &WorkId) -> PathBuf {
        match id {
            WorkId::Anchors => self.build_dir.join("anchors.yml"),
            WorkId::StaticMetadata => self.build_dir.join("static_metadata.yml"),
            WorkId::PreliminaryGlyphOrder => self.build_dir.join("glyph_order.preliminary.yml"),
            WorkId::GlyphOrder => self.build_dir.join("glyph_order.yml"),
            WorkId::GlobalMetrics => self.build_dir.join("global_metrics.yml"),
            WorkId::Glyph(name) => self.glyph_ir_file(name.as_str()),
            WorkId::GlyphIrDelete(name) => {
                self.build_dir.join(format!("delete-{}.yml", name.as_str()))
            }
            WorkId::Features => self.build_dir.join("features.yml"),
            WorkId::Kerning => self.build_dir.join("kerning.yml"),
        }
    }
}
