//! Where does IR go if we elect to write it to the filesystem?

use std::path::{Path, PathBuf};

use fontdrasil::{coords::NormalizedLocation, paths::string_to_filename};

use crate::orchestration::WorkId;

#[derive(Debug, Clone)]
pub struct Paths {
    build_dir: PathBuf,
    anchor_ir_dir: PathBuf,
    glyph_ir_dir: PathBuf,
    ir_input_file: PathBuf,
}

impl Paths {
    pub fn new(build_dir: &Path) -> Paths {
        let build_dir = build_dir.to_path_buf();
        let anchor_ir_dir = build_dir.join("anchor_ir");
        let glyph_ir_dir = build_dir.join("glyph_ir");
        let ir_input_file = build_dir.join("irinput.yml");
        Paths {
            build_dir,
            anchor_ir_dir,
            glyph_ir_dir,
            ir_input_file,
        }
    }

    pub fn build_dir(&self) -> &Path {
        &self.build_dir
    }

    pub fn anchor_ir_dir(&self) -> &Path {
        &self.anchor_ir_dir
    }

    pub fn glyph_ir_dir(&self) -> &Path {
        &self.glyph_ir_dir
    }

    pub fn ir_input_file(&self) -> &Path {
        &self.ir_input_file
    }

    fn anchor_ir_file(&self, name: &str) -> PathBuf {
        self.anchor_ir_dir.join(string_to_filename(name, ".yml"))
    }

    fn glyph_ir_file(&self, name: &str) -> PathBuf {
        self.glyph_ir_dir.join(string_to_filename(name, ".yml"))
    }

    fn kern_ir_file(&self, location: &NormalizedLocation) -> PathBuf {
        let filename = "kern_".to_string()
            + &location
                .iter()
                .map(|(tag, pos)| format!("{tag}_{:.2}", pos.to_f32()))
                .collect::<Vec<_>>()
                .join("_")
            + ".yml";
        self.build_dir.join(filename)
    }

    pub fn target_file(&self, id: &WorkId) -> PathBuf {
        match id {
            WorkId::Anchor(name) => self.anchor_ir_file(name.as_str()),
            WorkId::StaticMetadata => self.build_dir.join("static_metadata.yml"),
            WorkId::PreliminaryGlyphOrder => self.build_dir.join("glyph_order.preliminary.yml"),
            WorkId::GlyphOrder => self.build_dir.join("glyph_order.yml"),
            WorkId::GlobalMetrics => self.build_dir.join("global_metrics.yml"),
            WorkId::Glyph(name) => self.glyph_ir_file(name.as_str()),
            WorkId::GlyphIrDelete(name) => {
                self.build_dir.join(format!("delete-{}.yml", name.as_str()))
            }
            WorkId::Features => self.build_dir.join("features.yml"),
            WorkId::KerningGroups => self.build_dir.join("kern_groups.yml"),
            WorkId::KernInstance(location) => self.kern_ir_file(location),
        }
    }
}
