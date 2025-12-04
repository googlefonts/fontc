//! Where does IR go if we elect to write it to the filesystem?

use std::path::{Path, PathBuf};

use fontdrasil::{coords::NormalizedLocation, paths::string_to_filename};

use crate::orchestration::WorkId;

#[derive(Debug, Clone)]
pub struct Paths;

impl Paths {
    pub fn anchor_ir_dir(dir: &Path) -> PathBuf {
        dir.join("anchor_ir/")
    }

    fn anchor_ir_file(dir: &Path, name: &str) -> PathBuf {
        Paths::anchor_ir_dir(dir).join(string_to_filename(name, ".yml"))
    }

    pub fn glyph_ir_dir(dir: &Path) -> PathBuf {
        dir.join("glyph_ir/")
    }

    fn glyph_ir_file(dir: &Path, name: &str) -> PathBuf {
        Paths::glyph_ir_dir(dir).join(string_to_filename(name, ".yml"))
    }

    fn kern_ir_file(dir: &Path, location: &NormalizedLocation) -> PathBuf {
        let filename = "kern_".to_string()
            + &location
                .iter()
                .map(|(tag, pos)| format!("{tag}_{:.2}", pos.to_f64()))
                .collect::<Vec<_>>()
                .join("_")
            + ".yml";
        dir.join(filename)
    }

    pub fn target_file(dir: &Path, id: &WorkId) -> PathBuf {
        match id {
            WorkId::Anchor(name) => Paths::anchor_ir_file(dir, name.as_str()),
            WorkId::StaticMetadata => dir.join("static_metadata.yml"),
            WorkId::PreliminaryGlyphOrder => dir.join("glyph_order.preliminary.yml"),
            WorkId::GlyphOrder => dir.join("glyph_order.yml"),
            WorkId::GlobalMetrics => dir.join("global_metrics.yml"),
            WorkId::Glyph(name) => Paths::glyph_ir_file(dir, name.as_str()),
            WorkId::Features => dir.join("features.yml"),
            WorkId::KerningGroups => dir.join("kern_groups.yml"),
            WorkId::KernInstance(location) => Paths::kern_ir_file(dir, location),
            WorkId::ColorPalettes => dir.join("colors.yml"),
            WorkId::PaintGraph => dir.join("paint_graph.yml"),
        }
    }
}
