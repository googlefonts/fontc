//! Where to emit BE work when written to filesystem

use std::path::{Path, PathBuf};

use fontdrasil::paths::string_to_filename;

use crate::orchestration::WorkId;

#[derive(Debug, Clone)]
pub struct Paths;

impl Paths {
    pub fn debug_dir(dir: &Path) -> PathBuf {
        dir.join("debug")
    }

    pub fn glyph_dir(dir: &Path) -> PathBuf {
        dir.join("glyphs")
    }

    fn glyph_glyf_file(dir: &Path, name: &str) -> PathBuf {
        Paths::glyph_dir(dir).join(string_to_filename(name, ".glyf"))
    }

    fn glyph_gvar_file(dir: &Path, name: &str) -> PathBuf {
        Paths::glyph_dir(dir).join(string_to_filename(name, ".gvar"))
    }

    fn kern_fragment_file(dir: &Path, segment: usize) -> PathBuf {
        dir.join(string_to_filename(
            &format!("kern_fragment_{segment}"),
            ".bin",
        ))
    }

    pub fn target_file(dir: &Path, id: &WorkId) -> PathBuf {
        match id {
            WorkId::Features => dir.join("features.marker"),
            WorkId::FeaturesAst => dir.join("features_ast.bin"),
            WorkId::GlyfFragment(name) => Paths::glyph_glyf_file(dir, name.as_str()),
            WorkId::GvarFragment(name) => Paths::glyph_gvar_file(dir, name.as_str()),
            WorkId::Avar => dir.join("avar.table"),
            WorkId::Colr => dir.join("colr.table"),
            WorkId::Cpal => dir.join("cpal.table"),
            WorkId::Gasp => dir.join("gasp.table"),
            WorkId::Glyf => dir.join("glyf.table"),
            WorkId::Gsub => dir.join("gsub.table"),
            WorkId::Gpos => dir.join("gpos.table"),
            WorkId::Gdef => dir.join("gdef.table"),
            WorkId::Gvar => dir.join("gvar.table"),
            WorkId::Loca => dir.join("loca.table"),
            WorkId::LocaFormat => dir.join("loca.format"),
            WorkId::Cmap => dir.join("cmap.table"),
            WorkId::Fvar => dir.join("fvar.table"),
            WorkId::Head => dir.join("head.table"),
            WorkId::Hhea => dir.join("hhea.table"),
            WorkId::Hmtx => dir.join("hmtx.table"),
            WorkId::Hvar => dir.join("hvar.table"),
            WorkId::GatherIrKerning => dir.join("kern_scatter.bin"),
            WorkId::KernFragment(segment) => Paths::kern_fragment_file(dir, *segment),
            WorkId::GatherBeKerning => dir.join("kern_gather.bin"),
            WorkId::Marks => dir.join("marks.bin"),
            WorkId::Maxp => dir.join("maxp.table"),
            WorkId::Mvar => dir.join("mvar.table"),
            WorkId::Name => dir.join("name.table"),
            WorkId::Os2 => dir.join("os2.table"),
            WorkId::Post => dir.join("post.table"),
            WorkId::Stat => dir.join("stat.table"),
            WorkId::Meta => dir.join("meta.table"),
            WorkId::Vhea => dir.join("vhea.table"),
            WorkId::Vmtx => dir.join("vmtx.table"),
            WorkId::Vvar => dir.join("vvar.table"),
            WorkId::ExtraFeaTables => dir.join("extra_tables.bin"),
            WorkId::Font => dir.join("font.ttf"),
        }
    }
}
