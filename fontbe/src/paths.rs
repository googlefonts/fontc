//! Where to emit BE work when written to filesystem

use std::path::{Path, PathBuf};

use fontdrasil::paths::string_to_filename;

use crate::orchestration::WorkId;

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

    pub fn glyph_dir(&self) -> &Path {
        &self.glyph_dir
    }

    fn glyph_glyf_file(&self, name: &str) -> PathBuf {
        self.glyph_dir.join(string_to_filename(name, ".glyf"))
    }

    fn glyph_gvar_file(&self, name: &str) -> PathBuf {
        self.glyph_dir.join(string_to_filename(name, ".gvar"))
    }

    fn kern_fragment_file(&self, segment: usize) -> PathBuf {
        self.build_dir.join(string_to_filename(
            &format!("kern_fragment_{segment}"),
            ".bin",
        ))
    }

    pub fn target_file(&self, id: &WorkId) -> PathBuf {
        match id {
            WorkId::Features => self.build_dir.join("features.marker"),
            WorkId::FeaturesAst => self.build_dir.join("features_ast.bin"),
            WorkId::GlyfFragment(name) => self.glyph_glyf_file(name.as_str()),
            WorkId::GvarFragment(name) => self.glyph_gvar_file(name.as_str()),
            WorkId::Avar => self.build_dir.join("avar.table"),
            WorkId::Colr => self.build_dir.join("colr.table"),
            WorkId::Cpal => self.build_dir.join("cpal.table"),
            WorkId::Gasp => self.build_dir.join("gasp.table"),
            WorkId::Glyf => self.build_dir.join("glyf.table"),
            WorkId::Gsub => self.build_dir.join("gsub.table"),
            WorkId::Gpos => self.build_dir.join("gpos.table"),
            WorkId::Gdef => self.build_dir.join("gdef.table"),
            WorkId::Gvar => self.build_dir.join("gvar.table"),
            WorkId::Loca => self.build_dir.join("loca.table"),
            WorkId::LocaFormat => self.build_dir.join("loca.format"),
            WorkId::Cmap => self.build_dir.join("cmap.table"),
            WorkId::Fvar => self.build_dir.join("fvar.table"),
            WorkId::Head => self.build_dir.join("head.table"),
            WorkId::Hhea => self.build_dir.join("hhea.table"),
            WorkId::Hmtx => self.build_dir.join("hmtx.table"),
            WorkId::Hvar => self.build_dir.join("hvar.table"),
            WorkId::GatherIrKerning => self.build_dir.join("kern_scatter.bin"),
            WorkId::KernFragment(segment) => self.kern_fragment_file(*segment),
            WorkId::GatherBeKerning => self.build_dir.join("kern_gather.bin"),
            WorkId::Marks => self.build_dir.join("marks.bin"),
            WorkId::Maxp => self.build_dir.join("maxp.table"),
            WorkId::Mvar => self.build_dir.join("mvar.table"),
            WorkId::Name => self.build_dir.join("name.table"),
            WorkId::Os2 => self.build_dir.join("os2.table"),
            WorkId::Post => self.build_dir.join("post.table"),
            WorkId::Stat => self.build_dir.join("stat.table"),
            WorkId::Meta => self.build_dir.join("meta.table"),
            WorkId::Vhea => self.build_dir.join("vhea.table"),
            WorkId::Vmtx => self.build_dir.join("vmtx.table"),
            WorkId::Vvar => self.build_dir.join("vvar.table"),
            WorkId::ExtraFeaTables => self.build_dir.join("extra_tables.bin"),
            // This will be written out when we're emimtting IR, because while it isn't
            // *intermediate* work, it is work none the less. But the "official"
            // output file is written by write_font_file.
            WorkId::Font => self.build_dir.join("font.ttf"),
        }
    }
}
