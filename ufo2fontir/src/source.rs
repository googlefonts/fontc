use std::{
    collections::{BTreeMap, HashMap},
    io,
    path::{Path, PathBuf},
};

use fontir::{
    error::{Error, WorkError},
    filestate::FileStateSet,
    source::{Input, Source, Work},
};
use norad::designspace::DesignSpaceDocument;

pub struct DesignSpaceIrSource {
    designspace_file: PathBuf,
}

fn glif_files(ufo_dir: &Path) -> Result<BTreeMap<String, PathBuf>, Error> {
    let contents_file = ufo_dir.join("glyphs/contents.plist");
    if !contents_file.is_file() {
        return Err(Error::FileExpected(contents_file));
    }
    plist::from_file::<&Path, BTreeMap<String, PathBuf>>(&contents_file)
        .map_err(|e| Error::ParseError(contents_file, e.into()))
}

impl DesignSpaceIrSource {
    pub fn new(source: &Path) -> DesignSpaceIrSource {
        DesignSpaceIrSource {
            designspace_file: source.to_path_buf(),
        }
    }
}

impl Source for DesignSpaceIrSource {
    fn inputs(&self) -> Result<Input, Error> {
        let designspace = DesignSpaceDocument::load(&self.designspace_file)
            .map_err(|e| Error::UnableToLoadSource(Box::from(e)))?;
        let designspace_dir = self
            .designspace_file
            .parent()
            .expect("designspace file *must* be in a directory");

        // font info comes from the designspace and each ufo's fontinfo
        let mut font_info = FileStateSet::new();
        font_info.insert(&self.designspace_file)?;
        for source in designspace.sources.iter() {
            let font_info_file = designspace_dir
                .join(&source.filename)
                .join("fontinfo.plist");
            if !font_info_file.is_file() {
                return Err(Error::FileExpected(font_info_file));
            }
            font_info.insert(&font_info_file)?;
        }

        // glif filenames are not reversible so we need to read contents.plist to figure out groups
        // See https://github.com/unified-font-object/ufo-spec/issues/164.
        let mut glyphs: HashMap<String, FileStateSet> = HashMap::new();

        for source in designspace.sources {
            // Track files within each UFO
            // The UFO dir *must* exist since we were able to find fontinfo in it earlier
            let ufo_dir = designspace_dir.join(source.filename);

            let glif_files = glif_files(&ufo_dir)?;
            let glyph_dir = ufo_dir.join("glyphs");
            if !glyph_dir.is_dir() {
                return Err(Error::DirectoryExpected(glyph_dir));
            }
            for (glyph_name, glif_file) in glif_files {
                let glif_file = glyph_dir.join(&glif_file);
                if !glif_file.exists() {
                    return Err(Error::FileExpected(glif_file));
                }
                let glif_file = glif_file.clone();
                glyphs.entry(glyph_name).or_default().insert(&glif_file)?;
            }
        }

        Ok(Input { font_info, glyphs })
    }

    fn remove_glyph_ir(&self, glyph_name: &str) -> Result<(), io::Error> {
        todo!("Remove glyph IR for {}", glyph_name)
    }

    fn create_glyph_ir_work(
        &self,
        _glyph_name: &str,
        _glyph_files: &FileStateSet,
    ) -> Box<dyn Work<()>> {
        Box::from(GlyphIrWork {})
    }
}

struct GlyphIrWork {}

impl Work<()> for GlyphIrWork {
    fn exec(&self) -> Result<(), WorkError> {
        todo!()
    }
}
