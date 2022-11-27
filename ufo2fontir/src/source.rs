use std::{
    collections::{BTreeMap, HashMap},
    fs,
    path::{Path, PathBuf},
};

use fontir::{
    error::{Error, WorkError},
    filestate::FileStateSet,
    source::{Input, Paths, Source, Work},
};
use log::debug;
use norad::designspace::DesignSpaceDocument;

pub struct DesignSpaceIrSource {
    designspace_file: PathBuf,
    ir_paths: Paths,
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
    pub fn new(designspace_file: PathBuf, ir_paths: Paths) -> DesignSpaceIrSource {
        DesignSpaceIrSource {
            designspace_file,
            ir_paths,
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

    fn create_glyph_ir_work(
        &self,
        glyph_name: &str,
        _glyph_files: &FileStateSet,
    ) -> Box<dyn Work<()>> {
        Box::from(GlyphIrWork {
            glyph_name: glyph_name.to_string(),
            ir_file: self.ir_paths.glyph_ir_file(glyph_name),
        })
    }
}

struct GlyphIrWork {
    glyph_name: String,
    ir_file: PathBuf,
}

impl Work<()> for GlyphIrWork {
    fn exec(&self) -> Result<(), WorkError> {
        debug!("Generate IR for {}", self.glyph_name);
        fs::write(&self.ir_file, &self.glyph_name).map_err(WorkError::IoError)?;
        Ok(())
    }
}
