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
use norad::designspace::{self, DesignSpaceDocument};

pub struct DesignSpaceIrSource {
    designspace_file: PathBuf,
    ir_paths: Paths,
}

impl DesignSpaceIrSource {
    pub fn new(designspace_file: PathBuf, ir_paths: Paths) -> DesignSpaceIrSource {
        DesignSpaceIrSource {
            designspace_file,
            ir_paths,
        }
    }
}

fn glif_files<'a>(
    ufo_dir: &Path,
    layer_cache: &'a mut HashMap<String, HashMap<String, PathBuf>>,
    source: &designspace::Source,
) -> Result<BTreeMap<String, PathBuf>, Error> {
    let layer_name = layer_name(ufo_dir, layer_cache, source)?;
    let glyph_dir = ufo_dir.join(layer_name);
    if !glyph_dir.is_dir() {
        return Err(Error::DirectoryExpected(glyph_dir));
    }

    let glyph_list_file = glyph_dir.join("contents.plist");
    if !glyph_list_file.is_file() {
        return Err(Error::FileExpected(glyph_list_file));
    }
    let result: BTreeMap<String, PathBuf> = plist::from_file(&glyph_list_file)
        .map_err(|e| Error::ParseError(glyph_list_file, e.into()))?;

    Ok(result
        .into_iter()
        .map(|(glyph_name, path)| (glyph_name, glyph_dir.join(path)))
        .collect())
}

fn layer_contents(ufo_dir: &Path) -> Result<HashMap<String, PathBuf>, Error> {
    let file = ufo_dir.join("layercontents.plist");
    if !file.is_file() {
        return Err(Error::FileExpected(file));
    }
    let contents: Vec<(String, PathBuf)> =
        plist::from_file(&file).map_err(|e| Error::ParseError(file, e.into()))?;
    Ok(contents.into_iter().collect())
}

pub(crate) fn layer_name<'a>(
    ufo_dir: &Path,
    layer_cache: &'a mut HashMap<String, HashMap<String, PathBuf>>,
    source: &designspace::Source,
) -> Result<&'a PathBuf, Error> {
    static DEFAULT_LAYER_NAME: &str = "public.default";

    if !layer_cache.contains_key(&source.filename) {
        let contents = layer_contents(ufo_dir)?;
        layer_cache.insert(source.filename.clone(), contents);
    }
    let layer_name = source.layer.as_deref().unwrap_or(DEFAULT_LAYER_NAME);
    layer_cache
        .get(&source.filename)
        .unwrap()
        .get(layer_name)
        .ok_or_else(|| Error::NoSuchLayer(source.filename.clone()))
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

        // UFO filename => map of layer
        let mut layer_cache = HashMap::new();

        for source in designspace.sources {
            // Track files within each UFO
            // The UFO dir *must* exist since we were able to find fontinfo in it earlier
            let ufo_dir = designspace_dir.join(&source.filename);

            for (glyph_name, glif_file) in glif_files(&ufo_dir, &mut layer_cache, &source)? {
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

#[cfg(test)]
mod tests {
    use std::{
        collections::HashMap,
        path::{Path, PathBuf},
    };

    use norad::designspace::Source;

    use super::glif_files;

    fn ufo_dir(filename: &str) -> PathBuf {
        Path::new("../resources/testdata").join(filename)
    }

    fn glifs_for_layer(filename: &str, layer: Option<String>) -> Vec<PathBuf> {
        let mut layer_cache = HashMap::new();
        let source = Source {
            filename: filename.to_string(),
            layer,
            ..Default::default()
        };
        let ufo_dir = ufo_dir(&source.filename);
        glif_files(&ufo_dir, &mut layer_cache, &source)
            .unwrap()
            .into_values()
            .map(|p| p.strip_prefix(&ufo_dir).unwrap().to_path_buf())
            .collect::<Vec<PathBuf>>()
    }

    #[test]
    pub fn glyphs_from_default_layer() {
        assert_eq!(
            vec![PathBuf::from("glyphs/bar.glif")],
            glifs_for_layer("WghtVar-Regular.ufo", None)
        );
    }

    #[test]
    pub fn glyphs_from_specific_layer() {
        assert_eq!(
            vec![PathBuf::from("glyphs.{600}/bar.glif")],
            glifs_for_layer("WghtVar-Regular.ufo", Some("{600}".to_string()))
        );
    }
}
