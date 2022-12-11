use std::{
    collections::{BTreeMap, HashMap, HashSet},
    fs,
    path::{Path, PathBuf},
};

use fontir::{
    error::{Error, WorkError},
    ir::DesignSpaceLocation,
    orchestration::Context,
    source::{Input, Paths, Source, Work},
    stateset::{StateIdentifier, StateSet},
};
use log::debug;
use norad::designspace::{self, DesignSpaceDocument};

use crate::toir::{to_ir_glyph, to_ir_location};

pub struct DesignSpaceIrSource {
    designspace_file: PathBuf,
    designspace_dir: PathBuf,
    ir_paths: Paths,
    glif_locations: Option<GlifLocationCache>,
}

impl DesignSpaceIrSource {
    pub fn new(designspace_file: PathBuf, ir_paths: Paths) -> DesignSpaceIrSource {
        let designspace_dir = designspace_file
            .parent()
            .expect("designspace file *must* be in a directory")
            .to_path_buf();
        DesignSpaceIrSource {
            designspace_file,
            designspace_dir,
            ir_paths,
            glif_locations: None,
        }
    }
}

// A cache of locations, valid provided no global metadata changes
struct GlifLocationCache {
    global_metadata_sources: StateSet,
    locations: HashMap<PathBuf, Vec<DesignSpaceLocation>>,
}

impl GlifLocationCache {
    fn new(
        global_metadata_sources: StateSet,
        locations: HashMap<PathBuf, Vec<DesignSpaceLocation>>,
    ) -> GlifLocationCache {
        GlifLocationCache {
            global_metadata_sources,
            locations,
        }
    }

    fn is_valid_for(&self, global_metadata_sources: &StateSet) -> bool {
        self.global_metadata_sources == *global_metadata_sources
    }

    fn location_of(&self, glif_file: &Path) -> Option<&Vec<DesignSpaceLocation>> {
        self.locations.get(glif_file)
    }
}

fn glif_files<'a>(
    ufo_dir: &Path,
    layer_cache: &'a mut HashMap<String, HashMap<String, PathBuf>>,
    source: &designspace::Source,
) -> Result<BTreeMap<String, PathBuf>, Error> {
    let layer_name = layer_dir(ufo_dir, layer_cache, source)?;
    let glyph_dir = ufo_dir.join(layer_name);
    if !glyph_dir.is_dir() {
        return Err(Error::DirectoryExpected(glyph_dir));
    }

    let glyph_list_file = glyph_dir.join("contents.plist");
    if !glyph_list_file.is_file() {
        return Err(Error::FileExpected(glyph_list_file));
    }
    let result: BTreeMap<String, PathBuf> = plist::from_file(&glyph_list_file)
        .map_err(|e| Error::ParseError(glyph_list_file, e.to_string()))?;

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
        plist::from_file(&file).map_err(|e| Error::ParseError(file, e.to_string()))?;
    Ok(contents.into_iter().collect())
}

pub(crate) fn layer_dir<'a>(
    ufo_dir: &Path,
    layer_cache: &'a mut HashMap<String, HashMap<String, PathBuf>>,
    source: &designspace::Source,
) -> Result<&'a PathBuf, Error> {
    if !layer_cache.contains_key(&source.filename) {
        let contents = layer_contents(ufo_dir)?;
        layer_cache.insert(source.filename.clone(), contents);
    }
    let name_to_path = layer_cache.get_mut(&source.filename).unwrap();

    // No answer means dir is glyphs, which we'll stuff in under an empty string so the lifetime checks out
    let default_name = String::new();
    if source.layer.is_none() {
        name_to_path.insert(default_name.clone(), PathBuf::from("glyphs"));
    }
    name_to_path
        .get(source.layer.as_ref().unwrap_or(&default_name))
        .ok_or_else(|| Error::NoSuchLayer(source.filename.clone()))
}

impl DesignSpaceIrSource {
    fn load_designspace(&self) -> Result<DesignSpaceDocument, Error> {
        DesignSpaceDocument::load(&self.designspace_file)
            .map_err(|e| Error::UnableToLoadSource(Box::from(e)))
    }

    // When things like upem may have changed forget incremental and rebuild the whole thing
    fn global_rebuild_triggers(
        &self,
        designspace: &DesignSpaceDocument,
    ) -> Result<StateSet, Error> {
        let mut font_info = StateSet::new();
        font_info.track_file(&self.designspace_file)?;
        for source in designspace.sources.iter() {
            let ufo_dir = self.designspace_dir.join(&source.filename);
            for filename in ["fontinfo.plist", "layercontents.plist"] {
                let font_info_file = ufo_dir.join(filename);
                if !font_info_file.is_file() {
                    return Err(Error::FileExpected(font_info_file));
                }
                font_info.track_file(&font_info_file)?;
            }
        }
        Ok(font_info)
    }
}

impl Source for DesignSpaceIrSource {
    fn inputs(&mut self) -> Result<Input, Error> {
        let designspace = self.load_designspace()?;
        let global_metadata = self.global_rebuild_triggers(&designspace)?;

        // glif filenames are not reversible so we need to read contents.plist to figure out groups
        // See https://github.com/unified-font-object/ufo-spec/issues/164.
        let mut glyphs: HashMap<String, StateSet> = HashMap::new();

        // UFO filename => map of layer
        let mut layer_cache = HashMap::new();

        let mut glif_locations: HashMap<PathBuf, Vec<DesignSpaceLocation>> = HashMap::new();

        for source in designspace.sources {
            // Track files within each UFO
            // The UFO dir *must* exist since we were able to find fontinfo in it earlier
            let ufo_dir = self.designspace_dir.join(&source.filename);

            let location = to_ir_location(&source.location);

            for (glyph_name, glif_file) in glif_files(&ufo_dir, &mut layer_cache, &source)? {
                if !glif_file.exists() {
                    return Err(Error::FileExpected(glif_file));
                }
                let glif_file = glif_file.clone();
                glyphs
                    .entry(glyph_name)
                    .or_default()
                    .track_file(&glif_file)?;

                let glif_locations = glif_locations.entry(glif_file).or_default();
                glif_locations.push(location.clone());
            }
        }

        self.glif_locations = Some(GlifLocationCache::new(
            global_metadata.clone(),
            glif_locations,
        ));

        Ok(Input {
            global_metadata,
            glyphs,
        })
    }

    fn create_static_metadata_work(&self, context: &Context) -> Result<Box<dyn Work>, Error> {
        todo!()
    }

    fn create_glyph_ir_work(
        &self,
        glyph_names: &HashSet<&str>,
        input: &Input,
    ) -> Result<Vec<Box<dyn Work>>, Error> {
        // A single glif could be used by many source blocks that use the same layer
        // *gasp*
        // So resolve each file to 1..N locations in designspace

        let mut work: Vec<Box<dyn Work>> = Vec::new();

        // Do we have the location of glifs written down?
        // TODO: consider just recomputing here instead of failing
        if !self
            .glif_locations
            .as_ref()
            .map(|gl| gl.is_valid_for(&input.global_metadata))
            .unwrap_or(false)
        {
            return Err(Error::UnableToCreateGlyphIrWork);
        }

        for glyph_name in glyph_names {
            work.push(Box::from(
                self.create_work_for_one_glyph(glyph_name, input)?,
            ));
        }

        Ok(work)
    }
}

impl DesignSpaceIrSource {
    fn create_work_for_one_glyph(
        &self,
        glyph_name: &str,
        input: &Input,
    ) -> Result<GlyphIrWork, Error> {
        // A single glif could be used by many source blocks that use the same layer
        // *gasp*
        // So resolve each file to 1..N locations in designspace

        let glyph_name = glyph_name.to_string();
        let stateset = input
            .glyphs
            .get(&glyph_name)
            .ok_or_else(|| Error::NoStateForGlyph(glyph_name.clone()))?;
        let mut glif_files = HashMap::new();
        let glif_locations = self.glif_locations.as_ref().unwrap();
        for state_key in stateset.keys() {
            let StateIdentifier::File(glif_file) = state_key else {
                return Err(Error::UnexpectedState);
            };
            let locations = glif_locations
                .location_of(glif_file)
                .ok_or_else(|| Error::NoLocationsForGlyph(glyph_name.clone()))?;
            glif_files.insert(glif_file.to_path_buf(), locations.clone());
        }
        Ok(GlyphIrWork {
            glyph_name: glyph_name.clone(),
            glif_files,
            ir_file: self.ir_paths.glyph_ir_file(&glyph_name),
        })
    }
}

struct GlyphIrWork {
    glyph_name: String,
    glif_files: HashMap<PathBuf, Vec<DesignSpaceLocation>>,
    ir_file: PathBuf,
}

impl Work for GlyphIrWork {
    fn exec(&self, _: &Context) -> Result<(), WorkError> {
        debug!(
            "Generate {:#?} for {} {:#?}",
            self.ir_file, self.glyph_name, self.glif_files
        );
        let glyph_ir = to_ir_glyph(&self.glyph_name, &self.glif_files)
            .map_err(|e| WorkError::GlyphIrWorkError(self.glyph_name.clone(), e.to_string()))?;
        let glyph_ir_yml = serde_yaml::to_string(&glyph_ir).map_err(WorkError::YamlSerError)?;
        fs::write(&self.ir_file, glyph_ir_yml).map_err(WorkError::IoError)?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::{
        collections::HashMap,
        path::{Path, PathBuf},
    };

    use fontir::ir::DesignSpaceLocation;
    use fontir::source::{Input, Paths, Source};
    use norad::designspace;
    use ordered_float::OrderedFloat;
    use tempfile::tempdir;

    use super::{glif_files, DesignSpaceIrSource};

    fn testdata_dir() -> PathBuf {
        let dir = Path::new("../resources/testdata");
        assert!(dir.is_dir());
        dir.to_path_buf()
    }

    fn ufo_dir(filename: &str) -> PathBuf {
        testdata_dir().join(filename)
    }

    fn glifs_for_layer(filename: &str, layer: Option<String>) -> Vec<PathBuf> {
        let mut layer_cache = HashMap::new();
        let source = designspace::Source {
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
            vec![
                PathBuf::from("glyphs/bar.glif"),
                PathBuf::from("glyphs/plus.glif")
            ],
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

    fn test_source(build_dir: &Path) -> (DesignSpaceIrSource, Input) {
        let paths = Paths::new(build_dir);
        let mut source =
            DesignSpaceIrSource::new(testdata_dir().join("wght_var.designspace"), paths);
        let input = source.inputs().unwrap();
        (source, input)
    }

    fn add_location(
        add_to: &mut HashMap<PathBuf, Vec<DesignSpaceLocation>>,
        glif_file: &str,
        axis: &str,
        pos: f32,
    ) {
        add_to
            .entry(testdata_dir().join(glif_file))
            .or_default()
            .push(DesignSpaceLocation::from([(
                axis.to_string(),
                OrderedFloat(pos),
            )]));
    }

    #[test]
    pub fn create_work() {
        let temp_dir = tempdir().unwrap();
        let build_dir = temp_dir.path();
        let (ir_source, input) = test_source(build_dir);

        let work = ir_source.create_work_for_one_glyph("bar", &input).unwrap();

        let mut expected_glif_files = HashMap::new();
        add_location(
            &mut expected_glif_files,
            "WghtVar-Regular.ufo/glyphs/bar.glif",
            "Weight",
            400.0,
        );
        add_location(
            &mut expected_glif_files,
            "WghtVar-Regular.ufo/glyphs.{600}/bar.glif",
            "Weight",
            600.0,
        );
        add_location(
            &mut expected_glif_files,
            "WghtVar-Bold.ufo/glyphs/bar.glif",
            "Weight",
            700.0,
        );
        assert_eq!(expected_glif_files, work.glif_files);
    }

    #[test]
    pub fn create_sparse_work() {
        let temp_dir = tempdir().unwrap();
        let build_dir = temp_dir.path();
        let (ir_source, input) = test_source(build_dir);

        let work = ir_source.create_work_for_one_glyph("plus", &input).unwrap();

        // Note there is NOT a glyphs.{600} version of plus
        let mut expected_glif_files = HashMap::new();
        add_location(
            &mut expected_glif_files,
            "WghtVar-Regular.ufo/glyphs/plus.glif",
            "Weight",
            400.0,
        );
        add_location(
            &mut expected_glif_files,
            "WghtVar-Bold.ufo/glyphs/plus.glif",
            "Weight",
            700.0,
        );
        assert_eq!(expected_glif_files, work.glif_files);
    }
}
