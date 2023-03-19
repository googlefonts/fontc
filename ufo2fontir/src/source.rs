use std::{
    collections::{BTreeMap, HashMap, HashSet},
    path::{Path, PathBuf},
    sync::Arc,
};

use fontdrasil::{orchestration::Work, types::GlyphName};
use fontir::{
    coords::{DesignLocation, NormalizedLocation, UserCoord},
    error::{Error, WorkError},
    ir::{Features, NameId, NameKey, StaticMetadata},
    orchestration::{Context, IrWork},
    source::{Input, Source},
    stateset::{StateIdentifier, StateSet},
};
use indexmap::IndexSet;
use log::{debug, trace, warn};
use norad::designspace::{self, DesignSpaceDocument};
use write_fonts::OtRound;

use crate::toir::{to_design_location, to_ir_axes, to_ir_glyph, to_normalized_locations};

pub struct DesignSpaceIrSource {
    designspace_file: PathBuf,
    designspace_dir: PathBuf,
    cache: Option<Cache>,
}

impl DesignSpaceIrSource {
    pub fn new(designspace_file: PathBuf) -> DesignSpaceIrSource {
        let designspace_dir = designspace_file
            .parent()
            .expect("designspace file *must* be in a directory")
            .to_path_buf();
        DesignSpaceIrSource {
            designspace_file,
            designspace_dir,
            cache: None,
        }
    }
}

// A cache of locations, valid provided no global metadata changes
struct Cache {
    static_metadata: StateSet,
    locations: HashMap<PathBuf, Vec<DesignLocation>>,
    designspace_file: PathBuf,
    designspace: Arc<DesignSpaceDocument>,
    fea_files: Arc<Vec<PathBuf>>,
}

impl Cache {
    fn new(
        static_metadata: StateSet,
        locations: HashMap<PathBuf, Vec<DesignLocation>>,
        designspace_file: PathBuf,
        designspace: DesignSpaceDocument,
        feature_files: Vec<PathBuf>,
    ) -> Cache {
        Cache {
            static_metadata,
            locations,
            designspace_file,
            designspace: Arc::from(designspace),
            fea_files: Arc::from(feature_files),
        }
    }

    fn is_valid_for(&self, static_metadata: &StateSet) -> bool {
        self.static_metadata == *static_metadata
    }

    fn location_of(&self, glif_file: &Path) -> Option<&Vec<DesignLocation>> {
        self.locations.get(glif_file)
    }
}

fn glif_files(
    ufo_dir: &Path,
    layer_cache: &mut HashMap<String, HashMap<GlyphName, PathBuf>>,
    source: &designspace::Source,
) -> Result<BTreeMap<GlyphName, PathBuf>, Error> {
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
        .map_err(|e| Error::ParseError(glyph_list_file.clone(), e.to_string()))?;

    if result.is_empty() {
        warn!("{:?} is empty", glyph_list_file);
    }

    Ok(result
        .into_iter()
        .map(|(glyph_name, path)| (glyph_name.into(), glyph_dir.join(path)))
        .collect())
}

fn layer_contents(ufo_dir: &Path) -> Result<HashMap<GlyphName, PathBuf>, Error> {
    let file = ufo_dir.join("layercontents.plist");
    if !file.is_file() {
        return Ok(HashMap::new());
    }
    let contents: Vec<(String, PathBuf)> =
        plist::from_file(&file).map_err(|e| Error::ParseError(file, e.to_string()))?;
    Ok(contents.into_iter().map(|(k, v)| (k.into(), v)).collect())
}

pub(crate) fn layer_dir<'a>(
    ufo_dir: &Path,
    layer_cache: &'a mut HashMap<String, HashMap<GlyphName, PathBuf>>,
    source: &designspace::Source,
) -> Result<&'a PathBuf, Error> {
    if !layer_cache.contains_key(&source.filename) {
        let contents = layer_contents(ufo_dir)?;
        layer_cache.insert(source.filename.clone(), contents);
    }
    let name_to_path = layer_cache.get_mut(&source.filename).unwrap();

    // No answer means dir is glyphs, which we'll stuff in under an empty string so the lifetime checks out
    let glyph_name = source
        .layer
        .as_ref()
        .map_or_else(GlyphName::empty, |l| l.into());
    if source.layer.is_none() {
        name_to_path.insert(glyph_name.clone(), PathBuf::from("glyphs"));
    }

    name_to_path
        .get(&glyph_name)
        .ok_or_else(|| Error::NoSuchLayer(source.filename.clone()))
}

impl DesignSpaceIrSource {
    fn load_designspace(&self) -> Result<DesignSpaceDocument, Error> {
        DesignSpaceDocument::load(&self.designspace_file)
            .map_err(|e| Error::UnableToLoadSource(Box::new(e)))
    }

    // When things like upem may have changed forget incremental and rebuild the whole thing
    fn static_metadata_state(&self, designspace: &DesignSpaceDocument) -> Result<StateSet, Error> {
        let mut font_info = StateSet::new();
        font_info.track_file(&self.designspace_file)?;
        let (default_master_idx, _) = default_master(designspace)
            .ok_or_else(|| Error::NoDefaultMaster(self.designspace_file.clone()))?;

        for (idx, source) in designspace.sources.iter().enumerate() {
            let ufo_dir = self.designspace_dir.join(&source.filename);
            for filename in ["fontinfo.plist", "lib.plist"] {
                // Only track lib.plist for the default master
                if filename == "lib.plist" && idx != default_master_idx {
                    continue;
                }

                let font_info_file = ufo_dir.join(filename);
                // TODO: this is incorrect; several of these files are optional
                // File tracking curently assumes you only track extant files so keep it for now
                if !font_info_file.is_file() {
                    return Err(Error::FileExpected(font_info_file));
                }
                font_info.track_file(&font_info_file)?;
            }
        }
        Ok(font_info)
    }

    fn check_static_metadata(&self, global_metadata: &StateSet) -> Result<(), Error> {
        // Do we have the location of glifs written down?
        // TODO: consider just recomputing here instead of failing
        if !self
            .cache
            .as_ref()
            .map(|gl| gl.is_valid_for(global_metadata))
            .unwrap_or(false)
        {
            return Err(Error::InvalidGlobalMetadata);
        }
        Ok(())
    }
}

impl Source for DesignSpaceIrSource {
    fn inputs(&mut self) -> Result<Input, Error> {
        let designspace = self.load_designspace()?;
        let static_metadata = self.static_metadata_state(&designspace)?;

        // glif filenames are not reversible so we need to read contents.plist to figure out groups
        // See https://github.com/unified-font-object/ufo-spec/issues/164.
        let mut glyphs: HashMap<GlyphName, StateSet> = HashMap::new();

        // UFO filename => map of layer
        let mut layer_cache = HashMap::new();
        let mut glif_locations: HashMap<PathBuf, Vec<DesignLocation>> = HashMap::new();
        let mut glyph_names: HashSet<GlyphName> = HashSet::new();

        let Some((default_master_idx, default_master)) = default_master(&designspace) else {
            return Err(Error::NoDefaultMaster(self.designspace_file.clone()));
        };
        let mut sources_default_first = vec![default_master];
        sources_default_first.extend(
            designspace
                .sources
                .iter()
                .enumerate()
                .filter(|(idx, _)| *idx != default_master_idx)
                .map(|(_, s)| s),
        );

        for (idx, source) in sources_default_first.iter().enumerate() {
            // Track files within each UFO
            // The UFO dir *must* exist since we were able to find fontinfo in it earlier
            let ufo_dir = self.designspace_dir.join(&source.filename);

            let location = to_design_location(&source.location);

            for (glyph_name, glif_file) in glif_files(&ufo_dir, &mut layer_cache, source)? {
                if !glif_file.exists() {
                    return Err(Error::FileExpected(glif_file));
                }
                if idx > 0 && !glyph_names.contains(&glyph_name) {
                    warn!("The glyph name '{:?}' exists in {} but not in the default master and will be ignored", glyph_name, source.filename);
                    continue;
                }
                glyph_names.insert(glyph_name.clone());
                let glif_file = glif_file.clone();
                glyphs
                    .entry(glyph_name)
                    .or_default()
                    .track_file(&glif_file)?;
                let glif_locations = glif_locations.entry(glif_file).or_default();
                glif_locations.push(location.clone());
            }
        }

        if glyph_names.is_empty() {
            warn!("No glyphs identified");
        } else {
            debug!("{} glyphs identified", glyph_names.len());
        }

        let ds_dir = self.designspace_file.parent().unwrap();
        let fea_files: Vec<_> = designspace
            .sources
            .iter()
            .filter_map(|s| {
                let fea_file = ds_dir.join(&s.filename).join("features.fea");
                fea_file.is_file().then_some(fea_file)
            })
            .collect();
        let mut features = StateSet::new();
        for fea_file in fea_files.iter() {
            features.track_file(fea_file)?;
        }

        self.cache = Some(Cache::new(
            static_metadata.clone(),
            glif_locations,
            self.designspace_file.clone(),
            designspace,
            fea_files,
        ));

        Ok(Input {
            static_metadata,
            glyphs,
            features,
        })
    }

    fn create_static_metadata_work(&self, input: &Input) -> Result<Box<IrWork>, Error> {
        self.check_static_metadata(&input.static_metadata)?;
        let cache = self.cache.as_ref().unwrap();

        let glyph_names = Arc::new(input.glyphs.keys().cloned().collect());

        Ok(Box::new(StaticMetadataWork {
            designspace_file: cache.designspace_file.clone(),
            designspace: cache.designspace.clone(),
            glyph_names,
        }))
    }

    fn create_feature_ir_work(&self, input: &Input) -> Result<Box<IrWork>, Error> {
        self.check_static_metadata(&input.static_metadata)?;
        let cache = self.cache.as_ref().unwrap();

        Ok(Box::new(FeatureWork {
            designspace_file: cache.designspace_file.clone(),
            fea_files: cache.fea_files.clone(),
        }))
    }

    fn create_glyph_ir_work(
        &self,
        glyph_names: &IndexSet<GlyphName>,
        input: &Input,
    ) -> Result<Vec<Box<IrWork>>, Error> {
        self.check_static_metadata(&input.static_metadata)?;

        // A single glif could be used by many source blocks that use the same layer
        // *gasp*
        // So resolve each file to 1..N locations in designspace
        let mut work: Vec<Box<IrWork>> = Vec::new();

        for glyph_name in glyph_names {
            work.push(Box::new(self.create_work_for_one_glyph(glyph_name, input)?));
        }

        Ok(work)
    }
}

impl DesignSpaceIrSource {
    fn create_work_for_one_glyph(
        &self,
        glyph_name: &GlyphName,
        input: &Input,
    ) -> Result<GlyphIrWork, Error> {
        // A single glif could be used by many source blocks that use the same layer
        // *gasp*
        // So resolve each file to 1..N locations in designspace

        let stateset = input
            .glyphs
            .get(glyph_name)
            .ok_or_else(|| Error::NoStateForGlyph(glyph_name.clone()))?;
        let mut glif_files = HashMap::new();
        let cache = self.cache.as_ref().unwrap();
        for state_key in stateset.keys() {
            let StateIdentifier::File(glif_file) = state_key else {
                return Err(Error::UnexpectedState);
            };
            let locations = cache
                .location_of(glif_file)
                .ok_or_else(|| Error::NoLocationsForGlyph(glyph_name.clone()))?;
            glif_files.insert(glif_file.to_path_buf(), locations.clone());
        }
        Ok(GlyphIrWork {
            glyph_name: glyph_name.clone(),
            glif_files,
        })
    }
}

struct StaticMetadataWork {
    designspace_file: PathBuf,
    designspace: Arc<DesignSpaceDocument>,
    glyph_names: Arc<HashSet<GlyphName>>,
}

struct FeatureWork {
    designspace_file: PathBuf,
    fea_files: Arc<Vec<PathBuf>>,
}

fn default_master(designspace: &DesignSpaceDocument) -> Option<(usize, &designspace::Source)> {
    let ds_axes = to_ir_axes(&designspace.axes);
    let axes: HashMap<_, _> = ds_axes.iter().map(|a| (&a.name, a)).collect();

    let default_location = designspace
        .axes
        .iter()
        .map(|a| {
            let converter = &axes.get(&a.name).unwrap().converter;
            (
                a.name.clone(),
                UserCoord::new(a.default).to_design(converter),
            )
        })
        .collect();
    designspace
        .sources
        .iter()
        .enumerate()
        .find(|(_, source)| to_design_location(&source.location) == default_location)
}

fn load_plist(ufo_dir: &Path, name: &str) -> Result<plist::Dictionary, WorkError> {
    let lib_plist_file = ufo_dir.join(name);
    if !lib_plist_file.is_file() {
        return Err(WorkError::FileExpected(lib_plist_file));
    }
    plist::Value::from_file(&lib_plist_file)
        .map_err(|e| WorkError::ParseError(lib_plist_file.clone(), format!("{e}")))?
        .into_dictionary()
        .ok_or_else(|| WorkError::ParseError(lib_plist_file, "Not a dictionary".to_string()))
}

// Per https://github.com/googlefonts/fontmake-rs/pull/43/files#r1044596662
fn glyph_order(
    source: &norad::designspace::Source,
    designspace_dir: &Path,
    glyph_names: &HashSet<GlyphName>,
) -> Result<IndexSet<GlyphName>, WorkError> {
    // The UFO at the default master *may* elect to specify a glyph order
    // That glyph order *may* deign to overlap with the actual glyph set
    let mut glyph_order = IndexSet::new();
    let lib_plist = load_plist(&designspace_dir.join(&source.filename), "lib.plist")?;
    if let Some(plist::Value::Array(ufo_order)) = lib_plist.get("public.glyphOrder") {
        let mut pending_add: HashSet<_> = glyph_names.clone();
        // Add names from ufo glyph order union glyph_names in ufo glyph order
        ufo_order
            .iter()
            .filter_map(|v| v.as_string().map(|s| s.into()))
            .filter(|name| glyph_names.contains(name))
            .for_each(|name| {
                glyph_order.insert(name.clone());
                pending_add.remove(&name);
            });
        // Add anything leftover in sorted order
        let mut pending_add: Vec<_> = pending_add.into_iter().collect();
        pending_add.sort();
        glyph_order.extend(pending_add);
    }
    if glyph_order.is_empty() {
        let notdef = ".notdef".into();
        if glyph_names.contains(&notdef) {
            glyph_order.insert(notdef);
        }
        glyph_names
            .iter()
            .filter(|name| name.as_str() != ".notdef")
            .for_each(|name| {
                glyph_order.insert(name.clone());
            });
    }
    Ok(glyph_order)
}

fn units_per_em<'a>(
    font_infos: impl Iterator<Item = &'a norad::FontInfo>,
) -> Result<u16, WorkError> {
    const MIN_UPEM: f64 = 16.0;
    const MAX_UPEM: f64 = 16384.0;

    let upems: Vec<_> = font_infos
        .filter_map(|fi| fi.units_per_em)
        .map(|v| v.as_f64())
        .collect();
    for upem in upems.iter() {
        // Per <https://learn.microsoft.com/en-us/typography/opentype/spec/head>, 16..16384
        if *upem < MIN_UPEM || *upem > MAX_UPEM {
            return Err(WorkError::InvalidUpem(format!("{upem}")));
        }
    }
    let upems: HashSet<u16> = upems.into_iter().map(|v| v.ot_round()).collect();
    if upems.len() != 1 {
        let mut upems: Vec<_> = upems.into_iter().collect();
        upems.sort();
        return Err(WorkError::InconsistentUpem(upems));
    }
    Ok(*upems.iter().next().unwrap())
}

fn files_identical(f1: &Path, f2: &Path) -> Result<bool, WorkError> {
    if !f1.is_file() {
        return Err(WorkError::FileExpected(f1.to_path_buf()));
    }
    if !f2.is_file() {
        return Err(WorkError::FileExpected(f2.to_path_buf()));
    }
    let m1 = f1.metadata().map_err(WorkError::IoError)?;
    let m2 = f2.metadata().map_err(WorkError::IoError)?;
    if m1.len() != m2.len() {
        return Ok(false);
    }
    Ok(true)
}

/// Creates a map from UFO directory name => fontinfo.
///
/// That is, source.filename => fontinfo.
fn font_infos<'a>(
    designspace_dir: &Path,
    designspace: &'a DesignSpaceDocument,
) -> Result<HashMap<&'a String, norad::FontInfo>, WorkError> {
    let mut results = HashMap::new();
    for source in designspace.sources.iter() {
        let ufo_dir = designspace_dir.join(&source.filename);
        let data_request = norad::DataRequest::none();
        let font = norad::Font::load_requested_data(&ufo_dir, data_request)
            .map_err(|e| WorkError::ParseError(ufo_dir, format!("{e}")))?;
        results.insert(&source.filename, font.font_info);
    }
    Ok(results)
}

fn names(font_info: &norad::FontInfo) -> HashMap<NameKey, String> {
    let mut result = HashMap::new();

    let mut insert_if_present = |maybe_value: &Option<String>, name_id: NameId| {
        if let Some(value) = maybe_value.as_ref() {
            // For now let's very optimistically emit unicode names only
            // <https://learn.microsoft.com/en-us/typography/opentype/spec/name#platform-encoding-and-language-ids>
            result.insert(
                NameKey {
                    platform_id: 0, // Unicode
                    encoding_id: 5, // Unicode full repertoire
                    lang_id: 0,     // irrelevent for platform=Unicode
                    name_id,
                },
                value.clone(),
            );
        }
    };

    insert_if_present(&font_info.family_name, NameId::FamilyName);

    result
}

impl Work<Context, WorkError> for StaticMetadataWork {
    fn exec(&self, context: &Context) -> Result<(), WorkError> {
        debug!("Static metadata for {:#?}", self.designspace_file);
        let designspace_dir = self.designspace_file.parent().unwrap();
        let Some((_, default_master)) = default_master(&self.designspace) else {
            return Err(WorkError::NoDefaultMaster(self.designspace_file.clone()));
        };
        let font_infos = font_infos(designspace_dir, &self.designspace)?;

        let units_per_em = units_per_em(font_infos.values())?;
        let names = names(font_infos.get(&default_master.filename).unwrap());
        let axes = to_ir_axes(&self.designspace.axes);
        let glyph_locations = to_normalized_locations(&axes, &self.designspace.sources);

        let glyph_order = glyph_order(&default_master, designspace_dir, &self.glyph_names)?;

        context.set_init_static_metadata(
            StaticMetadata::new(units_per_em, names, axes, glyph_order, glyph_locations)
                .map_err(WorkError::VariationModelError)?,
        );
        Ok(())
    }
}

impl Work<Context, WorkError> for FeatureWork {
    fn exec(&self, context: &Context) -> Result<(), WorkError> {
        debug!("Features for {:#?}", self.designspace_file);

        // TODO: support feature files that aren't identical
        let fea_files = self.fea_files.as_ref();
        for fea_file in fea_files.iter().skip(1) {
            if !files_identical(&fea_files[0], fea_file)? {
                warn!("Bailing out due to non-identical feature files. This is an unnecessary limitation.");
                return Err(WorkError::FileMismatch(
                    fea_files[0].to_path_buf(),
                    fea_file.to_path_buf(),
                ));
            }
        }

        if !fea_files.is_empty() {
            context.set_features(Features::from_file(&fea_files[0]));
        } else {
            context.set_features(Features::empty());
        }

        Ok(())
    }
}

struct GlyphIrWork {
    glyph_name: GlyphName,
    glif_files: HashMap<PathBuf, Vec<DesignLocation>>,
}

impl Work<Context, WorkError> for GlyphIrWork {
    fn exec(&self, context: &Context) -> Result<(), WorkError> {
        trace!(
            "Generate glyph IR for {:?} from {:#?}",
            self.glyph_name,
            self.glif_files
        );
        let static_metadata = context.get_init_static_metadata();

        // Migrate glif_files into internal coordinates
        let axes_by_name = static_metadata.axes.iter().map(|a| (&a.name, a)).collect();
        let mut glif_files = HashMap::new();
        for (path, design_locations) in self.glif_files.iter() {
            let normalized_locations: Vec<NormalizedLocation> = design_locations
                .iter()
                .map(|dl| dl.to_normalized(&axes_by_name))
                .collect();
            glif_files.insert(path, normalized_locations);
        }

        let glyph_ir = to_ir_glyph(self.glyph_name.clone(), &glif_files)?;
        context.set_glyph_ir(glyph_ir);
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::{
        collections::{HashMap, HashSet},
        path::{Path, PathBuf},
    };

    use fontdrasil::types::GlyphName;
    use fontir::{
        coords::{DesignCoord, DesignLocation},
        source::{Input, Source},
    };
    use indexmap::IndexSet;
    use norad::designspace;

    use crate::{source::font_infos, toir::to_design_location};

    use super::{default_master, glif_files, glyph_order, units_per_em, DesignSpaceIrSource};

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

    fn load_designspace(name: &str) -> (DesignSpaceIrSource, Input) {
        let mut source = DesignSpaceIrSource::new(testdata_dir().join(name));
        let input = source.inputs().unwrap();
        (source, input)
    }

    fn load_wght_var() -> (DesignSpaceIrSource, Input) {
        load_designspace("wght_var.designspace")
    }

    fn add_design_location(
        add_to: &mut HashMap<PathBuf, Vec<DesignLocation>>,
        glif_file: &str,
        axis: &str,
        pos: f32,
    ) {
        let mut loc = DesignLocation::new();
        loc.set_pos(axis, DesignCoord::new(pos));
        add_to
            .entry(testdata_dir().join(glif_file))
            .or_default()
            .push(loc);
    }

    #[test]
    pub fn create_work() {
        let (ir_source, input) = load_wght_var();

        let work = ir_source
            .create_work_for_one_glyph(&"bar".into(), &input)
            .unwrap();

        let mut expected_glif_files = HashMap::new();
        add_design_location(
            &mut expected_glif_files,
            "WghtVar-Regular.ufo/glyphs/bar.glif",
            "Weight",
            400.0,
        );
        add_design_location(
            &mut expected_glif_files,
            "WghtVar-Regular.ufo/glyphs.{600}/bar.glif",
            "Weight",
            600.0,
        );
        add_design_location(
            &mut expected_glif_files,
            "WghtVar-Bold.ufo/glyphs/bar.glif",
            "Weight",
            700.0,
        );
        assert_eq!(expected_glif_files, work.glif_files);
    }

    #[test]
    pub fn create_sparse_work() {
        let (ir_source, input) = load_wght_var();

        let work = ir_source
            .create_work_for_one_glyph(&"plus".into(), &input)
            .unwrap();

        // Note there is NOT a glyphs.{600} version of plus
        let mut expected_glif_files = HashMap::new();
        add_design_location(
            &mut expected_glif_files,
            "WghtVar-Regular.ufo/glyphs/plus.glif",
            "Weight",
            400.0,
        );
        add_design_location(
            &mut expected_glif_files,
            "WghtVar-Bold.ufo/glyphs/plus.glif",
            "Weight",
            700.0,
        );
        assert_eq!(expected_glif_files, work.glif_files);
    }

    #[test]
    pub fn only_glyphs_present_in_default() {
        let (_, inputs) = load_wght_var();
        // bonus_bar is not present in the default master; should discard
        assert!(!inputs.glyphs.contains_key(&"bonus_bar".into()));
    }

    #[test]
    pub fn find_default_master() {
        let (source, _) = load_wght_var();
        let ds = source.load_designspace().unwrap();
        let mut loc = DesignLocation::new();
        loc.set_pos("Weight", DesignCoord::new(400.0));
        assert_eq!(
            loc,
            to_design_location(&default_master(&ds).unwrap().1.location)
        );
    }

    #[test]
    pub fn builds_glyph_order_for_wght_var() {
        // Only WghtVar-Regular.ufo has a lib.plist, and it only lists a subset of glyphs
        // Should still work.
        let (source, _) = load_wght_var();
        let ds = source.load_designspace().unwrap();
        let (_, default_master) = default_master(&ds).unwrap();
        let go = glyph_order(
            default_master,
            &source.designspace_dir,
            &HashSet::from(["bar".into(), "plus".into(), "an-imaginary-one".into()]),
        )
        .unwrap();
        // lib.plist specifies plus, so plus goes first and then the rest in alphabetical order
        let expected: IndexSet<GlyphName> = vec!["plus", "an-imaginary-one", "bar"]
            .iter()
            .map(|s| (*s).into())
            .collect();
        assert_eq!(expected, go);
    }

    #[test]
    pub fn fetches_upem() {
        let (source, _) = load_wght_var();
        let ds = source.load_designspace().unwrap();
        let font_infos = font_infos(&source.designspace_dir, &ds).unwrap();
        assert_eq!(1000, units_per_em(font_infos.values()).unwrap());
    }

    #[test]
    pub fn ot_rounds_upem() {
        let (source, _) = load_designspace("float_upem.designspace");
        let ds = source.load_designspace().unwrap();
        let font_infos = font_infos(&source.designspace_dir, &ds).unwrap();
        assert_eq!(
            256, // 255.5 rounded toward +infinity
            units_per_em(font_infos.values()).unwrap()
        );
    }
}
