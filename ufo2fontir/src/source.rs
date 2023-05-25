use std::{
    collections::{BTreeMap, HashMap, HashSet},
    path::{Path, PathBuf},
    str::FromStr,
    sync::Arc,
};

use font_types::{NameId, Tag};
use fontdrasil::{orchestration::Work, types::GlyphName};
use fontir::{
    coords::{DesignLocation, NormalizedLocation, UserCoord},
    error::{Error, WorkError},
    ir::{
        Features, GlobalMetric, GlobalMetrics, NameBuilder, NameKey, NamedInstance, StaticMetadata,
    },
    orchestration::{Context, IrWork},
    source::{Input, Source},
    stateset::{StateIdentifier, StateSet},
};
use indexmap::IndexSet;
use log::{debug, trace, warn};
use norad::{
    designspace::{self, DesignSpaceDocument},
    fontinfo::StyleMapStyle,
};
use write_fonts::{tables::os2::SelectionFlags, OtRound};

use crate::toir::{master_locations, to_design_location, to_ir_axes, to_ir_glyph};

pub struct DesignSpaceIrSource {
    designspace_file: PathBuf,
    designspace_dir: PathBuf,
    cache: Option<Cache>,
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

                let file = ufo_dir.join(filename);
                // TODO: this is incorrect; several of these files are optional
                // File tracking curently assumes you only track extant files so keep it for now
                if !file.is_file() {
                    return Err(Error::FileExpected(file));
                }
                font_info.track_file(&file)?;
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

        // fontinfo.plist spans static metadata and global metrics.
        // Just use the same change detection for both.
        Ok(Input {
            static_metadata: static_metadata.clone(),
            global_metrics: static_metadata,
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

    fn create_global_metric_work(&self, input: &Input) -> Result<Box<IrWork>, Error> {
        self.check_static_metadata(&input.static_metadata)?;
        let cache = self.cache.as_ref().unwrap();

        Ok(Box::new(GlobalMetricsWork {
            designspace_file: cache.designspace_file.clone(),
            designspace: cache.designspace.clone(),
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

struct StaticMetadataWork {
    designspace_file: PathBuf,
    designspace: Arc<DesignSpaceDocument>,
    glyph_names: Arc<HashSet<GlyphName>>,
}

struct GlobalMetricsWork {
    designspace_file: PathBuf,
    designspace: Arc<DesignSpaceDocument>,
}

struct FeatureWork {
    designspace_file: PathBuf,
    fea_files: Arc<Vec<PathBuf>>,
}

fn default_master(designspace: &DesignSpaceDocument) -> Option<(usize, &designspace::Source)> {
    let ds_axes = to_ir_axes(&designspace.axes).ok()?;
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

    let mut upems: Vec<_> = font_infos
        .filter_map(|fi| match fi.units_per_em.map(|x| x.as_f64()) {
            None => None,
            // Per <https://learn.microsoft.com/en-us/typography/opentype/spec/head>, 16..16384
            Some(val) if (MIN_UPEM..=MAX_UPEM).contains(&val) => Some(Ok(val.ot_round())),
            Some(bad_val) => Some(Err(WorkError::InvalidUpem(format!("{bad_val}")))),
        })
        .collect::<Result<_, _>>()?;

    upems.sort_unstable();
    upems.dedup();
    // https://github.com/googlefonts/ufo2ft/blob/fca66fe3ea1ea88ffb36f8264b21ce042d3afd05/Lib/ufo2ft/fontInfoData.py#L359
    match upems.as_slice() {
        [] => Ok(1000),
        [one] => Ok(*one),
        _multiple => Err(WorkError::InconsistentUpem(upems)),
    }
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
    let mut builder = NameBuilder::default();

    builder.set_version(
        font_info.version_major.unwrap_or_default(),
        font_info.version_minor.unwrap_or_default(),
    );

    // Name's that get individual fields
    builder.add_if_present(NameId::COPYRIGHT_NOTICE, &font_info.copyright);
    builder.add_if_present(
        NameId::FAMILY_NAME,
        &font_info
            .style_map_family_name
            .as_ref()
            .or(font_info.family_name.as_ref())
            .cloned(),
    );
    builder.add_if_present(
        NameId::SUBFAMILY_NAME,
        &font_info.style_map_style_name.as_ref().map(|s| {
            match s {
                norad::fontinfo::StyleMapStyle::Regular => "regular",
                norad::fontinfo::StyleMapStyle::Italic => "italic",
                norad::fontinfo::StyleMapStyle::Bold => "bold",
                norad::fontinfo::StyleMapStyle::BoldItalic => "bold italic",
            }
            .into()
        }),
    );
    builder.add_if_present(NameId::UNIQUE_ID, &font_info.open_type_name_unique_id);
    builder.add_if_present(NameId::VERSION_STRING, &font_info.open_type_name_version);
    builder.add_if_present(
        NameId::TYPOGRAPHIC_FAMILY_NAME,
        &font_info.open_type_name_preferred_family_name,
    );
    builder.add_if_present(NameId::POSTSCRIPT_NAME, &font_info.postscript_font_name);
    builder.add_if_present(NameId::TRADEMARK, &font_info.trademark);
    builder.add_if_present(NameId::MANUFACTURER, &font_info.open_type_name_manufacturer);
    builder.add_if_present(NameId::DESIGNER, &font_info.open_type_name_designer);
    builder.add_if_present(NameId::DESCRIPTION, &font_info.open_type_name_description);
    builder.add_if_present(
        NameId::VENDOR_URL,
        &font_info.open_type_name_manufacturer_url,
    );
    builder.add_if_present(NameId::DESIGNER_URL, &font_info.open_type_name_designer_url);
    builder.add_if_present(
        NameId::LICENSE_DESCRIPTION,
        &font_info.open_type_name_license,
    );
    builder.add_if_present(NameId::LICENSE_URL, &font_info.open_type_name_license_url);
    builder.add_if_present(
        NameId::COMPATIBLE_FULL_NAME,
        &font_info.open_type_name_compatible_full_name,
    );
    builder.add_if_present(NameId::SAMPLE_TEXT, &font_info.open_type_name_sample_text);
    builder.add_if_present(
        NameId::WWS_FAMILY_NAME,
        &font_info.open_type_name_wws_family_name,
    );
    builder.add_if_present(
        NameId::WWS_SUBFAMILY_NAME,
        &font_info.open_type_name_wws_subfamily_name,
    );

    // After our first pass at getting values, apply fallbacks
    builder.apply_default_fallbacks();

    // Name's that don't get individual fields
    if let Some(name_records) = font_info.open_type_name_records.as_ref() {
        for nr in name_records.iter() {
            let name_id: u16 = nr.name_id.try_into().unwrap();
            builder.add(name_id.into(), nr.string.clone());
        }
    }

    builder.into_inner()
}

impl Work<Context, WorkError> for StaticMetadataWork {
    fn exec(&self, context: &Context) -> Result<(), WorkError> {
        debug!("Static metadata for {:#?}", self.designspace_file);
        let designspace_dir = self.designspace_file.parent().unwrap();
        let Some((_, default_master)) = default_master(&self.designspace) else {
            return Err(WorkError::NoDefaultMaster(self.designspace_file.clone()));
        };
        let font_infos = font_infos(designspace_dir, &self.designspace)?;
        let font_info_at_default = font_infos.get(&default_master.filename).ok_or_else(|| {
            WorkError::FileExpected(designspace_dir.join(&default_master.filename))
        })?;

        let units_per_em = units_per_em(font_infos.values())?;
        let names = names(font_info_at_default);
        let axes = to_ir_axes(&self.designspace.axes)?;

        let axes_by_name = axes.iter().map(|a| (&a.name, a)).collect();
        let family_prefix = names
            .get(&NameKey::new_bmp_only(NameId::FAMILY_NAME))
            .map(|name| name.clone() + " ")
            .unwrap_or_default();
        let named_instances = self
            .designspace
            .instances
            .iter()
            .map(|inst| NamedInstance {
                name: match inst.name.strip_prefix(family_prefix.as_str()) {
                    Some(tail) => tail.to_string(),
                    None => inst.name.clone(),
                },
                location: to_design_location(&inst.location).to_user(&axes_by_name),
            })
            .collect();

        let master_locations = master_locations(&axes, &self.designspace.sources);
        let glyph_locations = master_locations.values().cloned().collect();
        let glyph_order = glyph_order(default_master, designspace_dir, &self.glyph_names)?;

        // https://unifiedfontobject.org/versions/ufo3/fontinfo.plist/#opentype-os2-table-fields
        // Start with the bits from selection flags
        let selection_flags = font_info_at_default
            .open_type_os2_selection
            .as_ref()
            .map(|flags| {
                flags.iter().fold(SelectionFlags::empty(), |acc, e| {
                    acc | SelectionFlags::from_bits_truncate(1 << e)
                })
            })
            .unwrap_or_default()
            // Also set any bits implied by the style map style name
            | match font_info_at_default
                .style_map_style_name
                .as_ref()
                .unwrap_or(&StyleMapStyle::Regular)
            {
                StyleMapStyle::Regular => SelectionFlags::REGULAR,
                StyleMapStyle::Bold => SelectionFlags::BOLD,
                StyleMapStyle::Italic => SelectionFlags::ITALIC,
                StyleMapStyle::BoldItalic => SelectionFlags::BOLD | SelectionFlags::ITALIC,
            };

        let mut static_metadata = StaticMetadata::new(
            units_per_em,
            names,
            axes,
            named_instances,
            glyph_order,
            glyph_locations,
        )
        .map_err(WorkError::VariationModelError)?;
        static_metadata.misc.selection_flags = selection_flags;
        if let Some(vendor_id) = &font_info_at_default.open_type_os2_vendor_id {
            static_metadata.misc.vendor_id =
                Tag::from_str(vendor_id).map_err(WorkError::InvalidTag)?;
        }

        context.set_init_static_metadata(static_metadata);
        Ok(())
    }
}

impl Work<Context, WorkError> for GlobalMetricsWork {
    fn exec(&self, context: &Context) -> Result<(), WorkError> {
        debug!("Global metrics for {:#?}", self.designspace_file);
        let static_metadata = context.get_init_static_metadata();

        let designspace_dir = self.designspace_file.parent().unwrap();
        let font_infos = font_infos(designspace_dir, &self.designspace)?;
        let master_locations = master_locations(&static_metadata.axes, &self.designspace.sources);
        let Some((_, default_master)) = default_master(&self.designspace) else {
            return Err(WorkError::NoDefaultMaster(self.designspace_file.clone()));
        };

        let mut metrics = GlobalMetrics::new(
            static_metadata.default_location().clone(),
            static_metadata.units_per_em,
            font_infos
                .get(&default_master.filename)
                .ok_or_else(|| {
                    WorkError::FileExpected(designspace_dir.join(&default_master.filename))
                })?
                .x_height
                .map(|v| v as f32),
        );
        for source in self.designspace.sources.iter() {
            let pos = master_locations.get(&source.name).unwrap();

            // Sources that use layer= specifically should not contribute metrics, only glyphs
            if source.layer.is_some() {
                trace!(
                    "{} {pos:?} is uses layer=, skipping global metrics",
                    source.filename
                );
                continue;
            }

            let font_info = font_infos
                .get(&source.filename)
                .ok_or_else(|| WorkError::FileExpected(designspace_dir.join(&source.filename)))?;

            metrics.set_if_some(GlobalMetric::Ascender, pos.clone(), font_info.ascender);
            metrics.set_if_some(GlobalMetric::Descender, pos.clone(), font_info.descender);
            metrics.set_if_some(GlobalMetric::CapHeight, pos.clone(), font_info.cap_height);
            metrics.set_if_some(GlobalMetric::XHeight, pos.clone(), font_info.x_height);
            metrics.set_if_some(
                GlobalMetric::Os2TypoAscender,
                pos.clone(),
                font_info.open_type_os2_typo_ascender.map(|v| v as f64),
            );
            metrics.set_if_some(
                GlobalMetric::Os2TypoDescender,
                pos.clone(),
                font_info.open_type_os2_typo_descender.map(|v| v as f64),
            );
            metrics.set_if_some(
                GlobalMetric::Os2TypoLineGap,
                pos.clone(),
                font_info.open_type_os2_typo_line_gap.map(|v| v as f64),
            );
            metrics.set_if_some(
                GlobalMetric::Os2WinAscent,
                pos.clone(),
                font_info.open_type_os2_win_ascent.map(|v| v as f64),
            );
            metrics.set_if_some(
                GlobalMetric::Os2WinDescent,
                pos.clone(),
                font_info.open_type_os2_win_descent.map(|v| v as f64),
            );
        }

        trace!("{:#?}", metrics);
        context.set_global_metrics(metrics);
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

    use font_types::Tag;
    use fontdrasil::{orchestration::Access, types::GlyphName};
    use fontir::{
        coords::{DesignCoord, DesignLocation, NormalizedCoord, NormalizedLocation, UserCoord},
        ir::{GlobalMetricsInstance, NameKey},
        orchestration::{Context, Flags, WorkId},
        paths::Paths,
        source::{Input, Source},
    };
    use indexmap::IndexSet;
    use norad::designspace;

    use pretty_assertions::assert_eq;
    use write_fonts::types::NameId;

    use crate::{
        source::{font_infos, names},
        toir::to_design_location,
    };

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

    fn build_static_metadata(name: &str) -> (impl Source, Context) {
        let _ = env_logger::builder().is_test(true).try_init();
        let (source, input) = load_designspace(name);
        let mut flags = Flags::default();
        flags.set(Flags::EMIT_IR, false); // we don't want to write anything down
        let context = Context::new_root(
            flags,
            Paths::new(Path::new("/nothing/should/write/here")),
            input,
        );
        let task_context =
            context.copy_for_work(Access::none(), Access::one(WorkId::InitStaticMetadata));
        source
            .create_static_metadata_work(&context.input)
            .unwrap()
            .exec(&task_context)
            .unwrap();
        (source, context)
    }

    fn build_global_metrics(name: &str) -> (impl Source, Context) {
        let (source, context) = build_static_metadata(name);
        let task_context = context.copy_for_work(
            Access::one(WorkId::InitStaticMetadata),
            Access::one(WorkId::GlobalMetrics),
        );
        source
            .create_global_metric_work(&context.input)
            .unwrap()
            .exec(&task_context)
            .unwrap();
        (source, context)
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
        let (source, _) = load_designspace("float.designspace");
        let ds = source.load_designspace().unwrap();
        let font_infos = font_infos(&source.designspace_dir, &ds).unwrap();
        assert_eq!(
            256, // 255.5 rounded toward +infinity
            units_per_em(font_infos.values()).unwrap()
        );
    }

    #[test]
    pub fn default_names_for_minimal() {
        let (source, _) = load_designspace("float.designspace");
        let ds = source.load_designspace().unwrap();
        let font_info = font_infos(&source.designspace_dir, &ds)
            .unwrap()
            .get(&String::from("Float-Regular.ufo"))
            .cloned()
            .unwrap();
        let mut names: Vec<_> = names(&font_info).into_iter().collect();
        names.sort_by_key(|(id, v)| (id.name_id, v.clone()));

        assert_eq!(
            vec![
                (
                    NameKey::new_bmp_only(NameId::FAMILY_NAME),
                    String::from("New Font")
                ),
                (
                    NameKey::new_bmp_only(NameId::SUBFAMILY_NAME),
                    String::from("Regular")
                ),
                (
                    NameKey::new_bmp_only(NameId::UNIQUE_ID),
                    String::from("0.000;NONE;NewFontRegular")
                ),
                (
                    NameKey::new_bmp_only(NameId::FULL_NAME),
                    String::from("New Font Regular")
                ),
                (
                    NameKey::new_bmp_only(NameId::VERSION_STRING),
                    String::from("Version 0.000")
                ),
                (
                    NameKey::new_bmp_only(NameId::POSTSCRIPT_NAME),
                    String::from("NewFontRegular")
                ),
                (
                    NameKey::new_bmp_only(NameId::TYPOGRAPHIC_FAMILY_NAME),
                    String::from("New Font")
                ),
                (
                    NameKey::new_bmp_only(NameId::TYPOGRAPHIC_SUBFAMILY_NAME),
                    String::from("Regular")
                ),
            ],
            names
        );
    }

    #[test]
    fn captures_global_metrics_from_ints() {
        let (_, context) = build_global_metrics("static.designspace");
        let static_metadata = &context.get_init_static_metadata();
        let default_metrics = context
            .get_global_metrics()
            .at(static_metadata.default_location());
        assert_eq!(
            (720.0, 510.0),
            (
                default_metrics.cap_height.into_inner(),
                default_metrics.x_height.into_inner()
            ),
        );
    }

    #[test]
    fn captures_global_metrics_from_floats() {
        let (_, context) = build_global_metrics("float.designspace");
        let static_metadata = &context.get_init_static_metadata();
        let default_metrics = context
            .get_global_metrics()
            .at(static_metadata.default_location());
        assert_eq!(
            (755.25, -174.5),
            (
                default_metrics.ascender.into_inner(),
                default_metrics.descender.into_inner()
            ),
        );
    }

    #[test]
    fn captures_os2_properties() {
        let (_, context) = build_static_metadata("fontinfo.designspace");
        assert_eq!(
            Tag::new(b"RODS"),
            context.get_init_static_metadata().misc.vendor_id
        );
    }

    #[test]
    fn captures_global_metrics() {
        let (_, context) = build_global_metrics("fontinfo.designspace");
        let static_metadata = &context.get_init_static_metadata();
        let default_metrics = context
            .get_global_metrics()
            .at(static_metadata.default_location());
        assert_eq!(
            GlobalMetricsInstance {
                pos: static_metadata.default_location().clone(),
                ascender: 737.0.into(),
                descender: (-42.0).into(),
                cap_height: 702.0.into(),
                x_height: 501.0.into(),
                y_subscript_x_size: 650.0.into(),
                y_subscript_y_size: 600.0.into(),
                y_subscript_y_offset: 75.0.into(),
                y_superscript_x_size: 650.0.into(),
                y_superscript_y_size: 600.0.into(),
                y_superscript_y_offset: 350.0.into(),
                y_strikeout_position: 300.6.into(),
                y_strikeout_size: 50.0.into(),
                os2_typo_ascender: 1193.0.into(),
                os2_typo_descender: (-289.0).into(),
                os2_typo_line_gap: 42.0.into(),
                os2_win_ascent: 1325.0.into(),
                os2_win_descent: 377.0.into(),
                ..Default::default()
            },
            default_metrics
        );
    }

    fn only_coord(loc: &NormalizedLocation) -> NormalizedCoord {
        assert_eq!(1, loc.axis_names().count());
        *loc.iter().next().unwrap().1
    }

    // Was tripping up on wght_var having two <source> with the same filename, different name and xvalue
    #[test]
    fn glyph_locations() {
        let (_, context) = build_static_metadata("wght_var.designspace");
        let static_metadata = &context.get_init_static_metadata();
        let wght = static_metadata.variable_axes.first().unwrap();

        assert_eq!(
            vec![
                (UserCoord::new(400.0), NormalizedCoord::new(0.0)),
                (UserCoord::new(600.0), NormalizedCoord::new(0.6666667,)),
                (UserCoord::new(700.0), NormalizedCoord::new(1.0)),
            ],
            static_metadata
                .variation_model
                .locations()
                .map(|loc| (only_coord(loc).to_user(&wght.converter), only_coord(loc)))
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn no_metrics_for_glyph_only_sources() {
        let (_, context) = build_global_metrics("wght_var.designspace");
        let static_metadata = &context.get_init_static_metadata();
        let wght = static_metadata.variable_axes.first().unwrap();
        let mut metric_locations = context
            .get_global_metrics()
            .iter()
            .map(|(loc, ..)| loc)
            .collect::<HashSet<_>>()
            .into_iter()
            .map(|loc| (only_coord(&loc).to_user(&wght.converter), only_coord(&loc)))
            .collect::<Vec<_>>();
        metric_locations.sort();

        // Note there are *not* metrics at (600, 0.67)
        assert_eq!(
            vec![
                (UserCoord::new(400.0), NormalizedCoord::new(0.0)),
                (UserCoord::new(700.0), NormalizedCoord::new(1.0)),
            ],
            metric_locations
        );
    }
}
