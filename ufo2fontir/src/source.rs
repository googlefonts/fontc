use std::{
    collections::{BTreeMap, BTreeSet, HashMap, HashSet},
    path::{Path, PathBuf},
    str::FromStr,
    sync::Arc,
};

use chrono::{DateTime, NaiveDateTime, Utc};
use fontdrasil::{
    coords::{DesignLocation, NormalizedLocation, UserCoord},
    orchestration::{Access, AccessBuilder, Work},
    types::GlyphName,
};
use fontir::{
    error::{BadSource, BadSourceKind, Error},
    ir::{
        AnchorBuilder, FeaturesSource, GdefCategories, GlobalMetric, GlobalMetrics, GlyphOrder,
        KernGroup, KernSide, KerningGroups, KerningInstance, MetaTableValues, NameBuilder, NameKey,
        NamedInstance, Panose, PostscriptNames, StaticMetadata, DEFAULT_VENDOR_ID,
    },
    orchestration::{Context, Flags, IrWork, WorkId},
    source::Source,
};
use log::{debug, log_enabled, trace, warn, Level};
use norad::{
    designspace::{self, DesignSpaceDocument},
    fontinfo::StyleMapStyle,
};
use write_fonts::{
    read::tables::gasp::GaspRangeBehavior,
    tables::{gasp::GaspRange, gdef::GlyphClassDef, os2::SelectionFlags},
    types::{NameId, Tag},
    OtRound,
};

use crate::toir::{master_locations, to_design_location, to_ir_axes, to_ir_glyph};

const UFO_KERN1_PREFIX: &str = "public.kern1.";
const UFO_KERN2_PREFIX: &str = "public.kern2.";

#[derive(Clone, Debug)]
pub struct DesignSpaceIrSource {
    designspace_or_ufo: Arc<PathBuf>,
    designspace: Arc<DesignSpaceDocument>,
    designspace_dir: Arc<PathBuf>,
    glyphs: Arc<HashMap<GlyphName, HashMap<PathBuf, Vec<DesignLocation>>>>,
    fea_files: Arc<Vec<PathBuf>>,
}

fn glif_files(
    ufo_dir: &Path,
    layer_cache: &mut HashMap<String, HashMap<GlyphName, PathBuf>>,
    source: &designspace::Source,
) -> Result<BTreeMap<GlyphName, PathBuf>, Error> {
    let layer_name = layer_dir(ufo_dir, layer_cache, source)?;
    let glyph_dir = ufo_dir.join(layer_name);
    if !glyph_dir.is_dir() {
        return Err(BadSource::new(glyph_dir, BadSourceKind::ExpectedDirectory).into());
    }

    let glyph_list_file = glyph_dir.join("contents.plist");
    if !glyph_list_file.is_file() {
        return Err(BadSource::new(glyph_list_file, BadSourceKind::ExpectedFile).into());
    }
    let result: BTreeMap<String, PathBuf> =
        plist::from_file(&glyph_list_file).map_err(|e| BadSource::custom(&glyph_list_file, e))?;

    if result.is_empty() {
        warn!("{:?} is empty", glyph_list_file);
    }

    Ok(result
        .into_iter()
        .map(|(glyph_name, path)| (glyph_name.into(), glyph_dir.join(path)))
        .collect())
}

fn layer_contents(ufo_dir: &Path) -> Result<HashMap<GlyphName, PathBuf>, BadSource> {
    let file = ufo_dir.join("layercontents.plist");
    if !file.is_file() {
        return Ok(HashMap::new());
    }
    let contents: Vec<(GlyphName, PathBuf)> =
        plist::from_file(&file).map_err(|e| BadSource::custom(file, e))?;
    Ok(contents.into_iter().collect())
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
        .map_or_else(GlyphName::empty, |l| l.as_str().into());
    if source.layer.is_none() {
        name_to_path.insert(glyph_name.clone(), PathBuf::from("glyphs"));
    }

    name_to_path
        .get(&glyph_name)
        .ok_or_else(|| Error::NoSuchLayer(source.filename.clone()))
}

impl DesignSpaceIrSource {
    fn create_work_for_one_glyph(
        &self,
        glyph_name: &GlyphName,
        export: bool,
    ) -> Result<GlyphIrWork, Error> {
        // A single glif could be used by many source blocks that use the same layer
        // *gasp*
        // So resolve each file to 1..N locations in designspace
        let Some(glif_files) = self.glyphs.get(glyph_name) else {
            return Err(Error::NoLocationsForGlyph(glyph_name.clone()));
        };

        Ok(GlyphIrWork {
            glyph_name: glyph_name.clone(),
            export,
            glif_files: glif_files.clone(),
        })
    }
}

fn load_designspace(
    designspace_or_ufo: &Path,
) -> Result<(PathBuf, DesignSpaceDocument), BadSourceKind> {
    let Some(designspace_dir) = designspace_or_ufo.parent().map(|d| d.to_path_buf()) else {
        return Err(BadSourceKind::ExpectedParent);
    };

    let Some(ext) = designspace_or_ufo
        .extension()
        .map(|s| s.to_ascii_lowercase())
    else {
        return Err(BadSourceKind::UnrecognizedExtension);
    };
    let designspace = match ext.to_str() {
        Some("designspace") => {
            DesignSpaceDocument::load(designspace_or_ufo).map_err(|e| match e {
                norad::error::DesignSpaceLoadError::Io(e) => BadSourceKind::Io(e),
                other => BadSourceKind::Custom(other.to_string()),
            })?
        }
        Some("ufo") => {
            let Some(filename) = designspace_or_ufo.file_name().and_then(|s| s.to_str()) else {
                return Err(BadSourceKind::ExpectedDirectory);
            };
            DesignSpaceDocument {
                format: 4.1,
                sources: vec![norad::designspace::Source {
                    filename: filename.to_owned(),
                    ..Default::default()
                }],
                ..Default::default()
            }
        }
        _ => return Err(BadSourceKind::UnrecognizedExtension),
    };
    debug!("Loaded {ext:?} from {designspace_or_ufo:?}");
    Ok((designspace_dir, designspace))
}

impl Source for DesignSpaceIrSource {
    fn new(designspace_or_ufo_file: &Path) -> Result<Self, Error> {
        let (designspace_dir, mut designspace) = load_designspace(designspace_or_ufo_file)
            .map_err(|kind| {
                Error::BadSource(BadSource::new(designspace_or_ufo_file.to_path_buf(), kind))
            })?;

        for (i, source) in designspace.sources.iter_mut().enumerate() {
            if source.name.is_none() {
                source.name = Some(format!("unnamed_source_{i}"));
            }
        }

        for (i, instance) in designspace.instances.iter_mut().enumerate() {
            if instance.name.is_none() {
                instance.name = Some(format!("unnamed_instance_{i}"))
            }
        }

        let axis_tags_by_name = designspace
            .axes
            .iter()
            .map(|a| {
                Tag::from_str(&a.tag)
                    .map(|tag| (a.name.as_str(), tag))
                    .map_err(|cause| Error::InvalidTag {
                        cause,
                        raw_tag: a.tag.clone(),
                    })
            })
            .collect::<Result<HashMap<&str, Tag>, _>>()?;

        // glif filenames are not reversible so we need to read contents.plist to figure out groups
        // See https://github.com/unified-font-object/ufo-spec/issues/164.
        // UFO filename => map of layer
        let mut layer_cache = HashMap::new();

        let mut glyphs = HashMap::<GlyphName, HashMap<PathBuf, Vec<DesignLocation>>>::new();

        let Some((default_master_idx, _)) = default_master(&designspace) else {
            return Err(Error::NoDefaultMaster(
                designspace_or_ufo_file.to_path_buf(),
            ));
        };
        let mut sources_indices_default_first = (0..designspace.sources.len()).collect::<Vec<_>>();
        sources_indices_default_first.swap(0, default_master_idx);

        for source_idx in sources_indices_default_first {
            let source = &designspace.sources[source_idx];
            // Track files within each UFO
            // The UFO dir *must* exist since we were able to find fontinfo in it earlier
            let ufo_dir = designspace_dir.join(&source.filename);

            let location = to_design_location(&axis_tags_by_name, &source.location);
            for (glyph_name, glif_file) in glif_files(&ufo_dir, &mut layer_cache, source)? {
                if !glif_file.exists() {
                    return Err(BadSource::new(glif_file, BadSourceKind::ExpectedFile).into());
                }
                // Default master went first, so if we've never seen this glyph before that's weird
                if source_idx != default_master_idx && !glyphs.contains_key(&glyph_name) {
                    warn!("The glyph name '{:?}' exists in {} but not in the default master and will be ignored", glyph_name, source.filename);
                    continue;
                }

                glyphs
                    .entry(glyph_name)
                    .or_default()
                    .entry(glif_file)
                    .or_default()
                    .push(location.clone());
            }
        }

        if glyphs.is_empty() {
            warn!("No glyphs identified");
        } else {
            debug!("{} glyphs identified", glyphs.len());
        }

        // For compilation purposes we start from ufo dir / features.fea
        let fea_files: Vec<_> = designspace
            .sources
            .iter()
            .filter_map(|s| {
                let fea_file = designspace_dir.join(&s.filename).join("features.fea");
                fea_file.is_file().then_some(fea_file)
            })
            .collect();

        Ok(DesignSpaceIrSource {
            designspace_or_ufo: Arc::new(designspace_or_ufo_file.to_path_buf()),
            designspace: Arc::new(designspace),
            designspace_dir: Arc::new(designspace_dir),
            glyphs: Arc::new(glyphs),
            fea_files: Arc::new(fea_files),
        })
    }

    fn create_static_metadata_work(&self) -> Result<Box<IrWork>, Error> {
        Ok(Box::new(StaticMetadataWork {
            designspace_or_ufo: self.designspace_or_ufo.clone(),
            designspace_dir: self.designspace_dir.clone(),
            designspace: self.designspace.clone(),
            glyph_names: Arc::new(self.glyphs.keys().cloned().collect()),
        }))
    }

    fn create_global_metric_work(&self) -> Result<Box<IrWork>, Error> {
        Ok(Box::new(GlobalMetricsWork {
            designspace_or_ufo: self.designspace_or_ufo.clone(),
            designspace_dir: self.designspace_dir.clone(),
            designspace: self.designspace.clone(),
        }))
    }

    fn create_feature_ir_work(&self) -> Result<Box<IrWork>, Error> {
        debug!("CREATE FEATURES");
        Ok(Box::new(FeatureWork {
            designspace_or_ufo: self.designspace_or_ufo.clone(),
            fea_files: self.fea_files.clone(),
        }))
    }

    fn create_kerning_group_ir_work(&self) -> Result<Box<IrWork>, Error> {
        Ok(Box::new(KerningGroupWork {
            designspace_or_ufo: self.designspace_or_ufo.clone(),
            designspace_dir: self.designspace_dir.clone(),
            designspace: self.designspace.clone(),
        }))
    }

    fn create_kerning_instance_ir_work(
        &self,
        at: NormalizedLocation,
    ) -> Result<Box<IrWork>, Error> {
        Ok(Box::new(KerningInstanceWork {
            designspace_or_ufo: self.designspace_or_ufo.clone(),
            designspace_dir: self.designspace_dir.clone(),
            designspace: self.designspace.clone(),
            location: at,
        }))
    }

    fn create_glyph_ir_work(&self) -> Result<Vec<Box<IrWork>>, Error> {
        let no_export: HashSet<GlyphName> = if let Some(plist::Value::Array(no_export)) =
            self.designspace.lib.get("public.skipExportGlyphs")
        {
            no_export
                .iter()
                .filter_map(|e| e.as_string())
                .map(|e| e.into())
                .collect()
        } else {
            HashSet::new()
        };

        // A single glif could be used by many source blocks that use the same layer
        // *gasp*
        // So resolve each file to 1..N locations in designspace
        let mut work: Vec<Box<IrWork>> = Vec::new();

        for glyph_name in self.glyphs.keys() {
            work.push(Box::new(self.create_work_for_one_glyph(
                glyph_name,
                !no_export.contains(glyph_name),
            )?));
        }

        Ok(work)
    }
}

#[derive(Debug)]
struct StaticMetadataWork {
    designspace_or_ufo: Arc<PathBuf>,
    designspace_dir: Arc<PathBuf>,
    designspace: Arc<DesignSpaceDocument>,
    glyph_names: Arc<HashSet<GlyphName>>,
}

#[derive(Debug)]
struct GlobalMetricsWork {
    designspace_or_ufo: Arc<PathBuf>,
    designspace_dir: Arc<PathBuf>,
    designspace: Arc<DesignSpaceDocument>,
}

#[derive(Debug)]
struct FeatureWork {
    designspace_or_ufo: Arc<PathBuf>,
    fea_files: Arc<Vec<PathBuf>>,
}

#[derive(Debug)]
struct KerningGroupWork {
    designspace_or_ufo: Arc<PathBuf>,
    designspace_dir: Arc<PathBuf>,
    designspace: Arc<DesignSpaceDocument>,
}

#[derive(Debug)]
struct KerningInstanceWork {
    designspace_or_ufo: Arc<PathBuf>,
    designspace_dir: Arc<PathBuf>,
    designspace: Arc<DesignSpaceDocument>,
    location: NormalizedLocation,
}

fn default_master(designspace: &DesignSpaceDocument) -> Option<(usize, &designspace::Source)> {
    let ds_axes = to_ir_axes(&designspace.axes).ok()?;
    let tags_by_name: HashMap<_, _> = ds_axes.iter().map(|a| (a.name.as_str(), a.tag)).collect();
    let axes: HashMap<_, _> = ds_axes.iter().map(|a| (a.tag, a)).collect();

    let default_location = designspace
        .axes
        .iter()
        .map(|a| {
            let tag = Tag::from_str(&a.tag).unwrap();
            let converter = &axes.get(&tag).unwrap().converter;
            (tag, UserCoord::new(a.default as f64).to_design(converter))
        })
        .collect();
    designspace
        .sources
        .iter()
        .enumerate()
        .find(|(_, source)| to_design_location(&tags_by_name, &source.location) == default_location)
}

fn load_plist(ufo_dir: &Path, name: &str) -> Result<plist::Dictionary, BadSource> {
    let lib_plist_file = ufo_dir.join(name);
    if !lib_plist_file.is_file() {
        return Err(BadSource::new(lib_plist_file, BadSourceKind::ExpectedFile));
    }
    plist::Value::from_file(&lib_plist_file)
        .map_err(|e| BadSource::custom(&lib_plist_file, e))?
        .into_dictionary()
        .ok_or_else(|| BadSource::custom(lib_plist_file, "not a dictionary"))
}

// Per https://github.com/googlefonts/fontmake-rs/pull/43/files#r1044596662
fn glyph_order(
    lib_plist: &plist::Dictionary,
    glyph_names: &HashSet<GlyphName>,
) -> Result<GlyphOrder, Error> {
    // The UFO at the default master *may* elect to specify a glyph order
    // That glyph order *may* deign to overlap with the actual glyph set
    let mut glyph_order = GlyphOrder::new();
    let mut pending_add: HashSet<_> = glyph_names.clone();
    if let Some(plist::Value::Array(ufo_order)) = lib_plist.get("public.glyphOrder") {
        // Add names from ufo glyph order union glyph_names in ufo glyph order
        ufo_order
            .iter()
            .filter_map(|v| v.as_string().map(GlyphName::new))
            .filter(|name| glyph_names.contains(name))
            .for_each(|name| {
                pending_add.remove(&name);
                glyph_order.insert(name);
            });
    }
    // Add anything leftover in sorted order
    let mut pending_add: Vec<_> = pending_add.into_iter().collect();
    pending_add.sort();
    glyph_order.extend(pending_add);
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

fn glyph_categories(
    lib_plist: &plist::Dictionary,
) -> Result<BTreeMap<GlyphName, GlyphClassDef>, BadSource> {
    const OPENTYPE_CATEGORIES: &str = "public.openTypeCategories";

    let categories = match lib_plist.get(OPENTYPE_CATEGORIES) {
        Some(plist::Value::Dictionary(categories)) => categories,
        Some(_other) => {
            return Err(BadSource::custom(
                "lib.plist",
                format!("value for '{OPENTYPE_CATEGORIES}' is not a dictionary"),
            ));
        }
        None => return Ok(Default::default()),
    };

    let categories = categories
        .iter()
        .filter_map(|(name, raw_cat)| match raw_cat.as_string() {
            Some("base") => Some((GlyphName::new(name), GlyphClassDef::Base)),
            Some("ligature") => Some((GlyphName::new(name), GlyphClassDef::Ligature)),
            Some("mark") => Some((GlyphName::new(name), GlyphClassDef::Mark)),
            Some("component") => Some((GlyphName::new(name), GlyphClassDef::Component)),
            // this is explicitly allowed, although it doesn't mean anything?
            // https://unifiedfontobject.org/versions/ufo3/lib.plist/#publicopentypecategories
            Some("unassigned") => None,
            Some(_) | None => {
                log::warn!("unknown value in public.openTypeCategories for glyph '{name}'");
                None
            }
        })
        .collect();
    Ok(categories)
}

fn postscript_names(lib_plist: &plist::Dictionary) -> Result<PostscriptNames, BadSource> {
    let postscript_names = match lib_plist.get("public.postscriptNames") {
        Some(value) => {
            let postscript_names_lib = value.as_dictionary().ok_or_else(|| {
                BadSource::custom("lib.plist", "public.postscriptNames isn't a dictionary")
            })?;
            postscript_names_lib
                .iter()
                .filter_map(|(glyph_name, ps_name)| match ps_name.as_string() {
                    Some(ps_name) => Some((
                        GlyphName::from(glyph_name.as_str()),
                        GlyphName::from(ps_name),
                    )),
                    None => {
                        warn!("public.postscriptNames: \"{glyph_name}\" has a non-string entry");
                        None
                    }
                })
                .collect()
        }
        None => HashMap::new(),
    };
    // Warn about duplicate public.postscriptNames values
    // Allocate space ahead of time for speed, and because in the happy path the set length will be
    // the same as the map len
    if log_enabled!(Level::Warn) {
        let mut seen_values = HashSet::with_capacity(postscript_names.len());
        let duplicate_values = postscript_names
            .values()
            .filter(|ps_name| !seen_values.insert((*ps_name).clone()))
            .collect::<BTreeSet<_>>();
        if !duplicate_values.is_empty() {
            warn!("public.postscriptNames: the following production names are used by multiple glyphs: {duplicate_values:?}");
        }
    }
    Ok(postscript_names)
}

fn units_per_em<'a>(font_infos: impl Iterator<Item = &'a norad::FontInfo>) -> Result<u16, Error> {
    const MIN_UPEM: f64 = 16.0;
    const MAX_UPEM: f64 = 16384.0;

    let mut upems: Vec<_> = font_infos
        .filter_map(|fi| match fi.units_per_em.map(|x| x.as_f64()) {
            None => None,
            // Per <https://learn.microsoft.com/en-us/typography/opentype/spec/head>, 16..16384
            Some(val) if (MIN_UPEM..=MAX_UPEM).contains(&val) => Some(Ok(val.ot_round())),
            Some(bad_val) => Some(Err(Error::InvalidUpem(bad_val))),
        })
        .collect::<Result<_, _>>()?;

    upems.sort_unstable();
    upems.dedup();
    // https://github.com/googlefonts/ufo2ft/blob/fca66fe3ea1ea88ffb36f8264b21ce042d3afd05/Lib/ufo2ft/fontInfoData.py#L359
    match upems.as_slice() {
        [] => Ok(1000),
        [one] => Ok(*one),
        _multiple => Err(Error::InconsistentUpem(upems)),
    }
}

fn files_identical(f1: &Path, f2: &Path) -> Result<bool, BadSource> {
    if !f1.is_file() {
        return Err(BadSource::new(f1, BadSourceKind::ExpectedFile));
    }
    if !f2.is_file() {
        return Err(BadSource::new(f2, BadSourceKind::ExpectedFile));
    }
    let m1 = f1
        .metadata()
        .map_err(|e| BadSource::new(f1, BadSourceKind::Io(e)))?;
    let m2 = f2
        .metadata()
        .map_err(|e| BadSource::new(f2, BadSourceKind::Io(e)))?;
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
) -> Result<HashMap<&'a String, norad::FontInfo>, BadSource> {
    let mut results = HashMap::new();
    for source in designspace.sources.iter() {
        let ufo_dir = designspace_dir.join(&source.filename);
        let data_request = norad::DataRequest::none();
        let font = norad::Font::load_requested_data(&ufo_dir, data_request)
            .map_err(|e| BadSource::custom(ufo_dir, e))?;
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
    builder.add_if_present(NameId::FAMILY_NAME, &font_info.style_map_family_name);
    builder.add_if_present(
        NameId::SUBFAMILY_NAME,
        &font_info.style_map_style_name.as_ref().map(|s| {
            match s {
                norad::fontinfo::StyleMapStyle::Regular => "Regular",
                norad::fontinfo::StyleMapStyle::Italic => "Italic",
                norad::fontinfo::StyleMapStyle::Bold => "Bold",
                norad::fontinfo::StyleMapStyle::BoldItalic => "Bold Italic",
            }
            .into()
        }),
    );

    builder.add_if_present(NameId::UNIQUE_ID, &font_info.open_type_name_unique_id);
    builder.add_if_present(NameId::VERSION_STRING, &font_info.open_type_name_version);
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
        NameId::TYPOGRAPHIC_FAMILY_NAME,
        &font_info
            .open_type_name_preferred_family_name
            .as_ref()
            .or(font_info.family_name.as_ref())
            .cloned(),
    );
    builder.add_if_present(
        NameId::TYPOGRAPHIC_SUBFAMILY_NAME,
        &font_info
            .open_type_name_preferred_subfamily_name
            .as_ref()
            .or(font_info.style_name.as_ref())
            .cloned(),
    );
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
    let vendor = font_info
        .open_type_os2_vendor_id
        .as_deref()
        .unwrap_or(DEFAULT_VENDOR_ID);
    builder.apply_default_fallbacks(vendor);

    // Name's that don't get individual fields
    if let Some(name_records) = font_info.open_type_name_records.as_ref() {
        for nr in name_records.iter() {
            let name_id: u16 = nr.name_id.try_into().unwrap();
            builder.add(name_id.into(), nr.string.clone());
        }
    }

    builder.into_inner()
}

/// <https://unifiedfontobject.org/versions/ufo3/fontinfo.plist/#opentype-head-table-fields>
fn try_parse_date(raw_date: Option<&String>) -> Option<DateTime<Utc>> {
    let raw_date = raw_date?;
    let parse_result =
        NaiveDateTime::parse_from_str(raw_date, "%Y/%m/%d %H:%M:%S").map(|nd| nd.and_utc());
    if let Err(e) = parse_result {
        warn!("Invalid date string {}: {e}", raw_date);
    }
    parse_result.ok()
}

impl Work<Context, WorkId, Error> for StaticMetadataWork {
    fn id(&self) -> WorkId {
        WorkId::StaticMetadata
    }

    fn also_completes(&self) -> Vec<WorkId> {
        vec![WorkId::PreliminaryGlyphOrder]
    }

    fn exec(&self, context: &Context) -> Result<(), Error> {
        debug!("Static metadata for {:#?}", self.designspace_or_ufo);
        let designspace_dir = self.designspace_dir.as_ref();
        let Some((_, default_master)) = default_master(&self.designspace) else {
            return Err(Error::NoDefaultMaster(
                self.designspace_or_ufo.to_path_buf(),
            ));
        };
        let font_infos = font_infos(designspace_dir, &self.designspace)?;
        let font_info_at_default = font_infos.get(&default_master.filename).ok_or_else(|| {
            BadSource::new(
                designspace_dir.join(&default_master.filename),
                BadSourceKind::ExpectedFile,
            )
        })?;

        let units_per_em = units_per_em(font_infos.values())?;
        let names = names(font_info_at_default);
        let axes = to_ir_axes(&self.designspace.axes)?;

        let tags_by_name = axes.iter().map(|a| (a.name.as_str(), a.tag)).collect();
        let axes_by_tag = axes.iter().map(|a| (a.tag, a)).collect();
        let family_prefix = names
            .get(&NameKey::new_bmp_only(NameId::FAMILY_NAME))
            .map(|name| name.clone() + " ")
            .unwrap_or_default();
        let named_instances = self
            .designspace
            .instances
            .iter()
            .map(|inst| {
                // TODO: Also support localised names, and names inferred from axis labels
                // (also used to build STAT table)
                NamedInstance {
                    name: inst.stylename.clone().unwrap_or_else(|| {
                        match inst
                            .name
                            .as_ref()
                            .unwrap()
                            .strip_prefix(family_prefix.as_str())
                        {
                            Some(tail) => tail.to_string(),
                            None => inst.name.clone().unwrap(),
                        }
                    }),
                    location: to_design_location(&tags_by_name, &inst.location)
                        .to_user(&axes_by_tag),
                }
            })
            .collect();

        let global_locations = master_locations(
            &axes,
            self.designspace
                .sources
                .iter()
                .filter(|s| !is_glyph_only(s)),
        )
        .into_values()
        .collect();

        let lib_plist =
            match load_plist(&designspace_dir.join(&default_master.filename), "lib.plist") {
                Ok(lib_plist) => lib_plist,
                Err(BadSource {
                    kind: BadSourceKind::ExpectedFile,
                    ..
                }) => Default::default(),
                Err(e) => return Err(e)?,
            };
        let glyph_order = glyph_order(&lib_plist, &self.glyph_names)?;
        let glyph_categories = glyph_categories(&lib_plist).map(|categories| GdefCategories {
            categories,
            prefer_gdef_categories_in_fea: true,
        })?;

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

        let postscript_names = if context.flags.contains(Flags::PRODUCTION_NAMES) {
            postscript_names(&lib_plist)?
        } else {
            PostscriptNames::default()
        };

        // https://github.com/googlefonts/ufo2ft/blob/0d2688cd847d003b41104534d16973f72ef26c40/Lib/ufo2ft/fontInfoData.py#L360
        let italic_angle = font_info_at_default.italic_angle.unwrap_or(0.0);

        let mut static_metadata = StaticMetadata::new(
            units_per_em,
            names,
            axes,
            named_instances,
            global_locations,
            postscript_names,
            italic_angle,
            glyph_categories,
            None,
        )
        .map_err(Error::VariationModelError)?;
        static_metadata.misc.selection_flags = selection_flags;
        if let Some(vendor_id) = font_info_at_default
            .open_type_os2_vendor_id
            .as_ref()
            // treat "    " (four spaces) as equivalent to no value; it means
            // 'null', per the spec
            .filter(|id| *id != "    ")
        {
            static_metadata.misc.vendor_id =
                Tag::from_str(vendor_id).map_err(|cause| Error::InvalidTag {
                    raw_tag: vendor_id.to_owned(),
                    cause,
                })?;
        }

        static_metadata.misc.us_weight_class = font_info_at_default
            .open_type_os2_weight_class
            .map(|v| v as u16);
        static_metadata.misc.us_width_class = font_info_at_default
            .open_type_os2_width_class
            .map(|v| v as u16);

        // <https://github.com/googlefonts/glyphsLib/blob/cb8a4a914b0a33431f0a77f474bf57eec2f19bcc/Lib/glyphsLib/builder/custom_params.py#L1117-L1119>
        static_metadata.misc.fs_type = Some(
            font_info_at_default
                .open_type_os2_type
                .as_ref()
                .map(|flags| flags.iter().fold(0_u16, |acc, e| acc | (1 << *e)))
                .unwrap_or(1_u16 << 2),
        );

        static_metadata.misc.is_fixed_pitch = font_info_at_default.postscript_is_fixed_pitch;

        static_metadata.misc.unicode_range_bits = font_info_at_default
            .open_type_os2_unicode_ranges
            .as_ref()
            .map(|bits| bits.iter().map(|b| *b as u32).collect());
        static_metadata.misc.codepage_range_bits = font_info_at_default
            .open_type_os2_code_page_ranges
            .as_ref()
            .map(|bits| bits.iter().map(|b| *b as u32).collect());

        if let Some(ot_panose) = &font_info_at_default.open_type_os2_panose {
            static_metadata.misc.panose = Some(Panose {
                family_type: ot_panose.family_type as u8,
                serif_style: ot_panose.serif_style as u8,
                weight: ot_panose.weight as u8,
                proportion: ot_panose.proportion as u8,
                contrast: ot_panose.contrast as u8,
                stroke_variation: ot_panose.stroke_variation as u8,
                arm_style: ot_panose.arm_style as u8,
                letterform: ot_panose.letterform as u8,
                midline: ot_panose.midline as u8,
                x_height: ot_panose.x_height as u8,
            });
        }

        static_metadata.misc.version_major = font_info_at_default
            .version_major
            .unwrap_or(static_metadata.misc.version_major);
        static_metadata.misc.version_minor = font_info_at_default
            .version_minor
            .unwrap_or(static_metadata.misc.version_minor);
        static_metadata.misc.lowest_rec_ppm = font_info_at_default
            .open_type_head_lowest_rec_ppem
            .map(|v| v as u16)
            .unwrap_or(static_metadata.misc.lowest_rec_ppm);
        static_metadata.misc.head_flags = font_info_at_default
            .open_type_head_flags
            .as_ref()
            .map(|bit_indices| bit_indices.iter().map(|i| 1 << i).fold(0, |acc, e| acc | e))
            .unwrap_or(static_metadata.misc.head_flags);

        static_metadata.misc.family_class = font_info_at_default
            .open_type_os2_family_class
            .as_ref()
            .map(|v| ((v.class_id as i16) << 8) | v.subclass_id as i16);

        static_metadata.misc.created =
            try_parse_date(font_info_at_default.open_type_head_created.as_ref())
                .or(static_metadata.misc.created);

        static_metadata.misc.meta_table = lib_plist
            .get("public.openTypeMeta")
            .and_then(parse_meta_table_values);

        if let Some(gasp_records) = font_info_at_default.open_type_gasp_range_records.as_ref() {
            static_metadata.misc.gasp = gasp_records
                .iter()
                .map(|g| GaspRange {
                    range_max_ppem: g.range_max_ppem as u16,
                    range_gasp_behavior: GaspRangeBehavior::from_bits_truncate(
                        g.range_gasp_behavior
                            .iter()
                            .map(|b| 1u16 << (*b as u8))
                            .fold(0u16, |acc, e| acc | e),
                    ),
                })
                .collect();
        }

        context.preliminary_glyph_order.set(glyph_order);
        context.static_metadata.set(static_metadata);
        Ok(())
    }
}

fn is_glyph_only(source: &norad::designspace::Source) -> bool {
    // Sources that use layer= specifically should not contribute metrics, only glyphs
    source.layer.is_some()
}

fn parse_meta_table_values(plist: &plist::Value) -> Option<MetaTableValues> {
    let plist = plist.as_dictionary()?;
    let mut ret = MetaTableValues::default();
    for (key, value) in plist {
        match key.as_str() {
            "dlng" => ret.dlng = parse_meta_scriptlangtags(value).map(Into::into).collect(),
            "slng" => ret.slng = parse_meta_scriptlangtags(value).map(Into::into).collect(),
            other => log::warn!("unhandled meta table tag '{other}'"),
        }
    }
    if ret.dlng.len() + ret.slng.len() > 0 {
        Some(ret)
    } else {
        None
    }
}

fn parse_meta_scriptlangtags(plist: &plist::Value) -> impl Iterator<Item = &str> {
    plist
        .as_array()
        .into_iter()
        .flatten()
        .filter_map(|val| val.as_string())
        // although the spec[1] says that this should be an array of strs and each
        // str should be a scriptlangtag, in practice[2] it seems like it often
        // ends up stored as a single comma-separated list of scriptlangtag
        //
        // 1: https://unifiedfontobject.org/versions/ufo3/lib.plist/#publicopentypemeta
        // 2: https://github.com/aaronbell/LxgwWenkaiTC/blob/fe7a4b88e91a02c097d69ba/sources/LXGWWenKaiTC-Regular.ufo/lib.plist#L1798-L1808
        .flat_map(|s| s.split(','))
        .map(str::trim)
}

fn set_default_underline_pos(
    metrics: &mut GlobalMetrics,
    location: &NormalizedLocation,
    units_per_em: u16,
) {
    // ufo2ft default underline position differs from fontir/Glyphs.app's so
    // we override it here.
    // https://github.com/googlefonts/ufo2ft/blob/a421acc/Lib/ufo2ft/fontInfoData.py#L319-L322
    metrics.set_if_absent(
        GlobalMetric::UnderlinePosition,
        location.clone(),
        -0.075 * units_per_em as f64,
    );
}

fn populate_default_metrics(
    metrics: &mut GlobalMetrics,
    location: &NormalizedLocation,
    units_per_em: u16,
    x_height: Option<f64>,
    ascender: Option<f64>,
    descender: Option<f64>,
    italic_angle: Option<f64>,
) {
    set_default_underline_pos(metrics, location, units_per_em);
    metrics.populate_defaults(
        location,
        units_per_em,
        x_height,
        ascender,
        descender,
        italic_angle,
    );
}

impl Work<Context, WorkId, Error> for GlobalMetricsWork {
    fn id(&self) -> WorkId {
        WorkId::GlobalMetrics
    }

    fn read_access(&self) -> Access<WorkId> {
        Access::Variant(WorkId::StaticMetadata)
    }

    fn exec(&self, context: &Context) -> Result<(), Error> {
        debug!("Global metrics for {:#?}", self.designspace_or_ufo);
        let static_metadata = context.static_metadata.get();

        let designspace_dir = self.designspace_dir.as_ref();
        let font_infos = font_infos(designspace_dir, &self.designspace)?;
        let master_locations =
            master_locations(&static_metadata.all_source_axes, &self.designspace.sources);

        let mut metrics = GlobalMetrics::new();

        for source in self
            .designspace
            .sources
            .iter()
            .filter(|s| !is_glyph_only(s))
        {
            let pos = master_locations.get(source.name.as_ref().unwrap()).unwrap();
            let font_info = font_infos.get(&source.filename).ok_or_else(|| {
                BadSource::new(
                    designspace_dir.join(&source.filename),
                    BadSourceKind::ExpectedFile,
                )
            })?;

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
            metrics.set_if_some(
                GlobalMetric::StrikeoutPosition,
                pos.clone(),
                font_info.open_type_os2_strikeout_position.map(|v| v as f64),
            );
            metrics.set_if_some(
                GlobalMetric::StrikeoutSize,
                pos.clone(),
                font_info.open_type_os2_strikeout_size.map(|v| v as f64),
            );
            metrics.set_if_some(
                GlobalMetric::SubscriptXOffset,
                pos.clone(),
                font_info.open_type_os2_subscript_x_offset.map(|v| v as f64),
            );
            metrics.set_if_some(
                GlobalMetric::SubscriptXSize,
                pos.clone(),
                font_info.open_type_os2_subscript_x_size.map(|v| v as f64),
            );
            metrics.set_if_some(
                GlobalMetric::SubscriptYOffset,
                pos.clone(),
                font_info.open_type_os2_subscript_y_offset.map(|v| v as f64),
            );
            metrics.set_if_some(
                GlobalMetric::SubscriptYSize,
                pos.clone(),
                font_info.open_type_os2_subscript_y_size.map(|v| v as f64),
            );
            metrics.set_if_some(
                GlobalMetric::SuperscriptXOffset,
                pos.clone(),
                font_info
                    .open_type_os2_superscript_x_offset
                    .map(|v| v as f64),
            );
            metrics.set_if_some(
                GlobalMetric::SuperscriptXSize,
                pos.clone(),
                font_info.open_type_os2_superscript_x_size.map(|v| v as f64),
            );
            metrics.set_if_some(
                GlobalMetric::SuperscriptYOffset,
                pos.clone(),
                font_info
                    .open_type_os2_superscript_y_offset
                    .map(|v| v as f64),
            );
            metrics.set_if_some(
                GlobalMetric::SuperscriptYSize,
                pos.clone(),
                font_info.open_type_os2_superscript_y_size.map(|v| v as f64),
            );
            metrics.set_if_some(
                GlobalMetric::HheaAscender,
                pos.clone(),
                font_info.open_type_hhea_ascender.map(|v| v as f64),
            );
            metrics.set_if_some(
                GlobalMetric::HheaDescender,
                pos.clone(),
                font_info.open_type_hhea_descender.map(|v| v as f64),
            );
            metrics.set_if_some(
                GlobalMetric::HheaLineGap,
                pos.clone(),
                font_info.open_type_hhea_line_gap.map(|v| v as f64),
            );
            metrics.set_if_some(
                GlobalMetric::CaretSlopeRise,
                pos.clone(),
                font_info.open_type_hhea_caret_slope_rise.map(|v| v as f64),
            );
            metrics.set_if_some(
                GlobalMetric::CaretSlopeRun,
                pos.clone(),
                font_info.open_type_hhea_caret_slope_run.map(|v| v as f64),
            );
            metrics.set_if_some(
                GlobalMetric::CaretOffset,
                pos.clone(),
                font_info.open_type_hhea_caret_offset.map(|v| v as f64),
            );
            metrics.set_if_some(
                GlobalMetric::UnderlineThickness,
                pos.clone(),
                font_info.postscript_underline_thickness,
            );
            metrics.set_if_some(
                GlobalMetric::UnderlinePosition,
                pos.clone(),
                font_info.postscript_underline_position,
            );

            populate_default_metrics(
                &mut metrics,
                pos,
                static_metadata.units_per_em,
                font_info.x_height,
                font_info.ascender,
                font_info.descender,
                font_info.italic_angle,
            );
        }

        trace!("{:#?}", metrics);
        context.global_metrics.set(metrics);
        Ok(())
    }
}

impl Work<Context, WorkId, Error> for FeatureWork {
    fn id(&self) -> WorkId {
        WorkId::Features
    }

    fn exec(&self, context: &Context) -> Result<(), Error> {
        debug!("Features for {:#?}", self.designspace_or_ufo);

        let fea_files = self.fea_files.as_ref();
        for fea_file in fea_files.iter().skip(1) {
            if !files_identical(&fea_files[0], fea_file)? {
                warn!("Bailing out due to non-identical feature files. This is an unnecessary limitation.");
                return Err(Error::NonIdenticalFea(
                    fea_files[0].to_path_buf(),
                    fea_file.to_path_buf(),
                ));
            }
        }

        if !fea_files.is_empty() {
            // Fea file is required to be ufo_dir/features.fea. Includes resolve as siblings
            // of ufo_dir.
            let fea_file = fea_files[0].clone();
            let include_dir = fea_file
                .parent()
                .and_then(|f| f.parent())
                .map(|v| v.to_path_buf())
                .ok_or_else(|| BadSource::new(&fea_file, BadSourceKind::ExpectedParent))?;
            context
                .features
                .set(FeaturesSource::from_file(fea_file, Some(include_dir)));
        } else {
            context.features.set(FeaturesSource::empty());
        }

        Ok(())
    }
}

fn kerning_groups_for(
    designspace_dir: &Path,
    glyph_order: &GlyphOrder,
    source: &norad::designspace::Source,
) -> Result<BTreeMap<KernGroup, BTreeSet<GlyphName>>, Error> {
    let ufo_dir = designspace_dir.join(&source.filename);
    let data_request = norad::DataRequest::none().groups(true);
    Ok(norad::Font::load_requested_data(&ufo_dir, data_request)
        .map_err(|e| BadSource::custom(ufo_dir, e))?
        .groups
        .into_iter()
        .filter_map(|(group_name, entries)| {
            KernGroup::from_group_name(group_name.as_str())
                .filter(|_| !entries.is_empty())
                .map(|name| (name, entries))
        })
        .filter_map(|(group_name, entries)| {
            let members: BTreeSet<_> = entries
                .into_iter()
                .filter_map(|glyph_name| {
                    let glyph_name = GlyphName::new(glyph_name);
                    if glyph_order.contains(&glyph_name) {
                        Some(glyph_name)
                    } else {
                        debug!(
                            "{} kerning group '{}' references non-existent glyph '{}'; ignoring",
                            source.filename, group_name, glyph_name
                        );
                        None
                    }
                })
                .collect();
            if !members.is_empty() {
                Some((group_name, members))
            } else {
                None
            }
        })
        .collect())
}

/// UFO specific behaviour for kern groups
trait KernGroupExt {
    fn from_group_name(name: &str) -> Option<KernGroup>;

    // used when computing the 'reverse groups'
    fn side_ord(&self) -> u8;
}

impl KernGroupExt for KernGroup {
    fn from_group_name(name: &str) -> Option<KernGroup> {
        name.strip_prefix(UFO_KERN1_PREFIX)
            .map(|name| Self::Side1(name.into()))
            .or_else(|| {
                name.strip_prefix(UFO_KERN2_PREFIX)
                    .map(|name| Self::Side2(name.into()))
            })
    }

    fn side_ord(&self) -> u8 {
        match self {
            KernGroup::Side1(_) => 1,
            KernGroup::Side2(_) => 2,
        }
    }
}

/// See <https://github.com/googlefonts/ufo2ft/blob/3e0563814cf541f7d8ca2bb7f6e446328e0e5e76/Lib/ufo2ft/featureWriters/kernFeatureWriter.py#L302-L357>
impl Work<Context, WorkId, Error> for KerningGroupWork {
    fn id(&self) -> WorkId {
        WorkId::KerningGroups
    }

    fn read_access(&self) -> Access<WorkId> {
        AccessBuilder::new()
            .variant(WorkId::StaticMetadata)
            .variant(WorkId::GlyphOrder)
            .build()
    }

    fn exec(&self, context: &Context) -> Result<(), Error> {
        debug!("Kerning groups for {:#?}", self.designspace_or_ufo);

        let designspace_dir = self.designspace_dir.as_ref();
        let glyph_order = context.glyph_order.get();
        let static_metadata = context.static_metadata.get();
        let master_locations =
            master_locations(&static_metadata.all_source_axes, &self.designspace.sources);
        let Some((default_master_idx, default_master)) = default_master(&self.designspace) else {
            return Err(Error::NoDefaultMaster(
                self.designspace_or_ufo.to_path_buf(),
            ));
        };

        // Step 1: find the groups

        // Based on discussion on https://github.com/googlefonts/ufo2ft/pull/635, take the groups of
        // the default master as the authoritative source on groups
        let mut kerning_groups = KerningGroups {
            groups: kerning_groups_for(designspace_dir, glyph_order.as_ref(), default_master)?,
            ..Default::default()
        };

        // Group names are side-specific (public.kern1/2) but the set of glyph names isn't
        // so include side in the key to avoid matching the wrong side
        let reverse_groups: HashMap<_, _> = kerning_groups
            .groups
            .iter()
            .map(|(k, v)| ((k.side_ord(), v), k))
            .collect();

        // https://gist.github.com/madig/76567a9650de639bbff51ce010783790#file-align-groups-py-L21
        // claims some sources use different names for groups of the same glyphs in different masters
        // so we need to create a renaming map.
        kerning_groups.groups.keys().for_each(|name| {
            kerning_groups
                .old_to_new_group_names
                .insert(name.clone(), name.clone());
        });

        for (_, source) in self
            .designspace
            .sources
            .iter()
            .enumerate()
            .filter(|(idx, source)| !is_glyph_only(source) && *idx != default_master_idx)
        {
            for (name, entries) in &kerning_groups.groups {
                let Some(real_name) = reverse_groups.get(&(name.side_ord(), entries)) else {
                    warn!(
                        "{name} exists only in {} and will be ignored",
                        source.name.as_ref().unwrap()
                    );
                    continue;
                };
                if name == *real_name {
                    continue;
                }
                warn!(
                    "{name} in {} matches {real_name} in {} and will be renamed",
                    source.name.as_ref().unwrap(),
                    default_master.name.as_ref().unwrap()
                );
                kerning_groups
                    .old_to_new_group_names
                    .insert(name.to_owned(), (*real_name).clone());
            }
        }

        for source in self
            .designspace
            .sources
            .iter()
            .filter(|s| !is_glyph_only(s))
        {
            let pos = master_locations.get(source.name.as_ref().unwrap()).unwrap();
            kerning_groups.locations.insert(pos.clone());
        }

        context.kerning_groups.set(kerning_groups);
        Ok(())
    }
}

/// See <https://github.com/googlefonts/ufo2ft/blob/3e0563814cf541f7d8ca2bb7f6e446328e0e5e76/Lib/ufo2ft/featureWriters/kernFeatureWriter.py#L302-L357>
impl Work<Context, WorkId, Error> for KerningInstanceWork {
    fn id(&self) -> WorkId {
        WorkId::KernInstance(self.location.clone())
    }

    fn read_access(&self) -> Access<WorkId> {
        AccessBuilder::new()
            .variant(WorkId::StaticMetadata)
            .variant(WorkId::GlyphOrder)
            .variant(WorkId::KerningGroups)
            .build()
    }

    fn exec(&self, context: &Context) -> Result<(), Error> {
        debug!(
            "Kerning for {:#?} at {:?}",
            self.designspace_or_ufo, self.location
        );

        let designspace_dir = self.designspace_dir.as_ref();
        let static_metadata = context.static_metadata.get();
        let glyph_order = context.glyph_order.get();
        let groups = context.kerning_groups.get();
        let master_locations =
            master_locations(&static_metadata.all_source_axes, &self.designspace.sources);

        // We know all the groups, read all the kerning and update group naming
        let source = self
            .designspace
            .sources
            .iter()
            .find(|s| {
                !is_glyph_only(s)
                    && *master_locations.get(s.name.as_ref().unwrap()).unwrap() == self.location
            })
            .unwrap();

        let ufo_dir = designspace_dir.join(&source.filename);
        let data_request = norad::DataRequest::none().kerning(true);
        let font = norad::Font::load_requested_data(&ufo_dir, data_request)
            .map_err(|e| BadSource::custom(ufo_dir, e))?;

        let resolve = |name: &norad::Name, group_prefix: &str| {
            if let Some(group_name) = KernGroup::from_group_name(name.as_str()) {
                if !name.starts_with(group_prefix) {
                    warn!("'{name}' should have prefix {group_prefix}; ignored");
                    return None;
                }
                if !groups.old_to_new_group_names.contains_key(&group_name) {
                    warn!("'{name}' is not a valid group name; ignored");
                    return None;
                }
                Some(KernSide::Group(group_name))
            } else {
                let glyph_name = GlyphName::from(name.as_str());
                if !glyph_order.contains(&glyph_name) {
                    warn!("'{name}' refers to a non-existent glyph; ignored");
                    return None;
                }
                Some(KernSide::Glyph(glyph_name))
            }
        };

        let mut kerns = KerningInstance {
            location: self.location.clone(),
            ..Default::default()
        };
        for (side1, side2, adjustment) in font.kerning.into_iter().flat_map(|(side1, kerns)| {
            kerns
                .into_iter()
                .map(move |(side2, adjustment)| (side1.clone(), side2, adjustment))
        }) {
            let (Some(side1), Some(side2)) = (
                resolve(&side1, UFO_KERN1_PREFIX),
                resolve(&side2, UFO_KERN2_PREFIX),
            ) else {
                warn!(
                    "{} kerning unable to resolve at least one of '{}', '{}'; ignoring",
                    source.name.as_ref().unwrap(),
                    side1.as_str(),
                    side2.as_str()
                );
                continue;
            };

            *kerns.kerns.entry((side1, side2)).or_default() = adjustment.into();
        }

        debug!("{:?} has {} kern entries", self.location, kerns.kerns.len());
        context.kerning_at.set(kerns);
        Ok(())
    }
}

#[derive(Debug)]
struct GlyphIrWork {
    glyph_name: GlyphName,
    export: bool,
    glif_files: HashMap<PathBuf, Vec<DesignLocation>>,
}

impl Work<Context, WorkId, Error> for GlyphIrWork {
    fn id(&self) -> WorkId {
        WorkId::Glyph(self.glyph_name.clone())
    }

    fn read_access(&self) -> Access<WorkId> {
        Access::Variant(WorkId::StaticMetadata)
    }

    fn write_access(&self) -> Access<WorkId> {
        AccessBuilder::new()
            .specific_instance(WorkId::Glyph(self.glyph_name.clone()))
            .specific_instance(WorkId::Anchor(self.glyph_name.clone()))
            .build()
    }

    fn also_completes(&self) -> Vec<WorkId> {
        vec![WorkId::Anchor(self.glyph_name.clone())]
    }

    fn exec(&self, context: &Context) -> Result<(), Error> {
        trace!(
            "Generate glyph IR for {:?} from {:#?}",
            self.glyph_name,
            self.glif_files
        );
        let static_metadata = context.static_metadata.get();

        // Migrate glif_files into internal coordinates
        let axes_by_name = static_metadata
            .all_source_axes
            .iter()
            .map(|a| (a.tag, a))
            .collect();
        let mut glif_files = HashMap::new();
        for (path, design_locations) in self.glif_files.iter() {
            let normalized_locations: Vec<NormalizedLocation> = design_locations
                .iter()
                .map(|dl| dl.to_normalized(&axes_by_name))
                .collect();
            glif_files.insert(path, normalized_locations);
        }

        let mut ir_anchors = AnchorBuilder::new(self.glyph_name.clone());
        let glyph_ir = to_ir_glyph(
            self.glyph_name.clone(),
            self.export,
            &glif_files,
            &mut ir_anchors,
        )?;

        context.anchors.set(ir_anchors.build()?);
        context.glyphs.set(glyph_ir);
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::{
        collections::{HashMap, HashSet},
        path::{Path, PathBuf},
    };

    use fontdrasil::{
        coords::{DesignCoord, DesignLocation, NormalizedCoord, NormalizedLocation, UserCoord},
        orchestration::{Access, AccessBuilder},
        types::GlyphName,
    };
    use fontir::{
        ir::{
            test_helpers::Round2, AnchorKind, GlobalMetricsInstance, GlyphOrder, NameKey,
            PostscriptNames,
        },
        orchestration::{Context, Flags, WorkId},
        paths::Paths,
        source::Source,
    };
    use norad::designspace;

    use pretty_assertions::assert_eq;
    use write_fonts::types::NameId;

    use crate::{
        source::{font_infos, names},
        toir::to_design_location,
    };

    use super::*;

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

    fn load_designspace(name: &str) -> DesignSpaceIrSource {
        DesignSpaceIrSource::new(&testdata_dir().join(name)).unwrap()
    }

    fn default_test_flags() -> Flags {
        Flags::default() - Flags::EMIT_IR
    }

    fn build_static_metadata(name: &str, flags: Flags) -> (impl Source, Context) {
        let _ = env_logger::builder().is_test(true).try_init();
        let source = load_designspace(name);
        let context = Context::new_root(flags, Paths::new(Path::new("/nothing/should/write/here")));
        let task_context = context.copy_for_work(
            Access::None,
            AccessBuilder::new()
                .variant(WorkId::StaticMetadata)
                .variant(WorkId::PreliminaryGlyphOrder)
                .build(),
        );
        source
            .create_static_metadata_work()
            .unwrap()
            .exec(&task_context)
            .unwrap();
        (source, context)
    }

    fn build_glyph_order(context: &Context) {
        let task_context = context.copy_for_work(
            Access::Variant(WorkId::PreliminaryGlyphOrder),
            Access::Variant(WorkId::GlyphOrder),
        );
        task_context
            .glyph_order
            .set((*task_context.preliminary_glyph_order.get()).clone());
    }

    fn build_global_metrics(name: &str) -> (impl Source, Context) {
        let (source, context) = build_static_metadata(name, default_test_flags());
        let task_context = context.copy_for_work(
            Access::Variant(WorkId::StaticMetadata),
            Access::Variant(WorkId::GlobalMetrics),
        );
        source
            .create_global_metric_work()
            .unwrap()
            .exec(&task_context)
            .unwrap();
        (source, context)
    }

    fn build_kerning(name: &str) -> (impl Source, Context) {
        let (source, context) = build_static_metadata(name, default_test_flags());

        // static metadata includes preliminary glyph order; just copy it to be the final one
        context
            .copy_for_work(
                Access::Variant(WorkId::PreliminaryGlyphOrder),
                Access::Variant(WorkId::GlyphOrder),
            )
            .glyph_order
            .set((*context.preliminary_glyph_order.get()).clone());

        let work = source.create_kerning_group_ir_work().unwrap();
        work.exec(&context.copy_for_work(work.read_access(), work.write_access()))
            .unwrap();

        for location in context.kerning_groups.get().locations.iter() {
            let work = source
                .create_kerning_instance_ir_work(location.clone())
                .unwrap();
            work.exec(&context.copy_for_work(work.read_access(), work.write_access()))
                .unwrap();
        }

        (source, context)
    }

    fn build_glyphs(name: &str) -> (impl Source, Context) {
        let (source, context) = build_static_metadata(name, default_test_flags());
        build_glyph_order(&context);

        let task_context = context.copy_for_work(Access::All, Access::All);
        source
            .create_glyph_ir_work()
            .unwrap()
            .into_iter()
            .for_each(|w| w.exec(&task_context).unwrap());

        (source, context)
    }

    fn load_wght_var() -> DesignSpaceIrSource {
        load_designspace("wght_var.designspace")
    }

    fn add_design_location(
        add_to: &mut HashMap<PathBuf, Vec<DesignLocation>>,
        glif_file: &str,
        tag: Tag,
        pos: f64,
    ) {
        let mut loc = DesignLocation::new();
        loc.insert(tag, DesignCoord::new(pos));
        add_to
            .entry(testdata_dir().join(glif_file))
            .or_default()
            .push(loc);
    }

    #[test]
    pub fn create_work() {
        let ir_source = load_wght_var();

        let work = ir_source
            .create_work_for_one_glyph(&"bar".into(), true)
            .unwrap();

        let mut expected_glif_files = HashMap::new();
        add_design_location(
            &mut expected_glif_files,
            "WghtVar-Regular.ufo/glyphs/bar.glif",
            Tag::new(b"wght"),
            400.0,
        );
        add_design_location(
            &mut expected_glif_files,
            "WghtVar-Regular.ufo/glyphs.{600}/bar.glif",
            Tag::new(b"wght"),
            600.0,
        );
        add_design_location(
            &mut expected_glif_files,
            "WghtVar-Bold.ufo/glyphs/bar.glif",
            Tag::new(b"wght"),
            700.0,
        );
        assert_eq!(expected_glif_files, work.glif_files);
    }

    #[test]
    pub fn create_sparse_work() {
        let ir_source = load_wght_var();

        let work = ir_source
            .create_work_for_one_glyph(&"plus".into(), true)
            .unwrap();

        // Note there is NOT a glyphs.{600} version of plus
        let mut expected_glif_files = HashMap::new();
        add_design_location(
            &mut expected_glif_files,
            "WghtVar-Regular.ufo/glyphs/plus.glif",
            Tag::new(b"wght"),
            400.0,
        );
        add_design_location(
            &mut expected_glif_files,
            "WghtVar-Bold.ufo/glyphs/plus.glif",
            Tag::new(b"wght"),
            700.0,
        );
        assert_eq!(expected_glif_files, work.glif_files);
    }

    #[test]
    pub fn only_glyphs_present_in_default() {
        let source = load_wght_var();
        // bonus_bar is not present in the default master; should discard
        assert!(!source.glyphs.contains_key("bonus_bar"));
    }

    #[test]
    pub fn find_default_master() {
        let source = load_wght_var();
        let tag = Tag::new(b"wght");
        let mut loc = DesignLocation::new();
        loc.insert(tag, DesignCoord::new(400.0));
        let tags_by_name = HashMap::from([("Weight", tag)]);
        assert_eq!(
            loc,
            to_design_location(
                &tags_by_name,
                &default_master(&source.designspace).unwrap().1.location
            )
        );
    }

    #[test]
    pub fn builds_glyph_order_for_wght_var() {
        // Only WghtVar-Regular.ufo has a lib.plist, and it only lists a subset of glyphs
        // Should still work.
        let source = load_wght_var();
        let (_, default_master) = default_master(&source.designspace).unwrap();
        let lib_plist = load_plist(
            &source.designspace_dir.join(&default_master.filename),
            "lib.plist",
        )
        .unwrap();
        let go = glyph_order(
            &lib_plist,
            &HashSet::from(["bar".into(), "plus".into(), "an-imaginary-one".into()]),
        )
        .unwrap();
        // lib.plist specifies plus, so plus goes first and then the rest in alphabetical order
        let expected: GlyphOrder = ["plus", "an-imaginary-one", "bar"]
            .iter()
            .map(|s| (*s).into())
            .collect();
        assert_eq!(expected, go);
    }

    #[test]
    pub fn builds_default_glyph_order() {
        let go = glyph_order(
            &Default::default(),
            &HashSet::from([
                "bar".into(),
                "plus".into(),
                "an-imaginary-one".into(),
                ".notdef".into(),
            ]),
        )
        .unwrap();
        // no 'public.glyphOrder' in lib.plist, all glyphs in alphabetical order, except '.notdef'
        let expected: GlyphOrder = [".notdef", "an-imaginary-one", "bar", "plus"]
            .iter()
            .map(|s| (*s).into())
            .collect();
        assert_eq!(expected, go);
    }

    #[test]
    pub fn fetches_upem() {
        let source = load_wght_var();
        let font_infos = font_infos(&source.designspace_dir, &source.designspace).unwrap();
        assert_eq!(1000, units_per_em(font_infos.values()).unwrap());
    }

    #[test]
    pub fn ot_rounds_upem() {
        let source = load_designspace("float.designspace");
        let font_infos = font_infos(&source.designspace_dir, &source.designspace).unwrap();
        assert_eq!(
            256, // 255.5 rounded toward +infinity
            units_per_em(font_infos.values()).unwrap()
        );
    }

    #[test]
    pub fn default_names_for_minimal() {
        let source = load_designspace("float.designspace");
        let font_info = font_infos(&source.designspace_dir, &source.designspace)
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
                    String::from("0.000;NONE;NewFont-Regular")
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
                    String::from("NewFont-Regular")
                ),
            ],
            names
        );
    }

    #[test]
    fn captures_global_metrics_from_ints() {
        let (_, context) = build_global_metrics("static.designspace");
        let static_metadata = &context.static_metadata.get();
        let default_metrics = context
            .global_metrics
            .get()
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
        let static_metadata = &context.static_metadata.get();
        let default_metrics = context
            .global_metrics
            .get()
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
        let (_, context) = build_static_metadata("fontinfo.designspace", default_test_flags());
        assert_eq!(
            Tag::new(b"RODS"),
            context.static_metadata.get().misc.vendor_id
        );
    }

    #[test]
    fn captures_global_metrics() {
        let (_, context) = build_global_metrics("fontinfo.designspace");
        let static_metadata = &context.static_metadata.get();
        let default_metrics = context
            .global_metrics
            .get()
            .at(static_metadata.default_location());
        assert_eq!(
            GlobalMetricsInstance {
                pos: static_metadata.default_location().clone(),
                ascender: 737.0.into(),
                descender: (-42.0).into(),
                caret_slope_rise: 1000.0.into(),
                cap_height: 702.0.into(),
                x_height: 501.0.into(),
                subscript_x_size: 650.0.into(),
                subscript_y_size: 600.0.into(),
                subscript_y_offset: 75.0.into(),
                superscript_x_size: 650.0.into(),
                superscript_y_size: 600.0.into(),
                superscript_y_offset: 350.0.into(),
                strikeout_position: 300.6.into(),
                strikeout_size: 50.0.into(),
                os2_typo_ascender: 1193.0.into(),
                os2_typo_descender: (-289.0).into(),
                os2_typo_line_gap: 42.0.into(),
                os2_win_ascent: 1325.0.into(),
                os2_win_descent: 377.0.into(),
                hhea_ascender: 1194.0.into(),
                hhea_descender: (-290.0).into(),
                hhea_line_gap: 43.0.into(),
                underline_thickness: 50.0.into(),
                underline_position: (-75.0).into(),
                ..Default::default()
            },
            default_metrics.round2()
        );
    }

    fn only_coord(loc: &NormalizedLocation) -> NormalizedCoord {
        assert_eq!(1, loc.axis_tags().count());
        *loc.iter().next().unwrap().1
    }

    // Was tripping up on wght_var having two <source> with the same filename, different name and xvalue
    #[test]
    fn global_locations() {
        let (_, context) = build_static_metadata("wght_var.designspace", default_test_flags());
        let static_metadata = &context.static_metadata.get();
        let wght = static_metadata.axes.first().unwrap();

        assert_eq!(
            vec![
                (UserCoord::new(400.0), NormalizedCoord::new(0.0)),
                // this intermediate/sparse layer location is omitted from the global model
                // because it only contributes glyphs, not metrics or kerning
                // (UserCoord::new(600.0), NormalizedCoord::new(0.6666667,)),
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
        let static_metadata = &context.static_metadata.get();
        let wght = static_metadata.axes.first().unwrap();
        let mut metric_locations = context
            .global_metrics
            .get()
            .iter()
            .flat_map(|(.., values)| values.keys())
            .collect::<HashSet<_>>()
            .into_iter()
            .map(|loc| (only_coord(loc).to_user(&wght.converter), only_coord(loc)))
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

    #[test]
    fn groups_renamed_to_match_master() {
        let (_, context) = build_kerning("wght_var.designspace");
        let kerning = context.kerning_groups.get();

        let mut groups: Vec<_> = kerning
            .groups
            .iter()
            .map(|(name, entries)| {
                let mut entries: Vec<_> = entries.iter().map(|e| e.as_str()).collect();
                entries.sort();
                (name.clone(), entries)
            })
            .collect();
        groups.sort();

        assert_eq!(
            groups,
            vec![(KernGroup::Side1("correct_name".into()), vec!["bar", "plus"],),],
        );
    }

    #[test]
    fn captures_anchors() {
        let base_name = "A".into();
        let mark_name = "macroncomb".into();
        let (_, context) = build_glyphs("designspace_from_glyphs/WghtVar_Anchors.designspace");

        let base = context.anchors.get(&WorkId::Anchor(base_name));
        let mark = context.anchors.get(&WorkId::Anchor(mark_name));

        assert_eq!(
            vec![AnchorKind::Base("top".into())],
            base.anchors
                .iter()
                .map(|a| a.kind.clone())
                .collect::<Vec<_>>()
        );
        assert_eq!(
            vec![AnchorKind::Mark("top".into())],
            mark.anchors
                .iter()
                .map(|a| a.kind.clone())
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn postscript_names_happy_path() {
        // Given
        let lib_plist = {
            let mut outer = plist::Dictionary::new();
            let mut inner = plist::Dictionary::new();
            inner.extend([
                (String::from("foo"), String::from("bar").into()),
                (String::from("baz"), String::from("qux").into()),
                (String::from("lorem"), String::from("ipsum").into()),
            ]);
            outer.insert(String::from("public.postscriptNames"), inner.into());
            outer
        };

        // When
        let postscript_names =
            postscript_names(&lib_plist).expect("postscript names should be parsed");

        // Then
        assert_eq!(
            postscript_names,
            HashMap::from_iter([
                (GlyphName::from("foo"), GlyphName::from("bar")),
                (GlyphName::from("baz"), GlyphName::from("qux")),
                (GlyphName::from("lorem"), GlyphName::from("ipsum")),
            ]),
        );
    }

    #[test]
    fn postscript_names_not_dictionary() {
        // Given
        let lib_plist = {
            let mut outer = plist::Dictionary::new();
            outer.insert(
                String::from("public.postscriptNames"),
                plist::Value::Boolean(false),
            );
            outer
        };

        // When
        let err = postscript_names(&lib_plist).expect_err("postscript names should fail");

        // Then
        assert!(
            matches!(err.kind, BadSourceKind::Custom(_)),
            "incorrect error variant"
        );
    }

    #[test]
    fn postscript_names_drop_duplicates() {
        // Given
        let lib_plist = {
            let mut outer = plist::Dictionary::new();
            let mut inner = plist::Dictionary::new();
            inner.extend([
                (String::from("foo"), String::from("bar").into()),
                (String::from("foo"), String::from("qux").into()),
                (String::from("lorem"), String::from("ipsum").into()),
            ]);
            outer.insert(String::from("public.postscriptNames"), inner.into());
            outer
        };

        // When
        let postscript_names =
            postscript_names(&lib_plist).expect("postscript names should be parsed");

        // Then
        assert_eq!(
            postscript_names,
            HashMap::from_iter([
                (GlyphName::from("foo"), GlyphName::from("qux")),
                (GlyphName::from("lorem"), GlyphName::from("ipsum")),
            ]),
        );
    }

    #[test]
    fn postscript_names_drops_non_strings() {
        // Given
        let lib_plist = {
            let mut outer = plist::Dictionary::new();
            let mut inner = plist::Dictionary::new();
            inner.extend([
                (String::from("foo"), String::from("bar").into()),
                (String::from("baz"), 12f32.into()),
                (String::from("lorem"), true.into()),
            ]);
            outer.insert(String::from("public.postscriptNames"), inner.into());
            outer
        };

        // When
        let postscript_names =
            postscript_names(&lib_plist).expect("postscript names should be parsed");

        // Then
        assert_eq!(
            postscript_names,
            HashMap::from_iter([(GlyphName::from("foo"), GlyphName::from("bar"))]),
        );
    }

    #[test]
    fn postscript_names_allows_duplicate_values() {
        // Given
        let lib_plist = {
            let mut outer = plist::Dictionary::new();
            let mut inner = plist::Dictionary::new();
            inner.extend([
                (String::from("foo"), String::from("bar").into()),
                (String::from("baz"), String::from("bar").into()),
                (String::from("lorem"), String::from("bar").into()),
            ]);
            outer.insert(String::from("public.postscriptNames"), inner.into());
            outer
        };

        // When
        let postscript_names =
            postscript_names(&lib_plist).expect("postscript names should be parsed");

        // Then
        assert_eq!(
            postscript_names,
            HashMap::from_iter([
                (GlyphName::from("foo"), GlyphName::from("bar")),
                (GlyphName::from("baz"), GlyphName::from("bar")),
                (GlyphName::from("lorem"), GlyphName::from("bar")),
            ]),
        );
    }

    #[test]
    fn static_metadata_loads_postscript_names() {
        let (_, context) = build_static_metadata(
            "designspace_from_glyphs/WghtVar.designspace",
            default_test_flags(),
        );
        let static_metadata = context.static_metadata.get();

        assert_eq!(
            static_metadata.postscript_names,
            PostscriptNames::from_iter([(
                GlyphName::from("manual-component"),
                GlyphName::from("manualcomponent")
            )]),
        );
    }

    #[test]
    fn static_metadata_disable_postscript_names() {
        let no_production_names = default_test_flags() - Flags::PRODUCTION_NAMES;
        let (_, context) = build_static_metadata(
            "designspace_from_glyphs/WghtVar.designspace",
            no_production_names,
        );

        let static_metadata = context.static_metadata.get();
        assert!(static_metadata.postscript_names.is_empty());
    }

    #[test]
    fn reads_skip_export_glyphs() {
        let (_, context) = build_glyphs("designspace_from_glyphs/WghtVar_NoExport.designspace");
        let is_export = |name: &str| {
            context
                .glyphs
                .get(&WorkId::Glyph(name.into()))
                .emit_to_binary
        };
        assert_eq!(
            vec![true, false, true],
            vec![
                is_export("manual-component"),
                is_export("hyphen"),
                is_export("space"),
            ]
        );
    }

    fn assert_fs_type(src: &str, expected: u16) {
        let (_, context) = build_static_metadata(src, default_test_flags());
        let static_metadata = context.static_metadata.get();
        assert_eq!(Some(expected), static_metadata.misc.fs_type);
    }

    #[test]
    fn default_fs_type() {
        assert_fs_type("wght_var.designspace", 1 << 2);
    }

    #[test]
    fn obeys_explicit_fs_type() {
        assert_fs_type("MVAR.designspace", 1 << 3);
    }

    #[test]
    fn default_panose() {
        let (_, context) = build_static_metadata("static.designspace", default_test_flags());

        let static_metadata = context.static_metadata.get();
        assert_eq!(None, static_metadata.misc.panose);
    }

    // <https://github.com/googlefonts/fontc/issues/1026>
    #[test]
    fn obeys_explicit_panose() {
        let (_, context) = build_static_metadata("wght_var.designspace", default_test_flags());

        let static_metadata = context.static_metadata.get();
        let expected: Panose = [2_u8, 11, 5, 2, 4, 5, 4, 2, 2, 4].into();
        assert_eq!(Some(expected), static_metadata.misc.panose);
    }

    #[test]
    fn parse_meta_table_values() {
        let (_, context) = build_static_metadata("MetaTable.ufo", default_test_flags());
        let static_meta = context.static_metadata.get();
        let meta_table = static_meta.misc.meta_table.as_ref().unwrap();
        assert_eq!(meta_table.dlng, ["en-latn", "fr-latn", "nl-Latn"]);
        assert_eq!(meta_table.slng, ["Latn", "Cyrl"]);
    }

    #[test]
    fn ignore_empty_meta_table_values() {
        let mut plist = plist::Dictionary::new();
        plist.insert(
            "public.openTypeMeta".into(),
            plist::Value::Array(Default::default()),
        );

        assert!(super::parse_meta_table_values(&plist::Value::Dictionary(plist)).is_none())
    }

    fn fixed_pitch_of(name: &str) -> Option<bool> {
        let (_, context) = build_static_metadata(name, default_test_flags());
        let static_metadata = context.static_metadata.get();
        static_metadata.misc.is_fixed_pitch
    }

    #[test]
    fn allow_four_spaces_for_vendor_id() {
        let (_, context) = build_static_metadata("empty-vendorid.ufo", default_test_flags());
        let static_meta = context.static_metadata.get();
        assert_eq!(
            static_meta.misc.vendor_id.as_ref(),
            fontir::ir::DEFAULT_VENDOR_ID.as_bytes()
        )
    }

    #[test]
    fn fixed_pitch_on() {
        assert_eq!(Some(true), fixed_pitch_of("FixedPitch.designspace"));
    }

    #[test]
    fn fixed_pitch_off() {
        assert_eq!(None, fixed_pitch_of("wght_var.designspace"));
    }
}
