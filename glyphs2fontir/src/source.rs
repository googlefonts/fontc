use font_types::{NameId, Tag};
use fontdrasil::orchestration::Work;
use fontdrasil::types::GlyphName;
use fontir::coords::NormalizedCoord;
use fontir::error::{Error, WorkError};
use fontir::ir::{
    self, GlobalMetric, GlobalMetrics, GlyphInstance, NameBuilder, NameKey, StaticMetadata,
};
use fontir::orchestration::{Context, IrWork};
use fontir::source::{Input, Source};
use fontir::stateset::StateSet;
use glyphs_reader::Font;
use indexmap::IndexSet;
use log::{debug, trace, warn};
use std::collections::HashSet;
use std::str::FromStr;
use std::sync::Arc;
use std::{collections::HashMap, path::PathBuf};

use crate::toir::{to_ir_contours_and_components, to_ir_features, FontInfo};

pub struct GlyphsIrSource {
    glyphs_file: PathBuf,
    cache: Option<Cache>,
}

struct Cache {
    global_metadata: StateSet,
    font_info: Arc<FontInfo>,
}

impl Cache {
    fn is_valid_for(&self, global_metadata: &StateSet) -> bool {
        self.global_metadata == *global_metadata
    }
}

fn glyph_identifier(glyph_name: &str) -> String {
    format!("/glyph/{glyph_name}")
}

fn glyph_states(font: &Font) -> Result<HashMap<GlyphName, StateSet>, Error> {
    let mut glyph_states = HashMap::new();

    for (glyphname, glyph) in font.glyphs.iter() {
        let mut state = StateSet::new();
        state.track_memory(glyph_identifier(glyphname), glyph)?;
        glyph_states.insert(glyphname.into(), state);
    }

    Ok(glyph_states)
}

impl GlyphsIrSource {
    pub fn new(glyphs_file: PathBuf) -> GlyphsIrSource {
        GlyphsIrSource {
            glyphs_file,
            cache: None,
        }
    }
    fn feature_inputs(&self, font: &Font) -> Result<StateSet, Error> {
        let mut state = StateSet::new();
        state.track_memory("/features".to_string(), &font.features)?;
        Ok(state)
    }

    // When things like upem may have changed forget incremental and rebuild the whole thing
    fn static_metadata_inputs(&self, font: &Font) -> Result<StateSet, Error> {
        let mut state = StateSet::new();
        // Wipe out glyph-related fields, track the rest
        // Explicitly field by field so if we add more compiler will force us to update here
        let font = Font {
            units_per_em: font.units_per_em,
            axes: font.axes.clone(),
            masters: font.masters.clone(),
            default_master_idx: font.default_master_idx,
            glyphs: Default::default(),
            glyph_order: Default::default(),
            glyph_to_codepoints: Default::default(),
            axis_mappings: font.axis_mappings.clone(),
            features: Default::default(),
            names: Default::default(),
            version_major: Default::default(),
            version_minor: Default::default(),
        };
        state.track_memory("/font_master".to_string(), &font)?;
        Ok(state)
    }

    // Things that could change global metrics.
    fn global_metric_inputs(&self, font: &Font) -> Result<StateSet, Error> {
        let mut state = StateSet::new();
        // Wipe out fields that can't impact global metrics.
        // Explicitly field by field so if we add more compiler will force us to update here
        let font = Font {
            units_per_em: font.units_per_em,
            axes: font.axes.clone(),
            masters: font.masters.clone(),
            default_master_idx: font.default_master_idx,
            glyphs: Default::default(),
            glyph_order: Default::default(),
            glyph_to_codepoints: Default::default(),
            axis_mappings: Default::default(),
            features: Default::default(),
            names: Default::default(),
            version_major: Default::default(),
            version_minor: Default::default(),
        };
        state.track_memory("/font_master".to_string(), &font)?;
        Ok(state)
    }

    fn check_static_metadata(&self, global_metadata: &StateSet) -> Result<(), Error> {
        // Do we have a plist cache?
        // TODO: consider just recomputing here instead of failing
        if !self
            .cache
            .as_ref()
            .map(|pc| pc.is_valid_for(global_metadata))
            .unwrap_or(false)
        {
            return Err(Error::InvalidGlobalMetadata);
        }
        Ok(())
    }

    fn create_work_for_one_glyph(
        &self,
        glyph_name: GlyphName,
        font_info: Arc<FontInfo>,
    ) -> Result<GlyphIrWork, Error> {
        Ok(GlyphIrWork {
            glyph_name,
            font_info,
        })
    }
}

impl Source for GlyphsIrSource {
    fn inputs(&mut self) -> Result<Input, Error> {
        // We have to read the glyphs file then shred it to figure out if anything changed
        let font_info = FontInfo::try_from(Font::load(&self.glyphs_file).map_err(|e| {
            Error::ParseError(
                self.glyphs_file.clone(),
                format!("Unable to read glyphs file: {e}"),
            )
        })?)?;
        let font = &font_info.font;
        let static_metadata = self.static_metadata_inputs(font)?;
        let global_metrics = self.global_metric_inputs(font)?;
        let features = self.feature_inputs(font)?;
        let glyphs = glyph_states(font)?;

        self.cache = Some(Cache {
            global_metadata: static_metadata.clone(),
            font_info: Arc::new(font_info),
        });

        Ok(Input {
            static_metadata,
            global_metrics,
            glyphs,
            features,
        })
    }

    fn create_static_metadata_work(&self, input: &Input) -> Result<Box<IrWork>, Error> {
        self.check_static_metadata(&input.static_metadata)?;
        let font_info = self.cache.as_ref().unwrap().font_info.clone();
        let glyph_names = Arc::new(input.glyphs.keys().cloned().collect());

        Ok(Box::new(StaticMetadataWork {
            font_info,
            glyph_names,
        }))
    }

    fn create_global_metric_work(&self, input: &Input) -> Result<Box<IrWork>, Error> {
        self.check_static_metadata(&input.static_metadata)?;
        let font_info = self.cache.as_ref().unwrap().font_info.clone();
        Ok(Box::new(GlobalMetricWork { font_info }))
    }

    fn create_glyph_ir_work(
        &self,
        glyph_names: &IndexSet<GlyphName>,
        input: &Input,
    ) -> Result<Vec<Box<IrWork>>, fontir::error::Error> {
        self.check_static_metadata(&input.static_metadata)?;

        let cache = self.cache.as_ref().unwrap();

        let mut work: Vec<Box<IrWork>> = Vec::new();
        for glyph_name in glyph_names {
            work.push(Box::new(self.create_work_for_one_glyph(
                glyph_name.clone(),
                cache.font_info.clone(),
            )?));
        }
        Ok(work)
    }

    fn create_feature_ir_work(&self, input: &Input) -> Result<Box<IrWork>, Error> {
        self.check_static_metadata(&input.static_metadata)?;

        let cache = self.cache.as_ref().unwrap();

        Ok(Box::new(FeatureWork {
            font_info: cache.font_info.clone(),
        }))
    }
}

fn try_name_id(name: &str) -> Option<NameId> {
    match name {
        "copyrights" => Some(NameId::COPYRIGHT_NOTICE),
        "familyNames" => Some(NameId::FAMILY_NAME),
        "uniqueID" => Some(NameId::UNIQUE_ID),
        "postscriptFullName" => Some(NameId::FULL_NAME),
        "version" => Some(NameId::VERSION_STRING),
        "postscriptFontName" => Some(NameId::POSTSCRIPT_NAME),
        "trademarks" => Some(NameId::TRADEMARK),
        "manufacturers" => Some(NameId::MANUFACTURER),
        "designers" => Some(NameId::DESIGNER),
        "manufacturerURL" => Some(NameId::VENDOR_URL),
        "designerURL" => Some(NameId::DESIGNER_URL),
        "licenses" => Some(NameId::LICENSE_DESCRIPTION),
        "licenseURL" => Some(NameId::LICENSE_URL),
        "compatibleFullNames" => Some(NameId::COMPATIBLE_FULL_NAME),
        "sampleTexts" => Some(NameId::SAMPLE_TEXT),
        "WWSFamilyName" => Some(NameId::WWS_FAMILY_NAME),
        _ => {
            warn!("Unknown 'name' entry {name}");
            None
        }
    }
}

fn names(font: &Font) -> HashMap<NameKey, String> {
    let mut builder = NameBuilder::default();
    builder.set_version(font.version_major, font.version_minor);
    for (name, value) in font.names.iter() {
        if let Some(name_id) = try_name_id(name) {
            builder.add(name_id, value.clone());
        }
    }
    builder.apply_default_fallbacks();

    builder.into_inner()
}

struct StaticMetadataWork {
    font_info: Arc<FontInfo>,
    glyph_names: Arc<HashSet<GlyphName>>,
}

impl Work<Context, WorkError> for StaticMetadataWork {
    fn exec(&self, context: &Context) -> Result<(), WorkError> {
        let font_info = self.font_info.as_ref();
        let font = &font_info.font;
        debug!(
            "Static metadata for {}",
            font.names
                .get("familyNames")
                .map(|s| s.as_str())
                .unwrap_or("<nameless family>")
        );
        let axes = font_info.axes.clone();
        let glyph_locations = font_info.master_locations.values().cloned().collect();
        let glyph_order = font
            .glyph_order
            .iter()
            .map(|s| s.into())
            .filter(|gn| self.glyph_names.contains(gn))
            .collect();

        let mut static_metadata = StaticMetadata::new(
            font.units_per_em,
            names(font),
            axes,
            glyph_order,
            glyph_locations,
        )
        .map_err(WorkError::VariationModelError)?;
        if let Some(vendor_id) = font.names.get("vendorID") {
            static_metadata.vendor_id = Tag::from_str(vendor_id).map_err(WorkError::InvalidTag)?;
        }

        context.set_init_static_metadata(static_metadata);
        Ok(())
    }
}

struct GlobalMetricWork {
    font_info: Arc<FontInfo>,
}

impl Work<Context, WorkError> for GlobalMetricWork {
    fn exec(&self, context: &Context) -> Result<(), WorkError> {
        let font_info = self.font_info.as_ref();
        let font = &font_info.font;
        debug!(
            "Global metrics for {}",
            font.names
                .get("familyNames")
                .map(|s| s.as_str())
                .unwrap_or("<nameless family>")
        );

        let static_metadata = context.get_init_static_metadata();
        let mut metrics = GlobalMetrics::new(
            static_metadata.default_location().clone(),
            static_metadata.units_per_em,
        );

        for master in font.masters.iter() {
            let pos = font_info.master_locations.get(&master.id).unwrap();
            metrics.set_if_some(GlobalMetric::Ascender, pos.clone(), master.ascender());
            metrics.set_if_some(GlobalMetric::Descender, pos.clone(), master.descender());
            metrics.set_if_some(GlobalMetric::CapHeight, pos.clone(), master.cap_height());
            metrics.set_if_some(GlobalMetric::XHeight, pos.clone(), master.x_height());
        }

        context.set_global_metrics(metrics);
        Ok(())
    }
}

struct FeatureWork {
    font_info: Arc<FontInfo>,
}

impl Work<Context, WorkError> for FeatureWork {
    fn exec(&self, context: &Context) -> Result<(), WorkError> {
        trace!("Generate features");
        let font_info = self.font_info.as_ref();
        let font = &font_info.font;

        context.set_features(to_ir_features(&font.features)?);
        Ok(())
    }
}

struct GlyphIrWork {
    glyph_name: GlyphName,
    font_info: Arc<FontInfo>,
}

fn check_pos(
    glyph_name: &GlyphName,
    positions: &HashSet<NormalizedCoord>,
    axis: &ir::Axis,
    pos: &NormalizedCoord,
) -> Result<(), WorkError> {
    if !positions.contains(pos) {
        return Err(WorkError::GlyphUndefAtNormalizedPosition {
            glyph_name: glyph_name.clone(),
            axis: axis.tag,
            pos: *pos,
        });
    }
    Ok(())
}

impl Work<Context, WorkError> for GlyphIrWork {
    fn exec(&self, context: &Context) -> Result<(), WorkError> {
        trace!("Generate IR for '{}'", self.glyph_name.as_str());
        let font_info = self.font_info.as_ref();
        let font = &font_info.font;

        let static_metadata = context.get_init_static_metadata();
        let axes = &static_metadata.axes;

        let glyph = font
            .glyphs
            .get(self.glyph_name.as_str())
            .ok_or_else(|| WorkError::NoGlyphForName(self.glyph_name.clone()))?;

        let mut ir_glyph = ir::GlyphBuilder::new(self.glyph_name.clone());

        if let Some(codepoints) = font.glyph_to_codepoints.get(self.glyph_name.as_str()) {
            codepoints.iter().for_each(|cp| {
                ir_glyph.codepoints.insert(*cp);
            });
        }

        // Glyphs have layers that match up with masters, and masters have locations
        let mut axis_positions: HashMap<String, HashSet<NormalizedCoord>> = HashMap::new();
        for instance in glyph.layers.iter() {
            let Some(master_idx) = font_info.master_indices.get(instance.layer_id.as_str()) else {
                return Err(WorkError::NoMasterForGlyph {
                    master: instance.layer_id.clone(),
                    glyph: self.glyph_name.clone(),
                });
            };
            let master = &font.masters[*master_idx];
            let location = &font_info.master_locations[master.id.as_str()];

            for (tag, coord) in location.iter() {
                axis_positions
                    .entry(tag.clone())
                    .or_default()
                    .insert(*coord);
            }

            // TODO populate width and height properly
            let (contours, components) =
                to_ir_contours_and_components(self.glyph_name.clone(), &instance.shapes)?;
            let glyph_instance = GlyphInstance {
                width: instance.width.into_inner(),
                height: None,
                contours,
                components,
            };

            ir_glyph
                .try_add_source(location, glyph_instance)
                .map_err(|e| {
                    WorkError::AddGlyphSource(format!(
                        "Unable to add source to {:?} at {:?}: {}",
                        self.glyph_name, location, e
                    ))
                })?;
        }

        // It's helpful if glyphs are defined at min, default, and max (some of which may be cooincident)
        for axis in axes.iter() {
            let min = axis.min.to_normalized(&axis.converter);
            let max = axis.max.to_normalized(&axis.converter);
            let default = axis.max.to_normalized(&axis.converter);
            let Some(positions) = axis_positions.get(&axis.name) else {
                return Err(WorkError::NoAxisPosition(self.glyph_name.clone(), axis.name.clone()));
            };
            check_pos(&self.glyph_name, positions, axis, &min)?;
            check_pos(&self.glyph_name, positions, axis, &default)?;
            check_pos(&self.glyph_name, positions, axis, &max)?;
        }

        context.set_glyph_ir(ir_glyph.try_into()?);
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::{
        collections::{HashMap, HashSet},
        path::{Path, PathBuf},
        str::FromStr,
    };

    use font_types::NameId;
    use font_types::Tag;
    use fontdrasil::{orchestration::Access, types::GlyphName};
    use fontir::{
        coords::{
            CoordConverter, DesignCoord, NormalizedCoord, NormalizedLocation, UserCoord,
            UserLocation,
        },
        error::WorkError,
        ir::{self, GlobalMetricsInstance, NameKey},
        orchestration::{Context, WorkId},
        paths::Paths,
        source::Source,
        stateset::StateSet,
    };
    use glyphs_reader::Font;
    use indexmap::IndexSet;

    use crate::source::names;

    use super::{glyph_states, GlyphsIrSource};

    use pretty_assertions::assert_eq;

    fn testdata_dir() -> PathBuf {
        let dir = Path::new("../resources/testdata");
        assert!(dir.is_dir());
        dir.to_path_buf()
    }

    fn glyphs2_dir() -> PathBuf {
        testdata_dir().join("glyphs2")
    }

    fn glyphs3_dir() -> PathBuf {
        testdata_dir().join("glyphs3")
    }

    fn glyph_state_for_file(dir: &Path, filename: &str) -> HashMap<GlyphName, StateSet> {
        let glyphs_file = dir.join(filename);
        let font = Font::load(&glyphs_file).unwrap();
        glyph_states(&font).unwrap()
    }

    #[test]
    fn find_glyphs() {
        assert_eq!(
            HashSet::from(["space", "hyphen", "exclam", "manual-component"]),
            glyph_state_for_file(&glyphs3_dir(), "WghtVar.glyphs")
                .keys()
                .map(|k| k.as_str())
                .collect::<HashSet<&str>>()
        );
        assert_eq!(
            HashSet::from(["space", "hyphen", "exclam"]),
            glyph_state_for_file(&glyphs3_dir(), "WghtVar_HeavyHyphen.glyphs")
                .keys()
                .map(|k| k.as_str())
                .collect::<HashSet<&str>>()
        );
    }

    #[test]
    fn detect_changed_glyphs() {
        let keys: HashSet<GlyphName> =
            HashSet::from(["space".into(), "hyphen".into(), "exclam".into()]);

        let g1 = glyph_state_for_file(&glyphs3_dir(), "WghtVar.glyphs");
        let g2 = glyph_state_for_file(&glyphs3_dir(), "WghtVar_HeavyHyphen.glyphs");

        let changed = keys
            .into_iter()
            .filter(|key| g1.get(key).unwrap() != g2.get(key).unwrap())
            .collect();
        assert_eq!(HashSet::<GlyphName>::from(["hyphen".into()]), changed);
    }

    fn context_for(glyphs_file: PathBuf) -> (impl Source, Context) {
        let mut source = GlyphsIrSource::new(glyphs_file);
        let input = source.inputs().unwrap();
        (
            source,
            Context::new_root(
                Default::default(),
                Paths::new(Path::new("/nothing/should/write/here")),
                input,
            ),
        )
    }

    #[test]
    fn static_metadata_ir() {
        let (source, context) = context_for(glyphs3_dir().join("WghtVar.glyphs"));
        let task_context =
            context.copy_for_work(Access::none(), Access::one(WorkId::InitStaticMetadata));
        source
            .create_static_metadata_work(&context.input)
            .unwrap()
            .exec(&task_context)
            .unwrap();

        assert_eq!(
            vec![Tag::from_str("wght").unwrap()],
            context
                .get_init_static_metadata()
                .axes
                .iter()
                .map(|a| a.tag)
                .collect::<Vec<_>>()
        );
        let expected: IndexSet<GlyphName> = vec!["space", "exclam", "hyphen", "manual-component"]
            .iter()
            .map(|s| (*s).into())
            .collect();
        assert_eq!(expected, context.get_init_static_metadata().glyph_order);
    }

    #[test]
    fn static_metadata_ir_multi_axis() {
        // Caused index out of bounds due to transposed master and value indices
        let (source, context) = context_for(glyphs2_dir().join("BadIndexing.glyphs"));
        let task_context =
            context.copy_for_work(Access::none(), Access::one(WorkId::InitStaticMetadata));
        source
            .create_static_metadata_work(&context.input)
            .unwrap()
            .exec(&task_context)
            .unwrap();
    }

    #[test]
    fn loads_axis_mappings_from_glyphs2() {
        let (source, context) = context_for(glyphs2_dir().join("OpszWghtVar_AxisMappings.glyphs"));
        let task_context =
            context.copy_for_work(Access::none(), Access::one(WorkId::InitStaticMetadata));
        source
            .create_static_metadata_work(&context.input)
            .unwrap()
            .exec(&task_context)
            .unwrap();
        let static_metadata = context.get_init_static_metadata();

        // Did you load the mappings? DID YOU?!
        assert_eq!(
            vec![
                ir::Axis {
                    name: "Weight".into(),
                    tag: Tag::from_str("wght").unwrap(),
                    min: UserCoord::new(100.0),
                    default: UserCoord::new(500.0),
                    max: UserCoord::new(700.0),
                    hidden: false,
                    converter: CoordConverter::new(
                        vec![
                            (UserCoord::new(100.0), DesignCoord::new(40.0)),
                            (UserCoord::new(200.0), DesignCoord::new(46.0)),
                            (UserCoord::new(300.0), DesignCoord::new(51.0)),
                            (UserCoord::new(400.0), DesignCoord::new(57.0)),
                            (UserCoord::new(500.0), DesignCoord::new(62.0)), // default
                            (UserCoord::new(600.0), DesignCoord::new(68.0)),
                            (UserCoord::new(700.0), DesignCoord::new(73.0)),
                        ],
                        4
                    ),
                },
                ir::Axis {
                    name: "Optical Size".into(),
                    tag: Tag::from_str("opsz").unwrap(),
                    min: UserCoord::new(12.0),
                    default: UserCoord::new(12.0),
                    max: UserCoord::new(72.0),
                    hidden: false,
                    converter: CoordConverter::new(
                        vec![
                            (UserCoord::new(12.0), DesignCoord::new(12.0)), // default
                            (UserCoord::new(72.0), DesignCoord::new(72.0)),
                        ],
                        0
                    ),
                },
            ],
            static_metadata.axes
        );
    }

    fn build_static_metadata(glyphs_file: PathBuf) -> (impl Source, Context) {
        let _ = env_logger::builder().is_test(true).try_init();
        let (source, context) = context_for(glyphs_file);
        let task_context =
            context.copy_for_work(Access::none(), Access::one(WorkId::InitStaticMetadata));
        source
            .create_static_metadata_work(&context.input)
            .unwrap()
            .exec(&task_context)
            .unwrap();
        (source, context)
    }

    fn build_global_metrics(glyphs_file: PathBuf) -> (impl Source, Context) {
        let (source, context) = build_static_metadata(glyphs_file);
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

    fn build_glyphs(
        source: &impl Source,
        context: &Context,
        glyph_names: &[&GlyphName],
    ) -> Result<(), WorkError> {
        for glyph_name in glyph_names {
            let glyph_name = *glyph_name;
            let work_items = source
                .create_glyph_ir_work(&IndexSet::from([glyph_name.clone()]), &context.input)
                .unwrap();
            for work in work_items.iter() {
                let task_context = context.copy_for_work(
                    Access::one(WorkId::InitStaticMetadata),
                    Access::one(WorkId::Glyph(glyph_name.clone())),
                );
                work.exec(&task_context)?;
            }
        }
        Ok(())
    }

    #[test]
    fn glyph_user_locations() {
        let glyph_name: GlyphName = "space".into();
        let (source, context) =
            build_static_metadata(glyphs2_dir().join("OpszWghtVar_AxisMappings.glyphs"));
        build_glyphs(&source, &context, &[&glyph_name]).unwrap(); // we dont' care about geometry

        let static_metadata = context.get_init_static_metadata();
        let axes = static_metadata.axes.iter().map(|a| (&a.name, a)).collect();

        let mut expected_locations = HashSet::new();
        for (opsz, wght) in &[
            (12.0, 100.0),
            (12.0, 500.0),
            (12.0, 700.0),
            (72.0, 100.0),
            (72.0, 500.0),
            (72.0, 700.0),
        ] {
            let mut loc = UserLocation::new();
            loc.set_pos("Optical Size", UserCoord::new(*opsz));
            loc.set_pos("Weight", UserCoord::new(*wght));
            let loc: _ = loc;
            expected_locations.insert(loc);
        }
        let actual_locations = context
            .get_glyph_ir(&glyph_name)
            .sources()
            .keys()
            .map(|c| c.to_user(&axes))
            .collect::<HashSet<_>>();

        assert_eq!(expected_locations, actual_locations);
    }

    #[test]
    fn glyph_normalized_locations() {
        let glyph_name: GlyphName = "space".into();
        let (source, context) =
            build_static_metadata(glyphs2_dir().join("OpszWghtVar_AxisMappings.glyphs"));
        build_glyphs(&source, &context, &[&glyph_name]).unwrap(); // we dont' care about geometry

        let mut expected_locations = HashSet::new();
        for (opsz, wght) in &[
            (0.0, -1.0),
            (0.0, 0.0),
            (0.0, 1.0),
            (1.0, -1.0),
            (1.0, 0.0),
            (1.0, 1.0),
        ] {
            let mut loc = NormalizedLocation::new();
            loc.set_pos("Optical Size", NormalizedCoord::new(*opsz));
            loc.set_pos("Weight", NormalizedCoord::new(*wght));
            let loc: _ = loc;
            expected_locations.insert(loc);
        }
        let actual_locations = context
            .get_glyph_ir(&glyph_name)
            .sources()
            .keys()
            .cloned()
            .collect::<HashSet<_>>();

        assert_eq!(expected_locations, actual_locations);
    }

    #[test]
    fn glyph_must_define_min() {
        let glyph_name = GlyphName::from("min-undefined");
        let (source, context) = build_static_metadata(glyphs2_dir().join("MinUndef.glyphs"));
        let result = build_glyphs(&source, &context, &[&glyph_name]);
        assert!(result.is_err());
        let Err(WorkError::GlyphUndefAtNormalizedPosition { glyph_name, axis, pos }) =  result else {
            panic!("Wrong error");
        };
        assert_eq!("min-undefined", glyph_name.as_str());
        assert_eq!(Tag::from_str("wght").unwrap(), axis);
        assert_eq!(NormalizedCoord::new(-1.0), pos);
    }

    #[test]
    fn read_axis_location() {
        let (_, context) = build_static_metadata(glyphs3_dir().join("WghtVar_AxisLocation.glyphs"));
        let wght = &context.get_init_static_metadata().axes;
        assert_eq!(1, wght.len());
        let wght = &wght[0];

        for (design, user) in &[
            (0.0, 400.0),
            (4.0, 450.0),
            (8.0, 500.0),
            (9.0, 600.0),
            (10.0, 700.0),
        ] {
            assert_eq!(
                DesignCoord::new(*design),
                UserCoord::new(*user).to_design(&wght.converter),
                "{:#?}",
                wght.converter
            );
        }
    }

    #[test]
    fn captures_single_codepoints() {
        let (source, context) = build_static_metadata(glyphs2_dir().join("WghtVar.glyphs"));
        build_glyphs(&source, &context, &[&"hyphen".into()]).unwrap();
        let glyph = context.get_glyph_ir(&"hyphen".into());
        assert_eq!(HashSet::from([0x002d]), glyph.codepoints);
    }

    #[test]
    fn captures_single_codepoints_unquoted_dec() {
        let (source, context) =
            build_static_metadata(glyphs3_dir().join("Unicode-UnquotedDec.glyphs"));
        build_glyphs(&source, &context, &[&"name".into()]).unwrap();
        let glyph = context.get_glyph_ir(&"name".into());
        assert_eq!(HashSet::from([182]), glyph.codepoints);
    }

    #[test]
    fn captures_multiple_codepoints_unquoted_dec() {
        let (source, context) =
            build_static_metadata(glyphs3_dir().join("Unicode-UnquotedDecSequence.glyphs"));
        build_glyphs(&source, &context, &[&"name".into()]).unwrap();
        let glyph = context.get_glyph_ir(&"name".into());
        assert_eq!(HashSet::from([1619, 1764]), glyph.codepoints);
    }

    // It's so minimal it's a good test
    #[test]
    fn loads_minimal() {
        let (_, context) = build_static_metadata(glyphs2_dir().join("NotDef.glyphs"));
        assert_eq!(1000, context.get_init_static_metadata().units_per_em);
    }

    #[test]
    fn name_table() {
        let font = Font::load(&glyphs3_dir().join("TheBestNames.glyphs")).unwrap();
        let mut names: Vec<_> = names(&font).into_iter().collect();
        names.sort_by_key(|(id, v)| (id.name_id, v.clone()));
        assert_eq!(
            vec![
                (
                    NameKey::new_bmp_only(NameId::COPYRIGHT_NOTICE),
                    String::from("Copy!")
                ),
                (
                    NameKey::new_bmp_only(NameId::FAMILY_NAME),
                    String::from("FamilyName")
                ),
                (
                    NameKey::new_bmp_only(NameId::UNIQUE_ID),
                    String::from("We are all unique")
                ),
                (
                    NameKey::new_bmp_only(NameId::FULL_NAME),
                    String::from("Full of names")
                ),
                (
                    NameKey::new_bmp_only(NameId::VERSION_STRING),
                    String::from("New Value")
                ),
                (
                    NameKey::new_bmp_only(NameId::POSTSCRIPT_NAME),
                    String::from("Postscript Name")
                ),
                (
                    NameKey::new_bmp_only(NameId::TRADEMARK),
                    String::from("A trade in marks")
                ),
                (
                    NameKey::new_bmp_only(NameId::MANUFACTURER),
                    String::from("Who made you?!")
                ),
                (
                    NameKey::new_bmp_only(NameId::DESIGNER),
                    String::from("Designed by me!")
                ),
                (
                    NameKey::new_bmp_only(NameId::VENDOR_URL),
                    String::from("https://example.com/manufacturer")
                ),
                (
                    NameKey::new_bmp_only(NameId::DESIGNER_URL),
                    String::from("https://example.com/designer")
                ),
                (
                    NameKey::new_bmp_only(NameId::LICENSE_DESCRIPTION),
                    String::from("Licensed to thrill")
                ),
                (
                    NameKey::new_bmp_only(NameId::LICENSE_URL),
                    String::from("https://example.com/my/font/license")
                ),
                (
                    NameKey::new_bmp_only(NameId::TYPOGRAPHIC_FAMILY_NAME),
                    String::from("FamilyName")
                ),
                (
                    NameKey::new_bmp_only(NameId::COMPATIBLE_FULL_NAME),
                    String::from("For the Mac's only")
                ),
                (
                    NameKey::new_bmp_only(NameId::SAMPLE_TEXT),
                    String::from("Sam pull text")
                ),
                (
                    NameKey::new_bmp_only(NameId::WWS_FAMILY_NAME),
                    String::from("We Will Slant you")
                ),
            ],
            names
        );
    }

    #[test]
    fn version_with_version_string() {
        let font = Font::load(&glyphs3_dir().join("TheBestNames.glyphs")).unwrap();
        assert_eq!(
            "New Value",
            names(&font)
                .get(&NameKey::new_bmp_only(NameId::VERSION_STRING))
                .unwrap()
        );
    }

    #[test]
    fn version_from_major_minor() {
        let font = Font::load(&glyphs3_dir().join("VersionMajorMinor.glyphs")).unwrap();
        assert_eq!(
            "Version 42.043",
            names(&font)
                .get(&NameKey::new_bmp_only(NameId::VERSION_STRING))
                .unwrap()
        );
    }

    #[test]
    fn version_default() {
        let font = Font::load(&glyphs3_dir().join("infinity.glyphs")).unwrap();
        assert_eq!(
            "Version 0.000",
            names(&font)
                .get(&NameKey::new_bmp_only(NameId::VERSION_STRING))
                .unwrap()
        );
    }

    #[test]
    fn captures_global_metrics() {
        let (_, context) = build_global_metrics(glyphs3_dir().join("WghtVar.glyphs"));
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
            },
            default_metrics
        );
    }

    #[test]
    fn captures_vendor_id() {
        let (_, context) = build_static_metadata(glyphs3_dir().join("TheBestNames.glyphs"));
        assert_eq!(
            Tag::new(b"RODS"),
            context.get_init_static_metadata().vendor_id
        );
    }
}
