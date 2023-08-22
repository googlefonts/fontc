use chrono::{TimeZone, Utc};
use font_types::{NameId, Tag};
use fontdrasil::orchestration::{Access, Work};
use fontdrasil::types::{GlyphName, GroupName};
use fontir::coords::NormalizedCoord;
use fontir::error::{Error, WorkError};
use fontir::ir::{
    self, GlobalMetric, GlobalMetrics, GlyphInstance, GlyphOrder, KernParticipant, Kerning,
    NameBuilder, NameKey, NamedInstance, StaticMetadata, DEFAULT_VENDOR_ID,
};
use fontir::orchestration::{Context, IrWork, WorkId};
use fontir::source::{Input, Source};
use fontir::stateset::StateSet;
use glyphs_reader::{Font, InstanceType};
use indexmap::IndexSet;
use log::{debug, trace, warn};
use read_fonts::tables::os2::SelectionFlags;
use std::collections::{BTreeSet, HashSet};
use std::str::FromStr;
use std::sync::Arc;
use std::{collections::HashMap, path::PathBuf};

use crate::glyphdata::is_nonspacing_mark;
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
        glyph_states.insert(glyphname.as_str().into(), state);
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
            use_typo_metrics: font.use_typo_metrics,
            has_wws_names: font.has_wws_names,
            axes: font.axes.clone(),
            masters: font.masters.clone(),
            default_master_idx: font.default_master_idx,
            glyphs: Default::default(),
            glyph_order: Default::default(),
            glyph_to_codepoints: Default::default(),
            axis_mappings: font.axis_mappings.clone(),
            features: Default::default(),
            names: Default::default(),
            instances: font.instances.clone(),
            version_major: Default::default(),
            version_minor: Default::default(),
            date: None,
            kerning_ltr: Default::default(),
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
            use_typo_metrics: font.use_typo_metrics,
            has_wws_names: None,
            axes: font.axes.clone(),
            masters: font.masters.clone(),
            default_master_idx: font.default_master_idx,
            glyphs: Default::default(),
            glyph_order: Default::default(),
            glyph_to_codepoints: Default::default(),
            axis_mappings: Default::default(),
            features: Default::default(),
            names: Default::default(),
            instances: font.instances.clone(),
            version_major: Default::default(),
            version_minor: Default::default(),
            date: None,
            kerning_ltr: font.kerning_ltr.clone(),
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

    fn create_kerning_ir_work(&self, input: &Input) -> Result<Box<IrWork>, Error> {
        self.check_static_metadata(&input.static_metadata)?;

        let cache = self.cache.as_ref().unwrap();

        Ok(Box::new(KerningWork {
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
    let vendor = font
        .vendor_id()
        .map(|v| v.as_str())
        .unwrap_or(DEFAULT_VENDOR_ID);
    builder.apply_default_fallbacks(vendor);

    builder.into_inner()
}

#[derive(Debug)]
struct StaticMetadataWork {
    font_info: Arc<FontInfo>,
    glyph_names: Arc<HashSet<GlyphName>>,
}

impl Work<Context, WorkId, WorkError> for StaticMetadataWork {
    fn id(&self) -> WorkId {
        WorkId::StaticMetadata
    }

    fn also_completes(&self) -> Vec<WorkId> {
        vec![WorkId::PreliminaryGlyphOrder]
    }

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
        let axis_map = axes.iter().map(|a| (a.tag, a)).collect();
        let named_instances = font
            .instances
            .iter()
            .filter_map(|inst| {
                if inst.type_ != InstanceType::Single || !inst.active {
                    return None;
                }
                Some(NamedInstance {
                    name: inst.name.clone(),
                    location: font_info
                        .locations
                        .get(&inst.axes_values)
                        .map(|nc| nc.to_user(&axis_map))
                        .unwrap(),
                })
            })
            .collect();
        let glyph_locations = font
            .masters
            .iter()
            .map(|m| font_info.locations.get(&m.axes_values).cloned().unwrap())
            .collect();

        let selection_flags = match font.use_typo_metrics.unwrap_or_default() {
            true => SelectionFlags::USE_TYPO_METRICS,
            false => SelectionFlags::empty(),
        } | match font.has_wws_names.unwrap_or_default() {
            true => SelectionFlags::WWS,
            false => SelectionFlags::empty(),
        } |
        // https://github.com/googlefonts/glyphsLib/blob/42bc1db912fd4b66f130fb3bdc63a0c1e774eb38/Lib/glyphsLib/builder/names.py#L27
        match font.default_master().name.to_ascii_lowercase().as_str() {
            "italic" => SelectionFlags::ITALIC,
            "bold" => SelectionFlags::BOLD,
            "bold italic" => SelectionFlags::BOLD | SelectionFlags::ITALIC,
            _ => SelectionFlags::REGULAR,
        };

        let mut static_metadata = StaticMetadata::new(
            font.units_per_em,
            names(font),
            axes,
            named_instances,
            glyph_locations,
        )
        .map_err(WorkError::VariationModelError)?;
        static_metadata.misc.selection_flags = selection_flags;
        if let Some(vendor_id) = font.vendor_id() {
            static_metadata.misc.vendor_id =
                Tag::from_str(vendor_id).map_err(WorkError::InvalidTag)?;
        }
        // <https://github.com/googlefonts/glyphsLib/blob/main/Lib/glyphsLib/builder/custom_params.py#L1116-L1125>
        static_metadata.misc.underline_thickness = 50.0.into();
        static_metadata.misc.underline_position = (-100.0).into();

        static_metadata.misc.version_major = font.version_major;
        static_metadata.misc.version_minor = font.version_minor;

        static_metadata.misc.created = font
            .date
            .as_ref()
            .and_then(|raw_date| {
                let parsed = Utc.datetime_from_str(raw_date, "%Y-%m-%d %H:%M:%S %Z");
                if let Err(e) = parsed {
                    warn!("Invalid creation date: {}: {e:?}", raw_date);
                }
                parsed.ok()
            })
            .or(static_metadata.misc.created);

        context.static_metadata.set(static_metadata);

        let glyph_order = font
            .glyph_order
            .iter()
            .map(|s| s.as_str().into())
            .filter(|gn| self.glyph_names.contains(gn))
            .collect();
        context.preliminary_glyph_order.set(glyph_order);
        Ok(())
    }
}

#[derive(Debug)]
struct GlobalMetricWork {
    font_info: Arc<FontInfo>,
}

impl Work<Context, WorkId, WorkError> for GlobalMetricWork {
    fn id(&self) -> WorkId {
        WorkId::GlobalMetrics
    }

    fn read_access(&self) -> Access<WorkId> {
        Access::One(WorkId::StaticMetadata)
    }

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

        let static_metadata = context.static_metadata.get();
        let mut metrics = GlobalMetrics::new(
            static_metadata.default_location().clone(),
            static_metadata.units_per_em,
            font.default_master().x_height().map(|v| v.0 as f32),
        );

        for master in font.masters.iter() {
            let pos = font_info.locations.get(&master.axes_values).unwrap();
            metrics.set_if_some(GlobalMetric::Ascender, pos.clone(), master.ascender());
            metrics.set_if_some(GlobalMetric::Descender, pos.clone(), master.descender());
            metrics.set_if_some(GlobalMetric::CapHeight, pos.clone(), master.cap_height());
            metrics.set_if_some(GlobalMetric::XHeight, pos.clone(), master.x_height());
            metrics.set_if_some(
                GlobalMetric::Os2TypoAscender,
                pos.clone(),
                master.typo_ascender.map(|v| v as f64),
            );
            metrics.set_if_some(
                GlobalMetric::Os2TypoDescender,
                pos.clone(),
                master.typo_descender.map(|v| v as f64),
            );
            metrics.set_if_some(
                GlobalMetric::Os2TypoLineGap,
                pos.clone(),
                master.typo_line_gap.map(|v| v as f64),
            );
            metrics.set_if_some(
                GlobalMetric::Os2WinAscent,
                pos.clone(),
                master.win_ascent.map(|v| v as f64),
            );
            metrics.set_if_some(
                GlobalMetric::Os2WinDescent,
                pos.clone(),
                master.win_descent.map(|v| v as f64),
            );
            metrics.set_if_some(
                GlobalMetric::HheaAscender,
                pos.clone(),
                master.hhea_ascender.map(|v| v as f64),
            );
            metrics.set_if_some(
                GlobalMetric::HheaDescender,
                pos.clone(),
                master.hhea_descender.map(|v| v as f64),
            );
            metrics.set_if_some(
                GlobalMetric::HheaLineGap,
                pos.clone(),
                master.hhea_line_gap.map(|v| v as f64),
            );
        }

        context.global_metrics.set(metrics);
        Ok(())
    }
}

#[derive(Debug)]
struct FeatureWork {
    font_info: Arc<FontInfo>,
}

impl Work<Context, WorkId, WorkError> for FeatureWork {
    fn id(&self) -> WorkId {
        WorkId::Features
    }

    fn exec(&self, context: &Context) -> Result<(), WorkError> {
        trace!("Generate features");
        let font_info = self.font_info.as_ref();
        let font = &font_info.font;

        context.features.set(to_ir_features(&font.features)?);
        Ok(())
    }
}

/// What side of the kern is this, in logical order
enum KernSide {
    Side1,
    Side2,
}

impl KernSide {
    fn class_prefix(&self) -> &'static str {
        match self {
            KernSide::Side1 => "@MMK_L_",
            KernSide::Side2 => "@MMK_R_",
        }
    }

    fn group_prefix(&self) -> &'static str {
        match self {
            KernSide::Side1 => "public.kern1.",
            KernSide::Side2 => "public.kern2.",
        }
    }
}

fn is_kerning_class(name: &str) -> bool {
    name.starts_with("@MMK_")
}

#[derive(Debug)]
struct KerningWork {
    font_info: Arc<FontInfo>,
}

/// See <https://github.com/googlefonts/glyphsLib/blob/42bc1db912fd4b66f130fb3bdc63a0c1e774eb38/Lib/glyphsLib/builder/kerning.py#L53-L72>
fn kern_participant(
    glyph_order: &GlyphOrder,
    groups: &HashMap<GlyphName, BTreeSet<GlyphName>>,
    side: KernSide,
    raw_side: &str,
) -> Option<KernParticipant> {
    if is_kerning_class(raw_side) {
        if raw_side.starts_with(side.class_prefix()) {
            let group_name = format!(
                "{}{}",
                side.group_prefix(),
                raw_side.strip_prefix(side.class_prefix()).unwrap()
            );
            let group = GroupName::from(group_name.as_str());
            if groups.contains_key(&group) {
                Some(KernParticipant::Group(group))
            } else {
                warn!("Invalid kern side: {raw_side}, no group {group_name}");
                None
            }
        } else {
            warn!(
                "Invalid kern side: {raw_side}, should have prefix {}",
                side.class_prefix()
            );
            None
        }
    } else {
        let name = GlyphName::from(raw_side);
        if glyph_order.contains(&name) {
            Some(KernParticipant::Glyph(name))
        } else {
            warn!("Invalid kern side: {raw_side}, no such glyph");
            None
        }
    }
}

impl Work<Context, WorkId, WorkError> for KerningWork {
    fn id(&self) -> WorkId {
        WorkId::Kerning
    }

    fn read_access(&self) -> Access<WorkId> {
        Access::Set(HashSet::from([WorkId::GlyphOrder, WorkId::StaticMetadata]))
    }

    fn exec(&self, context: &Context) -> Result<(), WorkError> {
        trace!("Generate IR for kerning");
        let static_metadata = context.static_metadata.get();
        let arc_glyph_order = context.glyph_order.get();
        let glyph_order = arc_glyph_order.as_ref();
        let font_info = self.font_info.as_ref();
        let font = &font_info.font;

        let variable_axes: HashSet<_> = static_metadata
            .variable_axes
            .iter()
            .map(|a| a.tag)
            .collect();
        let master_positions: HashMap<_, _> = font
            .masters
            .iter()
            .map(|m| (&m.id, font_info.locations.get(&m.axes_values).unwrap()))
            .map(|(id, pos)| {
                let mut pos = pos.clone();
                pos.retain(|tag, _| variable_axes.contains(tag));
                (id, pos)
            })
            .collect();

        let mut kerning = Kerning::default();

        // If glyph uses a group for either side it goes in that group
        font.glyphs
            .iter()
            .flat_map(|(glyph_name, glyph)| {
                glyph
                    .right_kern
                    .iter()
                    .map(|group| (KernSide::Side1, group))
                    .chain(glyph.left_kern.iter().map(|group| (KernSide::Side2, group)))
                    .map(|(side, group_name)| {
                        (
                            GroupName::from(format!("{}{}", side.group_prefix(), group_name)),
                            GlyphName::from(glyph_name.as_str()),
                        )
                    })
            })
            .for_each(|(group_name, glyph_name)| {
                kerning
                    .groups
                    .entry(group_name)
                    .or_default()
                    .insert(glyph_name);
            });

        font.kerning_ltr
            .iter()
            .filter_map(|(master_id, kerns)| match master_positions.get(master_id) {
                Some(pos) => Some((pos, kerns)),
                None => {
                    warn!("Kerning is present for non-existent master {master_id}");
                    None
                }
            })
            .flat_map(|(master_pos, kerns)| {
                kerns.iter().map(|((side1, side2), adjustment)| {
                    ((side1, side2), ((*master_pos).clone(), *adjustment))
                })
            })
            .filter_map(|((side1, side2), pos_adjust)| {
                let side1 = kern_participant(glyph_order, &kerning.groups, KernSide::Side1, side1);
                let side2 = kern_participant(glyph_order, &kerning.groups, KernSide::Side2, side2);
                let (Some(side1), Some(side2)) = (side1, side2) else {
                    return None
                };
                Some(((side1, side2), pos_adjust))
            })
            .for_each(|(participants, (pos, value))| {
                kerning
                    .kerns
                    .entry(participants)
                    .or_default()
                    .insert(pos, (value as f32).into());
            });

        context.kerning.set(kerning);
        Ok(())
    }
}

#[derive(Debug)]
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

impl Work<Context, WorkId, WorkError> for GlyphIrWork {
    fn id(&self) -> WorkId {
        WorkId::Glyph(self.glyph_name.clone())
    }

    fn read_access(&self) -> Access<WorkId> {
        Access::One(WorkId::StaticMetadata)
    }

    fn exec(&self, context: &Context) -> Result<(), WorkError> {
        trace!("Generate IR for '{}'", self.glyph_name.as_str());
        let font_info = self.font_info.as_ref();
        let font = &font_info.font;

        let static_metadata = context.static_metadata.get();
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

        // https://github.com/googlefonts/fontmake-rs/issues/285 glyphs non-spacing marks are 0-width
        let zero_width = is_nonspacing_mark(&ir_glyph.codepoints, ir_glyph.name.as_str());

        // Glyphs have layers that match up with masters, and masters have locations
        let mut axis_positions: HashMap<Tag, HashSet<NormalizedCoord>> = HashMap::new();
        for instance in glyph.layers.iter() {
            let Some(master_idx) = font_info.master_indices.get(instance.layer_id.as_str()) else {
                return Err(WorkError::NoMasterForGlyph {
                    master: instance.layer_id.clone(),
                    glyph: self.glyph_name.clone(),
                });
            };
            let master = &font.masters[*master_idx];
            let location = font_info.locations.get(&master.axes_values).unwrap();

            for (tag, coord) in location.iter() {
                axis_positions.entry(*tag).or_default().insert(*coord);
            }

            // TODO populate width and height properly
            let (contours, components) =
                to_ir_contours_and_components(self.glyph_name.clone(), &instance.shapes)?;
            let glyph_instance = GlyphInstance {
                width: if !zero_width {
                    instance.width.into_inner()
                } else {
                    0.0
                },
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
            let Some(positions) = axis_positions.get(&axis.tag) else {
                return Err(WorkError::NoAxisPosition(self.glyph_name.clone(), axis.name.clone()));
            };
            check_pos(&self.glyph_name, positions, axis, &min)?;
            check_pos(&self.glyph_name, positions, axis, &default)?;
            check_pos(&self.glyph_name, positions, axis, &max)?;
        }

        context.glyphs.set(ir_glyph.try_into()?);
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::{
        collections::{HashMap, HashSet},
        path::{Path, PathBuf},
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
        ir::{self, GlobalMetricsInstance, GlyphOrder, NameKey},
        orchestration::{Context, Flags, WorkId},
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
            HashSet::from([
                "space",
                "hyphen",
                "exclam",
                "bracketleft",
                "bracketright",
                "manual-component"
            ]),
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
        let mut flags = Flags::default();
        flags.set(Flags::EMIT_IR, false); // we don't want to write anything down
        (
            source,
            Context::new_root(
                flags,
                Paths::new(Path::new("/nothing/should/write/here")),
                input,
            ),
        )
    }

    #[test]
    fn static_metadata_ir() {
        let (source, context) = context_for(glyphs3_dir().join("WghtVar.glyphs"));
        let task_context = context.copy_for_work(
            Access::none(),
            Access::Set(HashSet::from([
                WorkId::StaticMetadata,
                WorkId::PreliminaryGlyphOrder,
            ])),
        );
        source
            .create_static_metadata_work(&context.input)
            .unwrap()
            .exec(&task_context)
            .unwrap();

        assert_eq!(
            vec![Tag::new(b"wght")],
            context
                .static_metadata
                .get()
                .axes
                .iter()
                .map(|a| a.tag)
                .collect::<Vec<_>>()
        );
        let expected: GlyphOrder = vec![
            "space",
            "exclam",
            "hyphen",
            "bracketleft",
            "bracketright",
            "manual-component",
        ]
        .iter()
        .map(|s| (*s).into())
        .collect();
        assert_eq!(expected, *context.preliminary_glyph_order.get());
    }

    #[test]
    fn static_metadata_ir_multi_axis() {
        // Caused index out of bounds due to transposed master and value indices
        let (source, context) = context_for(glyphs2_dir().join("BadIndexing.glyphs"));
        let task_context = context.copy_for_work(
            Access::none(),
            Access::Set(HashSet::from([
                WorkId::StaticMetadata,
                WorkId::PreliminaryGlyphOrder,
            ])),
        );
        source
            .create_static_metadata_work(&context.input)
            .unwrap()
            .exec(&task_context)
            .unwrap();
    }

    #[test]
    fn loads_axis_mappings_from_glyphs2() {
        let (source, context) = context_for(glyphs2_dir().join("OpszWghtVar_AxisMappings.glyphs"));
        let task_context = context.copy_for_work(
            Access::none(),
            Access::Set(HashSet::from([
                WorkId::StaticMetadata,
                WorkId::PreliminaryGlyphOrder,
            ])),
        );
        source
            .create_static_metadata_work(&context.input)
            .unwrap()
            .exec(&task_context)
            .unwrap();
        let static_metadata = context.static_metadata.get();

        // Did you load the mappings? DID YOU?!
        assert_eq!(
            vec![
                ir::Axis {
                    name: "Weight".into(),
                    tag: Tag::new(b"wght"),
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
                    tag: Tag::new(b"opsz"),
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
        let task_context = context.copy_for_work(
            Access::none(),
            Access::Set(HashSet::from([
                WorkId::StaticMetadata,
                WorkId::PreliminaryGlyphOrder,
            ])),
        );
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
            Access::one(WorkId::StaticMetadata),
            Access::one(WorkId::GlobalMetrics),
        );
        source
            .create_global_metric_work(&context.input)
            .unwrap()
            .exec(&task_context)
            .unwrap();
        (source, context)
    }

    fn build_kerning(glyphs_file: PathBuf) -> (impl Source, Context) {
        let (source, context) = build_static_metadata(glyphs_file);

        // static metadata includes preliminary glyph order; just copy it to be the final one
        context
            .copy_for_work(
                Access::One(WorkId::PreliminaryGlyphOrder),
                Access::One(WorkId::GlyphOrder),
            )
            .glyph_order
            .set((*context.preliminary_glyph_order.get()).clone());

        let task_context = context.copy_for_work(
            Access::Set(HashSet::from([WorkId::StaticMetadata, WorkId::GlyphOrder])),
            Access::one(WorkId::Kerning),
        );
        source
            .create_kerning_ir_work(&context.input)
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
                    Access::one(WorkId::StaticMetadata),
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

        let static_metadata = context.static_metadata.get();
        let axes = static_metadata.axes.iter().map(|a| (a.tag, a)).collect();

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
            loc.insert(Tag::new(b"opsz"), UserCoord::new(*opsz));
            loc.insert(Tag::new(b"wght"), UserCoord::new(*wght));
            let loc = loc;
            expected_locations.insert(loc);
        }
        let actual_locations = context
            .glyphs
            .get(&WorkId::Glyph(glyph_name))
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
            loc.insert(Tag::new(b"opsz"), NormalizedCoord::new(*opsz));
            loc.insert(Tag::new(b"wght"), NormalizedCoord::new(*wght));
            let loc = loc;
            expected_locations.insert(loc);
        }
        let actual_locations = context
            .glyphs
            .get(&WorkId::Glyph(glyph_name))
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
        assert_eq!(Tag::new(b"wght"), axis);
        assert_eq!(NormalizedCoord::new(-1.0), pos);
    }

    #[test]
    fn read_axis_location() {
        let (_, context) = build_static_metadata(glyphs3_dir().join("WghtVar_AxisLocation.glyphs"));
        let wght = &context.static_metadata.get().axes;
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
        let glyph = context.glyphs.get(&WorkId::Glyph("hyphen".into()));
        assert_eq!(HashSet::from([0x002d]), glyph.codepoints);
    }

    #[test]
    fn captures_single_codepoints_unquoted_dec() {
        let (source, context) =
            build_static_metadata(glyphs3_dir().join("Unicode-UnquotedDec.glyphs"));
        build_glyphs(&source, &context, &[&"name".into()]).unwrap();
        let glyph = context.glyphs.get(&WorkId::Glyph("name".into()));
        assert_eq!(HashSet::from([182]), glyph.codepoints);
    }

    #[test]
    fn captures_multiple_codepoints_unquoted_dec() {
        let (source, context) =
            build_static_metadata(glyphs3_dir().join("Unicode-UnquotedDecSequence.glyphs"));
        build_glyphs(&source, &context, &[&"name".into()]).unwrap();
        let glyph = context.glyphs.get(&WorkId::Glyph("name".into()));
        assert_eq!(HashSet::from([1619, 1764]), glyph.codepoints);
    }

    // It's so minimal it's a good test
    #[test]
    fn loads_minimal() {
        let (_, context) = build_static_metadata(glyphs2_dir().join("NotDef.glyphs"));
        assert_eq!(1000, context.static_metadata.get().units_per_em);
    }

    #[test]
    fn name_table() {
        let font = Font::load(&glyphs3_dir().join("TheBestNames.glyphs")).unwrap();
        let mut names: Vec<_> = names(&font).into_iter().collect();
        names.sort_by_key(|(id, v)| (id.name_id, v.clone()));
        // typographic family name == family name and should NOT be present
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
                    NameKey::new_bmp_only(NameId::SUBFAMILY_NAME),
                    String::from("Regular")
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
                caret_slope_rise: 1.0.into(),
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
                os2_typo_ascender: 1000.0.into(),
                os2_typo_descender: (-200.0).into(),
                os2_typo_line_gap: 200.0.into(),
                os2_win_ascent: 800.0.into(),
                os2_win_descent: (-200.0).into(),
                hhea_ascender: 1000.0.into(),
                hhea_descender: (-200.0).into(),
                hhea_line_gap: 0.0.into(),
                ..Default::default()
            },
            default_metrics
        );
    }

    #[test]
    fn captures_vendor_id() {
        let (_, context) = build_static_metadata(glyphs3_dir().join("TheBestNames.glyphs"));
        assert_eq!(
            Tag::new(b"RODS"),
            context.static_metadata.get().misc.vendor_id
        );
    }

    #[test]
    fn default_underline_settings() {
        let (_, context) = build_static_metadata(glyphs3_dir().join("Oswald-O.glyphs"));
        let static_metadata = context.static_metadata.get();
        assert_eq!(
            (1000, 50.0, -100.0),
            (
                static_metadata.units_per_em,
                static_metadata.misc.underline_thickness.0,
                static_metadata.misc.underline_position.0
            )
        );
    }

    #[test]
    fn build_glyph_contour_ir_containing_qcurves() {
        let glyph_name = "i";
        let expected = "M302,584 Q328,584 346,602 Q364,620 364,645 Q364,670 346,687.5 Q328,705 302,705 Q276,705 257.5,687.5 Q239,670 239,645 Q239,620 257.5,602 Q276,584 302,584 Z";
        for test_dir in &[glyphs2_dir(), glyphs3_dir()] {
            let (source, context) = build_static_metadata(test_dir.join("QCurve.glyphs"));
            build_glyphs(&source, &context, &[&glyph_name.into()]).unwrap();
            let glyph = context.glyphs.get(&WorkId::Glyph(glyph_name.into()));
            let default_instance = glyph
                .sources()
                .get(context.static_metadata.get().default_location())
                .unwrap();
            let first_contour = default_instance.contours.first().unwrap();
            assert_eq!(first_contour.to_svg(), expected);
        }
    }

    // .glyphs v2 defaults to Weight, Width, Custom if no axes are specified
    // Avoid ending up with kerning for locations like {XXXX: 0.00, wdth: 0.00, wght: 1.00}
    // when XXXX and wdth are point axes that won't be in fvar. Oswald was hitting this.
    #[test]
    fn kern_positions_on_live_axes() {
        let (_, context) = build_kerning(glyphs2_dir().join("KernImplicitAxes.glyphs"));
        let kerning = context.kerning.get();
        assert!(!kerning.is_empty(), "{kerning:#?}");
        let bad_kerns: Vec<_> = kerning
            .kerns
            .values()
            .flat_map(|v| v.keys())
            .filter(|pos| !pos.axis_tags().all(|tag| *tag == Tag::new(b"wght")))
            .collect();
        assert!(bad_kerns.is_empty(), "{bad_kerns:#?}");
    }
}
