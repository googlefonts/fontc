use std::{
    collections::{BTreeMap, BTreeSet, HashMap, HashSet},
    path::PathBuf,
    str::FromStr,
    sync::Arc,
};

use chrono::DateTime;
use indexmap::IndexSet;
use log::{debug, trace, warn};

use fontdrasil::{
    coords::{NormalizedCoord, NormalizedLocation},
    orchestration::{Access, AccessBuilder, Work},
    types::GlyphName,
};
use fontir::{
    error::{Error, WorkError},
    ir::{
        self, AnchorBuilder, AnchorKind, GdefCategories, GlobalMetric, GlobalMetrics,
        GlyphInstance, GlyphOrder, KernGroup, KernSide, KerningGroups, KerningInstance,
        NameBuilder, NameKey, NamedInstance, StaticMetadata, DEFAULT_VENDOR_ID,
    },
    orchestration::{Context, IrWork, WorkId},
    source::{Input, Source},
    stateset::StateSet,
};
use glyphs_reader::{
    glyphdata::{Category, Subcategory},
    Font, InstanceType,
};
use write_fonts::{
    tables::{gdef::GlyphClassDef, os2::SelectionFlags},
    types::{NameId, Tag},
};

use crate::toir::{design_location, to_ir_contours_and_components, to_ir_features, FontInfo};

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
            fs_type: font.fs_type,
            use_typo_metrics: font.use_typo_metrics,
            has_wws_names: font.has_wws_names,
            axes: font.axes.clone(),
            masters: font.masters.clone(),
            default_master_idx: font.default_master_idx,
            glyphs: Default::default(),
            glyph_order: Default::default(),
            axis_mappings: font.axis_mappings.clone(),
            virtual_masters: Default::default(),
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
            fs_type: None,
            use_typo_metrics: font.use_typo_metrics,
            has_wws_names: None,
            axes: font.axes.clone(),
            masters: font.masters.clone(),
            default_master_idx: font.default_master_idx,
            glyphs: Default::default(),
            glyph_order: Default::default(),
            axis_mappings: Default::default(),
            virtual_masters: Default::default(),
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

    fn create_kerning_group_ir_work(&self, input: &Input) -> Result<Box<IrWork>, Error> {
        self.check_static_metadata(&input.static_metadata)?;

        let cache = self.cache.as_ref().unwrap();

        Ok(Box::new(KerningGroupWork {
            font_info: cache.font_info.clone(),
        }))
    }

    fn create_kerning_instance_ir_work(
        &self,
        input: &Input,
        at: NormalizedLocation,
    ) -> Result<Box<IrWork>, Error> {
        self.check_static_metadata(&input.static_metadata)?;

        let cache = self.cache.as_ref().unwrap();

        Ok(Box::new(KerningInstanceWork {
            font_info: cache.font_info.clone(),
            location: at,
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

        // negate the italic angle because it's clockwise in Glyphs.app whereas it's
        // counter-clockwise in UFO/OpenType and our GlobalMetrics follow the latter
        // https://github.com/googlefonts/glyphsLib/blob/f162e7/Lib/glyphsLib/builder/masters.py#L36
        let italic_angle = font
            .default_master()
            .italic_angle()
            .map(|v| -v)
            .unwrap_or(0.0);
        let categories = make_glyph_categories(font);

        let mut static_metadata = StaticMetadata::new(
            font.units_per_em,
            names(font),
            axes,
            named_instances,
            glyph_locations,
            Default::default(), // TODO: impl reading PS names from Glyphs
            italic_angle,
            categories,
        )
        .map_err(WorkError::VariationModelError)?;
        static_metadata.misc.selection_flags = selection_flags;
        if let Some(vendor_id) = font.vendor_id() {
            static_metadata.misc.vendor_id =
                Tag::from_str(vendor_id).map_err(WorkError::InvalidTag)?;
        }

        // Default per <https://github.com/googlefonts/glyphsLib/blob/cb8a4a914b0a33431f0a77f474bf57eec2f19bcc/Lib/glyphsLib/builder/custom_params.py#L1117-L1119>
        static_metadata.misc.fs_type = font.fs_type.or(Some(1 << 3));

        static_metadata.misc.version_major = font.version_major;
        static_metadata.misc.version_minor = font.version_minor;

        static_metadata.misc.created = font
            .date
            .as_ref()
            .and_then(|raw_date| {
                let parsed =
                    DateTime::parse_from_str(raw_date, "%Y-%m-%d %H:%M:%S %z").map(|nd| nd.into());
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

fn make_glyph_categories(font: &Font) -> GdefCategories {
    let categories = font
        .glyphs
        .iter()
        .filter_map(|(name, glyph)| {
            category_for_glyph(glyph).map(|cat| (GlyphName::new(name), cat))
        })
        .collect();
    GdefCategories {
        categories,
        prefer_gdef_categories_in_fea: false,
    }
}

/// determine the GDEF category for this glyph, if appropriate
// see
// <https://github.com/googlefonts/glyphsLib/blob/e2ebf5b517/Lib/glyphsLib/builder/features.py#L205>
fn category_for_glyph(glyph: &glyphs_reader::Glyph) -> Option<GlyphClassDef> {
    let has_attaching_anchor = glyph
        .layers
        .iter()
        .flat_map(|layer| layer.anchors.iter())
        .any(|anchor| {
            AnchorKind::new(&anchor.name)
                .map(|a| a.is_attaching())
                .unwrap_or(false)
        });
    match (glyph.category?, glyph.sub_category.unwrap_or_default()) {
        (_, Subcategory::Ligature) if has_attaching_anchor => Some(GlyphClassDef::Ligature),
        (Category::Mark, Subcategory::Nonspacing | Subcategory::SpacingCombining) => {
            Some(GlyphClassDef::Mark)
        }
        _ if has_attaching_anchor => Some(GlyphClassDef::Base),
        _ => None,
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
        Access::Variant(WorkId::StaticMetadata)
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
        let default_master = font.default_master();
        let mut metrics = GlobalMetrics::new(
            static_metadata.default_location().clone(),
            static_metadata.units_per_em,
            default_master.x_height(),
            default_master.ascender(),
            default_master.descender(),
            static_metadata.italic_angle.into_inner(),
        );

        for master in font.masters.iter() {
            let pos = font_info.locations.get(&master.axes_values).unwrap();
            if !pos.is_default() {
                metrics.populate_defaults(
                    pos,
                    static_metadata.units_per_em,
                    master.x_height(),
                    master.ascender(),
                    master.descender(),
                    // turn clockwise angle counter-clockwise
                    master.italic_angle().map(|v| -v),
                );
            }

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
                GlobalMetric::StrikeoutPosition,
                pos.clone(),
                master.strikeout_position.map(|v| v as f64),
            );
            metrics.set_if_some(
                GlobalMetric::StrikeoutSize,
                pos.clone(),
                master.strikeout_size.map(|v| v as f64),
            );
            metrics.set_if_some(
                GlobalMetric::SubscriptXOffset,
                pos.clone(),
                master.subscript_x_offset.map(|v| v as f64),
            );
            metrics.set_if_some(
                GlobalMetric::SubscriptXSize,
                pos.clone(),
                master.subscript_x_size.map(|v| v as f64),
            );
            metrics.set_if_some(
                GlobalMetric::SubscriptYOffset,
                pos.clone(),
                master.subscript_y_offset.map(|v| v as f64),
            );
            metrics.set_if_some(
                GlobalMetric::SubscriptYSize,
                pos.clone(),
                master.subscript_y_size.map(|v| v as f64),
            );
            metrics.set_if_some(
                GlobalMetric::SuperscriptXOffset,
                pos.clone(),
                master.superscript_x_offset.map(|v| v as f64),
            );
            metrics.set_if_some(
                GlobalMetric::SuperscriptXSize,
                pos.clone(),
                master.superscript_x_size.map(|v| v as f64),
            );
            metrics.set_if_some(
                GlobalMetric::SuperscriptYOffset,
                pos.clone(),
                master.superscript_y_offset.map(|v| v as f64),
            );
            metrics.set_if_some(
                GlobalMetric::SuperscriptYSize,
                pos.clone(),
                master.superscript_y_size.map(|v| v as f64),
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
            metrics.set_if_some(
                GlobalMetric::UnderlineThickness,
                pos.clone(),
                master.underline_thickness,
            );
            metrics.set_if_some(
                GlobalMetric::UnderlinePosition,
                pos.clone(),
                master.underline_position,
            )
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

fn parse_kern_group(name: &str) -> Option<KernGroup> {
    name.strip_prefix(SIDE1_PREFIX)
        .map(|name| KernGroup::Side1(name.into()))
        .or_else(|| {
            name.strip_prefix(SIDE2_PREFIX)
                .map(|name| KernGroup::Side2(name.into()))
        })
}

const SIDE1_PREFIX: &str = "@MMK_L_";
const SIDE2_PREFIX: &str = "@MMK_R_";

#[derive(Debug)]
struct KerningGroupWork {
    font_info: Arc<FontInfo>,
}

#[derive(Debug)]
struct KerningInstanceWork {
    font_info: Arc<FontInfo>,
    location: NormalizedLocation,
}

/// See <https://github.com/googlefonts/glyphsLib/blob/42bc1db912fd4b66f130fb3bdc63a0c1e774eb38/Lib/glyphsLib/builder/kerning.py#L53-L72>
fn kern_participant(
    glyph_order: &GlyphOrder,
    groups: &BTreeMap<KernGroup, BTreeSet<GlyphName>>,
    expect_prefix: &str,
    raw_side: &str,
) -> Option<KernSide> {
    if let Some(group) = parse_kern_group(raw_side) {
        if !raw_side.starts_with(expect_prefix) {
            warn!("Invalid kern side: {raw_side}, should have prefix {expect_prefix}",);
            return None;
        }
        if groups.contains_key(&group) {
            Some(KernSide::Group(group))
        } else {
            warn!("Invalid kern side: {raw_side}, no group {group:?}");
            None
        }
    } else {
        let name = GlyphName::from(raw_side);
        if glyph_order.contains(&name) {
            Some(KernSide::Glyph(name))
        } else {
            warn!("Invalid kern side: {raw_side}, no such glyph");
            None
        }
    }
}

impl Work<Context, WorkId, WorkError> for KerningGroupWork {
    fn id(&self) -> WorkId {
        WorkId::KerningGroups
    }

    fn read_access(&self) -> Access<WorkId> {
        Access::None
    }

    fn exec(&self, context: &Context) -> Result<(), WorkError> {
        trace!("Generate IR for kerning");
        let font_info = self.font_info.as_ref();
        let font = &font_info.font;

        let mut groups = KerningGroups::default();

        // If glyph uses a group for either side it goes in that group
        font.glyphs
            .iter()
            // ignore non-export glyphs
            .filter(|(_, glyph)| glyph.export)
            .flat_map(|(glyph_name, glyph)| {
                glyph
                    .right_kern
                    .iter()
                    .cloned()
                    .map(KernGroup::Side1)
                    .chain(glyph.left_kern.iter().cloned().map(KernGroup::Side2))
                    .map(|group| (group, GlyphName::from(glyph_name.as_str())))
            })
            .for_each(|(group_name, glyph_name)| {
                groups
                    .groups
                    .entry(group_name)
                    .or_default()
                    .insert(glyph_name);
            });

        groups.locations = font
            .kerning_ltr
            .iter()
            .filter_map(
                |(master_id, _)| match font_info.master_positions.get(master_id) {
                    Some(pos) => Some(pos),
                    None => {
                        warn!("Kerning is present for non-existent master {master_id}");
                        None
                    }
                },
            )
            .cloned()
            .collect();

        context.kerning_groups.set(groups);
        Ok(())
    }
}

impl Work<Context, WorkId, WorkError> for KerningInstanceWork {
    fn id(&self) -> WorkId {
        WorkId::KernInstance(self.location.clone())
    }

    fn read_access(&self) -> Access<WorkId> {
        AccessBuilder::new()
            .variant(WorkId::GlyphOrder)
            .variant(WorkId::KerningGroups)
            .build()
    }

    fn exec(&self, context: &Context) -> Result<(), WorkError> {
        trace!("Generate IR for kerning at {:?}", self.location);
        let kerning_groups = context.kerning_groups.get();
        let groups = &kerning_groups.groups;
        let arc_glyph_order = context.glyph_order.get();
        let glyph_order = arc_glyph_order.as_ref();
        let font_info = self.font_info.as_ref();
        let font = &font_info.font;

        let mut kerning = KerningInstance {
            location: self.location.clone(),
            ..Default::default()
        };

        font.kerning_ltr
            .iter()
            // Only the kerns at our location
            .filter_map(|(master_id, kerns)| {
                font_info
                    .master_positions
                    .get(master_id)
                    .and_then(|pos| (*pos == self.location).then_some((pos, kerns)))
            })
            .flat_map(|(master_pos, kerns)| {
                kerns.iter().map(|((side1, side2), adjustment)| {
                    ((side1, side2), ((*master_pos).clone(), *adjustment))
                })
            })
            .filter_map(|((side1, side2), pos_adjust)| {
                let side1 = kern_participant(glyph_order, groups, SIDE1_PREFIX, side1);
                let side2 = kern_participant(glyph_order, groups, SIDE2_PREFIX, side2);
                let (Some(side1), Some(side2)) = (side1, side2) else {
                    return None;
                };
                Some(((side1, side2), pos_adjust))
            })
            .for_each(|(participants, (_, value))| {
                *kerning.kerns.entry(participants).or_default() = (value as f32).into();
            });

        context.kerning_at.set(kerning);
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
    axis: &fontdrasil::types::Axis,
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

    fn exec(&self, context: &Context) -> Result<(), WorkError> {
        trace!("Generate IR for '{}'", self.glyph_name.as_str());
        let font_info = self.font_info.as_ref();
        let font = &font_info.font;

        let static_metadata = context.static_metadata.get();
        let axes = &static_metadata.all_source_axes;

        let glyph = font
            .glyphs
            .get(self.glyph_name.as_str())
            .ok_or_else(|| WorkError::NoGlyphForName(self.glyph_name.clone()))?;

        let mut ir_glyph = ir::GlyphBuilder::new(self.glyph_name.clone());
        ir_glyph.emit_to_binary = glyph.export;

        ir_glyph.codepoints.extend(glyph.unicode.iter().copied());

        // https://github.com/googlefonts/fontmake-rs/issues/285 glyphs non-spacing marks are 0-width
        let zero_width = glyph.is_nonspacing_mark();

        let mut ir_anchors = AnchorBuilder::new(self.glyph_name.clone());

        // Glyphs have layers that match up with masters, and masters have locations
        let mut axis_positions: HashMap<Tag, HashSet<NormalizedCoord>> = HashMap::new();
        let axes_by_name = font_info.axes.iter().map(|a| (a.tag, a)).collect();
        for instance in glyph.layers.iter() {
            // skip not-yet-supported types of layers (e.g. alternate, color, etc.)
            if !(instance.is_master() || instance.is_intermediate()) {
                continue;
            }
            let master_id = instance
                .associated_master_id
                .as_ref()
                .unwrap_or(&instance.layer_id);
            let Some(master_idx) = font_info.master_indices.get(master_id) else {
                return Err(WorkError::NoMasterForGlyph {
                    master: master_id.clone(),
                    glyph: self.glyph_name.clone(),
                });
            };
            let master = &font.masters[*master_idx];
            let mut location = font_info
                .locations
                .get(&master.axes_values)
                .unwrap()
                .clone();
            // intermediate (aka 'brace') layers can override axis values from their
            // associated master
            if !instance.attributes.coordinates.is_empty() {
                for (tag, coord) in
                    design_location(&font_info.axes, &instance.attributes.coordinates)
                        .to_normalized(&axes_by_name)
                        .iter()
                {
                    location.insert(*tag, *coord);
                }
            }

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
                .try_add_source(&location, glyph_instance)
                .map_err(|e| {
                    WorkError::AddGlyphSource(format!(
                        "Unable to add source to {:?} at {:?}: {}",
                        self.glyph_name, location, e
                    ))
                })?;

            for anchor in instance.anchors.iter() {
                ir_anchors.add(anchor.name.as_str().into(), location.clone(), anchor.pos)?;
            }
        }

        // It's helpful if glyphs are defined at default
        for axis in axes.iter() {
            let default = axis.default.to_normalized(&axis.converter);
            let Some(positions) = axis_positions.get(&axis.tag) else {
                return Err(WorkError::NoAxisPosition(
                    self.glyph_name.clone(),
                    axis.name.clone(),
                ));
            };
            check_pos(&self.glyph_name, positions, axis, &default)?;
        }

        context.anchors.set(ir_anchors.build()?);
        context.glyphs.set(ir_glyph.build()?);
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
        coords::{
            CoordConverter, DesignCoord, NormalizedCoord, NormalizedLocation, UserCoord,
            UserLocation,
        },
        orchestration::{Access, AccessBuilder},
        types::GlyphName,
    };
    use fontir::{
        error::WorkError,
        ir::{AnchorKind, GlobalMetricsInstance, GlyphOrder, NameKey},
        orchestration::{Context, Flags, WorkId},
        paths::Paths,
        source::Source,
        stateset::StateSet,
    };
    use glyphs_reader::{glyphdata::Category, Font};
    use indexmap::IndexSet;
    use write_fonts::types::{NameId, Tag};

    use crate::source::names;

    use pretty_assertions::assert_eq;

    use super::*;

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
            Access::None,
            AccessBuilder::new()
                .variant(WorkId::StaticMetadata)
                .variant(WorkId::PreliminaryGlyphOrder)
                .build(),
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
                .all_source_axes
                .iter()
                .map(|a| a.tag)
                .collect::<Vec<_>>()
        );
        let expected: GlyphOrder = [
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
            Access::None,
            AccessBuilder::new()
                .variant(WorkId::StaticMetadata)
                .variant(WorkId::PreliminaryGlyphOrder)
                .build(),
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
            Access::None,
            AccessBuilder::new()
                .variant(WorkId::StaticMetadata)
                .variant(WorkId::PreliminaryGlyphOrder)
                .build(),
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
                fontdrasil::types::Axis {
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
                fontdrasil::types::Axis {
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
            static_metadata.all_source_axes
        );
    }

    fn build_static_metadata(glyphs_file: PathBuf) -> (impl Source, Context) {
        let _ = env_logger::builder().is_test(true).try_init();
        let (source, context) = context_for(glyphs_file);
        let task_context = context.copy_for_work(
            Access::None,
            AccessBuilder::new()
                .variant(WorkId::StaticMetadata)
                .variant(WorkId::PreliminaryGlyphOrder)
                .build(),
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
            Access::Variant(WorkId::StaticMetadata),
            Access::Variant(WorkId::GlobalMetrics),
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
                Access::Variant(WorkId::PreliminaryGlyphOrder),
                Access::Variant(WorkId::GlyphOrder),
            )
            .glyph_order
            .set((*context.preliminary_glyph_order.get()).clone());

        let work = source.create_kerning_group_ir_work(&context.input).unwrap();
        work.exec(&context.copy_for_work(work.read_access(), work.write_access()))
            .unwrap();

        for location in context.kerning_groups.get().locations.iter() {
            let work = source
                .create_kerning_instance_ir_work(&context.input, location.clone())
                .unwrap();
            work.exec(&context.copy_for_work(work.read_access(), work.write_access()))
                .unwrap();
        }

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
                    Access::Variant(WorkId::StaticMetadata),
                    AccessBuilder::new()
                        .specific_instance(WorkId::Glyph(glyph_name.clone()))
                        .specific_instance(WorkId::Anchor(glyph_name.clone()))
                        .build(),
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
        let axes = static_metadata
            .all_source_axes
            .iter()
            .map(|a| (a.tag, a))
            .collect();

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
    fn read_axis_location() {
        let (_, context) = build_static_metadata(glyphs3_dir().join("WghtVar_AxisLocation.glyphs"));
        let wght = &context.static_metadata.get().all_source_axes;
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

    // check that we're correctly parsing the 'optional 'category' field of glyphs
    #[test]
    fn includes_glyph_category_overrides() {
        let font = Font::load(&glyphs3_dir().join("Oswald-glyph-categories.glyphs")).unwrap();
        assert_eq!(font.glyphs["b"].category, Some(Category::Number));
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
                os2_typo_ascender: 737.0.into(),
                os2_typo_descender: (-42.0).into(),
                os2_typo_line_gap: 421.0.into(),
                os2_win_ascent: 1158.0.into(),
                os2_win_descent: 42.0.into(),
                hhea_ascender: 1158.0.into(),
                hhea_descender: (-42.0).into(),
                hhea_line_gap: 0.0.into(),
                underline_thickness: 50.0.into(),
                underline_position: (-100.0).into(),
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
        let kerns = context.kerning_at.all();
        assert!(!kerns.is_empty());

        let bad_kerns: Vec<_> = kerns
            .iter()
            .map(|(_, kern_at)| &kern_at.location)
            .filter(|pos| !pos.axis_tags().all(|tag| *tag == Tag::new(b"wght")))
            .collect();
        assert!(bad_kerns.is_empty(), "{bad_kerns:#?}");
    }

    #[test]
    fn captures_anchors() {
        let base_name = "A".into();
        let mark_name = "macroncomb".into();
        let (source, context) = build_static_metadata(glyphs3_dir().join("WghtVar_Anchors.glyphs"));
        build_glyphs(&source, &context, &[&base_name, &mark_name]).unwrap();

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
    fn reads_skip_export_glyphs() {
        let (source, context) =
            build_static_metadata(glyphs3_dir().join("WghtVar_NoExport.glyphs"));
        build_glyphs(
            &source,
            &context,
            &[
                &"manual-component".into(),
                &"hyphen".into(),
                &"space".into(),
            ],
        )
        .unwrap();
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

    #[test]
    fn reads_fs_type_0x0000() {
        let (_, context) = build_static_metadata(glyphs3_dir().join("fstype_0x0000.glyphs"));
        assert_eq!(Some(0), context.static_metadata.get().misc.fs_type);
    }

    #[test]
    fn reads_fs_type_0x0104() {
        let (_, context) = build_static_metadata(glyphs3_dir().join("fstype_0x0104.glyphs"));
        assert_eq!(Some(0x104), context.static_metadata.get().misc.fs_type);
    }
}
