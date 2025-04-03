use std::{
    collections::{BTreeMap, BTreeSet, HashMap, HashSet},
    path::Path,
    sync::Arc,
};

use chrono::DateTime;
use log::{debug, trace, warn};

use fontdrasil::{
    coords::{NormalizedCoord, NormalizedLocation},
    orchestration::{Access, AccessBuilder, Work},
    types::GlyphName,
};
use fontir::{
    error::{BadGlyph, BadGlyphKind, BadSource, Error},
    ir::{
        self, AnchorBuilder, Color, ColorPalettes, GdefCategories, GlobalMetric, GlobalMetrics,
        GlyphInstance, GlyphOrder, KernGroup, KernSide, KerningGroups, KerningInstance,
        MetaTableValues, NameBuilder, NameKey, NamedInstance, StaticMetadata, DEFAULT_VENDOR_ID,
    },
    orchestration::{Context, IrWork, WorkId},
    source::Source,
};
use glyphs_reader::{
    glyphdata::{Category, Subcategory},
    Font, InstanceType,
};
use ordered_float::OrderedFloat;
use smol_str::SmolStr;
use write_fonts::{
    tables::{
        gasp::{GaspRange, GaspRangeBehavior},
        gdef::GlyphClassDef,
        os2::SelectionFlags,
    },
    types::{NameId, Tag},
};

use crate::toir::{design_location, to_ir_contours_and_components, to_ir_features, FontInfo};

#[derive(Debug, Clone)]
pub struct GlyphsIrSource {
    glyph_names: Arc<HashSet<GlyphName>>,
    font_info: Arc<FontInfo>,
}

impl GlyphsIrSource {
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
    fn new(glyphs_file: &Path) -> Result<Self, Error> {
        // We have to read the glyphs file then shred it to figure out if anything changed
        let font_info = FontInfo::try_from(Font::load(glyphs_file).map_err(|e| {
            BadSource::custom(
                glyphs_file.to_path_buf(),
                format!("Unable to read glyphs file: {e}"),
            )
        })?)?;

        let glyph_names = font_info
            .font
            .glyphs
            .keys()
            .map(|s| s.as_str().into())
            .collect();

        Ok(Self {
            glyph_names: Arc::new(glyph_names),
            font_info: Arc::new(font_info),
        })
    }

    fn create_static_metadata_work(&self) -> Result<Box<IrWork>, Error> {
        Ok(Box::new(StaticMetadataWork(self.clone())))
    }

    fn create_global_metric_work(&self) -> Result<Box<IrWork>, Error> {
        Ok(Box::new(GlobalMetricWork(self.font_info.clone())))
    }

    fn create_glyph_ir_work(&self) -> Result<Vec<Box<IrWork>>, fontir::error::Error> {
        let mut work: Vec<Box<IrWork>> = Vec::new();
        for glyph_name in self.glyph_names.iter() {
            work.push(Box::new(self.create_work_for_one_glyph(
                glyph_name.clone(),
                self.font_info.clone(),
            )?));
        }
        Ok(work)
    }

    fn create_feature_ir_work(&self) -> Result<Box<IrWork>, Error> {
        Ok(Box::new(FeatureWork(self.font_info.clone())))
    }

    fn create_kerning_group_ir_work(&self) -> Result<Box<IrWork>, Error> {
        Ok(Box::new(KerningGroupWork(self.font_info.clone())))
    }

    fn create_kerning_instance_ir_work(
        &self,
        at: NormalizedLocation,
    ) -> Result<Box<IrWork>, Error> {
        Ok(Box::new(KerningInstanceWork {
            font_info: self.font_info.clone(),
            location: at,
        }))
    }

    fn create_color_palette_work(
        &self,
    ) -> Result<Box<fontir::orchestration::IrWork>, fontir::error::Error> {
        Ok(Box::new(ColorPaletteWork {
            font_info: self.font_info.clone(),
        }))
    }

    fn create_paint_graph_work(
        &self,
    ) -> Result<Box<fontir::orchestration::IrWork>, fontir::error::Error> {
        Ok(Box::new(PaintGraphWork {
            _font_info: self.font_info.clone(),
        }))
    }
}

fn try_name_id(name: &str) -> Option<NameId> {
    match name {
        "copyrights" => Some(NameId::COPYRIGHT_NOTICE),
        "familyNames" => Some(NameId::FAMILY_NAME),
        "uniqueID" => Some(NameId::UNIQUE_ID),
        // Python appears to *only* use this for CFF topDict construction, not name
        // "postscriptFullName" => Some(NameId::FULL_NAME),
        "version" => Some(NameId::VERSION_STRING),
        "postscriptFontName" => Some(NameId::POSTSCRIPT_NAME),
        "trademarks" => Some(NameId::TRADEMARK),
        "manufacturers" => Some(NameId::MANUFACTURER),
        "designers" => Some(NameId::DESIGNER),
        "descriptions" => Some(NameId::DESCRIPTION),
        "manufacturerURL" => Some(NameId::VENDOR_URL),
        "designerURL" => Some(NameId::DESIGNER_URL),
        "licenses" => Some(NameId::LICENSE_DESCRIPTION),
        "licenseURL" => Some(NameId::LICENSE_URL),
        // ttx_diff suggests python doesn't care about this one
        //"compatibleFullNames" => Some(NameId::COMPATIBLE_FULL_NAME),
        "sampleTexts" => Some(NameId::SAMPLE_TEXT),
        "WWSFamilyName" => Some(NameId::WWS_FAMILY_NAME),
        "preferredFamilyNames" => Some(NameId::TYPOGRAPHIC_FAMILY_NAME),
        "preferredSubfamilyNames" => Some(NameId::TYPOGRAPHIC_SUBFAMILY_NAME),
        "variationsPostScriptNamePrefix" => Some(NameId::VARIATIONS_POSTSCRIPT_NAME_PREFIX),
        "vendorID" => None, // handled separately
        _ => {
            warn!("Unknown 'name' entry {name}");
            None
        }
    }
}

fn names(font: &Font, flags: SelectionFlags) -> HashMap<NameKey, String> {
    let mut builder = NameBuilder::default();
    builder.set_version(font.version_major, font.version_minor);
    for (name, value) in font.names.iter() {
        if let Some(name_id) = try_name_id(name) {
            builder.add(name_id, value.clone());
        }
    }

    let subfamily = if flags.contains(SelectionFlags::BOLD | SelectionFlags::ITALIC) {
        "Bold Italic"
    } else if flags.contains(SelectionFlags::BOLD) {
        "Bold"
    } else if flags.contains(SelectionFlags::ITALIC) {
        "Italic"
    } else {
        "Regular"
    };
    builder.add(NameId::SUBFAMILY_NAME, subfamily.to_string());

    // Family name needs to include style, with some mutilation (drop last Regular, Bold, Italic)
    // <https://github.com/googlefonts/glyphsLib/blob/74c63244fdbef1da540d646b0784ae6d2c3ca834/Lib/glyphsLib/builder/names.py#L92>
    let original_family = builder
        .get(NameId::FAMILY_NAME)
        .map(|s| s.to_string())
        .unwrap_or_default();
    let family = NameBuilder::make_family_name(&original_family, &font.default_master().name, true);
    builder.add(NameId::FAMILY_NAME, family.clone());

    if let Some(typographic_family) = &builder
        .get(NameId::TYPOGRAPHIC_FAMILY_NAME)
        .or(Some(&original_family))
    {
        builder.add(
            NameId::TYPOGRAPHIC_FAMILY_NAME,
            typographic_family.to_string(),
        );
    }

    if let Some(typographic_subfamily) = &builder
        .get(NameId::TYPOGRAPHIC_SUBFAMILY_NAME)
        .or(Some(&font.default_master().name))
    {
        builder.add(
            NameId::TYPOGRAPHIC_SUBFAMILY_NAME,
            typographic_subfamily.to_string(),
        );
    }

    let vendor = font
        .vendor_id()
        .map(|v| v.as_str())
        .unwrap_or(DEFAULT_VENDOR_ID);
    builder.apply_default_fallbacks(vendor);

    builder.into_inner()
}

#[derive(Debug)]
struct StaticMetadataWork(GlyphsIrSource);

impl Work<Context, WorkId, Error> for StaticMetadataWork {
    fn id(&self) -> WorkId {
        WorkId::StaticMetadata
    }

    fn also_completes(&self) -> Vec<WorkId> {
        vec![WorkId::PreliminaryGlyphOrder]
    }

    fn exec(&self, context: &Context) -> Result<(), Error> {
        let font_info = self.0.font_info.as_ref();
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
        let global_locations = font
            .masters
            .iter()
            .map(|m| font_info.locations.get(&m.axes_values).cloned().unwrap())
            .collect();

        let number_values = get_number_values(font_info, font);

        // negate the italic angle because it's clockwise in Glyphs.app whereas it's
        // counter-clockwise in UFO/OpenType and our GlobalMetrics follow the latter
        // https://github.com/googlefonts/glyphsLib/blob/f162e7/Lib/glyphsLib/builder/masters.py#L36
        let italic_angle = font
            .default_master()
            .italic_angle()
            .map(|v| -v)
            .unwrap_or(0.0);

        let mut selection_flags = match font.custom_parameters.use_typo_metrics.unwrap_or_default() {
            true => SelectionFlags::USE_TYPO_METRICS,
            false => SelectionFlags::empty(),
        } | match font.custom_parameters.has_wws_names.unwrap_or_default() {
            true => SelectionFlags::WWS,
            false => SelectionFlags::empty(),
        } |
        // if there is an italic angle we're italic
        // <https://github.com/googlefonts/glyphsLib/blob/74c63244fdbef1da540d646b0784ae6d2c3ca834/Lib/glyphsLib/builder/names.py#L25>
        match italic_angle {
            0.0 => SelectionFlags::empty(),
            _ => SelectionFlags::ITALIC,
        } |
        // https://github.com/googlefonts/glyphsLib/blob/42bc1db912fd4b66f130fb3bdc63a0c1e774eb38/Lib/glyphsLib/builder/names.py#L27
        match font.default_master().name.to_ascii_lowercase().as_str() {
            "italic" => SelectionFlags::ITALIC,
            "bold" => SelectionFlags::BOLD,
            "bold italic" => SelectionFlags::BOLD | SelectionFlags::ITALIC,
            _ => SelectionFlags::empty(),
        };
        if selection_flags.intersection(SelectionFlags::ITALIC | SelectionFlags::BOLD)
            == SelectionFlags::empty()
        {
            selection_flags |= SelectionFlags::REGULAR;
        }

        let categories = make_glyph_categories(font);

        let mut static_metadata = StaticMetadata::new(
            font.units_per_em,
            names(font, selection_flags),
            axes,
            named_instances,
            global_locations,
            Default::default(), // TODO: impl reading PS names from Glyphs
            italic_angle,
            categories,
            number_values,
        )
        .map_err(Error::VariationModelError)?;
        static_metadata.misc.selection_flags = selection_flags;
        // treat "    " (four spaces) as equivalent to no value; it means
        // 'null', per the spec
        if let Some(vendor_id) = font.vendor_id().filter(|id| *id != "    ") {
            static_metadata.misc.vendor_id =
                vendor_id.parse().map_err(|cause| Error::InvalidTag {
                    raw_tag: vendor_id.to_owned(),
                    cause,
                })?;
        }

        // Default per <https://github.com/googlefonts/glyphsLib/blob/cb8a4a914b0a33431f0a77f474bf57eec2f19bcc/Lib/glyphsLib/builder/custom_params.py#L1117-L1119>
        static_metadata.misc.fs_type = font.custom_parameters.fs_type.or(Some(1 << 3));

        static_metadata.misc.is_fixed_pitch = font.custom_parameters.is_fixed_pitch;

        static_metadata.misc.unicode_range_bits = font
            .custom_parameters
            .unicode_range_bits
            .as_ref()
            .map(|bits| bits.iter().copied().collect());
        static_metadata.misc.codepage_range_bits = font
            .custom_parameters
            .codepage_range_bits
            .as_ref()
            .map(|bits| bits.iter().copied().collect());

        let default_master = font.default_master();
        let default_instance = font.variable_export_settings(default_master);
        if let Some(raw_panose) = default_instance
            .and_then(|di| di.custom_parameters.panose.as_ref())
            .or(default_master.custom_parameters.panose.as_ref())
            .or(font.custom_parameters.panose.as_ref())
        {
            let mut bytes = [0u8; 10];
            bytes
                .iter_mut()
                .zip(raw_panose)
                .for_each(|(dst, src)| *dst = *src as u8);
            static_metadata.misc.panose = Some(bytes.into());
        }

        static_metadata.misc.version_major = font.version_major;
        static_metadata.misc.version_minor = font.version_minor;
        if let Some(lowest_rec_ppm) = font.custom_parameters.lowest_rec_ppem {
            static_metadata.misc.lowest_rec_ppm = lowest_rec_ppm as _;
        }

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

        if let Some(meta_table) = &font.custom_parameters.meta_table {
            static_metadata.misc.meta_table = Some(MetaTableValues {
                dlng: meta_table.dlng.clone(),
                slng: meta_table.slng.clone(),
            });
        }

        if let Some(gasp) = &font.custom_parameters.gasp_table {
            for (max_ppem, behavior) in gasp.iter() {
                let Ok(range_max_ppem) = (*max_ppem).try_into() else {
                    warn!(
                        "Invalid gasp entry, rangeMaxPPEM {max_ppem} out of bounds, ignoring range"
                    );
                    continue;
                };
                let range_gasp_behavior = GaspRangeBehavior::from_bits_truncate(*behavior as u16);
                if range_gasp_behavior == GaspRangeBehavior::empty() {
                    warn!("Invalid gasp entry at rangeMaxPPEM {max_ppem}, no behavior bits set by {behavior}, ignoring range");
                    continue;
                }
                static_metadata.misc.gasp.push(GaspRange {
                    range_max_ppem,
                    range_gasp_behavior,
                });
            }
        }

        context.static_metadata.set(static_metadata);

        let glyph_order = font
            .glyph_order
            .iter()
            .map(|s| s.as_str().into())
            .filter(|gn| self.0.glyph_names.contains(gn))
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

fn get_number_values(
    fontinfo: &FontInfo,
    font: &Font,
) -> Option<HashMap<NormalizedLocation, BTreeMap<SmolStr, OrderedFloat<f64>>>> {
    if font.default_master().number_values.is_empty() {
        return None;
    }
    let values = font
        .masters
        .iter()
        .map(|m| {
            let location = fontinfo.locations.get(&m.axes_values).cloned().unwrap();
            (location, m.number_values.clone())
        })
        .collect();
    Some(values)
}

/// determine the GDEF category for this glyph, if appropriate
// see
// <https://github.com/googlefonts/glyphsLib/blob/e2ebf5b517/Lib/glyphsLib/builder/features.py#L205>
fn category_for_glyph(glyph: &glyphs_reader::Glyph) -> Option<GlyphClassDef> {
    let has_attaching_anchor = glyph
        .layers
        .iter()
        .flat_map(|layer| layer.anchors.iter())
        // glyphsLib considers any anchor that does not start with '_' as an
        // 'attaching anchor'; see https://github.com/googlefonts/glyphsLib/issues/1024
        .any(|anchor| !anchor.name.starts_with('_'));
    match (glyph.category, glyph.sub_category) {
        (_, Some(Subcategory::Ligature)) if has_attaching_anchor => Some(GlyphClassDef::Ligature),
        (
            Some(Category::Mark),
            Some(Subcategory::Nonspacing) | Some(Subcategory::SpacingCombining),
        ) => Some(GlyphClassDef::Mark),
        _ if has_attaching_anchor => Some(GlyphClassDef::Base),
        _ => None,
    }
}

#[derive(Debug)]
struct GlobalMetricWork(Arc<FontInfo>);

impl Work<Context, WorkId, Error> for GlobalMetricWork {
    fn id(&self) -> WorkId {
        WorkId::GlobalMetrics
    }

    fn read_access(&self) -> Access<WorkId> {
        Access::Variant(WorkId::StaticMetadata)
    }

    fn exec(&self, context: &Context) -> Result<(), Error> {
        let font_info = self.0.as_ref();
        let font = &font_info.font;
        debug!(
            "Global metrics for {}",
            font.names
                .get("familyNames")
                .map(|s| s.as_str())
                .unwrap_or("<nameless family>")
        );

        let static_metadata = context.static_metadata.get();
        let mut metrics = GlobalMetrics::new();

        for master in font.masters.iter() {
            let pos = font_info.locations.get(&master.axes_values).unwrap();

            // glyphsLib <https://github.com/googlefonts/glyphsLib/blob/1cb4fc5ae2/Lib/glyphsLib/classes.py#L1590-L1601>
            let cap_height = master.cap_height().unwrap_or(700.0);
            let x_height = master.x_height().unwrap_or(500.0);
            let ascender = master.ascender().unwrap_or(800.0);
            let descender = master.descender().unwrap_or(-200.0);

            metrics.set_if_some(GlobalMetric::CapHeight, pos.clone(), Some(cap_height));
            metrics.set_if_some(GlobalMetric::XHeight, pos.clone(), Some(x_height));

            // Some .glyphs files have a negative win descent so abs()
            // we don't use the macro here because of the special abs() logic
            metrics.set_if_some(
                GlobalMetric::Os2WinDescent,
                pos.clone(),
                master
                    .custom_parameters
                    .win_descent
                    .or(font.custom_parameters.win_descent)
                    .map(|v| v.abs() as f64),
            );

            macro_rules! set_metric {
                // the most common pattern
                ($variant:ident, $field_name:ident) => {
                    set_metric!(
                        $variant,
                        master
                            .custom_parameters
                            .$field_name
                            .or(font.custom_parameters.$field_name)
                            .map(|v| v as f64)
                    )
                };
                // a few fields have a manual default though
                ($variant:ident, $field_name:ident, $fallback:literal) => {
                    set_metric!(
                        $variant,
                        master
                            .custom_parameters
                            .$field_name
                            .or(font.custom_parameters.$field_name)
                            .or(Some($fallback.into()))
                    )
                };
                // base case, both branches above resolve to this
                ($variant:ident, $getter:expr ) => {
                    metrics.set_if_some(GlobalMetric::$variant, pos.clone(), $getter)
                };
            }
            set_metric!(Os2TypoAscender, typo_ascender);
            set_metric!(Os2TypoDescender, typo_descender);
            set_metric!(Os2TypoLineGap, typo_line_gap);
            set_metric!(Os2WinAscent, win_ascent);
            set_metric!(StrikeoutPosition, strikeout_position);
            set_metric!(StrikeoutSize, strikeout_size);
            set_metric!(SubscriptXOffset, subscript_x_offset);
            set_metric!(SubscriptXSize, subscript_x_size);
            set_metric!(SubscriptYOffset, subscript_y_offset);
            set_metric!(SubscriptYSize, subscript_y_size);
            set_metric!(SuperscriptXOffset, superscript_x_offset);
            set_metric!(SuperscriptXSize, superscript_x_size);
            set_metric!(SuperscriptYOffset, superscript_y_offset);
            set_metric!(SuperscriptYSize, superscript_y_size);
            set_metric!(HheaAscender, hhea_ascender);
            set_metric!(HheaDescender, hhea_descender);
            set_metric!(HheaLineGap, hhea_line_gap);
            set_metric!(CaretSlopeRun, hhea_caret_slope_run);
            set_metric!(CaretSlopeRise, hhea_caret_slope_rise);
            set_metric!(CaretOffset, hhea_caret_offset);
            // 50.0 is the Glyphs default <https://github.com/googlefonts/glyphsLib/blob/9d5828d874110c42dfc5f542db8eb84f88641eb5/Lib/glyphsLib/builder/custom_params.py#L1136-L1156>
            set_metric!(UnderlineThickness, underline_thickness, 50.0);
            // -100.0 is the Glyphs default <https://github.com/googlefonts/glyphsLib/blob/9d5828d874110c42dfc5f542db8eb84f88641eb5/Lib/glyphsLib/builder/custom_params.py#L1136-L1156>
            set_metric!(UnderlinePosition, underline_position, -100.0);

            metrics.populate_defaults(
                pos,
                static_metadata.units_per_em,
                Some(x_height),
                Some(ascender),
                Some(descender),
                // turn clockwise angle counter-clockwise
                master.italic_angle().map(|v| -v),
            );
        }

        context.global_metrics.set(metrics);
        Ok(())
    }
}

#[derive(Debug)]
struct FeatureWork(Arc<FontInfo>);

impl Work<Context, WorkId, Error> for FeatureWork {
    fn id(&self) -> WorkId {
        WorkId::Features
    }

    fn exec(&self, context: &Context) -> Result<(), Error> {
        trace!("Generate features");
        let font_info = self.0.as_ref();
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
struct KerningGroupWork(Arc<FontInfo>);

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

impl Work<Context, WorkId, Error> for KerningGroupWork {
    fn id(&self) -> WorkId {
        WorkId::KerningGroups
    }

    fn read_access(&self) -> Access<WorkId> {
        Access::None
    }

    fn exec(&self, context: &Context) -> Result<(), Error> {
        trace!("Generate IR for kerning");
        let font_info = self.0.as_ref();
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

impl Work<Context, WorkId, Error> for KerningInstanceWork {
    fn id(&self) -> WorkId {
        WorkId::KernInstance(self.location.clone())
    }

    fn read_access(&self) -> Access<WorkId> {
        AccessBuilder::new()
            .variant(WorkId::GlyphOrder)
            .variant(WorkId::KerningGroups)
            .build()
    }

    fn exec(&self, context: &Context) -> Result<(), Error> {
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
                *kerning.kerns.entry(participants).or_default() = value;
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
) -> Result<(), BadGlyph> {
    if !positions.contains(pos) {
        return Err(BadGlyph::new(
            glyph_name.clone(),
            BadGlyphKind::UndefinedAtNormalizedPosition {
                axis: axis.tag,
                pos: pos.to_owned(),
            },
        ));
    }
    Ok(())
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
        trace!("Generate IR for '{}'", self.glyph_name.as_str());
        let font_info = self.font_info.as_ref();
        let font = &font_info.font;

        let static_metadata = context.static_metadata.get();
        let axes = &static_metadata.all_source_axes;

        let glyph = font
            .glyphs
            .get(self.glyph_name.as_str())
            .ok_or_else(|| Error::NoGlyphForName(self.glyph_name.clone()))?;

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
                return Err(BadGlyph::new(
                    self.glyph_name.clone(),
                    BadGlyphKind::MissingMaster(master_id.clone()),
                )
                .into());
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

            ir_glyph.try_add_source(&location, glyph_instance)?;

            for anchor in instance.anchors.iter() {
                ir_anchors.add(anchor.name.as_str().into(), location.clone(), anchor.pos)?;
            }
        }

        // It's helpful if glyphs are defined at default
        for axis in axes.iter() {
            let default = axis.default.to_normalized(&axis.converter);
            let Some(positions) = axis_positions.get(&axis.tag) else {
                return Err(BadGlyph::new(
                    self.glyph_name.clone(),
                    BadGlyphKind::NoAxisPosition(axis.tag),
                )
                .into());
            };
            check_pos(&self.glyph_name, positions, axis, &default)?;
        }

        context.anchors.set(ir_anchors.build()?);
        context.glyphs.set(ir_glyph.build()?);
        Ok(())
    }
}

#[derive(Debug)]
struct ColorPaletteWork {
    font_info: Arc<FontInfo>,
}

impl Work<Context, WorkId, Error> for ColorPaletteWork {
    fn id(&self) -> WorkId {
        WorkId::ColorPalettes
    }

    fn read_access(&self) -> Access<WorkId> {
        Access::None
    }

    fn write_access(&self) -> Access<WorkId> {
        Access::Variant(WorkId::ColorPalettes)
    }

    fn exec(&self, context: &Context) -> Result<(), Error> {
        // Since we're only [for now, baby steps] producing one palette we can dedup it
        let palette = self
            .font_info
            .font
            .glyphs
            .values()
            .flat_map(|g| {
                g.layers
                    .iter()
                    .flat_map(|l| l.shapes.iter())
                    .flat_map(|s| (s.attributes().gradient.colors.iter()))
            })
            .map(|c| Color {
                r: c.r as u8,
                g: c.g as u8,
                b: c.b as u8,
                a: c.a as u8,
            })
            .collect::<Vec<_>>();
        debug!("{} color palette entries", palette.len());
        match ColorPalettes::new(vec![palette]) {
            Ok(Some(palettes)) => {
                context.colors.set(palettes);
                Ok(())
            }
            Ok(None) => Ok(()),
            Err(e) => Err(e),
        }
    }
}

#[derive(Debug)]
struct PaintGraphWork {
    _font_info: Arc<FontInfo>,
}

impl Work<Context, WorkId, Error> for PaintGraphWork {
    fn id(&self) -> WorkId {
        WorkId::PaintGraph
    }

    fn read_access(&self) -> Access<WorkId> {
        Access::None
    }

    fn write_access(&self) -> Access<WorkId> {
        Access::Variant(WorkId::PaintGraph)
    }

    fn exec(&self, _context: &Context) -> Result<(), Error> {
        debug!("TODO: actually create paint graph");
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::{
        collections::HashSet,
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
        error::Error,
        ir::{AnchorKind, GlobalMetricsInstance, GlyphOrder, NameKey},
        orchestration::{Context, Flags, WorkId},
        paths::Paths,
        source::Source,
    };
    use glyphs_reader::{glyphdata::Category, Font};

    use ir::{test_helpers::Round2, Panose};
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

    fn glyphs_of(glyphs_file: PathBuf) -> HashSet<GlyphName> {
        let font = Font::load(&glyphs_file).unwrap();
        font.glyph_order.iter().map(|s| s.as_str().into()).collect()
    }

    #[test]
    fn find_wght_var_glyphs() {
        assert_eq!(
            HashSet::from([
                GlyphName::new("space"),
                "hyphen".into(),
                "exclam".into(),
                "bracketleft".into(),
                "bracketright".into(),
                "manual-component".into()
            ]),
            glyphs_of(glyphs3_dir().join("WghtVar.glyphs"))
        );
    }

    #[test]
    fn find_wght_var_heavy_hyphen_glyphs() {
        assert_eq!(
            HashSet::from([GlyphName::new("space"), "hyphen".into(), "exclam".into()]),
            glyphs_of(glyphs3_dir().join("WghtVar_HeavyHyphen.glyphs"))
        );
    }

    fn context_for(glyphs_file: &Path) -> (impl Source, Context) {
        let source = GlyphsIrSource::new(glyphs_file).unwrap();
        let mut flags = Flags::default();
        flags.set(Flags::EMIT_IR, false); // we don't want to write anything down
        (
            source,
            Context::new_root(flags, Paths::new(Path::new("/nothing/should/write/here"))),
        )
    }

    #[test]
    fn static_metadata_ir() {
        let (source, context) = context_for(&glyphs3_dir().join("WghtVar.glyphs"));
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
        let (source, context) = context_for(&glyphs2_dir().join("BadIndexing.glyphs"));
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
    }

    #[test]
    fn loads_axis_mappings_from_glyphs2() {
        let (source, context) = context_for(&glyphs2_dir().join("OpszWghtVar_AxisMappings.glyphs"));
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
        let (source, context) = context_for(&glyphs_file);
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

    fn build_global_metrics(glyphs_file: PathBuf) -> (impl Source, Context) {
        let (source, context) = build_static_metadata(glyphs_file);
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

    fn build_glyphs(source: &impl Source, context: &Context) -> Result<(), Error> {
        let work_items = source.create_glyph_ir_work().unwrap();
        for work in work_items.iter() {
            let WorkId::Glyph(glyph_name) = work.id() else {
                panic!("{:?} should be glyph work!", work.id());
            };
            let task_context = context.copy_for_work(
                Access::Variant(WorkId::StaticMetadata),
                AccessBuilder::new()
                    .specific_instance(WorkId::Glyph(glyph_name.clone()))
                    .specific_instance(WorkId::Anchor(glyph_name.clone()))
                    .build(),
            );
            work.exec(&task_context)?;
        }
        Ok(())
    }

    #[test]
    fn glyph_user_locations() {
        let glyph_name: GlyphName = "space".into();
        let (source, context) =
            build_static_metadata(glyphs2_dir().join("OpszWghtVar_AxisMappings.glyphs"));
        build_glyphs(&source, &context).unwrap(); // we dont' care about geometry

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
            .get_glyph(glyph_name)
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
        build_glyphs(&source, &context).unwrap();

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
            .get_glyph(glyph_name)
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
        build_glyphs(&source, &context).unwrap();
        let glyph = context.glyphs.get(&WorkId::Glyph("hyphen".into()));
        assert_eq!(HashSet::from([0x002d]), glyph.codepoints);
    }

    #[test]
    fn captures_single_codepoints_unquoted_dec() {
        let (source, context) =
            build_static_metadata(glyphs3_dir().join("Unicode-UnquotedDec.glyphs"));
        build_glyphs(&source, &context).unwrap();
        let glyph = context.glyphs.get(&WorkId::Glyph("name".into()));
        assert_eq!(HashSet::from([182]), glyph.codepoints);
    }

    #[test]
    fn captures_multiple_codepoints_unquoted_dec() {
        let (source, context) =
            build_static_metadata(glyphs3_dir().join("Unicode-UnquotedDecSequence.glyphs"));
        build_glyphs(&source, &context).unwrap();
        let glyph = context.glyphs.get(&WorkId::Glyph("name".into()));
        assert_eq!(HashSet::from([1619, 1764]), glyph.codepoints);
    }

    // check that we're correctly parsing the 'optional 'category' field of glyphs
    #[test]
    fn includes_glyph_category_overrides() {
        let font = Font::load(&glyphs3_dir().join("Oswald-glyph-categories.glyphs")).unwrap();
        assert_eq!(font.glyphs["b"].category, Some(Category::Number));
    }

    #[test]
    fn category_can_be_none() {
        let font = Font::load(&glyphs3_dir().join("Oswald-glyph-categories.glyphs")).unwrap();
        let glyph = &font.glyphs["U1C82"];
        assert!(glyph.category.is_none());

        // even though glyphs does not assign this a category, we still determine
        // it is a base based on the presence of base anchors.
        assert_eq!(category_for_glyph(glyph), Some(GlyphClassDef::Base));
    }

    // It's so minimal it's a good test
    #[test]
    fn loads_minimal() {
        let (_, context) = build_static_metadata(glyphs2_dir().join("NotDef.glyphs"));
        assert_eq!(1000, context.static_metadata.get().units_per_em);
    }

    fn the_best_names() -> Vec<(NameKey, String)> {
        vec![
            (
                NameKey::new_bmp_only(NameId::COPYRIGHT_NOTICE),
                String::from("Copy!"),
            ),
            (
                NameKey::new_bmp_only(NameId::FAMILY_NAME),
                String::from("FamilyName"),
            ),
            (
                NameKey::new_bmp_only(NameId::SUBFAMILY_NAME),
                String::from("Bold"),
            ),
            (
                NameKey::new_bmp_only(NameId::UNIQUE_ID),
                String::from("We are all unique"),
            ),
            (
                NameKey::new_bmp_only(NameId::FULL_NAME),
                String::from("FamilyName Bold"),
            ),
            (
                NameKey::new_bmp_only(NameId::VERSION_STRING),
                String::from("New Value"),
            ),
            (
                NameKey::new_bmp_only(NameId::POSTSCRIPT_NAME),
                String::from("Postscript Name"),
            ),
            (
                NameKey::new_bmp_only(NameId::TRADEMARK),
                String::from("A trade in marks"),
            ),
            (
                NameKey::new_bmp_only(NameId::MANUFACTURER),
                String::from("Who made you?!"),
            ),
            (
                NameKey::new_bmp_only(NameId::DESIGNER),
                String::from("Designed by me!"),
            ),
            (
                NameKey::new_bmp_only(NameId::DESCRIPTION),
                String::from("The greatest weight var"),
            ),
            (
                NameKey::new_bmp_only(NameId::VENDOR_URL),
                String::from("https://example.com/manufacturer"),
            ),
            (
                NameKey::new_bmp_only(NameId::DESIGNER_URL),
                String::from("https://example.com/designer"),
            ),
            (
                NameKey::new_bmp_only(NameId::LICENSE_DESCRIPTION),
                String::from("Licensed to thrill"),
            ),
            (
                NameKey::new_bmp_only(NameId::LICENSE_URL),
                String::from("https://example.com/my/font/license"),
            ),
            (
                NameKey::new_bmp_only(NameId::SAMPLE_TEXT),
                String::from("Sam pull text"),
            ),
            (
                NameKey::new_bmp_only(NameId::WWS_FAMILY_NAME),
                String::from("We Will Slant you"),
            ),
        ]
    }

    fn update_expected_names(names: &mut Vec<(NameKey, String)>, updates: Vec<(NameKey, String)>) {
        names.retain(|(k, _)| !updates.iter().any(|(k2, _)| k == k2));
        names.extend(updates);
        names.sort_by_key(|(k, _)| k.name_id.to_u16());
    }

    #[test]
    fn name_table() {
        let font = Font::load(&glyphs3_dir().join("TheBestNames.glyphs")).unwrap();
        let mut names: Vec<_> = names(&font, SelectionFlags::BOLD).into_iter().collect();
        names.sort_by_key(|(id, v)| (id.name_id, v.clone()));
        // typographic family name == family name and should NOT be present
        assert_eq!(the_best_names(), names);
    }

    #[test]
    fn name_table_with_basic_names_light_origin() {
        let font = Font::load(&glyphs3_dir().join("LightOriginNames.glyphs")).unwrap();
        let mut names: Vec<_> = names(&font, SelectionFlags::REGULAR).into_iter().collect();
        names.sort_by_key(|(id, v)| (id.name_id, v.clone()));

        // Expected taken from fontmake output
        assert_eq!(
            vec![
                (
                    NameKey::new_bmp_only(NameId::FAMILY_NAME),
                    String::from("FamilyName Light"),
                ),
                (
                    NameKey::new_bmp_only(NameId::SUBFAMILY_NAME),
                    String::from("Regular"),
                ),
                (
                    NameKey::new_bmp_only(NameId::UNIQUE_ID),
                    String::from("42.042;NONE;FamilyName-Light"),
                ),
                (
                    NameKey::new_bmp_only(NameId::FULL_NAME),
                    String::from("FamilyName Light"),
                ),
                (
                    NameKey::new_bmp_only(NameId::VERSION_STRING),
                    String::from("Version 42.042"),
                ),
                (
                    NameKey::new_bmp_only(NameId::POSTSCRIPT_NAME),
                    String::from("FamilyName-Light"),
                ),
                (
                    NameKey::new_bmp_only(NameId::TYPOGRAPHIC_FAMILY_NAME),
                    String::from("FamilyName"),
                ),
                (
                    NameKey::new_bmp_only(NameId::TYPOGRAPHIC_SUBFAMILY_NAME),
                    String::from("Light"),
                ),
            ],
            names
        )
    }

    #[test]
    fn name_table_with_preferred_names() {
        let font = Font::load(&glyphs3_dir().join("PreferableNames.glyphs")).unwrap();
        let mut names: Vec<_> = names(&font, SelectionFlags::REGULAR).into_iter().collect();
        names.sort_by_key(|(id, v)| (id.name_id, v.clone()));
        // typographic family and subfamily should be present now
        let mut expected_names = the_best_names();
        update_expected_names(
            &mut expected_names,
            vec![
                (
                    NameKey::new_bmp_only(NameId::FAMILY_NAME),
                    "FamilyName".to_string(),
                ),
                (
                    NameKey::new_bmp_only(NameId::FULL_NAME),
                    "Pref Family Name Pref Regular".to_string(),
                ),
                (
                    NameKey::new_bmp_only(NameId::SUBFAMILY_NAME),
                    "Regular".to_string(),
                ),
                (
                    NameKey::new_bmp_only(NameId::TYPOGRAPHIC_FAMILY_NAME),
                    "Pref Family Name".to_string(),
                ),
                (
                    NameKey::new_bmp_only(NameId::TYPOGRAPHIC_SUBFAMILY_NAME),
                    "Pref Regular".to_string(),
                ),
                (
                    NameKey::new_bmp_only(NameId::VARIATIONS_POSTSCRIPT_NAME_PREFIX),
                    "Name 25?!".to_string(),
                ),
            ],
        );
        assert_eq!(expected_names, names);
    }

    #[test]
    fn version_with_version_string() {
        let font = Font::load(&glyphs3_dir().join("TheBestNames.glyphs")).unwrap();
        assert_eq!(
            "New Value",
            names(&font, SelectionFlags::empty())
                .get(&NameKey::new_bmp_only(NameId::VERSION_STRING))
                .unwrap()
        );
    }

    #[test]
    fn version_from_major_minor() {
        let font = Font::load(&glyphs3_dir().join("VersionMajorMinor.glyphs")).unwrap();
        assert_eq!(
            "Version 42.043",
            names(&font, SelectionFlags::empty())
                .get(&NameKey::new_bmp_only(NameId::VERSION_STRING))
                .unwrap()
        );
    }

    #[test]
    fn version_default() {
        let font = Font::load(&glyphs3_dir().join("infinity.glyphs")).unwrap();
        assert_eq!(
            "Version 0.000",
            names(&font, SelectionFlags::empty())
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
            default_metrics.round2()
        );
    }

    #[test]
    fn captures_abs_of_win_descent() {
        let (_, context) = build_global_metrics(glyphs3_dir().join("MVAR.glyphs"));
        let static_metadata = &context.static_metadata.get();
        let default_metrics = context
            .global_metrics
            .get()
            .at(static_metadata.default_location());
        assert_eq!(200.0, default_metrics.os2_win_descent.0);
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
            build_glyphs(&source, &context).unwrap();
            let glyph = context.get_glyph(glyph_name);
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
        build_glyphs(&source, &context).unwrap();

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
        build_glyphs(&source, &context).unwrap();
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

    #[test]
    fn captures_global_metrics_from_font() {
        let (_, context) =
            build_global_metrics(glyphs3_dir().join("GlobalMetrics_font_customParameters.glyphs"));
        let static_metadata = &context.static_metadata.get();
        let default_metrics = context
            .global_metrics
            .get()
            .at(static_metadata.default_location());
        assert_eq!(
            (
                NormalizedLocation::for_pos(&[("wght", 0.0)]),
                GlobalMetricsInstance {
                    pos: static_metadata.default_location().clone(),
                    ascender: 800.0.into(),
                    descender: (-200.0).into(),
                    caret_slope_rise: 1000.0.into(),
                    cap_height: 660.0.into(),
                    x_height: 478.0.into(),
                    subscript_x_size: 650.0.into(),
                    subscript_y_size: 600.0.into(),
                    subscript_y_offset: 75.0.into(),
                    superscript_x_size: 650.0.into(),
                    superscript_y_size: 600.0.into(),
                    superscript_y_offset: 350.0.into(),
                    strikeout_position: 286.8.into(),
                    strikeout_size: 40.0.into(), // fallback to underline thickness
                    os2_typo_ascender: 950.0.into(),
                    os2_typo_descender: (-350.0).into(),
                    os2_typo_line_gap: 0.0.into(),
                    os2_win_ascent: 1185.0.into(),
                    os2_win_descent: 420.0.into(),
                    hhea_ascender: 950.0.into(),
                    hhea_descender: (-350.0).into(),
                    hhea_line_gap: 0.0.into(),
                    underline_thickness: 40.0.into(), // overridden from global value
                    underline_position: (-300.0).into(),
                    ..Default::default()
                }
            ),
            (static_metadata.default_location().clone(), default_metrics)
        );
        let black = NormalizedLocation::for_pos(&[("wght", 1.0)]);
        let black_metrics = context.global_metrics.get().at(&black);
        assert_eq!(
            GlobalMetricsInstance {
                pos: black.clone(),
                ascender: 800.0.into(),
                descender: (-200.0).into(),
                caret_slope_rise: 1000.0.into(),
                cap_height: 650.0.into(),
                x_height: 500.0.into(),
                subscript_x_size: 650.0.into(),
                subscript_y_size: 600.0.into(),
                subscript_y_offset: 75.0.into(),
                superscript_x_size: 650.0.into(),
                superscript_y_size: 600.0.into(),
                superscript_y_offset: 350.0.into(),
                strikeout_position: 300.0.into(),
                strikeout_size: 42.0.into(), // fallback to underline thickness
                os2_typo_ascender: 1000.0.into(),
                os2_typo_descender: (-400.0).into(),
                os2_typo_line_gap: 0.0.into(),
                os2_win_ascent: 1185.0.into(),
                os2_win_descent: 420.0.into(),
                hhea_ascender: 1000.0.into(),
                hhea_descender: (-400.0).into(),
                hhea_line_gap: 0.0.into(),
                underline_thickness: 42.0.into(), // global value
                underline_position: (-300.0).into(),
                ..Default::default()
            },
            black_metrics
        );
    }

    fn unique_value(metrics: &GlobalMetrics, metric: GlobalMetric) -> f64 {
        let values = metrics
            .iter()
            .filter_map(|(which_metric, values)| {
                if metric == *which_metric {
                    Some(values.values().collect::<HashSet<_>>())
                } else {
                    None
                }
            })
            .flat_map(|v| v.into_iter())
            .collect::<Vec<_>>();

        assert_eq!(1, values.len(), "Too many {metric:?} values: {values:?}");
        values[0].0
    }

    #[test]
    fn glyphs_specific_default_underline_metrics() {
        // 1290 upem in honor of soft-type-jacquard/sources/Jacquard24Charted.glyphs which used to not match fontmake
        let (_, context) = build_global_metrics(glyphs3_dir().join("WghtVar1290upem.glyphs"));
        let metrics = context.global_metrics.get();
        let underline_pos = unique_value(&metrics, GlobalMetric::UnderlinePosition);
        let underline_thickness = unique_value(&metrics, GlobalMetric::UnderlineThickness);
        assert_eq!((-100.0, 50.0), (underline_pos, underline_thickness));
    }

    #[test]
    fn glyphs_specific_default_height_metrics() {
        let (_, context) = build_global_metrics(glyphs3_dir().join("WghtVar1290upem.glyphs"));
        let metrics = context.global_metrics.get();
        let x_height = unique_value(&metrics, GlobalMetric::XHeight);
        let cap_height = unique_value(&metrics, GlobalMetric::CapHeight);
        let ascender = unique_value(&metrics, GlobalMetric::Ascender);
        let descender = unique_value(&metrics, GlobalMetric::Descender);
        assert_eq!(
            (500.0, 700.0, 800.0, -200.0),
            (x_height, cap_height, ascender, descender)
        );
    }

    #[test]
    fn captures_panose() {
        // short parameter name
        let (_, context) = build_static_metadata(glyphs3_dir().join("WghtVarPanose.glyphs"));
        assert_eq!(
            Some([2, 0, 5, 3, 6, 0, 0, 2, 0, 3].into()),
            context.static_metadata.get().misc.panose
        );
    }

    #[test]
    fn captures_panose_long() {
        // long parameter name
        let (_, context) = build_static_metadata(glyphs3_dir().join("WghtVarPanoseLong.glyphs"));
        assert_eq!(
            Some([2, 0, 5, 3, 6, 0, 0, 2, 0, 3].into()),
            context.static_metadata.get().misc.panose
        );
    }

    #[test]
    fn captures_panose_precedence() {
        // both parameters; value under short name should be preferred
        let (_, context) = build_static_metadata(glyphs3_dir().join("WghtVarPanoseBoth.glyphs"));
        assert_eq!(
            Some([2, 0, 5, 3, 6, 0, 0, 2, 0, 3].into()),
            context.static_metadata.get().misc.panose
        );
    }

    #[test]
    fn capture_unsual_params() {
        // both parameters; value under short name should be preferred
        let (_, context) = build_static_metadata(glyphs3_dir().join("UnusualCustomParams.glyphs"));
        let meta = context.static_metadata.get();
        assert_eq!(meta.misc.lowest_rec_ppm, 7);

        // some of these end up as metrics:
        let (_, context) = build_global_metrics(glyphs3_dir().join("UnusualCustomParams.glyphs"));
        let metrics = context.global_metrics.get();
        assert_eq!(unique_value(&metrics, GlobalMetric::CaretOffset), 2.);
        assert_eq!(unique_value(&metrics, GlobalMetric::CaretSlopeRise), 1.);
        assert_eq!(unique_value(&metrics, GlobalMetric::CaretSlopeRun), 5.);
    }

    // <https://github.com/googlefonts/fontc/issues/1157>
    #[test]
    fn identifies_italicness() {
        let (_, context) = build_static_metadata(glyphs3_dir().join("An-Italic.glyphs"));
        let static_metadata = context.static_metadata.get();
        let selection_flags = static_metadata.misc.selection_flags;
        let name = |id: NameId| {
            static_metadata
                .names
                .get(&NameKey::new_bmp_only(id))
                .map(|s| s.as_str())
                .unwrap_or_default()
        };

        // Correct values taken from fontmake
        assert_eq!(
            (
                name(NameId::FAMILY_NAME),
                name(NameId::SUBFAMILY_NAME),
                name(NameId::UNIQUE_ID),
                name(NameId::FULL_NAME),
                name(NameId::POSTSCRIPT_NAME),
                name(NameId::TYPOGRAPHIC_FAMILY_NAME),
                name(NameId::TYPOGRAPHIC_SUBFAMILY_NAME),
                name(NameId::new(256)),
                selection_flags
            ),
            (
                "An Light",
                "Italic",
                "1.000;NONE;An-LightItalic",
                "An Light Italic",
                "An-LightItalic",
                "An",
                "Light Italic",
                "Weight",
                SelectionFlags::ITALIC
            )
        );
    }

    #[test]
    fn prefer_vendorid_in_properties() {
        let (_, context) = build_static_metadata(glyphs3_dir().join("MultipleVendorIDs.glyphs"));
        let static_metadata = context.static_metadata.get();
        assert_eq!(Tag::new(b"RGHT"), static_metadata.misc.vendor_id);
    }

    #[test]
    fn prefer_default_master_panose() {
        let (_, context) = build_static_metadata(glyphs3_dir().join("MultiplePanose.glyphs"));
        let static_metadata = context.static_metadata.get();
        assert_eq!(
            Some(Panose::from([2u8, 3, 4, 5, 6, 7, 8, 9, 10, 11])),
            static_metadata.misc.panose
        );
    }

    fn fixed_pitch_of(glyphs_file: PathBuf) -> Option<bool> {
        let (_, context) = build_static_metadata(glyphs_file);
        let static_metadata = context.static_metadata.get();
        static_metadata.misc.is_fixed_pitch
    }

    #[test]
    fn fixed_pitch_on() {
        assert_eq!(
            Some(true),
            fixed_pitch_of(glyphs3_dir().join("FixedPitch.glyphs"))
        );
    }

    #[test]
    fn fixed_pitch_off() {
        assert_eq!(None, fixed_pitch_of(glyphs3_dir().join("WghtVar.glyphs")));
    }

    #[test]
    fn invalid_vendor_id_no_crashy() {
        let (_, context) = build_static_metadata(glyphs3_dir().join("InvalidVendorID.glyphs"));
        let static_metadata = context.static_metadata.get();
        assert_eq!(Tag::new(b"NONE"), static_metadata.misc.vendor_id);
    }

    #[test]
    fn which_panose_shall_i_get() {
        // Learned from GSC that panose can live in instances
        let (_, context) =
            build_static_metadata(glyphs3_dir().join("WghtVarInstancePanose.glyphs"));
        let static_metadata = context.static_metadata.get();
        assert_eq!(
            Some(Panose::from([4, 5, 6, 7, 8, 9, 10, 11, 12, 13])),
            static_metadata.misc.panose
        );
    }

    #[test]
    fn mark_width_zeroing() {
        let (source, context) = build_static_metadata(glyphs3_dir().join("SpacingMark.glyphs"));
        build_glyphs(&source, &context).unwrap();
        let glyph = context.get_glyph("descender-cy");
        // this is a spacing-combining mark, so we shouldn't zero it's width
        assert!(glyph.default_instance().width != 0.0);
    }
}
