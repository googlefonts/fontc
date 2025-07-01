use std::{
    collections::{BTreeMap, BTreeSet, HashMap, HashSet},
    path::Path,
    sync::Arc,
};

use chrono::DateTime;
use log::{debug, trace, warn};

use fontdrasil::{
    coords::{DesignCoord, NormalizedCoord, NormalizedLocation},
    orchestration::{Access, AccessBuilder, Work},
    types::{Axes, GlyphName},
};
use fontir::{
    error::{BadGlyph, BadGlyphKind, BadSource, Error},
    feature_variations::{overlay_feature_variations, NBox},
    ir::{
        self, AnchorBuilder, Color, ColorPalettes, Condition, ConditionSet, GdefCategories,
        GlobalMetric, GlobalMetrics, GlyphInstance, GlyphOrder, KernGroup, KernSide, KerningGroups,
        KerningInstance, MetaTableValues, NameBuilder, NameKey, NamedInstance, PostscriptNames,
        Rule, StaticMetadata, Substitution, VariableFeature, DEFAULT_VENDOR_ID,
    },
    orchestration::{Context, Flags, IrWork, WorkId},
    source::Source,
};
use glyphs_reader::{
    glyphdata::{Category, Subcategory},
    Font, InstanceType, Layer,
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
    font_info: Arc<FontInfo>,
    source_path: Option<Arc<Path>>,
}

impl GlyphsIrSource {
    fn create_work_for_one_glyph(
        &self,
        glyph_name: GlyphName,
        base_name: Option<GlyphName>,
    ) -> Result<GlyphIrWork, Error> {
        Ok(GlyphIrWork {
            glyph_name,
            font_info: self.font_info.clone(),
            bracket_glyph_parent: base_name,
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

        Ok(Self {
            font_info: Arc::new(font_info),
            source_path: Some(glyphs_file.into()),
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
        for glyph_name in self.font_info.font.glyph_order.iter() {
            work.push(Box::new(
                self.create_work_for_one_glyph(glyph_name.clone().into(), None)?,
            ));

            let glyph = self.font_info.font.glyphs.get(glyph_name.as_str()).unwrap();
            for bracket_name in bracket_glyph_names(glyph, &self.font_info.axes) {
                work.push(Box::new(self.create_work_for_one_glyph(
                    bracket_name.0,
                    Some(glyph_name.clone().into()),
                )?))
            }
        }
        Ok(work)
    }

    fn create_feature_ir_work(&self) -> Result<Box<IrWork>, Error> {
        Ok(Box::new(FeatureWork {
            font_info: self.font_info.clone(),
            font_file_path: self.source_path.clone(),
        }))
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

impl GlyphsIrSource {
    pub fn new_from_memory(data: &str) -> Result<Self, Error> {
        let font = Font::load_from_string(data).map_err(|e| {
            BadSource::custom(
                "<memory>".to_string(),
                format!("Unable to read glyphs file: {e}"),
            )
        })?;
        Ok(Self {
            font_info: Arc::new(FontInfo::try_from(font)?),
            source_path: None,
        })
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
        let named_instances = font
            .instances
            .iter()
            .filter_map(|inst| {
                if inst.type_ != InstanceType::Single || !inst.active {
                    return None;
                }
                Some(NamedInstance {
                    name: inst.name.clone(),
                    postscript_name: inst.postscript_name().map(str::to_string),
                    location: font_info
                        .locations
                        .get(&inst.axes_values)
                        .map(|nc| nc.to_user(&axes))
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

        let categories = make_glyph_categories(font, &axes);

        // Only build vertical metrics if at least one glyph defines a vertical
        // attribute.
        // https://github.com/googlefonts/glyphsLib/blob/c4db6b98/Lib/glyphsLib/builder/builders.py#L191-L199
        let build_vertical = font
            .glyphs
            .values()
            .flat_map(|glyph| glyph.layers.iter())
            .any(|layer| layer.vert_width.is_some() || layer.vert_origin.is_some());

        let dont_use_prod_names = font
            .custom_parameters
            .dont_use_production_names
            .unwrap_or(false);

        let postscript_names =
            if context.flags.contains(Flags::PRODUCTION_NAMES) && !dont_use_prod_names {
                let mut postscript_names = PostscriptNames::default();
                for glyph in font.glyphs.values() {
                    if let Some(production_name) = glyph.production_name.as_ref() {
                        postscript_names
                            .insert(glyph.name.clone().into(), production_name.clone().into());

                        for (bracket_name, _) in bracket_glyph_names(glyph, &axes) {
                            let bracket_suffix = bracket_name
                                .as_str()
                                .strip_prefix(glyph.name.as_str())
                                .expect("glyph name always a prefix of bracket glyph name");
                            let bracket_prod_name =
                                smol_str::format_smolstr!("{production_name}{bracket_suffix}");
                            postscript_names.insert(bracket_name, bracket_prod_name.into());
                        }
                    }
                }
                Some(postscript_names)
            } else {
                None
            };

        let variations = make_feature_variations(font_info);
        let mut static_metadata = StaticMetadata::new(
            font.units_per_em,
            names(font, selection_flags),
            axes.into_inner(),
            named_instances,
            global_locations,
            postscript_names,
            italic_angle,
            categories,
            number_values,
            build_vertical,
        )
        .map_err(Error::VariationModelError)?;
        static_metadata.misc.selection_flags = selection_flags;
        static_metadata.variations = variations;
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
                    warn!("Invalid creation date: {raw_date}: {e:?}");
                }
                parsed.ok()
            })
            .or(static_metadata.misc.created);

        if let Some(meta_table) = default_instance
            .and_then(|di| di.custom_parameters.meta_table.as_ref())
            .or(default_master.custom_parameters.meta_table.as_ref())
            .or(font.custom_parameters.meta_table.as_ref())
        {
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

        let mut glyph_order: GlyphOrder =
            font.glyph_order.iter().cloned().map(Into::into).collect();

        let mut bracket_glyphs = font
            .glyphs
            .values()
            .filter(|g| g.export)
            .flat_map(|g| {
                bracket_glyph_names(g, &static_metadata.axes).map(|(bracket_name, _)| bracket_name)
            })
            .collect::<Vec<_>>();
        bracket_glyphs.sort();
        glyph_order.extend(bracket_glyphs);

        context.static_metadata.set(static_metadata);
        context.preliminary_glyph_order.set(glyph_order);
        Ok(())
    }
}

fn make_feature_variations(fontinfo: &FontInfo) -> Option<VariableFeature> {
    // by default, glyphs registers feature variations under 'rlig'
    // https://glyphsapp.com/learn/switching-shapes#g-1-alternate-layers-bracket-layers__feature-variations
    // but glyphsLib uses rvrn, so we go with that?
    // https://github.com/googlefonts/glyphsLib/blob/c4db6b981d/Lib/glyphsLib/builder/bracket_layers.py#L63
    const DEFAULT_FEATURE: Tag = Tag::new(b"rvrn");
    let mut rules = Vec::new();
    for glyph_name in &fontinfo.font.glyph_order {
        let glyph = fontinfo.font.glyphs.get(glyph_name).unwrap();
        if !glyph.export {
            continue;
        }
        for (condset, (sub_name, _layers)) in bracket_glyphs(glyph, &fontinfo.axes) {
            let nbox = condset_to_nbox(condset, &fontinfo.axes);
            rules.push((
                vec![nbox].into(),
                BTreeMap::from([(glyph.name.clone().into(), sub_name)]),
            ));
        }
    }
    if rules.is_empty() {
        return None;
    }

    let overlayed = overlay_feature_variations(rules);

    let raw_feature = fontinfo
        .font
        .custom_parameters
        .feature_for_feature_variations
        .as_ref();
    let feature = raw_feature.and_then(|s| s.parse::<Tag>().ok());
    if feature.is_none() {
        log::warn!("invalid or missing param 'Feature for Feature Variations': {raw_feature:?}");
    }
    let features = vec![feature.unwrap_or(DEFAULT_FEATURE)];
    let rules = overlayed
        .into_iter()
        .map(|(condset, substitutions)| Rule {
            conditions: vec![nbox_to_condset(condset, &fontinfo.axes)],
            substitutions: substitutions
                .into_iter()
                .flatten()
                .map(|(replace, with)| Substitution { replace, with })
                .collect(),
        })
        .collect();

    Some(VariableFeature { features, rules })
}

fn nbox_to_condset(nbox: NBox, axes: &Axes) -> ConditionSet {
    nbox.iter()
        .map(|(tag, (min, max))| {
            let axis = axes.get(&tag).unwrap();

            Condition::new(
                tag,
                Some(min.to_design(&axis.converter)),
                Some(max.to_design(&axis.converter)),
            )
        })
        .collect()
}

fn condset_to_nbox(condset: ConditionSet, axes: &Axes) -> NBox {
    condset
        .iter()
        .filter_map(|cond| {
            let axis = axes.get(&cond.axis)?;
            // we can filter out conditions with no min/max; missing axes are
            // treated as being fully included in the box.
            if cond.min.is_none() && cond.max.is_none() {
                return None;
            }
            Some((
                cond.axis,
                (
                    cond.min
                        .map(|ds| ds.to_normalized(&axis.converter))
                        .unwrap_or_else(|| axis.min.to_normalized(&axis.converter)),
                    cond.max
                        .map(|ds| ds.to_normalized(&axis.converter))
                        .unwrap_or_else(|| axis.max.to_normalized(&axis.converter)),
                ),
            ))
        })
        .collect()
}

pub(crate) fn bracket_glyph_names<'a>(
    glyph: &'a glyphs_reader::Glyph,
    axes: &Axes,
) -> impl Iterator<Item = (GlyphName, Vec<&'a Layer>)> {
    bracket_glyphs(glyph, axes).map(|x| x.1)
}

fn bracket_glyphs<'a>(
    glyph: &'a glyphs_reader::Glyph,
    axes: &Axes,
) -> impl Iterator<Item = (ConditionSet, (GlyphName, Vec<&'a Layer>))> {
    let mut seen_sets = HashMap::new();
    for layer in &glyph.bracket_layers {
        let condition_set = get_bracket_info(layer, axes);
        let next_alt = seen_sets.len() + 1;
        seen_sets
            .entry(condition_set)
            .or_insert_with(|| {
                (
                    //https://github.com/googlefonts/glyphsLib/blob/c4db6b981d577f/Lib/glyphsLib/builder/bracket_layers.py#L127
                    smol_str::format_smolstr!("{}.BRACKET.varAlt{next_alt:02}", glyph.name,).into(),
                    Vec::new(),
                )
            })
            .1
            .push(layer);
    }
    seen_sets.into_iter()
}

// https://github.com/googlefonts/glyphsLib/blob/c4db6b981d/Lib/glyphsLib/classes.py#L3947
fn get_bracket_info(layer: &Layer, axes: &Axes) -> ConditionSet {
    assert!(
        !layer.attributes.axis_rules.is_empty(),
        "all bracket layers have axis rules"
    );

    axes.iter()
        .zip(&layer.attributes.axis_rules)
        .map(|(axis, rule)| {
            let min = rule.min.map(|v| DesignCoord::new(v as f64));
            let max = rule.max.map(|v| DesignCoord::new(v as f64));
            Condition::new(axis.tag, min, max)
        })
        .collect()
}

fn make_glyph_categories(font: &Font, axes: &Axes) -> GdefCategories {
    let categories = font
        .glyphs
        .values()
        .flat_map(|glyph| categories_for_glyph_and_any_bracket_glyphs(glyph, axes))
        .collect();
    GdefCategories {
        categories,
        prefer_gdef_categories_in_fea: false,
    }
}

fn categories_for_glyph_and_any_bracket_glyphs<'a>(
    glyph: &'a glyphs_reader::Glyph,
    axes: &'a Axes,
) -> impl Iterator<Item = (GlyphName, GlyphClassDef)> + use<'a> {
    let main = category_for_glyph(&glyph.layers, glyph.category, glyph.sub_category)
        .map(|cat| (glyph.name.clone().into(), cat));
    main.into_iter().chain(
        bracket_glyph_names(glyph, axes).filter_map(|(name, layers)| {
            category_for_glyph(layers, glyph.category, glyph.sub_category).map(|cat| (name, cat))
        }),
    )
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
fn category_for_glyph<'a>(
    layers: impl IntoIterator<Item = &'a Layer>,
    category: Option<Category>,
    sub_category: Option<Subcategory>,
) -> Option<GlyphClassDef> {
    let has_attaching_anchor = layers
        .into_iter()
        .flat_map(|layer| layer.anchors.iter())
        // glyphsLib considers any anchor that does not start with '_' as an
        // 'attaching anchor'; see https://github.com/googlefonts/glyphsLib/issues/1024
        .any(|anchor| !anchor.name.starts_with('_'));
    match (category, sub_category) {
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
            set_metric!(VheaCaretSlopeRise, vhea_caret_slope_rise);
            set_metric!(VheaCaretSlopeRun, vhea_caret_slope_run);
            set_metric!(VheaCaretOffset, vhea_caret_offset);

            // https://github.com/googlefonts/glyphsLib/blob/c4db6b981d577f456d64ebe9993818770e170454/Lib/glyphsLib/builder/masters.py#L74-L92
            metrics.set(
                GlobalMetric::VheaAscender,
                pos.clone(),
                master
                    .custom_parameters
                    .vhea_ascender
                    .or(font.custom_parameters.vhea_ascender)
                    .map(|v| v as f64)
                    .unwrap_or(font.units_per_em as f64 / 2.0),
            );
            metrics.set(
                GlobalMetric::VheaDescender,
                pos.clone(),
                master
                    .custom_parameters
                    .vhea_descender
                    .or(font.custom_parameters.vhea_descender)
                    .map(|v| v as f64)
                    .unwrap_or(-(font.units_per_em as f64 / 2.0)),
            );
            metrics.set(
                GlobalMetric::VheaLineGap,
                pos.clone(),
                master
                    .custom_parameters
                    .vhea_line_gap
                    .or(font.custom_parameters.vhea_line_gap)
                    .map(|v| v as f64)
                    .unwrap_or(font.units_per_em as f64),
            );

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
struct FeatureWork {
    font_info: Arc<FontInfo>,
    font_file_path: Option<Arc<Path>>,
}

impl Work<Context, WorkId, Error> for FeatureWork {
    fn id(&self) -> WorkId {
        WorkId::Features
    }

    fn exec(&self, context: &Context) -> Result<(), Error> {
        trace!("Generate features");
        let font_info = self.font_info.as_ref();
        let font = &font_info.font;

        context.features.set(to_ir_features(
            &font.features,
            self.font_file_path.as_ref().map(|path| {
                path.canonicalize()
                    .expect("path cannot be canonicalized")
                    .parent()
                    .expect("the path must be in a directory")
                    .to_path_buf()
            }),
        )?);
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

        // build up the kern groups; a glyph may belong to a group on either or
        // both 'side'.
        for (name, glyph) in font
            .glyphs
            .iter()
            // ignore non-export glyphs
            .filter(|x| x.1.export)
        {
            // if there are bracket layers for this glyph, make sure the
            // generated glyphs are assigned the same groups as the parent
            let bracket_names = bracket_glyph_names(glyph, &font_info.axes)
                .map(|(name, _)| name)
                .collect::<Vec<_>>();
            let right = glyph.right_kern.clone().map(KernGroup::Side1);
            let left = glyph.left_kern.clone().map(KernGroup::Side2);
            for group in [right, left].into_iter().flatten() {
                groups.groups.entry(group).or_default().extend(
                    bracket_names
                        .iter()
                        .cloned()
                        .chain(Some(name.clone().into())),
                );
            }
        }

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

        let bracket_glyph_map = make_bracket_glyph_map(glyph_order);

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
            .flat_map(|(participants, (_, value))| {
                expand_kerning_to_brackets(&bracket_glyph_map, participants, value)
            })
            .for_each(|(participants, value)| {
                *kerning.kerns.entry(participants).or_default() = value;
            });

        context.kerning_at.set(kerning);
        Ok(())
    }
}

// map from base glyph to bracket glyphs
fn make_bracket_glyph_map(glyphs: &GlyphOrder) -> HashMap<&str, Vec<&GlyphName>> {
    let mut result = HashMap::new();
    for name in glyphs.names() {
        if let Some((base, _)) = name.as_str().split_once(".BRACKET") {
            result.entry(base).or_insert(Vec::new()).push(name);
        }
    }
    result
}

fn expand_kerning_to_brackets(
    bracket_glyph_map: &HashMap<&str, Vec<&GlyphName>>,
    participants: (KernSide, KernSide),
    value: OrderedFloat<f64>,
) -> impl Iterator<Item = ((KernSide, KernSide), OrderedFloat<f64>)> {
    let first_match = participants
        .0
        .glyph_name()
        .and_then(|name| bracket_glyph_map.get(name.as_str()));

    let second_match = participants
        .1
        .glyph_name()
        .and_then(|name| bracket_glyph_map.get(name.as_str()));

    let bracket_kerns: Vec<_> = match (first_match, second_match) {
        (Some(left), None) => left
            .iter()
            .copied()
            .map(|gn| (KernSide::Glyph(gn.clone()), participants.1.clone()))
            .collect(),
        (None, Some(right)) => right
            .iter()
            .copied()
            .map(|gn| (participants.0.clone(), gn.clone().into()))
            .collect(),
        (Some(left), Some(right)) => left
            .iter()
            .copied()
            .chain(participants.0.glyph_name())
            .flat_map(|left| {
                right
                    .iter()
                    .copied()
                    .chain(participants.1.glyph_name())
                    .map(|right| (left.clone().into(), right.clone().into()))
            })
            .collect(),
        (None, None) => Vec::new(),
    };

    bracket_kerns
        .into_iter()
        .chain(Some(participants))
        .map(move |participants| (participants, value))
}

#[derive(Debug)]
struct GlyphIrWork {
    glyph_name: GlyphName,
    // If present, we are building a bracket glyph.
    bracket_glyph_parent: Option<GlyphName>,
    font_info: Arc<FontInfo>,
}

impl GlyphIrWork {
    fn is_bracket_layer(&self) -> bool {
        self.bracket_glyph_parent.is_some()
    }
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
        AccessBuilder::new()
            .variant(WorkId::StaticMetadata)
            .variant(WorkId::GlobalMetrics)
            .build()
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
        let global_metrics = context.global_metrics.get();

        let glyph = font
            .glyphs
            .get(
                self.bracket_glyph_parent
                    .as_ref()
                    .unwrap_or(&self.glyph_name)
                    .as_str(),
            )
            .ok_or_else(|| Error::NoGlyphForName(self.glyph_name.clone()))?;

        let mut ir_glyph = ir::GlyphBuilder::new(self.glyph_name.clone());
        ir_glyph.emit_to_binary = glyph.export;
        // only non-bracket glyphs get codepoints
        ir_glyph.codepoints = if !self.is_bracket_layer() {
            glyph.unicode.iter().copied().collect()
        } else {
            Default::default()
        };

        let mut ir_anchors = AnchorBuilder::new(self.glyph_name.clone());
        let layers = if self.is_bracket_layer() {
            bracket_glyph_names(glyph, axes)
                .find(|(name, _layers)| *name == self.glyph_name)
                .ok_or_else(|| Error::NoGlyphForName(self.glyph_name.clone()))?
                .1
        } else {
            glyph
                .layers
                .iter()
                .filter(|l| l.is_master() || l.is_intermediate())
                .collect()
        };

        // Glyphs have layers that match up with masters, and masters have locations
        let mut axis_positions: HashMap<Tag, HashSet<NormalizedCoord>> = HashMap::new();
        let mut seen_master_ids = HashSet::new();
        for layer in layers.iter() {
            seen_master_ids.insert(layer.master_id());

            let (location, instance) = process_layer(glyph, layer, font_info, &global_metrics)?;

            for (tag, coord) in location.iter() {
                axis_positions.entry(*tag).or_default().insert(*coord);
            }
            ir_glyph.try_add_source(&location, instance)?;

            // we only care about anchors from exportable glyphs
            // https://github.com/googlefonts/fontc/issues/1397
            if glyph.export {
                for anchor in layer.anchors.iter() {
                    ir_anchors.add(anchor.name.clone(), location.clone(), anchor.pos)?;
                }
            }
        }

        // special logic, if this was a bracket layer:
        if self.is_bracket_layer() {
            // If any master locations don't have a bracket layer, reuse the
            // base layer for that location. See "Switching Only One Master" at
            // https://glyphsapp.com/tutorials/alternating-glyph-shapes.
            //
            // - see also https://github.com/googlefonts/glyphsLib/blob/c4db6b981d5/Lib/glyphsLib/builder/bracket_layers.py#L78

            for missing_master_id in font
                .masters
                .iter()
                .map(|m| m.id.as_str())
                .filter(|id| !seen_master_ids.contains(id))
            {
                // we use ids instead of locations because in glyphs2 you can
                // have the funny default axes in the instances
                if let Some(layer) = glyph
                    .layers
                    .iter()
                    .find(|l| l.master_id() == missing_master_id)
                {
                    let (loc, instance) = process_layer(glyph, layer, font_info, &global_metrics)?;
                    ir_glyph.try_add_source(&loc, instance)?;
                    for (tag, coord) in loc.iter() {
                        axis_positions.entry(*tag).or_default().insert(*coord);
                    }
                    layer
                        .anchors
                        .iter()
                        .try_for_each(|a| ir_anchors.add(a.name.clone(), loc.clone(), a.pos))?;
                }
            }
        }
        let mut glyph = ir_glyph.build()?;
        update_bracket_glyph_components(&mut glyph, font, axes);

        let anchors = ir_anchors.build()?;

        // It's helpful if glyphs are defined at default
        for axis in axes.iter() {
            let default = axis.default.to_normalized(&axis.converter);
            let positions = axis_positions.get(&axis.tag).ok_or_else(|| {
                BadGlyph::new(&self.glyph_name, BadGlyphKind::NoAxisPosition(axis.tag))
            })?;
            check_pos(&self.glyph_name, positions, axis, &default)?;
        }

        //TODO: expand kerning to brackets

        context.anchors.set(anchors);
        context.glyphs.set(glyph);
        Ok(())
    }
}

fn process_layer(
    glyph: &glyphs_reader::Glyph,
    instance: &Layer,
    font_info: &FontInfo,
    global_metrics: &GlobalMetrics,
) -> Result<(NormalizedLocation, GlyphInstance), Error> {
    // skip not-yet-supported types of layers (e.g. alternate, color, etc.)
    let master_id = instance.master_id();
    let master_idx = font_info.master_indices.get(master_id).ok_or_else(|| {
        BadGlyph::new(
            glyph.name.clone(),
            BadGlyphKind::MissingMaster(master_id.to_owned()),
        )
    })?;
    let master = &font_info.font.masters[*master_idx];
    let master_location = font_info.locations.get(&master.axes_values).unwrap();

    let mut location = master_location.clone();
    // intermediate (aka 'brace') layers can override axis values from their
    // associated master
    if !instance.attributes.coordinates.is_empty() {
        for (tag, coord) in design_location(&font_info.axes, &instance.attributes.coordinates)
            .to_normalized(&font_info.axes)
            .iter()
        {
            location.insert(*tag, *coord);
        }
    }

    // See https://github.com/googlefonts/glyphsLib/blob/c4db6b98/Lib/glyphsLib/builder/glyph.py#L359-L389
    let local_metrics = global_metrics.at(master_location);
    let height = instance
        .vert_width
        .unwrap_or_else(|| local_metrics.os2_typo_ascender - local_metrics.os2_typo_descender)
        .into_inner();
    let vertical_origin = instance
        .vert_origin
        .map(|origin| local_metrics.os2_typo_ascender - origin)
        .unwrap_or(local_metrics.os2_typo_ascender)
        .into_inner();

    // TODO populate width and height properly
    let (contours, components) =
        to_ir_contours_and_components(glyph.name.clone().into(), &instance.shapes)?;
    let glyph_instance = GlyphInstance {
        // https://github.com/googlefonts/fontmake-rs/issues/285 glyphs non-spacing marks are 0-width
        width: if glyph.is_nonspacing_mark() {
            0.0
        } else {
            instance.width.into_inner()
        },
        height: Some(height),
        vertical_origin: Some(vertical_origin),
        contours,
        components,
    };
    Ok((location, glyph_instance))
}

/// If a bracket glyph has components and they also have bracket layers,
/// we need to update the components to point to them.
fn update_bracket_glyph_components(glyph: &mut ir::Glyph, font: &Font, axes: &Axes) {
    if !glyph.name.as_str().contains("BRACKET") {
        return;
    }
    let instance = glyph.default_instance();
    let comp_map = instance
        .components
        .iter()
        .flat_map(|comp| {
            let raw_glyph = font.glyphs.get(comp.base.as_str())?;
            for (component_bracket_name, _) in bracket_glyph_names(raw_glyph, axes) {
                let suffix = component_bracket_name.as_str().rsplit_once('.').unwrap().1;
                if glyph.name.as_str().ends_with(suffix) {
                    return Some((comp.base.clone(), component_bracket_name));
                }
            }
            None
        })
        .collect::<HashMap<_, _>>();

    glyph
        .sources_mut()
        .flat_map(|(_, src)| src.components.iter_mut())
        .for_each(|comp| {
            if let Some(new_name) = comp_map.get(&comp.base) {
                comp.base = new_name.clone();
            }
        });
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
            Coord, CoordConverter, DesignCoord, NormalizedCoord, NormalizedLocation, UserCoord,
            UserLocation,
        },
        orchestration::{Access, AccessBuilder},
        types::{Axis, GlyphName},
    };
    use fontir::{
        error::Error,
        ir::{AnchorKind, GlobalMetricsInstance, Glyph, GlyphOrder, NameKey},
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
                            (UserCoord::new(450.0), DesignCoord::new(57.0)), // duplicate value
                            (UserCoord::new(500.0), DesignCoord::new(62.0)), // default
                            (UserCoord::new(600.0), DesignCoord::new(68.0)),
                            (UserCoord::new(700.0), DesignCoord::new(73.0)),
                        ],
                        5
                    ),
                    localized_names: Default::default(),
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
                    localized_names: Default::default(),
                },
            ],
            static_metadata.all_source_axes.clone().into_inner()
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
            let task_context = context.copy_for_work(work.read_access(), work.write_access());
            work.exec(&task_context)?;
        }
        Ok(())
    }

    #[test]
    fn glyph_user_locations() {
        let glyph_name: GlyphName = "space".into();
        let (source, context) =
            build_global_metrics(glyphs2_dir().join("OpszWghtVar_AxisMappings.glyphs"));
        build_glyphs(&source, &context).unwrap(); // we dont' care about geometry

        let static_metadata = context.static_metadata.get();

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
            .map(|c| c.to_user(&static_metadata.axes))
            .collect::<HashSet<_>>();

        assert_eq!(expected_locations, actual_locations);
    }

    #[test]
    fn glyph_normalized_locations() {
        let glyph_name: GlyphName = "space".into();
        let (source, context) =
            build_global_metrics(glyphs2_dir().join("OpszWghtVar_AxisMappings.glyphs"));
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
        let wght = wght.iter().next().unwrap();

        for (design, user) in &[
            (0.0, 400.0),
            (4.0, 450.0),
            (8.0, 500.0),
            (8.5, 600.0),
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
        let (source, context) = build_global_metrics(glyphs2_dir().join("WghtVar.glyphs"));
        build_glyphs(&source, &context).unwrap();
        let glyph = context.glyphs.get(&WorkId::Glyph("hyphen".into()));
        assert_eq!(HashSet::from([0x002d]), glyph.codepoints);
    }

    #[test]
    fn captures_single_codepoints_unquoted_dec() {
        let (source, context) =
            build_global_metrics(glyphs3_dir().join("Unicode-UnquotedDec.glyphs"));
        build_glyphs(&source, &context).unwrap();
        let glyph = context.glyphs.get(&WorkId::Glyph("name".into()));
        assert_eq!(HashSet::from([182]), glyph.codepoints);
    }

    #[test]
    fn captures_multiple_codepoints_unquoted_dec() {
        let (source, context) =
            build_global_metrics(glyphs3_dir().join("Unicode-UnquotedDecSequence.glyphs"));
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
        assert_eq!(
            category_for_glyph(&glyph.layers, glyph.category, glyph.sub_category),
            Some(GlyphClassDef::Base)
        );
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
                vhea_ascender: 500.0.into(),
                vhea_descender: (-500.0).into(),
                vhea_line_gap: 1000.0.into(),
                vhea_caret_slope_run: 1.0.into(),
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
            let (source, context) = build_global_metrics(test_dir.join("QCurve.glyphs"));
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

    //https://github.com/googlefonts/glyphsLib/blob/c4db6b981d/tests/builder/builder_test.py#L2360
    #[test]
    fn expand_kerning_to_brackets() {
        // helpers:
        fn parse_side(raw: &str) -> KernSide {
            if let Some(name) = raw.strip_prefix("@side1.") {
                KernSide::Group(KernGroup::Side1(name.into()))
            } else if let Some(name) = raw.strip_prefix("@side2.") {
                KernSide::Group(KernGroup::Side2(name.into()))
            } else {
                KernSide::Glyph(raw.into())
            }
        }
        fn make_kerning(
            items: &[(&str, &str, i32)],
        ) -> BTreeMap<(KernSide, KernSide), OrderedFloat<f64>> {
            items
                .iter()
                .map(|(side1, side2, val)| {
                    ((parse_side(side1), parse_side(side2)), (*val as f64).into())
                })
                .collect()
        }

        // actual test:
        let (_, context) = build_kerning(glyphs2_dir().join("BracketTestFontKerning.glyphs"));
        let groups = context.kerning_groups.get();
        assert_eq!(
            groups
                .groups
                .iter()
                .map(|(name, glyphs)| (
                    name.to_string(),
                    glyphs.iter().map(GlyphName::as_str).collect::<Vec<_>>()
                ))
                .collect::<Vec<_>>(),
            vec![
                (
                    "side1.foo".to_string(),
                    vec!["x", "x.BRACKET.varAlt01", "x.BRACKET.varAlt02"]
                ),
                ("side2.foo".to_string(), vec!["a", "a.BRACKET.varAlt01"]),
            ]
        );
        let light_location = NormalizedLocation::for_pos(&[("wdth", 0.0), ("wght", 0.0)]);
        let all_kerns = context.kerning_at.all();
        let light_kerns = &all_kerns
            .iter()
            .find(|thingie| thingie.1.location == light_location)
            .unwrap()
            .1;
        assert_eq!(
            light_kerns.kerns,
            make_kerning(&[
                ("@side1.foo", "@side2.foo", -200),
                ("a", "x", -100),
                ("a.BRACKET.varAlt01", "x", -100),
                ("a", "x.BRACKET.varAlt01", -100),
                ("a.BRACKET.varAlt01", "x.BRACKET.varAlt01", -100),
                ("a", "x.BRACKET.varAlt02", -100),
                ("a.BRACKET.varAlt01", "x.BRACKET.varAlt02", -100),
            ])
        );
    }

    #[test]
    fn captures_anchors() {
        let base_name = "A".into();
        let mark_name = "macroncomb".into();
        let (source, context) = build_global_metrics(glyphs3_dir().join("WghtVar_Anchors.glyphs"));
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
        let (source, context) = build_global_metrics(glyphs3_dir().join("WghtVar_NoExport.glyphs"));
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
                    vhea_ascender: 500.0.into(),
                    vhea_descender: (-500.0).into(),
                    vhea_line_gap: 1000.0.into(),
                    vhea_caret_slope_run: 1.0.into(),
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
                vhea_ascender: 500.0.into(),
                vhea_descender: (-500.0).into(),
                vhea_line_gap: 1000.0.into(),
                vhea_caret_slope_run: 1.0.into(),
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
        let (source, context) = build_global_metrics(glyphs3_dir().join("SpacingMark.glyphs"));
        build_glyphs(&source, &context).unwrap();
        let glyph = context.get_glyph("descender-cy");
        // this is a spacing-combining mark, so we shouldn't zero it's width
        assert!(glyph.default_instance().width != 0.0);
    }

    fn make_glyph_order<'a>(raw: impl IntoIterator<Item = &'a str>) -> GlyphOrder {
        raw.into_iter().map(GlyphName::from).collect()
    }

    #[test]
    fn bracket_glyph_names_v2() {
        let (source, context) =
            build_global_metrics(glyphs2_dir().join("WorkSans-minimal-bracketlayer.glyphs"));
        build_glyphs(&source, &context).unwrap();
        let prelim_order = context.preliminary_glyph_order.get();
        assert_eq!(
            prelim_order.as_ref(),
            &make_glyph_order(["colonsign", "colonsign.BRACKET.varAlt01"])
        );
    }

    #[test]
    fn bracket_glyph_names_v3() {
        let (source, context) =
            build_global_metrics(glyphs3_dir().join("LibreFranklin-bracketlayer.glyphs"));
        build_glyphs(&source, &context).unwrap();
        let prelim_order = context.preliminary_glyph_order.get();
        assert_eq!(
            prelim_order.as_ref(),
            &make_glyph_order([
                "peso",
                "yen",
                "peso.BRACKET.varAlt01",
                "yen.BRACKET.varAlt01"
            ])
        );
    }

    // brackets in the glyph order should be sorted lexicographically
    #[test]
    fn bracket_layer_sort_order() {
        let (source, context) =
            build_global_metrics(glyphs3_dir().join("bracket-glyph-order.glyphs"));
        build_glyphs(&source, &context).unwrap();
        let prelim_order = context.preliminary_glyph_order.get();

        assert_eq!(
            prelim_order.as_ref(),
            &make_glyph_order([
                "peso",
                "peso.001",
                "peso.001.BRACKET.varAlt01",
                "peso.BRACKET.varAlt01",
            ])
        );
    }

    fn get_components(glyph: &Glyph) -> Vec<&str> {
        glyph
            .default_instance()
            .components
            .iter()
            .map(|c| c.base.as_str())
            .collect()
    }

    #[test]
    fn bracket_glyph_components() {
        let (source, context) =
            build_global_metrics(glyphs3_dir().join("LibreFranklin-bracketlayer.glyphs"));
        build_glyphs(&source, &context).unwrap();

        let yen = context.get_glyph("yen");
        assert_eq!(get_components(&yen), ["peso"]);

        let yen_bracket = context.get_glyph("yen.BRACKET.varAlt01");
        assert_eq!(get_components(&yen_bracket), ["peso.BRACKET.varAlt01"]);
    }

    // if a glyph does not have bracket layers but a component does, we make
    // fake bracket layers on the glyph.
    #[test]
    fn non_bracket_glyph_with_bracket_component() {
        let (source, context) =
            build_global_metrics(glyphs3_dir().join("glyph-with-bracket-component.glyphs"));
        build_glyphs(&source, &context).unwrap();
        let yen = context.get_glyph("yen");
        assert_eq!(get_components(&yen), ["peso"]);

        let yen_bracket = context.get_glyph("yen.BRACKET.varAlt01");
        assert_eq!(get_components(&yen_bracket), ["peso.BRACKET.varAlt01"]);
    }

    #[test]
    fn bracket_glyph_anchors() {
        let (source, context) =
            build_global_metrics(glyphs3_dir().join("LibreFranklin-bracketlayer.glyphs"));
        build_glyphs(&source, &context).unwrap();

        let peso_anchors = context.get_anchor("peso");

        // non-bracket layer anchor x-positions are all even numbers
        assert!(peso_anchors
            .anchors
            .iter()
            .all(|a| a.default_pos().x as u32 % 2 == 0));

        // bracket anchor x positions are odd numbers
        let peso_bracket_anchors = context.get_anchor("peso.BRACKET.varAlt01");
        assert!(peso_bracket_anchors
            .anchors
            .iter()
            .all(|a| a.default_pos().x as u32 % 2 == 1));
    }

    #[test]
    fn bracket_glyphs_to_dspace_rules() {
        // this is taken from a real world font that didn't compile correctly
        // unless we did overlay_feature_variations as part of generating the dspace rules.
        let (_, context) =
            build_static_metadata(glyphs2_dir().join("Alexandria-bracketglyphs.glyphs"));
        let feat_vars = context.static_metadata.get().variations.clone().unwrap();

        assert_eq!(
            feat_vars.rules,
            vec![
                Rule::for_test(
                    &[&[("wght", (130., 215.))]],
                    &[
                        ("Udieresis.alt", "Udieresis.alt.BRACKET.varAlt01"),
                        ("Udieresis.ss01.alt", "Udieresis.ss01.alt.BRACKET.varAlt01"),
                        ("naira", "naira.BRACKET.varAlt01"),
                        ("peso", "peso.BRACKET.varAlt01"),
                        ("won", "won.BRACKET.varAlt01")
                    ]
                ),
                Rule::for_test(
                    &[&[("wght", (125., 130.))]],
                    &[
                        ("Udieresis.alt", "Udieresis.alt.BRACKET.varAlt01"),
                        ("Udieresis.ss01.alt", "Udieresis.ss01.alt.BRACKET.varAlt01"),
                        ("naira", "naira.BRACKET.varAlt01"),
                        ("peso", "peso.BRACKET.varAlt01"),
                    ]
                ),
                Rule::for_test(
                    &[&[("wght", (120., 130.))]],
                    &[
                        ("naira", "naira.BRACKET.varAlt01"),
                        ("peso", "peso.BRACKET.varAlt01"),
                    ]
                ),
            ]
        )
    }

    #[test]
    fn bracket_rules_in_glyph_order() {
        // when converting bracket glyphs into designspace rules, we want to
        // process glyphs in the same order as the glyph order, or we get
        // results that are different from fonttools.
        //
        // in this font, 'hbar' appears before 'c_t' in the glyph order, and
        // should be handled first.

        let (_, context) = build_static_metadata(
            glyphs3_dir().join("bracket-rules-processed-in-glyph-order.glyphs"),
        );
        let feat_vars = context.static_metadata.get().variations.clone().unwrap();

        assert_eq!(
            feat_vars.rules,
            vec![
                Rule::for_test(
                    &[&[("wght", (165., 193.))]],
                    &[
                        ("hbar", "hbar.BRACKET.varAlt01"),
                        ("c_t", "c_t.BRACKET.varAlt01"),
                    ]
                ),
                Rule::for_test(
                    &[&[("wght", (155., 165.))]],
                    &[("hbar", "hbar.BRACKET.varAlt01"),]
                ),
            ]
        )
    }

    const WGHT: Tag = Tag::new(b"wght");
    // when min==default then the normalized min value is 0, not -1.0
    #[test]
    fn condset_when_axis_min_is_also_default() {
        let min = Coord::new(400.0);
        let default = Coord::new(400.0);
        let max = Coord::new(800.0);

        let axis = Axis {
            name: "weight".into(),
            tag: WGHT,
            min,
            default,
            max,
            hidden: false,
            converter: CoordConverter::unmapped(min, default, max),
            localized_names: Default::default(),
        };
        let axes = Axes::new(vec![axis]);
        let cond = Condition::new(WGHT, None, Some(Coord::new(500.)));

        let condset = [cond].into_iter().collect();
        let box_ = condset_to_nbox(condset, &axes);
        let readable = box_
            .iter()
            .map(|x| (x.0, x.1 .0.to_f64(), x.1 .1.to_f64()))
            .collect::<Vec<_>>();

        assert_eq!(readable, [(WGHT, 0.0, 0.25)])
    }

    #[test]
    fn condset_skip_empty_axis() {
        let axes = Axes::for_test(&["wght"]);
        let wdth = Condition::new(Tag::new(b"wdth"), None, None);
        let wght = Condition::new(WGHT, Some(Coord::new(400.)), None);
        let condset = [wdth, wght].into_iter().collect();
        let box_ = condset_to_nbox(condset, &axes);
        let readable = box_
            .iter()
            .map(|x| (x.0, x.1 .0.to_f64(), x.1 .1.to_f64()))
            .collect::<Vec<_>>();

        assert_eq!(readable, [(WGHT, 0.0, 1.0)])
    }

    #[test]
    fn glyphs2_delete_default_point_axes() {
        // in old glyphs sources, if the Axes custom parameter is not used, we
        // assume that there are three variable axes, 'wght', 'wdth', and 'XXXX'.
        //
        // If any of these axes are not actually used, we want to make sure that
        // they are not passed through to fontir.
        let (_, context) =
            build_static_metadata(glyphs2_dir().join("WghtVar_Instances_implied_axes.glyphs"));
        let static_metadata = context.static_metadata.get();
        assert_eq!(
            static_metadata.named_instances[0]
                .location
                .axis_tags()
                .copied()
                .collect::<Vec<_>>(),
            [Tag::new(b"wght")]
        );
    }
}
