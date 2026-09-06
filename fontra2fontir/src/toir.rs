//! Functions to convert fontra things to fontc IR things

use std::{
    collections::{HashMap, HashSet},
    str::FromStr,
};

use fontdrasil::{
    coords::{CoordConverter, DesignCoord, NormalizedCoord, NormalizedLocation, UserCoord},
    types::{Axis, GlyphName},
};
use fontir::{
    error::{BadGlyph, BadGlyphKind, Error, PathConversionError},
    ir::{
        DEFAULT_VENDOR_ID, GlobalMetric, GlobalMetrics, GlobalMetricsBuilder, Glyph, GlyphInstance,
        GlyphPathBuilder, NameBuilder, NameKey, Panose, PreliminaryGdefCategories, StaticMetadata,
    },
};
use kurbo::BezPath;
use log::{trace, warn};
use write_fonts::{
    tables::{gdef::GlyphClassDef, os2::SelectionFlags},
    types::{NameId, Tag},
};

use crate::fontra::{
    AxisName, Contour, Font, FontSource, GlyphInfos, Point, PointType, VariableGlyph,
};

/// Normalize a design value against a font axis, clamped into the axis range
/// like Fontra's
/// [`normalizeValue`](https://github.com/fontra/fontra/blob/2a19b8bd1/src-js/fontra-core/src/var-model.js#L307-L323).
fn normalize_axis_value(value: f64, axis: &Axis) -> NormalizedCoord {
    let min = axis.min.to_design(&axis.converter).to_f64();
    let max = axis.max.to_design(&axis.converter).to_f64();
    DesignCoord::new(value.clamp(min, max)).to_normalized(&axis.converter)
}

fn default_source<'a>(font_data: &'a Font, axes: &[Axis]) -> Result<&'a FontSource, Error> {
    font_data
        .sources
        .values()
        .find(|source| {
            axes.iter().all(|axis| {
                let at_default = axis.default.to_normalized(&axis.converter);
                let coord = source
                    .location
                    .get(&axis.name)
                    .map(|v| normalize_axis_value(*v, axis))
                    .unwrap_or(at_default);
                coord == at_default
            })
        })
        .ok_or(Error::NoDefaultMaster)
}

fn to_ir_names(font_data: &Font, default_source: &FontSource) -> HashMap<NameKey, String> {
    let font_info = &font_data.font_info;
    let mut builder = NameBuilder::default();
    builder.set_version(
        font_info.version_major.unwrap_or(0),
        font_info.version_minor.unwrap_or(0).max(0) as u32,
    );
    builder.add_if_present(NameId::COPYRIGHT_NOTICE, &font_info.copyright);
    builder.add_if_present(NameId::TRADEMARK, &font_info.trademark);
    builder.add_if_present(NameId::DESCRIPTION, &font_info.description);
    builder.add_if_present(NameId::SAMPLE_TEXT, &font_info.sample_text);
    builder.add_if_present(NameId::DESIGNER, &font_info.designer);
    builder.add_if_present(NameId::DESIGNER_URL, &font_info.designer_url);
    builder.add_if_present(NameId::MANUFACTURER, &font_info.manufacturer);
    builder.add_if_present(NameId::VENDOR_URL, &font_info.manufacturer_url);
    builder.add_if_present(NameId::LICENSE_DESCRIPTION, &font_info.license_description);
    builder.add_if_present(NameId::LICENSE_URL, &font_info.license_info_url);
    let custom_data = |key: &str| {
        font_info
            .custom_data
            .get(key)
            .and_then(|v| v.as_str())
            .map(str::to_string)
    };
    builder.add_if_present(NameId::UNIQUE_ID, &custom_data("openTypeNameUniqueID"));
    builder.add_if_present(NameId::VERSION_STRING, &custom_data("openTypeNameVersion"));
    builder.add_if_present(
        NameId::TYPOGRAPHIC_FAMILY_NAME,
        &custom_data("openTypeNamePreferredFamilyName").or_else(|| font_info.family_name.clone()),
    );
    builder.add_if_present(
        NameId::WWS_FAMILY_NAME,
        &custom_data("openTypeNameWWSFamilyName"),
    );
    let source_custom_data = |key: &str| {
        default_source
            .custom_data
            .get(key)
            .and_then(|v| v.as_str())
            .map(str::to_string)
    };
    builder.add_if_present(
        NameId::TYPOGRAPHIC_SUBFAMILY_NAME,
        &source_custom_data("openTypeNamePreferredSubfamilyName")
            .or_else(|| Some(default_source.name.clone())),
    );
    builder.add_if_present(
        NameId::COMPATIBLE_FULL_NAME,
        &source_custom_data("openTypeNameCompatibleFullName"),
    );
    builder.add_if_present(
        NameId::WWS_SUBFAMILY_NAME,
        &source_custom_data("openTypeNameWWSSubfamilyName"),
    );
    builder.build(font_info.vendor_id.as_deref().unwrap_or(DEFAULT_VENDOR_ID))
}

fn apply_custom_data(
    static_metadata: &mut StaticMetadata,
    font_data: &Font,
    default_source: &FontSource,
) -> Result<(), Error> {
    let font_info = &font_data.font_info;
    let custom_data = &font_info.custom_data;
    let misc = &mut static_metadata.misc;
    misc.version_major = font_info.version_major.unwrap_or(0);
    misc.version_minor = font_info.version_minor.unwrap_or(0).max(0) as u32;
    misc.is_fixed_pitch = default_source
        .custom_data
        .get("postscriptIsFixedPitch")
        .and_then(|v| v.as_bool());
    if let Some(vendor_id) = font_info
        .vendor_id
        .as_deref()
        .filter(|id| !id.trim().is_empty())
    {
        misc.vendor_id = Tag::from_str(vendor_id).map_err(|cause| Error::InvalidTag {
            raw_tag: vendor_id.to_owned(),
            cause,
        })?;
    }
    let as_u16 = |key: &str| {
        custom_data
            .get(key)
            .and_then(|v| v.as_u64())
            .map(|v| v as u16)
    };
    let as_numbers = |key: &str| {
        custom_data
            .get(key)
            .and_then(|v| v.as_array())
            .map(|values| {
                values
                    .iter()
                    .filter_map(|value| value.as_u64())
                    .collect::<Vec<_>>()
            })
    };
    let as_bits = |key: &str| {
        as_numbers(key).map(|bits| {
            bits.iter()
                .fold(0_u16, |acc, bit| match u16::try_from(*bit) {
                    Ok(bit) if bit < 16 => acc | (1 << bit),
                    _ => {
                        warn!("ignoring out of range {key} bit {bit}");
                        acc
                    }
                })
        })
    };
    misc.us_weight_class = as_u16("openTypeOS2WeightClass").or(misc.us_weight_class);
    misc.us_width_class = as_u16("openTypeOS2WidthClass").or(misc.us_width_class);
    // ufo2ft defaults fsType to the installable-embedding bit when unset
    misc.fs_type = Some(as_bits("openTypeOS2Type").unwrap_or(1 << 2));
    // ufo2ft sets the style bit of the style map style name, which falls
    // back to the preferred subfamily name and then to the style name:
    // https://github.com/googlefonts/ufo2ft/blob/2f11b0ff84ef1f2494e54d1a7a15d92806d8337b/Lib/ufo2ft/fontInfoData.py#L76
    let style = default_source
        .custom_data
        .get("openTypeNamePreferredSubfamilyName")
        .and_then(|v| v.as_str())
        .unwrap_or(&default_source.name)
        .trim()
        .to_lowercase();
    misc.selection_flags |= match style.as_str() {
        "italic" => SelectionFlags::ITALIC,
        "bold" => SelectionFlags::BOLD,
        "bold italic" => SelectionFlags::BOLD | SelectionFlags::ITALIC,
        _ => SelectionFlags::REGULAR,
    };
    if let Some(bits) = as_bits("openTypeOS2Selection") {
        misc.selection_flags |= SelectionFlags::from_bits_truncate(bits);
    }
    misc.unicode_range_bits =
        as_numbers("openTypeOS2UnicodeRanges").map(|bits| bits.iter().map(|b| *b as u32).collect());
    misc.codepage_range_bits = as_numbers("openTypeOS2CodePageRanges")
        .map(|bits| bits.iter().map(|b| *b as u32).collect());
    if let Some(panose) = as_numbers("openTypeOS2Panose").filter(|values| values.len() == 10) {
        misc.panose = Some(Panose {
            family_type: panose[0] as u8,
            serif_style: panose[1] as u8,
            weight: panose[2] as u8,
            proportion: panose[3] as u8,
            contrast: panose[4] as u8,
            stroke_variation: panose[5] as u8,
            arm_style: panose[6] as u8,
            letterform: panose[7] as u8,
            midline: panose[8] as u8,
            x_height: panose[9] as u8,
        });
    }
    if let Some(class) = as_numbers("openTypeOS2FamilyClass").filter(|values| values.len() == 2) {
        misc.family_class = Some(((class[0] as i16) << 8) | class[1] as i16);
    }
    if let Some(created) = custom_data
        .get("openTypeHeadCreated")
        .and_then(|v| v.as_str())
    {
        match chrono::NaiveDateTime::parse_from_str(created, "%Y/%m/%d %H:%M:%S") {
            Ok(date) => misc.created = Some(date.and_utc()),
            Err(e) => warn!("invalid openTypeHeadCreated {created:?}: {e}"),
        }
    }
    Ok(())
}

pub(crate) fn to_ir_static_metadata(font_data: &Font) -> Result<StaticMetadata, Error> {
    let axes = font_data
        .axes
        .axes
        .iter()
        .map(|a| match a {
            crate::fontra::Axis::Discrete(_) => {
                Err(Error::UnsupportedConstruct(format!("discrete axis {a:?}")))
            }
            crate::fontra::Axis::Continuous(a) => Ok(a),
        })
        .map(|a| {
            let a = a?;
            let min = UserCoord::new(a.min_value);
            let default = UserCoord::new(a.default_value);
            let max = UserCoord::new(a.max_value);

            if min > default || max < default {
                return Err(Error::InconsistentAxisDefinitions(format!("{a:?}")));
            }

            let converter = if !a.mapping.is_empty() {
                let examples: Vec<_> = a
                    .mapping
                    .iter()
                    .map(|[raw_user, raw_design]| {
                        (UserCoord::new(*raw_user), DesignCoord::new(*raw_design))
                    })
                    .collect();
                let has_min_max = examples.iter().any(|(u, _)| *u == min)
                    && examples.iter().any(|(u, _)| *u == max);
                let default_idx = examples
                    .iter()
                    .position(|(u, _)| *u == default)
                    .filter(|_| has_min_max)
                    .ok_or(Error::MissingAxisMapping(a.tag))?;
                let converter = CoordConverter::new(examples, default_idx)?;
                if min.to_design(&converter) > max.to_design(&converter) {
                    return Err(Error::InconsistentAxisDefinitions(format!(
                        "the mapping of axis {:?} is not ascending",
                        a.name
                    )));
                }
                converter
            } else {
                CoordConverter::unmapped(min, default, max)
            };

            Ok(Axis {
                tag: a.tag,
                name: a.name.to_string(),
                hidden: a.hidden,
                min,
                default,
                max,
                converter,
                localized_names: Default::default(),
            })
        })
        .collect::<Result<Vec<_>, _>>()?;

    let global_locations = font_data
        .sources
        .values()
        .map(|source| to_ir_location(&axes, &source.location))
        .collect();

    let default_source = default_source(font_data, &axes)?;
    let italic_angle = default_source.italic_angle;
    let vertical_metrics = &default_source.line_metrics_vertical_layout;
    let build_vertical = vertical_metrics.contains_key("ascender")
        && vertical_metrics.contains_key("descender")
        && (vertical_metrics.contains_key("lineGap")
            || default_source
                .custom_data
                .contains_key("openTypeVheaVertTypoLineGap"));

    let mut static_metadata = StaticMetadata::new(
        font_data.units_per_em,
        to_ir_names(font_data, default_source),
        axes,
        Default::default(),
        global_locations,
        Default::default(),
        italic_angle,
        None,
        build_vertical,
    )
    .map_err(Error::VariationModelError)?;
    apply_custom_data(&mut static_metadata, font_data, default_source)?;
    Ok(static_metadata)
}

/// Normalize a design-space location, filling missing axes with their default.
fn to_ir_location<'a>(
    axes: impl IntoIterator<Item = &'a Axis>,
    design_location: &HashMap<AxisName, f64>,
) -> NormalizedLocation {
    axes.into_iter()
        .map(|axis| {
            let coord = design_location
                .get(&axis.name)
                .map(|v| normalize_axis_value(*v, axis))
                .unwrap_or_else(|| axis.default.to_normalized(&axis.converter));
            (axis.tag, coord)
        })
        .collect()
}

pub(crate) fn to_ir_global_metrics(
    static_metadata: &StaticMetadata,
    font_data: &Font,
) -> Result<GlobalMetrics, Error> {
    let mut metrics = GlobalMetricsBuilder::new();

    for source in font_data.sources.values() {
        let pos = to_ir_location(static_metadata.all_source_axes.iter(), &source.location);

        // A sparse source carries no metrics, but every metric needs a
        // master at the default location.
        if source.is_sparse && !pos.is_default() {
            continue;
        }

        macro_rules! set_metric {
            ($variant:ident, $key:literal) => {
                set_metric!(
                    $variant,
                    source.custom_data.get($key).and_then(|v| v.as_f64())
                )
            };
            ($variant:ident, $getter:expr) => {
                metrics.set_if_some(GlobalMetric::$variant, pos.clone(), $getter)
            };
        }

        let horizontal_metrics = |name: &str| {
            source
                .line_metrics_horizontal_layout
                .get(name)
                .map(|m| m.value)
        };

        let vertical_metrics = |name: &str| {
            source
                .line_metrics_vertical_layout
                .get(name)
                .map(|m| m.value)
        };

        let ascender = horizontal_metrics("ascender");
        let descender = horizontal_metrics("descender");
        let x_height = horizontal_metrics("xHeight");

        set_metric!(CapHeight, horizontal_metrics("capHeight"));
        set_metric!(XHeight, x_height);
        set_metric!(VheaAscender, vertical_metrics("ascender"));
        set_metric!(VheaDescender, vertical_metrics("descender"));
        set_metric!(VheaLineGap, vertical_metrics("lineGap"));

        // https://github.com/fontra/fontra/blob/2a19b8bd1/src/fontra/backends/designspace.py#L172
        set_metric!(HheaAscender, "openTypeHheaAscender");
        set_metric!(CaretOffset, "openTypeHheaCaretOffset");
        set_metric!(CaretSlopeRise, "openTypeHheaCaretSlopeRise");
        set_metric!(CaretSlopeRun, "openTypeHheaCaretSlopeRun");
        set_metric!(HheaDescender, "openTypeHheaDescender");
        set_metric!(HheaLineGap, "openTypeHheaLineGap");
        set_metric!(StrikeoutPosition, "openTypeOS2StrikeoutPosition");
        set_metric!(StrikeoutSize, "openTypeOS2StrikeoutSize");
        set_metric!(SubscriptXOffset, "openTypeOS2SubscriptXOffset");
        set_metric!(SubscriptXSize, "openTypeOS2SubscriptXSize");
        set_metric!(SubscriptYOffset, "openTypeOS2SubscriptYOffset");
        set_metric!(SubscriptYSize, "openTypeOS2SubscriptYSize");
        set_metric!(SuperscriptXOffset, "openTypeOS2SuperscriptXOffset");
        set_metric!(SuperscriptXSize, "openTypeOS2SuperscriptXSize");
        set_metric!(SuperscriptYOffset, "openTypeOS2SuperscriptYOffset");
        set_metric!(SuperscriptYSize, "openTypeOS2SuperscriptYSize");
        set_metric!(Os2TypoAscender, "openTypeOS2TypoAscender");
        set_metric!(Os2TypoDescender, "openTypeOS2TypoDescender");
        set_metric!(Os2TypoLineGap, "openTypeOS2TypoLineGap");
        set_metric!(Os2WinAscent, "openTypeOS2WinAscent");
        set_metric!(Os2WinDescent, "openTypeOS2WinDescent");
        set_metric!(VheaCaretOffset, "openTypeVheaCaretOffset");
        set_metric!(VheaCaretSlopeRise, "openTypeVheaCaretSlopeRise");
        set_metric!(VheaCaretSlopeRun, "openTypeVheaCaretSlopeRun");
        set_metric!(VheaLineGap, "openTypeVheaVertTypoLineGap");
        set_metric!(UnderlinePosition, "postscriptUnderlinePosition");
        set_metric!(UnderlineThickness, "postscriptUnderlineThickness");

        metrics.populate_defaults(
            &pos,
            static_metadata.units_per_em,
            x_height,
            ascender,
            descender,
            Some(source.italic_angle),
        );
    }

    metrics.build(&static_metadata.axes)
}

#[allow(dead_code)] // TEMPORARY
fn to_ir_glyph(
    global_axes: HashMap<AxisName, Tag>,
    codepoints: HashSet<u32>,
    fontra_glyph: &VariableGlyph,
) -> Result<Glyph, BadGlyph> {
    let _local_axes: HashMap<_, _> = fontra_glyph
        .axes
        .iter()
        .map(|a| (a.name.as_str(), a))
        .collect();

    let layer_locations: HashMap<_, _> = fontra_glyph
        .sources
        .iter()
        .map(|s| (&s.layer_name, &s.location))
        .collect();

    let mut instances = HashMap::new();
    for (layer_name, layer) in fontra_glyph.layers.iter() {
        // TODO: we need IR VARC support to proceed
        if !fontra_glyph.axes.is_empty() {
            todo!("Support local axes");
        }

        let Some(location) = layer_locations.get(layer_name) else {
            return Err(BadGlyph::new(
                fontra_glyph.name.clone(),
                BadGlyphKind::MissingLayer(layer_name.clone()),
            ));
        };
        let global_location: NormalizedLocation = global_axes
            .iter()
            .map(|(name, tag)| {
                (
                    *tag,
                    NormalizedCoord::new(location.get(name).copied().unwrap_or_default()),
                )
            })
            .collect();

        let contours: Vec<_> = layer
            .glyph
            .path
            .contours()
            .iter()
            .map(|c| to_ir_path(fontra_glyph.name.clone(), c))
            .collect::<Result<_, _>>()?;
        if instances
            .insert(
                global_location.clone(),
                GlyphInstance {
                    width: layer.glyph.x_advance,
                    contours,
                    ..Default::default()
                },
            )
            .is_some()
        {
            return Err(BadGlyph::new(
                fontra_glyph.name.clone(),
                BadGlyphKind::DuplicateLocation(global_location),
            ));
        };
    }

    Glyph::new(fontra_glyph.name.clone(), true, codepoints, instances)
}

#[allow(dead_code)] // TEMPORARY
fn add_to_path<'a>(
    path_builder: &'a mut GlyphPathBuilder,
    points: impl Iterator<Item = &'a Point>,
) -> Result<(), PathConversionError> {
    // Walk through the remaining points, accumulating off-curve points until we see an on-curve
    // https://github.com/googlefonts/glyphsLib/blob/24b4d340e4c82948ba121dcfe563c1450a8e69c9/Lib/glyphsLib/pens.py#L92
    for point in points {
        let point_type = point.point_type()?;
        // Smooth is only relevant to editors so ignore here
        match point_type {
            PointType::OnCurve | PointType::OnCurveSmooth => {
                path_builder.curve_to((point.x, point.y))?
            }
            PointType::OffCurveQuad | PointType::OffCurveCubic => {
                path_builder.offcurve((point.x, point.y))?
            }
        }
    }
    Ok(())
}

fn to_ir_path(glyph_name: GlyphName, contour: &Contour) -> Result<BezPath, BadGlyph> {
    // Based on glyphs2fontir/src/toir.rs to_ir_path
    // TODO(https://github.com/googlefonts/fontc/issues/700): share code
    if contour.points.is_empty() {
        return Ok(BezPath::new());
    }

    let mut path_builder = GlyphPathBuilder::new(contour.points.len());

    if !contour.is_closed {
        let first = contour.points.first().unwrap();
        let first_type = first
            .point_type()
            .map_err(|e| BadGlyph::new(glyph_name.clone(), e))?;
        if first_type.is_off_curve() {
            return Err(BadGlyph::new(
                glyph_name.clone(),
                PathConversionError::Parse("Open path starts with off-curve points".into()),
            ));
        }
        path_builder
            .move_to((first.x, first.y))
            .map_err(|e| BadGlyph::new(glyph_name.clone(), e))?;
        add_to_path(&mut path_builder, contour.points[1..].iter())
            .map_err(|e| BadGlyph::new(glyph_name.clone(), e))?;
    } else {
        add_to_path(&mut path_builder, contour.points.iter())
            .map_err(|e| BadGlyph::new(glyph_name.clone(), e))?;
    }

    let path = path_builder
        .build()
        .map_err(|e| BadGlyph::new(glyph_name.clone(), e))?;
    trace!(
        "Built a {} entry path for {}",
        path.elements().len(),
        glyph_name
    );
    Ok(path)
}

pub(crate) fn to_ir_gdef_categories(glyph_infos: &GlyphInfos) -> PreliminaryGdefCategories {
    let mark_category_glyphs = glyph_infos
        .iter()
        .filter(|(_, info)| info.category.as_deref() == Some("Mark"))
        .map(|(name, _)| name.clone())
        .collect();

    let categories = glyph_infos
        .iter()
        .filter_map(|(name, info)| {
            gdef_class(info.category.as_deref(), info.sub_category.as_deref())
                .map(|class| (name.clone(), class))
        })
        .collect();

    PreliminaryGdefCategories {
        categories,
        infer_from_anchors: true,
        mark_category_glyphs,
    }
}

fn gdef_class(category: Option<&str>, subcategory: Option<&str>) -> Option<GlyphClassDef> {
    match (category, subcategory) {
        (Some("Mark"), Some("Nonspacing" | "Spacing Combining")) => Some(GlyphClassDef::Mark),
        (_, Some("Ligature")) => Some(GlyphClassDef::Ligature),
        _ => None,
    }
}

#[cfg(test)]
mod tests {
    use std::collections::{HashMap, HashSet};

    use fontdrasil::{
        coords::{CoordConverter, DesignCoord, UserCoord},
        types::{Axes, Axis},
    };
    use fontir::ir::Glyph;
    use kurbo::{BezPath, PathEl};
    use write_fonts::{
        tables::os2::SelectionFlags,
        types::{NameId, Tag},
    };

    use crate::{
        fontra::{Font, VariableGlyph},
        test::testdata_dir,
        toir::to_ir_static_metadata,
    };

    use super::{
        Error, NameKey, normalize_axis_value, to_ir_global_metrics, to_ir_glyph, to_ir_names,
    };

    fn axis_tuples(axes: &Axes) -> Vec<(&str, Tag, f64, f64, f64)> {
        axes.iter()
            .map(|a| {
                (
                    a.name.as_str(),
                    a.tag,
                    a.min.to_f64(),
                    a.default.to_f64(),
                    a.max.to_f64(),
                )
            })
            .collect::<Vec<_>>()
    }

    fn commands(b: &BezPath) -> String {
        b.elements()
            .iter()
            .map(|e| match e {
                PathEl::MoveTo(..) => 'M',
                PathEl::LineTo(..) => 'L',
                PathEl::QuadTo(..) => 'Q',
                PathEl::CurveTo(..) => 'C',
                PathEl::ClosePath => 'Z',
            })
            .collect()
    }

    fn assert_contour_compatibility(glyph: &Glyph) {
        // compatible if all sources have the same drawing commands in the same order
        let unique_command_seqs = glyph
            .sources()
            .values()
            .map(|s| {
                s.contours
                    .iter()
                    .map(commands)
                    .collect::<Vec<_>>()
                    .join(" ")
            })
            .collect::<HashSet<_>>();
        assert_eq!(1, unique_command_seqs.len(), "{unique_command_seqs:?}");
    }

    #[test]
    fn static_metadata_of_2glyphs() {
        let font_data = Font::load(&testdata_dir().join("2glyphs.fontra")).unwrap();
        let static_metadata = to_ir_static_metadata(&font_data).unwrap();
        assert_eq!(1000, static_metadata.units_per_em);
        assert_eq!(
            vec![
                ("Weight", Tag::new(b"wght"), 200.0, 200.0, 900.0),
                ("Width", Tag::new(b"wdth"), 50.0, 100.0, 125.0)
            ],
            axis_tuples(&static_metadata.axes)
        );
    }

    #[test]
    fn ir_of_glyph_u20089() {
        let glyph_file = testdata_dir().join("2glyphs.fontra/glyphs/u20089.json");
        let fontra_glyph = VariableGlyph::from_file(&glyph_file).unwrap();
        let glyph = to_ir_glyph(
            HashMap::from([("Weight".to_string(), Tag::new(b"wght"))]),
            Default::default(),
            &fontra_glyph,
        )
        .unwrap();
        assert_eq!(
            vec![(2, 0), (2, 0)],
            glyph
                .sources()
                .values()
                .map(|s| (s.contours.len(), s.components.len()))
                .collect::<Vec<_>>()
        );
        assert_contour_compatibility(&glyph);
    }

    #[test]
    fn names_from_custom_data() {
        let mut font_data = Font::load(&testdata_dir().join("2glyphs.fontra")).unwrap();
        font_data.font_info.family_name = Some("Family".to_string());
        for (key, value) in [
            ("openTypeNameUniqueID", "unique-id"),
            ("openTypeNameVersion", "Version 9.876"),
            ("openTypeNamePreferredFamilyName", "Pref"),
            ("openTypeNameWWSFamilyName", "WWS"),
        ] {
            font_data
                .font_info
                .custom_data
                .insert(key.to_string(), serde_json::json!(value));
        }
        let default_source = font_data.sources.values().next().unwrap();
        let names = to_ir_names(&font_data, default_source);
        let name = |id: NameId| {
            names
                .iter()
                .find(|(key, _)| key.name_id == id)
                .map(|(_, value)| value.as_str())
        };
        assert_eq!(Some("unique-id"), name(NameId::UNIQUE_ID));
        assert_eq!(Some("Version 9.876"), name(NameId::VERSION_STRING));
        assert_eq!(Some("Pref"), name(NameId::FAMILY_NAME));
        assert_eq!(None, name(NameId::TYPOGRAPHIC_FAMILY_NAME));
        assert_eq!(Some("WWS"), name(NameId::WWS_FAMILY_NAME));
    }

    #[test]
    fn subfamily_names_from_the_default_source() {
        let mut font_data = Font::load(&testdata_dir().join("2glyphs.fontra")).unwrap();
        font_data.font_info.family_name = Some("Family".to_string());
        let mut default_source = font_data.sources.values().next().unwrap().clone();
        default_source.name = "Light".to_string();
        default_source.custom_data.insert(
            "openTypeNameWWSSubfamilyName".to_string(),
            serde_json::json!("WWS Light"),
        );
        let names = to_ir_names(&font_data, &default_source);
        let name = |id: NameId| {
            names
                .iter()
                .find(|(key, _)| key.name_id == id)
                .map(|(_, value)| value.as_str())
        };
        assert_eq!(Some("Family Light"), name(NameId::FAMILY_NAME));
        assert_eq!(Some("Regular"), name(NameId::SUBFAMILY_NAME));
        assert_eq!(Some("Family"), name(NameId::TYPOGRAPHIC_FAMILY_NAME));
        assert_eq!(Some("Light"), name(NameId::TYPOGRAPHIC_SUBFAMILY_NAME));
        assert_eq!(Some("WWS Light"), name(NameId::WWS_SUBFAMILY_NAME));
    }

    #[test]
    fn out_of_range_values_clamp_to_the_default_of_a_one_sided_axis() {
        // An axis whose default is its minimum has no normalized value below
        // 0, so a value under the minimum belongs at the default.
        let (min, default, max) = (
            UserCoord::new(100.0),
            UserCoord::new(100.0),
            UserCoord::new(900.0),
        );
        let axis = Axis {
            tag: Tag::new(b"wght"),
            name: "weight".into(),
            hidden: false,
            min,
            default,
            max,
            converter: CoordConverter::new(
                vec![
                    (min, DesignCoord::new(150.0)),
                    (max, DesignCoord::new(850.0)),
                ],
                0,
            )
            .unwrap(),
            localized_names: Default::default(),
        };
        assert_eq!(
            vec![0.0, 0.0, 0.5, 1.0, 1.0],
            [0.0, 150.0, 500.0, 850.0, 1000.0]
                .map(|v| normalize_axis_value(v, &axis).to_f64())
                .to_vec()
        );
    }

    #[test]
    fn a_decreasing_axis_mapping_is_an_error() {
        let mut font_data = Font::load(&testdata_dir().join("2glyphs.fontra")).unwrap();
        for axis in font_data.axes.axes.iter_mut() {
            if let crate::fontra::Axis::Continuous(axis) = axis
                && axis.tag == Tag::new(b"wght")
            {
                axis.mapping = vec![[200.0, 1.0], [300.018, 0.095], [900.0, 0.0]];
            }
        }
        assert!(matches!(
            to_ir_static_metadata(&font_data),
            Err(Error::InconsistentAxisDefinitions(_))
        ));
    }

    #[test]
    fn global_metrics_with_all_sources_sparse() {
        // A degenerate font whose sources are all sparse still gets metrics
        // masters at the default location.
        let mut font_data = Font::load(&testdata_dir().join("vertical.fontra")).unwrap();
        for source in font_data.sources.values_mut() {
            source.is_sparse = true;
        }
        let static_metadata = to_ir_static_metadata(&font_data).unwrap();
        let metrics = to_ir_global_metrics(&static_metadata, &font_data).unwrap();
        let at_default = metrics.at(static_metadata.default_location());
        assert_eq!(750.0, at_default.ascender.into_inner());
    }

    #[test]
    fn global_metrics_from_custom_data() {
        let font_data = Font::load(&testdata_dir().join("vertical.fontra")).unwrap();
        let static_metadata = to_ir_static_metadata(&font_data).unwrap();
        let metrics = to_ir_global_metrics(&static_metadata, &font_data).unwrap();
        let at_default = metrics.at(static_metadata.default_location());
        assert_eq!(725.0, at_default.os2_typo_ascender.into_inner());
        assert_eq!(950.0, at_default.caret_slope_rise.into_inner());
        assert_eq!(-120.0, at_default.underline_position.into_inner());
    }

    #[test]
    fn vertical_tables_need_the_three_vertical_metrics() {
        let mut font_data = Font::load(&testdata_dir().join("vertical.fontra")).unwrap();
        assert!(to_ir_static_metadata(&font_data).unwrap().build_vertical);
        for source in font_data.sources.values_mut() {
            source.line_metrics_vertical_layout.remove("lineGap");
        }
        assert!(!to_ir_static_metadata(&font_data).unwrap().build_vertical);
        for source in font_data.sources.values_mut() {
            source.custom_data.insert(
                "openTypeVheaVertTypoLineGap".to_string(),
                serde_json::json!(0),
            );
        }
        assert!(to_ir_static_metadata(&font_data).unwrap().build_vertical);
    }

    #[test]
    fn version_from_font_info() {
        let mut font_data = Font::load(&testdata_dir().join("2glyphs.fontra")).unwrap();
        font_data.font_info.version_major = Some(2);
        font_data.font_info.version_minor = Some(5);
        let static_metadata = to_ir_static_metadata(&font_data).unwrap();
        assert_eq!(
            (2, 5),
            (
                static_metadata.misc.version_major,
                static_metadata.misc.version_minor
            )
        );
        assert_eq!(
            Some(&"Version 2.005".to_string()),
            static_metadata
                .names
                .get(&NameKey::new_bmp_only(NameId::VERSION_STRING))
        );
    }

    #[test]
    fn fixed_pitch_from_the_default_source() {
        let mut font_data = Font::load(&testdata_dir().join("2glyphs.fontra")).unwrap();
        assert_eq!(
            None,
            to_ir_static_metadata(&font_data)
                .unwrap()
                .misc
                .is_fixed_pitch
        );
        for source in font_data.sources.values_mut() {
            source.custom_data.insert(
                "postscriptIsFixedPitch".to_string(),
                serde_json::json!(true),
            );
        }
        assert_eq!(
            Some(true),
            to_ir_static_metadata(&font_data)
                .unwrap()
                .misc
                .is_fixed_pitch
        );
    }

    #[test]
    fn vendor_id_from_font_info() {
        let mut font_data = Font::load(&testdata_dir().join("2glyphs.fontra")).unwrap();
        font_data.font_info.vendor_id = Some("TEST".to_string());
        let static_metadata = to_ir_static_metadata(&font_data).unwrap();
        assert_eq!(Tag::new(b"TEST"), static_metadata.misc.vendor_id);

        font_data.font_info.vendor_id = Some("  ".to_string());
        let static_metadata = to_ir_static_metadata(&font_data).unwrap();
        assert_eq!(Tag::new(b"NONE"), static_metadata.misc.vendor_id);
    }

    #[test]
    fn font_info_custom_data_of_vertical() {
        let font_data = Font::load(&testdata_dir().join("vertical.fontra")).unwrap();
        let static_metadata = to_ir_static_metadata(&font_data).unwrap();
        let misc = &static_metadata.misc;
        assert_eq!(Some(450), misc.us_weight_class);
        assert_eq!(Some(3), misc.us_width_class);
        assert_eq!(Some(0), misc.fs_type);
        // The default source is Regular, the bit 7 is authored.
        assert_eq!(
            SelectionFlags::REGULAR | SelectionFlags::USE_TYPO_METRICS,
            misc.selection_flags
        );
        let panose = misc.panose.as_ref().unwrap();
        assert_eq!((2, 5), (panose.family_type, panose.weight));
        assert_eq!(Some(0x0105), misc.family_class);
        assert_eq!(
            Some(
                chrono::NaiveDate::from_ymd_opt(2024, 1, 2)
                    .unwrap()
                    .and_hms_opt(3, 4, 5)
                    .unwrap()
                    .and_utc()
            ),
            misc.created
        );
    }

    #[test]
    fn out_of_range_flag_bits_are_ignored() {
        // A bit number that does not fit the field.
        let mut font_data = Font::load(&testdata_dir().join("vertical.fontra")).unwrap();
        font_data.font_info.custom_data.insert(
            "openTypeOS2Selection".to_string(),
            serde_json::json!([7, 16, 99]),
        );
        font_data
            .font_info
            .custom_data
            .insert("openTypeOS2Type".to_string(), serde_json::json!([2, 42]));
        let static_metadata = to_ir_static_metadata(&font_data).unwrap();
        assert!(
            static_metadata
                .misc
                .selection_flags
                .contains(SelectionFlags::USE_TYPO_METRICS)
        );
        assert_eq!(Some(1 << 2), static_metadata.misc.fs_type);
    }
}
