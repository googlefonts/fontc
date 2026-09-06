//! Functions to convert fontra things to fontc IR things

use std::{
    collections::{BTreeMap, BTreeSet, HashMap, HashSet},
    str::FromStr,
};

use fontdrasil::{
    coords::{CoordConverter, DesignCoord, NormalizedCoord, NormalizedLocation, UserCoord},
    types::{Axes, Axis, GlyphName},
};
use fontir::{
    error::{BadGlyph, BadGlyphKind, Error, PathConversionError},
    ir::{
        AnchorBuilder, Component, DEFAULT_VENDOR_ID, GlobalMetric, GlobalMetrics,
        GlobalMetricsBuilder, Glyph, GlyphInstance, GlyphOrder, GlyphPathBuilder, KernGroup,
        KernSide, KerningInstance, KerningLocations, NameBuilder, NameKey, Panose,
        PreliminaryGdefCategories, StaticMetadata,
    },
};
use kurbo::BezPath;
use log::{debug, trace, warn};
use smol_str::SmolStr;
use write_fonts::{
    tables::{gdef::GlyphClassDef, os2::SelectionFlags},
    types::{NameId, Tag},
};

use crate::fontra::{
    self, AxisName, Contour, Font, FontSource, GlyphAxis, GlyphInfos, GlyphSource, Kerning,
    Location, Point, PointType, VariableGlyph,
};

/// A hidden axis with a synthetic tag for each glyph-local axis that does not
/// correspond to a font axis.
fn glyph_axes(font_data: &Font) -> Result<BTreeMap<GlyphName, Axes>, Error> {
    let font_axis_names: HashSet<&str> = font_data
        .axes
        .axes
        .iter()
        .map(|a| a.name().as_str())
        .collect();
    let mut result = BTreeMap::new();
    for (glyph_name, glyph) in font_data.glyphs.iter() {
        let mut axes = Vec::new();
        for glyph_axis in &glyph.axes {
            // Maps to the font axis of the same name.
            if font_axis_names.contains(glyph_axis.name.as_str()) {
                continue;
            }
            if glyph_axis.min_value > glyph_axis.default_value
                || glyph_axis.default_value > glyph_axis.max_value
            {
                return Err(Error::InconsistentAxisDefinitions(format!(
                    "glyph axis {:?} of {glyph_name}",
                    glyph_axis.name
                )));
            }
            let tag = local_axis_tag(axes.len());
            let (min, default, max) = axis_tuple(glyph_axis);
            axes.push(Axis {
                tag,
                name: glyph_axis.name.clone(),
                hidden: true,
                min,
                default,
                max,
                converter: CoordConverter::unmapped(min, default, max),
                localized_names: Default::default(),
            });
        }
        if !axes.is_empty() {
            result.insert(glyph_name.clone(), Axes::new(axes));
        }
    }
    Ok(result)
}

/// The synthetic tag of the nth glyph-local axis.
fn local_axis_tag(index: usize) -> Tag {
    let tag = format!("V{index:03}");
    Tag::new(
        tag.as_bytes()
            .try_into()
            .expect("index fits in three digits"),
    )
}

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

pub(crate) fn to_ir_static_metadata(
    font_data: &Font,
    emit_varc: bool,
) -> Result<StaticMetadata, Error> {
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

    // Add glyph-local axes to fvar as hidden axes.
    let glyph_axes = glyph_axes(font_data)?;
    let mut axes = axes;
    if emit_varc {
        let tags: BTreeSet<Tag> = glyph_axes
            .values()
            .flat_map(|axes| axes.iter().map(|axis| axis.tag))
            .collect();
        axes.extend(tags.into_iter().map(|tag| {
            let (min, default, max) = (
                UserCoord::new(-1.0),
                UserCoord::new(0.0),
                UserCoord::new(1.0),
            );
            Axis {
                tag,
                name: tag.to_string(),
                hidden: true,
                min,
                default,
                max,
                converter: CoordConverter::unmapped(min, default, max),
                localized_names: Default::default(),
            }
        }));
    }

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
    static_metadata.glyph_axes = glyph_axes;
    apply_custom_data(&mut static_metadata, font_data, default_source)?;
    Ok(static_metadata)
}

fn glyph_source_location(
    font_data: &Font,
    glyph: &VariableGlyph,
    source: &GlyphSource,
) -> Location {
    let mut location = source
        .location_base
        .as_ref()
        .and_then(|base| font_data.sources.get(base))
        .map(|font_source| font_source.location.clone())
        .unwrap_or_default();
    location.retain(|name, _| !glyph.axes.iter().any(|a| a.name == *name));
    location.extend(source.location.iter().map(|(name, v)| (name.clone(), *v)));
    location
}

/// Normalize a design-space location, filling missing axes with their default.
pub(crate) fn to_ir_location<'a>(
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

pub(crate) fn to_ir_glyph(
    static_metadata: &StaticMetadata,
    font_data: &Font,
    codepoints: HashSet<u32>,
    fontra_glyph: &VariableGlyph,
    anchors: &mut AnchorBuilder,
) -> Result<Glyph, BadGlyph> {
    let axes = &static_metadata.all_source_axes;
    let local_axes = static_metadata.glyph_axes.get(&fontra_glyph.name);
    let mut responds_to_global_axes_cache: HashMap<GlyphName, bool> = HashMap::new();

    let mut instances = HashMap::new();
    // Layers not referenced by any active source, e.g. backgrounds, do not
    // contribute to the final font.
    for source in fontra_glyph.sources.iter().filter(|s| !s.inactive) {
        let Some(layer) = fontra_glyph.layers.get(&source.layer_name) else {
            return Err(BadGlyph::new(
                fontra_glyph.name.clone(),
                BadGlyphKind::MissingLayer(source.layer_name.clone()),
            ));
        };

        let design_location = glyph_source_location(font_data, fontra_glyph, source);
        let location_axes = axes
            .iter()
            .map(|axis| {
                local_axes
                    .and_then(|local| local.get(&axis.tag))
                    .unwrap_or(axis)
            })
            .chain(
                local_axes
                    .into_iter()
                    .flat_map(|local| local.iter())
                    .filter(|axis| !axes.contains(&axis.tag)),
            );
        let global_location: NormalizedLocation = location_axes
            .map(|axis| {
                let value = design_location
                    .get(&axis.name)
                    .filter(|_| {
                        local_axes.is_some_and(|local| local.contains(&axis.tag))
                            || font_data.axes.axes.iter().any(|a| a.name() == &axis.name)
                    })
                    .copied();
                let glyph_axis = fontra_glyph.axes.iter().find(|a| a.name == axis.name);
                let coord = match (value, glyph_axis) {
                    // A glyph axis defines the range for its values, whether
                    // they map to its own local tag or to a font axis's tag.
                    (Some(value), Some(glyph_axis)) => {
                        normalize_glyph_axis_value(value, glyph_axis)
                    }
                    (Some(value), None) => normalize_axis_value(value, axis),
                    (None, _) => axis.default.to_normalized(&axis.converter),
                };
                (axis.tag, coord)
            })
            .collect();

        // Keep the first of multiple sources at the same location.
        if instances.contains_key(&global_location) {
            warn!(
                "'{}': ignoring source '{}' at duplicate location {global_location:?}",
                fontra_glyph.name, source.name
            );
            continue;
        }

        let contours: Vec<_> = layer
            .glyph
            .path
            .contours()
            .iter()
            .map(|c| to_ir_path(fontra_glyph.name.clone(), c))
            .collect::<Result<_, _>>()?;

        let components = layer
            .glyph
            .components
            .iter()
            .map(|c| {
                let base_glyph_axes = font_data
                    .glyphs
                    .get(&c.name)
                    .map(|g| g.axes.as_slice())
                    .unwrap_or(&[]);
                let reset = !responds_to_global_axes(
                    &c.name,
                    font_data,
                    &mut responds_to_global_axes_cache,
                );
                to_ir_component(c, static_metadata, base_glyph_axes, reset)
            })
            .collect();

        for anchor in layer.glyph.anchors.iter() {
            let Some(name) = &anchor.name else {
                warn!("'{}': ignoring an anchor without a name", fontra_glyph.name);
                continue;
            };
            anchors.add(
                name.clone(),
                global_location.clone(),
                kurbo::Point::new(anchor.x, anchor.y),
            )?;
        }

        instances.insert(
            global_location,
            GlyphInstance {
                width: layer.glyph.x_advance,
                height: layer.glyph.y_advance,
                vertical_origin: layer.glyph.vertical_origin,
                contours,
                components,
            },
        );
    }

    // Set a component axis that any source specifies in every source. Set it at the
    // default value where a source omits it. The VARC table lists the axes
    // once per component.
    let component_count = instances.values().next().map(|i| i.components.len());
    if let Some(count) = component_count
        && instances.values().all(|i| i.components.len() == count)
    {
        for idx in 0..count {
            let tags: BTreeSet<Tag> = instances
                .values()
                .flat_map(|i| i.components[idx].location.axis_tags().copied())
                .collect();
            for instance in instances.values_mut() {
                let location = &mut instance.components[idx].location;
                for tag in &tags {
                    if location.get(*tag).is_none() {
                        location.insert(*tag, NormalizedCoord::new(0.0));
                    }
                }
            }
        }
    }

    let mut glyph = Glyph::new(fontra_glyph.name.clone(), true, codepoints, instances)?;
    if let Some(local_axes) = local_axes {
        glyph.set_axes(local_axes.clone());
    }
    Ok(glyph)
}

/// Similar to fontra-compile's
/// [`respondsToGlobalAxes`](https://github.com/fontra/fontra-compile/blob/01d784d86c/src/fontra_compile/builder.py#L420-L429),
/// with `locationBase` resolved.
fn responds_to_global_axes(
    name: &GlyphName,
    font_data: &Font,
    cache: &mut HashMap<GlyphName, bool>,
) -> bool {
    if let Some(&cached) = cache.get(name) {
        return cached;
    }
    // Break component cycles: assume "no" while recursing through this glyph.
    cache.insert(name.clone(), false);
    let Some(glyph) = font_data.glyphs.get(name) else {
        return false;
    };
    let local: HashSet<&str> = glyph.axes.iter().map(|a| a.name.as_str()).collect();
    let resolved: Vec<Location> = glyph
        .sources
        .iter()
        .map(|source| glyph_source_location(font_data, glyph, source))
        .collect();
    let global_axes: HashSet<&str> = resolved
        .iter()
        .flat_map(|location| location.keys().map(String::as_str))
        .filter(|name| !local.contains(name))
        .collect();
    // A source that sets an axis, even to its default value, differs from a
    // source that omits it.
    let mut responds = global_axes.iter().any(|axis| {
        let mut values = resolved.iter().map(|location| location.get(*axis).copied());
        let first = values.next().unwrap_or_default();
        values.any(|value| value != first)
    });
    if !responds {
        'outer: for layer in glyph.layers.values() {
            for component in &layer.glyph.components {
                if responds_to_global_axes(&component.name, font_data, cache) {
                    responds = true;
                    break 'outer;
                }
            }
        }
    }
    cache.insert(name.clone(), responds);
    responds
}

/// Port of fontra-compile's
/// [`axisTuple`](https://github.com/fontra/fontra-compile/blob/01d784d86c/src/fontra_compile/builder.py#L874-L896).
fn axis_tuple(axis: &GlyphAxis) -> (UserCoord, UserCoord, UserCoord) {
    let (mut min_value, default_value, mut max_value) =
        (axis.min_value, axis.default_value, axis.max_value);
    if min_value < default_value && default_value < max_value {
        let min_diff = default_value - min_value;
        let max_diff = max_value - default_value;
        if min_diff > max_diff {
            max_value = default_value + min_diff;
        } else if min_diff < max_diff {
            min_value = default_value - max_diff;
        }
    }
    (
        UserCoord::new(min_value),
        UserCoord::new(default_value),
        UserCoord::new(max_value),
    )
}

/// Similar to fontTools'
/// [`normalizeValue`](https://github.com/fonttools/fonttools/blob/7af8bf5cbf/Lib/fontTools/varLib/models.py#L54-L84).
fn normalize_glyph_axis_value(value: f64, axis: &GlyphAxis) -> NormalizedCoord {
    let (min, default, max) = axis_tuple(axis);
    let value = value.clamp(min.to_f64(), max.to_f64());
    let converter = CoordConverter::unmapped(min, default, max);
    DesignCoord::new(value).to_normalized(&converter)
}

fn to_ir_component(
    component: &fontra::Component,
    static_metadata: &StaticMetadata,
    base_glyph_axes: &[GlyphAxis],
    reset_unspecified_axes: bool,
) -> Component {
    let global_axes = &static_metadata.all_source_axes;
    let location: NormalizedLocation = component
        .location
        .iter()
        .filter_map(|(name, value)| {
            if let Some(glyph_axis) = base_glyph_axes.iter().find(|a| a.name == *name) {
                let tag = static_metadata
                    .glyph_axes
                    .get(&component.name)
                    .and_then(|axes| axes.iter().find(|a| a.name == *name))
                    .map(|a| a.tag)
                    .or_else(|| global_axes.iter().find(|a| a.name == *name).map(|a| a.tag))?;
                return Some((tag, normalize_glyph_axis_value(*value, glyph_axis)));
            }
            global_axes
                .iter()
                .find(|a| a.name == *name)
                .map(|a| (a.tag, normalize_axis_value(*value, a)))
        })
        .collect();
    Component::new_variable(
        component.name.clone(),
        component.transformation.clone(),
        location,
        reset_unspecified_axes,
    )
}

/// The sources that kern.
pub(crate) fn kerning_sources<'a>(
    static_metadata: &'a StaticMetadata,
    font_data: &'a Font,
    kerning: &'a Kerning,
) -> impl Iterator<Item = (usize, NormalizedLocation)> + 'a {
    kerning
        .source_identifiers
        .iter()
        .enumerate()
        .filter_map(move |(idx, identifier)| {
            let Some(source) = font_data.sources.get(identifier) else {
                warn!("kerning references unknown font source {identifier:?}");
                return None;
            };
            // Fontra ignores kerning at sparse sources.
            if source.is_sparse {
                warn!("ignoring kerning at sparse font source {identifier:?}");
                return None;
            }
            // Drop a source with no kerning so the kern interpolates across
            // it instead of being pinned toward 0 there.
            if !kern_source_has_values(kerning, idx) {
                return None;
            }
            let location = to_ir_location(static_metadata.all_source_axes.iter(), &source.location);
            Some((idx, location))
        })
}

/// The default location and the location of every source that kerns.
pub(crate) fn to_ir_kerning_locations(
    static_metadata: &StaticMetadata,
    font_data: &Font,
    kerning: Option<&Kerning>,
) -> KerningLocations {
    let mut locations = KerningLocations::default();
    // Keep the default source (it anchors the variation model).
    locations
        .locations
        .insert(static_metadata.default_location().clone());
    if let Some(kerning) = kerning {
        locations.locations.extend(
            kerning_sources(static_metadata, font_data, kerning).map(|(_, location)| location),
        );
    }
    locations
}

/// Whether a kerning source has any value.
fn kern_source_has_values(kerning: &Kerning, idx: usize) -> bool {
    kerning
        .values
        .values()
        .flat_map(|side2_values| side2_values.values())
        .any(|values| values.get(idx).is_some_and(|value| value.is_some()))
}

/// The kern groups of both sides, with members missing from the glyph order
/// pruned.
pub(crate) fn to_ir_kern_groups(
    kerning: &Kerning,
    glyph_order: &GlyphOrder,
) -> BTreeMap<KernGroup, BTreeSet<GlyphName>> {
    let side1 = kerning
        .groups_side1
        .iter()
        .map(|(name, members)| (KernGroup::Side1(name.clone()), members));
    let side2 = kerning
        .groups_side2
        .iter()
        .map(|(name, members)| (KernGroup::Side2(name.clone()), members));
    side1
        .chain(side2)
        .filter_map(|(group, members)| {
            let members: BTreeSet<_> = members
                .iter()
                .filter_map(|member| {
                    let member = GlyphName::new(member.as_str());
                    if glyph_order.contains(&member) {
                        Some(member)
                    } else {
                        debug!(
                            "kern group {group:?} references non-existent glyph '{member}'; ignoring"
                        );
                        None
                    }
                })
                .collect();
            (!members.is_empty()).then_some((group, members))
        })
        .collect()
}

/// The kern values of one kerning source.
pub(crate) fn to_ir_kerning_instance(
    kerning: &Kerning,
    source_idx: usize,
    location: &NormalizedLocation,
    glyph_order: &GlyphOrder,
) -> KerningInstance {
    let groups = to_ir_kern_groups(kerning, glyph_order);
    let resolve = |name: &str, group: fn(SmolStr) -> KernGroup| {
        if let Some(group_name) = name.strip_prefix('@') {
            let group = group(group_name.into());
            if !groups.contains_key(&group) {
                warn!("'{name}' is not a valid kern group; ignored");
                return None;
            }
            Some(KernSide::Group(group))
        } else {
            let glyph_name = GlyphName::new(name);
            if !glyph_order.contains(&glyph_name) {
                warn!("'{name}' refers to a non-existent glyph; ignored");
                return None;
            }
            Some(KernSide::Glyph(glyph_name))
        }
    };

    let mut instance = KerningInstance {
        location: location.clone(),
        ..Default::default()
    };
    for (side1, side2_values) in kerning.values.iter() {
        for (side2, values) in side2_values.iter() {
            let Some(Some(value)) = values.get(source_idx) else {
                continue;
            };
            let (Some(side1), Some(side2)) = (
                resolve(side1, KernGroup::Side1),
                resolve(side2, KernGroup::Side2),
            ) else {
                warn!("kerning unable to resolve at least one of '{side1}', '{side2}'; ignoring");
                continue;
            };
            instance.kerns.insert((side1, side2), (*value).into());
        }
    }
    instance.groups = groups;
    instance
}

fn add_to_path<'a>(
    path_builder: &'a mut GlyphPathBuilder,
    points: impl Iterator<Item = &'a Point>,
    mut segment_type: Option<PointType>,
) -> Result<(), PathConversionError> {
    // Walk through the remaining points, accumulating off-curve points until we see an on-curve
    // https://github.com/googlefonts/glyphsLib/blob/24b4d340e4c82948ba121dcfe563c1450a8e69c9/Lib/glyphsLib/pens.py#L92
    for point in points {
        let point_type = point.point_type()?;
        // Smooth is only relevant to editors so ignore here
        match point_type {
            PointType::OnCurve | PointType::OnCurveSmooth => {
                if segment_type == Some(PointType::OffCurveQuad) {
                    path_builder.qcurve_to((point.x, point.y))?
                } else {
                    path_builder.curve_to((point.x, point.y))?
                }
                segment_type = None;
            }
            PointType::OffCurveQuad | PointType::OffCurveCubic => {
                path_builder.offcurve((point.x, point.y))?;
                segment_type = Some(point_type);
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
        // Fontra strips the leading and trailing off-curve points of an open
        // contour:
        // https://github.com/fontra/fontra/blob/2a19b8bd1/src/fontra/core/path.py#L202-L214
        let point_types = contour
            .points
            .iter()
            .map(Point::point_type)
            .collect::<Result<Vec<_>, _>>()
            .map_err(|e| BadGlyph::new(glyph_name.clone(), e))?;
        let Some(first) = point_types.iter().position(|t| !t.is_off_curve()) else {
            return Ok(BezPath::new());
        };
        let last = point_types.iter().rposition(|t| !t.is_off_curve()).unwrap();
        let points = &contour.points[first..=last];
        path_builder
            .move_to((points[0].x, points[0].y))
            .map_err(|e| BadGlyph::new(glyph_name.clone(), e))?;
        add_to_path(&mut path_builder, points[1..].iter(), None)
            .map_err(|e| BadGlyph::new(glyph_name.clone(), e))?;
    } else {
        // https://github.com/fontra/fontra/blob/2a19b8bd1/src/fontra/core/path.py#L218-L222
        let segment_type = contour
            .points
            .last()
            .and_then(|point| point.point_type().ok())
            .filter(|point_type| point_type.is_off_curve());
        add_to_path(&mut path_builder, contour.points.iter(), segment_type)
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
        coords::{CoordConverter, DesignCoord, NormalizedLocation, UserCoord},
        types::{Axes, Axis, GlyphName},
    };
    use fontir::ir::{AnchorBuilder, Glyph, GlyphOrder, KernGroup, KernSide};
    use kurbo::{BezPath, PathEl};
    use write_fonts::{
        tables::os2::SelectionFlags,
        types::{NameId, Tag},
    };

    use crate::{
        fontra::{self, Font, GlyphAxis, Kerning, VariableGlyph},
        source::HORIZONTAL_KERNING_TYPE,
        test::testdata_dir,
        toir::to_ir_static_metadata,
    };

    use super::{
        Error, NameKey, StaticMetadata, normalize_axis_value, normalize_glyph_axis_value,
        responds_to_global_axes, to_ir_global_metrics, to_ir_glyph, to_ir_kerning_instance,
        to_ir_kerning_locations, to_ir_names, to_ir_path,
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
        let static_metadata = to_ir_static_metadata(&font_data, false).unwrap();
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
    fn quad_points_make_quadratic_segments() {
        let contour = packed_contour(
            &[
                (0.0, 0.0, 0),
                (50.0, 100.0, 1),
                (150.0, 100.0, 1),
                (200.0, 0.0, 0),
            ],
            true,
        );
        let path = to_ir_path(GlyphName::new("test"), &contour).unwrap();
        assert_eq!("M0,0 Q50,100 100,100 Q150,100 200,0 L0,0 Z", path.to_svg());
    }

    #[test]
    fn an_open_contour_loses_its_leading_and_trailing_off_curve_points() {
        let contour = packed_contour(
            &[
                (-10.0, 0.0, 2),
                (0.0, 0.0, 0),
                (50.0, 100.0, 2),
                (150.0, 100.0, 2),
                (200.0, 0.0, 0),
                (210.0, 0.0, 2),
            ],
            false,
        );
        let path = to_ir_path(GlyphName::new("test"), &contour).unwrap();
        assert_eq!("M0,0 C50,100 150,100 200,0", path.to_svg());

        let contour = packed_contour(&[(-10.0, 0.0, 2), (210.0, 0.0, 1)], false);
        let path = to_ir_path(GlyphName::new("test"), &contour).unwrap();
        assert!(path.elements().is_empty());
    }

    #[test]
    fn trailing_quad_points_lead_to_the_first_point() {
        let contour = packed_contour(&[(0.0, 0.0, 0), (50.0, 100.0, 1), (150.0, 100.0, 1)], true);
        let path = to_ir_path(GlyphName::new("test"), &contour).unwrap();
        assert_eq!("M0,0 Q50,100 100,100 Q150,100 0,0 Z", path.to_svg());
    }

    fn packed_contour(points: &[(f64, f64, u8)], is_closed: bool) -> fontra::Contour {
        let packed = fontra::PackedPath {
            coordinates: points.iter().flat_map(|(x, y, _)| [*x, *y]).collect(),
            point_types: points
                .iter()
                .map(|(_, _, point_type)| *point_type)
                .collect(),
            contour_info: vec![fontra::ContourInfo {
                end_point: points.len() - 1,
                is_closed,
            }],
        };
        packed.unpacked_contours().remove(0)
    }

    #[test]
    fn ir_of_glyph_u20089() {
        let font_data = Font::load(&testdata_dir().join("2glyphs.fontra")).unwrap();
        let static_metadata = to_ir_static_metadata(&font_data, false).unwrap();
        let glyph_file = testdata_dir().join("2glyphs.fontra/glyphs/u20089.json");
        let fontra_glyph = VariableGlyph::from_file(&glyph_file).unwrap();
        let glyph = to_ir_glyph(
            &static_metadata,
            &font_data,
            Default::default(),
            &fontra_glyph,
            &mut AnchorBuilder::new(fontra_glyph.name.clone()),
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
            to_ir_static_metadata(&font_data, false),
            Err(Error::InconsistentAxisDefinitions(_))
        ));
    }

    #[test]
    fn source_locations_clamp_to_the_axis_range() {
        // behDotless-ar has a source at Mashq 0, below the axis minimum of 7.
        let font_data = Font::load(&testdata_dir().join("Raqq.fontra")).unwrap();
        let static_metadata = to_ir_static_metadata(&font_data, false).unwrap();
        let name = GlyphName::new("behDotless-ar");
        let fontra_glyph = font_data.glyphs.get(&name).unwrap();
        let mut anchors = AnchorBuilder::new(name);
        let glyph = to_ir_glyph(
            &static_metadata,
            &font_data,
            Default::default(),
            fontra_glyph,
            &mut anchors,
        )
        .unwrap();
        let mut mashq: Vec<f64> = glyph
            .sources()
            .keys()
            .map(|loc| loc.get(Tag::new(b"MSHQ")).unwrap().to_f64())
            .collect();
        mashq.sort_by(|a, b| a.partial_cmp(b).unwrap());
        assert_eq!(vec![-1.0, 0.0, 1.0], mashq);

        let anchors = anchors.build().unwrap();
        let bottom = anchors
            .anchors
            .iter()
            .find(|a| a.original_name == "bottom")
            .unwrap();
        let below_min = NormalizedLocation::for_pos(&[("MSHQ", -1.0), ("SPAC", 0.0)]);
        assert_eq!(
            Some(&kurbo::Point::new(407.0, 19.0)),
            bottom.positions.get(&below_min)
        );
    }

    #[test]
    fn global_metrics_with_all_sources_sparse() {
        // A degenerate font whose sources are all sparse still gets metrics
        // masters at the default location.
        let mut font_data = Font::load(&testdata_dir().join("vertical.fontra")).unwrap();
        for source in font_data.sources.values_mut() {
            source.is_sparse = true;
        }
        let static_metadata = to_ir_static_metadata(&font_data, false).unwrap();
        let metrics = to_ir_global_metrics(&static_metadata, &font_data).unwrap();
        let at_default = metrics.at(static_metadata.default_location());
        assert_eq!(750.0, at_default.ascender.into_inner());
    }

    #[test]
    fn global_metrics_from_custom_data() {
        let font_data = Font::load(&testdata_dir().join("vertical.fontra")).unwrap();
        let static_metadata = to_ir_static_metadata(&font_data, false).unwrap();
        let metrics = to_ir_global_metrics(&static_metadata, &font_data).unwrap();
        let at_default = metrics.at(static_metadata.default_location());
        assert_eq!(725.0, at_default.os2_typo_ascender.into_inner());
        assert_eq!(950.0, at_default.caret_slope_rise.into_inner());
        assert_eq!(-120.0, at_default.underline_position.into_inner());
    }

    #[test]
    fn vertical_tables_need_the_three_vertical_metrics() {
        let mut font_data = Font::load(&testdata_dir().join("vertical.fontra")).unwrap();
        assert!(
            to_ir_static_metadata(&font_data, false)
                .unwrap()
                .build_vertical
        );
        for source in font_data.sources.values_mut() {
            source.line_metrics_vertical_layout.remove("lineGap");
        }
        assert!(
            !to_ir_static_metadata(&font_data, false)
                .unwrap()
                .build_vertical
        );
        for source in font_data.sources.values_mut() {
            source.custom_data.insert(
                "openTypeVheaVertTypoLineGap".to_string(),
                serde_json::json!(0),
            );
        }
        assert!(
            to_ir_static_metadata(&font_data, false)
                .unwrap()
                .build_vertical
        );
    }

    #[test]
    fn version_from_font_info() {
        let mut font_data = Font::load(&testdata_dir().join("2glyphs.fontra")).unwrap();
        font_data.font_info.version_major = Some(2);
        font_data.font_info.version_minor = Some(5);
        let static_metadata = to_ir_static_metadata(&font_data, false).unwrap();
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
            to_ir_static_metadata(&font_data, false)
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
            to_ir_static_metadata(&font_data, false)
                .unwrap()
                .misc
                .is_fixed_pitch
        );
    }

    #[test]
    fn vendor_id_from_font_info() {
        let mut font_data = Font::load(&testdata_dir().join("2glyphs.fontra")).unwrap();
        font_data.font_info.vendor_id = Some("TEST".to_string());
        let static_metadata = to_ir_static_metadata(&font_data, false).unwrap();
        assert_eq!(Tag::new(b"TEST"), static_metadata.misc.vendor_id);

        font_data.font_info.vendor_id = Some("  ".to_string());
        let static_metadata = to_ir_static_metadata(&font_data, false).unwrap();
        assert_eq!(Tag::new(b"NONE"), static_metadata.misc.vendor_id);
    }

    #[test]
    fn font_info_custom_data_of_vertical() {
        let font_data = Font::load(&testdata_dir().join("vertical.fontra")).unwrap();
        let static_metadata = to_ir_static_metadata(&font_data, false).unwrap();
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
        let static_metadata = to_ir_static_metadata(&font_data, false).unwrap();
        assert!(
            static_metadata
                .misc
                .selection_flags
                .contains(SelectionFlags::USE_TYPO_METRICS)
        );
        assert_eq!(Some(1 << 2), static_metadata.misc.fs_type);
    }

    #[test]
    fn kerning_instance_of_mutator_sans() {
        let font_data = Font::load(&testdata_dir().join("MutatorSans.fontra")).unwrap();
        let kerning = font_data.kerning.get(HORIZONTAL_KERNING_TYPE).unwrap();
        // No Adieresis, so group members prune.
        let glyph_order: GlyphOrder = ["A", "Aacute", "T", "V"]
            .iter()
            .map(GlyphName::new)
            .collect();
        let loc = NormalizedLocation::for_pos(&[("wght", 0.0)]);

        // light-condensed is the first kerning source.
        let light = to_ir_kerning_instance(kerning, 0, &loc, &glyph_order);
        let glyph_t = KernSide::Glyph(GlyphName::new("T"));
        let glyph_a = KernSide::Glyph(GlyphName::new("A"));
        let group_a2 = KernSide::Group(KernGroup::Side2("A".into()));
        assert_eq!(
            Some(-75.0),
            light
                .kerns
                .get(&(glyph_t.clone(), group_a2.clone()))
                .map(|v| v.0)
        );
        // (T, A) has no value at light-condensed, only at bold-condensed.
        assert!(
            !light
                .kerns
                .contains_key(&(glyph_t.clone(), glyph_a.clone()))
        );
        let bold = to_ir_kerning_instance(kerning, 1, &loc, &glyph_order);
        assert_eq!(
            Some(-65.0),
            bold.kerns.get(&(glyph_t, glyph_a)).map(|v| v.0)
        );

        // Groups convert with members pruned to the glyph order.
        assert_eq!(
            Some(&["A", "Aacute"].iter().map(GlyphName::new).collect()),
            light.groups.get(&KernGroup::Side1("A".into()))
        );
    }

    #[test]
    fn kerning_locations_skip_sparse_empty_and_unknown_sources() {
        let mut font_data = Font::load(&testdata_dir().join("vertical.fontra")).unwrap();
        // regular kerns. medium is sparse, bold has no values, and ghost
        // does not exist.
        font_data.kerning.insert(
            HORIZONTAL_KERNING_TYPE.to_string(),
            Kerning {
                groups_side1: Default::default(),
                groups_side2: Default::default(),
                source_identifiers: vec![
                    "regular".into(),
                    "medium".into(),
                    "bold".into(),
                    "ghost".into(),
                ],
                values: HashMap::from([(
                    "vbase".into(),
                    HashMap::from([(
                        "vcomp".into(),
                        vec![Some(-10.0), Some(5.0), None, Some(1.0)],
                    )]),
                )]),
            },
        );
        let static_metadata = to_ir_static_metadata(&font_data, false).unwrap();
        let locations = to_ir_kerning_locations(
            &static_metadata,
            &font_data,
            font_data.kerning.get(HORIZONTAL_KERNING_TYPE),
        );
        assert_eq!(
            vec![static_metadata.default_location().clone()],
            locations.locations.into_iter().collect::<Vec<_>>()
        );
    }

    #[test]
    fn variable_composite_glyph_to_ir() {
        let font_data = Font::load(&testdata_dir().join("component.fontra")).unwrap();
        let static_metadata = to_ir_static_metadata(&font_data, false).unwrap();
        let convert = |name: &str| {
            let g = font_data.glyphs.get(&GlyphName::new(name)).unwrap();
            to_ir_glyph(
                &static_metadata,
                &font_data,
                Default::default(),
                g,
                &mut AnchorBuilder::new(g.name.clone()),
            )
            .unwrap()
        };
        // A deep-component glyph with glyph-local axes.
        assert!(!convert("VG_4E00_00").axes().is_empty());
        // The composite with variable components.
        let uni4e00 = convert("uni4E00");
        let default = uni4e00.default_instance();
        assert!(!default.components.is_empty());
        assert!(
            default
                .components
                .iter()
                .all(|c| c.is_variable() && c.base == GlyphName::new("VG_4E00_00"))
        );
    }

    #[test]
    fn glyph_axis_with_a_font_axis_name_maps_to_its_tag() {
        // R.alt redefines the weight and width font axes as glyph axes with a
        // 0..1 range. Source values map to the font tags, normalized against
        // the glyph axis range, and no local axes are synthesized.
        let font_data = Font::load(&testdata_dir().join("MutatorSans.fontra")).unwrap();
        let axis = |name: &str, tag: &[u8; 4], min: f64, default: f64, max: f64| {
            let (min, default, max) = (
                UserCoord::new(min),
                UserCoord::new(default),
                UserCoord::new(max),
            );
            Axis {
                tag: Tag::new(tag),
                name: name.into(),
                hidden: false,
                min,
                default,
                max,
                converter: CoordConverter::unmapped(min, default, max),
                localized_names: Default::default(),
            }
        };
        let static_metadata = StaticMetadata::new(
            1000,
            Default::default(),
            vec![
                axis("weight", b"wght", 100.0, 100.0, 900.0),
                axis("width", b"wdth", 0.0, 0.0, 1000.0),
            ],
            Default::default(),
            Default::default(),
            Default::default(),
            Default::default(),
            None,
            false,
        )
        .unwrap();
        let fontra_glyph = font_data.glyphs.get(&GlyphName::new("R.alt")).unwrap();
        let glyph = to_ir_glyph(
            &static_metadata,
            &font_data,
            Default::default(),
            fontra_glyph,
            &mut AnchorBuilder::new(fontra_glyph.name.clone()),
        )
        .unwrap();
        assert!(glyph.axes().is_empty());
        let mut corners: Vec<(f64, f64)> = glyph
            .sources()
            .keys()
            .map(|loc| {
                (
                    loc.get(Tag::new(b"wght")).unwrap().to_f64(),
                    loc.get(Tag::new(b"wdth")).unwrap().to_f64(),
                )
            })
            .collect();
        corners.sort_by(|a, b| a.partial_cmp(b).unwrap());
        assert_eq!(
            vec![(0.0, 0.0), (0.0, 1.0), (1.0, 0.0), (1.0, 1.0)],
            corners
        );
    }

    #[test]
    fn a_glyph_axis_with_min_above_max_is_an_error() {
        let mut font_data = Font::load(&testdata_dir().join("2glyphs.fontra")).unwrap();
        let glyph = font_data.glyphs.get_mut(&GlyphName::new("u20089")).unwrap();
        glyph.axes.push(GlyphAxis {
            name: "depth".to_string(),
            min_value: 50.0,
            default_value: 0.0,
            max_value: -10.0,
        });
        assert!(matches!(
            to_ir_static_metadata(&font_data, false),
            Err(Error::InconsistentAxisDefinitions(_))
        ));
    }

    #[test]
    fn responds_to_global_axes_resolves_location_base() {
        let font_data = Font::load(&testdata_dir().join("MutatorSans.fontra")).unwrap();
        let mut responds_to_global_axes_cache = HashMap::new();
        // A's sources take their locations from four font sources through
        // locationBase alone.
        let name: GlyphName = "A".into();
        assert!(responds_to_global_axes(
            &name,
            &font_data,
            &mut responds_to_global_axes_cache
        ));
    }

    #[test]
    fn responds_to_global_axes_ignores_glyph_local_variation() {
        let font_data = Font::load(&testdata_dir().join("component.fontra")).unwrap();
        let mut responds_to_global_axes_cache = HashMap::new();
        let deep: GlyphName = "VG_4E00_00".into();
        assert!(!responds_to_global_axes(
            &deep,
            &font_data,
            &mut responds_to_global_axes_cache
        ));
        let root: GlyphName = "uni4E00".into();
        assert!(responds_to_global_axes(
            &root,
            &font_data,
            &mut responds_to_global_axes_cache
        ));
    }

    #[test]
    fn anchors_of_raqq_kashida() {
        let font_data = Font::load(&testdata_dir().join("Raqq.fontra")).unwrap();
        let static_metadata = to_ir_static_metadata(&font_data, false).unwrap();
        let name = GlyphName::new("kashida-ar");
        let fontra_glyph = font_data.glyphs.get(&name).unwrap();
        let mut anchors = AnchorBuilder::new(name);
        to_ir_glyph(
            &static_metadata,
            &font_data,
            Default::default(),
            fontra_glyph,
            &mut anchors,
        )
        .unwrap();
        let anchors = anchors.build().unwrap();
        assert_eq!(
            vec!["entry", "exit", "kasra"],
            anchors
                .anchors
                .iter()
                .map(|a| a.original_name.as_str())
                .collect::<Vec<_>>()
        );
        let entry = &anchors.anchors[0];
        assert!(entry.is_cursive());
        assert_eq!(kurbo::Point::new(100.0, 0.0), entry.default_pos());
        let kasra = &anchors.anchors[2];
        assert!(!kasra.is_mark());
        assert_eq!(kurbo::Point::new(50.0, -54.0), kasra.default_pos());
    }

    #[test]
    fn variable_anchor_positions_of_raqq_space() {
        let font_data = Font::load(&testdata_dir().join("Raqq.fontra")).unwrap();
        let static_metadata = to_ir_static_metadata(&font_data, false).unwrap();
        let name = GlyphName::new("space");
        let fontra_glyph = font_data.glyphs.get(&name).unwrap();
        let mut anchors = AnchorBuilder::new(name);
        to_ir_glyph(
            &static_metadata,
            &font_data,
            Default::default(),
            fontra_glyph,
            &mut anchors,
        )
        .unwrap();
        let anchors = anchors.build().unwrap();
        let alefabove = &anchors.anchors[0];
        assert_eq!("alefabove", alefabove.original_name);
        let mut positions: Vec<(f64, f64, f64)> = alefabove
            .positions
            .iter()
            .map(|(loc, p)| (loc.get(Tag::new(b"SPAC")).unwrap().to_f64(), p.x, p.y))
            .collect();
        positions.sort_by(|a, b| a.partial_cmp(b).unwrap());
        assert_eq!(
            vec![(-1.0, 0.0, 0.0), (0.0, 200.0, 0.0), (1.0, 250.0, 0.0)],
            positions
        );
    }

    #[test]
    fn unnamed_anchors_are_skipped() {
        let mut font_data = Font::load(&testdata_dir().join("Raqq.fontra")).unwrap();
        let static_metadata = to_ir_static_metadata(&font_data, false).unwrap();
        let name = GlyphName::new("space");
        for layer in font_data.glyphs.get_mut(&name).unwrap().layers.values_mut() {
            for anchor in layer.glyph.anchors.iter_mut() {
                anchor.name = None;
            }
        }
        let fontra_glyph = font_data.glyphs.get(&name).unwrap();
        let mut anchors = AnchorBuilder::new(name);
        to_ir_glyph(
            &static_metadata,
            &font_data,
            Default::default(),
            fontra_glyph,
            &mut anchors,
        )
        .unwrap();
        assert!(anchors.build().unwrap().anchors.is_empty());
    }

    #[test]
    fn glyph_axis_values_normalize_against_symmetrized_range() {
        // (-50, 0, 100): the shorter min side extends to -100, so a
        // below-default value normalizes against the longer side and
        // out-of-range values clamp to the extended range.
        let axis = GlyphAxis {
            name: "depth".into(),
            min_value: -50.0,
            default_value: 0.0,
            max_value: 100.0,
        };
        assert_eq!(
            vec![-1.0, -0.25, 0.0, 0.5, 1.0],
            [-120.0, -25.0, 0.0, 50.0, 100.0]
                .map(|v| normalize_glyph_axis_value(v, &axis).to_f64())
                .to_vec(),
        );
    }
}
