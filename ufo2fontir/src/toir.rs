use std::{collections::HashMap, path::PathBuf, str::FromStr};

use fontdrasil::{
    coords::{CoordConverter, DesignCoord, DesignLocation, NormalizedLocation, UserCoord},
    types::GlyphName,
};
use fontir::{
    error::{BadGlyph, BadSource, Error},
    ir::{self, AnchorBuilder, GlyphPathBuilder},
};
use kurbo::{Affine, BezPath};
use log::trace;
use norad::designspace::{self, Dimension};
use smol_str::SmolStr;
use write_fonts::types::Tag;

use crate::source::vertical_origin;

/// Key for glyphsLib's component info storage in UFO glyph lib.
///
/// See: <https://github.com/googlefonts/glyphsLib/blob/de5b4e34/Lib/glyphsLib/builder/constants.py#L27>
const COMPONENT_INFO_KEY: &str = "com.schriftgestaltung.Glyphs.ComponentInfo";

/// Convert a source location to design coordinates.
///
/// Source locations must use `xvalue`; `uservalue` is only meaningful for
/// instances, see [`to_instance_design_location`].
pub(crate) fn to_design_location(
    tags_by_name: &HashMap<&str, Tag>,
    loc: &[Dimension],
) -> Result<DesignLocation, Error> {
    loc.iter()
        .filter_map(|d| {
            let Some(tag) = tags_by_name.get(d.name.as_str()) else {
                // warn and skip dimensions with unknown axis names:
                // https://github.com/fonttools/fonttools/blob/8e5c1bf7/Lib/fontTools/designspaceLib/__init__.py#L2424
                log::warn!("Location with undefined axis: {:?}, skipping", d.name);
                return None;
            };
            Some(match d.xvalue {
                Some(x) => Ok((*tag, DesignCoord::new(x as f64))),
                None => Err(Error::InvalidEntry(
                    "source location",
                    format!(
                        "dimension {:?} has no xvalue{}",
                        d.name,
                        if d.uservalue.is_some() {
                            " (uservalue is only supported for instances)"
                        } else {
                            ""
                        }
                    ),
                )),
            })
        })
        .collect()
}

/// Convert an instance location to design coordinates.
///
/// Mirrors fontTools' `InstanceDescriptor.getFullDesignLocation`: every axis
/// starts at its default; each dimension then overrides its axis with the
/// explicit design value (`xvalue`) if present, else the user value
/// (`uservalue`) mapped forward through the axis map. A dimension with neither
/// is an error, as in designspaceLib. Dimensions naming an unknown axis are
/// skipped with a warning, matching [`to_design_location`].
///
/// <https://github.com/fonttools/fonttools/blob/8e5c1bf7/Lib/fontTools/designspaceLib/__init__.py#L819-L850>
pub(crate) fn to_instance_design_location(
    axes: &fontdrasil::types::Axes,
    tags_by_name: &HashMap<&str, Tag>,
    loc: &[Dimension],
) -> Result<DesignLocation, Error> {
    let mut result: DesignLocation = axes
        .iter()
        .map(|a| (a.tag, a.default.convert(&a.converter)))
        .collect();
    for d in loc {
        let Some(tag) = tags_by_name.get(d.name.as_str()) else {
            log::warn!("Location with undefined axis: {:?}, skipping", d.name);
            continue;
        };
        let coord = match (d.xvalue, d.uservalue) {
            (Some(x), _) => DesignCoord::new(x as f64),
            (None, Some(u)) => {
                let axis = axes
                    .get(tag)
                    .ok_or_else(|| Error::NoEntryInAxes(tag.to_string()))?;
                UserCoord::new(u as f64).convert(&axis.converter)
            }
            (None, None) => {
                return Err(Error::InvalidEntry(
                    "instance location",
                    format!(
                        "dimension {:?} must have exactly one of xvalue or uservalue",
                        d.name
                    ),
                ));
            }
        };
        result.insert(*tag, coord);
    }
    Ok(result)
}

/// Whether `loc` lies within every axis' user-space range (inclusive).
///
/// fontmake drops named instances located outside the variable font's axis ranges
/// (`designspaceLib.split._extractSubSpace`); such instances can't be reached by a
/// variable font and would only clamp to the nearest edge. Axes absent from `loc`
/// are at their default and therefore in range.
pub(crate) fn within_axis_ranges(
    axes: &fontdrasil::types::Axes,
    loc: &fontdrasil::coords::UserLocation,
) -> bool {
    axes.iter().all(|axis| {
        loc.get(axis.tag)
            .is_none_or(|coord| axis.min <= coord && coord <= axis.max)
    })
}

fn to_ir_contour(
    glyph_name: GlyphName,
    contour: &norad::Contour,
    erase_open_corners: bool,
) -> Result<BezPath, BadGlyph> {
    if contour.points.is_empty() {
        return Ok(BezPath::new());
    }

    let mut path_builder = GlyphPathBuilder::new(contour.points.len());

    // Walk through the remaining points, accumulating off-curve points until we see an on-curve
    for node in contour.points.iter() {
        match node.typ {
            norad::PointType::Move => path_builder.move_to((node.x, node.y)),
            norad::PointType::Line => path_builder.line_to((node.x, node.y)),
            norad::PointType::QCurve => path_builder.qcurve_to((node.x, node.y)),
            norad::PointType::Curve => path_builder.curve_to((node.x, node.y)),
            norad::PointType::OffCurve => path_builder.offcurve((node.x, node.y)),
        }
        .map_err(|e| BadGlyph::new(glyph_name.clone(), e))?;
    }

    if erase_open_corners
        && path_builder
            .erase_open_corners()
            .map_err(|e| BadGlyph::new(&glyph_name, e))?
    {
        log::debug!("erased open corners on {glyph_name}");
    }

    let path = path_builder
        .build()
        .map_err(|e| BadGlyph::new(glyph_name.clone(), e))?;
    trace!(
        "Built a {} entry path for {glyph_name}",
        path.elements().len(),
    );
    Ok(path)
}

fn to_ir_component(component: &norad::Component, anchor: Option<SmolStr>) -> ir::Component {
    ir::Component {
        base: component.base.as_str().into(),
        transform: Affine::new([
            component.transform.x_scale,
            // For the 2nd and 3rd field of its 2x3 affine transformation, norad uses
            // the same labels as fonttools' Transform, respectively `xy` and `yx`.
            // Elsewhere (e.g. FreeType, Cairo or read-fonts Transform) these labels are
            // inverted, but their meaning is still the same: i.e. the "y component of
            // the î basis vector", and "x component of the ĵ basis vector".
            // See https://github.com/googlefonts/fontc/pull/721
            component.transform.xy_scale,
            component.transform.yx_scale,
            component.transform.y_scale,
            component.transform.x_offset,
            component.transform.y_offset,
        ]),
        anchor,
    }
}

/// Extract component anchors from glyphsLib's `ComponentInfo` in glyph lib.
///
/// Returns a map from component index to anchor name.
fn component_anchors(glyph: &norad::Glyph) -> HashMap<usize, SmolStr> {
    // ComponentInfo is an array of dictionaries with "index" and "anchor" keys.
    // See <https://github.com/googlefonts/glyphsLib/blob/de5b4e34/Lib/glyphsLib/builder/components.py#L85-L93>
    let Some(plist::Value::Array(info_list)) = glyph.lib.get(COMPONENT_INFO_KEY) else {
        return HashMap::new();
    };

    info_list
        .iter()
        .filter_map(|entry| {
            let dict = entry.as_dictionary()?;
            let index = dict.get("index")?.as_unsigned_integer()? as usize;
            let anchor = dict.get("anchor")?.as_string()?;
            Some((index, SmolStr::from(anchor)))
        })
        .collect()
}

fn to_ir_glyph_instance(
    glyph: &norad::Glyph,
    path: &PathBuf,
    erase_open_corners: bool,
) -> Result<ir::GlyphInstance, Error> {
    let mut contours = Vec::new();
    for contour in glyph.contours.iter() {
        contours.push(to_ir_contour(
            glyph.name().as_str().into(),
            contour,
            erase_open_corners,
        )?);
    }

    let vertical_origin = vertical_origin(glyph, path)?;

    // Look up explicit component anchors from glyphsLib's ComponentInfo
    let anchors = component_anchors(glyph);
    let components = glyph
        .components
        .iter()
        .enumerate()
        .map(|(i, comp)| to_ir_component(comp, anchors.get(&i).cloned()))
        .collect();

    Ok(ir::GlyphInstance {
        width: glyph.width,
        height: Some(glyph.height),
        vertical_origin,
        contours,
        components,
    })
}

/// Create a map from source filename (e.g. x.ufo) => normalized location
pub fn master_locations<'a>(
    axes: &fontdrasil::types::Axes,
    sources: impl IntoIterator<Item = &'a designspace::Source>,
) -> Result<HashMap<String, NormalizedLocation>, Error> {
    let tags_by_name: HashMap<_, _> = axes.iter().map(|a| (a.name.as_str(), a.tag)).collect();
    sources
        .into_iter()
        .map(|s| {
            Ok((
                s.name.clone().unwrap(),
                to_design_location(&tags_by_name, &s.location)?.to_normalized(axes)?,
            ))
        })
        .collect()
}

pub fn to_ir_axes(axes: &[designspace::Axis]) -> Result<fontdrasil::types::Axes, Error> {
    axes.iter().map(to_ir_axis).collect()
}

pub fn to_ir_axis(axis: &designspace::Axis) -> Result<fontdrasil::types::Axis, Error> {
    let tag = Tag::from_str(&axis.tag).map_err(|cause| Error::InvalidTag {
        raw_tag: axis.tag.clone(),
        cause,
    })?;

    // <https://fonttools.readthedocs.io/en/latest/designspaceLib/xml.html#axis-element>
    let min = UserCoord::new(axis.minimum.unwrap() as f64);
    let default = UserCoord::new(axis.default as f64);
    let max = UserCoord::new(axis.maximum.unwrap() as f64);

    // <https://fonttools.readthedocs.io/en/latest/designspaceLib/xml.html#map-element>
    let converter = if let Some(mappings) = &axis.map {
        let examples: Vec<_> = mappings
            .iter()
            .map(|map| {
                (
                    UserCoord::new(map.input as f64),
                    DesignCoord::new(map.output as f64),
                )
            })
            .collect();

        // make sure we have min/max/default mappings:
        let has_min_max =
            examples.iter().any(|(u, _)| *u == min) && examples.iter().any(|(u, _)| *u == max);

        // # mappings is generally small, repeated linear probing is fine
        let default_idx = examples
            .iter()
            .position(|(u, _)| *u == default)
            // error if we don't have all of min/max/default
            .filter(|_| has_min_max)
            .ok_or(Error::MissingAxisMapping(tag))?;
        CoordConverter::new(examples, default_idx)?
    } else {
        CoordConverter::unmapped(min, default, max)
    };
    let localized_names = axis
        .label_names
        .iter()
        .map(|ln| (ln.language.clone(), ln.string.clone()))
        .collect();

    Ok(fontdrasil::types::Axis {
        name: axis.name.clone(),
        tag,
        hidden: axis.hidden,
        min,
        default,
        max,
        converter,
        localized_names,
    })
}

/// Invariant: the default location is always first in the glif_files list.
pub fn to_ir_glyph(
    glyph_name: GlyphName,
    emit_to_binary: bool,
    erase_open_corners: bool,
    glif_files: &[(Vec<NormalizedLocation>, &PathBuf)],
    anchors: &mut AnchorBuilder,
) -> Result<ir::Glyph, Error> {
    let mut glyph = ir::GlyphBuilder::new(glyph_name.clone());
    glyph.emit_to_binary = emit_to_binary;

    // We stash the codepoints from the default location so we can warn
    // if any other instances have different codepoints
    let mut default_loc_codepoints = None;
    for (locations, glif_file) in glif_files.iter() {
        let mut norad_glyph =
            norad::Glyph::load(glif_file).map_err(|e| BadSource::custom(glif_file, e))?;

        for location in locations {
            glyph.try_add_source(
                location,
                to_ir_glyph_instance(&norad_glyph, glif_file, erase_open_corners)?,
            )?;

            // we only care about anchors from exportable glyphs
            // https://github.com/googlefonts/fontc/issues/1397
            if emit_to_binary {
                for anchor in norad_glyph.anchors.iter() {
                    anchors.add(
                        anchor.name.as_ref().unwrap().as_str().into(),
                        location.clone(),
                        (anchor.x, anchor.y).into(),
                    )?;
                }
            }
        }
        match default_loc_codepoints.as_ref() {
            None => {
                glyph.codepoints = norad_glyph.codepoints.iter().map(|cp| cp as u32).collect();
                default_loc_codepoints = Some(std::mem::take(&mut norad_glyph.codepoints));
            }
            Some(cps) if !norad_glyph.codepoints.is_empty() && cps != &norad_glyph.codepoints => {
                log::warn!(
                    "Glyph '{glyph_name}' codepoints differ between instances. Default: '{cps:?}', {glif_file:?}: {:?}",
                    norad_glyph.codepoints
                );
            }
            Some(_) => (),
        }
    }
    glyph.build().map_err(Into::into)
}

#[cfg(test)]
mod tests {
    use std::{
        collections::{HashMap, HashSet},
        path::{Path, PathBuf},
    };

    use fontdrasil::coords::{NormalizedCoord, NormalizedLocation};
    use fontir::ir::AnchorBuilder;
    use norad::{AffineTransform, Component, ContourPoint, Name};

    use super::*;

    fn testdata_dir() -> PathBuf {
        let dir = Path::new("../resources/testdata");
        assert!(dir.is_dir());
        dir.to_path_buf()
    }

    fn contour_point(x: f64, y: f64, typ: norad::PointType) -> ContourPoint {
        ContourPoint::new(x, y, typ, false, None, None)
    }

    // https://unifiedfontobject.org/versions/ufo3/glyphs/glif/#point-types
    // observes if a contour does *not* start with a move it is cyclic.
    // real fonts use this, such as to open with a curve command and end with
    // dangling offcurves
    #[test]
    fn closed_contour_box() {
        let points = vec![
            contour_point(1.0, 1.0, norad::PointType::Line),
            contour_point(9.0, 1.0, norad::PointType::Line),
            contour_point(9.0, 2.0, norad::PointType::Line),
            contour_point(1.0, 2.0, norad::PointType::Line),
        ];
        let contour = norad::Contour::new(points, None);
        let bez = to_ir_contour("test".into(), &contour, false).unwrap();
        assert_eq!("M1,1 L9,1 L9,2 L1,2 L1,1 Z", bez.to_svg());
    }

    // https://unifiedfontobject.org/versions/ufo3/glyphs/glif/#point-types
    // observes if a contour does *not* start with a move it is cyclic.
    // real fonts use this, such as to open with a curve command and end with
    // dangling offcurves
    #[test]
    fn closed_contour_single_cubic() {
        // Cubic teardrop
        let points = vec![
            contour_point(32.0, 32.0, norad::PointType::Curve),
            contour_point(64.0, 64.0, norad::PointType::OffCurve),
            contour_point(64.0, 0.0, norad::PointType::OffCurve),
        ];
        let contour = norad::Contour::new(points, None);
        let bez = to_ir_contour("test".into(), &contour, false).unwrap();
        assert_eq!("M32,32 C64,64 64,0 32,32 Z", bez.to_svg());
    }

    #[test]
    pub fn captures_codepoints() {
        let mut norm_loc = NormalizedLocation::new();
        norm_loc.insert(Tag::new(b"wght"), NormalizedCoord::new(0.0));
        let mut anchors = AnchorBuilder::new("bar".into());
        let glif_path = testdata_dir().join("WghtVar-Regular.ufo/glyphs/bar.glif");
        let glyph = to_ir_glyph(
            "bar".into(),
            true,
            false,
            &[(vec![norm_loc], &glif_path)],
            &mut anchors,
        )
        .unwrap();
        assert_eq!(HashSet::from([0x007C]), glyph.codepoints);
    }

    #[test]
    pub fn component_transforms() {
        let mut c = Component::new(
            Name::new("A").unwrap(),
            AffineTransform {
                x_scale: 1.0,
                xy_scale: 0.0,
                yx_scale: 0.0,
                y_scale: 1.0,
                x_offset: 0.0,
                y_offset: 0.0,
            },
            None,
        );
        assert_eq!(
            to_ir_component(&c, None).transform,
            Affine::new([1.0, 0.0, 0.0, 1.0, 0.0, 0.0])
        );

        c.transform = AffineTransform {
            x_scale: 1.0,
            xy_scale: 0.0,
            yx_scale: 0.0,
            y_scale: 1.0,
            x_offset: 10.0,
            y_offset: 10.0,
        };
        assert_eq!(
            to_ir_component(&c, None).transform,
            Affine::new([1.0, 0.0, 0.0, 1.0, 10.0, 10.0])
        );

        // <component base="a" xScale="0.4366" xyScale="-0.4366" yScale="0.4425" yxScale="0.4415" xOffset="282" yOffset="5" identifier="5402E799"/>
        c.transform = AffineTransform {
            x_scale: 0.4366,
            xy_scale: -0.4366,
            yx_scale: 0.4415,
            y_scale: 0.4425,
            x_offset: 282.0,
            y_offset: 5.0,
        };
        // Switchy switchy!
        assert_eq!(
            to_ir_component(&c, None).transform,
            Affine::new([0.4366, -0.4366, 0.4415, 0.4425, 282.0, 5.0])
        );
    }

    // Tilt-Fonts TiltNeon[XROT,YROT].designspace uses axis tags ("XROT") instead of names
    #[test]
    fn to_design_location_skips_undefined_axis() {
        let tags_by_name: HashMap<&str, Tag> = HashMap::from([
            ("Rotation in X", Tag::new(b"XROT")),
            ("Rotation in Y", Tag::new(b"YROT")),
        ]);

        // Normal case: dimension names match axis names
        let loc = vec![
            Dimension {
                name: "Rotation in X".into(),
                xvalue: Some(10.0),
                yvalue: None,
                uservalue: None,
            },
            Dimension {
                name: "Rotation in Y".into(),
                xvalue: Some(20.0),
                yvalue: None,
                uservalue: None,
            },
        ];
        let result = to_design_location(&tags_by_name, &loc).unwrap();
        assert_eq!(result.get(Tag::new(b"XROT")), Some(DesignCoord::new(10.0)));
        assert_eq!(result.get(Tag::new(b"YROT")), Some(DesignCoord::new(20.0)));

        // Bug case: dimension uses axis tags instead of axis names
        let loc_with_tags = vec![
            Dimension {
                name: "XROT".into(),
                xvalue: Some(0.0),
                yvalue: None,
                uservalue: None,
            },
            Dimension {
                name: "YROT".into(),
                xvalue: Some(0.0),
                yvalue: None,
                uservalue: None,
            },
        ];
        let result = to_design_location(&tags_by_name, &loc_with_tags).unwrap();
        assert_eq!(result.iter().count(), 0, "undefined axes should be skipped");
    }

    #[test]
    fn to_design_location_rejects_missing_xvalue() {
        let tags_by_name: HashMap<&str, Tag> = HashMap::from([("Weight", Tag::new(b"wght"))]);
        // uservalue is only valid for instances
        let loc = vec![Dimension {
            name: "Weight".into(),
            xvalue: None,
            yvalue: None,
            uservalue: Some(700.0),
        }];
        let err = to_design_location(&tags_by_name, &loc).unwrap_err();
        assert!(
            matches!(err, Error::InvalidEntry("source location", _)),
            "{err:?}"
        );
    }

    fn wght_axis_with_map() -> fontdrasil::types::Axes {
        // user 300..700 maps to design 30..70, default 400 -> 40
        fontdrasil::types::Axes::new(vec![fontdrasil::types::Axis {
            name: "Weight".into(),
            tag: Tag::new(b"wght"),
            min: UserCoord::new(300.0),
            default: UserCoord::new(400.0),
            max: UserCoord::new(700.0),
            hidden: false,
            converter: CoordConverter::new(
                vec![
                    (UserCoord::new(300.0), DesignCoord::new(30.0)),
                    (UserCoord::new(400.0), DesignCoord::new(40.0)),
                    (UserCoord::new(700.0), DesignCoord::new(70.0)),
                ],
                1,
            )
            .unwrap(),
            localized_names: Default::default(),
        }])
    }

    fn dim(name: &str, xvalue: Option<f32>, uservalue: Option<f32>) -> Dimension {
        Dimension {
            name: name.into(),
            xvalue,
            yvalue: None,
            uservalue,
        }
    }

    // https://github.com/googlefonts/fontc/issues/1649
    #[test]
    fn instance_location_maps_uservalue_through_axis_map() {
        let axes = wght_axis_with_map();
        let tags_by_name = HashMap::from([("Weight", Tag::new(b"wght"))]);
        let wght = Tag::new(b"wght");

        // uservalue is mapped forward through the axis map
        let loc =
            to_instance_design_location(&axes, &tags_by_name, &[dim("Weight", None, Some(700.0))])
                .unwrap();
        assert_eq!(loc.get(wght), Some(DesignCoord::new(70.0)));

        // xvalue is taken as-is
        let loc =
            to_instance_design_location(&axes, &tags_by_name, &[dim("Weight", Some(55.0), None)])
                .unwrap();
        assert_eq!(loc.get(wght), Some(DesignCoord::new(55.0)));

        // an axis missing from the location gets the axis default (in design space)
        let loc = to_instance_design_location(&axes, &tags_by_name, &[]).unwrap();
        assert_eq!(loc.get(wght), Some(DesignCoord::new(40.0)));

        // a dimension with neither value is an error, as in fontTools
        let err = to_instance_design_location(&axes, &tags_by_name, &[dim("Weight", None, None)])
            .unwrap_err();
        assert!(
            matches!(err, Error::InvalidEntry("instance location", _)),
            "{err:?}"
        );
    }

    #[test]
    fn detects_instance_outside_axis_range() {
        let axes = wght_axis_with_map();
        let wght = Tag::new(b"wght");
        let at = |v: f64| fontdrasil::coords::UserLocation::from(vec![(wght, UserCoord::new(v))]);
        assert!(within_axis_ranges(&axes, &at(300.0)));
        assert!(within_axis_ranges(&axes, &at(700.0)));
        assert!(!within_axis_ranges(&axes, &at(2000.0)));
        assert!(!within_axis_ranges(&axes, &at(100.0)));
        // axes missing from the location are at their default
        assert!(within_axis_ranges(
            &axes,
            &fontdrasil::coords::UserLocation::new()
        ));
    }

    /// Test parsing component anchors from glyphsLib's ComponentInfo in glyph lib.
    ///
    /// This tests that UFO files exported by glyphsLib with explicit component
    /// anchors (e.g., for ligature attachment) are correctly parsed.
    #[test]
    fn component_anchors_from_lib() {
        use plist::Value;

        // Create a glyph with ComponentInfo in its lib
        let mut glyph = norad::Glyph::new("test");

        // Simulate glyphsLib's ComponentInfo structure:
        // [{"name": "aa", "index": 0}, {"name": "acutecomb", "index": 1, "anchor": "top_2"}]
        // See: <https://github.com/googlefonts/glyphsLib/blob/de5b4e34/Lib/glyphsLib/builder/components.py#L85-L93>
        let component_info = Value::Array(vec![
            Value::Dictionary(
                [
                    ("name".to_string(), Value::String("aa".to_string())),
                    ("index".to_string(), Value::Integer(0.into())),
                ]
                .into_iter()
                .collect(),
            ),
            Value::Dictionary(
                [
                    ("name".to_string(), Value::String("acutecomb".to_string())),
                    ("index".to_string(), Value::Integer(1.into())),
                    ("anchor".to_string(), Value::String("top_2".to_string())),
                ]
                .into_iter()
                .collect(),
            ),
        ]);
        glyph
            .lib
            .insert(COMPONENT_INFO_KEY.to_string(), component_info);

        let anchors = component_anchors(&glyph);

        // Only component at index 1 has an anchor
        assert_eq!(anchors.len(), 1);
        assert_eq!(anchors.get(&1), Some(&SmolStr::from("top_2")));
        assert_eq!(anchors.get(&0), None);
    }
}
