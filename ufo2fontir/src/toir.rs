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
use write_fonts::types::Tag;

use crate::source::vertical_origin;

pub(crate) fn to_design_location(
    tags_by_name: &HashMap<&str, Tag>,
    loc: &[Dimension],
) -> DesignLocation {
    // TODO: what if Dimension uses uservalue? - new in DS5.0
    loc.iter()
        .map(|d| {
            (
                *tags_by_name.get(d.name.as_str()).unwrap(),
                DesignCoord::new(d.xvalue.unwrap() as f64),
            )
        })
        .collect()
}

fn to_ir_contour(glyph_name: GlyphName, contour: &norad::Contour) -> Result<BezPath, BadGlyph> {
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

    let path = path_builder
        .build()
        .map_err(|e| BadGlyph::new(glyph_name.clone(), e))?;
    trace!(
        "Built a {} entry path for {}",
        path.elements().len(),
        glyph_name,
    );
    Ok(path)
}

fn to_ir_component(component: &norad::Component) -> ir::Component {
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
    }
}

fn to_ir_glyph_instance(glyph: &norad::Glyph, path: &PathBuf) -> Result<ir::GlyphInstance, Error> {
    let mut contours = Vec::new();
    for contour in glyph.contours.iter() {
        contours.push(to_ir_contour(glyph.name().as_str().into(), contour)?);
    }

    let vertical_origin = vertical_origin(glyph, path)?;

    Ok(ir::GlyphInstance {
        width: glyph.width,
        height: Some(glyph.height),
        vertical_origin,
        contours,
        components: glyph.components.iter().map(to_ir_component).collect(),
    })
}

/// Create a map from source filename (e.g. x.ufo) => normalized location
pub fn master_locations<'a>(
    axes: &fontdrasil::types::Axes,
    sources: impl IntoIterator<Item = &'a designspace::Source>,
) -> HashMap<String, NormalizedLocation> {
    let tags_by_name: HashMap<_, _> = axes.iter().map(|a| (a.name.as_str(), a.tag)).collect();
    sources
        .into_iter()
        .map(|s| {
            (
                s.name.clone().unwrap(),
                to_design_location(&tags_by_name, &s.location).to_normalized(axes),
            )
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
        CoordConverter::new(examples, default_idx)
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

pub fn to_ir_glyph(
    glyph_name: GlyphName,
    emit_to_binary: bool,
    glif_files: &HashMap<&PathBuf, Vec<NormalizedLocation>>,
    anchors: &mut AnchorBuilder,
) -> Result<ir::Glyph, Error> {
    let mut glyph = ir::GlyphBuilder::new(glyph_name.clone());
    glyph.emit_to_binary = emit_to_binary;
    for (glif_file, locations) in glif_files {
        let norad_glyph =
            norad::Glyph::load(glif_file).map_err(|e| BadSource::custom(glif_file, e))?;

        norad_glyph.codepoints.iter().for_each(|cp| {
            glyph.codepoints.insert(cp as u32);
        });
        for location in locations {
            glyph.try_add_source(location, to_ir_glyph_instance(&norad_glyph, glif_file)?)?;

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
        let bez = to_ir_contour("test".into(), &contour).unwrap();
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
        let bez = to_ir_contour("test".into(), &contour).unwrap();
        assert_eq!("M32,32 C64,64 64,0 32,32 Z", bez.to_svg());
    }

    #[test]
    pub fn captures_codepoints() {
        let mut norm_loc = NormalizedLocation::new();
        norm_loc.insert(Tag::new(b"wght"), NormalizedCoord::new(0.0));
        let mut anchors = AnchorBuilder::new("bar".into());
        let glyph = to_ir_glyph(
            "bar".into(),
            true,
            &HashMap::from([(
                &testdata_dir().join("WghtVar-Regular.ufo/glyphs/bar.glif"),
                vec![norm_loc],
            )]),
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
            to_ir_component(&c).transform,
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
            to_ir_component(&c).transform,
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
            to_ir_component(&c).transform,
            Affine::new([0.4366, -0.4366, 0.4415, 0.4425, 282.0, 5.0])
        );
    }
}
