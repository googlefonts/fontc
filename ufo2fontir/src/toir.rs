use std::{collections::HashMap, path::PathBuf, str::FromStr};

use fontdrasil::{
    coords::{CoordConverter, DesignCoord, DesignLocation, NormalizedLocation, UserCoord},
    types::GlyphName,
};
use fontir::{
    error::WorkError,
    ir::{self, AnchorBuilder, GlyphPathBuilder},
};
use kurbo::{Affine, BezPath};
use log::trace;
use norad::designspace::{self, Dimension};
use write_fonts::types::Tag;

pub(crate) fn to_design_location(
    tags_by_name: &HashMap<&str, Tag>,
    loc: &[Dimension],
) -> DesignLocation {
    // TODO: what if Dimension uses uservalue? - new in DS5.0
    loc.iter()
        .map(|d| {
            (
                *tags_by_name.get(d.name.as_str()).unwrap(),
                DesignCoord::new(d.xvalue.unwrap()),
            )
        })
        .collect()
}

fn to_ir_contour(glyph_name: GlyphName, contour: &norad::Contour) -> Result<BezPath, WorkError> {
    if contour.points.is_empty() {
        return Ok(BezPath::new());
    }

    let mut path_builder = GlyphPathBuilder::new(glyph_name.clone(), contour.points.len());

    // Walk through the remaining points, accumulating off-curve points until we see an on-curve
    for node in contour.points.iter() {
        match node.typ {
            norad::PointType::Move => path_builder.move_to((node.x, node.y))?,
            norad::PointType::Line => path_builder.line_to((node.x, node.y))?,
            norad::PointType::QCurve => path_builder.qcurve_to((node.x, node.y))?,
            norad::PointType::Curve => path_builder.curve_to((node.x, node.y))?,
            norad::PointType::OffCurve => path_builder.offcurve((node.x, node.y))?,
        }
    }

    let path = path_builder.build()?;
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

fn to_ir_glyph_instance(glyph: &norad::Glyph) -> Result<ir::GlyphInstance, WorkError> {
    let mut contours = Vec::new();
    for contour in glyph.contours.iter() {
        contours.push(to_ir_contour(glyph.name().as_str().into(), contour)?);
    }
    Ok(ir::GlyphInstance {
        width: glyph.width,
        height: Some(glyph.height),
        contours,
        components: glyph.components.iter().map(to_ir_component).collect(),
    })
}

/// Create a map from source filename (e.g. x.ufo) => normalized location
pub fn master_locations(
    axes: &[fontdrasil::types::Axis],
    sources: &[designspace::Source],
) -> HashMap<String, NormalizedLocation> {
    let tags_by_name: HashMap<_, _> = axes.iter().map(|a| (a.name.as_str(), a.tag)).collect();
    let axes = axes.iter().map(|a| (a.tag, a)).collect();
    sources
        .iter()
        .map(|s| {
            (
                s.name.clone().unwrap(),
                to_design_location(&tags_by_name, &s.location).to_normalized(&axes),
            )
        })
        .collect()
}

pub fn to_ir_axes(axes: &[designspace::Axis]) -> Result<Vec<fontdrasil::types::Axis>, WorkError> {
    axes.iter().map(to_ir_axis).collect()
}

pub fn to_ir_axis(axis: &designspace::Axis) -> Result<fontdrasil::types::Axis, WorkError> {
    let tag = Tag::from_str(&axis.tag).map_err(WorkError::InvalidTag)?;

    // <https://fonttools.readthedocs.io/en/latest/designspaceLib/xml.html#axis-element>
    let min = UserCoord::new(axis.minimum.unwrap());
    let default = UserCoord::new(axis.default);
    let max = UserCoord::new(axis.maximum.unwrap());

    // <https://fonttools.readthedocs.io/en/latest/designspaceLib/xml.html#map-element>
    let converter = if let Some(mappings) = &axis.map {
        let examples: Vec<_> = mappings
            .iter()
            .map(|map| (UserCoord::new(map.input), DesignCoord::new(map.output)))
            .collect();
        // # mappings is generally small, repeated linear probing is fine
        let default_idx = examples
            .iter()
            .position(|(u, _)| *u == default)
            .ok_or_else(|| WorkError::AxisMustMapDefault(tag))?;
        examples
            .iter()
            .position(|(u, _)| *u == min)
            .ok_or_else(|| WorkError::AxisMustMapMin(tag))?;
        examples
            .iter()
            .position(|(u, _)| *u == max)
            .ok_or_else(|| WorkError::AxisMustMapMax(tag))?;
        CoordConverter::new(examples, default_idx)
    } else {
        CoordConverter::unmapped(min, default, max)
    };
    Ok(fontdrasil::types::Axis {
        name: axis.name.clone(),
        tag,
        hidden: axis.hidden,
        min,
        default,
        max,
        converter,
    })
}

pub fn to_ir_glyph(
    glyph_name: GlyphName,
    emit_to_binary: bool,
    glif_files: &HashMap<&PathBuf, Vec<NormalizedLocation>>,
    anchors: &mut AnchorBuilder,
) -> Result<ir::Glyph, WorkError> {
    let mut glyph = ir::GlyphBuilder::new(glyph_name.clone());
    glyph.emit_to_binary = emit_to_binary;
    for (glif_file, locations) in glif_files {
        let norad_glyph =
            norad::Glyph::load(glif_file).map_err(|e| WorkError::InvalidSourceGlyph {
                glyph_name: glyph.name.clone(),
                message: format!("glif load failed due to {e}"),
            })?;
        norad_glyph.codepoints.iter().for_each(|cp| {
            glyph.codepoints.insert(cp as u32);
        });
        for location in locations {
            glyph.try_add_source(location, to_ir_glyph_instance(&norad_glyph)?)?;

            for anchor in norad_glyph.anchors.iter() {
                anchors.add(
                    anchor.name.as_ref().unwrap().as_str().into(),
                    location.clone(),
                    (anchor.x, anchor.y).into(),
                )?;
            }
        }
    }
    glyph.build()
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
        ContourPoint::new(x, y, typ, false, None, None, None)
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
        let contour = norad::Contour::new(points, None, None);
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
        let contour = norad::Contour::new(points, None, None);
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
