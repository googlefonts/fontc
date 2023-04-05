use std::path::PathBuf;
use std::{collections::HashMap, str::FromStr};

use font_types::Tag;
use fontdrasil::types::GlyphName;
use fontir::{
    coords::{CoordConverter, DesignCoord, DesignLocation, NormalizedLocation, UserCoord},
    error::WorkError,
    ir::{self, GlyphPathBuilder},
};
use kurbo::{Affine, BezPath};
use log::trace;
use norad::designspace::{self, Dimension};

pub(crate) fn to_design_location(loc: &[Dimension]) -> DesignLocation {
    // TODO: what if Dimension uses uservalue? - new in DS5.0
    loc.iter()
        .map(|d| (d.name.clone(), DesignCoord::new(d.xvalue.unwrap())))
        .collect()
}

fn to_ir_contour(glyph_name: GlyphName, contour: &norad::Contour) -> Result<BezPath, WorkError> {
    let mut path_builder = GlyphPathBuilder::new(glyph_name.clone());
    if contour.points.is_empty() {
        return Ok(path_builder.build());
    }

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

    // "A closed contour does not start with a move"
    // https://unifiedfontobject.org/versions/ufo3/glyphs/glif/#point-types
    if norad::PointType::Move != contour.points[0].typ {
        path_builder.close_path()?;
    }

    let path = path_builder.build();
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
            component.transform.yx_scale,
            component.transform.xy_scale,
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
    axes: &[ir::Axis],
    sources: &[designspace::Source],
) -> HashMap<String, NormalizedLocation> {
    let axes = axes.iter().map(|a| (&a.name, a)).collect();
    sources
        .iter()
        .map(|s| {
            (
                s.name.clone(),
                to_design_location(&s.location).to_normalized(&axes),
            )
        })
        .collect()
}

pub fn to_ir_axes(axes: &[designspace::Axis]) -> Result<Vec<ir::Axis>, WorkError> {
    axes.iter().map(to_ir_axis).collect()
}

pub fn to_ir_axis(axis: &designspace::Axis) -> Result<ir::Axis, WorkError> {
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
    Ok(ir::Axis {
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
    glif_files: &HashMap<&PathBuf, Vec<NormalizedLocation>>,
) -> Result<ir::Glyph, WorkError> {
    let mut glyph = ir::GlyphBuilder::new(glyph_name);
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
        }
    }
    glyph.try_into()
}

#[cfg(test)]
mod tests {
    use std::{
        collections::{HashMap, HashSet},
        path::{Path, PathBuf},
    };

    use fontir::coords::{NormalizedCoord, NormalizedLocation};
    use norad::ContourPoint;

    use super::{to_ir_contour, to_ir_glyph};

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
        norm_loc.set_pos("Weight", NormalizedCoord::new(0.0));
        let glyph = to_ir_glyph(
            "bar".into(),
            &HashMap::from([(
                &testdata_dir().join("WghtVar-Regular.ufo/glyphs/bar.glif"),
                vec![norm_loc],
            )]),
        )
        .unwrap();
        assert_eq!(HashSet::from([0x007C]), glyph.codepoints);
    }
}
