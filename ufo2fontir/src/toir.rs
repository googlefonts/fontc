use std::collections::HashMap;
use std::path::PathBuf;

use fontir::{
    coords::{CoordConverter, DesignCoord, DesignLocation, NormalizedLocation, UserCoord},
    ir,
};
use norad::designspace::{self, Dimension};

use crate::error::Error;

pub(crate) fn to_design_location(loc: &[Dimension]) -> DesignLocation {
    // TODO: what if Dimension uses uservalue? - new in DS5.0
    loc.iter()
        .map(|d| (d.name.clone(), DesignCoord::new(d.xvalue.unwrap())))
        .collect()
}

fn to_ir_point_type(typ: &norad::PointType) -> ir::PointType {
    match typ {
        norad::PointType::Move => ir::PointType::Move,
        norad::PointType::Line => ir::PointType::Line,
        norad::PointType::OffCurve => ir::PointType::OffCurve,
        norad::PointType::QCurve => ir::PointType::QCurve,
        norad::PointType::Curve => ir::PointType::Curve,
    }
}

fn to_ir_contour_point(point: &norad::ContourPoint) -> ir::ContourPoint {
    ir::ContourPoint {
        x: point.x,
        y: point.y,
        typ: to_ir_point_type(&point.typ),
    }
}

fn to_ir_contour(contour: &norad::Contour) -> ir::Contour {
    contour.points.iter().map(to_ir_contour_point).collect()
}

fn to_ir_component(component: &norad::Component) -> ir::Component {
    ir::Component {
        base: component.base.to_string(),
        transform: ir::Affine2x3 {
            xx: component.transform.x_scale,
            yx: component.transform.yx_scale,
            xy: component.transform.xy_scale,
            yy: component.transform.y_scale,
            dx: component.transform.x_offset,
            dy: component.transform.y_offset,
        },
    }
}

fn to_ir_glyph_instance(glyph: &norad::Glyph) -> ir::GlyphInstance {
    ir::GlyphInstance {
        width: glyph.width,
        height: Some(glyph.height),
        contours: glyph.contours.iter().map(to_ir_contour).collect(),
        components: glyph.components.iter().map(to_ir_component).collect(),
    }
}

pub fn to_ir_axis(axis: &designspace::Axis) -> ir::Axis {
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
        let default_idx = examples.iter().position(|(u, _)| *u == default).expect(
            "We currently require that you have a mapping for the default if you have mappings",
        );
        examples.iter().position(|(u, _)| *u == min).expect(
            "We currently require that you have a mapping for the min if you have mappings",
        );
        examples.iter().position(|(u, _)| *u == max).expect(
            "We currently require that you have a mapping for the max if you have mappings",
        );
        CoordConverter::new(examples, default_idx)
    } else {
        CoordConverter::unmapped(min, default, max)
    };
    ir::Axis {
        name: axis.name.clone(),
        tag: axis.tag.clone(),
        hidden: axis.hidden,
        min,
        default,
        max,
        converter,
    }
}

pub fn to_ir_glyph<S>(
    glyph_name: S,
    glif_files: &HashMap<&PathBuf, Vec<NormalizedLocation>>,
) -> Result<ir::Glyph, Error>
where
    S: Into<String>,
{
    let mut glyph = ir::Glyph::new(glyph_name.into());
    for (glif_file, locations) in glif_files {
        let norad_glyph = norad::Glyph::load(glif_file).map_err(Error::GlifLoadError)?;
        for location in locations {
            glyph.try_add_source(location, to_ir_glyph_instance(&norad_glyph))?;
        }
    }
    Ok(glyph)
}

#[cfg(test)]
mod tests {}
