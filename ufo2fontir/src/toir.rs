use std::collections::HashMap;
use std::path::PathBuf;

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

fn to_ir_contour(glyph_name: &str, contour: &norad::Contour) -> Result<BezPath, WorkError> {
    let mut path_builder = GlyphPathBuilder::new(glyph_name);
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

    if norad::PointType::Move == contour.points[0].typ {
        path_builder.close_path()?;
    }

    let path = path_builder.build();
    trace!(
        "Built a {} entry path for {}",
        path.elements().len(),
        glyph_name
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
        contours.push(to_ir_contour(glyph.name().as_str(), contour)?);
    }
    Ok(ir::GlyphInstance {
        width: glyph.width,
        height: Some(glyph.height),
        contours,
        components: glyph.components.iter().map(to_ir_component).collect(),
    })
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

pub fn to_ir_glyph(
    glyph_name: GlyphName,
    glif_files: &HashMap<&PathBuf, Vec<NormalizedLocation>>,
) -> Result<ir::Glyph, WorkError> {
    let mut glyph = ir::Glyph::new(glyph_name);
    for (glif_file, locations) in glif_files {
        let norad_glyph =
            norad::Glyph::load(glif_file).map_err(|e| WorkError::InvalidSourceGlyph {
                glyph_name: glyph.name.clone(),
                message: format!("glif load failed due to {}", e),
            })?;
        for location in locations {
            glyph.try_add_source(location, to_ir_glyph_instance(&norad_glyph)?)?;
        }
    }
    Ok(glyph)
}

#[cfg(test)]
mod tests {}
