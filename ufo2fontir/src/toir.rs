use std::collections::HashMap;
use std::path::PathBuf;

use fontir::ir;
use norad::designspace::{self, DesignSpaceDocument, Dimension};
use ordered_float::OrderedFloat;

use crate::error::Error;

// TODO we will need the ability to map coordinates and a test font that does. Then no unwrap.
pub(crate) fn to_ir_location(loc: &[Dimension]) -> ir::DesignSpaceLocation {
    loc.iter()
        .map(|d| (d.name.clone(), OrderedFloat(d.xvalue.unwrap())))
        .collect()
}

pub fn designspace_to_ir(designspace: DesignSpaceDocument) -> Result<Vec<ir::Axis>, Error> {
    // Truly we have done something amazing here today
    let ir_axes: Vec<ir::Axis> = designspace.axes.into_iter().map(to_ir_axis).collect();

    // Someday we will return something useful! But ... not today.
    Ok(ir_axes)
}

fn to_ir_axis(axis: designspace::Axis) -> ir::Axis {
    ir::Axis {
        name: axis.name,
        tag: axis.tag,
        min: axis.minimum.expect("Discrete axes not supported yet"),
        default: axis.default,
        max: axis.maximum.expect("Discrete axes not supported yet"),
        hidden: axis.hidden,
    }
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

pub fn to_ir_glyph<S>(
    glyph_name: S,
    glif_files: &HashMap<PathBuf, Vec<ir::DesignSpaceLocation>>,
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
mod tests {
    use norad::designspace::DesignSpaceDocument;
    use std::path::Path;

    use crate::toir::designspace_to_ir;
    use fontir::ir;

    #[test]
    fn simple_wght_variable() {
        let ds = DesignSpaceDocument::load(Path::new("testdata/wght_var.designspace")).unwrap();
        assert_eq!(
            vec![ir::Axis {
                name: "Weight".to_string(),
                tag: "wght".to_string(),
                min: 400.,
                default: 400.,
                max: 700.,
                hidden: false
            }],
            designspace_to_ir(ds).unwrap()
        );
    }
}
