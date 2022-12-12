use std::collections::HashMap;
use std::path::PathBuf;

use fontir::{
    coords::{
        temporary_design_to_user_conversion, DesignSpaceCoord, UserSpaceCoord, UserSpaceLocation,
    },
    ir,
    piecewise_linear_map::PiecewiseLinearMap,
};
use norad::designspace::{self, Dimension};
use ordered_float::OrderedFloat;

use crate::error::Error;

// TODO we will need the ability to map coordinates and a test font that does. Then no unwrap.
pub(crate) fn to_ir_location(loc: &[Dimension]) -> UserSpaceLocation {
    loc.iter()
        .map(|d| {
            // TODO: what if d has uservalue (new in DS5.0)
            let coord = DesignSpaceCoord::new(OrderedFloat(d.xvalue.unwrap()));
            let coord = temporary_design_to_user_conversion(coord);
            (d.name.clone(), coord)
        })
        .collect()
}

fn to_user(design_to_user: &PiecewiseLinearMap, design_coord: f32) -> UserSpaceCoord {
    UserSpaceCoord::new(design_to_user.map(OrderedFloat(design_coord)))
}

pub fn to_ir_axis(axis: &designspace::Axis) -> ir::Axis {
    // We're not in designspace anymore Dorothy
    let design_to_user = if let Some(mappings) = &axis.map {
        let map = mappings
            .iter()
            .map(|m| (OrderedFloat(m.input), OrderedFloat(m.output)))
            .collect();
        PiecewiseLinearMap::new(map)
    } else {
        PiecewiseLinearMap::nop()
    };

    ir::Axis {
        name: axis.name.clone(),
        tag: axis.tag.clone(),
        hidden: axis.hidden,
        min: to_user(&design_to_user, axis.minimum.unwrap()),
        default: to_user(&design_to_user, axis.default),
        max: to_user(&design_to_user, axis.maximum.unwrap()),
        design_to_user,
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
    glif_files: &HashMap<PathBuf, Vec<UserSpaceLocation>>,
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
    use ordered_float::OrderedFloat;
    use std::path::PathBuf;

    use fontir::{coords::UserSpaceCoord, ir, piecewise_linear_map::PiecewiseLinearMap};

    use crate::toir::to_ir_axis;

    use pretty_assertions::assert_eq;

    fn user_coord(v: f32) -> UserSpaceCoord {
        UserSpaceCoord::new(OrderedFloat(v))
    }

    fn testdata_dir() -> PathBuf {
        PathBuf::from("../resources/testdata")
    }

    #[test]
    fn simple_wght_variable() {
        let ds = DesignSpaceDocument::load(testdata_dir().join("wght_var.designspace")).unwrap();
        assert_eq!(
            vec![ir::Axis {
                name: "Weight".to_string(),
                tag: "wght".to_string(),
                min: user_coord(400_f32),
                default: user_coord(400_f32),
                max: user_coord(700_f32),
                hidden: false,
                design_to_user: PiecewiseLinearMap::nop(),
            }],
            ds.axes.iter().map(to_ir_axis).collect::<Vec<_>>()
        );
    }

    #[test]
    fn simple_axis_mapping() {
        let ds = DesignSpaceDocument::load(testdata_dir().join("mapping.designspace")).unwrap();
        // Norad doesn't seem to give us the value from <labelname xml:lang="en">Weight</labelname> for axes so we get lowercase
        assert_eq!(
            vec![
                ir::Axis {
                    name: "weight".to_string(),
                    tag: "wght".to_string(),
                    min: user_coord(-1.0),
                    default: user_coord(-0.1),
                    max: user_coord(1.125),
                    hidden: false,
                    design_to_user: PiecewiseLinearMap::new(vec![
                        (OrderedFloat(100.0), OrderedFloat(-1.0)),
                        (OrderedFloat(200.0), OrderedFloat(-0.825)),
                        (OrderedFloat(300.0), OrderedFloat(-0.55)),
                        (OrderedFloat(400.0), OrderedFloat(-0.1)),
                        (OrderedFloat(500.0), OrderedFloat(0.35)),
                        (OrderedFloat(600.0), OrderedFloat(0.54)),
                        (OrderedFloat(700.0), OrderedFloat(0.73)),
                        (OrderedFloat(800.0), OrderedFloat(0.9275)),
                        (OrderedFloat(900.0), OrderedFloat(1.125)),
                    ]),
                },
                ir::Axis {
                    name: "width".to_string(),
                    tag: "wdth".to_string(),
                    min: user_coord(75_f32),
                    default: user_coord(100_f32),
                    max: user_coord(100_f32),
                    hidden: false,
                    design_to_user: PiecewiseLinearMap::nop(),
                },
                ir::Axis {
                    name: "italic".to_string(),
                    tag: "ital".to_string(),
                    min: user_coord(0_f32),
                    default: user_coord(0_f32),
                    max: user_coord(1_f32),
                    hidden: false,
                    design_to_user: PiecewiseLinearMap::nop(),
                }
            ],
            ds.axes.iter().map(to_ir_axis).collect::<Vec<_>>()
        );
    }
}
