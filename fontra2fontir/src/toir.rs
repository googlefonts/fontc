//! Functions to convert fontra things to fontc IR things

use std::collections::{HashMap, HashSet};

use fontdrasil::{
    coords::{CoordConverter, DesignCoord, NormalizedCoord, NormalizedLocation, UserCoord},
    types::{Axis, GlyphName},
};
use fontir::{
    error::WorkError,
    ir::{Glyph, GlyphInstance, GlyphPathBuilder, StaticMetadata},
};
use kurbo::BezPath;
use log::trace;
use write_fonts::types::Tag;

use crate::fontra::{AxisName, FontraContour, FontraFontData, FontraGlyph, FontraPoint, PointType};

pub(crate) fn to_ir_static_metadata(
    font_data: &FontraFontData,
) -> Result<StaticMetadata, WorkError> {
    let axes = font_data
        .axes
        .iter()
        .map(|a| match a {
            crate::fontra::FontraAxis::Discrete(_) => Err(WorkError::UnsupportedConstruct(
                format!("discrete axis {a:?}"),
            )),
            crate::fontra::FontraAxis::Continuous(a) => Ok(a),
        })
        .map(|a| {
            let a = a?;
            let min = UserCoord::new(a.min_value as f32);
            let default = UserCoord::new(a.default_value as f32);
            let max = UserCoord::new(a.max_value as f32);

            if min > default || max < default {
                return Err(WorkError::InconsistentAxisDefinitions(format!("{a:?}")));
            }

            let converter = if !a.mapping.is_empty() {
                let examples: Vec<_> = a
                    .mapping
                    .iter()
                    .map(|[raw_user, raw_design]| {
                        (
                            UserCoord::new(*raw_user as f32),
                            DesignCoord::new(*raw_design as f32),
                        )
                    })
                    .collect();
                let default_idx = examples
                    .iter()
                    .position(|(u, _)| *u == default)
                    .ok_or_else(|| WorkError::AxisMustMapDefault(a.tag))?;
                examples
                    .iter()
                    .position(|(u, _)| *u == min)
                    .ok_or_else(|| WorkError::AxisMustMapMin(a.tag))?;
                examples
                    .iter()
                    .position(|(u, _)| *u == max)
                    .ok_or_else(|| WorkError::AxisMustMapMax(a.tag))?;
                CoordConverter::new(examples, default_idx)
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
            })
        })
        .collect::<Result<_, _>>()?;

    StaticMetadata::new(
        font_data.units_per_em,
        Default::default(),
        axes,
        Default::default(),
        Default::default(), // TODO: glyph locations we really do need
        Default::default(),
        Default::default(),
        Default::default(),
    )
    .map_err(WorkError::VariationModelError)
}

#[allow(dead_code)] // TEMPORARY
fn to_ir_glyph(
    global_axes: HashMap<AxisName, Tag>,
    codepoints: HashSet<u32>,
    fontra_glyph: &FontraGlyph,
) -> Result<Glyph, WorkError> {
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
            return Err(WorkError::NoSourceForName(layer_name.to_string()));
        };
        let global_location: NormalizedLocation = global_axes
            .iter()
            .map(|(name, tag)| {
                (
                    *tag,
                    NormalizedCoord::new(location.get(name).copied().unwrap_or_default() as f32),
                )
            })
            .collect();

        let contours: Vec<_> = layer
            .glyph
            .path
            .contours
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
            return Err(WorkError::DuplicateNormalizedLocation {
                what: "Multiple glyph instances".to_string(),
                loc: global_location,
            });
        };
    }

    Glyph::new(fontra_glyph.name.clone(), true, codepoints, instances)
}

#[allow(dead_code)] // TEMPORARY
fn add_to_path<'a>(
    glyph_name: GlyphName,
    path_builder: &'a mut GlyphPathBuilder,
    points: impl Iterator<Item = &'a FontraPoint>,
) -> Result<(), WorkError> {
    // Walk through the remaining points, accumulating off-curve points until we see an on-curve
    // https://github.com/googlefonts/glyphsLib/blob/24b4d340e4c82948ba121dcfe563c1450a8e69c9/Lib/glyphsLib/pens.py#L92
    for point in points {
        let point_type = point
            .point_type()
            .map_err(|e| WorkError::InvalidSourceGlyph {
                glyph_name: glyph_name.clone(),
                message: format!("No point type for {point:?}: {e}"),
            })?;
        // Smooth is only relevant to editors so ignore here
        match point_type {
            PointType::OnCurve | PointType::OnCurveSmooth => path_builder
                .curve_to((point.x, point.y))
                .map_err(WorkError::PathConversionError)?,
            PointType::OffCurveQuad | PointType::OffCurveCubic => path_builder
                .offcurve((point.x, point.y))
                .map_err(WorkError::PathConversionError)?,
        }
    }
    Ok(())
}

fn to_ir_path(glyph_name: GlyphName, contour: &FontraContour) -> Result<BezPath, WorkError> {
    // Based on glyphs2fontir/src/toir.rs to_ir_path
    // TODO(https://github.com/googlefonts/fontc/issues/700): share code
    if contour.points.is_empty() {
        return Ok(BezPath::new());
    }

    let mut path_builder = GlyphPathBuilder::new(glyph_name.clone(), contour.points.len());

    if !contour.is_closed {
        let first = contour.points.first().unwrap();
        let first_type = first
            .point_type()
            .map_err(|e| WorkError::InvalidSourceGlyph {
                glyph_name: glyph_name.clone(),
                message: format!("No point type for {first:?}: {e}"),
            })?;
        if first_type.is_off_curve() {
            return Err(WorkError::InvalidSourceGlyph {
                glyph_name: glyph_name.clone(),
                message: String::from("Open path starts with off-curve points"),
            });
        }
        path_builder.move_to((first.x, first.y))?;
        add_to_path(
            glyph_name.clone(),
            &mut path_builder,
            contour.points[1..].iter(),
        )?;
    } else {
        add_to_path(glyph_name.clone(), &mut path_builder, contour.points.iter())?;
    }

    let path = path_builder.build()?;
    trace!(
        "Built a {} entry path for {}",
        path.elements().len(),
        glyph_name
    );
    Ok(path)
}

#[cfg(test)]
mod tests {
    use std::collections::{HashMap, HashSet};

    use fontdrasil::types::Axis;
    use fontir::ir::Glyph;
    use kurbo::{BezPath, PathEl};
    use write_fonts::types::Tag;

    use crate::{
        fontra::{FontraFontData, FontraGlyph},
        test::testdata_dir,
        toir::to_ir_static_metadata,
    };

    use super::to_ir_glyph;

    fn axis_tuples(axes: &[Axis]) -> Vec<(&str, Tag, f64, f64, f64)> {
        axes.iter()
            .map(|a| {
                (
                    a.name.as_str(),
                    a.tag,
                    a.min.to_f32() as f64,
                    a.default.to_f32() as f64,
                    a.max.to_f32() as f64,
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
            .iter()
            .map(|(_, s)| {
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
        let fontdata_file = testdata_dir().join("2glyphs.fontra/font-data.json");
        let font_data = FontraFontData::from_file(&fontdata_file).unwrap();
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
        let fontra_glyph = FontraGlyph::from_file(&glyph_file).unwrap();
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
                .iter()
                .map(|(_, s)| (s.contours.len(), s.components.len()))
                .collect::<Vec<_>>()
        );
        assert_contour_compatibility(&glyph);
    }
}
