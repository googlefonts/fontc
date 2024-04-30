//! Generates the [hmtx](https://learn.microsoft.com/en-us/typography/opentype/spec/hmtx) and
//! [hhea](https://learn.microsoft.com/en-us/typography/opentype/spec/hhea) tables

use std::{
    cmp::{max, min},
    collections::{HashMap, HashSet},
};

use fontdrasil::orchestration::{Access, AccessBuilder, Work};
use fontir::orchestration::WorkId as FeWorkId;
use write_fonts::{
    dump_table,
    tables::{
        glyf::{Bbox, Contour, Glyph as RawGlyph},
        hhea::Hhea,
        hmtx::Hmtx,
        maxp::Maxp,
        vmtx::LongMetric,
    },
    types::{FWord, GlyphId},
    OtRound,
};

use crate::{
    error::Error,
    orchestration::{AnyWorkId, BeWork, Context, Glyph, WorkId},
};

#[derive(Debug)]
struct MetricAndLimitWork {}

pub fn create_metric_and_limit_work() -> Box<BeWork> {
    Box::new(MetricAndLimitWork {})
}

/// Font-wide, or global, limits
#[derive(Debug, Default)]
struct FontLimits {
    min_left_side_bearing: Option<i16>,
    min_right_side_bearing: Option<i16>,
    x_max_extent: Option<i16>,
    advance_width_max: u16,
    max_points: u16,
    max_contours: u16,
    max_component_elements: u16,
    glyph_info: HashMap<GlyphId, GlyphInfo>,
    bbox: Option<Bbox>,
}

#[derive(Debug)]
struct GlyphInfo {
    /// For simple glyphs always present. For composites, set by [`FontLimits::update_composite_limits`]
    limits: Option<GlyphLimits>,
    components: Option<HashSet<GlyphId>>,
}

impl GlyphInfo {
    fn is_component(&self) -> bool {
        self.components.is_some()
    }
}

/// Limits of a single glyph
#[derive(Default, Debug, Copy, Clone)]
struct GlyphLimits {
    max_points: u16,
    max_contours: u16,
    max_depth: u16,
}

impl GlyphLimits {
    fn max(&self, other: GlyphLimits) -> GlyphLimits {
        GlyphLimits {
            max_points: self.max_points.max(other.max_points),
            max_contours: self.max_contours.max(other.max_contours),
            max_depth: self.max_depth.max(other.max_depth),
        }
    }
}

impl FontLimits {
    fn update(&mut self, id: GlyphId, advance: u16, glyph: &Glyph) {
        // min side bearings are only for non-empty glyphs
        // we will presume only simple glyphs with no contours are empty
        if let Some(bbox) = glyph.data.bbox() {
            let left_side_bearing = bbox.x_min;
            // aw - (lsb + xMax - xMin) ... but if lsb == xMin then just advance - xMax?
            let right_side_bearing: i16 = match advance as i32 - bbox.x_max as i32 {
                value if value < i16::MIN as i32 => i16::MIN,
                value if value > i16::MAX as i32 => i16::MAX,
                value => value as i16,
            };
            self.min_left_side_bearing = self
                .min_left_side_bearing
                .map(|v| min(v, left_side_bearing))
                .or(Some(left_side_bearing));
            self.min_right_side_bearing = self
                .min_right_side_bearing
                .map(|v| min(v, right_side_bearing))
                .or(Some(right_side_bearing));
            self.x_max_extent = self
                .x_max_extent
                .map(|v| max(v, bbox.x_max))
                .or(Some(bbox.x_max));
            self.advance_width_max = max(self.advance_width_max, advance);
            self.bbox = self.bbox.map(|b| b.union(bbox)).or(Some(bbox));
        }

        let glyph_info = match &glyph.data {
            RawGlyph::Simple(simple) => {
                let num_points = simple.contours().iter().map(Contour::len).sum::<usize>() as u16;
                let num_contours = simple.contours().len() as u16;
                self.max_points = max(self.max_points, num_points);
                self.max_contours = max(self.max_contours, num_contours);
                GlyphInfo {
                    limits: Some(GlyphLimits {
                        max_points: num_points,
                        max_contours: num_contours,
                        max_depth: 0,
                    }),
                    components: None,
                }
            }
            RawGlyph::Composite(composite) => {
                let num_components = composite.components().len() as u16;
                self.max_component_elements = max(self.max_component_elements, num_components);
                let components = Some(composite.components().iter().map(|c| c.glyph).collect());
                GlyphInfo {
                    limits: None,
                    components,
                }
            }
            RawGlyph::Empty => GlyphInfo {
                limits: Some(GlyphLimits::default()),
                components: None,
            },
        };
        self.glyph_info.insert(id, glyph_info);
    }

    fn update_composite_limits(&mut self) -> GlyphLimits {
        let mut pending = self
            .glyph_info
            .iter()
            .filter_map(|(gid, gi)| if gi.is_component() { Some(*gid) } else { None })
            .collect::<Vec<_>>();
        let mut overall_max = GlyphLimits::default();
        let mut components: Vec<Option<GlyphLimits>> = Vec::with_capacity(8);
        while !pending.is_empty() {
            let size_before = pending.len();
            pending.retain(|gid| {
                let glyph_info = self.glyph_info.get(gid).unwrap();
                components.clear();
                components.extend(
                    glyph_info
                        .components
                        .as_ref()
                        .unwrap()
                        .iter()
                        .map(|gid| self.glyph_info.get(gid).unwrap().limits),
                );

                // If we contain components whose limits are not yet known we can't proceed
                // Simple glyphs always know, so this only applies to nested composites
                if !components.iter().all(|limits| limits.is_some()) {
                    return true;
                }
                // We know the limits of all child components; a final result is achievable
                let limit = components.iter().map(|limits| limits.unwrap()).fold(
                    GlyphLimits::default(),
                    |acc, e| GlyphLimits {
                        max_points: acc.max_points + e.max_points,
                        max_contours: acc.max_contours + e.max_contours,
                        max_depth: acc.max_depth.max(e.max_depth + 1),
                    },
                );
                self.glyph_info.get_mut(gid).unwrap().limits = Some(limit);
                overall_max = overall_max.max(limit);
                false
            });
            assert!(
                pending.len() < size_before,
                "Stuck with {size_before} of unknown depth"
            );
        }

        overall_max
    }
}

impl Work<Context, AnyWorkId, Error> for MetricAndLimitWork {
    fn id(&self) -> AnyWorkId {
        WorkId::Hmtx.into()
    }

    fn read_access(&self) -> Access<AnyWorkId> {
        AccessBuilder::new()
            .variant(FeWorkId::GlobalMetrics)
            .variant(FeWorkId::GlyphOrder)
            .variant(WorkId::Head)
            .variant(FeWorkId::ALL_GLYPHS)
            .variant(WorkId::ALL_GLYF_FRAGMENTS)
            .build()
    }

    fn write_access(&self) -> Access<AnyWorkId> {
        AccessBuilder::new()
            .variant(WorkId::Hmtx)
            .variant(WorkId::Hhea)
            .variant(WorkId::Maxp)
            .variant(WorkId::Head)
            .build()
    }

    fn also_completes(&self) -> Vec<AnyWorkId> {
        vec![WorkId::Hhea.into(), WorkId::Maxp.into()]
    }

    /// Generate:
    ///
    /// * [hmtx](https://learn.microsoft.com/en-us/typography/opentype/spec/hmtx)
    /// * [hhea](https://learn.microsoft.com/en-us/typography/opentype/spec/hhea)
    /// * [maxp](https://learn.microsoft.com/en-us/typography/opentype/spec/maxp)
    ///
    /// Touchup [head](https://learn.microsoft.com/en-us/typography/opentype/spec/head)
    fn exec(&self, context: &Context) -> Result<(), Error> {
        let static_metadata = context.ir.static_metadata.get();
        let glyph_order = context.ir.glyph_order.get();
        let default_metrics = context
            .ir
            .global_metrics
            .get()
            .at(static_metadata.default_location());

        let mut glyph_limits = FontLimits::default();

        let mut long_metrics: Vec<LongMetric> = glyph_order
            .iter()
            .enumerate()
            .map(|(gid, gn)| {
                let gid = GlyphId::new(gid as u16);
                let advance: u16 = context
                    .ir
                    .glyphs
                    .get(&FeWorkId::Glyph(gn.clone()))
                    .default_instance()
                    .width
                    .ot_round();
                let glyph = context.glyphs.get(&WorkId::GlyfFragment(gn.clone()).into());
                glyph_limits.update(gid, advance, &glyph);
                LongMetric {
                    advance,
                    side_bearing: glyph.data.bbox().map(|bbox| bbox.x_min).unwrap_or_default(),
                }
            })
            .collect();

        // If there's a run at the end with matching advances we can save some bytes
        let num_lsb_only = if !long_metrics.is_empty() {
            let last_advance = long_metrics.last().unwrap().advance;
            let mut lsb_run = 0;
            for metric in long_metrics.iter().rev() {
                if metric.advance != last_advance {
                    break;
                }
                lsb_run += 1;
            }

            // Carve 1 less than the length of the run off so the last metric retained has the advance
            // that repeats
            lsb_run - 1
        } else {
            0
        };

        let lsbs = long_metrics
            .split_off(long_metrics.len() - num_lsb_only)
            .into_iter()
            .map(|metric| metric.side_bearing)
            .collect();

        // Before we cede ownership of Hmtx grab a few notes for Hhea
        let min_left_side_bearing = glyph_limits
            .min_left_side_bearing
            .unwrap_or_default()
            .into();
        let min_right_side_bearing = glyph_limits.min_right_side_bearing.unwrap_or_default();
        let min_right_side_bearing = min_right_side_bearing.into();
        let x_max_extent = glyph_limits.x_max_extent.unwrap_or_default().into();
        let hhea = Hhea {
            ascender: FWord::new(default_metrics.hhea_ascender.into_inner().ot_round()),
            descender: FWord::new(default_metrics.hhea_descender.into_inner().ot_round()),
            line_gap: FWord::new(default_metrics.hhea_line_gap.into_inner().ot_round()),
            advance_width_max: glyph_limits.advance_width_max.into(),
            min_left_side_bearing,
            min_right_side_bearing,
            x_max_extent,
            caret_slope_rise: default_metrics.caret_slope_rise.into_inner().ot_round(),
            caret_slope_run: default_metrics.caret_slope_run.into_inner().ot_round(),
            caret_offset: default_metrics.caret_offset.into_inner().ot_round(),
            number_of_long_metrics: long_metrics.len().try_into().map_err(|_| {
                Error::OutOfBounds {
                    what: "number_of_long_metrics".into(),
                    value: format!("{}", long_metrics.len()),
                }
            })?,
        };
        context.hhea.set_unconditionally(hhea.into());

        // Send hmtx out into the world
        let hmtx = Hmtx::new(long_metrics, lsbs);
        let raw_hmtx = dump_table(&hmtx)
            .map_err(|e| Error::DumpTableError {
                e,
                context: "hmtx".into(),
            })?
            .into();
        context.hmtx.set_unconditionally(raw_hmtx);

        // Might as well do maxp while we're here
        let composite_limits = glyph_limits.update_composite_limits();
        let maxp = Maxp {
            num_glyphs: glyph_order.len().try_into().unwrap(),
            // maxp computes it's version based on whether fields are set
            // if you fail to set any of them it gets angry with you so set all of them
            max_points: Some(glyph_limits.max_points),
            max_contours: Some(glyph_limits.max_contours),
            max_composite_points: Some(composite_limits.max_points),
            max_composite_contours: Some(composite_limits.max_contours),
            max_zones: Some(1),
            max_twilight_points: Some(0),
            max_storage: Some(0),
            max_function_defs: Some(0),
            max_instruction_defs: Some(0),
            max_stack_elements: Some(0),
            max_size_of_instructions: Some(0),
            max_component_elements: Some(glyph_limits.max_component_elements),
            max_component_depth: Some(composite_limits.max_depth),
        };
        context.maxp.set_unconditionally(maxp.into());

        // Set x/y min/max in head
        let mut head = context.head.get().0.clone().unwrap();
        let bbox = glyph_limits.bbox.unwrap_or_default();
        head.x_min = bbox.x_min;
        head.y_min = bbox.y_min;
        head.x_max = bbox.x_max;
        head.y_max = bbox.y_max;
        context.head.set_unconditionally(head.into());

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use kurbo::BezPath;
    use write_fonts::tables::glyf::SimpleGlyph;

    use super::*;

    // advance 0, bbox (-437,611) => (-334, 715) encountered in NotoSansKayahLi.designspace
    #[test]
    fn negative_xmax_does_not_crash() {
        let mut glyph_limits = FontLimits::default();
        // path crafted to give the desired bbox
        glyph_limits.update(
            GlyphId::new(0),
            0,
            &crate::orchestration::Glyph::new(
                "don't care".into(),
                SimpleGlyph::from_bezpath(
                    &BezPath::from_svg("M-437,611 L-334,715 L-334,611 Z").unwrap(),
                )
                .unwrap(),
            ),
        );
        assert_eq!(
            (Some(-437), Some(334)),
            (
                glyph_limits.min_left_side_bearing,
                glyph_limits.min_right_side_bearing
            )
        );
    }
}
