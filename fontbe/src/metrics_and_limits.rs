//! Generates the [hmtx](https://learn.microsoft.com/en-us/typography/opentype/spec/hmtx),
//! [hhea](https://learn.microsoft.com/en-us/typography/opentype/spec/hhea), and
//! [maxp](https://learn.microsoft.com/en-us/typography/opentype/spec/maxp) tables

use std::{
    cmp::{max, min},
    collections::HashMap,
    sync::Arc,
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
    types::{FWord, GlyphId16, UfWord},
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

/// A builder for aggregating metrics and calculating their limits, for
/// populating Hhea/Vhea and Hmtx/Vmtx respectively.
#[derive(Debug, Default)]
pub(crate) struct MetricsBuilder {
    long_metrics: Vec<LongMetric>,

    advance_max: u16,
    min_first_side_bearing: Option<i16>,
    min_second_side_bearing: Option<i16>,
    max_extent: Option<i16>,
}

/// The finished and explicit metrics, casted and with defaults populated for
/// immediate serialisation.
#[derive(Debug)]
pub(crate) struct Metrics {
    // The actual metrics.
    pub(crate) long_metrics: Vec<LongMetric>,
    pub(crate) first_side_bearings: Vec<i16>,

    // Aggregated summaries for the header table.
    pub(crate) advance_max: UfWord,
    pub(crate) min_first_side_bearing: FWord,
    pub(crate) min_second_side_bearing: FWord,
    pub(crate) max_extent: FWord,
}

/// Maximum contents and bounds, for constructing `maxp` and finishing `head`.
#[derive(Debug, Default)]
struct MaxBuilder {
    max_points: u16,
    max_contours: u16,
    max_component_elements: u16,
    glyph_info: HashMap<GlyphId16, GlyphInfo>,
    bbox: Option<Bbox>,
}

#[derive(Debug)]
struct GlyphInfo {
    /// For simple glyphs always present. For composites, set by [`MaxBuilder::update_composite_limits`]
    limits: Option<GlyphLimits>,
    components: Option<Vec<GlyphId16>>,
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

impl MetricsBuilder {
    pub(crate) fn update(&mut self, advance: u16, side_bearing: i16, bounds_advance: Option<i32>) {
        // https://github.com/googlefonts/ufo2ft/blob/16ed156bd6a8b9bc035d0aa8045a1271ef79a52e/Lib/ufo2ft/outlineCompiler.py#L797-L816
        self.long_metrics.push(LongMetric {
            advance,
            side_bearing,
        });

        // maximum advance should consider empty glyphs too
        self.advance_max = max(self.advance_max, advance);

        // min side bearings are only for non-empty glyphs
        // we will presume only simple glyphs with no contours are empty
        if let Some(bounds_advance) = bounds_advance {
            self.min_first_side_bearing = self
                .min_first_side_bearing
                .map(|v| min(v, side_bearing))
                .or(Some(side_bearing));

            let second_side_bearing: i16 =
                match advance as i32 - side_bearing as i32 - bounds_advance {
                    value if value < i16::MIN as i32 => i16::MIN,
                    value if value > i16::MAX as i32 => i16::MAX,
                    value => value as i16,
                };
            self.min_second_side_bearing = self
                .min_second_side_bearing
                .map(|v| min(v, second_side_bearing))
                .or(Some(second_side_bearing));

            let extent = match side_bearing as i32 + bounds_advance {
                value if value < i16::MIN as i32 => i16::MIN,
                value if value > i16::MAX as i32 => i16::MAX,
                value => value as i16,
            };
            self.max_extent = self.max_extent.map(|v| max(v, extent)).or(Some(extent));
        }
    }

    pub(crate) fn build(self) -> Metrics {
        let Self {
            mut long_metrics,
            advance_max,
            min_first_side_bearing,
            min_second_side_bearing,
            max_extent,
        } = self;

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

        let first_side_bearings = long_metrics
            .split_off(long_metrics.len() - num_lsb_only)
            .into_iter()
            .map(|metric| metric.side_bearing)
            .collect();

        Metrics {
            long_metrics,
            first_side_bearings,

            advance_max: advance_max.into(),
            min_first_side_bearing: min_first_side_bearing.unwrap_or_default().into(),
            min_second_side_bearing: min_second_side_bearing.unwrap_or_default().into(),
            max_extent: max_extent.unwrap_or_default().into(),
        }
    }
}

impl MaxBuilder {
    fn update(&mut self, id: GlyphId16, glyph: &Glyph) {
        if let Some(bbox) = glyph.data.bbox() {
            self.bbox = self.bbox.map(|b| b.union(bbox)).or(Some(bbox));
        }

        let glyph_info = match &glyph.data {
            RawGlyph::Simple(simple) => {
                let num_points = simple.contours.iter().map(Contour::len).sum::<usize>() as u16;
                let num_contours = simple.contours.len() as u16;
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

    // FontTools maxp <https://github.com/fonttools/fonttools/blob/e8146a6d0725d398cfa110cba683946ee762f8e2/Lib/fontTools/ttLib/tables/_m_a_x_p.py#L53>
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
            // We need composite bboxes to be calculated:
            .variant(WorkId::Glyf)
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

        // Collate horizontal metrics
        let builder =
            glyph_order
                .iter()
                .fold(MetricsBuilder::default(), |mut builder, (_gid, gn)| {
                    // https://github.com/googlefonts/ufo2ft/blob/2f11b0ff/Lib/ufo2ft/outlineCompiler.py#L741-L747
                    let advance: u16 = context
                        .ir
                        .get_glyph(gn.clone())
                        .default_instance()
                        .width
                        .ot_round();

                    let glyph = context.glyphs.get(&WorkId::GlyfFragment(gn.clone()).into());

                    let side_bearing = glyph.data.bbox().map(|bbox| bbox.x_min).unwrap_or_default();
                    let bounds_advance = glyph
                        .data
                        .bbox()
                        .map(|bbox| bbox.x_max as i32 - bbox.x_min as i32);

                    builder.update(advance, side_bearing, bounds_advance);
                    builder
                });

        let metrics = builder.build();

        // Build and send horizontal metrics tables out into the world
        let hhea = Hhea {
            ascender: FWord::new(default_metrics.hhea_ascender.into_inner().ot_round()),
            descender: FWord::new(default_metrics.hhea_descender.into_inner().ot_round()),
            line_gap: FWord::new(default_metrics.hhea_line_gap.into_inner().ot_round()),
            advance_width_max: metrics.advance_max,
            min_left_side_bearing: metrics.min_first_side_bearing,
            min_right_side_bearing: metrics.min_second_side_bearing,
            x_max_extent: metrics.max_extent,
            caret_slope_rise: default_metrics.caret_slope_rise.into_inner().ot_round(),
            caret_slope_run: default_metrics.caret_slope_run.into_inner().ot_round(),
            caret_offset: default_metrics.caret_offset.into_inner().ot_round(),
            number_of_h_metrics: metrics.long_metrics.len().try_into().map_err(|_| {
                Error::OutOfBounds {
                    what: "number_of_long_metrics".into(),
                    value: format!("{}", metrics.long_metrics.len()),
                }
            })?,
        };
        context.hhea.set(hhea);

        let hmtx = Hmtx::new(metrics.long_metrics, metrics.first_side_bearings);
        let raw_hmtx = dump_table(&hmtx)
            .map_err(|e| Error::DumpTableError {
                e,
                context: "hmtx".into(),
            })?
            .into();
        context.hmtx.set(raw_hmtx);

        let mut max_builder =
            glyph_order
                .iter()
                .fold(MaxBuilder::default(), |mut builder, (gid, gn)| {
                    let glyph = context.glyphs.get(&WorkId::GlyfFragment(gn.clone()).into());
                    builder.update(gid, &glyph);
                    builder
                });

        // Might as well do maxp while we're here
        let composite_limits = max_builder.update_composite_limits();
        let maxp = Maxp {
            num_glyphs: glyph_order.len().try_into().unwrap(),
            // maxp computes it's version based on whether fields are set
            // if you fail to set any of them it gets angry with you so set all of them
            max_points: Some(max_builder.max_points),
            max_contours: Some(max_builder.max_contours),
            max_composite_points: Some(composite_limits.max_points),
            max_composite_contours: Some(composite_limits.max_contours),
            max_zones: Some(1),
            max_twilight_points: Some(0),
            max_storage: Some(0),
            max_function_defs: Some(0),
            max_instruction_defs: Some(0),
            max_stack_elements: Some(0),
            max_size_of_instructions: Some(0),
            max_component_elements: Some(max_builder.max_component_elements),
            max_component_depth: Some(composite_limits.max_depth),
        };
        context.maxp.set(maxp);

        // Set x/y min/max in head
        let mut head = Arc::unwrap_or_clone(context.head.get());
        let bbox = max_builder.bbox.unwrap_or_default();
        head.x_min = bbox.x_min;
        head.y_min = bbox.y_min;
        head.x_max = bbox.x_max;
        head.y_max = bbox.y_max;

        // If every glyph lsb == x_min we can set a flag in head
        // Since we never set lsb to anything other than x_min it would appear we can *always* set this
        // It's set by default so the only way it gets unset is when source explicitly sets head flags
        // Ref <https://github.com/fonttools/fonttools/blob/7e374c53da9a7443d32b31138a0e5be478bcbab9/Lib/fontTools/ttLib/tables/_m_a_x_p.py#L81C9-L122>
        head.flags |= 0b10;

        context.head.set(head);

        Ok(())
    }
}

#[cfg(test)]
mod tests {

    use super::*;

    // advance 0, bbox (-437,611) => (-334, 715) encountered in NotoSansKayahLi.designspace
    #[test]
    fn negative_xmax_does_not_crash() {
        let mut glyph_limits = MetricsBuilder::default();
        glyph_limits.update(0, -437, Some(-334 - (-437)));
        assert_eq!(
            (Some(-437), Some(334)),
            (
                glyph_limits.min_first_side_bearing,
                glyph_limits.min_second_side_bearing
            )
        );
    }

    #[test]
    fn empty_glyph_contributes_to_max() {
        let width = 123u16;

        // confirm that empty glyphs still contribute to the advance_width_max
        // field, as they will still originate an hmtx entry
        let mut glyph_limits = MetricsBuilder::default();
        glyph_limits.update(width, 0, None);
        assert_eq!(width, glyph_limits.advance_max);
    }
}
