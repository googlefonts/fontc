//! Generates a [VARC](https://github.com/harfbuzz/boring-expansion-spec/blob/main/VARC.md) table.

use std::collections::{BTreeMap, BTreeSet, HashMap};

use fontdrasil::{
    coords::NormalizedLocation,
    orchestration::{Access, AccessBuilder, Work},
    types::{Axes, GlyphName},
    variations::VariationModel,
};
use fontir::{ir, orchestration::WorkId as FeWorkId};
use write_fonts::{
    OtRound, dump_table,
    tables::{
        layout::CoverageTable,
        varc::{DecomposedTransform, VarComponent, VarCompositeGlyph, Varc, VarcVariationIndex},
        variations::mivs_builder::{MultiItemVariationStoreBuilder, SparseRegion},
    },
    types::{F2Dot14, F4Dot12, F6Dot10, GlyphId, GlyphId16, Tag},
};

use crate::{
    error::Error,
    orchestration::{AnyWorkId, BeWork, Context, WorkId},
};

#[derive(Debug)]
struct VarcWork {}

pub fn create_varc_work() -> Box<BeWork> {
    Box::new(VarcWork {})
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum Field {
    TranslateX,
    TranslateY,
    Rotation,
    ScaleX,
    ScaleY,
    SkewX,
    SkewY,
    CenterX,
    CenterY,
}

const FIELDS: [Field; 9] = [
    Field::TranslateX,
    Field::TranslateY,
    Field::Rotation,
    Field::ScaleX,
    Field::ScaleY,
    Field::SkewX,
    Field::SkewY,
    Field::CenterX,
    Field::CenterY,
];

impl Field {
    fn get(self, t: &ir::DecomposedTransform) -> Option<f64> {
        match self {
            Field::TranslateX => t.translate_x,
            Field::TranslateY => t.translate_y,
            Field::Rotation => t.rotation,
            Field::ScaleX => t.scale_x,
            Field::ScaleY => t.scale_y,
            Field::SkewX => t.skew_x,
            Field::SkewY => t.skew_y,
            Field::CenterX => t.center_x,
            Field::CenterY => t.center_y,
        }
    }

    fn set(self, t: &mut ir::DecomposedTransform, value: f64) {
        let slot = match self {
            Field::TranslateX => &mut t.translate_x,
            Field::TranslateY => &mut t.translate_y,
            Field::Rotation => &mut t.rotation,
            Field::ScaleX => &mut t.scale_x,
            Field::ScaleY => &mut t.scale_y,
            Field::SkewX => &mut t.skew_x,
            Field::SkewY => &mut t.skew_y,
            Field::CenterX => &mut t.center_x,
            Field::CenterY => &mut t.center_y,
        };
        *slot = Some(value);
    }

    fn identity(self) -> f64 {
        match self {
            Field::ScaleX | Field::ScaleY => 1.0,
            _ => 0.0,
        }
    }

    /// Encode a field value to its fixed-point representation (as f64),
    /// matching how write-fonts serializes the base transform.
    fn encode(self, value: f64) -> f64 {
        match self {
            Field::TranslateX | Field::TranslateY | Field::CenterX | Field::CenterY => {
                OtRound::<i16>::ot_round(value) as f64
            }
            Field::Rotation | Field::SkewY => {
                F4Dot12::from_f32((value / 180.0) as f32).to_bits() as f64
            }
            Field::SkewX => F4Dot12::from_f32((value / -180.0) as f32).to_bits() as f64,
            Field::ScaleX | Field::ScaleY => F6Dot10::from_f32(value as f32).to_bits() as f64,
        }
    }
}

/// Convert an IR decomposed transform to the write-fonts (VARC) form.
///
/// Rotation and skew are multiples of pi in VARC (F4Dot12). The IR uses
/// degrees, so divide by 180. VARC skewX is counter-clockwise and the IR
/// skewX is clockwise, so negate it.
fn to_write_fonts_transform(t: &ir::DecomposedTransform) -> DecomposedTransform {
    DecomposedTransform {
        translate_x: t.translate_x.map(|v| OtRound::<i16>::ot_round(v) as f64),
        translate_y: t.translate_y.map(|v| OtRound::<i16>::ot_round(v) as f64),
        rotation: t.rotation.map(|deg| deg / 180.0),
        scale_x: t.scale_x,
        scale_y: t.scale_y,
        skew_x: t.skew_x.map(|deg| deg / -180.0),
        skew_y: t.skew_y.map(|deg| deg / 180.0),
        center_x: t.center_x.map(|v| OtRound::<i16>::ot_round(v) as f64),
        center_y: t.center_y.map(|v| OtRound::<i16>::ot_round(v) as f64),
    }
}

/// The transform fields present across all instances of a component, in canonical
/// order. A field is present if it is set in any instance.
///
/// Scale is special-cased: if either scaleX or scaleY is present, so is the other.
/// An absent scale field is identity (1.0) in our IR, but VARC reads an absent scaleY
/// as *equal to scaleX* (its uniform-scale shorthand: HAVE_SCALE_X set without
/// HAVE_SCALE_Y). Emitting scaleY explicitly keeps a non-uniform component (e.g.
/// scaleX 2.0, scaleY identity) from round-tripping as uniform (scaleX 2.0, scaleY 2.0).
fn present_transform_fields(
    instances: &[(&NormalizedLocation, &ir::VariableComponent)],
) -> Vec<Field> {
    let mut present: Vec<bool> = FIELDS
        .iter()
        .map(|f| {
            instances
                .iter()
                .any(|(_, vc)| f.get(&vc.transform).is_some())
        })
        .collect();
    let (scale_x, scale_y) = (present[3], present[4]);
    if scale_x || scale_y {
        present[3] = true;
        present[4] = true;
    }
    FIELDS
        .iter()
        .zip(present)
        .filter_map(|(f, keep)| keep.then_some(*f))
        .collect()
}

/// Compute deltas for `point_seqs` and add them to the store, returning a pending
/// variation index. Returns `None` when nothing actually varies.
fn add_deltas(
    model: &VariationModel,
    point_seqs: &HashMap<NormalizedLocation, Vec<f64>>,
    axes: &Axes,
    store_builder: &mut MultiItemVariationStoreBuilder,
    name: &GlyphName,
) -> Result<Option<VarcVariationIndex>, Error> {
    let deltas = model
        .deltas(point_seqs)
        .map_err(|e| Error::GlyphDeltaError(name.clone(), e))?;
    let regions: Vec<(SparseRegion, Vec<i32>)> = deltas
        .into_iter()
        .filter(|(region, values)| {
            !region.is_default() && values.iter().any(|v| v.round_ties_even() as i32 != 0)
        })
        .map(|(region, values)| {
            (
                region.to_write_fonts_sparse_region(axes),
                values.iter().map(|v| v.round_ties_even() as i32).collect(),
            )
        })
        .collect();
    if regions.is_empty() {
        return Ok(None);
    }
    let id = store_builder
        .add_deltas(regions)
        .map_err(|e| Error::VarcDeltaStoreError(name.clone(), e))?;
    Ok(Some(VarcVariationIndex::PendingVariationIndex(id)))
}

/// Build one variable component, gathering the varying transform and axis values
/// across its `instances`.
#[allow(clippy::too_many_arguments)]
fn build_variable_component(
    gid: GlyphId,
    default: &ir::VariableComponent,
    instances: &[(&NormalizedLocation, &ir::VariableComponent)],
    model: &VariationModel,
    axes: &Axes,
    axis_map: &HashMap<Tag, u16>,
    store_builder: &mut MultiItemVariationStoreBuilder,
    name: &GlyphName,
) -> Result<VarComponent, Error> {
    // Axis values reference the glyph's variable space, keyed by fvar
    // axis index. Take the union across instances, ordered by index.
    let mut axis_entries: Vec<(u16, Tag)> = instances
        .iter()
        .flat_map(|(_, vc)| vc.location.axis_tags())
        .filter_map(|tag| axis_map.get(tag).map(|i| (*i, *tag)))
        .collect::<BTreeSet<_>>()
        .into_iter()
        .collect();
    axis_entries.sort_by_key(|(index, _)| *index);

    let axis_values: BTreeMap<u16, f32> = axis_entries
        .iter()
        .map(|(index, tag)| {
            let coord = default
                .location
                .get(*tag)
                .map(|c| c.to_f64() as f32)
                .unwrap_or(0.0);
            (*index, coord)
        })
        .collect();

    let axis_values_var_index = if axis_entries.is_empty() {
        None
    } else {
        let point_seqs = instances
            .iter()
            .map(|(loc, vc)| {
                let values = axis_entries
                    .iter()
                    .map(|(_, tag)| {
                        let coord = vc.location.get(*tag).map(|c| c.to_f64()).unwrap_or(0.0);
                        F2Dot14::from_f32(coord as f32).to_bits() as f64
                    })
                    .collect();
                ((*loc).clone(), values)
            })
            .collect();
        add_deltas(model, &point_seqs, axes, store_builder, name)?
    };

    // Transform: present fields (with identity for those a given instance omits).
    let present = present_transform_fields(instances);
    let mut base = ir::DecomposedTransform::default();
    for field in &present {
        field.set(
            &mut base,
            field.get(&default.transform).unwrap_or(field.identity()),
        );
    }
    let transform = to_write_fonts_transform(&base);

    let transform_var_index = if present.is_empty() {
        None
    } else {
        let point_seqs = instances
            .iter()
            .map(|(loc, vc)| {
                let values = present
                    .iter()
                    .map(|field| field.encode(field.get(&vc.transform).unwrap_or(field.identity())))
                    .collect();
                ((*loc).clone(), values)
            })
            .collect();
        add_deltas(model, &point_seqs, axes, store_builder, name)?
    };

    Ok(VarComponent {
        reset_unspecified_axes: default.reset_unspecified_axes,
        gid,
        condition_index: None,
        axis_values: (!axis_values.is_empty()).then_some(axis_values),
        axis_values_var_index,
        transform,
        transform_var_index,
    })
}

/// Assemble a VARC table from composites in glyph-id order, or `None` if there
/// are no variable composites.
fn build_varc(
    coverage: Vec<GlyphId16>,
    composites: Vec<VarCompositeGlyph>,
    store_builder: MultiItemVariationStoreBuilder,
) -> Option<Varc> {
    if composites.is_empty() {
        return None;
    }
    let coverage: CoverageTable = coverage.into_iter().collect();
    Some(Varc::new_from_composite_glyphs(
        coverage,
        store_builder,
        Vec::new(),
        composites,
    ))
}

/// Every source must have the same component count, and per index the same
/// base and reset flag. A VarComponent stores these once. Axis values and
/// transform fields may vary freely, absent entries count as the default.
/// Component axis tags must be font axes.
fn validate_variable_component_topology(
    name: &GlyphName,
    glyph: &ir::Glyph,
    axis_map: &HashMap<Tag, u16>,
) -> Result<(), Error> {
    let default = &glyph.default_instance().variable_components;
    let mut locations: Vec<_> = glyph.sources().keys().collect();
    locations.sort();
    for loc in locations {
        let inst = &glyph.sources()[loc];
        let vcs = &inst.variable_components;
        if vcs.len() != default.len() {
            return Err(Error::InconsistentVariableComponents {
                glyph: name.clone(),
                detail: format!(
                    "source {loc:?} has {} variable components, the default has {}",
                    vcs.len(),
                    default.len()
                ),
            });
        }
        for vc in vcs {
            if let Some(tag) = vc
                .location
                .axis_tags()
                .find(|tag| !axis_map.contains_key(*tag))
            {
                return Err(Error::VariableComponentUnknownAxis {
                    glyph: name.clone(),
                    tag: *tag,
                });
            }
        }
        for (idx, (d, v)) in default.iter().zip(vcs).enumerate() {
            if d.base != v.base {
                return Err(Error::InconsistentVariableComponents {
                    glyph: name.clone(),
                    detail: format!(
                        "component {idx} at source {loc:?} has base '{}', the default has '{}'",
                        v.base, d.base
                    ),
                });
            }
            if d.reset_unspecified_axes != v.reset_unspecified_axes {
                return Err(Error::InconsistentVariableComponents {
                    glyph: name.clone(),
                    detail: format!(
                        "component {idx} at source {loc:?} differs from the default in reset_unspecified_axes"
                    ),
                });
            }
        }
    }
    Ok(())
}

impl Work<Context, AnyWorkId, Error> for VarcWork {
    fn id(&self) -> AnyWorkId {
        WorkId::Varc.into()
    }

    fn read_access(&self) -> Access<AnyWorkId> {
        AccessBuilder::new()
            .variant(FeWorkId::StaticMetadata)
            .variant(FeWorkId::GlyphOrder)
            .variant(FeWorkId::Glyph(GlyphName::NOTDEF))
            .build()
    }

    /// Generate [VARC](https://github.com/harfbuzz/boring-expansion-spec/blob/main/VARC.md)
    #[tracing::instrument(name = "fontbe::VarcWork::exec", skip_all)]
    fn exec(&self, context: &Context) -> Result<(), Error> {
        let static_metadata = context.ir.static_metadata.get();
        let glyph_order = context.ir.glyph_order.get();
        let axes = &static_metadata.axes;

        // fvar axis tag -> index, for mapping component axis values.
        let axis_map: HashMap<Tag, u16> = axes
            .iter()
            .enumerate()
            .map(|(i, a)| (a.tag, i as u16))
            .collect();

        let mut store_builder = MultiItemVariationStoreBuilder::new();
        let mut coverage = Vec::new();
        let mut composites = Vec::new();

        // Gather variable composites in glyph-id order.
        for (gid, name) in glyph_order.iter() {
            let glyph = context.ir.glyphs.get(&FeWorkId::Glyph(name.clone()));
            let default_instance = glyph.default_instance();
            if glyph
                .sources()
                .values()
                .all(|inst| inst.variable_components.is_empty())
            {
                continue;
            }
            validate_variable_component_topology(name, &glyph, &axis_map)?;

            let model =
                VariationModel::new(glyph.sources().keys().cloned().collect(), axes.axis_order());

            let mut components = Vec::with_capacity(default_instance.variable_components.len());
            for (idx, component) in default_instance.variable_components.iter().enumerate() {
                let component_gid =
                    GlyphId::from(glyph_order.glyph_id(&component.base).ok_or_else(|| {
                        Error::VariableComponentBaseNotInGlyphOrder {
                            glyph: name.clone(),
                            base: component.base.clone(),
                        }
                    })?);
                let instances: Vec<(&NormalizedLocation, &ir::VariableComponent)> = glyph
                    .sources()
                    .iter()
                    .map(|(loc, inst)| (loc, &inst.variable_components[idx]))
                    .collect();
                components.push(build_variable_component(
                    component_gid,
                    component,
                    &instances,
                    &model,
                    axes,
                    &axis_map,
                    &mut store_builder,
                    name,
                )?);
            }
            coverage.push(gid);
            composites.push(VarCompositeGlyph(components));
        }

        let Some(varc) = build_varc(coverage, composites, store_builder) else {
            log::debug!("skipping VARC, no variable composites");
            return Ok(());
        };

        let raw_varc = dump_table(&varc).map_err(|e| Error::DumpTableError {
            e,
            context: "VARC".into(),
        })?;
        context.varc.set(raw_varc);
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn rejects_inconsistent_variable_components() {
        use std::collections::{HashMap, HashSet};

        let axis_map = HashMap::from([(Tag::new(b"wght"), 0u16)]);
        let make_vc = |base: &str| ir::VariableComponent {
            base: GlyphName::new(base),
            location: NormalizedLocation::for_pos(&[("wght", 0.0)]),
            transform: ir::DecomposedTransform::default(),
            reset_unspecified_axes: false,
        };
        let source_at = |pos: f64, vcs: Vec<ir::VariableComponent>| {
            (
                NormalizedLocation::for_pos(&[("wght", pos)]),
                ir::GlyphInstance {
                    variable_components: vcs,
                    ..Default::default()
                },
            )
        };
        let glyph_with = |sources: [(NormalizedLocation, ir::GlyphInstance); 2]| {
            ir::Glyph::new(
                GlyphName::new("g"),
                true,
                HashSet::new(),
                HashMap::from(sources),
            )
            .unwrap()
        };
        let validate = |glyph: &ir::Glyph| {
            validate_variable_component_topology(&GlyphName::new("g"), glyph, &axis_map)
        };

        // Same base at each source: consistent.
        let glyph = glyph_with([
            source_at(0.0, vec![make_vc("a")]),
            source_at(1.0, vec![make_vc("a")]),
        ]);
        assert!(validate(&glyph).is_ok());

        // Sparse transform fields and axis values may vary across sources.
        let mut sparse = make_vc("a");
        sparse.transform.translate_y = Some(-10.0);
        let glyph = glyph_with([
            source_at(0.0, vec![make_vc("a")]),
            source_at(1.0, vec![sparse]),
        ]);
        assert!(validate(&glyph).is_ok());

        // Different base at the non-default source: inconsistent.
        let glyph = glyph_with([
            source_at(0.0, vec![make_vc("a")]),
            source_at(1.0, vec![make_vc("b")]),
        ]);
        assert!(matches!(
            validate(&glyph),
            Err(Error::InconsistentVariableComponents { .. })
        ));

        // Different component count, incl. an empty default: inconsistent.
        let glyph = glyph_with([
            source_at(0.0, vec![make_vc("a")]),
            source_at(1.0, vec![make_vc("a"), make_vc("a")]),
        ]);
        assert!(matches!(
            validate(&glyph),
            Err(Error::InconsistentVariableComponents { .. })
        ));
        let glyph = glyph_with([
            source_at(0.0, Vec::new()),
            source_at(1.0, vec![make_vc("a")]),
        ]);
        assert!(matches!(
            validate(&glyph),
            Err(Error::InconsistentVariableComponents { .. })
        ));

        // Different reset flag: inconsistent.
        let mut reset = make_vc("a");
        reset.reset_unspecified_axes = true;
        let glyph = glyph_with([
            source_at(0.0, vec![make_vc("a")]),
            source_at(1.0, vec![reset]),
        ]);
        assert!(matches!(
            validate(&glyph),
            Err(Error::InconsistentVariableComponents { .. })
        ));

        // An axis that is not a font axis: error.
        let mut unknown = make_vc("a");
        unknown.location = NormalizedLocation::for_pos(&[("SMRT", 0.5)]);
        let glyph = glyph_with([
            source_at(0.0, vec![make_vc("a")]),
            source_at(1.0, vec![unknown]),
        ]);
        assert!(matches!(
            validate(&glyph),
            Err(Error::VariableComponentUnknownAxis { .. })
        ));
    }

    #[test]
    fn transform_rotation_and_skew_degrees_to_multiples_of_pi() {
        let ir_transform = ir::DecomposedTransform {
            translate_x: Some(10.0),
            rotation: Some(90.0), // degrees
            skew_x: Some(45.0),   // degrees
            scale_x: Some(1.5),
            ..Default::default()
        };
        let write_fonts_transform = to_write_fonts_transform(&ir_transform);
        assert_eq!(Some(10.0), write_fonts_transform.translate_x); // pass-through
        assert_eq!(Some(0.5), write_fonts_transform.rotation); // 90/180 = 0.5 multiples of pi
        assert_eq!(Some(-0.25), write_fonts_transform.skew_x); // 45/-180
        assert_eq!(Some(1.5), write_fonts_transform.scale_x); // pass-through
        assert_eq!(None, write_fonts_transform.translate_y); // absent stays absent
    }

    #[test]
    fn round_trips_a_static_variable_composite() {
        use fontir::ir::GlyphOrder;
        use write_fonts::read::{FontData, FontRead, tables::varc as read_varc};

        let mut glyph_order = GlyphOrder::new();
        glyph_order.insert(GlyphName::new(".notdef"));
        glyph_order.insert(GlyphName::new("radical"));
        glyph_order.insert(GlyphName::new("varc"));

        let component = VarComponent {
            reset_unspecified_axes: true,
            gid: GlyphId::from(glyph_order.glyph_id(&GlyphName::new("radical")).unwrap()),
            condition_index: None,
            axis_values: Some(BTreeMap::from([(0u16, 0.5f32)])),
            axis_values_var_index: None,
            transform: DecomposedTransform {
                translate_x: Some(100.0),
                rotation: Some(0.5),
                ..Default::default()
            },
            transform_var_index: None,
        };
        let composite_gid = glyph_order.glyph_id(&GlyphName::new("varc")).unwrap();
        let varc = build_varc(
            vec![composite_gid],
            vec![VarCompositeGlyph(vec![component])],
            MultiItemVariationStoreBuilder::new(),
        )
        .unwrap();

        // The emitted table round-trips through the read-fonts VARC parser, and our
        // one composite is covered.
        let bytes = dump_table(&varc).unwrap();
        let parsed = read_varc::Varc::read(FontData::new(&bytes)).unwrap();
        assert_eq!(Some(0), parsed.coverage().unwrap().get(composite_gid));
    }

    #[test]
    fn varies_transform_and_axis_values_across_sources() {
        use std::collections::HashSet;

        use fontdrasil::coords::NormalizedLocation;
        use fontir::ir::GlyphOrder;
        use write_fonts::read::{FontData, FontRead, tables::varc as read_varc};

        let mut glyph_order = GlyphOrder::new();
        glyph_order.insert(GlyphName::new(".notdef"));
        glyph_order.insert(GlyphName::new("base"));
        glyph_order.insert(GlyphName::new("composite"));
        let base_gid = GlyphId::from(glyph_order.glyph_id(&GlyphName::new("base")).unwrap());

        let axes = Axes::for_test(&["wght"]);
        let axis_index = HashMap::from([(Tag::new(b"wght"), 0u16)]);

        let default_loc = NormalizedLocation::for_pos(&[("wght", 0.0)]);
        let wght1_loc = NormalizedLocation::for_pos(&[("wght", 1.0)]);

        // translateX 50 -> 20, translateY absent (0) -> -10; axis value 0.0 -> 1.0.
        let default_vc = ir::VariableComponent {
            base: GlyphName::new("base"),
            location: NormalizedLocation::for_pos(&[("wght", 0.0)]),
            transform: ir::DecomposedTransform {
                translate_x: Some(50.0),
                ..Default::default()
            },
            reset_unspecified_axes: true,
        };
        let wght1_vc = ir::VariableComponent {
            base: GlyphName::new("base"),
            location: NormalizedLocation::for_pos(&[("wght", 1.0)]),
            transform: ir::DecomposedTransform {
                translate_x: Some(20.0),
                translate_y: Some(-10.0),
                ..Default::default()
            },
            reset_unspecified_axes: true,
        };
        let instances = vec![(&default_loc, &default_vc), (&wght1_loc, &wght1_vc)];

        let model = VariationModel::new(
            HashSet::from([default_loc.clone(), wght1_loc.clone()]),
            axes.axis_order(),
        );
        let mut store_builder = MultiItemVariationStoreBuilder::new();
        let component = build_variable_component(
            base_gid,
            &default_vc,
            &instances,
            &model,
            &axes,
            &axis_index,
            &mut store_builder,
            &GlyphName::new("composite"),
        )
        .unwrap();

        // Base transform carries the default values, incl. translateY=0 so it can vary.
        assert_eq!(Some(50.0), component.transform.translate_x);
        assert_eq!(Some(0.0), component.transform.translate_y);
        // Both the transform and the axis values vary.
        assert!(component.transform_var_index.is_some());
        assert!(component.axis_values_var_index.is_some());

        let composite_gid = glyph_order.glyph_id(&GlyphName::new("composite")).unwrap();
        let varc = build_varc(
            vec![composite_gid],
            vec![VarCompositeGlyph(vec![component])],
            store_builder,
        )
        .unwrap();
        let bytes = dump_table(&varc).unwrap();
        let parsed = read_varc::Varc::read(FontData::new(&bytes)).unwrap();
        // A multi-variation store was emitted.
        assert!(parsed.multi_var_store().is_some());
    }
}
