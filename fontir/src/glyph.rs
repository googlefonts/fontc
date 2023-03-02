//! IR glyph processing.
//!
//! Notably includes splitting glyphs with contours and components into one new glyph with
//! the contours and one updated glyph with no contours that references the new gyph as a component.

use std::collections::{HashSet, VecDeque};

use fontdrasil::{orchestration::Work, types::GlyphName};
use indexmap::IndexSet;
use kurbo::Affine;
use log::{debug, trace};
use ordered_float::OrderedFloat;
use write_fonts::pens::{write_to_pen, BezPathPen, ReverseContourPen};

use crate::{
    coords::NormalizedLocation,
    error::WorkError,
    ir::{Component, Glyph},
    orchestration::{Context, IrWork},
};

pub fn create_finalize_static_metadata_work() -> Box<IrWork> {
    Box::new(FinalizeStaticMetadataWork {})
}

struct FinalizeStaticMetadataWork {}

/// Glyph should split if it has components *and* contours.
///
/// Such a glyph turns into a simple glyf with the contours and a
/// composite glyph that references the simple glyph as a component.
///
/// <https://learn.microsoft.com/en-us/typography/opentype/spec/glyf>
fn has_components_and_contours(glyph: &Glyph) -> bool {
    glyph
        .sources
        .values()
        .any(|inst| !inst.components.is_empty() && !inst.contours.is_empty())
}

/// Does the Glyph uses components whose transform is different at different locations designspace?
fn has_consistent_component_transforms(glyph: &Glyph) -> bool {
    distinct_component_seqs(glyph).len() <= 1
}

fn name_for_derivative(base_name: &GlyphName, names_in_use: &IndexSet<GlyphName>) -> GlyphName {
    let mut i = 0;
    let base_name = base_name.as_str();
    loop {
        let new_name: GlyphName = format!("{base_name}.{i}").into();
        if !names_in_use.contains(&new_name) {
            return new_name;
        }
        i += 1;
    }
}

fn split_glyph(glyph_order: &IndexSet<GlyphName>, original: &Glyph) -> (Glyph, Glyph) {
    // Make a simple glyph by erasing the components from it
    let mut simple_glyph = original.clone();
    simple_glyph.sources.iter_mut().for_each(|(_, inst)| {
        inst.components.clear();
    });

    // Find a free name for the contour glyph
    let simple_glyph_name = name_for_derivative(&original.name, glyph_order);
    simple_glyph.name = simple_glyph_name.clone();

    // Use the contour glyph as a component in the original glyph and erase it's contours
    let mut composite_glyph = original.clone();
    composite_glyph.sources.iter_mut().for_each(|(_, inst)| {
        inst.contours.clear();
        inst.components.push(Component {
            base: simple_glyph_name.clone(),
            transform: Affine::IDENTITY,
        });
    });

    (simple_glyph, composite_glyph)
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct HashableComponent {
    base: GlyphName,
    transform: [OrderedFloat<f64>; 6],
}

impl From<&Component> for HashableComponent {
    fn from(value: &Component) -> Self {
        let mut transform = [OrderedFloat(0.0f64); 6];
        transform
            .iter_mut()
            .zip(value.transform.as_coeffs())
            .for_each(|(of, f)| *of = f.into());
        HashableComponent {
            base: value.base.clone(),
            transform,
        }
    }
}

/// Every distinct sequence of components used at some position in designspace.
///
/// The (glyphname, transform) pair is considered for uniqueness.
///
/// Primary use is expected to be checking if there is >1 or not.
fn distinct_component_seqs(glyph: &Glyph) -> HashSet<Vec<HashableComponent>> {
    glyph
        .sources
        .values()
        .map(|inst| {
            inst.components
                .iter()
                .map(HashableComponent::from)
                .collect::<Vec<_>>()
        })
        .collect()
}

/// Every distinct set of components glyph names used at some position in designspace.
///
/// Only the glyph name is considered for uniqueness.
///
/// Primary use is expected to be checking if there is >1 or not.
fn distinct_component_glyph_seqs(glyph: &Glyph) -> HashSet<Vec<GlyphName>> {
    distinct_component_seqs(glyph)
        .into_iter()
        .map(|components| components.into_iter().map(|c| c.base).collect::<Vec<_>>())
        .collect()
}

/// Returns components transformed by the input transform
///
/// Panics if the sequence of glyphs used as components (ignoring transform) is
/// different at any point in design space.
fn components(glyph: &Glyph, transform: Affine) -> VecDeque<(NormalizedLocation, Component)> {
    if glyph.sources.is_empty() {
        return Default::default();
    }

    // Kerplode if the set of unique components is inconsistent across sources
    let component_glyph_seqs = distinct_component_glyph_seqs(glyph);
    if component_glyph_seqs.len() != 1 {
        panic!(
            "'{}' has {} unique sets of components; must have exactly 1\n{:?}",
            glyph.name,
            component_glyph_seqs.len(),
            component_glyph_seqs
        );
    }

    glyph
        .sources
        .iter()
        .flat_map(|(loc, inst)| inst.components.iter().map(|c| (loc.clone(), c)))
        .map(|(loc, component)| {
            (
                loc,
                Component {
                    base: component.base.clone(),
                    transform: transform * component.transform,
                },
            )
        })
        .collect()
}

/// Convert a glyph with contours and components to a contour-only, aka simple, glyph
///
/// At time of writing we only support this if every instance uses the same set of components.
///
/// <https://github.com/googlefonts/ufo2ft/blob/dd738cdcddf61cce2a744d1cafab5c9b33e92dd4/Lib/ufo2ft/util.py#L165>
fn convert_components_to_contours(context: &Context, original: &Glyph) -> Result<Glyph, WorkError> {
    let mut simple = original.clone();
    simple
        .sources
        .iter_mut()
        .for_each(|(_, inst)| inst.components.clear());

    // Component until you can't component no more
    let mut frontier: VecDeque<_> = components(original, Affine::IDENTITY);
    let mut visited: HashSet<(NormalizedLocation, HashableComponent)> = HashSet::new();
    while let Some((loc, component)) = frontier.pop_front() {
        if !visited.insert((loc.clone(), HashableComponent::from(&component))) {
            continue;
        }

        let referenced_glyph = context.get_glyph_ir(&component.base);
        frontier.extend(
            components(&referenced_glyph, component.transform)
                .iter()
                .filter(|(component_loc, _)| *component_loc == loc)
                .cloned(),
        );

        // Any contours of the referenced glyph at this location should be kept
        // For now just fail if the component source locations don't match ours, don't try to interpolate
        trace!("'{0}' retains {component:?} at {loc:?}", original.name);
        let Some(inst) = simple.sources.get_mut(&loc) else {
            return Err(WorkError::GlyphUndefAtNormalizedLocation {
                glyph_name: simple.name.clone(),
                pos: loc.clone(),
            });
        };
        let Some(ref_inst) = referenced_glyph.sources.get(&loc) else {
            return Err(WorkError::GlyphUndefAtNormalizedLocation {
                glyph_name: referenced_glyph.name.clone(),
                pos: loc.clone(),
            });
        };

        for contour in ref_inst.contours.iter() {
            let mut contour = contour.clone();
            contour.apply_affine(component.transform);

            // See https://github.com/googlefonts/ufo2ft/blob/dd738cdcddf61cce2a744d1cafab5c9b33e92dd4/Lib/ufo2ft/util.py#L205
            if component.transform.determinant() < 0.0 {
                let mut bez_pen = BezPathPen::new();
                let mut rev_pen = ReverseContourPen::new(&mut bez_pen);
                write_to_pen(&contour, &mut rev_pen);
                rev_pen
                    .flush()
                    .map_err(|e| WorkError::ContourReversalError(format!("{e:?}")))?;
                inst.contours.push(bez_pen.into_inner());
            } else {
                inst.contours.push(contour);
            }
        }
    }

    Ok(simple)
}

impl Work<Context, WorkError> for FinalizeStaticMetadataWork {
    fn exec(&self, context: &Context) -> Result<(), WorkError> {
        // We should now have access to *all* the glyph IR
        // Some of it may need to be massaged to produce BE glyphs
        // In particular, glyphs with both paths and components need to push the path into a component
        let current_metadata = context.get_static_metadata();
        let mut new_glyph_order = current_metadata.glyph_order.clone();

        // Glyphs with paths and components, and glyphs whose components are transformed vary over designspace
        // are not directly supported in fonts. To resolve we must do one of:
        // 1) need to push their paths to a new glyph that is a component
        // 2) collapse such glyphs into a simple (contour-only) glyph
        // fontmake (Python) prefers option 2.
        for (glyph_to_fix, has_consistent_component_transforms) in current_metadata
            .glyph_order
            .iter()
            .map(|gn| context.get_glyph_ir(gn))
            .map(|glyph| {
                let consistent_transforms = has_consistent_component_transforms(&glyph);
                (glyph, consistent_transforms)
            })
            .filter(|(glyph, has_consistent_component_transforms)| {
                *has_consistent_component_transforms || has_components_and_contours(glyph)
            })
        {
            if !has_consistent_component_transforms || context.match_legacy {
                if !has_consistent_component_transforms {
                    debug!(
                        "Coalescing'{0}' into a simple glyph because component transforms vary across the designspace",
                        glyph_to_fix.name
                    );
                } else {
                    debug!(
                        "Coalescing'{0}' into a simple glyph because it has contours and components and that's how fontmake handles it",
                        glyph_to_fix.name
                    );
                }
                let simple = convert_components_to_contours(context, &glyph_to_fix)?;
                debug_assert!(
                    simple.name == glyph_to_fix.name,
                    "{} != {}",
                    simple.name,
                    glyph_to_fix.name
                );
                context.set_glyph_ir(simple);
            } else {
                // We don't have to match fontmake; prefer to retain components than to collapse to simple glyph
                debug!(
                    "Splitting '{0}' because it has contours and components and we don't have to match legacy",
                    glyph_to_fix.name
                );
                let (simple, composite) = split_glyph(&new_glyph_order, &glyph_to_fix);

                // Capture the updated/new IR and update glyph order
                debug_assert!(composite.name == glyph_to_fix.name);
                debug_assert!(simple.name != glyph_to_fix.name);

                new_glyph_order.insert(simple.name.clone());
                context.set_glyph_ir(simple);
                context.set_glyph_ir(composite);
            }
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::{
        collections::{HashMap, HashSet},
        path::Path,
        sync::Arc,
    };

    use fontdrasil::types::GlyphName;
    use indexmap::IndexSet;
    use kurbo::{Affine, BezPath};
    use write_fonts::pens::{write_to_pen, BezPathPen, ReverseContourPen};

    use crate::{
        coords::{NormalizedCoord, NormalizedLocation},
        ir::{Component, Glyph, GlyphInstance},
        orchestration::{Context, WorkId},
        paths::Paths,
        source::Input,
    };

    use super::{
        convert_components_to_contours, has_components_and_contours, name_for_derivative,
        split_glyph,
    };

    fn norm_loc(positions: &[(&str, f32)]) -> NormalizedLocation {
        positions
            .iter()
            .map(|(axis_name, value)| (axis_name.to_string(), NormalizedCoord::new(*value)))
            .collect()
    }

    fn component_instance() -> GlyphInstance {
        GlyphInstance {
            components: vec![Component {
                base: "component".into(),
                transform: Affine::translate((3.0, 3.0)),
            }],
            ..Default::default()
        }
    }

    fn contour() -> BezPath {
        let mut path = BezPath::new();
        path.move_to((1.0, 1.0));
        path.line_to((2.0, 1.0));
        path.line_to((2.0, 2.0));
        path.close_path();
        path
    }

    fn contour_instance() -> GlyphInstance {
        GlyphInstance {
            contours: vec![contour()],
            ..Default::default()
        }
    }

    fn contour_and_component_instance() -> GlyphInstance {
        GlyphInstance {
            contours: contour_instance().contours,
            components: component_instance().components,
            ..Default::default()
        }
    }

    fn test_root() -> Context {
        Context::new_root(
            false,
            false,
            true,
            Paths::new(Path::new("/fake/path")),
            Input::new(),
        )
    }

    #[test]
    fn has_components_and_contours_false() {
        let glyph = Glyph {
            name: "duck".into(),
            sources: HashMap::from([
                // Just component
                (norm_loc(&[("W", 0.0)]), component_instance()),
                // Just contour
                (norm_loc(&[("W", 1.0)]), contour_instance()),
            ]),
        };
        assert!(!has_components_and_contours(&glyph));
    }

    #[test]
    fn has_components_and_contours_true() {
        let glyph = Glyph {
            name: "duck".into(),
            sources: HashMap::from([(norm_loc(&[("W", 0.0)]), contour_and_component_instance())]),
        };
        assert!(has_components_and_contours(&glyph));
    }

    #[test]
    fn names_for_derivatives() {
        let mut names = IndexSet::new();
        assert_eq!(
            GlyphName::from("duck.0"),
            name_for_derivative(&"duck".into(), &names)
        );

        names.insert(GlyphName::from("mallard"));
        names.insert(GlyphName::from("duck.2"));
        names.insert(GlyphName::from("duck.0"));
        assert_eq!(
            GlyphName::from("duck.1"),
            name_for_derivative(&"duck".into(), &names)
        );
    }

    fn contour_glyph(name: &str) -> Glyph {
        Glyph {
            name: name.into(),
            sources: HashMap::from([
                (norm_loc(&[("W", 0.0)]), contour_instance()),
                (norm_loc(&[("W", 1.0)]), contour_instance()),
            ]),
        }
    }

    fn component_glyph(name: &str, base: GlyphName, transform: Affine) -> Glyph {
        let component = GlyphInstance {
            components: vec![Component { base, transform }],
            ..Default::default()
        };
        Glyph {
            name: name.into(),
            sources: HashMap::from([
                (norm_loc(&[("W", 0.0)]), component.clone()),
                (norm_loc(&[("W", 1.0)]), component),
            ]),
        }
    }

    fn contour_and_component_weight_glyph(name: &str) -> Glyph {
        Glyph {
            name: name.into(),
            sources: HashMap::from([
                (norm_loc(&[("W", 0.0)]), contour_and_component_instance()),
                (norm_loc(&[("W", 1.0)]), contour_and_component_instance()),
            ]),
        }
    }

    fn assert_simple(glyph: &Glyph) {
        assert!(glyph.sources.values().all(|gi| gi.components.is_empty()));
        assert!(glyph.sources.values().all(|gi| !gi.contours.is_empty()));
    }

    #[test]
    fn split_a_glyph() {
        let split_me = contour_and_component_weight_glyph("glyphname");
        let (simple, composite) = split_glyph(&IndexSet::new(), &split_me);

        let expected_locs = split_me.sources.keys().collect::<HashSet<_>>();
        assert_eq!(expected_locs, simple.sources.keys().collect::<HashSet<_>>());
        assert_eq!(
            expected_locs,
            composite.sources.keys().collect::<HashSet<_>>()
        );

        assert_simple(&simple);
        assert!(composite.sources.values().all(|gi| gi.contours.is_empty()));
        assert!(composite
            .sources
            .values()
            .all(|gi| !gi.components.is_empty()));
    }

    #[test]
    fn components_to_contours_shallow() {
        let coalesce_me = contour_and_component_weight_glyph("coalesce_me");

        let context = test_root().copy_for_work(
            Some(HashSet::from([WorkId::Glyph("component".into())])),
            Arc::new(|_| true),
        );
        context.set_glyph_ir(contour_glyph("component"));

        let simple = convert_components_to_contours(&context, &coalesce_me).unwrap();
        assert_simple(&simple);

        // Our sample is unimaginative; both weights are identical
        for (loc, inst) in simple.sources.iter() {
            assert_eq!(
                // The original contour, and the component w/transform
                vec!["M1 1L2 1L2 2Z", "M4 4L5 4L5 5Z",],
                inst.contours
                    .iter()
                    .map(|bez| bez.to_svg())
                    .collect::<Vec<_>>(),
                "At {loc:?}"
            );
        }
    }

    #[test]
    fn components_to_contours_deep() {
        // base shape
        let reuse_me = contour_glyph("shape");
        // add c1, reusing shape w/90 degrees ccw rotation: x-basis 0,1 y-basis -1,0
        let c1 = component_glyph(
            "c1",
            reuse_me.name.clone(),
            Affine::new([0.0, -1.0, 1.0, 0.0, 0.0, 0.0]),
        );
        // add c2, reusing c1 w/translation
        let c2 = component_glyph("c2", c1.name.clone(), Affine::translate((5.0, 0.0)));

        let context = test_root().copy_for_work(
            Some(HashSet::from([
                WorkId::Glyph(reuse_me.name.clone()),
                WorkId::Glyph(c1.name.clone()),
                WorkId::Glyph(c2.name.clone()),
            ])),
            Arc::new(|_| true),
        );
        context.set_glyph_ir(reuse_me);
        context.set_glyph_ir(c1.clone());
        context.set_glyph_ir(c2.clone());

        let nested_components = Glyph {
            name: "g".into(),
            sources: HashMap::from([(
                norm_loc(&[("W", 0.0)]),
                GlyphInstance {
                    components: vec![
                        Component {
                            base: c1.name.clone(),
                            transform: Affine::IDENTITY,
                        },
                        Component {
                            base: c1.name,
                            transform: Affine::translate((0.0, 2.0)),
                        },
                        Component {
                            base: c2.name,
                            transform: Affine::translate((0.0, 5.0)),
                        },
                    ],
                    contours: vec![contour()],
                    ..Default::default()
                },
            )]),
        };
        let simple = convert_components_to_contours(&context, &nested_components).unwrap();
        assert_simple(&simple);
        assert_eq!(1, simple.sources.len());
        let inst = simple.sources.values().next().unwrap();

        assert_eq!(
            vec![
                // The original shape
                "M1 1L2 1L2 2Z",
                // c1: shape rotated 90 degrees ccw
                "M1 -1L1 -2L2 -2Z",
                // c1 moved by (0, 2)
                "M1 1L1 0L2 0Z",
                // c2 moved by (0,5), c2 is c1 moved by (5,0)
                "M6 4L6 3L7 3Z",
            ],
            inst.contours
                .iter()
                .map(|bez| bez.to_svg())
                .collect::<Vec<_>>(),
        );
    }

    #[test]
    fn components_to_contours_retains_direction() {
        // A transform with a negative determinant changes contour direction
        // Our contour conversion should reverse the contour when this occurs
        let the_neg = Affine::new([1.0, 2.0, 2.0, 1.0, 0.0, 0.0]);
        assert!(the_neg.determinant() < 0.0);

        // base shape
        let reuse_me = contour_glyph("shape");

        let glyph = Glyph {
            name: "g".into(),
            sources: HashMap::from([(
                norm_loc(&[("W", 0.0)]),
                GlyphInstance {
                    components: vec![Component {
                        base: reuse_me.name.clone(),
                        transform: the_neg,
                    }],
                    ..Default::default()
                },
            )]),
        };

        let context = test_root().copy_for_work(
            Some(HashSet::from([WorkId::Glyph(reuse_me.name.clone())])),
            Arc::new(|_| true),
        );
        context.set_glyph_ir(reuse_me);

        let simple = convert_components_to_contours(&context, &glyph).unwrap();
        assert_simple(&simple);
        assert_eq!(1, simple.sources.len());
        let inst = simple.sources.values().next().unwrap();

        // what we should get back is the contour with the_neg applied, reversed because
        // the_neg is notoriously negative in determinant
        let mut bez_pen = BezPathPen::new();
        let mut rev_pen = ReverseContourPen::new(&mut bez_pen);
        let mut expected = contour();
        expected.apply_affine(the_neg);
        write_to_pen(&expected, &mut rev_pen);
        rev_pen.flush().unwrap();
        let expected = bez_pen.into_inner().to_svg();

        assert_eq!(
            vec![expected],
            inst.contours
                .iter()
                .map(|bez| bez.to_svg())
                .collect::<Vec<_>>(),
        );
    }

    fn adjust_transform_for_each_instance(glyph: &Glyph) -> Glyph {
        assert!(
            glyph.sources.len() > 1,
            "this operation is meaningless w/o multiple sources"
        );
        let mut glyph = glyph.clone();
        glyph
            .sources
            .values_mut()
            .enumerate()
            .for_each(|(i, inst)| {
                inst.components
                    .iter_mut()
                    .for_each(|c| c.transform *= Affine::translate((i as f64, i as f64)));
            });
        glyph
    }

    #[test]
    fn component_with_varied_transform() {
        let mut glyph = contour_and_component_weight_glyph("nameless");
        glyph
            .sources
            .values_mut()
            .for_each(|inst| inst.contours.clear());
        let glyph = adjust_transform_for_each_instance(&glyph);

        let context = test_root().copy_for_work(
            Some(HashSet::from([WorkId::Glyph("component".into())])),
            Arc::new(|_| true),
        );
        context.set_glyph_ir(contour_glyph("component"));

        let simple = convert_components_to_contours(&context, &glyph).unwrap();
        assert_simple(&simple);
    }

    #[test]
    fn contour_and_component_with_varied_transform() {
        let glyph = contour_and_component_weight_glyph("nameless");
        let glyph = adjust_transform_for_each_instance(&glyph);

        let context = test_root().copy_for_work(
            Some(HashSet::from([WorkId::Glyph("component".into())])),
            Arc::new(|_| true),
        );
        context.set_glyph_ir(contour_glyph("component"));

        let simple = convert_components_to_contours(&context, &glyph).unwrap();
        assert_simple(&simple);
    }
}
