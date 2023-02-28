//! IR glyph processing.
//!
//! Notably includes splitting glyphs with contours and components into one new glyph with
//! the contours and one updated glyph with no contours that references the new gyph as a component.

use std::collections::{BTreeSet, HashSet, VecDeque};

use fontdrasil::{orchestration::Work, types::GlyphName};
use indexmap::IndexSet;
use kurbo::Affine;
use log::{debug, trace};
use ordered_float::OrderedFloat;

use crate::{
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

/// Returns a Vec of Components transformed by transform.
///
/// If glyph components are not the same for all instances, panics.
fn consistent_components(glyph: &Glyph, transform: Affine) -> VecDeque<Component> {
    if glyph.sources.is_empty() {
        return Default::default();
    }

    // Kerplode if the set of unique components is inconsistent across sources
    let unique_components: HashSet<_> = glyph
        .sources
        .values()
        .map(|inst| {
            inst.components
                .iter()
                .map(HashableComponent::from)
                .collect::<BTreeSet<_>>()
        })
        .collect();
    if unique_components.len() != 1 {
        panic!(
            "'{}' has {} unique sets of components; must have exactly 1",
            glyph.name,
            unique_components.len()
        );
    }

    // We have at least 1 source and all sources have the same components
    // so take the components from the first source
    glyph
        .sources
        .values()
        .take(1)
        .flat_map(|inst| inst.components.iter())
        .map(|c| Component {
            base: c.base.clone(),
            transform: transform * c.transform,
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
    let mut frontier: VecDeque<_> = consistent_components(original, Affine::IDENTITY);
    let mut visited: HashSet<HashableComponent> = HashSet::new();
    while let Some(component) = frontier.pop_front() {
        let hashable = HashableComponent::from(&component);
        if !visited.insert(hashable) {
            continue;
        }

        let referenced_glyph = context.get_glyph_ir(&component.base);
        frontier.extend(
            consistent_components(&referenced_glyph, component.transform)
                .iter()
                .cloned(),
        );

        // Any contours of the referenced glyph should be kept
        // For now just fail if the component source locations don't match ours, don't try to interpolate
        trace!("'{0}' retains {component:?}", original.name);
        for (loc, inst) in simple.sources.iter_mut() {
            let ref_inst = referenced_glyph.sources.get(loc).ok_or_else(|| {
                WorkError::GlyphUndefAtNormalizedLocation {
                    glyph_name: component.base.clone(),
                    pos: loc.clone(),
                }
            })?;
            for contour in ref_inst.contours.iter() {
                let mut contour = contour.clone();
                contour.apply_affine(component.transform);
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

        // Glyphs with paths and components need to push their paths to a new glyph that is a component
        // OR to collapse such glyphs into a simple (contour-only) glyph to match existing fontmake behavior.
        for glyph_to_fix in current_metadata
            .glyph_order
            .iter()
            .map(|gn| context.get_glyph_ir(gn))
            .filter(|glyph| has_components_and_contours(glyph))
        {
            if context.match_legacy {
                debug!(
                    "Coalescing'{0}' into a simple glyph because it has contours and components and that's how fontmake handles it",
                    glyph_to_fix.name
                );
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
        todo!("reverse contours sometimes maybe")
    }
}
