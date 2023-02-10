//! IR glyph processing.
//!
//! Notably includes splitting glyphs with contours and components into one new glyph with
//! the contours and one updated glyph with no contours that references the new gyph as a component.

use fontdrasil::{orchestration::Work, types::GlyphName};
use indexmap::IndexSet;
use kurbo::Affine;
use log::debug;

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
fn should_split(glyph: &Glyph) -> bool {
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

impl Work<Context, WorkError> for FinalizeStaticMetadataWork {
    fn exec(&self, context: &Context) -> Result<(), WorkError> {
        // We should now have access to *all* the glyph IR
        // Some of it may need to be massaged to produce BE glyphs
        // In particular, glyphs with both paths and components need to push the path into a component
        let current_metadata = context.get_static_metadata();
        let mut new_glyph_order = current_metadata.glyph_order.clone();

        // Glyphs with paths and components need to push their paths to a new glyph that is a component
        for glyph_to_split in current_metadata
            .glyph_order
            .iter()
            .map(|gn| context.get_glyph_ir(gn))
            .filter(|glyph| should_split(glyph))
        {
            debug!(
                "Splitting '{0}' because it has contours and components",
                glyph_to_split.name
            );
            let (simple, composite) = split_glyph(&new_glyph_order, &glyph_to_split);

            // Capture the updated/new IR and update glyph order
            debug_assert!(composite.name == glyph_to_split.name);
            debug_assert!(simple.name != glyph_to_split.name);

            new_glyph_order.insert(simple.name.clone());
            context.set_glyph_ir(simple.name.clone(), simple);
            context.set_glyph_ir(composite.name.clone(), composite);
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::collections::{HashMap, HashSet};

    use fontdrasil::types::GlyphName;
    use indexmap::IndexSet;
    use kurbo::{Affine, BezPath};

    use crate::{
        coords::{NormalizedCoord, NormalizedLocation},
        ir::{Component, Glyph, GlyphInstance},
    };

    use super::{name_for_derivative, should_split, split_glyph};

    fn norm_loc(positions: &[(&str, f32)]) -> NormalizedLocation {
        positions
            .iter()
            .map(|(axis_name, value)| (axis_name.to_string(), NormalizedCoord::new(*value)))
            .collect()
    }

    fn component_instance() -> GlyphInstance {
        GlyphInstance {
            components: vec![Component {
                base: "my_component".into(),
                transform: Affine::IDENTITY,
            }],
            ..Default::default()
        }
    }

    fn contour_instance() -> GlyphInstance {
        GlyphInstance {
            contours: vec![BezPath::new()],
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

    #[test]
    fn contours_xor_components_nop() {
        let glyph = Glyph {
            name: "duck".into(),
            sources: HashMap::from([
                // Just component
                (norm_loc(&[("W", 0.0)]), component_instance()),
                // Just contour
                (norm_loc(&[("W", 1.0)]), contour_instance()),
            ]),
        };
        assert!(!should_split(&glyph));
    }

    #[test]
    fn contours_and_components_split() {
        let glyph = Glyph {
            name: "duck".into(),
            sources: HashMap::from([(norm_loc(&[("W", 0.0)]), contour_and_component_instance())]),
        };
        assert!(should_split(&glyph));
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

    #[test]
    fn split_a_glyph() {
        let split_me = Glyph {
            name: "duck".into(),
            sources: HashMap::from([
                (norm_loc(&[("W", 0.0)]), contour_and_component_instance()),
                (norm_loc(&[("W", 1.0)]), contour_and_component_instance()),
            ]),
        };
        let (simple, composite) = split_glyph(&IndexSet::new(), &split_me);

        let expected_locs = split_me.sources.keys().collect::<HashSet<_>>();
        assert_eq!(expected_locs, simple.sources.keys().collect::<HashSet<_>>());
        assert_eq!(
            expected_locs,
            composite.sources.keys().collect::<HashSet<_>>()
        );

        assert!(simple.sources.values().all(|gi| gi.components.is_empty()));
        assert!(simple.sources.values().all(|gi| !gi.contours.is_empty()));
        assert!(composite.sources.values().all(|gi| gi.contours.is_empty()));
        assert!(composite
            .sources
            .values()
            .all(|gi| !gi.components.is_empty()));
    }
}
