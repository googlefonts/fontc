//! IR glyph processing.
//!
//! Notably includes splitting glyphs with contours and components into one new glyph with
//! the contours and one updated glyph with no contours that references the new gyph as a component.

use std::{
    collections::{HashMap, HashSet, VecDeque},
    sync::Arc,
};

use fontdrasil::{
    coords::NormalizedLocation,
    orchestration::{Access, AccessBuilder, Work},
    types::GlyphName,
};
use kurbo::Affine;
use log::{debug, log_enabled, trace};
use ordered_float::OrderedFloat;
use write_fonts::types::GlyphId16;

use crate::{
    error::{BadGlyph, BadGlyphKind, Error},
    ir::{Component, Glyph, GlyphBuilder, GlyphOrder},
    orchestration::{Context, Flags, IrWork, WorkId},
};

pub fn create_glyph_order_work() -> Box<IrWork> {
    Box::new(GlyphOrderWork {})
}

#[derive(Debug)]
struct GlyphOrderWork {}

/// Glyph should split if it has components *and* contours.
///
/// Such a glyph turns into a simple glyf with the contours and a
/// composite glyph that references the simple glyph as a component.
///
/// <https://learn.microsoft.com/en-us/typography/opentype/spec/glyf>
fn has_components_and_contours(glyph: &Glyph) -> bool {
    glyph
        .sources()
        .values()
        .any(|inst| !inst.components.is_empty() && !inst.contours.is_empty())
}

fn name_for_derivative(base_name: &GlyphName, names_in_use: &GlyphOrder) -> GlyphName {
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

/// Returns a tuple of (simple glyph, composite glyph).
///
/// The former contains all the contours, the latter contains all the components.
fn split_glyph(glyph_order: &GlyphOrder, original: &Glyph) -> Result<(Glyph, Glyph), BadGlyph> {
    // Make a simple glyph by erasing the components from it
    let mut simple_glyph = GlyphBuilder::from(original.clone());
    simple_glyph.sources.iter_mut().for_each(|(_, inst)| {
        inst.components.clear();
    });
    simple_glyph.codepoints.clear();

    // Find a free name for the contour glyph
    let simple_glyph_name = name_for_derivative(&original.name, glyph_order);
    simple_glyph.name = simple_glyph_name.clone();

    // Use the contour glyph as a component in the original glyph and erase it's contours
    let mut composite_glyph = GlyphBuilder::from(original.clone());
    composite_glyph.sources.iter_mut().for_each(|(_, inst)| {
        inst.contours.clear();
        inst.components.push(Component {
            base: simple_glyph_name.clone(),
            transform: Affine::IDENTITY,
        });
    });

    Ok((simple_glyph.build()?, composite_glyph.build()?))
}

/// Component with full transform.
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct HashableComponent {
    base: GlyphName,
    transform: [OrderedFloat<f64>; 6],
    // <https://github.com/googlefonts/fontc/issues/1115> permit multiple identical instantiations
    index: usize,
}

impl HashableComponent {
    fn affine(&self) -> Affine {
        Affine::new([
            self.transform[0].0,
            self.transform[1].0,
            self.transform[2].0,
            self.transform[3].0,
            self.transform[4].0,
            self.transform[5].0,
        ])
    }
}

/// Every distinct set of components glyph names used at some position in designspace.
///
/// Only the glyph name is considered for uniqueness.
///
/// Primary use is expected to be checking if there is >1 or not.
fn distinct_component_glyph_seqs(glyph: &Glyph) -> HashSet<Vec<GlyphName>> {
    glyph
        .sources()
        .values()
        .map(|inst| {
            inst.components
                .iter()
                .map(|c| c.base.clone())
                .collect::<Vec<_>>()
        })
        .collect()
}

/// Returns components transformed by the input transform
///
/// Panics if the sequence of glyphs used as components (ignoring transform) is
/// different at any point in design space.
fn components(
    glyph: &Glyph,
    transform: Affine,
) -> VecDeque<(NormalizedLocation, HashableComponent)> {
    if glyph.sources().is_empty() {
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
        .sources()
        .iter()
        .flat_map(|(loc, inst)| inst.components.iter().map(|c| (loc.clone(), c)))
        .enumerate()
        .map(|(index, (loc, component))| {
            let coeffs = (transform * component.transform).as_coeffs();
            let mut transform = [OrderedFloat(0f64); 6];
            transform
                .iter_mut()
                .zip(coeffs)
                .for_each(|(t, c)| *t = OrderedFloat(c));
            (
                loc,
                HashableComponent {
                    base: component.base.clone(),
                    transform,
                    index,
                },
            )
        })
        .collect()
}

// Operations performed on glyphs with mixed contours/components
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum GlyphOp {
    // convert comonents in this mixed glyph into contours
    ConvertToContour,
    // move contours in this mixed glyph into components
    MoveContoursToComponent,
}

/// Fix glyphs with mixed components/contours.
///
/// We presume component cycles are checked elsewhere and do not check for them here
fn resolve_inconsistencies(
    context: &Context,
    mut todo: VecDeque<(GlyphOp, Arc<Glyph>)>,
    order: &mut GlyphOrder,
    mut apply_fix: impl FnMut(GlyphOp, &Glyph, &mut GlyphOrder) -> Result<(), BadGlyph>,
) -> Result<(), BadGlyph> {
    let mut pending = todo
        .iter()
        .map(|(_, g)| g.name.clone())
        .collect::<HashSet<_>>();
    let mut curr_components = Vec::with_capacity(8); // avoid inner reallocs
    'next_todo: while let Some((op, glyph)) = todo.pop_front() {
        // Only fix curr if nothing else that needs fixing is reachable
        curr_components.clear();
        curr_components.extend(glyph.component_names().cloned());
        while let Some(component_name) = curr_components.pop() {
            if pending.contains(&component_name) {
                // We can't got yet
                todo.push_back((op, glyph));
                continue 'next_todo;
            }
            let glyph = context.glyphs.get(&WorkId::Glyph(component_name));
            curr_components.extend(glyph.component_names().cloned());
        }

        // Our component graph doesn't reach any pending component, we are a go!
        debug!("{op:?} {}", glyph.name);
        apply_fix(op, &glyph, order)?;

        // I ain't a-gonna pend no more
        pending.remove(&glyph.name);
    }

    Ok(())
}

/// Convert a glyph with contours and components to a contour-only, aka simple, glyph
///
/// At time of writing we only support this if every instance uses the same set of components.
///
/// <https://github.com/googlefonts/ufo2ft/blob/dd738cdcddf61cce2a744d1cafab5c9b33e92dd4/Lib/ufo2ft/util.py#L165>
fn convert_components_to_contours(context: &Context, original: &Glyph) -> Result<(), BadGlyph> {
    let mut simple = GlyphBuilder::from(original.clone());
    simple
        .sources
        .iter_mut()
        .for_each(|(_, inst)| inst.components.clear());

    // Component until you can't component no more
    let mut frontier: VecDeque<_> = components(original, Affine::IDENTITY);
    // Note that here we care about the entire component transform
    let mut visited: HashSet<(NormalizedLocation, HashableComponent)> = HashSet::new();
    while let Some((loc, component)) = frontier.pop_front() {
        let component_base = component.base.clone();
        let component_affine = component.affine();
        if !visited.insert((loc.clone(), component)) {
            continue;
        }

        let referenced_glyph = context.get_glyph(component_base.clone());
        frontier.extend(
            components(&referenced_glyph, component_affine)
                .iter()
                .filter(|(component_loc, _)| *component_loc == loc)
                .cloned(),
        );

        // Any contours of the referenced glyph at this location should be kept
        // For now just fail if the component source locations don't match ours, don't try to interpolate
        trace!(
            "'{}' retains {} {component_affine:?} at {loc:?}",
            original.name,
            referenced_glyph.name
        );
        let Some(inst) = simple.sources.get_mut(&loc) else {
            return Err(BadGlyph::new(
                simple.name.clone(),
                BadGlyphKind::UndefinedAtNormalizedLocation(loc.clone()),
            ));
        };
        let Some(ref_inst) = referenced_glyph.sources().get(&loc) else {
            return Err(BadGlyph::new(
                referenced_glyph.name.clone(),
                BadGlyphKind::UndefinedAtNormalizedLocation(loc.clone()),
            ));
        };

        for contour in ref_inst.contours.iter() {
            let mut contour = contour.clone();
            contour.apply_affine(component_affine);

            // See https://github.com/googlefonts/ufo2ft/blob/dd738cdcddf61cce2a744d1cafab5c9b33e92dd4/Lib/ufo2ft/util.py#L205
            if component_affine.determinant() < 0.0 {
                inst.contours.push(contour.reverse_subpaths());
            } else {
                inst.contours.push(contour);
            }
        }
    }

    let simple: Glyph = simple.build()?;
    debug_assert!(
        simple.name == original.name,
        "{} != {}",
        simple.name,
        original.name
    );
    context.glyphs.set(simple);
    Ok(())
}

fn move_contours_to_new_component(
    context: &Context,
    new_glyph_order: &mut GlyphOrder,
    glyph: &Glyph,
) -> Result<(), BadGlyph> {
    debug!(
        "Hoisting the contours from '{0}' into a new component",
        glyph.name
    );
    let (simple, composite) = split_glyph(new_glyph_order, glyph)?;

    // Capture the updated/new IR and update glyph order
    debug_assert!(composite.name == glyph.name);
    debug_assert!(simple.name != glyph.name);

    new_glyph_order.insert(simple.name.clone());
    context.glyphs.set(simple);
    context.glyphs.set(composite);
    Ok(())
}

/// Make sure components only reference simple (contour) glyphs.
///
/// Assumed to run after component consistency is checked/fixed so we can assume
/// that no mixed contour+component glyphs exist.
///
/// See <https://github.com/googlefonts/ufo2ft/blob/main/Lib/ufo2ft/filters/flattenComponents.py>
fn flatten_glyph(context: &Context, glyph: &Glyph) -> Result<(), BadGlyph> {
    // Guard: nothing to see here folks
    if glyph.default_instance().components.is_empty() {
        return Ok(());
    }
    trace!(
        "Flatten {} {:?}",
        glyph.name,
        glyph.default_instance().components
    );
    let mut glyph = glyph.clone();
    for (loc, inst) in glyph.sources_mut() {
        let mut simple = Vec::new();
        let mut frontier = VecDeque::new();
        frontier.extend(inst.components.split_off(0));
        while let Some(component) = frontier.pop_front() {
            let ref_glyph = context.get_glyph(component.base.clone());
            let ref_inst = ref_glyph.sources().get(loc).ok_or_else(|| {
                BadGlyph::new(
                    ref_glyph.name.clone(),
                    BadGlyphKind::UndefinedAtNormalizedLocation(loc.clone()),
                )
            })?;
            if ref_inst.components.is_empty() {
                simple.push(component.clone());
            } else {
                for ref_component in ref_inst.components.iter().rev() {
                    frontier.push_front(Component {
                        base: ref_component.base.clone(),
                        transform: component.transform * ref_component.transform,
                    });
                }
            }
        }
        inst.components = simple;
    }
    trace!(
        "Flattened {} to {:?}",
        glyph.name,
        glyph.default_instance().components
    );
    context.glyphs.set(glyph);
    Ok(())
}

/// Run some optional transformations on the glyphs listed.
///
/// This includes decomposing all components, or only those with non-identity
/// 2x2 transforms, and flattening nested composite glyphs so that they all
/// have depth 1 (no components that reference components).
fn apply_optional_transformations(
    context: &Context,
    glyph_order: &GlyphOrder,
) -> Result<(), BadGlyph> {
    // If we are decomposing all components, the rest of the flags can be ignored
    if context.flags.contains(Flags::DECOMPOSE_COMPONENTS) {
        for glyph_name in glyph_order.names() {
            let glyph = context.get_glyph(glyph_name.clone());
            if !glyph.default_instance().components.is_empty() {
                convert_components_to_contours(context, &glyph)?;
            }
        }
        return Ok(());
    }

    // If both --flatten-components and --decompose-transformed-components flags
    // are set, we want to decompose any transformed components first and *then*
    // flatten the rest. That's how fontmake (ufo2ft) does, and also tends to
    // keep more components, which usually means smaller glyf size.
    // https://github.com/googlefonts/fontc/issues/929
    if context
        .flags
        .contains(Flags::DECOMPOSE_TRANSFORMED_COMPONENTS)
    {
        for glyph_name in glyph_order.names() {
            let glyph = context.get_glyph(glyph_name.clone());
            if glyph.has_nonidentity_2x2() {
                convert_components_to_contours(context, &glyph)?;
            }
        }
    }

    if context.flags.contains(Flags::FLATTEN_COMPONENTS) {
        for glyph_name in glyph_order.names() {
            let glyph = context.get_glyph(glyph_name.clone());
            flatten_glyph(context, &glyph)?;
        }
    }

    Ok(())
}

fn ensure_notdef_exists_and_is_gid_0(
    context: &Context,
    glyph_order: &mut GlyphOrder,
) -> Result<(), BadGlyph> {
    // Make sure we have a .notdef and that it's gid 0
    match glyph_order.glyph_id(&GlyphName::NOTDEF) {
        Some(GlyphId16::NOTDEF) => (), // .notdef is gid 0; life is good
        Some(..) => {
            trace!("Move {} to gid 0", GlyphName::NOTDEF);
            glyph_order.set_glyph_id(&GlyphName::NOTDEF, 0);
        }
        None => {
            trace!("Generate {} and make it gid 0", GlyphName::NOTDEF);
            glyph_order.set_glyph_id(&GlyphName::NOTDEF, 0);
            let static_metadata = context.static_metadata.get();
            let metrics = context
                .global_metrics
                .get()
                .at(static_metadata.default_location());
            let builder = GlyphBuilder::new_notdef(
                static_metadata.default_location().clone(),
                static_metadata.units_per_em,
                metrics.ascender.0,
                metrics.descender.0,
            );
            context.glyphs.set(builder.build()?);
        }
    }
    Ok(())
}

impl Work<Context, WorkId, Error> for GlyphOrderWork {
    fn id(&self) -> WorkId {
        WorkId::GlyphOrder
    }

    fn read_access(&self) -> Access<WorkId> {
        AccessBuilder::new()
            .variant(WorkId::StaticMetadata)
            .variant(WorkId::PreliminaryGlyphOrder)
            .variant(WorkId::GlobalMetrics)
            .variant(WorkId::ALL_GLYPHS)
            .build()
    }

    fn write_access(&self) -> Access<WorkId> {
        AccessBuilder::new()
            .variant(WorkId::GlyphOrder)
            .variant(WorkId::ALL_GLYPHS)
            .build()
    }

    fn exec(&self, context: &Context) -> Result<(), Error> {
        // We should now have access to *all* the glyph IR
        // Some of it may need to be massaged to produce BE glyphs
        // In particular, glyphs with both paths and components need to push the path into a component
        let arc_current = context.preliminary_glyph_order.get();
        let current_glyph_order = &*arc_current;
        let original_glyphs: HashMap<_, _> = current_glyph_order
            .names()
            .map(|gn| (gn, context.get_glyph(gn.clone())))
            .collect();

        // Anything the source specifically said not to retain shouldn't end up in the final font
        let mut new_glyph_order = current_glyph_order.clone();
        for glyph_name in current_glyph_order.names() {
            let glyph = original_glyphs.get(glyph_name).unwrap();
            if !glyph.emit_to_binary {
                new_glyph_order.remove(glyph_name);
            }
        }

        // Resolve component references to glyphs that are not retained by conversion to contours
        // Glyphs have to have consistent components at this point so it's safe to just check the default
        // See https://github.com/googlefonts/fontc/issues/532
        for glyph_name in new_glyph_order.names() {
            // We are only int
            let glyph = context.get_glyph(glyph_name.clone());
            for component in glyph.default_instance().components.iter() {
                if !new_glyph_order.contains(&component.base) {
                    convert_components_to_contours(context, &glyph)?;
                }
            }
        }

        // Glyphs with paths and components, and glyphs whose component 2x2
        // transforms vary over designspace are not directly supported in fonts.
        // To resolve we must do one of:
        // 1) need to push their paths to a new glyph that is a component
        // 2) collapse such glyphs into a simple (contour-only) glyph
        //    fontmake (Python) prefers option 2.
        let mut todo = VecDeque::new();
        for glyph_name in new_glyph_order.names() {
            let glyph = original_glyphs.get(glyph_name).unwrap();
            if !glyph.has_consistent_components() {
                debug!(
                    "Coalescing '{glyph_name}' into a simple glyph because \
                        component 2x2s vary across the designspace"
                );
                todo.push_back((GlyphOp::ConvertToContour, glyph.clone()));
            } else if has_components_and_contours(glyph) {
                if context.flags.contains(Flags::PREFER_SIMPLE_GLYPHS) {
                    todo.push_back((GlyphOp::ConvertToContour, glyph.clone()));
                    debug!(
                        "Coalescing '{glyph_name} into a simple glyph; it has \
                            contours and components and prefer simple glyphs is set",
                    );
                } else {
                    todo.push_back((GlyphOp::MoveContoursToComponent, glyph.clone()));
                }
            }
        }
        resolve_inconsistencies(
            context,
            todo,
            &mut new_glyph_order,
            |op, glyph, order| match op {
                GlyphOp::ConvertToContour => convert_components_to_contours(context, glyph),
                GlyphOp::MoveContoursToComponent => {
                    move_contours_to_new_component(context, order, glyph)
                }
            },
        )?;
        drop(original_glyphs); // lets not accidentally use that from here on

        apply_optional_transformations(context, &new_glyph_order)?;

        ensure_notdef_exists_and_is_gid_0(context, &mut new_glyph_order)?;

        // We now have the final static metadata
        // If the glyph order changed try not to forget about it
        if *current_glyph_order != new_glyph_order {
            if log_enabled!(log::Level::Trace) {
                let mut new_glyphs: Vec<_> =
                    new_glyph_order.difference(current_glyph_order).collect();
                new_glyphs.sort();
                trace!(
                    "Added {} additional glyphs: {new_glyphs:?}",
                    new_glyphs.len()
                )
            }
        } else {
            trace!("No new glyphs, final glyph order == preliminary glyph order");
        }

        context.glyph_order.set(new_glyph_order);
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::{collections::HashSet, path::Path};

    use fontdrasil::{orchestration::Access, types::GlyphName};
    use kurbo::{Affine, BezPath};

    use crate::{
        ir::{Component, Glyph, GlyphBuilder, GlyphInstance, GlyphOrder},
        orchestration::{Context, Flags, WorkId},
        paths::Paths,
    };

    use super::*;

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

    fn test_context() -> Context {
        let mut flags = Flags::default();
        flags.set(Flags::EMIT_IR, false); // we don't want to write anything down
        Context::new_root(flags, Paths::new(Path::new("/fake/path")))
            .copy_for_work(Access::All, Access::All)
    }

    struct DeepComponent {
        simple_glyph: Glyph,
        // Reuses simple_glyph
        shallow_component: Glyph,
        // Reuses shallow_component
        deep_component: Glyph,
    }

    impl DeepComponent {
        fn write_to(&self, context: &Context) {
            context.glyphs.set(self.simple_glyph.clone());
            context.glyphs.set(self.shallow_component.clone());
            context.glyphs.set(self.deep_component.clone());
        }

        fn glyph_order(&self) -> GlyphOrder {
            let mut order = GlyphOrder::new();
            order.insert(self.simple_glyph.name.clone());
            order.insert(self.shallow_component.name.clone());
            order.insert(self.deep_component.name.clone());
            order
        }
    }

    fn deep_component() -> DeepComponent {
        let simple_glyph_name = "shape";
        let shallow_component_name = "c1";
        DeepComponent {
            // base shape
            simple_glyph: contour_glyph(simple_glyph_name),
            // add c1, reusing shape w/90 degrees ccw rotation: x-basis 0,1 y-basis -1,0
            shallow_component: component_glyph(
                shallow_component_name,
                simple_glyph_name.into(),
                Affine::new([0.0, -1.0, 1.0, 0.0, 0.0, 0.0]),
            ),
            // add c2, reusing c1 w/translation
            deep_component: component_glyph(
                "c2",
                shallow_component_name.into(),
                Affine::translate((5.0, 0.0)),
            ),
        }
    }

    #[test]
    fn has_components_and_contours_false() {
        let mut glyph = GlyphBuilder::new("duck".into());
        glyph
            .try_add_source(
                &NormalizedLocation::for_pos(&[("wght", 0.0)]),
                component_instance(),
            )
            .unwrap();
        glyph
            .try_add_source(
                &NormalizedLocation::for_pos(&[("wght", 1.0)]),
                contour_instance(),
            )
            .unwrap();
        let glyph = glyph.build().unwrap();
        assert!(!has_components_and_contours(&glyph));
    }

    #[test]
    fn has_components_and_contours_true() {
        let mut glyph = GlyphBuilder::new("duck".into());
        glyph
            .try_add_source(
                &NormalizedLocation::for_pos(&[("wght", 0.0)]),
                contour_and_component_instance(),
            )
            .unwrap();
        let glyph = glyph.build().unwrap();
        assert!(has_components_and_contours(&glyph));
    }

    #[test]
    fn names_for_derivatives() {
        let mut names = GlyphOrder::new();
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
        let mut glyph = GlyphBuilder::new(name.into());
        glyph
            .try_add_source(
                &NormalizedLocation::for_pos(&[("wght", 0.0)]),
                contour_instance(),
            )
            .unwrap();
        glyph
            .try_add_source(
                &NormalizedLocation::for_pos(&[("wght", 1.0)]),
                contour_instance(),
            )
            .unwrap();
        glyph.build().unwrap()
    }

    fn component_glyph(name: &str, base: GlyphName, transform: Affine) -> Glyph {
        let component = GlyphInstance {
            components: vec![Component { base, transform }],
            ..Default::default()
        };
        let mut glyph = GlyphBuilder::new(name.into());
        glyph
            .try_add_source(
                &NormalizedLocation::for_pos(&[("wght", 0.0)]),
                component.clone(),
            )
            .unwrap();
        glyph
            .try_add_source(&NormalizedLocation::for_pos(&[("wght", 1.0)]), component)
            .unwrap();
        glyph.build().unwrap()
    }

    fn contour_and_component_weight_glyph(name: &str) -> Glyph {
        let mut glyph = GlyphBuilder::new(name.into());
        glyph
            .try_add_source(
                &NormalizedLocation::for_pos(&[("wght", 0.0)]),
                contour_and_component_instance(),
            )
            .unwrap();
        glyph
            .try_add_source(
                &NormalizedLocation::for_pos(&[("wght", 1.0)]),
                contour_and_component_instance(),
            )
            .unwrap();
        glyph.build().unwrap()
    }

    fn assert_simple(glyph: &Glyph) {
        assert!(glyph.sources().values().all(|gi| gi.components.is_empty()));
        assert!(glyph.sources().values().all(|gi| !gi.contours.is_empty()));
    }

    #[test]
    fn split_a_glyph() {
        let split_me = contour_and_component_weight_glyph("glyphname");
        let (simple, composite) = split_glyph(&GlyphOrder::new(), &split_me).unwrap();

        let expected_locs = split_me.sources().keys().collect::<HashSet<_>>();
        assert_eq!(
            expected_locs,
            simple.sources().keys().collect::<HashSet<_>>()
        );
        assert_eq!(
            expected_locs,
            composite.sources().keys().collect::<HashSet<_>>()
        );

        assert_simple(&simple);
        assert!(composite
            .sources()
            .values()
            .all(|gi| gi.contours.is_empty()));
        assert!(composite
            .sources()
            .values()
            .all(|gi| !gi.components.is_empty()));
    }

    #[test]
    fn components_to_contours_shallow() {
        let coalesce_me = contour_and_component_weight_glyph("coalesce_me");

        let context = test_context();
        context.glyphs.set(contour_glyph("component"));

        convert_components_to_contours(&context, &coalesce_me).unwrap();
        let simple = context.get_glyph(coalesce_me.name.clone());
        assert_simple(&simple);

        // Our sample is unimaginative; both weights are identical
        for (loc, inst) in simple.sources().iter() {
            assert_eq!(
                // The original contour, and the component w/transform
                vec!["M1,1 L2,1 L2,2 Z", "M4,4 L5,4 L5,5 Z",],
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
        let test_data = deep_component();
        let context = test_context();
        test_data.write_to(&context);

        let mut nested_components = GlyphBuilder::new("g".into());
        nested_components
            .try_add_source(
                &NormalizedLocation::for_pos(&[("wght", 0.0)]),
                GlyphInstance {
                    components: vec![
                        Component {
                            base: test_data.shallow_component.name.clone(),
                            transform: Affine::IDENTITY,
                        },
                        Component {
                            base: test_data.shallow_component.name,
                            transform: Affine::translate((0.0, 2.0)),
                        },
                        Component {
                            base: test_data.deep_component.name,
                            transform: Affine::translate((0.0, 5.0)),
                        },
                    ],
                    contours: vec![contour()],
                    ..Default::default()
                },
            )
            .unwrap();
        let nested_components = nested_components.build().unwrap();

        convert_components_to_contours(&context, &nested_components).unwrap();
        let simple = context.get_glyph(nested_components.name.clone());
        assert_simple(&simple);
        assert_eq!(1, simple.sources().len());
        let inst = simple.default_instance();

        assert_eq!(
            vec![
                // The original shape
                "M1,1 L2,1 L2,2 Z",
                // c1: shape rotated 90 degrees ccw
                "M1,-1 L1,-2 L2,-2 Z",
                // c1 moved by (0, 2)
                "M1,1 L1,0 L2,0 Z",
                // c2 moved by (0,5), c2 is c1 moved by (5,0)
                "M6,4 L6,3 L7,3 Z",
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

        let mut glyph = GlyphBuilder::new("g".into());
        glyph
            .try_add_source(
                &NormalizedLocation::for_pos(&[("wght", 0.0)]),
                GlyphInstance {
                    components: vec![Component {
                        base: reuse_me.name.clone(),
                        transform: the_neg,
                    }],
                    ..Default::default()
                },
            )
            .unwrap();

        let context = test_context();
        context.glyphs.set(reuse_me);

        let glyph = glyph.build().unwrap();
        convert_components_to_contours(&context, &glyph).unwrap();
        let simple = context.get_glyph(glyph.name.clone());
        assert_simple(&simple);
        assert_eq!(1, simple.sources().len());
        let inst = simple.sources().values().next().unwrap();

        // what we should get back is the contour with the_neg applied, reversed because
        // the_neg is notoriously negative in determinant
        let mut expected = contour();
        expected.apply_affine(the_neg);
        let expected = expected.reverse_subpaths().to_svg();

        assert_eq!(
            vec![expected],
            inst.contours
                .iter()
                .map(|bez| bez.to_svg())
                .collect::<Vec<_>>(),
        );
    }

    fn adjust_transform_for_each_instance(
        glyph: &Glyph,
        adjust_nth: impl Fn(usize) -> Affine,
    ) -> Glyph {
        assert!(
            glyph.sources().len() > 1,
            "this operation is meaningless w/o multiple sources"
        );
        let mut glyph = GlyphBuilder::from(glyph.clone());
        glyph
            .sources
            .values_mut()
            .enumerate()
            .for_each(|(i, inst)| {
                inst.components
                    .iter_mut()
                    .for_each(|c| c.transform *= adjust_nth(i));
            });
        glyph.build().unwrap()
    }

    #[test]
    fn component_with_varied_transform() {
        let glyph = contour_and_component_weight_glyph("nameless");
        let mut glyph: GlyphBuilder = glyph.into();
        glyph
            .sources
            .values_mut()
            .for_each(|inst| inst.contours.clear());
        let glyph = adjust_transform_for_each_instance(&glyph.build().unwrap(), |i| {
            Affine::scale(i as f64)
        });

        let context = test_context();
        context.glyphs.set(contour_glyph("component"));

        convert_components_to_contours(&context, &glyph).unwrap();
        let simple = context.get_glyph(glyph.name.clone());
        assert_simple(&simple);
    }

    #[test]
    fn contour_and_component_with_varied_transform() {
        let glyph = contour_and_component_weight_glyph("nameless");
        let glyph = adjust_transform_for_each_instance(&glyph, |i| Affine::scale(i as f64));

        let context = test_context();
        context.glyphs.set(contour_glyph("component"));

        convert_components_to_contours(&context, &glyph).unwrap();
        let simple = context.get_glyph(glyph.name.clone());
        assert_simple(&simple);
    }

    #[test]
    fn varied_translation_is_ok() {
        let glyph = contour_and_component_weight_glyph("nameless");
        let glyph =
            adjust_transform_for_each_instance(&glyph, |i| Affine::translate((i as f64, i as f64)));
        assert!(glyph.has_consistent_components());
    }

    fn assert_is_simple_glyph(context: &Context, glyph_name: GlyphName) {
        let glyph = context.get_glyph(glyph_name);
        assert!(glyph
            .sources()
            .values()
            .all(|inst| !inst.contours.is_empty() && inst.components.is_empty()));
    }

    fn assert_is_flattened_component(context: &Context, glyph_name: GlyphName) {
        let glyph = context.get_glyph(glyph_name);
        for (loc, inst) in glyph.sources().iter() {
            assert!(!inst.components.is_empty());
            for component in inst.components.iter() {
                assert!(context
                    .glyphs
                    .get(&WorkId::Glyph(component.base.clone()))
                    .sources()
                    .get(loc)
                    .unwrap()
                    .components
                    .is_empty());
            }
        }
    }

    #[test]
    fn flatten_shallow_component() {
        let test_data = deep_component();
        let context = test_context();
        test_data.write_to(&context);
        flatten_glyph(&context, &test_data.shallow_component).unwrap();
        assert_is_flattened_component(&context, test_data.shallow_component.name);
    }

    #[test]
    fn flatten_deep_component() {
        let test_data = deep_component();
        let context = test_context();
        test_data.write_to(&context);
        flatten_glyph(&context, &test_data.deep_component).unwrap();
        assert_is_flattened_component(&context, test_data.deep_component.name);
    }

    #[test]
    fn decompose_transformed_and_flatten_components() {
        // when both flags are set, the flattening should happen last, after
        // the decomposition of the transformed components
        // https://github.com/googlefonts/fontc/issues/929
        let test_data = deep_component();
        let mut context = test_context();
        context
            .flags
            .set(Flags::DECOMPOSE_TRANSFORMED_COMPONENTS, true);
        context.flags.set(Flags::FLATTEN_COMPONENTS, true);
        test_data.write_to(&context);

        apply_optional_transformations(&context, &test_data.glyph_order()).unwrap();

        // the shallow_component had a non-identity 2x2 transform so it was
        // converted to a simple glyph
        assert_is_simple_glyph(&context, test_data.shallow_component.name);
        // the deep_component glyph should still be a composite but no longer nested;
        // if flattening happened first, it would have been converted to a simple glyph,
        // because the non-id 2x2 transform of the shallow_component would have
        // infected the deep_component and caused it to be decomposed.
        assert_is_flattened_component(&context, test_data.deep_component.name);
    }

    #[derive(Default)]
    struct GlyphOrderBuilder(Vec<Arc<Glyph>>);

    impl GlyphOrderBuilder {
        fn add_glyph<const N: usize>(&mut self, name: &str, components: [&str; N]) {
            let instance = GlyphInstance {
                components: components
                    .into_iter()
                    .map(|name| Component {
                        base: name.into(),
                        transform: Default::default(),
                    })
                    .collect(),
                ..Default::default()
            };
            let loc = NormalizedLocation::for_pos(&[("axis", 0.0)]);
            let glyph = Glyph::new(
                name.into(),
                true,
                Default::default(),
                HashMap::from([(loc, instance)]),
            )
            .unwrap();
            self.0.push(Arc::new(glyph))
        }
    }

    #[test]
    fn component_sorting() {
        let mut builder = GlyphOrderBuilder::default();
        builder.add_glyph("a", ["b", "c"]);
        builder.add_glyph("b", ["z"]);
        builder.add_glyph("c", ["d"]);
        builder.add_glyph("d", ["x", "y"]);
        builder.add_glyph("e", ["z"]);

        let mut order: GlyphOrder = builder.0.iter().map(|g| g.name.clone()).collect();
        let context = test_context();
        for glyph in builder.0.iter() {
            context.glyphs.set((**glyph).clone());
            for component_name in glyph.component_names() {
                if order.contains(component_name) {
                    continue;
                }
                context.glyphs.set(contour_glyph(component_name.as_str()));
                order.insert(component_name.clone());
            }
        }
        let todo = builder
            .0
            .into_iter()
            .map(|g| (GlyphOp::ConvertToContour, g))
            .collect();

        let mut fix_order = Vec::new();

        resolve_inconsistencies(&context, todo, &mut order, |_op, glyph, _order| {
            fix_order.push(glyph.name.clone());
            Ok(())
        })
        .unwrap();

        assert_eq!(fix_order, ["b", "d", "e", "c", "a"]);
    }
}
