//! IR glyph processing.
//!
//! Notably includes splitting glyphs with contours and components into one new glyph with
//! the contours and one updated glyph with no contours that references the new gyph as a component.

use std::{
    borrow::Cow,
    collections::{HashMap, HashSet, VecDeque},
    sync::Arc,
};

use fontdrasil::{
    coords::NormalizedLocation,
    orchestration::{Access, AccessBuilder, Work},
    types::GlyphName,
    variations::{RoundingBehaviour, VariationModel},
};
use kurbo::{Affine, BezPath};
use log::{debug, log_enabled, trace};
use ordered_float::OrderedFloat;
use write_fonts::{OtRound, types::GlyphId16};

use crate::{
    error::{BadGlyph, Error},
    ir::{Component, GlobalMetric, Glyph, GlyphBuilder, GlyphInstance, GlyphOrder, StaticMetadata},
    orchestration::{Context, Flags, IrWork, WorkId},
};

pub fn create_glyph_order_work() -> Box<IrWork> {
    Box::new(GlyphOrderWork {})
}

#[derive(Debug)]
struct GlyphOrderWork {}

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
    simple_glyph.clear_components();
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

impl fontdrasil::util::CompositeLike for &Glyph {
    fn name(&self) -> smol_str::SmolStr {
        self.name.clone().into_inner()
    }

    fn has_components(&self) -> bool {
        Glyph::component_names(self).next().is_some()
    }

    fn component_names(&self) -> impl Iterator<Item = smol_str::SmolStr> {
        Glyph::component_names(self).map(|name| name.clone().into_inner())
    }
}

/// Equivalent to 'SkipExportGlyphsFilter' in pythonland:
///
// See <https://github.com/googlefonts/ufo2ft/blob/98e8916a/Lib/ufo2ft/filters/skipExportGlyphs.py#L29>
///
/// in python this is handled via the decomposeCompositeGlyph filter, which takes
/// a bunch of options.
///
/// It seemed simpler to me to just have this be a little standalone fn with
/// limited scope.
fn flatten_all_non_export_components(context: &Context) -> Result<(), BadGlyph> {
    let glyphs = context.glyphs.all();
    let glyphs = glyphs
        .iter()
        .map(|g| (g.1.name.clone().into_inner(), g.1.as_ref()))
        .collect();

    // instead of tracking a chain of affines, we will just process the glyphs
    // in composite order.
    let depth_first = fontdrasil::util::depth_sorted_composite_glyphs(&glyphs);

    for glyph_name in depth_first {
        let glyph = glyphs.get(&glyph_name).unwrap();
        if glyph_has_non_export_components(glyph, context) {
            let new_glyph = flatten_non_export_components_for_glyph(context, glyph)?;
            context.glyphs.set(new_glyph);
        }
    }
    Ok(())
}

/// Return a new glyph with any non-export components inlined.
fn flatten_non_export_components_for_glyph(
    context: &Context,
    glyph: &Glyph,
) -> Result<Glyph, BadGlyph> {
    let glyph = ensure_composite_defined_at_component_locations(context, glyph);
    let mut builder = GlyphBuilder::from(glyph.clone());
    builder.clear_components();

    log::debug!("flattening non-export components of '{}'", glyph.name);
    for (loc, instance) in glyph.sources() {
        let mut new_instance = instance.clone();
        new_instance.components.clear();

        for component in &instance.components {
            let id = WorkId::Glyph(component.base.clone());
            let referenced_glyph = context.glyphs.get(&id);
            if referenced_glyph.emit_to_binary {
                new_instance.components.push(component.clone());
                continue;
            }

            // okay so now we have a component that is not going to be exported,
            // and we need to flatten.
            let xform = component.transform;
            let referenced_instance = get_or_instantiate_instance(&referenced_glyph, loc, context)?;

            for mut referenced_component in referenced_instance.components.iter().cloned() {
                referenced_component.transform = xform * referenced_component.transform;
                new_instance.components.push(referenced_component);
            }

            for mut contour in referenced_instance.contours.iter().cloned() {
                contour.apply_affine(xform);

                // See https://github.com/googlefonts/ufo2ft/blob/dd738cdc/Lib/ufo2ft/util.py#L205
                if xform.determinant() < 0.0 {
                    contour = contour.reverse_subpaths();
                }
                new_instance.contours.push(contour);
            }
        }
        builder.sources.insert(loc.clone(), new_instance);
    }
    // unwrap is okay because all used locations are from previously validated glyph
    builder.build()
}

fn glyph_has_non_export_components(glyph: &Glyph, context: &Context) -> bool {
    glyph
        .component_names()
        .any(|name| !context.get_glyph(name.as_str()).emit_to_binary)
}

fn ensure_composite_defined_at_component_locations(context: &Context, composite: &Glyph) -> Glyph {
    let mut glyph = composite.to_owned();
    let child_locations = collect_component_locations_nested(context, &glyph);
    for loc in child_locations {
        if !glyph.sources().contains_key(&loc) {
            let new_layer = instantiate_instance(&glyph, &loc, context).unwrap();
            glyph.sources_mut().insert(loc, new_layer);
        }
    }
    glyph
}

fn collect_component_locations_nested(
    context: &Context,
    glyph: &Glyph,
) -> HashSet<NormalizedLocation> {
    let mut out: HashSet<_> = glyph.sources().keys().cloned().collect();
    let mut seen = HashSet::new();
    let mut todo = glyph.component_names().cloned().collect::<Vec<_>>();

    while let Some(next) = todo.pop() {
        if seen.insert(next.clone()) {
            let Some(nextg) = context.try_get_glyph(next) else {
                // component is missing, we log this elsewhere
                continue;
            };
            out.extend(nextg.sources().keys().cloned());
            todo.extend(nextg.component_names().cloned());
        }
    }
    out
}

/// Convert a glyph with contours and components to a contour-only, aka simple, glyph
///
/// At time of writing we only support this if every instance uses the same set of components.
///
/// <https://github.com/googlefonts/ufo2ft/blob/dd738cdcd/Lib/ufo2ft/util.py#L165>
fn convert_components_to_contours(context: &Context, original: &Glyph) -> Result<(), BadGlyph> {
    let original = ensure_composite_defined_at_component_locations(context, original);
    // Component until you can't component no more
    let mut frontier: VecDeque<_> = components(&original, Affine::IDENTITY);

    let mut simple = GlyphBuilder::from(original.clone());
    simple.clear_components();

    // Note that here we care about the entire component transform
    let mut visited: HashSet<(NormalizedLocation, HashableComponent)> = HashSet::new();
    while let Some((loc, component)) = frontier.pop_front() {
        let component_base = component.base.clone();
        let component_affine = component.affine();
        if !visited.insert((loc.clone(), component)) {
            continue;
        }

        let Some(referenced_glyph) = context.try_get_glyph(component_base.clone()) else {
            log::warn!(
                "skipping missing component '{component_base}' of glyph '{}'",
                original.name
            );
            //https://github.com/fonttools/fonttools/blob/03a3c8ed9e/Lib/fontTools/pens/ttGlyphPen.py#L101
            continue;
        };
        // ensure referenced glyph has any required intermediate locations
        let referenced_glyph =
            ensure_component_has_consistent_layers(&original, &referenced_glyph, context)?;
        frontier.extend(
            components(&referenced_glyph, component_affine)
                .iter()
                .filter(|(component_loc, _)| *component_loc == loc)
                .cloned(),
        );

        trace!(
            "'{}' retains {} {component_affine:?} at {loc:?}",
            original.name, referenced_glyph.name
        );
        let inst = simple
            .sources
            .get_mut(&loc)
            .expect("only instances at known locations are added to queue above");
        // unwrap fine because of 'ensure_component_has_consistent_layers' above
        let ref_inst = referenced_glyph.sources().get(&loc).unwrap();

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
    assert_eq!(simple.name, original.name,);
    context.glyphs.set(simple);
    Ok(())
}

// if any locations in base are missing in component, instantiate them.
fn ensure_component_has_consistent_layers<'a>(
    base: &Glyph,
    component: &'a Glyph,
    context: &Context,
) -> Result<Cow<'a, Glyph>, BadGlyph> {
    // same sources: great!
    if base.sources().len() == component.sources().len()
        && component
            .sources()
            .keys()
            .all(|k| base.sources().contains_key(k))
    {
        return Ok(Cow::Borrowed(component));
    }

    let mut component = component.to_owned();
    for loc in base.sources().keys() {
        if component.sources().contains_key(loc) {
            continue;
        }

        let new_instance = get_or_instantiate_instance(&component, loc, context)?.into_owned();
        component.sources_mut().insert(loc.to_owned(), new_instance);
    }

    Ok(Cow::Owned(component))
}

/// Return the instance at this location, or instantiate one via interpolation.
fn get_or_instantiate_instance<'a>(
    glyph: &'a Glyph,
    loc: &NormalizedLocation,
    context: &Context,
) -> Result<Cow<'a, GlyphInstance>, BadGlyph> {
    if let Some(instance) = glyph.sources().get(loc) {
        return Ok(Cow::Borrowed(instance));
    }
    instantiate_instance(glyph, loc, context).map(Cow::Owned)
}

fn instantiate_instance(
    glyph: &Glyph,
    loc: &NormalizedLocation,
    context: &Context,
) -> Result<GlyphInstance, BadGlyph> {
    log::debug!("instantiating '{}' at {loc:?}", glyph.name);
    let meta = context.static_metadata.get();
    let model = variation_model_for_glyph(glyph, &meta);
    let point_seqs = glyph
        .sources()
        .iter()
        .map(|(loc, instance)| (loc.clone(), instance.values_for_interpolation()))
        .collect();
    // when instantiating intermediates we don't want to do rounding (this is
    // a significant problem if we round some component transformations, where
    // the fractional bits can be very important).
    // This matches fonttools, see https://github.com/googlefonts/ufo2ft/blob/01d3faee/Lib/ufo2ft/_compilers/baseCompiler.py#L266
    let deltas = model
        .deltas_with_rounding(&point_seqs, RoundingBehaviour::None)
        .map_err(|e| BadGlyph::new(&glyph.name, e))?;
    let points = model.interpolate_from_deltas(loc, &deltas);
    Ok(glyph
        .default_instance()
        .new_with_interpolated_values(&points))
}

fn variation_model_for_glyph<'a>(
    glyph: &Glyph,
    meta: &'a StaticMetadata,
) -> Cow<'a, VariationModel> {
    if meta
        .variation_model
        .locations()
        .all(|loc| glyph.sources().contains_key(loc))
        && meta.variation_model.num_locations() == glyph.sources().len()
    {
        // great, we have the same model
        return Cow::Borrowed(&meta.variation_model);
    }

    // otherwise we need a special model for this glyph.
    // This code is duplicated in various places (hvar, e.g.)
    // and maybe we can share it? or cache these models more globally?
    Cow::Owned(VariationModel::new(
        glyph.sources().keys().cloned().collect(),
        meta.axes.iter().map(|ax| ax.tag).collect(),
    ))
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
    log::trace!(
        "Flattening {} (components: {:?})",
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
            let ref_inst = get_or_instantiate_instance(&ref_glyph, loc, context)?;
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
            let notdef = synthesize_notdef(context)?;
            context.glyphs.set(notdef);
        }
    }
    Ok(())
}

/// Create a (possibly variable) .notdef glyph
///
/// * see <https://github.com/googlefonts/ufo2ft/blob/b3895a96ca/Lib/ufo2ft/outlineCompiler.py#L1666-L1694>
/// * and <https://github.com/googlefonts/fontc/issues/1262>
fn synthesize_notdef(context: &Context) -> Result<Glyph, BadGlyph> {
    let static_metadata = context.static_metadata.get();
    let global_metrics = context.global_metrics.get();

    let mut builder = GlyphBuilder::new(GlyphName::NOTDEF);
    let upem = static_metadata.units_per_em;
    let width = (upem as f64 * 0.5).ot_round();
    let default = global_metrics.at(static_metadata.default_location());
    let default_outline = make_notdef_outline(upem, default.ascender.0, default.descender.0);
    // NOTE: Most glyphs have `None` heights, but here we are just matching ufo2ft:
    // See https://github.com/googlefonts/ufo2ft/blob/b3895a96/Lib/ufo2ft/outlineCompiler.py#L1656-L1
    let height = Some(default.ascender.0 - default.descender.0);

    builder.try_add_source(
        static_metadata.default_location(),
        GlyphInstance {
            width,
            height,
            contours: vec![default_outline],
            ..Default::default()
        },
    )?;
    if static_metadata.axes.is_empty() {
        return builder.build();
    }

    for location in static_metadata.variation_model.locations() {
        let ascender = global_metrics.get(GlobalMetric::Ascender, location);
        let descender = global_metrics.get(GlobalMetric::Descender, location);
        if (ascender, descender) == (default.ascender, default.descender) {
            continue;
        }
        let outline = make_notdef_outline(upem, ascender.0, descender.0);

        let instance = GlyphInstance {
            width,
            height: Some(ascender.0 - descender.0),
            contours: vec![outline],
            ..Default::default()
        };
        builder.try_add_source(location, instance)?;
    }

    builder.build()
}

/// Make a tofu outline: a rectangle sized based on the provided metrics
fn make_notdef_outline(upm: u16, ascender: f64, descender: f64) -> BezPath {
    let upm = upm as f64;
    let width = OtRound::<u16>::ot_round(upm * 0.5) as f64;
    let stroke = OtRound::<u16>::ot_round(upm * 0.05) as f64;

    let mut path = BezPath::new();

    // outer box
    let x_min = stroke;
    let x_max = width - stroke;
    let y_max = ascender;
    let y_min = descender;
    path.move_to((x_min, y_min));
    path.line_to((x_max, y_min));
    path.line_to((x_max, y_max));
    path.line_to((x_min, y_max));
    path.line_to((x_min, y_min));
    path.close_path();

    // inner, cut out, box
    let x_min = x_min + stroke;
    let x_max = x_max - stroke;
    let y_max = y_max - stroke;
    let y_min = y_min + stroke;
    path.move_to((x_min, y_min));
    path.line_to((x_min, y_max));
    path.line_to((x_max, y_max));
    path.line_to((x_max, y_min));
    path.line_to((x_min, y_min));
    path.close_path();
    path
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
        // preflght: lets remove component references to non-export glyphs.
        // In python this happens during preprocessing
        // (https://github.com/googlefonts/ufo2ft/blob/98e8916a8/Lib/ufo2ft/preProcessor.py#L92)
        // (https://github.com/googlefonts/ufo2ft/blob/98e8916a8/Lib/ufo2ft/util.py#L112)

        flatten_all_non_export_components(context)?;

        // then generate the final glyph order and do final glyph processing
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
                log::debug!(
                    "Coalescing '{glyph_name}' into a simple glyph: \
                        component 2x2s vary across the designspace"
                );
                todo.push_back((GlyphOp::ConvertToContour, glyph.clone()));
            } else if glyph.has_overflowing_component_transforms() {
                log::debug!(
                    "Decomposing '{glyph_name}' into a simple glyph: \
                        component transforms overflow F2Dot14 [-2.0, 2.0] range"
                );
                todo.push_back((GlyphOp::ConvertToContour, glyph.clone()));
            } else if glyph.has_mixed_contours_and_components() {
                if context.flags.contains(Flags::PREFER_SIMPLE_GLYPHS) {
                    todo.push_back((GlyphOp::ConvertToContour, glyph.clone()));
                    log::debug!(
                        "Coalescing '{glyph_name}' into a simple glyph: 'prefer simple glyphs' is set."
                    );
                } else {
                    log::debug!("Moving contours of {glyph_name} to components");
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
    use std::{
        collections::{BTreeSet, HashSet},
        path::Path,
    };

    use fontdrasil::{
        orchestration::Access,
        types::{Axis, GlyphName},
    };
    use kurbo::{Affine, BezPath, Rect, Shape};
    use rstest::rstest;

    use crate::{
        ir::{
            Component, GlobalMetricsBuilder, Glyph, GlyphBuilder, GlyphInstance, GlyphOrder,
            StaticMetadata,
        },
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
        let loc = NormalizedLocation::for_pos(&[("wght", 0.0)]);
        test_context_with_locations(vec![loc])
    }

    // make a new `Context` that includes a variation model based on the
    // provided locations
    fn test_context_with_locations(locations: Vec<NormalizedLocation>) -> Context {
        let axes = locations
            .iter()
            .flat_map(|loc| loc.axis_tags())
            .collect::<BTreeSet<_>>();
        let axes = axes
            .into_iter()
            .map(|tag| Axis::for_test(&tag.to_string()))
            .collect::<Vec<_>>();
        let meta = StaticMetadata::new(
            1000,
            Default::default(),
            axes,
            Vec::new(),
            locations.into_iter().collect(),
            None,
            1.0,
            Default::default(),
            None,
            false,
        )
        .unwrap();
        let mut flags = Flags::default();
        flags.set(Flags::EMIT_IR, false); // we don't want to write anything down
        let ctx = Context::new_root(flags, Paths::new(Path::new("/fake/path")))
            .copy_for_work(Access::All, Access::All);
        ctx.static_metadata.set(meta);
        ctx
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
            simple_glyph: static_contour_glyph(simple_glyph_name),
            // add c1, reusing shape w/90 degrees ccw rotation: x-basis 0,1 y-basis -1,0
            shallow_component: static_component_glyph(
                shallow_component_name,
                simple_glyph_name.into(),
                Affine::new([0.0, -1.0, 1.0, 0.0, 0.0, 0.0]),
            ),
            // add c2, reusing c1 w/translation
            deep_component: static_component_glyph(
                "c2",
                shallow_component_name.into(),
                Affine::translate((5.0, 0.0)),
            ),
        }
    }

    fn make_notdef_with_ascenders(ascender: &[(&NormalizedLocation, f64)]) -> Glyph {
        let context = test_context_with_locations(ascender.iter().map(|x| x.0.clone()).collect());
        let mut global_metrics = GlobalMetricsBuilder::new();
        for (loc, val) in ascender {
            global_metrics.populate_defaults(loc, 1000, None, Some(*val), Some(0.0), None);
        }
        let global_metrics = global_metrics
            .build(&context.static_metadata.get().axes)
            .unwrap();
        context.global_metrics.set(global_metrics);
        synthesize_notdef(&context).unwrap()
    }

    #[test]
    fn variable_notdef() {
        let [loc0, loc1] = make_wght_locations([0.0, 1.0]);
        let notdef = make_notdef_with_ascenders(&[(&loc0, 600.), (&loc1, 700.)]);
        assert_eq!(notdef.sources()[&loc0].height, Some(600.));
        assert_eq!(notdef.sources()[&loc1].height, Some(700.));
    }

    #[test]
    fn non_variable_notdef() {
        let [loc0, loc1] = make_wght_locations([0.0, 1.0]);
        let notdef = make_notdef_with_ascenders(&[(&loc0, 600.), (&loc1, 600.)]);
        assert_eq!(notdef.sources().len(), 1);
        assert_eq!(notdef.sources()[&loc0].height, Some(600.));
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
        assert!(!glyph.has_mixed_contours_and_components());
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
        assert!(glyph.has_mixed_contours_and_components());
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

    fn static_contour_glyph(name: &str) -> Glyph {
        let mut glyph = TestGlyph::new(name);
        glyph.add_contour(contour());
        glyph.0
    }

    fn variable_contour_glyph(name: &str) -> Glyph {
        let [loc1, loc2] = make_wght_locations([0.0, 1.0]);
        let mut glyph = TestGlyph::new(name);
        glyph.add_var_contour(&[(&loc1, contour()), (&loc2, contour())]);
        glyph.0
    }

    fn static_component_glyph(name: &str, base: GlyphName, transform: Affine) -> Glyph {
        let mut glyph = TestGlyph::new(name);
        glyph.add_component(base.as_str(), transform);
        glyph.0
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
        assert!(
            composite
                .sources()
                .values()
                .all(|gi| gi.contours.is_empty())
        );
        assert!(
            composite
                .sources()
                .values()
                .all(|gi| !gi.components.is_empty())
        );
    }

    #[test]
    fn components_to_contours_shallow() {
        let coalesce_me = contour_and_component_weight_glyph("coalesce_me");

        let context = test_context();
        context.glyphs.set(variable_contour_glyph("component"));

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

        let mut nested_components = TestGlyph::new("g");
        nested_components
            .add_contour(contour())
            .add_component(test_data.shallow_component.name.as_str(), Affine::IDENTITY)
            .add_component(
                test_data.shallow_component.name.as_str(),
                Affine::translate((0.0, 2.0)),
            )
            .add_component(
                test_data.deep_component.name.as_str(),
                Affine::translate((0.0, 5.0)),
            );

        convert_components_to_contours(&context, &nested_components.0).unwrap();
        let simple = context.get_glyph("g");
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
        let mut reuse_me = TestGlyph::new("shape");
        reuse_me.add_contour(contour());
        let mut glyph = TestGlyph::new("g");
        glyph.add_component("shape", the_neg);

        let context = test_context();
        context.glyphs.set(reuse_me.0);

        convert_components_to_contours(&context, &glyph.0).unwrap();
        let simple = context.get_glyph("g");
        assert_simple(&simple);
        assert_eq!(1, simple.sources().len());
        let inst = simple.sources().values().next().unwrap();

        // what we should get back is the contour with the_neg applied, reversed because
        // the_neg is notoriously negative in determinant
        let expected = the_neg * contour();
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
        context.glyphs.set(variable_contour_glyph("component"));

        convert_components_to_contours(&context, &glyph).unwrap();
        let simple = context.get_glyph(glyph.name.clone());
        assert_simple(&simple);
    }

    #[test]
    fn contour_and_component_with_varied_transform() {
        let glyph = contour_and_component_weight_glyph("nameless");
        let glyph = adjust_transform_for_each_instance(&glyph, |i| Affine::scale(i as f64));

        let context = test_context();
        context.glyphs.set(variable_contour_glyph("component"));

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
        assert!(
            glyph
                .sources()
                .values()
                .all(|inst| !inst.contours.is_empty() && inst.components.is_empty())
        );
    }

    fn assert_is_flattened_component(context: &Context, glyph_name: GlyphName) {
        let glyph = context.get_glyph(glyph_name);
        for (loc, inst) in glyph.sources().iter() {
            assert!(!inst.components.is_empty());
            for component in inst.components.iter() {
                assert!(
                    context
                        .glyphs
                        .get(&WorkId::Glyph(component.base.clone()))
                        .sources()
                        .get(loc)
                        .unwrap()
                        .components
                        .is_empty()
                );
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
    fn flatten_components_with_flag() {
        let test_data = deep_component();
        let mut context = test_context();
        context.flags.set(Flags::FLATTEN_COMPONENTS, true);

        test_data.write_to(&context);
        apply_optional_transformations(&context, &test_data.glyph_order()).unwrap();
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

    trait AffineLike {
        fn to_affine(self) -> Affine;
    }

    impl AffineLike for Affine {
        fn to_affine(self) -> Affine {
            self
        }
    }

    impl AffineLike for (i16, i16) {
        fn to_affine(self) -> Affine {
            Affine::translate((self.0 as f64, self.1 as f64))
        }
    }

    #[derive(Default)]
    struct GlyphOrderBuilder(Vec<Glyph>);

    #[derive(Debug)]
    struct TestGlyph(Glyph);

    impl TestGlyph {
        fn new(name: &str) -> TestGlyph {
            let glyph = Glyph::new(
                name.into(),
                true,
                Default::default(),
                [(
                    NormalizedLocation::for_pos(&[("wght", 0.0)]),
                    GlyphInstance::default(),
                )]
                .into(),
            )
            .unwrap();
            TestGlyph(glyph)
        }

        fn emit_to_binary(&mut self, emit: bool) -> &mut Self {
            self.0.emit_to_binary = emit;
            self
        }

        fn variable_width(&mut self, width: &[f64]) -> &mut Self {
            for (inst, val) in self.0.sources_mut().values_mut().zip(width) {
                inst.width = *val;
            }
            self
        }

        fn variable_vertical_origin(&mut self, vals: &[f64]) -> &mut Self {
            for (inst, val) in self.0.sources_mut().values_mut().zip(vals) {
                inst.vertical_origin = Some(*val);
            }
            self
        }

        fn default_instance_mut(&mut self) -> &mut GlyphInstance {
            self.0.default_instance_mut()
        }

        fn add_contour(&mut self, path: BezPath) -> &mut Self {
            assert_eq!(self.0.sources().len(), 1, "expects non-variable glyph");
            self.default_instance_mut().contours.push(path);
            self
        }

        fn add_component(&mut self, name: &str, xform: impl AffineLike) -> &mut Self {
            assert_eq!(self.0.sources().len(), 1, "expects non-variable glyph");
            let component = Component {
                base: name.into(),
                transform: xform.to_affine(),
            };
            self.default_instance_mut().components.push(component);
            self
        }

        fn add_var_contour(&mut self, instances: &[(&NormalizedLocation, BezPath)]) -> &mut Self {
            for (loc, contour) in instances {
                self.0
                    .sources_mut()
                    .entry((*loc).clone())
                    .or_default()
                    .contours
                    .push(contour.clone());
            }
            self
        }

        /// Add a component at multiple locations
        fn add_var_component(
            &mut self,
            name: &str,
            instances: &[(&NormalizedLocation, Affine)],
        ) -> &mut Self {
            for (loc, xform) in instances {
                self.0
                    .sources_mut()
                    .entry((*loc).clone())
                    .or_default()
                    .components
                    .push(Component {
                        base: name.into(),
                        transform: *xform,
                    });
            }
            self
        }
    }

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
            self.0.push(glyph)
        }

        // gives more control over things like component transforms
        fn add_glyph_fancy(&mut self, name: &str, mut build_fn: impl FnMut(&mut TestGlyph)) {
            let pos = NormalizedLocation::for_pos(&[("axis", 0.0)]);
            let instance = GlyphInstance::default();
            let glyph = Glyph::new(
                name.into(),
                true,
                Default::default(),
                [(pos, instance)].into(),
            )
            .unwrap();
            let mut gbuilder = TestGlyph(glyph);
            build_fn(&mut gbuilder);
            self.0.push(gbuilder.0);
        }

        /// Make a `Context` containing these glyphs.
        ///
        /// This assumes that all referenced glyphs exist etc.
        fn into_context(self) -> Context {
            let context = test_context();
            for glyph in self.0 {
                context.glyphs.set(glyph);
            }
            context
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
            context.glyphs.set(glyph.clone());
            for component_name in glyph.component_names() {
                if order.contains(component_name) {
                    continue;
                }
                context
                    .glyphs
                    .set(variable_contour_glyph(component_name.as_str()));
                order.insert(component_name.clone());
            }
        }
        let todo = builder
            .0
            .into_iter()
            .map(|g| (GlyphOp::ConvertToContour, g.into()))
            .collect();

        let mut fix_order = Vec::new();

        resolve_inconsistencies(&context, todo, &mut order, |_op, glyph, _order| {
            fix_order.push(glyph.name.clone());
            Ok(())
        })
        .unwrap();

        assert_eq!(fix_order, ["b", "d", "e", "c", "a"]);
    }

    fn simple_square_path() -> BezPath {
        let mut path = BezPath::new();
        path.move_to((-5., -5.));
        path.line_to((-5., 5.));
        path.line_to((5., 5.));
        path.line_to((5., -5.));
        path.line_to((-5., -5.));
        path
    }

    #[test]
    fn missing_component_no_crashy() {
        // make sure we don't crash in this fn if a component is missing
        let mut builder = GlyphOrderBuilder::default();
        builder.add_glyph("a", ["b", "missing"]);
        builder.add_glyph("b", []);
        let context = builder.into_context();
        let glyph = context.get_glyph("a");
        convert_components_to_contours(&context, &glyph).unwrap();
    }

    #[test]
    fn non_export_component_simple() {
        let mut builder = GlyphOrderBuilder::default();
        builder.add_glyph_fancy("a", |a| {
            a.emit_to_binary(false);
            a.add_contour(simple_square_path());
        });
        builder.add_glyph_fancy("b", |b| {
            b.add_component("a", (20, 20));
        });

        let context = builder.into_context();
        flatten_all_non_export_components(&context).unwrap();

        let b = context.get_glyph("b");

        let instance = b.default_instance();
        assert!(instance.components.is_empty());

        assert_eq!(instance.contours.len(), 1);
        let contour = &instance.contours[0];
        // simple square path, transformed by (20x, 20y)
        assert_eq!(contour.bounding_box(), Rect::new(15., 15., 25., 25.));
    }

    #[test]
    fn non_export_component_nested() {
        let mut builder = GlyphOrderBuilder::default();
        builder.add_glyph_fancy("a", |a| {
            a.add_contour(simple_square_path());
        });
        builder.add_glyph_fancy("b", |b| {
            b.add_component("a", (20, 20));
            b.emit_to_binary(false);
        });
        builder.add_glyph_fancy("c", |b| {
            b.add_component("b", (9, -7));
        });

        let context = builder.into_context();
        flatten_all_non_export_components(&context).unwrap();
        let c = context.get_glyph("c");

        let instance = c.default_instance();
        assert!(instance.contours.is_empty());
        assert_eq!(instance.components.len(), 1);

        let comp = &instance.components[0];

        assert_eq!(comp.base, "a");
        assert_eq!(comp.transform, Affine::translate((29., 13.)));
    }

    #[test]
    fn non_export_needs_reverse_contour() {
        let mut builder = GlyphOrderBuilder::default();
        builder.add_glyph_fancy("a", |a| {
            a.emit_to_binary(false);
            a.add_contour(simple_square_path());
        });
        builder.add_glyph_fancy("b", |b| {
            b.add_component("a", Affine::scale_non_uniform(-1.0, 1.0));
        });

        let context = builder.into_context();
        flatten_all_non_export_components(&context).unwrap();

        let b = context.get_glyph("b");

        let instance = b.default_instance();
        assert!(instance.components.is_empty());

        assert_eq!(instance.contours.len(), 1);
        let contour = &instance.contours[0];
        let expected =
            (Affine::scale_non_uniform(-1., 1.) * simple_square_path()).reverse_subpaths();

        assert_eq!(contour, &expected);
    }

    #[test]
    fn flatten_when_leaf_component_has_extra_layer() {
        // in this case we don't need to have a matching layer in the component
        let [loc1, loc2] = make_wght_locations([0.0, 1.0]);

        let mut composite = TestGlyph::new("a");
        composite.add_var_component("b", &[(&loc1, Affine::IDENTITY)]);
        let mut comp1 = TestGlyph::new("b");
        comp1.add_var_component("c", &[(&loc1, Affine::IDENTITY)]);
        let mut comp2 = TestGlyph::new("c");

        comp2.add_var_contour(&[
            (&loc1, simple_square_path()),
            (&loc2, Affine::translate((0.0, 10.0)) * simple_square_path()),
        ]);

        let context = test_context_with_locations(vec![loc1.clone(), loc2.clone()]);
        context.glyphs.set(composite.0.clone());
        context.glyphs.set(comp1.0);
        context.glyphs.set(comp2.0);

        flatten_glyph(&context, &composite.0).unwrap();
        let after = context.get_glyph("a");
        assert_eq!(after.sources().len(), 1);
        assert_eq!(
            after
                .component_names()
                .map(|g| g.to_string())
                .collect::<Vec<_>>(),
            ["c"]
        );
    }

    #[test]
    fn instantiation_smoke_test() {
        let [loc1, intermediate, loc2] = make_wght_locations([0.0, 0.5, 1.0]);
        let mut glyph = TestGlyph::new("glyphie");
        glyph
            .add_var_contour(&[
                (&loc1, simple_square_path()),
                (&loc2, Affine::translate((10., 0.)) * simple_square_path()),
            ])
            .add_var_component(
                "derp",
                &[
                    (&loc1, Affine::translate((14.0, 14.0))),
                    (&loc2, Affine::translate((20.0, 20.0))),
                ],
            )
            .variable_width(&[400., 600.])
            .variable_vertical_origin(&[100., 200.]);

        let context = test_context_with_locations(vec![loc1.clone(), loc2.clone()]);
        context.glyphs.set(glyph.0.clone());

        let inst = instantiate_instance(&glyph.0, &intermediate, &context).unwrap();

        assert_eq!(
            inst.contours[0],
            Affine::translate((5.0, 0.)) * simple_square_path()
        );
        assert_eq!(
            inst.components[0].transform,
            Affine::translate((17.0, 17.0))
        );
        assert_eq!(inst.width, 500.);
        assert!(inst.height.is_none());
        assert_eq!(inst.vertical_origin, Some(150.));
    }

    fn interpolate_transform_at_midpoint(one: Affine, two: Affine) -> Affine {
        let [loc1, intermediate, loc2] = make_wght_locations([0.0, 0.5, 1.0]);
        let mut glyph = TestGlyph::new("glyphie");
        glyph.add_var_component("derp", &[(&loc1, one), (&loc2, two)]);

        let context = test_context_with_locations(vec![loc1.clone(), loc2.clone()]);
        context.glyphs.set(glyph.0.clone());

        let inst = instantiate_instance(&glyph.0, &intermediate, &context).unwrap();
        inst.components[0].transform
    }

    #[test]
    fn interpolate_transform_with_rotation() {
        const ROT_180: Affine = Affine::new([-1.0, 0.0, 0.0, -1.0, 0.0, 0.0]);
        let one = Affine::translate((100.0, 100.0)) * ROT_180;
        let two = Affine::translate((200.0, 200.0)) * ROT_180;
        assert_eq!(
            interpolate_transform_at_midpoint(one, two),
            Affine::new([-1., 0., 0., -1., 150., 150.])
        );
    }

    #[test]
    fn interpolate_transform_with_scale() {
        assert_eq!(
            interpolate_transform_at_midpoint(Affine::IDENTITY, Affine::scale(1.6)),
            // this was failing before we stopped rounding deltas.
            Affine::scale(1.3)
        );
    }

    #[test]
    fn non_export_component_has_intermediate_layer() {
        let _ = env_logger::builder().is_test(true).try_init();
        // https://github.com/googlefonts/fontc/issues/1592
        let [loc1, intermediate, loc2] = make_wght_locations([0.0, 0.5, 1.0]);

        let mut aogonek = TestGlyph::new("Aogonek");
        aogonek
            .add_var_component(
                "A.ogonekAccent",
                &[(&loc1, Affine::IDENTITY), (&loc2, Affine::IDENTITY)],
            )
            .add_var_component(
                "ogonekcomb.case",
                &[(&loc1, Affine::IDENTITY), (&loc2, Affine::IDENTITY)],
            );

        let mut aogonek_accent = TestGlyph::new("A.ogonekAccent");
        aogonek_accent
            .add_var_contour(&[(&loc1, simple_square_path()), (&loc2, simple_square_path())])
            .emit_to_binary(false);

        let mut ogonekcomb_case = TestGlyph::new("ogonekcomb.case");
        ogonekcomb_case.add_var_component(
            "ogonek",
            &[(&loc1, Affine::IDENTITY), (&loc2, Affine::IDENTITY)],
        );
        let mut ogonek = TestGlyph::new("ogonek");
        ogonek.add_var_contour(&[
            (&loc1, simple_square_path()),
            (
                &intermediate,
                Affine::translate((0.0, 10.0)) * simple_square_path(),
            ),
            (
                &loc2,
                Affine::translate((0.0, 100.0)) * simple_square_path(),
            ),
        ]);

        let context = test_context_with_locations(vec![loc1.clone(), loc2.clone()]);
        context.glyphs.set(aogonek.0.clone());
        context.glyphs.set(aogonek_accent.0);
        context.glyphs.set(ogonekcomb_case.0);
        context.glyphs.set(ogonek.0);

        flatten_all_non_export_components(&context).unwrap();
        let after = context.get_glyph("Aogonek");
        assert_eq!(after.sources().len(), 3);
    }

    fn make_wght_locations<const N: usize>(positions: [f64; N]) -> [NormalizedLocation; N] {
        positions
            .iter()
            .map(|pos| NormalizedLocation::for_pos(&[("wght", *pos)]))
            .collect::<Vec<_>>()
            .try_into()
            .unwrap()
    }

    // in this case we need to interpolate an instance for the component
    // at the missing location.
    #[test]
    fn composite_with_intermediate_layer_not_present_in_component() {
        let [loc1, intermediate, loc2] = make_wght_locations([0.0, 0.5, 1.0]);

        let mut composite = TestGlyph::new("a");
        composite.add_var_component(
            "b",
            &[
                (&loc1, Affine::translate((0., 0.))),
                (&intermediate, Affine::translate((10., 0.))),
                (&loc2, Affine::translate((30., 0.))),
            ],
        );
        let mut component = TestGlyph::new("b");
        component.add_var_contour(&[
            (&loc1, simple_square_path()),
            (&loc2, Affine::translate((0.0, 10.0)) * simple_square_path()),
        ]);

        let context = test_context_with_locations(vec![loc1.clone(), loc2.clone()]);
        context.glyphs.set(composite.0.clone());
        context.glyphs.set(component.0);

        convert_components_to_contours(&context, &composite.0).unwrap();
        let after = context.get_glyph("a");
        assert_eq!(after.sources().len(), 3);

        let interm = after.sources().get(&intermediate).unwrap();
        // 10.0 is the component xform at the intermediate location,
        // 5.0 is interpolated from the contour itself
        let expected_contour = Affine::translate((10.0, 5.0)) * simple_square_path();
        assert_eq!(&expected_contour, &interm.contours[0]);
    }

    // this tests that we also interpolate the component transform.
    #[test]
    fn nested_composite_with_intermediate_layer_not_present_in_component() {
        let _ = env_logger::builder().is_test(true).try_init();
        let [loc1, intermediate, loc2] = make_wght_locations([0.0, 0.5, 1.0]);

        let mut composite = TestGlyph::new("a");
        composite.add_var_component(
            "b",
            &[
                (&loc1, Affine::translate((0., 0.))),
                (&intermediate, Affine::translate((10., 0.))),
                (&loc2, Affine::translate((30., 0.))),
            ],
        );
        let mut component1 = TestGlyph::new("b");
        component1.add_var_component(
            "z",
            &[
                (&loc1, Affine::translate((0.0, 0.0))),
                (&loc2, Affine::translate((0.0, 10.0))),
            ],
        );
        let mut component2 = TestGlyph::new("z");
        component2.add_var_contour(&[
            (
                &loc1,
                Affine::translate((100., 100.)) * simple_square_path(),
            ),
            (
                &loc2,
                Affine::translate((100., 100.)) * simple_square_path(),
            ),
        ]);

        let context = test_context_with_locations(vec![loc1.clone(), loc2.clone()]);
        context.glyphs.set(composite.0.clone());
        context.glyphs.set(component1.0);
        context.glyphs.set(component2.0);

        convert_components_to_contours(&context, &composite.0).unwrap();
        let after = context.get_glyph("a");
        assert_eq!(after.sources().len(), 3);

        let interm = after.sources().get(&intermediate).unwrap();
        // 100, 100 is the translation of component 2
        // 10.0, 0. is the component xform at the intermediate location,
        // 5.0, 0. is interpolated from the contour itself
        let expected_contour = Affine::translate((110.0, 105.0)) * simple_square_path();
        assert_eq!(&expected_contour, &interm.contours[0]);
    }

    #[test]
    fn composite_with_intermediate_component_layer_and_another_nested_compoonent_without_intermediates()
     {
        // based on ecaron in Savate.glyphs
        let _ = env_logger::builder().is_test(true).try_init();

        let [loc1, intermediate, loc2] = make_wght_locations([0.0, 0.5, 1.0]);
        let mut ecaron = TestGlyph::new("ecaron");
        ecaron
            .add_var_component("e", &[(&loc1, Affine::IDENTITY), (&loc2, Affine::IDENTITY)])
            .add_var_component(
                "caroncomb",
                &[
                    (&loc1, Affine::IDENTITY),
                    (&loc2, Affine::translate((36., 0.))),
                ],
            );

        // has intermediate layer
        let mut e = TestGlyph::new("e");
        e.add_var_contour(&[
            (&loc1, simple_square_path()),
            (
                &intermediate,
                Affine::translate((0.0, 10.0)) * simple_square_path(),
            ),
            (
                &loc2,
                Affine::translate((0.0, 100.0)) * simple_square_path(),
            ),
        ]);

        // has no intermediate
        let mut caroncomb = TestGlyph::new("caroncomb");
        // NOTE: in the real font this has a complex transform, which will likely
        // also cause problems; but for now let's just test that we end up with
        // all the right locations defined
        caroncomb.add_var_component(
            "circumflexcomb",
            &[(&loc1, Affine::IDENTITY), (&loc2, Affine::IDENTITY)],
        );
        let mut circumflexcomb = TestGlyph::new("circumflexcomb");
        circumflexcomb.add_var_contour(&[
            (&loc1, contour()),
            (&loc2, Affine::translate((3., 3.)) * contour()),
        ]);
        let context = test_context_with_locations(vec![loc1.clone(), loc2.clone()]);
        context.glyphs.set(ecaron.0.clone());
        context.glyphs.set(e.0);
        context.glyphs.set(caroncomb.0);
        context.glyphs.set(circumflexcomb.0);

        convert_components_to_contours(&context, &ecaron.0).unwrap();
        let after = context.get_glyph("ecaron");
        assert_eq!(after.sources().len(), 3);
    }

    #[test]
    fn one_component_with_intermediates_but_not_the_other() {
        let [loc1, intermediate, loc2] = make_wght_locations([0.0, 0.5, 1.0]);
        let mut composite = TestGlyph::new("a");
        composite
            .add_var_component(
                "b",
                &[
                    (&loc1, Affine::translate((0., 0.))),
                    (&intermediate, Affine::translate((6.0, 0.))),
                    (&loc2, Affine::translate((10., 0.))),
                ],
            )
            .add_var_component(
                "c",
                &[
                    (&loc1, Affine::translate((100., 0.))),
                    (&intermediate, Affine::translate((120., 0.))),
                    (&loc2, Affine::translate((200., 0.))),
                ],
            );

        let mut component1 = TestGlyph::new("b");
        component1.add_var_contour(&[
            (&loc1, simple_square_path()),
            (
                &intermediate,
                Affine::translate((0.0, 10.0)) * simple_square_path(),
            ),
            (
                &loc2,
                Affine::translate((0.0, 100.0)) * simple_square_path(),
            ),
        ]);
        let mut component2 = TestGlyph::new("c");
        component2.add_var_contour(&[
            (&loc1, simple_square_path()),
            (
                &loc2,
                Affine::translate((0.0, 100.0)) * simple_square_path(),
            ),
        ]);
        let context = test_context_with_locations(vec![loc1.clone(), loc2.clone()]);
        context.glyphs.set(composite.0.clone());
        context.glyphs.set(component1.0);
        context.glyphs.set(component2.0);

        convert_components_to_contours(&context, &composite.0).unwrap();
        let after = context.get_glyph("a");
        assert_eq!(after.sources().len(), 3);
        let inst = after.sources().get(&intermediate).unwrap();

        // 6, 10 are explicit intermediate positions
        let expected1 = Affine::translate((6.0, 10.0)) * simple_square_path();
        // 120 is explicit (componnet pos) and 50 is interpolated (half the xform we applied)
        let expected2 = Affine::translate((120.0, 50.0)) * simple_square_path();
        assert_eq!([expected1, expected2], inst.contours.as_slice());
    }

    // we had a crash here because when updating instances on the component
    // we would end up in a state where the component had one intermediate layer,
    // and then trying to interpolate the second would crash the master var model
    // because it wasn't expecting the first intermediate.
    #[test]
    fn multiple_intermediates() {
        let [loc1, interm1, interm2, loc2] = make_wght_locations([0.0, 0.2, 0.5, 1.0]);

        let mut composite = TestGlyph::new("a");
        composite.add_var_component(
            "b",
            &[
                (&loc1, Affine::translate((0., 0.))),
                (&interm1, Affine::translate((10., 0.))),
                (&interm2, Affine::translate((30., 0.))),
                (&loc2, Affine::translate((100., 0.))),
            ],
        );
        let mut component = TestGlyph::new("b");
        component.add_var_contour(&[
            (&loc1, simple_square_path()),
            (&loc2, Affine::translate((0.0, 10.0)) * simple_square_path()),
        ]);

        let context = test_context_with_locations(vec![loc1.clone(), loc2.clone()]);
        context.glyphs.set(composite.0.clone());
        context.glyphs.set(component.0);

        // just don't crash
        convert_components_to_contours(&context, &composite.0).unwrap();
    }

    #[test]
    fn decompose_component_has_intermediate_layer() {
        // https://github.com/googlefonts/fontc/issues/552
        let [loc1, intermediate, loc2] = make_wght_locations([0.0, 0.5, 1.0]);

        let mut mixed_glyph = TestGlyph::new("a");
        mixed_glyph
            .add_var_component(
                "b",
                &[
                    (&loc1, Affine::translate((0., 0.))),
                    (&loc2, Affine::translate((10., 0.))),
                ],
            )
            .add_var_contour(&[
                (&loc1, simple_square_path()),
                (&loc2, Affine::translate((100., 0.)) * simple_square_path()),
            ]);
        let mut component = TestGlyph::new("b");
        component.add_var_contour(&[
            (&loc1, simple_square_path()),
            (
                &intermediate,
                Affine::translate((0.0, 10.0)) * simple_square_path(),
            ),
            (
                &loc2,
                Affine::translate((0.0, 100.0)) * simple_square_path(),
            ),
        ]);

        let context = test_context_with_locations(vec![loc1.clone(), loc2.clone()]);
        context.glyphs.set(mixed_glyph.0.clone());
        context.glyphs.set(component.0);

        convert_components_to_contours(&context, &mixed_glyph.0).unwrap();
        let after = context.get_glyph("a");
        assert_eq!(after.sources().len(), 3);
        assert!(after.sources().contains_key(&intermediate));
        let interm = after.sources().get(&intermediate).unwrap();
        assert_eq!(
            interm.contours,
            vec![
                Affine::translate((50., 0.)) * simple_square_path(),
                Affine::translate((5., 10.)) * simple_square_path()
            ]
        );
    }

    #[rstest]
    #[case::normal_scale(1.5, false)]
    #[case::near_positive_edge(1.999999, false)]
    #[case::near_negative_edge(-1.999999, false)]
    #[case::exactly_minus_2(-2.0, false)]
    #[case::exactly_2(2.0, false)]
    #[case::positive_just_over_2(2.000001, true)]
    #[case::negative_just_under_minus_2(-2.000001, true)]
    #[case::positive_over_2(2.5, true)]
    #[case::negative_under_minus_2(-2.5, true)]
    fn glyph_has_overflowing_transforms(#[case] scale: f64, #[case] expected_overflow: bool) {
        let mut instance = GlyphInstance::default();
        instance.components.push(Component {
            base: "base".into(),
            transform: Affine::scale(scale),
        });
        let mut sources = HashMap::new();
        sources.insert(NormalizedLocation::default(), instance);

        let glyph = Glyph::new("test".into(), true, Default::default(), sources).unwrap();

        assert_eq!(
            glyph.has_overflowing_component_transforms(),
            expected_overflow
        );
    }
}
