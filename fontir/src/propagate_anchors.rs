//! Propagating anchors from components to their composites
//!
//! Glyphs.app has a feature where anchors defined in the components
//! of composite glyphs are copied into the composites themselves.
//! This implementation works at the IR level, processing each NormalizedLocation
//! independently and then aggregating into variable anchors.

use std::collections::{BTreeSet, HashMap, HashSet};

use indexmap::IndexMap;
use kurbo::{Affine, Point, Vec2};
use smol_str::{SmolStr, format_smolstr};
use write_fonts::tables::gdef::GlyphClassDef;
use write_fonts::types::Tag;

use fontdrasil::{coords::NormalizedLocation, types::GlyphName, variations::VariationModel};

use crate::{
    error::{BadGlyph, Error},
    ir::{AnchorBuilder, Component, GlyphAnchors},
    orchestration::{Context, WorkId},
};

/// Simple anchor with a name and position (not yet variable)
#[derive(Debug, Clone, PartialEq)]
struct RawAnchor {
    name: SmolStr,
    pos: Point,
}

/// Propagate anchors for all composite glyphs.
///
/// This is the main entry point for anchor propagation.
pub fn propagate_all_anchors(context: &Context) -> Result<(), Error> {
    let gdef_categories = &*context.preliminary_gdef_categories.get();
    let meta = context.static_metadata.get();
    let axis_order = meta.axes.axis_order();

    // 1. Depth-sort glyphs (process components before composites).
    // The reference implementation (Glyphs.app) does this recursively, but we opt to
    // implement it by pre-sorting the work to ensure we always process components
    // first.
    let glyphs = context.glyphs.all();
    let glyphs = glyphs
        .iter()
        .map(|g| (g.1.name.clone().into_inner(), g.1.as_ref()))
        .collect();
    let todo = fontdrasil::util::depth_sorted_composite_glyphs(&glyphs);

    // 2. For each glyph (in depth order), for each location, compute propagated anchors.
    // We store anchors for ALL glyphs here (including simple glyphs) so that when
    // processing composites, we can look up the component's anchors. However, we only
    // write anchors back to the context for glyphs that have components (step 3).
    let mut done_anchors: HashMap<(GlyphName, NormalizedLocation), Vec<RawAnchor>> = HashMap::new();
    // Track number of base glyphs for ligature anchor numbering
    let mut base_glyph_counts: HashMap<(GlyphName, NormalizedLocation), usize> = HashMap::new();
    // Cache of variation models keyed by location set, for interpolating component
    // anchors at missing locations.
    let mut variation_models: HashMap<BTreeSet<NormalizedLocation>, VariationModel> =
        HashMap::new();

    for glyph_name_smol in &todo {
        let glyph_name: GlyphName = glyph_name_smol.as_str().into();
        let glyph = context.glyphs.get(&WorkId::Glyph(glyph_name.clone()));

        for (location, instance) in glyph.sources() {
            let is_mark = gdef_categories
                .categories
                .get(&glyph_name)
                .is_some_and(|c| matches!(c, GlyphClassDef::Mark));

            let is_ligature = gdef_categories
                .categories
                .get(&glyph_name)
                .is_some_and(|c| matches!(c, GlyphClassDef::Ligature));

            // Read original anchors from source for this (glyph, location)
            // using try_get() since composites may not have anchors initially
            let existing_anchors = context
                .anchors
                .try_get(&WorkId::Anchor(glyph_name.clone()))
                .map(|ga| {
                    ga.anchors
                        .iter()
                        .filter_map(|anchor| {
                            anchor.positions.get(location).map(|pos| RawAnchor {
                                name: anchor.original_name.clone(),
                                pos: *pos,
                            })
                        })
                        .collect::<Vec<_>>()
                })
                .unwrap_or_default();

            // Compute propagated anchors for this (glyph, location)
            let anchors = anchors_traversing_components(
                &glyph_name,
                &existing_anchors,
                &instance.components,
                is_mark,
                is_ligature,
                location,
                &done_anchors,
                &mut base_glyph_counts,
                &axis_order,
                &mut variation_models,
            );

            done_anchors.insert((glyph_name.clone(), location.clone()), anchors);
        }
    }

    // 3. Build variable anchors and write to context (only for glyphs with components)
    let mut per_glyph_anchors: HashMap<GlyphName, HashMap<NormalizedLocation, Vec<_>>> =
        HashMap::new();
    for ((glyph_name, location), anchors) in done_anchors {
        per_glyph_anchors
            .entry(glyph_name)
            .or_default()
            .insert(location, anchors);
    }
    for (glyph_name, per_location_anchors) in per_glyph_anchors {
        let glyph = context.glyphs.get(&WorkId::Glyph(glyph_name.clone()));
        if glyph.has_components() {
            let variable_anchors =
                build_variable_anchors(glyph_name.clone(), per_location_anchors)?;
            context.anchors.set(variable_anchors);
        }
    }

    Ok(())
}

/// Build variable anchors from per-location raw anchors
fn build_variable_anchors(
    glyph_name: GlyphName,
    per_location_anchors: HashMap<NormalizedLocation, Vec<RawAnchor>>,
) -> Result<GlyphAnchors, BadGlyph> {
    let mut builder = AnchorBuilder::new(glyph_name);

    for (location, anchors) in per_location_anchors {
        for anchor in anchors {
            builder.add(anchor.name.clone(), location.clone(), anchor.pos)?;
        }
    }

    builder.build()
}

/// Interpolate a component's anchors at a location where it has no explicit source.
///
/// The canonical set of anchor names comes from the default source. Each anchor is
/// interpolated independently from its own set of source locations, so anchors
/// missing at some non-default locations are treated as sparse (those locations are
/// simply omitted from the interpolation for that anchor). Anchors absent from the
/// default but present at non-default locations are ignored.
///
/// Returns `None` if the component has no entries in `done_anchors`, most likely
/// because the referenced component glyph doesn't exist in the font. Individual
/// anchors that fail interpolation are skipped with a warning.
fn interpolate_component_anchors(
    component_name: &GlyphName,
    target_location: &NormalizedLocation,
    done_anchors: &HashMap<(GlyphName, NormalizedLocation), Vec<RawAnchor>>,
    axis_order: &[Tag],
    variation_models: &mut HashMap<BTreeSet<NormalizedLocation>, VariationModel>,
) -> Option<Vec<RawAnchor>> {
    // Collect all (location, anchors) pairs for this component
    let per_location: Vec<_> = done_anchors
        .iter()
        .filter(|((name, _), _)| name == component_name)
        .map(|((_, loc), anchors)| (loc.clone(), anchors.clone()))
        .collect();

    if per_location.is_empty() {
        return None;
    }

    // Get canonical anchor names from the default source location.
    let default_anchors = &per_location
        .iter()
        .find(|(loc, _)| loc.is_default())
        .expect("component should have a default source")
        .1;
    let anchor_names: Vec<SmolStr> = default_anchors.iter().map(|a| a.name.clone()).collect();
    if anchor_names.is_empty() {
        return Some(Vec::new());
    }

    // Build a per-anchor map: anchor_name -> {location -> position}
    // Each anchor tracks only the locations where it's present.
    let mut per_anchor: HashMap<&SmolStr, HashMap<NormalizedLocation, Point>> = HashMap::new();
    for (loc, anchors) in &per_location {
        for anchor in anchors {
            if anchor_names.contains(&anchor.name) {
                per_anchor
                    .entry(&anchor.name)
                    .or_default()
                    .insert(loc.clone(), anchor.pos);
            }
        }
    }

    // Interpolate each anchor independently from its own set of source locations
    let mut result = Vec::with_capacity(anchor_names.len());
    for name in &anchor_names {
        let Some(positions) = per_anchor.get(name) else {
            continue;
        };
        let locations: BTreeSet<NormalizedLocation> = positions.keys().cloned().collect();
        let point_seqs: HashMap<NormalizedLocation, Vec<Point>> = positions
            .iter()
            .map(|(loc, &pos)| (loc.clone(), vec![pos]))
            .collect();

        let model = variation_models
            .entry(locations.clone())
            .or_insert_with(|| {
                VariationModel::new(
                    locations.into_iter().collect::<HashSet<_>>(),
                    axis_order.to_vec(),
                )
            });
        let deltasets = match model.deltas(&point_seqs) {
            Ok(d) => d,
            Err(e) => {
                log::warn!(
                    "failed to compute deltas for anchor '{}' on component '{}': {e}",
                    name,
                    component_name
                );
                continue;
            }
        };
        let interpolated = model.interpolate_from_deltas(target_location, &deltasets);
        if let Some(&pos) = interpolated.first() {
            result.push(RawAnchor {
                name: name.clone(),
                pos: pos.to_point(),
            });
        }
    }

    Some(result)
}

/// Return the anchors for a glyph at a specific location, including anchors from components.
///
/// This is a reimplementation of a similarly named function in Glyphs.app.
///
/// The logic for copying anchors from components into their containing composites
/// is tricky. Anchors need to be adjusted in various ways:
///
/// - a special "*origin" anchor may exist, which modifies the position of other anchors
/// - if a component is flipped on the x or y axes, we rename "top" to "bottom"
///   and/or "left" to "right"
/// - we need to apply the transform from the component
/// - we may need to rename an anchor when the component is part of a ligature glyph
#[allow(clippy::too_many_arguments)]
fn anchors_traversing_components(
    glyph_name: &GlyphName,
    existing_anchors: &[RawAnchor],
    components: &[Component],
    is_mark: bool,
    is_ligature: bool,
    location: &NormalizedLocation,
    // Map of (glyph, location) -> anchors, updated as we process each glyph.
    // Since we sort by component depth before doing work, we know that any
    // components of the current glyph have been done first.
    done_anchors: &HashMap<(GlyphName, NormalizedLocation), Vec<RawAnchor>>,
    // each (glyph, location) writes its number of base glyphs into this map during traversal
    base_glyph_counts: &mut HashMap<(GlyphName, NormalizedLocation), usize>,
    // axis order for building VariationModel when interpolating missing component anchors
    axis_order: &[Tag],
    // cache of variation models keyed by location set
    variation_models: &mut HashMap<BTreeSet<NormalizedLocation>, VariationModel>,
) -> Vec<RawAnchor> {
    if existing_anchors.is_empty() && components.is_empty() {
        base_glyph_counts.insert((glyph_name.clone(), location.clone()), 0);
        return Vec::new();
    }

    // If this is a mark and it has anchors, just return them
    // (as in, don't even look at the components)
    if !existing_anchors.is_empty() && is_mark {
        base_glyph_counts.insert((glyph_name.clone(), location.clone()), 0); // marks have 0 base glyphs
        return origin_adjusted_anchors(existing_anchors).collect();
    }

    let mut has_underscore = existing_anchors
        .iter()
        .any(|anchor| anchor.name.starts_with('_'));

    let mut number_of_base_glyphs = 0usize;
    // we use an index map so we get the same ordering behaviour as python
    let mut all_anchors = IndexMap::new();

    for (component_idx, component) in components.iter().enumerate() {
        // Get the anchors for this component at this location.
        // Because we process dependencies first we know that all components
        // referenced have already been propagated.
        let mut anchors = match done_anchors
            .get(&(component.base.clone(), location.clone()))
            .cloned()
        {
            Some(a) => a,
            None => {
                // Component doesn't have an explicit source at this location (e.g. the
                // composite has an intermediate/brace layer that its component doesn't).
                // Interpolate the component's anchors from its available locations.
                // See: https://github.com/googlefonts/fontc/issues/1661
                match interpolate_component_anchors(
                    &component.base,
                    location,
                    done_anchors,
                    axis_order,
                    variation_models,
                ) {
                    Some(a) => a,
                    None => {
                        log::warn!(
                            "could not get or interpolate anchors for component '{}' \
                             at location {:?} in glyph '{}'",
                            component.base,
                            location,
                            glyph_name
                        );
                        continue;
                    }
                }
            }
        };

        // If this component has an explicitly set attachment anchor, use it.
        // Only applies to non-first components (component_idx > 0).
        if let Some(comp_anchor) = component.anchor.as_ref().filter(|_| component_idx > 0) {
            maybe_rename_component_anchor(comp_anchor, &mut anchors);
        }

        // Get the number of base glyphs in this component (for ligature anchor numbering).
        // If the component doesn't have a source at this exact location (e.g. the composite
        // has an intermediate layer that the component doesn't), fall back to the default
        // location's count; it should be a structural property consistent across locations.
        // The default entry is guaranteed to exist because depth-sorting ensures all of the
        // component's locations are fully processed before the current glyph, and
        // Glyph invariants require a source at the default location.
        let component_number_of_base_glyphs = base_glyph_counts
            .get(&(component.base.clone(), location.clone()))
            .or_else(|| {
                base_glyph_counts
                    .iter()
                    .find(|((name, loc), _)| name == &component.base && loc.is_default())
                    .map(|(_, count)| count)
            })
            .copied()
            .expect("base_glyph_counts should have a default-location entry for every component");

        let comb_has_underscore = anchors
            .iter()
            .any(|a| a.name.len() >= 2 && a.name.starts_with('_'));
        let comb_has_exit = anchors.iter().any(|a| a.name.starts_with("exit"));

        if !(comb_has_underscore | comb_has_exit) {
            // delete exit anchors we may have taken from earlier components
            // (since a glyph should only have one exit anchor, and logically its at the end)
            all_anchors.retain(|name: &SmolStr, _| !name.starts_with("exit"));
        }

        let scale = get_xy_rotation(component.transform);
        for mut anchor in anchors {
            let new_has_underscore = anchor.name.starts_with('_');
            if (component_idx > 0 || has_underscore) && new_has_underscore {
                continue;
            }
            // skip entry anchors on non-first glyphs
            if component_idx > 0 && anchor.name.starts_with("entry") {
                continue;
            }

            let mut new_anchor_name = rename_anchor_for_scale(&anchor.name, scale);
            if is_ligature
                && component_number_of_base_glyphs > 0
                && !new_has_underscore
                && !(new_anchor_name.starts_with("exit") || new_anchor_name.starts_with("entry"))
            {
                // dealing with marks like top_1 on a ligature
                new_anchor_name = make_liga_anchor_name(new_anchor_name, number_of_base_glyphs);
            }

            apply_transform_to_anchor(&mut anchor, component.transform);
            anchor.name = new_anchor_name;
            all_anchors.insert(anchor.name.clone(), anchor);
            has_underscore |= new_has_underscore;
        }
        number_of_base_glyphs += component_number_of_base_glyphs;
    }

    // now we've handled all the anchors from components, so copy over anchors
    // that were explicitly defined
    all_anchors.extend(origin_adjusted_anchors(existing_anchors).map(|a| (a.name.clone(), a)));

    // Calculate this glyph's base count from its anchors (for ligature numbering)
    let mut component_count_from_anchors = 0;
    let mut has_underscore_anchor = false;
    let mut has_mark_anchor = false;

    for name in all_anchors.keys() {
        has_underscore_anchor |= name.starts_with('_');
        has_mark_anchor |= name.chars().next().unwrap_or('\0').is_ascii_alphabetic();

        // Count components based on numbered anchors like top_3
        if let Some((_, suffix)) = name.split_once('_').filter(|_| {
            !is_ligature
                && number_of_base_glyphs == 0
                && !name.starts_with('_')
                && !name.starts_with("entry")
                && !name.starts_with("exit")
        }) {
            // Carets count space between components, so the last caret is n_components - 1
            let maybe_add_one = if name.starts_with("caret") { 1 } else { 0 };
            let parsed_suffix = suffix.parse::<usize>();
            if parsed_suffix.is_err() {
                log::warn!(
                    "Malformed anchor name suffix '{}' in anchor '{}', treating as 0",
                    suffix,
                    name
                );
            }
            let anchor_index = parsed_suffix.unwrap_or(0) + maybe_add_one;
            component_count_from_anchors = component_count_from_anchors.max(anchor_index);
        }
    }

    // If no underscore anchors and no base glyphs yet, but has mark anchors, set to 1
    if !has_underscore_anchor && number_of_base_glyphs == 0 && has_mark_anchor {
        number_of_base_glyphs += 1;
    }
    number_of_base_glyphs = number_of_base_glyphs.max(component_count_from_anchors);

    // Remove anchors from the opposite side of where this mark attaches
    if existing_anchors.iter().any(|a| a.name == "_bottom") {
        all_anchors.shift_remove("top");
        all_anchors.shift_remove("_top");
    }
    if existing_anchors.iter().any(|a| a.name == "_top") {
        all_anchors.shift_remove("bottom");
        all_anchors.shift_remove("_bottom");
    }

    // Store the count for this (glyph, location) so components can use it
    base_glyph_counts.insert(
        (glyph_name.clone(), location.clone()),
        number_of_base_glyphs,
    );

    all_anchors.into_values().collect()
}

/// Returns an iterator over anchors, accounting for a possible "*origin" anchor
///
/// If that anchor is present it will be used to adjust the positions of other
/// anchors, and will not be included in the output.
fn origin_adjusted_anchors(anchors: &[RawAnchor]) -> impl Iterator<Item = RawAnchor> + '_ {
    let origin = anchors
        .iter()
        .find(|a| a.name == "*origin")
        .map(|a| a.pos.to_vec2())
        .unwrap_or_default();
    anchors
        .iter()
        .filter(|a| a.name != "*origin")
        .cloned()
        .map(move |mut a| {
            a.pos -= origin;
            a
        })
}

/// Returns a vec2 where for each axis a negative value indicates that axis is flipped
fn get_xy_rotation(xform: Affine) -> Vec2 {
    // this is based on examining the behaviour of glyphs via the macro panel
    // and careful testing.
    let [xx, xy, ..] = xform.as_coeffs();
    // first take the rotation
    let angle = xy.atan2(xx);
    // then remove the rotation, and take the scale
    let rotated = xform.pre_rotate(-angle).as_coeffs();
    let mut scale = Vec2::new(rotated[0], rotated[3]);
    // then invert the scale if the rotation was >= 180°
    if (angle.to_degrees() - 180.0).abs() < 0.001 {
        scale *= -1.0;
    }

    scale
}

/// Apply the transform and round to avoid floating point errors
fn apply_transform_to_anchor(anchor: &mut RawAnchor, transform: Affine) {
    const ROUND_TO: f64 = 1e6;
    let mut pos = (transform * anchor.pos).to_vec2();
    pos *= ROUND_TO;
    pos = pos.round();
    pos /= ROUND_TO;
    anchor.pos = pos.to_point();
}

/// Rename anchor for ligatures: top -> top_1, top_2 -> top_3, etc
fn make_liga_anchor_name(name: SmolStr, base_number: usize) -> SmolStr {
    match name.split_once('_') {
        // if this anchor already has a number (like 'top_2') we want to consider that
        Some((name, suffix)) => {
            let parsed_suffix = suffix.parse::<usize>();
            if parsed_suffix.is_err() {
                log::warn!(
                    "Malformed ligature anchor name suffix '{}' in anchor '{}', treating as 1",
                    suffix,
                    name
                );
            }
            let suffix = base_number + parsed_suffix.unwrap_or(1);
            format_smolstr!("{name}_{suffix}")
        }
        // otherwise we're turning 'top' into 'top_N'
        None => format_smolstr!("{name}_{}", base_number + 1),
    }
}

/// If a component is rotated, flip bottom/top, left/right, entry/exit
fn rename_anchor_for_scale(name: &SmolStr, scale: Vec2) -> SmolStr {
    // swap the two words in the target, if they're present
    fn swap_pair(s: &mut String, one: &str, two: &str) {
        fn replace(s: &mut String, target: &str, by: &str) -> bool {
            if let Some(idx) = s.find(target) {
                s.replace_range(idx..idx + target.len(), by);
                return true;
            }
            false
        }
        // once we swap 'left' for 'right' we don't want to then check for 'right'!
        if !replace(s, one, two) {
            replace(s, two, one);
        }
    }

    if scale.x >= 0. && scale.y >= 0. {
        return name.to_owned();
    }

    let mut name = name.to_string();
    if scale.y < 0. {
        swap_pair(&mut name, "bottom", "top");
    }
    if scale.x < 0. {
        swap_pair(&mut name, "left", "right");
        swap_pair(&mut name, "exit", "entry");
    }

    SmolStr::from(name)
}

/// Rename a component's stacking anchor to match an explicit attachment anchor.
///
/// When `comp_anchor` is e.g. "top_2" or "top_alt", this renames the component's
/// "top" anchor to match (if the component has both "top" and "_top" anchors).
/// This ensures proper stacking when a mark is attached to a specific anchor.
///
/// Two common scenarios:
/// - **Ligature**: `comp_anchor = "top_2"` => rename `top` to `top_2`
/// - **Alternative**: `comp_anchor = "top_alt"` => rename `top` to `top_alt`
///
/// See: <https://handbook.glyphsapp.com/components/#reusing-shapes/anchors>
fn maybe_rename_component_anchor(comp_anchor: &SmolStr, anchors: &mut [RawAnchor]) {
    // e.g., comp_anchor = "top_2" or "top_alt" => sub_name = "top"
    let Some((sub_name, _)) = comp_anchor.as_str().split_once('_') else {
        return;
    };
    let mark_name = format_smolstr!("_{sub_name}");

    // Only rename if the component has both the base anchor (e.g., "top")
    // and the mark anchor (e.g., "_top"): i.e., it can both attach and stack
    if anchors.iter().any(|a| a.name == sub_name)
        && anchors.iter().any(|a| a.name == mark_name)
        && let Some(anchor) = anchors.iter_mut().find(|a| a.name == sub_name)
    {
        anchor.name = comp_anchor.clone();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        ir::{
            Component, GdefCategories, Glyph, GlyphInstance, PreliminaryGdefCategories,
            StaticMetadata,
        },
        orchestration::{Context, Flags},
    };
    use fontdrasil::{orchestration::Access, types::Axis};
    use std::collections::BTreeSet;

    /// Helper to create a test context with proper setup
    fn test_context() -> Context {
        let loc = NormalizedLocation::for_pos(&[("wght", 0.0)]);
        test_context_with_locations(vec![loc])
    }

    /// Create a test context with multiple locations (for variable font tests)
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
            None,
            false,
        )
        .unwrap();
        let flags = Flags::default();
        let ctx = Context::new_root(flags, None) // ir_dir=None => no IR writing
            .copy_for_work(Access::All, Access::All);
        ctx.static_metadata.set(meta);

        // Initialize empty GDEF categories (will be set by test builders)
        ctx.preliminary_gdef_categories
            .set(PreliminaryGdefCategories::default());
        ctx.gdef_categories.set(GdefCategories::default());

        ctx
    }

    /// Builder for creating a set of glyphs with anchors and components
    struct GlyphSetBuilder {
        context: Context,
        glyphs: Vec<(GlyphName, Glyph)>,
        anchors: Vec<(GlyphName, GlyphAnchors)>,
        categories: HashMap<GlyphName, GlyphClassDef>,
    }

    impl GlyphSetBuilder {
        fn new(context: Context) -> Self {
            Self {
                context,
                glyphs: Vec::new(),
                anchors: Vec::new(),
                categories: HashMap::new(),
            }
        }

        /// Add a glyph with anchors and components at the default location
        fn add_glyph(&mut self, name: &str, build_fn: impl FnOnce(&mut GlyphBuilder)) -> &mut Self {
            let mut glyph = GlyphBuilder {
                name: name.into(),
                anchors: Vec::new(),
                components: Vec::new(),
                category: None,
            };
            build_fn(&mut glyph);
            self.finalize_glyph(glyph);
            self
        }

        /// Add a glyph with per-location anchors and components (for variable font tests)
        #[allow(clippy::type_complexity)]
        fn add_variable_glyph(
            &mut self,
            name: &str,
            per_loc_data: Vec<(
                NormalizedLocation,
                Vec<(&str, f64, f64)>, // anchors: (name, x, y)
                Vec<(&str, f64, f64)>, // components: (base, x, y)
            )>,
            category: Option<GlyphClassDef>,
        ) -> &mut Self {
            let name: GlyphName = name.into();

            // Create glyph instances per location
            let mut sources = HashMap::new();
            for (loc, _, components) in &per_loc_data {
                let mut instance = GlyphInstance::default();
                for (base, x, y) in components {
                    instance
                        .components
                        .push(Component::new(*base, Affine::translate((*x, *y))));
                }
                sources.insert(loc.clone(), instance);
            }

            let glyph = Glyph::new(name.clone(), true, Default::default(), sources).unwrap();
            self.glyphs.push((name.clone(), glyph));

            // Create anchors per location
            let mut builder = AnchorBuilder::new(name.clone());
            for (loc, anchors, _) in &per_loc_data {
                for (anchor_name, x, y) in anchors {
                    builder
                        .add((*anchor_name).into(), loc.clone(), Point::new(*x, *y))
                        .unwrap();
                }
            }
            if per_loc_data
                .iter()
                .any(|(_, anchors, _)| !anchors.is_empty())
            {
                let glyph_anchors = builder.build().unwrap();
                self.anchors.push((name.clone(), glyph_anchors));
            }

            if let Some(cat) = category {
                self.categories.insert(name, cat);
            }

            self
        }

        /// Build the test environment and write to context, returning the context
        fn build(self) -> Context {
            // First pass: create all glyphs
            for (_name, glyph) in &self.glyphs {
                self.context.glyphs.set(glyph.clone());
            }

            // Second pass: add anchors
            for (_name, anchor) in &self.anchors {
                self.context.anchors.set(anchor.clone());
            }

            // Set preliminary GDEF categories
            let mut gdef_categories = PreliminaryGdefCategories::default();
            for (name, cat) in &self.categories {
                gdef_categories.categories.insert(name.clone(), *cat);
            }
            self.context
                .preliminary_gdef_categories
                .set(gdef_categories);

            self.context
        }

        /// Finalize a glyph and add it to the set
        fn finalize_glyph(&mut self, glyph: GlyphBuilder) {
            let loc = self
                .context
                .static_metadata
                .get()
                .variation_model
                .locations()
                .next()
                .unwrap()
                .clone();

            // Create glyph instance with components
            let mut instance = GlyphInstance::default();
            for (base, transform, anchor) in &glyph.components {
                instance.components.push(Component {
                    base: base.clone(),
                    transform: *transform,
                    anchor: anchor.clone(),
                });
            }

            // Create the IR glyph
            let ir_glyph = Glyph::new(
                glyph.name.clone(),
                true,
                Default::default(),
                [(loc.clone(), instance)].into(),
            )
            .unwrap();

            self.glyphs.push((ir_glyph.name.clone(), ir_glyph));

            // Create anchors if any
            if !glyph.anchors.is_empty() {
                let mut builder = AnchorBuilder::new(glyph.name.clone());
                for (name, pos) in &glyph.anchors {
                    builder.add(name.clone(), loc.clone(), *pos).unwrap();
                }
                let glyph_anchors = builder.build().unwrap();
                self.anchors.push((glyph.name.clone(), glyph_anchors));
            }

            // Store category if specified
            if let Some(cat) = glyph.category {
                self.categories.insert(glyph.name.clone(), cat);
            }
        }
    }

    /// Builder for a single glyph's anchors and components
    struct GlyphBuilder {
        name: GlyphName,
        anchors: Vec<(SmolStr, Point)>,
        /// Components: (base_name, transform, component_anchor)
        components: Vec<(GlyphName, Affine, Option<SmolStr>)>,
        category: Option<GlyphClassDef>,
    }

    impl GlyphBuilder {
        fn add_anchor(&mut self, name: &str, pos: (f64, f64)) -> &mut Self {
            self.anchors.push((name.into(), Point::new(pos.0, pos.1)));
            self
        }

        fn add_component(&mut self, base: &str, transform: Affine) -> &mut Self {
            self.components.push((base.into(), transform, None));
            self
        }

        fn add_component_at(&mut self, base: &str, pos: (f64, f64)) -> &mut Self {
            self.components
                .push((base.into(), Affine::translate(pos), None));
            self
        }

        /// Set component.anchor for the last added component
        fn set_component_anchor(&mut self, anchor: &str) -> &mut Self {
            if let Some(last) = self.components.last_mut() {
                last.2 = Some(anchor.into());
            }
            self
        }

        fn set_category(&mut self, category: GlyphClassDef) -> &mut Self {
            self.category = Some(category);
            self
        }
    }

    /// Helper to assert an anchor exists with expected position
    fn assert_anchor(context: &Context, glyph_name: &str, anchor_name: &str, expected: Point) {
        let glyph_anchors = context
            .anchors
            .try_get(&WorkId::Anchor(glyph_name.into()))
            .unwrap_or_else(|| panic!("Glyph {} should have anchors", glyph_name));

        let anchor = glyph_anchors
            .anchors
            .iter()
            .find(|a| a.kind.to_name() == anchor_name)
            .unwrap_or_else(|| {
                panic!(
                    "Anchor {} should exist on glyph {}",
                    anchor_name, glyph_name
                )
            });

        let static_metadata = context.static_metadata.get();
        let loc = static_metadata.default_location();
        let pos = anchor.positions.get(loc).unwrap_or_else(|| {
            panic!(
                "Anchor {} should have position at default location",
                anchor_name
            )
        });

        assert_eq!(
            *pos, expected,
            "Anchor {anchor_name} on {glyph_name} has wrong position"
        );
    }

    /// Helper to assert an anchor exists with expected position at a specific location
    fn assert_anchor_at_loc(
        context: &Context,
        glyph_name: &str,
        anchor_name: &str,
        loc: &NormalizedLocation,
        expected: Point,
    ) {
        let glyph_anchors = context
            .anchors
            .try_get(&WorkId::Anchor(glyph_name.into()))
            .unwrap_or_else(|| panic!("Glyph {} should have anchors", glyph_name));

        let anchor = glyph_anchors
            .anchors
            .iter()
            .find(|a| a.kind.to_name() == anchor_name)
            .unwrap_or_else(|| {
                panic!(
                    "Anchor {} should exist on glyph {}",
                    anchor_name, glyph_name
                )
            });

        let pos = anchor.positions.get(loc).unwrap_or_else(|| {
            panic!(
                "Anchor {} should have position at location {:?}",
                anchor_name, loc
            )
        });

        assert_eq!(
            *pos, expected,
            "Anchor {anchor_name} on {glyph_name} at {loc:?} has wrong position"
        );
    }

    /// Helper to get all anchor names for a glyph
    fn get_anchor_names(context: &Context, glyph_name: &str) -> Vec<String> {
        context
            .anchors
            .try_get(&WorkId::Anchor(glyph_name.into()))
            .map(|ga| {
                let mut names: Vec<String> = ga
                    .anchors
                    .iter()
                    .map(|a| a.original_name.to_string())
                    .collect();
                names.sort();
                names
            })
            .unwrap_or_default()
    }

    #[test]
    fn no_components_no_propagation() {
        let mut builder = GlyphSetBuilder::new(test_context());

        builder.add_glyph("A", |glyph| {
            glyph
                .add_anchor("top", (100.0, 500.0))
                .add_anchor("bottom", (100.0, 0.0));
        });

        let ctx = builder.build();

        // No components, so propagation should do nothing
        propagate_all_anchors(&ctx).unwrap();

        // Original anchors should remain unchanged
        assert_anchor(&ctx, "A", "top", Point::new(100.0, 500.0));
        assert_anchor(&ctx, "A", "bottom", Point::new(100.0, 0.0));
    }

    #[test]
    fn basic_composite_propagation() {
        let mut builder = GlyphSetBuilder::new(test_context());

        // Base glyph "A" with anchors
        builder.add_glyph("A", |glyph| {
            glyph
                .add_anchor("top", (234.0, 810.0))
                .add_anchor("bottom", (234.0, 0.0))
                .add_anchor("ogonek", (411.0, 0.0));
        });

        // Mark "acutecomb" with attaching anchor _top
        builder.add_glyph("acutecomb", |glyph| {
            glyph
                .add_anchor("_top", (0.0, 578.0))
                .add_anchor("top", (0.0, 810.0))
                .set_category(GlyphClassDef::Mark);
        });

        // Composite "Aacute" = A + acutecomb
        builder.add_glyph("Aacute", |glyph| {
            glyph
                .add_component_at("A", (0.0, 0.0))
                .add_component_at("acutecomb", (234.0, 232.0));
        });

        let ctx = builder.build();

        propagate_all_anchors(&ctx).unwrap();

        // Aacute should get:
        // - bottom and ogonek from A
        // - top from acutecomb (offset by component position + anchor offset)
        assert_anchor(&ctx, "Aacute", "bottom", Point::new(234.0, 0.0));
        assert_anchor(&ctx, "Aacute", "ogonek", Point::new(411.0, 0.0));
        // top = acutecomb position (234, 232) + acutecomb's top anchor (0, 810) = (234, 1042)
        assert_anchor(&ctx, "Aacute", "top", Point::new(234.0, 1042.0));

        // _top should NOT be propagated (attaching anchor on mark)
        let names = get_anchor_names(&ctx, "Aacute");
        assert!(
            !names.contains(&"_top".to_string()),
            "Attaching anchor _top should not be propagated"
        );
    }

    #[test]
    fn ligature_numbered_anchors() {
        // derived from the observed behaviour of glyphs 3.2.2 (3259)
        // this is based on the IJ glyph in Oswald (ExtraLight)
        let mut builder = GlyphSetBuilder::new(test_context());

        builder.add_glyph("I", |glyph| {
            glyph
                .add_anchor("bottom", (103.0, 0.0))
                .add_anchor("ogonek", (103.0, 0.0))
                .add_anchor("top", (103.0, 810.0))
                .add_anchor("topleft", (20.0, 810.0));
        });

        builder.add_glyph("J", |glyph| {
            glyph
                .add_anchor("bottom", (133.0, 0.0))
                .add_anchor("top", (163.0, 810.0));
        });

        builder.add_glyph("IJ", |glyph| {
            glyph
                .set_category(GlyphClassDef::Ligature)
                .add_component_at("I", (0.0, 0.0))
                .add_component_at("J", (206.0, 0.0));
        });

        let ctx = builder.build();
        propagate_all_anchors(&ctx).unwrap();

        // these were derived by running the built in glyphs.app propagate anchors
        // method from the macro panel
        assert_anchor(&ctx, "IJ", "bottom_1", Point::new(103.0, 0.0));
        assert_anchor(&ctx, "IJ", "ogonek_1", Point::new(103.0, 0.0));
        assert_anchor(&ctx, "IJ", "top_1", Point::new(103.0, 810.0));
        assert_anchor(&ctx, "IJ", "topleft_1", Point::new(20.0, 810.0));
        assert_anchor(&ctx, "IJ", "bottom_2", Point::new(339.0, 0.0));
        assert_anchor(&ctx, "IJ", "top_2", Point::new(369.0, 810.0));
    }

    #[test]
    fn anchor_transform_scale() {
        let mut builder = GlyphSetBuilder::new(test_context());

        builder.add_glyph("A", |glyph| {
            glyph.add_anchor("top", (100.0, 500.0));
        });

        builder.add_glyph("Alarge", |glyph| {
            glyph.add_component("A", Affine::scale(2.0));
        });

        let ctx = builder.build();
        propagate_all_anchors(&ctx).unwrap();

        // Anchor should be scaled 2x
        assert_anchor(&ctx, "Alarge", "top", Point::new(200.0, 1000.0));
    }

    #[test]
    fn invert_names_on_rotation() {
        // derived from the observed behaviour of glyphs 3.2.2 (3259)
        let mut builder = GlyphSetBuilder::new(test_context());

        builder.add_glyph("comma", |_| {});

        builder.add_glyph("commaaccentcomb", |glyph| {
            glyph
                .add_anchor("_bottom", (289.0, 0.0))
                .add_anchor("mybottom", (277.0, -308.0))
                .add_component_at("comma", (9.0, -164.0));
        });

        // 180° rotation with translation
        let xform = Affine::rotate(std::f64::consts::PI).then_translate((589.0, 502.0).into());
        builder.add_glyph("commaturnedabovecomb", |glyph| {
            glyph.add_component("commaaccentcomb", xform);
        });

        let ctx = builder.build();
        propagate_all_anchors(&ctx).unwrap();

        // _bottom -> _top, mybottom -> mytop
        assert_anchor(
            &ctx,
            "commaturnedabovecomb",
            "_top",
            Point::new(300.0, 502.0),
        );
        assert_anchor(
            &ctx,
            "commaturnedabovecomb",
            "mytop",
            Point::new(312.0, 810.0),
        );
    }

    #[test]
    fn affine_scale() {
        // 180° rotation: both axes negative
        let affine = Affine::rotate(std::f64::consts::PI).then_translate((589.0, 502.0).into());
        let delta = get_xy_rotation(affine);
        assert!(delta.x.is_sign_negative() && delta.y.is_sign_negative());

        // Pure translation: both axes positive
        let affine = Affine::translate((10.0, 10.0));
        let delta = get_xy_rotation(affine);
        assert!(delta.x.is_sign_positive() && delta.y.is_sign_positive());

        // Flip Y: y negative, x positive
        let flip_y = get_xy_rotation(Affine::FLIP_Y);
        assert!(flip_y.y.is_sign_negative());
        assert!(flip_y.x.is_sign_positive());

        // Flip X: x negative, y positive
        let flip_x = get_xy_rotation(Affine::FLIP_X);
        assert!(flip_x.y.is_sign_positive());
        assert!(flip_x.x.is_sign_negative());

        // Rotate 180° then flip X: x positive, y negative
        let rotate_flip = Affine::rotate(std::f64::consts::PI)
            .then_translate((589.0, 502.0).into())
            * Affine::FLIP_X;
        let rotate_flip = get_xy_rotation(rotate_flip);
        assert!(rotate_flip.x.is_sign_positive());
        assert!(rotate_flip.y.is_sign_negative());
    }

    #[test]
    fn empty_glyph_set() {
        let ctx = test_context();
        // Should not panic with empty glyph set
        let result = propagate_all_anchors(&ctx);
        assert!(result.is_ok());
    }

    #[test]
    fn attaching_anchors_from_first_component_only() {
        let mut builder = GlyphSetBuilder::new(test_context());

        builder.add_glyph("acutecomb", |glyph| {
            glyph
                .add_anchor("_top", (0.0, 500.0))
                .add_anchor("top", (0.0, 200.0))
                .set_category(GlyphClassDef::Mark);
        });

        builder.add_glyph("gravecomb", |glyph| {
            glyph
                .add_anchor("_top", (0.0, 500.0))
                .add_anchor("top", (0.0, 200.0))
                .set_category(GlyphClassDef::Mark);
        });

        // Composite mark
        builder.add_glyph("acutegrave", |glyph| {
            glyph
                .set_category(GlyphClassDef::Mark)
                .add_component_at("acutecomb", (0.0, 0.0))
                .add_component_at("gravecomb", (0.0, 300.0));
        });

        let ctx = builder.build();
        propagate_all_anchors(&ctx).unwrap();

        // Should get _top from first component only
        let names = get_anchor_names(&ctx, "acutegrave");
        assert!(names.contains(&"top".to_string()), "Should have 'top'");
        assert!(
            names.contains(&"_top".to_string()),
            "Should have '_top' from first component"
        );

        // Verify the _top anchor position is from the first component
        assert_anchor(&ctx, "acutegrave", "_top", Point::new(0.0, 500.0));
    }

    #[test]
    fn base_glyphs_propagate_attaching_anchors() {
        let mut builder = GlyphSetBuilder::new(test_context());

        builder.add_glyph("o", |glyph| {
            glyph
                .add_anchor("_top", (50.0, 400.0))
                .add_anchor("top", (50.0, 500.0))
                .set_category(GlyphClassDef::Base);
        });

        builder.add_glyph("e", |glyph| {
            glyph
                .add_anchor("top", (50.0, 450.0))
                .set_category(GlyphClassDef::Base);
        });

        builder.add_glyph("oe", |glyph| {
            glyph
                .add_component_at("o", (0.0, 0.0))
                .add_component_at("e", (100.0, 0.0));
        });

        let ctx = builder.build();
        propagate_all_anchors(&ctx).unwrap();

        // Should get all anchors including _top (attaching anchors DO propagate from base glyphs)
        let names = get_anchor_names(&ctx, "oe");
        assert!(names.contains(&"top".to_string()), "Should have 'top'");
        assert!(
            names.contains(&"_top".to_string()),
            "Should have '_top' (attaching anchors propagate from base glyphs)"
        );
    }

    /// Test that composites without ligature category don't get numbered anchors
    /// (digraphs like IJ aren't ligatures unless marked as such)
    #[test]
    fn digraphs_arent_ligatures() {
        // derived from the observed behaviour of glyphs 3.2.2 (3259)
        // this is based on the IJ glyph in Oswald (ExtraLight)
        let mut builder = GlyphSetBuilder::new(test_context());

        builder.add_glyph("I", |glyph| {
            glyph
                .add_anchor("bottom", (103.0, 0.0))
                .add_anchor("ogonek", (103.0, 0.0))
                .add_anchor("top", (103.0, 810.0))
                .add_anchor("topleft", (20.0, 810.0));
        });

        builder.add_glyph("J", |glyph| {
            glyph
                .add_anchor("bottom", (133.0, 0.0))
                .add_anchor("top", (163.0, 810.0));
        });

        // IJ without ligature category
        builder.add_glyph("IJ", |glyph| {
            glyph
                .add_component_at("I", (0.0, 0.0))
                .add_component_at("J", (206.0, 0.0));
        });

        let ctx = builder.build();
        propagate_all_anchors(&ctx).unwrap();

        // Anchors should NOT be numbered (no _1, _2 suffixes)
        let names = get_anchor_names(&ctx, "IJ");
        assert!(
            names.contains(&"bottom".to_string()),
            "Should have 'bottom' (not 'bottom_1')"
        );
        assert!(
            names.contains(&"top".to_string()),
            "Should have 'top' (not 'top_1')"
        );
        assert!(
            !names.iter().any(|n| n.contains("_1") || n.contains("_2")),
            "Non-ligature should not have numbered anchors, got {:?}",
            names
        );

        // Check positions - should get anchors from second component (J) for shared names
        assert_anchor(&ctx, "IJ", "bottom", Point::new(339.0, 0.0)); // J's bottom + 206 offset
        assert_anchor(&ctx, "IJ", "top", Point::new(369.0, 810.0)); // J's top + 206 offset
        assert_anchor(&ctx, "IJ", "ogonek", Point::new(103.0, 0.0)); // I's ogonek (unique)
        assert_anchor(&ctx, "IJ", "topleft", Point::new(20.0, 810.0)); // I's topleft (unique)
    }

    /// Test that exit anchors are removed when subsequent component lacks entry/exit
    #[test]
    fn remove_exit_anchor_on_component() {
        let mut builder = GlyphSetBuilder::new(test_context());

        // Empty glyph (like comma)
        builder.add_glyph("comma", |_| {});

        builder.add_glyph("ain-ar.init", |glyph| {
            glyph
                .add_anchor("top", (294.0, 514.0))
                .add_anchor("exit", (0.0, 0.0));
        });

        builder.add_glyph("ain-ar.init.alt", |glyph| {
            glyph
                .add_component_at("ain-ar.init", (0.0, 0.0))
                .add_component_at("comma", (0.0, 0.0));
        });

        let ctx = builder.build();
        propagate_all_anchors(&ctx).unwrap();

        // exit anchor should be removed because comma doesn't have entry/exit
        let names = get_anchor_names(&ctx, "ain-ar.init.alt");
        assert!(names.contains(&"top".to_string()), "Should have 'top'");
        assert!(
            !names.contains(&"exit".to_string()),
            "exit should be removed when component lacks entry/exit"
        );
    }

    /// Test that *origin anchor adjusts positions of other anchors
    #[test]
    fn origin_anchor_adjusts_positions() {
        let mut builder = GlyphSetBuilder::new(test_context());

        builder.add_glyph("a", |glyph| {
            glyph
                .add_anchor("*origin", (-20.0, 0.0))
                .add_anchor("bottom", (242.0, 7.0))
                .add_anchor("ogonek", (402.0, 9.0))
                .add_anchor("top", (246.0, 548.0));
        });

        builder.add_glyph("acutecomb", |glyph| {
            glyph
                .add_anchor("_top", (150.0, 580.0))
                .add_anchor("top", (170.0, 792.0))
                .set_category(GlyphClassDef::Mark);
        });

        builder.add_glyph("aacute", |glyph| {
            glyph
                .add_component_at("a", (0.0, 0.0))
                .add_component_at("acutecomb", (116.0, -32.0));
        });

        let ctx = builder.build();
        propagate_all_anchors(&ctx).unwrap();

        // *origin at (-20, 0) should shift all anchors by +20 on x
        assert_anchor(&ctx, "aacute", "bottom", Point::new(262.0, 7.0)); // 242 + 20
        assert_anchor(&ctx, "aacute", "ogonek", Point::new(422.0, 9.0)); // 402 + 20
        assert_anchor(&ctx, "aacute", "top", Point::new(286.0, 760.0)); // acutecomb top at 116+170=286, y=792-32=760
    }

    /// Test anchor propagation across multiple locations (variable font)
    #[test]
    fn propagate_across_multiple_locations() {
        // Two masters: wght=0 (light) and wght=1 (bold)
        let loc0 = NormalizedLocation::for_pos(&[("wght", 0.0)]);
        let loc1 = NormalizedLocation::for_pos(&[("wght", 1.0)]);
        let mut builder = GlyphSetBuilder::new(test_context_with_locations(vec![
            loc0.clone(),
            loc1.clone(),
        ]));

        // A glyph with different anchor positions at each master
        builder.add_variable_glyph(
            "A",
            vec![
                (
                    loc0.clone(),
                    vec![
                        ("bottom", 290.0, 10.0),
                        ("ogonek", 490.0, 3.0),
                        ("top", 290.0, 690.0),
                    ],
                    vec![],
                ),
                (
                    loc1.clone(),
                    vec![
                        ("bottom", 300.0, 0.0),
                        ("ogonek", 540.0, 10.0),
                        ("top", 300.0, 700.0),
                    ],
                    vec![],
                ),
            ],
            None,
        );

        // acutecomb with different positions per master
        builder.add_variable_glyph(
            "acutecomb",
            vec![
                (
                    loc0.clone(),
                    vec![("_top", 335.0, 502.0), ("top", 353.0, 721.0)],
                    vec![],
                ),
                (
                    loc1.clone(),
                    vec![("_top", 366.0, 500.0), ("top", 366.0, 765.0)],
                    vec![],
                ),
            ],
            Some(GlyphClassDef::Mark),
        );

        // Aacute composite with different component positions per master
        builder.add_variable_glyph(
            "Aacute",
            vec![
                (
                    loc0.clone(),
                    vec![],
                    vec![("A", 0.0, 0.0), ("acutecomb", -45.0, 188.0)],
                ),
                (
                    loc1.clone(),
                    vec![],
                    vec![("A", 0.0, 0.0), ("acutecomb", -66.0, 200.0)],
                ),
            ],
            None,
        );

        let ctx = builder.build();
        propagate_all_anchors(&ctx).unwrap();

        // Check anchors at loc0 (light master)
        // bottom and ogonek from A, top from acutecomb (shifted by component offset)
        assert_anchor_at_loc(&ctx, "Aacute", "bottom", &loc0, Point::new(290.0, 10.0));
        assert_anchor_at_loc(&ctx, "Aacute", "ogonek", &loc0, Point::new(490.0, 3.0));
        // top = acutecomb.top (353, 721) + component offset (-45, 188) = (308, 909)
        assert_anchor_at_loc(&ctx, "Aacute", "top", &loc0, Point::new(308.0, 909.0));

        // Check anchors at loc1 (bold master)
        assert_anchor_at_loc(&ctx, "Aacute", "bottom", &loc1, Point::new(300.0, 0.0));
        assert_anchor_at_loc(&ctx, "Aacute", "ogonek", &loc1, Point::new(540.0, 10.0));
        // top = acutecomb.top (366, 765) + component offset (-66, 200) = (300, 965)
        assert_anchor_at_loc(&ctx, "Aacute", "top", &loc1, Point::new(300.0, 965.0));
    }

    /// Test explicit component attachment anchors (component.anchor in Glyphs)
    ///
    /// When a mark component has an explicit anchor like "top_2", it should:
    /// 1. Attach to that specific ligature anchor position
    /// 2. Have its "top" anchor renamed to "top_2" for proper stacking
    #[test]
    fn component_anchor() {
        // Derived from the observed behaviour of glyphs 3.2.2 (3259)
        let mut builder = GlyphSetBuilder::new(test_context());

        // acutecomb: a mark with both _top (for attachment) and top (for stacking)
        builder.add_glyph("acutecomb", |glyph| {
            glyph
                .add_anchor("_top", (150.0, 580.0))
                .add_anchor("top", (170.0, 792.0))
                .set_category(GlyphClassDef::Mark);
        });

        // aa: a ligature with numbered anchors
        builder.add_glyph("aa", |glyph| {
            glyph
                .add_anchor("bottom_1", (218.0, 8.0))
                .add_anchor("bottom_2", (742.0, 7.0))
                .add_anchor("ogonek_1", (398.0, 9.0))
                .add_anchor("ogonek_2", (902.0, 9.0))
                .add_anchor("top_1", (227.0, 548.0))
                .add_anchor("top_2", (746.0, 548.0))
                .set_category(GlyphClassDef::Ligature);
        });

        // a_a: simple composite referencing aa
        builder.add_glyph("a_a", |glyph| {
            glyph
                .add_component_at("aa", (0.0, 0.0))
                .set_category(GlyphClassDef::Ligature);
        });

        // a_aacute: composite with acutecomb attached to top_2 (second letter)
        builder.add_glyph("a_aacute", |glyph| {
            glyph
                .add_component_at("a_a", (0.0, 0.0))
                .add_component_at("acutecomb", (596.0, -32.0))
                .set_component_anchor("top_2") // Attach to top_2, not top_1
                .set_category(GlyphClassDef::Ligature);
        });

        let ctx = builder.build();
        propagate_all_anchors(&ctx).unwrap();

        // Expected anchors for a_aacute:
        // - bottom_1, bottom_2, ogonek_1, ogonek_2, top_1 from aa (via a_a)
        // - top_2 should be the acutecomb's "top" renamed and transformed
        //   acutecomb.top (170, 792) + offset (596, -32) = (766, 760)
        assert_anchor(&ctx, "a_aacute", "bottom_1", Point::new(218.0, 8.0));
        assert_anchor(&ctx, "a_aacute", "bottom_2", Point::new(742.0, 7.0));
        assert_anchor(&ctx, "a_aacute", "ogonek_1", Point::new(398.0, 9.0));
        assert_anchor(&ctx, "a_aacute", "ogonek_2", Point::new(902.0, 9.0));
        assert_anchor(&ctx, "a_aacute", "top_1", Point::new(227.0, 548.0));
        // top_2 is replaced by acutecomb's stacking anchor
        assert_anchor(&ctx, "a_aacute", "top_2", Point::new(766.0, 760.0));
    }

    /// Test alternative anchor attachment (Vietnamese diacritics scenario)
    ///
    /// When a base has multiple anchors like `top` and `top_alt`, a mark can
    /// be explicitly attached to `top_alt`. The mark's `top` anchor should
    /// be renamed to `top_alt` for proper stacking.
    ///
    /// See: https://handbook.glyphsapp.com/components/#reusing-shapes/anchors
    #[test]
    fn component_anchor_alternative() {
        let mut builder = GlyphSetBuilder::new(test_context());

        // acutecomb: a mark with _top (attachment) and top (stacking)
        builder.add_glyph("acutecomb", |glyph| {
            glyph
                .add_anchor("_top", (150.0, 580.0))
                .add_anchor("top", (170.0, 792.0))
                .set_category(GlyphClassDef::Mark);
        });

        // Acircumflex: base with top (normal) and top_alt (alternative, for Vietnamese)
        builder.add_glyph("Acircumflex", |glyph| {
            glyph
                .add_anchor("top", (300.0, 700.0))
                .add_anchor("top_alt", (320.0, 720.0))
                .set_category(GlyphClassDef::Base);
        });

        // Acircumflexacute: composite using top_alt for the acute
        // This is U+1EA4, common in Vietnamese
        builder.add_glyph("Acircumflexacute", |glyph| {
            glyph
                .add_component_at("Acircumflex", (0.0, 0.0))
                .add_component_at("acutecomb", (170.0, 140.0)) // offset to position
                .set_component_anchor("top_alt")
                .set_category(GlyphClassDef::Base);
        });

        let ctx = builder.build();
        propagate_all_anchors(&ctx).unwrap();

        // Expected anchors for Acircumflexacute:
        // - top from Acircumflex (unchanged, for normal marks)
        // - top_alt renamed from acutecomb's top, at transformed position
        //   acutecomb.top (170, 792) + offset (170, 140) = (340, 932)
        assert_anchor(&ctx, "Acircumflexacute", "top", Point::new(300.0, 700.0));
        assert_anchor(
            &ctx,
            "Acircumflexacute",
            "top_alt",
            Point::new(340.0, 932.0),
        );
    }

    /// Test that suffixed entry anchors (e.g. "entry.2") on non-first components are skipped.
    ///
    /// Mirrors glyphsLib test: entry anchors start with "entry", not end with it.
    /// Before the fix, "entry.2" would not match `ends_with("entry")` and would
    /// leak through from the second component.
    #[test]
    fn entry_anchor_with_suffix_on_non_first_component() {
        let mut builder = GlyphSetBuilder::new(test_context());

        builder.add_glyph("part1", |glyph| {
            glyph.add_anchor("top", (10.0, 700.0));
        });

        builder.add_glyph("part2", |glyph| {
            glyph.add_anchor("entry.2", (10.0, 0.0));
        });

        builder.add_glyph("combo", |glyph| {
            glyph
                .add_component_at("part1", (0.0, 0.0))
                .add_component_at("part2", (100.0, 0.0));
        });

        let ctx = builder.build();
        propagate_all_anchors(&ctx).unwrap();

        // "entry.2" from the second component should be skipped
        let names = get_anchor_names(&ctx, "combo");
        assert!(names.contains(&"top".to_string()), "Should have 'top'");
        assert!(
            !names.contains(&"entry.2".to_string()),
            "entry.2 from non-first component should be skipped"
        );
    }

    /// Test that suffixed exit anchors (e.g. "exit.1") are removed when the next
    /// component lacks cursive anchors, and that cursive anchors survive on ligatures.
    #[test]
    fn cursive_anchors_with_suffix_on_ligature() {
        let mut builder = GlyphSetBuilder::new(test_context());

        builder.add_glyph("part1_part2", |glyph| {
            glyph
                .add_anchor("entry.1", (10.0, 0.0))
                .add_anchor("exit.1", (100.0, 0.0));
        });

        builder.add_glyph("combo_liga", |glyph| {
            glyph
                .set_category(GlyphClassDef::Ligature)
                .add_component_at("part1_part2", (0.0, 0.0));
        });

        let ctx = builder.build();
        propagate_all_anchors(&ctx).unwrap();

        let names = get_anchor_names(&ctx, "combo_liga");
        assert!(
            names.contains(&"entry.1".to_string()),
            "entry.1 should be propagated on ligature"
        );
        assert!(
            names.contains(&"exit.1".to_string()),
            "exit.1 should be propagated on ligature"
        );
    }

    /// Test that component anchors are interpolated at locations where a composite glyph
    /// has an intermediate ("brace") layer but its component does not.
    ///
    /// See: <https://github.com/googlefonts/fontc/issues/1661>
    #[test]
    fn interpolate_missing_component_anchors() {
        let loc_light = NormalizedLocation::for_pos(&[("wght", 0.0)]);
        let loc_medium = NormalizedLocation::for_pos(&[("wght", 0.5)]);
        let loc_bold = NormalizedLocation::for_pos(&[("wght", 1.0)]);

        let mut builder = GlyphSetBuilder::new(test_context_with_locations(vec![
            loc_light.clone(),
            loc_medium.clone(),
            loc_bold.clone(),
        ]));

        // 'ae': base glyph with 3 masters (has intermediate layer)
        builder.add_variable_glyph(
            "ae",
            vec![
                (
                    loc_light.clone(),
                    vec![("top", 300.0, 600.0), ("bottom", 300.0, 0.0)],
                    vec![],
                ),
                (
                    loc_medium.clone(),
                    vec![("top", 340.0, 650.0), ("bottom", 340.0, 0.0)],
                    vec![],
                ),
                (
                    loc_bold.clone(),
                    vec![("top", 380.0, 700.0), ("bottom", 380.0, 0.0)],
                    vec![],
                ),
            ],
            None,
        );

        // 'acutecomb': only has 2 masters (light and bold), NO intermediate layer
        builder.add_variable_glyph(
            "acutecomb",
            vec![
                (
                    loc_light.clone(),
                    vec![("_top", 100.0, 500.0), ("top", 120.0, 700.0)],
                    vec![],
                ),
                (
                    loc_bold.clone(),
                    vec![("_top", 140.0, 520.0), ("top", 160.0, 740.0)],
                    vec![],
                ),
            ],
            Some(GlyphClassDef::Mark),
        );

        // 'gravecomb': also only 2 masters (same locations as acutecomb),
        // so its variation model should be reused from the cache
        builder.add_variable_glyph(
            "gravecomb",
            vec![
                (
                    loc_light.clone(),
                    vec![("_top", 90.0, 500.0), ("top", 110.0, 680.0)],
                    vec![],
                ),
                (
                    loc_bold.clone(),
                    vec![("_top", 130.0, 520.0), ("top", 150.0, 720.0)],
                    vec![],
                ),
            ],
            Some(GlyphClassDef::Mark),
        );

        // 'aeacute': composite with 3 locations (including the intermediate)
        builder.add_variable_glyph(
            "aeacute",
            vec![
                (
                    loc_light.clone(),
                    vec![],
                    vec![("ae", 0.0, 0.0), ("acutecomb", 200.0, 100.0)],
                ),
                (
                    loc_medium.clone(),
                    vec![],
                    vec![("ae", 0.0, 0.0), ("acutecomb", 220.0, 110.0)],
                ),
                (
                    loc_bold.clone(),
                    vec![],
                    vec![("ae", 0.0, 0.0), ("acutecomb", 240.0, 120.0)],
                ),
            ],
            None,
        );

        // 'aegrave': second composite, also needs interpolation for gravecomb at medium.
        builder.add_variable_glyph(
            "aegrave",
            vec![
                (
                    loc_light.clone(),
                    vec![],
                    vec![("ae", 0.0, 0.0), ("gravecomb", 180.0, 100.0)],
                ),
                (
                    loc_medium.clone(),
                    vec![],
                    vec![("ae", 0.0, 0.0), ("gravecomb", 200.0, 110.0)],
                ),
                (
                    loc_bold.clone(),
                    vec![],
                    vec![("ae", 0.0, 0.0), ("gravecomb", 220.0, 120.0)],
                ),
            ],
            None,
        );

        let ctx = builder.build();
        propagate_all_anchors(&ctx).unwrap();

        // --- aeacute ---
        // Light master: component anchors from explicit sources
        // bottom from ae, top from acutecomb.top (120, 700) + offset (200, 100) = (320, 800)
        assert_anchor_at_loc(
            &ctx,
            "aeacute",
            "bottom",
            &loc_light,
            Point::new(300.0, 0.0),
        );
        assert_anchor_at_loc(&ctx, "aeacute", "top", &loc_light, Point::new(320.0, 800.0));

        // Bold master: component anchors from explicit sources
        // bottom from ae, top from acutecomb.top (160, 740) + offset (240, 120) = (400, 860)
        assert_anchor_at_loc(&ctx, "aeacute", "bottom", &loc_bold, Point::new(380.0, 0.0));
        assert_anchor_at_loc(&ctx, "aeacute", "top", &loc_bold, Point::new(400.0, 860.0));

        // Medium master: acutecomb should be INTERPOLATED at wght=0.5
        // Interpolated acutecomb anchors:
        //   _top: (100, 500) + 0.5 * ((140, 520) - (100, 500)) = (120, 510)
        //   top:  (120, 700) + 0.5 * ((160, 740) - (120, 700)) = (140, 720)
        // Then component offset at medium is (220, 110):
        //   top = (140, 720) + (220, 110) = (360, 830)
        // ae's bottom at medium is (340, 0)
        assert_anchor_at_loc(
            &ctx,
            "aeacute",
            "bottom",
            &loc_medium,
            Point::new(340.0, 0.0),
        );
        assert_anchor_at_loc(
            &ctx,
            "aeacute",
            "top",
            &loc_medium,
            Point::new(360.0, 830.0),
        );

        // --- aegrave ---
        // Exercises cache reuse: gravecomb has the same {light, bold} location set
        // as acutecomb, so the VariationModel built for acutecomb is reused.

        // Light: top from gravecomb.top (110, 680) + offset (180, 100) = (290, 780)
        assert_anchor_at_loc(
            &ctx,
            "aegrave",
            "bottom",
            &loc_light,
            Point::new(300.0, 0.0),
        );
        assert_anchor_at_loc(&ctx, "aegrave", "top", &loc_light, Point::new(290.0, 780.0));

        // Bold: top from gravecomb.top (150, 720) + offset (220, 120) = (370, 840)
        assert_anchor_at_loc(&ctx, "aegrave", "bottom", &loc_bold, Point::new(380.0, 0.0));
        assert_anchor_at_loc(&ctx, "aegrave", "top", &loc_bold, Point::new(370.0, 840.0));

        // Medium: gravecomb INTERPOLATED at wght=0.5
        //   top:  (110, 680) + 0.5 * ((150, 720) - (110, 680)) = (130, 700)
        // Component offset at medium is (200, 110):
        //   top = (130, 700) + (200, 110) = (330, 810)
        assert_anchor_at_loc(
            &ctx,
            "aegrave",
            "bottom",
            &loc_medium,
            Point::new(340.0, 0.0),
        );
        assert_anchor_at_loc(
            &ctx,
            "aegrave",
            "top",
            &loc_medium,
            Point::new(330.0, 810.0),
        );
    }

    /// Test that `interpolate_component_anchors` handles sparse anchors correctly:
    /// when different anchors have different sets of source locations, each is
    /// interpolated independently from its own locations.
    #[test]
    fn interpolate_sparse_component_anchors() {
        let loc_light = NormalizedLocation::for_pos(&[("wght", 0.0)]);
        let loc_medium = NormalizedLocation::for_pos(&[("wght", 0.5)]);
        let loc_semibold = NormalizedLocation::for_pos(&[("wght", 0.75)]);
        let loc_bold = NormalizedLocation::for_pos(&[("wght", 1.0)]);

        let axis_order = vec![Tag::new(b"wght")];

        let component_name: GlyphName = "acutecomb".into();

        // The component has sources at light, semibold, and bold (not medium).
        // The composite has a source at medium, which is why we need interpolation.
        // "top" is present at all 3 component locations, "_top" only at light
        // and bold (missing at semibold), so each anchor uses a different
        // VariationModel.
        let mut done_anchors: HashMap<(GlyphName, NormalizedLocation), Vec<RawAnchor>> =
            HashMap::new();
        done_anchors.insert(
            (component_name.clone(), loc_light.clone()),
            vec![
                RawAnchor {
                    name: "_top".into(),
                    pos: Point::new(100.0, 500.0),
                },
                RawAnchor {
                    name: "top".into(),
                    pos: Point::new(120.0, 700.0),
                },
            ],
        );
        done_anchors.insert(
            (component_name.clone(), loc_semibold.clone()),
            vec![
                // _top is MISSING at semibold (sparse)
                RawAnchor {
                    name: "top".into(),
                    pos: Point::new(155.0, 738.0),
                },
            ],
        );
        done_anchors.insert(
            (component_name.clone(), loc_bold.clone()),
            vec![
                RawAnchor {
                    name: "_top".into(),
                    pos: Point::new(140.0, 520.0),
                },
                RawAnchor {
                    name: "top".into(),
                    pos: Point::new(160.0, 740.0),
                },
            ],
        );

        let mut variation_models = HashMap::new();
        let result = interpolate_component_anchors(
            &component_name,
            &loc_medium,
            &done_anchors,
            &axis_order,
            &mut variation_models,
        )
        .expect("interpolation should succeed");

        // "top" has sources at {light, semibold, bold}, interpolated at medium (wght=0.5).
        // The semibold value (155, 738) is off the linear path between light (120, 700)
        // and bold (160, 740), so the 3-source model produces a different result than
        // a simple light+bold lerp (which would give (140, 720)).
        let top = result
            .iter()
            .find(|a| a.name == "top")
            .expect("should have top");
        assert!(
            (top.pos.x - 143.33).abs() < 0.01 && (top.pos.y - 725.33).abs() < 0.01,
            "top should be ~(143.33, 725.33), got {:?}",
            top.pos
        );

        // "_top" has sources at {light, bold} only, interpolated at medium (wght=0.5):
        //   (100, 500) + 0.5 * ((140, 520) - (100, 500)) = (120, 510)
        let _top = result
            .iter()
            .find(|a| a.name == "_top")
            .expect("should have _top");
        assert_eq!(
            _top.pos,
            Point::new(120.0, 510.0),
            "_top should be interpolated from light+bold"
        );

        // "top" uses a 3-location model, "_top" uses a 2-location model
        assert_eq!(
            variation_models.len(),
            2,
            "should have 2 cached models for different location sets"
        );
    }

    /// Test that a ligature composite with an intermediate layer where its
    /// component doesn't have that layer still produces consistent anchor names.
    ///
    /// Previously, `base_glyph_counts` was only populated at locations where the
    /// component had explicit sources. At missing locations, the count defaulted to 0,
    /// causing ligature anchor renumbering to be skipped (e.g. producing `bottom` instead
    /// of `bottom_2`). This mismatch between master and intermediate locations caused
    /// `AnchorBuilder::build()` to fail with "no value at default location".
    #[test]
    fn ligature_anchor_numbering_at_missing_component_location() {
        let loc_light = NormalizedLocation::for_pos(&[("wght", 0.0)]);
        let loc_medium = NormalizedLocation::for_pos(&[("wght", 0.5)]);
        let loc_bold = NormalizedLocation::for_pos(&[("wght", 1.0)]);

        let mut builder = GlyphSetBuilder::new(test_context_with_locations(vec![
            loc_light.clone(),
            loc_medium.clone(),
            loc_bold.clone(),
        ]));

        // 'f': simple glyph at all 3 locations
        builder.add_variable_glyph(
            "f",
            vec![
                (
                    loc_light.clone(),
                    vec![("bottom", 195.0, 0.0), ("top", 300.0, 800.0)],
                    vec![],
                ),
                (
                    loc_medium.clone(),
                    vec![("bottom", 230.0, 0.0), ("top", 340.0, 800.0)],
                    vec![],
                ),
                (
                    loc_bold.clone(),
                    vec![("bottom", 270.0, 0.0), ("top", 380.0, 800.0)],
                    vec![],
                ),
            ],
            None,
        );

        // 'i': composite at 2 locations only (NO intermediate)
        // composed of 'idotless' + 'dotaccentcomb'
        builder.add_variable_glyph(
            "idotless",
            vec![
                (
                    loc_light.clone(),
                    vec![("bottom", 97.0, 0.0), ("top", 200.0, 600.0)],
                    vec![],
                ),
                (
                    loc_bold.clone(),
                    vec![("bottom", 150.0, 0.0), ("top", 250.0, 620.0)],
                    vec![],
                ),
            ],
            None,
        );
        builder.add_variable_glyph(
            "dotaccentcomb",
            vec![
                (
                    loc_light.clone(),
                    vec![("_top", 100.0, 600.0), ("top", 120.0, 800.0)],
                    vec![],
                ),
                (
                    loc_bold.clone(),
                    vec![("_top", 130.0, 620.0), ("top", 160.0, 840.0)],
                    vec![],
                ),
            ],
            Some(GlyphClassDef::Mark),
        );
        builder.add_variable_glyph(
            "i",
            vec![
                (
                    loc_light.clone(),
                    vec![],
                    vec![("idotless", 0.0, 0.0), ("dotaccentcomb", 0.0, 0.0)],
                ),
                (
                    loc_bold.clone(),
                    vec![],
                    vec![("idotless", 0.0, 0.0), ("dotaccentcomb", 0.0, 0.0)],
                ),
            ],
            None,
        );

        // 'fi': ligature with 3 locations (including intermediate where 'i' is missing)
        builder.add_variable_glyph(
            "fi",
            vec![
                (
                    loc_light.clone(),
                    vec![],
                    vec![("f", 0.0, 0.0), ("i", 400.0, 0.0)],
                ),
                (
                    loc_medium.clone(),
                    vec![],
                    vec![("f", 0.0, 0.0), ("i", 450.0, 0.0)],
                ),
                (
                    loc_bold.clone(),
                    vec![],
                    vec![("f", 0.0, 0.0), ("i", 500.0, 0.0)],
                ),
            ],
            Some(GlyphClassDef::Ligature),
        );

        let ctx = builder.build();
        // This should not panic with "no value at default location"
        propagate_all_anchors(&ctx).unwrap();

        // 'fi' is a ligature of 'f' (1 base) + 'i' (1 base), so anchors should
        // be consistently numbered _1 / _2 at all locations including intermediate.
        assert_eq!(
            get_anchor_names(&ctx, "fi"),
            vec!["bottom_1", "bottom_2", "top_1", "top_2"],
        );
    }

    /// Regression test for ComforterBrush ffi/ffl: non-standard caret anchor names
    /// like "caret_1_" must survive propagation. Previously, `AnchorKind::to_name()`
    /// mapped both "caret_1" and "caret_1_" to "caret_1", losing one caret position.
    #[test]
    fn caret_anchor_names_preserved_through_propagation() {
        let mut builder = GlyphSetBuilder::new(test_context());

        builder.add_glyph("f", |glyph| {
            glyph.add_anchor("top", (300.0, 800.0));
        });
        builder.add_glyph("i", |glyph| {
            glyph.add_anchor("top", (150.0, 800.0));
        });

        // Composite with explicit caret anchors using non-standard naming
        builder.add_glyph("ffi", |glyph| {
            glyph
                .add_component_at("f", (0.0, 0.0))
                .add_component_at("f", (242.0, 0.0))
                .add_component_at("i", (588.0, 0.0))
                .add_anchor("caret_1", (242.0, 0.0))
                .add_anchor("caret_1_", (588.0, 0.0))
                .set_category(GlyphClassDef::Ligature);
        });

        let ctx = builder.build();
        propagate_all_anchors(&ctx).unwrap();

        assert_eq!(
            get_anchor_names(&ctx, "ffi"),
            vec!["caret_1", "caret_1_", "top_1", "top_2", "top_3"],
        );
    }
}
