//! Propagating anchors from components to their composites
//!
//! Glyphs.app has a nice feature where anchors defined in the components
//! of composite glyphs are copied into the composites themselves. This feature
//! is not very extensively documented, and the code here is based off the
//! Objective-C implementation, which was shared with us privately.

use std::collections::{BTreeMap, HashMap};

use indexmap::IndexMap;
use kurbo::{Affine, Vec2};
use smol_str::{format_smolstr, SmolStr};

use crate::{
    font::Anchor,
    glyphdata::{Category, Subcategory},
    Component, Font, Glyph, Layer, Shape,
};

impl Font {
    /// Copy anchors from component glyphs into their including composites
    pub fn propagate_all_anchors(&mut self) {
        propagate_all_anchors_impl(&mut self.glyphs);
    }

    /// returns a list of all glyphs, sorted by component depth.
    ///
    /// This is also used for bracket layers.
    ///
    /// That is: a glyph in the list will always occur before any other glyph that
    /// references it as a component.
    pub(crate) fn depth_sorted_composite_glyphs(&self) -> Vec<SmolStr> {
        depth_sorted_composite_glyphs_impl(&self.glyphs)
    }
}

// the actual implementation: it's easier to test a free fn
fn propagate_all_anchors_impl(glyphs: &mut BTreeMap<SmolStr, Glyph>) {
    // the reference implementation does this recursively, but we opt to
    // implement it by pre-sorting the work to ensure we always process components
    // first.
    let todo = depth_sorted_composite_glyphs_impl(glyphs);
    let mut num_base_glyphs = HashMap::new();
    // NOTE: there's an important detail here, which is that we need to call the
    // 'anchors_traversing_components' function on each glyph, and save the returned
    // anchors, but we only *set* those anchors on glyphs that have components.
    // to make this work, we write the anchors to a separate data structure, and
    // then only update the actual glyphs after we've done all the work.
    let mut all_anchors = HashMap::new();
    for name in todo {
        let glyph = glyphs.get(&name).unwrap();
        for layer in glyph.layers.iter().chain(glyph.bracket_layers.iter()) {
            let anchors =
                anchors_traversing_components(glyph, layer, &all_anchors, &mut num_base_glyphs);
            maybe_log_new_anchors(&anchors, glyph, layer);
            // if this is a bracket layer we use the actual axis values as the key,
            // since it's possible that layers with the same axis values do not share
            // the same layer id.
            let layer_id = layer
                .axis_rules_key()
                .unwrap_or_else(|| layer.layer_id.clone());
            all_anchors
                .entry(name.clone())
                .or_default()
                .insert(layer_id, anchors);
        }
    }

    // finally update our glyphs with the new anchors, where appropriate
    for (name, layer_anchors) in all_anchors {
        let glyph = glyphs.get_mut(&name).unwrap();
        if glyph.has_components() {
            for layer in &mut glyph.layers {
                if let Some(new_anchors) = layer_anchors.get(&layer.layer_id) {
                    layer.anchors = new_anchors.clone();
                }
            }

            for bracket in &mut glyph.bracket_layers {
                if let Some(new_anchors) = bracket
                    .axis_rules_key()
                    .and_then(|id| layer_anchors.get(&id))
                    .or_else(|| layer_anchors.get(bracket.master_id()))
                {
                    bracket.anchors = new_anchors.clone();
                }
            }
        }
    }
}

fn maybe_log_new_anchors(anchors: &[Anchor], glyph: &Glyph, layer: &Layer) {
    if !glyph.has_components() || !log::log_enabled!(log::Level::Trace) || anchors == layer.anchors
    {
        return;
    }
    let prev_names: Vec<_> = layer.anchors.iter().map(|a| &a.name).collect();
    let new_names: Vec<_> = anchors.iter().map(|a| &a.name).collect();
    log::trace!(
        "propagated anchors for ('{}': {prev_names:?} -> {new_names:?}",
        glyph.name,
    );
}

/// Return the anchors for this glyph, including anchors from components
///
/// This function is a reimplmentation of a similarly named function in glyphs.app.
///
/// The logic for copying anchors from components into their containing composites
/// is tricky. Anchors need to be adjusted in various ways:
///
/// - a speical "*origin" anchor may exist, which modifies the position of other anchors
/// - if a component is flipped on the x or y axes, we rename "top" to "bottom"
///   and/or "left" to "right"
/// - we need to apply the transform from the component
/// - we may need to rename an anchor when the component is part of a ligature glyph
fn anchors_traversing_components<'a>(
    glyph: &'a Glyph,
    layer: &'a Layer,
    // map of glyph -> anchors, per-layer, updated as we do each glyph;
    // since we sort by component depth before doing work, we know that any components
    // of the current glyph have been done first.
    done_anchors: &HashMap<SmolStr, HashMap<String, Vec<Anchor>>>,
    // each (glyph, layer) writes its number of base glyphs into this map during traversal
    base_glyph_counts: &mut HashMap<(SmolStr, &'a str), usize>,
) -> Vec<Anchor> {
    if layer.anchors.is_empty() && layer.components().count() == 0 {
        return Vec::new();
    }

    // if this is a mark and it has anchors, just return them
    // (as in, don't even look at the components)
    if !layer.anchors.is_empty() && glyph.category == Some(Category::Mark) {
        return origin_adjusted_anchors(&layer.anchors).collect();
    }

    let is_ligature = glyph.sub_category == Some(Subcategory::Ligature);
    let mut has_underscore = layer
        .anchors
        .iter()
        .any(|anchor| anchor.name.starts_with('_'));

    let mut number_of_base_glyphs = 0usize;
    // we use an index map so we get the same ordering behaviour as python
    let mut all_anchors = IndexMap::new();
    for (component_idx, component) in layer.components().enumerate() {
        // because we process dependencies first we know that all components
        // referenced have already been propagated
        let Some(mut anchors) =
            // equivalent to the recursive call in the reference impl
            get_component_layer_anchors(component, layer,  done_anchors)
        else {
            log::warn!(
                "could not get layer '{}' for component '{}' of glyph '{}'",
                layer.layer_id,
                component.name,
                glyph.name
            );
            continue;
        };

        // if this component has an explicitly set attachment anchor, use it
        if let Some(comp_anchor) = component.anchor.as_ref().filter(|_| component_idx > 0) {
            maybe_rename_component_anchor(comp_anchor.to_owned(), &mut anchors);
        }

        let component_number_of_base_glyphs = base_glyph_counts
            .get(&(component.name.clone(), layer.layer_id.as_str()))
            .copied()
            .unwrap_or(0);

        let comb_has_underscore = anchors
            .iter()
            .any(|a| a.name.len() >= 2 && a.name.starts_with('_'));
        let comb_has_exit = anchors.iter().any(|a| a.name.ends_with("exit"));
        if !(comb_has_underscore | comb_has_exit) {
            // delete exit anchors we may have taken from earlier components
            // (since a glyph should only have one exit anchor, and logically its at the end)
            all_anchors.retain(|name: &SmolStr, _| !name.ends_with("exit"));
        }

        let scale = get_xy_rotation(component.transform);
        for mut anchor in anchors {
            let new_has_underscore = anchor.name.starts_with('_');
            if (component_idx > 0 || has_underscore) && new_has_underscore {
                continue;
            }
            // skip entry anchors on non-first glyphs
            if component_idx > 0 && anchor.name.ends_with("entry") {
                continue;
            }

            let mut new_anchor_name = rename_anchor_for_scale(&anchor.name, scale);
            if is_ligature
                && component_number_of_base_glyphs > 0
                && !new_has_underscore
                && !(new_anchor_name.ends_with("exit") || new_anchor_name.ends_with("entry"))
            {
                // dealing with marks like top_1 on a ligature
                new_anchor_name = make_liga_anchor_name(new_anchor_name, number_of_base_glyphs);
            }

            apply_transform_to_anchor(&mut anchor, component.transform);
            anchor.name = new_anchor_name;
            all_anchors.insert(anchor.name.clone(), anchor);
            has_underscore |= new_has_underscore;
        }
        number_of_base_glyphs += base_glyph_counts
            .get(&(component.name.clone(), layer.layer_id.as_str()))
            .copied()
            .unwrap_or(0);
    }

    // now we've handled all the anchors from components, so copy over anchors
    // that were explicitly defined on this layer:
    all_anchors.extend(origin_adjusted_anchors(&layer.anchors).map(|a| (a.name.clone(), a)));
    let mut has_underscore_anchor = false;
    let mut has_mark_anchor = false;
    let mut component_count_from_anchors = 0;

    // now we count how many components we have, based on our anchors
    for name in all_anchors.keys() {
        has_underscore_anchor |= name.starts_with('_');
        has_mark_anchor |= name.chars().next().unwrap_or('\0').is_ascii_alphabetic();
        if !is_ligature
            && number_of_base_glyphs == 0
            && !name.starts_with('_')
            && !(name.ends_with("entry") | name.ends_with("exit"))
            && name.contains('_')
        {
            let (_, suffix) = name.split_once('_').unwrap();
            // carets count space between components, so the last caret
            // is n_components - 1
            let maybe_add_one = if name.starts_with("caret") { 1 } else { 0 };
            let anchor_index = suffix.parse::<usize>().unwrap_or(0) + maybe_add_one;
            component_count_from_anchors = component_count_from_anchors.max(anchor_index);
        }
    }
    if !has_underscore_anchor && number_of_base_glyphs == 0 && has_mark_anchor {
        number_of_base_glyphs += 1;
    }
    number_of_base_glyphs = number_of_base_glyphs.max(component_count_from_anchors);

    if layer.anchors.iter().any(|a| a.name == "_bottom") {
        all_anchors.shift_remove("top");
        all_anchors.shift_remove("_top");
    }
    if layer.anchors.iter().any(|a| a.name == "_top") {
        all_anchors.shift_remove("bottom");
        all_anchors.shift_remove("_bottom");
    }
    base_glyph_counts.insert(
        (glyph.name.clone(), layer.layer_id.as_str()),
        number_of_base_glyphs,
    );
    all_anchors.into_values().collect()
}

/// returns an iterator over anchors in the layer, accounting for a possible "*origin" anchor
///
/// If that anchor is present it will be used to adjust the positions of other
/// anchors, and will not be included in the output.
fn origin_adjusted_anchors(anchors: &[Anchor]) -> impl Iterator<Item = Anchor> + '_ {
    let origin = anchors
        .iter()
        .find_map(Anchor::origin_delta)
        .unwrap_or_default();
    anchors
        .iter()
        .filter(|a| !a.is_origin())
        .cloned()
        .map(move |mut a| {
            a.pos -= origin;
            a
        })
}

// returns a vec2 where for each axis a negative value indicates that axis is considerd flipped
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

// apply the transform but also do some rounding, so we don't have anchors
// with points like (512, 302.000000006)
fn apply_transform_to_anchor(anchor: &mut Anchor, transform: Affine) {
    // how many zeros do we care about? not this many
    const ROUND_TO: f64 = 1e6;
    let mut pos = (transform * anchor.pos).to_vec2();
    pos *= ROUND_TO;
    pos = pos.round();
    pos /= ROUND_TO;
    anchor.pos = pos.to_point();
}

fn maybe_rename_component_anchor(comp_name: SmolStr, anchors: &mut [Anchor]) {
    // e.g, go from 'top' to 'top_1'
    let Some((sub_name, _)) = comp_name.as_str().split_once('_') else {
        return;
    };
    let mark_name = format_smolstr!("_{sub_name}");
    if anchors.iter().any(|a| a.name == sub_name) && anchors.iter().any(|a| a.name == mark_name) {
        anchors
            .iter_mut()
            .find(|a| a.name == sub_name)
            .unwrap()
            .name = comp_name.clone();
    }
}

fn make_liga_anchor_name(name: SmolStr, base_number: usize) -> SmolStr {
    match name.split_once('_') {
        // if this anchor already has a number (like 'top_2') we want to consider that
        Some((name, suffix)) => {
            let suffix = base_number + suffix.parse::<usize>().ok().unwrap_or(1);
            format_smolstr!("{name}_{suffix}")
        }
        // otherwise we're turning 'top' into 'top_N'
        None => format_smolstr!("{name}_{}", base_number + 1),
    }
}

// if a component is rotated, flip bottom/top, left/right, entry/exit
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

// in glyphs.app this function will synthesize a layer if it is missing.
fn get_component_layer_anchors(
    component: &Component,
    layer: &Layer,
    anchors: &HashMap<SmolStr, HashMap<String, Vec<Anchor>>>,
) -> Option<Vec<Anchor>> {
    let anchors = anchors.get(&component.name)?;
    if let Some(id) = layer.axis_rules_key() {
        anchors.get(&id).or_else(|| anchors.get(layer.master_id()))
    } else {
        anchors.get(&layer.layer_id)
    }
    .cloned()
}

/// returns a list of all glyphs, sorted by component depth.
///
/// That is: a glyph in the list will always occur before any other glyph that
/// references it as a component.
fn depth_sorted_composite_glyphs_impl(glyphs: &BTreeMap<SmolStr, Glyph>) -> Vec<SmolStr> {
    // map of the maximum component depth of a glyph.
    // - a glyph with no components has depth 0,
    // - a glyph with a component has depth 1,
    // - a glyph with a component that itself has a component has depth 2, etc

    // For context, in a typical font most glyphs are not composites and composites are not terribly deep
    // indeterminate_depth is initially all components then empties as we find depths for them
    let mut indeterminate_depth = Vec::new();
    let mut depths: HashMap<_, _> = glyphs
        .iter()
        .filter_map(|(name, glyph)| {
            if glyph.has_components() {
                indeterminate_depth.push(glyph); // maybe some of our components are components
                None
            } else {
                Some((name, 0))
            }
        })
        .collect();

    // Progress is the number of glyphs we processed in a cycle, initially the number of simple glyphs.
    // If we fail to make progress all that's left is glyphs with cycles or bad references
    let mut progress = glyphs.len() - indeterminate_depth.len();
    while progress > 0 {
        progress = indeterminate_depth.len();

        // We know the depth once every component we rely on has a depth
        indeterminate_depth.retain(|glyph| {
            let max_component_depth = glyph
                .layers
                .iter()
                .chain(glyph.bracket_layers.iter())
                .flat_map(|layer| layer.shapes.iter())
                .filter_map(|shape| match shape {
                    Shape::Path(..) => None,
                    Shape::Component(c) => Some(&c.name),
                })
                .map(|name| depths.get(name).copied())
                .try_fold(0, |acc, e| e.map(|e| acc.max(e)));
            if let Some(max_component_depth) = max_component_depth {
                depths.insert(&glyph.name, max_component_depth + 1);
            }
            max_component_depth.is_none() // retain if we don't yet have an answer
        });

        progress -= indeterminate_depth.len();
    }

    // We may have failed some of you
    if !indeterminate_depth.is_empty() {
        // Shouldn't we return an error instead of just dropping results?
        for g in indeterminate_depth.iter() {
            depths.remove(&g.name);
        }

        if log::log_enabled!(log::Level::Warn) {
            let mut names = indeterminate_depth
                .into_iter()
                .map(|g| g.name.as_str())
                .collect::<Vec<_>>();
            names.sort();
            log::warn!(
                "Invalid component graph (cycles or bad refs) for {} glyphs: {:?}",
                names.len(),
                names
            );
        }
    }

    let mut by_depth = depths
        .into_iter()
        .map(|(glyph, depth)| (depth, glyph))
        .collect::<Vec<_>>();

    by_depth.sort();
    by_depth.into_iter().map(|(_, name)| name.clone()).collect()
}

#[cfg(test)]
mod tests {

    use std::collections::BTreeSet;

    use kurbo::Point;

    use crate::{
        font::{AxisRule, LayerAttributes},
        glyphdata::GlyphData,
        Layer, Shape,
    };

    use super::*;

    #[derive(Debug, Default)]
    struct GlyphSetBuilder(BTreeMap<SmolStr, Glyph>);

    impl GlyphSetBuilder {
        fn new() -> Self {
            Default::default()
        }

        fn build(&self) -> BTreeMap<SmolStr, Glyph> {
            self.0.clone()
        }

        fn add_glyph(&mut self, name: &str, build_fn: impl FnOnce(&mut GlyphBuilder)) -> &mut Self {
            let mut glyph = GlyphBuilder::new(name);
            build_fn(&mut glyph);
            self.0.insert(glyph.glyph.name.clone(), glyph.build());
            self
        }
    }
    // a little helper to make it easier to generate data for these tests
    #[derive(Debug, Default)]
    struct GlyphBuilder {
        glyph: Glyph,
        last_layer_is_bracket: bool,
    }

    impl GlyphBuilder {
        fn new(name: &str) -> Self {
            let mut this = GlyphBuilder {
                glyph: Glyph {
                    name: name.into(),
                    export: true,
                    ..Default::default()
                },
                last_layer_is_bracket: false,
            };
            if let Some(result) = GlyphData::default().query(name, None) {
                this.set_category(result.category);
                if let Some(subcategory) = result.subcategory {
                    this.set_subcategory(subcategory);
                }
                if let Some(unicode) = result.codepoint {
                    this.set_unicode(unicode);
                }
            }
            this.add_layer();
            this
        }

        fn build(&self) -> Glyph {
            self.glyph.clone()
        }

        /// Add a new layer to a glyph; all other operations work on the last added layer
        fn add_layer(&mut self) -> &mut Self {
            self.glyph.layers.push(Layer {
                layer_id: format!("master-{}", self.glyph.layers.len()),
                ..Default::default()
            });
            self.last_layer_is_bracket = false;
            self
        }

        fn add_bracket_layer(&mut self, axis_rules: Vec<(Option<i64>, Option<i64>)>) -> &mut Self {
            let assoc_master_id = self.glyph.layers.last().unwrap().layer_id.clone();
            self.glyph.bracket_layers.push({
                Layer {
                    layer_id: format!("bracket-{}", self.glyph.bracket_layers.len()),
                    associated_master_id: Some(assoc_master_id),
                    attributes: LayerAttributes {
                        axis_rules: axis_rules
                            .into_iter()
                            .map(|(min, max)| AxisRule { min, max })
                            .collect(),
                        ..Default::default()
                    },
                    ..Default::default()
                }
            });
            self.last_layer_is_bracket = true;
            self
        }

        fn last_layer_mut(&mut self) -> &mut Layer {
            if self.last_layer_is_bracket {
                self.glyph.bracket_layers.last_mut().unwrap()
            } else {
                self.glyph.layers.last_mut().unwrap()
            }
        }

        fn set_unicode(&mut self, unicode: u32) -> &mut Self {
            self.glyph.unicode = BTreeSet::from([unicode]);
            self
        }

        fn set_category(&mut self, category: Category) -> &mut Self {
            self.glyph.category = Some(category);
            self
        }

        fn set_subcategory(&mut self, sub_category: Subcategory) -> &mut Self {
            self.glyph.sub_category = Some(sub_category);
            self
        }

        // use an int for pos to simplify the call site ('0' instead of'0.0')
        fn add_component(&mut self, name: &str, pos: (i32, i32)) -> &mut Self {
            self.last_layer_mut()
                .shapes
                .push(Shape::Component(Component {
                    name: name.into(),
                    transform: Affine::translate((pos.0 as f64, pos.1 as f64)),
                    ..Default::default()
                }));
            self
        }

        /// Set an explicit translate + rotation for the component
        fn rotate_component(&mut self, degrees: f64) -> &mut Self {
            if let Some(Shape::Component(comp)) = self.last_layer_mut().shapes.last_mut() {
                comp.transform = comp.transform.pre_rotate(degrees.to_radians());
            }
            self
        }

        /// add an explicit anchor to the last added component
        fn add_component_anchor(&mut self, name: &str) -> &mut Self {
            if let Some(Shape::Component(comp)) = self.last_layer_mut().shapes.last_mut() {
                comp.anchor = Some(name.into());
            }
            self
        }
        fn add_anchor(&mut self, name: &str, pos: (i32, i32)) -> &mut Self {
            self.last_layer_mut().anchors.push(Anchor {
                name: name.into(),
                pos: Point::new(pos.0 as _, pos.1 as _),
            });
            self
        }
    }

    impl PartialEq<(&str, (f64, f64))> for Anchor {
        fn eq(&self, other: &(&str, (f64, f64))) -> bool {
            self.name == other.0 && self.pos == other.1.into()
        }
    }

    #[test]
    fn components_by_depth() {
        fn make_glyph(name: &str, components: &[&str]) -> Glyph {
            let mut builder = GlyphBuilder::new(name);
            for comp in components {
                builder.add_component(comp, (0, 0)); // pos doesn't matter for this test
            }
            builder.build()
        }

        let glyphs: &[(&str, &[&str])] = &[
            ("A", &[]),
            ("E", &[]),
            ("acutecomb", &[]),
            ("brevecomb", &[]),
            ("brevecomb_acutecomb", &["acutecomb", "brevecomb"]),
            ("AE", &["A", "E"]),
            ("Aacute", &["A", "acutecomb"]),
            ("Aacutebreve", &["A", "brevecomb_acutecomb"]),
            ("AEacutebreve", &["AE", "brevecomb_acutecomb"]),
        ];
        let glyphs = glyphs
            .iter()
            .map(|(name, components)| make_glyph(name, components))
            .map(|glyph| (glyph.name.clone(), glyph))
            .collect();

        let result = depth_sorted_composite_glyphs_impl(&glyphs);
        let expected = [
            "A",
            "E",
            "acutecomb",
            "brevecomb",
            "AE",
            "Aacute",
            "brevecomb_acutecomb",
            "AEacutebreve",
            "Aacutebreve",
        ]
        .into_iter()
        .map(SmolStr::new)
        .collect::<Vec<_>>();

        assert_eq!(result, expected)
    }

    #[test]
    fn no_components_anchors_are_unchanged() {
        // derived from the observed behaviour of glyphs 3.2.2 (3259)
        let glyphs = GlyphSetBuilder::new()
            .add_glyph("A", |glyph| {
                glyph
                    .add_anchor("bottom", (234, 0))
                    .add_anchor("ogonek", (411, 0))
                    .add_anchor("top", (234, 810));
            })
            .add_glyph("acutecomb", |glyph| {
                glyph
                    .add_anchor("_top", (0, 578))
                    .add_anchor("top", (0, 810));
            })
            .build();

        let mut glyphs_2 = glyphs.clone();
        propagate_all_anchors_impl(&mut glyphs_2);
        assert_eq!(glyphs, glyphs_2, "nothing should change here");
    }

    #[test]
    fn basic_composite_anchor() {
        // derived from the observed behaviour of glyphs 3.2.2 (3259)
        let mut glyphs = GlyphSetBuilder::new()
            .add_glyph("A", |glyph| {
                glyph
                    .add_anchor("bottom", (234, 0))
                    .add_anchor("ogonek", (411, 0))
                    .add_anchor("top", (234, 810));
            })
            .add_glyph("acutecomb", |glyph| {
                glyph
                    .add_anchor("_top", (0, 578))
                    .add_anchor("top", (0, 810));
            })
            .add_glyph("Aacute", |glyph| {
                glyph
                    .add_component("A", (0, 0))
                    .add_component("acutecomb", (234, 232));
            })
            .build();
        propagate_all_anchors_impl(&mut glyphs);

        let new_glyph = glyphs.get("Aacute").unwrap();
        assert_eq!(
            new_glyph.layers[0].anchors,
            [
                ("bottom", (234., 0.)),
                ("ogonek", (411., 0.)),
                ("top", (234., 1042.))
            ]
        );
    }

    #[test]
    fn propagate_ligature_anchors() {
        // derived from the observed behaviour of glyphs 3.2.2 (3259)
        // this is based on the IJ glyph in Oswald (ExtraLight)
        let mut glyphs = GlyphSetBuilder::new()
            .add_glyph("I", |glyph| {
                glyph
                    .add_anchor("bottom", (103, 0))
                    .add_anchor("ogonek", (103, 0))
                    .add_anchor("top", (103, 810))
                    .add_anchor("topleft", (20, 810));
            })
            .add_glyph("J", |glyph| {
                glyph
                    .add_anchor("bottom", (133, 0))
                    .add_anchor("top", (163, 810));
            })
            .add_glyph("IJ", |glyph| {
                glyph
                    // we need to manually override this, it isn't actually a
                    // ligature by default
                    .set_subcategory(Subcategory::Ligature)
                    .add_component("I", (0, 0))
                    .add_component("J", (206, 0));
            })
            .build();
        propagate_all_anchors_impl(&mut glyphs);
        let ij = glyphs.get("IJ").unwrap();
        // these were derived by running the built in glyphs.app propagate anchors
        // method from the macro panel
        assert_eq!(
            ij.layers[0].anchors,
            [
                ("bottom_1", (103., 0.)),
                ("ogonek_1", (103., 0.)),
                ("top_1", (103., 810.)),
                ("topleft_1", (20., 810.)),
                ("bottom_2", (339., 0.)),
                ("top_2", (369., 810.))
            ]
        )
    }

    #[test]
    fn digraphs_arent_ligatures() {
        // derived from the observed behaviour of glyphs 3.2.2 (3259)
        // this is based on the IJ glyph in Oswald (ExtraLight)
        let mut glyphs = GlyphSetBuilder::new()
            .add_glyph("I", |glyph| {
                glyph
                    .add_anchor("bottom", (103, 0))
                    .add_anchor("ogonek", (103, 0))
                    .add_anchor("top", (103, 810))
                    .add_anchor("topleft", (20, 810));
            })
            .add_glyph("J", |glyph| {
                glyph
                    .add_anchor("bottom", (133, 0))
                    .add_anchor("top", (163, 810));
            })
            .add_glyph("IJ", |glyph| {
                glyph
                    .add_component("I", (0, 0))
                    .add_component("J", (206, 0));
            })
            .build();
        propagate_all_anchors_impl(&mut glyphs);
        let ij = glyphs.get("IJ").unwrap();
        // these were derived by running the built in glyphs.app propagate anchors
        // method from the macro panel
        assert_eq!(
            ij.layers[0].anchors,
            [
                ("bottom", (339., 0.)),
                ("ogonek", (103., 0.)),
                ("top", (369., 810.)),
                ("topleft", (20., 810.)),
            ]
        )
    }

    #[test]
    fn propagate_across_layers() {
        // derived from the observed behaviour of glyphs 3.2.2 (3259)
        let mut glyphs = GlyphSetBuilder::new()
            .add_glyph("A", |glyph| {
                glyph
                    .add_anchor("bottom", (290, 10))
                    .add_anchor("ogonek", (490, 3))
                    .add_anchor("top", (290, 690))
                    .add_layer()
                    .add_anchor("bottom", (300, 0))
                    .add_anchor("ogonek", (540, 10))
                    .add_anchor("top", (300, 700));
            })
            .add_glyph("acutecomb", |glyph| {
                glyph
                    .add_anchor("_top", (335, 502))
                    .add_anchor("top", (353, 721))
                    .add_layer()
                    .add_anchor("_top", (366, 500))
                    .add_anchor("top", (366, 765));
            })
            .add_glyph("Aacute", |glyph| {
                glyph
                    .add_component("A", (0, 0))
                    .add_component("acutecomb", (-45, 188))
                    .add_layer()
                    .add_component("A", (0, 0))
                    .add_component("acutecomb", (-66, 200));
            })
            .build();
        propagate_all_anchors_impl(&mut glyphs);

        let new_glyph = glyphs.get("Aacute").unwrap();
        assert_eq!(
            new_glyph.layers[0].anchors,
            [
                ("bottom", (290., 10.)),
                ("ogonek", (490., 3.)),
                ("top", (308., 909.))
            ]
        );

        assert_eq!(
            new_glyph.layers[1].anchors,
            [
                ("bottom", (300., 0.)),
                ("ogonek", (540., 10.)),
                ("top", (300., 965.))
            ]
        );
    }

    // adapted from https://github.com/googlefonts/glyphsLib/blob/f81c00c74/tests/builder/transformations/propagate_anchors_test.py#L384
    // which was itself adapted from our test, above, but more is better?
    #[test]
    fn propagate_across_layers_including_bracket_layers() {
        let _ = env_logger::builder().is_test(true).try_init();
        let mut glyphs = GlyphSetBuilder::new()
            .add_glyph("A", |glyph| {
                glyph
                    .add_anchor("bottom", (206, 16))
                    .add_anchor("ogonek", (360, 13))
                    .add_anchor("top", (212, 724))
                    .add_bracket_layer(vec![(Some(500), None)])
                    .add_anchor("bottom", (206, 16))
                    .add_anchor("top", (212, 724))
                    .add_layer()
                    .add_anchor("bottom", (278, 12))
                    .add_anchor("ogonek", (464, 13))
                    .add_anchor("top", (281, 758))
                    .add_bracket_layer(vec![(Some(500), None)])
                    .add_anchor("bottom", (278, 12))
                    .add_anchor("top", (281, 758));
                //.add_backup_layer()
                //.add_anchor("top", (290, 690))
            })
            .add_glyph("acutecomb", |glyph| {
                glyph
                    .add_anchor("_top", (150, 580))
                    .add_anchor("top", (170, 792))
                    .add_layer()
                    .add_anchor("_top", (167, 580))
                    .add_anchor("top", (170, 792));
                //.add_backup_layer()
                //.add_anchor("_top", (335, 502));
            })
            .add_glyph("Aacute", |glyph| {
                glyph
                    .add_component("A", (0, 0))
                    .add_component("acutecomb", (62, 144))
                    .add_bracket_layer(vec![(Some(500), None)])
                    .add_component("A", (20, 0))
                    .add_component("acutecomb", (82, 144))
                    .add_layer()
                    .add_component("A", (0, 0))
                    .add_component("acutecomb", (114, 178))
                    .add_bracket_layer(vec![(Some(500), None)])
                    .add_component("A", (30, 0))
                    .add_component("acutecomb", (144, 178));
                //.add_backup_layer()
                //.add_component("A", (0, 0))
                //.add_component("acutecomb", (-45, 188))
            })
            .build();

        propagate_all_anchors_impl(&mut glyphs);

        let new_glyph = glyphs.get("Aacute").unwrap();
        assert_eq!(new_glyph.layers[0].layer_id, "master-0");
        assert_eq!(
            new_glyph.layers[0].anchors,
            [
                ("bottom", (206., 16.)),
                ("ogonek", (360., 13.)),
                ("top", (232., 936.))
            ]
        );

        assert_eq!(new_glyph.layers[1].layer_id, "master-1");
        assert_eq!(
            new_glyph.layers[1].anchors,
            [
                ("bottom", (278., 12.)),
                ("ogonek", (464., 13.)),
                ("top", (284., 970.))
            ]
        );

        // and bracket layers too??
        assert_eq!(new_glyph.bracket_layers.len(), 2);
        assert_eq!(
            new_glyph.bracket_layers[0].anchors,
            [
                // from bracket layer on A, with xform from transform on bracket component
                ("bottom", (226., 16.)),
                // from default master on 'acute', shifted by xform
                ("top", (252., 936.))
            ]
        );
        assert_eq!(
            new_glyph.bracket_layers[1].anchors,
            [("bottom", (308., 12.)), ("top", (314., 970.))]
        );
    }

    #[test]
    fn remove_exit_anchor_on_component() {
        // derived from the observed behaviour of glyphs 3.2.2 (3259)
        let mut glyphs = GlyphSetBuilder::new()
            .add_glyph("comma", |_| {})
            .add_glyph("ain-ar.init", |glyph| {
                glyph
                    .add_anchor("top", (294, 514))
                    .add_anchor("exit", (0, 0));
            })
            .add_glyph("ain-ar.init.alt", |glyph| {
                glyph
                    .add_component("ain-ar.init", (0, 0))
                    .add_component("comma", (0, 0));
            })
            .build();
        propagate_all_anchors_impl(&mut glyphs);

        let new_glyph = glyphs.get("ain-ar.init.alt").unwrap();
        assert_eq!(new_glyph.layers[0].anchors, [("top", (294., 514.)),]);
    }

    #[test]
    fn component_anchor() {
        // derived from the observed behaviour of glyphs 3.2.2 (3259)
        let mut glyphs = GlyphSetBuilder::new()
            .add_glyph("acutecomb", |glyph| {
                glyph
                    .add_anchor("_top", (150, 580))
                    .add_anchor("top", (170, 792));
            })
            .add_glyph("aa", |glyph| {
                glyph
                    .add_anchor("bottom_1", (218, 8))
                    .add_anchor("bottom_2", (742, 7))
                    .add_anchor("ogonek_1", (398, 9))
                    .add_anchor("ogonek_2", (902, 9))
                    .add_anchor("top_1", (227, 548))
                    .add_anchor("top_2", (746, 548));
            })
            .add_glyph("a_a", |glyph| {
                glyph.add_component("aa", (0, 0));
            })
            .add_glyph("a_aacute", |glyph| {
                glyph
                    .add_component("a_a", (0, 0))
                    .add_component("acutecomb", (596, -32))
                    .add_component_anchor("top_2");
            })
            .build();
        propagate_all_anchors_impl(&mut glyphs);

        let new_glyph = glyphs.get("a_aacute").unwrap();
        assert_eq!(
            new_glyph.layers[0].anchors,
            [
                ("bottom_1", (218., 8.)),
                ("bottom_2", (742., 7.)),
                ("ogonek_1", (398., 9.)),
                ("ogonek_2", (902., 9.)),
                ("top_1", (227., 548.)),
                ("top_2", (766., 760.)),
            ]
        );
    }

    #[test]
    fn origin_anchor() {
        // derived from the observed behaviour of glyphs 3.2.2 (3259)
        let mut glyphs = GlyphSetBuilder::new()
            .add_glyph("a", |glyph| {
                glyph
                    .add_anchor("*origin", (-20, 0))
                    .add_anchor("bottom", (242, 7))
                    .add_anchor("ogonek", (402, 9))
                    .add_anchor("top", (246, 548));
            })
            .add_glyph("acutecomb", |glyph| {
                glyph
                    .add_anchor("_top", (150, 580))
                    .add_anchor("top", (170, 792));
            })
            .add_glyph("aacute", |glyph| {
                glyph
                    .add_component("a", (0, 0))
                    .add_component("acutecomb", (116, -32));
            })
            .build();
        propagate_all_anchors_impl(&mut glyphs);

        let new_glyph = glyphs.get("aacute").unwrap();
        assert_eq!(
            new_glyph.layers[0].anchors,
            [
                ("bottom", (262.0, 7.0)),
                ("ogonek", (422.0, 9.0)),
                ("top", (286.0, 760.0)),
            ]
        );
    }

    #[test]
    fn invert_names_on_rotation() {
        // derived from the observed behaviour of glyphs 3.2.2 (3259)
        let mut glyphs = GlyphSetBuilder::new()
            .add_glyph("comma", |_| {})
            .add_glyph("commaaccentcomb", |glyph| {
                glyph
                    .add_anchor("_bottom", (289, 0))
                    .add_anchor("mybottom", (277, -308))
                    .add_component("comma", (9, -164));
            })
            .add_glyph("commaturnedabovecomb", |glyph| {
                glyph
                    .add_component("commaaccentcomb", (589, 502))
                    .rotate_component(180.);
            })
            .build();
        propagate_all_anchors_impl(&mut glyphs);

        let new_glyph = glyphs.get("commaturnedabovecomb").unwrap();
        assert_eq!(
            new_glyph.layers[0].anchors,
            [("_top", (300., 502.)), ("mytop", (312., 810.)),]
        );
    }

    #[test]
    fn affine_scale() {
        let affine = Affine::rotate((180.0f64).to_radians()).then_translate((589., 502.).into());
        let delta = get_xy_rotation(affine);
        assert!(delta.x.is_sign_negative() && delta.y.is_sign_negative());

        let affine = Affine::translate((10., 10.));
        let delta = get_xy_rotation(affine);
        assert!(delta.x.is_sign_positive() && delta.y.is_sign_positive());
        let flip_y = get_xy_rotation(Affine::FLIP_Y);
        assert!(flip_y.y.is_sign_negative());
        assert!(flip_y.x.is_sign_positive());
        let flip_x = get_xy_rotation(Affine::FLIP_X);
        assert!(flip_x.y.is_sign_positive());
        assert!(flip_x.x.is_sign_negative());

        let rotate_flip = Affine::rotate((180.0f64).to_radians())
            .then_translate((589., 502.).into())
            * Affine::FLIP_X;
        let rotate_flip = get_xy_rotation(rotate_flip);
        assert!(rotate_flip.x.is_sign_positive());
        assert!(rotate_flip.y.is_sign_negative());
    }

    // the tricky parts of these files have been factored out into separate tests,
    // but we'll keep them in case there are other regressions lurking
    #[test]
    fn real_files() {
        let expected =
            Font::load_raw("../resources/testdata/glyphs3/PropagateAnchorsTest-propagated.glyphs")
                .unwrap();
        let font = Font::load(std::path::Path::new(
            "../resources/testdata/glyphs3/PropagateAnchorsTest.glyphs",
        ))
        .unwrap();

        assert_eq!(expected.glyphs.len(), font.glyphs.len());
        assert!(expected
            .glyphs
            .keys()
            .zip(font.glyphs.keys())
            .all(|(a, b)| a == b));

        for (g1, g2) in expected.glyphs.values().zip(font.glyphs.values()) {
            assert_eq!(g1.layers.len(), g2.layers.len());
            for (l1, l2) in g1.layers.iter().zip(g2.layers.iter()) {
                let a1 = l1.anchors.clone();
                let a2 = l2.anchors.clone();
                assert_eq!(a1, a2, "{}", g1.name);
            }
        }
    }

    #[test]
    fn composite_cycle() {
        let _ = env_logger::builder().is_test(true).try_init();
        let glyphs = GlyphSetBuilder::new()
            .add_glyph("A", |glyph| {
                glyph.add_component("B", (0, 0));
            })
            .add_glyph("B", |glyph| {
                glyph.add_component("A", (0, 0));
            })
            .build();
        // all we actually care about is that this doesn't run forever
        let sorted = depth_sorted_composite_glyphs_impl(&glyphs);
        // cycles should be dropped
        assert!(sorted.is_empty())
    }

    #[test]
    fn composite_not_a_cycle() {
        let _ = env_logger::builder().is_test(true).try_init();
        let glyphs = GlyphSetBuilder::new()
            .add_glyph("A", |_glyph| {})
            .add_glyph("B", |glyph| {
                glyph.add_component("A", (0, 0)).add_component("D", (0, 0));
            })
            .add_glyph("C", |_glyph| {})
            .add_glyph("D", |glyph| {
                glyph.add_component("E", (0, 0));
            })
            .add_glyph("E", |glyph| {
                glyph.add_component("C", (0, -180));
            })
            .build();
        let sorted = depth_sorted_composite_glyphs_impl(&glyphs);
        assert_eq!(sorted, ["A", "C", "E", "D", "B"]);
    }

    #[test]
    fn dont_propagate_anchors() {
        let font = Font::load(std::path::Path::new(
            "../resources/testdata/glyphs2/DontPropagateAnchors.glyphs",
        ))
        .unwrap();
        assert_eq!(font.custom_parameters.propagate_anchors, Some(false));
        let glyph = font.glyphs.get("Aacute").unwrap();
        assert!(glyph.layers.first().unwrap().anchors.is_empty());
    }
}
