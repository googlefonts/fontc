//! Propagating anchors from components to their composites

use std::collections::{BTreeMap, HashMap, VecDeque};

use kurbo::{Affine, Vec2};
use smol_str::{format_smolstr, SmolStr};

use crate::{
    font::Anchor,
    glyphdata::{Category, Subcategory},
    Component, Font, Glyph, Layer,
};

impl Font {
    /// Copy anchors from component glyphs into their including composites
    pub fn propagate_all_anchors(&mut self) {
        let todo = depth_sorted_composite_glyphs(&self.glyphs);
        let mut num_base_glyphs = HashMap::new();
        for name in todo {
            // temporarily remove the glyph so we can mess with it without borrowing all glyphs
            let mut glyph = self.glyphs.remove(&name).unwrap();
            // we work with indices here to also get around borrowck, because
            // if we borrow the layers in the loop we cannot set the anchors in the loop body
            for layer_idx in 0..glyph.layers.len() {
                let anchors = anchors_traversing_components(
                    &glyph,
                    layer_idx,
                    &self.glyphs,
                    &mut num_base_glyphs,
                );
                if anchors.len() != glyph.layers[layer_idx].anchors.len() {
                    debug_new_anchors(&anchors, &glyph, layer_idx);
                }
                glyph.layers[layer_idx].anchors = anchors;
            }
            // put the glyph back when we're done
            self.glyphs.insert(name, glyph);
        }
    }
}

fn debug_new_anchors(anchors: &[Anchor], glyph: &Glyph, layer_idx: usize) {
    if !log::log_enabled!(log::Level::Info) {
        return;
    }
    let layer = &glyph.layers[layer_idx];
    let prev_names: Vec<_> = layer.anchors.iter().map(|a| &a.name).collect();
    let new_names: Vec<_> = anchors.iter().map(|a| &a.name).collect();
    log::info!(
        "propagated anchors for ('{}::L{layer_idx}': {prev_names:?} -> {new_names:?}",
        glyph.name,
    );
}

/// Return the anchors for this glyph, including anchors from components
fn anchors_traversing_components(
    glyph: &Glyph,
    layer_idx: usize,
    glyphs: &BTreeMap<SmolStr, Glyph>,
    // each (glyph, layer) writes its number of base glyphs into this map during compilation
    base_glyph_counts: &mut HashMap<(SmolStr, usize), usize>,
) -> Vec<Anchor> {
    let layer = &glyph.layers[layer_idx];
    if layer.anchors.is_empty() && layer.components().count() == 0 {
        return Vec::new();
    }

    // if this is a mark and it has anchors, just return them.
    if !layer.anchors.is_empty() && glyph.category == Some(Category::Mark) {
        //TODO: glyphs uses a special anchor named "*origin" that shifts anchors
        //and outlines. we aren't handling that yet, because we need to handle it
        //eveywhere (i.e. also in outlines?)
        return layer.anchors.clone();
    }

    let is_ligature = glyph.sub_category == Some(Subcategory::Ligature);
    let mut has_underscore = layer
        .anchors
        .iter()
        .any(|anchor| anchor.name.starts_with('_'));

    let mut number_of_base_glyphs = 0usize;
    let mut all_anchors = BTreeMap::new();
    for (component_idx, component) in layer.components().enumerate() {
        // because we process dependencies first we know that all components
        // referenced have already been propagated
        let Some(component_layer) = get_component_layer(component, layer, glyphs) else {
            log::warn!(
                "could not get layer '{:?}' for component '{}' of glyph '{}'",
                layer.associated_master_id,
                component.name,
                glyph.name
            );
            continue;
        };

        //TODO: the python code here is referencing the component.anchor,
        //which we do not currently parse

        let anchors = &component_layer.anchors;
        let scale = get_transform_scale(component.transform);
        let component_number_of_base_glyphs = base_glyph_counts
            .get(&(component.name.clone(), layer_idx))
            .copied()
            .unwrap_or(0);

        /*
         there's some code here that seems to be a nop
        let comb_has_underscore = anchor_names
            .iter()
            .any(|a| a.len() >= 2 && a.starts_with('_'));
        let comb_has_exit = anchor_names.iter().any(|a| a.ends_with("exit"));
        if !(comb_has_underscore | comb_has_exit) {
            // the source we're porting here removes anchors from an empty array?
        }
        */

        for anchor in anchors {
            let new_has_underscore = anchor.name.starts_with('_');
            if (component_idx > 0 || has_underscore) && new_has_underscore {
                continue;
            }
            if component_idx > 0 && anchor.name.ends_with("entry") {
                continue;
            }

            let mut new_anchor_name = rename_anchor_for_scale(&anchor.name, scale);
            if is_ligature
                && component_number_of_base_glyphs > 0
                && !new_anchor_name.starts_with('_')
                && !(new_anchor_name.ends_with("exit") || new_anchor_name.ends_with("entry"))
            {
                // dealing with marks like top_1 on a ligature
                new_anchor_name = make_liga_anchor_name(new_anchor_name, number_of_base_glyphs);
            }

            let mut anchor = anchor.to_owned();
            anchor.pos = component.transform * anchor.pos;
            anchor.name = new_anchor_name;
            all_anchors.insert(anchor.name.clone(), anchor);
            has_underscore |= new_has_underscore;
        }
        number_of_base_glyphs += base_glyph_counts
            .get(&(component.name.clone(), layer_idx))
            .copied()
            .unwrap_or(0);
    }

    // now we've handled all the anchors from components, so copy over anchors
    // that were explicitly defined on this layer:
    all_anchors.extend(layer.anchors.iter().cloned().map(|a| (a.name.clone(), a)));
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
            let maybe_add_one = name.starts_with("caret").then_some(1).unwrap_or_default();
            let anchor_index = suffix.parse::<usize>().unwrap_or(0) + maybe_add_one;
            component_count_from_anchors = component_count_from_anchors.max(anchor_index);
        }
    }
    if !has_underscore_anchor && number_of_base_glyphs == 0 && has_mark_anchor {
        number_of_base_glyphs += 1;
    }
    if layer.anchors.iter().any(|a| a.name == "_bottom") {
        all_anchors.remove("top");
        all_anchors.remove("_top");
    }
    if layer.anchors.iter().any(|a| a.name == "_top") {
        all_anchors.remove("bottom");
        all_anchors.remove("_bottom");
    }
    base_glyph_counts.insert((glyph.name.clone(), layer_idx), number_of_base_glyphs);
    all_anchors.into_values().collect()
}

fn get_transform_scale(xform: Affine) -> Vec2 {
    let [xx, xy, _, yy, ..] = xform.as_coeffs();
    let angle = xy.atan2(xx).to_degrees();
    let mut scale = Vec2::new(xx, yy);
    // scale is inverted if we're rotated?
    if (angle - 180.0).abs() < 0.001 {
        scale *= -1.0;
    }

    scale
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
fn get_component_layer<'a>(
    component: &Component,
    layer: &Layer,
    glyphs: &'a BTreeMap<SmolStr, Glyph>,
) -> Option<&'a Layer> {
    let glyph = glyphs.get(&component.name)?;
    glyph
        .layers
        .iter()
        .find(|comp_layer| comp_layer.associated_master_id == layer.associated_master_id)
}

/// returns a list of all glyphs, sorted by component depth.
///
/// That is: a glyph in the list will always occur before any other glyph that
/// references it as a component.
fn depth_sorted_composite_glyphs(glyphs: &BTreeMap<SmolStr, Glyph>) -> Vec<SmolStr> {
    let mut queue = VecDeque::with_capacity(glyphs.len());
    // map of the maximum component depth of a glyph.
    // - a glyph with no components has depth 0,
    // - a glyph with a component has depth 1,
    // - a glyph with a component that itself has a component has depth 2, etc
    let mut depths = HashMap::with_capacity(glyphs.len());
    let mut component_buf = Vec::new();
    for (name, glyph) in glyphs {
        if glyph.has_components() {
            queue.push_back(glyph);
        } else {
            depths.insert(name, 0);
        }
    }

    while let Some(next) = queue.pop_front() {
        // all all components from this glyph to our reuseable buffer
        component_buf.clear();
        component_buf.extend(
            next.layers
                .iter()
                .flat_map(|layer| layer.shapes.iter())
                .filter_map(|shape| match shape {
                    crate::Shape::Path(_) => None,
                    crate::Shape::Component(comp) => Some(comp.name.clone()),
                }),
        );
        assert!(!component_buf.is_empty());
        if let Some(depth) = component_buf
            .iter()
            .map(|comp| depths.get(&comp).copied())
            // this is reducing to option<int>, taking the max depth only
            // if all components have been seen
            .reduce(|one, two| one.zip(two).map(|(a, b)| a.max(b)))
            .flatten()
        {
            // this is only Some if all items were already seen
            depths.insert(&next.name, depth + 1);
        } else {
            // else push to the back to try again after we've done the rest
            // (including the currently missing components)
            queue.push_back(next);
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

    use crate::{Layer, Shape};

    use super::*;

    #[test]
    fn components_by_depth() {
        fn make_glyph(name: &str, components: &[&str]) -> Glyph {
            let layer = Layer {
                layer_id: "my_layer".into(),
                width: 0.0.into(),
                shapes: components
                    .iter()
                    .map(|comp| {
                        Shape::Component(crate::Component {
                            name: SmolStr::new(comp),
                            transform: Default::default(),
                        })
                    })
                    .collect(),
                associated_master_id: Some("master_id".into()),
                anchors: Vec::new(),
                attributes: Default::default(),
            };
            Glyph {
                name: name.into(),
                export: true,
                unicode: BTreeSet::from([0]),
                layers: vec![layer],
                left_kern: None,
                right_kern: None,
                category: None,
                sub_category: None,
            }
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
            .into_iter()
            .map(|(name, components)| make_glyph(name, components))
            .map(|glyph| (glyph.name.clone(), glyph))
            .collect();

        let result = depth_sorted_composite_glyphs(&glyphs);
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
}
