use smol_str::SmolStr;

use std::collections::{BTreeMap, HashMap, HashSet};

/// Abstracts over something that can have components
///
/// This might be an IR glyph or it might be some other representation.
pub trait CompositeLike {
    fn name(&self) -> SmolStr;
    fn has_components(&self) -> bool;
    fn component_names(&self) -> impl Iterator<Item = SmolStr>;
}

/// returns a list of all glyphs, sorted by component depth.
///
/// That is: a glyph in the list will always occur before any other glyph that
/// references it as a component.
///
/// Glyphs whose depth can't be determined (because they are part of a component
/// cycle, reference a missing glyph, or depend on a glyph that does) are dropped
/// with a warning. Use [`component_cycles`] to detect cycles up front.
pub fn depth_sorted_composite_glyphs<T: CompositeLike>(
    glyphs: &BTreeMap<SmolStr, T>,
) -> Vec<SmolStr> {
    let (sorted, mut unresolved) = depth_sort(glyphs);

    if !unresolved.is_empty() && log::log_enabled!(log::Level::Warn) {
        unresolved.sort();
        log::warn!(
            "Invalid component graph (cycles or bad refs) for {} glyphs: {:?}",
            unresolved.len(),
            unresolved
        );
    }
    sorted
}

/// Returns the sorted names of all glyphs that are part of a component cycle.
///
/// A glyph is part of a cycle if following its components leads back to it,
/// including a glyph that uses itself as a component. Glyphs that merely use
/// a glyph in a cycle are not included.
pub fn component_cycles<T: CompositeLike>(glyphs: &BTreeMap<SmolStr, T>) -> Vec<SmolStr> {
    let (_, unresolved) = depth_sort(glyphs);
    // Only unresolved glyphs can be in a cycle, and a cycle can only pass
    // through unresolved glyphs, so we only need to search among those.
    let unresolved: HashSet<_> = unresolved.into_iter().collect();
    let mut cycles = unresolved
        .iter()
        .filter(|name| {
            let mut seen = HashSet::new();
            let mut todo: Vec<SmolStr> = glyphs[*name].component_names().collect();
            while let Some(component) = todo.pop() {
                if component == **name {
                    return true;
                }
                if unresolved.contains(&component) && seen.insert(component.clone()) {
                    todo.extend(glyphs[&component].component_names());
                }
            }
            false
        })
        .cloned()
        .collect::<Vec<_>>();
    cycles.sort();
    cycles
}

/// Returns the glyphs sorted by component depth, and those whose depth is
/// indeterminate (cycles or bad refs), in no particular order.
fn depth_sort<T: CompositeLike>(glyphs: &BTreeMap<SmolStr, T>) -> (Vec<SmolStr>, Vec<SmolStr>) {
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
                Some((name.clone(), 0))
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
                .component_names()
                .map(|name| depths.get(&name).copied())
                .try_fold(0, |acc, e| e.map(|e| acc.max(e)));
            if let Some(max_component_depth) = max_component_depth {
                depths.insert(glyph.name(), max_component_depth + 1);
            }
            max_component_depth.is_none() // retain if we don't yet have an answer
        });

        progress -= indeterminate_depth.len();
    }

    let unresolved = indeterminate_depth.into_iter().map(|g| g.name()).collect();

    let mut by_depth = depths
        .into_iter()
        .map(|(glyph, depth)| (depth, glyph))
        .collect::<Vec<_>>();

    by_depth.sort();
    let sorted = by_depth.into_iter().map(|(_, name)| name).collect();
    (sorted, unresolved)
}

#[cfg(test)]
mod tests {
    use super::*;

    struct MockComposite {
        name: SmolStr,
        components: Vec<SmolStr>,
    }

    impl CompositeLike for MockComposite {
        fn name(&self) -> SmolStr {
            self.name.clone()
        }

        fn has_components(&self) -> bool {
            !self.components.is_empty()
        }

        fn component_names(&self) -> impl Iterator<Item = SmolStr> {
            self.components.iter().cloned()
        }
    }

    #[derive(Default)]
    struct GlyphSetBuilder {
        glyphs: BTreeMap<SmolStr, MockComposite>,
    }

    impl GlyphSetBuilder {
        fn build(self) -> BTreeMap<SmolStr, MockComposite> {
            self.glyphs
        }

        fn add(mut self, name: &str, components: &[&str]) -> Self {
            let components = components.iter().copied().map(SmolStr::from).collect();
            let composite = MockComposite {
                name: name.into(),
                components,
            };
            self.glyphs.insert(name.into(), composite);
            self
        }
    }

    #[test]
    fn composite_cycle() {
        let _ = tracing_subscriber::fmt().with_test_writer().try_init();
        let glyphs = GlyphSetBuilder::default()
            .add("A", &["B"])
            .add("B", &["A"])
            .build();
        // all we actually care about is that this doesn't run forever
        let sorted = depth_sorted_composite_glyphs(&glyphs);
        // cycles should be dropped
        assert!(sorted.is_empty());
        assert_eq!(component_cycles(&glyphs), ["A", "B"]);
    }

    #[test]
    fn component_cycles_excludes_users_and_missing() {
        let glyphs = GlyphSetBuilder::default()
            .add("A", &["B"])
            .add("B", &["A"])
            .add("C", &[])
            .add("D", &["A", "C"])
            .add("E", &["missing"])
            .add("self", &["C", "self"])
            .build();
        // D only uses a glyph in a cycle, and E has a missing component; neither
        // is part of a cycle
        assert_eq!(component_cycles(&glyphs), ["A", "B", "self"]);
    }

    #[test]
    fn no_component_cycles() {
        let glyphs = GlyphSetBuilder::default()
            .add("A", &[])
            .add("B", &["A", "A"])
            .add("C", &["B", "A"])
            .build();
        assert!(component_cycles(&glyphs).is_empty());
    }

    #[test]
    fn composite_not_a_cycle() {
        let _ = tracing_subscriber::fmt().with_test_writer().try_init();
        let glyphs = GlyphSetBuilder::default()
            .add("A", &[])
            .add("B", &["A", "D"])
            .add("C", &[])
            .add("D", &["E"])
            .add("E", &["C"])
            .build();
        let sorted = depth_sorted_composite_glyphs(&glyphs);
        assert_eq!(sorted, ["A", "C", "E", "D", "B"]);
    }

    #[test]
    fn components_by_depth() {
        let glyphs = GlyphSetBuilder::default()
            .add("A", &[])
            .add("E", &[])
            .add("acutecomb", &[])
            .add("brevecomb", &[])
            .add("brevecomb_acutecomb", &["acutecomb", "brevecomb"])
            .add("AE", &["A", "E"])
            .add("Aacute", &["A", "acutecomb"])
            .add("Aacutebreve", &["A", "brevecomb_acutecomb"])
            .add("AEacutebreve", &["AE", "brevecomb_acutecomb"])
            .build();

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
        ];

        assert_eq!(result, expected)
    }
}
