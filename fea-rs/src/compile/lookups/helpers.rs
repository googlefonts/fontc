//! utils and types shared between multiple lookups

use std::collections::{HashMap, HashSet};

use write_fonts::tables::layout::{ClassDef, ClassDefBuilder};

use crate::common::{GlyphId, GlyphSet};

// There is a ClassDef builder in write-fonts, but it's a bit anemic.
//
// we want:
//
// - to assign the largest class the lowest id
// - to be able to retrieve the id for a given class, before building
// - to handle optionally assigning class 0 or not
//
// TODO: use this in other lookups?
#[derive(Clone, Debug, Default, PartialEq)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub(crate) struct ClassDefBuilder2 {
    classes: HashSet<GlyphSet>,
    glyphs: HashSet<GlyphId>,
    use_class_0: bool,
}

impl ClassDefBuilder2 {
    /// Create a new class def builder.
    ///
    /// If `use_class_0` is true, we will assign the '0' class id to one of the
    /// added classes; otherwise it will be unused (and implicitly refer to
    /// 'all other glyphs').
    ///
    /// class 0 is only used explicitly in class-based PairPos subtables?
    pub(crate) fn new(use_class_0: bool) -> Self {
        Self {
            use_class_0,
            ..Default::default()
        }
    }

    pub(crate) fn can_add(&self, cls: &GlyphSet) -> bool {
        self.classes.contains(cls) || cls.iter().all(|gid| !self.glyphs.contains(&gid))
    }

    /// Check that this class can be added to this classdef, and add it if so.
    ///
    /// returns `true` if the class is added, and `false` otherwise.
    pub(crate) fn checked_add(&mut self, cls: GlyphSet) -> bool {
        if self.can_add(&cls) {
            self.glyphs.extend(cls.iter());
            self.classes.insert(cls);
            true
        } else {
            false
        }
    }

    /// Returns a compiled glyphclass, as well as a mapping from our class objects
    /// to the final class ids
    pub(crate) fn build(self) -> (ClassDef, HashMap<GlyphSet, u16>) {
        let mut classes = self.classes.into_iter().collect::<Vec<_>>();
        // we match the sort order used by fonttools, see:
        // <https://github.com/fonttools/fonttools/blob/9a46f9d3ab01e3/Lib/fontTools/otlLib/builder.py#L2677>
        classes.sort_unstable_by_key(|cls| {
            (
                std::cmp::Reverse(cls.len()),
                cls.iter().next().unwrap_or_default().to_u16(),
            )
        });
        classes.dedup();
        let add_one = u16::from(!self.use_class_0);
        let mapping = classes
            .into_iter()
            .enumerate()
            .map(|(i, cls)| (cls, i as u16 + add_one))
            .collect::<HashMap<_, _>>();
        let class_def = mapping
            .iter()
            .flat_map(|(cls, id)| cls.iter().map(move |gid| (gid, *id)))
            .collect::<ClassDefBuilder>()
            .build();

        (class_def, mapping)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn make_glyph_class<const N: usize>(glyphs: [u16; N]) -> GlyphSet {
        glyphs.into_iter().map(GlyphId::new).collect()
    }

    #[test]
    fn smoke_test_class_builder() {
        let mut builder = ClassDefBuilder2::new(false);
        builder.checked_add(make_glyph_class([6, 10]));
        let (cls, _) = builder.build();
        assert_eq!(cls.get(GlyphId::new(6)), 1);

        let mut builder = ClassDefBuilder2::new(true);
        builder.checked_add(make_glyph_class([6, 10]));
        let (cls, _) = builder.build();
        assert_eq!(cls.get(GlyphId::new(6)), 0);
        assert_eq!(cls.get(GlyphId::new(10)), 0);
    }

    #[test]
    fn classdef_assign_order() {
        // - longer classes before short ones
        // - if tied, lowest glyph id first

        let mut builder = ClassDefBuilder2::default();
        builder.checked_add(make_glyph_class([7, 8, 9]));
        builder.checked_add(make_glyph_class([1, 12]));
        builder.checked_add(make_glyph_class([3, 4]));
        let (cls, _) = builder.build();
        assert_eq!(cls.get(GlyphId::new(9)), 1);
        assert_eq!(cls.get(GlyphId::new(1)), 2);
        assert_eq!(cls.get(GlyphId::new(4)), 3);
        // notdef
        assert_eq!(cls.get(GlyphId::new(5)), 0);
    }

    #[test]
    fn we_handle_dupes() {
        let mut builder = ClassDefBuilder2::default();
        let c1 = make_glyph_class([1, 2, 3, 4]);
        let c2 = make_glyph_class([4, 3, 2, 1, 1]);
        let c3 = make_glyph_class([1, 5, 6, 7]);
        assert!(builder.checked_add(c1.clone()));
        assert!(builder.checked_add(c2.clone()));
        assert!(!builder.checked_add(c3.clone()));

        let (_, map) = builder.build();
        assert_eq!(map.get(&c1), map.get(&c2));
        assert!(!map.contains_key(&c3));
    }
}
