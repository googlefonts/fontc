use std::rc::Rc;

use write_fonts::types::GlyphId;

use super::GlyphOrClass;

/// A glyph class, as used in the FEA spec.
///
/// This type is currently somewhat confused; in certain places the spec expects
/// that a glyph class is sorted and deduplicated, and in other places it expects
/// a glyph class to be an arbitrary sequence of glyphs.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct GlyphClass(Rc<[GlyphId]>);

impl std::iter::FromIterator<GlyphId> for GlyphClass {
    fn from_iter<T: IntoIterator<Item = GlyphId>>(iter: T) -> Self {
        GlyphClass(iter.into_iter().collect())
    }
}

impl<'a> std::iter::IntoIterator for &'a GlyphClass {
    type Item = &'a GlyphId;

    type IntoIter = std::slice::Iter<'a, GlyphId>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}

impl GlyphClass {
    pub fn items(&self) -> &[GlyphId] {
        &self.0
    }

    pub fn empty() -> Self {
        Self(Rc::new([]))
    }

    pub fn sort_and_dedupe(&self) -> GlyphClass {
        //idfk I guess this is fine
        let mut vec = self.0.iter().cloned().collect::<Vec<_>>();
        vec.sort_unstable();
        vec.dedup();
        GlyphClass(vec.into())
    }

    pub fn iter(&self) -> impl Iterator<Item = GlyphId> + '_ {
        self.items().iter().copied()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }
}

impl From<Vec<GlyphId>> for GlyphClass {
    fn from(src: Vec<GlyphId>) -> GlyphClass {
        GlyphClass(src.into())
    }
}

impl From<GlyphId> for GlyphClass {
    fn from(src: GlyphId) -> GlyphClass {
        let slice: &[_] = &[src];
        GlyphClass(slice.into())
    }
}

impl From<GlyphOrClass> for GlyphClass {
    fn from(src: GlyphOrClass) -> GlyphClass {
        match src {
            GlyphOrClass::Class(class) => class,
            GlyphOrClass::Glyph(id) => id.into(),
            GlyphOrClass::Null => GlyphClass::empty(),
        }
    }
}
