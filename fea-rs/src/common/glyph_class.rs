use write_fonts::{read::collections::IntSet, types::GlyphId16};

use super::GlyphOrClass;

/// A glyph class, as used in the FEA spec.
///
/// This should not be confused with the `ClassDef` table used in OpenType.
/// In a feature file, a glyph class is an ordered sequence of glyphs, and
/// may contain duplicates.
///
/// For example, single sub format C can take the form,
///
/// ```fea
/// sub [a b c] by [z z d];
/// # equivalent to:
/// sub a by z;
/// sub b by z;
/// sub c by d;
/// ```
///
/// See the [spec docs] for more information.
///
/// [spec docs]: http://adobe-type-tools.github.io/afdko/OpenTypeFeatureFileSpecification.html#2g-glyph-classes
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub(crate) struct GlyphClass(Vec<GlyphId16>);

/// A sorted set of unique glyph ids.
///
/// This type exists to clearly separate the use of 'glyph class' in the fea spec
/// from how we use it when building tables that contain OpenType glyph ClassDefs.
pub type GlyphSet = IntSet<GlyphId16>;

impl GlyphClass {
    /// An empty glyph class
    pub const EMPTY: Self = GlyphClass(Vec::new());

    pub(crate) fn items(&self) -> &[GlyphId16] {
        &self.0
    }

    /// Return a `GlyphSet` containing the unique glyphs in this class.
    pub(crate) fn to_glyph_set(&self) -> GlyphSet {
        self.iter().collect()
    }

    pub(crate) fn iter(&self) -> impl Iterator<Item = GlyphId16> + '_ {
        self.items().iter().copied()
    }

    pub(crate) fn len(&self) -> usize {
        self.0.len()
    }
}

impl std::iter::FromIterator<GlyphId16> for GlyphClass {
    fn from_iter<T: IntoIterator<Item = GlyphId16>>(iter: T) -> Self {
        GlyphClass(iter.into_iter().collect())
    }
}

impl<'a> std::iter::IntoIterator for &'a GlyphClass {
    type Item = &'a GlyphId16;

    type IntoIter = std::slice::Iter<'a, GlyphId16>;

    fn into_iter(self) -> Self::IntoIter {
        self.0.iter()
    }
}

impl From<Vec<GlyphId16>> for GlyphClass {
    fn from(src: Vec<GlyphId16>) -> GlyphClass {
        GlyphClass(src)
    }
}

impl From<GlyphClass> for GlyphSet {
    fn from(value: GlyphClass) -> Self {
        value.iter().collect()
    }
}

impl From<GlyphId16> for GlyphClass {
    fn from(src: GlyphId16) -> GlyphClass {
        let slice: &[_] = &[src];
        GlyphClass(slice.into())
    }
}

impl From<GlyphOrClass> for GlyphClass {
    fn from(src: GlyphOrClass) -> GlyphClass {
        match src {
            GlyphOrClass::Class(class) => class,
            GlyphOrClass::Glyph(id) => id.into(),
            GlyphOrClass::Null => GlyphClass::EMPTY,
        }
    }
}
