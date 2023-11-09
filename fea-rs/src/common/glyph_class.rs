use std::rc::Rc;

use write_fonts::types::GlyphId;

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
pub(crate) struct GlyphClass(Rc<[GlyphId]>);

/// A sorted set of unique glyph ids.
///
/// This type exists to clearly separate the use of 'glyph class' in the fea spec
/// from how we use it when building tables that contain OpenType glyph ClassDefs.
///
/// In the former case, we want to be able to do things like compare for equality
/// and stabily sort, so we ensure that these classes are sorted and deduped.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct GlyphSet(Rc<[GlyphId]>);

impl GlyphClass {
    pub(crate) fn items(&self) -> &[GlyphId] {
        &self.0
    }

    /// Return a new, empty glyph class
    pub fn empty() -> Self {
        Self(Rc::new([]))
    }

    /// Return a `GlyphSet` containing the unique glyphs in this class.
    pub(crate) fn to_glyph_set(&self) -> GlyphSet {
        self.iter().collect()
    }

    pub(crate) fn iter(&self) -> impl Iterator<Item = GlyphId> + '_ {
        self.items().iter().copied()
    }

    pub(crate) fn len(&self) -> usize {
        self.0.len()
    }
}

impl GlyphSet {
    /// Iterate over the glyphs in this class
    pub fn iter(&self) -> impl Iterator<Item = GlyphId> + '_ {
        self.0.iter().copied()
    }

    pub(crate) fn len(&self) -> usize {
        self.0.len()
    }

    /// Return true if this glyph set is empty
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }
}

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

impl From<Vec<GlyphId>> for GlyphClass {
    fn from(src: Vec<GlyphId>) -> GlyphClass {
        GlyphClass(src.into())
    }
}

impl From<GlyphClass> for GlyphSet {
    fn from(value: GlyphClass) -> Self {
        value.iter().collect::<Vec<_>>().into()
    }
}

// our base constructor; all other logic goes through here
impl From<Vec<GlyphId>> for GlyphSet {
    fn from(mut value: Vec<GlyphId>) -> Self {
        value.sort_unstable();
        value.dedup();
        Self(value.into())
    }
}

impl std::iter::FromIterator<GlyphId> for GlyphSet {
    fn from_iter<T: IntoIterator<Item = GlyphId>>(iter: T) -> Self {
        iter.into_iter().collect::<Vec<_>>().into()
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
