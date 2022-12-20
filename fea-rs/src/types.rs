use std::{
    fmt::{Display, Formatter},
    rc::Rc,
};

use smol_str::SmolStr;
pub use write_fonts::types::GlyphId;

mod glyph_map;

pub use glyph_map::GlyphMap;

pub type GlyphName = SmolStr;

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct GlyphClass(Rc<[GlyphId]>);

#[derive(Debug, Clone)]
pub enum GlyphOrClass {
    Glyph(GlyphId),
    Class(GlyphClass),
    Null,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum GlyphIdent {
    Name(GlyphName),
    Cid(u16),
}

impl From<GlyphOrClass> for GlyphClass {
    fn from(src: GlyphOrClass) -> GlyphClass {
        match src {
            GlyphOrClass::Class(class) => class,
            GlyphOrClass::Glyph(id) => id.into(),
            GlyphOrClass::Null => GlyphClass(Rc::new([])),
        }
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

impl<T: Into<GlyphName>> From<T> for GlyphIdent {
    fn from(src: T) -> Self {
        GlyphIdent::Name(src.into())
    }
}

impl Display for GlyphIdent {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            GlyphIdent::Name(name) => write!(f, "{}", name),
            GlyphIdent::Cid(cid) => write!(f, "Cid({})", cid),
        }
    }
}

impl GlyphOrClass {
    pub(crate) fn len(&self) -> usize {
        match self {
            GlyphOrClass::Class(cls) => cls.len(),
            _ => 1,
        }
    }

    pub(crate) fn is_class(&self) -> bool {
        matches!(self, GlyphOrClass::Class(_))
    }

    pub(crate) fn into_class(self) -> Option<GlyphClass> {
        match self {
            GlyphOrClass::Glyph(gid) => Some(gid.into()),
            GlyphOrClass::Class(class) => Some(class),
            GlyphOrClass::Null => None,
        }
    }

    pub(crate) fn as_glyph(&self) -> Option<GlyphId> {
        match self {
            GlyphOrClass::Glyph(gid) => Some(*gid),
            _ => None,
        }
    }

    pub(crate) fn iter(&self) -> impl Iterator<Item = GlyphId> + '_ {
        let mut idx = 0;
        std::iter::from_fn(move || {
            let next = match &self {
                GlyphOrClass::Glyph(id) if idx == 0 => Some(*id),
                GlyphOrClass::Class(cls) => cls.0.get(idx).copied(),
                _ => None,
            };
            idx += 1;
            next
        })
    }

    /// an iterator that loops forever, and which returns NOTDEF for null.
    ///
    /// this is used to create the replacement targets for class -> glyph or
    /// class -> null substitutions.
    pub(crate) fn into_iter_for_target(self) -> impl Iterator<Item = GlyphId> {
        let mut idx = 0;
        std::iter::from_fn(move || {
            let next = match &self {
                GlyphOrClass::Glyph(id) if idx == 0 => Some(*id),
                GlyphOrClass::Null if idx == 0 => Some(GlyphId::NOTDEF),
                GlyphOrClass::Class(cls) => cls.0.get(idx).copied(),
                _ => None,
            };
            idx += 1;
            idx %= self.len();
            next
        })
    }
}

impl GlyphClass {
    pub fn items(&self) -> &[GlyphId] {
        &self.0
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
