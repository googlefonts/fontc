//! Types and helpers shared across modules

use std::fmt::{Display, Formatter};

use fontdrasil::types::GlyphName;
pub use write_fonts::types::GlyphId;

mod glyph_class;
mod glyph_map;

pub(crate) use glyph_class::GlyphClass;

pub use glyph_class::GlyphSet;
pub use glyph_map::GlyphMap;

use crate::compile::Anchor;

/// A glyph or glyph class.
///
/// Various places in the FEA spec accept either a single glyph or a glyph class.
#[derive(Debug, Clone)]
pub(crate) enum GlyphOrClass {
    /// A resolved GlyphId
    Glyph(GlyphId),
    /// A resolved glyph class
    Class(GlyphClass),
    /// An explicit `<NULL>` glyph
    Null,
}

/// Either a glyph name or a CID
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum GlyphIdent {
    /// A glyph name
    Name(GlyphName),
    /// a CID
    Cid(u16),
}

#[derive(Clone, Debug, Default)]
pub(crate) struct MarkClass {
    pub(crate) members: Vec<(GlyphClass, Option<Anchor>)>,
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

    pub(crate) fn is_null(&self) -> bool {
        matches!(self, GlyphOrClass::Null)
    }

    pub(crate) fn to_class(&self) -> Option<GlyphClass> {
        match self {
            GlyphOrClass::Glyph(gid) => Some((*gid).into()),
            GlyphOrClass::Class(class) => Some(class.clone()),
            GlyphOrClass::Null => None,
        }
    }

    pub(crate) fn to_glyph(&self) -> Option<GlyphId> {
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
                GlyphOrClass::Class(cls) => cls.items().get(idx).copied(),
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
                GlyphOrClass::Class(cls) => cls.items().get(idx).copied(),
                _ => None,
            };
            idx += 1;
            idx %= self.len();
            next
        })
    }
}
