use std::{
    convert::TryFrom,
    fmt::{Display, Formatter},
    num::TryFromIntError,
    rc::Rc,
};

use smol_str::SmolStr;

mod glyph_map;
mod metrics;
mod rules;
mod tag;

pub use glyph_map::GlyphMap;
pub use rules::{GposRule, GsubRule};
pub use tag::{InvalidTag, Tag};

pub type GlyphName = SmolStr;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct GlyphId(u16);

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct GlyphClass(Rc<[GlyphId]>);

#[derive(Debug, Clone)]
enum GlyphOrClass {
    Glyph(GlyphId),
    Class(GlyphClass),
}

// the general case; different uses have different constraints, which
// we will not bother to have specific types for
struct GlyphSequence(Rc<[GlyphOrClass]>);

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum GlyphIdent {
    Name(GlyphName),
    Cid(u32),
}

impl<T: Into<GlyphName>> From<T> for GlyphIdent {
    fn from(src: T) -> Self {
        GlyphIdent::Name(src.into())
    }
}

impl From<u16> for GlyphId {
    fn from(src: u16) -> GlyphId {
        GlyphId(src)
    }
}

impl TryFrom<usize> for GlyphId {
    type Error = TryFromIntError;

    fn try_from(value: usize) -> Result<Self, Self::Error> {
        u16::try_from(value).map(GlyphId)
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

impl GlyphClass {
    pub fn items(&self) -> &[GlyphId] {
        &self.0
    }
}

impl From<Vec<GlyphId>> for GlyphClass {
    fn from(src: Vec<GlyphId>) -> GlyphClass {
        GlyphClass(src.into())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct LanguageSystem {
    pub script: Tag,
    pub language: Tag,
}

impl LanguageSystem {
    pub const DEFAULT: LanguageSystem = LanguageSystem {
        script: Tag::DFLT_SCRIPT,
        language: Tag::DFLT_LANG,
    };
}
