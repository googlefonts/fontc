use std::{convert::TryFrom, num::TryFromIntError};

use smol_str::SmolStr;

mod glyph_map;
mod tag;

pub use glyph_map::GlyphMap;
use tag::Tag;

pub type GlyphName = SmolStr;

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct GlyphId(u16);

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

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct LanguageSystem {
    pub script: Tag,
    pub language: Tag,
}
