//! Basic types useful for font compilation.
//!
//! Particularly types where it's nice for FE and BE to match.

use std::fmt::{Debug, Display};

use serde::{Deserialize, Serialize};
use smol_str::SmolStr;

use write_fonts::types::Tag;

use crate::coords::{CoordConverter, UserCoord};

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct GlyphName(SmolStr);

impl GlyphName {
    /// The name of the undefined glyph
    pub const NOTDEF: GlyphName = GlyphName(SmolStr::new_inline(".notdef"));

    pub fn new(s: impl AsRef<str>) -> Self {
        Self(SmolStr::new(s))
    }

    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }

    pub fn empty() -> GlyphName {
        "".into()
    }

    pub fn into_inner(self) -> SmolStr {
        self.0
    }
}

impl From<String> for GlyphName {
    fn from(value: String) -> Self {
        GlyphName(value.into())
    }
}

impl From<&str> for GlyphName {
    fn from(value: &str) -> Self {
        GlyphName(value.into())
    }
}

impl From<SmolStr> for GlyphName {
    fn from(value: SmolStr) -> Self {
        GlyphName(value)
    }
}

impl Debug for GlyphName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.as_str())
    }
}

impl Display for GlyphName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.as_str())
    }
}

impl AsRef<str> for GlyphName {
    fn as_ref(&self) -> &str {
        self.as_str()
    }
}

// this means if you have a HashSet<GlyphName> you can use &str to check
// if an item is contained
impl std::borrow::Borrow<str> for GlyphName {
    fn borrow(&self) -> &str {
        self.0.borrow()
    }
}

impl PartialEq<&str> for GlyphName {
    fn eq(&self, other: &&str) -> bool {
        self.as_str() == *other
    }
}

#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct Axis {
    pub name: String,
    pub tag: Tag,
    pub min: UserCoord,
    pub default: UserCoord,
    pub max: UserCoord,
    pub hidden: bool,
    pub converter: CoordConverter,
}

impl Axis {
    pub fn is_point(&self) -> bool {
        self.min == self.default && self.max == self.default
    }
}

// OS/2 width class
// https://docs.microsoft.com/en-gb/typography/opentype/spec/os2#uswidthclass
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum WidthClass {
    UltraCondensed = 1,
    ExtraCondensed = 2,
    Condensed = 3,
    SemiCondensed = 4,
    Medium = 5,
    SemiExpanded = 6,
    Expanded = 7,
    ExtraExpanded = 8,
    UltraExpanded = 9,
}

impl TryFrom<u16> for WidthClass {
    type Error = String;

    fn try_from(value: u16) -> Result<Self, Self::Error> {
        WidthClass::all_values()
            .get((value - 1) as usize)
            .copied()
            .ok_or_else(|| format!("Unsupported width class value: {}", value))
    }
}

impl WidthClass {
    pub fn all_values() -> &'static [Self; 9] {
        &[
            WidthClass::UltraCondensed,
            WidthClass::ExtraCondensed,
            WidthClass::Condensed,
            WidthClass::SemiCondensed,
            WidthClass::Medium,
            WidthClass::SemiExpanded,
            WidthClass::Expanded,
            WidthClass::ExtraExpanded,
            WidthClass::UltraExpanded,
        ]
    }

    pub fn to_percent(&self) -> f32 {
        match self {
            WidthClass::UltraCondensed => 50.0,
            WidthClass::ExtraCondensed => 62.5,
            WidthClass::Condensed => 75.0,
            WidthClass::SemiCondensed => 87.5,
            WidthClass::Medium => 100.0,
            WidthClass::SemiExpanded => 112.5,
            WidthClass::Expanded => 125.0,
            WidthClass::ExtraExpanded => 150.0,
            WidthClass::UltraExpanded => 200.0,
        }
    }
}
