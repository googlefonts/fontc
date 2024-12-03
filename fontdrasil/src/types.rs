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

    /// Initialize a `CoordConverter` for the default normalization from this axis user space.
    pub fn default_converter(&self) -> CoordConverter {
        CoordConverter::default_normalization(self.min, self.default, self.max)
    }

    /// Display name for the axis.
    ///
    /// Some frontends (e.g. designspace) support the notion of an axis' UI label name distinct
    /// from the axis name; the former is used for displaying the axis in UI and can be
    /// localised, whereas the latter is only used for internal cross-references.
    /// In Designspace documents, these are stored in the axes' `<labelname>` elements.
    /// FontTools uses these to build the name records associated with axis names referenced
    /// by fvar and STAT tables, or else falls back to the axis.name. fontc doesn't know about
    /// them until norad is able to parse them (<https://github.com/linebender/norad/issues/323>).
    /// But even when the labelnames are ommited, there's a special group of registered
    /// axis names that were common in old MutatorMath source files before the `<labelname>`
    /// element itself got standardised, which continue to receive a special treatment in
    /// fonttools: i.e., the lowercase, shortened name gets replaced with a title-case,
    /// expanded one (e.g. 'weight' => 'Weight', 'optical' => 'Optical Size' etc.).
    /// For the sake of matching fontmake (which uses fonttools), we do the same here.
    ///
    /// For additional info see: <https://github.com/googlefonts/fontc/issues/1020>
    pub fn ui_label_name(&self) -> &str {
        // TODO: support localised labelnames when norad does
        let axis_name = self.name.as_str();
        match axis_name {
            "weight" => "Weight",
            "width" => "Width",
            "slant" => "Slant",
            "optical" => "Optical Size",
            "italic" => "Italic",
            _ => axis_name,
        }
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
    // Lookup percent using the assigned value of the WidthClass
    const _PERCENT_LUT: [f64; 10] = [
        0.0, // no width class has value 0, never used
        50.0, 62.5, 75.0, 87.5, 100.0, 112.5, 125.0, 150.0, 200.0,
    ];

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

    pub fn to_percent(&self) -> f64 {
        Self::_PERCENT_LUT[*self as usize]
    }

    /// Returns the WidthClass with the nearest percent width
    pub fn nearest(percent: f64) -> Self {
        Self::all_values()
            .iter()
            .map(|v| (*v, (v.to_percent() - percent).abs()))
            .reduce(
                |(w0, d0), (w1, d1)| {
                    if d1 < d0 {
                        (w1, d1)
                    } else {
                        (w0, d0)
                    }
                },
            )
            .unwrap()
            .0
    }
}

#[cfg(test)]
mod tests {
    use super::WidthClass;

    #[test]
    fn all_widths_have_percents_and_they_ascend() {
        let pcts = WidthClass::all_values()
            .iter()
            .map(|v| v.to_percent() as i32)
            .collect::<Vec<_>>();
        let mut sorted_pcts = pcts.clone();
        sorted_pcts.sort();
        assert_eq!(sorted_pcts, pcts);
    }

    #[test]
    fn wdth_from_pct_42() {
        assert_eq!(WidthClass::UltraCondensed, WidthClass::nearest(42.0));
    }

    #[test]
    fn wdth_from_pct_76() {
        assert_eq!(WidthClass::Condensed, WidthClass::nearest(76.0));
    }

    #[test]
    fn wdth_from_pct_99() {
        assert_eq!(WidthClass::Medium, WidthClass::nearest(99.0));
    }

    #[test]
    fn wdth_from_pct_1000() {
        assert_eq!(WidthClass::UltraExpanded, WidthClass::nearest(1000.0));
    }
}
