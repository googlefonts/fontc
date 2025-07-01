//! Basic types useful for font compilation.
//!
//! Particularly types where it's nice for FE and BE to match.

use std::{
    collections::HashMap,
    fmt::{Debug, Display},
};

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

impl From<&GlyphName> for GlyphName {
    fn from(value: &GlyphName) -> Self {
        value.to_owned()
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

/// A set of axes, defining a design space.
#[derive(Serialize, Deserialize, Default, Debug, Clone, PartialEq, Eq)]
pub struct Axes {
    axes: Vec<Axis>,
    by_tag: HashMap<Tag, usize>,
}

/// A variable font axis.
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct Axis {
    /// A human-readable name, like 'Weight'.
    pub name: String,
    /// The axis tag, like 'wght'.
    pub tag: Tag,
    /// The minimum value for this axis, in [`UserSpace`][super::coords::UserSpace].
    pub min: UserCoord,
    /// The default value for this axis, in [`UserSpace`][super::coords::UserSpace].
    pub default: UserCoord,
    /// The max value for this axis, in [`UserSpace`][super::coords::UserSpace].
    pub max: UserCoord,
    pub hidden: bool,
    pub converter: CoordConverter,
    /// Localized names for the axis, e.g. `{"en": "Weight"}`
    pub localized_names: HashMap<String, String>,
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
    /// by fvar and STAT tables, or else falls back to the axis.name.
    /// But even when the labelnames are ommited, there's a special group of registered
    /// axis names that were common in old MutatorMath source files before the `<labelname>`
    /// element itself got standardised, which continue to receive a special treatment in
    /// fonttools: i.e., the lowercase, shortened name gets replaced with a title-case,
    /// expanded one (e.g. 'weight' => 'Weight', 'optical' => 'Optical Size' etc.).
    /// For the sake of matching fontmake (which uses fonttools), we do the same here.
    ///
    /// For additional info see: <https://github.com/googlefonts/fontc/issues/1020>
    pub fn ui_label_name(&self) -> &str {
        for (lang, string) in &self.localized_names {
            if lang == "en" {
                return string.as_str();
            }
        }

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

    /// Convenience constructor for use during tests.
    ///
    /// Panics if tag is not one of 'wght', 'wdth', 'ital', 'foo', 'bar', or 'axis'.
    #[doc(hidden)]
    pub fn for_test(tag: &str) -> Self {
        let (name, tag, min, default, max) = match tag {
            "wght" => ("Weight", "wght", 300, 400, 700),
            "wdth" => ("Width", "wdth", 75, 100, 125),
            "ital" => ("Italic", "ital", 0, 0, 1),
            "foo" => ("Foo", "foo ", -1, 0, 1),
            "bar" => ("Bar", "bar ", -1, 0, 1),
            "axis" => ("Axis", "axis", 0, 0, 1),
            _ => panic!("No definition for {tag}, add it?"),
        };
        let min = UserCoord::new(min as f64);
        let default = UserCoord::new(default as f64);
        let max = UserCoord::new(max as f64);
        Axis {
            name: name.to_string(),
            tag: std::str::FromStr::from_str(tag).unwrap(),
            min,
            default,
            max,
            hidden: false,
            converter: CoordConverter::new(
                vec![
                    (min, crate::coords::DesignCoord::new(min.into_inner())),
                    (
                        default,
                        crate::coords::DesignCoord::new(default.into_inner()),
                    ),
                    (max, crate::coords::DesignCoord::new(max.into_inner())),
                ],
                1,
            ),
            localized_names: HashMap::new(),
        }
    }
}

impl Axes {
    pub fn new(axes: Vec<Axis>) -> Self {
        let by_tag = axes.iter().enumerate().map(|(i, ax)| (ax.tag, i)).collect();
        Self { axes, by_tag }
    }

    #[doc(hidden)]
    pub fn for_test(tags: &[&str]) -> Self {
        tags.iter().copied().map(Axis::for_test).collect()
    }

    pub fn into_inner(self) -> Vec<Axis> {
        self.axes
    }

    pub fn len(&self) -> usize {
        self.axes.len()
    }

    pub fn is_empty(&self) -> bool {
        self.axes.is_empty()
    }

    pub fn iter(&self) -> impl Iterator<Item = &Axis> {
        self.axes.iter()
    }

    pub fn get(&self, tag: &Tag) -> Option<&Axis> {
        self.by_tag.get(tag).and_then(|idx| self.axes.get(*idx))
    }

    pub fn contains(&self, tag: &Tag) -> bool {
        self.by_tag.contains_key(tag)
    }
}

impl FromIterator<Axis> for Axes {
    fn from_iter<T: IntoIterator<Item = Axis>>(iter: T) -> Self {
        Axes::new(iter.into_iter().collect())
    }
}

impl From<Vec<Axis>> for Axes {
    fn from(src: Vec<Axis>) -> Axes {
        Axes::new(src)
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
            .ok_or_else(|| format!("Unsupported width class value: '{value}'"))
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
