//! Font IR types.
use std::{
    collections::{hash_map::RandomState, BTreeMap, BTreeSet, HashMap, HashSet},
    fmt::{Debug, Display},
    io::Read,
    path::PathBuf,
};

use indexmap::IndexSet;
use kurbo::{Affine, BezPath, PathEl, Point};
use log::{log_enabled, trace, warn};
use ordered_float::OrderedFloat;
use serde::{de::Error as _, Deserialize, Serialize};
use smol_str::SmolStr;
use write_fonts::{
    types::{GlyphId16, NameId, Tag},
    OtRound,
};

use fontdrasil::{coords::NormalizedLocation, types::GlyphName};

use crate::{
    error::{BadAnchor, BadAnchorReason, BadGlyph, BadGlyphKind, Error},
    orchestration::{IdAware, Persistable, WorkId},
};

mod path_builder;
mod static_metadata;

pub use path_builder::GlyphPathBuilder;
pub use static_metadata::{
    Condition, ConditionSet, GdefCategories, MetaTableValues, MiscMetadata, NameKey, NamedInstance,
    Panose, PostscriptNames, Rule, StaticMetadata, Substitution, VariableFeature,
};

pub const DEFAULT_VENDOR_ID: &str = "NONE";

/// The name of every glyph, in the order it will be emitted
///
/// <https://rsheeter.github.io/font101/#glyph-ids-and-the-cmap-table>
#[derive(Serialize, Deserialize, Default, Debug, Clone)]
pub struct GlyphOrder(IndexSet<GlyphName>);

impl Extend<GlyphName> for GlyphOrder {
    fn extend<T: IntoIterator<Item = GlyphName>>(&mut self, iter: T) {
        self.0.extend(iter)
    }
}

impl FromIterator<GlyphName> for GlyphOrder {
    fn from_iter<T: IntoIterator<Item = GlyphName>>(iter: T) -> Self {
        GlyphOrder(iter.into_iter().collect::<IndexSet<_>>())
    }
}

impl Eq for GlyphOrder {}

// IndexSet does not consider order for the purposes of equality, but it is
// important to us,
impl PartialEq for GlyphOrder {
    fn eq(&self, other: &Self) -> bool {
        self.len() == other.len() && self.iter().zip(other.iter()).all(|(a, b)| a == b)
    }
}

impl GlyphOrder {
    pub fn new() -> Self {
        GlyphOrder(IndexSet::new())
    }

    pub fn glyph_id<Q>(&self, name: &Q) -> Option<GlyphId16>
    where
        Q: std::hash::Hash + indexmap::Equivalent<GlyphName> + ?Sized,
    {
        self.0.get_index_of(name).map(|i| GlyphId16::new(i as _))
    }

    pub fn glyph_name(&self, index: usize) -> Option<&GlyphName> {
        self.0.get_index(index)
    }

    pub fn contains<Q>(&self, name: &Q) -> bool
    where
        Q: std::hash::Hash + indexmap::Equivalent<GlyphName> + ?Sized,
    {
        self.0.contains(name)
    }

    /// Iterate over ids and names
    pub fn iter(&self) -> impl Iterator<Item = (GlyphId16, &GlyphName)> {
        self.0
            .iter()
            .enumerate()
            .map(|(i, name)| (GlyphId16::new(i as _), name))
    }

    /// Iterate glyph names, in order
    pub fn names(&self) -> impl Iterator<Item = &GlyphName> {
        self.0.iter()
    }

    pub fn remove(&mut self, name: &GlyphName) -> bool {
        self.0.shift_remove(name)
    }

    pub fn difference<'a>(
        &'a self,
        other: &'a GlyphOrder,
    ) -> indexmap::set::Difference<'a, GlyphName, RandomState> {
        self.0.difference(&other.0)
    }

    pub fn insert(&mut self, name: GlyphName) -> bool {
        self.0.insert(name)
    }

    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    pub fn len(&self) -> usize {
        self.0.len()
    }

    pub(crate) fn set_glyph_id(&mut self, name: &GlyphName, new_index: usize) {
        match self.0.get_index_of(name) {
            Some(index) if index == new_index => (), // nop
            Some(index) => self.0.move_index(index, new_index),
            None => {
                self.insert(name.clone());
                self.set_glyph_id(name, new_index)
            }
        }
    }
}

impl IntoIterator for GlyphOrder {
    type Item = GlyphName;
    type IntoIter = <IndexSet<GlyphName> as IntoIterator>::IntoIter;

    fn into_iter(self) -> Self::IntoIter {
        self.0.into_iter()
    }
}

/// In logical (reading) order
pub type KernPair = (KernSide, KernSide);

/// IR representation of kerning groups.
///
/// In UFO terms, roughly [groups.plist](https://unifiedfontobject.org/versions/ufo3/groups.plist/)
/// plus the set of locations that can have kerning.
#[derive(Serialize, Deserialize, Debug, Default, Clone, PartialEq, Eq)]
pub struct KerningGroups {
    pub groups: BTreeMap<KernGroup, BTreeSet<GlyphName>>,
    /// The locations that have kerning defined.
    ///
    /// Must be a subset of the master locations.
    pub locations: BTreeSet<NormalizedLocation>,

    /// Optional group renaming map, meant for [KerningInstance] to consume
    ///
    /// The rhs should be the name used in the groups map.
    pub old_to_new_group_names: BTreeMap<KernGroup, KernGroup>,
}

/// IR representation of kerning for a location.
///
/// In UFO terms, roughly [kerning.plist](https://unifiedfontobject.org/versions/ufo3/kerning.plist/).
#[derive(Serialize, Deserialize, Debug, Default, Clone, PartialEq, Eq)]
pub struct KerningInstance {
    pub location: NormalizedLocation,
    /// An adjustment to the space *between* two glyphs in logical order.
    ///
    /// Maps (side1, side2) => a mapping location:adjustment.
    ///
    /// Used for both LTR and RTL. The BE application differs but the concept
    /// is the same.
    pub kerns: BTreeMap<KernPair, OrderedFloat<f64>>,
}

/// A named set of glyphs with common kerning behaviour
///
/// Identical sets can have different behaviour depending on whether or not they
/// are in the first or second logical position.
#[derive(Clone, Debug, PartialEq, PartialOrd, Eq, Ord, Hash)]
pub enum KernGroup {
    Side1(SmolStr),
    Side2(SmolStr),
}

/// One side of a kern pair, represented as a glyph or group name
///
/// <https://unifiedfontobject.org/versions/ufo3/kerning.plist/#kerning-pair-types>
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum KernSide {
    Glyph(GlyphName),
    Group(KernGroup),
}

impl Display for KernSide {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            KernSide::Glyph(g) => Display::fmt(g, f),
            KernSide::Group(name) => write!(f, "@{name}"),
        }
    }
}

impl Display for KernGroup {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            KernGroup::Side1(name) => write!(f, "side1.{name}"),
            KernGroup::Side2(name) => write!(f, "side2.{name}"),
        }
    }
}

// we need custom impls here because yaml does not support nested enums
impl Serialize for KernGroup {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        match self {
            KernGroup::Side1(name) => serializer.serialize_str(&format!("side1.{name}")),
            KernGroup::Side2(name) => serializer.serialize_str(&format!("side2.{name}")),
        }
    }
}

impl<'de> Deserialize<'de> for KernGroup {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        s.strip_prefix("side1.")
            .map(|s| KernGroup::Side1(s.into()))
            .or_else(|| s.strip_prefix("side2.").map(|s| KernGroup::Side2(s.into())))
            .ok_or_else(|| D::Error::custom(format!("missing side1/side2 prefix: {s}")))
    }
}

impl KernSide {
    #[inline]
    pub fn is_glyph(&self) -> bool {
        matches!(self, KernSide::Glyph(_))
    }

    /// If this is a glyph, return its name.
    pub fn glyph_name(&self) -> Option<&GlyphName> {
        match self {
            KernSide::Glyph(glyph_name) => Some(glyph_name),
            KernSide::Group(_) => None,
        }
    }

    #[inline]
    pub fn is_group(&self) -> bool {
        matches!(self, KernSide::Group(_))
    }
}

impl From<GlyphName> for KernSide {
    fn from(src: GlyphName) -> KernSide {
        KernSide::Glyph(src)
    }
}

impl From<KernGroup> for KernSide {
    fn from(src: KernGroup) -> KernSide {
        KernSide::Group(src)
    }
}

/// Global metrics. Ascender/descender, cap height, etc.
///
/// Represents the values of these metrics at a specific position in design space.
/// At a minimum should be defined at the default location.
#[derive(Serialize, Deserialize, Default, Debug, Clone, PartialEq, Eq)]
pub struct GlobalMetrics(
    pub(crate) HashMap<GlobalMetric, HashMap<NormalizedLocation, OrderedFloat<f64>>>,
);

#[derive(Serialize, Deserialize, Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum GlobalMetric {
    Ascender,
    Descender,
    HheaAscender,
    HheaDescender,
    HheaLineGap,
    VheaAscender,
    VheaDescender,
    VheaLineGap,
    Os2TypoAscender,
    Os2TypoDescender,
    Os2TypoLineGap,
    Os2WinAscent,
    Os2WinDescent,
    CapHeight,
    CaretSlopeRise,
    CaretSlopeRun,
    CaretOffset,
    VheaCaretSlopeRise,
    VheaCaretSlopeRun,
    VheaCaretOffset,
    UnderlineThickness,
    UnderlinePosition,
    XHeight,
    StrikeoutPosition,
    StrikeoutSize,
    SubscriptXOffset,
    SubscriptXSize,
    SubscriptYOffset,
    SubscriptYSize,
    SuperscriptXOffset,
    SuperscriptXSize,
    SuperscriptYOffset,
    SuperscriptYSize,
}

impl GlobalMetric {
    /// Return the 4-byte tag used to represent a global metric in the `MVAR` table.
    ///
    /// `None` if this metric is not associated with an `MVAR` value tag, or if
    /// we don't support it yet (e.g. vertical typesetting metrics).
    ///
    /// <https://learn.microsoft.com/en-us/typography/opentype/spec/mvar#value-tags>
    pub fn mvar_tag(&self) -> Option<Tag> {
        // We support the same subset of the metrics defined in the spec
        // as fonttools does (except vertical ones which we don't support yet):
        // https://github.com/fonttools/fonttools/blob/0c5cb3b/Lib/fontTools/varLib/mvar.py
        match self {
            GlobalMetric::Os2TypoAscender => Some(Tag::new(b"hasc")),
            GlobalMetric::Os2TypoDescender => Some(Tag::new(b"hdsc")),
            GlobalMetric::Os2TypoLineGap => Some(Tag::new(b"hlgp")),
            GlobalMetric::Os2WinAscent => Some(Tag::new(b"hcla")),
            GlobalMetric::Os2WinDescent => Some(Tag::new(b"hcld")),
            GlobalMetric::VheaAscender => Some(Tag::new(b"vasc")),
            GlobalMetric::VheaDescender => Some(Tag::new(b"vdsc")),
            GlobalMetric::VheaLineGap => Some(Tag::new(b"vlgp")),
            GlobalMetric::CaretSlopeRise => Some(Tag::new(b"hcrs")),
            GlobalMetric::CaretSlopeRun => Some(Tag::new(b"hcrn")),
            GlobalMetric::CaretOffset => Some(Tag::new(b"hcof")),
            GlobalMetric::VheaCaretSlopeRise => Some(Tag::new(b"vcrs")),
            GlobalMetric::VheaCaretSlopeRun => Some(Tag::new(b"vcrn")),
            GlobalMetric::VheaCaretOffset => Some(Tag::new(b"vcof")),
            GlobalMetric::XHeight => Some(Tag::new(b"xhgt")),
            GlobalMetric::CapHeight => Some(Tag::new(b"cpht")),
            GlobalMetric::SubscriptXSize => Some(Tag::new(b"sbxs")),
            GlobalMetric::SubscriptYSize => Some(Tag::new(b"sbys")),
            GlobalMetric::SubscriptXOffset => Some(Tag::new(b"sbxo")),
            GlobalMetric::SubscriptYOffset => Some(Tag::new(b"sbyo")),
            GlobalMetric::SuperscriptXSize => Some(Tag::new(b"spxs")),
            GlobalMetric::SuperscriptYSize => Some(Tag::new(b"spys")),
            GlobalMetric::SuperscriptXOffset => Some(Tag::new(b"spxo")),
            GlobalMetric::SuperscriptYOffset => Some(Tag::new(b"spyo")),
            GlobalMetric::StrikeoutSize => Some(Tag::new(b"strs")),
            GlobalMetric::StrikeoutPosition => Some(Tag::new(b"stro")),
            GlobalMetric::UnderlineThickness => Some(Tag::new(b"unds")),
            GlobalMetric::UnderlinePosition => Some(Tag::new(b"undo")),
            // TODO gsp0, gsp1, etc. for gasp table
            _ => None,
        }
    }
}

/// Adjust Y offset based on italic angle, to get X offset.
///
///  <https://github.com/googlefonts/ufo2ft/blob/0d2688cd847d003b41104534d16973f72ef26c40/Lib/ufo2ft/outlineCompiler.py#L571-L573>
fn adjust_offset(offset: f64, angle: f64) -> f64 {
    if angle.abs() >= f64::EPSILON {
        offset * (-angle).to_radians().tan()
    } else {
        0.0
    }
}

/// Map of values associated with a given global metric at various locations.
pub type GlobalMetricValues = HashMap<NormalizedLocation, OrderedFloat<f64>>;

impl GlobalMetrics {
    /// Creates a new, empty, [GlobalMetrics]
    pub fn new() -> Self {
        Default::default()
    }

    /// Populate default values for all metrics at the given location.
    ///
    /// This is used to populate the default values for the default location whenever a
    /// new [GlobalMetrics] is created. But it can also be used to populate the
    /// default values for other non-default locations, to make these 'dense'
    /// (i.e. non 'sparse'). That is the current behavior of fontmake when building
    /// variable fonts from Glyphs or Designspace sources.
    pub fn populate_defaults(
        &mut self,
        pos: &NormalizedLocation,
        units_per_em: u16,
        x_height: Option<f64>,
        ascender: Option<f64>,
        descender: Option<f64>,
        italic_angle: Option<f64>,
    ) {
        let units_per_em = units_per_em as f64;

        let mut set_if_absent = |metric, value| self.set_if_absent(metric, pos.clone(), value);

        // https://github.com/googlefonts/ufo2ft/blob/fca66fe3ea1ea88ffb36f8264b21ce042d3afd05/Lib/ufo2ft/fontInfoData.py#L38-L45
        let ascender = ascender.unwrap_or(0.8 * units_per_em);
        let descender = descender.unwrap_or(-0.2 * units_per_em);
        set_if_absent(GlobalMetric::Ascender, ascender);
        set_if_absent(GlobalMetric::Descender, descender);

        // https://github.com/googlefonts/ufo2ft/blob/0d2688cd847d003b41104534d16973f72ef26c40/Lib/ufo2ft/fontInfoData.py#L229-L238
        let typo_line_gap = units_per_em * 1.2 + descender - ascender;
        let typo_line_gap = if typo_line_gap > 0.0 {
            typo_line_gap
        } else {
            0.0
        };
        set_if_absent(GlobalMetric::Os2TypoLineGap, typo_line_gap);

        // https://github.com/googlefonts/ufo2ft/blob/0d2688cd847d003b41104534d16973f72ef26c40/Lib/ufo2ft/fontInfoData.py#L215-L226
        set_if_absent(GlobalMetric::Os2TypoAscender, ascender);
        set_if_absent(GlobalMetric::Os2TypoDescender, descender);

        // https://github.com/googlefonts/ufo2ft/blob/0d2688cd847d003b41104534d16973f72ef26c40/Lib/ufo2ft/fontInfoData.py#L126-L130
        set_if_absent(GlobalMetric::HheaAscender, ascender + typo_line_gap);
        set_if_absent(GlobalMetric::HheaDescender, descender);
        // https://github.com/googlefonts/ufo2ft/blob/0d2688cd847d003b41104534d16973f72ef26c40/Lib/ufo2ft/fontInfoData.py#L366
        set_if_absent(GlobalMetric::HheaLineGap, 0.0);

        // https://github.com/googlefonts/ufo2ft/blob/0d2688cd847d003b41104534d16973f72ef26c40/Lib/ufo2ft/fontInfoData.py#L241-L254
        set_if_absent(GlobalMetric::Os2WinAscent, ascender + typo_line_gap);
        set_if_absent(GlobalMetric::Os2WinDescent, descender.abs());

        // https://github.com/googlefonts/ufo2ft/blob/fca66fe3ea1ea88ffb36f8264b21ce042d3afd05/Lib/ufo2ft/fontInfoData.py#L48-L55
        set_if_absent(GlobalMetric::CapHeight, 0.7 * units_per_em);

        let x_height = x_height.unwrap_or(0.5 * units_per_em);
        set_if_absent(GlobalMetric::XHeight, x_height);

        // https://github.com/googlefonts/ufo2ft/blob/150c2d6a00da9d5854173c8457a553ce03b89cf7/Lib/ufo2ft/fontInfoData.py#L133-L148
        // https://github.com/googlefonts/ufo2ft/blob/150c2d6a00da9d5854173c8457a553ce03b89cf7/Lib/ufo2ft/fontInfoData.py#L151-L161
        let italic_angle = italic_angle.unwrap_or(0.0);
        set_if_absent(GlobalMetric::CaretSlopeRise, units_per_em);
        set_if_absent(
            GlobalMetric::CaretSlopeRun,
            adjust_offset(units_per_em, italic_angle),
        );

        // https://github.com/googlefonts/ufo2ft/blob/0d2688cd847d003b41104534d16973f72ef26c40/Lib/ufo2ft/fontInfoData.py#L367
        set_if_absent(GlobalMetric::CaretOffset, 0.0);

        // https://github.com/googlefonts/ufo2ft/blob/0d2688cd847d003b41104534d16973f72ef26c40/Lib/ufo2ft/outlineCompiler.py#L575-L616
        let subscript_x_size = units_per_em * 0.65;
        let subscript_y_size = units_per_em * 0.60;
        let subscript_y_offset = units_per_em * 0.075;
        let superscript_y_offset = units_per_em * 0.35;

        set_if_absent(GlobalMetric::SubscriptXSize, subscript_x_size);
        set_if_absent(GlobalMetric::SubscriptYSize, subscript_y_size);
        set_if_absent(
            GlobalMetric::SubscriptXOffset,
            adjust_offset(-subscript_y_offset, italic_angle),
        );
        set_if_absent(GlobalMetric::SubscriptYOffset, subscript_y_offset);

        set_if_absent(GlobalMetric::SuperscriptXSize, subscript_x_size);
        set_if_absent(GlobalMetric::SuperscriptYSize, subscript_y_size);
        set_if_absent(
            GlobalMetric::SuperscriptXOffset,
            adjust_offset(superscript_y_offset, italic_angle),
        );
        set_if_absent(GlobalMetric::SuperscriptYOffset, superscript_y_offset);

        // ufo2ft and Glyphs.app have different defaults for the post.underlinePosition:
        // the former uses 0.075*UPEM whereas the latter 0.1*UPEM (both use the same
        // underlineThickness 0.05*UPEM). We prefer to match Glyphs.app as more widely used.
        // https://github.com/googlefonts/ufo2ft/blob/main/Lib/ufo2ft/fontInfoData.py#L313-L322
        // https://github.com/googlefonts/glyphsLib/blob/main/Lib/glyphsLib/builder/custom_params.py#L1116-L1125
        let underline_thickness =
            set_if_absent(GlobalMetric::UnderlineThickness, 0.05 * units_per_em);
        set_if_absent(GlobalMetric::UnderlinePosition, -0.1 * units_per_em);

        // https://github.com/googlefonts/ufo2ft/blob/fca66fe3/Lib/ufo2ft/outlineCompiler.py#L609-L616
        set_if_absent(GlobalMetric::StrikeoutSize, underline_thickness.0);
        set_if_absent(
            GlobalMetric::StrikeoutPosition,
            if x_height != 0. {
                x_height * 0.6
            } else {
                units_per_em * 0.22
            },
        );

        // https://github.com/googlefonts/ufo2ft/blob/0d2688cd847d003b41104534d16973f72ef26c40/Lib/ufo2ft/fontInfoData.py#L403-L408
        set_if_absent(GlobalMetric::VheaCaretSlopeRise, 0.0);
        set_if_absent(GlobalMetric::VheaCaretSlopeRun, 1.0);
        set_if_absent(GlobalMetric::VheaCaretOffset, 0.0);

        // XXX: These should _always_ be overwritten if the static metadata
        // indicates that there is vertical data to build.
        // https://github.com/googlefonts/ufo2ft/blob/0d2688cd847d003b41104534d16973f72ef26c40/Lib/ufo2ft/fontInfoData.py#L397-L402
        set_if_absent(GlobalMetric::VheaAscender, 0.0);
        set_if_absent(GlobalMetric::VheaDescender, 0.0);
        set_if_absent(GlobalMetric::VheaLineGap, 0.0);
    }

    fn values(&self, metric: GlobalMetric) -> &GlobalMetricValues {
        // We presume that ctor initializes for every GlobalMetric
        self.0.get(&metric).unwrap()
    }

    fn values_mut(&mut self, metric: GlobalMetric) -> &mut GlobalMetricValues {
        self.0.entry(metric).or_default()
    }

    pub fn get(&self, metric: GlobalMetric, pos: &NormalizedLocation) -> OrderedFloat<f64> {
        let Some(value) = self.values(metric).get(pos) else {
            panic!("interpolated metric values are not yet supported");
        };
        *value
    }

    pub fn set(
        &mut self,
        metric: GlobalMetric,
        pos: NormalizedLocation,
        value: impl Into<OrderedFloat<f64>>,
    ) {
        self.values_mut(metric).insert(pos, value.into());
    }

    pub fn set_if_absent(
        &mut self,
        metric: GlobalMetric,
        pos: NormalizedLocation,
        value: impl Into<OrderedFloat<f64>>,
    ) -> OrderedFloat<f64> {
        *self.values_mut(metric).entry(pos).or_insert(value.into())
    }

    pub fn set_if_some(
        &mut self,
        metric: GlobalMetric,
        pos: NormalizedLocation,
        maybe_value: Option<impl Into<OrderedFloat<f64>>>,
    ) {
        if let Some(value) = maybe_value {
            self.set(metric, pos, value.into().into_inner());
        }
    }

    pub fn at(&self, pos: &NormalizedLocation) -> GlobalMetricsInstance {
        GlobalMetricsInstance {
            pos: pos.clone(),
            ascender: self.get(GlobalMetric::Ascender, pos),
            descender: self.get(GlobalMetric::Descender, pos),
            caret_slope_rise: self.get(GlobalMetric::CaretSlopeRise, pos),
            caret_slope_run: self.get(GlobalMetric::CaretSlopeRun, pos),
            caret_offset: self.get(GlobalMetric::CaretOffset, pos),
            cap_height: self.get(GlobalMetric::CapHeight, pos),
            x_height: self.get(GlobalMetric::XHeight, pos),
            subscript_x_size: self.get(GlobalMetric::SubscriptXSize, pos),
            subscript_y_size: self.get(GlobalMetric::SubscriptYSize, pos),
            subscript_x_offset: self.get(GlobalMetric::SubscriptXOffset, pos),
            subscript_y_offset: self.get(GlobalMetric::SubscriptYOffset, pos),
            superscript_x_size: self.get(GlobalMetric::SuperscriptXSize, pos),
            superscript_y_size: self.get(GlobalMetric::SuperscriptYSize, pos),
            superscript_x_offset: self.get(GlobalMetric::SuperscriptXOffset, pos),
            superscript_y_offset: self.get(GlobalMetric::SuperscriptYOffset, pos),
            strikeout_size: self.get(GlobalMetric::StrikeoutSize, pos),
            strikeout_position: self.get(GlobalMetric::StrikeoutPosition, pos),
            os2_typo_ascender: self.get(GlobalMetric::Os2TypoAscender, pos),
            os2_typo_descender: self.get(GlobalMetric::Os2TypoDescender, pos),
            os2_typo_line_gap: self.get(GlobalMetric::Os2TypoLineGap, pos),
            os2_win_ascent: self.get(GlobalMetric::Os2WinAscent, pos),
            os2_win_descent: self.get(GlobalMetric::Os2WinDescent, pos),
            hhea_ascender: self.get(GlobalMetric::HheaAscender, pos),
            hhea_descender: self.get(GlobalMetric::HheaDescender, pos),
            hhea_line_gap: self.get(GlobalMetric::HheaLineGap, pos),
            vhea_ascender: self.get(GlobalMetric::VheaAscender, pos),
            vhea_descender: self.get(GlobalMetric::VheaDescender, pos),
            vhea_line_gap: self.get(GlobalMetric::VheaLineGap, pos),
            vhea_caret_slope_rise: self.get(GlobalMetric::VheaCaretSlopeRise, pos),
            vhea_caret_slope_run: self.get(GlobalMetric::VheaCaretSlopeRun, pos),
            vhea_caret_offset: self.get(GlobalMetric::VheaCaretOffset, pos),
            underline_thickness: self.get(GlobalMetric::UnderlineThickness, pos),
            underline_position: self.get(GlobalMetric::UnderlinePosition, pos),
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = (&GlobalMetric, &GlobalMetricValues)> + '_ {
        self.0.iter()
    }
}

/// Metrics at a specific [NormalizedLocation]
#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct GlobalMetricsInstance {
    pub pos: NormalizedLocation,
    pub ascender: OrderedFloat<f64>,
    pub descender: OrderedFloat<f64>,
    pub caret_slope_rise: OrderedFloat<f64>,
    pub caret_slope_run: OrderedFloat<f64>,
    pub caret_offset: OrderedFloat<f64>,
    pub cap_height: OrderedFloat<f64>,
    pub x_height: OrderedFloat<f64>,
    pub os2_typo_ascender: OrderedFloat<f64>,
    pub os2_typo_descender: OrderedFloat<f64>,
    pub os2_typo_line_gap: OrderedFloat<f64>,
    pub os2_win_ascent: OrderedFloat<f64>,
    pub os2_win_descent: OrderedFloat<f64>,
    pub hhea_ascender: OrderedFloat<f64>,
    pub hhea_descender: OrderedFloat<f64>,
    pub hhea_line_gap: OrderedFloat<f64>,
    pub vhea_ascender: OrderedFloat<f64>,
    pub vhea_descender: OrderedFloat<f64>,
    pub vhea_line_gap: OrderedFloat<f64>,
    pub vhea_caret_slope_rise: OrderedFloat<f64>,
    pub vhea_caret_slope_run: OrderedFloat<f64>,
    pub vhea_caret_offset: OrderedFloat<f64>,
    pub strikeout_position: OrderedFloat<f64>,
    pub strikeout_size: OrderedFloat<f64>,
    pub subscript_x_offset: OrderedFloat<f64>,
    pub subscript_x_size: OrderedFloat<f64>,
    pub subscript_y_offset: OrderedFloat<f64>,
    pub subscript_y_size: OrderedFloat<f64>,
    pub superscript_x_offset: OrderedFloat<f64>,
    pub superscript_x_size: OrderedFloat<f64>,
    pub superscript_y_offset: OrderedFloat<f64>,
    pub superscript_y_size: OrderedFloat<f64>,
    pub underline_thickness: OrderedFloat<f64>,
    pub underline_position: OrderedFloat<f64>,
}

#[doc(hidden)]
pub mod test_helpers {
    use ordered_float::OrderedFloat;

    use super::GlobalMetricsInstance;

    /// Round to 2 decimal places for testing because sometimes one wants to write 1.5 not 1.499994
    pub trait Round2 {
        fn round2(self) -> Self;
    }

    impl Round2 for OrderedFloat<f64> {
        fn round2(self) -> Self {
            OrderedFloat((self.0 * 100.0).round() / 100.0)
        }
    }

    impl Round2 for GlobalMetricsInstance {
        fn round2(self) -> Self {
            GlobalMetricsInstance {
                pos: self.pos.clone(),
                ascender: self.ascender.round2(),
                descender: self.descender.round2(),
                caret_slope_rise: self.caret_slope_rise.round2(),
                vhea_caret_slope_rise: self.vhea_caret_slope_rise.round2(),
                cap_height: self.cap_height.round2(),
                x_height: self.x_height.round2(),
                subscript_x_size: self.subscript_x_size.round2(),
                subscript_y_size: self.subscript_y_size.round2(),
                subscript_y_offset: self.subscript_y_offset.round2(),
                superscript_x_size: self.superscript_x_size.round2(),
                superscript_y_size: self.superscript_y_size.round2(),
                superscript_y_offset: self.superscript_y_offset.round2(),
                strikeout_position: self.strikeout_position.round2(),
                strikeout_size: self.strikeout_size.round2(),
                os2_typo_ascender: self.os2_typo_ascender.round2(),
                os2_typo_descender: self.os2_typo_descender.round2(),
                os2_typo_line_gap: self.os2_typo_line_gap.round2(),
                os2_win_ascent: self.os2_win_ascent.round2(),
                os2_win_descent: self.os2_win_descent.round2(),
                hhea_ascender: self.hhea_ascender.round2(),
                hhea_descender: self.hhea_descender.round2(),
                hhea_line_gap: self.hhea_line_gap.round2(),
                vhea_ascender: self.vhea_ascender.round2(),
                vhea_descender: self.vhea_descender.round2(),
                vhea_line_gap: self.vhea_line_gap.round2(),
                underline_thickness: self.underline_thickness.round2(),
                underline_position: self.underline_position.round2(),
                caret_slope_run: self.caret_slope_run.round2(),
                caret_offset: self.caret_offset.round2(),
                vhea_caret_slope_run: self.vhea_caret_slope_run.round2(),
                vhea_caret_offset: self.vhea_caret_offset.round2(),
                subscript_x_offset: self.subscript_x_offset.round2(),
                superscript_x_offset: self.superscript_x_offset.round2(),
            }
        }
    }
}

/// Helps accumulate 'name' values.
///
/// See <https://github.com/googlefonts/ufo2ft/blob/fca66fe3ea1ea88ffb36f8264b21ce042d3afd05/Lib/ufo2ft/outlineCompiler.py#L367>.
#[derive(Default)]
pub struct NameBuilder {
    names: HashMap<NameKey, String>,
    /// Helps lookup entries in name when all we have is a NameId
    name_to_key: HashMap<NameId, NameKey>,
    version_major: i32,
    version_minor: u32,
}

/// Returns true if the style name is one of the four standard style names.
fn is_ribbi(style_name: &str) -> bool {
    matches!(
        style_name.to_lowercase().as_str(),
        "regular" | "italic" | "bold" | "bold italic"
    )
}

impl NameBuilder {
    /// If drop_rbbi_suffix remove trailing Regular, Bold, or Italic, e.g. convert Family Regular to just Family
    pub fn make_family_name(family: &str, subfamily: &str, drop_rbbi_suffix: bool) -> String {
        let mut family = vec![family];
        family.extend(subfamily.split_ascii_whitespace());
        if drop_rbbi_suffix {
            while let Some(last) = family.last().copied() {
                if matches!(last, "Regular" | "Bold" | "Italic") {
                    family.pop();
                } else {
                    break;
                }
            }
        }
        family.join(" ")
    }

    pub fn add(&mut self, name_id: NameId, value: String) {
        let key = NameKey::new(name_id, &value);
        self.names.insert(key, value);
        self.name_to_key.insert(name_id, key);
    }

    pub fn remove(&mut self, name_id: NameId) {
        if let Some(key) = self.name_to_key.remove(&name_id) {
            self.names.remove(&key);
        }
    }

    pub fn add_if_present(&mut self, name_id: NameId, value: &Option<String>) {
        if let Some(value) = value.as_ref().cloned() {
            self.add(name_id, value);
        }
    }

    pub fn apply_fallback(&mut self, name_id: NameId, fallbacks: &[NameId]) {
        if self.contains_key(name_id) {
            return;
        }
        if let Some(fallback) = self.get_fallback_or_default(name_id, fallbacks) {
            self.add(name_id, fallback.to_string());
        }
    }

    pub fn contains_key(&self, name_id: NameId) -> bool {
        self.name_to_key.contains_key(&name_id)
    }

    pub fn get(&self, name_id: NameId) -> Option<&str> {
        self.name_to_key
            .get(&name_id)
            .and_then(|key| self.names.get(key).map(|s| s.as_str()))
    }

    pub fn into_inner(self) -> HashMap<NameKey, String> {
        self.names
    }

    pub fn get_fallback_or_default(&self, name_id: NameId, fallbacks: &[NameId]) -> Option<&str> {
        fallbacks
            .iter()
            .find_map(|n| {
                let key = self.name_to_key.get(n)?;
                self.names.get(key).map(|s| s.as_str())
            })
            .or_else(|| default_value(name_id))
    }

    /// Fetch a fallback that definitely has a value in a String
    ///
    /// Only safe to use with names that have default valuesa to fall back to
    /// if the fallback itself is missing.
    fn fallback_string(&self, name_id: NameId, fallback: NameId) -> String {
        self.get_fallback_or_default(name_id, &[fallback])
            .unwrap()
            .to_string()
    }

    pub fn apply_default_fallbacks(&mut self, vendor_id: &str) {
        // If the legacy subfamily isn't set, we fall back to the typographic subfamily;
        // but because the spec recommends the former to only contain "Regular",
        // "Bold", "Italic", or "Bold Italic", if this fallback isn't RIBBI already,
        // we append it to the legacy family name and set the legacy subfamily to the
        // default "Regular", the same way fontmake does via ufo2ft.

        // https://github.com/googlefonts/ufo2ft/blob/bb79cae53f1c160c7174ebef0d463c7a28a7552a/Lib/ufo2ft/fontInfoData.py#L76
        let family_suffix = if self.contains_key(NameId::SUBFAMILY_NAME) {
            // assume this is good, no need to add suffix to family name.
            None
        } else {
            let fallback_subfamily =
                self.fallback_string(NameId::SUBFAMILY_NAME, NameId::TYPOGRAPHIC_SUBFAMILY_NAME);

            let (fallback_subfamily, family_suffix) = if is_ribbi(&fallback_subfamily) {
                (fallback_subfamily, None)
            } else {
                ("Regular".to_string(), Some(fallback_subfamily))
            };

            self.add(NameId::SUBFAMILY_NAME, fallback_subfamily);

            family_suffix
        };

        // https://github.com/googlefonts/ufo2ft/blob/bb79cae53f1c160c7174ebef0d463c7a28a7552a/Lib/ufo2ft/fontInfoData.py#L57
        if !self.contains_key(NameId::FAMILY_NAME) {
            let mut fallback_family =
                self.fallback_string(NameId::FAMILY_NAME, NameId::TYPOGRAPHIC_FAMILY_NAME);
            if let Some(family_suffix) = family_suffix.as_ref() {
                fallback_family.push(' ');
                fallback_family.push_str(family_suffix);
            }
            self.add(NameId::FAMILY_NAME, fallback_family);
        };

        // https://github.com/googlefonts/ufo2ft/blob/fca66fe3ea1ea88ffb36f8264b21ce042d3afd05/Lib/ufo2ft/fontInfoData.py#L188
        self.apply_fallback(NameId::TYPOGRAPHIC_FAMILY_NAME, &[NameId::FAMILY_NAME]);

        // https://github.com/googlefonts/ufo2ft/blob/fca66fe3ea1ea88ffb36f8264b21ce042d3afd05/Lib/ufo2ft/fontInfoData.py#L195
        self.apply_fallback(
            NameId::TYPOGRAPHIC_SUBFAMILY_NAME,
            &[NameId::SUBFAMILY_NAME],
        );

        // https://github.com/googlefonts/ufo2ft/blob/fca66fe3ea1ea88ffb36f8264b21ce042d3afd05/Lib/ufo2ft/fontInfoData.py#L169
        if !self.contains_key(NameId::VERSION_STRING) {
            let major = self.version_major;
            let minor = self.version_minor;
            self.add(
                NameId::VERSION_STRING,
                format!("Version {major}.{minor:0>3}"),
            );
        }

        // https://github.com/googlefonts/ufo2ft/blob/fca66fe3ea1ea88ffb36f8264b21ce042d3afd05/Lib/ufo2ft/fontInfoData.py#L296
        if !self.contains_key(NameId::FULL_NAME) {
            self.add(
                NameId::FULL_NAME,
                NameBuilder::make_family_name(
                    self.get(NameId::TYPOGRAPHIC_FAMILY_NAME)
                        .unwrap_or_default(),
                    self.get(NameId::TYPOGRAPHIC_SUBFAMILY_NAME)
                        .unwrap_or_default(),
                    false,
                ),
            );
        }

        // https://github.com/googlefonts/ufo2ft/blob/fca66fe3ea1ea88ffb36f8264b21ce042d3afd05/Lib/ufo2ft/fontInfoData.py#L178-L185
        if !self.contains_key(NameId::POSTSCRIPT_NAME) {
            let mut family = self
                .get(NameId::TYPOGRAPHIC_FAMILY_NAME)
                .unwrap_or_default()
                .replace(" ", "");
            let subfamily = self
                .get(NameId::TYPOGRAPHIC_SUBFAMILY_NAME)
                .unwrap_or_default();
            if !subfamily.is_empty() {
                family += "-";
            }
            let mut value = NameBuilder::make_family_name(&family, subfamily, false);
            normalize_for_postscript(&mut value, false);
            self.add(NameId::POSTSCRIPT_NAME, value);
        }

        // https://github.com/googlefonts/ufo2ft/blob/fca66fe3ea1ea88ffb36f8264b21ce042d3afd05/Lib/ufo2ft/fontInfoData.py#L178-L185
        if !self.contains_key(NameId::UNIQUE_ID) {
            let version = self
                .get(NameId::VERSION_STRING)
                .unwrap()
                .replace("Version ", "");
            // fontmake pulls the openTypeOS2VendorID but we don't have that so just use their default
            let postscript_name = self.get(NameId::POSTSCRIPT_NAME).unwrap();
            self.add(
                NameId::UNIQUE_ID,
                format!("{version};{vendor_id};{postscript_name}"),
            );
        }

        // https://github.com/googlefonts/ufo2ft/blob/main/Lib/ufo2ft/outlineCompiler.py#L417-L421
        // Drop typographic names if they match the legacy ones
        let family = self.get(NameId::FAMILY_NAME);
        let sub_family = self.get(NameId::SUBFAMILY_NAME);
        if (family.is_some() && sub_family.is_some())
            && (family == self.get(NameId::TYPOGRAPHIC_FAMILY_NAME)
                && sub_family == self.get(NameId::TYPOGRAPHIC_SUBFAMILY_NAME))
        {
            self.remove(NameId::TYPOGRAPHIC_FAMILY_NAME);
            self.remove(NameId::TYPOGRAPHIC_SUBFAMILY_NAME);
        }
    }

    pub fn set_version(&mut self, major: i32, minor: u32) {
        self.version_major = major;
        self.version_minor = minor;
    }
}

/// <https://github.com/googlefonts/ufo2ft/blob/fca66fe3ea1ea88ffb36f8264b21ce042d3afd05/Lib/ufo2ft/fontInfoData.py#L263>
fn normalize_for_postscript(value: &mut String, allow_spaces: bool) {
    value.retain(|c| {
        if !allow_spaces && c.is_ascii_whitespace() {
            return false;
        }
        if "[](){}<>/%".contains(c) {
            return false;
        }
        if !(33..127).contains(&(c as u32)) {
            warn!("fontmake performs decomposition, we just ignore the character");
            return false;
        }
        true
    });
}

/// Match fontmake defaults.
pub fn default_value(name: NameId) -> Option<&'static str> {
    match name {
        // https://github.com/googlefonts/ufo2ft/blob/fca66fe3ea1ea88ffb36f8264b21ce042d3afd05/Lib/ufo2ft/fontInfoData.py#L352
        NameId::FAMILY_NAME => Some("New Font"),
        // https://github.com/googlefonts/ufo2ft/blob/fca66fe3ea1ea88ffb36f8264b21ce042d3afd05/Lib/ufo2ft/fontInfoData.py#L77
        NameId::SUBFAMILY_NAME => Some("Regular"),
        _ => None,
    }
}

/// Source for any feature code (Adobe FEA).
///
/// In time will split gpos/gsub, have different features for different
/// locations, etc.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub enum FeaturesSource {
    Empty,
    File {
        fea_file: PathBuf,
        include_dir: Option<PathBuf>,
    },
    Memory {
        fea_content: String,
        include_dir: Option<PathBuf>,
    },
}

impl FeaturesSource {
    pub fn empty() -> FeaturesSource {
        FeaturesSource::Empty
    }

    pub fn from_file(fea_file: PathBuf, include_dir: Option<PathBuf>) -> FeaturesSource {
        FeaturesSource::File {
            fea_file,
            include_dir,
        }
    }

    pub fn from_string(fea_content: String) -> FeaturesSource {
        FeaturesSource::Memory {
            fea_content,
            include_dir: None,
        }
    }
}

/// The anchors for a [Glyph]
///
/// Not having any is fine.
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct GlyphAnchors {
    pub glyph_name: GlyphName,
    pub anchors: Vec<Anchor>,
}

impl GlyphAnchors {
    pub fn new(glyph_name: GlyphName, anchors: Vec<Anchor>) -> Self {
        GlyphAnchors {
            glyph_name,
            anchors,
        }
    }

    /// `true` if any of our anchors are mark anchors
    pub fn contains_marks(&self) -> bool {
        self.anchors
            .iter()
            .any(|a| matches!(a.kind, AnchorKind::Mark(_)))
    }
}

/// The name shared by a group of anchors
///
/// This is used to determine the relationship between anchors: mark anchors
/// in the group 'top' attach to the 'top' anchor of base glyphs.
pub type GroupName = SmolStr;

/// The type of an anchor.
///
/// Anchors serve multiple purposes in font sources. By convention, these different
/// types of anchors are differentiated via naming convention. In IR, we parse
/// these names to determine the intended type.
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub enum AnchorKind {
    /// An attachment anchor on a base glyph
    Base(GroupName),
    /// An attachment anchor on a mark glyph
    Mark(GroupName),
    /// A base attachment on a ligature glyph
    Ligature {
        group_name: GroupName,
        index: usize,
    },
    /// An anchor marking the presence of a ligature component with no anchors.
    ///
    /// These are names like '_3'.
    ComponentMarker(usize),
    /// An anchor indicating a caret position in a ligature glyph.
    ///
    /// These are names like `caret_1`
    Caret(usize),
    /// A vertical caret position in a ligature glyph.
    VCaret(usize),
    CursiveEntry,
    CursiveExit,
}

impl AnchorKind {
    // this logic from
    // <https://github.com/googlefonts/ufo2ft/blob/6787e37e6/Lib/ufo2ft/featureWriters/markFeatureWriter.py#L101>
    pub fn new(name: impl AsRef<str>) -> Result<AnchorKind, BadAnchorReason> {
        let name = name.as_ref();

        if name == "entry" {
            return Ok(AnchorKind::CursiveEntry);
        }
        if name == "exit" {
            return Ok(AnchorKind::CursiveExit);
        }

        if let Some(suffix) = name
            .strip_prefix("caret_")
            .or_else(|| name.strip_prefix("vcaret_"))
        {
            if let Ok(index) = suffix.parse::<usize>() {
                if index == 0 {
                    return Err(BadAnchorReason::ZeroIndex);
                } else if name.starts_with('v') {
                    return Ok(AnchorKind::VCaret(index));
                } else {
                    return Ok(AnchorKind::Caret(index));
                }
            } else {
                if !suffix.is_empty() {
                    // this is slightly unexpected, so let's at least log:
                    log::warn!("anchor '{name}' will be treated as a caret");
                }
                if name.starts_with('v') {
                    return Ok(AnchorKind::VCaret(1));
                } else {
                    return Ok(AnchorKind::Caret(1));
                }
            }
        }

        // the '_' char is used as a prefix for marks, and as a space character
        // in ligature mark names (e.g. top_1, top_2). The funny exception is
        // names like '_4', (i.e. an underscore followed by a number) which
        // means "there is an extra component in this ligature but it doesn't
        // actually have any marks".

        if let Some(suffix) = name.strip_prefix('_') {
            // first check for the empty ligature case
            if let Ok(index) = suffix.parse::<usize>() {
                if index == 0 {
                    return Err(BadAnchorReason::ZeroIndex);
                } else {
                    return Ok(AnchorKind::ComponentMarker(index));
                }
            }
            // error on plain '_'
            if suffix.is_empty() {
                return Err(BadAnchorReason::NilMarkGroup);
            }
            // error on '_top_3'
            if let Some((_, suffix)) = suffix.rsplit_once('_') {
                if suffix.parse::<usize>().is_ok() {
                    return Err(BadAnchorReason::NumberedMarkAnchor);
                }
            }
            // looks like a mark!
            return Ok(AnchorKind::Mark(suffix.into()));
        // does not start with '_', but contains one (looks like a ligature):
        } else if let Some((name, suffix)) = name.rsplit_once('_') {
            // _1 suffix means a base in a ligature glyph
            if let Ok(index) = suffix.parse::<usize>() {
                if index == 0 {
                    return Err(BadAnchorReason::ZeroIndex);
                } else {
                    return Ok(AnchorKind::Ligature {
                        group_name: name.into(),
                        index,
                    });
                }
            }
        }
        // no '_' found looks like a base
        Ok(AnchorKind::Base(name.into()))
    }

    /// Returns `true` If this is a base or ligature base anchor
    ///
    /// (i.e, if it is an anchor that marks attach to)
    pub fn is_attaching(&self) -> bool {
        matches!(self, AnchorKind::Base(_) | AnchorKind::Ligature { .. })
    }
}

/// A variable definition of an anchor.
///
/// Must have at least one definition, at the default location.
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct Anchor {
    pub kind: AnchorKind,
    pub positions: HashMap<NormalizedLocation, Point>,
}

impl Anchor {
    pub fn default_pos(&self) -> Point {
        self.positions
            .iter()
            .find(|(loc, _)| loc.is_default())
            .map(|(_, p)| *p)
            .unwrap()
    }

    /// If this is a mark, base, or ligature anchor, return the group name
    pub fn mark_group_name(&self) -> Option<&SmolStr> {
        match &self.kind {
            AnchorKind::Base(group_name)
            | AnchorKind::Mark(group_name)
            | AnchorKind::Ligature { group_name, .. } => Some(group_name),
            _ => None,
        }
    }

    pub fn is_mark(&self) -> bool {
        matches!(self.kind, AnchorKind::Mark(_))
    }

    pub fn is_component_marker(&self) -> bool {
        matches!(self.kind, AnchorKind::ComponentMarker(_))
    }

    pub fn is_cursive(&self) -> bool {
        matches!(self.kind, AnchorKind::CursiveEntry)
            || matches!(self.kind, AnchorKind::CursiveExit)
    }

    /// If this is a ligature component anchor, return the index
    pub fn ligature_index(&self) -> Option<usize> {
        match self.kind {
            AnchorKind::Ligature { index, .. } | AnchorKind::ComponentMarker(index) => Some(index),
            _ => None,
        }
    }
}

/// A type for building glyph anchors, reused by different backends
#[derive(Debug, Clone)]
pub struct AnchorBuilder {
    glyph_name: GlyphName,
    anchors: HashMap<SmolStr, HashMap<NormalizedLocation, Point>>,
}

impl AnchorBuilder {
    pub fn new(glyph_name: GlyphName) -> Self {
        Self {
            glyph_name,
            anchors: Default::default(),
        }
    }

    pub fn add(
        &mut self,
        anchor_name: SmolStr,
        loc: NormalizedLocation,
        pos: Point,
    ) -> Result<(), BadGlyph> {
        if self
            .anchors
            .entry(anchor_name.clone())
            .or_default()
            .insert(loc.clone(), pos)
            .is_some()
        {
            return Err(BadGlyph::new(
                self.glyph_name.clone(),
                BadAnchor::new(anchor_name, BadAnchorReason::Ambiguous(loc)),
            ));
        }
        Ok(())
    }

    pub fn build(self) -> Result<GlyphAnchors, BadGlyph> {
        // It would be nice if everyone was defined at default
        for (anchor, positions) in &self.anchors {
            if !positions.keys().any(|loc| !loc.has_any_non_zero()) {
                return Err(BadGlyph::new(
                    self.glyph_name.clone(),
                    BadAnchor::new(anchor.clone(), BadAnchorReason::NoDefault),
                ));
            }
        }

        let anchors = self
            .anchors
            .into_iter()
            .map(|(name, positions)| {
                AnchorKind::new(&name)
                    .map(|type_| Anchor {
                        kind: type_,
                        positions,
                    })
                    .map_err(|reason| BadAnchor::new(name, reason))
            })
            .collect::<Result<_, _>>()
            .map_err(|e| BadGlyph::new(self.glyph_name.clone(), e))?;
        Ok(GlyphAnchors::new(self.glyph_name, anchors))
    }
}

/// A variable definition of a single glyph.
///
/// Guarrantees at least one definition. Currently that must be at
/// the default location. In theory that limitation is unnecessary,
/// a variable glyph that covers the default location could compile.
///
/// If defined in many locations, presumed to vary continuously
/// between positions and required to have variation compatible structure.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct Glyph {
    pub name: GlyphName,
    /// Whether to "export" in source terms
    pub emit_to_binary: bool,
    pub codepoints: HashSet<u32>, // single unicodes that each point to this glyph. Typically 0 or 1.
    default_location: NormalizedLocation,
    sources: HashMap<NormalizedLocation, GlyphInstance>,
    has_consistent_2x2_transforms: bool,
}

/// Compute this during glyph processing and without allocation
///
/// See <https://github.com/googlefonts/fontc/issues/458>
fn has_consistent_2x2_transforms(
    name: &GlyphName,
    sources: &HashMap<NormalizedLocation, GlyphInstance>,
) -> bool {
    let mut instances = sources.values();
    let Some(first) = instances.next() else {
        return true; // all none of us are the same
    };

    let consistent = instances.all(|inst| {
        if first.components.len() != inst.components.len() {
            return false;
        }
        first
            .components
            .iter()
            .zip(inst.components.iter())
            .all(|(c1, c2)| {
                c1.base == c2.base && c1.transform.as_coeffs()[..4] == c2.transform.as_coeffs()[..4]
            })
    });
    if log_enabled!(log::Level::Trace) && !consistent {
        trace!("{name} has inconsistent component names or 2x2 transforms");
    }
    consistent
}

impl Glyph {
    pub fn new(
        name: GlyphName,
        emit_to_binary: bool,
        codepoints: HashSet<u32>,
        instances: HashMap<NormalizedLocation, GlyphInstance>,
    ) -> Result<Self, BadGlyph> {
        if instances.is_empty() {
            return Err(BadGlyph::new(name, BadGlyphKind::NoInstances));
        }
        let mut defaults = instances
            .keys()
            .filter(|loc| !loc.iter().any(|(_, c)| c.into_inner() != 0.0));
        let default_location = match (defaults.next(), defaults.next()) {
            (None, _) => return Err(BadGlyph::new(name, BadGlyphKind::NoDefaultLocation)),
            (Some(_), Some(_)) => {
                return Err(BadGlyph::new(name, BadGlyphKind::MultipleDefaultLocations))
            }
            (Some(pos), None) => pos.to_owned(),
        };

        let has_consistent_2x2_transforms = has_consistent_2x2_transforms(&name, &instances);
        Ok(Glyph {
            name,
            emit_to_binary,
            codepoints,
            default_location,
            sources: instances,
            has_consistent_2x2_transforms,
        })
    }

    pub fn default_instance(&self) -> &GlyphInstance {
        self.sources.get(&self.default_location).unwrap()
    }

    pub fn sources(&self) -> &HashMap<NormalizedLocation, GlyphInstance> {
        &self.sources
    }

    pub fn sources_mut(
        &mut self,
    ) -> impl Iterator<Item = (&NormalizedLocation, &mut GlyphInstance)> {
        self.sources.iter_mut()
    }

    pub fn source_mut(&mut self, loc: &NormalizedLocation) -> Option<&mut GlyphInstance> {
        self.sources.get_mut(loc)
    }

    /// Iterate over the names of all components in all instances.
    ///
    /// This will return duplicates if multiple instances have identical
    /// components (which is normal)
    pub(crate) fn component_names(&self) -> impl Iterator<Item = &GlyphName> {
        self.sources
            .values()
            .flat_map(|inst| inst.components.iter().map(|comp| &comp.base))
    }

    /// Does the Glyph use the same components, (name, 2x2 transform), for all instances?
    ///
    /// The (glyphname, 2x2 transform) pair is considered for uniqueness. Note that
    /// translation IS not considered for uniqueness because components are allowed
    /// to vary in translation.
    #[inline]
    pub(crate) fn has_consistent_components(&self) -> bool {
        self.has_consistent_2x2_transforms
    }

    /// Does the glyph have any component with a non-identity 2x2
    ///
    /// See <https://github.com/googlefonts/fontc/issues/291#issuecomment-1557358538>
    pub(crate) fn has_nonidentity_2x2(&self) -> bool {
        self.sources
            .values()
            .flat_map(|inst| inst.components.iter())
            .any(|c| c.transform.as_coeffs()[..4] != [1.0, 0.0, 0.0, 1.0])
    }
}

impl IdAware<WorkId> for Glyph {
    fn id(&self) -> WorkId {
        WorkId::Glyph(self.name.clone())
    }
}

impl Persistable for Glyph {
    fn read(from: &mut dyn Read) -> Self {
        serde_yaml::from_reader(from).unwrap()
    }

    fn write(&self, to: &mut dyn std::io::Write) {
        serde_yaml::to_writer(to, self).unwrap();
    }
}

impl Persistable for GlyphOrder {
    fn read(from: &mut dyn Read) -> Self {
        serde_yaml::from_reader(from).unwrap()
    }

    fn write(&self, to: &mut dyn std::io::Write) {
        serde_yaml::to_writer(to, self).unwrap();
    }
}

impl Persistable for GlobalMetrics {
    fn read(from: &mut dyn Read) -> Self {
        serde_yaml::from_reader(from).unwrap()
    }

    fn write(&self, to: &mut dyn std::io::Write) {
        serde_yaml::to_writer(to, self).unwrap();
    }
}

impl Persistable for FeaturesSource {
    fn read(from: &mut dyn Read) -> Self {
        serde_yaml::from_reader(from).unwrap()
    }

    fn write(&self, to: &mut dyn std::io::Write) {
        serde_yaml::to_writer(to, self).unwrap();
    }
}

impl Persistable for KerningGroups {
    fn read(from: &mut dyn Read) -> Self {
        serde_yaml::from_reader(from).unwrap()
    }

    fn write(&self, to: &mut dyn std::io::Write) {
        serde_yaml::to_writer(to, self).unwrap();
    }
}

impl Persistable for KerningInstance {
    fn read(from: &mut dyn Read) -> Self {
        serde_yaml::from_reader(from).unwrap()
    }

    fn write(&self, to: &mut dyn std::io::Write) {
        serde_yaml::to_writer(to, self).unwrap();
    }
}

impl IdAware<WorkId> for KerningInstance {
    fn id(&self) -> WorkId {
        WorkId::KernInstance(self.location.clone())
    }
}

impl IdAware<WorkId> for GlyphAnchors {
    fn id(&self) -> WorkId {
        WorkId::Anchor(self.glyph_name.clone())
    }
}

impl Persistable for GlyphAnchors {
    fn read(from: &mut dyn Read) -> Self {
        serde_yaml::from_reader(from).unwrap()
    }

    fn write(&self, to: &mut dyn std::io::Write) {
        serde_yaml::to_writer(to, self).unwrap();
    }
}

impl Persistable for ColorPalettes {
    fn read(from: &mut dyn Read) -> Self {
        serde_yaml::from_reader(from).unwrap()
    }

    fn write(&self, to: &mut dyn std::io::Write) {
        serde_yaml::to_writer(to, self).unwrap();
    }
}

impl Persistable for PaintGraph {
    fn read(from: &mut dyn Read) -> Self {
        serde_yaml::from_reader(from).unwrap()
    }

    fn write(&self, to: &mut dyn std::io::Write) {
        serde_yaml::to_writer(to, self).unwrap();
    }
}

/// A variable definition of a single glyph.
///
/// If defined in many locations, presumed to vary continuously
/// between positions and required to have variation compatible structure.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct GlyphBuilder {
    pub name: GlyphName,
    pub emit_to_binary: bool,
    pub codepoints: HashSet<u32>, // single unicodes that each point to this glyph. Typically 0 or 1.
    pub sources: HashMap<NormalizedLocation, GlyphInstance>,
}

impl GlyphBuilder {
    pub fn new(name: GlyphName) -> Self {
        Self {
            name,
            emit_to_binary: true,
            codepoints: HashSet::new(),
            sources: HashMap::new(),
        }
    }

    pub fn try_add_source(
        &mut self,
        unique_location: &NormalizedLocation,
        source: GlyphInstance,
    ) -> Result<(), BadGlyph> {
        if self.sources.contains_key(unique_location) {
            return Err(BadGlyph::new(
                self.name.clone(),
                BadGlyphKind::DuplicateLocation(unique_location.clone()),
            ));
        }
        self.sources.insert(unique_location.clone(), source);
        Ok(())
    }

    /// * see <https://github.com/googlefonts/ufo2ft/blob/b3895a96ca910c1764df016bfee4719448cfec4a/Lib/ufo2ft/outlineCompiler.py#L1666-L1694>
    pub fn new_notdef(
        default_location: NormalizedLocation,
        upem: u16,
        ascender: f64,
        descender: f64,
    ) -> Self {
        let upem = upem as f64;
        let width: u16 = (upem * 0.5).ot_round();
        let width = width as f64;
        let stroke: u16 = (upem * 0.05).ot_round();
        let stroke = stroke as f64;

        let mut path = BezPath::new();

        // outer box
        let x_min = stroke;
        let x_max = width - stroke;
        let y_max = ascender;
        let y_min = descender;
        path.move_to((x_min, y_min));
        path.line_to((x_max, y_min));
        path.line_to((x_max, y_max));
        path.line_to((x_min, y_max));
        path.line_to((x_min, y_min));
        path.close_path();

        // inner, cut out, box
        let x_min = x_min + stroke;
        let x_max = x_max - stroke;
        let y_max = y_max - stroke;
        let y_min = y_min + stroke;
        path.move_to((x_min, y_min));
        path.line_to((x_min, y_max));
        path.line_to((x_max, y_max));
        path.line_to((x_max, y_min));
        path.line_to((x_min, y_min));
        path.close_path();

        // TODO: Should this be None to match other glyphs' heights? This would deviate from ufo2ft
        // See https://github.com/googlefonts/ufo2ft/blob/b3895a96/Lib/ufo2ft/outlineCompiler.py#L1656-L1658
        let height = Some(ascender - descender);

        Self {
            name: GlyphName::NOTDEF.clone(),
            emit_to_binary: true,
            codepoints: HashSet::new(),
            sources: HashMap::from([(
                default_location,
                GlyphInstance {
                    width,
                    height,
                    contours: vec![path],
                    ..Default::default()
                },
            )]),
        }
    }

    pub fn build(self) -> Result<Glyph, BadGlyph> {
        Glyph::new(
            self.name,
            self.emit_to_binary,
            self.codepoints,
            self.sources,
        )
    }
}

impl From<Glyph> for GlyphBuilder {
    fn from(value: Glyph) -> Self {
        Self {
            name: value.name,
            emit_to_binary: value.emit_to_binary,
            codepoints: value.codepoints,
            sources: value.sources,
        }
    }
}

/// A Glyph at a specific position in designspace.
#[derive(Serialize, Deserialize, Debug, Default, Clone, PartialEq)]
pub struct GlyphInstance {
    /// Advance width.
    pub width: f64,
    /// Advance height; if None, assumed to equal font's typo ascender - descender.
    pub height: Option<f64>,
    /// Vertical origin; if None, assumed to equal font's typo ascender.
    pub vertical_origin: Option<f64>,
    /// List of glyph contours.
    pub contours: Vec<BezPath>,
    /// List of glyph components.
    pub components: Vec<Component>,
}

impl GlyphInstance {
    /// Returns the concatenation of the element types for each outline.
    ///
    /// These are 'M' for moveto, 'L' for lineto, 'Q' for quadto, 'C' for
    /// curveto and 'Z' for closepath.
    pub fn path_elements(&self) -> String {
        fn path_el_type(el: &PathEl) -> &'static str {
            match el {
                PathEl::MoveTo(..) => "M",
                PathEl::LineTo(..) => "L",
                PathEl::QuadTo(..) => "Q",
                PathEl::CurveTo(..) => "C",
                PathEl::ClosePath => "Z",
            }
        }
        self.contours
            .iter()
            .flat_map(|c| c.elements().iter().map(path_el_type))
            .collect()
    }

    /// Get the advance height of this instance, falling back to a value derived
    /// from the metrics for this instance's position.
    ///
    /// See [`Self::height`](field@Self::height).
    pub fn height(&self, metrics: &GlobalMetricsInstance) -> u16 {
        // https://github.com/googlefonts/glyphsLib/blob/c4db6b98/Lib/glyphsLib/builder/glyph.py#L359-L389
        // TODO: UFO always defines this; should it be made non-optional?
        self.height
            .unwrap_or_else(|| {
                metrics.os2_typo_ascender.into_inner() - metrics.os2_typo_descender.into_inner()
            })
            .ot_round()
    }

    /// Get the vertical origin of this instance, falling back to a value
    /// derived from the metrics for this instance's position.
    ///
    /// See [`Self::vertical_origin`](field@Self::vertical_origin).
    pub fn vertical_origin(&self, metrics: &GlobalMetricsInstance) -> i16 {
        // https://github.com/googlefonts/ufo2ft/blob/16ed156b/Lib/ufo2ft/outlineCompiler.py#L74-L81
        self.vertical_origin
            .unwrap_or(metrics.os2_typo_ascender.into_inner())
            .ot_round()
    }
}

/// A single glyph component, reference to another glyph.
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct Component {
    /// The name of the referenced glyph.
    pub base: GlyphName,
    /// Affine transformation to apply to the referenced glyph.
    pub transform: Affine,
}

/// Data to inform construction of [CPAL](https://learn.microsoft.com/en-us/typography/opentype/spec/cpal#palette-table-header)
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct ColorPalettes {
    // All palettes have the same number of colors; there is at least one palette with at least one color
    pub palettes: Vec<Vec<Color>>,
}

impl ColorPalettes {
    pub fn new(mut palettes: Vec<Vec<Color>>) -> Result<Option<Self>, Error> {
        // Fail if boring
        if !palettes.iter().any(|p| !p.is_empty()) {
            return Ok(None);
        }

        // Fail if inconsistent
        if let Some(bad_idx) = palettes.iter().position(|p| p.len() != palettes[0].len()) {
            return Err(Error::InconsistentPaletteLength {
                size_0: palettes[0].len(),
                n: bad_idx,
                size_n: palettes[bad_idx].len(),
            });
        }

        // Trivial optimization: if there is only one palette we can deduplicate it
        if let [one_palette] = palettes.as_mut_slice() {
            one_palette.sort();
            one_palette.dedup();
        }
        // Sort for stability in output
        for p in palettes.iter_mut() {
            p.sort();
        }

        Ok(Some(Self { palettes }))
    }
}

/// We may in time need more than RGBA, but likely not for some years so start simple
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Color {
    pub r: u8,
    pub g: u8,
    pub b: u8,
    pub a: u8,
}

/// Data to inform construction of [COLR](https://learn.microsoft.com/en-us/typography/opentype/spec/colr)
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct PaintGraph {}

#[cfg(test)]
mod tests {

    use std::collections::HashMap;

    use write_fonts::types::NameId;

    use pretty_assertions::assert_eq;

    use super::*;

    // from
    // <https://github.com/googlefonts/ufo2ft/blob/6787e37e6/tests/featureWriters/markFeatureWriter_test.py#L34>
    #[test]
    fn anchor_names() {
        assert_eq!(AnchorKind::new("top"), Ok(AnchorKind::Base("top".into())));
        assert_eq!(AnchorKind::new("top_"), Ok(AnchorKind::Base("top_".into())));
        assert_eq!(AnchorKind::new("top1"), Ok(AnchorKind::Base("top1".into())));
        assert_eq!(
            AnchorKind::new("_bottom"),
            Ok(AnchorKind::Mark("bottom".into()))
        );
        assert_eq!(
            AnchorKind::new("bottom_2"),
            Ok(AnchorKind::Ligature {
                group_name: "bottom".into(),
                index: 2
            })
        );
        assert_eq!(
            AnchorKind::new("top_right_1"),
            Ok(AnchorKind::Ligature {
                group_name: "top_right".into(),
                index: 1
            })
        );
    }

    #[test]
    fn caret_anchor_names() {
        assert_eq!(AnchorKind::new("caret_1"), Ok(AnchorKind::Caret(1)));
        assert_eq!(AnchorKind::new("vcaret_1"), Ok(AnchorKind::VCaret(1)));
        assert_eq!(AnchorKind::new("vcaret_0"), Err(BadAnchorReason::ZeroIndex));
        // fontmake accepts this, so we should too
        assert_eq!(AnchorKind::new("caret_"), Ok(AnchorKind::Caret(1)));
        assert_eq!(AnchorKind::new("vcaret_"), Ok(AnchorKind::VCaret(1)));
    }

    #[test]
    fn ligature_empty_component_anchor_name() {
        assert_eq!(AnchorKind::new("_3"), Ok(AnchorKind::ComponentMarker(3)));
        assert_eq!(AnchorKind::new("_0"), Err(BadAnchorReason::ZeroIndex));
    }

    // from
    // <https://github.com/googlefonts/ufo2ft/blob/6787e37e6/tests/featureWriters/markFeatureWriter_test.py#L50-L59>
    #[test]
    fn bad_anchor_names() {
        assert_eq!(
            AnchorKind::new("_top_2"),
            Err(BadAnchorReason::NumberedMarkAnchor)
        );
        assert_eq!(AnchorKind::new("_"), Err(BadAnchorReason::NilMarkGroup));
        assert_eq!(AnchorKind::new("top_0"), Err(BadAnchorReason::ZeroIndex));
    }

    fn assert_names(expected: &[(NameId, &str)], actual: HashMap<NameKey, String>) {
        let mut actual: Vec<_> = actual
            .iter()
            .map(|(k, v)| (k.name_id, v.as_str()))
            .collect();
        actual.sort_by_key(|(k, _)| *k);
        assert_eq!(expected, actual.as_slice());
    }

    fn assert_name(names: &HashMap<NameKey, String>, expected: &str, name_id: NameId) {
        assert_eq!(
            Some(expected),
            names
                .get(&NameKey::new(name_id, expected))
                .map(String::as_str)
        );
    }

    #[test]
    fn empty_name_builder_default_fallbacks() {
        let mut builder = NameBuilder::default();
        builder.apply_default_fallbacks(DEFAULT_VENDOR_ID);
        let names = builder.into_inner();

        assert_names(
            &[
                (NameId::FAMILY_NAME, "New Font"),
                (NameId::SUBFAMILY_NAME, "Regular"),
                (NameId::UNIQUE_ID, "0.000;NONE;NewFont-Regular"),
                (NameId::FULL_NAME, "New Font Regular"),
                (NameId::VERSION_STRING, "Version 0.000"),
                (NameId::POSTSCRIPT_NAME, "NewFont-Regular"),
            ],
            names,
        )
    }

    #[test]
    fn fallback_version_and_unique_id() {
        let mut builder = NameBuilder::default();
        builder.set_version(1, 2);
        builder.apply_default_fallbacks(DEFAULT_VENDOR_ID);
        let names = builder.into_inner();

        assert_name(&names, "Version 1.002", NameId::VERSION_STRING);
        assert_name(&names, "1.002;NONE;NewFont-Regular", NameId::UNIQUE_ID);
    }

    #[test]
    fn fallbacks_from_ribbi_typographic_names() {
        let mut builder = NameBuilder::default();
        builder.add(NameId::TYPOGRAPHIC_FAMILY_NAME, "Family".into());
        builder.add(NameId::TYPOGRAPHIC_SUBFAMILY_NAME, "Bold Italic".into());
        builder.apply_default_fallbacks(DEFAULT_VENDOR_ID);
        let names = builder.into_inner();

        assert_names(
            &[
                (NameId::FAMILY_NAME, "Family"),
                (NameId::SUBFAMILY_NAME, "Bold Italic"),
                (NameId::UNIQUE_ID, "0.000;NONE;Family-BoldItalic"),
                (NameId::FULL_NAME, "Family Bold Italic"),
                (NameId::VERSION_STRING, "Version 0.000"),
                (NameId::POSTSCRIPT_NAME, "Family-BoldItalic"),
            ],
            names,
        )
    }

    #[test]
    fn fallbacks_from_non_ribbi_typographic_names() {
        let mut builder = NameBuilder::default();
        builder.add(NameId::TYPOGRAPHIC_FAMILY_NAME, "Family".into());
        builder.add(NameId::TYPOGRAPHIC_SUBFAMILY_NAME, "Subfamily".into());
        builder.apply_default_fallbacks(DEFAULT_VENDOR_ID);
        let names = builder.into_inner();

        assert_names(
            &[
                (NameId::FAMILY_NAME, "Family Subfamily"),
                (NameId::SUBFAMILY_NAME, "Regular"),
                (NameId::UNIQUE_ID, "0.000;NONE;Family-Subfamily"),
                (NameId::FULL_NAME, "Family Subfamily"),
                (NameId::VERSION_STRING, "Version 0.000"),
                (NameId::POSTSCRIPT_NAME, "Family-Subfamily"),
                (NameId::TYPOGRAPHIC_FAMILY_NAME, "Family"),
                (NameId::TYPOGRAPHIC_SUBFAMILY_NAME, "Subfamily"),
            ],
            names,
        )
    }

    #[test]
    fn both_legacy_and_typo_names_defined_and_distinct() {
        let mut builder = NameBuilder::default();
        builder.add(NameId::FAMILY_NAME, "Legacy Family".into());
        builder.add(NameId::SUBFAMILY_NAME, "Regular".into());
        builder.add(NameId::TYPOGRAPHIC_FAMILY_NAME, "Family".into());
        builder.add(NameId::TYPOGRAPHIC_SUBFAMILY_NAME, "Subfamily".into());
        builder.apply_default_fallbacks(DEFAULT_VENDOR_ID);
        let names = builder.into_inner();

        assert_names(
            &[
                (NameId::FAMILY_NAME, "Legacy Family"),
                (NameId::SUBFAMILY_NAME, "Regular"),
                (NameId::UNIQUE_ID, "0.000;NONE;Family-Subfamily"),
                (NameId::FULL_NAME, "Family Subfamily"),
                (NameId::VERSION_STRING, "Version 0.000"),
                (NameId::POSTSCRIPT_NAME, "Family-Subfamily"),
                (NameId::TYPOGRAPHIC_FAMILY_NAME, "Family"),
                (NameId::TYPOGRAPHIC_SUBFAMILY_NAME, "Subfamily"),
            ],
            names,
        )
    }

    #[test]
    fn both_legacy_and_typo_names_defined_and_same() {
        let mut builder = NameBuilder::default();
        builder.add(NameId::FAMILY_NAME, "Family".into());
        builder.add(NameId::SUBFAMILY_NAME, "Subfamily".into());
        builder.add(NameId::TYPOGRAPHIC_FAMILY_NAME, "Family".into());
        builder.add(NameId::TYPOGRAPHIC_SUBFAMILY_NAME, "Subfamily".into());
        builder.apply_default_fallbacks(DEFAULT_VENDOR_ID);
        let names = builder.into_inner();

        assert_names(
            &[
                (NameId::FAMILY_NAME, "Family"),
                (NameId::SUBFAMILY_NAME, "Subfamily"),
                (NameId::UNIQUE_ID, "0.000;NONE;Family-Subfamily"),
                (NameId::FULL_NAME, "Family Subfamily"),
                (NameId::VERSION_STRING, "Version 0.000"),
                (NameId::POSTSCRIPT_NAME, "Family-Subfamily"),
                // duplicate typographic names are not included
            ],
            names,
        )
    }

    #[test]
    fn only_legacy_family_and_subfamily_defined() {
        let mut builder = NameBuilder::default();
        builder.add(NameId::FAMILY_NAME, "Family".into());
        builder.add(NameId::SUBFAMILY_NAME, "Italic".into());
        builder.apply_default_fallbacks(DEFAULT_VENDOR_ID);
        let names = builder.into_inner();

        assert_names(
            &[
                (NameId::FAMILY_NAME, "Family"),
                (NameId::SUBFAMILY_NAME, "Italic"),
                (NameId::UNIQUE_ID, "0.000;NONE;Family-Italic"),
                (NameId::FULL_NAME, "Family Italic"),
                (NameId::VERSION_STRING, "Version 0.000"),
                (NameId::POSTSCRIPT_NAME, "Family-Italic"),
            ],
            names,
        )
    }

    // We had a bug where ((1290 as f64 * 0.35) as f32).ot_round() was 452.
    // Without the as f32 (caused by GlobalMetrics using f32) it's 451. In Python it's 451.
    #[test]
    fn round_like_py() {
        let pos = NormalizedLocation::for_pos(&[("wght", 0.0)]);
        let mut metrics = GlobalMetrics::new();
        metrics.populate_defaults(&pos, 1290, None, None, None, None);
        let rounded: i16 = metrics
            .get(GlobalMetric::SuperscriptYOffset, &pos)
            .0
            .ot_round();
        assert_eq!(451, rounded);
    }

    #[test]
    fn only_clear_preferred_names_if_both_identical() {
        fn make_builder(names: &[(NameId, &str)]) -> NameBuilder {
            let mut builder = NameBuilder::default();
            for (id, name) in names {
                builder.add(*id, name.to_string());
            }
            builder.apply_default_fallbacks("cmyr");
            builder
        }

        let identical = make_builder(&[
            (NameId::FAMILY_NAME, "Derp"),
            (NameId::TYPOGRAPHIC_FAMILY_NAME, "Derp"),
            (NameId::SUBFAMILY_NAME, "Regular"),
            (NameId::TYPOGRAPHIC_SUBFAMILY_NAME, "Regular"),
        ]);

        assert!(identical.get(NameId::TYPOGRAPHIC_FAMILY_NAME).is_none());
        assert!(identical.get(NameId::TYPOGRAPHIC_SUBFAMILY_NAME).is_none());

        let different_subfamily = make_builder(&[
            (NameId::FAMILY_NAME, "Derp"),
            (NameId::TYPOGRAPHIC_FAMILY_NAME, "Derp"),
            (NameId::SUBFAMILY_NAME, "Regular"),
            (NameId::TYPOGRAPHIC_SUBFAMILY_NAME, "Regular Italic"),
        ]);

        assert!(different_subfamily
            .get(NameId::TYPOGRAPHIC_FAMILY_NAME)
            .is_some());
        assert!(different_subfamily
            .get(NameId::TYPOGRAPHIC_SUBFAMILY_NAME)
            .is_some());
    }

    // If x-height is undefined, ufo2ft derives the strikeout position from its
    // default fallback:
    //    https://github.com/googlefonts/ufo2ft/blob/2f11b0ff/Lib/ufo2ft/outlineCompiler.py#L587
    //    https://github.com/googlefonts/ufo2ft/blob/2f11b0ff/Lib/ufo2ft/outlineCompiler.py#L633
    #[test]
    fn default_strikeout_when_xheight_none() {
        let pos = NormalizedLocation::for_pos(&[("wght", 0.0)]);

        let mut metrics = GlobalMetrics::new();
        metrics.populate_defaults(&pos, 1000, None, None, None, None);

        let default = metrics.get(GlobalMetric::StrikeoutPosition, &pos);
        assert_eq!(default.into_inner(), 300.);
    }

    // An explicit x-height of zero has specific semantic meaning:
    //    https://learn.microsoft.com/en-us/typography/opentype/spec/os2#sxheight
    // and ufo2ft derives strikeout position from UPM instead in this scenario:
    //    https://github.com/googlefonts/ufo2ft/blob/2f11b0ff/Lib/ufo2ft/outlineCompiler.py#L633
    #[test]
    fn default_strikeout_when_xheight_zero() {
        let pos = NormalizedLocation::for_pos(&[("wght", 0.0)]);

        let mut metrics = GlobalMetrics::new();
        metrics.populate_defaults(&pos, 1000, Some(0.), None, None, None);

        let default = metrics.get(GlobalMetric::StrikeoutPosition, &pos);
        assert_eq!(default.into_inner(), 220.);
    }

    fn make_glyph_order<'a>(raw: impl IntoIterator<Item = &'a str>) -> GlyphOrder {
        raw.into_iter().map(GlyphName::from).collect()
    }

    #[test]
    fn glyphorder_equality() {
        assert_eq!(make_glyph_order(["a", "b"]), make_glyph_order(["a", "b"]));
    }

    #[test]
    fn glyphorder_non_equality() {
        assert_ne!(make_glyph_order(["a", "b"]), make_glyph_order(["b", "a"]));
    }
}
