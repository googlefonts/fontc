//! Font IR types.
use chrono::{DateTime, Utc};
use font_types::NameId;
use font_types::Tag;
use fontdrasil::{
    coords::{NormalizedCoord, NormalizedLocation, UserLocation},
    types::{AnchorName, Axis, GlyphName, GroupName},
};
use indexmap::IndexSet;
use kurbo::{Affine, BezPath, PathEl, Point};
use log::{log_enabled, trace, warn};
use ordered_float::OrderedFloat;
use serde::{Deserialize, Serialize};
use std::{
    collections::{hash_map::RandomState, BTreeMap, BTreeSet, HashMap, HashSet},
    fmt::Debug,
    io::Read,
    path::PathBuf,
};
use write_fonts::{tables::os2::SelectionFlags, OtRound};

use crate::{
    error::{PathConversionError, VariationModelError, WorkError},
    orchestration::{IdAware, Persistable, WorkId},
    serde::{
        GlobalMetricsSerdeRepr, GlyphOrderSerdeRepr, GlyphSerdeRepr, KerningSerdeRepr,
        MiscSerdeRepr, StaticMetadataSerdeRepr,
    },
    variations::VariationModel,
};

pub const DEFAULT_VENDOR_ID: &str = "NONE";
const DEFAULT_VENDOR_ID_TAG: Tag = Tag::new(b"NONE");

/// Global font info that cannot vary across the design space.
///
/// For example, upem, axis definitions, etc, as distinct from
/// metadata that varies across design space such as ascender/descender.
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
#[serde(from = "StaticMetadataSerdeRepr", into = "StaticMetadataSerdeRepr")]
pub struct StaticMetadata {
    /// See <https://learn.microsoft.com/en-us/typography/opentype/spec/head>.
    pub units_per_em: u16,

    /// Every axis used by the font being compiled, including point axes.
    ///
    /// This is relatively rarely what you want.
    pub all_source_axes: Vec<Axis>,

    /// Every variable (non-point) axis used by the font being compiled.
    ///
    /// If empty this is a static font.
    pub axes: Vec<Axis>,

    /// Named locations in variation space
    pub named_instances: Vec<NamedInstance>,

    /// A model of how variation space is split into regions that have deltas.
    ///
    /// This copy includes all locations used in the entire font. That is, every
    /// location any glyph has an instance. Use of a location not in the global model
    /// is an error. This model enforces the no delta at the default location constraint
    /// used in things like gvar.
    pub variation_model: VariationModel,

    default_location: NormalizedLocation,

    /// See <https://learn.microsoft.com/en-us/typography/opentype/spec/name>.
    pub names: HashMap<NameKey, String>,

    /// See <https://learn.microsoft.com/en-us/typography/opentype/spec/post> and
    /// <https://github.com/adobe-type-tools/agl-specification>
    pub postscript_names: PostscriptNames,

    /// Miscellaneous font-wide data that didn't seem worthy of top billing
    pub misc: MiscMetadata,
}

/// Metadata primarily feeding the OS/2 table.
///
/// <https://learn.microsoft.com/en-us/typography/opentype/spec/os2>
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
#[serde(from = "MiscSerdeRepr", into = "MiscSerdeRepr")]
pub struct MiscMetadata {
    /// See <https://learn.microsoft.com/en-us/typography/opentype/spec/os2#fsselection>
    pub selection_flags: SelectionFlags,

    /// See <https://learn.microsoft.com/en-us/typography/opentype/spec/os2#achvendid>
    pub vendor_id: Tag,

    pub underline_thickness: OrderedFloat<f32>,
    pub underline_position: OrderedFloat<f32>,

    /// UFO appears to allow negative major versions.
    ///
    /// See <https://unifiedfontobject.org/versions/ufo3/fontinfo.plist/#generic-identification-information>
    pub version_major: i32,
    pub version_minor: u32,

    pub head_flags: u16,
    pub lowest_rec_ppm: u16,

    pub created: Option<DateTime<Utc>>,
}

/// The name of every glyph, in the order it will be emitted
///
/// <https://rsheeter.github.io/font101/#glyph-ids-and-the-cmap-table>
#[derive(Serialize, Deserialize, Default, Debug, Clone, PartialEq, Eq)]
#[serde(from = "GlyphOrderSerdeRepr", into = "GlyphOrderSerdeRepr")]
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

impl GlyphOrder {
    pub fn new() -> Self {
        GlyphOrder(IndexSet::new())
    }

    pub fn glyph_id(&self, name: &GlyphName) -> Option<u32> {
        self.0.get_index_of(name).map(|i| i as u32)
    }

    pub fn glyph_name(&self, index: usize) -> Option<&GlyphName> {
        self.0.get_index(index)
    }

    pub fn contains(&self, name: &GlyphName) -> bool {
        self.0.contains(name)
    }

    pub fn iter(&self) -> impl Iterator<Item = &GlyphName> {
        self.0.iter()
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

/// Glyph names mapped to postscript names
pub type PostscriptNames = HashMap<GlyphName, GlyphName>;

/// In logical (reading) order
type KernPair = (KernParticipant, KernParticipant);
type KernValues = BTreeMap<NormalizedLocation, OrderedFloat<f32>>;

/// IR representation of kerning.
///
/// In UFO terms, roughly [groups.plist](https://unifiedfontobject.org/versions/ufo3/groups.plist/)
/// and [kerning.plist](https://unifiedfontobject.org/versions/ufo3/kerning.plist/) combined.
#[derive(Serialize, Deserialize, Debug, Default, Clone, PartialEq, Eq)]
#[serde(from = "KerningSerdeRepr", into = "KerningSerdeRepr")]
pub struct Kerning {
    pub groups: HashMap<GroupName, BTreeSet<GlyphName>>,
    /// An adjustment to the space *between* two glyphs in logical order.
    ///
    /// Maps (side1, side2) => a mapping location:adjustment.
    ///
    /// Used for both LTR and RTL. The BE application differs but the concept
    /// is the same.
    pub kerns: HashMap<KernPair, KernValues>,
}

impl Kerning {
    pub fn is_empty(&self) -> bool {
        self.kerns.is_empty()
    }
}

/// A participant in kerning, one of the entries in a kerning pair.
///
/// Concretely, a glyph or a group of glyphs.
///
/// <https://unifiedfontobject.org/versions/ufo3/kerning.plist/#kerning-pair-types>
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum KernParticipant {
    Glyph(GlyphName),
    Group(GroupName),
}

impl KernParticipant {
    #[inline]
    pub fn is_glyph(&self) -> bool {
        matches!(self, KernParticipant::Glyph(_))
    }

    #[inline]
    pub fn is_group(&self) -> bool {
        matches!(self, KernParticipant::Group(_))
    }
}

impl StaticMetadata {
    pub fn new(
        units_per_em: u16,
        names: HashMap<NameKey, String>,
        axes: Vec<Axis>,
        named_instances: Vec<NamedInstance>,
        glyph_locations: HashSet<NormalizedLocation>,
        postscript_names: PostscriptNames,
    ) -> Result<StaticMetadata, VariationModelError> {
        // Point axes are less exciting than ranged ones
        let variable_axes: Vec<_> = axes.iter().filter(|a| !a.is_point()).cloned().collect();

        // Claim names for axes and named instances
        let mut name_id_gen = 255;
        let mut key_to_name = names;
        let mut visited = HashSet::new();
        variable_axes
            .iter()
            .map(|axis| &axis.name)
            .chain(named_instances.iter().map(|ni| &ni.name))
            .for_each(|name| {
                if !visited.insert(name) {
                    return;
                }
                name_id_gen += 1;
                key_to_name.insert(NameKey::new(name_id_gen.into(), name), name.clone());
            });

        let variation_model = VariationModel::new(glyph_locations, variable_axes.clone())?;

        let default_location = axes
            .iter()
            .map(|a| (a.tag, NormalizedCoord::new(0.0)))
            .collect();

        Ok(StaticMetadata {
            units_per_em,
            names: key_to_name,
            all_source_axes: axes,
            axes: variable_axes,
            named_instances,
            variation_model,
            default_location,
            postscript_names,
            misc: MiscMetadata {
                selection_flags: Default::default(),
                vendor_id: DEFAULT_VENDOR_ID_TAG,
                underline_thickness: 0.0.into(),
                underline_position: 0.0.into(),
                // https://github.com/googlefonts/ufo2ft/blob/0d2688cd847d003b41104534d16973f72ef26c40/Lib/ufo2ft/fontInfoData.py#L353-L354
                version_major: 0,
                version_minor: 0,
                // <https://github.com/googlefonts/ufo2ft/blob/0d2688cd847d003b41104534d16973f72ef26c40/Lib/ufo2ft/fontInfoData.py#L364>
                lowest_rec_ppm: 6,
                // <https://github.com/googlefonts/ufo2ft/blob/0d2688cd847d003b41104534d16973f72ef26c40/Lib/ufo2ft/fontInfoData.py#L365>
                head_flags: 3,
                created: None,
            },
        })
    }

    /// The default on all variable axes.
    pub fn default_location(&self) -> &NormalizedLocation {
        &self.default_location
    }
}

/// Global metrics. Ascender/descender, cap height, etc.
///
/// Represents the values of these metrics at a specific position in design space.
/// At a minimum should be defined at the default location.
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
#[serde(from = "GlobalMetricsSerdeRepr", into = "GlobalMetricsSerdeRepr")]
pub struct GlobalMetrics(
    pub(crate) HashMap<GlobalMetric, HashMap<NormalizedLocation, OrderedFloat<f32>>>,
);

#[derive(Serialize, Deserialize, Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum GlobalMetric {
    Ascender,
    Descender,
    HheaAscender,
    HheaDescender,
    HheaLineGap,
    Os2TypoAscender,
    Os2TypoDescender,
    Os2TypoLineGap,
    Os2WinAscent,
    Os2WinDescent,
    CapHeight,
    CaretSlopeRise,
    XHeight,
    ItalicAngle,
    YSubscriptXSize,
    YSubscriptYSize,
    YSubscriptXOffset,
    YSubscriptYOffset,
    YSuperscriptXSize,
    YSuperscriptYSize,
    YSuperscriptXOffset,
    YSuperscriptYOffset,
    YStrikeoutSize,
    YStrikeoutPosition,
}

/// Adjust Y offset based on italic angle, to get X offset.
///
///  <https://github.com/googlefonts/ufo2ft/blob/0d2688cd847d003b41104534d16973f72ef26c40/Lib/ufo2ft/outlineCompiler.py#L571-L573>
fn adjust_offset(offset: f32, angle: f32) -> f32 {
    if angle.abs() >= f32::EPSILON {
        offset * (-angle).to_radians().tan()
    } else {
        0.0
    }
}

impl GlobalMetrics {
    /// Creates a new [GlobalMetrics] with default values at the default location.
    pub fn new(
        default_location: NormalizedLocation,
        units_per_em: u16,
        x_height: Option<f32>,
    ) -> GlobalMetrics {
        let mut metrics = GlobalMetrics(HashMap::new());
        let mut set = |metric, value| metrics.set(metric, default_location.clone(), value);

        // https://github.com/googlefonts/ufo2ft/blob/fca66fe3ea1ea88ffb36f8264b21ce042d3afd05/Lib/ufo2ft/fontInfoData.py#L38-L45
        let ascender = 0.8 * units_per_em as f32;
        let descender = -0.2 * units_per_em as f32;
        set(GlobalMetric::Ascender, ascender);
        set(GlobalMetric::Descender, descender);

        // https://github.com/googlefonts/ufo2ft/blob/0d2688cd847d003b41104534d16973f72ef26c40/Lib/ufo2ft/fontInfoData.py#L229-L238
        let typo_line_gap = units_per_em as f32 * 1.2 + descender - ascender;
        let typo_line_gap = if typo_line_gap > 0.0 {
            typo_line_gap
        } else {
            0.0
        };
        set(GlobalMetric::Os2TypoLineGap, typo_line_gap);

        // https://github.com/googlefonts/ufo2ft/blob/0d2688cd847d003b41104534d16973f72ef26c40/Lib/ufo2ft/fontInfoData.py#L215-L226
        set(GlobalMetric::Os2TypoAscender, ascender + typo_line_gap);
        set(GlobalMetric::Os2TypoDescender, descender);

        // https://github.com/googlefonts/ufo2ft/blob/0d2688cd847d003b41104534d16973f72ef26c40/Lib/ufo2ft/fontInfoData.py#L126-L130
        set(GlobalMetric::HheaAscender, ascender + typo_line_gap);
        set(GlobalMetric::HheaDescender, descender);
        // https://github.com/googlefonts/ufo2ft/blob/0d2688cd847d003b41104534d16973f72ef26c40/Lib/ufo2ft/fontInfoData.py#L366
        set(GlobalMetric::HheaLineGap, 0.0);

        // https://github.com/googlefonts/ufo2ft/blob/0d2688cd847d003b41104534d16973f72ef26c40/Lib/ufo2ft/fontInfoData.py#L241-L254
        set(GlobalMetric::Os2WinAscent, ascender);
        set(GlobalMetric::Os2WinDescent, descender);

        // https://github.com/googlefonts/ufo2ft/blob/fca66fe3ea1ea88ffb36f8264b21ce042d3afd05/Lib/ufo2ft/fontInfoData.py#L48-L55
        set(GlobalMetric::CapHeight, 0.7 * units_per_em as f32);
        let x_height = x_height.unwrap_or(0.5 * units_per_em as f32);
        set(GlobalMetric::XHeight, x_height);

        // https://github.com/googlefonts/ufo2ft/blob/0d2688cd847d003b41104534d16973f72ef26c40/Lib/ufo2ft/fontInfoData.py#L360
        let italic_angle = 0.0;
        set(GlobalMetric::ItalicAngle, italic_angle);

        // https://github.com/googlefonts/ufo2ft/blob/0d2688cd847d003b41104534d16973f72ef26c40/Lib/ufo2ft/fontInfoData.py#L133-L150
        set(GlobalMetric::CaretSlopeRise, 1.0);

        // https://github.com/googlefonts/ufo2ft/blob/0d2688cd847d003b41104534d16973f72ef26c40/Lib/ufo2ft/outlineCompiler.py#L575-L616
        let y_subscript_x_size = units_per_em as f32 * 0.65;
        let y_subscript_y_size = units_per_em as f32 * 0.60;
        let y_subscript_y_offset = units_per_em as f32 * 0.075;
        let y_superscript_y_offset = units_per_em as f32 * 0.35;
        set(GlobalMetric::YSubscriptXSize, y_subscript_x_size);
        set(GlobalMetric::YSubscriptYSize, y_subscript_y_size);
        set(
            GlobalMetric::YSubscriptXOffset,
            adjust_offset(-y_subscript_y_offset, italic_angle),
        );
        set(GlobalMetric::YSubscriptYOffset, y_subscript_y_offset);

        set(GlobalMetric::YSuperscriptXSize, y_subscript_x_size);
        set(GlobalMetric::YSuperscriptYSize, y_subscript_y_size);
        set(
            GlobalMetric::YSuperscriptXOffset,
            adjust_offset(y_superscript_y_offset, italic_angle),
        );
        set(GlobalMetric::YSuperscriptYOffset, y_superscript_y_offset);

        // // https://github.com/googlefonts/ufo2ft/blob/0d2688cd847d003b41104534d16973f72ef26c40/Lib/ufo2ft/fontInfoData.py#L313-L316
        set(GlobalMetric::YStrikeoutSize, 0.05 * units_per_em as f32);
        set(GlobalMetric::YStrikeoutPosition, x_height * 0.6);

        metrics
    }

    fn values(&self, metric: GlobalMetric) -> &HashMap<NormalizedLocation, OrderedFloat<f32>> {
        // We presume that ctor initializes for every GlobalMetric
        self.0.get(&metric).unwrap()
    }

    fn values_mut(
        &mut self,
        metric: GlobalMetric,
    ) -> &mut HashMap<NormalizedLocation, OrderedFloat<f32>> {
        self.0.entry(metric).or_default()
    }

    pub fn get(&self, metric: GlobalMetric, pos: &NormalizedLocation) -> OrderedFloat<f32> {
        let Some(value) = self.values(metric).get(pos) else {
            panic!("interpolated metric values are not yet supported");
        };
        *value
    }

    pub fn set(
        &mut self,
        metric: GlobalMetric,
        pos: NormalizedLocation,
        value: impl Into<OrderedFloat<f32>>,
    ) {
        self.values_mut(metric).insert(pos, value.into());
    }

    pub fn set_if_some(
        self: &mut GlobalMetrics,
        metric: GlobalMetric,
        pos: NormalizedLocation,
        maybe_value: Option<impl Into<OrderedFloat<f64>>>,
    ) {
        if let Some(value) = maybe_value {
            self.set(metric, pos, value.into().into_inner() as f32);
        }
    }

    pub fn at(&self, pos: &NormalizedLocation) -> GlobalMetricsInstance {
        GlobalMetricsInstance {
            pos: pos.clone(),
            ascender: self.get(GlobalMetric::Ascender, pos),
            descender: self.get(GlobalMetric::Descender, pos),
            caret_slope_rise: self.get(GlobalMetric::CaretSlopeRise, pos),
            cap_height: self.get(GlobalMetric::CapHeight, pos),
            x_height: self.get(GlobalMetric::XHeight, pos),
            italic_angle: self.get(GlobalMetric::ItalicAngle, pos),
            y_subscript_x_size: self.get(GlobalMetric::YSubscriptXSize, pos),
            y_subscript_y_size: self.get(GlobalMetric::YSubscriptYSize, pos),
            y_subscript_x_offset: self.get(GlobalMetric::YSubscriptXOffset, pos),
            y_subscript_y_offset: self.get(GlobalMetric::YSubscriptYOffset, pos),
            y_superscript_x_size: self.get(GlobalMetric::YSuperscriptXSize, pos),
            y_superscript_y_size: self.get(GlobalMetric::YSuperscriptYSize, pos),
            y_superscript_x_offset: self.get(GlobalMetric::YSuperscriptXOffset, pos),
            y_superscript_y_offset: self.get(GlobalMetric::YSuperscriptYOffset, pos),
            y_strikeout_size: self.get(GlobalMetric::YStrikeoutSize, pos),
            y_strikeout_position: self.get(GlobalMetric::YStrikeoutPosition, pos),
            os2_typo_ascender: self.get(GlobalMetric::Os2TypoAscender, pos),
            os2_typo_descender: self.get(GlobalMetric::Os2TypoDescender, pos),
            os2_typo_line_gap: self.get(GlobalMetric::Os2TypoLineGap, pos),
            os2_win_ascent: self.get(GlobalMetric::Os2WinAscent, pos),
            os2_win_descent: self.get(GlobalMetric::Os2WinDescent, pos),
            hhea_ascender: self.get(GlobalMetric::HheaAscender, pos),
            hhea_descender: self.get(GlobalMetric::HheaDescender, pos),
            hhea_line_gap: self.get(GlobalMetric::HheaLineGap, pos),
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = (NormalizedLocation, GlobalMetric, f32)> + '_ {
        self.0
            .iter()
            .flat_map(|(m, values)| values.iter().map(|(l, v)| (l.clone(), *m, v.into_inner())))
    }
}

/// Metrics at a specific [NormalizedLocation]
#[derive(Default, Debug, Clone, PartialEq, Eq)]
pub struct GlobalMetricsInstance {
    pub pos: NormalizedLocation,
    pub ascender: OrderedFloat<f32>,
    pub descender: OrderedFloat<f32>,
    pub caret_slope_rise: OrderedFloat<f32>,
    pub cap_height: OrderedFloat<f32>,
    pub x_height: OrderedFloat<f32>,
    pub italic_angle: OrderedFloat<f32>,
    pub y_subscript_x_size: OrderedFloat<f32>,
    pub y_subscript_y_size: OrderedFloat<f32>,
    pub y_subscript_x_offset: OrderedFloat<f32>,
    pub y_subscript_y_offset: OrderedFloat<f32>,
    pub y_superscript_x_size: OrderedFloat<f32>,
    pub y_superscript_y_size: OrderedFloat<f32>,
    pub y_superscript_x_offset: OrderedFloat<f32>,
    pub y_superscript_y_offset: OrderedFloat<f32>,
    pub y_strikeout_size: OrderedFloat<f32>,
    pub y_strikeout_position: OrderedFloat<f32>,
    pub os2_typo_ascender: OrderedFloat<f32>,
    pub os2_typo_descender: OrderedFloat<f32>,
    pub os2_typo_line_gap: OrderedFloat<f32>,
    pub os2_win_ascent: OrderedFloat<f32>,
    pub os2_win_descent: OrderedFloat<f32>,
    pub hhea_ascender: OrderedFloat<f32>,
    pub hhea_descender: OrderedFloat<f32>,
    pub hhea_line_gap: OrderedFloat<f32>,
}

/// Helps accumulate 'name' values.
///
/// See <https://github.com/googlefonts/ufo2ft/blob/fca66fe3ea1ea88ffb36f8264b21ce042d3afd05/Lib/ufo2ft/outlineCompiler.py#L367>.
pub struct NameBuilder {
    names: HashMap<NameKey, String>,
    /// Helps lookup entries in name when all we have is a NameId
    name_to_key: HashMap<NameId, NameKey>,
    version_major: i32,
    version_minor: u32,
}

impl Default for NameBuilder {
    fn default() -> Self {
        let mut builder = Self {
            names: Default::default(),
            name_to_key: Default::default(),
            version_major: Default::default(),
            version_minor: Default::default(),
        };
        // Based on diffing fontmake Oswald build
        builder.add(NameId::SUBFAMILY_NAME, "Regular".to_string());
        builder
    }
}

impl NameBuilder {
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
        let value = if let Some(value) = value {
            Some(value.clone())
        } else {
            default_value(name_id).map(String::from)
        };
        if let Some(value) = value {
            self.add(name_id, value);
        }
    }

    pub fn apply_fallback(&mut self, name_id: NameId, fallbacks: &[NameId]) {
        if let Some(fallback_id) = fallbacks.iter().find(|n| {
            let Some(key) = self.name_to_key.get(*n) else {
                return false;
            };
            self.names.contains_key(key)
        }) {
            self.add(name_id, self.names[&self.name_to_key[fallback_id]].clone());
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

    pub fn apply_default_fallbacks(&mut self, vendor_id: &str) {
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
                format!(
                    "{} {}",
                    self.get(NameId::TYPOGRAPHIC_FAMILY_NAME)
                        .unwrap_or_default(),
                    self.get(NameId::TYPOGRAPHIC_SUBFAMILY_NAME)
                        .unwrap_or_default(),
                ),
            );
        }

        // https://github.com/googlefonts/ufo2ft/blob/fca66fe3ea1ea88ffb36f8264b21ce042d3afd05/Lib/ufo2ft/fontInfoData.py#L178-L185
        if !self.contains_key(NameId::POSTSCRIPT_NAME) {
            let mut value = format!(
                "{}-{}",
                self.get(NameId::TYPOGRAPHIC_FAMILY_NAME)
                    .unwrap_or_default(),
                self.get(NameId::TYPOGRAPHIC_SUBFAMILY_NAME)
                    .unwrap_or_default(),
            );
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
        if let Some(family) = self.get(NameId::FAMILY_NAME) {
            if Some(family) == self.get(NameId::TYPOGRAPHIC_FAMILY_NAME) {
                self.remove(NameId::TYPOGRAPHIC_FAMILY_NAME);
            }
        }
        if let Some(subfamily) = self.get(NameId::SUBFAMILY_NAME) {
            if Some(subfamily) == self.get(NameId::TYPOGRAPHIC_SUBFAMILY_NAME) {
                self.remove(NameId::TYPOGRAPHIC_SUBFAMILY_NAME);
            }
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

/// The encoding for a Windows-platform (which works everywhere) name.
///
/// See <https://learn.microsoft.com/en-us/typography/opentype/spec/name#platform-specific-encoding-and-language-ids-windows-platform-platform-id-3>
fn encoding_for(value: &str) -> u16 {
    if value.chars().all(|c| (c as u32) < 0xFFFF) {
        1 // Unicode BMP
    } else {
        10 // Unicode full repetoire
    }
}

/// See <https://learn.microsoft.com/en-us/typography/opentype/spec/name>
#[derive(Serialize, Deserialize, Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct NameKey {
    pub name_id: NameId,
    pub platform_id: u16,
    pub encoding_id: u16,
    pub lang_id: u16,
}

impl NameKey {
    /// Create's a [NameKey] suitable for use with the provided value.
    ///
    /// The value matters because if it uses values from outside the Unicode BMP
    /// the key changes.
    pub fn new(name_id: NameId, value: &str) -> NameKey {
        // The spec offers a Unicode platform but fontmake uses Windows because that's more widely supported.
        // Match that. <https://github.com/googlefonts/ufo2ft/blob/fca66fe3ea1ea88ffb36f8264b21ce042d3afd05/Lib/ufo2ft/outlineCompiler.py#L430-L432>.
        NameKey {
            platform_id: 3, // Windows
            encoding_id: encoding_for(value),
            // https://learn.microsoft.com/en-us/typography/opentype/spec/name#windows-language-ids
            lang_id: 0x409, // English, United States.
            name_id,
        }
    }

    pub fn new_bmp_only(name_id: NameId) -> NameKey {
        Self::new(name_id, "")
    }
}

/// IR for a named position in variation space
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct NamedInstance {
    pub name: String,
    pub location: UserLocation,
}

/// Features (Adobe fea).
///
/// In time will split gpos/gsub, have different features for different
/// locations, etc.
#[derive(Serialize, Deserialize, Clone, Debug, PartialEq, Eq)]
pub enum Features {
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

impl Features {
    pub fn empty() -> Features {
        Features::Empty
    }
    pub fn from_file(fea_file: PathBuf, include_dir: Option<PathBuf>) -> Features {
        Features::File {
            fea_file,
            include_dir,
        }
    }
    pub fn from_string(fea_content: String) -> Features {
        Features::Memory {
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
    pub fn new(glyph_name: GlyphName, anchors: Vec<Anchor>) -> Result<Self, WorkError> {
        Ok(GlyphAnchors {
            glyph_name,
            anchors,
        })
    }
}

/// A variable definition of an anchor.
///
/// Must have at least one definition, at the default location.
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct Anchor {
    pub name: AnchorName,
    pub positions: HashMap<NormalizedLocation, Point>,
}

#[derive(Debug, Clone)]
pub struct AnchorBuilder {
    glyph_name: GlyphName,
    anchors: HashMap<AnchorName, HashMap<NormalizedLocation, Point>>,
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
        name: AnchorName,
        loc: NormalizedLocation,
        pos: Point,
    ) -> Result<(), WorkError> {
        if self
            .anchors
            .entry(name.clone())
            .or_default()
            .insert(loc.clone(), pos)
            .is_some()
        {
            return Err(WorkError::AmbiguousAnchor {
                glyph: self.glyph_name.clone(),
                anchor: name,
                loc,
            });
        }
        Ok(())
    }
}

impl TryInto<GlyphAnchors> for AnchorBuilder {
    type Error = WorkError;

    fn try_into(self) -> Result<GlyphAnchors, Self::Error> {
        // It would be nice if everyone was defined at default
        for (anchor, positions) in self.anchors.iter() {
            if !positions.keys().any(|loc| !loc.has_any_non_zero()) {
                return Err(WorkError::NoDefaultForAnchor {
                    glyph: self.glyph_name.clone(),
                    anchor: anchor.clone(),
                });
            }
        }
        GlyphAnchors::new(
            self.glyph_name,
            self.anchors
                .into_iter()
                .map(|(name, positions)| Anchor { name, positions })
                .collect(),
        )
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
#[serde(from = "GlyphSerdeRepr", into = "GlyphSerdeRepr")]
pub struct Glyph {
    pub name: GlyphName,
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
        codepoints: HashSet<u32>,
        sources: HashMap<NormalizedLocation, GlyphInstance>,
    ) -> Result<Self, WorkError> {
        if sources.is_empty() {
            return Err(WorkError::InvalidSourceGlyph {
                glyph_name: name,
                message: "No sources".into(),
            });
        }
        let defaults: Vec<_> = sources
            .keys()
            .filter(|loc| !loc.iter().any(|(_, c)| c.into_inner() != 0.0))
            .collect();
        if defaults.len() != 1 {
            return Err(WorkError::InvalidSourceGlyph {
                glyph_name: name,
                message: format!("Must have exactly 1 default, got {defaults:?} from {sources:?}"),
            });
        }
        let default_location = defaults[0].clone();
        let has_consistent_2x2_transforms = has_consistent_2x2_transforms(&name, &sources);
        Ok(Glyph {
            name,
            codepoints,
            default_location,
            sources,
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

impl Persistable for StaticMetadata {
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

impl Persistable for Features {
    fn read(from: &mut dyn Read) -> Self {
        serde_yaml::from_reader(from).unwrap()
    }

    fn write(&self, to: &mut dyn std::io::Write) {
        serde_yaml::to_writer(to, self).unwrap();
    }
}

impl Persistable for Kerning {
    fn read(from: &mut dyn Read) -> Self {
        serde_yaml::from_reader(from).unwrap()
    }

    fn write(&self, to: &mut dyn std::io::Write) {
        serde_yaml::to_writer(to, self).unwrap();
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

/// A variable definition of a single glyph.
///
/// If defined in many locations, presumed to vary continuously
/// between positions and required to have variation compatible structure.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct GlyphBuilder {
    pub name: GlyphName,
    pub codepoints: HashSet<u32>, // single unicodes that each point to this glyph. Typically 0 or 1.
    pub sources: HashMap<NormalizedLocation, GlyphInstance>,
}

impl GlyphBuilder {
    pub fn new(name: GlyphName) -> Self {
        Self {
            name,
            codepoints: HashSet::new(),
            sources: HashMap::new(),
        }
    }

    pub fn try_add_source(
        &mut self,
        unique_location: &NormalizedLocation,
        source: GlyphInstance,
    ) -> Result<(), WorkError> {
        if self.sources.contains_key(unique_location) {
            return Err(WorkError::DuplicateNormalizedLocation {
                what: format!("glyph '{}' source", self.name.as_str()),
                loc: unique_location.clone(),
            });
        }
        self.sources.insert(unique_location.clone(), source);
        Ok(())
    }

    /// * see <https://github.com/googlefonts/ufo2ft/blob/b3895a96ca910c1764df016bfee4719448cfec4a/Lib/ufo2ft/outlineCompiler.py#L1666-L1694>
    pub fn new_notdef(
        default_location: NormalizedLocation,
        upem: u16,
        ascender: f32,
        descender: f32,
    ) -> Self {
        let upem = upem as f32;
        let width: u16 = (upem * 0.5).ot_round();
        let width = width as f64;
        let stroke: u16 = (upem * 0.05).ot_round();
        let stroke = stroke as f64;

        let mut path = BezPath::new();

        // outer box
        let x_min = stroke;
        let x_max = width - stroke;
        let y_max = ascender as f64;
        let y_min = descender as f64;
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

        Self {
            name: GlyphName::NOTDEF.clone(),
            codepoints: HashSet::new(),
            sources: HashMap::from([(
                default_location,
                GlyphInstance {
                    width,
                    contours: vec![path],
                    ..Default::default()
                },
            )]),
        }
    }
}

impl TryInto<Glyph> for GlyphBuilder {
    type Error = WorkError;

    fn try_into(self) -> Result<Glyph, Self::Error> {
        Glyph::new(self.name, self.codepoints, self.sources)
    }
}

impl From<Glyph> for GlyphBuilder {
    fn from(value: Glyph) -> Self {
        Self {
            name: value.name,
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
    /// Advance height; if None, assumed to equal font's ascender - descender.
    pub height: Option<f64>,
    /// List of glyph contours.
    pub contours: Vec<BezPath>,
    /// List of glyph components.
    pub components: Vec<Component>,
}

/// A single glyph component, reference to another glyph.
#[derive(Serialize, Deserialize, Debug, Clone, PartialEq)]
pub struct Component {
    /// The name of the referenced glyph.
    pub base: GlyphName,
    /// Affine transformation to apply to the referenced glyph.
    pub transform: Affine,
}

#[derive(Debug, Copy, Clone, PartialEq)]
enum OnCurve {
    Move(Point),
    Line(Point),
    Quad(Point),
    Cubic(Point),
}

impl OnCurve {
    fn point(&self) -> &Point {
        match self {
            OnCurve::Move(p) => p,
            OnCurve::Line(p) => p,
            OnCurve::Quad(p) => p,
            OnCurve::Cubic(p) => p,
        }
    }

    fn is_move(&self) -> bool {
        matches!(self, OnCurve::Move(_))
    }
}

/// Helps convert points-of-type to a bezier path.
///
/// Source formats tend to use streams of point-of-type. Curve manipulation is
/// often easier on bezier path, so provide a mechanism to convert.
/// While kurbo::BezPath can contain multiple subpaths, and this builder could be
/// used to convert multiple contours (i.e. list of points) into a single BezPath,
/// our GlyphInstance.contours is defined as a `Vec<BezPath>`, so frontends should
/// convert one contour at a time.
#[derive(Debug)]
pub struct GlyphPathBuilder {
    glyph_name: GlyphName,
    offcurve: Vec<Point>,
    leading_offcurve: Vec<Point>,
    path: Vec<PathEl>,
    first_oncurve: Option<OnCurve>,
}

impl GlyphPathBuilder {
    pub fn new(glyph_name: GlyphName, estimated_num_elements: usize) -> GlyphPathBuilder {
        let mut capacity = estimated_num_elements.next_power_of_two();
        if capacity == estimated_num_elements {
            capacity += 4; // close path often adds a few
        }
        GlyphPathBuilder {
            glyph_name,
            offcurve: Vec::with_capacity(2),
            leading_offcurve: Vec::new(),
            path: Vec::with_capacity(capacity),
            first_oncurve: None,
        }
    }

    fn check_num_offcurve(
        &self,
        expected: impl Fn(usize) -> bool,
    ) -> Result<(), PathConversionError> {
        if !expected(self.offcurve.len()) {
            return Err(PathConversionError::TooManyOffcurvePoints {
                glyph_name: self.glyph_name.clone(),
                num_offcurve: self.offcurve.len(),
                points: self.offcurve.clone(),
            });
        }
        Ok(())
    }

    fn is_empty(&self) -> bool {
        self.first_oncurve.is_none() && self.leading_offcurve.is_empty()
    }

    fn begin_path(&mut self, oncurve: OnCurve) -> Result<(), PathConversionError> {
        assert!(self.first_oncurve.is_none());
        self.path.push(PathEl::MoveTo(*oncurve.point()));
        self.first_oncurve = Some(oncurve);
        Ok(())
    }

    /// Lifts the "pen" to Point `p` and marks the beginning of an open contour.
    ///
    /// A point of this type can only be the first point in a contour.
    /// Cf. "move" in <https://unifiedfontobject.org/versions/ufo3/glyphs/glif/#point-types>
    pub fn move_to(&mut self, p: impl Into<Point>) -> Result<(), PathConversionError> {
        if !self.is_empty() {
            return Err(PathConversionError::MoveAfterFirstPoint {
                glyph_name: self.glyph_name.clone(),
                point: p.into(),
            });
        }
        self.begin_path(OnCurve::Move(p.into()))
    }

    /// Draws a line from the previous point to Point `p`.
    ///
    /// The previous point cannot be an off-curve point.
    /// If this is the first point in a contour, the contour is assumed to be closed,
    /// i.e. a cyclic list of points with no predominant start point.
    /// Cf. "line" in <https://unifiedfontobject.org/versions/ufo3/glyphs/glif/#point-types>
    pub fn line_to(&mut self, p: impl Into<Point>) -> Result<(), PathConversionError> {
        self.check_num_offcurve(|v| v == 0)?;
        if self.first_oncurve.is_none() {
            self.begin_path(OnCurve::Line(p.into()))?;
        } else {
            self.path.push(PathEl::LineTo(p.into()));
        }
        Ok(())
    }

    /// Draws a quadratic curve/spline from the last non-offcurve point to Point `p`.
    ///
    /// This uses the TrueType "implied on-curve point" principle.
    /// The number of preceding off-curve points can be n >= 0. When n=0, a straight line is
    /// implied. If n=1, a single quadratic Bezier curve is drawn. If n>=2, a sequence of
    /// quadratic Bezier curves is drawn, with the implied on-curve points at the midpoints
    /// between pairs of successive off-curve points.
    /// If this is the first point in a contour, the contour is assumed to be closed.
    /// Cf. "qcurve" in <https://unifiedfontobject.org/versions/ufo3/glyphs/glif/#point-types>
    pub fn qcurve_to(&mut self, p: impl Into<Point>) -> Result<(), PathConversionError> {
        // https://github.com/googlefonts/fontmake-rs/issues/110
        // Guard clauses: degenerate cases
        if self.first_oncurve.is_none() {
            return self.begin_path(OnCurve::Quad(p.into()));
        }
        if self.offcurve.is_empty() {
            return self.line_to(p);
        }

        // Insert an implied oncurve between every pair of offcurve points
        for window in self.offcurve.windows(2) {
            let curr = window[0];
            let next = window[1];
            // current offcurve to halfway to the next one
            let implied = Point::new((curr.x + next.x) / 2.0, (curr.y + next.y) / 2.0);
            self.path.push(PathEl::QuadTo(curr, implied));
        }
        // last but not least, the last offcurve to the provided point
        self.path
            .push(PathEl::QuadTo(*self.offcurve.last().unwrap(), p.into()));
        self.offcurve.clear();

        Ok(())
    }

    /// Draws a cubic curve from the previous non-offcurve point to Point `p`.
    ///
    /// Type of curve depends on the number of accumulated off-curves: 0 (straight line),
    /// 1 (quadratic Bezier) or 2 (cubic Bezier).
    /// If this is the first point in a contour, the contour is assumed to be closed.
    /// Cf. "curve" in <https://unifiedfontobject.org/versions/ufo3/glyphs/glif/#point-types>
    pub fn curve_to(&mut self, p: impl Into<Point>) -> Result<(), PathConversionError> {
        if self.first_oncurve.is_some() {
            match self.offcurve.len() {
                0 => self.path.push(PathEl::LineTo(p.into())),
                1 => self.path.push(PathEl::QuadTo(self.offcurve[0], p.into())),
                2 => self.path.push(PathEl::CurveTo(
                    self.offcurve[0],
                    self.offcurve[1],
                    p.into(),
                )),
                _ => self.check_num_offcurve(|v| v < 3)?,
            }
            self.offcurve.clear();
        } else {
            self.begin_path(OnCurve::Cubic(p.into()))?;
        }
        Ok(())
    }

    /// Append off-curve point `p` to the following curve segment.
    ///
    /// The type of curve is defined by following on-curve point, which can be either a
    /// (cubic) "curve" or (quadratic) "qcurve".
    /// If offcurve is the first point in a contour, the contour is assumed to be closed.
    /// Cf. "offcurve" in <https://unifiedfontobject.org/versions/ufo3/glyphs/glif/#point-types>
    pub fn offcurve(&mut self, p: impl Into<Point>) -> Result<(), PathConversionError> {
        if self.first_oncurve.is_some() {
            self.offcurve.push(p.into());
        } else {
            self.leading_offcurve.push(p.into());
        }
        Ok(())
    }

    /// Ends the current sub-path.
    ///
    /// It's called automatically by `build()` thus can be
    /// omitted when building one BezPath per contour, but can be called manually in
    /// order to build multiple contours into a single BezPath.
    pub fn end_path(&mut self) -> Result<(), PathConversionError> {
        // a contour that does *not* start with a move is assumed to be closed
        // https://unifiedfontobject.org/versions/ufo3/glyphs/glif/#point-types
        if !self.first_oncurve.is_some_and(|on| on.is_move()) {
            self.close_path()?;
        }

        self.check_num_offcurve(|v| v == 0)?;
        self.first_oncurve = None;
        Ok(())
    }

    fn close_path(&mut self) -> Result<(), PathConversionError> {
        // Flush any leading off-curves to the end. This matches fontTools' PointToSegmentPen
        // always starting/ending a closed contour on the first on-curve point:
        // https://github.com/fonttools/fonttools/blob/57fb47/Lib/fontTools/pens/pointPen.py#L147-L155
        if !self.leading_offcurve.is_empty() {
            self.offcurve.append(&mut self.leading_offcurve);
        }
        // Take dangling off-curves to imply a curve back to sub-path start.
        // For closed paths we explicitly output the implied closing line
        // equivalent to fontTools' PointToSegmentPen(outputImpliedClosingLine=True)
        if let Some(first_oncurve) = self.first_oncurve {
            match first_oncurve {
                OnCurve::Line(pt) => self.line_to(pt)?,
                OnCurve::Quad(pt) => self.qcurve_to(pt)?,
                OnCurve::Cubic(pt) => self.curve_to(pt)?,
                _ => unreachable!(),
            }
            self.path.push(PathEl::ClosePath);
        } else if !self.offcurve.is_empty() {
            // special TrueType oncurve-less quadratic contour, we assume the path
            // starts at midpoint between the first and last offcurves
            let first_offcurve = self.offcurve[0];
            let last_offcurve = *self.offcurve.last().unwrap();
            let implied_oncurve = first_offcurve.midpoint(last_offcurve);
            self.begin_path(OnCurve::Quad(implied_oncurve))?;
            self.close_path()?;
        }
        Ok(())
    }

    /// Builds the kurbo::BezPath from the accumulated points.
    pub fn build(mut self) -> Result<BezPath, PathConversionError> {
        self.end_path()?;
        Ok(BezPath::from_vec(self.path))
    }
}

#[cfg(test)]
mod tests {

    use std::{
        collections::{HashMap, HashSet},
        fmt::Debug,
    };

    use serde::{Deserialize, Serialize};

    use font_types::{NameId, Tag};
    use fontdrasil::coords::{CoordConverter, NormalizedCoord, UserCoord};
    use write_fonts::tables::os2::SelectionFlags;

    use crate::{error::PathConversionError, ir::Axis, variations::VariationModel};

    use pretty_assertions::assert_eq;

    use super::{GlyphPathBuilder, MiscMetadata, NameKey, NamedInstance, StaticMetadata};

    const WGHT: Tag = Tag::from_be_bytes(*b"wght");

    fn test_axis() -> Axis {
        let min = UserCoord::new(100.0);
        let default = UserCoord::new(400.0);
        let max = UserCoord::new(900.0);
        let converter = CoordConverter::unmapped(min, default, max);
        Axis {
            name: String::from("Weight"),
            tag: WGHT,
            min,
            default,
            max,
            hidden: false,
            converter,
        }
    }

    fn test_static_metadata() -> StaticMetadata {
        let axis = test_axis();
        let mut point_axis = axis.clone();
        point_axis.min = point_axis.default;
        point_axis.max = point_axis.default;

        StaticMetadata {
            units_per_em: 1000,
            all_source_axes: vec![axis.clone(), point_axis],
            axes: vec![axis.clone()],
            named_instances: vec![NamedInstance {
                name: "Nobody".to_string(),
                location: vec![(WGHT, UserCoord::new(100.0))].into(),
            }],
            variation_model: VariationModel::new(
                HashSet::from([
                    vec![(WGHT, NormalizedCoord::new(-1.0))].into(),
                    vec![(WGHT, NormalizedCoord::new(0.0))].into(),
                    vec![(WGHT, NormalizedCoord::new(1.0))].into(),
                ]),
                vec![axis.clone()],
            )
            .unwrap(),
            default_location: vec![(WGHT, NormalizedCoord::new(0.0))].into(),
            names: HashMap::from([
                (
                    NameKey::new_bmp_only(NameId::FAMILY_NAME),
                    "Fam".to_string(),
                ),
                (
                    NameKey::new_bmp_only(NameId::new(256)),
                    "Weight".to_string(),
                ),
                (
                    NameKey::new_bmp_only(NameId::new(257)),
                    "Nobody".to_string(),
                ),
            ]),
            postscript_names: HashMap::from([("lhs".into(), "rhs".into())]),
            misc: MiscMetadata {
                selection_flags: SelectionFlags::default(),
                vendor_id: Tag::from_be_bytes(*b"DUCK"),
                underline_thickness: 0.15.into(),
                underline_position: 16.0.into(),
                version_major: 42,
                version_minor: 24,
                head_flags: 42,
                lowest_rec_ppm: 42,
                created: None,
            },
        }
    }

    fn assert_yml_round_trip<T>(thing: T)
    where
        for<'a> T: Serialize + Deserialize<'a> + PartialEq + Debug,
    {
        let yml = serde_yaml::to_string(&thing).unwrap();
        assert_eq!(thing, serde_yaml::from_str(&yml).unwrap());
    }

    fn assert_bincode_round_trip<T>(thing: T)
    where
        for<'a> T: Serialize + Deserialize<'a> + PartialEq + Debug,
    {
        let bin = bincode::serialize(&thing).unwrap();
        assert_eq!(thing, bincode::deserialize(&bin).unwrap());
    }

    #[test]
    fn axis_yaml() {
        assert_yml_round_trip(test_axis());
    }

    #[test]
    fn axis_bincode() {
        assert_bincode_round_trip(test_axis());
    }

    #[test]
    fn static_metadata_yaml() {
        assert_yml_round_trip(test_static_metadata());
    }

    #[test]
    fn static_metadata_bincode() {
        assert_bincode_round_trip(test_static_metadata());
    }

    #[test]
    fn a_qcurve_with_no_offcurve_is_a_line_open_contour() {
        let mut builder = GlyphPathBuilder::new("test".into(), 0);
        builder.move_to((2.0, 2.0)).unwrap(); // open contour
        builder.qcurve_to((4.0, 2.0)).unwrap();
        assert_eq!("M2,2 L4,2", builder.build().unwrap().to_svg());
    }

    #[test]
    fn a_qcurve_with_no_offcurve_is_a_line_closed_contour() {
        let mut builder = GlyphPathBuilder::new("test".into(), 0);
        builder.qcurve_to((2.0, 2.0)).unwrap(); // closed, ie not starting with 'move'
        builder.qcurve_to((4.0, 2.0)).unwrap();
        assert_eq!("M2,2 L4,2 L2,2 Z", builder.build().unwrap().to_svg());
    }

    #[test]
    fn a_curve_with_no_offcurve_is_a_line_open_contour() {
        let mut builder = GlyphPathBuilder::new("test".into(), 0);
        builder.move_to((2.0, 2.0)).unwrap(); // open contour
        builder.curve_to((4.0, 2.0)).unwrap();
        assert_eq!("M2,2 L4,2", builder.build().unwrap().to_svg());
    }

    #[test]
    fn a_curve_with_no_offcurve_is_a_line_closed_contour() {
        let mut builder = GlyphPathBuilder::new("test".into(), 0);
        builder.curve_to((2.0, 2.0)).unwrap(); // closed
        builder.curve_to((4.0, 2.0)).unwrap();
        assert_eq!("M2,2 L4,2 L2,2 Z", builder.build().unwrap().to_svg());
    }

    #[test]
    fn a_curve_with_one_offcurve_is_a_single_quad_open_contour() {
        let mut builder = GlyphPathBuilder::new("test".into(), 0);
        builder.move_to((2.0, 2.0)).unwrap(); // open
        builder.offcurve((3.0, 0.0)).unwrap();
        builder.curve_to((4.0, 2.0)).unwrap();
        assert_eq!("M2,2 Q3,0 4,2", builder.build().unwrap().to_svg());
    }

    #[test]
    fn a_curve_with_one_offcurve_is_a_single_quad_closed_contour() {
        let mut builder = GlyphPathBuilder::new("test".into(), 0);
        builder.curve_to((2.0, 2.0)).unwrap(); // closed
        builder.offcurve((3.0, 0.0)).unwrap();
        builder.curve_to((4.0, 2.0)).unwrap();
        assert_eq!("M2,2 Q3,0 4,2 L2,2 Z", builder.build().unwrap().to_svg());
    }

    #[test]
    fn a_qcurve_with_one_offcurve_is_a_single_quad_to_open_contour() {
        let mut builder = GlyphPathBuilder::new("test".into(), 0);
        builder.move_to((2.0, 2.0)).unwrap();
        builder.offcurve((3.0, 0.0)).unwrap();
        builder.qcurve_to((4.0, 2.0)).unwrap();
        assert_eq!("M2,2 Q3,0 4,2", builder.build().unwrap().to_svg());
    }

    #[test]
    fn a_qcurve_with_one_offcurve_is_a_single_quad_to_closed_contour() {
        let mut builder = GlyphPathBuilder::new("test".into(), 0);
        builder.qcurve_to((2.0, 2.0)).unwrap(); // closed
        builder.offcurve((3.0, 0.0)).unwrap();
        builder.qcurve_to((4.0, 2.0)).unwrap();
        assert_eq!("M2,2 Q3,0 4,2 L2,2 Z", builder.build().unwrap().to_svg());
    }

    #[test]
    fn a_qcurve_with_two_offcurve_is_two_quad_to_open_contour() {
        let mut builder = GlyphPathBuilder::new("test".into(), 0);
        builder.move_to((2.0, 2.0)).unwrap();
        builder.offcurve((3.0, 0.0)).unwrap();
        builder.offcurve((5.0, 4.0)).unwrap();
        builder.qcurve_to((6.0, 2.0)).unwrap();
        assert_eq!("M2,2 Q3,0 4,2 Q5,4 6,2", builder.build().unwrap().to_svg());
    }

    #[test]
    fn a_qcurve_with_two_offcurve_is_two_quad_to_closed_contour() {
        let mut builder = GlyphPathBuilder::new("test".into(), 0);
        builder.qcurve_to((2.0, 2.0)).unwrap(); // closed
        builder.offcurve((3.0, 0.0)).unwrap();
        builder.offcurve((5.0, 4.0)).unwrap();
        builder.qcurve_to((6.0, 2.0)).unwrap();
        assert_eq!(
            "M2,2 Q3,0 4,2 Q5,4 6,2 L2,2 Z",
            builder.build().unwrap().to_svg()
        );
    }

    #[test]
    fn last_line_always_emits_implied_closing_line() {
        let mut builder = GlyphPathBuilder::new("test".into(), 0);
        builder.line_to((2.0, 2.0)).unwrap();
        builder.line_to((4.0, 2.0)).unwrap();
        // a closing line is implied by Z, but emit it nonetheless
        assert_eq!("M2,2 L4,2 L2,2 Z", builder.build().unwrap().to_svg());
    }

    #[test]
    fn last_line_emits_nop_implied_closing_line() {
        let mut builder = GlyphPathBuilder::new("test".into(), 0);
        builder.line_to((2.0, 2.0)).unwrap();
        builder.line_to((4.0, 2.0)).unwrap();
        // duplicate last point, not to be confused with the closing line implied by Z
        builder.line_to((2.0, 2.0)).unwrap();
        assert_eq!("M2,2 L4,2 L2,2 L2,2 Z", builder.build().unwrap().to_svg());
    }

    #[test]
    fn last_quad_equals_move_no_closing_line() {
        // if last curve point is equal to move, there's no need to disambiguate it from
        // the implicit closing line, so we don't emit one
        let mut builder = GlyphPathBuilder::new("test".into(), 0);
        builder.offcurve((3.0, 0.0)).unwrap();
        builder.qcurve_to((2.0, 2.0)).unwrap();
        assert_eq!("M2,2 Q3,0 2,2 Z", builder.build().unwrap().to_svg());
    }

    #[test]
    fn last_cubic_equals_move_no_closing_line() {
        let mut builder = GlyphPathBuilder::new("test".into(), 0);
        builder.offcurve((3.0, 0.0)).unwrap();
        builder.offcurve((0.0, 3.0)).unwrap();
        builder.curve_to((2.0, 2.0)).unwrap();
        assert_eq!("M2,2 C3,0 0,3 2,2 Z", builder.build().unwrap().to_svg());
    }

    #[test]
    fn last_quad_not_equal_move_do_emit_closing_line() {
        // if last point is different from move, then emit the implied closing line
        let mut builder = GlyphPathBuilder::new("test".into(), 0);
        builder.line_to((2.0, 2.0)).unwrap();
        builder.offcurve((3.0, 0.0)).unwrap();
        builder.qcurve_to((4.0, 2.0)).unwrap();
        assert_eq!("M2,2 Q3,0 4,2 L2,2 Z", builder.build().unwrap().to_svg());
    }

    #[test]
    fn last_cubic_not_equal_move_do_emit_closing_line() {
        let mut builder = GlyphPathBuilder::new("test".into(), 0);
        builder.line_to((2.0, 2.0)).unwrap();
        builder.offcurve((3.0, 0.0)).unwrap();
        builder.offcurve((0.0, 3.0)).unwrap();
        builder.curve_to((4.0, 2.0)).unwrap();
        assert_eq!(
            "M2,2 C3,0 0,3 4,2 L2,2 Z",
            builder.build().unwrap().to_svg()
        );
    }

    #[test]
    fn start_on_first_oncurve_irrespective_of_offcurves() {
        // the following three closed contours are all equivalent and get normalized
        // to the same path, which begins/ends on the first on-curve point i.e. (2,2).
        let expected = "M2,2 C6,0 0,6 4,2 C3,0 0,3 2,2 Z";

        let mut builder = GlyphPathBuilder::new("test".into(), 0);
        builder.offcurve((3.0, 0.0)).unwrap();
        builder.offcurve((0.0, 3.0)).unwrap();
        builder.curve_to((2.0, 2.0)).unwrap();
        builder.offcurve((6.0, 0.0)).unwrap();
        builder.offcurve((0.0, 6.0)).unwrap();
        builder.curve_to((4.0, 2.0)).unwrap();
        assert_eq!(expected, builder.build().unwrap().to_svg());

        let mut builder = GlyphPathBuilder::new("test".into(), 0);
        builder.offcurve((0.0, 3.0)).unwrap();
        builder.curve_to((2.0, 2.0)).unwrap();
        builder.offcurve((6.0, 0.0)).unwrap();
        builder.offcurve((0.0, 6.0)).unwrap();
        builder.curve_to((4.0, 2.0)).unwrap();
        builder.offcurve((3.0, 0.0)).unwrap();
        assert_eq!(expected, builder.build().unwrap().to_svg());

        let mut builder = GlyphPathBuilder::new("test".into(), 0);
        builder.curve_to((2.0, 2.0)).unwrap();
        builder.offcurve((6.0, 0.0)).unwrap();
        builder.offcurve((0.0, 6.0)).unwrap();
        builder.curve_to((4.0, 2.0)).unwrap();
        builder.offcurve((3.0, 0.0)).unwrap();
        builder.offcurve((0.0, 3.0)).unwrap();
        assert_eq!(expected, builder.build().unwrap().to_svg());
    }

    #[test]
    fn closed_quadratic_contour_without_oncurve_points() {
        let mut builder = GlyphPathBuilder::new("test".into(), 0);
        // builder.qcurve_to((0.0, 1.0)).unwrap();  // implied
        builder.offcurve((1.0, 1.0)).unwrap();
        builder.offcurve((1.0, -1.0)).unwrap();
        builder.offcurve((-1.0, -1.0)).unwrap();
        builder.offcurve((-1.0, 1.0)).unwrap();
        assert_eq!(
            "M0,1 Q1,1 1,0 Q1,-1 0,-1 Q-1,-1 -1,0 Q-1,1 0,1 Z",
            builder.build().unwrap().to_svg()
        );
    }

    #[test]
    fn invalid_move_after_first_point() {
        // A point of type 'move' must be the first point in an (open) contour.
        let mut builder = GlyphPathBuilder::new("test".into(), 0);
        builder.move_to((2.0, 2.0)).unwrap();
        builder.end_path().unwrap();
        // move_to after ending the current subpath is OK
        builder.move_to((3.0, 3.0)).unwrap();
        // but it's an error if we try to do move_to again
        let result = builder.move_to((4.0, 4.0));

        assert!(result.is_err());
        let Err(PathConversionError::MoveAfterFirstPoint { glyph_name, point }) = result else {
            panic!("unexpected error: {:?}", result);
        };
        assert_eq!("test", glyph_name.as_str());
        assert_eq!((4.0, 4.0), (point.x, point.y));

        builder.end_path().unwrap();
        builder.line_to((5.0, 5.0)).unwrap();
        // can't move_to in the middle of a closed (not starting with move_to) subpath
        let result = builder.move_to((6.0, 6.0));

        assert!(result.is_err());
        let Err(PathConversionError::MoveAfterFirstPoint { glyph_name, point }) = result else {
            panic!("unexpected error: {:?}", result);
        };
        assert_eq!("test", glyph_name.as_str());
        assert_eq!((6.0, 6.0), (point.x, point.y));

        builder.end_path().unwrap();
        builder.offcurve((7.0, 7.0)).unwrap();
        // can't move_to after an offcurve point
        let result = builder.move_to((8.0, 8.0));

        assert!(result.is_err());
        let Err(PathConversionError::MoveAfterFirstPoint { glyph_name, point }) = result else {
            panic!("unexpected error: {:?}", result);
        };
        assert_eq!("test", glyph_name.as_str());
        assert_eq!((8.0, 8.0), (point.x, point.y));
    }

    #[test]
    fn closed_path_with_trailing_cubic_offcurves() {
        let mut builder = GlyphPathBuilder::new("test".into(), 0);
        builder.curve_to((10.0, 0.0)).unwrap();
        builder.line_to((0.0, 10.0)).unwrap();
        builder.offcurve((5.0, 10.0)).unwrap();
        builder.offcurve((10.0, 5.0)).unwrap();

        let path = builder.build().unwrap();

        assert_eq!("M10,0 L0,10 C5,10 10,5 10,0 Z", path.to_svg());
    }

    #[test]
    fn closed_path_with_trailing_quadratic_offcurves() {
        let mut builder = GlyphPathBuilder::new("test".into(), 0);
        builder.qcurve_to((10.0, 0.0)).unwrap();
        builder.line_to((0.0, 10.0)).unwrap();
        builder.offcurve((5.0, 10.0)).unwrap();
        builder.offcurve((10.0, 5.0)).unwrap();

        let path = builder.build().unwrap();

        assert_eq!("M10,0 L0,10 Q5,10 7.5,7.5 Q10,5 10,0 Z", path.to_svg());
    }
}
