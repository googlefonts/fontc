//! Font IR types.

use crate::{
    coords::{CoordConverter, NormalizedCoord, NormalizedLocation, UserCoord, UserLocation},
    error::{PathConversionError, VariationModelError, WorkError},
    serde::{GlobalMetricsSerdeRepr, GlyphSerdeRepr, MiscSerdeRepr, StaticMetadataSerdeRepr},
    variations::VariationModel,
};
use font_types::NameId;
use font_types::Tag;
use fontdrasil::types::GlyphName;
use indexmap::IndexSet;
use kurbo::{Affine, BezPath, PathEl, Point};
use log::{trace, warn};
use ordered_float::OrderedFloat;
use serde::{Deserialize, Serialize};
use std::{
    collections::{HashMap, HashSet},
    fmt::Debug,
    path::{Path, PathBuf},
};
use write_fonts::tables::os2::SelectionFlags;

const DEFAULT_VENDOR_ID: Tag = Tag::new(b"NONE");

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
    pub axes: Vec<Axis>,

    /// Every variable (non-point) axis used by the font being compiled.
    ///
    /// If empty this is a static font.
    pub variable_axes: Vec<Axis>,

    /// Named locations in variation space
    pub named_instances: Vec<NamedInstance>,

    /// The name of every glyph, in the order it will be emitted
    ///
    /// <https://rsheeter.github.io/font101/#glyph-ids-and-the-cmap-table>
    pub glyph_order: IndexSet<GlyphName>,

    /// A model of how variation space is split into regions that have deltas.
    ///
    /// This copy includes all locations used in the entire font. That is, every
    /// location any glyph has an instance. Use of a location not in the global model
    /// is an error.
    pub variation_model: VariationModel,

    axes_default: NormalizedLocation,
    variable_axes_default: NormalizedLocation,

    /// See <https://learn.microsoft.com/en-us/typography/opentype/spec/name>.
    pub names: HashMap<NameKey, String>,

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
}

impl StaticMetadata {
    pub fn new(
        units_per_em: u16,
        names: HashMap<NameKey, String>,
        axes: Vec<Axis>,
        named_instances: Vec<NamedInstance>,
        mut glyph_order: IndexSet<GlyphName>,
        glyph_locations: HashSet<NormalizedLocation>,
    ) -> Result<StaticMetadata, VariationModelError> {
        // Point axes are less exciting than ranged ones
        let variable_axes: Vec<_> = axes.iter().filter(|a| !a.is_point()).cloned().collect();

        // Claim names for axes and named instances
        let mut name_id_gen = 255;
        let mut key_to_name = names;
        let mut visited = HashSet::new();
        axes.iter()
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

        let axes_default = axes
            .iter()
            .map(|a| (a.name.clone(), NormalizedCoord::new(0.0)))
            .collect();
        let variable_axes_default = axes
            .iter()
            .map(|a| (a.name.clone(), NormalizedCoord::new(0.0)))
            .collect();

        // cmap has strong beliefs wrt .notdef coming first
        let notdef: GlyphName = ".notdef".into();
        if let Some(idx) = glyph_order.get_index_of(&notdef) {
            if idx > 0 {
                trace!("Migrate .notdef from {idx} to 0");
                glyph_order.move_index(idx, 0);
            }
        }

        Ok(StaticMetadata {
            units_per_em,
            names: key_to_name,
            axes,
            variable_axes,
            named_instances,
            glyph_order,
            variation_model,
            axes_default,
            variable_axes_default,
            misc: MiscMetadata {
                selection_flags: Default::default(),
                vendor_id: DEFAULT_VENDOR_ID,
            },
        })
    }

    pub fn glyph_id(&self, name: &GlyphName) -> Option<u32> {
        self.glyph_order.get_index_of(name).map(|i| i as u32)
    }

    /// The default on all known axes.
    pub fn default_location(&self) -> &NormalizedLocation {
        &self.axes_default
    }

    /// The default on all variable (non-point) axes.
    pub fn variable_axes_default(&self) -> &NormalizedLocation {
        &self.variable_axes_default
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
    Os2TypoAscender,
    Os2TypoDescender,
    Os2TypoLineGap,
    Os2WinAscent,
    Os2WinDescent,
    CapHeight,
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

impl NameBuilder {
    pub fn add(&mut self, name_id: NameId, value: String) {
        let key = NameKey::new(name_id, &value);
        self.names.insert(key, value);
        self.name_to_key.insert(name_id, key);
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

    pub fn apply_default_fallbacks(&mut self) {
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
                "{} {}",
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
            let vendor = "NONE";
            let postscript_name = self.get(NameId::POSTSCRIPT_NAME).unwrap();
            self.add(
                NameId::UNIQUE_ID,
                format!("{version};{vendor};{postscript_name}"),
            );
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
#[derive(Serialize, Deserialize, Debug, PartialEq, Eq)]
pub enum Features {
    Empty,
    File(PathBuf),
    Memory(String),
}

impl Features {
    pub fn empty() -> Features {
        Features::Empty
    }
    pub fn from_file(file: &Path) -> Features {
        Features::File(file.to_path_buf())
    }
    pub fn from_string(fea_content: String) -> Features {
        Features::Memory(fea_content)
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
        Ok(Glyph {
            name,
            codepoints,
            default_location,
            sources,
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
}

impl TryInto<Glyph> for GlyphBuilder {
    type Error = WorkError;

    fn try_into(self) -> Result<Glyph, Self::Error> {
        Glyph::new(self.name, self.codepoints, self.sources)
    }
}

impl From<&Glyph> for GlyphBuilder {
    fn from(value: &Glyph) -> Self {
        Self {
            name: value.name.clone(),
            codepoints: value.codepoints.clone(),
            sources: value.sources.clone(),
        }
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

/// Helps convert points-of-type to a bezier path.
///
/// Source formats tend to use streams of point-of-type. Curve manipulation is
/// often easier on bezier path, so provide a mechanism to convert.
#[derive(Debug)]
pub struct GlyphPathBuilder {
    glyph_name: GlyphName,
    offcurve: Vec<Point>,
    path: BezPath,
    last_move_to: Option<Point>,
}

impl GlyphPathBuilder {
    pub fn new(glyph_name: GlyphName) -> GlyphPathBuilder {
        GlyphPathBuilder {
            glyph_name,
            offcurve: Vec::new(),
            path: BezPath::new(),
            last_move_to: None,
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

    pub fn is_empty(&self) -> bool {
        self.offcurve.is_empty() && self.path.elements().is_empty()
    }

    pub fn move_to(&mut self, p: impl Into<Point>) -> Result<(), PathConversionError> {
        self.check_num_offcurve(|v| v == 0)?;
        let p = p.into();
        self.path.move_to(p);
        self.last_move_to = Some(p);
        Ok(())
    }

    pub fn line_to(&mut self, p: impl Into<Point>) -> Result<(), PathConversionError> {
        self.check_num_offcurve(|v| v == 0)?;
        if self.is_empty() {
            self.move_to(p)?;
        } else {
            self.path.line_to(p);
        }
        Ok(())
    }

    pub fn qcurve_to(&mut self, p: impl Into<Point>) -> Result<(), PathConversionError> {
        // https://github.com/googlefonts/fontmake-rs/issues/110
        // Guard clauses: degenerate cases
        if self.is_empty() {
            return self.move_to(p);
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
            self.path.quad_to(curr, implied);
        }
        // last but not least, the last offcurve to the provided point
        self.path.quad_to(*self.offcurve.last().unwrap(), p.into());
        self.offcurve.clear();

        Ok(())
    }

    /// Type of curve depends on accumulated off-curves
    ///
    /// <https://unifiedfontobject.org/versions/ufo3/glyphs/glif/#point-types>
    pub fn curve_to(&mut self, p: impl Into<Point>) -> Result<(), PathConversionError> {
        if !self.is_empty() {
            match self.offcurve.len() {
                0 => self.path.line_to(p),
                1 => self.path.quad_to(self.offcurve[0], p.into()),
                2 => self
                    .path
                    .curve_to(self.offcurve[0], self.offcurve[1], p.into()),
                _ => self.check_num_offcurve(|v| v < 3)?,
            }
            self.offcurve.clear();
        } else {
            self.move_to(p)?;
        }
        Ok(())
    }

    pub fn offcurve(&mut self, p: impl Into<Point>) -> Result<(), PathConversionError> {
        self.offcurve.push(p.into());
        Ok(())
    }

    pub fn close_path(&mut self) -> Result<(), PathConversionError> {
        // Take dangling off-curves to imply a curve back to sub-path start
        if let Some(last_move) = self.last_move_to {
            if !self.offcurve.is_empty() {
                self.curve_to(last_move)?;
            }
            // explicitly output the implied closing line
            // equivalent to fontTools' PointToSegmentPen(outputImpliedClosingLine=True)
            match self.path.elements().last() {
                // The last point of the closed contour is actually of type "line"
                // Explicitly emit it so it doesn't get lost if we turn back into a pointstream
                Some(PathEl::LineTo(_)) => {
                    self.path.line_to(last_move);
                }

                // The source ends in something that isn't a line, and it's closed.
                // Add an explicit line if it wouldn't be zero-length as this is how
                // a closed point-stream is interpreted.
                Some(PathEl::QuadTo(_, last_pt)) | Some(PathEl::CurveTo(_, _, last_pt)) => {
                    if *last_pt != last_move {
                        self.path.line_to(last_move)
                    }
                }
                _ => (),
            }
            self.path.close_path();
        }
        Ok(())
    }

    pub fn build(self) -> BezPath {
        self.path
    }
}

#[cfg(test)]
mod tests {

    use std::str::FromStr;

    use font_types::Tag;

    use crate::{
        coords::{CoordConverter, UserCoord},
        ir::Axis,
    };

    use super::GlyphPathBuilder;

    fn test_axis() -> Axis {
        let min = UserCoord::new(100.0);
        let default = UserCoord::new(400.0);
        let max = UserCoord::new(900.0);
        let converter = CoordConverter::unmapped(min, default, max);
        Axis {
            name: String::from("Weight"),
            tag: Tag::from_str("wght").unwrap(),
            min,
            default,
            max,
            hidden: false,
            converter,
        }
    }

    #[test]
    fn axis_yaml() {
        let test_axis = test_axis();
        let yml = serde_yaml::to_string(&test_axis).unwrap();
        assert_eq!(test_axis, serde_yaml::from_str(&yml).unwrap());
    }

    #[test]
    fn axis_bincode() {
        let test_axis = test_axis();
        let bin = bincode::serialize(&test_axis).unwrap();
        assert_eq!(test_axis, bincode::deserialize(&bin).unwrap());
    }

    #[test]
    fn a_qcurve_with_no_offcurve_is_a_line() {
        let mut builder = GlyphPathBuilder::new("test".into());
        builder.move_to((2.0, 2.0)).unwrap();
        builder.qcurve_to((4.0, 2.0)).unwrap();
        assert_eq!("M2,2 L4,2", builder.build().to_svg());
    }

    #[test]
    fn a_qcurve_with_one_offcurve_is_a_single_quad_to() {
        let mut builder = GlyphPathBuilder::new("test".into());
        builder.move_to((2.0, 2.0)).unwrap();
        builder.offcurve((3.0, 0.0)).unwrap();
        builder.qcurve_to((4.0, 2.0)).unwrap();
        assert_eq!("M2,2 Q3,0 4,2", builder.build().to_svg());
    }

    #[test]
    fn a_qcurve_with_two_offcurve_is_two_quad_to() {
        let mut builder = GlyphPathBuilder::new("test".into());
        builder.move_to((2.0, 2.0)).unwrap();
        builder.offcurve((3.0, 0.0)).unwrap();
        builder.offcurve((5.0, 4.0)).unwrap();
        builder.qcurve_to((6.0, 2.0)).unwrap();
        assert_eq!("M2,2 Q3,0 4,2 Q5,4 6,2", builder.build().to_svg());
    }

    #[test]
    fn last_line_always_emit_implied_closing_line() {
        let mut builder = GlyphPathBuilder::new("test".into());
        builder.move_to((2.0, 2.0)).unwrap();
        builder.line_to((4.0, 2.0)).unwrap();
        builder.close_path().unwrap();
        // a closing line is implied by Z, but emit it nonetheless
        assert_eq!("M2,2 L4,2 L2,2 Z", builder.build().to_svg());

        let mut builder = GlyphPathBuilder::new("test".into());
        builder.move_to((2.0, 2.0)).unwrap();
        builder.line_to((4.0, 2.0)).unwrap();
        // duplicate last point, not to be confused with the closing line implied by Z
        builder.line_to((2.0, 2.0)).unwrap();
        builder.close_path().unwrap();
        assert_eq!("M2,2 L4,2 L2,2 L2,2 Z", builder.build().to_svg());
    }

    #[test]
    fn last_curve_equals_move_no_closing_line() {
        // if last curve point is equal to move, there's no need to disambiguate it from
        // the implicit closing line, so we don't emit one
        let mut builder = GlyphPathBuilder::new("test".into());
        builder.move_to((2.0, 2.0)).unwrap();
        builder.offcurve((3.0, 0.0)).unwrap();
        builder.qcurve_to((2.0, 2.0)).unwrap();
        builder.close_path().unwrap();
        assert_eq!("M2,2 Q3,0 2,2 Z", builder.build().to_svg());

        let mut builder = GlyphPathBuilder::new("test".into());
        builder.move_to((2.0, 2.0)).unwrap();
        builder.offcurve((3.0, 0.0)).unwrap();
        builder.offcurve((0.0, 3.0)).unwrap();
        builder.curve_to((2.0, 2.0)).unwrap();
        builder.close_path().unwrap();
        assert_eq!("M2,2 C3,0 0,3 2,2 Z", builder.build().to_svg());
    }

    #[test]
    fn last_curve_not_equal_move_do_emit_closing_line() {
        // if last point is different from move, then emit the implied closing line
        let mut builder = GlyphPathBuilder::new("test".into());
        builder.move_to((2.0, 2.0)).unwrap();
        builder.offcurve((3.0, 0.0)).unwrap();
        builder.qcurve_to((4.0, 2.0)).unwrap();
        builder.close_path().unwrap();
        assert_eq!("M2,2 Q3,0 4,2 L2,2 Z", builder.build().to_svg());

        let mut builder = GlyphPathBuilder::new("test".into());
        builder.move_to((2.0, 2.0)).unwrap();
        builder.offcurve((3.0, 0.0)).unwrap();
        builder.offcurve((0.0, 3.0)).unwrap();
        builder.curve_to((4.0, 2.0)).unwrap();
        builder.close_path().unwrap();
        assert_eq!("M2,2 C3,0 0,3 4,2 L2,2 Z", builder.build().to_svg());
    }
}
