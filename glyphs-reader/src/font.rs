//! The general strategy is just to use a plist for storage. Also, lots of
//! unwrapping.
//!
//! There are lots of other ways this could go, including something serde-like
//! where it gets serialized to more Rust-native structures, proc macros, etc.

use std::borrow::Cow;
use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::ffi::OsStr;
use std::hash::Hash;
use std::ops::RangeInclusive;
use std::str::FromStr;
use std::sync::{LazyLock, Mutex};
use std::{fs, path};

use crate::glyphdata::{Category, GlyphData, Subcategory};
use ascii_plist_derive::FromPlist;
use fontdrasil::types::WidthClass;
use indexmap::{IndexMap, IndexSet};
use kurbo::{Affine, Point, Vec2};
use log::{debug, warn};
use ordered_float::OrderedFloat;
use regex::Regex;
use smol_str::SmolStr;

use crate::error::Error;
use crate::plist::{FromPlist, Plist, Token, Tokenizer, VecDelimiters};

// Static set to track warnings we've already logged, to avoid repetition
static LOGGED_WARNINGS: LazyLock<Mutex<HashSet<String>>> =
    LazyLock::new(|| Mutex::new(HashSet::new()));

/// Log a warning message only once per process lifetime
macro_rules! log_once_warn {
    ($($arg:tt)*) => {{
        if log::log_enabled!(log::Level::Warn) {
            let msg = format!($($arg)*);
            let mut logged = LOGGED_WARNINGS.lock().unwrap();
            if !logged.contains(&msg) {
                log::warn!("{}", msg);
                logged.insert(msg);
            }
        }
    }};
}

const V3_METRIC_NAMES: [&str; 6] = [
    "ascender",
    "baseline",
    "descender",
    "cap height",
    "x-height",
    "italic angle",
];

#[derive(Clone, Debug, Default, PartialEq, Hash)]
pub struct UserToDesignMapping(BTreeMap<String, AxisUserToDesignMap>);

#[derive(Clone, Debug, Default, PartialEq, Hash)]
pub struct AxisUserToDesignMap(Vec<(OrderedFloat<f64>, OrderedFloat<f64>)>);

/// A tidied up font from a plist.
///
/// Normalized representation of Glyphs 2/3 content
#[derive(Clone, Debug, Default, PartialEq, Hash)]
pub struct Font {
    pub units_per_em: u16,
    pub axes: Vec<Axis>,
    pub masters: Vec<FontMaster>,
    pub default_master_idx: usize,
    pub glyphs: BTreeMap<SmolStr, Glyph>,
    pub glyph_order: Vec<SmolStr>,
    // tag => (user:design) tuples
    pub axis_mappings: UserToDesignMapping,
    pub virtual_masters: Vec<BTreeMap<String, OrderedFloat<f64>>>,
    pub features: Vec<FeatureSnippet>,
    // Incliudes both default (English) and localized property names.
    // Maps property key (e.g., "familyNames") to list of (language code, value)
    pub all_names: BTreeMap<String, Vec<(String, String)>>,
    pub instances: Vec<Instance>,
    pub version_major: i32,
    pub version_minor: u32,
    pub date: Option<String>,

    // master id => { (name or class, name or class) => adjustment }
    pub kerning_ltr: Kerning,
    pub kerning_rtl: Kerning,

    pub custom_parameters: CustomParameters,
}

/// Custom parameter options that can be set on a glyphs font
#[derive(Clone, Debug, PartialEq, Hash, Default)]
pub struct CustomParameters {
    pub propagate_anchors: Option<bool>,
    pub use_typo_metrics: Option<bool>,
    pub is_fixed_pitch: Option<bool>,
    pub fs_type: Option<u16>,
    pub has_wws_names: Option<bool>,
    pub typo_ascender: Option<i64>,
    pub typo_descender: Option<i64>,
    pub typo_line_gap: Option<i64>,
    pub win_ascent: Option<i64>,
    pub win_descent: Option<i64>,
    pub hhea_ascender: Option<i64>,
    pub hhea_descender: Option<i64>,
    pub hhea_line_gap: Option<i64>,
    pub vhea_ascender: Option<i64>,
    pub vhea_descender: Option<i64>,
    pub vhea_line_gap: Option<i64>,
    pub underline_thickness: Option<OrderedFloat<f64>>,
    pub underline_position: Option<OrderedFloat<f64>>,
    pub strikeout_position: Option<i64>,
    pub strikeout_size: Option<i64>,
    pub subscript_x_offset: Option<i64>,
    pub subscript_x_size: Option<i64>,
    pub subscript_y_offset: Option<i64>,
    pub subscript_y_size: Option<i64>,
    pub superscript_x_offset: Option<i64>,
    pub superscript_x_size: Option<i64>,
    pub superscript_y_offset: Option<i64>,
    pub superscript_y_size: Option<i64>,
    pub unicode_range_bits: Option<BTreeSet<u32>>,
    pub codepage_range_bits: Option<BTreeSet<u32>>,
    pub panose: Option<Vec<i64>>,

    pub lowest_rec_ppem: Option<i64>,
    pub hhea_caret_slope_run: Option<i64>,
    pub hhea_caret_slope_rise: Option<i64>,
    pub hhea_caret_offset: Option<i64>,
    pub vhea_caret_slope_run: Option<i64>,
    pub vhea_caret_slope_rise: Option<i64>,
    pub vhea_caret_offset: Option<i64>,
    pub meta_table: Option<MetaTableValues>,
    pub dont_use_production_names: Option<bool>,
    // these fields are parsed via the config, but are stored
    // in the top-level `Font` struct
    pub virtual_masters: Option<Vec<BTreeMap<String, OrderedFloat<f64>>>>,
    pub glyph_order: Option<Vec<SmolStr>>,
    pub gasp_table: Option<BTreeMap<i64, i64>>,
    pub feature_for_feature_variations: Option<SmolStr>,
}

/// Values for the 'meta Table' custom parameter
#[derive(Clone, Debug, PartialEq, Hash, Default)]
pub struct MetaTableValues {
    pub dlng: Vec<SmolStr>,
    pub slng: Vec<SmolStr>,
}

impl MetaTableValues {
    fn from_plist(plist: &Plist) -> Option<Self> {
        let mut ret = MetaTableValues::default();
        let as_array = plist.as_array()?;
        for item in as_array {
            let tag = item.get("tag").and_then(Plist::as_str)?;
            let data = item.get("data").and_then(Plist::as_str)?;
            let data = data.split(',').map(SmolStr::new).collect();

            match tag {
                "dlng" => ret.dlng = data,
                "slng" => ret.slng = data,
                _ => log_once_warn!("Unknown meta table tag '{tag}'"),
            }
        }

        if ret.dlng.len() + ret.slng.len() > 0 {
            Some(ret)
        } else {
            None
        }
    }
}

/// master id => { (name or class, name or class) => adjustment }
#[derive(Clone, Debug, Default, PartialEq, Hash)]
pub struct Kerning(BTreeMap<String, BTreeMap<(SmolStr, SmolStr), OrderedFloat<f64>>>);

impl Kerning {
    pub fn get(&self, master_id: &str) -> Option<&BTreeMap<(SmolStr, SmolStr), OrderedFloat<f64>>> {
        self.0.get(master_id)
    }

    pub fn keys(&self) -> impl Iterator<Item = &String> {
        self.0.keys()
    }

    pub fn iter(
        &self,
    ) -> impl Iterator<Item = (&String, &BTreeMap<(SmolStr, SmolStr), OrderedFloat<f64>>)> {
        self.0.iter()
    }

    fn insert(
        &mut self,
        master_id: String,
        lhs_class_or_group: SmolStr,
        rhs_class_or_group: SmolStr,
        kern: f64,
    ) {
        *self
            .0
            .entry(master_id)
            .or_default()
            .entry((lhs_class_or_group, rhs_class_or_group))
            .or_default() = kern.into();
    }
}

/// Hand-parse because it's a bit weird
impl FromPlist for Kerning {
    fn parse(tokenizer: &mut Tokenizer<'_>) -> Result<Self, crate::plist::Error> {
        let mut kerning = Kerning::default();

        tokenizer.eat(b'{')?;

        loop {
            if tokenizer.eat(b'}').is_ok() {
                break;
            }

            // parse string-that-is-master-id = {
            let master_id: String = tokenizer.parse()?;
            tokenizer.eat(b'=')?;

            // The map for the master
            tokenizer.eat(b'{')?;
            loop {
                if tokenizer.eat(b'}').is_ok() {
                    break;
                }
                let lhs_name_or_class: SmolStr = tokenizer.parse()?;
                tokenizer.eat(b'=')?;
                tokenizer.eat(b'{')?;

                // rhs name = value pairs
                loop {
                    if tokenizer.eat(b'}').is_ok() {
                        break;
                    }

                    let rhs_name_or_class: SmolStr = tokenizer.parse()?;
                    tokenizer.eat(b'=')?;
                    let value: f64 = tokenizer.parse()?;
                    tokenizer.eat(b';')?;

                    kerning.insert(
                        master_id.clone(),
                        lhs_name_or_class.clone(),
                        rhs_name_or_class,
                        value,
                    );
                }
                tokenizer.eat(b';')?;
            }

            tokenizer.eat(b';')?;
        }

        Ok(kerning)
    }
}

/// A chunk of FEA code
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct FeatureSnippet {
    pub content: String,
    /// If `true` the content should be ignored.
    pub disabled: bool,
}

impl FeatureSnippet {
    pub fn new(content: String, disabled: bool) -> Self {
        FeatureSnippet { content, disabled }
    }

    pub fn str_if_enabled(&self) -> Option<&str> {
        (!self.disabled).then_some(&self.content)
    }
}

#[derive(Clone, Default, Debug, PartialEq, Hash)]
pub struct Glyph {
    pub name: SmolStr,
    pub export: bool,
    pub layers: Vec<Layer>,
    pub bracket_layers: Vec<Layer>,
    pub unicode: BTreeSet<u32>,
    /// The left kerning group
    pub left_kern: Option<SmolStr>,
    /// The right kerning group
    pub right_kern: Option<SmolStr>,
    pub category: Option<Category>,
    pub sub_category: Option<Subcategory>,
    pub production_name: Option<SmolStr>,
    /// If this is a smart component, these are the axe names -> user coords
    pub smart_component_axes: BTreeMap<SmolStr, RangeInclusive<i64>>,
}

impl Glyph {
    pub fn is_nonspacing_mark(&self) -> bool {
        matches!(
            (self.category, self.sub_category),
            (Some(Category::Mark), Some(Subcategory::Nonspacing))
        )
    }

    pub(crate) fn has_components(&self) -> bool {
        self.layers
            .iter()
            .chain(self.bracket_layers.iter())
            .flat_map(Layer::components)
            .next()
            .is_some()
    }
}

/// Whether a given layer of a smart component is at the min or max of a given axis.
#[derive(Clone, Debug, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum AxisPole {
    /// this layer is at the minimum value for this property
    Min,
    /// this layer is at the maximum value for this property
    Max,
}

#[derive(Debug, Default, Clone, PartialEq, Hash)]
pub struct Layer {
    pub layer_id: String,
    pub associated_master_id: Option<String>,
    pub width: OrderedFloat<f64>,
    pub vert_width: Option<OrderedFloat<f64>>,
    pub vert_origin: Option<OrderedFloat<f64>>,
    pub shapes: Vec<Shape>,
    pub anchors: Vec<Anchor>,
    pub attributes: LayerAttributes,
    /// If this layer is part of a smart component, this defines its location.
    ///
    /// Smart component layers can only be defined at the min or max of a given
    /// axis.
    pub smart_component_positions: BTreeMap<SmolStr, AxisPole>,
}

impl Layer {
    pub fn is_master(&self) -> bool {
        self.associated_master_id.is_none()
    }

    /// associated master id if set, else layer id
    pub fn master_id(&self) -> &str {
        self.associated_master_id
            .as_deref()
            .unwrap_or(&self.layer_id)
    }

    /// A key used to identify a bracket layer during anchor propagation
    pub(crate) fn axis_rules_key(&self) -> Option<String> {
        (!self.attributes.axis_rules.is_empty())
            .then(|| format!("{:?} {}", self.attributes.axis_rules, self.master_id()))
    }

    pub fn is_intermediate(&self) -> bool {
        self.associated_master_id.is_some() && !self.attributes.coordinates.is_empty()
    }

    pub(crate) fn components(&self) -> impl Iterator<Item = &Component> + '_ {
        self.shapes.iter().filter_map(|shape| match shape {
            Shape::Path(_) => None,
            Shape::Component(comp) => Some(comp),
        })
    }

    /// NOTE: panics if called on a non-bracket layer
    fn bracket_info(&self, axes: &[Axis]) -> BTreeMap<String, (Option<i64>, Option<i64>)> {
        assert!(
            !self.attributes.axis_rules.is_empty(),
            "all bracket layers have axis rules"
        );
        axes.iter()
            .zip(&self.attributes.axis_rules)
            .map(|(axis, rule)| (axis.tag.clone(), (rule.min, rule.max)))
            .collect()
    }

    // TODO add is_alternate, is_color, etc.
}

#[derive(Clone, Default, Debug, PartialEq, Hash)]
pub struct LayerAttributes {
    pub coordinates: Vec<OrderedFloat<f64>>,
    pub color: bool,
    // in the same order that axes are declared for the font
    pub axis_rules: Vec<AxisRule>,
}

#[derive(Clone, Default, FromPlist, Debug, PartialEq, Hash)]
pub struct AxisRule {
    // if missing, assume default min/max for font
    pub min: Option<i64>,
    pub max: Option<i64>,
}

impl AxisRule {
    /// Parse an AxisRule from a glyphs v2 layer name.
    ///
    /// See <https://glyphsapp.com/learn/alternating-glyph-shapes> for an
    /// overview of the naming conventions.
    fn from_layer_name(name: &str) -> Option<Self> {
        // the pattern can either be "$name [100]" or "$name ]100]"
        // (python uses a regex for this:)
        // https://github.com/googlefonts/glyphsLib/blob/c4db6b981d/Lib/glyphsLib/classes.py#L3938
        let idx = name.find([']', '['])?;
        let reversed = name.as_bytes()[idx] == b']';
        let tail = name.get(idx + 1..)?;
        let (value, _) = tail.split_once(']')?;
        let value = str::parse::<u32>(value.trim()).ok()?;
        let (min, max) = if reversed {
            (None, Some(value as _))
        } else {
            (Some(value as _), None)
        };
        Some(AxisRule { min, max })
    }
}

// hand-parse because they can take multiple shapes
impl FromPlist for LayerAttributes {
    fn parse(tokenizer: &mut Tokenizer<'_>) -> Result<Self, crate::plist::Error> {
        let mut coordinates = Vec::new();
        let mut color = false;
        let mut axis_rules = Vec::new();

        tokenizer.eat(b'{')?;

        loop {
            if tokenizer.eat(b'}').is_ok() {
                break;
            }

            let key: String = tokenizer.parse()?;
            tokenizer.eat(b'=')?;
            match key.as_str() {
                "coordinates" => coordinates = tokenizer.parse()?,
                "color" => color = tokenizer.parse()?,
                "axisRules" => axis_rules = tokenizer.parse()?,
                // skip unsupported attributes for now
                // TODO: match the others
                _ => tokenizer.skip_rec()?,
            }
            tokenizer.eat(b';')?;
        }

        Ok(LayerAttributes {
            coordinates,
            color,
            axis_rules,
        })
    }
}

#[derive(Clone, Default, Debug, PartialEq, Eq, Hash, FromPlist)]
pub struct ShapeAttributes {
    pub gradient: Option<Gradient>,
    pub fill_color: Option<Color>,
}

impl ShapeAttributes {
    pub fn colors(&self) -> impl Iterator<Item = &Color> {
        self.gradient
            .iter()
            .flat_map(|g| g.colors())
            .chain(self.fill_color.iter())
    }
}

#[derive(Clone, Default, Debug, PartialEq, Eq, Hash, FromPlist)]
pub struct Gradient {
    pub start: Vec<OrderedFloat<f64>>,
    pub end: Vec<OrderedFloat<f64>>,
    pub colors: Vec<ColorStop>,
    #[fromplist(key = "type")]
    pub style: String,
}

impl Gradient {
    fn colors(&self) -> impl Iterator<Item = &Color> {
        self.colors.iter().map(|c| &c.color)
    }
}

#[derive(Clone, Copy, Default, Debug, PartialEq, Eq, Hash)]
pub struct Color {
    pub r: i64,
    pub g: i64,
    pub b: i64,
    pub a: i64,
}

#[derive(Clone, Copy, Default, Debug, PartialEq, Eq, Hash)]
pub struct ColorStop {
    pub color: Color,
    // The position on the color line, see <https://learn.microsoft.com/en-us/typography/opentype/spec/colr#color-lines>
    pub stop_offset: OrderedFloat<f64>,
}

impl Color {
    pub fn rgba(r: i64, g: i64, b: i64, a: i64) -> Self {
        Self { r, g, b, a }
    }
}

// hand-parse because it's a list of inconsistent types
impl FromPlist for Color {
    fn parse(tokenizer: &mut Tokenizer<'_>) -> Result<Self, crate::plist::Error> {
        let colors = tokenizer.parse::<Vec<i64>>()?;

        // See <https://github.com/googlefonts/glyphsLib/blob/c4db6b981d577f456d64ebe9993818770e170454/Lib/glyphsLib/builder/common.py#L41-L50>
        match *colors.as_slice() {
            // Grayscale
            [black, alpha] => Ok(Color::rgba(black, black, black, alpha)),
            // RGBA
            [r, g, b, a] => Ok(Color::rgba(r, g, b, a)),
            // 5 is CMYK, match python by not supporting that
            _ => Err(crate::plist::Error::UnexpectedNumberOfValues {
                value_type: "grayscale (2) or rgba (4)",
                actual: colors.len(),
            }),
        }
    }
}

// hand-parse because it's a list of inconsistent types
impl FromPlist for ColorStop {
    fn parse(tokenizer: &mut Tokenizer<'_>) -> Result<Self, crate::plist::Error> {
        tokenizer.eat(b'(')?;

        let color = Color::parse(tokenizer)?;
        tokenizer.eat(b',')?;
        let stop_offset = tokenizer.parse::<f64>()?;
        tokenizer.eat(b')')?;

        Ok(ColorStop {
            color,
            stop_offset: stop_offset.into(),
        })
    }
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub enum Shape {
    Path(Path),
    Component(Component),
}

impl Shape {
    pub fn attributes(&self) -> &ShapeAttributes {
        match self {
            Shape::Path(p) => &p.attributes,
            Shape::Component(c) => &c.attributes,
        }
    }

    pub(crate) fn as_path(&self) -> Option<&Path> {
        match self {
            Shape::Path(path) => Some(path),
            _ => None,
        }
    }

    pub(crate) fn as_smart_component(&self) -> Option<&Component> {
        match self {
            Shape::Component(component) if !component.smart_component_values.is_empty() => {
                Some(component)
            }
            _ => None,
        }
    }

    /// If the shape is a path, reverse it
    pub(crate) fn reverse(&mut self) {
        if let Shape::Path(path) = self {
            path.reverse();
        }
    }
}

#[derive(Clone, Copy, Debug, Default, PartialEq, PartialOrd)]
enum FormatVersion {
    #[default]
    V2,
    V3,
}

// The font you get directly from a plist, minimally modified
// Types chosen specifically to accomodate plist translation.
#[derive(Default, Debug, PartialEq, FromPlist)]
#[allow(non_snake_case)]
struct RawFont {
    #[fromplist(key = ".formatVersion")]
    format_version: FormatVersion,
    units_per_em: Option<i64>,
    metrics: Vec<RawMetric>,
    family_name: String,
    date: Option<String>,
    copyright: Option<String>,
    designer: Option<String>,
    designerURL: Option<String>,
    manufacturer: Option<String>,
    manufacturerURL: Option<String>,
    versionMajor: Option<i64>,
    versionMinor: Option<i64>,
    axes: Vec<Axis>,
    glyphs: Vec<RawGlyph>,
    font_master: Vec<RawFontMaster>,
    instances: Vec<RawInstance>,
    feature_prefixes: Vec<RawFeature>,
    features: Vec<RawFeature>,
    classes: Vec<RawFeature>,
    properties: Vec<RawName>,
    #[fromplist(alt_name = "kerning")]
    kerning_LTR: Kerning,
    kerning_RTL: Kerning,
    custom_parameters: RawCustomParameters,
    numbers: Vec<NumberName>,
}

#[derive(Default, Debug, PartialEq, FromPlist)]
struct NumberName {
    name: SmolStr,
}

// we use a vec of tuples instead of a map because there can be multiple
// values for the same name (e.g. 'Virtual Master')
#[derive(Clone, Default, Debug, PartialEq, Eq, Hash)]
pub(crate) struct RawCustomParameters(Vec<RawCustomParameterValue>);

#[derive(Clone, Default, Debug, PartialEq, Eq, Hash, FromPlist)]
struct RawCustomParameterValue {
    name: SmolStr,
    value: Plist,
    disabled: Option<bool>,
}

impl RawCustomParameterValue {
    /// Returns true if this parameter has been taken
    ///
    /// We use a special sentinel value to indicate that a parameter has been handled
    /// specially and shouldn't be parsed as/stored in CustomParameters.
    fn taken(&self) -> bool {
        self == &Self::default()
    }
}

impl FromPlist for FormatVersion {
    fn parse(tokenizer: &mut Tokenizer) -> Result<Self, crate::plist::Error> {
        let raw: i64 = FromPlist::parse(tokenizer)?;
        if raw == 3 {
            Ok(FormatVersion::V3)
        } else {
            // format version 2 is the default value, if no explicit version is set
            Err(crate::plist::Error::Parse(
                "'3' is the only known format version".into(),
            ))
        }
    }
}

impl FormatVersion {
    fn is_v2(self) -> bool {
        self == FormatVersion::V2
    }

    fn codepoint_radix(self) -> u32 {
        match self {
            FormatVersion::V2 => 16,
            FormatVersion::V3 => 10,
        }
    }
}

impl FromPlist for RawCustomParameters {
    fn parse(tokenizer: &mut Tokenizer) -> Result<Self, crate::plist::Error> {
        Vec::parse(tokenizer).map(RawCustomParameters)
    }
}

/// Convenience methods for converting `Plist` structs to concrete custom param types
trait PlistParamsExt {
    fn as_codepage_bits(&self) -> Option<BTreeSet<u32>>;
    fn as_unicode_code_ranges(&self) -> Option<BTreeSet<u32>>;
    fn as_fs_type(&self) -> Option<u16>;
    fn as_mapping_values(&self) -> Option<Vec<(OrderedFloat<f64>, OrderedFloat<f64>)>>;
    fn as_axis_location(&self) -> Option<AxisLocation>;
    fn as_axis(&self) -> Option<Axis>;
    fn as_bool(&self) -> Option<bool>;
    fn as_ordered_f64(&self) -> Option<OrderedFloat<f64>>;
    fn as_vec_of_ints(&self) -> Option<Vec<i64>>;
    fn as_axis_locations(&self) -> Option<Vec<AxisLocation>>;
    fn as_vec_of_string(&self) -> Option<Vec<SmolStr>>;
    fn as_axes(&self) -> Option<Vec<Axis>>;
    fn as_axis_mappings(&self) -> Option<Vec<AxisMapping>>;
    fn as_virtual_master(&self) -> Option<BTreeMap<String, OrderedFloat<f64>>>;
    fn as_gasp_table(&self) -> Option<BTreeMap<i64, i64>>;
}

impl PlistParamsExt for Plist {
    fn as_bool(&self) -> Option<bool> {
        self.as_i64().map(|val| val == 1)
    }

    fn as_ordered_f64(&self) -> Option<OrderedFloat<f64>> {
        self.as_f64().map(OrderedFloat)
    }

    fn as_vec_of_ints(&self) -> Option<Vec<i64>> {
        // exciting! apparently glyphs (at least sometimes?) serializes
        // this as a vec of strings? (this is true in WghtVar_OS2.glyphs)
        if let Some(thing_that_makes_sense) = self.as_array()?.iter().map(Plist::as_i64).collect() {
            return Some(thing_that_makes_sense);
        }

        self.as_array()?
            .iter()
            .map(|val| val.as_str().and_then(|s| s.parse::<i64>().ok()))
            .collect()
    }

    fn as_axis_location(&self) -> Option<AxisLocation> {
        let plist = self.as_dict()?;
        let name = plist.get("Axis").and_then(Plist::as_str)?;
        let location = plist.get("Location").and_then(Plist::as_f64)?;
        Some(AxisLocation {
            axis_name: name.into(),
            location: location.into(),
        })
    }

    fn as_axis_locations(&self) -> Option<Vec<AxisLocation>> {
        let array = self.as_array()?;
        array.iter().map(Plist::as_axis_location).collect()
    }

    fn as_vec_of_string(&self) -> Option<Vec<SmolStr>> {
        self.as_array()?
            .iter()
            .map(|plist| plist.as_str().map(SmolStr::from))
            .collect()
    }

    fn as_axis(&self) -> Option<Axis> {
        let plist = self.as_dict()?;
        let name = plist.get("Name").and_then(Plist::as_str)?;
        let tag = plist.get("Tag").and_then(Plist::as_str)?;
        let hidden = plist.get("hidden").and_then(Plist::as_bool);
        Some(Axis {
            name: name.into(),
            tag: tag.into(),
            hidden,
        })
    }

    fn as_axes(&self) -> Option<Vec<Axis>> {
        self.as_array()?.iter().map(Plist::as_axis).collect()
    }

    fn as_mapping_values(&self) -> Option<Vec<(OrderedFloat<f64>, OrderedFloat<f64>)>> {
        let plist = self.as_dict()?;
        plist
            .iter()
            .map(|(key, value)| {
                // our keys are strings, but we want to interpret them as floats:
                let key = key
                    .parse::<f64>()
                    .or_else(|_| key.parse::<i64>().map(|i| i as f64))
                    .ok()?;
                let val = value.as_f64()?;
                Some((key.into(), val.into()))
            })
            .collect()
    }

    fn as_axis_mappings(&self) -> Option<Vec<AxisMapping>> {
        self.as_dict()?
            .iter()
            .map(|(tag, mappings)| {
                let user_to_design = mappings.as_mapping_values()?;
                Some(AxisMapping {
                    tag: tag.to_string(),
                    user_to_design,
                })
            })
            .collect()
    }

    fn as_virtual_master(&self) -> Option<BTreeMap<String, OrderedFloat<f64>>> {
        self.as_array()?
            .iter()
            .map(Plist::as_axis_location)
            .map(|loc| loc.map(|loc| (loc.axis_name, loc.location)))
            .collect()
    }

    fn as_fs_type(&self) -> Option<u16> {
        Some(self.as_vec_of_ints()?.iter().map(|bit| 1 << bit).sum())
    }

    fn as_unicode_code_ranges(&self) -> Option<BTreeSet<u32>> {
        let bits = self.as_vec_of_ints()?;
        Some(bits.iter().map(|bit| *bit as u32).collect())
    }

    fn as_codepage_bits(&self) -> Option<BTreeSet<u32>> {
        let bits = self.as_vec_of_ints()?;
        bits.iter()
            .map(|b| codepage_range_bit(*b as _))
            .collect::<Result<_, _>>()
            .ok()
    }

    fn as_gasp_table(&self) -> Option<BTreeMap<i64, i64>> {
        Some(
            self.as_dict()?
                .iter()
                .filter_map(|(k, v)| k.parse::<i64>().ok().zip(v.as_i64()))
                .collect(),
        )
    }
}

impl RawCustomParameters {
    ////convert into the parsed params for a top-level font
    fn to_custom_params(&self) -> Result<CustomParameters, Error> {
        let mut params = CustomParameters::default();
        let mut virtual_masters = Vec::<BTreeMap<String, OrderedFloat<f64>>>::new();
        // PANOSE custom parameter is accessible under a short name and a long name:
        //     https://github.com/googlefonts/glyphsLib/blob/050ef62c/Lib/glyphsLib/builder/custom_params.py#L322-L323
        // ...with the value under the short name taking precendence:
        //     https://github.com/googlefonts/glyphsLib/blob/050ef62c/Lib/glyphsLib/builder/custom_params.py#L258-L269
        let mut panose = None;
        let mut panose_old = None;

        for param in &self.0 {
            // Silently skip special parameters that were handled elsewhere
            if param.taken() {
                continue;
            }

            let name = &param.name;
            let value = &param.value;
            let disabled = &param.disabled;

            // we need to use a macro here because you can't pass the name of a field to a
            // function.
            macro_rules! add_and_report_issues {
                ($field:ident, $converter:path) => {{ add_and_report_issues!($field, $converter(value)) }};

                ($field:ident, $converter:path, into) => {{ add_and_report_issues!($field, $converter(value).map(Into::into)) }};

                ($field:ident, $value_expr:expr_2021) => {{
                    let value = $value_expr;

                    if value.is_none() {
                        log::warn!("failed to parse param for '{}'", stringify!($field));
                    }
                    if params.$field.is_some() {
                        log::warn!("duplicate param value for field '{}'", stringify!($field));
                    }
                    params.$field = value;
                }};
            }

            if *disabled == Some(true) {
                log::debug!("skipping disabled custom param '{name}'");
                continue;
            }
            match name.as_str() {
                "Propagate Anchors" => add_and_report_issues!(propagate_anchors, Plist::as_bool),
                "Use Typo Metrics" => add_and_report_issues!(use_typo_metrics, Plist::as_bool),
                // <https://github.com/googlefonts/glyphsLib/blob/52c982399ba20dc96a2c2195df6fc6cea1f9a906/Lib/glyphsLib/builder/custom_params.py#L356>
                "postscriptIsFixedPitch" | "isFixedPitch" => {
                    add_and_report_issues!(is_fixed_pitch, Plist::as_bool)
                }
                "Has WWS Names" => add_and_report_issues!(has_wws_names, Plist::as_bool),
                "typoAscender" => add_and_report_issues!(typo_ascender, Plist::as_i64),
                "typoDescender" => add_and_report_issues!(typo_descender, Plist::as_i64),
                "typoLineGap" => add_and_report_issues!(typo_line_gap, Plist::as_i64),
                "winAscent" => add_and_report_issues!(win_ascent, Plist::as_i64),
                "winDescent" => add_and_report_issues!(win_descent, Plist::as_i64),
                "hheaAscender" => add_and_report_issues!(hhea_ascender, Plist::as_i64),
                "hheaDescender" => add_and_report_issues!(hhea_descender, Plist::as_i64),
                "hheaLineGap" => add_and_report_issues!(hhea_line_gap, Plist::as_i64),
                "vheaVertAscender" => add_and_report_issues!(vhea_ascender, Plist::as_i64),
                "vheaVertDescender" => add_and_report_issues!(vhea_descender, Plist::as_i64),
                "vheaVertLineGap" => add_and_report_issues!(vhea_line_gap, Plist::as_i64),
                "underlineThickness" => {
                    add_and_report_issues!(underline_thickness, Plist::as_ordered_f64)
                }
                "underlinePosition" => {
                    add_and_report_issues!(underline_position, Plist::as_ordered_f64)
                }
                "strikeoutPosition" => add_and_report_issues!(strikeout_position, Plist::as_i64),
                "strikeoutSize" => add_and_report_issues!(strikeout_size, Plist::as_i64),
                "subscriptXOffset" => add_and_report_issues!(subscript_x_offset, Plist::as_i64),
                "subscriptXSize" => add_and_report_issues!(subscript_x_size, Plist::as_i64),
                "subscriptYOffset" => add_and_report_issues!(subscript_y_offset, Plist::as_i64),
                "subscriptYSize" => add_and_report_issues!(subscript_y_size, Plist::as_i64),
                "superscriptXOffset" => add_and_report_issues!(superscript_x_offset, Plist::as_i64),
                "superscriptXSize" => add_and_report_issues!(superscript_x_size, Plist::as_i64),
                "superscriptYOffset" => add_and_report_issues!(superscript_y_offset, Plist::as_i64),
                "superscriptYSize" => add_and_report_issues!(superscript_y_size, Plist::as_i64),
                "fsType" => add_and_report_issues!(fs_type, Plist::as_fs_type),
                "unicodeRanges" | "openTypeOS2UnicodeRanges" => {
                    add_and_report_issues!(unicode_range_bits, Plist::as_unicode_code_ranges)
                }
                "codePageRanges" => {
                    add_and_report_issues!(codepage_range_bits, Plist::as_codepage_bits)
                }
                // these values are not listed in the glyphs.app UI, but exist
                // in some fonts:
                // https://github.com/googlefonts/fontc/pull/1144#issuecomment-2503134008
                "openTypeHeadLowestRecPPEM" => {
                    add_and_report_issues!(lowest_rec_ppem, Plist::as_i64)
                }
                "openTypeHheaCaretSlopeRun" => {
                    add_and_report_issues!(hhea_caret_slope_run, Plist::as_i64)
                }
                "openTypeHheaCaretSlopeRise" => {
                    add_and_report_issues!(hhea_caret_slope_rise, Plist::as_i64)
                }
                "openTypeHheaCaretOffset" => {
                    add_and_report_issues!(hhea_caret_offset, Plist::as_i64)
                }
                "openTypeVheaCaretSlopeRun" => {
                    add_and_report_issues!(vhea_caret_slope_run, Plist::as_i64)
                }
                "openTypeVheaCaretSlopeRise" => {
                    add_and_report_issues!(vhea_caret_slope_rise, Plist::as_i64)
                }
                "openTypeVheaCaretOffset" => {
                    add_and_report_issues!(vhea_caret_offset, Plist::as_i64)
                }
                "meta Table" => {
                    add_and_report_issues!(meta_table, MetaTableValues::from_plist)
                }
                "Don't use Production Names" => {
                    add_and_report_issues!(dont_use_production_names, Plist::as_bool)
                }
                // these might need to be handled? they're in the same list as
                // the items above:
                // https://github.com/googlefonts/glyphsLib/blob/74c63244fdb/Lib/glyphsLib/builder/custom_params.py#L429
                "openTypeOS2FamilyClass" | "openTypeHeadFlags" => {
                    log_once_warn!("unhandled custom param '{name}'")
                }

                "Virtual Master" => match value.as_virtual_master() {
                    Some(val) => virtual_masters.push(val),
                    None => log::warn!("failed to parse virtual master '{value:?}'"),
                },
                "panose" => panose = value.as_vec_of_ints(),
                "openTypeOS2Panose" => panose_old = value.as_vec_of_ints(),
                "glyphOrder" => add_and_report_issues!(glyph_order, Plist::as_vec_of_string),
                "gasp Table" => add_and_report_issues!(gasp_table, Plist::as_gasp_table),
                "Feature for Feature Variations" => {
                    add_and_report_issues!(feature_for_feature_variations, Plist::as_str, into)
                }
                _ => log_once_warn!("unknown custom parameter '{name}'"),
            }
        }
        params.panose = panose.or(panose_old);
        params.virtual_masters = Some(virtual_masters).filter(|x| !x.is_empty());
        Ok(params)
    }

    fn contains(&self, name: &str) -> bool {
        self.0.iter().any(|val| val.name == name)
    }

    /// Consume and return the first (non-disabled) parameter with the given name, if any.
    ///
    /// This is for parameters that are handled elsewhere before `to_custom_params` and
    /// shouldn't be stored in CustomParameters.
    fn take(&mut self, name: &str) -> Option<Plist> {
        let pos = self
            .0
            .iter()
            .position(|val| val.name == name && val.disabled != Some(true))?;

        // Replace with a sentinel value rather than removing it from the Vec to avoid
        // the overhead of shifting elements. This is skipped during `to_custom_params`
        let taken = std::mem::take(&mut self.0[pos]);

        Some(taken.value)
    }

    fn take_string(&mut self, name: &str) -> Option<String> {
        self.take(name)
            .and_then(|p| p.as_str().map(|s| s.to_owned()))
    }

    fn take_axes(&mut self) -> Option<Vec<Axis>> {
        self.take("Axes").and_then(|p| p.as_axes())
    }

    fn take_axis_mappings(&mut self) -> Option<Vec<AxisMapping>> {
        self.take("Axis Mappings")
            .and_then(|p| p.as_axis_mappings())
    }

    fn take_axis_locations(&mut self) -> Option<Vec<AxisLocation>> {
        self.take("Axis Location")
            .and_then(|p| p.as_axis_locations())
    }
}

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash)]
pub struct AxisLocation {
    axis_name: String,
    location: OrderedFloat<f64>,
}

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash)]
pub struct AxisMapping {
    //TODO this should really be a `Tag`?
    tag: String,
    user_to_design: Vec<(OrderedFloat<f64>, OrderedFloat<f64>)>,
}

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash, FromPlist)]
struct RawMetric {
    // So named to let FromPlist populate it from a field called "type"
    type_: String,
}

#[derive(Default, Clone, Debug, PartialEq, Eq, Hash, FromPlist)]
struct RawName {
    key: String,
    value: Option<String>,
    values: Vec<RawNameValue>,
}

/// Compute a priority score for a language code when selecting default names.
///
/// Lower scores have higher priority.
/// Priority order: dflt > default > ENG > others (by insertion order)
fn default_language_score(lang: &str, index: usize, scale: i32) -> i32 {
    match lang {
        "dflt" => -scale * 3 - index as i32,
        "default" => -scale * 2 - index as i32,
        "ENG" => -scale - index as i32,
        _ => index as i32,
    }
}

impl RawName {
    fn is_empty(&self) -> bool {
        self.value.is_none() && self.values.is_empty()
    }

    fn get_value(&self) -> Option<&str> {
        if let Some(value) = &self.value {
            return Some(value.as_str());
        }

        // This is a localized (multivalued) name. Pick a winner.
        // See <https://github.com/googlefonts/fontc/issues/1011>
        // In order of preference: dflt, default, ENG, whatever is first
        // <https://github.com/googlefonts/glyphsLib/blob/1cb4fc5ae2cf385df95d2b7768e7ab4eb60a5ac3/Lib/glyphsLib/classes.py#L3155-L3161>
        // If the same name is defined repeatedly, e.g. dflt has 2 values, pick the last occurrence.

        let scale = self.values.len() as i32;
        self.values
            .iter()
            .enumerate()
            .map(|(i, raw)| {
                (
                    default_language_score(raw.language.as_str(), i, scale),
                    raw.value.as_str(),
                )
            })
            .min()
            .map(|(_, raw)| raw)
    }

    fn localized_values(&self) -> impl Iterator<Item = (&str, &str)> {
        self.values
            .iter()
            .map(|raw| (raw.language.as_str(), raw.value.as_str()))
    }
}

#[derive(Default, Clone, Debug, PartialEq, Eq, Hash, FromPlist)]
struct RawNameValue {
    language: String,
    value: String,
}

#[derive(Default, Clone, Debug, PartialEq, Eq, Hash, FromPlist)]
struct RawFeature {
    automatic: Option<i64>,
    disabled: Option<i64>,
    name: Option<String>,
    tag: Option<String>,
    notes: Option<String>,
    code: String,
    labels: Vec<RawNameValue>,

    #[fromplist(ignore)]
    other_stuff: BTreeMap<String, Plist>,
}

#[derive(Default, Clone, Debug, PartialEq, Eq, Hash, FromPlist)]
pub struct Axis {
    #[fromplist(alt_name = "Name")]
    pub name: String,
    #[fromplist(alt_name = "Tag")]
    pub tag: String,
    pub hidden: Option<bool>,
}

#[derive(Default, Clone, Debug, PartialEq, FromPlist)]
struct RawGlyph {
    layers: Vec<RawLayer>,
    glyphname: SmolStr,
    export: Option<bool>,
    #[fromplist(alt_name = "leftKerningGroup")]
    kern_left: Option<SmolStr>,
    #[fromplist(alt_name = "rightKerningGroup")]
    kern_right: Option<SmolStr>,
    unicode: Option<String>,
    category: Option<SmolStr>,
    sub_category: Option<SmolStr>,
    #[fromplist(alt_name = "production")]
    production_name: Option<SmolStr>,
    parts_settings: Vec<RawPartSetting>,
    #[fromplist(ignore)]
    other_stuff: BTreeMap<String, Plist>,
}

#[derive(Default, Clone, Debug, PartialEq, FromPlist)]
struct RawPartSetting {
    name: SmolStr,
    bottom_name: Option<SmolStr>,
    bottom_value: i64,
    top_name: Option<SmolStr>,
    top_value: i64,
}

#[derive(Default, Clone, Debug, PartialEq, FromPlist)]
struct RawLayer {
    name: String,
    layer_id: String,
    associated_master_id: Option<String>,
    width: Option<OrderedFloat<f64>>,
    vert_width: Option<OrderedFloat<f64>>,
    vert_origin: Option<OrderedFloat<f64>>,
    shapes: Vec<RawShape>,
    paths: Vec<Path>,
    components: Vec<Component>,
    anchors: Vec<RawAnchor>,
    #[fromplist(alt_name = "attr")]
    attributes: LayerAttributes,
    // if this layer is part of a smart component; values should be 1 or 2
    // (this is validated when we convert to Layer)
    part_selection: BTreeMap<SmolStr, i64>,
    // glyphs2 only?
    user_data: LayerUserData,
    #[fromplist(ignore)]
    other_stuff: BTreeMap<String, Plist>,
}

#[derive(Default, Clone, Debug, PartialEq, FromPlist)]
struct LayerUserData {
    #[fromplist(alt_name = "PartSelection")]
    part_selection: BTreeMap<SmolStr, i64>,
}

impl RawLayer {
    /// Return true if the layer is a draft that is not meant to be compiled.
    ///
    /// The presence of an associated master indicates this is not a simple 'master' instance.
    /// Without 'attributes' that specify whether it's a special intermediate, alternate or
    /// color layer, we can assume the non-master layer is a draft.
    fn is_draft(&self) -> bool {
        self.associated_master_id.is_some()
            && self.attributes == Default::default()
            && self.part_selection.is_empty() // v3
            && self.user_data.part_selection.is_empty() // v2
    }

    /// Glyphs uses the concept of 'bracket layers' to represent GSUB feature variations.
    ///
    /// See <https://glyphsapp.com/learn/switching-shapes#g-1-alternate-layers-bracket-layers>
    /// for more information.
    fn is_bracket_layer(&self, format_version: FormatVersion) -> bool {
        // can't be a bracket without an associated_master_id
        //https://github.com/googlefonts/glyphsLib/blob/c4db6b981d57/Lib/glyphsLib/builder/builders.py#L270
        self.associated_master_id.is_some()
            && self.associated_master_id.as_ref() != Some(&self.layer_id)
            //https://github.com/googlefonts/glyphsLib/blob/c4db6b981d5/Lib/glyphsLib/classes.py#L3942
            && match format_version {
                FormatVersion::V2 => AxisRule::from_layer_name(&self.name).is_some(),
                FormatVersion::V3 => !self.attributes.axis_rules.is_empty(),
            }
    }

    fn v2_to_v3_attributes(&mut self) {
        // In Glyphs v2, 'brace' or intermediate layer coordinates are stored in the
        // layer name as comma-separated values inside braces
        let mut brace_coordinates = Vec::new();
        if let (Some(start), Some(end)) = (self.name.find('{'), self.name.find('}')) {
            let mut tokenizer = Tokenizer::new(&self.name[start..=end]);
            // we don't want this to fail, technically '{foobar}' is valid inside a
            // layer name which is not meant to specify intermediate coordinates.
            // Typos are also possible. Perhaps we should warn?
            brace_coordinates = tokenizer
                .parse_delimited_vec(VecDelimiters::CSV_IN_BRACES)
                .unwrap_or_default();
        }
        if !brace_coordinates.is_empty() {
            self.attributes.coordinates = brace_coordinates;
        }
        // TODO: handle 'bracket' layers and other attributes
    }
}

/// Represents a path OR a component
///
/// <https://github.com/schriftgestalt/GlyphsSDK/blob/Glyphs3/GlyphsFileFormat/GlyphsFileFormatv3.md#differences-between-version-2>
#[derive(Default, Clone, Debug, PartialEq, FromPlist)]
struct RawShape {
    // TODO: add numerous unsupported attributes

    // When I'm a path
    closed: Option<bool>,
    nodes: Vec<Node>,

    // When I'm a component I specifically want all my attributes to end up in other_stuff
    // My Component'ness can be detected by presence of a ref (Glyphs3) or name(Glyphs2) attribute
    // ref is reserved so take advantage of alt names
    #[fromplist(alt_name = "ref", alt_name = "name")]
    glyph_name: Option<SmolStr>,
    // if this is a reference to a smart component, this is the location in the
    // mini designspace of the component instance (user coords).
    piece: BTreeMap<SmolStr, f64>,

    // for components, an optional name to rename an anchor
    // on the target glyph during anchor propagation
    anchor: Option<SmolStr>,
    transform: Option<String>, // v2
    pos: Vec<f64>,             // v3
    angle: Option<f64>,        // v3
    scale: Vec<f64>,           // v3

    #[fromplist(alt_name = "attr")]
    attributes: ShapeAttributes,
}

/// <https://github.com/schriftgestalt/GlyphsSDK/blob/Glyphs3/GlyphsFileFormat/GlyphsFileFormatv3.md#spec-glyphs-3-path>
#[derive(Default, Clone, Debug, PartialEq, Eq, Hash, FromPlist)]
pub struct Path {
    pub closed: bool,
    pub nodes: Vec<Node>,
    pub attributes: ShapeAttributes,
}

/// <https://github.com/schriftgestalt/GlyphsSDK/blob/Glyphs3/GlyphsFileFormat/GlyphsFileFormatv3.md#spec-glyphs-3-component>
#[derive(Default, Clone, Debug, FromPlist)]
pub struct Component {
    /// The glyph this component references
    pub name: SmolStr,
    /// meh
    pub transform: Affine,
    /// An alternative anchor name used during anchor propagation
    ///
    /// For instance, if an acute accent is a component of a ligature glyph,
    /// we might rename its 'top' anchor to 'top_2'
    pub anchor: Option<SmolStr>,
    /// For smart components, the location in the mini designspace of the instance.
    ///
    /// Keys should reference the axes in the component, with the value being
    /// a position in user coords.
    #[fromplist(alt_name = "piece")]
    pub smart_component_values: BTreeMap<SmolStr, f64>,
    pub attributes: ShapeAttributes,
}

impl PartialEq for Component {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name
            && Into::<AffineForEqAndHash>::into(self.transform) == other.transform.into()
    }
}

impl Eq for Component {}

impl Hash for Component {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state);
        Into::<AffineForEqAndHash>::into(self.transform).hash(state);
    }
}

#[derive(Clone, Debug)]
pub struct Node {
    pub pt: Point,
    pub node_type: NodeType,
}

impl Node {
    pub fn is_on_curve(&self) -> bool {
        !matches!(self.node_type, NodeType::OffCurve)
    }
}

impl PartialEq for Node {
    fn eq(&self, other: &Self) -> bool {
        Into::<PointForEqAndHash>::into(self.pt) == other.pt.into()
            && self.node_type == other.node_type
    }
}

impl Eq for Node {}

impl Hash for Node {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        PointForEqAndHash::new(self.pt).hash(state);
        self.node_type.hash(state);
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum NodeType {
    Line,
    LineSmooth,
    OffCurve,
    Curve,
    CurveSmooth,
    QCurve,
    QCurveSmooth,
}

#[derive(Default, Clone, Debug, PartialEq, FromPlist)]
struct RawAnchor {
    name: SmolStr,
    pos: Option<Point>,       // v3
    position: Option<String>, // v2
}

#[derive(Clone, Debug, PartialEq)]
pub struct Anchor {
    pub name: SmolStr,
    pub pos: Point,
}

impl Anchor {
    pub(crate) fn is_origin(&self) -> bool {
        self.name == "*origin"
    }

    pub(crate) fn origin_delta(&self) -> Option<Vec2> {
        self.is_origin().then_some(self.pos.to_vec2())
    }
}

impl Hash for Anchor {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.name.hash(state);
        PointForEqAndHash::new(self.pos).hash(state);
    }
}

#[derive(Clone, Debug, PartialEq, Hash)]
pub struct FontMaster {
    pub id: String,
    pub name: String,
    pub axes_values: Vec<OrderedFloat<f64>>,
    metric_values: BTreeMap<String, MetricValue>,
    pub number_values: BTreeMap<SmolStr, OrderedFloat<f64>>,
    pub custom_parameters: CustomParameters,
    pub user_data: BTreeMap<SmolStr, Plist>,
}

impl FontMaster {
    fn read_metric(&self, metric_name: &str) -> Option<f64> {
        self.metric_values
            .get(metric_name)
            .map(|metric| metric.pos.into_inner())
    }

    pub fn ascender(&self) -> Option<f64> {
        self.read_metric("ascender")
    }

    pub fn descender(&self) -> Option<f64> {
        self.read_metric("descender")
    }

    pub fn x_height(&self) -> Option<f64> {
        self.read_metric("x-height")
    }

    pub fn cap_height(&self) -> Option<f64> {
        self.read_metric("cap height")
    }

    pub fn italic_angle(&self) -> Option<f64> {
        self.read_metric("italic angle")
    }
}

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash, FromPlist)]
struct RawFontMaster {
    id: String,
    name: Option<String>,

    weight: Option<String>,
    width: Option<String>,
    custom: Option<String>,

    weight_value: Option<OrderedFloat<f64>>,
    interpolation_weight: Option<OrderedFloat<f64>>,

    width_value: Option<OrderedFloat<f64>>,
    interpolation_width: Option<OrderedFloat<f64>>,

    custom_value: Option<OrderedFloat<f64>>,

    typo_ascender: Option<i64>,
    typo_descender: Option<OrderedFloat<f64>>,
    typo_line_gap: Option<OrderedFloat<f64>>,
    win_ascender: Option<OrderedFloat<f64>>,
    win_descender: Option<OrderedFloat<f64>>,

    axes_values: Vec<OrderedFloat<f64>>,
    metric_values: Vec<RawMetricValue>, // v3

    ascender: Option<OrderedFloat<f64>>,   // v2
    baseline: Option<OrderedFloat<f64>>,   // v2
    descender: Option<OrderedFloat<f64>>,  // v2
    cap_height: Option<OrderedFloat<f64>>, // v2
    x_height: Option<OrderedFloat<f64>>,   // v2
    #[fromplist(alt_name = "italic angle")]
    italic_angle: Option<OrderedFloat<f64>>, // v2

    alignment_zones: Vec<String>, // v2

    custom_parameters: RawCustomParameters,
    number_values: Vec<OrderedFloat<f64>>,
    user_data: BTreeMap<SmolStr, Plist>,

    #[fromplist(ignore)]
    other_stuff: BTreeMap<String, Plist>,
}

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash, FromPlist)]
struct RawMetricValue {
    pos: Option<OrderedFloat<f64>>,
    over: Option<OrderedFloat<f64>>,
}

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash)]
pub struct MetricValue {
    pos: OrderedFloat<f64>,
    over: OrderedFloat<f64>,
}

impl From<RawMetricValue> for MetricValue {
    fn from(src: RawMetricValue) -> MetricValue {
        MetricValue {
            pos: src.pos.unwrap_or_default(),
            over: src.over.unwrap_or_default(),
        }
    }
}

#[derive(Clone, Debug, PartialEq, Hash)]
pub struct Instance {
    pub name: String,
    pub active: bool,
    // So named to let FromPlist populate it from a field called "type"
    pub type_: InstanceType,
    pub axis_mappings: BTreeMap<String, AxisUserToDesignMap>,
    pub axes_values: Vec<OrderedFloat<f64>>,
    pub custom_parameters: CustomParameters,
    properties: Vec<RawName>, // used for name resolution
}

/// <https://github.com/googlefonts/glyphsLib/blob/6f243c1f732ea1092717918d0328f3b5303ffe56/Lib/glyphsLib/classes.py#L150>
#[derive(Clone, Debug, PartialEq, Hash)]
pub enum InstanceType {
    Single,
    Variable,
}

impl From<&str> for InstanceType {
    fn from(value: &str) -> Self {
        if value.eq_ignore_ascii_case("variable") {
            InstanceType::Variable
        } else {
            InstanceType::Single
        }
    }
}

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash, FromPlist)]
struct RawInstance {
    name: String,
    exports: Option<i64>,
    active: Option<i64>,
    type_: Option<String>,
    axes_values: Vec<OrderedFloat<f64>>,

    weight_value: Option<OrderedFloat<f64>>,
    interpolation_weight: Option<OrderedFloat<f64>>,

    width_value: Option<OrderedFloat<f64>>,
    interpolation_width: Option<OrderedFloat<f64>>,

    custom_value: Option<OrderedFloat<f64>>,

    weight_class: Option<String>,
    width_class: Option<String>,
    properties: Vec<RawName>,
    custom_parameters: RawCustomParameters,
}

impl RawInstance {
    /// Per glyphsLib both "exports=0" and "active=0" mean inactive
    /// <https://github.com/googlefonts/glyphsLib/blob/6f243c1f732ea1092717918d0328f3b5303ffe56/Lib/glyphsLib/builder/axes.py#L637>
    fn is_active(&self) -> bool {
        self.exports.unwrap_or(1) != 0 && self.active.unwrap_or(1) != 0
    }
}

trait GlyphsV2OrderedAxes {
    fn weight_value(&self) -> Option<OrderedFloat<f64>>;
    fn interpolation_weight(&self) -> Option<OrderedFloat<f64>>;
    fn width_value(&self) -> Option<OrderedFloat<f64>>;
    fn interpolation_width(&self) -> Option<OrderedFloat<f64>>;
    fn custom_value(&self) -> Option<OrderedFloat<f64>>;

    fn value_for_nth_axis(&self, nth_axis: usize) -> Result<OrderedFloat<f64>, Error> {
        // Per https://github.com/googlefonts/fontmake-rs/pull/42#pullrequestreview-1211619812
        // the field to use is based on the order in axes NOT the tag.
        // That is, whatever the first axis is, it's value is in the weightValue field. Long sigh.
        // Defaults per https://github.com/googlefonts/fontmake-rs/pull/42#discussion_r1044415236.
        // v2 instances use novel field names so send back several for linear probing.
        Ok(match nth_axis {
            0 => self
                .weight_value()
                .or(self.interpolation_weight())
                .unwrap_or(100.0.into()),
            1 => self
                .width_value()
                .or(self.interpolation_width())
                .unwrap_or(100.0.into()),
            2 => self.custom_value().unwrap_or(0.0.into()),
            _ => {
                return Err(Error::StructuralError(format!(
                    "We don't know what field to use for axis {nth_axis}"
                )));
            }
        })
    }

    fn axis_values(&self, axes: &[Axis]) -> Result<Vec<OrderedFloat<f64>>, Error> {
        (0..axes.len())
            .map(|nth_axis| self.value_for_nth_axis(nth_axis))
            .collect::<Result<Vec<OrderedFloat<f64>>, Error>>()
    }
}

impl GlyphsV2OrderedAxes for RawFontMaster {
    fn weight_value(&self) -> Option<OrderedFloat<f64>> {
        self.weight_value
    }

    fn interpolation_weight(&self) -> Option<OrderedFloat<f64>> {
        self.interpolation_weight
    }

    fn width_value(&self) -> Option<OrderedFloat<f64>> {
        self.width_value
    }

    fn interpolation_width(&self) -> Option<OrderedFloat<f64>> {
        self.interpolation_width
    }

    fn custom_value(&self) -> Option<OrderedFloat<f64>> {
        self.custom_value
    }
}

impl GlyphsV2OrderedAxes for RawInstance {
    fn weight_value(&self) -> Option<OrderedFloat<f64>> {
        self.weight_value
    }

    fn interpolation_weight(&self) -> Option<OrderedFloat<f64>> {
        self.interpolation_weight
    }

    fn width_value(&self) -> Option<OrderedFloat<f64>> {
        self.width_value
    }

    fn interpolation_width(&self) -> Option<OrderedFloat<f64>> {
        self.interpolation_width
    }

    fn custom_value(&self) -> Option<OrderedFloat<f64>> {
        self.custom_value
    }
}

fn parse_node_from_string(value: &str) -> Node {
    let mut spl = value.splitn(3, ' ');
    let x = spl.next().unwrap().parse().unwrap();
    let y = spl.next().unwrap().parse().unwrap();
    let pt = Point::new(x, y);
    let mut raw_node_type = spl.next().unwrap();
    // drop the userData dict, we don't use it for compilation
    if raw_node_type.contains('{') {
        raw_node_type = raw_node_type.split('{').next().unwrap().trim_end();
    }
    let node_type = raw_node_type.parse().unwrap();
    Node { pt, node_type }
}

fn parse_node_from_tokenizer(tokenizer: &mut Tokenizer<'_>) -> Result<Node, crate::plist::Error> {
    // (x,y,type)
    let x: f64 = tokenizer.parse()?;
    tokenizer.eat(b',')?;
    let y: f64 = tokenizer.parse()?;
    tokenizer.eat(b',')?;
    let node_type: String = tokenizer.parse()?;
    let node_type = NodeType::from_str(&node_type)
        .map_err(|_| crate::plist::Error::Parse(format!("unknown node type '{node_type}'")))?;

    // Sometimes there is userData; ignore it
    if tokenizer.eat(b',').is_ok() {
        tokenizer.skip_rec()?;
    }

    Ok(Node {
        pt: Point { x, y },
        node_type,
    })
}

impl std::str::FromStr for NodeType {
    type Err = String;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            // Glyphs 2 style
            "LINE" => Ok(NodeType::Line),
            "LINE SMOOTH" => Ok(NodeType::LineSmooth),
            "OFFCURVE" => Ok(NodeType::OffCurve),
            "CURVE" => Ok(NodeType::Curve),
            "CURVE SMOOTH" => Ok(NodeType::CurveSmooth),
            "QCURVE" => Ok(NodeType::QCurve),
            "QCURVE SMOOTH" => Ok(NodeType::QCurveSmooth),
            // Glyphs 3 style
            "l" => Ok(NodeType::Line),
            "ls" => Ok(NodeType::LineSmooth),
            "o" => Ok(NodeType::OffCurve),
            "c" => Ok(NodeType::Curve),
            "cs" => Ok(NodeType::CurveSmooth),
            "q" => Ok(NodeType::QCurve),
            "qs" => Ok(NodeType::QCurveSmooth),
            _ => Err(format!("unknown node type {s}")),
        }
    }
}

// Hand-parse Node because it doesn't follow the normal structure
impl FromPlist for Node {
    fn parse(tokenizer: &mut Tokenizer<'_>) -> Result<Self, crate::plist::Error> {
        use crate::plist::Error;
        let tok = tokenizer.lex()?;
        let node = match &tok {
            Token::Atom(value) => parse_node_from_string(value),
            Token::String(value) => parse_node_from_string(value),
            Token::OpenParen => {
                let node = parse_node_from_tokenizer(tokenizer)?;
                tokenizer.eat(b')')?;
                node
            }
            _ => return Err(Error::ExpectedString),
        };
        Ok(node)
    }
}

impl Path {
    pub fn new(closed: bool) -> Path {
        Path {
            nodes: Vec::new(),
            closed,
            ..Default::default()
        }
    }

    pub fn add(&mut self, pt: impl Into<Point>, node_type: NodeType) {
        let pt = pt.into();
        self.nodes.push(Node { pt, node_type });
    }

    /// Rotate left by one, placing the first point at the end. This is because
    /// it's what glyphs seems to expect.
    pub fn rotate_left(&mut self, delta: usize) {
        self.nodes.rotate_left(delta);
    }

    pub fn reverse(&mut self) {
        self.nodes.reverse();
    }
}

fn v2_to_v3_name(v2_prop: Option<&str>, v3_name: &str) -> Option<RawName> {
    // https://github.com/schriftgestalt/GlyphsSDK/blob/Glyphs3/GlyphsFileFormat/GlyphsFileFormatv3.md#properties
    // Keys ending with "s" are localizable that means the second key is values
    v2_prop.map(|value| {
        if v3_name.ends_with('s') {
            RawName {
                key: v3_name.into(),
                value: None,
                values: vec![RawNameValue {
                    language: "dflt".into(),
                    value: value.to_string(),
                }],
            }
        } else {
            RawName {
                key: v3_name.into(),
                value: Some(value.to_string()),
                values: vec![],
            }
        }
    })
}

impl RawFont {
    pub fn load_from_string(raw_content: &str) -> Result<Self, crate::plist::Error> {
        let raw_content = preprocess_unparsed_plist(raw_content);
        Self::parse_plist(&raw_content)
    }

    pub fn load(glyphs_file: &path::Path) -> Result<Self, Error> {
        if glyphs_file.extension() == Some(OsStr::new("glyphspackage")) {
            return Self::load_package(glyphs_file);
        }

        debug!("Read glyphs {glyphs_file:?}");
        let raw_content = fs::read_to_string(glyphs_file).map_err(Error::IoError)?;
        Self::load_from_string(&raw_content)
            .map_err(|e| Error::ParseError(glyphs_file.to_path_buf(), e.to_string()))
    }

    /// load from a .glyphspackage
    fn load_package(glyphs_package: &path::Path) -> Result<RawFont, Error> {
        if !glyphs_package.is_dir() {
            return Err(Error::NotAGlyphsPackage(glyphs_package.to_path_buf()));
        }
        debug!("Read glyphs package {glyphs_package:?}");

        let fontinfo_file = glyphs_package.join("fontinfo.plist");
        let fontinfo_data = fs::read_to_string(&fontinfo_file).map_err(Error::IoError)?;
        let mut raw_font = RawFont::parse_plist(&fontinfo_data)
            .map_err(|e| Error::ParseError(fontinfo_file.to_path_buf(), format!("{e}")))?;

        let mut glyphs: HashMap<SmolStr, RawGlyph> = HashMap::new();
        let glyphs_dir = glyphs_package.join("glyphs");
        if glyphs_dir.is_dir() {
            for entry in fs::read_dir(glyphs_dir).map_err(Error::IoError)? {
                let entry = entry.map_err(Error::IoError)?;
                let path = entry.path();
                if path.extension() == Some(OsStr::new("glyph")) {
                    let glyph_data = fs::read_to_string(&path).map_err(Error::IoError)?;
                    let glyph_data = preprocess_unparsed_plist(&glyph_data);
                    let glyph = RawGlyph::parse_plist(&glyph_data)
                        .map_err(|e| Error::ParseError(path.clone(), e.to_string()))?;
                    if glyph.glyphname.is_empty() {
                        return Err(Error::ParseError(
                            path.clone(),
                            "Glyph dict must have a 'glyphname' key".to_string(),
                        ));
                    }
                    glyphs.insert(glyph.glyphname.clone(), glyph);
                }
            }
        }

        // if order.plist file exists, read it and sort glyphs in it accordingly
        let order_file = glyphs_package.join("order.plist");
        let mut ordered_glyphs = Vec::new();
        if order_file.exists() {
            let order_data = fs::read_to_string(&order_file).map_err(Error::IoError)?;
            let order_plist = Plist::parse(&order_data)
                .map_err(|e| Error::ParseError(order_file.to_path_buf(), e.to_string()))?;
            let order = order_plist
                .expect_array()
                .map_err(|e| Error::ParseError(order_file.to_path_buf(), e.to_string()))?;
            for glyph_name in order {
                let glyph_name = glyph_name
                    .expect_string()
                    .map_err(|e| Error::ParseError(order_file.to_path_buf(), e.to_string()))?;
                if let Some(glyph) = glyphs.remove(glyph_name.as_str()) {
                    ordered_glyphs.push(glyph);
                }
            }
        }
        // sort the glyphs not in order.plist by their name
        let mut glyph_names: Vec<_> = glyphs.keys().cloned().collect();
        glyph_names.sort();
        ordered_glyphs.extend(
            glyph_names
                .into_iter()
                .map(|glyph_name| glyphs.remove(&glyph_name).unwrap()),
        );
        assert!(glyphs.is_empty());
        raw_font.glyphs = ordered_glyphs;

        // ignore UIState.plist which stuff like displayStrings that are not used by us

        Ok(raw_font)
    }

    fn v2_to_v3_axes(&mut self) -> Result<Vec<String>, Error> {
        let mut tags = Vec::new();
        if let Some(v2_axes) = self.custom_parameters.take_axes() {
            for v2_axis in v2_axes {
                tags.push(v2_axis.tag.clone());
                self.axes.push(v2_axis.clone());
            }
        }

        // Match the defaults from https://github.com/googlefonts/glyphsLib/blob/f6e9c4a29ce764d34c309caef5118c48c156be36/Lib/glyphsLib/builder/axes.py#L526
        // if we have nothing
        if self.axes.is_empty() {
            self.axes.push(Axis {
                name: "Weight".into(),
                tag: "wght".into(),
                hidden: None,
            });
            self.axes.push(Axis {
                name: "Width".into(),
                tag: "wdth".into(),
                hidden: None,
            });
            self.axes.push(Axis {
                name: "Custom".into(),
                tag: "XXXX".into(),
                hidden: None,
            });
        }

        if self.axes.len() > 3 {
            return Err(Error::StructuralError(
                "We only understand 0..3 axes for Glyphs v2".into(),
            ));
        }

        // v2 stores values for axes in specific fields, find them and put them into place
        // "Axis position related properties (e.g. weightValue, widthValue, customValue) have been replaced by the axesValues list which is indexed in parallel with the toplevel axes list."
        for master in self.font_master.iter_mut() {
            master.axes_values = master.axis_values(&self.axes)?;
        }
        for instance in self.instances.iter_mut() {
            instance.axes_values = instance.axis_values(&self.axes)?;
        }

        Ok(tags)
    }

    fn v2_to_v3_metrics(&mut self) -> Result<(), Error> {
        // setup storage for the basic metrics
        self.metrics = V3_METRIC_NAMES
            .iter()
            .map(|n| RawMetric {
                type_: n.to_string(),
            })
            .collect();

        let mut used_metrics = [false; 6];
        // first which metric occurs in any master:
        for m in &self.font_master {
            for (i, val) in [
                m.ascender,
                m.baseline,
                m.descender,
                m.cap_height,
                m.x_height,
                m.italic_angle,
            ]
            .iter()
            .enumerate()
            {
                used_metrics[i] |= val.is_some();
            }
        }

        // add only used metrics to the metric list
        self.metrics = V3_METRIC_NAMES
            .into_iter()
            .zip(used_metrics)
            .filter(|(_, used)| *used)
            .map(|(name, _)| RawMetric {
                type_: name.to_string(),
            })
            .collect();

        let mut non_metric_alignment_zones = vec![vec![]; self.font_master.len()];

        // in each font master setup the parallel array
        for (i, master) in self.font_master.iter_mut().enumerate() {
            // each master has to have an entry for each defined metric,
            // even if it is 'None', and they need to be in the same order
            // as in the self.metrics list.
            let mut metric_values = vec![RawMetricValue::default(); self.metrics.len()];
            for (metric_idx, name) in self.metrics.iter().enumerate() {
                let value = match name.type_.as_ref() {
                    "ascender" => master.ascender,
                    "baseline" => master.baseline,
                    "descender" => master.descender,
                    "cap height" => master.cap_height,
                    "x-height" => master.x_height,
                    "italic angle" => master.italic_angle,
                    _ => unreachable!("only these values exist in V3_METRIC_NAMES"),
                };
                metric_values[metric_idx].pos = value;
            }

            // "alignmentZones is now a set of over (overshoot) properties attached to metrics"
            for alignment_zone in &master.alignment_zones {
                let Some((pos, over)) = parse_alignment_zone(alignment_zone) else {
                    warn!("Confusing alignment zone '{alignment_zone}', skipping");
                    continue;
                };

                // skip zero-height zones
                if over == 0. {
                    continue;
                }

                // special handling for this; we assume it's the baseline, and
                // we know where that is in our vec (and it has pos: None, set
                // above)
                if pos == 0. {
                    if let Some((idx, _)) = self
                        .metrics
                        .iter()
                        .enumerate()
                        .find(|(_, name)| name.type_ == "baseline")
                    {
                        metric_values[idx].over = Some(over);
                    }
                    continue;
                }

                // now look for a metric that has the same position as this zone
                // this is quadratic but N is small
                if let Some(metric) = metric_values.iter_mut().find(|x| x.pos == Some(pos)) {
                    metric.over = Some(over);
                } else {
                    non_metric_alignment_zones[i].push((pos, over))
                }
            }
            master.metric_values = metric_values;
        }

        // now handle any non-metric alignment zones, converting them to metrics.
        // first we assign a name to each unique position:
        let mut new_metrics = HashMap::new();
        for pos in non_metric_alignment_zones
            .iter()
            .flat_map(|master| master.iter().map(|(pos, _)| *pos))
        {
            if !new_metrics.contains_key(&pos) {
                let next_zone = new_metrics.len() + 1;
                let idx = self.metrics.len();
                self.metrics.push(RawMetric {
                    type_: format!("zone {next_zone}"),
                });
                new_metrics.insert(pos, idx);
            }
        }

        // flip our map, so it's ordered on index:
        let new_metrics: BTreeMap<_, _> = new_metrics.into_iter().map(|(k, v)| (v, k)).collect();

        // then for each master, add a metric value for each newly named metric
        for (idx, metrics) in non_metric_alignment_zones.into_iter().enumerate() {
            for pos_to_add in new_metrics.values().copied() {
                let to_add = metrics.iter().copied().find_map(|(pos, over)| {
                    (pos == pos_to_add).then_some(RawMetricValue {
                        pos: Some(pos),
                        over: Some(over),
                    })
                });

                self.font_master[idx]
                    .metric_values
                    .push(to_add.unwrap_or_default());
            }
        }
        Ok(())
    }

    fn v2_to_v3_master_names(&mut self) -> Result<(), Error> {
        // in Glyphs 2, masters don't have a single 'name' attribute, but rather
        // a concatenation of three other optional attributes weirdly called
        // 'width', 'weight' and 'custom' (in exactly this order).
        // The first two can only contain few predefined values, the last one is
        // residual and free-form. They default to 'Regular' when omitted in
        // the source. See:
        // https://github.com/schriftgestalt/GlyphsSDK/blob/Glyphs3/GlyphsFileFormat/GlyphsFileFormatv2.md
        // https://github.com/googlefonts/glyphsLib/blob/9d5828d/Lib/glyphsLib/classes.py#L1700-L1711
        for master in self.font_master.iter_mut() {
            // Even though glyphs2 masters don't officially have a 'name' attribute,
            // some glyphs2 sources produced by more recent versions of Glyphs
            // sometimes have it (unclear exactly when or from which version on).
            // We keep the 'name' attribute as is, instead of generating a one.
            if master.name.is_some() {
                continue;
            }

            // Remove Nones, empty strings and redundant occurrences of 'Regular'
            let mut names = [
                master.width.as_deref(),
                master.weight.as_deref(),
                master.custom.as_deref(),
            ]
            .iter()
            .flatten()
            .flat_map(|n| n.split_ascii_whitespace())
            .filter(|x| *x != "Regular")
            .collect::<Vec<_>>();

            // append "Italic" if italic angle != 0
            if let Some(italic_angle) = master.italic_angle
                && italic_angle != 0.0
                && (names.is_empty()
                    || !names
                        .iter()
                        .any(|name| *name == "Italic" || *name == "Oblique"))
            {
                names.push("Italic");
            }
            // if all are empty, default to "Regular"
            master.name = if names.is_empty() {
                Some("Regular".into())
            } else {
                Some(names.join(" "))
            };
        }
        Ok(())
    }

    fn v2_to_v3_names(&mut self) -> Result<(), Error> {
        // The copyright, designer, designerURL, manufacturer, manufacturerURL top-level entries
        // have been moved into new top-level properties dictionary and made localizable.
        // Take properties to avoid incompatible borrowing against self
        let mut properties = std::mem::take(&mut self.properties);

        properties.extend(v2_to_v3_name(self.copyright.as_deref(), "copyrights"));
        properties.extend(v2_to_v3_name(self.designer.as_deref(), "designers"));
        properties.extend(v2_to_v3_name(self.designerURL.as_deref(), "designerURL"));
        properties.extend(v2_to_v3_name(self.manufacturer.as_deref(), "manufacturers"));
        properties.extend(v2_to_v3_name(
            self.manufacturerURL.as_deref(),
            "manufacturerURL",
        ));

        // glyphsLib tries both long and short names, with short names taking precedence
        //https://github.com/googlefonts/glyphsLib/blob/c4db6b981d577f456d64ebe9993818770e170454/Lib/glyphsLib/builder/custom_params.py#L258
        let mut v2_to_v3_param = |v2_names: &[&str], v3_name: &str| {
            if properties.iter().any(|n| n.key == v3_name && !n.is_empty()) {
                return;
            }
            for v2_name in v2_names {
                if let Some(value) = v2_to_v3_name(
                    self.custom_parameters.take_string(v2_name).as_deref(),
                    v3_name,
                ) {
                    properties.push(value);
                    return;
                }
            }
        };

        v2_to_v3_param(&["description", "openTypeNameDescription"], "descriptions");
        v2_to_v3_param(&["licenseURL", "openTypeNameLicenseURL"], "licenseURL");
        v2_to_v3_param(&["versionString", "openTypeNameVersion"], "versionString");
        v2_to_v3_param(&["compatibleFullName"], "compatibleFullNames");
        v2_to_v3_param(&["license", "openTypeNameLicense"], "licenses");
        v2_to_v3_param(&["uniqueID", "openTypeNameUniqueID"], "uniqueID");
        v2_to_v3_param(&["trademark"], "trademarks");
        v2_to_v3_param(&["sampleText", "openTypeNameSampleText"], "sampleTexts");
        v2_to_v3_param(&["postscriptFullName"], "postscriptFullName");
        v2_to_v3_param(&["postscriptFontName"], "postscriptFontName");
        v2_to_v3_param(
            &["WWSFamilyName", "openTypeNameWWSFamilyName"],
            "WWSFamilyName",
        );
        v2_to_v3_param(&["vendorID", "openTypeOS2VendorID"], "vendorID");

        self.properties = properties;

        Ok(())
    }

    fn v2_to_v3_instances(&mut self) -> Result<(), Error> {
        for instance in self.instances.iter_mut() {
            if let Some(custom_weight_class) = instance.custom_parameters.take("weightClass") {
                instance.weight_class = custom_weight_class.to_string().into();
            }
            // named clases become #s in v3
            for (tag, opt) in [
                ("wght", &mut instance.weight_class),
                ("wdth", &mut instance.width_class),
            ] {
                let Some(value) = opt.as_ref() else {
                    continue;
                };
                if f64::from_str(value).is_ok() {
                    continue;
                };
                let Some(value) = lookup_class_value(tag, value) else {
                    return Err(Error::UnknownValueName(value.clone()));
                };
                let _ = opt.insert(value.to_string());
            }

            instance.properties.extend(v2_to_v3_name(
                instance
                    .custom_parameters
                    .take_string("postscriptFontName")
                    .as_deref(),
                "postscriptFontName",
            ));
        }

        Ok(())
    }

    fn v2_to_v3_layer_attributes(&mut self) {
        for raw_glyph in self.glyphs.iter_mut() {
            for layer in raw_glyph.layers.iter_mut() {
                layer.v2_to_v3_attributes();
            }
        }
    }

    /// `<See https://github.com/schriftgestalt/GlyphsSDK/blob/Glyphs3/GlyphsFileFormat/GlyphsFileFormatv3.md#differences-between-version-2>`
    fn v2_to_v3(&mut self) -> Result<(), Error> {
        self.v2_to_v3_master_names()?;
        self.v2_to_v3_axes()?;
        self.v2_to_v3_metrics()?;
        self.v2_to_v3_instances()?;
        self.v2_to_v3_names()?; // uses instances
        self.v2_to_v3_layer_attributes();
        Ok(())
    }
}

// in the form '{INT, INT}'
fn parse_alignment_zone(zone: &str) -> Option<(OrderedFloat<f64>, OrderedFloat<f64>)> {
    let (one, two) = zone.split_once(',')?;
    let one = one.trim_start_matches(['{', ' ']).parse::<i32>().ok()?;
    let two = two.trim_start().trim_end_matches('}').parse::<i32>().ok()?;
    Some((OrderedFloat(one as f64), OrderedFloat(two as f64)))
}

fn make_glyph_order(glyphs: &[RawGlyph], custom_order: Option<Vec<SmolStr>>) -> Vec<SmolStr> {
    let mut valid_names: HashSet<_> = glyphs.iter().map(|g| &g.glyphname).collect();
    let mut glyph_order = Vec::new();

    // Add all valid glyphOrder entries in order
    // See https://github.com/googlefonts/fontmake-rs/pull/43/files#r1044627972
    for name in custom_order.into_iter().flatten() {
        if valid_names.remove(&name) {
            glyph_order.push(name.clone());
        }
    }

    // Add anything left over in file order
    glyph_order.extend(
        glyphs
            .iter()
            .filter(|g| valid_names.contains(&g.glyphname))
            .map(|g| g.glyphname.clone()),
    );

    glyph_order
}

// glyphs2 uses hex, glyphs3 uses base10
fn parse_codepoint_str(s: &str, radix: u32) -> BTreeSet<u32> {
    s.split(',')
        .map(|cp| u32::from_str_radix(cp, radix).unwrap())
        .collect()
}

/// <https://github.com/googlefonts/glyphsLib/blob/6f243c1f732ea1092717918d0328f3b5303ffe56/Lib/glyphsLib/builder/axes.py#L578>
fn default_master_idx(raw_font: &mut RawFont) -> usize {
    // Prefer an explicit origin
    // https://github.com/googlefonts/fontmake-rs/issues/44
    if let Some(master_idx) = raw_font
        .custom_parameters
        .take_string("Variable Font Origin")
        .and_then(|origin| {
            raw_font
                .font_master
                .iter()
                .position(|master| master.id == origin)
        })
    {
        return master_idx;
    }

    // No explicit origin, try to pick a winner

    // Contenders: (ordinal, words in name) for all masters that have names
    let contenders = raw_font
        .font_master
        .iter()
        .enumerate()
        .filter_map(|(i, m)| {
            m.name
                .as_deref()
                .map(|name| (i, whitespace_separated_tokens(name)))
        })
        .collect::<Vec<_>>();

    // EARLY EXIT: no contenders, just pick 0
    if contenders.is_empty() {
        return 0;
    }

    // In Python find_base_style <https://github.com/googlefonts/glyphsLib/blob/9d5828d874110c42dfc5f542db8eb84f88641eb5/Lib/glyphsLib/builder/axes.py#L652-L663>
    let mut common_words = contenders[0].1.clone();
    for (_, words) in contenders.iter().skip(1) {
        common_words.retain(|w| words.contains(w));
    }

    // Find the best match:
    //   Find the common words in the master names
    //   If any master is named exactly that, it wins
    //      "Foo Bar" is the best match for {Foo Bar Donkey, Foo Bar Cat, Foo Bar}
    //   Otherwise, a master whose name matches the common words if we delete "Regular" wins
    //      "Foo Bar Regular" is the best match for {Foo Bar Italic, Foo Bar Majestic, Foo Bar Regular}
    let mut best_idx = 0;
    for (idx, mut words) in contenders {
        // if name exactly matches common words you just win
        if *common_words == words {
            best_idx = idx;
            break;
        }

        // if our words excluding "Regular" match we're the best
        // a subsequent contender could match exactly so we don't win yet
        words.retain(|w| *w != "Regular");
        if *common_words == words {
            best_idx = idx;
        }
    }
    best_idx
}

fn whitespace_separated_tokens(s: &str) -> Vec<&str> {
    s.split_whitespace().collect()
}

fn axis_index(axes: &[Axis], pred: impl Fn(&Axis) -> bool) -> Option<usize> {
    axes.iter()
        .enumerate()
        .find_map(|(i, a)| if pred(a) { Some(i) } else { None })
}

fn user_to_design_from_axis_mapping(
    from: &mut RawFont,
) -> Option<BTreeMap<String, AxisUserToDesignMap>> {
    let mappings = from.custom_parameters.take_axis_mappings()?;
    let mut axis_mappings: BTreeMap<String, AxisUserToDesignMap> = BTreeMap::new();
    for mapping in mappings {
        let Some(axis_index) = axis_index(&from.axes, |a| a.tag == mapping.tag) else {
            log::warn!(
                "axis mapping includes tag {:?} not included in font",
                mapping.tag
            );
            continue;
        };
        let axis_name = &from.axes.get(axis_index).unwrap().name;
        for (user, design) in mapping.user_to_design.iter() {
            axis_mappings
                .entry(axis_name.clone())
                .or_default()
                .add_if_new(*user, *design);
        }
    }
    Some(axis_mappings)
}

fn user_to_design_from_axis_location(
    from: &mut RawFont,
) -> Option<BTreeMap<String, AxisUserToDesignMap>> {
    // glyphsLib only trusts Axis Location when all masters have it, match that
    // https://github.com/googlefonts/fontmake-rs/pull/83#discussion_r1065814670
    let master_locations: Vec<_> = from
        .font_master
        .iter_mut()
        .filter_map(|m| m.custom_parameters.take_axis_locations())
        .collect();
    if master_locations.len() != from.font_master.len() {
        if !master_locations.is_empty() {
            warn!(
                "{}/{} masters have Axis Location; ignoring",
                master_locations.len(),
                from.font_master.len()
            );
        }
        return None;
    }

    let mut axis_mappings: BTreeMap<String, AxisUserToDesignMap> = BTreeMap::new();
    for (master, axis_locations) in from.font_master.iter().zip(master_locations) {
        // masters' Axis Locations custom parameters can reference axes that aren't defined in the font,
        // so take font axes as the source of truth.
        for (idx, axis) in from.axes.iter().enumerate() {
            let Some(axis_location) = axis_locations.iter().find(|loc| loc.axis_name == axis.name)
            else {
                continue;
            };
            let user = axis_location.location;
            let design = master.axes_values[idx];

            axis_mappings
                .entry(axis_location.axis_name.clone())
                .or_default()
                .add_if_new(user, design);
        }
    }
    Some(axis_mappings)
}

impl AxisUserToDesignMap {
    fn add_any_new(&mut self, incoming: &AxisUserToDesignMap) {
        for (user, design) in incoming.0.iter() {
            self.add_if_new(*user, *design);
        }
    }

    fn add_if_new(&mut self, user: OrderedFloat<f64>, design: OrderedFloat<f64>) {
        // only keys (input/user-space) should be unique, values (output/design-space) can be duplicated
        if self.0.iter().any(|(u, _)| *u == user) {
            return;
        }
        self.0.push((user, design));
    }

    fn add_identity_map(&mut self, value: OrderedFloat<f64>) {
        // no duplicate keys nor values allowed
        if self.0.iter().any(|(u, d)| *u == value || *d == value) {
            return;
        }
        self.0.push((value, value));
    }

    pub fn iter(&self) -> impl Iterator<Item = &(OrderedFloat<f64>, OrderedFloat<f64>)> {
        self.0.iter()
    }

    pub fn is_identity(&self) -> bool {
        self.0.iter().all(|(u, d)| u == d)
    }
}

impl UserToDesignMapping {
    /// From most to least preferred: Axis Mappings, Axis Location, mappings from instances, assume user == design
    ///
    /// <https://github.com/googlefonts/glyphsLib/blob/6f243c1f732ea1092717918d0328f3b5303ffe56/Lib/glyphsLib/builder/axes.py#L155>
    fn new(from: &mut RawFont, instances: &[Instance]) -> Self {
        let from_axis_mapping = user_to_design_from_axis_mapping(from);
        let from_axis_location = user_to_design_from_axis_location(from);
        let (result, incomplete_mapping) = match (from_axis_mapping, from_axis_location) {
            (Some(from_mapping), Some(..)) => {
                warn!("Axis Mapping *and* Axis Location are defined; using Axis Mapping");
                (from_mapping, false)
            }
            (Some(from_mapping), None) => (from_mapping, false),
            (None, Some(from_location)) => (from_location, true),
            (None, None) => (BTreeMap::new(), true),
        };
        let mut result = Self(result);
        if incomplete_mapping {
            //https://github.com/googlefonts/glyphsLib/blob/682ff4b17/Lib/glyphsLib/builder/axes.py#L251
            result.add_instance_mappings_if_new(instances);
            if result.0.is_empty() || result.0.values().all(|v| v.is_identity()) {
                result.add_master_mappings_if_new(from);
            }
        }
        result
    }

    pub fn contains(&self, axis_name: &str) -> bool {
        self.0.contains_key(axis_name)
    }

    pub fn get(&self, axis_name: &str) -> Option<&AxisUserToDesignMap> {
        self.0.get(axis_name)
    }

    /// * <https://github.com/googlefonts/glyphsLib/blob/6f243c1f732ea1092717918d0328f3b5303ffe56/Lib/glyphsLib/builder/axes.py#L128>
    /// * <https://github.com/googlefonts/glyphsLib/blob/6f243c1f732ea1092717918d0328f3b5303ffe56/Lib/glyphsLib/builder/axes.py#L353>
    fn add_instance_mappings_if_new(&mut self, instances: &[Instance]) {
        for instance in instances
            .iter()
            .filter(|i| i.active && i.type_ == InstanceType::Single)
        {
            for (axis_name, inst_mapping) in instance.axis_mappings.iter() {
                self.0
                    .entry(axis_name.clone())
                    .or_default()
                    .add_any_new(inst_mapping);
            }
        }
    }

    fn add_master_mappings_if_new(&mut self, from: &RawFont) {
        for master in from.font_master.iter() {
            for (axis, value) in from.axes.iter().zip(&master.axes_values) {
                self.0
                    .entry(axis.name.clone())
                    .or_default()
                    .add_identity_map(*value);
            }
        }
    }
}

impl TryFrom<RawShape> for Shape {
    type Error = Error;

    fn try_from(from: RawShape) -> Result<Self, Self::Error> {
        // TODO: handle numerous unsupported attributes
        // See <https://github.com/schriftgestalt/GlyphsSDK/blob/Glyphs3/GlyphsFileFormat/GlyphsFileFormatv3.md#differences-between-version-2>

        let shape = if let Some(glyph_name) = from.glyph_name {
            assert!(!glyph_name.is_empty(), "A pointless component");

            // V3 vs v2: The transform entry has been replaced by angle, pos and scale entries.
            let mut transform = if let Some(transform) = from.transform {
                Affine::parse_plist(&transform)?
            } else {
                Affine::IDENTITY
            };

            // Glyphs 3 gives us {angle, pos, scale}. Glyphs 2 gives us the standard 2x3 matrix.
            // The matrix is more general and less ambiguous (what order do you apply the angle, pos, scale?)
            // so convert Glyphs 3 to that. Order based on saving the same transformed comonent as
            // Glyphs 2 and Glyphs 3 then trying to convert one to the other.
            if !from.pos.is_empty() {
                if from.pos.len() != 2 {
                    return Err(Error::StructuralError(format!("Bad pos: {:?}", from.pos)));
                }
                transform *= Affine::translate((from.pos[0], from.pos[1]));
            }
            if let Some(angle) = from.angle {
                transform *= normalized_rotation(angle);
            }
            if !from.scale.is_empty() {
                if from.scale.len() != 2 {
                    return Err(Error::StructuralError(format!(
                        "Bad scale: {:?}",
                        from.scale
                    )));
                }
                transform *= Affine::scale_non_uniform(from.scale[0], from.scale[1]);
            }

            Shape::Component(Component {
                name: glyph_name,
                transform,
                anchor: from.anchor,
                attributes: from.attributes,
                smart_component_values: from.piece,
            })
        } else {
            // no ref; presume it's a path
            Shape::Path(Path {
                closed: from.closed.unwrap_or_default(),
                nodes: from.nodes.clone(),
                attributes: from.attributes,
            })
        };
        Ok(shape)
    }
}

/// Return [kurbo::Affine] rotation around angle (in degrees) with normalized sin/cos.
///
/// This ensures that for the four "cardinal" rotations (0, 90, 180, 270), the values
/// of sin(angle) and cos(angle) are rounded to *exactly* 0.0, +1.0 or -1.0, thus avoiding
/// very small values (e.g. 1.2246467991473532e-16) whose effect may be amplified as some
/// transformed point coordinates end up close to the 0.5 threshold before being rounded
/// to integer later in the build.
/// It matches the output of the fontTools' Transform.rotate() used by glyphsLib.
/// <https://github.com/fonttools/fonttools/blob/b7509b2/Lib/fontTools/misc/transform.py#L246-L258>
fn normalized_rotation(angle_deg: f64) -> Affine {
    const ROT_90: Affine = Affine::new([0.0, 1.0, -1.0, 0.0, 0.0, 0.0]);
    const ROT_180: Affine = Affine::new([-1.0, 0.0, 0.0, -1.0, 0.0, 0.0]);
    const ROT_270: Affine = Affine::new([0.0, -1.0, 1.0, 0.0, 0.0, 0.0]);
    // Normalize angle to [0, 360)
    let normalized_angle = angle_deg.rem_euclid(360.0);

    match normalized_angle {
        0.0 => Affine::IDENTITY,
        90.0 => ROT_90,
        180.0 => ROT_180,
        270.0 => ROT_270,
        _ => Affine::rotate(angle_deg.to_radians()),
    }
}

fn map_and_push_if_present<T, U>(dest: &mut Vec<T>, src: Vec<U>, map: fn(U) -> T) {
    src.into_iter().map(map).for_each(|v| dest.push(v));
}

impl RawLayer {
    fn build(self, format_version: FormatVersion) -> Result<Layer, Error> {
        // we do what glyphsLib does:
        // https://github.com/googlefonts/glyphsLib/blob/c4db6b981d577f4/Lib/glyphsLib/classes.py#L3662
        // which is apparently standard, in that if a field is missing it has
        // some implied default value? Although I don't know where these are
        // all documented, outside of glyphsLib.
        const DEFAULT_LAYER_WIDTH: f64 = 600.;
        let mut shapes = Vec::new();

        // Glyphs v2 uses paths and components
        map_and_push_if_present(&mut shapes, self.paths, Shape::Path);
        map_and_push_if_present(&mut shapes, self.components, Shape::Component);

        // Glyphs v3 uses shapes for both
        for raw_shape in self.shapes {
            shapes.push(raw_shape.try_into()?);
        }

        let anchors = self
            .anchors
            .into_iter()
            .map(|ra| {
                let pos = if let Some(pos) = ra.pos {
                    pos
                } else if let Some(raw) = ra.position {
                    Point::parse_plist(&raw).unwrap()
                } else {
                    Point::ZERO
                };
                Anchor { name: ra.name, pos }
            })
            .collect();

        let mut attributes = self.attributes;
        // convert v2 bracket layers (based on name) into AxisRule attrs
        if let Some(axis_rule) =
            AxisRule::from_layer_name(&self.name).filter(|_| format_version.is_v2())
        {
            assert!(
                attributes.axis_rules.is_empty(),
                "glyphs v2 does not use axisRules attr"
            );
            attributes.axis_rules.push(axis_rule);
        }
        let smart_component_positions = if format_version.is_v2() {
            self.user_data.part_selection
        } else {
            self.part_selection
        }
        .into_iter()
        .map(|(k, v)| match v {
            1 => Ok((k, AxisPole::Min)),
            2 => Ok((k, AxisPole::Max)),
            other => Err(Error::BadValue(format!(
                "expected only 1 or 2 in partSelection, found '{other}'"
            ))),
        })
        .collect::<Result<_, _>>()?;
        Ok(Layer {
            layer_id: self.layer_id,
            associated_master_id: self.associated_master_id,
            width: self.width.unwrap_or(DEFAULT_LAYER_WIDTH.into()),
            vert_width: self.vert_width,
            vert_origin: self.vert_origin,
            shapes,
            anchors,
            attributes,
            smart_component_positions,
        })
    }
}

impl RawGlyph {
    // we pass in the radix because it depends on the version, stored in the font struct
    fn build(self, format_version: FormatVersion, glyph_data: &GlyphData) -> Result<Glyph, Error> {
        let mut instances = Vec::new();
        let mut bracket_layers = Vec::new();
        for mut layer in self.layers {
            if layer.is_bracket_layer(format_version) {
                bracket_layers.push(layer.build(format_version)?);
            } else if !layer.is_draft() {
                // it's possible for these to exist even if there's no
                // associated master id, in which case we don't care
                layer.attributes.axis_rules.clear();
                instances.push(layer.build(format_version)?);
            }
        }
        // if category/subcategory were set in the source, we keep them;
        // otherwise we look them up based on the bundled GlyphData.
        // (we use this info later to determine GDEF categories, zero the width
        // on non-spacing marks, etc)
        fn parse_category<T>(s: Option<&str>, glyph: &SmolStr) -> Option<T>
        where
            T: FromStr<Err = SmolStr>,
        {
            match s.filter(|s| !s.is_empty()).map(T::from_str).transpose() {
                Ok(x) => x,
                // if we don't know a category ignore it and we'll compute it later
                Err(err) => {
                    log::warn!("Unknown category '{err}' for glyph '{glyph}'");
                    None
                }
            }
        }

        let mut category = parse_category(self.category.as_deref(), &self.glyphname);
        let mut sub_category = parse_category(self.sub_category.as_deref(), &self.glyphname);
        let mut production_name = self.production_name;

        let codepoints = self
            .unicode
            .map(|s| parse_codepoint_str(&s, format_version.codepoint_radix()))
            .unwrap_or_default();

        if (category.is_none() || sub_category.is_none() || production_name.is_none())
            && let Some(result) = glyph_data.query(&self.glyphname, Some(&codepoints))
        {
            // if they were manually set don't change them, otherwise do
            category = category.or(Some(result.category));
            sub_category = sub_category.or(result.subcategory);
            production_name = production_name.or(result.production_name.map(Into::into));
        }

        let part_settings = self
            .parts_settings
            .iter()
            .map(|raw| (raw.name.clone(), raw.bottom_value..=raw.top_value))
            .collect();

        Ok(Glyph {
            name: self.glyphname,
            export: self.export.unwrap_or(true),
            layers: instances,
            bracket_layers,
            left_kern: self.kern_left,
            right_kern: self.kern_right,
            unicode: codepoints,
            category,
            sub_category,
            production_name,
            smart_component_axes: part_settings,
        })
    }
}

// https://github.com/googlefonts/glyphsLib/blob/24b4d340e4c82948ba121dcfe563c1450a8e69c9/Lib/glyphsLib/builder/constants.py#L186
#[rustfmt::skip]
static GLYPHS_TO_OPENTYPE_LANGUAGE_ID: &[(&str, i32)] = &[
    ("AFK", 0x0436), ("ARA", 0x0C01), ("ASM", 0x044D), ("AZE", 0x042C), ("BEL", 0x0423),
    ("BEN", 0x0845), ("BGR", 0x0402), ("BRE", 0x047E), ("CAT", 0x0403), ("CSY", 0x0405),
    ("DAN", 0x0406), ("DEU", 0x0407), ("ELL", 0x0408), ("ENG", 0x0409), ("ESP", 0x0C0A),
    ("ETI", 0x0425), ("EUQ", 0x042D), ("FIN", 0x040B), ("FLE", 0x0813), ("FOS", 0x0438),
    ("FRA", 0x040C), ("FRI", 0x0462), ("GRN", 0x046F), ("GUJ", 0x0447), ("HAU", 0x0468),
    ("HIN", 0x0439), ("HRV", 0x041A), ("HUN", 0x040E), ("HVE", 0x042B), ("IRI", 0x083C),
    ("ISL", 0x040F), ("ITA", 0x0410), ("IWR", 0x040D), ("JPN", 0x0411), ("KAN", 0x044B),
    ("KAT", 0x0437), ("KAZ", 0x043F), ("KHM", 0x0453), ("KOK", 0x0457), ("LAO", 0x0454),
    ("LSB", 0x082E), ("LTH", 0x0427), ("LVI", 0x0426), ("MAR", 0x044E), ("MKD", 0x042F),
    ("MLR", 0x044C), ("MLY", 0x043E), ("MNG", 0x0352), ("MTS", 0x043A), ("NEP", 0x0461),
    ("NLD", 0x0413), ("NOB", 0x0414), ("ORI", 0x0448), ("PAN", 0x0446), ("PAS", 0x0463),
    ("PLK", 0x0415), ("PTG", 0x0816), ("PTG-BR", 0x0416), ("RMS", 0x0417), ("ROM", 0x0418),
    ("RUS", 0x0419), ("SAN", 0x044F), ("SKY", 0x041B), ("SLV", 0x0424), ("SQI", 0x041C),
    ("SRB", 0x081A), ("SVE", 0x041D), ("TAM", 0x0449), ("TAT", 0x0444), ("TEL", 0x044A),
    ("THA", 0x041E), ("TIB", 0x0451), ("TRK", 0x041F), ("UKR", 0x0422), ("URD", 0x0420),
    ("USB", 0x042E), ("UYG", 0x0480), ("UZB", 0x0443), ("VIT", 0x042A), ("WEL", 0x0452),
    ("ZHH", 0x0C04), ("ZHS", 0x0804), ("ZHT", 0x0404),
    ("dflt", 0x0409),
];

/// Maps Glyphs language codes to OpenType language IDs
pub fn glyphs_to_opentype_lang_id(lang: &str) -> Option<u16> {
    GLYPHS_TO_OPENTYPE_LANGUAGE_ID
        .binary_search_by_key(&lang, |entry| entry.0)
        .ok()
        .map(|idx| GLYPHS_TO_OPENTYPE_LANGUAGE_ID[idx].1 as u16)
}

impl RawFeature {
    // https://github.com/googlefonts/glyphsLib/blob/24b4d340e4c82948ba121dcfe563c1450a8e69c9/Lib/glyphsLib/builder/features.py#L43
    fn autostr(&self) -> &str {
        match self.automatic {
            Some(1) => "# automatic\n",
            _ => "",
        }
    }

    fn name(&self) -> Result<&str, Error> {
        self.name
            .as_deref()
            .or(self.tag.as_deref())
            .ok_or_else(|| Error::StructuralError(format!("{self:?} missing name and tag")))
    }

    fn disabled(&self) -> bool {
        self.disabled == Some(1)
    }

    /// Some glyphs sources store stylistic set names in the 'note' field
    ///
    /// See the little sidebar item here:
    /// <https://glyphsapp.com/learn/stylistic-sets#g-names-for-stylistic-sets>
    fn legacy_name_record_maybe(&self) -> Option<String> {
        let name = self.notes.as_deref()?.strip_prefix("Name:")?.trim();
        if name.is_empty() {
            None
        } else {
            Some(format!("name 3 1 0x409 \"{name}\";"))
        }
    }

    // https://github.com/googlefonts/glyphsLib/blob/24b4d340e4c82948ba121dcfe563c1450a8e69c9/Lib/glyphsLib/builder/features.py#L134
    fn feature_names(&self) -> String {
        let labels = self
            .labels
            .iter()
            .filter_map(|label| label.to_fea())
            .chain(self.legacy_name_record_maybe())
            .collect::<Vec<_>>()
            .join("\n");
        if labels.is_empty() {
            Default::default()
        } else {
            format!("featureNames {{\n{labels}\n}};\n")
        }
    }

    // https://github.com/googlefonts/glyphsLib/blob/24b4d340e4c82948ba121dcfe563c1450a8e69c9/Lib/glyphsLib/builder/features.py#L90
    fn prefix_to_feature(&self) -> Result<FeatureSnippet, Error> {
        let name = self.name.as_deref().unwrap_or_default();
        let code = format!("# Prefix: {}\n{}{}", name, self.autostr(), self.code);
        Ok(FeatureSnippet::new(code, self.disabled()))
    }

    // https://github.com/googlefonts/glyphsLib/blob/24b4d340e4c82948ba121dcfe563c1450a8e69c9/Lib/glyphsLib/builder/features.py#L101
    fn class_to_feature(&self) -> Result<FeatureSnippet, Error> {
        let name = self.name()?;
        let code = format!(
            "{}{}{name} = [ {}\n];",
            self.autostr(),
            if name.starts_with('@') { "" } else { "@" },
            self.code
        );
        Ok(FeatureSnippet::new(code, self.disabled()))
    }

    // https://github.com/googlefonts/glyphsLib/blob/24b4d340e4c82948ba121dcfe563c1450a8e69c9/Lib/glyphsLib/builder/features.py#L113
    fn raw_feature_to_feature(&self) -> Result<FeatureSnippet, Error> {
        let name = self.name()?;
        let insert_mark = self.insert_mark_if_manual_kern_feature();
        let code = format!(
            "feature {name} {{\n{}{}{}{insert_mark}\n}} {name};",
            self.autostr(),
            self.feature_names(),
            self.code
        );
        Ok(FeatureSnippet::new(code, self.disabled()))
    }

    //https://github.com/googlefonts/glyphsLib/blob/c4db6b981d577f4/Lib/glyphsLib/builder/features.py#L180
    fn insert_mark_if_manual_kern_feature(&self) -> &str {
        if self.name().unwrap_or_default() == "kern"
            && self.automatic != Some(1)
            && !self.code.contains("# Automatic Code")
        {
            "# Automatic Code\n"
        } else {
            ""
        }
    }
}

impl RawNameValue {
    fn to_fea(&self) -> Option<String> {
        if self.value.is_empty() {
            // skip empty names:
            // https://github.com/googlefonts/glyphsLib/blob/c4db6b981d577f/Lib/glyphsLib/builder/features.py#L155
            return None;
        }

        let Some(language_id) = glyphs_to_opentype_lang_id(&self.language) else {
            warn!("Unknown feature label language: {}", self.language);
            return None;
        };
        let name = self.value.replace("\\", "\\005c").replace("\"", "\\0022");
        Some(format!("  name 3 1 0x{language_id:04X} \"{name}\";"))
    }
}

/// <https://github.com/googlefonts/glyphsLib/blob/6f243c1f732ea1092717918d0328f3b5303ffe56/Lib/glyphsLib/classes.py#L220-L249>
fn lookup_class_value(axis_tag: &str, user_class: &str) -> Option<u16> {
    let user_class = match user_class {
        value if !value.is_empty() => {
            let mut value = value.to_ascii_lowercase();
            value.retain(|c| c != ' ');
            value
        }
        _ => String::from(""),
    };
    match (axis_tag, user_class.as_str()) {
        ("wght", "thin") => Some(100),
        ("wght", "extralight" | "ultralight") => Some(200),
        ("wght", "light") => Some(300),
        ("wght", "" | "normal" | "regular") => Some(400),
        ("wght", "medium") => Some(500),
        ("wght", "demibold" | "semibold") => Some(600),
        ("wght", "bold") => Some(700),
        ("wght", "ultrabold" | "extrabold") => Some(800),
        ("wght", "black" | "heavy") => Some(900),
        ("wdth", "ultracondensed") => Some(1),
        ("wdth", "extracondensed") => Some(2),
        ("wdth", "condensed") => Some(3),
        ("wdth", "semicondensed") => Some(4),
        ("wdth", "" | "Medium (normal)") => Some(5),
        ("wdth", "semiexpanded") => Some(6),
        ("wdth", "expanded") => Some(7),
        ("wdth", "extraexpanded") => Some(8),
        ("wdth", "ultraexpanded") => Some(9),
        _ => {
            warn!("Unrecognized ('{axis_tag}', '{user_class}')");
            None
        }
    }
}

fn add_mapping_if_new(
    axis_mappings: &mut BTreeMap<String, AxisUserToDesignMap>,
    axes: &[Axis],
    axis_tag: &str,
    axes_values: &[OrderedFloat<f64>],
    value: f64,
) {
    let Some(idx) = axes.iter().position(|a| a.tag == axis_tag) else {
        return;
    };
    let axis = &axes[idx];
    let Some(design) = axes_values.get(idx) else {
        return;
    };

    axis_mappings
        .entry(axis.name.clone())
        .or_default()
        .add_if_new(value.into(), *design);
}

impl Instance {
    /// Glyphs 2 instances have fun fields.
    ///
    /// Mappings based on
    /// <https://github.com/googlefonts/glyphsLib/blob/6f243c1f732ea1092717918d0328f3b5303ffe56/Lib/glyphsLib/classes.py#L3451>
    fn new(
        axes: &[Axis],
        value: &mut RawInstance,
        masters_have_axis_locations: bool,
    ) -> Result<Self, Error> {
        let active = value.is_active();
        let mut axis_mappings: BTreeMap<String, AxisUserToDesignMap> = BTreeMap::new();

        // Instances can also have "Axis Location" custom parameters, complementing the ones
        // from the masters: https://github.com/googlefonts/fontc/issues/918
        let mut tags_done = BTreeSet::new();
        for axis_location in value
            .custom_parameters
            .take_axis_locations()
            .unwrap_or_default()
        {
            let Some(axis_index) = axis_index(axes, |a| a.name == axis_location.axis_name) else {
                log_once_warn!(
                    "{} instance's 'Axis Location' includes axis {:?} not included in font",
                    value.name,
                    axis_location.axis_name
                );
                continue;
            };
            let user = axis_location.location;
            let design = value.axes_values[axis_index];

            axis_mappings
                .entry(axis_location.axis_name.clone())
                .or_default()
                .add_if_new(user, design);

            tags_done.insert(axes[axis_index].tag.as_str());
        }

        // only infer legacy mappings when Axis Locations aren't defined
        if !masters_have_axis_locations && !tags_done.contains("wght") {
            // OS/2 weight_class corresponds to 'wght' axis user-space value
            add_mapping_if_new(
                &mut axis_mappings,
                axes,
                "wght",
                &value.axes_values,
                value
                    .weight_class
                    .as_ref()
                    .map(|v| f64::from_str(v).unwrap())
                    .unwrap_or(400.0),
            );
        }

        if !masters_have_axis_locations && !tags_done.contains("wdth") {
            // OS/2 width_class gets mapped to 'wdth' percent scale, see:
            // https://github.com/googlefonts/glyphsLib/blob/7041311e/Lib/glyphsLib/builder/constants.py#L222
            add_mapping_if_new(
                &mut axis_mappings,
                axes,
                "wdth",
                value.axes_values.as_ref(),
                value
                    .width_class
                    .as_ref()
                    .map(|v| match WidthClass::try_from(u16::from_str(v).unwrap()) {
                        Ok(width_class) => width_class.to_percent(),
                        Err(err) => {
                            warn!("{err}");
                            100.0
                        }
                    })
                    .unwrap_or(100.0),
            );
        }

        Ok(Instance {
            name: value.name.clone(),
            active,
            type_: value
                .type_
                .as_ref()
                .map(|v| v.as_str().into())
                .unwrap_or(InstanceType::Single),
            axis_mappings,
            axes_values: value.axes_values.clone(),
            properties: value.properties.clone(),
            custom_parameters: value.custom_parameters.to_custom_params()?,
        })
    }

    fn family_name(&self) -> Option<&str> {
        self.properties
            .iter()
            .find(|raw| raw.key == "familyNames")
            .and_then(RawName::get_value)
        // glyphsLib here also checks for 'familyName' in customParams, but we
        // don't have that key? Possible that we're ignoring it in the raw params
        // conversion..
        // https://github.com/googlefonts/glyphsLib/blob/c4db6b981d577/Lib/glyphsLib/classes.py#L3271
    }

    /// Get the optional postscript name to use for the `fvar` named instance.
    pub fn postscript_name(&self) -> Option<&str> {
        // https://handbook.glyphsapp.com/custom-parameter-descriptions/
        // https://github.com/googlefonts/glyphsLib/blob/7a8f643a711ffb9d351d037425a7eb60759e6219/Lib/glyphsLib/builder/instances.py#L118-L121
        ["variablePostscriptFontName", "postscriptFontName"]
            .iter()
            .find_map(|key| {
                self.properties
                    .iter()
                    .find(|raw| raw.key == *key)
                    .and_then(RawName::get_value)
            })
    }
}

/// Glyphs appears to use code page identifiers rather than bits
///
/// <https://learn.microsoft.com/en-us/typography/opentype/spec/os2#ulcodepagerange>
fn codepage_range_bit(codepage: u32) -> Result<u32, Error> {
    Ok(match codepage {
        1252 => 0,  // Latin 1
        1250 => 1,  // Latin 2: Eastern Europe
        1251 => 2,  // Cyrillic
        1253 => 3,  // Greek
        1254 => 4,  // Turkish
        1255 => 5,  // Hebrew
        1256 => 6,  // Arabic
        1257 => 7,  // Windows Baltic
        1258 => 8,  // Vietnamese
        874 => 16,  // Thai
        932 => 17,  // JIS/Japan
        936 => 18,  // Chinese: Simplified PRC and Singapore
        949 => 19,  // Korean Wansung
        950 => 20,  // Chinese: Traditional Taiwan and Hong Kong SAR
        1361 => 21, // Korean Johab
        869 => 48,  // IBM Greek
        866 => 49,  // MS-DOS Russian
        865 => 50,  // MS-DOS Nordic
        864 => 51,  // Arabic
        863 => 52,  //	MS-DOS Canadian French
        862 => 53,  //		Hebrew
        861 => 54,  //		MS-DOS Icelandic
        860 => 55,  //		MS-DOS Portuguese
        857 => 56,  //		IBM Turkish
        855 => 57,  //	IBM Cyrillic; primarily Russian
        852 => 58,  //		Latin 2
        775 => 59,  //		MS-DOS Baltic
        737 => 60,  //	Greek; former 437 G
        708 => 61,  //	Arabic; ASMO 708
        850 => 62,  //	WE/Latin 1
        437 => 63,  //	US

        v if v < 64 => v, // an actual bit
        _ => return Err(Error::InvalidCodePage(codepage)),
    })
}

fn update_names(all_names: &mut BTreeMap<String, Vec<(String, String)>>, raw_names: &[RawName]) {
    for name in raw_names {
        // Collect ALL language values into IndexMap to deduplicate (last occurrence wins)
        // while preserving insertion order. This is critical for the fallback behavior:
        // when there's no dflt/default/ENG, the FIRST entry becomes the English fallback,
        // see `return list(self._localized_values.values())[0]` in glyphsLib:
        // https://github.com/googlefonts/glyphsLib/blob/f90e406/Lib/glyphsLib/classes.py#L3162
        let mut lang_map: IndexMap<String, String> = IndexMap::new();

        // Handle old-style single value (v2 format); we treat as dflt
        if let Some(value) = &name.value {
            lang_map.insert("dflt".to_string(), value.clone());
        }

        // Handle new-style localized values (v3 format)
        for (lang, val) in name.localized_values() {
            lang_map.insert(lang.to_string(), val.to_string());
        }

        if !lang_map.is_empty() {
            let values: Vec<_> = lang_map.into_iter().collect();
            all_names.insert(name.key.clone(), values);
        }
    }
}

impl TryFrom<RawFont> for Font {
    type Error = Error;

    fn try_from(mut from: RawFont) -> Result<Self, Self::Error> {
        if from.format_version.is_v2() {
            from.v2_to_v3()?;
        } else {
            // <https://github.com/googlefonts/fontc/issues/1029>
            from.v2_to_v3_names()?;
        }

        // TODO: this should be provided in a manner that allows for overrides
        let glyph_data = GlyphData::default();

        // originally glyphsLib would infer wght/wdth mappings from the instance weight/width classes;
        // then, a new "Axis Location" custom parameter was defined for both masters and instances;
        // when (all) masters use the latter, only explicit "Axis Location" parameters get used from
        // instances, and the legacy heuristic for wght/wdth is disabled, so we need to keep track
        // of this when building instance axis mappings:
        // https://github.com/googlefonts/glyphsLib/blob/7a8f643a/Lib/glyphsLib/builder/axes.py#L225-L227
        let masters_have_axis_locations = from
            .font_master
            .iter()
            .all(|master| master.custom_parameters.contains("Axis Location"));

        let instances: Vec<_> = from
            .instances
            .iter_mut()
            .map(|ri| Instance::new(&from.axes, ri, masters_have_axis_locations))
            .collect::<Result<Vec<_>, Error>>()?;

        // parameters like "Axis Location", "Axis Mappings" and "Variable Font Origin" are
        // handled separately from to_custom_params() and need to be taken before the latter
        // is called, to avoid spurious "unknown custom parameter" warnings
        // https://github.com/googlefonts/fontc/issues/1682
        let axis_mappings = UserToDesignMapping::new(&mut from, &instances);
        let default_master_idx = default_master_idx(&mut from);

        let mut custom_parameters = from.custom_parameters.to_custom_params()?;

        let glyph_order = make_glyph_order(&from.glyphs, custom_parameters.glyph_order.take());

        let mut glyphs = BTreeMap::new();
        for raw_glyph in from.glyphs.into_iter() {
            let mut glyph = raw_glyph.build(from.format_version, &glyph_data)?;
            // v2 bracket layers can only reference a single axis position (on the
            // first axis in the font) so we add empty axis rules for any other
            // axes.
            for layer in glyph.bracket_layers.iter_mut() {
                layer
                    .attributes
                    .axis_rules
                    .resize(from.axes.len(), Default::default());
            }
            glyphs.insert(glyph.name.clone(), glyph);
        }

        let mut features = Vec::new();
        for class in from.classes {
            features.push(class.class_to_feature()?);
        }
        for prefix in from.feature_prefixes {
            features.push(prefix.prefix_to_feature()?);
        }
        for feature in from.features {
            features.push(feature.raw_feature_to_feature()?);
        }

        let units_per_em = from.units_per_em.ok_or(Error::NoUnitsPerEm)?;
        let units_per_em = units_per_em.try_into().map_err(Error::InvalidUpem)?;

        let mut all_names = BTreeMap::new();
        update_names(&mut all_names, &from.properties);
        for instance in &instances {
            if instance.active
                && instance.type_ == InstanceType::Variable
                    // glyphsLib has instance.familyName return the root familyName
                    // if it has no other value; we just unwrap_or_true here
                    // https://github.com/googlefonts/glyphsLib/blob/c4db6b981d577/Lib/glyphsLib/classes.py#L3272
                && instance.family_name().map(|name| name == from.family_name.as_str()).unwrap_or(true)
            {
                update_names(&mut all_names, &instance.properties);
            }
        }
        // Insert RawFont.family_name as the default familyNames where downstream code
        // expects to find it
        let family_names = all_names
            .entry("familyNames".into())
            .or_insert_with(Vec::new);
        family_names.retain(|(lang, _)| lang != "dflt");
        family_names.insert(0, ("dflt".to_string(), from.family_name.clone()));
        // Rename versionString to version
        if let Some(version_values) = all_names.remove("versionString") {
            all_names.insert("version".into(), version_values);
        }

        let metric_names: BTreeMap<usize, String> = from
            .metrics
            .into_iter()
            .enumerate()
            .map(|(idx, metric)| (idx, metric.type_))
            .collect();

        let masters = from
            .font_master
            .into_iter()
            .map(|m| {
                let custom_parameters = m.custom_parameters.to_custom_params()?;
                Ok(FontMaster {
                    id: m.id,
                    name: m.name.unwrap_or_default(),
                    axes_values: m.axes_values,
                    metric_values: m
                        .metric_values
                        .into_iter()
                        .enumerate()
                        .filter_map(|(idx, value)| {
                            metric_names.get(&idx).map(|name| (name.clone(), value))
                        })
                        .fold(BTreeMap::new(), |mut acc, (name, value)| {
                            // only insert a metric if one with the same name hasn't been added
                            // yet; matches glyphsLib's behavior where the first duplicate wins
                            // https://github.com/googlefonts/fontc/issues/1269
                            acc.entry(name).or_insert(value.into());
                            acc
                        }),
                    number_values: from
                        .numbers
                        .iter()
                        .zip(m.number_values.iter())
                        .map(|(k, v)| (k.name.clone(), *v))
                        .collect(),
                    custom_parameters,
                    user_data: m.user_data,
                })
            })
            .collect::<Result<_, Error>>()?;

        let virtual_masters = custom_parameters.virtual_masters.take().unwrap_or_default();
        Ok(Font {
            units_per_em,
            axes: from.axes,
            masters,
            default_master_idx,
            glyphs,
            glyph_order,
            axis_mappings,
            virtual_masters,
            features,
            all_names,
            instances,
            version_major: from.versionMajor.unwrap_or_default() as i32,
            version_minor: from.versionMinor.unwrap_or_default() as u32,
            date: from.date,
            kerning_ltr: from.kerning_LTR,
            kerning_rtl: from.kerning_RTL,
            custom_parameters,
        })
    }
}

fn preprocess_unparsed_plist(s: &str) -> Cow<'_, str> {
    // Glyphs has a wide variety of unicode definitions, not all of them parser friendly
    // Make unicode always a string, without any wrapping () so we can parse as csv, radix based on format version
    let unicode_re =
        Regex::new(r"(?m)^(?P<prefix>\s*unicode\s*=\s*)[(]?(?P<value>[0-9a-zA-Z,]+)[)]?;\s*$")
            .unwrap();
    unicode_re.replace_all(s, r#"$prefix"$value";"#)
}

fn variable_instance_for<'a>(instances: &'a [Instance], name: &str) -> Option<&'a Instance> {
    instances
        .iter()
        .find(|i| i.active && i.type_ == InstanceType::Variable && i.name == name)
}

impl Font {
    pub fn load_from_string(data: &str) -> Result<Font, Error> {
        let raw_font = RawFont::load_from_string(data)?;
        let mut font = Font::try_from(raw_font)?;
        font.preprocess()?;
        Ok(font)
    }

    pub fn load(glyphs_file: &path::Path) -> Result<Font, Error> {
        let mut font = Self::load_raw(glyphs_file)?;
        font.preprocess()?;
        Ok(font)
    }

    fn preprocess(&mut self) -> Result<(), Error> {
        // ensure that glyphs with components that have bracket layers
        // also have bracket layers.
        self.align_bracket_layers();

        // propagate anchors by default unless explicitly set to false
        if self.custom_parameters.propagate_anchors.unwrap_or(true) {
            self.propagate_all_anchors();
        }
        // if any glyphs reference smart components, convert them to outlines.
        // (see https://glyphsapp.com/learn/smart-components)
        self.instantiate_all_smart_components()
    }

    /// if a glyph has components that have alternate layers, copy the layer
    /// locations into the glyph.
    ///
    /// This is required to correctly handle bracket glyphs.
    // https://github.com/googlefonts/glyphsLib/blob/c4db6b981d/Lib/glyphsLib/builder/bracket_layers.py#L176
    fn align_bracket_layers(&mut self) {
        // python does this recursively but we'll presort instead
        let todo = self.depth_sorted_composite_glyphs();

        for name in &todo {
            let mut needed = IndexMap::new();
            let glyph = self.glyphs.get(name).unwrap();
            let my_bracket_layers = glyph
                .bracket_layers
                .iter()
                .map(|l| l.bracket_info(&self.axes))
                .collect::<IndexSet<_>>();
            for layer in &glyph.layers {
                for comp in layer.components() {
                    let comp_glyph = self.glyphs.get(&comp.name).unwrap();

                    let comp_bracket_layers = comp_glyph
                        .bracket_layers
                        .iter()
                        .map(|l| l.bracket_info(&self.axes))
                        .collect::<IndexSet<_>>();
                    if comp_bracket_layers != my_bracket_layers {
                        let missing = comp_bracket_layers.difference(&my_bracket_layers);
                        needed
                            .entry(layer.layer_id.clone())
                            .or_insert(IndexSet::new())
                            .extend(missing.cloned());
                    }
                }
            }

            if needed.is_empty() {
                // all good, next glyph
                continue;
            }

            let mut new_layers = Vec::new();
            for (master, needed_brackets) in needed {
                let master_name = self
                    .masters
                    .iter()
                    .find_map(|m| (m.id == master).then_some(m.name.as_str()))
                    .unwrap_or(master.as_str());
                if !my_bracket_layers.is_empty() {
                    log::warn!(
                        "Glyph {name} in master {master_name} has different bracket layers \
                        than its components. This is not supported, so some bracket layers \
                        will not be applied. Consider fixing the source instead."
                    );
                }

                let base_layer = glyph.layers.iter().find(|l| l.layer_id == master).unwrap();
                for box_ in needed_brackets {
                    log::debug!("synthesized layer {box_:?} in master {master_name} for '{name}'",);
                    let new_layer = synthesize_bracket_layer(base_layer, box_, &self.axes);
                    new_layers.push(new_layer);
                }
            }

            self.glyphs
                .get_mut(name)
                .unwrap()
                .bracket_layers
                .extend_from_slice(&new_layers);
        }
    }

    // smart components get converted into normal paths before IR.
    // see https://glyphsapp.com/learn/smart-components
    fn instantiate_all_smart_components(&mut self) -> Result<(), Error> {
        // find all the glyphs that include smart components
        let mut glyphs_with_smart_components = self
            .glyphs
            .values()
            .filter(|glyph| {
                glyph
                    .layers
                    .iter()
                    .flat_map(|layer| layer.components())
                    .any(|comp| {
                        //https://github.com/googlefonts/glyphsLib/blob/52c982399b/Lib/glyphsLib/builder/components.py#L77
                        !comp.smart_component_values.is_empty()
                            && self
                                .glyphs
                                .get(&comp.name)
                                .map(|ref_glyph| !ref_glyph.smart_component_axes.is_empty())
                                .unwrap_or(false)
                    })
            })
            .cloned()
            .collect::<Vec<_>>();

        if glyphs_with_smart_components.is_empty() {
            return Ok(());
        }
        // then make sure we do everything depth-first, so that if any smart components
        // contain _other_ smart components, those are instantiated first.
        let depth_ranked = self
            .depth_sorted_composite_glyphs()
            .into_iter()
            .enumerate()
            .map(|(rank, name)| (name, rank))
            .collect::<HashMap<_, _>>();

        glyphs_with_smart_components.sort_by_key(|g| depth_ranked.get(&g.name).unwrap());

        // convert the smart components to normal outlines, per-glyph
        for mut glyph in glyphs_with_smart_components {
            for layer in glyph.layers.iter_mut() {
                let old_shapes = std::mem::take(&mut layer.shapes);
                let mut new_shapes = Vec::new();
                for shape in old_shapes {
                    if let Some(comp) = shape.as_smart_component()
                        && let Some(ref_glyph) = self
                            .glyphs
                            .get(&comp.name)
                            .filter(|g| !g.smart_component_axes.is_empty())
                    {
                        log::debug!(
                            "instantiating smart component '{}' for '{}'",
                            comp.name,
                            glyph.name
                        );
                        new_shapes.extend(
                            crate::smart_components::instantiate_for_layer(
                                layer.master_id(),
                                comp,
                                ref_glyph,
                            )
                            .map_err(|issue| {
                                Error::BadSmartComponent {
                                    glyph: glyph.name.clone(),
                                    component: comp.name.clone(),
                                    issue,
                                }
                            })?,
                        );
                    } else {
                        layer.shapes.push(shape);
                    }
                }
                layer.shapes.extend(new_shapes);
            }
            // replace the old glyph with the new, component-free glyph
            self.glyphs.insert(glyph.name.clone(), glyph);
        }
        Ok(())
    }

    // load without propagating anchors or components
    pub(crate) fn load_raw(glyphs_file: impl AsRef<path::Path>) -> Result<Font, Error> {
        RawFont::load(glyphs_file.as_ref()).and_then(Font::try_from)
    }

    pub fn default_master(&self) -> &FontMaster {
        &self.masters[self.default_master_idx]
    }

    /// <https://handbook.glyphsapp.com/exports/>
    pub fn variable_export_settings(&self, master: &FontMaster) -> Option<&Instance> {
        variable_instance_for(&self.instances, &master.name)
    }

    /// Get the default (English) value for a name property.
    ///
    /// Searches for a value with language "dflt", "default", or "ENG" in the all_names field,
    /// in order of preference: dflt > default > ENG > first value.
    /// Returns None if the property key doesn't exist or has no values.
    pub fn get_default_name(&self, key: &str) -> Option<&str> {
        let values = self.all_names.get(key)?;
        let scale = values.len() as i32;
        values
            .iter()
            .enumerate()
            .map(|(i, (lang, val))| {
                (
                    default_language_score(lang.as_str(), i, scale),
                    val.as_str(),
                )
            })
            .min()
            .map(|(_, val)| val)
    }

    /// Get all localized values for a name property.
    ///
    /// Returns an iterator of (language code, value) tuples. Default languages
    /// ("dflt", "default", "ENG") are excluded.
    pub fn get_localized_names(&self, key: &str) -> impl Iterator<Item = (&str, &str)> + '_ {
        self.all_names
            .get(key)
            .into_iter()
            .flat_map(|v| v.iter())
            .filter(|(lang, _)| !matches!(lang.as_str(), "dflt" | "default" | "ENG"))
            .map(|(lang, val)| (lang.as_str(), val.as_str()))
    }

    /// Iterate over all default name entries.
    ///
    /// Returns an iterator of (property key, default value) tuples.
    /// Uses priority order: dflt > default > ENG > first value.
    pub fn default_names(&self) -> impl Iterator<Item = (&str, &str)> + '_ {
        self.all_names
            .keys()
            .filter_map(|key| self.get_default_name(key).map(|val| (key.as_str(), val)))
    }

    /// Iterate over all localized name entries.
    ///
    /// Returns an iterator of (property key, language code, value) tuples.
    /// Default languages ("dflt", "default", "ENG") are excluded.
    pub fn localized_names(&self) -> impl Iterator<Item = (&str, &str, &str)> + '_ {
        self.all_names.iter().flat_map(|(key, values)| {
            values
                .iter()
                .filter(|(lang, _)| !matches!(lang.as_str(), "dflt" | "default" | "ENG"))
                .map(move |(lang, val)| (key.as_str(), lang.as_str(), val.as_str()))
        })
    }

    pub fn vendor_id(&self) -> Option<&str> {
        self.get_default_name("vendorID")
    }
}

//https://github.com/googlefonts/glyphsLib/blob/c4db6b981d/Lib/glyphsLib/builder/bracket_layers.py#L258
fn synthesize_bracket_layer(
    old_layer: &Layer,
    box_: BTreeMap<String, (Option<i64>, Option<i64>)>,
    axes: &[Axis],
) -> Layer {
    let mut new_layer = old_layer.clone();
    new_layer.associated_master_id = Some(std::mem::take(&mut new_layer.layer_id));
    new_layer.attributes.axis_rules = axes
        .iter()
        .map(|axis| {
            if let Some((min, max)) = box_.get(&axis.tag) {
                AxisRule {
                    min: min.map(|x| x as _),
                    max: max.map(|x| x as _),
                }
            } else {
                Default::default()
            }
        })
        .collect();

    new_layer
}

/// Convert [kurbo::Point] to this for eq and hash/
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct PointForEqAndHash {
    x: OrderedFloat<f64>,
    y: OrderedFloat<f64>,
}

impl PointForEqAndHash {
    fn new(point: Point) -> PointForEqAndHash {
        point.into()
    }
}

impl From<Point> for PointForEqAndHash {
    fn from(value: Point) -> Self {
        PointForEqAndHash {
            x: value.x.into(),
            y: value.y.into(),
        }
    }
}

/// Convert [kurbo::Affine] to this for eq and hash/
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
struct AffineForEqAndHash([OrderedFloat<f64>; 6]);

impl From<Affine> for AffineForEqAndHash {
    fn from(value: Affine) -> Self {
        Self(value.as_coeffs().map(|coeff| coeff.into()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{Font, FontMaster, Node, Shape, plist::FromPlist, smart_components};
    use std::{
        collections::{BTreeMap, BTreeSet, HashSet},
        path::{Path, PathBuf},
    };

    use ordered_float::OrderedFloat;

    use pretty_assertions::assert_eq;

    use kurbo::{Affine, Point, Rect};

    use rstest::rstest;
    use smol_str::ToSmolStr;

    fn testdata_dir() -> PathBuf {
        // working dir varies CLI vs VSCode
        let mut dir = Path::new("../resources/testdata");
        if !dir.is_dir() {
            dir = Path::new("./resources/testdata");
        }
        assert!(dir.is_dir());
        dir.to_path_buf()
    }

    fn glyphs2_dir() -> PathBuf {
        testdata_dir().join("glyphs2")
    }

    fn glyphs3_dir() -> PathBuf {
        testdata_dir().join("glyphs3")
    }

    fn round(transform: Affine, digits: u8) -> Affine {
        let m = 10f64.powi(digits as i32);
        let mut coeffs = transform.as_coeffs();
        for c in coeffs.iter_mut() {
            *c = (*c * m).round() / m;
        }
        Affine::new(coeffs)
    }

    #[test]
    fn v2_format_version() {
        let v2_font = glyphs2_dir().join("Mono.glyphs");
        let as_str = std::fs::read_to_string(&v2_font).unwrap();
        assert!(!as_str.contains(".formatVersion"), "only exists in v3");
        let font = RawFont::load(&v2_font).unwrap();
        // falls back to default
        assert_eq!(font.format_version, FormatVersion::V2);
    }

    #[test]
    fn v3_format_version() {
        let v3_font = glyphs3_dir().join("MasterNames.glyphs");
        let as_str = std::fs::read_to_string(&v3_font).unwrap();
        assert!(as_str.contains(".formatVersion"), "exists in v3");
        let font = RawFont::load(&v3_font).unwrap();
        // falls back to default
        assert_eq!(font.format_version, FormatVersion::V3);
    }

    #[test]
    fn test_glyphs3_node() {
        let node: Node = Node::parse_plist("(354, 183, l)").unwrap();
        assert_eq!(
            Node {
                node_type: crate::NodeType::Line,
                pt: super::Point { x: 354.0, y: 183.0 }
            },
            node
        );
    }

    #[test]
    fn test_glyphs2_node() {
        let node: Node = Node::parse_plist("\"354 183 LINE\"").unwrap();
        assert_eq!(
            Node {
                node_type: crate::NodeType::Line,
                pt: super::Point { x: 354.0, y: 183.0 }
            },
            node
        );
    }

    #[test]
    fn test_glyphs3_node_userdata() {
        let node = Node::parse_plist("(354, 183, l,{name = hr00;})").unwrap();
        assert_eq!(
            Node {
                node_type: crate::NodeType::Line,
                pt: super::Point { x: 354.0, y: 183.0 }
            },
            node
        );
    }

    #[test]
    fn test_glyphs2_node_userdata() {
        let node = Node::parse_plist("\"354 183 LINE {name=duck}\"").unwrap();
        assert_eq!(
            Node {
                node_type: crate::NodeType::Line,
                pt: super::Point { x: 354.0, y: 183.0 }
            },
            node
        );
    }

    // unquoted infinity likes to parse as a float which is suboptimal for glyph names. Survive.
    // Observed on Work Sans and Lexend.
    #[test]
    fn survive_unquoted_infinity() {
        // Read a minimal glyphs file that reproduces the error
        Font::load(&glyphs3_dir().join("infinity.glyphs")).unwrap();
    }

    fn assert_wght_var_metrics(font: &Font) {
        let default_master = font.default_master();
        assert_eq!(737.0, default_master.ascender().unwrap());
        assert_eq!(-42.0, default_master.descender().unwrap());
    }

    #[test]
    fn read_wght_var_2_metrics() {
        assert_wght_var_metrics(&Font::load(&glyphs2_dir().join("WghtVar.glyphs")).unwrap());
    }

    #[test]
    fn read_wght_var_3_metrics() {
        assert_wght_var_metrics(&Font::load(&glyphs3_dir().join("WghtVar.glyphs")).unwrap());
    }

    /// So far we don't have any package-only examples
    enum LoadCompare {
        Glyphs,
        GlyphsAndPackage,
    }

    /// Asserts that two Font structs are equal, excluding localized name entries.
    /// This is useful for comparing v2 and v3 Glyphs files where localized names
    /// may differ due to format changes. Only default (dflt/default/ENG) names are compared.
    fn assert_fonts_equal_sans_localized_names(f1: &Font, f2: &Font, context: &str) {
        let mut f1_clone = f1.clone();
        let mut f2_clone = f2.clone();
        // Filter all_names to keep only default entries (dflt/default/ENG)
        for all_names in [&mut f1_clone.all_names, &mut f2_clone.all_names] {
            for values in all_names.values_mut() {
                values.retain(|(lang, _)| matches!(lang.as_str(), "dflt" | "default" | "ENG"));
            }
        }
        assert_eq!(f1_clone, f2_clone, "{context}");
    }

    /// Asserts that two Font structs are equal, including localized name entries.
    /// This is useful for comparing two v3 Glyphs files where all fields should match.
    fn assert_fonts_equal(f1: &Font, f2: &Font, context: &str) {
        assert_eq!(f1, f2, "{context}");
    }

    fn assert_load_v2_matches_load_v3(name: &str, compare: LoadCompare) {
        let has_package = matches!(compare, LoadCompare::GlyphsAndPackage);
        let _ = env_logger::builder().is_test(true).try_init();
        let filename = format!("{name}.glyphs");
        let pkgname = format!("{name}.glyphspackage");
        let g2_file = glyphs2_dir().join(filename.clone());
        let g3_file = glyphs3_dir().join(filename.clone());
        let g2 = Font::load(&g2_file).unwrap();
        let g3 = Font::load(&g3_file).unwrap();

        // Handy if troubleshooting
        std::fs::write("/tmp/g2.glyphs.txt", format!("{g2:#?}")).unwrap();
        std::fs::write("/tmp/g3.glyphs.txt", format!("{g3:#?}")).unwrap();

        assert_fonts_equal_sans_localized_names(
            &g2,
            &g3,
            &format!("g2 vs g3: {g2_file:?} vs {g3_file:?}"),
        );

        if has_package {
            let g2_pkg = Font::load(&glyphs2_dir().join(pkgname.clone())).unwrap();
            let g3_pkg = Font::load(&glyphs3_dir().join(pkgname.clone())).unwrap();

            std::fs::write("/tmp/g2.glyphspackage.txt", format!("{g2_pkg:#?}")).unwrap();
            std::fs::write("/tmp/g3.glyphspackage.txt", format!("{g3_pkg:#?}")).unwrap();

            assert_fonts_equal_sans_localized_names(&g2_pkg, &g3_pkg, "g2_pkg vs g3_pkg");
            assert_fonts_equal(&g3_pkg, &g3, "g3_pkg vs g3");
        }
    }

    #[test]
    fn read_wght_var_2_and_3() {
        assert_load_v2_matches_load_v3("WghtVar", LoadCompare::GlyphsAndPackage);
    }

    #[test]
    fn read_wght_var_avar_2_and_3() {
        assert_load_v2_matches_load_v3("WghtVar_Avar", LoadCompare::GlyphsAndPackage);
    }

    #[test]
    fn read_wght_var_instances_2_and_3() {
        assert_load_v2_matches_load_v3("WghtVar_Instances", LoadCompare::GlyphsAndPackage);
    }

    #[test]
    fn read_wght_var_os2_2_and_3() {
        assert_load_v2_matches_load_v3("WghtVar_OS2", LoadCompare::GlyphsAndPackage);
    }

    #[test]
    fn read_wght_var_anchors_2_and_3() {
        assert_load_v2_matches_load_v3("WghtVar_Anchors", LoadCompare::GlyphsAndPackage);
    }

    #[test]
    fn read_infinity_2_and_3() {
        assert_load_v2_matches_load_v3("infinity", LoadCompare::GlyphsAndPackage);
    }

    #[test]
    fn read_wght_var_noexport_2_and_3() {
        assert_load_v2_matches_load_v3("WghtVar_NoExport", LoadCompare::Glyphs);
    }

    #[test]
    fn read_master_names_2_and_3() {
        assert_load_v2_matches_load_v3("MasterNames", LoadCompare::Glyphs);
    }

    #[test]
    fn read_master_names_with_italic_2_and_3() {
        assert_load_v2_matches_load_v3("MasterNames-Italic", LoadCompare::Glyphs);
    }

    fn only_shape_in_only_layer<'a>(font: &'a Font, glyph_name: &str) -> &'a Shape {
        let glyph = font.glyphs.get(glyph_name).unwrap();
        assert_eq!(1, glyph.layers.len());
        assert_eq!(1, glyph.layers[0].shapes.len());
        &glyph.layers[0].shapes[0]
    }

    fn check_v2_to_v3_transform(glyphs_file: &str, glyph_name: &str, expected: Affine) {
        let g2 = Font::load(&glyphs2_dir().join(glyphs_file)).unwrap();
        let g3 = Font::load(&glyphs3_dir().join(glyphs_file)).unwrap();

        // We're exclusively interested in the transform
        let g2_shape = only_shape_in_only_layer(&g2, glyph_name);
        let g3_shape = only_shape_in_only_layer(&g3, glyph_name);

        let Shape::Component(g2_shape) = g2_shape else {
            panic!("{g2_shape:?} should be a component");
        };
        let Shape::Component(g3_shape) = g3_shape else {
            panic!("{g3_shape:?} should be a component");
        };

        assert_eq!(expected, round(g2_shape.transform, 4));
        assert_eq!(expected, round(g3_shape.transform, 4));
    }

    #[test]
    fn read_transformed_component_2_and_3_uniform_scale() {
        let expected = Affine::new([1.6655, 1.1611, -1.1611, 1.6655, -233.0, -129.0]);
        check_v2_to_v3_transform("Component.glyphs", "comma", expected);
    }

    #[test]
    fn read_transformed_component_2_and_3_nonuniform_scale() {
        let expected = Affine::new([0.8452, 0.5892, -1.1611, 1.6655, -233.0, -129.0]);
        check_v2_to_v3_transform("Component.glyphs", "non_uniform_scale", expected);
    }

    #[test]
    fn upgrade_2_to_3_with_implicit_axes() {
        let font = Font::load(&glyphs2_dir().join("WghtVar_ImplicitAxes.glyphs")).unwrap();
        assert_eq!(
            font.axes
                .iter()
                .map(|a| a.tag.as_str())
                .collect::<Vec<&str>>(),
            vec!["wght", "wdth", "XXXX"]
        );
    }

    #[test]
    fn understand_v2_style_unquoted_hex_unicode() {
        let font = Font::load(&glyphs2_dir().join("Unicode-UnquotedHex.glyphs")).unwrap();
        assert_eq!(
            BTreeSet::from([0x1234]),
            font.glyphs.get("name").unwrap().unicode,
        );
        assert_eq!(1, font.glyphs.len());
    }

    #[test]
    fn understand_v2_style_quoted_hex_unicode_sequence() {
        let font = Font::load(&glyphs2_dir().join("Unicode-QuotedHexSequence.glyphs")).unwrap();
        assert_eq!(
            BTreeSet::from([0x2044, 0x200D, 0x2215]),
            font.glyphs.get("name").unwrap().unicode,
        );
        assert_eq!(1, font.glyphs.len());
    }

    #[test]
    fn understand_v3_style_unquoted_decimal_unicode() {
        let font = Font::load(&glyphs3_dir().join("Unicode-UnquotedDec.glyphs")).unwrap();
        assert_eq!(
            BTreeSet::from([182]),
            font.glyphs.get("name").unwrap().unicode
        );
        assert_eq!(1, font.glyphs.len());
    }

    #[test]
    fn understand_v3_style_unquoted_decimal_unicode_sequence() {
        let font = Font::load(&glyphs3_dir().join("Unicode-UnquotedDecSequence.glyphs")).unwrap();
        assert_eq!(
            BTreeSet::from([1619, 1764]),
            font.glyphs.get("name").unwrap().unicode,
        );
        assert_eq!(1, font.glyphs.len());
    }

    #[test]
    fn axes_not_hidden() {
        let font = Font::load(&glyphs3_dir().join("WghtVar.glyphs")).unwrap();
        assert_eq!(
            font.axes.iter().map(|a| a.hidden).collect::<Vec<_>>(),
            vec![None]
        );
    }

    #[test]
    fn axis_hidden() {
        let font = Font::load(&glyphs3_dir().join("WghtVar_3master_CustomOrigin.glyphs")).unwrap();
        assert_eq!(
            font.axes.iter().map(|a| a.hidden).collect::<Vec<_>>(),
            vec![Some(true)]
        );
    }

    #[test]
    fn vf_origin_single_axis_default() {
        let font = Font::load(&glyphs3_dir().join("WghtVar.glyphs")).unwrap();
        assert_eq!(0, font.default_master_idx);
    }

    #[test]
    fn vf_origin_multi_axis_default() {
        let font = Font::load(&glyphs2_dir().join("WghtVar_ImplicitAxes.glyphs")).unwrap();
        assert_eq!(0, font.default_master_idx);
    }

    #[test]
    fn vf_origin_multi_axis_custom() {
        let font = Font::load(&glyphs3_dir().join("WghtVar_3master_CustomOrigin.glyphs")).unwrap();
        assert_eq!(2, font.default_master_idx);
    }

    #[test]
    fn vf_origin_unquoted_string() {
        // the 'Variable Font Origin' custom parameter has the value `m01`,
        // un un-quoted plist string, which happens to be the default master.id
        // that Glyphs.app assigns to the predefined 'Regular' master that any
        // "New Font" comes with when it is first crated.
        // We just test that we do not crash attempting to parse the unquoted
        // string as an integer.
        let font = Font::load(&glyphs3_dir().join("CustomOrigin.glyphs")).unwrap();
        assert_eq!(1, font.default_master_idx);
    }

    #[rstest]
    #[case::base_style_without_regular(
        &[
            "Expanded Thin Italic",
            "Expanded Italic",
            "Expanded Bold Italic",
        ],
        "Expanded Italic"  // is common and exactly matches [1]
    )]
    #[case::base_style_contains_regular(
        &[
            "Regular Foo Bar",
            "Regular Foo Baz",
            "Regular Foo",
        ],
        "Regular Foo" // is common and exactly matches [2]
    )]
    #[case::base_style_with_regular_omitted(
        &[
            "Condensed Thin",
            "Condensed Light",
            "Condensed Regular",
        ],
        // "Condensed" is common and matches "Condensed Regular" when "Regular" is ignored
        "Condensed Regular"
    )]
    // "" is common and matches "Regular when "Regular" is ignored
    #[case::default_to_regular(
        &["Thin", "Light", "Regular", "Medium", "Bold"],
        "Regular"
    )]
    // "" is common, nothing matches, just take the first
    #[case::default_to_first(&["Foo", "Bar", "Baz"], "Foo")]
    fn find_default_master(#[case] master_names: &[&str], #[case] expected: &str) {
        let mut font = RawFont::default();
        for name in master_names {
            let master = RawFontMaster {
                name: Some(name.to_string()),
                ..Default::default()
            };
            font.font_master.push(master);
        }

        let idx = default_master_idx(&mut font);

        assert_eq!(expected, font.font_master[idx].name.as_deref().unwrap());
    }

    #[test]
    fn glyph_order_default_is_file_order() {
        let font = Font::load(&glyphs3_dir().join("WghtVar.glyphs")).unwrap();
        assert_eq!(
            vec![
                "space",
                "exclam",
                "hyphen",
                "bracketleft",
                "bracketright",
                "manual-component"
            ],
            font.glyph_order
        );
    }

    #[test]
    fn glyph_order_override_obeyed() {
        let _ = env_logger::builder().is_test(true).try_init();
        let font = Font::load(&glyphs3_dir().join("WghtVar_GlyphOrder.glyphs")).unwrap();
        assert_eq!(vec!["hyphen", "space", "exclam"], font.glyph_order);
    }

    #[test]
    fn loads_global_axis_mappings_from_glyphs2() {
        let font = Font::load(&glyphs2_dir().join("OpszWghtVar_AxisMappings.glyphs")).unwrap();

        // Did you load the mappings? DID YOU?!
        assert_eq!(
            UserToDesignMapping(BTreeMap::from([
                (
                    "Optical Size".into(),
                    AxisUserToDesignMap(vec![
                        (OrderedFloat(12.0), OrderedFloat(12.0)),
                        (OrderedFloat(72.0), OrderedFloat(72.0))
                    ])
                ),
                (
                    "Weight".into(),
                    AxisUserToDesignMap(vec![
                        (OrderedFloat(100.0), OrderedFloat(40.0)),
                        (OrderedFloat(200.0), OrderedFloat(46.0)),
                        (OrderedFloat(300.0), OrderedFloat(51.0)),
                        (OrderedFloat(400.0), OrderedFloat(57.0)),
                        (OrderedFloat(450.0), OrderedFloat(57.0)), // duplicate value is valid
                        (OrderedFloat(500.0), OrderedFloat(62.0)),
                        (OrderedFloat(600.0), OrderedFloat(68.0)),
                        (OrderedFloat(700.0), OrderedFloat(73.0)),
                    ])
                ),
            ])),
            font.axis_mappings
        );
    }

    #[test]
    fn loads_global_axis_locations_from_glyphs3() {
        let font = Font::load(&glyphs3_dir().join("WghtVar_AxisLocation.glyphs")).unwrap();

        // Did you load the mappings? DID YOU?!
        assert_eq!(
            UserToDesignMapping(BTreeMap::from([(
                "Weight".into(),
                AxisUserToDesignMap(vec![
                    (OrderedFloat(400.0), OrderedFloat(0.0)),
                    (OrderedFloat(500.0), OrderedFloat(8.0)),
                    (OrderedFloat(700.0), OrderedFloat(10.0)),
                    // this comes from the 'SemiBold' instance's 'Axis Location'
                    (OrderedFloat(600.0), OrderedFloat(8.5)),
                    // the 'SemiMedium' instance contains no explict 'Axis Location' so
                    // it doesn't show up (implicit mappings are ignored in this case)
                    // (OrderedFloat(450.0), OrderedFloat(4.0)),
                ])
            ),])),
            font.axis_mappings
        );
    }

    #[test]
    fn loads_global_axis_mappings_from_instances_wght_glyphs3() {
        let font = Font::load(&glyphs3_dir().join("WghtVar_Avar_From_Instances.glyphs")).unwrap();

        let wght_idx = font.axes.iter().position(|a| a.tag == "wght").unwrap();
        assert_eq!(
            vec![60.0, 80.0, 132.0],
            font.masters
                .iter()
                .map(|m| m.axes_values[wght_idx].into_inner())
                .collect::<Vec<_>>()
        );
        // the default master is the 'Bold' in this test font
        assert_eq!(
            (132.0, 2),
            (
                font.default_master().axes_values[wght_idx].into_inner(),
                font.default_master_idx
            )
        );

        // Did you load the mappings? DID YOU?!
        assert_eq!(
            UserToDesignMapping(BTreeMap::from([(
                "Weight".into(),
                AxisUserToDesignMap(vec![
                    (OrderedFloat(300.0), OrderedFloat(60.0)),
                    // we expect a map 400:80 here, even though the 'Regular' instance's
                    // Weight Class property is omitted in the .glyphs source because it
                    // is equal to its default value (400):
                    // https://github.com/googlefonts/fontc/issues/905
                    (OrderedFloat(400.0), OrderedFloat(80.0)),
                    (OrderedFloat(500.0), OrderedFloat(100.0)),
                    (OrderedFloat(700.0), OrderedFloat(132.0)),
                ])
            ),])),
            font.axis_mappings
        );
    }

    #[test]
    fn loads_global_axis_mappings_from_instances_wdth_glyphs3() {
        let font = Font::load(&glyphs3_dir().join("WdthVar.glyphs")).unwrap();

        assert_eq!(font.axes.len(), 1);
        assert_eq!(font.axes[0].tag, "wdth");
        assert_eq!(
            vec![22.0, 62.0],
            font.masters
                .iter()
                .map(|m| m.axes_values[0].into_inner())
                .collect::<Vec<_>>()
        );
        // the default master is the 'Condensed' in this test font
        assert_eq!(
            (22.0, 0),
            (
                font.default_master().axes_values[0].into_inner(),
                font.default_master_idx
            )
        );
        // Did you load the mappings? DID YOU?!
        assert_eq!(
            UserToDesignMapping(BTreeMap::from([(
                "Width".into(),
                AxisUserToDesignMap(vec![
                    // The "1: Ultra-condensed" instance width class corresponds to a
                    // `wdth` of 50 (user-space), in turn mapped to 22 (design-space).
                    (OrderedFloat(50.0), OrderedFloat(22.0)),
                    // We expect a map 100:41 here, even though the 'Regular' instance's
                    // Width Class property is omitted in the .glyphs source because it
                    // is equal to its default value "5: Medium (normal)" (or wdth=100):
                    // https://github.com/googlefonts/fontc/issues/905
                    (OrderedFloat(100.0), OrderedFloat(41.0)),
                    // The "9: Ultra-expanded" instance width class corresponds to a
                    // `wdth` of 200 (user-space), in turn mapped to 62 (design-space).
                    (OrderedFloat(200.0), OrderedFloat(62.0)),
                ])
            ),])),
            font.axis_mappings
        );
    }

    #[test]
    fn fea_for_class() {
        let font = Font::load(&glyphs2_dir().join("Fea_Class.glyphs")).unwrap();
        assert_eq!(
            vec![
                concat!("# automatic\n", "@Uppercase = [ A B C\n", "];",),
                concat!("@Lowercase = [ a b c\n", "];",),
            ],
            font.features
                .iter()
                .filter_map(|f| f.str_if_enabled())
                .collect::<Vec<_>>()
        )
    }

    #[test]
    fn fea_for_prefix() {
        let font = Font::load(&glyphs2_dir().join("Fea_Prefix.glyphs")).unwrap();
        assert_eq!(
            vec![
                "\
# Prefix: Languagesystems
# automatic
languagesystem DFLT dflt;

languagesystem latn dflt;
and more;
",
                "# Prefix: \n# automatic\nthanks for all the fish;",
            ],
            font.features
                .iter()
                .filter_map(|f| f.str_if_enabled())
                .collect::<Vec<_>>()
        )
    }

    #[test]
    fn fea_for_feature() {
        let font = Font::load(&glyphs2_dir().join("Fea_Feature.glyphs")).unwrap();
        assert_eq!(
            vec![
                "\
feature aalt {
feature locl;
feature tnum;
} aalt;",
                "\
feature ccmp {
# automatic
lookup ccmp_Other_2 {
  sub @Markscomb' @MarkscombCase by @MarkscombCase;
  sub @MarkscombCase @Markscomb' by @MarkscombCase;
} ccmp_Other_2;

etc;
} ccmp;",
            ],
            font.features
                .iter()
                .filter_map(|f| f.str_if_enabled())
                .collect::<Vec<_>>()
        )
    }

    #[test]
    fn fea_order() {
        let font = Font::load(&glyphs2_dir().join("Fea_Order.glyphs")).unwrap();
        assert_eq!(
            vec![
                "@class_first = [ meh\n];",
                "# Prefix: second\nmeh",
                "feature third {\nmeh\n} third;",
            ],
            font.features
                .iter()
                .filter_map(|f| f.str_if_enabled())
                .collect::<Vec<_>>()
        )
    }

    #[test]
    fn fea_labels() {
        let font = Font::load(&glyphs3_dir().join("Fea_Labels.glyphs")).unwrap();
        assert_eq!(
            vec![
                concat!(
                    "feature ss01 {\n",
                    "# automatic\n",
                    "featureNames {\n",
                    "  name 3 1 0x0409 \"Test 1\";\n",
                    "  name 3 1 0x0C01 \"اختبار ١\";\n",
                    "};\n",
                    "sub a by a.ss01;\n",
                    "sub b by b.ss01;\n\n",
                    "} ss01;",
                ),
                concat!(
                    "feature ss02 {\n",
                    "featureNames {\n",
                    "  name 3 1 0x0409 \"Test 2\";\n",
                    "};\n",
                    "sub c by c.alt;\n",
                    "} ss02;",
                ),
            ],
            font.features
                .iter()
                .filter_map(|f| f.str_if_enabled())
                .collect::<Vec<_>>()
        )
    }

    #[test]
    fn tags_make_excellent_names() {
        let raw = RawFeature {
            tag: Some("aalt".to_string()),
            code: "blah".to_string(),
            ..Default::default()
        };
        assert_eq!("aalt", raw.name().unwrap());
    }

    #[test]
    fn manual_kern_always_gets_insert_mark() {
        let feature = RawFeature {
            tag: Some("kern".into()),
            ..Default::default()
        };

        assert!(
            feature
                .raw_feature_to_feature()
                .unwrap()
                .content
                .contains("# Automatic Code")
        )
    }

    #[test]
    fn but_automatic_does_not() {
        let feature = RawFeature {
            tag: Some("kern".into()),
            automatic: Some(1),
            ..Default::default()
        };

        assert!(
            !feature
                .raw_feature_to_feature()
                .unwrap()
                .content
                .contains("# Automatic Code")
        )
    }

    #[test]
    fn v2_to_v3_simple_names() {
        let v2 = Font::load(&glyphs2_dir().join("WghtVar.glyphs")).unwrap();
        let v3 = Font::load(&glyphs3_dir().join("WghtVar.glyphs")).unwrap();
        // Compare only default names - v2 and v3 may have different localized entries
        let v2_defaults: BTreeMap<_, _> = v2.default_names().collect();
        let v3_defaults: BTreeMap<_, _> = v3.default_names().collect();
        assert_eq!(v3_defaults, v2_defaults);
    }

    #[test]
    fn v2_to_v3_more_names() {
        let v2 = Font::load(&glyphs2_dir().join("TheBestNames.glyphs")).unwrap();
        let v3 = Font::load(&glyphs3_dir().join("TheBestNames.glyphs")).unwrap();

        // V2 root-level fields (designer, manufacturer, copyright, etc.) are converted
        // to properties format by v2_to_v3_names(), so v2 and v3 should have identical defaults
        let v2_defaults: BTreeMap<_, _> = v2.default_names().collect();
        let v3_defaults: BTreeMap<_, _> = v3.default_names().collect();

        assert_eq!(v2_defaults, v3_defaults);
    }

    #[test]
    fn v2_long_param_names() {
        let v2 = Font::load(&glyphs2_dir().join("LongParamNames.glyphs")).unwrap();
        assert_eq!(v2.get_default_name("vendorID"), Some("DERP"));
        assert_eq!(
            v2.get_default_name("descriptions"),
            Some("legacy description")
        );
        assert_eq!(
            v2.get_default_name("licenseURL"),
            Some("www.example.com/legacy")
        );
        assert_eq!(v2.get_default_name("version"), Some("legacy version"));
        assert_eq!(v2.get_default_name("licenses"), Some("legacy license"));
        assert_eq!(v2.get_default_name("uniqueID"), Some("legacy unique id"));
        assert_eq!(
            v2.get_default_name("sampleTexts"),
            Some("legacy sample text")
        );
    }

    #[test]
    fn v2_style_names_in_a_v3_file() {
        let v3_mixed_with_v2 =
            Font::load(&glyphs3_dir().join("TheBestV2NamesInAV3File.glyphs")).unwrap();
        let v3 = Font::load(&glyphs3_dir().join("TheBestNames.glyphs")).unwrap();

        // V3 file with v2-style root-level fields (copyright, designer, etc.) gets those
        // converted to properties format by v2_to_v3_names(), so should match the pure v3 file
        let v3_mixed_defaults: BTreeMap<_, _> = v3_mixed_with_v2.default_names().collect();
        let v3_defaults: BTreeMap<_, _> = v3.default_names().collect();

        assert_eq!(v3_mixed_defaults, v3_defaults);
    }

    #[test]
    fn no_english_names_fallback() {
        // Test file with NO dflt/default/ENG entries, only FRA, DEU, ITA, ESP, JPN
        // This validates that the fallback uses insertion-order first, and not
        // alphabetical first, and that the overlap between default_names() and
        // localized_names() is as intended.
        let font = Font::load(&glyphs3_dir().join("NoEnglishNames.glyphs")).unwrap();

        // copyrights: FRA (1st), DEU (2nd), JPN (3rd) => should use FRA
        assert_eq!(
            font.get_default_name("copyrights"),
            Some("Droit d'auteur français")
        );

        // designers: FRA (1st), DEU (2nd) => should use FRA
        assert_eq!(
            font.get_default_name("designers"),
            Some("Concepteur français")
        );

        // manufacturers: ITA (1st), ESP (2nd) => should use ITA
        assert_eq!(
            font.get_default_name("manufacturers"),
            Some("Produttore italiano")
        );

        let localized: Vec<_> = font.localized_names().collect();

        // The FRA copyright should appear in both default and localized
        assert!(localized.contains(&("copyrights", "FRA", "Droit d'auteur français")));
        // The DEU and JPN copyright should only appear in localized
        assert!(localized.contains(&("copyrights", "DEU", "Deutsches Urheberrecht")));
        assert!(localized.contains(&("copyrights", "JPN", "日本の著作権")));
    }

    fn assert_wghtvar_avar_master_and_axes(glyphs_file: &Path) {
        let font = Font::load(glyphs_file).unwrap();
        let wght_idx = font.axes.iter().position(|a| a.tag == "wght").unwrap();
        assert_eq!(
            vec![300.0, 400.0, 700.0],
            font.masters
                .iter()
                .map(|m| m.axes_values[wght_idx].into_inner())
                .collect::<Vec<_>>()
        );
        assert_eq!(
            (400.0, 1),
            (
                font.default_master().axes_values[wght_idx].into_inner(),
                font.default_master_idx
            )
        );
    }

    #[test]
    fn favor_regular_as_origin_glyphs2() {
        assert_wghtvar_avar_master_and_axes(&glyphs2_dir().join("WghtVar_Avar.glyphs"));
    }

    #[test]
    fn favor_regular_as_origin_glyphs3() {
        assert_wghtvar_avar_master_and_axes(&glyphs3_dir().join("WghtVar_Avar.glyphs"));
    }

    #[test]
    fn have_all_the_best_instances() {
        let font = Font::load(&glyphs3_dir().join("WghtVar_Instances.glyphs")).unwrap();
        assert_eq!(
            vec![
                ("Regular", vec![("Weight", 400.0)]),
                ("Bold", vec![("Weight", 700.0)])
            ],
            font.instances
                .iter()
                .map(|inst| (
                    inst.name.as_str(),
                    font.axes
                        .iter()
                        .zip(&inst.axes_values)
                        .map(|(a, v)| (a.name.as_str(), v.0))
                        .collect::<Vec<_>>()
                ))
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn read_typo_whatsits() {
        let font = Font::load(&glyphs2_dir().join("WghtVar_OS2.glyphs")).unwrap();
        let master = font.default_master();
        assert_eq!(Some(1193), master.custom_parameters.typo_ascender);
        assert_eq!(Some(-289), master.custom_parameters.typo_descender);
    }

    #[test]
    fn read_os2_flags_default_set() {
        let font = Font::load(&glyphs2_dir().join("WghtVar.glyphs")).unwrap();
        assert_eq!(font.custom_parameters.use_typo_metrics, Some(true));
        assert_eq!(font.custom_parameters.has_wws_names, Some(true));
    }

    #[test]
    fn read_os2_flags_default_unset() {
        let font = Font::load(&glyphs2_dir().join("WghtVar_OS2.glyphs")).unwrap();
        assert_eq!(font.custom_parameters.use_typo_metrics, None);
        assert_eq!(font.custom_parameters.has_wws_names, None);
    }

    #[test]
    fn read_simple_kerning() {
        let font = Font::load(&glyphs3_dir().join("WghtVar.glyphs")).unwrap();
        assert_eq!(
            HashSet::from(["m01", "E09E0C54-128D-4FEA-B209-1B70BEFE300B",]),
            font.kerning_ltr
                .keys()
                .map(|k| k.as_str())
                .collect::<HashSet<_>>()
        );

        let actual_groups: Vec<_> = font
            .glyphs
            .iter()
            .filter_map(|(name, glyph)| {
                if glyph.left_kern.is_some() || glyph.right_kern.is_some() {
                    Some((
                        name.as_str(),
                        glyph.left_kern.as_deref(),
                        glyph.right_kern.as_deref(),
                    ))
                } else {
                    None
                }
            })
            .collect();

        let actual_kerning = font
            .kerning_ltr
            .get("m01")
            .unwrap()
            .iter()
            .map(|((n1, n2), value)| (n1.as_str(), n2.as_str(), value.0))
            .collect::<Vec<_>>();

        assert_eq!(
            (
                vec![
                    ("bracketleft", Some("bracketleft_L"), Some("bracketleft_R")),
                    (
                        "bracketright",
                        Some("bracketright_L"),
                        Some("bracketright_R")
                    ),
                ],
                vec![
                    ("@MMK_L_bracketleft_R", "exclam", -165.),
                    ("bracketleft", "bracketright", -300.),
                    ("exclam", "@MMK_R_bracketright_L", -160.),
                    ("exclam", "exclam", -360.),
                    ("exclam", "hyphen", 20.),
                    ("hyphen", "hyphen", -150.),
                ],
            ),
            (actual_groups, actual_kerning),
            "{:?}",
            font.kerning_ltr
        );
    }

    #[test]
    fn kern_floats() {
        let font = Font::load(&glyphs3_dir().join("KernFloats.glyphs")).unwrap();

        let kerns = font.kerning_ltr.get("m01").unwrap();
        let key = ("space".to_smolstr(), "space".to_smolstr());
        assert_eq!(kerns.get(&key), Some(&OrderedFloat(4.2001)));
    }

    #[test]
    fn read_simple_anchor() {
        let font = Font::load(&glyphs3_dir().join("WghtVar_Anchors.glyphs")).unwrap();
        assert_eq!(
            vec![
                ("m01", "top", Point::new(300.0, 700.0)),
                ("l2", "top", Point::new(325.0, 725.0))
            ],
            font.glyphs
                .get("A")
                .unwrap()
                .layers
                .iter()
                .flat_map(|l| l.anchors.iter().map(|a| (
                    l.layer_id.as_str(),
                    a.name.as_str(),
                    a.pos
                )))
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn read_export_glyph() {
        let font = Font::load(&glyphs3_dir().join("WghtVar_NoExport.glyphs")).unwrap();
        assert_eq!(
            vec![
                ("bracketleft", true),
                ("bracketright", true),
                ("exclam", true),
                ("hyphen", false),
                ("manual-component", true),
                ("space", true),
            ],
            font.glyphs
                .iter()
                .map(|(name, glyph)| (name.as_str(), glyph.export))
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn read_fstype_none() {
        let font = Font::load(&glyphs3_dir().join("infinity.glyphs")).unwrap();
        assert!(font.custom_parameters.fs_type.is_none());
    }

    #[test]
    fn read_fstype_zero() {
        let font = Font::load(&glyphs3_dir().join("fstype_0x0000.glyphs")).unwrap();
        assert_eq!(Some(0), font.custom_parameters.fs_type);
    }

    #[test]
    fn read_fstype_bits() {
        let font = Font::load(&glyphs3_dir().join("fstype_0x0104.glyphs")).unwrap();
        assert_eq!(Some(0x104), font.custom_parameters.fs_type);
    }

    #[test]
    fn anchor_components() {
        let font = Font::load(&glyphs3_dir().join("ComponentAnchor.glyphs")).unwrap();
        let glyph = font.glyphs.get("A_Aacute").unwrap();
        let acute_comb = glyph.layers[0]
            .shapes
            .iter()
            .find_map(|shape| match shape {
                Shape::Component(c) if c.name == "acutecomb" => Some(c),
                _ => None,
            })
            .unwrap();
        assert_eq!(acute_comb.anchor.as_deref(), Some("top_2"));
    }

    #[test]
    fn parse_alignment_zone_smoke_test() {
        assert_eq!(
            super::parse_alignment_zone("{1, -12}").map(|x| (x.0.0, x.1.0)),
            Some((1., -12.))
        );
        assert_eq!(
            super::parse_alignment_zone("{-5001, 12}").map(|x| (x.0.0, x.1.0)),
            Some((-5001., 12.))
        );
    }

    // a little helper used in tests below
    impl FontMaster {
        fn get_metric(&self, name: &str) -> Option<(f64, f64)> {
            self.metric_values
                .get(name)
                .map(|raw| (raw.pos.0, raw.over.0))
        }
    }

    #[test]
    fn v2_alignment_zones_to_metrics() {
        let font = Font::load(&glyphs2_dir().join("alignment_zones_v2.glyphs")).unwrap();
        let master = font.default_master();

        assert_eq!(master.get_metric("ascender"), Some((800., 17.)));
        assert_eq!(master.get_metric("cap height"), Some((700., 16.)));
        assert_eq!(master.get_metric("baseline"), Some((0., -16.)));
        assert_eq!(master.get_metric("descender"), Some((-200., -17.)));
        assert_eq!(master.get_metric("x-height"), Some((500., 15.)));
        assert_eq!(master.get_metric("italic angle"), None);
    }

    #[test]
    fn v3_duplicate_metrics_first_wins() {
        // In this test font, the default master contains two 'x-height' metric values,
        // the first (501) that applies globally, and a second one (450) that applies
        // only to small-caps, using GSMetric's `filter` attribute which we ignore as
        // it is not relevant to build OS/2 and MVAR tables.
        // We match glyphsLib and only consider the first metric with a given name.
        let font = Font::load(&glyphs3_dir().join("WghtVar_OS2.glyphs")).unwrap();
        let master = font.default_master();

        assert_eq!(master.get_metric("x-height"), Some((501., 0.)));
    }

    #[test]
    fn v2_preserve_custom_alignment_zones() {
        let font = Font::load(&glyphs2_dir().join("alignment_zones_v2.glyphs")).unwrap();
        let master = font.default_master();
        assert_eq!(master.get_metric("zone 1"), Some((1000., 20.)));
        assert_eq!(master.get_metric("zone 2"), Some((-100., -15.)));
    }

    // If category is unknown, we should ignore and compute it
    #[test]
    fn unknown_glyph_category() {
        let raw = super::RawGlyph {
            glyphname: "A".into(),
            category: Some("Fake".into()),
            ..Default::default()
        };

        let cooked = raw.build(FormatVersion::V2, &GlyphData::default()).unwrap();
        assert_eq!(
            (cooked.category, cooked.sub_category),
            (Some(Category::Letter), None)
        );
    }

    #[test]
    fn custom_params_disable() {
        let font = Font::load(&glyphs3_dir().join("custom_param_disable.glyphs")).unwrap();

        assert!(font.custom_parameters.fs_type.is_none())
    }

    #[test]
    fn parse_numbers() {
        let font = Font::load(&glyphs3_dir().join("number_value.glyphs")).unwrap();
        assert_eq!(
            font.masters[0].number_values.get("foo"),
            Some(&OrderedFloat(12.4f64))
        );
        assert_eq!(
            font.masters[1].number_values.get("foo"),
            Some(&OrderedFloat(0f64))
        );
    }

    #[test]
    fn read_font_metrics() {
        let font =
            Font::load(&glyphs3_dir().join("GlobalMetrics_font_customParameters.glyphs")).unwrap();
        assert_eq!(Some(950), font.custom_parameters.typo_ascender);
        assert_eq!(Some(-350), font.custom_parameters.typo_descender);
        assert_eq!(Some(0), font.custom_parameters.typo_line_gap);
        assert_eq!(Some(950), font.custom_parameters.hhea_ascender);
        assert_eq!(Some(-350), font.custom_parameters.hhea_descender);
        assert_eq!(Some(0), font.custom_parameters.hhea_line_gap);
        assert_eq!(Some(1185), font.custom_parameters.win_ascent);
        assert_eq!(Some(420), font.custom_parameters.win_descent);
        assert_eq!(
            Some(OrderedFloat(42_f64)),
            font.custom_parameters.underline_thickness
        );
        assert_eq!(
            Some(OrderedFloat(-300_f64)),
            font.custom_parameters.underline_position
        );
    }

    #[test]
    fn parse_legacy_stylistic_set_name() {
        let font = Font::load(&glyphs2_dir().join("FeaLegacyName.glyphs")).unwrap();
        assert_eq!(font.features.len(), 2);
        let [ss01, ss02] = font.features.as_slice() else {
            panic!("wrong number of features");
        };

        assert!(
            ss01.content
                .contains("name 3 1 0x409 \"Alternate placeholder\"")
        );
        assert!(!ss02.content.contains("name 3 1"))
    }

    // <https://github.com/googlefonts/fontc/issues/1175>
    #[test]
    fn one_italic_is_enough() {
        let font = Font::load(&glyphs2_dir().join("ItalicItalic.glyphs")).unwrap();
        for master in font.masters {
            let mut fragments = master.name.split_ascii_whitespace().collect::<Vec<_>>();
            fragments.sort();
            for words in fragments.windows(2) {
                assert!(
                    words[0] != words[1],
                    "Multiple instances of {} in {}",
                    words[0],
                    master.name
                );
            }
        }
    }

    // We had a bug where if a master wasn't at a mapping point the Axis Mapping was modified
    #[test]
    fn ignore_masters_if_axis_mapping() {
        let font = Font::load(&glyphs2_dir().join("MasterNotMapped.glyphs")).unwrap();
        let mapping = &font.axis_mappings.0.get("Weight").unwrap().0;
        assert_eq!(
            vec![
                (OrderedFloat(400_f64), OrderedFloat(40.0)),
                (OrderedFloat(700_f64), OrderedFloat(70.0))
            ],
            *mapping
        );
    }

    #[rstest]
    #[case::rotate_0(0.0, Affine::new([1.0, 0.0, 0.0, 1.0, 0.0, 0.0]))]
    #[case::rotate_360(360.0, Affine::new([1.0, 0.0, 0.0, 1.0, 0.0, 0.0]))]
    #[case::rotate_90(90.0, Affine::new([0.0, 1.0, -1.0, 0.0, 0.0, 0.0]))]
    #[case::rotate_minus_90(-90.0, Affine::new([0.0, -1.0, 1.0, 0.0, 0.0, 0.0]))]
    #[case::rotate_180(180.0, Affine::new([-1.0, 0.0, 0.0, -1.0, 0.0, 0.0]))]
    #[case::rotate_minus_180(-180.0, Affine::new([-1.0, 0.0, 0.0, -1.0, 0.0, 0.0]))]
    #[case::rotate_270(270.0, Affine::new([0.0, -1.0, 1.0, 0.0, 0.0, 0.0]))]
    #[case::rotate_minus_270(-270.0, Affine::new([0.0, 1.0, -1.0, 0.0, 0.0, 0.0]))]
    #[case::rotate_450(450.0, Affine::new([0.0, 1.0, -1.0, 0.0, 0.0, 0.0]))]
    #[case::rotate_540(540.0, Affine::new([-1.0, 0.0, 0.0, -1.0, 0.0, 0.0]))]
    fn cardinal_rotation_contain_exact_zeros_and_ones(
        #[case] angle: f64,
        #[case] expected: Affine,
    ) {
        // When Glyphs 3 components are rotated by a 90, 180 or 270 degree angle,
        // we want to match fontTools Transform.rotate() and have the sine and
        // cosine terms rounded exactly to 0.0 or ±1.0 to avoid unnecessary diffs
        // upon rounding transformed coordinates to integer:
        // https://github.com/googlefonts/fontc/issues/1218
        assert_eq!(expected, normalized_rotation(angle));
    }

    #[rstest]
    #[case::rotate_30(30.0, 4, Affine::new([0.866, 0.5, -0.5, 0.866, 0.0, 0.0]))]
    #[case::rotate_minus_30(-30.0, 4, Affine::new([0.866, -0.5, 0.5, 0.866, 0.0, 0.0]))]
    #[case::rotate_almost_90(
        90.01, 8, Affine::new([-0.00017453, 0.99999998, -0.99999998, -0.00017453, 0.0, 0.0])
    )]
    fn non_cardinal_rotation_left_untouched(
        #[case] angle: f64,
        #[case] precision: u8,
        #[case] expected: Affine,
    ) {
        // any other angles' sin and cos != (0, ±1) are passed through unchanged
        assert_eq!(expected, round(normalized_rotation(angle), precision));
    }

    #[test]
    fn parse_colrv1_identify_colr_glyphs() {
        let font = Font::load(&glyphs3_dir().join("COLRv1-gradient.glyphs")).unwrap();
        let expected_colr = HashSet::from(["A", "B", "C", "D", "K", "L", "M", "N"]);
        assert_eq!(
            expected_colr,
            font.glyphs
                .values()
                .filter(|g| g.layers.iter().all(|l| l.attributes.color))
                .map(|g| g.name.as_str())
                .collect::<HashSet<_>>()
        );
    }

    #[test]
    fn parse_colrv1_gradients() {
        let font = Font::load(&glyphs3_dir().join("COLRv1-gradient.glyphs")).unwrap();
        let expected_colr = HashSet::from([
            (
                "A",
                Gradient {
                    start: vec![OrderedFloat(0.1), OrderedFloat(0.1)],
                    end: vec![OrderedFloat(0.9), OrderedFloat(0.9)],
                    colors: vec![
                        ColorStop {
                            color: Color::rgba(255, 0, 0, 255),
                            stop_offset: 0.into(),
                        },
                        ColorStop {
                            color: Color::rgba(0, 0, 255, 255),
                            stop_offset: 1.into(),
                        },
                    ],
                    ..Default::default()
                },
            ),
            (
                "N",
                Gradient {
                    start: vec![OrderedFloat(1.0), OrderedFloat(1.0)],
                    end: vec![OrderedFloat(0.0), OrderedFloat(0.0)],
                    colors: vec![
                        ColorStop {
                            color: Color::rgba(255, 0, 0, 255),
                            stop_offset: 0.into(),
                        },
                        ColorStop {
                            color: Color::rgba(0, 0, 255, 255),
                            stop_offset: 1.into(),
                        },
                    ],
                    style: "circle".to_string(),
                },
            ),
        ]);
        assert_eq!(
            expected_colr,
            font.glyphs
                .values()
                .filter(|g| expected_colr.iter().any(|(name, _)| *name == g.name))
                .flat_map(|g| g.layers.iter().flat_map(|l| l.shapes.iter()).map(|s| (
                    g.name.as_str(),
                    s.attributes().gradient.as_ref().unwrap().clone()
                )))
                .collect::<HashSet<_>>()
        );
    }

    #[test]
    fn parse_grayscale_colors() {
        let font = Font::load(&glyphs3_dir().join("COLRv1-grayscale.glyphs")).unwrap();
        assert_eq!(
            vec![
                ColorStop {
                    color: Color::rgba(64, 64, 64, 255),
                    stop_offset: 0.into(),
                },
                ColorStop {
                    color: Color::rgba(0, 0, 0, 255),
                    stop_offset: 1.into(),
                },
            ],
            font.glyphs
                .values()
                .flat_map(
                    |g| g.layers.iter().flat_map(|l| l.shapes.iter()).flat_map(|s| s
                        .attributes()
                        .gradient
                        .iter()
                        .flat_map(|g| g.colors.iter())
                        .cloned())
                )
                .collect::<Vec<_>>()
        );
    }

    // we want to match glyphsLib and use a value of 600 if the width is missing.
    #[test]
    fn missing_width_no_problem() {
        let font = Font::load(&glyphs3_dir().join("MissingWidth.glyphs")).unwrap();
        let glyph = font.glyphs.get("widthless").unwrap();
        assert_eq!(glyph.layers[0].width, 600.);
    }

    #[test]
    fn read_preferred_names_from_properties() {
        let font = Font::load(&glyphs3_dir().join("PreferableNames.glyphs")).unwrap();
        assert_eq!(
            vec![
                Some("Pref Family Name"),
                Some("Pref Regular"),
                Some("Name 25?!")
            ],
            vec![
                font.get_default_name("preferredFamilyNames"),
                font.get_default_name("preferredSubfamilyNames"),
                font.get_default_name("variationsPostScriptNamePrefix"),
            ]
        );
    }

    #[test]
    fn zero_value_metrics() {
        let font = Font::load(&glyphs3_dir().join("ZeroMetrics.glyphs")).unwrap();
        let master = font.default_master();
        assert_eq!(master.ascender(), Some(789.));
        // this has no value set, but has overshoot set, so this should be a 0
        assert_eq!(master.cap_height(), Some(0.));
        // this has neither value nor overshoot, but since the metric is defined,
        // it's still here and still zero
        assert_eq!(master.x_height(), Some(0.));
        // but this metric is not defined in the font, so it it's `None`
        assert_eq!(master.italic_angle(), None);
    }

    /// If there is a variable instance and it does not have a set familyName,
    /// we should still use names stored in that instance.
    #[test]
    fn names_from_instances() {
        let font = Font::load(&glyphs3_dir().join("InstanceNames.glyphs")).unwrap();
        assert_eq!(
            font.get_default_name("preferredSubfamilyNames").unwrap(),
            "Italic"
        )
    }

    #[rstest]
    #[case::v2(glyphs2_dir())]
    #[case::v3(glyphs3_dir())]
    fn glyph_production_names(#[case] glyphs_dir: PathBuf) {
        let font = Font::load(&glyphs_dir.join("ProductionNames.glyphs")).unwrap();
        let glyphs = font.glyphs;

        // this glyph has no production name in GlyphData.xml nor in the .glyphs file
        assert_eq!(glyphs.get("A").unwrap().production_name, None);

        // this one would have 'dotlessi' in GlyphData.xml, but the .glyphs file overrides it
        assert_eq!(
            glyphs.get("idotless").unwrap().production_name,
            Some("uni0131".into())
        );

        // this has no custom production_name in the .glyphs file, and the one from
        // GlyphData.xml is used
        assert_eq!(
            glyphs.get("nbspace").unwrap().production_name,
            Some("uni00A0".into())
        );
    }

    #[test]
    fn parse_axis_rules() {
        let raw = RawFont::load(&glyphs3_dir().join("AxisRules.glyphs")).unwrap();
        let space = &raw.glyphs[0];
        let axes = &space.layers[0].attributes.axis_rules;
        assert_eq!(
            axes,
            &[
                AxisRule {
                    min: None,
                    max: Some(400)
                },
                AxisRule {
                    min: Some(100),
                    max: None,
                },
                AxisRule {
                    min: None,
                    max: None,
                },
            ]
        )
    }

    #[test]
    fn drop_axis_rules_if_no_assoc_master() {
        // as in, if it's a bracket layer it doesn't need axis rules
        let font = Font::load(&glyphs3_dir().join("AxisRules.glyphs")).unwrap();
        let space = font.glyphs.get("space").unwrap();
        assert!(space.layers[0].attributes.axis_rules.is_empty());
    }

    #[test]
    fn parse_v2_bracket_layers() {
        let font = Font::load(&glyphs2_dir().join("AxisRules.glyphs")).unwrap();
        let glyph = font.glyphs.get("space").unwrap();
        assert_eq!(
            glyph.bracket_layers[0].attributes.axis_rules,
            [
                AxisRule {
                    min: None,
                    max: Some(141)
                },
                AxisRule::default()
            ]
        );
    }

    #[test]
    fn parse_layer_normal() {
        for name in &["[60]", "Light Extended [ 60 ]"] {
            let rule = AxisRule::from_layer_name(name);
            assert_eq!(
                rule,
                Some(AxisRule {
                    min: Some(60),
                    max: None
                }),
                "{name}"
            )
        }
    }

    #[test]
    fn parse_layer_reversed() {
        for name in &["]60]", "Light Extended ] 60 ]"] {
            let rule = AxisRule::from_layer_name(name);
            assert_eq!(
                rule,
                Some(AxisRule {
                    min: None,
                    max: Some(60)
                })
            )
        }
    }

    #[test]
    fn parse_layer_fails() {
        for name in &["[hi]", "[45opsz]", "Medium [499‹wg]"] {
            assert!(AxisRule::from_layer_name(name).is_none(), "{name}")
        }
    }

    #[test]
    fn v2_bracket_layers() {
        let font = Font::load(&glyphs2_dir().join("WorkSans-minimal-bracketlayer.glyphs")).unwrap();
        let glyph = font.glyphs.get("colonsign").unwrap();
        assert_eq!(glyph.layers.len(), 3);
        assert_eq!(glyph.bracket_layers.len(), 3);

        assert!(
            glyph
                .layers
                .iter()
                .all(|l| l.attributes.axis_rules.is_empty())
        );
        assert!(
            glyph
                .bracket_layers
                .iter()
                .all(|l| !l.attributes.axis_rules.is_empty())
        );
    }

    #[test]
    fn v3_bracket_layers() {
        let font = Font::load(&glyphs3_dir().join("LibreFranklin-bracketlayer.glyphs")).unwrap();
        let glyph = font.glyphs.get("peso").unwrap();
        assert_eq!(glyph.layers.len(), 2);
        assert_eq!(glyph.bracket_layers.len(), 2);

        assert!(
            glyph
                .layers
                .iter()
                .all(|l| l.attributes.axis_rules.is_empty())
        );
        assert!(
            glyph
                .bracket_layers
                .iter()
                .all(|l| !l.attributes.axis_rules.is_empty())
        );
    }

    #[test]
    fn bracket_layers_where_only_brackets_have_a_component_and_it_has_anchors() {
        let font = Font::load(&glyphs2_dir().join("AlumniSans-wononly.glyphs")).unwrap();
        let glyph = font.glyphs.get("won").unwrap();

        assert_eq!(glyph.layers.len(), 2);
        assert_eq!(glyph.bracket_layers.len(), 2);

        for layer in glyph.layers.iter().chain(glyph.bracket_layers.iter()) {
            assert_eq!(layer.anchors.len(), 2, "{}", layer.layer_id);
        }
    }

    #[test]
    fn glyphs2_weight_class_custom_instance_parameter() {
        // older glyphs sources can use a custom param in the instance
        // in order to pass a custom value for the weight class.
        let font = Font::load(&glyphs2_dir().join("WeightClassCustomParam.glyphs")).unwrap();
        let mapping = &font.axis_mappings.0.get("Weight").unwrap().0;
        assert_eq!(
            mapping.as_slice(),
            &[
                (OrderedFloat(200f64), OrderedFloat(42f64)),
                (OrderedFloat(700.), OrderedFloat(125.)),
                (OrderedFloat(1000.), OrderedFloat(208.))
            ]
        )
    }

    #[test]
    fn glyphs2_avoid_adding_mapping_from_master() {
        // this source has a master which does not correspond to an instance,
        // but the instances provide a valid non-identity mapping; we should
        // avoid adding the mapping from the master.

        let font = Font::load(&glyphs2_dir().join("master_missing_from_instances.glyphs")).unwrap();

        let mapping = &font.axis_mappings.0.get("Weight").unwrap().0;
        assert_eq!(
            mapping.as_slice(),
            &[
                (OrderedFloat(100f64), OrderedFloat(20f64)),
                (OrderedFloat(300.), OrderedFloat(50.)),
                (OrderedFloat(400.), OrderedFloat(71.)),
                (OrderedFloat(700.), OrderedFloat(156.)),
                (OrderedFloat(900.), OrderedFloat(226.)),
            ]
        )
    }

    #[test]
    fn does_not_truncate_axis_location() {
        let font = Font::load(&glyphs3_dir().join("WghtVar_AxisLocationFloat.glyphs")).unwrap();
        let mapping = &font.axis_mappings.0.get("Weight").unwrap().0;

        // The user location 400.75 must *NOT* be truncated
        assert_eq!(
            mapping.as_slice(),
            &[(OrderedFloat(400.75), OrderedFloat(0f64)),]
        )
    }

    fn make_axis_location_params(values: &[(&str, i64)]) -> RawCustomParameterValue {
        RawCustomParameterValue {
            name: "Axis Location".into(),
            value: Plist::Array(
                values
                    .iter()
                    .map(|(name, loc)| {
                        Plist::Dictionary(BTreeMap::from([
                            ("Axis".into(), Plist::String(name.to_string())),
                            ("Location".into(), Plist::Integer(*loc)),
                        ]))
                    })
                    .collect(),
            ),
            disabled: None,
        }
    }

    #[test]
    fn user_to_design_with_no_axes() {
        let _ = env_logger::builder().is_test(true).try_init();
        let mut myfont = RawFont {
            font_master: vec![RawFontMaster {
                custom_parameters: RawCustomParameters(vec![make_axis_location_params(&[(
                    "Weight", 400,
                )])]),
                ..Default::default()
            }],
            ..Default::default()
        };

        let _just_dont_crash_plz = user_to_design_from_axis_location(&mut myfont);
    }

    #[test]
    fn user_to_design_with_unknown_axis_location() {
        let _ = env_logger::builder().is_test(true).try_init();
        let mut myfont = RawFont {
            axes: vec![Axis {
                name: "Weight".into(),
                tag: "wght".into(),
                hidden: None,
            }],
            font_master: vec![RawFontMaster {
                custom_parameters: RawCustomParameters(vec![make_axis_location_params(&[
                    ("Weight", 400),
                    ("Italic", 0),
                ])]),
                axes_values: vec![60.0.into()],
                ..Default::default()
            }],
            ..Default::default()
        };

        let _just_dont_crash_plz = user_to_design_from_axis_location(&mut myfont);
    }

    #[test]
    fn axis_location_string_value() {
        let raw = Plist::Dictionary(BTreeMap::from([
            ("Axis".into(), Plist::String("Weight".into())),
            ("Location".into(), Plist::String("42".into())),
        ]));
        assert_eq!(
            Some(AxisLocation {
                axis_name: "Weight".into(),
                location: 42.0.into(),
            }),
            raw.as_axis_location()
        );
    }

    #[test]
    fn glyphs2_unicode_ranges() {
        let font = Font::load(&glyphs2_dir().join("UnicodeRanges.glyphs")).unwrap();
        assert_eq!(
            Some(BTreeSet::from([8])),
            font.custom_parameters.unicode_range_bits
        );
    }

    #[test]
    fn parse_smart_components_v3() {
        // we load from raw directly so that we can skip preprocessing, which would
        // replace the components with the actual outlines
        let font = RawFont::load(&glyphs3_dir().join("SmartComponents.glyphs")).unwrap();
        let font = Font::try_from(font).unwrap();
        let shoulder = font.glyphs.get("_part.shoulder").unwrap();
        assert_eq!(
            shoulder.smart_component_axes,
            BTreeMap::from([
                ("shoulderWidth".into(), 0..=100),
                ("crotchDepth".into(), -100..=0)
            ])
        );

        let m = font.glyphs.get("m").unwrap();
        let expected = [
            Component {
                name: "_part.stem".into(),
                ..Default::default()
            },
            Component {
                name: "_part.shoulder".into(),
                transform: Affine::translate((17., 0.)),
                smart_component_values: BTreeMap::from([
                    ("crotchDepth".into(), -44.28304),
                    ("shoulderWidth".into(), 28.78011),
                ]),
                ..Default::default()
            },
            Component {
                name: "_part.shoulder".into(),
                transform: Affine::translate((180., 1.)),
                smart_component_values: BTreeMap::from([
                    ("crotchDepth".into(), -81.24796),
                    ("shoulderWidth".into(), 0.0),
                ]),
                ..Default::default()
            },
        ];
        assert_eq!(
            m.layers[0].components().cloned().collect::<Vec<_>>(),
            expected
        );
    }

    fn instantiate_shoulder_at(location: &[(&str, f64)]) -> Shape {
        fn make_component_values(inp: &[(&str, f64)]) -> BTreeMap<SmolStr, f64> {
            inp.iter().map(|(k, v)| (k.to_smolstr(), *v)).collect()
        }
        let font = RawFont::load(&glyphs3_dir().join("SmartComponents.glyphs")).unwrap();
        let font = Font::try_from(font).unwrap();

        let smart_comp = font.glyphs.get("_part.shoulder").unwrap();
        let default_pos = Component {
            name: "_part.shoulder".into(),
            smart_component_values: make_component_values(location),
            ..Default::default()
        };

        let shapes =
            smart_components::instantiate_for_layer("m01", &default_pos, smart_comp).unwrap();
        assert_eq!(shapes.len(), 1);
        shapes[0].clone()
    }

    #[test]
    fn instantiate_smart_component_at_default() {
        let font = Font::load(&glyphs3_dir().join("SmartComponents.glyphs")).unwrap();
        let smart_comp = font.glyphs.get("_part.shoulder").unwrap();
        let shape = instantiate_shoulder_at(&[("shoulderWidth", 100.), ("crotchDepth", 0.)]);

        assert_eq!(smart_comp.layers[1].layer_id, "m01");
        assert_eq!(shape, smart_comp.layers[1].shapes[0]);
    }

    #[test]
    fn instantiate_smart_component_at_not_default() {
        let Shape::Path(path) =
            instantiate_shoulder_at(&[("shoulderWidth", 0.), ("crotchDepth", -100.)])
        else {
            panic!("wat")
        };
        // the first point is at its lowest value (crotch -100) (first point is at end!)
        assert_eq!(path.nodes.last().unwrap().pt.round().y, 267.0);
        // the shoulder width means the max x is also at its lowest value
        assert_eq!(
            path.nodes.iter().map(|nd| nd.pt.x.round() as i64).max(),
            Some(335)
        );
    }

    impl crate::Path {
        fn to_points(&self) -> Vec<Point> {
            self.nodes.iter().map(|n| n.pt).collect()
        }
        fn bbox(&self) -> Option<Rect> {
            let points = self.to_points();
            let (head, rest) = points.as_slice().split_first()?;
            Some(
                rest.iter()
                    .fold(Rect::ZERO.with_origin(*head), |bbox, pt| bbox.union_pt(*pt)),
            )
        }
    }

    #[test]
    fn smart_component_v2() {
        let font = Font::load(&glyphs2_dir().join("SmartComponent.glyphs")).unwrap();

        let glyph = font.glyphs.get("composite").unwrap();
        let shapes = &glyph.layers[0].shapes;
        let paths = shapes
            .iter()
            .map(|s| s.as_path().unwrap().to_points())
            .collect::<Vec<_>>();
        let expected1 = [(9., 405.), (9., 300.), (276., 300.), (276.0, 405.0)]
            .into_iter()
            .map(Point::from)
            .collect::<Vec<_>>();

        let expected2 = [(9., 405.), (9., 383.), (276., 383.), (276.0, 405.0)]
            .into_iter()
            .map(|pt| Point::from(pt) + Vec2::new(100., 100.))
            .collect::<Vec<_>>();

        assert_eq!(paths, [expected1, expected2]);
    }

    #[test]
    fn nested_smart_components() {
        // ensure that if a smart component has its own smart components, those
        // get instantiated first.
        let font = Font::load(&glyphs2_dir().join("NestedSmartComponent.glyphs")).unwrap();
        let glyph = font.glyphs.get("w").unwrap();
        let shapes = &glyph.layers[0].shapes;
        let rects = shapes
            .iter()
            .map(|s| s.as_path().unwrap().bbox().unwrap())
            .collect::<Vec<_>>();
        assert_eq!(
            rects,
            [
                Rect::new(0., 200., 400., 400.),
                Rect::new(0., 0., 400., 100.),
                Rect::new(500., 200., 550., 400.),
                Rect::new(500., 0., 550., 100.),
            ]
        );
    }
}
