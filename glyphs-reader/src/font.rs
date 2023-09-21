//! The general strategy is just to use a plist for storage. Also, lots of
//! unwrapping.
//!
//! There are lots of other ways this could go, including something serde-like
//! where it gets serialized to more Rust-native structures, proc macros, etc.

use std::borrow::Cow;
use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::ffi::OsStr;
use std::hash::Hash;
use std::{fs, path};

use kurbo::{Affine, Point};
use log::{debug, log_enabled, trace, warn};
use ordered_float::OrderedFloat;
use regex::Regex;

use crate::error::Error;
use crate::from_plist::FromPlist;
use crate::plist::{Array, Dictionary, Plist};

const V3_METRIC_NAMES: [&str; 6] = [
    "ascender",
    "baseline",
    "descender",
    "cap height",
    "x-height",
    "italic angle",
];
const V2_METRIC_NAMES: [&str; 6] = [
    "ascender",
    "baseline",
    "descender",
    "capHeight",
    "xHeight",
    "italic angle",
];

#[derive(Clone, Debug, Default, PartialEq, Hash)]
pub struct RawUserToDesignMapping(BTreeMap<String, RawAxisUserToDesignMap>);

#[derive(Clone, Debug, Default, PartialEq, Hash)]
pub struct RawAxisUserToDesignMap(Vec<(OrderedFloat<f32>, OrderedFloat<f32>)>);

/// A tidied up font from a plist.
///
/// Normalized representation of Glyphs 2/3 content
#[derive(Debug, PartialEq, Hash)]
pub struct Font {
    pub units_per_em: u16,
    pub use_typo_metrics: Option<bool>,
    pub has_wws_names: Option<bool>,
    pub axes: Vec<Axis>,
    pub masters: Vec<FontMaster>,
    pub default_master_idx: usize,
    pub glyphs: BTreeMap<String, Glyph>,
    pub glyph_order: Vec<String>,
    pub glyph_to_codepoints: BTreeMap<String, BTreeSet<u32>>,
    // tag => (user:design) tuples
    pub axis_mappings: RawUserToDesignMapping,
    pub features: Vec<FeatureSnippet>,
    pub names: BTreeMap<String, String>,
    pub instances: Vec<Instance>,
    pub version_major: i32,
    pub version_minor: u32,
    pub date: Option<String>,

    // master id => { (name or class, name or class) => adjustment }
    pub kerning_ltr: BTreeMap<String, BTreeMap<(String, String), i32>>,
}

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct FeatureSnippet(String);

impl FeatureSnippet {
    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }
}

#[derive(Debug, PartialEq, Hash)]
pub struct Glyph {
    pub glyphname: String,
    pub layers: Vec<Layer>,
    /// The left kerning group
    pub left_kern: Option<String>,
    /// The right kerning group
    pub right_kern: Option<String>,
}

#[derive(Debug, PartialEq, Hash)]
pub struct Layer {
    pub layer_id: String,
    pub width: OrderedFloat<f64>,
    pub shapes: Vec<Shape>,
    pub anchors: Vec<Anchor>,
}

#[derive(Debug, PartialEq, Hash)]
pub enum Shape {
    Path(Path),
    Component(Component),
}

// The font you get directly from a plist, minimally modified
// Types chosen specifically to accomodate plist translation.
#[derive(Debug, FromPlist, PartialEq)]
#[allow(non_snake_case)]
struct RawFont {
    pub units_per_em: Option<i64>,
    pub metrics: Option<Vec<RawMetric>>,
    pub family_name: String,
    pub date: Option<String>,
    pub copyright: Option<String>,
    pub designer: Option<String>,
    pub designerURL: Option<String>,
    pub manufacturer: Option<String>,
    pub manufacturerURL: Option<String>,
    pub versionMajor: Option<i64>,
    pub versionMinor: Option<i64>,
    pub axes: Option<Vec<Axis>>,
    pub glyphs: Vec<RawGlyph>,
    pub font_master: Vec<RawFontMaster>,
    #[fromplist(default)]
    pub instances: Vec<RawInstance>,
    pub feature_prefixes: Option<Vec<RawFeature>>,
    pub features: Option<Vec<RawFeature>>,
    pub classes: Option<Vec<RawFeature>>,
    pub properties: Option<Vec<RawName>>,

    #[fromplist(rest)]
    pub other_stuff: BTreeMap<String, Plist>,
}

#[derive(Debug, Clone, FromPlist, PartialEq, Eq, Hash)]
pub struct RawMetric {
    // So named to let FromPlist populate it from a field called "type"
    type_: Option<String>,
}

#[derive(Clone, Debug, FromPlist, PartialEq, Eq, Hash)]
pub struct RawName {
    pub key: String,
    pub value: Option<String>,
    pub values: Option<Vec<RawNameValue>>,
}

#[derive(Clone, Debug, FromPlist, PartialEq, Eq, Hash)]
pub struct RawNameValue {
    pub language: String,
    pub value: String,
}

#[derive(Clone, Debug, FromPlist, PartialEq, Eq, Hash)]
pub struct RawFeature {
    pub automatic: Option<i64>,
    pub name: Option<String>,
    pub tag: Option<String>,
    pub code: String,

    #[fromplist(rest)]
    pub other_stuff: BTreeMap<String, Plist>,
}

#[derive(Clone, Debug, FromPlist, PartialEq, Eq, Hash)]
pub struct Axis {
    pub name: String,
    pub tag: String,
    pub hidden: Option<bool>,
}

#[derive(Clone, Debug, FromPlist, PartialEq)]
pub struct RawGlyph {
    pub layers: Vec<RawLayer>,
    pub glyphname: String,
    pub kern_left: Option<String>,
    pub kern_right: Option<String>,
    #[fromplist(rest)]
    pub other_stuff: BTreeMap<String, Plist>,
}

#[derive(Clone, Debug, FromPlist, PartialEq)]
pub struct RawLayer {
    pub layer_id: String,
    pub associated_master_id: Option<String>,
    pub width: OrderedFloat<f64>,
    shapes: Option<Vec<RawShape>>,
    paths: Option<Vec<Path>>,
    components: Option<Vec<Component>>,
    pub anchors: Option<Vec<RawAnchor>>,
    #[fromplist(rest)]
    pub other_stuff: BTreeMap<String, Plist>,
}

/// Represents a path OR a component
///
/// <https://github.com/schriftgestalt/GlyphsSDK/blob/Glyphs3/GlyphsFileFormat/GlyphsFileFormatv3.md#differences-between-version-2>
#[derive(Clone, Debug, FromPlist, PartialEq, Eq)]
pub struct RawShape {
    // TODO: add numerous unsupported attributes

    // When I'm a path
    pub closed: Option<bool>,
    pub nodes: Option<Vec<Node>>,

    // When I'm a component I specifically want all my attributes to end up in other_stuff
    // My Component'ness can be detected by presence of a ref (Glyphs3) or name(Glyphs2) attribute

    // Always
    #[fromplist(rest)]
    pub other_stuff: BTreeMap<String, Plist>,
}

#[derive(Clone, Debug, FromPlist, PartialEq, Eq, Hash)]
pub struct Path {
    pub closed: bool,
    pub nodes: Vec<Node>,
}

#[derive(Clone, Debug)]
pub struct Component {
    /// The glyph this component references
    pub glyph_name: String,
    pub transform: Affine,
    pub other_stuff: BTreeMap<String, Plist>,
}

impl PartialEq for Component {
    fn eq(&self, other: &Self) -> bool {
        self.glyph_name == other.glyph_name
            && Into::<AffineForEqAndHash>::into(self.transform) == other.transform.into()
            && self.other_stuff == other.other_stuff
    }
}

impl Eq for Component {}

impl Hash for Component {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.glyph_name.hash(state);
        Into::<AffineForEqAndHash>::into(self.transform).hash(state);
        self.other_stuff.hash(state);
    }
}

#[derive(Clone, Debug)]
pub struct Node {
    pub pt: Point,
    pub node_type: NodeType,
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

#[derive(Clone, Debug, FromPlist, PartialEq)]
pub struct RawAnchor {
    pub name: String,
    pub pos: Option<Point>,       // v3
    pub position: Option<String>, // v2
}

#[derive(Clone, Debug, FromPlist, PartialEq)]
pub struct Anchor {
    pub name: String,
    pub pos: Point,
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
    metric_values: BTreeMap<String, RawMetricValue>,
    pub typo_ascender: Option<i64>,
    pub typo_descender: Option<i64>,
    pub typo_line_gap: Option<i64>,
    pub win_ascent: Option<i64>,
    pub win_descent: Option<i64>,
    pub hhea_ascender: Option<i64>,
    pub hhea_descender: Option<i64>,
    pub hhea_line_gap: Option<i64>,
}

impl FontMaster {
    fn read_metric(&self, metric_name: &str) -> Option<OrderedFloat<f64>> {
        self.metric_values
            .get(metric_name)
            .and_then(|metric| metric.pos)
    }

    pub fn ascender(&self) -> Option<OrderedFloat<f64>> {
        self.read_metric("ascender")
    }

    pub fn descender(&self) -> Option<OrderedFloat<f64>> {
        self.read_metric("descender")
    }

    pub fn x_height(&self) -> Option<OrderedFloat<f64>> {
        self.read_metric("x-height")
    }

    pub fn cap_height(&self) -> Option<OrderedFloat<f64>> {
        self.read_metric("cap height")
    }
}

#[derive(Debug, Clone, FromPlist, PartialEq, Eq, Hash)]
struct RawFontMaster {
    pub id: String,
    pub name: Option<String>,

    pub typo_ascender: Option<i64>,
    pub typo_descender: Option<OrderedFloat<f64>>,
    pub typo_line_gap: Option<OrderedFloat<f64>>,
    pub win_ascender: Option<OrderedFloat<f64>>,
    pub win_descender: Option<OrderedFloat<f64>>,

    #[fromplist(default)]
    pub axes_values: Vec<OrderedFloat<f64>>,
    #[fromplist(default)]
    pub metric_values: Vec<RawMetricValue>,
    #[fromplist(rest)]
    pub other_stuff: BTreeMap<String, Plist>,
}

#[derive(Debug, Clone, FromPlist, PartialEq, Eq, Hash)]
pub struct RawMetricValue {
    pos: Option<OrderedFloat<f64>>,
    over: Option<OrderedFloat<f64>>,
}

impl RawMetricValue {
    fn is_empty(&self) -> bool {
        self.pos.is_none() && self.over.is_none()
    }
}

impl RawGlyph {
    pub fn get_layer(&self, layer_id: &str) -> Option<&RawLayer> {
        self.layers.iter().find(|l| l.layer_id == layer_id)
    }
}

#[derive(Clone, Debug, PartialEq, Hash)]
pub struct Instance {
    pub name: String,
    pub active: bool,
    // So named to let FromPlist populate it from a field called "type"
    pub type_: InstanceType,
    pub axis_mappings: BTreeMap<String, RawAxisUserToDesignMap>,
    pub axes_values: Vec<OrderedFloat<f64>>,
}

/// <https://github.com/googlefonts/glyphsLib/blob/6f243c1f732ea1092717918d0328f3b5303ffe56/Lib/glyphsLib/classes.py#L150>
#[derive(Clone, Debug, PartialEq, Hash)]
pub enum InstanceType {
    Single,
    Variable,
}

impl From<&str> for InstanceType {
    fn from(value: &str) -> Self {
        if value.to_ascii_lowercase() == "variable" {
            InstanceType::Variable
        } else {
            InstanceType::Single
        }
    }
}

#[derive(Debug, Clone, FromPlist, PartialEq, Eq, Hash)]
struct RawInstance {
    pub name: String,
    pub exports: Option<i64>,
    pub active: Option<i64>,
    pub type_: Option<String>,
    #[fromplist(default)]
    pub axes_values: Vec<OrderedFloat<f64>>,

    #[fromplist(rest)]
    pub other_stuff: BTreeMap<String, Plist>,
}

impl RawInstance {
    /// Per glyphsLib both "exports=0" and "active=0" mean inactive
    /// <https://github.com/googlefonts/glyphsLib/blob/6f243c1f732ea1092717918d0328f3b5303ffe56/Lib/glyphsLib/builder/axes.py#L637>
    fn is_active(&self) -> bool {
        self.exports.unwrap_or(1) != 0 && self.active.unwrap_or(1) != 0
    }
}

impl FromPlist for Node {
    fn from_plist(plist: Plist) -> Self {
        match &plist {
            Plist::String(value) => {
                let mut spl = value.splitn(3, ' ');
                let x = spl.next().unwrap().parse().unwrap();
                let y = spl.next().unwrap().parse().unwrap();
                let pt = Point::new(x, y);
                let node_type = spl.next().unwrap().parse().unwrap();
                Node { pt, node_type }
            }
            Plist::Array(value) => {
                if value.len() != 3 {
                    panic!("Invalid node content {plist:?}");
                };
                let x = value[0].as_f64().unwrap();
                let y = value[1].as_f64().unwrap();
                let pt = Point::new(x, y);
                let node_type = value[2].as_str().unwrap().parse().unwrap();
                Node { pt, node_type }
            }
            _ => {
                panic!("Invalid node content {plist:?}");
            }
        }
    }
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

fn try_f64(plist: &Plist) -> Result<f64, Error> {
    plist
        .as_f64()
        .ok_or_else(|| Error::StructuralError(format!("Bad f64:{plist:?}")))
}

impl TryFrom<BTreeMap<String, Plist>> for Component {
    type Error = Error;

    fn try_from(mut dict: BTreeMap<String, Plist>) -> Result<Self, Self::Error> {
        // Glyphs v3 name has been renamed to ref; look for both
        let glyph_name = if let Some(Plist::String(glyph_name)) = dict.remove("ref") {
            glyph_name
        } else if let Some(Plist::String(glyph_name)) = dict.remove("name") {
            glyph_name
        } else {
            return Err(Error::StructuralError(format!(
                "Neither ref nor name present: {dict:?}"
            )));
        };

        // V3 vs v2: The transform entry has been replaced by angle, pos and scale entries.
        let mut transform = if let Some(plist) = dict.remove("transform") {
            Affine::from_plist(plist)
        } else {
            Affine::IDENTITY
        };

        // Glyphs 3 gives us {angle, pos, scale}. Glyphs 2 gives us the standard 2x3 matrix.
        // The matrix is more general and less ambiguous (what order do you apply the angle, pos, scale?)
        // so convert Glyphs 3 to that. Order based on saving the same transformed comonent as
        // Glyphs 2 and Glyphs 3 then trying to convert one to the other.

        // Translate
        if let Some(Plist::Array(pos)) = dict.remove("pos") {
            if pos.len() != 2 {
                return Err(Error::StructuralError(format!("Bad pos: {pos:?}")));
            }
            transform *= Affine::translate((try_f64(&pos[0])?, try_f64(&pos[1])?));
        }

        // Rotate
        if let Some(angle) = dict.remove("angle") {
            transform *= Affine::rotate(try_f64(&angle)?.to_radians());
        }

        // Scale
        if let Some(Plist::Array(scale)) = dict.remove("scale") {
            if scale.len() != 2 {
                return Err(Error::StructuralError(format!("Bad scale: {scale:?}")));
            }
            transform *= Affine::scale_non_uniform(try_f64(&scale[0])?, try_f64(&scale[1])?);
        }

        Ok(Component {
            glyph_name,
            transform,
            other_stuff: dict,
        })
    }
}

impl FromPlist for Component {
    fn from_plist(plist: Plist) -> Self {
        let Plist::Dictionary(dict) = plist else {
            panic!("Component must be a dict: {plist:?}");
        };
        dict.try_into().expect("Unable to parse Component")
    }
}

impl FromPlist for Affine {
    fn from_plist(plist: Plist) -> Self {
        let raw = plist.as_str().unwrap();
        let raw = &raw[1..raw.len() - 1];
        let coords: Vec<f64> = raw.split(", ").map(|c| c.parse().unwrap()).collect();
        Affine::new([
            coords[0], coords[1], coords[2], coords[3], coords[4], coords[5],
        ])
    }
}

impl FromPlist for Point {
    fn from_plist(plist: Plist) -> Self {
        match plist {
            Plist::Array(values) if values.len() == 2 => {
                Point::new(values[0].as_f64().unwrap(), values[1].as_f64().unwrap())
            }
            Plist::String(value) => {
                let raw = &value[1..value.len() - 1];
                let coords: Vec<f64> = raw.split(", ").map(|c| c.parse().unwrap()).collect();
                Point::new(coords[0], coords[1])
            }
            _ => panic!("Cannot parse point from {plist:?}"),
        }
    }
}

impl FromPlist for OrderedFloat<f64> {
    fn from_plist(plist: Plist) -> Self {
        plist.as_f64().unwrap().into()
    }
}

impl Path {
    pub fn new(closed: bool) -> Path {
        Path {
            nodes: Vec::new(),
            closed,
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

fn custom_params_mut(other_stuff: &mut BTreeMap<String, Plist>) -> Option<&mut Vec<Plist>> {
    let custom_params = other_stuff.get_mut("customParameters");
    custom_params.as_ref()?;
    let Some(Plist::Array(custom_params)) = custom_params else {
        warn!("customParameters isn't an array\n{:#?}", custom_params);
        return None;
    };
    Some(custom_params)
}

fn custom_param_mut<'a>(
    other_stuff: &'a mut BTreeMap<String, Plist>,
    key: &str,
) -> Option<(usize, &'a mut Plist)> {
    let Some(custom_params) = custom_params_mut(other_stuff) else {
        return None;
    };

    let name_key = "name".to_string();
    for (idx, custom_param) in custom_params.iter_mut().enumerate() {
        let Plist::Dictionary(dict) = custom_param else {
            warn!("custom param isn't a dictionary\n{:#?}", custom_param);
            continue;
        };
        let Some(Plist::String(param_key)) = dict.get(&name_key) else {
            warn!("custom param has a non-string name\n{:#?}", custom_param);
            continue;
        };
        if key == param_key {
            return Some((idx, custom_param));
        }
    }
    None
}

fn glyphs_v2_field_name_and_default(
    nth_axis: usize,
) -> Result<(&'static [&'static str], f64), Error> {
    // Per https://github.com/googlefonts/fontmake-rs/pull/42#pullrequestreview-1211619812
    // the field to use is based on the order in axes NOT the tag.
    // That is, whatever the first axis is, it's value is in the weightValue field. Long sigh.
    // Defaults per https://github.com/googlefonts/fontmake-rs/pull/42#discussion_r1044415236.
    // v2 instances use novel field names so send back several for linear probing.
    Ok(match nth_axis {
        0 => (&["weightValue", "interpolationWeight"], 100.0),
        1 => (&["widthValue", "interpolationWidth"], 100.0),
        2 => (&["customValue"], 0.0),
        _ => {
            return Err(Error::StructuralError(format!(
                "We don't know what field to use for axis {nth_axis}"
            )))
        }
    })
}

fn custom_params(other_stuff: &BTreeMap<String, Plist>) -> Option<&Vec<Plist>> {
    let custom_params = other_stuff.get("customParameters");
    custom_params.as_ref()?;
    let Some(Plist::Array(custom_params)) = custom_params else {
        warn!("customParameters isn't an array\n{:#?}", custom_params);
        return None;
    };
    Some(custom_params)
}

fn custom_param<'a>(
    other_stuff: &'a BTreeMap<String, Plist>,
    key: &str,
) -> Option<(usize, &'a Plist)> {
    let Some(custom_params) = custom_params(other_stuff) else {
        return None;
    };

    let name_key = "name".to_string();
    for (idx, custom_param) in custom_params.iter().enumerate() {
        let Plist::Dictionary(dict) = custom_param else {
            warn!("custom param isn't a dictionary\n{:#?}", custom_param);
            continue;
        };
        let Some(Plist::String(param_key)) = dict.get(&name_key) else {
            warn!("custom param has a non-string name\n{:#?}", custom_param);
            continue;
        };
        if key == param_key {
            return Some((idx, custom_param));
        }
    }
    None
}

fn custom_param_int(other_stuff: &BTreeMap<String, Plist>, key: &str) -> Option<i64> {
    let Some((_, Plist::Dictionary(dict))) = custom_param(other_stuff, key) else {
        return None;
    };
    let Some(Plist::Integer(value)) = dict.get("value") else {
        return None;
    };
    Some(*value)
}

fn v2_to_v3_name(properties: &mut Vec<RawName>, v2_prop: &Option<String>, v3_name: &str) {
    // https://github.com/schriftgestalt/GlyphsSDK/blob/Glyphs3/GlyphsFileFormat/GlyphsFileFormatv3.md#properties
    // Keys ending with "s" are localizable that means the second key is values
    if let Some(value) = v2_prop {
        properties.push(if v3_name.ends_with('s') {
            RawName {
                key: v3_name.into(),
                value: None,
                values: Some(vec![RawNameValue {
                    language: "dflt".into(),
                    value: value.clone(),
                }]),
            }
        } else {
            RawName {
                key: v3_name.into(),
                value: Some(value.clone()),
                values: None,
            }
        });
    }
}

/// Convert to axesValues, handy for a master or instance
fn v2_to_v3_axis_values(
    axes: &Vec<Axis>,
    other_stuff: &mut BTreeMap<String, Plist>,
) -> Result<Vec<OrderedFloat<f64>>, Error> {
    let mut axis_values = Vec::new();
    for idx in 0..axes.len() {
        // Per https://github.com/googlefonts/fontmake-rs/pull/42#pullrequestreview-1211619812
        // the field to use is based on the order in axes NOT the tag.
        // That is, whatever the first axis is, it's value is in the weightValue field. Long sigh.
        let (field_names, default_value) = glyphs_v2_field_name_and_default(idx)?;
        let value = field_names
            .iter()
            .find_map(|field_name| other_stuff.remove(*field_name).and_then(|v| v.as_f64()))
            .unwrap_or_else(|| {
                warn!("Invalid '{:?}' in\n{:#?}", field_names, other_stuff);
                default_value
            });
        axis_values.push(value.into());
    }
    Ok(axis_values)
}

impl RawFont {
    fn is_v2(&self) -> bool {
        let mut is_v2 = true;
        if let Some(Plist::Integer(version)) = self.other_stuff.get(".formatVersion") {
            is_v2 = *version < 3; // .formatVersion is only present for v3+
        }
        is_v2
    }

    fn v2_to_v3_axes(&mut self) -> Result<Vec<String>, Error> {
        let mut tags = Vec::new();
        if self.axes.is_none() {
            self.axes = Some(Vec::new());
        }
        if let Some((axes_idx, custom_param)) = custom_param_mut(&mut self.other_stuff, "Axes") {
            if let Plist::Dictionary(dict) = custom_param {
                let v3_axes = self.axes.as_mut().unwrap();
                let Some(Plist::Array(v2_axes)) = dict.get_mut("value") else {
                    return Err(Error::StructuralError(
                        "No value for Axes custom parameter".into(),
                    ));
                };
                for v2_axis in v2_axes {
                    let Plist::Dictionary(v2_axis) = v2_axis else {
                        return Err(Error::StructuralError("Axis value must be a dict".into()));
                    };
                    let tag = v2_axis.get("Tag").unwrap().as_str().unwrap();
                    tags.push(tag.to_string());
                    v3_axes.push(Axis {
                        name: v2_axis.get("Name").unwrap().as_str().unwrap().into(),
                        tag: tag.into(),
                        hidden: v2_axis.get("hidden").map(|v| v.as_i64() == Some(1)),
                    });
                }
            }
            custom_params_mut(&mut self.other_stuff).map(|p| p.remove(axes_idx));
        }

        // Match the defaults from https://github.com/googlefonts/glyphsLib/blob/f6e9c4a29ce764d34c309caef5118c48c156be36/Lib/glyphsLib/builder/axes.py#L526
        // if we have nothing
        let axes = self.axes.as_mut().unwrap();
        if axes.is_empty() {
            axes.push(Axis {
                name: "Weight".into(),
                tag: "wght".into(),
                hidden: None,
            });
            axes.push(Axis {
                name: "Width".into(),
                tag: "wdth".into(),
                hidden: None,
            });
            axes.push(Axis {
                name: "Custom".into(),
                tag: "XXXX".into(),
                hidden: None,
            });
        }

        if axes.len() > 3 {
            return Err(Error::StructuralError(
                "We only understand 0..3 axes for Glyphs v2".into(),
            ));
        }

        // v2 stores values for axes in specific fields, find them and put them into place
        // "Axis position related properties (e.g. weightValue, widthValue, customValue) have been replaced by the axesValues list which is indexed in parallel with the toplevel axes list."
        for master in self.font_master.iter_mut() {
            master.axes_values = v2_to_v3_axis_values(axes, &mut master.other_stuff)?;
        }
        for instance in self.instances.iter_mut() {
            instance.axes_values = v2_to_v3_axis_values(axes, &mut instance.other_stuff)?;
        }

        if custom_params_mut(&mut self.other_stuff).map_or(false, |d| d.is_empty()) {
            self.other_stuff.remove("customParameters");
        }
        Ok(tags)
    }

    fn v2_to_v3_metrics(&mut self) -> Result<(), Error> {
        // metrics are in parallel arrays in v3
        assert!(V2_METRIC_NAMES.len() == V3_METRIC_NAMES.len());

        // setup root storage for the basic metrics
        self.metrics = Some(
            V3_METRIC_NAMES
                .iter()
                .map(|n| RawMetric {
                    type_: Some(n.to_string()),
                })
                .collect(),
        );

        // in each font master setup the parallel array
        for master in self.font_master.iter_mut() {
            // Copy the v2 metrics from actual fields into the parallel array rig
            let mut metric_values = Vec::new();
            for v2_name in V2_METRIC_NAMES.iter() {
                let mut pos = None;
                match master.other_stuff.remove(&v2_name.to_string()) {
                    Some(Plist::Integer(value)) if value != 0 => {
                        pos = Some(OrderedFloat(value as f64))
                    }
                    Some(Plist::Float(value)) if value != OrderedFloat(0.0) => pos = Some(value),
                    _ => (),
                };
                metric_values.push(RawMetricValue { pos, over: None });
            }
            // "alignmentZones is now a set of over (overshoot) properties attached to metrics"
            // TODO: are these actually aligned by index or do you just lookup lhs to get over from rhs
            if let Some(Plist::Array(zones)) = master.other_stuff.get("alignmentZones") {
                assert!(
                    zones.len() <= metric_values.len(),
                    "{} should be <= {}",
                    zones.len(),
                    metric_values.len()
                );
                for (idx, zone) in zones.iter().enumerate() {
                    let Plist::String(zone) = zone else {
                        warn!("Non-string alignment zone, skipping");
                        continue;
                    };
                    // Alignment zones look like {800, 16}, but (800, 16) would be more useful
                    let zone = zone.replace('{', "(").replace('}', ")");
                    let Ok(Plist::Array(values)) = Plist::parse(&zone) else {
                        warn!("Confusing alignment zone, skipping");
                        continue;
                    };
                    if values.len() != 2 {
                        warn!("Confusing alignment zone, skipping");
                        continue;
                    };
                    // values are pos, over. pos comes across from metrics so just copy non-zero over's.
                    let Plist::Integer(over) = values[1] else {
                        warn!("Confusing alignment zone, skipping");
                        continue;
                    };
                    if over != 0 {
                        metric_values[idx].over = Some(OrderedFloat(over as f64));
                    }
                }
            }

            if log_enabled!(log::Level::Trace) {
                for (n, m) in V2_METRIC_NAMES.iter().zip(metric_values.iter()) {
                    trace!("{} {n} = {m:?}", master.id);
                }
            }

            master.metric_values = metric_values;
        }
        Ok(())
    }

    fn v2_to_v3_weight(&mut self) -> Result<(), Error> {
        for master in self.font_master.iter_mut() {
            // Don't remove weightValue, we need it to understand axes
            let Some(Plist::Integer(..)) = master.other_stuff.get("weightValue") else {
                continue;
            };
            let name = match master.other_stuff.remove("weight") {
                Some(Plist::String(name)) => name,
                _ => String::from("Regular"), // Missing = default = Regular per @anthrotype
            };
            master.name = Some(name);
        }
        Ok(())
    }

    fn v2_to_v3_names(&mut self) -> Result<(), Error> {
        // The copyright, designer, designerURL, manufacturer, manufacturerURL top-level entries
        // have been moved into new top-level properties dictionary and made localizable.
        let properties = self.properties.get_or_insert_with(Default::default);

        v2_to_v3_name(properties, &self.copyright, "copyrights");
        v2_to_v3_name(properties, &self.designer, "designers");
        v2_to_v3_name(properties, &self.designerURL, "designerURL");
        v2_to_v3_name(properties, &self.manufacturer, "manufacturers");
        v2_to_v3_name(properties, &self.manufacturerURL, "manufacturerURL");

        let mut v2_to_v3_param = |v2_name: &str, v3_name: &str| {
            if let Some((_, Plist::Dictionary(param))) = custom_param(&self.other_stuff, v2_name) {
                if let Some(Plist::String(value)) = param.get("value") {
                    v2_to_v3_name(properties, &Some(value.clone()), v3_name);
                }
            }
        };
        v2_to_v3_param("description", "descriptions");
        v2_to_v3_param("licenseURL", "licenseURL");
        v2_to_v3_param("versionString", "versionString");
        v2_to_v3_param("compatibleFullName", "compatibleFullNames");
        v2_to_v3_param("license", "licenses");
        v2_to_v3_param("uniqueID", "uniqueID");
        v2_to_v3_param("trademark", "trademarks");
        v2_to_v3_param("sampleText", "sampleTexts");
        v2_to_v3_param("postscriptFullName", "postscriptFullName");
        v2_to_v3_param("postscriptFontName", "postscriptFontName");
        v2_to_v3_param("WWSFamilyName", "WWSFamilyName");
        v2_to_v3_param("vendorID", "vendorID");

        Ok(())
    }

    fn v2_to_v3_instances(&mut self) -> Result<(), Error> {
        for instance in self.instances.iter_mut() {
            // named clases become #s in v3
            for (tag, name) in &[("wght", "weightClass"), ("wdth", "widthClass")] {
                let Some(Plist::String(value)) = instance.other_stuff.get(*name) else {
                    continue;
                };
                let Some(value) = lookup_class_value(tag, value) else {
                    return Err(Error::UnknownValueName(value.clone()));
                };
                instance
                    .other_stuff
                    .insert(name.to_string(), Plist::Integer(value as i64));
            }
        }

        Ok(())
    }

    fn v2_to_v3_kerning(&mut self) -> Result<(), Error> {
        if let Some(kerning) = self.other_stuff.remove("kerning") {
            self.other_stuff.insert("kerningLTR".to_string(), kerning);
        };
        for glyph in self.glyphs.iter_mut() {
            if let Some(Plist::String(group)) = glyph.other_stuff.remove("leftKerningGroup") {
                glyph.kern_left = Some(group);
            }
            if let Some(Plist::String(group)) = glyph.other_stuff.remove("rightKerningGroup") {
                glyph.kern_right = Some(group);
            }
        }
        Ok(())
    }

    /// `<See https://github.com/schriftgestalt/GlyphsSDK/blob/Glyphs3/GlyphsFileFormat/GlyphsFileFormatv3.md#differences-between-version-2>`
    fn v2_to_v3(&mut self) -> Result<(), Error> {
        self.v2_to_v3_weight()?;
        self.v2_to_v3_axes()?;
        self.v2_to_v3_metrics()?;
        self.v2_to_v3_names()?;
        self.v2_to_v3_instances()?;
        self.v2_to_v3_kerning()?;
        Ok(())
    }
}

fn parse_glyph_order(raw_font: &RawFont) -> Vec<String> {
    let mut valid_names: HashSet<_> = raw_font.glyphs.iter().map(|g| &g.glyphname).collect();
    let mut glyph_order = Vec::new();

    // Add all valid glyphOrder entries in order
    // See https://github.com/googlefonts/fontmake-rs/pull/43/files#r1044627972
    if let Some((_, Plist::Dictionary(dict))) = custom_param(&raw_font.other_stuff, "glyphOrder") {
        if let Some(Plist::Array(entries)) = dict.get("value") {
            entries
                .iter()
                .filter_map(|e| e.as_str())
                .map(|s| s.to_string())
                .for_each(|s| {
                    if valid_names.remove(&s) {
                        glyph_order.push(s);
                    }
                });
        };
    };

    // Add anything left over in file order
    raw_font
        .glyphs
        .iter()
        .filter(|g| valid_names.contains(&g.glyphname))
        .for_each(|g| glyph_order.push(g.glyphname.clone()));

    glyph_order
}

/// Returns a map from glyph name to codepoint(s).
fn parse_codepoints(raw_font: &mut RawFont, radix: u32) -> BTreeMap<String, BTreeSet<u32>> {
    let mut name_to_cp: BTreeMap<String, BTreeSet<u32>> = BTreeMap::new();
    for glyph in raw_font.glyphs.iter_mut() {
        if let Some(Plist::String(val)) = glyph.other_stuff.remove("unicode") {
            val.split(',')
                .map(|v| i64::from_str_radix(v, radix).unwrap() as u32)
                .for_each(|cp| {
                    name_to_cp
                        .entry(glyph.glyphname.clone())
                        .or_default()
                        .insert(cp);
                });
        };
    }
    name_to_cp
}

fn parse_kerning(kerning: Option<&Plist>) -> BTreeMap<String, BTreeMap<(String, String), i32>> {
    let mut result = BTreeMap::new();
    let Some(Plist::Dictionary(kerning)) = kerning else {
        return result;
    };
    for (master_id, kerning) in kerning {
        let mut master_kerns = BTreeMap::new();
        if let Plist::Dictionary(kerning) = kerning {
            for (kern_from, kern_tos) in kerning {
                let Plist::Dictionary(kern_tos) = kern_tos else {
                    continue;
                };
                for (kern_to, adjustment) in kern_tos {
                    let Some(adjustment) = adjustment.as_i64() else {
                        continue;
                    };
                    master_kerns.insert((kern_from.clone(), kern_to.clone()), adjustment as i32);
                }
            }
        }
        result.insert(master_id.clone(), master_kerns);
    }
    result
}

/// <https://github.com/googlefonts/glyphsLib/blob/6f243c1f732ea1092717918d0328f3b5303ffe56/Lib/glyphsLib/builder/axes.py#L578>
fn default_master_idx(raw_font: &RawFont) -> usize {
    // Prefer an explicit origin
    // https://github.com/googlefonts/fontmake-rs/issues/44
    custom_param(&raw_font.other_stuff, "Variable Font Origin")
        .map(|(_, param)| {
            let Plist::Dictionary(dict) = param else {
                return 0;
            };
            let Some(Plist::String(origin)) = dict.get("value") else {
                warn!("Incomprehensible Variable Font Origin");
                return 0;
            };
            raw_font
                .font_master
                .iter()
                .enumerate()
                .find(|(_, master)| &master.id == origin)
                .map(|(idx, _)| idx)
                .unwrap_or(0)
        })
        // TODO: implement searching for "a base style shared between all masters" as glyphsLib does
        // Still nothing? - just look for one called Regular
        .or_else(|| {
            raw_font
                .font_master
                .iter()
                .position(|m| matches!(m.name.as_deref(), Some("Regular")))
        })
        .unwrap_or_default()
}

fn axis_index(from: &RawFont, pred: impl Fn(&Axis) -> bool) -> Option<usize> {
    from.axes
        .as_ref()
        .map(|axes| {
            axes.iter()
                .enumerate()
                .find_map(|(i, a)| if pred(a) { Some(i) } else { None })
        })
        .unwrap_or_default()
}

fn user_to_design_from_axis_mapping(
    from: &RawFont,
) -> Option<BTreeMap<String, RawAxisUserToDesignMap>> {
    // Fetch mapping from Axis Mappings, if any
    let Some((_, axis_map)) = custom_param(&from.other_stuff, "Axis Mappings") else {
        return None;
    };
    let Plist::Dictionary(axis_map) = axis_map.get("value").unwrap() else {
        panic!("Incomprehensible axis map {axis_map:?}");
    };
    let mut axis_mappings: BTreeMap<String, RawAxisUserToDesignMap> = BTreeMap::new();
    for (axis_tag, mappings) in axis_map.iter() {
        let Plist::Dictionary(mappings) = mappings else {
            panic!("Incomprehensible mappings {mappings:?}");
        };
        let Some(axis_index) = axis_index(from, |a| &a.tag == axis_tag) else {
            panic!("No such axes: {axis_tag:?}");
        };
        // We could not have found an axis index if there are no axes
        let axis_name = &from.axes.as_ref().unwrap().get(axis_index).unwrap().name;
        for (user, design) in mappings.iter() {
            let user: f32 = user.parse().unwrap();
            let design = design.as_f64().unwrap() as f32;
            axis_mappings
                .entry(axis_name.clone())
                .or_default()
                .add_if_new(user.into(), design.into());
        }
    }
    Some(axis_mappings)
}

fn user_to_design_from_axis_location(
    from: &RawFont,
) -> Option<BTreeMap<String, RawAxisUserToDesignMap>> {
    // glyphsLib only trusts Axis Location when all masters have it, match that
    // https://github.com/googlefonts/fontmake-rs/pull/83#discussion_r1065814670
    let master_locations: Vec<&Plist> = from
        .font_master
        .iter()
        .filter_map(|m| custom_param(&m.other_stuff, "Axis Location").map(|(_, al)| al))
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

    let mut axis_mappings: BTreeMap<String, RawAxisUserToDesignMap> = BTreeMap::new();
    for (master, axis_locations) in from.font_master.iter().zip(&master_locations) {
        let Plist::Dictionary(axis_locations) = axis_locations else {
            panic!("Axis Location must be a dict {axis_locations:?}");
        };
        let Some(Plist::Array(axis_locations)) = axis_locations.get("value") else {
            panic!("Value must be a dict {axis_locations:?}");
        };
        for axis_location in axis_locations {
            let Some(Plist::String(axis_name)) = axis_location.get("Axis") else {
                panic!("Axis name must be a string {axis_location:?}");
            };
            let Some(user) = axis_location.get("Location") else {
                panic!("Incomprehensible axis location {axis_location:?}");
            };
            let Some(user) = user.as_f64() else {
                panic!("Incomprehensible axis location {axis_location:?}");
            };
            let Some(axis_index) = axis_index(from, |a| &a.name == axis_name) else {
                panic!("Axis has no index {axis_location:?}");
            };
            let user = user as f32;
            let design = master.axes_values[axis_index].into_inner() as f32;

            axis_mappings
                .entry(axis_name.clone())
                .or_default()
                .add_if_new(user.into(), design.into());
        }
    }
    Some(axis_mappings)
}

impl RawAxisUserToDesignMap {
    fn add_any_new(&mut self, incoming: &RawAxisUserToDesignMap) {
        for (user, design) in incoming.0.iter() {
            self.add_if_new(*user, *design);
        }
    }

    fn add_if_new(&mut self, user: OrderedFloat<f32>, design: OrderedFloat<f32>) {
        if self.0.iter().any(|(u, d)| *u == user || *d == design) {
            return;
        }
        self.0.push((user, design));
    }

    pub fn iter(&self) -> impl Iterator<Item = &(OrderedFloat<f32>, OrderedFloat<f32>)> {
        self.0.iter()
    }
}

impl RawUserToDesignMapping {
    /// From most to least preferred: Axis Mappings, Axis Location, mappings from instances, assume user == design
    ///
    /// <https://github.com/googlefonts/glyphsLib/blob/6f243c1f732ea1092717918d0328f3b5303ffe56/Lib/glyphsLib/builder/axes.py#L155>
    fn new(from: &RawFont, instances: &[Instance]) -> Self {
        let from_axis_mapping = user_to_design_from_axis_mapping(from);
        let from_axis_location = user_to_design_from_axis_location(from);
        let (result, add_instance_mappings) = match (from_axis_mapping, from_axis_location) {
            (Some(from_mapping), Some(..)) => {
                warn!("Axis Mapping *and* Axis Location are defined; using Axis Mapping");
                (from_mapping, false)
            }
            (Some(from_mapping), None) => (from_mapping, false),
            (None, Some(from_location)) => (from_location, true),
            (None, None) => (BTreeMap::new(), true),
        };
        let mut result = Self(result);
        if add_instance_mappings {
            result.add_instance_mappings_if_new(instances);
        }
        result.add_master_mappings_if_new(from);
        result
    }

    pub fn contains(&self, axis_name: &str) -> bool {
        self.0.contains_key(axis_name)
    }

    pub fn get(&self, axis_name: &str) -> Option<&RawAxisUserToDesignMap> {
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
            let Some(axes) = from.axes.as_ref() else {
                continue;
            };
            for (axis, value) in axes.iter().zip(&master.axes_values) {
                let value = OrderedFloat(value.0 as f32);
                self.0
                    .entry(axis.name.clone())
                    .or_default()
                    .add_if_new(value, value);
            }
        }
    }
}

impl TryFrom<RawShape> for Shape {
    type Error = Error;

    fn try_from(from: RawShape) -> Result<Self, Self::Error> {
        // TODO: handle numerous unsupported attributes
        // See <https://github.com/schriftgestalt/GlyphsSDK/blob/Glyphs3/GlyphsFileFormat/GlyphsFileFormatv3.md#differences-between-version-2>

        let shape = if let Some(Plist::String(..)) = from.other_stuff.get("ref") {
            // only components use ref in Glyphs 3
            Shape::Component(from.other_stuff.try_into()?)
        } else {
            // no ref; presume it's a path
            Shape::Path(Path {
                closed: from.closed.unwrap_or_default(),
                nodes: from.nodes.clone().unwrap_or_default(),
            })
        };
        Ok(shape)
    }
}

fn map_and_push_if_present<T, U>(dest: &mut Vec<T>, src: Option<Vec<U>>, map: fn(U) -> T) {
    let Some(src) = src else {
        return; // nop
    };
    src.into_iter().map(map).for_each(|v| dest.push(v));
}

impl TryFrom<RawLayer> for Layer {
    type Error = Error;

    fn try_from(from: RawLayer) -> Result<Self, Self::Error> {
        let mut shapes = Vec::new();

        // Glyphs v2 uses paths and components
        map_and_push_if_present(&mut shapes, from.paths, Shape::Path);
        map_and_push_if_present(&mut shapes, from.components, Shape::Component);

        // Glyphs v3 uses shapes for both
        for raw_shape in from.shapes.unwrap_or_default() {
            shapes.push(raw_shape.try_into()?);
        }

        let anchors = from
            .anchors
            .unwrap_or_default()
            .into_iter()
            .map(|ra| {
                let pos = if let Some(pos) = ra.pos {
                    pos
                } else if let Some(pos) = ra.position {
                    Point::from_plist(Plist::String(pos))
                } else {
                    Point::ZERO
                };
                Anchor { name: ra.name, pos }
            })
            .collect();

        Ok(Layer {
            layer_id: from.layer_id,
            width: from.width,
            shapes,
            anchors,
        })
    }
}

impl TryFrom<RawGlyph> for Glyph {
    type Error = Error;

    fn try_from(from: RawGlyph) -> Result<Self, Self::Error> {
        let mut instances = Vec::new();
        for layer in from.layers {
            // The presence of an associated master indicates this is not a simple instance
            // It's either a draft or a more complex usage, such as an alternate
            if layer.associated_master_id.is_some() {
                continue;
            }
            instances.push(layer.try_into()?);
        }
        Ok(Glyph {
            glyphname: from.glyphname,
            layers: instances,
            left_kern: from.kern_left,
            right_kern: from.kern_right,
        })
    }
}

impl RawFeature {
    // https://github.com/googlefonts/glyphsLib/blob/24b4d340e4c82948ba121dcfe563c1450a8e69c9/Lib/glyphsLib/builder/features.py#L43
    fn autostr(&self) -> String {
        match self.automatic {
            Some(1) => "# automatic\n".to_string(),
            _ => "".to_string(),
        }
    }

    fn name(&self) -> Result<String, Error> {
        match (&self.name, &self.tag) {
            (Some(name), _) => Ok(name.clone()),
            (None, Some(tag)) => Ok(tag.clone()),
            (None, None) => Err(Error::StructuralError(format!(
                "{self:?} missing name and tag"
            ))),
        }
    }
}

// https://github.com/googlefonts/glyphsLib/blob/24b4d340e4c82948ba121dcfe563c1450a8e69c9/Lib/glyphsLib/builder/features.py#L90
fn prefix_to_feature(prefix: RawFeature) -> Result<FeatureSnippet, Error> {
    let name = match &prefix.name {
        Some(name) => name.as_str(),
        None => "",
    };
    let code = format!("# Prefix: {}\n{}{}", name, prefix.autostr(), prefix.code);
    Ok(FeatureSnippet(code))
}

// https://github.com/googlefonts/glyphsLib/blob/24b4d340e4c82948ba121dcfe563c1450a8e69c9/Lib/glyphsLib/builder/features.py#L101
fn class_to_feature(feature: RawFeature) -> Result<FeatureSnippet, Error> {
    let name = feature.name()?;
    let code = format!(
        "{}{}{} = [ {}\n];",
        feature.autostr(),
        if name.starts_with('@') { "" } else { "@" },
        name,
        feature.code
    );
    Ok(FeatureSnippet(code))
}

// https://github.com/googlefonts/glyphsLib/blob/24b4d340e4c82948ba121dcfe563c1450a8e69c9/Lib/glyphsLib/builder/features.py#L113
fn raw_feature_to_feature(feature: RawFeature) -> Result<FeatureSnippet, Error> {
    let name = feature.name()?;
    let code = format!(
        "feature {0} {{\n{1}{2}\n}} {0};",
        name,
        feature.autostr(),
        feature.code
    );
    Ok(FeatureSnippet(code))
}

/// <https://github.com/googlefonts/glyphsLib/blob/6f243c1f732ea1092717918d0328f3b5303ffe56/Lib/glyphsLib/classes.py#L220-L249>
fn lookup_class_value(axis_tag: &str, user_class: &str) -> Option<f32> {
    let user_class = match user_class {
        value if !value.is_empty() => {
            let mut value = value.to_ascii_lowercase();
            value.retain(|c| c != ' ');
            value
        }
        _ => String::from(""),
    };
    match (axis_tag, user_class.as_str()) {
        ("wght", "thin") => Some(100.0),
        ("wght", "extralight") => Some(200.0),
        ("wght", "ultralight") => Some(200.0),
        ("wght", "light") => Some(300.0),
        ("wght", "") => Some(400.0),
        ("wght", "normal") => Some(400.0),
        ("wght", "regular") => Some(400.0),
        ("wght", "medium") => Some(500.0),
        ("wght", "demibold") => Some(600.0),
        ("wght", "semibold") => Some(600.0),
        ("wght", "bold") => Some(700.0),
        ("wght", "ultrabold") => Some(800.0),
        ("wght", "extrabold") => Some(800.0),
        ("wght", "black") => Some(900.0),
        ("wght", "heavy") => Some(900.0),
        ("wdth", "ultracondensed") => Some(50.0),
        ("wdth", "extracondensed") => Some(62.5),
        ("wdth", "condensed") => Some(75.0),
        ("wdth", "semicondensed") => Some(87.5),
        ("wdth", "") => Some(100.0),
        ("wdth", "Medium (normal)") => Some(100.0),
        ("wdth", "semiexpanded") => Some(112.5),
        ("wdth", "expanded") => Some(125.0),
        ("wdth", "extraexpanded") => Some(150.0),
        ("wdth", "ultraexpanded") => Some(200.0),
        _ => {
            warn!("Unrecognized ('{axis_tag}', '{user_class}')");
            None
        }
    }
}

fn add_mapping_if_present(
    axis_mappings: &mut BTreeMap<String, RawAxisUserToDesignMap>,
    axes: &[Axis],
    axis_tag: &str,
    axes_values: &[OrderedFloat<f64>],
    value: Option<&Plist>,
) {
    let Some(idx) = axes.iter().position(|a| a.tag == axis_tag) else {
        return;
    };
    let axis = &axes[idx];
    let Some(design) = axes_values.get(idx) else {
        return;
    };
    let Some(value) = value.and_then(|v| v.as_f64()) else {
        return;
    };
    let user = OrderedFloat(value as f32);

    axis_mappings
        .entry(axis.name.clone())
        .or_default()
        .add_if_new(user, OrderedFloat(design.into_inner() as f32));
}

impl Instance {
    /// Glyphs 2 instances have fun fields.
    ///
    /// Mappings based on
    /// <https://github.com/googlefonts/glyphsLib/blob/6f243c1f732ea1092717918d0328f3b5303ffe56/Lib/glyphsLib/classes.py#L3451>
    fn new(axes: &[Axis], value: &RawInstance) -> Self {
        let active = value.is_active();
        let mut axis_mappings = BTreeMap::new();

        // TODO loop over same consts as other usage
        add_mapping_if_present(
            &mut axis_mappings,
            axes,
            "wght",
            &value.axes_values,
            value.other_stuff.get("weightClass"),
        );
        add_mapping_if_present(
            &mut axis_mappings,
            axes,
            "wdth",
            value.axes_values.as_ref(),
            value.other_stuff.get("widthClass"),
        );

        Instance {
            name: value.name.clone(),
            active,
            type_: value
                .type_
                .as_ref()
                .map(|v| v.as_str().into())
                .unwrap_or(InstanceType::Single),
            axis_mappings,
            axes_values: value.axes_values.clone(),
        }
    }
}

impl TryFrom<RawFont> for Font {
    type Error = Error;

    fn try_from(mut from: RawFont) -> Result<Self, Self::Error> {
        if from.is_v2() {
            from.v2_to_v3()?;
        }

        let radix = if from.is_v2() { 16 } else { 10 };
        from.other_stuff.remove("date"); // exists purely to make diffs fail
        from.other_stuff.remove(".formatVersion"); // no longer relevent

        let glyph_order = parse_glyph_order(&from);
        let glyph_to_codepoints = parse_codepoints(&mut from, radix);

        let use_typo_metrics =
            custom_param_int(&from.other_stuff, "Use Typo Metrics").map(|v| v == 1);
        let has_wws_names = custom_param_int(&from.other_stuff, "Has WWS Names").map(|v| v == 1);

        let axes = from.axes.clone().unwrap_or_default();
        let instances: Vec<_> = from
            .instances
            .iter()
            .map(|ri| Instance::new(&axes, ri))
            .collect();

        let default_master_idx = default_master_idx(&from);
        let axis_mappings = RawUserToDesignMapping::new(&from, &instances);

        let mut glyphs = BTreeMap::new();
        for raw_glyph in from.glyphs.into_iter() {
            glyphs.insert(raw_glyph.glyphname.clone(), raw_glyph.try_into()?);
        }

        let mut features = Vec::new();
        for class in from.classes.unwrap_or_default().into_iter() {
            features.push(class_to_feature(class)?);
        }
        for prefix in from.feature_prefixes.unwrap_or_default().into_iter() {
            features.push(prefix_to_feature(prefix)?);
        }
        for feature in from.features.unwrap_or_default().into_iter() {
            features.push(raw_feature_to_feature(feature)?);
        }

        let Some(units_per_em) = from.units_per_em else {
            return Err(Error::NoUnitsPerEm);
        };
        let units_per_em = units_per_em.try_into().map_err(Error::InvalidUpem)?;

        let mut names = BTreeMap::new();
        for name in from.properties.unwrap_or_default() {
            // TODO: we only support dflt, .glyphs l10n names are ignored
            name.value
                .or_else(|| {
                    name.values
                        .unwrap_or_default()
                        .iter()
                        .find(|v| v.language == "dflt")
                        .map(|v| v.value.clone())
                })
                .and_then(|value| names.insert(name.key, value));
        }
        names.insert("familyNames".into(), from.family_name);
        if let Some(version) = names.remove("versionString") {
            names.insert("version".into(), version);
        }

        let metric_names: BTreeMap<usize, String> = from
            .metrics
            .unwrap_or_default()
            .into_iter()
            .enumerate()
            .filter_map(|(idx, metric)| metric.type_.map(|name| (idx, name)))
            .collect();

        let masters = from
            .font_master
            .into_iter()
            .map(|m| FontMaster {
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
                    .filter(|(_, metric)| !metric.is_empty())
                    .collect(),
                typo_ascender: custom_param_int(&m.other_stuff, "typoAscender"),
                typo_descender: custom_param_int(&m.other_stuff, "typoDescender"),
                typo_line_gap: custom_param_int(&m.other_stuff, "typoLineGap"),
                win_ascent: custom_param_int(&m.other_stuff, "winAscent"),
                win_descent: custom_param_int(&m.other_stuff, "winDescent"),
                hhea_ascender: custom_param_int(&m.other_stuff, "hheaAscender"),
                hhea_descender: custom_param_int(&m.other_stuff, "hheaDescender"),
                hhea_line_gap: custom_param_int(&m.other_stuff, "hheaLineGap"),
            })
            .collect();

        Ok(Font {
            units_per_em,
            use_typo_metrics,
            has_wws_names,
            axes,
            masters,
            default_master_idx,
            glyphs,
            glyph_order,
            glyph_to_codepoints,
            axis_mappings,
            features,
            names,
            instances,
            version_major: from.versionMajor.unwrap_or_default() as i32,
            version_minor: from.versionMinor.unwrap_or_default() as u32,
            date: from.date,
            kerning_ltr: parse_kerning(from.other_stuff.get("kerningLTR")),
        })
    }
}

fn preprocess_unparsed_plist(s: &str) -> Cow<str> {
    // Glyphs has a wide variety of unicode definitions, not all of them parser friendly
    // Make unicode always a string, without any wrapping () so we can parse as csv, radix based on format version
    let unicode_re =
        Regex::new(r"(?m)^(?P<prefix>\s*unicode\s*=\s*)[(]?(?P<value>[0-9a-zA-Z,]+)[)]?;\s*$")
            .unwrap();
    unicode_re.replace_all(s, r#"$prefix"$value";"#)
}

impl Font {
    pub fn load(glyphs_file: &path::Path) -> Result<Font, Error> {
        if glyphs_file.extension() == Some(OsStr::new("glyphspackage")) {
            return Font::load_package(glyphs_file);
        }

        debug!("Read {glyphs_file:?}");
        let raw_content = fs::read_to_string(glyphs_file).map_err(Error::IoError)?;
        let raw_content = preprocess_unparsed_plist(&raw_content);
        let raw_content = Plist::parse(&raw_content)
            .map_err(|e| Error::ParseError(glyphs_file.to_path_buf(), e.to_string()))?;

        let raw_font = RawFont::from_plist(raw_content);
        raw_font.try_into()
    }

    fn load_package(glyphs_package: &path::Path) -> Result<Font, Error> {
        if !glyphs_package.is_dir() {
            return Err(Error::NotAGlyphsPackage(glyphs_package.to_path_buf()));
        }
        debug!("Read {glyphs_package:?}");

        let fontinfo_file = glyphs_package.join("fontinfo.plist");
        let fontinfo_data = fs::read_to_string(&fontinfo_file).map_err(Error::IoError)?;
        let mut font_plist = Plist::parse(&fontinfo_data)
            .map_err(|e| Error::ParseError(fontinfo_file.to_path_buf(), e.to_string()))?;

        let Plist::Dictionary(ref mut root_dict) = font_plist else {
            return Err(Error::ParseError(
                fontinfo_file.to_path_buf(),
                "Root must be a dict".to_string(),
            ));
        };

        let mut glyphs: HashMap<String, Dictionary> = HashMap::new();
        let glyphs_dir = glyphs_package.join("glyphs");
        if glyphs_dir.is_dir() {
            for entry in fs::read_dir(glyphs_dir).map_err(Error::IoError)? {
                let entry = entry.map_err(Error::IoError)?;
                let path = entry.path();
                if path.extension() == Some(OsStr::new("glyph")) {
                    let glyph_data = fs::read_to_string(&path).map_err(Error::IoError)?;
                    let glyph_data = preprocess_unparsed_plist(&glyph_data);
                    let glyph_dict = Plist::parse(&glyph_data)
                        .and_then(Plist::expect_dict)
                        .map_err(|e| Error::ParseError(path.clone(), e.to_string()))?;
                    let glyph_name = glyph_dict
                        .get("glyphname")
                        .ok_or_else(|| {
                            Error::ParseError(
                                path.clone(),
                                "Glyph dict must have a 'glyphname' key".to_string(),
                            )
                        })?
                        .clone()
                        .expect_string()
                        .map_err(|e| Error::ParseError(path.clone(), e.to_string()))?;
                    glyphs.insert(glyph_name, glyph_dict);
                }
            }
        }

        // if order.plist file exists, read it and sort glyphs in it accordingly
        let order_file = glyphs_package.join("order.plist");
        let mut ordered_glyphs = Array::new();
        if order_file.exists() {
            let order_data = fs::read_to_string(&order_file).map_err(Error::IoError)?;
            let order_plist = Plist::parse(&order_data)
                .map_err(|e| Error::ParseError(order_file.to_path_buf(), e.to_string()))?;
            let Plist::Array(order) = order_plist else {
                return Err(Error::ParseError(
                    order_file.to_path_buf(),
                    "Root must be an array".to_string(),
                ));
            };
            for glyph_name in order {
                let glyph_name = match glyph_name {
                    Plist::String(glyph_name) => glyph_name,
                    _ => {
                        return Err(Error::ParseError(
                            order_file.to_path_buf(),
                            "Glyph name must be a string".to_string(),
                        ));
                    }
                };
                if glyphs.contains_key(&glyph_name) {
                    ordered_glyphs.push(Plist::Dictionary(glyphs.remove(&glyph_name).unwrap()));
                }
            }
        }
        // sort the glyphs not in order.plist by their name
        let mut glyph_names: Vec<String> = glyphs.keys().cloned().collect();
        glyph_names.sort();
        ordered_glyphs.extend(
            glyph_names
                .into_iter()
                .map(|glyph_name| Plist::Dictionary(glyphs.remove(&glyph_name).unwrap())),
        );
        assert!(glyphs.is_empty());
        root_dict.insert("glyphs".to_string(), Plist::Array(ordered_glyphs));

        // ignore UIState.plist which stuff like displayStrings that are not used by us

        let raw_font = RawFont::from_plist(font_plist);
        raw_font.try_into()
    }

    pub fn default_master(&self) -> &FontMaster {
        &self.masters[self.default_master_idx]
    }

    pub fn vendor_id(&self) -> Option<&String> {
        self.names.get("vendorID")
    }
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
    use crate::{
        font::{RawAxisUserToDesignMap, RawFeature, RawUserToDesignMapping},
        Font, FromPlist, Node, Plist, Shape,
    };
    use std::{
        collections::{BTreeMap, BTreeSet, HashSet},
        path::{Path, PathBuf},
    };

    use ordered_float::OrderedFloat;

    use pretty_assertions::assert_eq;

    use kurbo::{Affine, Point};

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
    fn test_glyphs3_node() {
        assert_eq!(
            Node {
                node_type: crate::NodeType::Line,
                pt: super::Point { x: 354.0, y: 183.0 }
            },
            Node::from_plist(Plist::Array(vec![
                Plist::Integer(354),
                Plist::Integer(183),
                Plist::String("l".into()),
            ]))
        );
    }

    #[test]
    fn test_glyphs2_node() {
        assert_eq!(
            Node {
                node_type: crate::NodeType::Line,
                pt: super::Point { x: 354.0, y: 183.0 }
            },
            Node::from_plist(Plist::String("354 183 LINE".into()))
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
        assert_eq!(737.0, default_master.ascender().unwrap().into_inner());
        assert_eq!(-42.0, default_master.descender().unwrap().into_inner());
    }

    #[test]
    fn read_wght_var_2_metrics() {
        assert_wght_var_metrics(&Font::load(&glyphs2_dir().join("WghtVar.glyphs")).unwrap());
    }

    #[test]
    fn read_wght_var_3_metrics() {
        assert_wght_var_metrics(&Font::load(&glyphs3_dir().join("WghtVar.glyphs")).unwrap());
    }

    fn assert_load_v2_matches_load_v3(name: &str) {
        let _ = env_logger::builder().is_test(true).try_init();
        let filename = format!("{name}.glyphs");
        let pkgname = format!("{name}.glyphspackage");
        let g2 = Font::load(&glyphs2_dir().join(filename.clone())).unwrap();
        let g2_pkg = Font::load(&glyphs2_dir().join(pkgname.clone())).unwrap();
        let g3 = Font::load(&glyphs3_dir().join(filename.clone())).unwrap();
        let g3_pkg = Font::load(&glyphs3_dir().join(pkgname.clone())).unwrap();

        // Handy if troubleshooting
        std::fs::write("/tmp/g2.glyphs.txt", format!("{g2:#?}")).unwrap();
        std::fs::write("/tmp/g2.glyphspackage.txt", format!("{g2_pkg:#?}")).unwrap();
        std::fs::write("/tmp/g3.glyphs.txt", format!("{g3:#?}")).unwrap();
        std::fs::write("/tmp/g3.glyphspackage.txt", format!("{g3_pkg:#?}")).unwrap();

        // Assert fields that often don't match individually before doing the whole struct for nicer diffs
        assert_eq!(g2.axes, g3.axes);
        for (g2m, g3m) in g2.masters.iter().zip(g3.masters.iter()) {
            assert_eq!(g2m, g3m);
        }
        assert_eq!(g2, g3);
        assert_eq!(g2_pkg, g3_pkg);
        assert_eq!(g3_pkg, g3);
    }

    #[test]
    fn read_wght_var_2_and_3() {
        assert_load_v2_matches_load_v3("WghtVar");
    }

    #[test]
    fn read_wght_var_avar_2_and_3() {
        assert_load_v2_matches_load_v3("WghtVar_Avar");
    }

    #[test]
    fn read_wght_var_instances_2_and_3() {
        assert_load_v2_matches_load_v3("WghtVar_Instances");
    }

    #[test]
    fn read_wght_var_os2_2_and_3() {
        assert_load_v2_matches_load_v3("WghtVar_OS2");
    }

    #[test]
    fn read_wght_var_anchors_2_and_3() {
        assert_load_v2_matches_load_v3("WghtVar_Anchors");
    }

    #[test]
    fn read_infinity_2_and_3() {
        assert_load_v2_matches_load_v3("infinity");
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
            &BTreeSet::from([0x1234]),
            font.glyph_to_codepoints.get("name").unwrap(),
        );
        assert_eq!(1, font.glyphs.len());
    }

    #[test]
    fn understand_v2_style_quoted_hex_unicode_sequence() {
        let font = Font::load(&glyphs2_dir().join("Unicode-QuotedHexSequence.glyphs")).unwrap();
        assert_eq!(
            &BTreeSet::from([0x2044, 0x200D, 0x2215]),
            font.glyph_to_codepoints.get("name").unwrap(),
        );
        assert_eq!(1, font.glyphs.len());
    }

    #[test]
    fn understand_v3_style_unquoted_decimal_unicode() {
        let font = Font::load(&glyphs3_dir().join("Unicode-UnquotedDec.glyphs")).unwrap();
        assert_eq!(
            &BTreeSet::from([182]),
            font.glyph_to_codepoints.get("name").unwrap()
        );
        assert_eq!(1, font.glyphs.len());
    }

    #[test]
    fn understand_v3_style_unquoted_decimal_unicode_sequence() {
        let font = Font::load(&glyphs3_dir().join("Unicode-UnquotedDecSequence.glyphs")).unwrap();
        assert_eq!(
            &BTreeSet::from([1619, 1764]),
            font.glyph_to_codepoints.get("name").unwrap(),
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
        let font = Font::load(&glyphs3_dir().join("WghtVar_GlyphOrder.glyphs")).unwrap();
        assert_eq!(vec!["hyphen", "space", "exclam"], font.glyph_order);
    }

    #[test]
    fn loads_global_axis_mappings_from_glyphs2() {
        let font = Font::load(&glyphs2_dir().join("OpszWghtVar_AxisMappings.glyphs")).unwrap();

        // Did you load the mappings? DID YOU?!
        assert_eq!(
            RawUserToDesignMapping(BTreeMap::from([
                (
                    "Optical Size".to_string(),
                    RawAxisUserToDesignMap(vec![
                        (OrderedFloat(12.0), OrderedFloat(12.0)),
                        (OrderedFloat(72.0), OrderedFloat(72.0))
                    ])
                ),
                (
                    "Weight".to_string(),
                    RawAxisUserToDesignMap(vec![
                        (OrderedFloat(100.0), OrderedFloat(40.0)),
                        (OrderedFloat(200.0), OrderedFloat(46.0)),
                        (OrderedFloat(300.0), OrderedFloat(51.0)),
                        (OrderedFloat(400.0), OrderedFloat(57.0)),
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
            RawUserToDesignMapping(BTreeMap::from([(
                "Weight".to_string(),
                RawAxisUserToDesignMap(vec![
                    (OrderedFloat(400.0), OrderedFloat(0.0)),
                    (OrderedFloat(500.0), OrderedFloat(8.0)),
                    (OrderedFloat(700.0), OrderedFloat(10.0)),
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
            font.features.iter().map(|f| f.as_str()).collect::<Vec<_>>()
        )
    }

    #[test]
    fn fea_for_prefix() {
        let font = Font::load(&glyphs2_dir().join("Fea_Prefix.glyphs")).unwrap();
        assert_eq!(
            vec![
                concat!(
                    "# Prefix: Languagesystems\n",
                    "# automatic\n",
                    "languagesystem DFLT dflt;\n\n",
                    "languagesystem latn dflt;\n",
                    "and more;\n",
                ),
                concat!("# Prefix: \n# automatic\nthanks for all the fish;",),
            ],
            font.features.iter().map(|f| f.as_str()).collect::<Vec<_>>()
        )
    }

    #[test]
    fn fea_for_feature() {
        let font = Font::load(&glyphs2_dir().join("Fea_Feature.glyphs")).unwrap();
        assert_eq!(
            vec![
                concat!(
                    "feature aalt {\n",
                    "feature locl;\n",
                    "feature tnum;\n",
                    "} aalt;",
                ),
                concat!(
                    "feature ccmp {\n",
                    "# automatic\n",
                    "lookup ccmp_Other_2 {\n",
                    "  sub @Markscomb' @MarkscombCase by @MarkscombCase;\n",
                    "  sub @MarkscombCase @Markscomb' by @MarkscombCase;\n",
                    "} ccmp_Other_2;\n\n",
                    "etc;\n",
                    "} ccmp;",
                ),
            ],
            font.features.iter().map(|f| f.as_str()).collect::<Vec<_>>()
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
            font.features.iter().map(|f| f.as_str()).collect::<Vec<_>>()
        )
    }

    #[test]
    fn tags_make_excellent_names() {
        let raw = RawFeature {
            name: None,
            tag: Some("aalt".to_string()),
            automatic: None,
            code: "blah".to_string(),
            other_stuff: BTreeMap::new(),
        };
        assert_eq!("aalt", raw.name().unwrap());
    }

    #[test]
    fn v2_to_v3_simple_names() {
        let v2 = Font::load(&glyphs2_dir().join("WghtVar.glyphs")).unwrap();
        let v3 = Font::load(&glyphs3_dir().join("WghtVar.glyphs")).unwrap();
        assert_eq!(v3.names, v2.names);
    }

    #[test]
    fn v2_to_v3_more_names() {
        let v2 = Font::load(&glyphs2_dir().join("TheBestNames.glyphs")).unwrap();
        let v3 = Font::load(&glyphs3_dir().join("TheBestNames.glyphs")).unwrap();
        assert_eq!(v3.names, v2.names);
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
                        .map(|(a, v)| (a.name.as_str(), v.0 as f32))
                        .collect::<Vec<_>>()
                ))
                .collect::<Vec<_>>()
        );
    }

    #[test]
    fn read_typo_whatsits() {
        let font = Font::load(&glyphs2_dir().join("WghtVar_OS2.glyphs")).unwrap();
        assert_eq!(Some(1193), font.default_master().typo_ascender);
        assert_eq!(Some(-289), font.default_master().typo_descender);
    }

    #[test]
    fn read_os2_flags_default_set() {
        let font = Font::load(&glyphs2_dir().join("WghtVar.glyphs")).unwrap();
        assert_eq!(
            (Some(true), Some(true)),
            (font.use_typo_metrics, font.has_wws_names)
        );
    }

    #[test]
    fn read_os2_flags_default_unset() {
        let font = Font::load(&glyphs2_dir().join("WghtVar_OS2.glyphs")).unwrap();
        assert_eq!((None, None), (font.use_typo_metrics, font.has_wws_names));
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
            .map(|((n1, n2), value)| (n1.as_str(), n2.as_str(), *value))
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
                    ("@MMK_L_bracketleft_R", "exclam", -165),
                    ("bracketleft", "bracketright", -300),
                    ("exclam", "@MMK_R_bracketright_L", -160),
                    ("exclam", "exclam", -360),
                    ("exclam", "hyphen", 20),
                    ("hyphen", "hyphen", -150),
                ],
            ),
            (actual_groups, actual_kerning),
        );
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
}
