//! The general strategy is just to use a plist for storage. Also, lots of
//! unwrapping.
//!
//! There are lots of other ways this could go, including something serde-like
//! where it gets serialized to more Rust-native structures, proc macros, etc.

use std::borrow::Cow;
use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::ffi::OsStr;
use std::hash::Hash;
use std::str::FromStr;
use std::{fs, path};

use crate::glyphdata::{Category, GlyphData, Subcategory};
use crate::plist::FromPlist;
use kurbo::{Affine, Point, Vec2};
use log::{debug, warn};
use ordered_float::OrderedFloat;
use plist_derive::FromPlist;
use regex::Regex;
use smol_str::SmolStr;

use crate::error::Error;
use crate::plist::{Plist, Token, Tokenizer, VecDelimiters};

const V3_METRIC_NAMES: [&str; 6] = [
    "ascender",
    "baseline",
    "descender",
    "cap height",
    "x-height",
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
    pub fs_type: Option<u16>,
    pub use_typo_metrics: Option<bool>,
    pub has_wws_names: Option<bool>,
    pub axes: Vec<Axis>,
    pub masters: Vec<FontMaster>,
    pub default_master_idx: usize,
    pub glyphs: BTreeMap<SmolStr, Glyph>,
    pub glyph_order: Vec<SmolStr>,
    // tag => (user:design) tuples
    pub axis_mappings: RawUserToDesignMapping,
    pub virtual_masters: Vec<BTreeMap<String, OrderedFloat<f64>>>,
    pub features: Vec<FeatureSnippet>,
    pub names: BTreeMap<String, String>,
    pub instances: Vec<Instance>,
    pub version_major: i32,
    pub version_minor: u32,
    pub date: Option<String>,

    // master id => { (name or class, name or class) => adjustment }
    pub kerning_ltr: Kerning,
}

/// master id => { (name or class, name or class) => adjustment }
#[derive(Clone, Debug, Default, PartialEq, Hash)]
pub struct Kerning(BTreeMap<String, BTreeMap<(String, String), i32>>);

impl Kerning {
    pub fn get(&self, master_id: &str) -> Option<&BTreeMap<(String, String), i32>> {
        self.0.get(master_id)
    }

    pub fn keys(&self) -> impl Iterator<Item = &String> {
        self.0.keys()
    }

    pub fn iter(&self) -> impl Iterator<Item = (&String, &BTreeMap<(String, String), i32>)> {
        self.0.iter()
    }

    fn insert(
        &mut self,
        master_id: String,
        lhs_class_or_group: String,
        rhs_class_or_group: String,
        kern: i64,
    ) {
        *self
            .0
            .entry(master_id)
            .or_default()
            .entry((lhs_class_or_group, rhs_class_or_group))
            .or_default() = kern as i32;
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
                let lhs_name_or_class: String = tokenizer.parse()?;
                tokenizer.eat(b'=')?;
                tokenizer.eat(b'{')?;

                // rhs name = value pairs
                loop {
                    if tokenizer.eat(b'}').is_ok() {
                        break;
                    }

                    let rhs_name_or_class: String = tokenizer.parse()?;
                    tokenizer.eat(b'=')?;
                    let value: i64 = tokenizer.parse()?;
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

#[derive(Debug, PartialEq, Eq, Hash)]
pub struct FeatureSnippet {
    pub content: String,
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
    pub unicode: BTreeSet<u32>,
    /// The left kerning group
    pub left_kern: Option<SmolStr>,
    /// The right kerning group
    pub right_kern: Option<SmolStr>,
    pub category: Option<Category>,
    pub sub_category: Option<Subcategory>,
}

impl Glyph {
    pub fn is_nonspacing_mark(&self) -> bool {
        matches!(
            (self.category, self.sub_category),
            (
                Some(Category::Mark),
                Some(Subcategory::Nonspacing | Subcategory::SpacingCombining)
            )
        )
    }

    pub(crate) fn has_components(&self) -> bool {
        self.layers
            .iter()
            .flat_map(Layer::components)
            .next()
            .is_some()
    }
}

#[derive(Debug, Default, Clone, PartialEq, Hash)]
pub struct Layer {
    pub layer_id: String,
    pub associated_master_id: Option<String>,
    pub width: OrderedFloat<f64>,
    pub shapes: Vec<Shape>,
    pub anchors: Vec<Anchor>,
    pub attributes: LayerAttributes,
}

impl Layer {
    pub fn is_master(&self) -> bool {
        self.associated_master_id.is_none()
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

    // TODO add is_alternate, is_color, etc.
}

#[derive(Clone, Default, Debug, PartialEq, Hash)]
pub struct LayerAttributes {
    pub coordinates: Vec<OrderedFloat<f64>>,
    // TODO: add axisRules, color, etc.
}

// hand-parse because they can take multiple shapes
impl FromPlist for LayerAttributes {
    fn parse(tokenizer: &mut Tokenizer<'_>) -> Result<Self, crate::plist::Error> {
        let mut coordinates = Vec::new();

        tokenizer.eat(b'{')?;

        loop {
            if tokenizer.eat(b'}').is_ok() {
                break;
            }

            let key: String = tokenizer.parse()?;
            tokenizer.eat(b'=')?;
            match key.as_str() {
                "coordinates" => {
                    coordinates = tokenizer.parse()?;
                }
                // skip unsupported attributes for now
                // TODO: match the others
                _ => tokenizer.skip_rec()?,
            }
            tokenizer.eat(b';')?;
        }

        Ok(LayerAttributes { coordinates })
    }
}

#[derive(Debug, Clone, PartialEq, Hash)]
pub enum Shape {
    Path(Path),
    Component(Component),
}

// The font you get directly from a plist, minimally modified
// Types chosen specifically to accomodate plist translation.
#[derive(Default, Debug, PartialEq, FromPlist)]
#[allow(non_snake_case)]
struct RawFont {
    #[fromplist(key = ".appVersion")]
    app_version: i64,
    #[fromplist(key = ".formatVersion")]
    format_version: i64,
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
    custom_parameters: CustomParameters,
}

// we use a vec of tuples instead of a map because there can be multiple
// values for the same name (e.g. 'Virtual Master')
#[derive(Clone, Default, Debug, PartialEq, Eq, Hash)]
pub(crate) struct CustomParameters(Vec<(String, CustomParameterValue)>);

impl CustomParameters {
    /// Get the first parameter with the given name, or `None` if not found.
    fn get(&self, name: &str) -> Option<&CustomParameterValue> {
        self.0.iter().find_map(|(n, v)| (n == name).then_some(v))
    }

    fn int(&self, name: &str) -> Option<i64> {
        let Some(CustomParameterValue::Int(i)) = self.get(name) else {
            return None;
        };
        Some(*i)
    }

    fn float(&self, name: &str) -> Option<OrderedFloat<f64>> {
        let value = self.get(name)?;
        match value {
            CustomParameterValue::Int(i) => Some((*i as f64).into()),
            CustomParameterValue::Float(f) => Some(*f),
            _ => None,
        }
    }

    fn bool(&self, name: &str) -> Option<bool> {
        self.int(name).map(|v| v == 1)
    }

    fn string(&self, name: &str) -> Option<&str> {
        let Some(CustomParameterValue::String(str)) = self.get(name) else {
            return None;
        };
        Some(str)
    }

    fn axes(&self) -> Option<&Vec<Axis>> {
        let Some(CustomParameterValue::Axes(axes)) = self.get("Axes") else {
            return None;
        };
        Some(axes)
    }

    fn axis_mappings(&self) -> Option<&Vec<AxisMapping>> {
        let Some(CustomParameterValue::AxesMappings(mappings)) = self.get("Axis Mappings") else {
            return None;
        };
        Some(mappings)
    }

    fn axis_locations(&self) -> Option<&Vec<AxisLocation>> {
        let Some(CustomParameterValue::AxisLocations(locations)) = self.get("Axis Location") else {
            return None;
        };
        Some(locations)
    }

    fn glyph_order(&self) -> Option<&Vec<SmolStr>> {
        let Some(CustomParameterValue::GlyphOrder(names)) = self.get("glyphOrder") else {
            return None;
        };
        Some(names)
    }

    fn virtual_masters(&self) -> impl Iterator<Item = &Vec<AxisLocation>> {
        self.0.iter().filter_map(|(name, value)| {
            if name == "Virtual Master" {
                let CustomParameterValue::VirtualMaster(locations) = value else {
                    panic!("Virtual Master parameter has wrong type!");
                };
                return Some(locations);
            }
            None
        })
    }

    fn fs_type(&self) -> Option<&Vec<i64>> {
        let Some(CustomParameterValue::FsType(bits)) = self.get("fsType") else {
            return None;
        };
        Some(bits)
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
enum CustomParameterValue {
    Int(i64),
    Float(OrderedFloat<f64>),
    String(String),
    Axes(Vec<Axis>),
    AxesMappings(Vec<AxisMapping>),
    AxisLocations(Vec<AxisLocation>),
    GlyphOrder(Vec<SmolStr>),
    VirtualMaster(Vec<AxisLocation>),
    FsType(Vec<i64>),
}

/// Hand-parse these because they take multiple shapes
impl FromPlist for CustomParameters {
    fn parse(tokenizer: &mut Tokenizer<'_>) -> Result<Self, crate::plist::Error> {
        use crate::plist::Error;
        let mut params = Vec::new();

        tokenizer.eat(b'(')?;

        loop {
            if tokenizer.eat(b')').is_ok() {
                break;
            }

            tokenizer.eat(b'{')?;

            // Can we at least count on always having a name and a value?
            let mut name = None;
            let mut value = None;
            for _ in 0..2 {
                let key: String = tokenizer.parse()?;
                tokenizer.eat(b'=')?;
                match key.as_str() {
                    "name" => {
                        let the_name: String = tokenizer.parse()?;
                        name = Some(the_name);
                    }
                    "value" => {
                        let peek = tokenizer.peek()?;
                        match peek {
                            Token::Atom(..) => {
                                let Token::Atom(val) = tokenizer.lex()? else {
                                    panic!("That shouldn't happen");
                                };
                                value = match Plist::parse(val)? {
                                    Plist::Integer(i) => Some(CustomParameterValue::Int(i)),
                                    Plist::Float(f) => Some(CustomParameterValue::Float(f)),
                                    Plist::String(s) => Some(CustomParameterValue::String(s)),
                                    _ => panic!("atom has to be int, float, or string"),
                                };
                            }
                            Token::OpenBrace if name == Some(String::from("Axis Mappings")) => {
                                let mappings: Vec<AxisMapping> = tokenizer
                                    .parse_delimited_vec(VecDelimiters::SEMICOLON_SV_IN_BRACES)?;
                                value = Some(CustomParameterValue::AxesMappings(mappings));
                            }
                            Token::String(..) => {
                                let token = tokenizer.lex()?;
                                let Token::String(val) = token else {
                                    return Err(Error::UnexpectedDataType {
                                        expected: "String",
                                        found: token.name(),
                                    });
                                };
                                value = Some(CustomParameterValue::String(val.to_string()));
                            }
                            _ if name == Some(String::from("Axes")) => {
                                let Token::OpenParen = peek else {
                                    return Err(Error::UnexpectedChar('('));
                                };
                                value = Some(CustomParameterValue::Axes(tokenizer.parse()?));
                            }
                            _ if name == Some(String::from("glyphOrder")) => {
                                let Token::OpenParen = peek else {
                                    return Err(Error::UnexpectedChar('('));
                                };
                                value = Some(CustomParameterValue::GlyphOrder(tokenizer.parse()?));
                            }
                            _ if name == Some(String::from("Axis Location")) => {
                                let Token::OpenParen = peek else {
                                    return Err(Error::UnexpectedChar('('));
                                };
                                value =
                                    Some(CustomParameterValue::AxisLocations(tokenizer.parse()?));
                            }
                            _ if name == Some(String::from("Virtual Master")) => {
                                let Token::OpenParen = peek else {
                                    return Err(Error::UnexpectedChar('('));
                                };
                                value =
                                    Some(CustomParameterValue::VirtualMaster(tokenizer.parse()?));
                            }
                            _ if name == Some(String::from("fsType")) => {
                                let Token::OpenParen = peek else {
                                    return Err(Error::UnexpectedChar('('));
                                };
                                value = Some(CustomParameterValue::FsType(tokenizer.parse()?));
                            }
                            _ => tokenizer.skip_rec()?,
                        }
                    }
                    _ => return Err(Error::SomethingWentWrong),
                }
                tokenizer.eat(b';')?;
            }

            if let (Some(name), Some(value)) = (name, value) {
                params.push((name, value));
            }

            tokenizer.eat(b'}')?;
            // Optional comma
            let _ = tokenizer.eat(b',');
        }

        // the close paren broke the loop, don't consume here
        Ok(CustomParameters(params))
    }
}

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash, FromPlist)]
pub struct CustomParam {
    name: String,
    value: String,
}

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash, FromPlist)]
pub struct AxisLocation {
    #[fromplist(alt_name = "Axis")]
    axis_name: String,
    #[fromplist(alt_name = "Location")]
    location: OrderedFloat<f64>,
}

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash)]
pub struct AxisMapping {
    tag: String,
    user_to_design: Vec<(OrderedFloat<f64>, OrderedFloat<f64>)>,
}

impl FromPlist for AxisMapping {
    fn parse(tokenizer: &mut Tokenizer<'_>) -> Result<Self, crate::plist::Error> {
        let tag = tokenizer.parse()?;
        tokenizer.eat(b'=')?;
        tokenizer.eat(b'{')?;
        let mut user_to_design = Vec::new();
        while tokenizer.eat(b'}').is_err() {
            let user: OrderedFloat<f64> = tokenizer.parse()?;
            tokenizer.eat(b'=')?;
            let design: OrderedFloat<f64> = tokenizer.parse()?;
            tokenizer.eat(b';')?;
            user_to_design.push((user, design));
        }
        Ok(AxisMapping {
            tag,
            user_to_design,
        })
    }
}

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash, FromPlist)]
struct RawMetric {
    // So named to let FromPlist populate it from a field called "type"
    type_: Option<String>,
}

#[derive(Default, Clone, Debug, PartialEq, Eq, Hash, FromPlist)]
struct RawName {
    key: String,
    value: Option<String>,
    values: Vec<RawNameValue>,
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
    code: String,

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
    #[fromplist(ignore)]
    other_stuff: BTreeMap<String, Plist>,
}

#[derive(Default, Clone, Debug, PartialEq, FromPlist)]
struct RawLayer {
    name: String,
    layer_id: String,
    associated_master_id: Option<String>,
    width: OrderedFloat<f64>,
    shapes: Vec<RawShape>,
    paths: Vec<Path>,
    components: Vec<Component>,
    anchors: Vec<RawAnchor>,
    #[fromplist(alt_name = "attr")]
    attributes: LayerAttributes,
    #[fromplist(ignore)]
    other_stuff: BTreeMap<String, Plist>,
}

impl RawLayer {
    /// Return true if the layer is a draft that is not meant to be compiled.
    ///
    /// The presence of an associated master indicates this is not a simple 'master' instance.
    /// Without 'attributes' that specify whether it's a special intermediate, alternate or
    /// color layer, we can assume the non-master layer is a draft.
    fn is_draft(&self) -> bool {
        self.associated_master_id.is_some() && self.attributes == Default::default()
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

    // for components, an optional name to rename an anchor
    // on the target glyph during anchor propagation
    anchor: Option<SmolStr>,
    transform: Option<String>, // v2
    pos: Vec<f64>,             // v3
    angle: Option<f64>,        // v3
    scale: Vec<f64>,           // v3
}

#[derive(Default, Clone, Debug, PartialEq, Eq, Hash, FromPlist)]
pub struct Path {
    pub closed: bool,
    pub nodes: Vec<Node>,
}

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
    metric_values: BTreeMap<String, RawMetricValue>,
    pub typo_ascender: Option<i64>,
    pub typo_descender: Option<i64>,
    pub typo_line_gap: Option<i64>,
    pub win_ascent: Option<i64>,
    pub win_descent: Option<i64>,
    pub hhea_ascender: Option<i64>,
    pub hhea_descender: Option<i64>,
    pub hhea_line_gap: Option<i64>,
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
}

impl FontMaster {
    fn read_metric(&self, metric_name: &str) -> Option<f64> {
        self.metric_values
            .get(metric_name)
            .and_then(|metric| metric.pos)
            .map(|x| x.into_inner())
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

    custom_parameters: CustomParameters,

    #[fromplist(ignore)]
    other_stuff: BTreeMap<String, Plist>,
}

#[derive(Default, Debug, Clone, PartialEq, Eq, Hash, FromPlist)]
pub struct RawMetricValue {
    pos: Option<OrderedFloat<f64>>,
    over: Option<OrderedFloat<f64>>,
}

impl RawMetricValue {
    fn is_empty(&self) -> bool {
        self.pos.is_none() && self.over.is_none()
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
                )))
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
    let node_type =
        NodeType::from_str(&node_type).map_err(|_| crate::plist::Error::SomethingWentWrong)?;

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

fn v2_to_v3_name(properties: &mut Vec<RawName>, v2_prop: Option<&str>, v3_name: &str) {
    // https://github.com/schriftgestalt/GlyphsSDK/blob/Glyphs3/GlyphsFileFormat/GlyphsFileFormatv3.md#properties
    // Keys ending with "s" are localizable that means the second key is values
    if let Some(value) = v2_prop {
        properties.push(if v3_name.ends_with('s') {
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
        });
    }
}

impl RawFont {
    fn is_v2(&self) -> bool {
        self.format_version < 3
    }

    fn v2_to_v3_axes(&mut self) -> Result<Vec<String>, Error> {
        let mut tags = Vec::new();
        if let Some(v2_axes) = self.custom_parameters.axes() {
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
                type_: Some(n.to_string()),
            })
            .collect();

        // in each font master setup the parallel array
        for master in self.font_master.iter_mut() {
            // Copy the v2 metrics from actual fields into the parallel array rig
            // the order matters :(
            let mut metric_values: Vec<_> = [
                master.ascender,
                master.baseline,
                master.descender,
                master.cap_height,
                master.x_height,
                master.italic_angle,
            ]
            .into_iter()
            .map(|pos| pos.and_then(|v| (v != 0.0).then_some(v)))
            .map(|pos| RawMetricValue { pos, over: None })
            .collect();

            // "alignmentZones is now a set of over (overshoot) properties attached to metrics"
            // TODO: are these actually aligned by index or do you just lookup lhs to get over from rhs
            assert!(
                master.alignment_zones.len() <= metric_values.len(),
                "{} should be <= {}",
                master.alignment_zones.len(),
                metric_values.len()
            );
            for (metric_value, alignment_zone) in
                metric_values.iter_mut().zip(master.alignment_zones.iter())
            {
                // Alignment zones look like {800, 16}, but (800, 16) would be more useful
                let alignment_zone = alignment_zone.replace('{', "(").replace('}', ")");
                let values = Vec::<i64>::parse_plist(&alignment_zone)?;
                if values.len() != 2 {
                    warn!("Confusing alignment zone {alignment_zone}, skipping");
                    continue;
                };
                if values[1] != 0 {
                    metric_value.over = Some(OrderedFloat(values[1] as f64));
                }
            }
            master.metric_values = metric_values;
        }
        Ok(())
    }

    fn v2_to_v3_weight(&mut self) -> Result<(), Error> {
        for master in self.font_master.iter_mut() {
            // Don't remove weightValue, we need it to understand axes
            if master.weight_value.is_none() {
                continue;
            }
            // Missing = default = Regular per @anthrotype
            master.name = Some(
                master
                    .weight
                    .take()
                    .unwrap_or_else(|| String::from("Regular")),
            );
        }
        Ok(())
    }

    fn v2_to_v3_names(&mut self) -> Result<(), Error> {
        // The copyright, designer, designerURL, manufacturer, manufacturerURL top-level entries
        // have been moved into new top-level properties dictionary and made localizable.
        // Take properties to avoid incompatible borrowing against self
        let mut properties = std::mem::take(&mut self.properties);

        v2_to_v3_name(&mut properties, self.copyright.as_deref(), "copyrights");
        v2_to_v3_name(&mut properties, self.designer.as_deref(), "designers");
        v2_to_v3_name(&mut properties, self.designerURL.as_deref(), "designerURL");
        v2_to_v3_name(
            &mut properties,
            self.manufacturer.as_deref(),
            "manufacturers",
        );
        v2_to_v3_name(
            &mut properties,
            self.manufacturerURL.as_deref(),
            "manufacturerURL",
        );

        let mut v2_to_v3_param = |v2_name: &str, v3_name: &str| {
            if let Some(value) = self.custom_parameters.string(v2_name) {
                v2_to_v3_name(&mut properties, Some(value), v3_name);
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

        self.properties = properties;

        Ok(())
    }

    fn v2_to_v3_instances(&mut self) -> Result<(), Error> {
        for instance in self.instances.iter_mut() {
            // named clases become #s in v3
            for (tag, opt) in [
                ("wght", &mut instance.weight_class),
                ("wdth", &mut instance.width_class),
            ] {
                let Some(value) = opt.as_ref() else {
                    continue;
                };
                if f32::from_str(value).is_ok() {
                    continue;
                };
                let Some(value) = lookup_class_value(tag, value) else {
                    return Err(Error::UnknownValueName(value.clone()));
                };
                let _ = opt.insert(format!("{}", value as i64));
            }
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
        self.v2_to_v3_weight()?;
        self.v2_to_v3_axes()?;
        self.v2_to_v3_metrics()?;
        self.v2_to_v3_names()?;
        self.v2_to_v3_instances()?;
        self.v2_to_v3_layer_attributes();
        Ok(())
    }
}

fn parse_glyph_order(raw_font: &RawFont) -> Vec<SmolStr> {
    let mut valid_names: HashSet<_> = raw_font.glyphs.iter().map(|g| &g.glyphname).collect();
    let mut glyph_order = Vec::new();

    // Add all valid glyphOrder entries in order
    // See https://github.com/googlefonts/fontmake-rs/pull/43/files#r1044627972
    if let Some(names) = raw_font.custom_parameters.glyph_order() {
        names.iter().for_each(|name| {
            if valid_names.remove(name) {
                glyph_order.push(name.clone());
            }
        })
    }

    // Add anything left over in file order
    raw_font
        .glyphs
        .iter()
        .filter(|g| valid_names.contains(&g.glyphname))
        .for_each(|g| glyph_order.push(g.glyphname.clone()));

    glyph_order
}

// glyphs2 uses hex, glyphs3 uses base10
fn parse_codepoint_str(s: &str, radix: u32) -> BTreeSet<u32> {
    s.split(',')
        .map(|cp| u32::from_str_radix(cp, radix).unwrap())
        .collect()
}

/// <https://github.com/googlefonts/glyphsLib/blob/6f243c1f732ea1092717918d0328f3b5303ffe56/Lib/glyphsLib/builder/axes.py#L578>
fn default_master_idx(raw_font: &RawFont) -> usize {
    // Prefer an explicit origin
    // https://github.com/googlefonts/fontmake-rs/issues/44
    raw_font
        .custom_parameters
        .string("Variable Font Origin")
        .map(|origin| {
            raw_font
                .font_master
                .iter()
                .enumerate()
                .find(|(_, master)| master.id == origin)
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
        .iter()
        .enumerate()
        .find_map(|(i, a)| if pred(a) { Some(i) } else { None })
}

fn user_to_design_from_axis_mapping(
    from: &RawFont,
) -> Option<BTreeMap<String, RawAxisUserToDesignMap>> {
    let mappings = from.custom_parameters.axis_mappings()?;
    let mut axis_mappings: BTreeMap<String, RawAxisUserToDesignMap> = BTreeMap::new();
    for mapping in mappings {
        let Some(axis_index) = axis_index(from, |a| a.tag == mapping.tag) else {
            panic!("No such axes: {:?}", mapping.tag);
        };
        let axis_name = &from.axes.get(axis_index).unwrap().name;
        for (user, design) in mapping.user_to_design.iter() {
            let user: f32 = user.0 as f32;
            let design = design.0 as f32;
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
    let master_locations: Vec<_> = from
        .font_master
        .iter()
        .filter_map(|m| m.custom_parameters.axis_locations())
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
    for (master, axis_locations) in from.font_master.iter().zip(master_locations) {
        for axis_location in axis_locations {
            let Some(axis_index) = axis_index(from, |a| a.name == axis_location.axis_name) else {
                panic!("Axis has no index {axis_location:?}");
            };
            let user = axis_location.location.0 as f32;
            let design = master.axes_values[axis_index].into_inner() as f32;

            axis_mappings
                .entry(axis_location.axis_name.clone())
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

    pub fn is_identity(&self) -> bool {
        self.0.iter().all(|(u, d)| u == d)
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
            for (axis, value) in from.axes.iter().zip(&master.axes_values) {
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
                transform *= Affine::rotate(angle.to_radians());
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
            })
        } else {
            // no ref; presume it's a path
            Shape::Path(Path {
                closed: from.closed.unwrap_or_default(),
                nodes: from.nodes.clone(),
            })
        };
        Ok(shape)
    }
}

fn map_and_push_if_present<T, U>(dest: &mut Vec<T>, src: Vec<U>, map: fn(U) -> T) {
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
        for raw_shape in from.shapes {
            shapes.push(raw_shape.try_into()?);
        }

        let anchors = from
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

        Ok(Layer {
            layer_id: from.layer_id,
            associated_master_id: from.associated_master_id,
            width: from.width,
            shapes,
            anchors,
            attributes: from.attributes,
        })
    }
}

impl RawGlyph {
    // we pass in the radix because it depends on the version, stored in the font struct
    fn build(self, codepoint_radix: u32) -> Result<Glyph, Error> {
        let mut instances = Vec::new();
        for layer in self.layers {
            if layer.is_draft() {
                continue;
            }
            instances.push(layer.try_into()?);
        }
        // if category/subcategory were set in the source, we keep them;
        // otherwise we look them up based on the bundled GlyphData.
        // (we use this info later to determine GDEF categories, zero the width
        // on non-spacing marks, etc)
        let mut category = self
            .category
            .map(|cat| Category::from_str(&cat))
            .transpose()
            .map_err(|category| Error::BadCategory {
                glyph: self.glyphname.clone(),
                category,
            })?;
        let mut sub_category = self
            .sub_category
            .map(|cat| Subcategory::from_str(&cat))
            .transpose()
            .map_err(|category| Error::BadCategory {
                glyph: self.glyphname.clone(),
                category,
            })?;

        let codepoints = self
            .unicode
            .map(|s| parse_codepoint_str(&s, codepoint_radix))
            .unwrap_or_default();

        if category.is_none() || sub_category.is_none() {
            if let Some((computed_category, computed_subcategory)) =
                get_glyph_category(&self.glyphname, &codepoints)
            {
                // if they were manually set don't change them, otherwise do
                category = category.or(Some(computed_category));
                sub_category = sub_category.or(Some(computed_subcategory));
            }
        }

        Ok(Glyph {
            name: self.glyphname,
            export: self.export.unwrap_or(true),
            layers: instances,
            left_kern: self.kern_left,
            right_kern: self.kern_right,
            unicode: codepoints,
            category,
            sub_category,
        })
    }
}

// This will eventually need to be replaced with something that can handle
// custom GlyphData.xml files, as well as handle overrides that are part of the
// glyph source.
fn get_glyph_category(name: &str, codepoints: &BTreeSet<u32>) -> Option<(Category, Subcategory)> {
    GlyphData::bundled()
        .get_glyph(name, Some(codepoints))
        .map(|info| (info.category, info.subcategory))
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

    fn disabled(&self) -> bool {
        self.disabled == Some(1)
    }
}

// https://github.com/googlefonts/glyphsLib/blob/24b4d340e4c82948ba121dcfe563c1450a8e69c9/Lib/glyphsLib/builder/features.py#L90
fn prefix_to_feature(prefix: RawFeature) -> Result<FeatureSnippet, Error> {
    let name = match &prefix.name {
        Some(name) => name.as_str(),
        None => "",
    };
    let code = format!("# Prefix: {}\n{}{}", name, prefix.autostr(), prefix.code);
    Ok(FeatureSnippet::new(code, prefix.disabled()))
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
    Ok(FeatureSnippet::new(code, feature.disabled()))
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
    Ok(FeatureSnippet::new(code, feature.disabled()))
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
    value: Option<f64>,
) {
    let Some(idx) = axes.iter().position(|a| a.tag == axis_tag) else {
        return;
    };
    let axis = &axes[idx];
    let Some(design) = axes_values.get(idx) else {
        return;
    };
    let Some(value) = value else {
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
            value
                .weight_class
                .as_ref()
                .map(|v| f64::from_str(v).unwrap()),
        );
        add_mapping_if_present(
            &mut axis_mappings,
            axes,
            "wdth",
            value.axes_values.as_ref(),
            value
                .width_class
                .as_ref()
                .map(|v| f64::from_str(v).unwrap()),
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
        let glyph_order = parse_glyph_order(&from);

        let use_typo_metrics = from.custom_parameters.bool("Use Typo Metrics");
        let has_wws_names = from.custom_parameters.bool("Has WWS Names");

        let axes = from.axes.clone();
        let instances: Vec<_> = from
            .instances
            .iter()
            .map(|ri| Instance::new(&axes, ri))
            .collect();

        let default_master_idx = default_master_idx(&from);
        let axis_mappings = RawUserToDesignMapping::new(&from, &instances);

        let mut glyphs = BTreeMap::new();
        for raw_glyph in from.glyphs.into_iter() {
            glyphs.insert(raw_glyph.glyphname.clone(), raw_glyph.build(radix)?);
        }

        let mut features = Vec::new();
        for class in from.classes {
            features.push(class_to_feature(class)?);
        }
        for prefix in from.feature_prefixes {
            features.push(prefix_to_feature(prefix)?);
        }
        for feature in from.features {
            features.push(raw_feature_to_feature(feature)?);
        }

        let Some(units_per_em) = from.units_per_em else {
            return Err(Error::NoUnitsPerEm);
        };
        let units_per_em = units_per_em.try_into().map_err(Error::InvalidUpem)?;

        let fs_type = from
            .custom_parameters
            .fs_type()
            .map(|bits| bits.iter().map(|bit| 1 << bit).sum());

        let mut names = BTreeMap::new();
        for name in from.properties {
            // TODO: we only support dflt, .glyphs l10n names are ignored
            name.value
                .or_else(|| {
                    name.values
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
                typo_ascender: m.custom_parameters.int("typoAscender"),
                typo_descender: m.custom_parameters.int("typoDescender"),
                typo_line_gap: m.custom_parameters.int("typoLineGap"),
                win_ascent: m.custom_parameters.int("winAscent"),
                win_descent: m.custom_parameters.int("winDescent"),
                hhea_ascender: m.custom_parameters.int("hheaAscender"),
                hhea_descender: m.custom_parameters.int("hheaDescender"),
                hhea_line_gap: m.custom_parameters.int("hheaLineGap"),
                underline_thickness: m.custom_parameters.float("underlineThickness"),
                underline_position: m.custom_parameters.float("underlinePosition"),
                strikeout_position: m.custom_parameters.int("strikeoutPosition"),
                strikeout_size: m.custom_parameters.int("strikeoutSize"),
                subscript_x_offset: m.custom_parameters.int("subscriptXOffset"),
                subscript_x_size: m.custom_parameters.int("subscriptXSize"),
                subscript_y_offset: m.custom_parameters.int("subscriptYOffset"),
                subscript_y_size: m.custom_parameters.int("subscriptYSize"),
                superscript_x_offset: m.custom_parameters.int("superscriptXOffset"),
                superscript_x_size: m.custom_parameters.int("superscriptXSize"),
                superscript_y_offset: m.custom_parameters.int("superscriptYOffset"),
                superscript_y_size: m.custom_parameters.int("superscriptYSize"),
            })
            .collect();

        let virtual_masters = from
            .custom_parameters
            .virtual_masters()
            .map(|vm| {
                vm.iter()
                    .map(
                        |AxisLocation {
                             axis_name,
                             location,
                         }| (axis_name.clone(), *location),
                    )
                    .collect()
            })
            .collect();

        Ok(Font {
            units_per_em,
            fs_type,
            use_typo_metrics,
            has_wws_names,
            axes,
            masters,
            default_master_idx,
            glyphs,
            glyph_order,
            axis_mappings,
            virtual_masters,
            features,
            names,
            instances,
            version_major: from.versionMajor.unwrap_or_default() as i32,
            version_minor: from.versionMinor.unwrap_or_default() as u32,
            date: from.date,
            kerning_ltr: from.kerning_LTR,
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
        let mut font = Self::load_impl(glyphs_file)?;
        font.propagate_all_anchors();
        Ok(font)
    }

    // load without propagating anchors
    pub(crate) fn load_impl(glyphs_file: impl AsRef<path::Path>) -> Result<Font, Error> {
        let glyphs_file = glyphs_file.as_ref();
        if glyphs_file.extension() == Some(OsStr::new("glyphspackage")) {
            return Font::load_package(glyphs_file);
        }

        debug!("Read glyphs {glyphs_file:?}");
        let raw_content = fs::read_to_string(glyphs_file).map_err(Error::IoError)?;
        let raw_content = preprocess_unparsed_plist(&raw_content);
        let raw_font = RawFont::parse_plist(&raw_content)
            .map_err(|e| Error::ParseError(glyphs_file.to_path_buf(), format!("{e}")))?;
        raw_font.try_into()
    }

    fn load_package(glyphs_package: &path::Path) -> Result<Font, Error> {
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
        plist::FromPlist,
        Font, Node, Shape,
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

    fn assert_load_v2_matches_load_v3(name: &str, compare: LoadCompare) {
        let has_package = matches!(compare, LoadCompare::GlyphsAndPackage);
        let _ = env_logger::builder().is_test(true).try_init();
        let filename = format!("{name}.glyphs");
        let pkgname = format!("{name}.glyphspackage");
        let g2 = Font::load(&glyphs2_dir().join(filename.clone())).unwrap();
        let g3 = Font::load(&glyphs3_dir().join(filename.clone())).unwrap();

        // Handy if troubleshooting
        std::fs::write("/tmp/g2.glyphs.txt", format!("{g2:#?}")).unwrap();
        std::fs::write("/tmp/g3.glyphs.txt", format!("{g3:#?}")).unwrap();

        // Assert fields that often don't match individually before doing the whole struct for nicer diffs
        assert_eq!(g2.axes, g3.axes);
        for (g2m, g3m) in g2.masters.iter().zip(g3.masters.iter()) {
            assert_eq!(g2m, g3m);
        }
        assert_eq!(g2, g3, "g2 should match g3");

        if has_package {
            let g2_pkg = Font::load(&glyphs2_dir().join(pkgname.clone())).unwrap();
            let g3_pkg = Font::load(&glyphs3_dir().join(pkgname.clone())).unwrap();

            std::fs::write("/tmp/g2.glyphspackage.txt", format!("{g2_pkg:#?}")).unwrap();
            std::fs::write("/tmp/g3.glyphspackage.txt", format!("{g3_pkg:#?}")).unwrap();

            assert_eq!(g2_pkg, g3_pkg, "g2_pkg should match g3_pkg");
            assert_eq!(g3_pkg, g3, "g3_pkg should match g3");
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
                concat!(
                    "# Prefix: Languagesystems\n",
                    "# automatic\n",
                    "languagesystem DFLT dflt;\n\n",
                    "languagesystem latn dflt;\n",
                    "and more;\n",
                ),
                concat!("# Prefix: \n# automatic\nthanks for all the fish;",),
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
    fn tags_make_excellent_names() {
        let raw = RawFeature {
            name: None,
            tag: Some("aalt".to_string()),
            automatic: None,
            disabled: None,
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
            "{:?}",
            font.kerning_ltr
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
        assert!(font.fs_type.is_none());
    }

    #[test]
    fn read_fstype_zero() {
        let font = Font::load(&glyphs3_dir().join("fstype_0x0000.glyphs")).unwrap();
        assert_eq!(Some(0), font.fs_type);
    }

    #[test]
    fn read_fstype_bits() {
        let font = Font::load(&glyphs3_dir().join("fstype_0x0104.glyphs")).unwrap();
        assert_eq!(Some(0x104), font.fs_type);
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
}
