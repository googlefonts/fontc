//! The general strategy is just to use a plist for storage. Also, lots of
//! unwrapping.
//!
//! There are lots of other ways this could go, including something serde-like
//! where it gets serialized to more Rust-native structures, proc macros, etc.

use std::collections::BTreeMap;
use std::hash::Hash;
use std::{fs, path};

use log::warn;
use ordered_float::OrderedFloat;
use regex::Regex;

use crate::error::Error;
use crate::from_plist::FromPlist;
use crate::plist::Plist;

const V3_METRIC_NAMES: [&str; 5] = [
    "ascender",
    "baseline",
    "descender",
    "cap height",
    "x-height",
];
const V2_METRIC_NAMES: [&str; 5] = ["ascender", "baseline", "descender", "capHeight", "xHeight"];

#[derive(Debug, FromPlist, PartialEq, Eq, Hash)]
pub struct Font {
    pub family_name: String,
    pub axes: Option<Vec<Axis>>,
    pub glyphs: Vec<Glyph>,
    pub font_master: Vec<FontMaster>,
    #[rest]
    pub other_stuff: BTreeMap<String, Plist>,
}

#[derive(Clone, Debug, FromPlist, PartialEq, Eq, Hash)]
pub struct Axis {
    pub name: String,
    pub tag: String,
    pub hidden: Option<bool>,
}

#[derive(Clone, Debug, FromPlist, PartialEq, Eq, Hash)]
pub struct Glyph {
    pub layers: Vec<Layer>,
    pub glyphname: String,
    pub codepoints: Option<Vec<i64>>,
    #[rest]
    pub other_stuff: BTreeMap<String, Plist>,
}

#[derive(Clone, Debug, FromPlist, PartialEq, Eq, Hash)]
pub struct Layer {
    pub layer_id: String,
    pub width: OrderedFloat<f64>,
    pub shapes: Option<Vec<Path>>,
    paths: Option<Vec<Path>>, // private, migrated to shapes if present
    //pub components: Option<Vec<Component>>,
    pub anchors: Option<Vec<Anchor>>,
    #[rest]
    pub other_stuff: BTreeMap<String, Plist>,
}

#[derive(Clone, Debug, FromPlist, PartialEq, Eq, Hash)]
pub struct Path {
    pub closed: bool,
    pub nodes: Vec<Node>,
}

// We do not use kurbo's point because it does not hash
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Point {
    x: OrderedFloat<f64>,
    y: OrderedFloat<f64>,
}

impl Point {
    pub fn new(x: f64, y: f64) -> Point {
        Point {
            x: x.into(),
            y: y.into(),
        }
    }
}

// We do not use kurbo's affine because it does not hash
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Affine([OrderedFloat<f64>; 6]);

impl Affine {
    pub fn new(matrix: [f64; 6]) -> Affine {
        Affine([
            matrix[0].into(),
            matrix[1].into(),
            matrix[2].into(),
            matrix[3].into(),
            matrix[4].into(),
            matrix[5].into(),
        ])
    }
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct Node {
    pub pt: Point,
    pub node_type: NodeType,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum NodeType {
    Line,
    LineSmooth,
    OffCurve,
    Curve,
    CurveSmooth,
}

#[derive(Clone, Debug, FromPlist, PartialEq, Eq, Hash)]
pub struct Component {
    pub name: String,
    pub transform: Option<Affine>,
    #[rest]
    pub other_stuff: BTreeMap<String, Plist>,
}

#[derive(Clone, Debug, FromPlist, PartialEq, Eq, Hash)]
pub struct Anchor {
    pub name: String,
    pub position: Point,
}

#[derive(Debug, FromPlist, PartialEq, Eq, Hash)]
pub struct FontMaster {
    pub id: String,
    pub axes_values: Option<Vec<OrderedFloat<f64>>>,
    #[rest]
    pub other_stuff: BTreeMap<String, Plist>,
}

impl Font {
    pub fn get_glyph(&self, glyphname: &str) -> Option<&Glyph> {
        self.glyphs.iter().find(|g| g.glyphname == glyphname)
    }

    pub fn get_glyph_mut(&mut self, glyphname: &str) -> Option<&mut Glyph> {
        self.glyphs.iter_mut().find(|g| g.glyphname == glyphname)
    }
}

impl Glyph {
    pub fn get_layer(&self, layer_id: &str) -> Option<&Layer> {
        self.layers.iter().find(|l| l.layer_id == layer_id)
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
                    panic!("Invalid node content {:?}", plist);
                };
                let x = value[0].as_f64().unwrap();
                let y = value[1].as_f64().unwrap();
                let pt = Point::new(x, y);
                let node_type = value[2].as_str().unwrap().parse().unwrap();
                Node { pt, node_type }
            }
            _ => {
                panic!("Invalid node content {:?}", plist);
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
            // Glyphs 3 style
            "l" => Ok(NodeType::Line),
            "ls" => Ok(NodeType::LineSmooth),
            "o" => Ok(NodeType::OffCurve),
            "c" => Ok(NodeType::Curve),
            "cs" => Ok(NodeType::CurveSmooth),
            _ => Err(format!("unknown node type {}", s)),
        }
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
        let raw = plist.as_str().unwrap();
        let raw = &raw[1..raw.len() - 1];
        let coords: Vec<f64> = raw.split(", ").map(|c| c.parse().unwrap()).collect();
        Point::new(coords[0], coords[1])
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

fn for_raw_glyphs(
    glyphs_file: &path::Path,
    root_dict: &mut BTreeMap<String, Plist>,
    callback: fn(&mut BTreeMap<String, Plist>) -> Result<(), Error>,
) -> Result<(), Error> {
    if !root_dict.contains_key("glyphs") {
        return Ok(());
    }
    let Plist::Array(glyphs) = root_dict.get_mut("glyphs").unwrap() else {
        return Err(Error::ParseError(glyphs_file.to_path_buf(), "Must have a glyphs array".to_string()));
    };
    for glyph in glyphs.iter_mut() {
        let Plist::Dictionary(glyph) = glyph else {
            return Err(Error::ParseError(glyphs_file.to_path_buf(), "Glyph must be a dict".to_string()));
        };
        callback(glyph)?;
    }
    Ok(())
}

fn fix_glyphs_named_infinity(
    glyphs_file: &path::Path,
    root_dict: &mut BTreeMap<String, Plist>,
) -> Result<(), Error> {
    for_raw_glyphs(glyphs_file, root_dict, |glyph| {
        if !glyph.contains_key("glyphname") {
            return Ok(());
        }
        if let Plist::Float(..) = glyph.get("glyphname").unwrap() {
            glyph.insert(
                "glyphname".to_string(),
                Plist::String("infinity".to_string()),
            );
        }
        Ok(())
    })
}

fn custom_params_mut(other_stuff: &mut BTreeMap<String, Plist>) -> Option<&mut Vec<Plist>> {
    let custom_params = other_stuff.get_mut("customParameters");
    custom_params.as_ref()?;
    let Some(Plist::Array(custom_params)) = custom_params else {
        warn!("customParameters isn't an array, omg omg omg");
        return None;
    };
    Some(custom_params)
}

fn custom_param_mut<'a>(
    other_stuff: &'a mut BTreeMap<String, Plist>,
    key: &str,
) -> Option<(usize, &'a mut Plist)> {
    let Some(custom_params) = custom_params_mut(other_stuff) else {
        warn!("customParameters isn't an array, omg omg omg");
        return None;
    };

    let name_key = "name".to_string();
    for (idx, custom_param) in custom_params.iter_mut().enumerate() {
        let Plist::Dictionary(dict) = custom_param else {
            continue;
        };
        let Some(Plist::String(param_key)) = dict.get(&name_key) else {
            continue;
        };
        if key == param_key {
            return Some((idx, custom_param));
        }
    }
    None
}

fn glyphs_v2_field_name_and_default(nth_axis: usize) -> Result<(&'static str, f64), Error> {
    // Per https://github.com/googlefonts/fontmake-rs/pull/42#pullrequestreview-1211619812
    // the field to use is based on the order in axes NOT the tag.
    // That is, whatever the first axis is, it's value is in the weightValue field. Long sigh.
    // Defaults per https://github.com/googlefonts/fontmake-rs/pull/42#discussion_r1044415236.
    Ok(match nth_axis {
        0 => ("weightValue", 100_f64),
        1 => ("widthValue", 100_f64),
        2 => ("customValue", 0_f64),
        _ => {
            return Err(Error::StructuralError(format!(
                "We don't know what field to use for axis {}",
                nth_axis
            )))
        }
    })
}

fn custom_params(other_stuff: &BTreeMap<String, Plist>) -> Option<&Vec<Plist>> {
    let custom_params = other_stuff.get("customParameters");
    custom_params.as_ref()?;
    let Some(Plist::Array(custom_params)) = custom_params else {
        warn!("customParameters isn't an array, omg omg omg");
        return None;
    };
    Some(custom_params)
}

fn custom_param<'a>(
    other_stuff: &'a BTreeMap<String, Plist>,
    key: &str,
) -> Option<(usize, &'a Plist)> {
    let Some(custom_params) = custom_params(other_stuff) else {
        warn!("customParameters isn't an array, omg omg omg");
        return None;
    };

    let name_key = "name".to_string();
    for (idx, custom_param) in custom_params.iter().enumerate() {
        let Plist::Dictionary(dict) = custom_param else {
            continue;
        };
        let Some(Plist::String(param_key)) = dict.get(&name_key) else {
            continue;
        };
        if key == param_key {
            return Some((idx, custom_param));
        }
    }
    None
}

impl Font {
    fn parse_codepoints(&mut self, radix: u32) {
        for glyph in self.glyphs.iter_mut() {
            if let Some(Plist::String(val)) = glyph.other_stuff.remove("unicode") {
                let codepoints: Vec<i64> = val
                    .split(',')
                    .into_iter()
                    .map(|v| i64::from_str_radix(v, radix).unwrap())
                    .collect();
                glyph.codepoints = Some(codepoints);
            };
        }
    }

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
                    return Err(Error::StructuralError("No value for Axes custom parameter".into()));
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
            let mut axis_values = Vec::new();
            for idx in 0..axes.len() {
                // Per https://github.com/googlefonts/fontmake-rs/pull/42#pullrequestreview-1211619812
                // the field to use is based on the order in axes NOT the tag.
                // That is, whatever the first axis is, it's value is in the weightValue field. Long sigh.
                let (field_name, default_value) = glyphs_v2_field_name_and_default(idx)?;
                let value = master
                    .other_stuff
                    .remove(field_name)
                    .unwrap_or_else(|| Plist::Float(default_value.into()))
                    .as_f64()
                    .ok_or_else(|| {
                        Error::StructuralError(format!(
                            "Invalid '{}' in\n{:#?}",
                            field_name, master.other_stuff
                        ))
                    })?;
                axis_values.push(value.into());
            }
            master.axes_values = Some(axis_values);
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
        let mut metrics = Vec::new();
        for name in V3_METRIC_NAMES {
            metrics.push(Plist::Dictionary(BTreeMap::from([(
                "type".to_string(),
                Plist::String(name.into()),
            )])));
        }
        self.other_stuff
            .insert("metrics".into(), Plist::Array(metrics));

        // in each font master setup the parallel array
        for master in self.font_master.iter_mut() {
            let mut metric_dicts = Vec::new();
            for v2_name in V2_METRIC_NAMES.iter() {
                let mut dict = BTreeMap::new();
                if let Some(Plist::Integer(value)) = master.other_stuff.remove(&v2_name.to_string())
                {
                    // leave blank for 0
                    if value != 0 {
                        dict.insert("pos".to_string(), Plist::Integer(value));
                    }
                }
                metric_dicts.push(dict);
            }
            // "alignmentZones is now a set of over (overshoot) properties attached to metrics"
            if let Some(Plist::Array(zones)) = master.other_stuff.get("alignmentZones") {
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
                        metric_dicts[idx].insert("over".into(), Plist::Integer(over));
                    }
                }
            }
            master.other_stuff.remove("alignmentZones");

            let mut metric_plists = Vec::new();
            for metric_dict in metric_dicts {
                metric_plists.push(Plist::Dictionary(metric_dict));
            }
            master
                .other_stuff
                .insert("metricValues".into(), Plist::Array(metric_plists));
        }
        Ok(())
    }

    fn v2_to_v3_shapes(&mut self) -> Result<(), Error> {
        // shapes in v3 encompasses paths and components in v2
        for glyph in self.glyphs.iter_mut() {
            // Paths and components combine in shapes
            // TODO components
            for layer in glyph.layers.iter_mut() {
                if let Some(paths) = layer.paths.take() {
                    match &mut layer.shapes {
                        Some(shapes) => {
                            shapes.extend(paths);
                        }
                        None => {
                            layer.shapes = Some(paths);
                        }
                    }
                }
            }
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
            master
                .other_stuff
                .insert("name".into(), Plist::String(name));
        }
        Ok(())
    }

    /// `<See https://github.com/schriftgestalt/GlyphsSDK/blob/Glyphs3/GlyphsFileFormat/GlyphsFileFormatv3.md#differences-between-version-2>`
    fn v2_to_v3(&mut self) -> Result<(), Error> {
        self.v2_to_v3_weight()?;
        self.v2_to_v3_axes()?;
        self.v2_to_v3_metrics()?;
        self.v2_to_v3_shapes()?;
        Ok(())
    }
}

impl Font {
    pub fn load(glyphs_file: &path::Path) -> Result<Font, Error> {
        let raw_content = fs::read_to_string(glyphs_file).map_err(Error::IoError)?;

        // Glyphs has a wide variety of unicode definitions, not all of them parser friendly
        // Make unicode always a string, without any wrapping () so we can parse as csv, radix based on format version
        let re = Regex::new(
            r#"(?m)^(?P<prefix>\s*unicode\s*=\s*)[(]?(?P<value>[0-9a-zA-Z,]+)[)]?;\s*$"#,
        )
        .unwrap();
        let raw_content = re.replace_all(&raw_content, r#"$prefix"$value";"#);

        let mut raw_content = Plist::parse(&raw_content)
            .map_err(|e| Error::ParseError(glyphs_file.to_path_buf(), format!("{:#?}", e)))?;

        // Fix any issues with the raw plist
        let Plist::Dictionary(ref mut root_dict) = raw_content else {
            return Err(Error::ParseError(glyphs_file.to_path_buf(), "Root must be a dict".to_string()));
        };
        fix_glyphs_named_infinity(glyphs_file, root_dict)?;

        // Try to migrate glyphs2 to glyphs3
        let mut font = Font::from_plist(raw_content);
        let mut radix = 10;
        if font.is_v2() {
            font.v2_to_v3()?;
            radix = 16;
        }
        font.parse_codepoints(radix);

        font.other_stuff.remove("date"); // exists purely to make diffs fail
        font.other_stuff.remove(".formatVersion"); // no longer relevent

        Ok(font)
    }
}

impl Font {
    pub fn default_master_idx(&self) -> usize {
        // https://github.com/googlefonts/fontmake-rs/issues/44
        custom_param(&self.other_stuff, "Variable Font Origin")
            .map(|(_, param)| match param {
                Plist::Dictionary(dict) => dict.get("value"),
                _ => None,
            })
            .map(|value| {
                let Some(Plist::String(origin)) = value else {
                    warn!("Incomprehensible Variable Font Origin");
                    return 0;
                };
                self.font_master
                    .iter()
                    .enumerate()
                    .find(|(_, master)| &master.id == origin)
                    .map(|(idx, _)| idx)
                    .unwrap_or_default()
            })
            .unwrap_or_default()
    }
}

#[cfg(test)]
mod tests {
    use std::path::{Path, PathBuf};

    use crate::{Font, FromPlist, Node, Plist};

    use pretty_assertions::assert_eq;

    fn testdata_dir() -> PathBuf {
        let dir = Path::new("../resources/testdata");
        assert!(dir.is_dir());
        dir.to_path_buf()
    }

    fn glyphs2_dir() -> PathBuf {
        testdata_dir().join("glyphs2")
    }

    fn glyphs3_dir() -> PathBuf {
        testdata_dir().join("glyphs3")
    }

    #[test]
    fn test_glyphs3_node() {
        assert_eq!(
            Node {
                node_type: crate::NodeType::Line,
                pt: super::Point {
                    x: 354.0.into(),
                    y: 183.0.into()
                }
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
                pt: super::Point {
                    x: 354.0.into(),
                    y: 183.0.into()
                }
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

    #[test]
    fn read_2_and_3() {
        let g2 = Font::load(&glyphs2_dir().join("WghtVar.glyphs")).unwrap();
        let mut g3 = Font::load(&glyphs3_dir().join("WghtVar.glyphs")).unwrap();

        // for test purposes we are nto interested in icon name
        for master in g3.font_master.iter_mut() {
            master.other_stuff.remove("iconName");
        }

        assert_eq!(g2, g3);
    }

    #[test]
    fn upgrade_2_to_3_with_implicit_axes() {
        let font = Font::load(&glyphs2_dir().join("WghtVar_ImplicitAxes.glyphs")).unwrap();
        assert_eq!(
            font.axes
                .unwrap()
                .iter()
                .map(|a| a.tag.as_str())
                .collect::<Vec<&str>>(),
            vec!["wght", "wdth", "XXXX"]
        );
    }

    #[test]
    fn understand_v2_style_unquoted_hex_unicode() {
        let font = Font::load(&glyphs2_dir().join("Unicode-UnquotedHex.glyphs")).unwrap();
        assert_eq!(Some(vec![0x1234_i64]), font.glyphs[0].codepoints);
        assert_eq!(1, font.glyphs.len());
    }

    #[test]
    fn understand_v2_style_quoted_hex_unicode_sequence() {
        let font = Font::load(&glyphs2_dir().join("Unicode-QuotedHexSequence.glyphs")).unwrap();
        assert_eq!(
            Some(vec![0x2044_i64, 0x200D_i64, 0x2215_i64]),
            font.glyphs[0].codepoints
        );
        assert_eq!(1, font.glyphs.len());
    }

    #[test]
    fn understand_v3_style_unquoted_decimal_unicode() {
        let font = Font::load(&glyphs3_dir().join("Unicode-UnquotedDec.glyphs")).unwrap();
        assert_eq!(Some(vec![182_i64]), font.glyphs[0].codepoints);
        assert_eq!(1, font.glyphs.len());
    }

    #[test]
    fn understand_v3_style_unquoted_decimal_unicode_sequence() {
        let font = Font::load(&glyphs3_dir().join("Unicode-UnquotedDecSequence.glyphs")).unwrap();
        assert_eq!(Some(vec![1619_i64, 1764_i64]), font.glyphs[0].codepoints);
        assert_eq!(1, font.glyphs.len());
    }

    #[test]
    fn axes_not_hidden() {
        let font = Font::load(&glyphs3_dir().join("WghtVar.glyphs")).unwrap();
        assert_eq!(
            font.axes
                .unwrap()
                .iter()
                .map(|a| a.hidden)
                .collect::<Vec<_>>(),
            vec![None]
        );
    }

    #[test]
    fn axis_hidden() {
        let font = Font::load(&glyphs3_dir().join("WghtVar_3master_CustomOrigin.glyphs")).unwrap();
        assert_eq!(
            font.axes
                .unwrap()
                .iter()
                .map(|a| a.hidden)
                .collect::<Vec<_>>(),
            vec![Some(true)]
        );
    }

    #[test]
    fn vf_origin_single_axis_default() {
        let font = Font::load(&glyphs3_dir().join("WghtVar.glyphs")).unwrap();
        assert_eq!(0, font.default_master_idx());
    }

    #[test]
    fn vf_origin_multi_axis_default() {
        let font = Font::load(&glyphs2_dir().join("WghtVar_ImplicitAxes.glyphs")).unwrap();
        assert_eq!(0, font.default_master_idx());
    }

    #[test]
    fn vf_origin_multi_axis_custom() {
        let font = Font::load(&glyphs3_dir().join("WghtVar_3master_CustomOrigin.glyphs")).unwrap();
        assert_eq!(2, font.default_master_idx());
    }
}
