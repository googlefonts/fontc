//! The general strategy is just to use a plist for storage. Also, lots of
//! unwrapping.
//!
//! There are lots of other ways this could go, including something serde-like
//! where it gets serialized to more Rust-native structures, proc macros, etc.

use std::collections::BTreeMap;
use std::hash::Hash;
use std::{fs, path};

use ordered_float::OrderedFloat;

use crate::error::Error;
use crate::from_plist::FromPlist;
use crate::plist::Plist;
use crate::to_plist::ToPlist;

#[derive(Debug, FromPlist, ToPlist, PartialEq, Hash)]
pub struct Font {
    pub family_name: String,
    pub axes: Option<Vec<Axis>>,
    pub glyphs: Vec<Glyph>,
    pub font_master: Vec<FontMaster>,
    #[rest]
    pub other_stuff: BTreeMap<String, Plist>,
}

#[derive(Clone, Debug, FromPlist, ToPlist, PartialEq, Hash)]
pub struct Axis {
    pub name: String,
    pub tag: String,
}

#[derive(Clone, Debug, FromPlist, ToPlist, PartialEq, Hash)]
pub struct Glyph {
    pub layers: Vec<Layer>,
    pub glyphname: String,
    #[rest]
    pub other_stuff: BTreeMap<String, Plist>,
}

#[derive(Clone, Debug, FromPlist, ToPlist, PartialEq, Hash)]
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

#[derive(Clone, Debug, FromPlist, ToPlist, PartialEq, Hash)]
pub struct Path {
    pub closed: bool,
    pub nodes: Vec<Node>,
}

// We do not use kurbo's point because it does not hash
#[derive(Clone, Debug, PartialEq, Hash)]
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
#[derive(Clone, Debug, PartialEq, Hash)]
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

#[derive(Clone, Debug, PartialEq, Hash)]
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

#[derive(Clone, Debug, FromPlist, ToPlist, PartialEq, Hash)]
pub struct Component {
    pub name: String,
    pub transform: Option<Affine>,
    #[rest]
    pub other_stuff: BTreeMap<String, Plist>,
}

#[derive(Clone, Debug, FromPlist, ToPlist, PartialEq, Hash)]
pub struct Anchor {
    pub name: String,
    pub position: Point,
}

#[derive(Debug, FromPlist, ToPlist, PartialEq, Hash)]
pub struct FontMaster {
    pub id: String,
    #[rest]
    pub other_stuff: BTreeMap<String, Plist>,
}

impl Font {
    pub fn load(path: &std::path::Path) -> Result<Font, String> {
        let contents = std::fs::read_to_string(path).map_err(|e| format!("{:?}", e))?;
        let plist = Plist::parse(&contents).map_err(|e| format!("{:?}", e))?;
        Ok(FromPlist::from_plist(plist))
    }

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

impl NodeType {
    fn glyphs_str(&self) -> &'static str {
        match self {
            NodeType::Line => "LINE",
            NodeType::LineSmooth => "LINE SMOOTH",
            NodeType::OffCurve => "OFFCURVE",
            NodeType::Curve => "CURVE",
            NodeType::CurveSmooth => "CURVE SMOOTH",
        }
    }
}

impl ToPlist for Node {
    fn to_plist(self) -> Plist {
        format!(
            "{} {} {}",
            self.pt.x,
            self.pt.y,
            self.node_type.glyphs_str()
        )
        .into()
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

impl ToPlist for Affine {
    fn to_plist(self) -> Plist {
        let c = self.0;
        format!(
            "{{{}, {}, {}, {}, {}, {}}}",
            c[0], c[1], c[2], c[3], c[4], c[5]
        )
        .into()
    }
}

impl FromPlist for Point {
    fn from_plist(plist: Plist) -> Self {
        eprintln!("{:?}", plist);
        let raw = plist.as_str().unwrap();
        let raw = &raw[1..raw.len() - 1];
        let coords: Vec<f64> = raw.split(", ").map(|c| c.parse().unwrap()).collect();
        Point::new(coords[0], coords[1])
    }
}

impl ToPlist for Point {
    fn to_plist(self) -> Plist {
        format!("{{{}, {}}}", self.x, self.y).into()
    }
}

impl FromPlist for OrderedFloat<f64> {
    fn from_plist(plist: Plist) -> Self {
        plist.as_f64().unwrap().into()
    }
}

impl ToPlist for OrderedFloat<f64> {
    fn to_plist(self) -> Plist {
        Plist::Float(self)
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

fn custom_param<'a>(other_stuff: &'a mut BTreeMap<String, Plist>, key: &str) -> Option<&'a mut Plist> {
    let custom_params = other_stuff.get_mut("customParameters");
    if custom_params.is_none() {
        return None;
    }
    let Some(Plist::Array(custom_params)) = custom_params else {
        panic!("customParameters isn't an array, omg omg omg");
    };
    
    None
}

impl Font {
    fn is_v2(&self) -> bool {
        let mut is_v2 = true;
        if let Some(Plist::Integer(version)) = self.other_stuff.get(".formatVersion") {
            is_v2 = *version < 3; // .formatVersion is only present for v3+
        }
        is_v2
    }

    fn v2_to_v3_axes(&mut self) -> Result<Vec<String>, Error> {
        let mut tags = Vec::new();
        if let Some(Plist::Array(custom_params)) = self.other_stuff.get_mut("customParameters") {
            // Axes migrates from customParameters
            let mut axes_idx = None;
            for (idx, custom_param) in custom_params.iter_mut().enumerate() {
                if let Plist::Dictionary(dict) = custom_param {
                    if Some(&Plist::String("Axes".to_string())) == dict.get("name") {
                        axes_idx = Some(idx);

                        if self.axes.is_none() {
                            self.axes = Some(Vec::new());
                        }
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
                            });
                        }
                    }
                }
            }
            axes_idx.map(|idx| custom_params.remove(idx));

            if custom_params.is_empty() {
                self.other_stuff.remove("customParameters");
            }            
        }
        Ok(tags)
    }

    fn v2_to_v3_metrics(&mut self) -> Result<(), Error> {
        // metrics are in parallel arrays in v3
        let v3_metric_names = ["ascender".to_string(), "baseline".to_string(), "descender".to_string(), "cap height".to_string(), "x-height".to_string()];
        let v2_metric_names = ["ascender".to_string(), "baseline".to_string(), "descender".to_string(), "capHeight".to_string(), "xHeight".to_string()];
        assert!(v2_metric_names.len() == v3_metric_names.len());

        // setup root storage for the basic metrics
        let mut metrics = Vec::new();
        for name in v3_metric_names {
            metrics.push(Plist::Dictionary(BTreeMap::from([("type".to_string(), Plist::String(name.clone()))])));
        }        
        self.other_stuff.insert("metrics".into(), Plist::Array(metrics));

        // in each font master setup the parallel array
        for master in self.font_master.iter_mut() {
            let mut metric_values = Vec::new();
            for v2_name in v2_metric_names.iter() {
                let mut dict = BTreeMap::new();
                if let Some(Plist::Integer(value)) = master.other_stuff.remove(v2_name) {
                    // leave blank for 0
                    if value != 0 {
                        dict.insert("pos".to_string(), Plist::Integer(value));
                    }                    
                }
                metric_values.push(Plist::Dictionary(dict));
            }
            master.other_stuff.insert("metricValues".into(), Plist::Array(metric_values));
        }
        Ok(())
    }

    fn v2_to_v3_shapes(&mut self) -> Result<(), Error> {
        // shapes in v3 encompasses paths and components in v2
        for glyph in self.glyphs.iter_mut() {
            // v2 uses single codepoint strings, turn into int to match v3 for now
            // In time we will likely parse unicode more carefully
            glyph.other_stuff.entry("unicode".into()).and_modify(|v| {
                if let Plist::String(val) = v {
                    *v = Plist::Integer(i64::from_str_radix(val, 16).unwrap())
                }
            });

            // Paths and components combine in shapes
            // TODO components
            for layer in glyph.layers.iter_mut() {
                let paths = layer.paths.take();
                if paths.is_some() {
                    match &mut layer.shapes {
                        Some(shapes) => {
                            shapes.extend(paths.unwrap());
                        }
                        None => {
                            layer.shapes = paths;
                        }
                    }
                }
            }
        }
        Ok(())
    }

    fn v2_to_v3_weight(&mut self, tags: &Vec<String>) -> Result<(), Error> {
        if tags.len() != 1 || tags[0] != "wght" {
            return Err(Error::StructuralError("Only wght supported so far".into()));
        }
        for master in self.font_master.iter_mut() {
            let Some(Plist::Integer(wght)) = master.other_stuff.remove("weightValue") else {
                continue;
            };
            master.other_stuff.insert("axesValues".into(), Plist::Array(vec![Plist::Integer(wght)]));

            let name = match master.other_stuff.remove("weight") {
                Some(Plist::String(name)) => name,
                _ => String::from("Regular"),  // Missing = default = Regular per @anthrotype
            };
            master.other_stuff.insert("name".into(), Plist::String(name));
        }        
        Ok(())
    }

    /// See https://github.com/schriftgestalt/GlyphsSDK/blob/Glyphs3/GlyphsFileFormat/GlyphsFileFormatv3.md#differences-between-version-2
    fn v2_to_v3(&mut self) -> Result<(), Error> {        
        let tags = self.v2_to_v3_axes()?;
        self.v2_to_v3_weight(&tags)?;
        self.v2_to_v3_metrics()?;
        self.v2_to_v3_shapes()?;
        Ok(())
    }
}

impl Font {
    pub fn read_glyphs_file(glyphs_file: &path::Path) -> Result<Font, Error> {
        let raw_content = fs::read_to_string(glyphs_file).map_err(Error::IoError)?;
        let mut raw_content = Plist::parse(&raw_content)
            .map_err(|e| Error::ParseError(glyphs_file.to_path_buf(), format!("{:#?}", e)))?;

        // Fix any issues with the raw plist
        let Plist::Dictionary(ref mut root_dict) = raw_content else {
            return Err(Error::ParseError(glyphs_file.to_path_buf(), "Root must be a dict".to_string()));
        };
        fix_glyphs_named_infinity(glyphs_file, root_dict)?;

        // Try to migrate glyphs2 to glyphs3
        let mut font = Font::from_plist(raw_content);
        if font.is_v2() {
            font.v2_to_v3()?
        }

        font.other_stuff.remove("date");  // exists purely to make diffs fail
        font.other_stuff.remove(".formatVersion");  // no longer relevent

        Ok(font)
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
        Font::read_glyphs_file(&glyphs3_dir().join("infinity.glyphs")).unwrap();
    }

    #[test]
    fn read_2_and_3() {
        let g2 = Font::read_glyphs_file(&glyphs2_dir().join("WghtVar.glyphs")).unwrap();
        let mut g3 = Font::read_glyphs_file(&glyphs3_dir().join("WghtVar.glyphs")).unwrap();

        // for test purposes we are nto interested in icon name
        for master in g3.font_master.iter_mut() {
            master.other_stuff.remove("iconName");
        }

        assert_eq!(g2, g3);
    }
}
