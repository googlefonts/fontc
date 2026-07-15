//! Parse .fontra json filesets into structs
//!
//! See <https://github.com/fontra/fontra/blob/469a001f8/src/fontra/core/classes.py>

#![allow(dead_code)] // TEMPORARY

use std::{
    borrow::Cow,
    collections::{BTreeMap, HashMap},
    fs::{self, File},
    io::{BufRead, BufReader},
    path::{self, PathBuf},
};

use fontdrasil::{paths::string_to_filename, types::GlyphName};
use fontir::error::{BadSource, BadSourceKind, Error, PathConversionError};
use serde::Deserialize;
use write_fonts::types::Tag;

pub(crate) type AxisName = String;
pub(crate) type LayerName = String;
pub(crate) type GlyphMap = BTreeMap<GlyphName, Vec<u32>>;
pub(crate) type GlyphInfos = BTreeMap<GlyphName, GlyphInfo>;

/// Corresponds to a Fontra Location
/// <https://github.com/fontra/fontra/blob/469a001f8/src/fontra/core/classes.py#L318>
pub(crate) type Location = HashMap<AxisName, f64>;

pub(crate) fn glyph_file(glyph_dir: &path::Path, glyph: GlyphName) -> PathBuf {
    glyph_dir.join(string_to_filename(glyph.as_str(), ".json"))
}

fn from_file<T>(p: &path::Path) -> Result<T, BadSource>
where
    for<'a> T: Deserialize<'a>,
{
    let raw = fs::read_to_string(p).map_err(|e| BadSource::new(p, e))?;
    serde_json::from_str(&raw).map_err(|e| BadSource::custom(p, e))
}

/// An entry of glyph-info.csv
#[derive(Debug, Clone, Default, PartialEq, Deserialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct GlyphInfo {
    pub(crate) category: Option<String>,
    pub(crate) sub_category: Option<String>,
}

/// Corresponds to a Fontra FontInfo
/// <https://github.com/fontra/fontra/blob/469a001f8/src/fontra/core/classes.py#L17>
#[derive(Debug, Clone, Default, Deserialize)]
#[serde(default, rename_all = "camelCase")]
pub(crate) struct FontInfo {
    pub(crate) family_name: Option<String>,
    pub(crate) version_major: Option<i32>,
    pub(crate) version_minor: Option<i32>,
    pub(crate) copyright: Option<String>,
    pub(crate) trademark: Option<String>,
    pub(crate) description: Option<String>,
    pub(crate) sample_text: Option<String>,
    pub(crate) designer: Option<String>,
    #[serde(rename = "designerURL")]
    pub(crate) designer_url: Option<String>,
    pub(crate) manufacturer: Option<String>,
    #[serde(rename = "manufacturerURL")]
    pub(crate) manufacturer_url: Option<String>,
    pub(crate) license_description: Option<String>,
    #[serde(rename = "licenseInfoURL")]
    pub(crate) license_info_url: Option<String>,
    #[serde(rename = "vendorID")]
    pub(crate) vendor_id: Option<String>,
}

/// Corresponds to a Fontra Axes
/// <https://github.com/fontra/fontra/blob/469a001f8/src/fontra/core/classes.py#L36>
#[derive(Debug, Clone, Default, Deserialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct Axes {
    #[serde(default)]
    pub(crate) axes: Vec<Axis>,
    #[serde(default)]
    pub(crate) mappings: Vec<CrossAxisMapping>,
    #[serde(rename = "elidedFallBackname")]
    pub(crate) elided_fallback_name: Option<String>,
}

/// Corresponds to a Fontra CrossAxisMapping
/// <https://github.com/fontra/fontra/blob/469a001f8/src/fontra/core/classes.py#L44>
#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct CrossAxisMapping {
    pub(crate) description: Option<String>,
    pub(crate) group_description: Option<String>,
    pub(crate) input_location: Location,
    pub(crate) output_location: Location,
    #[serde(default)]
    pub(crate) inactive: bool,
}

/// Corresponds to a Fontra OpenTypeFeatures
/// <https://github.com/fontra/fontra/blob/469a001f8/src/fontra/core/classes.py#L59>
#[derive(Debug, Clone, Deserialize)]
#[serde(default, rename_all = "camelCase")]
pub(crate) struct OpenTypeFeatures {
    pub(crate) language: String,
    pub(crate) text: String,
}

impl Default for OpenTypeFeatures {
    fn default() -> Self {
        Self {
            language: "fea".to_string(),
            text: String::new(),
        }
    }
}

/// Corresponds to a Fontra SubstitutionCondition
/// <https://github.com/fontra/fontra/blob/469a001f8/src/fontra/core/classes.py#L66>
#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct SubstitutionCondition {
    pub(crate) name: String,
    pub(crate) min_value: Option<f64>,
    pub(crate) max_value: Option<f64>,
}

/// Corresponds to a Fontra SubstitutionConditionSet
/// <https://github.com/fontra/fontra/blob/469a001f8/src/fontra/core/classes.py#L73>
#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct SubstitutionConditionSet {
    #[serde(default)]
    pub(crate) conditions: Vec<SubstitutionCondition>,
}

/// Corresponds to a Fontra SubstitionRule
/// <https://github.com/fontra/fontra/blob/469a001f8/src/fontra/core/classes.py#L78>
#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct SubstitutionRule {
    pub(crate) name: Option<String>,
    pub(crate) condition_sets: Vec<SubstitutionConditionSet>,
    pub(crate) substitutions: HashMap<String, String>,
}

/// Corresponds to a Fontra ConditionalSubstitutions
/// <https://github.com/fontra/fontra/blob/469a001f8/src/fontra/core/classes.py#L85>
#[derive(Debug, Clone, Deserialize)]
#[serde(default, rename_all = "camelCase")]
pub(crate) struct ConditionalSubstitutions {
    pub(crate) feature_tags: Vec<String>,
    pub(crate) rules: Vec<SubstitutionRule>,
}

impl Default for ConditionalSubstitutions {
    fn default() -> Self {
        Self {
            feature_tags: vec!["rclt".to_string()],
            rules: Vec::new(),
        }
    }
}

/// Corresponds to a Fontra Kerning
/// <https://github.com/fontra/fontra/blob/469a001f8/src/fontra/core/classes.py#L93>
#[derive(Debug, Clone, Default, Deserialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct Kerning {
    pub(crate) groups_side1: HashMap<String, Vec<String>>,
    pub(crate) groups_side2: HashMap<String, Vec<String>>,
    pub(crate) source_identifiers: Vec<String>,
    /// left glyph/group -> right glyph/group -> source index -> value
    pub(crate) values: HashMap<String, HashMap<String, Vec<Option<f64>>>>,
}

/// Corresponds to a Fontra Font
/// <https://github.com/fontra/fontra/blob/469a001f8/src/fontra/core/classes.py#L102>
#[derive(Debug, Clone, Default, Deserialize)]
#[serde(default, rename_all = "camelCase")]
pub(crate) struct Font {
    #[serde(default = "default_units_per_em")]
    pub(crate) units_per_em: u16,
    pub(crate) font_info: FontInfo,
    #[serde(skip)]
    pub(crate) glyphs: BTreeMap<GlyphName, VariableGlyph>,
    #[serde(skip)]
    pub(crate) glyph_map: GlyphMap,
    #[serde(skip)]
    pub(crate) glyph_infos: GlyphInfos, // In Fontra this is a CustomData
    pub(crate) axes: Axes,
    pub(crate) sources: BTreeMap<String, FontSource>,
    #[serde(skip)]
    pub(crate) kerning: BTreeMap<String, Kerning>,
    pub(crate) features: OpenTypeFeatures,
    pub(crate) conditional_substitutions: ConditionalSubstitutions,
}

impl Font {
    pub(crate) fn from_file(p: &path::Path) -> Result<Self, BadSource> {
        from_file(p)
    }
}

pub(crate) fn parse_glyph_info(
    fontra_dir: &path::Path,
) -> Result<BTreeMap<GlyphName, (PathBuf, Vec<u32>)>, Error> {
    let glyphinfo_file = fontra_dir.join("glyph-info.csv");
    if !glyphinfo_file.is_file() {
        return Err(BadSource::new(glyphinfo_file, BadSourceKind::ExpectedFile).into());
    }

    // Read the glyph-info file
    let file = File::open(&glyphinfo_file).map_err(|e| BadSource::new(&glyphinfo_file, e))?;

    let glyph_dir = fontra_dir.join("glyphs");
    if !glyph_dir.is_dir() {
        return Err(BadSource::new(glyph_dir, BadSourceKind::ExpectedDirectory).into());
    }

    // Example files suggest the first line is just the column headers. Hopefully always :)
    // This file is tool generated so it shouldn't be full of human error. Fail if we don't understand.
    let mut glyph_info = BTreeMap::default();
    for (i, line) in BufReader::new(file).lines().enumerate().skip(1) {
        let line = line.map_err(|e| BadSource::new(&glyphinfo_file, BadSourceKind::Io(e)))?;
        let parts: Vec<_> = line.split(';').collect();
        if parts.len() != 2 {
            return Err(BadSource::custom(
                &glyphinfo_file,
                format!("Expected two parts in line {i} separated by ;"),
            )
            .into());
        }
        let glyph_name = GlyphName::new(parts[0].trim());
        let codepoints = parts[1]
            .split(',')
            .filter_map(|codepoint| {
                let codepoint = codepoint.trim();
                if codepoint.is_empty() {
                    return None;
                }
                let Some(codepoint) = codepoint.strip_prefix("U+") else {
                    return Some(Err(BadSource::custom(
                        &glyphinfo_file,
                        format!("Unintelligible codepoint {codepoint:?} at line {i}"),
                    )));
                };
                Some(u32::from_str_radix(codepoint, 16).map_err(|e| {
                    BadSource::custom(
                        &glyphinfo_file,
                        format!("Unintelligible codepoint {codepoint:?} at line {i}: {e}"),
                    )
                }))
            })
            .collect::<Result<Vec<_>, _>>()?;
        let glyph_file = glyph_file(&glyph_dir, glyph_name.clone());
        if !glyph_file.is_file() {
            return Err(BadSource::new(glyph_file, BadSourceKind::ExpectedFile).into());
        }

        if glyph_info
            .insert(glyph_name.clone(), (glyph_file, codepoints))
            .is_some()
        {
            return Err(BadSource::custom(
                &glyphinfo_file,
                format!("Multiple definitions of '{glyph_name}'"),
            )
            .into());
        }
    }

    Ok(glyph_info)
}

/// Corresponds to a Fontra FontSource
/// <https://github.com/fontra/fontra/blob/469a001f8/src/fontra/core/classes.py#L128>
#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct FontSource {
    pub(crate) name: String,
    #[serde(default)]
    pub(crate) is_sparse: bool,
    #[serde(default)]
    pub(crate) location: Location,
    #[serde(default)]
    pub(crate) line_metrics_horizontal_layout: HashMap<String, LineMetric>,
    #[serde(default)]
    pub(crate) line_metrics_vertical_layout: HashMap<String, LineMetric>,
    #[serde(default)]
    pub(crate) italic_angle: f64,
    // guidelines
}

/// Corresponds to a Fontra LineMetric
/// <https://github.com/fontra/fontra/blob/469a001f8/src/fontra/core/classes.py#L140>
#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct LineMetric {
    pub(crate) value: f64,
    #[serde(default)]
    pub(crate) zone: f64,
}

/// Corresponds to a Fontra AxisValueLabel
/// <https://github.com/fontra/fontra/blob/469a001f8/src/fontra/core/classes.py#L157>
#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct AxisValueLabel {
    pub(crate) name: String,
    pub(crate) value: f64,
    pub(crate) min_value: Option<f64>,
    pub(crate) max_value: Option<f64>,
    pub(crate) linked_value: Option<f64>,
    #[serde(default)]
    pub(crate) elidable: bool,
    #[serde(default)]
    pub(crate) older_sibling: bool,
}

#[derive(Debug, Clone, Deserialize)]
#[serde(untagged)]
pub(crate) enum Axis {
    Continuous(FontAxis),
    Discrete(DiscreteFontAxis),
}

impl Axis {
    pub(crate) fn name(&self) -> &AxisName {
        match self {
            Axis::Continuous(a) => &a.name,
            Axis::Discrete(a) => &a.name,
        }
    }

    pub(crate) fn tag(&self) -> Tag {
        match self {
            Axis::Continuous(a) => a.tag,
            Axis::Discrete(a) => a.tag,
        }
    }

    pub(crate) fn default_value(&self) -> f64 {
        match self {
            Axis::Continuous(a) => a.default_value,
            Axis::Discrete(a) => a.default_value,
        }
    }

    pub(crate) fn hidden(&self) -> bool {
        match self {
            Axis::Continuous(a) => a.hidden,
            Axis::Discrete(a) => a.hidden,
        }
    }

    /// Pairs of [user, design] defining a pairwise linear map
    pub(crate) fn mapping(&self) -> &Vec<[f64; 2]> {
        match self {
            Axis::Continuous(a) => &a.mapping,
            Axis::Discrete(a) => &a.mapping,
        }
    }
}

/// Corresponds to a Fontra FontAxis
/// <https://github.com/fontra/fontra/blob/469a001f8/src/fontra/core/classes.py#L168>
#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct FontAxis {
    pub(crate) name: AxisName,
    pub(crate) label: String,
    pub(crate) tag: Tag,
    pub(crate) min_value: f64,
    pub(crate) default_value: f64,
    pub(crate) max_value: f64,
    #[serde(default)]
    pub(crate) mapping: Vec<[f64; 2]>,
    #[serde(default)]
    pub(crate) value_labels: Vec<AxisValueLabel>,
    #[serde(default)]
    pub(crate) hidden: bool,
}

/// Corresponds to a Fontra DiscreteFontAxis
/// <https://github.com/fontra/fontra/blob/469a001f8/src/fontra/core/classes.py#L182>
#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct DiscreteFontAxis {
    pub(crate) name: AxisName,
    pub(crate) label: String,
    pub(crate) tag: Tag,
    pub(crate) values: Vec<f64>,
    pub(crate) default_value: f64,
    #[serde(default)]
    pub(crate) mapping: Vec<[f64; 2]>,
    #[serde(default)]
    pub(crate) value_labels: Vec<AxisValueLabel>,
    #[serde(default)]
    pub(crate) hidden: bool,
}

/// An axis specific to a glyph meant to be used as a variable component
/// Corresponds to a Fontra GlyphAxis
/// <https://github.com/fontra/fontra/blob/469a001f8/src/fontra/core/classes.py#L195>
#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct GlyphAxis {
    pub(crate) name: String,
    pub(crate) min_value: f64,
    pub(crate) default_value: f64,
    pub(crate) max_value: f64,
}

/// Corresponds to a Fontra VariableGlyph
/// <https://github.com/fontra/fontra/blob/469a001f8/src/fontra/core/classes.py#L204>
#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct VariableGlyph {
    pub(crate) name: GlyphName,
    /// Variable component, or glyph-local, axes
    #[serde(default)]
    pub(crate) axes: Vec<GlyphAxis>,
    #[serde(default)]
    pub(crate) sources: Vec<GlyphSource>,
    #[serde(default)]
    pub(crate) layers: BTreeMap<LayerName, Layer>,
}

impl VariableGlyph {
    pub(crate) fn from_file(p: &path::Path) -> Result<Self, BadSource> {
        from_file(p)
    }
}

/// Corresponds to a Fontra GlyphSource
/// <https://github.com/fontra/fontra/blob/469a001f8/src/fontra/core/classes.py#L219>
#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct GlyphSource {
    pub(crate) name: String,
    pub(crate) layer_name: LayerName,
    #[serde(default)]
    pub(crate) location: Location,
    pub(crate) location_base: Option<String>,
    #[serde(default)]
    pub(crate) inactive: bool,
}

/// Corresponds to a Fontra Layer
/// <https://github.com/fontra/fontra/blob/469a001f8/src/fontra/core/classes.py#L229>
#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct Layer {
    pub(crate) glyph: StaticGlyph,
}

/// Corresponds to a Fontra StaticGlyph
/// <https://github.com/fontra/fontra/blob/469a001f8/src/fontra/core/classes.py#L280>
#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct StaticGlyph {
    #[serde(default)]
    pub(crate) path: Path,
    #[serde(default)]
    pub(crate) components: Vec<Component>,
    pub(crate) x_advance: f64,
    pub(crate) y_advance: Option<f64>,
    pub(crate) vertical_origin: Option<f64>,
    #[serde(default)]
    pub(crate) anchors: Vec<Anchor>,
    // guidelines
    // background_image
}

/// Corresponds to a Fontra Component
/// <https://github.com/fontra/fontra/blob/469a001f8/src/fontra/core/classes.py#L303>
#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct Component {
    pub(crate) name: GlyphName,
    #[serde(default)]
    pub(crate) transformation: DecomposedTransform,
    // This location is in terms of axes defined by the referenced glyph
    #[serde(default)]
    pub(crate) location: Location,
}

/// Corresponds to a Fontra Anchor
/// <https://github.com/fontra/fontra/blob/469a001f8/src/fontra/core/classes.py#L311>
#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct Anchor {
    pub(crate) name: Option<String>,
    pub(crate) x: f64,
    pub(crate) y: f64,
}

/// Corresponds to a Fontra Point
/// <https://github.com/fontra/fontra/blob/469a001f8/src/fontra/core/path.py#L22>
#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct Point {
    pub(crate) x: f64,
    pub(crate) y: f64,
    #[serde(rename = "type")]
    raw_type: Option<String>,
    #[serde(default)]
    pub(crate) smooth: bool,
    // attrs
}

impl Point {
    /// <https://github.com/fontra/fontra/blob/469a001f8/src/fontra/core/path.py#L583>
    pub(crate) fn point_type(&self) -> Result<PointType, PathConversionError> {
        match (self.smooth, self.raw_type.as_deref()) {
            (false, Some("cubic")) => Ok(PointType::OffCurveCubic),
            (false, Some("quad")) => Ok(PointType::OffCurveQuad),
            (false, None) => Ok(PointType::OnCurve),
            (true, None) => Ok(PointType::OnCurveSmooth),
            _ => Err(PathConversionError::Parse(format!(
                "Unrecognized combination, smooth {}, type '{}'",
                self.smooth,
                self.raw_type.clone().unwrap_or_default()
            ))),
        }
    }
}

/// Corresponds to a Fontra Contour
/// <https://github.com/fontra/fontra/blob/469a001f8/src/fontra/core/path.py#L31>
#[derive(Default, Debug, Clone, Deserialize)]
#[serde(default, rename_all = "camelCase")]
pub(crate) struct Contour {
    pub(crate) points: Vec<Point>,
    pub(crate) is_closed: bool,
}

/// Corresponds to a Fontra Path
/// <https://github.com/fontra/fontra/blob/469a001f8/src/fontra/core/path.py#L37>
#[derive(Default, Debug, Clone, Deserialize)]
#[serde(default, rename_all = "camelCase")]
pub(crate) struct UnpackedPath {
    pub(crate) contours: Vec<Contour>,
}

/// Corresponds to a Fontra ContourInfo
/// <https://github.com/fontra/fontra/blob/469a001f8/src/fontra/core/path.py#L62>
#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct ContourInfo {
    pub(crate) end_point: usize,
    #[serde(default)]
    pub(crate) is_closed: bool,
}

/// Corresponds to a Fontra PointType
/// <https://github.com/fontra/fontra/blob/469a001f8/src/fontra/core/path.py#L67>
#[derive(Debug, Clone, Copy, Default, PartialEq)]
pub(crate) enum PointType {
    #[default]
    OnCurve = 0x00,
    OffCurveQuad = 0x01,
    OffCurveCubic = 0x02,
    OnCurveSmooth = 0x08,
}

impl PointType {
    pub(crate) fn is_off_curve(&self) -> bool {
        match self {
            PointType::OffCurveCubic | PointType::OffCurveQuad => true,
            PointType::OnCurve | PointType::OnCurveSmooth => false,
        }
    }
}

/// Corresponds to a Fontra PackedPath
/// <https://github.com/fontra/fontra/blob/469a001f8/src/fontra/core/path.py#L75>
#[derive(Debug, Clone, Default, Deserialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct PackedPath {
    #[serde(default)]
    pub(crate) coordinates: Vec<f64>,
    pub(crate) point_types: Vec<u8>,
    #[serde(default)]
    pub(crate) contour_info: Vec<ContourInfo>,
    // point_attributes
}

impl PackedPath {
    // https://github.com/fontra/fontra/blob/469a001f8/src/fontra/core/path.py#L168
    pub(crate) fn unpacked_contours(&self) -> Vec<Contour> {
        let mut contours = Vec::with_capacity(self.contour_info.len());
        let mut start = 0;
        for info in &self.contour_info {
            let points = (start..=info.end_point)
                .map(|i| {
                    // https://github.com/fontra/fontra/blob/469a001f8/src/fontra/core/path.py#L548
                    let (raw_type, smooth) =
                        match self.point_types.get(i).copied().unwrap_or_default() {
                            t if t == PointType::OffCurveQuad as u8 => {
                                (Some("quad".to_string()), false)
                            }
                            t if t == PointType::OffCurveCubic as u8 => {
                                (Some("cubic".to_string()), false)
                            }
                            t if t == PointType::OnCurveSmooth as u8 => (None, true),
                            _ => (None, false), // on-curve
                        };
                    Point {
                        x: self.coordinates.get(i * 2).copied().unwrap_or_default(),
                        y: self.coordinates.get(i * 2 + 1).copied().unwrap_or_default(),
                        raw_type,
                        smooth,
                    }
                })
                .collect();
            contours.push(Contour {
                points,
                is_closed: info.is_closed,
            });
            start = info.end_point + 1;
        }
        contours
    }
}

/// In the .json glyph files, path objects are always of type [`UnpackedPath`],
/// not [`PackedPath`] (in memory they are generally [`PackedPath`],
/// the type of [`StaticGlyph::path`] is [`UnpackedPath`] | [`PackedPath`]).
#[derive(Debug, Clone, Deserialize)]
#[serde(untagged)]
pub(crate) enum Path {
    Packed(PackedPath),
    Unpacked(UnpackedPath),
}

impl Default for Path {
    fn default() -> Self {
        Path::Unpacked(UnpackedPath::default())
    }
}

impl Path {
    pub(crate) fn contours(&self) -> Cow<'_, [Contour]> {
        match self {
            Path::Unpacked(unpacked) => Cow::Borrowed(unpacked.contours.as_slice()),
            Path::Packed(packed) => Cow::Owned(packed.unpacked_contours()),
        }
    }
}

/// Corresponds to a FontTools DecomposedTransform
/// <https://github.com/fonttools/fonttools/blob/0572f78718/Lib/fontTools/misc/transform.py#L410>
#[derive(Debug, Clone, Deserialize)]
#[serde(default, rename_all = "camelCase")]
pub(crate) struct DecomposedTransform {
    pub(crate) translate_x: f64,
    pub(crate) translate_y: f64,
    /// in degrees counter-clockwise in font coordinate space
    pub(crate) rotation: f64,
    pub(crate) scale_x: f64,
    pub(crate) scale_y: f64,
    /// in degrees clockwise in font coordinate space
    pub(crate) skew_x: f64,
    /// in degrees counter-clockwise in font coordinate space
    pub(crate) skew_y: f64,
    pub(crate) t_center_x: f64,
    pub(crate) t_center_y: f64,
}

impl Default for DecomposedTransform {
    fn default() -> Self {
        // The identity transform: unit scale, everything else zero.
        Self {
            translate_x: 0.0,
            translate_y: 0.0,
            rotation: 0.0,
            scale_x: 1.0,
            scale_y: 1.0,
            skew_x: 0.0,
            skew_y: 0.0,
            t_center_x: 0.0,
            t_center_y: 0.0,
        }
    }
}

fn default_units_per_em() -> u16 {
    1000
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use pretty_assertions::assert_eq;
    use write_fonts::types::Tag;

    use crate::test::testdata_dir;

    use super::*;

    fn axis_tuples(font_data: &Font) -> Vec<(&str, Tag, f64, f64, f64)> {
        font_data
            .axes
            .axes
            .iter()
            .map(|a| match a {
                Axis::Continuous(a) => a,
                Axis::Discrete(a) => panic!("Unexpected discrete axis: {a:#?}"),
            })
            .map(|a| {
                (
                    a.name.as_str(),
                    a.tag,
                    a.min_value,
                    a.default_value,
                    a.max_value,
                )
            })
            .collect::<Vec<_>>()
    }

    fn glyph_axis_tuples(glyph: &VariableGlyph) -> Vec<(&str, f64, f64, f64)> {
        glyph
            .axes
            .iter()
            .map(|a| (a.name.as_str(), a.min_value, a.default_value, a.max_value))
            .collect::<Vec<_>>()
    }

    fn read_test_glyph(fontra_dir: &str, glyph_name: &str) -> VariableGlyph {
        let file = testdata_dir()
            .join(fontra_dir)
            .join("glyphs")
            .join(string_to_filename(glyph_name, ".json"));
        VariableGlyph::from_file(&file).unwrap_or_else(|e| panic!("Unable to read {file:?}: {e}"))
    }

    #[test]
    fn fontdata_of_minimal() {
        let font_data =
            Font::from_file(&testdata_dir().join("minimal.fontra/font-data.json")).unwrap();
        assert_eq!(1000, font_data.units_per_em);
        assert_eq!(
            vec![("Weight", Tag::new(b"wght"), 200.0, 200.0, 900.0),],
            axis_tuples(&font_data)
        );
    }

    #[test]
    fn fontdata_of_2glyphs() {
        let font_data =
            Font::from_file(&testdata_dir().join("2glyphs.fontra/font-data.json")).unwrap();
        assert_eq!(1000, font_data.units_per_em);
        assert_eq!(
            vec![
                ("Weight", Tag::new(b"wght"), 200.0, 200.0, 900.0),
                ("Width", Tag::new(b"wdth"), 50.0, 100.0, 125.0)
            ],
            axis_tuples(&font_data)
        );
        let wght = font_data
            .axes
            .axes
            .iter()
            .find(|a| a.tag() == Tag::new(b"wght"))
            .unwrap();
        assert_eq!(
            vec![[200.0, 0.0], [300.018, 0.095], [900.0, 1.0]],
            *wght.mapping()
        );
    }

    #[test]
    fn read_notdef() {
        let glyph = read_test_glyph("minimal.fontra", ".notdef");
        assert_eq!(GlyphName::new(".notdef"), glyph.name, "{glyph:#?}");
        assert_eq!(
            1000.0, glyph.layers["foreground"].glyph.x_advance,
            "{glyph:#?}"
        );
    }

    #[test]
    fn read_varc_axes() {
        let glyph = read_test_glyph("component.fontra", "VG_4E00_00");
        assert_eq!(
            vec![
                ("width", 200.0, 1000.0, 1000.0),
                ("weight", 10.0, 33.0, 50.0),
                ("serif_height", -50.0, 0.0, 100.0),
                ("serif_width", -100.0, 0.0, 100.0),
                ("serif_top_moveH", -50.0, 0.0, 50.0),
                ("H_L_length", -800.0, 0.0, 800.0),
            ],
            glyph_axis_tuples(&glyph),
        )
    }

    #[test]
    fn read_simple_contours() {
        let glyph = read_test_glyph("2glyphs.fontra", "u20089");
        assert_eq!(GlyphName::new("u20089"), glyph.name, "{glyph:#?}");
        let mut layer_names: Vec<_> = glyph.layers.keys().cloned().collect();
        layer_names.sort();
        assert_eq!(vec!["foreground", "wght=1"], layer_names);
        assert_eq!(
            HashSet::from([10, 11]),
            glyph
                .layers
                .values()
                .flat_map(|l| {
                    l.glyph
                        .path
                        .contours()
                        .iter()
                        .map(|c| c.points.len())
                        .collect::<Vec<_>>()
                })
                .collect::<HashSet<_>>(),
            "{glyph:#?}"
        );
        let foreground = glyph.layers["foreground"].glyph.path.contours();
        let contour = foreground.first().unwrap();
        assert_eq!(PointType::OnCurve, contour.points[0].point_type().unwrap());
        assert_eq!(
            PointType::OffCurveCubic,
            contour.points[2].point_type().unwrap()
        );
    }

    #[test]
    fn read_simple_component() {
        let glyph = read_test_glyph("component.fontra", "uni4E00");
        assert_eq!(GlyphName::new("uni4E00"), glyph.name, "{glyph:#?}");
        assert_eq!(
            vec![
                ("foreground".to_string(), GlyphName::new("VG_4E00_00")),
                ("wght=1".to_string(), GlyphName::new("VG_4E00_00"))
            ],
            glyph
                .layers
                .iter()
                .flat_map(|(n, l)| l
                    .glyph
                    .components
                    .iter()
                    .map(|c| (n.to_string(), c.name.clone())))
                .collect::<Vec<_>>(),
        );
    }

    #[test]
    fn transform_defaults_to_identity() {
        let c: Component = serde_json::from_str(r#"{"name":"a"}"#).unwrap();
        assert_eq!(
            (1.0, 1.0),
            (c.transformation.scale_x, c.transformation.scale_y)
        );
        let c: Component =
            serde_json::from_str(r#"{"name":"a","transformation":{"translateX":5.0}}"#).unwrap();
        assert_eq!(5.0, c.transformation.translate_x);
        assert_eq!(
            (1.0, 1.0),
            (c.transformation.scale_x, c.transformation.scale_y)
        );
    }

    // https://github.com/fontra/fontra/blob/469a001f8/test-py/test_path.py#L187
    #[test]
    fn packed_path_unpacks_to_contours() {
        let packed = PackedPath {
            coordinates: vec![
                232.0, -10.0, 338.0, -10.0, 403.0, 38.0, 403.0, 182.0, 403.0, 700.0, 363.0, 700.0,
                363.0, 182.0, 363.0, 60.0, 313.0, 26.0, 232.0, 26.0, 151.0, 26.0, 100.0, 60.0,
                100.0, 182.0, 100.0, 280.0, 60.0, 280.0, 60.0, 182.0, 60.0, 38.0, 124.0, -10.0,
            ],
            point_types: vec![8, 2, 2, 8, 0, 0, 8, 2, 2, 8, 2, 2, 8, 0, 0, 8, 2, 2],
            contour_info: vec![ContourInfo {
                end_point: 17,
                is_closed: true,
            }],
        };

        let contours = packed.unpacked_contours();
        assert_eq!(1, contours.len());
        let contour = &contours[0];
        assert!(contour.is_closed);

        let expected = [
            (232.0, -10.0, PointType::OnCurveSmooth),
            (338.0, -10.0, PointType::OffCurveCubic),
            (403.0, 38.0, PointType::OffCurveCubic),
            (403.0, 182.0, PointType::OnCurveSmooth),
            (403.0, 700.0, PointType::OnCurve),
            (363.0, 700.0, PointType::OnCurve),
            (363.0, 182.0, PointType::OnCurveSmooth),
            (363.0, 60.0, PointType::OffCurveCubic),
            (313.0, 26.0, PointType::OffCurveCubic),
            (232.0, 26.0, PointType::OnCurveSmooth),
            (151.0, 26.0, PointType::OffCurveCubic),
            (100.0, 60.0, PointType::OffCurveCubic),
            (100.0, 182.0, PointType::OnCurveSmooth),
            (100.0, 280.0, PointType::OnCurve),
            (60.0, 280.0, PointType::OnCurve),
            (60.0, 182.0, PointType::OnCurveSmooth),
            (60.0, 38.0, PointType::OffCurveCubic),
            (124.0, -10.0, PointType::OffCurveCubic),
        ];

        assert_eq!(expected.len(), contour.points.len());
        for (point, (x, y, point_type)) in contour.points.iter().zip(expected) {
            assert_eq!((x, y), (point.x, point.y));
            assert_eq!(point_type, point.point_type().unwrap());
        }
    }

    #[test]
    fn path_discriminates_packed_and_unpacked() {
        let unpacked: Path = serde_json::from_str(r#"{"contours": []}"#).unwrap();
        assert!(matches!(unpacked, Path::Unpacked(_)));
        let packed: Path =
            serde_json::from_str(r#"{"coordinates": [], "pointTypes": [], "contourInfo": []}"#)
                .unwrap();
        assert!(matches!(packed, Path::Packed(_)));
    }

    #[test]
    fn match_python_string_to_filename() {
        // expected is as observed in Python with .json appended
        let exemplars = vec![
            ("AUX", "AUX^7.json"),
            (".notdef", "%2Enotdef.json"),
            ("4E00", "4E00^2.json"),
            ("VG_4E00_01", "VG_4E00_01^J.json"),
            ("duck:goose/mallard", "duck%3Agoose%2Fmallard.json"),
            ("Hi ❤️‍🔥 hru", "Hi ❤️\u{200d}🔥 hru^1.json"),
        ];
        let mut errors = Vec::new();
        for (input, expected) in exemplars {
            let actual = string_to_filename(input, ".json");
            if expected != actual {
                errors.push(format!(
                    "\"{input}\" should convert to \"{expected}\" not \"{actual}\""
                ));
            }
        }
        assert_eq!(0, errors.len(), "{errors:#?}");
    }
}
