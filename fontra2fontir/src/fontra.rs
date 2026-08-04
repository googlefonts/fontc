//! Parse .fontra json filesets into structs
//!
//! See <https://github.com/fontra/fontra/blob/469a001f8/src/fontra/core/classes.py>

#![allow(dead_code)] // TEMPORARY

use std::{
    borrow::Cow,
    collections::{BTreeMap, HashMap},
    fs::{self, File},
    io::{BufRead, BufReader},
    path,
};

use fontdrasil::{paths::string_to_filename, types::GlyphName};
use fontir::error::{BadSource, BadSourceKind, PathConversionError};
use serde::Deserialize;
use smol_str::SmolStr;
use write_fonts::types::Tag;

pub(crate) type AxisName = String;
pub(crate) type LayerName = String;
pub(crate) type GlyphMap = BTreeMap<GlyphName, Vec<u32>>;
pub(crate) type GlyphInfos = BTreeMap<GlyphName, GlyphInfo>;

/// Corresponds to a Fontra Location
/// <https://github.com/fontra/fontra/blob/469a001f8/src/fontra/core/classes.py#L318>
pub(crate) type Location = HashMap<AxisName, f64>;

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
    pub(crate) category: Option<SmolStr>,
    pub(crate) sub_category: Option<SmolStr>,
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

type KerningValues = HashMap<SmolStr, HashMap<SmolStr, Vec<Option<f64>>>>;

/// Corresponds to a Fontra Kerning
/// <https://github.com/fontra/fontra/blob/469a001f8/src/fontra/core/classes.py#L93>
#[derive(Debug, Clone, Default, Deserialize)]
#[serde(rename_all = "camelCase")]
pub(crate) struct Kerning {
    pub(crate) groups_side1: HashMap<SmolStr, Vec<SmolStr>>,
    pub(crate) groups_side2: HashMap<SmolStr, Vec<SmolStr>>,
    pub(crate) source_identifiers: Vec<String>,
    /// left glyph/group -> right glyph/group -> source index -> value
    pub(crate) values: KerningValues,
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
    pub(crate) fn load(path: &path::Path) -> Result<Self, BadSource> {
        // The font metadata lives in font-data.json
        let fontdata_file = path.join("font-data.json");
        if !fontdata_file.is_file() {
            return Err(BadSource::new(fontdata_file, BadSourceKind::ExpectedFile));
        }
        let mut font: Font = from_file(&fontdata_file)?;

        // The glyph map and infos live in glyph-info.csv.
        let (glyph_map, glyph_infos) = parse_glyph_info(path)?;
        font.glyph_map = glyph_map;
        font.glyph_infos = glyph_infos;

        // The glyph data live in individual .json files under "glyphs" dir
        let glyphs_dir = path.join("glyphs");
        font.glyphs = font
            .glyph_map
            .keys()
            .map(|name| {
                let file = glyphs_dir.join(string_to_filename(name.as_str(), ".json"));
                Ok((name.clone(), VariableGlyph::from_file(&file)?))
            })
            .collect::<Result<_, BadSource>>()?;

        // The features live in features.txt
        let features_file = path.join("features.txt");
        if features_file.is_file() {
            font.features.text = fs::read_to_string(&features_file)
                .map_err(|e| BadSource::new(&features_file, e))?;
        }

        // The kerning lives in kerning.csv
        font.kerning = parse_kerning(path)?;

        Ok(font)
    }
}

/// Parse glyph-info.csv into the glyph map (code points) and glyph infos.
///
/// Fontra keeps these in glyph-info.csv rather than font-data.json.
fn parse_glyph_info(fontra_dir: &path::Path) -> Result<(GlyphMap, GlyphInfos), BadSource> {
    let glyphinfo_file = fontra_dir.join("glyph-info.csv");
    if !glyphinfo_file.is_file() {
        return Err(BadSource::new(glyphinfo_file, BadSourceKind::ExpectedFile));
    }
    let file = File::open(&glyphinfo_file).map_err(|e| BadSource::new(&glyphinfo_file, e))?;
    let mut lines = BufReader::new(file).lines();

    // The first line is the header. The first two columns are mandatory
    // "glyph name" and "code points". All remaining columns are optional,
    // so we resolve the ones we want by name.
    let header = lines
        .next()
        .transpose()
        .map_err(|e| BadSource::new(&glyphinfo_file, BadSourceKind::Io(e)))?
        .ok_or_else(|| BadSource::custom(&glyphinfo_file, "Empty glyph-info.csv"))?;
    let column = |name: &str| header.split(';').position(|h| h.trim() == name);
    let category_col = column("category");
    let subcategory_col = column("subcategory");

    let mut glyph_map = GlyphMap::new();
    let mut glyph_infos = GlyphInfos::new();
    for (i, line) in lines.enumerate() {
        let i = i + 1;
        let line = line.map_err(|e| BadSource::new(&glyphinfo_file, BadSourceKind::Io(e)))?;
        let parts: Vec<_> = line.split(';').collect();
        if parts.len() < 2 {
            return Err(BadSource::custom(
                &glyphinfo_file,
                format!("Expected at least two parts in line {i} separated by ;"),
            ));
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

        let cell = |col: Option<usize>| {
            col.and_then(|c| parts.get(c))
                .map(|s| s.trim())
                .filter(|s| !s.is_empty())
                .map(SmolStr::from)
        };
        let info = GlyphInfo {
            category: cell(category_col),
            sub_category: cell(subcategory_col),
        };

        if glyph_map.insert(glyph_name.clone(), codepoints).is_some() {
            return Err(BadSource::custom(
                &glyphinfo_file,
                format!("Multiple definitions of '{glyph_name}'"),
            ));
        }
        glyph_infos.insert(glyph_name, info);
    }

    Ok((glyph_map, glyph_infos))
}

/// Parse kerning.
///
/// Fontra keeps kerning in kerning.csv file.
///
/// The file is split into blank line-delimited sections (TYPE, GROUPS1, GROUPS2, VALUES), repeated
/// once per kerning type. Each section contains semicolon-delimited entries.
///
/// The legacy single-GROUPS format is not supported.
///
/// <https://github.com/fontra/fontra/blob/469a001f8/src/fontra/backends/fontra.py#L538>
fn parse_kerning(fontra_dir: &path::Path) -> Result<BTreeMap<String, Kerning>, BadSource> {
    let kerning_file = fontra_dir.join("kerning.csv");
    if !kerning_file.is_file() {
        return Ok(BTreeMap::new());
    }
    let content =
        fs::read_to_string(&kerning_file).map_err(|e| BadSource::new(&kerning_file, e))?;
    let mut row_iter = (1..).zip(
        content
            .lines()
            .map(|line| line.split(';').collect::<Vec<_>>()),
    );

    let mut kerning = BTreeMap::new();
    while let Some(kern_type) = kerning_read_type(&mut row_iter, &kerning_file)? {
        let groups_side1 = kerning_read_groups(&mut row_iter, &kerning_file, "GROUPS1")?;
        let groups_side2 = kerning_read_groups(&mut row_iter, &kerning_file, "GROUPS2")?;
        let (source_identifiers, values) = kerning_read_values(&mut row_iter, &kerning_file)?;
        kerning.insert(
            kern_type,
            Kerning {
                groups_side1,
                groups_side2,
                source_identifiers,
                values,
            },
        );
    }

    Ok(kerning)
}

fn next_non_blank_row<'a>(
    row_iter: &mut impl Iterator<Item = (usize, Vec<&'a str>)>,
) -> (Option<usize>, Vec<&'a str>) {
    row_iter
        .find(|(_, row)| !row[0].is_empty())
        .map(|(line_number, row)| (Some(line_number), row))
        .unwrap_or_default()
}

fn next_row<'a>(
    row_iter: &mut impl Iterator<Item = (usize, Vec<&'a str>)>,
) -> (Option<usize>, Vec<&'a str>) {
    row_iter
        .next()
        .map(|(line_number, row)| (Some(line_number), row))
        .unwrap_or_default()
}

fn kerning_read_type<'a>(
    row_iter: &mut impl Iterator<Item = (usize, Vec<&'a str>)>,
    kerning_file: &path::Path,
) -> Result<Option<String>, BadSource> {
    let (line_number, row) = next_non_blank_row(row_iter);
    if line_number.is_none() {
        return Ok(None);
    }
    if row[0] != "TYPE" {
        return Err(BadSource::custom(
            kerning_file,
            format!("expected TYPE keyword (line {line_number:?})"),
        ));
    }

    let (line_number, row) = next_row(row_iter);
    if row.is_empty() || row[0].is_empty() {
        return Err(BadSource::custom(
            kerning_file,
            format!("expected TYPE value string (line {line_number:?})"),
        ));
    }
    Ok(Some(row[0].to_string()))
}

fn kerning_read_groups<'a>(
    row_iter: &mut impl Iterator<Item = (usize, Vec<&'a str>)>,
    kerning_file: &path::Path,
    keyword: &str,
) -> Result<HashMap<SmolStr, Vec<SmolStr>>, BadSource> {
    let (line_number, row) = next_non_blank_row(row_iter);
    if row.is_empty() || row[0] != keyword {
        return Err(BadSource::custom(
            kerning_file,
            format!("expected {keyword} keyword (line {line_number:?})"),
        ));
    }

    let mut groups = HashMap::new();
    for (_, row) in row_iter {
        if row[0].is_empty() {
            break;
        }
        let members = row[1..].iter().map(|&s| s.into()).collect();
        groups.insert(row[0].into(), members);
    }

    Ok(groups)
}

fn kerning_read_values<'a>(
    row_iter: &mut impl Iterator<Item = (usize, Vec<&'a str>)>,
    kerning_file: &path::Path,
) -> Result<(Vec<String>, KerningValues), BadSource> {
    let (line_number, row) = next_non_blank_row(row_iter);
    if row.is_empty() || row[0] != "VALUES" {
        return Err(BadSource::custom(
            kerning_file,
            format!("expected VALUES keyword (line {line_number:?})"),
        ));
    }

    let (line_number, row) = next_row(row_iter);
    if row.len() < 3 || row[0] != "side1" || row[1] != "side2" {
        return Err(BadSource::custom(
            kerning_file,
            format!("expected source identifier row (line {line_number:?})"),
        ));
    }
    let source_identifiers: Vec<_> = row[2..].iter().map(|s| s.to_string()).collect();

    let mut values: KerningValues = HashMap::new();
    for (line_number, row) in row_iter {
        if row[0].is_empty() {
            break;
        }
        if row.len() < 2 {
            return Err(BadSource::custom(
                kerning_file,
                format!("expected kern values (line {line_number})"),
            ));
        }

        let left = row[0];
        let right = row[1];
        let kerns = row[2..]
            .iter()
            .map(|v| match *v {
                "" => Ok(None),
                v => v.parse().map(Some).map_err(|_| {
                    BadSource::custom(
                        kerning_file,
                        format!("parse error: {v:?} (line {line_number})"),
                    )
                }),
            })
            .collect::<Result<Vec<_>, _>>()?;
        values
            .entry(left.into())
            .or_default()
            .insert(right.into(), kerns);
    }
    Ok((source_identifiers, values))
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
    pub(crate) name: Option<SmolStr>,
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
        let font_data = Font::load(&testdata_dir().join("minimal.fontra")).unwrap();
        assert_eq!(1000, font_data.units_per_em);
        assert_eq!(
            vec![("Weight", Tag::new(b"wght"), 200.0, 200.0, 900.0),],
            axis_tuples(&font_data)
        );
    }

    #[test]
    fn fontdata_of_2glyphs() {
        let font_data = Font::load(&testdata_dir().join("2glyphs.fontra")).unwrap();
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

    fn groups(entries: &[(&str, &[&str])]) -> HashMap<SmolStr, Vec<SmolStr>> {
        entries
            .iter()
            .map(|(name, members)| {
                (
                    SmolStr::from(*name),
                    members.iter().map(|m| SmolStr::from(*m)).collect(),
                )
            })
            .collect()
    }

    fn load_font(fontra_dir: &str) -> Font {
        let dir = testdata_dir().join(fontra_dir);
        Font::load(&dir).unwrap_or_else(|e| panic!("Unable to load {dir:?}: {e}"))
    }

    #[test]
    fn kerning_of_mutatorsans() {
        // A single "kern" table with five sources and many missing (empty) cells.
        let font = load_font("MutatorSans.fontra");
        assert_eq!(
            vec!["kern"],
            font.kerning.keys().map(String::as_str).collect::<Vec<_>>()
        );
        let kern = &font.kerning["kern"];
        assert_eq!(
            vec![
                "light-condensed",
                "bold-condensed",
                "light-wide",
                "bold-wide",
                "light-condensed-italic",
            ],
            kern.source_identifiers
        );
        assert_eq!(
            groups(&[("A", &["A", "Aacute", "Adieresis"])]),
            kern.groups_side1
        );
        assert_eq!(
            groups(&[("A", &["A", "Aacute", "Adieresis"])]),
            kern.groups_side2
        );
        // "T;@A;-75;;-215;-150;" and "T;A;;-65;;;": missing values become None,
        // and the value references the group "A" by its "@A" name.
        assert_eq!(
            vec![Some(-75.0), None, Some(-215.0), Some(-150.0), None],
            kern.values["T"]["@A"]
        );
        assert_eq!(
            vec![None, Some(-65.0), None, None, None],
            kern.values["T"]["A"]
        );
    }

    #[test]
    fn kerning_of_glyphs_unit_test_sans() {
        // Two kerning tables in one file: horizontal "kern" and vertical "vkrn".
        let font = load_font("GlyphsUnitTestSans3.fontra");
        assert_eq!(
            vec!["kern", "vkrn"],
            font.kerning.keys().map(String::as_str).collect::<Vec<_>>()
        );

        let kern = &font.kerning["kern"];
        assert_eq!(3, kern.source_identifiers.len());
        assert_eq!(5, kern.groups_side1.len());
        assert_eq!(vec!["h", "m", "n"], kern.groups_side1["nKernRight"]);
        assert_eq!(vec!["n"], kern.groups_side2["n"]);
        assert_eq!(
            vec![Some(-30.0), Some(-20.0), Some(-10.0)],
            kern.values["@A"]["@J"]
        );
        assert_eq!(vec![None, None, Some(-10.0)], kern.values["@B"]["@y"]);

        let vkrn = &font.kerning["vkrn"];
        assert_eq!(
            groups(&[("ABottom", &["A"]), ("VBottom", &["V"])]),
            vkrn.groups_side1
        );
        assert_eq!(
            groups(&[("ATop", &["A"]), ("VTop", &["V"])]),
            vkrn.groups_side2
        );
        assert_eq!(
            vec![Some(-301.0), Some(-302.0), Some(-303.0)],
            vkrn.values["@ABottom"]["@VTop"]
        );
        assert_eq!(
            vec![Some(-301.0), Some(-302.0), Some(-303.0)],
            vkrn.values["Adieresis"]["@VTop"]
        );
    }

    #[test]
    fn glyph_info_of_raqq() {
        // glyph-info.csv here has both the "category" and "subcategory" columns.
        let font = load_font("Raqq.fontra");

        let codepoints = |name: &str| font.glyph_map[&GlyphName::new(name)].clone();
        assert_eq!(vec![0x0639], codepoints("ain-ar"));
        // Multiple code points.
        assert_eq!(vec![0x064C, 0x08F1], codepoints("dammatan-ar"));
        assert_eq!(Vec::<u32>::new(), codepoints("fehDotless_alef-ar"));
        assert_eq!(Vec::<u32>::new(), codepoints(".notdef"));

        let info = |name: &str| font.glyph_infos[&GlyphName::new(name)].clone();
        let glyph_info = |category: &str, sub: Option<&str>| GlyphInfo {
            category: Some(category.into()),
            sub_category: sub.map(SmolStr::from),
        };
        assert_eq!(glyph_info("Letter", None), info("ain-ar"));
        assert_eq!(
            glyph_info("Letter", Some("Ligature")),
            info("fehDotless_alef-ar")
        );
        assert_eq!(glyph_info("Mark", Some("Nonspacing")), info("dammatan-ar"));
        assert_eq!(glyph_info("Number", Some("Decimal Digit")), info("eight"));
        assert_eq!(glyph_info("Punctuation", None), info("endofayah-ar"));
        assert_eq!(glyph_info("Symbol", None), info("dottedCircle"));
        assert_eq!(glyph_info("Separator", Some("Space")), info("hairspace"));
        assert_eq!(glyph_info("Separator", None), info(".notdef"));
        // Columns present but with empty cells leave the info blank.
        assert_eq!(GlyphInfo::default(), info("_ayah.005"));
    }

    #[test]
    fn glyph_info_of_mutatorsans() {
        // glyph-info.csv without category columns: only the code points populate.
        let font = load_font("MutatorSans.fontra");
        let codepoints = |name: &str| font.glyph_map[&GlyphName::new(name)].clone();
        assert_eq!(vec![0x0041, 0x0061], codepoints("A"));
        assert_eq!(vec![0x00B4], codepoints("acute"));
        assert_eq!(Vec::<u32>::new(), codepoints("I.narrow"));
        // No category/subcategory columns, so the infos are empty.
        assert_eq!(GlyphInfo::default(), font.glyph_infos[&GlyphName::new("A")]);
    }

    #[test]
    fn glyph_info_of_glyphs_smart_components() {
        // glyph-info.csv here has a "category" column but no "subcategory".
        let font = load_font("GlyphsSmartComponents.fontra");
        let info = |name: &str| font.glyph_infos[&GlyphName::new(name)].clone();
        // Category is resolved by column name; subcategory stays None as its
        // column is absent.
        assert_eq!(
            GlyphInfo {
                category: Some("Letter".into()),
                sub_category: None,
            },
            info("uni2E8A-CN")
        );
        // An empty category cell leaves the info blank.
        assert_eq!(GlyphInfo::default(), info(".notdef"));
        assert_eq!(GlyphInfo::default(), info("_part.Bar_H_2x"));
    }
}
