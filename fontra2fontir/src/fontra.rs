//! Parse .fontra json filesets into structs
//!
//! See <https://github.com/googlefonts/fontra/blob/main/src/fontra/core/classes.py>

use std::{
    collections::{BTreeMap, HashMap},
    fs,
    path::{Path, PathBuf},
};

use fontdrasil::{paths::string_to_filename, types::GlyphName};
use fontir::error::Error;
use serde::Deserialize;
use write_fonts::types::Tag;

pub(crate) type AxisName = String;
pub(crate) type LayerName = String;

pub(crate) fn glyph_file(glyph_dir: &Path, glyph: GlyphName) -> PathBuf {
    glyph_dir.join(string_to_filename(glyph.as_str(), ".json"))
}

fn from_file<T>(p: &Path) -> Result<T, Error>
where
    for<'a> T: Deserialize<'a>,
{
    let raw = fs::read_to_string(p).map_err(Error::IoError)?;
    serde_json::from_str(&raw).map_err(|e| Error::ParseError(p.to_path_buf(), format!("{e}")))
}

/// serde type used to load font-data.json
#[derive(Debug, Clone, Deserialize)]
pub(crate) struct FontraFontData {
    #[serde(rename = "unitsPerEm")]
    pub(crate) units_per_em: u16,
    #[serde(default)]
    pub(crate) axes: Vec<FontraAxis>,
}

impl FontraFontData {
    pub(crate) fn from_file(p: &Path) -> Result<Self, Error> {
        from_file(p)
    }
}

#[derive(Debug, Clone, Deserialize)]
#[serde(untagged)]
pub(crate) enum FontraAxis {
    Continuous(FontraContinuousAxis),
    Discrete(FontraDiscreteAxis),
}

#[allow(dead_code)] // TEMPORARY
impl FontraAxis {
    pub(crate) fn name(&self) -> &AxisName {
        match self {
            FontraAxis::Continuous(a) => &a.name,
            FontraAxis::Discrete(a) => &a.name,
        }
    }

    pub(crate) fn tag(&self) -> Tag {
        match self {
            FontraAxis::Continuous(a) => a.tag,
            FontraAxis::Discrete(a) => a.tag,
        }
    }

    pub(crate) fn default_value(&self) -> f64 {
        match self {
            FontraAxis::Continuous(a) => a.default_value,
            FontraAxis::Discrete(a) => a.default_value,
        }
    }

    pub(crate) fn hidden(&self) -> bool {
        match self {
            FontraAxis::Continuous(a) => a.hidden,
            FontraAxis::Discrete(a) => a.hidden,
        }
    }

    /// Pairs of [user, design] defining a pairwise linear map
    pub(crate) fn mapping(&self) -> &Vec<[f64; 2]> {
        match self {
            FontraAxis::Continuous(a) => &a.mapping,
            FontraAxis::Discrete(a) => &a.mapping,
        }
    }
}

/// Corresponds to a Fontra GlobalAxis
/// <https://github.com/googlefonts/fontra/blob/1c330c3e4243611191d9999dd6b4af37cca9daff/src/fontra/core/classes.py#L104>
#[derive(Debug, Clone, Deserialize)]
pub(crate) struct FontraContinuousAxis {
    pub(crate) name: AxisName,
    pub(crate) tag: Tag,
    #[serde(default)]
    pub(crate) hidden: bool,
    #[serde(rename = "defaultValue")]
    pub(crate) default_value: f64,
    #[serde(default)]
    pub(crate) mapping: Vec<[f64; 2]>,

    #[serde(rename = "minValue")]
    pub(crate) min_value: f64,
    #[serde(rename = "maxValue")]
    pub(crate) max_value: f64,
}

/// Corresponds to a Fontra GlobalDiscreteAxis
/// <https://github.com/googlefonts/fontra/blob/1c330c3e4243611191d9999dd6b4af37cca9daff/src/fontra/core/classes.py#L118>
#[derive(Debug, Clone, Deserialize)]
#[allow(dead_code)] // TEMPORARY
pub(crate) struct FontraDiscreteAxis {
    pub(crate) name: AxisName,
    pub(crate) tag: Tag,
    #[serde(default)]
    pub(crate) hidden: bool,
    #[serde(rename = "defaultValue")]
    pub(crate) default_value: f64,
    #[serde(default)]
    pub(crate) mapping: Vec<[f64; 2]>,
    values: Vec<f64>,
}

/// serde type used to load .fontra/glyphs/namelike.json files
///
/// <https://github.com/googlefonts/fontra/blob/a4edd06837118e583804fd963c22ed806a315b04/src/fontra/core/classes.py#L104-L116>
#[derive(Debug, Clone, Deserialize)]
#[allow(dead_code)] // TEMPORARY
pub(crate) struct FontraGlyph {
    pub(crate) name: GlyphName,
    /// Variable component, or glyph-local, axes
    #[serde(default)]
    pub(crate) axes: Vec<FontraGlyphAxis>,
    pub(crate) sources: Vec<FontraSource>,
    pub(crate) layers: BTreeMap<LayerName, FontraLayer>,
}

/// An axis specific to a glyph meant to be used as a variable component
///
/// <https://github.com/googlefonts/fontra/blob/15bc0b8401054390484cfb86d509d633d29657a1/src/fontra/core/classes.py#L96-L101>
#[derive(Debug, Clone, Deserialize)]
#[allow(dead_code)] // TEMPORARY
pub(crate) struct FontraGlyphAxis {
    pub(crate) name: String,
    #[serde(rename = "minValue")]
    pub(crate) min_value: f64,
    #[serde(rename = "defaultValue")]
    pub(crate) default_value: f64,
    #[serde(rename = "maxValue")]
    pub(crate) max_value: f64,
}

/// <https://github.com/googlefonts/fontra/blob/a4edd06837118e583804fd963c22ed806a315b04/src/fontra/core/classes.py#L119-L126>
#[derive(Debug, Clone, Deserialize)]
#[allow(dead_code)] // TEMPORARY
pub(crate) struct FontraSource {
    pub(crate) name: String,
    #[serde(rename = "layerName")]
    pub(crate) layer_name: LayerName,
    #[serde(default)]
    pub(crate) location: HashMap<AxisName, f64>,
    // TODO: locationBase
    #[serde(default)]
    pub(crate) inactive: bool,
}

/// <https://github.com/googlefonts/fontra/blob/a4edd06837118e583804fd963c22ed806a315b04/src/fontra/core/classes.py#L129-L132>
#[derive(Debug, Clone, Deserialize)]
#[allow(dead_code)] // TEMPORARY
pub(crate) struct FontraLayer {
    pub(crate) glyph: FontraGlyphInstance,
}

/// <https://github.com/googlefonts/fontra/blob/a4edd06837118e583804fd963c22ed806a315b04/src/fontra/core/classes.py#L135-L151>
#[derive(Debug, Clone, Deserialize)]
#[allow(dead_code)] // TEMPORARY
pub(crate) struct FontraGlyphInstance {
    #[serde(rename = "xAdvance")]
    pub(crate) x_advance: f64,
    // TODO: Fontra has two representations, packed and unpacked. This only covers one.
    #[serde(default)]
    pub(crate) path: FontraPath,
    #[serde(default)]
    pub(crate) components: Vec<FontraComponent>,
}

impl FontraGlyph {
    #[allow(dead_code)] // TEMPORARY
    pub(crate) fn from_file(p: &Path) -> Result<Self, Error> {
        from_file(p)
    }
}

/// <https://github.com/googlefonts/fontra/blob/a4edd06837118e583804fd963c22ed806a315b04/src/fontra/core/path.py#L34-L53>
#[derive(Default, Debug, Clone, Deserialize)]
#[allow(dead_code)] // TEMPORARY
pub(crate) struct FontraPath {
    #[serde(default)]
    pub(crate) contours: Vec<FontraContour>,
}

/// <https://github.com/googlefonts/fontra/blob/a4edd06837118e583804fd963c22ed806a315b04/src/fontra/core/path.py#L28-L31>
#[derive(Debug, Clone, Deserialize)]
#[allow(dead_code)] // TEMPORARY
pub(crate) struct FontraContour {
    pub(crate) points: Vec<FontraPoint>,
    #[serde(rename = "isClosed", default)]
    pub(crate) is_closed: bool,
}

#[derive(Debug, Clone, Deserialize)]
#[allow(dead_code)] // TEMPORARY
pub(crate) struct FontraPoint {
    pub(crate) x: f64,
    pub(crate) y: f64,
    #[serde(default)]
    pub(crate) smooth: bool,
    #[serde(rename = "type")]
    raw_type: Option<String>,
}

impl FontraPoint {
    /// <https://github.com/googlefonts/fontra/blob/a4edd06837118e583804fd963c22ed806a315b04/src/fontra/core/path.py#L396-L406>
    #[allow(dead_code)] // TEMPORARY
    pub(crate) fn point_type(&self) -> Result<PointType, Error> {
        match (self.smooth, self.raw_type.as_deref()) {
            (false, Some("cubic")) => Ok(PointType::OffCurveCubic),
            (false, Some("quad")) => Ok(PointType::OffCurveQuad),
            (false, None) => Ok(PointType::OnCurve),
            (true, None) => Ok(PointType::OnCurveSmooth),
            _ => Err(Error::InvalidInputData(format!(
                "Unrecognized combination, smooth {}, type {:?}",
                self.smooth, self.raw_type
            ))),
        }
    }
}

/// <https://github.com/googlefonts/fontra/blob/a4edd06837118e583804fd963c22ed806a315b04/src/fontra/core/path.py#L65-L69>
#[derive(Debug, Clone, Copy, Default, PartialEq)]
#[allow(dead_code)] // TEMPORARY
pub(crate) enum PointType {
    #[default]
    OnCurve,
    OffCurveQuad,
    OffCurveCubic,
    OnCurveSmooth,
}

impl PointType {
    pub(crate) fn is_off_curve(&self) -> bool {
        match self {
            PointType::OffCurveCubic | PointType::OffCurveQuad => true,
            PointType::OnCurve | PointType::OnCurveSmooth => false,
        }
    }
}

/// <https://github.com/googlefonts/fontra/blob/a4edd06837118e583804fd963c22ed806a315b04/src/fontra/core/classes.py#L154-L158>
#[derive(Debug, Clone, Deserialize)]
#[allow(dead_code)] // TEMPORARY
pub(crate) struct FontraComponent {
    pub(crate) name: GlyphName,
    #[serde(default)]
    pub(crate) transformation: FontraTransform,
    // This location is in terms of axes defined by the referenced glyph
    #[serde(default)]
    pub(crate) location: HashMap<String, f64>,
}

/// What FontTools calls a DecomposedTransform
///
/// <https://github.com/fonttools/fonttools/blob/0572f7871823bdef3ceceaf41dedd0a6bd100995/Lib/fontTools/misc/transform.py#L410-L424>
#[derive(Default, Debug, Clone, Deserialize)]
#[allow(dead_code)] // TEMPORARY
pub(crate) struct FontraTransform {
    #[serde(rename = "translateX", default)]
    translate_x: f64,
    #[serde(rename = "translateY", default)]
    translate_y: f64,
    /// in degrees counter-clockwise in font coordinate space
    #[serde(default)]
    rotation: f64,
    #[serde(rename = "scaleX", default = "float_one")]
    scale_x: f64,
    #[serde(rename = "scaleY", default = "float_one")]
    scale_y: f64,
    /// in degrees clockwise in font coordinate space
    #[serde(rename = "skewX", default)]
    skew_x: f64,
    /// in degrees counter-clockwise in font coordinate space
    #[serde(rename = "skewY", default)]
    skew_y: f64,
    #[serde(rename = "tCenterX", default)]
    t_center_x: f64,
    #[serde(rename = "tCenterY", default)]
    t_center_y: f64,
}

fn float_one() -> f64 {
    1.0
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use pretty_assertions::assert_eq;
    use write_fonts::types::Tag;

    use crate::test::testdata_dir;

    use super::*;

    fn axis_tuples(font_data: &FontraFontData) -> Vec<(&str, Tag, f64, f64, f64)> {
        font_data
            .axes
            .iter()
            .map(|a| match a {
                FontraAxis::Continuous(a) => a,
                FontraAxis::Discrete(a) => panic!("Unexpected discrete axis: {a:#?}"),
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

    fn glyph_axis_tuples(glyph: &FontraGlyph) -> Vec<(&str, f64, f64, f64)> {
        glyph
            .axes
            .iter()
            .map(|a| (a.name.as_str(), a.min_value, a.default_value, a.max_value))
            .collect::<Vec<_>>()
    }

    fn read_test_glyph(fontra_dir: &str, glyph_name: &str) -> FontraGlyph {
        let file = testdata_dir()
            .join(fontra_dir)
            .join("glyphs")
            .join(string_to_filename(glyph_name, ".json"));
        FontraGlyph::from_file(&file).unwrap_or_else(|e| panic!("Unable to read {file:?}: {e}"))
    }

    #[test]
    fn fontdata_of_minimal() {
        let font_data =
            FontraFontData::from_file(&testdata_dir().join("minimal.fontra/font-data.json"))
                .unwrap();
        assert_eq!(1000, font_data.units_per_em);
        assert_eq!(
            vec![("Weight", Tag::new(b"wght"), 200.0, 200.0, 900.0),],
            axis_tuples(&font_data)
        );
    }

    #[test]
    fn fontdata_of_2glyphs() {
        let font_data =
            FontraFontData::from_file(&testdata_dir().join("2glyphs.fontra/font-data.json"))
                .unwrap();
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
                .flat_map(|l| l.glyph.path.contours.iter().map(|c| c.points.len()))
                .collect::<HashSet<_>>(),
            "{glyph:#?}"
        );
        let contour = glyph.layers["foreground"]
            .glyph
            .path
            .contours
            .first()
            .unwrap();
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
