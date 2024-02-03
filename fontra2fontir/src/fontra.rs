//! Parse .fontra json filesets into structs
//!
//! See <https://github.com/googlefonts/fontra/blob/main/src/fontra/core/classes.py>

use std::{
    collections::HashMap,
    fs,
    path::{Path, PathBuf},
};

use fontdrasil::types::GlyphName;
use fontir::error::Error;
use percent_encoding::{utf8_percent_encode, NON_ALPHANUMERIC};
use serde::Deserialize;
use write_fonts::types::Tag;

pub(crate) fn glyph_file(glyph_dir: &Path, glyph: GlyphName) -> PathBuf {
    // TODO: this probably over-encodes. Should be sufficient to get started.
    let filename = utf8_percent_encode(glyph.as_str(), NON_ALPHANUMERIC);
    glyph_dir.join(filename.to_string() + ".json")
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
pub(crate) struct FontraAxis {
    pub(crate) name: String,
    pub(crate) tag: Tag,
    #[serde(default)]
    pub(crate) hidden: bool,
    #[serde(rename = "minValue")]
    pub(crate) min_value: f64,
    #[serde(rename = "defaultValue")]
    pub(crate) default_value: f64,
    #[serde(rename = "maxValue")]
    pub(crate) max_value: f64,
    #[serde(default)]
    pub(crate) mapping: Vec<[f64; 2]>,
}

/// serde type used to load .fontra/glyphs/namelike.json files
///
/// <https://github.com/googlefonts/fontra/blob/a4edd06837118e583804fd963c22ed806a315b04/src/fontra/core/classes.py#L104-L116>
#[derive(Debug, Clone, Deserialize)]
#[allow(dead_code)] // TEMPORARY
pub(crate) struct FontraGlyph {
    pub(crate) name: GlyphName,
    pub(crate) sources: Vec<FontraSource>,
    pub(crate) layers: HashMap<String, FontraLayer>,
}

/// <https://github.com/googlefonts/fontra/blob/a4edd06837118e583804fd963c22ed806a315b04/src/fontra/core/classes.py#L119-L126>
#[derive(Debug, Clone, Deserialize)]
#[allow(dead_code)] // TEMPORARY
pub(crate) struct FontraSource {
    pub(crate) name: String,
    #[serde(rename = "layerName")]
    pub(crate) layer_name: String,
    #[serde(default)]
    pub(crate) location: HashMap<Tag, f64>,
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
            .find(|a| a.tag == Tag::new(b"wght"))
            .unwrap();
        assert_eq!(
            vec![[200.0, 0.0], [300.018, 0.095], [900.0, 1.0]],
            wght.mapping
        );
    }

    #[test]
    fn read_notdef() {
        let glyph =
            FontraGlyph::from_file(&testdata_dir().join("minimal.fontra/glyphs/%2Enotdef.json"))
                .unwrap();
        assert_eq!(GlyphName::new(".notdef"), glyph.name, "{glyph:#?}");
        assert_eq!(
            1000.0, glyph.layers["foreground"].glyph.x_advance,
            "{glyph:#?}"
        );
    }

    #[test]
    fn read_u20089() {
        let glyph =
            FontraGlyph::from_file(&testdata_dir().join("2glyphs.fontra/glyphs/u20089.json"))
                .unwrap();
        assert_eq!(GlyphName::new("u20089"), glyph.name, "{glyph:#?}");
        let mut layer_names: Vec<_> = glyph.layers.keys().map(|n| n.as_str()).collect();
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
}
