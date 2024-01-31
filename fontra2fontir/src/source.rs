use std::{
    collections::HashMap,
    fs::{self, File},
    io::{BufRead, BufReader},
    path::{Path, PathBuf},
};

use fontdrasil::types::GlyphName;
use fontir::{
    error::Error,
    source::{Input, Source},
    stateset::StateSet,
};
use percent_encoding::{utf8_percent_encode, NON_ALPHANUMERIC};
use serde::Deserialize;
use write_fonts::types::Tag;

pub struct FontraIrSource {
    fontdata_file: PathBuf,
    glyphinfo_file: PathBuf,
    glyph_dir: PathBuf,
    glyph_info: HashMap<GlyphName, (PathBuf, Option<u32>)>,
}

impl FontraIrSource {
    pub fn new(fontra_dir: PathBuf) -> Result<Self, Error> {
        let fontdata_file = fontra_dir.join("font-data.json");
        if !fontdata_file.is_file() {
            return Err(Error::FileExpected(fontdata_file));
        }
        let glyphinfo_file = fontra_dir.join("glyph-info.csv");
        if !glyphinfo_file.is_file() {
            return Err(Error::FileExpected(glyphinfo_file));
        }
        let glyph_dir = fontra_dir.join("glyphs");
        if !glyph_dir.is_dir() {
            return Err(Error::DirectoryExpected(glyph_dir));
        }
        Ok(FontraIrSource {
            fontdata_file,
            glyphinfo_file,
            glyph_dir,
            glyph_info: Default::default(),
        })
    }

    fn glyph_file(&self, glyph: GlyphName) -> PathBuf {
        // TODO: this probably over-encodes. Should be sufficient to get started.
        let filename = utf8_percent_encode(glyph.as_str(), NON_ALPHANUMERIC);
        self.glyph_dir.join(filename.to_string() + ".json")
    }

    // When things like upem may have changed forget incremental and rebuild the whole thing
    fn static_metadata_state(&self) -> Result<StateSet, Error> {
        let mut font_info = StateSet::new();
        font_info.track_file(&self.fontdata_file)?;
        font_info.track_file(&self.glyphinfo_file)?;
        Ok(font_info)
    }

    fn load_glyphinfo(&mut self) -> Result<(), Error> {
        // Read the glyph-info file
        let file = File::open(&self.glyphinfo_file).map_err(Error::IoError)?;

        // Example files suggest the first line is just the column headers. Hopefully always :)
        // This file is tool generated so it shouldn't be full of human error. Fail if we don't understand.
        for (i, line) in BufReader::new(file).lines().enumerate().skip(1) {
            let line = line.map_err(Error::IoError)?;
            let parts: Vec<_> = line.split(';').collect();
            if parts.len() != 2 {
                return Err(Error::ParseError(
                    self.glyphinfo_file.clone(),
                    format!("Expected two parts in line {i} separated by ;"),
                ));
            }
            let glyph_name = GlyphName::new(parts[0].trim());
            // TODO: support multiple codepoints
            let codepoint = parts[1].trim();
            let codepoint = if !codepoint.is_empty() {
                let Some(codepoint) = codepoint.strip_prefix("U+") else {
                    return Err(Error::ParseError(
                        self.glyphinfo_file.clone(),
                        format!("Unintelligible codepoint at line {i}"),
                    ));
                };
                Some(u32::from_str_radix(codepoint, 16).map_err(|e| {
                    Error::ParseError(
                        self.glyphinfo_file.clone(),
                        format!("Unintelligible codepoint at line {i}: {e}"),
                    )
                })?)
            } else {
                None
            };
            let glyph_file = self.glyph_file(glyph_name.clone());
            if !glyph_file.is_file() {
                return Err(Error::FileExpected(glyph_file));
            }
            if self
                .glyph_info
                .insert(glyph_name.clone(), (glyph_file, codepoint))
                .is_some()
            {
                return Err(Error::ParseError(
                    self.glyphinfo_file.clone(),
                    format!("Multiple definitions of '{glyph_name}'"),
                ));
            }
        }
        Ok(())
    }

    fn glyph_state(&mut self) -> Result<HashMap<GlyphName, StateSet>, Error> {
        let mut glyph_state = HashMap::default();
        for (glyph_name, (glyph_file, _)) in self.glyph_info.iter() {
            let mut tracker = StateSet::new();
            tracker.track_file(glyph_file)?;
            if glyph_state.insert(glyph_name.clone(), tracker).is_some() {
                return Err(Error::ParseError(
                    self.glyphinfo_file.clone(),
                    format!("Multiple definitions of '{glyph_name}'"),
                ));
            }
        }
        Ok(glyph_state)
    }
}

impl Source for FontraIrSource {
    fn inputs(&mut self) -> Result<fontir::source::Input, fontir::error::Error> {
        let static_metadata = self.static_metadata_state()?;
        self.load_glyphinfo()?;
        let glyphs = self.glyph_state()?;

        // Not sure how fontra features work
        let features = StateSet::new();

        // fontinfo.plist spans static metadata and global metrics.
        // Just use the same change detection for both.
        Ok(Input {
            static_metadata: static_metadata.clone(),
            global_metrics: static_metadata,
            glyphs,
            features,
        })
    }

    fn create_static_metadata_work(
        &self,
        _input: &fontir::source::Input,
    ) -> Result<Box<fontir::orchestration::IrWork>, fontir::error::Error> {
        let _font_data = FontData::from_file(&self.fontdata_file)?;
        todo!()
    }

    fn create_global_metric_work(
        &self,
        _input: &fontir::source::Input,
    ) -> Result<Box<fontir::orchestration::IrWork>, fontir::error::Error> {
        todo!()
    }

    fn create_glyph_ir_work(
        &self,
        _glyph_names: &indexmap::IndexSet<fontdrasil::types::GlyphName>,
        _input: &fontir::source::Input,
    ) -> Result<Vec<Box<fontir::orchestration::IrWork>>, fontir::error::Error> {
        todo!()
    }

    fn create_feature_ir_work(
        &self,
        _input: &fontir::source::Input,
    ) -> Result<Box<fontir::orchestration::IrWork>, fontir::error::Error> {
        todo!()
    }

    fn create_kerning_group_ir_work(
        &self,
        _input: &fontir::source::Input,
    ) -> Result<Box<fontir::orchestration::IrWork>, fontir::error::Error> {
        todo!()
    }

    fn create_kerning_instance_ir_work(
        &self,
        _input: &fontir::source::Input,
        _at: fontdrasil::coords::NormalizedLocation,
    ) -> Result<Box<fontir::orchestration::IrWork>, fontir::error::Error> {
        todo!()
    }
}

/// serde type used to load font-data
#[derive(Debug, Clone, Deserialize)]
struct FontData {
    #[serde(rename = "unitsPerEm")]
    _units_per_em: u16,
    #[serde(default)]
    _axes: Vec<FontraAxis>,
}

impl FontData {
    fn from_file(p: &Path) -> Result<Self, Error> {
        let raw = fs::read_to_string(p).map_err(Error::IoError)?;
        serde_json::from_str(&raw).map_err(|e| Error::ParseError(p.to_path_buf(), format!("{e}")))
    }
}

#[derive(Debug, Clone, Deserialize)]
struct FontraAxis {
    _name: String,
    _tag: Tag,
    #[serde(rename = "minValue")]
    _min_value: f64,
    #[serde(rename = "defaultValue")]
    _default_value: f64,
    #[serde(rename = "maxValue")]
    _max_value: f64,
    #[serde(default)]
    _mapping: Vec<[f64; 2]>,
}

#[cfg(test)]
mod tests {
    use std::path::Path;

    use fontdrasil::types::GlyphName;
    use pretty_assertions::assert_eq;

    use super::*;

    fn testdata_dir() -> PathBuf {
        let dir = Path::new("../resources/testdata/fontra");
        assert!(dir.is_dir());
        dir.to_path_buf()
    }

    fn axis_tuples(font_data: &FontData) -> Vec<(&str, Tag, f64, f64, f64)> {
        font_data
            ._axes
            .iter()
            .map(|a| {
                (
                    a._name.as_str(),
                    a._tag,
                    a._min_value,
                    a._default_value,
                    a._max_value,
                )
            })
            .collect::<Vec<_>>()
    }

    #[test]
    fn glyph_names_of_minimal() {
        let mut source = FontraIrSource::new(testdata_dir().join("minimal.fontra")).unwrap();
        let inputs = source.inputs().unwrap();
        assert_eq!(
            vec![GlyphName::new(".notdef")],
            inputs.glyphs.keys().cloned().collect::<Vec<_>>()
        );
    }

    #[test]
    fn glyph_names_of_2glyphs() {
        let mut source = FontraIrSource::new(testdata_dir().join("2glyphs.fontra")).unwrap();
        let inputs = source.inputs().unwrap();
        let mut glyph_names = inputs.glyphs.keys().cloned().collect::<Vec<_>>();
        glyph_names.sort();
        assert_eq!(
            vec![GlyphName::new(".notdef"), GlyphName::new("u20089")],
            glyph_names
        );
    }

    #[test]
    fn fontdata_of_minimal() {
        let font_data =
            FontData::from_file(&testdata_dir().join("minimal.fontra/font-data.json")).unwrap();
        assert_eq!(1000, font_data._units_per_em);
        assert_eq!(
            vec![("Weight", Tag::from_be_bytes(*b"wght"), 200.0, 200.0, 900.0),],
            axis_tuples(&font_data)
        );
    }

    #[test]
    fn fontdata_of_2glyphs() {
        let font_data =
            FontData::from_file(&testdata_dir().join("2glyphs.fontra/font-data.json")).unwrap();
        assert_eq!(1000, font_data._units_per_em);
        assert_eq!(
            vec![
                ("Weight", Tag::from_be_bytes(*b"wght"), 200.0, 200.0, 900.0),
                ("Width", Tag::from_be_bytes(*b"wdth"), 50.0, 100.0, 125.0)
            ],
            axis_tuples(&font_data)
        );
        let wght = font_data
            ._axes
            .iter()
            .find(|a| a._tag == Tag::from_be_bytes(*b"wght"))
            .unwrap();
        assert_eq!(
            vec![[200.0, 0.0], [300.018, 0.095], [900.0, 1.0]],
            wght._mapping
        );
    }
}
