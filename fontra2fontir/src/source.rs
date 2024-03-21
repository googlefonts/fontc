use std::{
    collections::{BTreeMap, HashMap},
    fs::File,
    io::{BufRead, BufReader},
    path::{Path, PathBuf},
    sync::Arc,
};

use fontdrasil::{orchestration::Work, types::GlyphName};
use fontir::{
    error::{Error, WorkError},
    ir::StaticMetadata,
    orchestration::{Context, WorkId},
    source::{Input, Source},
    stateset::StateSet,
};
use log::debug;

use crate::{
    fontra::{self, FontraFontData},
    toir::to_ir_static_metadata,
};

pub struct FontraIrSource {
    fontdata_file: PathBuf,
    glyphinfo_file: PathBuf,
    glyph_dir: PathBuf,
    glyph_info: Arc<BTreeMap<GlyphName, (PathBuf, Vec<u32>)>>,
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

    // When things like upem may have changed forget incremental and rebuild the whole thing
    fn static_metadata_state(&self) -> Result<StateSet, Error> {
        let mut font_info = StateSet::new();
        font_info.track_file(&self.fontdata_file)?;
        font_info.track_file(&self.glyphinfo_file)?;
        Ok(font_info)
    }

    fn load_glyphinfo(&mut self) -> Result<(), Error> {
        if !self.glyph_info.is_empty() {
            return Ok(());
        }

        // Read the glyph-info file
        let file = File::open(&self.glyphinfo_file).map_err(Error::IoError)?;

        // Example files suggest the first line is just the column headers. Hopefully always :)
        // This file is tool generated so it shouldn't be full of human error. Fail if we don't understand.
        let mut glyph_info = BTreeMap::default();
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
            let codepoints = parts[1]
                .split(",")
                .filter_map(|codepoint| {
                    let codepoint = codepoint.trim();
                    if codepoint.is_empty() {
                        return None;
                    }
                    let Some(codepoint) = codepoint.strip_prefix("U+") else {
                        return Some(Err(Error::ParseError(
                            self.glyphinfo_file.clone(),
                            format!("Unintelligible codepoint {codepoint:?} at line {i}"),
                        )));
                    };
                    let r = u32::from_str_radix(codepoint, 16).map_err(|e| {
                        Error::ParseError(
                            self.glyphinfo_file.clone(),
                            format!("Unintelligible codepoint {codepoint:?} at line {i}: {e}"),
                        )
                    });
                    Some(r)
                })
                .collect::<Result<Vec<_>, _>>()?;
            let glyph_file = fontra::glyph_file(&self.glyph_dir, glyph_name.clone());
            if !glyph_file.is_file() {
                return Err(Error::FileExpected(glyph_file));
            }

            if glyph_info
                .insert(glyph_name.clone(), (glyph_file, codepoints))
                .is_some()
            {
                return Err(Error::ParseError(
                    self.glyphinfo_file.clone(),
                    format!("Multiple definitions of '{glyph_name}'"),
                ));
            }
        }
        self.glyph_info = Arc::new(glyph_info);
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
        FontraFontData::from_file(&self.fontdata_file)?;
        Ok(Box::new(StaticMetadataWork {
            fontdata_file: self.fontdata_file.clone(),
            glyph_info: self.glyph_info.clone(),
        }))
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

#[derive(Debug)]
struct StaticMetadataWork {
    fontdata_file: PathBuf,
    glyph_info: Arc<BTreeMap<GlyphName, (PathBuf, Vec<u32>)>>,
}

fn create_static_metadata(fontdata_file: &Path) -> Result<StaticMetadata, WorkError> {
    debug!("Static metadata for {:#?}", fontdata_file);
    let font_data = FontraFontData::from_file(fontdata_file)
        .map_err(|e| WorkError::ParseError(fontdata_file.to_path_buf(), format!("{e}")))?;
    to_ir_static_metadata(&font_data)
}

impl Work<Context, WorkId, WorkError> for StaticMetadataWork {
    fn id(&self) -> WorkId {
        WorkId::StaticMetadata
    }

    fn also_completes(&self) -> Vec<WorkId> {
        vec![WorkId::PreliminaryGlyphOrder]
    }

    fn exec(&self, context: &Context) -> Result<(), WorkError> {
        debug!("Static metadata for {:#?}", self.fontdata_file);
        context
            .preliminary_glyph_order
            .set(self.glyph_info.keys().cloned().collect());
        context
            .static_metadata
            .set(create_static_metadata(&self.fontdata_file)?);
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use fontdrasil::types::GlyphName;
    use pretty_assertions::assert_eq;

    use crate::test::testdata_dir;

    use super::*;

    #[test]
    fn glyph_info_of_minimal() {
        let mut source = FontraIrSource::new(testdata_dir().join("minimal.fontra")).unwrap();
        // populates glyph_info
        source.inputs().unwrap();
        assert_eq!(
            Arc::new(
                vec![(
                    GlyphName::new(".notdef"),
                    (
                        "../resources/testdata/fontra/minimal.fontra/glyphs/%2Enotdef.json".into(),
                        vec![]
                    )
                )]
                .into_iter()
                .collect::<BTreeMap<_, _>>()
            ),
            source.glyph_info
        );
    }

    #[test]
    fn glyph_info_of_2glyphs() {
        let mut source = FontraIrSource::new(testdata_dir().join("2glyphs.fontra")).unwrap();
        // populates glyph_info
        source.inputs().unwrap();
        assert_eq!(
            Arc::new(
                [
                    (
                        GlyphName::new(".notdef"),
                        (
                            "../resources/testdata/fontra/2glyphs.fontra/glyphs/%2Enotdef.json"
                                .into(),
                            vec![]
                        )
                    ),
                    (
                        GlyphName::new("u20089"),
                        (
                            "../resources/testdata/fontra/2glyphs.fontra/glyphs/u20089.json".into(),
                            vec![0x20089]
                        )
                    )
                ]
                .into_iter()
                .collect::<BTreeMap<_, _>>()
            ),
            source.glyph_info
        );
    }

    #[test]
    fn glyph_info_of_mutator_sans() {
        let mut source = FontraIrSource::new(testdata_dir().join("MutatorSans.fontra")).unwrap();
        let inputs = source.inputs().unwrap();
        let error = source.create_static_metadata_work(&inputs).unwrap_err();
        assert_eq!(
            r#"Unable to parse "../resources/testdata/fontra/MutatorSans.fontra/font-data.json": missing field `minValue` at line 41 column 1"#,
            error.to_string()
        );
        // assert_eq!(
        //     vec![
        //         ("A", vec![0x0041, 0x0061]),
        //         ("Aacute", vec![0x00C1, 0x00E1]),
        //         ("Adieresis", vec![0x00C4, 0x00E4]),
        //         ("B", vec![0x0042, 0x0062]),
        //         ("C", vec![0x0043, 0x0063]),
        //         ("D", vec![0x0044, 0x0064]),
        //         ("E", vec![0x0045, 0x0065]),
        //         ("F", vec![0x0046, 0x0066]),
        //         ("G", vec![0x0047, 0x0067]),
        //         ("H", vec![0x0048, 0x0068]),
        //         ("I", vec![0x0049, 0x0069]),
        //         ("I.narrow", vec![]),
        //         ("IJ", vec![]),
        //         ("J", vec![0x004A, 0x006A]),
        //         ("J.narrow", vec![]),
        //         ("K", vec![0x004B, 0x006B]),
        //         ("L", vec![0x004C, 0x006C]),
        //         ("M", vec![0x004D, 0x006D]),
        //         ("N", vec![0x004E, 0x006E]),
        //         ("O", vec![0x004F, 0x006F]),
        //         ("P", vec![0x0050, 0x0070]),
        //         ("Q", vec![0x0051, 0x0071]),
        //         ("R", vec![0x0052, 0x0072]),
        //         ("R.alt", vec![]),
        //         ("S", vec![0x0053, 0x0073]),
        //         ("S.closed", vec![]),
        //         ("T", vec![0x0054, 0x0074]),
        //         ("U", vec![0x0055, 0x0075]),
        //         ("V", vec![0x0056, 0x0076]),
        //         ("W", vec![0x0057, 0x0077]),
        //         ("X", vec![0x0058, 0x0078]),
        //         ("Y", vec![0x0059, 0x0079]),
        //         ("Z", vec![0x005A, 0x007A]),
        //         ("acute", vec![0x00B4]),
        //         ("arrowdown", vec![0x2193]),
        //         ("arrowleft", vec![0x2190]),
        //         ("arrowright", vec![0x2192]),
        //         ("arrowup", vec![0x2191]),
        //         ("colon", vec![0x003A]),
        //         ("comma", vec![0x002C]),
        //         ("dieresis", vec![0x00A8]),
        //         ("dot", vec![0x27D1]),
        //         ("em", vec![]),
        //         ("nestedcomponents", vec![]),
        //         ("nlitest", vec![]),
        //         ("period", vec![0x002E]),
        //         ("quotedblbase", vec![0x201E]),
        //         ("quotedblleft", vec![0x201C]),
        //         ("quotedblright", vec![0x201D]),
        //         ("quotesinglbase", vec![0x201A]),
        //         ("semicolon", vec![0x003B]),
        //         ("space", vec![0x0020]),
        //         ("varcotest1", vec![0xE000]),
        //         ("varcotest2", vec![0xE001])
        //     ]
        //     .into_iter()
        //     .map(|(name, codepoints)| (GlyphName::new(name), codepoints))
        //     .collect::<Vec<_>>(),
        //     static_metadata.glyph_info
        //         .iter()
        //         .map(|(name, (_, codepoints))| (name.clone(), codepoints.clone()))
        //         .collect::<Vec<_>>()
        // )
    }
}
