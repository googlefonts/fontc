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
    glyph_info: Arc<BTreeMap<GlyphName, (PathBuf, Option<u32>)>>,
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
            let glyph_file = fontra::glyph_file(&self.glyph_dir, glyph_name.clone());
            if !glyph_file.is_file() {
                return Err(Error::FileExpected(glyph_file));
            }

            if glyph_info
                .insert(glyph_name.clone(), (glyph_file, codepoint))
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
        let _font_data = FontraFontData::from_file(&self.fontdata_file)?;
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
    glyph_info: Arc<BTreeMap<GlyphName, (PathBuf, Option<u32>)>>,
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
}
