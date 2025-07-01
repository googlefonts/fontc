use std::{
    collections::BTreeMap,
    fs::File,
    io::{BufRead, BufReader},
    path::{Path, PathBuf},
    sync::Arc,
};

use fontdrasil::{orchestration::Work, types::GlyphName};
use fontir::{
    error::{BadSource, BadSourceKind, Error},
    ir::StaticMetadata,
    orchestration::{Context, WorkId},
    source::Source,
};
use log::debug;

use crate::{
    fontra::{self, FontraFontData},
    toir::to_ir_static_metadata,
};

pub struct FontraIrSource {
    fontdata_file: PathBuf,
    glyph_info: Arc<BTreeMap<GlyphName, (PathBuf, Vec<u32>)>>,
}

fn parse_glyph_info(fontra_dir: &Path) -> Result<BTreeMap<GlyphName, (PathBuf, Vec<u32>)>, Error> {
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
        let glyph_file = fontra::glyph_file(&glyph_dir, glyph_name.clone());
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

impl Source for FontraIrSource {
    fn new(fontra_dir: &Path) -> Result<Self, Error> {
        let fontdata_file = fontra_dir.join("font-data.json");
        if !fontdata_file.is_file() {
            return Err(BadSource::new(fontdata_file, BadSourceKind::ExpectedFile).into());
        }

        let glyph_info = parse_glyph_info(fontra_dir)?;

        Ok(FontraIrSource {
            fontdata_file,
            glyph_info: Arc::new(glyph_info),
        })
    }

    fn create_static_metadata_work(
        &self,
    ) -> Result<Box<fontir::orchestration::IrWork>, fontir::error::Error> {
        FontraFontData::from_file(&self.fontdata_file)?;
        Ok(Box::new(StaticMetadataWork {
            fontdata_file: self.fontdata_file.clone(),
            glyph_info: self.glyph_info.clone(),
        }))
    }

    fn create_global_metric_work(
        &self,
    ) -> Result<Box<fontir::orchestration::IrWork>, fontir::error::Error> {
        todo!()
    }

    fn create_glyph_ir_work(
        &self,
    ) -> Result<Vec<Box<fontir::orchestration::IrWork>>, fontir::error::Error> {
        todo!()
    }

    fn create_feature_ir_work(
        &self,
    ) -> Result<Box<fontir::orchestration::IrWork>, fontir::error::Error> {
        todo!()
    }

    fn create_kerning_group_ir_work(
        &self,
    ) -> Result<Box<fontir::orchestration::IrWork>, fontir::error::Error> {
        todo!()
    }

    fn create_kerning_instance_ir_work(
        &self,
        _at: fontdrasil::coords::NormalizedLocation,
    ) -> Result<Box<fontir::orchestration::IrWork>, fontir::error::Error> {
        todo!()
    }

    fn create_color_palette_work(
        &self,
    ) -> Result<Box<fontir::orchestration::IrWork>, fontir::error::Error> {
        todo!()
    }

    fn create_paint_graph_work(
        &self,
    ) -> Result<Box<fontir::orchestration::IrWork>, fontir::error::Error> {
        todo!()
    }
}

#[derive(Debug)]
struct StaticMetadataWork {
    fontdata_file: PathBuf,
    glyph_info: Arc<BTreeMap<GlyphName, (PathBuf, Vec<u32>)>>,
}

fn create_static_metadata(fontdata_file: &Path) -> Result<StaticMetadata, Error> {
    debug!("Static metadata for {fontdata_file:#?}");
    let font_data = FontraFontData::from_file(fontdata_file)?;
    to_ir_static_metadata(&font_data)
}

impl Work<Context, WorkId, Error> for StaticMetadataWork {
    fn id(&self) -> WorkId {
        WorkId::StaticMetadata
    }

    fn also_completes(&self) -> Vec<WorkId> {
        vec![WorkId::PreliminaryGlyphOrder]
    }

    fn exec(&self, context: &Context) -> Result<(), Error> {
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
        let source = FontraIrSource::new(&testdata_dir().join("minimal.fontra")).unwrap();
        assert_eq!(
            Arc::new(
                vec![(
                    GlyphName::new(".notdef"),
                    (
                        testdata_dir().join("minimal.fontra/glyphs/%2Enotdef.json"),
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
        let source = FontraIrSource::new(&testdata_dir().join("2glyphs.fontra")).unwrap();
        assert_eq!(
            Arc::new(
                [
                    (
                        GlyphName::new(".notdef"),
                        (
                            testdata_dir().join("2glyphs.fontra/glyphs/%2Enotdef.json"),
                            vec![]
                        )
                    ),
                    (
                        GlyphName::new("u20089"),
                        (
                            testdata_dir().join("2glyphs.fontra/glyphs/u20089.json"),
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
    fn glyph_info_0_1_n_codepoints() {
        let source = FontraIrSource::new(&testdata_dir().join("codepoints.fontra")).unwrap();
        assert_eq!(
            vec![
                (".notdef", vec![]),
                ("A", vec![0x0041, 0x0061]),
                ("Aacute", vec![0x00C1, 0x00E1]),
                (
                    "handshake_mediumlight_medium",
                    vec![0x1FAF1, 0x1F3FC, 0x200D, 0x1FAF2, 0x1F3FD]
                ),
                ("space", vec![0x0020]),
            ]
            .into_iter()
            .map(|(name, codepoints)| (GlyphName::new(name), codepoints))
            .collect::<Vec<_>>(),
            source
                .glyph_info
                .iter()
                .map(|(name, (_, codepoints))| (name.clone(), codepoints.clone()))
                .collect::<Vec<_>>()
        )
    }
}
