use std::{path::Path, sync::Arc};

use fontdrasil::orchestration::Work;
use fontir::{
    error::Error,
    orchestration::{Context, WorkId},
    source::Source,
};
use log::debug;

use crate::{fontra::Font, toir::to_ir_static_metadata};

pub struct FontraIrSource {
    font_data: Arc<Font>,
}

impl Source for FontraIrSource {
    fn new(fontra_dir: &Path) -> Result<Self, Error> {
        let font_data = Font::load(fontra_dir)?;

        Ok(FontraIrSource {
            font_data: Arc::new(font_data),
        })
    }

    fn create_static_metadata_work(
        &self,
    ) -> Result<Box<fontir::orchestration::IrWork>, fontir::error::Error> {
        Ok(Box::new(StaticMetadataWork {
            font_data: self.font_data.clone(),
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

    fn create_kerning_locations_ir_work(
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

    fn create_color_glyphs_work(
        &self,
    ) -> Result<Box<fontir::orchestration::IrWork>, fontir::error::Error> {
        todo!()
    }
}

#[derive(Debug)]
struct StaticMetadataWork {
    font_data: Arc<Font>,
}

impl Work<Context, WorkId, Error> for StaticMetadataWork {
    fn id(&self) -> WorkId {
        WorkId::StaticMetadata
    }

    fn also_completes(&self) -> Vec<WorkId> {
        vec![WorkId::PreliminaryGlyphOrder]
    }

    fn exec(&self, context: &Context) -> Result<(), Error> {
        debug!(
            "Static metadata for {}",
            self.font_data
                .font_info
                .family_name
                .as_deref()
                .unwrap_or("<nameless family>")
        );
        context
            .preliminary_glyph_order
            .set(self.font_data.glyph_map.keys().cloned().collect());
        context
            .static_metadata
            .set(to_ir_static_metadata(&self.font_data)?);
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use fontdrasil::types::GlyphName;
    use pretty_assertions::assert_eq;

    use crate::test::testdata_dir;

    use super::*;

    fn glyph_map(fontra_dir: &str) -> Vec<(GlyphName, Vec<u32>)> {
        let source = FontraIrSource::new(&testdata_dir().join(fontra_dir)).unwrap();
        source
            .font_data
            .glyph_map
            .iter()
            .map(|(name, codepoints)| (name.clone(), codepoints.clone()))
            .collect()
    }

    #[test]
    fn glyph_map_of_minimal() {
        assert_eq!(
            vec![(GlyphName::new(".notdef"), vec![])],
            glyph_map("minimal.fontra")
        );
    }

    #[test]
    fn glyph_map_of_2glyphs() {
        assert_eq!(
            vec![
                (GlyphName::new(".notdef"), vec![]),
                (GlyphName::new("u20089"), vec![0x20089]),
            ],
            glyph_map("2glyphs.fontra")
        );
    }

    #[test]
    fn glyph_map_0_1_n_codepoints() {
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
            glyph_map("codepoints.fontra"),
        )
    }
}
