use std::{path::Path, sync::Arc};

use fontdrasil::{coords::NormalizedLocation, orchestration::Work};
use fontir::{
    error::Error,
    orchestration::{Context, IrWork, WorkId},
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

    fn create_static_metadata_work(&self) -> Result<Box<IrWork>, Error> {
        Ok(Box::new(StaticMetadataWork {
            font_data: self.font_data.clone(),
        }))
    }

    fn create_global_metric_work(&self) -> Result<Box<IrWork>, Error> {
        Ok(Box::new(NoopWork(WorkId::GlobalMetrics)))
    }

    fn create_glyph_ir_work(&self) -> Result<Vec<Box<IrWork>>, Error> {
        Ok(Vec::new())
    }

    fn create_feature_ir_work(&self) -> Result<Box<IrWork>, Error> {
        Ok(Box::new(NoopWork(WorkId::Features)))
    }

    fn create_kerning_locations_ir_work(&self) -> Result<Box<IrWork>, Error> {
        Ok(Box::new(NoopWork(WorkId::KerningLocations)))
    }

    fn create_kerning_instance_ir_work(
        &self,
        at: NormalizedLocation,
    ) -> Result<Box<IrWork>, Error> {
        Ok(Box::new(NoopWork(WorkId::KernInstance(at))))
    }

    fn create_color_palette_work(&self) -> Result<Box<IrWork>, Error> {
        Ok(Box::new(NoopWork(WorkId::ColorPalettes)))
    }

    fn create_color_glyphs_work(&self) -> Result<Box<IrWork>, Error> {
        Ok(Box::new(NoopWork(WorkId::PaintGraph)))
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

/// A work that produces nothing.
#[derive(Debug)]
struct NoopWork(WorkId);

impl Work<Context, WorkId, Error> for NoopWork {
    fn id(&self) -> WorkId {
        self.0.clone()
    }

    fn exec(&self, _context: &Context) -> Result<(), Error> {
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use fontdrasil::{
        orchestration::{Access, AccessBuilder},
        types::GlyphName,
    };
    use fontir::orchestration::Flags;
    use pretty_assertions::assert_eq;
    use write_fonts::types::Tag;

    use crate::test::testdata_dir;

    use super::*;

    fn context_for(fontra_dir: &str) -> (FontraIrSource, Context) {
        let source = FontraIrSource::new(&testdata_dir().join(fontra_dir)).unwrap();
        (source, Context::new_root(Flags::empty(), None))
    }

    #[test]
    fn compile_raqq() {
        let (source, context) = context_for("Raqq.fontra");

        let task_context = context.copy_for_work(
            Access::None,
            AccessBuilder::new()
                .variant(WorkId::StaticMetadata)
                .variant(WorkId::PreliminaryGlyphOrder)
                .variant(WorkId::PreliminaryGdefCategories)
                .build(),
        );
        source
            .create_static_metadata_work()
            .unwrap()
            .exec(&task_context)
            .unwrap();

        let static_metadata = context.static_metadata.get();
        assert_eq!(800, static_metadata.units_per_em);
        assert_eq!(
            vec![Tag::new(b"SPAC"), Tag::new(b"MSHQ")],
            static_metadata
                .axes
                .iter()
                .map(|a| a.tag)
                .collect::<Vec<_>>()
        );

        let glyph_order = context.preliminary_glyph_order.get();
        assert!(glyph_order.contains(&GlyphName::new(".notdef")));
        assert!(glyph_order.contains(&GlyphName::new("beh-ar")));
    }

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
