use std::path::PathBuf;

use fontir::source::Source;

pub struct FontraIrSource {
    fontra_dir: PathBuf,
}

impl FontraIrSource {
    pub fn new(fontra_dir: PathBuf) -> Self {
        FontraIrSource { fontra_dir }
    }
}

impl Source for FontraIrSource {
    fn inputs(&mut self) -> Result<fontir::source::Input, fontir::error::Error> {
        todo!()
    }

    fn create_static_metadata_work(
        &self,
        _input: &fontir::source::Input,
    ) -> Result<Box<fontir::orchestration::IrWork>, fontir::error::Error> {
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
