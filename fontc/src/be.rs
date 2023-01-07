//! Backend work for fontc. Likely to move to own crate in time.

use std::path::PathBuf;

use fea_rs::GlyphName;
use fontir::{
    error::WorkError,
    source::{Paths, Work},
};
use log::warn;

use crate::Error;

pub fn create_feature_work(paths: &Paths) -> Result<Box<dyn Work + Send>, Error> {
    Ok(Box::from(FeatureWork {
        fea_file: paths.feature_ir_file(),
        build_dir: paths.build_dir().to_path_buf(),
    }))
}

struct FeatureWork {
    fea_file: PathBuf,
    build_dir: PathBuf,
}

impl Work for FeatureWork {
    fn exec(
        &self,
        context: &fontir::orchestration::Context,
    ) -> Result<(), fontir::error::WorkError> {
        let static_metadata = context.get_static_metadata();
        let glyph_map = static_metadata
            .glyph_order
            .iter()
            .map(Into::<GlyphName>::into)
            .collect();
        let _parsed = fea_rs::parse_root_file(
            &self.fea_file,
            Some(&glyph_map),
            Some(self.build_dir.clone()),
        )
        .map_err(|e| WorkError::FeaError(format!("{:?} parsing {:?}", e, self.fea_file)))?;
        warn!("ARE YOU NOT PARSED?!");
        Err(WorkError::FeaError(String::from(
            "TODO: anything useful with fea",
        )))
    }
}
