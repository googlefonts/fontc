//! Generic model of font sources.

use std::{fs, path::PathBuf};

use log::debug;

use fontdrasil::{coords::NormalizedLocation, orchestration::Work, types::GlyphName};

use crate::{
    error::Error,
    orchestration::{Context, IrWork, WorkId},
};

/// Destroy a file, such as the IR for a deleted glyph
#[derive(Debug)]
pub struct DeleteWork {
    glyph_name: GlyphName,
}

impl DeleteWork {
    pub fn create(glyph_name: GlyphName) -> Box<IrWork> {
        Box::new(DeleteWork { glyph_name })
    }
}

impl Work<Context, WorkId, Error> for DeleteWork {
    fn id(&self) -> WorkId {
        WorkId::GlyphIrDelete(self.glyph_name.clone())
    }

    fn exec(&self, context: &Context) -> Result<(), Error> {
        let path = context.persistent_storage.paths.target_file(&self.id());
        debug!("Delete {:#?}", path);
        if path.exists() {
            fs::remove_file(&path).map_err(|source| Error::DeleteFailed { path, source })?
        }
        Ok(())
    }
}

/// Manipulations on some sort of font source.
pub trait Source {
    /// path is to the root entry, e.g. .glyphs file, .designspace, etc
    fn new(root: PathBuf) -> Result<Self, Error>
    where
        Self: Sized;

    /// Create a function that could be called to generate [crate::ir::StaticMetadata].
    ///
    /// When run work should update [Context] with new [crate::ir::StaticMetadata].
    fn create_static_metadata_work(&self) -> Result<Box<IrWork>, Error>;

    /// Create a function that could be called to generate [crate::ir::StaticMetadata].
    ///
    /// When run work should update [Context] with new [crate::ir::GlobalMetrics].
    fn create_global_metric_work(&self) -> Result<Box<IrWork>, Error>;

    /// Create a function that could be called to generate IR for glyphs.
    ///
    /// Batched because some formats require IO to figure out the work.
    /// Expected to return a Vec aligned with the glyph_names input. That is,
    /// result vec nth entry is the work for the nth glyph name.
    ///
    /// When run work should update [Context] with [crate::ir::Glyph] and [crate::ir::Anchor]
    /// for the glyph name.
    fn create_glyph_ir_work(&self) -> Result<Vec<Box<IrWork>>, Error>;

    /// Create a function that could be called to generate or identify fea file(s).
    ///
    /// When run work should update [Context] with [crate::ir::FeaturesSource].
    fn create_feature_ir_work(&self) -> Result<Box<IrWork>, Error>;

    /// Create a function that could be called to produce kerning groups.
    ///
    /// When run work should update [Context] with [crate::ir::KerningGroups].
    fn create_kerning_group_ir_work(&self) -> Result<Box<IrWork>, Error>;

    /// Create a function that could be called to generate or identify kerning for a location.
    ///
    /// When run work should update [Context] with [crate::ir::KerningInstance].
    fn create_kerning_instance_ir_work(&self, at: NormalizedLocation)
        -> Result<Box<IrWork>, Error>;
}
