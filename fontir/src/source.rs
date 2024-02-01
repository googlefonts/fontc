//! Generic model of font sources.

use std::{collections::HashMap, fs};

use indexmap::IndexSet;
use log::debug;
use serde::{Deserialize, Serialize};

use fontdrasil::{coords::NormalizedLocation, orchestration::Work, types::GlyphName};

use crate::{
    error::{Error, WorkError},
    orchestration::{Context, IrWork, WorkId},
    stateset::StateSet,
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

impl Work<Context, WorkId, WorkError> for DeleteWork {
    fn id(&self) -> WorkId {
        WorkId::GlyphIrDelete(self.glyph_name.clone())
    }

    fn exec(&self, context: &Context) -> Result<(), WorkError> {
        let path = context.persistent_storage.paths.target_file(&self.id());
        debug!("Delete {:#?}", path);
        if path.exists() {
            fs::remove_file(&path).map_err(WorkError::IoError)?
        }
        Ok(())
    }
}

/// Manipulations on some sort of font source.
pub trait Source {
    /// Resolve a source to a set of files and their dependencies.
    ///
    /// Mut to permit caching.
    fn inputs(&mut self) -> Result<Input, Error>;

    /// Create a function that could be called to generate [crate::ir::StaticMetadata].
    ///
    /// When run work should update [Context] with new [crate::ir::StaticMetadata].
    fn create_static_metadata_work(&self, input: &Input) -> Result<Box<IrWork>, Error>;

    /// Create a function that could be called to generate [crate::ir::StaticMetadata].
    ///
    /// When run work should update [Context] with new [crate::ir::GlobalMetrics].
    fn create_global_metric_work(&self, input: &Input) -> Result<Box<IrWork>, Error>;

    /// Create a function that could be called to generate IR for glyphs.
    ///
    /// Batched because some formats require IO to figure out the work.
    /// Expected to return a Vec aligned with the glyph_names input. That is,
    /// result vec nth entry is the work for the nth glyph name.
    ///
    /// When run work should update [Context] with [crate::ir::Glyph] and [crate::ir::Anchor]
    /// for the glyph name.
    fn create_glyph_ir_work(
        &self,
        glyph_names: &IndexSet<GlyphName>,
        input: &Input,
    ) -> Result<Vec<Box<IrWork>>, Error>;

    /// Create a function that could be called to generate or identify fea file(s).
    ///
    /// When run work should update [Context] with [crate::ir::FeaturesSource].
    fn create_feature_ir_work(&self, input: &Input) -> Result<Box<IrWork>, Error>;

    /// Create a function that could be called to produce kerning groups.
    ///
    /// When run work should update [Context] with [crate::ir::KerningGroups].
    fn create_kerning_group_ir_work(&self, input: &Input) -> Result<Box<IrWork>, Error>;

    /// Create a function that could be called to generate or identify kerning for a location.
    ///
    /// When run work should update [Context] with [crate::ir::KerningInstance].
    fn create_kerning_instance_ir_work(
        &self,
        input: &Input,
        at: NormalizedLocation,
    ) -> Result<Box<IrWork>, Error>;
}

/// The files (in future non-file sources?) that drive various parts of IR
#[derive(Serialize, Deserialize, Debug, Default, Clone, PartialEq)]
pub struct Input {
    /// Font-wide metadata, such as upem. Things that should trigger a non-incremental build if they change.
    /// Files that contribute to [crate::ir::StaticMetadata].
    pub static_metadata: StateSet,

    /// Font-wide metrics, such as ascender.
    /// Files that contribute to [crate::ir::GlobalMetrics].
    pub global_metrics: StateSet,

    /// The input(s) that inform glyph IR construction, grouped by gyph name
    pub glyphs: HashMap<GlyphName, StateSet>,

    /// The input(s) that inform feature IR construction
    pub features: StateSet,
}

impl Input {
    pub fn new() -> Input {
        Default::default()
    }
}

#[cfg(test)]
mod tests {
    use std::{
        collections::HashMap,
        fs,
        path::{Path, PathBuf},
    };

    use tempfile::{tempdir, TempDir};

    use crate::stateset::StateSet;

    use super::Input;

    fn write(temp_dir: &TempDir, path: &Path, content: &str) -> PathBuf {
        let path = temp_dir.path().join(path);
        fs::write(&path, content).unwrap();
        path
    }

    fn create_test_input(temp_dir: &TempDir) -> Input {
        let mut font_info = StateSet::new();
        font_info
            .track_file(&write(temp_dir, Path::new("some.designspace"), "blah"))
            .unwrap();

        let mut glyph = StateSet::new();
        glyph
            .track_file(&write(temp_dir, Path::new("regular.space.glif"), "blah"))
            .unwrap();
        glyph
            .track_file(&write(temp_dir, Path::new("bold.space.glif"), "blah"))
            .unwrap();

        let mut glyphs = HashMap::new();
        glyphs.insert("space".into(), glyph);

        let mut features = StateSet::new();
        features
            .track_file(&write(temp_dir, Path::new("features.fea"), "blah"))
            .unwrap();

        Input {
            static_metadata: font_info.clone(),
            global_metrics: font_info,
            glyphs,
            features,
        }
    }

    #[test]
    fn read_write_yaml() {
        let temp_dir = tempdir().unwrap();
        let ir_input = create_test_input(&temp_dir);
        let yml = serde_yaml::to_string(&ir_input).unwrap();
        let restored: Input = serde_yaml::from_str(&yml).unwrap();
        assert_eq!(ir_input, restored);
    }

    #[test]
    fn read_write_bincode() {
        let temp_dir = tempdir().unwrap();
        let ir_input = create_test_input(&temp_dir);
        let bc = bincode::serialize(&ir_input).unwrap();
        let restored: Input = bincode::deserialize(&bc).unwrap();
        assert_eq!(ir_input, restored);
    }
}
