//! Generic model of font sources.

use std::{
    collections::HashMap,
    fs,
    path::PathBuf,
};

use indexmap::IndexSet;
use log::debug;
use serde::{Deserialize, Serialize};

use crate::{
    error::{Error, WorkError},
    orchestration::Context,
    stateset::StateSet,
};

/// A unit of work safe to run in parallel
///
/// Naively you'd think we'd just return FnOnce + Send but that didn't want to compile
/// See <https://github.com/rust-lang/rust/issues/29625>.
///
/// Data produced by work is written into [Context].
pub trait Work {
    fn exec(&self, context: &Context) -> Result<(), WorkError>;
}

/// Destroy a file, such as the IR for a deleted glyph
pub struct DeleteWork {
    path: PathBuf,
}

impl DeleteWork {
    pub fn create(path: PathBuf) -> Box<dyn Work + Send> {
        Box::from(DeleteWork { path })
    }
}

impl Work for DeleteWork {
    fn exec(&self, _: &Context) -> Result<(), WorkError> {
        debug!("Delete {:#?}", self.path);
        if self.path.exists() {
            fs::remove_file(&self.path).map_err(WorkError::IoError)?
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
    fn create_static_metadata_work(&self, input: &Input) -> Result<Box<dyn Work + Send>, Error>;

    /// Create a function that could be called to generate IR for glyphs.
    ///
    /// Batched because some formats require IO to figure out the work.
    /// Expected to return a Vec aligned with the glyph_names input. That is,
    /// result vec nth entry is the work for the nth glyph name.
    ///
    /// When run work should update [Context] with [crate::ir::Glyph] for the glyph name.
    fn create_glyph_ir_work(
        &self,
        glyph_names: &IndexSet<&str>,
        input: &Input,
    ) -> Result<Vec<Box<dyn Work + Send>>, Error>;

    /// Create a function that could be called to generate or identify fea file(s).
    ///
    /// When run work should update [Context] with [crate::ir::Features].
    fn create_feature_ir_work(&self, input: &Input) -> Result<Box<dyn Work + Send>, Error>;
}

/// The files (in future non-file sources?) that drive various parts of IR
#[derive(Serialize, Deserialize, Debug, Default, Clone, PartialEq)]
pub struct Input {
    /// Font-wide metadata, such as upem. Things that should trigger a non-incremental build if they change.
    /// Files that contribute to [crate::ir::StaticMetadata].
    pub static_metadata: StateSet,

    /// The input(s) that inform glyph IR construction, grouped by gyph name
    pub glyphs: HashMap<String, StateSet>,

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
        glyphs.insert("space".to_string(), glyph);

        let mut features = StateSet::new();
        features
            .track_file(&write(temp_dir, Path::new("features.fea"), "blah"))
            .unwrap();

        Input {
            static_metadata: font_info,
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
