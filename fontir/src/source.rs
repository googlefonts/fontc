//! Generic model of font sources.

use std::{
    collections::{HashMap, HashSet},
    fs,
    path::{Path, PathBuf},
};

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
pub trait Work: Send {
    fn exec(&self, context: &Context) -> Result<(), WorkError>;
}

/// Destroy a file, such as the IR for a deleted glyph
pub struct DeleteWork {
    path: PathBuf,
}

impl DeleteWork {
    pub fn create(path: PathBuf) -> Box<dyn Work> {
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
    fn create_static_metadata_work(&self, context: &Context) -> Result<Box<dyn Work>, Error>;

    /// Create a function that could be called to generate IR for glyphs.
    ///
    /// Batched because some formats require IO to figure out the work.
    fn create_glyph_ir_work(
        &self,
        glyph_names: &HashSet<&str>,
        context: &Context,
    ) -> Result<Vec<Box<dyn Work>>, Error>;
}

/// Where does IR go anyway?
#[derive(Debug, Clone)]
pub struct Paths {
    build_dir: PathBuf,
    glyph_ir_dir: PathBuf,
    ir_input_file: PathBuf,
}

impl Paths {
    pub fn new(build_dir: &Path) -> Paths {
        let build_dir = build_dir.to_path_buf();
        let glyph_ir_dir = build_dir.join("glyph_ir");
        let ir_input_file = build_dir.join("irinput.yml");
        Paths {
            build_dir,
            glyph_ir_dir,
            ir_input_file,
        }
    }

    pub fn build_dir(&self) -> &Path {
        &self.build_dir
    }

    pub fn ir_input_file(&self) -> &Path {
        &self.ir_input_file
    }

    pub fn static_metadata_ir_file(&self) -> PathBuf {
        self.build_dir.join("static_metadata.yml")
    }

    pub fn glyph_ir_dir(&self) -> &Path {
        &self.glyph_ir_dir
    }

    pub fn glyph_ir_file(&self, glyph_name: &str) -> PathBuf {
        // TODO handle names that are invalid for the filesystem
        // Ref https://github.com/unified-font-object/ufo-spec/issues/164
        self.glyph_ir_dir.join(glyph_name.to_owned() + ".yml")
    }
}

/// The files (in future non-file sources?) that drive various parts of IR
#[derive(Serialize, Deserialize, Debug, Default, Clone, PartialEq)]
pub struct Input {
    /// Font-wide metadata, such as upem. Things that should trigger a non-incremental build if they change.
    pub global_metadata: StateSet,
    /// The input(s) that inform glyph construction, grouped by gyph name
    pub glyphs: HashMap<String, StateSet>,
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

        Input {
            global_metadata: font_info,
            glyphs,
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
