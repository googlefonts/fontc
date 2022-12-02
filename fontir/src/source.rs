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
    stateset::StateSet,
};

/// A unit of work safe to run in parallel
///
/// Naively you'd think we'd just return FnOnce + Send but that didn't want to compile
/// See <https://github.com/rust-lang/rust/issues/29625>.
pub trait Work<R>: Send {
    fn exec(&self) -> Result<R, WorkError>;
}

/// Destroy a file, such as the IR for a deleted glyph
pub struct DeleteWork {
    path: PathBuf,
}

impl DeleteWork {
    pub fn create(path: PathBuf) -> Box<dyn Work<()>> {
        Box::from(DeleteWork { path })
    }
}

impl Work<()> for DeleteWork {
    fn exec(&self) -> Result<(), WorkError> {
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

    /// Create a function that could be called to generate IR for glyphs.
    ///
    /// Batched because some formats require IO to figure out the work.
    fn create_glyph_ir_work(
        &self,
        glyph_names: &HashSet<&str>,
        input: &Input,
    ) -> Result<Vec<Box<dyn Work<()>>>, Error>;
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
        let ir_input_file = build_dir.join("irinput.toml");
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

    pub fn glyph_ir_dir(&self) -> &Path {
        &self.glyph_ir_dir
    }

    pub fn glyph_ir_file(&self, glyph_name: &str) -> PathBuf {
        // TODO handle names that are invalid for the filesystem
        // Ref https://github.com/unified-font-object/ufo-spec/issues/164
        self.glyph_ir_dir.join(glyph_name.to_owned() + ".toml")
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
    fn read_write_toml() {
        let temp_dir = tempdir().unwrap();
        let ir_input = create_test_input(&temp_dir);
        let toml = toml::ser::to_string_pretty(&ir_input).unwrap();
        let restored: Input = toml::from_str(&toml).unwrap();
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
