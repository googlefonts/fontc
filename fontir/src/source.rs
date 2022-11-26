//! Generic model of font sources.

use std::{collections::HashMap, io};

use serde::{Deserialize, Serialize};

use crate::{
    error::{Error, WorkError},
    filestate::FileStateSet,
};

/// A unit of work safe to run in parallel
///
/// Naively you'd think we'd just return FnOnce + Send but that didn't want to compile
/// https://github.com/rust-lang/rust/issues/29625
pub trait Work<R>: Send {
    fn exec(&self) -> Result<R, WorkError>;
}

/// Manipulations on some sort of font source.
pub trait Source {
    /// Resolve a source to a set of files and their dependencies.
    fn inputs(&self) -> Result<Input, Error>;

    /// Delete IR for a glyph, probably because it was removed from sources
    fn remove_glyph_ir(&self, glyph_name: &str) -> Result<(), io::Error>;

    // Create a function that could be called to generate IR for a glyph
    fn create_glyph_ir_work(
        &self,
        glyph_name: &str,
        glyph_files: &FileStateSet,
    ) -> Box<dyn Work<()>>;
}

/// The files (in future non-file sources?) that drive various parts of IR
#[derive(Serialize, Deserialize, Debug, Default, Clone, PartialEq)]
pub struct Input {
    /// The input(s) that inform font_info
    pub font_info: FileStateSet,
    /// The input(s) that inform glyph construction, grouped by gyph name
    pub glyphs: HashMap<String, FileStateSet>,
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

    use crate::filestate::FileStateSet;

    use super::Input;

    fn write(temp_dir: &TempDir, path: &Path, content: &str) -> PathBuf {
        let path = temp_dir.path().join(path);
        fs::write(&path, content).unwrap();
        path
    }

    fn create_test_input(temp_dir: &TempDir) -> Input {
        let mut font_info = FileStateSet::new();
        font_info
            .insert(&write(temp_dir, Path::new("some.designspace"), "blah"))
            .unwrap();

        let mut glyph = FileStateSet::new();
        glyph
            .insert(&write(temp_dir, Path::new("regular.space.glif"), "blah"))
            .unwrap();
        glyph
            .insert(&write(temp_dir, Path::new("bold.space.glif"), "blah"))
            .unwrap();

        let mut glyphs = HashMap::new();
        glyphs.insert("space".to_string(), glyph);

        Input { font_info, glyphs }
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
