//! Generic model of font sources.

use std::collections::HashMap;

use serde::{Deserialize, Serialize};

use crate::{error::FontIrError, filestate::FileStateSet};

/// Manipulations on some sort of font source.
pub trait IrSource {
    /// Resolve a source to a set of files and their dependencies.
    fn inputs(&self) -> Result<IrInput, FontIrError>;
}

/// The files (in future non-file sources?) that drive various parts of IR
#[derive(Serialize, Deserialize, Debug, Default, Clone, PartialEq)]
pub struct IrInput {
    /// The input(s) that inform font_info
    pub font_info: FileStateSet,
    /// The input(s) that inform glyph construction, grouped by gyph name
    pub glyphs: HashMap<String, FileStateSet>,
}

impl IrInput {
    pub fn new() -> IrInput {
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

    use super::IrInput;

    fn write(temp_dir: &TempDir, path: &Path, content: &str) -> PathBuf {
        let path = temp_dir.path().join(path);
        fs::write(&path, content).unwrap();
        path
    }

    fn create_test_input(temp_dir: &TempDir) -> IrInput {
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

        IrInput { font_info, glyphs }
    }

    #[test]
    fn read_write_toml() {
        let temp_dir = tempdir().unwrap();
        let ir_input = create_test_input(&temp_dir);
        let toml = toml::ser::to_string_pretty(&ir_input).unwrap();
        let restored: IrInput = toml::from_str(&toml).unwrap();
        assert_eq!(ir_input, restored);
    }

    #[test]
    fn read_write_bincode() {
        let temp_dir = tempdir().unwrap();
        let ir_input = create_test_input(&temp_dir);
        let bc = bincode::serialize(&ir_input).unwrap();
        let restored: IrInput = bincode::deserialize(&bc).unwrap();
        assert_eq!(ir_input, restored);
    }
}
