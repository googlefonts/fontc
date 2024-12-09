use crate::{error::Error, serde::PartialSourceSerdeRepr};
use serde::{Deserialize, Serialize};

use std::{
    collections::HashSet,
    hash::Hash,
    path::{Path, PathBuf},
};

/// A subset of a larger source that can meaningfully change independently
///
/// For example, a single glyph from a .glyphs file or designspace.
/// A single fragment could be made up of multiple parts of the input.
///
/// We once stored (mtime, size) for files and blake3 hash for memory to try to enable
/// change detection, then removed change detection in favor of doing full compiles
/// every time.
#[derive(Serialize, Deserialize, Debug, Default, Clone, PartialEq)]
#[serde(from = "PartialSourceSerdeRepr", into = "PartialSourceSerdeRepr")]
pub struct PartialSource {
    pub(crate) entries: HashSet<PartialSourceIdentifier>,
}

// The key for a stateful thing of some specific type
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum PartialSourceIdentifier {
    File(PathBuf),
    Memory(String),
}

impl PartialSource {
    pub fn new() -> PartialSource {
        Default::default()
    }

    pub fn keys(&self) -> impl Iterator<Item = &PartialSourceIdentifier> {
        self.entries.iter()
    }

    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }

    pub fn contains(&self, path: &Path) -> bool {
        self.entries
            .contains(&PartialSourceIdentifier::File(path.to_path_buf()))
    }

    /// This file contributes
    pub fn add_file(&mut self, path: &Path) -> Result<(), Error> {
        if !path.exists() {
            return Err(Error::NoSuchPath(path.to_path_buf()));
        }
        self.entries
            .insert(PartialSourceIdentifier::File(path.to_path_buf()));
        Ok(())
    }

    /// This memory contributes.
    ///
    /// Identifier can be whatever you like. For file fragments a path-like construct
    /// is suggested, e.g. myfile.glyphs/glyphs/layer/glyph perhaps.
    pub fn add_memory(&mut self, identifier: String) {
        self.entries
            .insert(PartialSourceIdentifier::Memory(identifier));
    }

    // For tests.
    #[doc(hidden)]
    pub fn set_file_state(&mut self, path: &Path) {
        self.entries
            .insert(PartialSourceIdentifier::File(path.to_path_buf()));
    }
}

#[cfg(test)]
mod tests {
    use std::{
        fs,
        path::{Path, PathBuf},
    };

    use tempfile::{tempdir, TempDir};

    use super::PartialSource;

    fn write(temp_dir: &TempDir, path: &Path, content: &str) -> PathBuf {
        let path = temp_dir.path().join(path);
        fs::write(&path, content).unwrap();
        path
    }

    fn two_file_partial_source(temp_dir: &TempDir) -> PartialSource {
        let unchanged = write(temp_dir, Path::new("a"), "eh");
        let changed = write(temp_dir, Path::new("b"), "todo:change");

        let mut fs = PartialSource::new();
        fs.add_file(&unchanged).unwrap();
        fs.add_file(&changed).unwrap();
        write(temp_dir, &changed, "eh");

        assert!(fs.contains(&changed));
        assert!(fs.contains(&unchanged));

        fs
    }

    #[test]
    fn read_write_yaml() {
        let temp_dir = tempdir().unwrap();

        let mut fs = two_file_partial_source(&temp_dir);
        fs.add_memory("/glyph/glyph_name".to_string());

        let yml = serde_yaml::to_string(&fs).unwrap();
        let restored: PartialSource = serde_yaml::from_str(&yml).expect(&yml);
        assert_eq!(fs, restored);
    }

    #[test]
    fn read_write_bincode() {
        let temp_dir = tempdir().unwrap();

        let mut fs = two_file_partial_source(&temp_dir);
        fs.add_memory("/glyph/glyph_name".to_string());

        let bc = bincode::serialize(&fs).unwrap();
        let restored: PartialSource = bincode::deserialize(&bc).unwrap();
        assert_eq!(fs, restored);
    }
}
