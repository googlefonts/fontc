use crate::serde::StateSetSerdeRepr;
use filetime::FileTime;
use serde::{Deserialize, Serialize};

use std::{
    collections::{hash_map::Keys, HashMap, HashSet},
    fs, io,
    path::{Path, PathBuf},
    vec,
};

/// Helps to identify changes in a set of stateful things.
#[derive(Serialize, Deserialize, Debug, Default, Clone, PartialEq)]
#[serde(from = "StateSetSerdeRepr", into = "StateSetSerdeRepr")]
pub struct StateSet {
    pub(crate) entries: HashMap<PathBuf, State>,
}

// Sometimes state comes from a file, sometimes it comes from a
// a slice of a string.
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum State {
    File(FileState),
    Slice(SliceState),
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub(crate) struct FileState {
    pub(crate) mtime: FileTime,
    pub(crate) size: u64,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SliceState {
    pub(crate) hash: blake3::Hash,
    pub(crate) size: usize,
}

impl FileState {
    fn of(path: &Path) -> Result<FileState, io::Error> {
        let metadata = path.metadata()?;
        Ok(FileState {
            mtime: FileTime::from_system_time(metadata.modified()?),
            size: metadata.len(),
        })
    }
}

impl SliceState {
    fn of(slice: &str) -> Result<SliceState, io::Error> {
        let hash = blake3::hash(slice.as_bytes());
        Ok(SliceState {
            hash,
            size: slice.len(),
        })
    }
}

impl StateSet {
    pub fn new() -> StateSet {
        Default::default()
    }
}

impl<'a> IntoIterator for &'a StateSet {
    type Item = &'a Path;
    type IntoIter = StateSetIntoIter<'a>;

    fn into_iter(self) -> Self::IntoIter {
        StateSetIntoIter {
            iter: self.entries.keys(),
        }
    }
}

pub struct StateSetIntoIter<'a> {
    iter: Keys<'a, PathBuf, State>,
}

impl<'a> Iterator for StateSetIntoIter<'a> {
    type Item = &'a Path;
    fn next(&mut self) -> Option<&'a Path> {
        self.iter.next().map(|pb| pb.as_path())
    }
}

impl StateSet {
    // For tests.
    #[doc(hidden)]
    pub fn set_state(&mut self, path: &Path, mtime: FileTime, size: u64) {
        self.entries
            .insert(path.to_path_buf(), State::File(FileState { mtime, size }));
    }
}

impl StateSet {
    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }

    pub fn contains(&self, path: &Path) -> bool {
        self.entries.contains_key(path)
    }

    /// Pay attention to path, we'd like to know if it changes.
    pub fn track_file(&mut self, path: &Path) -> Result<(), io::Error> {
        self.entries
            .insert(path.to_path_buf(), State::File(FileState::of(path)?));

        // For a dir to register unchanged we need to add it's current contents
        if path.is_dir() {
            let mut dirs_visited = HashSet::new();
            for new_path in self.new_files(&mut dirs_visited, path) {
                self.entries
                    .insert(new_path.clone(), State::File(FileState::of(&new_path)?));
            }
        }
        Ok(())
    }

    /// Pay attention to slice, we'd quite like to know if it changes
    ///
    /// Identifier can be whatever you like. For file fragments a path-like construct
    /// is suggested, e.g. myfile.glyphs/glyphs/layer/glyph perhaps.
    pub fn track_slice(&mut self, identifier: &str, slice: &str) -> Result<(), io::Error> {
        self.entries.insert(
            PathBuf::from(identifier),
            State::Slice(SliceState::of(slice)?),
        );
        Ok(())
    }

    fn new_files(&self, dirs_visited: &mut HashSet<PathBuf>, dir: &Path) -> Vec<PathBuf> {
        assert!(dir.is_dir());
        if dirs_visited.contains(dir) {
            return Vec::new();
        }

        let mut frontier = vec![dir.to_owned()];
        let mut results: Vec<PathBuf> = Vec::new();

        while let Some(dir) = frontier.pop() {
            let dir_entries = fs::read_dir(&dir).expect("Unable to iterate directory");
            for dir_entry in dir_entries {
                let dir_entry = dir_entry.expect("Cannot read dir entry").path();

                // Queue subdir for processing. Bravely assume lack of cycles.
                if dir_entry.is_dir() && !dirs_visited.contains(&dir_entry) {
                    frontier.push(dir_entry.to_owned());
                }

                // A file we've never seen before?!
                if !dir_entry.is_dir() && !self.entries.contains_key(&dir_entry) {
                    results.push(dir_entry);
                }
            }
            dirs_visited.insert(dir.to_owned());
        }
        results
    }

    /// Generate a [Self] for the same files with new file state from filesystem.
    ///
    /// Anything that no longer exists will be missing from the new snapshot.
    ///
    /// Only tracked files can be so updated, tracked string slices will never update.
    pub fn updated_snapshot(&self) -> Result<StateSet, io::Error> {
        let mut state = StateSet::new();
        for path in self.entries.keys() {
            if !path.exists() {
                continue;
            }
            state.track_file(path)?;
        }
        Ok(state)
    }
}

#[derive(Debug, Default, PartialEq, Eq)]
pub struct StateDiff {
    pub added: HashSet<PathBuf>,
    pub updated: HashSet<PathBuf>,
    pub removed: HashSet<PathBuf>,
}

impl StateDiff {
    pub fn new() -> StateDiff {
        StateDiff {
            added: HashSet::new(),
            updated: HashSet::new(),
            removed: HashSet::new(),
        }
    }
}

impl StateSet {
    /// We're what's new, Self - old_state; what's changed?
    pub fn diff(&self, old_state: &StateSet) -> Result<StateDiff, io::Error> {
        let old_keys: HashSet<&PathBuf> = old_state.entries.keys().collect();
        let new_keys: HashSet<&PathBuf> = self.entries.keys().collect();

        let added: HashSet<&Path> = new_keys
            .difference(&old_keys)
            .map(|e| e.as_path())
            .collect();

        Ok(StateDiff {
            added: added.iter().map(|e| e.to_path_buf()).collect(),
            updated: new_keys
                .intersection(&old_keys)
                .into_iter()
                .filter(|e| {
                    old_state.entries.get(e.as_path()).unwrap()
                        != self.entries.get(e.as_path()).unwrap()
                })
                .filter(|e| !added.contains(e.as_path()))
                .map(|e| (*e).clone())
                .collect(),
            removed: old_keys
                .difference(&new_keys)
                .into_iter()
                .map(|e| (*e).clone())
                .collect(),
        })
    }
}

#[cfg(test)]
mod tests {
    use std::{
        collections::HashSet,
        fs,
        ops::Add,
        path::{Path, PathBuf},
        time::Duration,
    };

    use filetime::set_file_mtime;
    use tempfile::{tempdir, TempDir};

    use super::{StateDiff, StateSet};

    fn assert_no_file_changes(fs: &StateSet) {
        assert_eq!(
            StateDiff::new(),
            fs.updated_snapshot().unwrap().diff(fs).unwrap(),
        )
    }

    #[test]
    fn detect_slice_change() {
        let p1 = "Font.glyphs/glyphs/layer/space";
        let p2 = "Font.glyphs/glyphs/layer/hyphen";

        let mut s1 = StateSet::new();
        s1.track_slice(p1, "this is a glyph").unwrap();
        s1.track_slice(p2, "another glyph").unwrap();

        let mut s2 = s1.clone();
        s2.track_slice(p1, "this changes everything").unwrap();

        assert_eq!(
            StateDiff {
                updated: HashSet::from([PathBuf::from(p1)]),
                ..Default::default()
            },
            s2.diff(&s1).unwrap(),
        )
    }

    #[test]
    fn detect_file_change() {
        let temp_dir = tempdir().unwrap();

        let file = temp_dir.path().join("a");
        fs::write(&file, "eh").unwrap();

        let mut fs = StateSet::new();
        fs.track_file(&file).unwrap();

        assert_no_file_changes(&fs);

        // Detect changed size
        fs::write(&file, "whoa").unwrap();
        let updated = fs.updated_snapshot().unwrap();
        let diff = updated.diff(&fs).unwrap();
        assert_eq!(
            StateDiff {
                updated: HashSet::from([file.to_owned()]),
                ..Default::default()
            },
            diff
        );
        assert_no_file_changes(&updated);

        // Detect changed mtime
        let new_mtime = file
            .metadata()
            .unwrap()
            .modified()
            .unwrap()
            .add(Duration::from_secs(1));
        set_file_mtime(&file, new_mtime.into()).unwrap();
        let updated = fs.updated_snapshot().unwrap();
        let diff = updated.diff(&fs).unwrap();
        assert_eq!(
            StateDiff {
                updated: HashSet::from([file]),
                ..Default::default()
            },
            diff
        );
        assert_no_file_changes(&updated);
    }

    #[test]
    fn detect_dir_change() {
        let temp_dir = tempdir().unwrap();

        let subdir = temp_dir.path().join("glif");
        fs::create_dir(&subdir).unwrap();

        let unchanged = temp_dir.path().join("fileA");
        let modified = subdir.join("fileB");
        let removed = subdir.join("fileC");
        fs::write(&unchanged, "eh").unwrap();
        fs::write(&modified, "eh").unwrap();
        fs::write(&removed, "eh").unwrap();

        // Track the parent dir from it's current state
        let mut fs = StateSet::new();
        fs.track_file(temp_dir.path()).unwrap();

        // Notably, we should NOT report the files temp dir as changed
        // because we added the entire directory as unchanged and nothing
        // in it has changed since then
        assert_no_file_changes(&fs);

        // If we change or add files in the tracked dir that should count
        let added = subdir.join("fileD");
        fs::write(&modified, "eh+").unwrap();
        fs::write(&added, "eh").unwrap();
        fs::remove_file(&removed).unwrap();

        let diff = fs.updated_snapshot().unwrap().diff(&fs).unwrap();
        assert_eq!(
            StateDiff {
                added: HashSet::from([added]),
                updated: HashSet::from([modified]),
                removed: HashSet::from([removed]),
            },
            diff
        );
    }

    fn write(temp_dir: &TempDir, path: &Path, content: &str) -> PathBuf {
        let path = temp_dir.path().join(path);
        fs::write(&path, content).unwrap();
        path
    }

    fn one_changed_file_one_not(temp_dir: &TempDir) -> (PathBuf, PathBuf, StateSet) {
        let unchanged = write(temp_dir, Path::new("a"), "eh");
        let changed = write(temp_dir, Path::new("b"), "todo:change");

        let mut fs = StateSet::new();
        fs.track_file(&unchanged).unwrap();
        fs.track_file(&changed).unwrap();
        write(temp_dir, &changed, "eh");

        assert!(fs.contains(&changed));
        assert!(fs.contains(&unchanged));

        (changed, unchanged, fs)
    }

    #[test]
    fn file_diff() {
        let temp_dir = tempdir().unwrap();
        let (changed, unchanged, fs) = one_changed_file_one_not(&temp_dir);
        let fs2 = fs.clone();

        // Nothing changed
        assert_eq!(StateDiff::new(), fs2.diff(&fs).unwrap());

        // Some stuff changed!
        let added = write(&temp_dir, Path::new("new"), "meh");
        fs::remove_file(&unchanged).unwrap();
        let mut fs2 = fs.updated_snapshot().unwrap();
        fs2.track_file(&added).unwrap();
        assert_eq!(
            StateDiff {
                added: [added].into(),
                updated: [changed].into(),
                removed: [unchanged].into(),
            },
            fs2.diff(&fs).unwrap()
        );
    }

    #[test]
    fn read_write_toml() {
        let temp_dir = tempdir().unwrap();

        let (_, _, mut fs) = one_changed_file_one_not(&temp_dir);
        fs.track_slice("file.glyphs/glyph/layer/glyph_name", "Hi World!")
            .unwrap();

        let toml = toml::ser::to_string_pretty(&fs).unwrap();
        let restored: StateSet = toml::from_str(&toml).expect(&toml);
        assert_eq!(fs, restored);
    }

    #[test]
    fn read_write_bincode() {
        let temp_dir = tempdir().unwrap();

        let (_, _, mut fs) = one_changed_file_one_not(&temp_dir);
        fs.track_slice("file.glyphs/glyph/layer/glyph_name", "Hi World!")
            .unwrap();

        let bc = bincode::serialize(&fs).unwrap();
        let restored: StateSet = bincode::deserialize(&bc).unwrap();
        assert_eq!(fs, restored);
    }
}
