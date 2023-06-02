use crate::serde::StateSetSerdeRepr;
use filetime::FileTime;
use serde::{Deserialize, Serialize};

use std::{
    collections::{hash_map, HashMap, HashSet},
    fs,
    hash::{Hash, Hasher},
    io,
    path::{Path, PathBuf},
    vec,
};

/// Helps to identify changes in a set of stateful things.
#[derive(Serialize, Deserialize, Debug, Default, Clone, PartialEq)]
#[serde(from = "StateSetSerdeRepr", into = "StateSetSerdeRepr")]
pub struct StateSet {
    pub(crate) entries: HashMap<StateIdentifier, State>,
}

// The key for a stateful thing of some specific type
#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum StateIdentifier {
    File(PathBuf),
    Memory(String),
}

// A stateful thing of some specific type
#[derive(Debug, Clone, PartialEq)]
pub(crate) enum State {
    File(FileState),
    Memory(MemoryState),
}

#[derive(Debug, Copy, Clone, PartialEq)]
pub(crate) struct FileState {
    pub(crate) mtime: FileTime,
    pub(crate) size: u64,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MemoryState {
    pub(crate) hash: blake3::Hash,
    pub(crate) size: u64,
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

impl MemoryState {
    fn of(thing: impl Hash) -> Result<MemoryState, io::Error> {
        let mut hasher = Blake3Hasher::new();
        thing.hash(&mut hasher);
        let hash = hasher.b3.finalize();
        Ok(MemoryState {
            hash,
            size: hasher.b3.count(),
        })
    }
}

// blake3 doesn't implement std Hasher which makes hashing an impl Hash difficult
struct Blake3Hasher {
    b3: blake3::Hasher,
}

impl Blake3Hasher {
    fn new() -> Blake3Hasher {
        Blake3Hasher {
            b3: blake3::Hasher::new(),
        }
    }
}

impl Hasher for Blake3Hasher {
    fn finish(&self) -> u64 {
        panic!("Use .b3.finalize() instead");
    }

    fn write(&mut self, bytes: &[u8]) {
        self.b3.update(bytes);
    }
}

impl StateSet {
    pub fn new() -> StateSet {
        Default::default()
    }

    pub fn keys(&self) -> KeyIterator {
        KeyIterator {
            iter: self.entries.keys(),
        }
    }

    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }

    pub fn contains(&self, path: &Path) -> bool {
        self.entries
            .contains_key(&StateIdentifier::File(path.to_path_buf()))
    }

    /// Pay attention to path, we'd like to know if it changes.
    pub fn track_file(&mut self, path: &Path) -> Result<(), io::Error> {
        self.entries.insert(
            StateIdentifier::File(path.to_path_buf()),
            State::File(FileState::of(path)?),
        );

        // For a dir to register unchanged we need to add it's current contents
        if path.is_dir() {
            let mut dirs_visited = HashSet::new();
            for new_path in self.new_files(&mut dirs_visited, path) {
                self.entries.insert(
                    StateIdentifier::File(new_path.clone()),
                    State::File(FileState::of(&new_path)?),
                );
            }
        }
        Ok(())
    }

    /// Pay attention to this hashable thing, we'd quite like to know if it changes
    ///
    /// Identifier can be whatever you like. For file fragments a path-like construct
    /// is suggested, e.g. myfile.glyphs/glyphs/layer/glyph perhaps.
    pub fn track_memory(
        &mut self,
        identifier: String,
        memory: &(impl Hash + ?Sized),
    ) -> Result<(), io::Error> {
        self.entries.insert(
            StateIdentifier::Memory(identifier),
            State::Memory(MemoryState::of(memory)?),
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
                if !dir_entry.is_dir()
                    && !self
                        .entries
                        .contains_key(&StateIdentifier::File(dir_entry.clone()))
                {
                    results.push(dir_entry);
                }
            }
            dirs_visited.insert(dir.to_owned());
        }
        results
    }

    /// We're what's new, Self - old_state; what's changed?
    pub fn diff(&self, old_state: &StateSet) -> Result<StateDiff, io::Error> {
        let old_keys: HashSet<&StateIdentifier> = old_state.entries.keys().collect();
        let new_keys: HashSet<&StateIdentifier> = self.entries.keys().collect();

        let added: HashSet<&StateIdentifier> = new_keys.difference(&old_keys).cloned().collect();

        Ok(StateDiff {
            added: added.iter().map(|e| (*e).clone()).collect(),
            updated: new_keys
                .intersection(&old_keys)
                .filter(|e| old_state.entries.get(e).unwrap() != self.entries.get(e).unwrap())
                .filter(|e| !added.contains(*e))
                .map(|e| (*e).clone())
                .collect(),
            removed: old_keys
                .difference(&new_keys)
                .map(|e| (*e).clone())
                .collect(),
        })
    }
    // For tests.
    #[doc(hidden)]
    pub fn set_file_state(&mut self, path: &Path, mtime: FileTime, size: u64) {
        self.entries.insert(
            StateIdentifier::File(path.to_path_buf()),
            State::File(FileState { mtime, size }),
        );
    }
}

pub struct KeyIterator<'a> {
    iter: hash_map::Keys<'a, StateIdentifier, State>,
}

impl<'a> Iterator for KeyIterator<'a> {
    type Item = &'a StateIdentifier;

    fn next(&mut self) -> Option<Self::Item> {
        self.iter.next()
    }
}

#[derive(Debug, Default, PartialEq, Eq)]
pub struct StateDiff {
    pub added: HashSet<StateIdentifier>,
    pub updated: HashSet<StateIdentifier>,
    pub removed: HashSet<StateIdentifier>,
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

#[cfg(test)]
mod tests {
    use std::{
        collections::HashSet,
        fs, io,
        ops::Add,
        path::{Path, PathBuf},
        time::Duration,
    };

    use filetime::set_file_mtime;
    use tempfile::{tempdir, TempDir};

    use super::{StateDiff, StateIdentifier, StateSet};

    fn assert_no_file_changes(fs: &StateSet) {
        assert_eq!(
            StateDiff::new(),
            update_file_entries(fs).unwrap().diff(fs).unwrap(),
        )
    }

    #[test]
    fn detect_memory_change() {
        let p1 = String::from("/glyphs/space");
        let p2 = String::from("/glyphs/hyphen");

        let mut s1 = StateSet::new();
        s1.track_memory(p1.clone(), "this is a glyph").unwrap();
        s1.track_memory(p2, "another glyph").unwrap();

        let mut s2 = s1.clone();
        s2.track_memory(p1.clone(), "this changes everything")
            .unwrap();

        assert_eq!(
            StateDiff {
                updated: HashSet::from([StateIdentifier::Memory(p1)]),
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
        let updated = update_file_entries(&fs).unwrap();
        let diff = updated.diff(&fs).unwrap();
        assert_eq!(
            StateDiff {
                updated: HashSet::from([StateIdentifier::File(file.to_owned())]),
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
        let updated = update_file_entries(&fs).unwrap();
        let diff = updated.diff(&fs).unwrap();
        assert_eq!(
            StateDiff {
                updated: HashSet::from([StateIdentifier::File(file)]),
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
        fs::write(unchanged, "eh").unwrap();
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

        let diff = update_file_entries(&fs).unwrap().diff(&fs).unwrap();
        assert_eq!(
            StateDiff {
                added: HashSet::from([StateIdentifier::File(added)]),
                updated: HashSet::from([StateIdentifier::File(modified)]),
                removed: HashSet::from([StateIdentifier::File(removed)]),
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

    fn update_file_entries(s: &StateSet) -> Result<StateSet, io::Error> {
        let mut state = StateSet::new();
        for key in s.keys() {
            if let StateIdentifier::File(path) = key {
                if !path.exists() {
                    continue;
                }
                state.track_file(path)?;
            }
        }
        Ok(state)
    }

    #[test]
    fn file_diff() {
        let temp_dir = tempdir().unwrap();
        let (changed, unchanged, fs) = one_changed_file_one_not(&temp_dir);

        // Nothing changed
        assert_eq!(StateDiff::new(), fs.diff(&fs).unwrap());

        // Some stuff changed!
        let added = write(&temp_dir, Path::new("new"), "meh");
        fs::remove_file(&unchanged).unwrap();
        let mut fs2 = update_file_entries(&fs).unwrap();
        fs2.track_file(&added).unwrap();
        assert_eq!(
            StateDiff {
                added: [StateIdentifier::File(added)].into(),
                updated: [StateIdentifier::File(changed)].into(),
                removed: [StateIdentifier::File(unchanged)].into(),
            },
            fs2.diff(&fs).unwrap()
        );
    }

    #[test]
    fn read_write_yaml() {
        let temp_dir = tempdir().unwrap();

        let (_, _, mut fs) = one_changed_file_one_not(&temp_dir);
        fs.track_memory("/glyph/glyph_name".to_string(), "Hi World!")
            .unwrap();

        let yml = serde_yaml::to_string(&fs).unwrap();
        let restored: StateSet = serde_yaml::from_str(&yml).expect(&yml);
        assert_eq!(fs, restored);
    }

    #[test]
    fn read_write_bincode() {
        let temp_dir = tempdir().unwrap();

        let (_, _, mut fs) = one_changed_file_one_not(&temp_dir);
        fs.track_memory("/glyph/glyph_name".to_string(), "Hi World!")
            .unwrap();

        let bc = bincode::serialize(&fs).unwrap();
        let restored: StateSet = bincode::deserialize(&bc).unwrap();
        assert_eq!(fs, restored);
    }
}
