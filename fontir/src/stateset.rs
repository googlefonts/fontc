use filetime::FileTime;
use serde::{Deserialize, Serialize};

use std::{
    collections::{HashMap, HashSet},
    fs,
    hash::{Hash, Hasher},
    io,
    path::{Path, PathBuf},
    vec,
};

/// Helps to identify changes in a set of stateful things.
#[derive(Serialize, Deserialize, Debug, Default, Clone, PartialEq)]
pub struct StateSet {
    // our entries can either be files on disk or items in memory, but
    // as a simplification just use PathBuf as the key type; we know
    // if it's an actual file or not based on the value type.
    pub(crate) entries: HashMap<PathBuf, State>,
}

// A stateful thing of some specific type
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub(crate) enum State {
    File(FileState),
    Memory(MemoryState),
}

#[derive(Debug, Copy, Clone, PartialEq, Serialize, Deserialize)]
pub(crate) struct FileState {
    #[serde(with = "file_time_serde")]
    pub(crate) mtime: FileTime,
    pub(crate) size: u64,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
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

impl State {
    fn is_file(&self) -> bool {
        matches!(self, State::File(_))
    }
}

impl StateSet {
    pub fn new() -> StateSet {
        Default::default()
    }

    /// An iterator over any paths we are currently tracking.
    ///
    /// This is for files only, not objects in memory.
    pub fn tracked_paths(&self) -> impl Iterator<Item = &Path> {
        self.entries
            .iter()
            .filter(|(_, value)| value.is_file())
            .map(|(key, _)| key.as_path())
    }

    pub fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }

    pub fn contains(&self, path: &Path) -> bool {
        self.entries.contains_key(path)
    }

    /// Pay attention to path, we'd like to know if it changes.
    pub fn track_file(&mut self, path: &Path) -> Result<(), io::Error> {
        self.entries
            .insert(path.to_owned(), State::File(FileState::of(path)?));

        // For a dir to register unchanged we need to add it's current contents
        if path.is_dir() {
            let mut dirs_visited = HashSet::new();
            for new_path in self.new_files(&mut dirs_visited, path) {
                let state = FileState::of(&new_path)?;
                self.entries.insert(new_path, State::File(state));
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
        self.entries
            .insert(identifier.into(), State::Memory(MemoryState::of(memory)?));
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
                if !dir_entry.is_dir() && !self.contains(&dir_entry) {
                    results.push(dir_entry);
                }
            }
            dirs_visited.insert(dir.to_owned());
        }
        results
    }

    /// We're what's new, Self - old_state; what's changed?
    pub fn diff(&self, old_state: &StateSet) -> Result<StateDiff, io::Error> {
        let old_keys: HashSet<_> = old_state.entries.keys().collect();
        let new_keys: HashSet<_> = self.entries.keys().collect();
        let added: HashSet<_> = new_keys.difference(&old_keys).cloned().collect();

        Ok(StateDiff {
            added: added.iter().map(|e| (*e).clone()).collect(),
            updated: new_keys
                .intersection(&old_keys)
                .filter(|e| old_state.entries.get(**e).unwrap() != self.entries.get(**e).unwrap())
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
        self.entries
            .insert(path.to_owned(), State::File(FileState { mtime, size }));
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
        Default::default()
    }
}

// handle deserializing the external FileTime type
pub(crate) mod file_time_serde {
    use filetime::FileTime;
    use serde::{Deserialize, Deserializer, Serialize, Serializer};

    #[derive(Deserialize, Serialize)]
    struct Helper(i64, u32);

    pub(crate) fn serialize<S>(item: &FileTime, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        let helper = Helper(item.unix_seconds(), item.nanoseconds());
        helper.serialize(serializer)
    }

    pub(crate) fn deserialize<'de, D>(deserializer: D) -> Result<FileTime, D::Error>
    where
        D: Deserializer<'de>,
    {
        let helper = Helper::deserialize(deserializer)?;
        Ok(FileTime::from_unix_time(helper.0, helper.1))
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

    use super::*;

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
                updated: HashSet::from([p1.into()]),
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
                updated: HashSet::from([file.clone()]),
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

    fn update_file_entries(s: &StateSet) -> Result<StateSet, io::Error> {
        let mut state = StateSet::new();
        for (key, value) in &s.entries {
            if value.is_file() {
                if !key.exists() {
                    continue;
                }
                state.track_file(key)?;
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
                added: [added].into(),
                updated: [changed].into(),
                removed: [unchanged].into(),
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
