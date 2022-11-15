use filetime::FileTime;
use serde::{Deserialize, Serialize};

use std::{
    collections::{HashMap, HashSet},
    fs, io,
    path::{Path, PathBuf},
    vec,
};

/// Helps identify changes in a set of files.
#[derive(Serialize, Deserialize, Debug, Default, Clone)]
#[serde(from = "DepGraphSerdeRepr", into = "DepGraphSerdeRepr")]
pub struct DepGraph {
    entries: HashMap<PathBuf, PathState>,
}

#[derive(Debug, Copy, Clone, PartialEq)]
enum PathState {
    Seen { mtime: FileTime, size: u64 },
    Unknown,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
struct DepGraphSerdeRepr {
    entries: Vec<PathStateSerdeRepr>,
}

impl From<DepGraphSerdeRepr> for DepGraph {
    fn from(from: DepGraphSerdeRepr) -> Self {
        DepGraph {
            entries: from
                .entries
                .iter()
                .map(|d| {
                    (
                        PathBuf::from(&d.path),
                        if d.has_graph_entry() {
                            PathState::Seen {
                                mtime: FileTime::from_unix_time(d.unix_seconds, d.nanos),
                                size: d.size,
                            }
                        } else {
                            PathState::Unknown
                        },
                    )
                })
                .collect(),
        }
    }
}

impl From<DepGraph> for DepGraphSerdeRepr {
    fn from(dg: DepGraph) -> Self {
        DepGraphSerdeRepr {
            entries: dg
                .entries
                .iter()
                .map(|e| {
                    let path = e.0.to_str().expect("Only UTF names please").to_string();
                    match e.1 {
                        PathState::Seen { mtime, size } => PathStateSerdeRepr {
                            path,
                            unix_seconds: mtime.unix_seconds(),
                            nanos: mtime.nanoseconds(),
                            size: *size,
                        },
                        PathState::Unknown => PathStateSerdeRepr {
                            path,
                            unix_seconds: 0,
                            nanos: 0,
                            size: 0,
                        },
                    }
                })
                .collect(),
        }
    }
}

/// The serde-friendly representation of a DepGraphEntry.
///
/// SystemTime lacks a platform independent representation we can
/// depend on so use FileTime's unix_seconds,nanos.
/// unix_seconds = nanos = size = 0 is used to represent lack of a DepGraphEntry.
#[derive(Serialize, Deserialize, Debug, Clone)]
struct PathStateSerdeRepr {
    path: String,
    unix_seconds: i64,
    nanos: u32,
    size: u64,
}

impl PathStateSerdeRepr {
    fn has_graph_entry(&self) -> bool {
        !(self.unix_seconds == 0 && self.nanos == 0 && self.size == 0)
    }
}

/// A new snapshot of the state of a file.
///
/// Always contains PathState::Seen.
pub struct Change {
    path: PathBuf,
    path_state: PathState,
}

pub enum InitialState {
    NotChanged,
    Changed,
}

impl PathState {
    fn of(path: &Path) -> Result<PathState, io::Error> {
        let metadata = path.metadata()?;
        Ok(PathState::Seen {
            mtime: FileTime::from_system_time(metadata.modified()?),
            size: metadata.len(),
        })
    }
}

impl DepGraph {
    pub fn new() -> DepGraph {
        Default::default()
    }
}

impl DepGraph {
    pub fn contains(&self, path: &PathBuf) -> bool {
        self.entries.contains_key(path)
    }

    pub fn track(&mut self, path: &PathBuf, default_state: InitialState) -> Result<(), io::Error> {
        if !self.entries.contains_key(path) {
            self.entries.insert(
                path.clone(),
                match default_state {
                    InitialState::NotChanged => PathState::of(path)?,
                    InitialState::Changed => PathState::Unknown,
                },
            );

            // For a dir to register unchanged we need to add it's current contents
            if let InitialState::NotChanged = default_state {
                if path.is_dir() {
                    let mut dirs_visited = HashSet::new();
                    if let Some(new_paths) = self.new_files(&mut dirs_visited, path) {
                        for new_path in new_paths {
                            self.entries
                                .insert(new_path.clone(), PathState::of(&new_path)?);
                        }
                    }
                }
            }
        }
        Ok(())
    }

    pub fn has_changed(&self, change: &Change) -> bool {
        // If we are tracking with different state or we're not tracking at all then you've changed
        let current_state = self
            .entries
            .get(&change.path)
            .unwrap_or(&PathState::Unknown);
        *current_state != change.path_state
    }

    pub fn update(&mut self, changes: &[Change]) {
        for change in changes {
            if self.has_changed(change) {
                self.entries.insert(change.path.clone(), change.path_state);
            }
        }
    }

    fn new_files(
        &self,
        dirs_visited: &mut HashSet<PathBuf>,
        dir: &PathBuf,
    ) -> Option<Vec<PathBuf>> {
        assert!(dir.is_dir());
        if dirs_visited.contains(dir) {
            return None;
        }

        let mut frontier = vec![dir.to_owned()];
        let mut results: Option<Vec<PathBuf>> = None;

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
                    let new_files = results.get_or_insert_with(Vec::new);
                    new_files.push(dir_entry);
                }
            }
            dirs_visited.insert(dir.to_owned());
        }
        results
    }

    /// Set of files that have either changed themselves or had
    /// something they depend on change.
    ///
    /// Directories can be tracked but never report as changed.
    pub fn changed(&self) -> Result<Vec<Change>, io::Error> {
        let mut paths: Vec<&PathBuf> = self.entries.iter().map(|(k, _)| k).collect();

        let mut changes: Vec<Change> = Vec::new();

        // Add any new files in tracked dirs
        let mut dirs_visited: HashSet<PathBuf> = HashSet::new();
        let new_files: Vec<PathBuf> = paths
            .iter()
            .filter(|p| p.is_dir())
            .flat_map(|p| self.new_files(&mut dirs_visited, p).unwrap_or_default())
            .collect();
        new_files.iter().for_each(|p| paths.push(p));

        // Anything change 'round here?
        for path in paths {
            let path_state = PathState::of(path)?;
            let has_changed = self
                .entries
                .get(path)
                .map(|prior_state| *prior_state != path_state)
                .unwrap_or(true);

            if has_changed {
                changes.push(Change {
                    path: path.to_owned(),
                    path_state,
                });
            }
        }

        Ok(changes)
    }
}

#[cfg(test)]
mod tests {
    use std::{fs, ops::Add, path::PathBuf, time::Duration};

    use filetime::set_file_mtime;
    use tempfile::{tempdir, TempDir};

    use super::DepGraph;

    fn assert_no_changes(dg: &DepGraph) {
        assert_eq!(
            Vec::<&PathBuf>::new(),
            dg.changed()
                .unwrap()
                .iter()
                .map(|c| &c.path)
                .collect::<Vec<&PathBuf>>()
        )
    }

    #[test]
    fn detect_file_change() {
        let temp_dir = tempdir().unwrap();

        let file = temp_dir.path().join("a");
        fs::write(&file, "eh").unwrap();

        let mut dg = DepGraph::new();
        dg.track(&file, super::InitialState::NotChanged).unwrap();

        assert_no_changes(&dg);

        // Detect changed size
        fs::write(&file, "whoa").unwrap();
        let changes = dg.changed().unwrap();
        assert_eq!(
            vec![&file],
            changes.iter().map(|c| &c.path).collect::<Vec<&PathBuf>>()
        );

        // We're happy with that change
        dg.update(&changes);
        assert_no_changes(&dg);

        // Detect changed mtime
        let new_mtime = file
            .metadata()
            .unwrap()
            .modified()
            .unwrap()
            .add(Duration::from_secs(1));
        set_file_mtime(&file, new_mtime.into()).unwrap();
        let changes = dg.changed().unwrap();
        assert_eq!(
            vec![&file],
            changes.iter().map(|c| &c.path).collect::<Vec<&PathBuf>>()
        );
    }

    #[test]
    fn detect_dir_change() {
        let temp_dir = tempdir().unwrap();

        let subdir = temp_dir.path().join("glif");
        fs::create_dir(&subdir).unwrap();

        let f1 = temp_dir.path().join("fileA");
        let f2 = subdir.join("fileB");
        fs::write(&f1, "eh").unwrap();
        fs::write(&f2, "eh").unwrap();

        // Track the parent dir from it's current state
        let mut dg = DepGraph::new();
        dg.track(
            &temp_dir.path().to_path_buf(),
            super::InitialState::NotChanged,
        )
        .unwrap();

        // Notably, we should NOT report the files temp dir as changed
        // because we added the entire directory as unchanged and nothing
        // in it has changed since then
        assert_no_changes(&dg);

        // If we change or add files in the tracked dir that should count
        let f3 = subdir.join("fileC");
        fs::write(&f2, "eh+").unwrap();
        fs::write(&f3, "eh").unwrap();

        let changes = dg.changed().unwrap();
        assert_eq!(
            vec![&f2, &f3],
            changes.iter().map(|c| &c.path).collect::<Vec<&PathBuf>>()
        );
    }

    fn one_changed_file_one_not(temp_dir: &TempDir) -> (PathBuf, PathBuf, DepGraph) {
        let unchanged = temp_dir.path().join("a");
        let changed = temp_dir.path().join("b");

        fs::write(&unchanged, "eh").unwrap();
        fs::write(&changed, "eh").unwrap();

        let mut dg = DepGraph::new();
        dg.track(&unchanged, super::InitialState::NotChanged)
            .unwrap();
        dg.track(&changed, super::InitialState::Changed).unwrap();

        assert!(dg.contains(&changed));
        assert!(dg.contains(&unchanged));

        (changed, unchanged, dg)
    }

    #[test]
    fn track_with_initial_state() {
        let temp_dir = tempdir().unwrap();

        let (changed, _, mut dg) = one_changed_file_one_not(&temp_dir);

        let changes = dg.changed().unwrap();
        assert_eq!(
            vec![&changed],
            changes.iter().map(|c| &c.path).collect::<Vec<&PathBuf>>()
        );

        dg.update(&changes);
        let changes = dg.changed().unwrap();
        assert!(changes.is_empty());
    }

    #[test]
    fn read_write_toml() {
        let temp_dir = tempdir().unwrap();

        let (changed, _, dg) = one_changed_file_one_not(&temp_dir);

        let toml = toml::ser::to_string_pretty(&dg).unwrap();
        let dg: DepGraph = toml::from_str(&toml).unwrap();

        assert_eq!(
            vec![&changed],
            dg.changed()
                .unwrap()
                .iter()
                .map(|c| &c.path)
                .collect::<Vec<&PathBuf>>()
        );
    }

    #[test]
    fn read_write_bincode() {
        let temp_dir = tempdir().unwrap();

        let (changed, _, dg) = one_changed_file_one_not(&temp_dir);

        let bc = bincode::serialize(&dg).unwrap();
        let dg: DepGraph = bincode::deserialize(&bc).unwrap();

        assert_eq!(
            vec![&changed],
            dg.changed()
                .unwrap()
                .iter()
                .map(|c| &c.path)
                .collect::<Vec<&PathBuf>>()
        );
    }
}
