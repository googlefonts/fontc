//! Live progress, for the status shown on the report.
//!
//! This writes the file passed as `--progress-file`. `run.sh` in the
//! [fontc_crater repo] folds it into the `status.json` it publishes, along with
//! what only it knows, such as failures from before crater starts.
//!
//! Everything here is best-effort: failures are logged and never end a run.
//!
//! [fontc_crater repo]: https://github.com/googlefonts/fontc_crater

use std::{
    path::{Path, PathBuf},
    sync::Mutex,
};

#[derive(serde::Serialize)]
#[serde(tag = "phase", rename_all = "snake_case")]
enum Phase {
    Skipped,
    Running { done: usize, total: usize },
}

/// Record that this run was skipped because nothing changed.
pub(super) fn write_skipped(path: &Path) {
    write_phase(path, &Phase::Skipped);
}

/// Counts finished targets, rewriting the progress file after each one.
pub(super) struct Progress {
    path: PathBuf,
    total: usize,
    done: Mutex<usize>,
}

impl Progress {
    pub(super) fn start(path: &Path, total: usize) -> Self {
        write_phase(path, &Phase::Running { done: 0, total });
        Progress {
            path: path.to_owned(),
            total,
            done: Mutex::new(0),
        }
    }

    pub(super) fn target_finished(&self) {
        let mut done = self.done.lock().unwrap();
        *done += 1;
        let phase = Phase::Running {
            done: *done,
            total: self.total,
        };
        write_phase(&self.path, &phase);
    }
}

/// Write via a temporary file so `run.sh` never reads a partial file.
fn write_phase(path: &Path, phase: &Phase) {
    let json = serde_json::to_string(phase).expect("phase always serializes");
    let tmp_path = path.with_extension("tmp");
    if let Err(e) = std::fs::write(&tmp_path, json).and_then(|_| std::fs::rename(&tmp_path, path)) {
        log::warn!("failed to write '{}': '{e}'", path.display());
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn progress_counts_finished_targets() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("progress.json");
        let progress = Progress::start(&path, 3);
        progress.target_finished();
        progress.target_finished();
        let written = std::fs::read_to_string(&path).unwrap();
        assert_eq!(written, r#"{"phase":"running","done":2,"total":3}"#);
    }

    #[test]
    fn skipped() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("progress.json");
        write_skipped(&path);
        let written = std::fs::read_to_string(&path).unwrap();
        assert_eq!(written, r#"{"phase":"skipped"}"#);
    }
}
