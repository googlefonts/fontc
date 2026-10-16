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
    Preparing,
    Skipped,
    Running { done: usize, total: usize },
}

#[derive(serde::Serialize)]
struct Record<'a> {
    #[serde(flatten)]
    phase: Phase,
    fontc_rev: &'a str,
    compare_url: &'a str,
}

/// The progress file for a run, which names the fontc revision being tested.
pub(super) struct ProgressFile {
    path: PathBuf,
    fontc_rev: String,
    compare_url: String,
}

impl ProgressFile {
    /// Record that a run of `fontc_rev` is preparing.
    ///
    /// `compare_url` is the link the run will get in the report.
    pub(super) fn new(path: &Path, fontc_rev: &str, compare_url: String) -> Self {
        let file = ProgressFile {
            path: path.to_owned(),
            fontc_rev: fontc_rev.to_owned(),
            compare_url,
        };
        file.write(Phase::Preparing);
        file
    }

    /// Record that this run was skipped because nothing changed.
    pub(super) fn write_skipped(&self) {
        self.write(Phase::Skipped);
    }

    /// Write via a temporary file so `run.sh` never reads a partial file.
    fn write(&self, phase: Phase) {
        let record = Record {
            phase,
            fontc_rev: &self.fontc_rev,
            compare_url: &self.compare_url,
        };
        let json = serde_json::to_string(&record).expect("record always serializes");
        let tmp_path = self.path.with_extension("tmp");
        if let Err(e) =
            std::fs::write(&tmp_path, json).and_then(|_| std::fs::rename(&tmp_path, &self.path))
        {
            log::warn!("failed to write '{}': '{e}'", self.path.display());
        }
    }
}

/// Counts finished targets, rewriting the progress file after each one.
pub(super) struct Progress {
    file: ProgressFile,
    total: usize,
    done: Mutex<usize>,
}

impl Progress {
    pub(super) fn start(file: ProgressFile, total: usize) -> Self {
        file.write(Phase::Running { done: 0, total });
        Progress {
            file,
            total,
            done: Mutex::new(0),
        }
    }

    pub(super) fn target_finished(&self) {
        let mut done = self.done.lock().unwrap();
        *done += 1;
        self.file.write(Phase::Running {
            done: *done,
            total: self.total,
        });
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const URL: &str = "https://example.com/compare";

    #[test]
    fn preparing() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("progress.json");
        ProgressFile::new(&path, "abc123", URL.into());
        let written = std::fs::read_to_string(&path).unwrap();
        assert_eq!(
            written,
            r#"{"phase":"preparing","fontc_rev":"abc123","compare_url":"https://example.com/compare"}"#
        );
    }

    #[test]
    fn progress_counts_finished_targets() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("progress.json");
        let progress = Progress::start(ProgressFile::new(&path, "abc123", URL.into()), 3);
        progress.target_finished();
        progress.target_finished();
        let written = std::fs::read_to_string(&path).unwrap();
        assert_eq!(
            written,
            r#"{"phase":"running","done":2,"total":3,"fontc_rev":"abc123","compare_url":"https://example.com/compare"}"#
        );
    }

    #[test]
    fn skipped() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("progress.json");
        ProgressFile::new(&path, "abc123", URL.into()).write_skipped();
        let written = std::fs::read_to_string(&path).unwrap();
        assert_eq!(
            written,
            r#"{"phase":"skipped","fontc_rev":"abc123","compare_url":"https://example.com/compare"}"#
        );
    }
}
