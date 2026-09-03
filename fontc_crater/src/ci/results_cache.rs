//! Caching fontmake's output between runs

use std::path::{Path, PathBuf};

use crate::{
    RunResult, Target,
    ttx_diff_runner::{DiffError, DiffOutput},
};

static CACHE_DIR_NAME: &str = "crater_cached_results";

// the files that we cache for each target
static FONT_FILE: &str = "fontmake.ttf";
static TTX_FILE: &str = "fontmake.ttx";
static MARKKERN_FILE: &str = "fontmake.markkern.txt";
// the previous run's result, keyed by the font fontc produced for this target
static RESULT_FILE: &str = "result.json";

/// A previous run's result, and the sha256 of the fontc.ttf it describes.
///
/// The diff is a function of the two compiled fonts, so if fontc produces the
/// same font again and fontmake's side still comes from this cache, this result
/// stands. This is the hash of fontc's *output*, not of the fontc binary; the
/// binary changes every run, which is the thing we are trying to look past.
#[derive(Debug, serde::Serialize, serde::Deserialize)]
pub(crate) struct CachedRun {
    pub(crate) fontc_ttf_hash: String,
    result: CachedResult,
}

#[derive(Debug, serde::Serialize, serde::Deserialize)]
#[serde(rename_all = "snake_case")]
enum CachedResult {
    Success(DiffOutput),
    Failure(DiffError),
}

impl CachedRun {
    pub(crate) fn into_result(self) -> RunResult<DiffOutput, DiffError> {
        match self.result {
            CachedResult::Success(output) => RunResult::Success(output),
            CachedResult::Failure(err) => RunResult::Fail(err),
        }
    }
}

/// Manages a cache of files on disk
pub(crate) struct ResultsCache {
    base_results_cache_dir: PathBuf,
}

impl ResultsCache {
    /// argument is the directory that will contain the cache dir.
    ///
    /// By convention this is the same directory where we checkout git repos.
    pub fn in_dir(path: &Path) -> Self {
        Self {
            base_results_cache_dir: path.join(CACHE_DIR_NAME),
        }
    }

    /// Delete any cache contents
    pub fn delete_all(&self) {
        if self.base_results_cache_dir.exists() {
            std::fs::remove_dir_all(&self.base_results_cache_dir).expect("failed to remove cache")
        }
    }

    /// if we have cached files for this target, copy them into the build directory.
    ///
    /// Returns `true` if fontmake's output came from the cache, and so will not
    /// be rebuilt.
    pub fn copy_cached_files_to_build_dir(&self, target: &Target, build_dir: &Path) -> bool {
        let target_cache_dir = target.cache_dir(&self.base_results_cache_dir);
        if !target_cache_dir.exists() {
            log::trace!("no cached files for {target}");
            return false;
        }

        let copied = copy_cache_files(&target_cache_dir, build_dir).unwrap();
        if copied {
            log::trace!("reused cached files for {target}",);
        }
        copied
    }

    /// The previous run's result for this target, if we have one.
    pub fn load_result(&self, target: &Target) -> Option<CachedRun> {
        let path = target
            .cache_dir(&self.base_results_cache_dir)
            .join(RESULT_FILE);
        if !path.exists() {
            return None;
        }
        match crate::try_read_json(&path) {
            Ok(run) => Some(run),
            Err(e) => {
                log::warn!("failed to load cached result for {target}: '{e}'");
                None
            }
        }
    }

    /// Record this run's result so the next run can skip the comparison if the
    /// font fontc produces is unchanged.
    pub fn save_result(
        &self,
        target: &Target,
        fontc_ttf_hash: String,
        result: &RunResult<DiffOutput, DiffError>,
    ) {
        let result = match result {
            RunResult::Success(output) => CachedResult::Success(output.clone()),
            RunResult::Fail(DiffError::CompileFailed(err)) => {
                CachedResult::Failure(DiffError::CompileFailed(err.clone()))
            }
            // a runtime error says nothing about these two binaries; retry next time
            RunResult::Fail(DiffError::Other(_)) => return,
        };
        let target_cache_dir = target.cache_dir(&self.base_results_cache_dir);
        if !target_cache_dir.exists() {
            std::fs::create_dir_all(&target_cache_dir).unwrap();
        }
        let run = CachedRun {
            fontc_ttf_hash,
            result,
        };
        if let Err(e) = crate::try_write_json(&run, &target_cache_dir.join(RESULT_FILE)) {
            log::warn!("failed to save cached result for {target}: '{e}'");
        }
    }

    /// Copy files generated from a previous run into the permanent cache.
    pub fn save_built_files_to_cache(&self, target: &Target, build_dir: &Path) {
        let target_cache_dir = target.cache_dir(&self.base_results_cache_dir);
        if !target_cache_dir.exists() {
            std::fs::create_dir_all(&target_cache_dir).unwrap();
        }
        // no need to overwrite existing cache
        if [FONT_FILE, TTX_FILE, MARKKERN_FILE]
            .into_iter()
            .all(|p| target_cache_dir.join(p).exists())
        {
            return;
        }
        if copy_cache_files(build_dir, &target_cache_dir).unwrap() {
            log::trace!("saved cached files for {target}");
        }
    }
}

fn copy_cache_files(from_dir: &Path, to_dir: &Path) -> std::io::Result<bool> {
    let font = from_dir.join(FONT_FILE);
    let ttx = from_dir.join(TTX_FILE);
    let markkern = from_dir.join(MARKKERN_FILE);

    if [&font, &ttx, &markkern].into_iter().all(|p| p.exists()) {
        if !to_dir.exists() {
            std::fs::create_dir_all(to_dir)?;
        }
        std::fs::copy(font, to_dir.join(FONT_FILE)).map(|_| ())?;
        std::fs::copy(ttx, to_dir.join(TTX_FILE)).map(|_| ())?;
        std::fs::copy(markkern, to_dir.join(MARKKERN_FILE)).map(|_| ())?;
        Ok(true)
    } else {
        Ok(false)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ttx_diff_runner::{CompileFailed, CompilerFailure, DiffValue};

    fn test_target() -> Target {
        Target::new(
            "org/repo_deadbeefc0",
            "deadbeefc0ffee",
            "sources/config.yaml",
            false,
            "Font.glyphs",
        )
    }

    #[test]
    fn result_round_trip() {
        let tempdir = tempfile::tempdir().unwrap();
        let cache = ResultsCache::in_dir(tempdir.path());
        let target = test_target();
        assert!(cache.load_result(&target).is_none());

        let diffs = [("GPOS".to_string(), DiffValue::Ratio(0.5))]
            .into_iter()
            .collect();
        cache.save_result(
            &target,
            "abc123".into(),
            &RunResult::Success(DiffOutput::Diffs(diffs)),
        );

        let loaded = cache.load_result(&target).expect("just saved it");
        assert_eq!(loaded.fontc_ttf_hash, "abc123");
        let RunResult::Success(DiffOutput::Diffs(diffs)) = loaded.into_result() else {
            panic!("expected diffs");
        };
        assert_eq!(diffs.get("GPOS"), Some(&DiffValue::Ratio(0.5)));
    }

    #[test]
    fn compile_failures_are_cached() {
        let tempdir = tempfile::tempdir().unwrap();
        let cache = ResultsCache::in_dir(tempdir.path());
        let target = test_target();
        cache.save_result(
            &target,
            "abc123".into(),
            &RunResult::Fail(DiffError::CompileFailed(CompileFailed {
                fontc: None,
                fontmake: Some(CompilerFailure {
                    command: "fontmake -o variable".into(),
                    stderr: "oh no".into(),
                }),
            })),
        );

        let loaded = cache.load_result(&target).expect("just saved it");
        let RunResult::Fail(DiffError::CompileFailed(failed)) = loaded.into_result() else {
            panic!("expected a compile failure");
        };
        assert!(failed.fontc.is_none());
        assert_eq!(failed.fontmake.unwrap().stderr, "oh no");
    }

    #[test]
    fn runtime_failures_are_not_cached() {
        let tempdir = tempfile::tempdir().unwrap();
        let cache = ResultsCache::in_dir(tempdir.path());
        let target = test_target();
        cache.save_result(
            &target,
            "abc123".into(),
            &RunResult::Fail(DiffError::Other("ttx_diff timed out".into())),
        );
        assert!(cache.load_result(&target).is_none());
    }

    #[test]
    fn delete_all_clears_results() {
        let tempdir = tempfile::tempdir().unwrap();
        let cache = ResultsCache::in_dir(tempdir.path());
        let target = test_target();
        cache.save_result(
            &target,
            "abc123".into(),
            &RunResult::Success(DiffOutput::Identical),
        );
        assert!(cache.load_result(&target).is_some());
        cache.delete_all();
        assert!(cache.load_result(&target).is_none());
    }
}
