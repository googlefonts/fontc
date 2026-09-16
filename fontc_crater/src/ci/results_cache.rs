//! Caching fontmake's output between runs

use std::path::{Path, PathBuf};

use crate::{
    RunResult, Target,
    ttx_diff_runner::{DiffError, DiffOutput},
};

static CACHE_DIR_NAME: &str = "crater_cached_results";

// the files that we cache for each target. The font is all that is required;
// other files are derived from it.
static FONT_FILE: &str = "fontmake.ttf";
static DERIVED_FILES: [&str; 2] = ["fontmake.ttx", "fontmake.markkern.txt"];
// what ttx_diff leaves in place of the font when fontmake fails; keep in sync
// with core.py
static FAILURE_FILE: &str = "fontmake.failure.json";
// the previous run's result, keyed by the font fontc produced for this target
static RESULT_FILE: &str = "result.json";

/// What fontmake left in a build directory: a font, or a record of how it
/// failed. Either is enough to skip building it again.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum FontmakeOutput {
    Font,
    Failure,
}

impl FontmakeOutput {
    fn in_dir(dir: &Path) -> Option<Self> {
        if dir.join(FONT_FILE).exists() {
            Some(Self::Font)
        } else if dir.join(FAILURE_FILE).exists() {
            Some(Self::Failure)
        } else {
            None
        }
    }

    fn files(self) -> Vec<&'static str> {
        match self {
            Self::Font => std::iter::once(FONT_FILE).chain(DERIVED_FILES).collect(),
            Self::Failure => vec![FAILURE_FILE],
        }
    }
}

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
    /// Returns which of fontmake's outputs came from the cache, and so will
    /// not be rebuilt.
    pub fn copy_cached_files_to_build_dir(
        &self,
        target: &Target,
        build_dir: &Path,
    ) -> Option<FontmakeOutput> {
        let target_cache_dir = target.cache_dir(&self.base_results_cache_dir);
        if !target_cache_dir.exists() {
            log::trace!("no cached files for {target}");
            return None;
        }

        let copied = copy_cache_files(&target_cache_dir, build_dir).unwrap();
        if copied.is_some() {
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
        if copy_cache_files(build_dir, &target_cache_dir)
            .unwrap()
            .is_some()
        {
            log::trace!("saved cached files for {target}");
        }
    }
}

/// Copy whatever fontmake left in `from_dir`, skipping any files the
/// destination already has.
fn copy_cache_files(from_dir: &Path, to_dir: &Path) -> std::io::Result<Option<FontmakeOutput>> {
    let Some(output) = FontmakeOutput::in_dir(from_dir) else {
        return Ok(None);
    };
    if !to_dir.exists() {
        std::fs::create_dir_all(to_dir)?;
    }
    for name in output.files() {
        let (from, to) = (from_dir.join(name), to_dir.join(name));
        if from.exists() && !to.exists() {
            std::fs::copy(from, to)?;
        }
    }
    Ok(Some(output))
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

    fn write_files(dir: &Path, names: &[&str]) {
        std::fs::create_dir_all(dir).unwrap();
        for name in names {
            std::fs::write(dir.join(name), name).unwrap();
        }
    }

    fn file_names(dir: &Path) -> Vec<String> {
        let mut names = std::fs::read_dir(dir)
            .unwrap()
            .map(|e| e.unwrap().file_name().into_string().unwrap())
            .collect::<Vec<_>>();
        names.sort();
        names
    }

    #[test]
    fn font_alone_is_enough_to_cache() {
        let tempdir = tempfile::tempdir().unwrap();
        let cache = ResultsCache::in_dir(tempdir.path());
        let target = test_target();
        let build_dir = tempdir.path().join("build");
        write_files(&build_dir, &[FONT_FILE]);
        cache.save_built_files_to_cache(&target, &build_dir);

        let next_build_dir = tempdir.path().join("next_build");
        assert_eq!(
            cache.copy_cached_files_to_build_dir(&target, &next_build_dir),
            Some(FontmakeOutput::Font)
        );
        assert_eq!(file_names(&next_build_dir), [FONT_FILE]);
    }

    #[test]
    fn derived_files_are_added_to_an_existing_entry() {
        let tempdir = tempfile::tempdir().unwrap();
        let cache = ResultsCache::in_dir(tempdir.path());
        let target = test_target();
        let build_dir = tempdir.path().join("build");
        write_files(&build_dir, &[FONT_FILE]);
        cache.save_built_files_to_cache(&target, &build_dir);

        write_files(&build_dir, &DERIVED_FILES);
        cache.save_built_files_to_cache(&target, &build_dir);

        let next_build_dir = tempdir.path().join("next_build");
        assert_eq!(
            cache.copy_cached_files_to_build_dir(&target, &next_build_dir),
            Some(FontmakeOutput::Font)
        );
        assert_eq!(
            file_names(&next_build_dir),
            ["fontmake.markkern.txt", "fontmake.ttf", "fontmake.ttx"]
        );
    }

    #[test]
    fn nothing_is_cached_without_the_font() {
        let tempdir = tempfile::tempdir().unwrap();
        let cache = ResultsCache::in_dir(tempdir.path());
        let target = test_target();
        let build_dir = tempdir.path().join("build");
        write_files(&build_dir, &DERIVED_FILES);
        cache.save_built_files_to_cache(&target, &build_dir);

        let next_build_dir = tempdir.path().join("next_build");
        assert_eq!(
            cache.copy_cached_files_to_build_dir(&target, &next_build_dir),
            None
        );
    }

    #[test]
    fn failure_is_cached_in_place_of_the_font() {
        let tempdir = tempfile::tempdir().unwrap();
        let cache = ResultsCache::in_dir(tempdir.path());
        let target = test_target();
        let build_dir = tempdir.path().join("build");
        write_files(&build_dir, &[FAILURE_FILE]);
        cache.save_built_files_to_cache(&target, &build_dir);

        let next_build_dir = tempdir.path().join("next_build");
        assert_eq!(
            cache.copy_cached_files_to_build_dir(&target, &next_build_dir),
            Some(FontmakeOutput::Failure)
        );
        assert_eq!(file_names(&next_build_dir), [FAILURE_FILE]);
    }

    #[test]
    fn font_takes_precedence_over_failure() {
        let tempdir = tempfile::tempdir().unwrap();
        let cache = ResultsCache::in_dir(tempdir.path());
        let target = test_target();
        write_files(
            &target.cache_dir(&cache.base_results_cache_dir),
            &[FONT_FILE, FAILURE_FILE],
        );

        let next_build_dir = tempdir.path().join("next_build");
        assert_eq!(
            cache.copy_cached_files_to_build_dir(&target, &next_build_dir),
            Some(FontmakeOutput::Font)
        );
        assert_eq!(file_names(&next_build_dir), [FONT_FILE]);
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
