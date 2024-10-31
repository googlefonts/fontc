//! Caching fontmake's output between runs

use std::path::{Path, PathBuf};

use crate::Target;

static CACHE_DIR_NAME: &str = "crater_cached_results";

// the files that we cache for each target
static FONT_FILE: &str = "fontmake.ttf";
static TTX_FILE: &str = "fontmake.ttx";
static MARKKERN_FILE: &str = "fontmake.markkern.txt";

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
    pub fn copy_cached_files_to_build_dir(&self, target: &Target, build_dir: &Path) {
        let target_cache_dir = target.cache_dir(&self.base_results_cache_dir);
        if !target_cache_dir.exists() {
            log::info!("{} does not exist, skipping", target_cache_dir.display());
            return;
        }

        if copy_cache_files(&target_cache_dir, build_dir).unwrap() {
            log::info!("reused cached files for {target}",);
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
            log::debug!("cached files to {}", target_cache_dir.display());
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
