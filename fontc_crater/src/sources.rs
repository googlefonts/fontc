//! finding font sources
use std::{
    path::Path,
    time::{Duration, SystemTime},
};

use google_fonts_sources::RepoInfo;
use serde::{Deserialize, Serialize};

use crate::error::Error;

static CACHED_REPO_INFO_FILE: &str = "google_fonts_repos.json";
const ONE_WEEK: Duration = Duration::from_secs(60 * 60 * 24 * 7);

#[derive(Clone, Debug, Deserialize, Serialize)]
pub(crate) struct RepoList {
    pub(crate) created: SystemTime,
    pub(crate) sources: Vec<RepoInfo>,
}

impl RepoList {
    pub(crate) fn get_or_create(cache_dir: &Path) -> Result<Self, Error> {
        let cache_file_path = cache_dir.join(CACHED_REPO_INFO_FILE);
        if let Some(cached_list) = Self::load(&cache_file_path)? {
            let stale = cached_list
                .created
                .elapsed()
                .map(|d| d > ONE_WEEK)
                .unwrap_or(true);
            if !stale {
                return Ok(cached_list);
            }
        }

        let mut sources = google_fonts_sources::discover_sources(cache_dir)?;

        // only keep sources for which we have a repo + config
        sources.retain(|s| !s.config_files.is_empty());

        Ok(RepoList {
            created: SystemTime::now(),
            sources,
        })
    }

    fn load(path: &Path) -> Result<Option<Self>, Error> {
        if !path.exists() {
            return Ok(None);
        }
        Some(super::try_read_json(path)).transpose()
    }

    pub(crate) fn save(&self, cache_dir: &Path) -> Result<(), Error> {
        let path = cache_dir.join(CACHED_REPO_INFO_FILE);
        super::try_write_json(&self, &path)
    }
}
