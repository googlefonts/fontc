//! finding font sources
use std::{
    path::Path,
    time::{Duration, SystemTime},
};

use google_fonts_sources::RepoInfo;
use serde::{Deserialize, Serialize};
use write_fonts::types::Tag;

use crate::error::Error;

static CACHED_REPO_INFO_FILE: &str = "google_fonts_repos.json";
const ONE_WEEK: Duration = Duration::from_secs(60 * 60 * 24 * 7);

/// Google fonts config file ('config.yaml')
#[derive(Clone, Debug, serde::Deserialize)]
#[serde(rename_all = "camelCase")]
// there are a few fields of this that we dont' care about but parse anyway?
// maybe we will want them later?
#[allow(dead_code)]
pub(crate) struct Config {
    pub(crate) sources: Vec<String>,
    family_name: Option<String>,
    #[serde(default)]
    build_variable: bool,
    #[serde(default)]
    axis_order: Vec<Tag>,
}

#[derive(Debug, thiserror::Error)]
pub(crate) enum BadConfig {
    #[error(transparent)]
    Read(#[from] std::io::Error),
    #[error(transparent)]
    Yaml(serde_yaml::Error),
}

impl Config {
    /// Parse and return a config.yaml file for the provided font source
    pub(crate) fn load(config_path: &Path) -> Result<Self, BadConfig> {
        let contents = std::fs::read_to_string(config_path)?;
        serde_yaml::from_str(&contents).map_err(BadConfig::Yaml)
    }
}

#[derive(Clone, Debug, Deserialize, Serialize)]
pub(crate) struct RepoList {
    pub(crate) created: SystemTime,
    pub(crate) sources: Vec<RepoInfo>,
}

impl RepoList {
    pub(crate) fn get_or_create(
        cache_dir: &Path,
        fonts_repo: Option<&Path>,
    ) -> Result<Self, Error> {
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

        let mut sources =
            google_fonts_sources::discover_sources(fonts_repo, Some(cache_dir), false);

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
        let string = std::fs::read_to_string(path).map_err(Error::InputFile)?;
        Some(serde_json::from_str(&string).map_err(Error::InputJson)).transpose()
    }

    pub(crate) fn save(&self, cache_dir: &Path) -> Result<(), Error> {
        let path = cache_dir.join(CACHED_REPO_INFO_FILE);
        let string = serde_json::to_string_pretty(&self).map_err(Error::OutputJson)?;
        std::fs::write(&path, string).map_err(|error| Error::WriteFile { path, error })
    }
}
