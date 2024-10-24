//! run bulk operations on fonts

use std::{
    collections::BTreeMap,
    fmt::Display,
    path::{Path, PathBuf},
    str::FromStr,
    sync::atomic::{AtomicUsize, Ordering},
};

use clap::Parser;
use google_fonts_sources::LoadRepoError;
use rayon::{prelude::*, ThreadPoolBuilder};

mod args;
mod ci;
mod error;
mod ttx_diff_runner;

use serde::{de::DeserializeOwned, Deserialize, Serialize};

use args::{Args, Commands};
use error::Error;

fn main() {
    env_logger::init();
    let args = Args::parse();
    if let Err(e) = run(&args) {
        eprintln!("{e}");
    }
}

fn run(args: &Args) -> Result<(), Error> {
    match &args.command {
        Commands::Ci(args) => ci::run_ci(args),
    }
}

/// Results of all runs
#[derive(Clone, Debug, serde::Serialize, serde::Deserialize)]
struct Results<T, E> {
    pub(crate) success: BTreeMap<TargetId, T>,
    pub(crate) failure: BTreeMap<TargetId, E>,
}

pub(crate) struct Target {
    // will be used in gftools mode
    pub(crate) _config: PathBuf,
    pub(crate) source: PathBuf,
}

/// Uniquely identify a source + build type (default, gftools)
///
/// this is separate from 'target' because it doesn't preserve the config path.
#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct TargetId {
    pub(crate) path: PathBuf,
    pub(crate) build: BuildType,
}

#[derive(Clone, Debug, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
enum BuildType {
    Default,
    GfTools,
}

/// The output of trying to run on one font.
///
/// We don't use a normal Result because failure is okay, we will report it all at the end.
enum RunResult<T, E> {
    Success(T),
    Fail(E),
}

/// Reason why we did not run a font
#[derive(Clone, Debug, serde::Deserialize, serde::Serialize)]
enum SkipReason {
    /// Checkout failed
    GitFail,
    /// There was no config.yaml file
    NoConfig,
    BadConfig(String),
}

#[allow(clippy::type_complexity)] // come on, it's not _that_ bad
fn run_all<T: Send, E: Send>(
    targets: Vec<Target>,
    cache_dir: &Path,
    runner: impl Fn(&Path, &Target) -> RunResult<T, E> + Send + Sync,
) -> Result<Vec<(Target, RunResult<T, E>)>, Error> {
    let total_targets = targets.len();
    let counter = AtomicUsize::new(0);
    let currently_running = AtomicUsize::new(0);
    let threadpool = ThreadPoolBuilder::new().build().unwrap();

    let results = threadpool.install(|| {
        targets
            .into_par_iter()
            .map(|target| {
                let i = counter.fetch_add(1, Ordering::Relaxed) + 1;
                currently_running.fetch_add(1, Ordering::Relaxed);
                log::debug!("starting {} ({i}/{total_targets})", target);
                let r = runner(cache_dir, &target);
                let n_running = currently_running.fetch_sub(1, Ordering::Relaxed);
                log::debug!("finished {} ({n_running} active)", target);
                (target, r)
            })
            .collect()
    });
    Ok(results)
}

/// Get the short sha of the current commit in the provided repository.
///
/// If no repo provided, run in current directory
///
/// returns `None` if the `git` command fails (for instance if the path is not
/// a git repository)
fn get_git_rev(repo_path: Option<&Path>) -> Option<String> {
    let mut cmd = std::process::Command::new("git");
    cmd.args(["rev-parse", "--short", "HEAD"]);

    if let Some(dir) = repo_path {
        cmd.current_dir(dir);
    }
    let output = cmd.output().unwrap();

    Some(
        std::str::from_utf8(&output.stdout)
            .expect("rev is always ascii/hex string")
            .trim()
            .to_owned(),
    )
}

impl Target {
    pub(crate) fn id(&self) -> TargetId {
        TargetId {
            path: self.source.clone(),
            build: crate::BuildType::Default,
        }
    }
}

impl Display for Target {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.id().fmt(f)
    }
}

impl Display for BuildType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            BuildType::Default => f.write_str("default"),
            BuildType::GfTools => f.write_str("gftools"),
        }
    }
}

impl Display for TargetId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{} ({})", self.path.display(), self.build)
    }
}

impl Serialize for TargetId {
    fn serialize<S: serde::Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        self.to_string().serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for TargetId {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let s: &str = Deserialize::deserialize(deserializer)?;
        FromStr::from_str(s).map_err(serde::de::Error::custom)
    }
}

impl FromStr for TargetId {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let s = s.trim();
        if !s.ends_with(')') {
            return Ok(Self {
                path: PathBuf::from(s),
                build: BuildType::Default,
            });
        }
        // else expect the format,
        // PATH (default|gftools)
        let (path, type_) = s
            .rsplit_once('(')
            .ok_or_else(|| "missing opening paren".to_string())?;

        let path = PathBuf::from(path.trim());
        let type_ = match type_.trim_end_matches(')') {
            "default" => BuildType::Default,
            "gftools" => BuildType::GfTools,
            other => return Err(format!("unknown build type '{other}'")),
        };

        Ok(TargetId { path, build: type_ })
    }
}

impl<T, E> FromIterator<(TargetId, RunResult<T, E>)> for Results<T, E> {
    fn from_iter<I: IntoIterator<Item = (TargetId, RunResult<T, E>)>>(iter: I) -> Self {
        let mut out = Results::default();
        for (path, reason) in iter.into_iter() {
            match reason {
                RunResult::Success(output) => {
                    out.success.insert(path, output);
                }
                RunResult::Fail(reason) => {
                    out.failure.insert(path, reason);
                }
            }
        }
        out
    }
}

impl<T, E> Default for Results<T, E> {
    fn default() -> Self {
        Self {
            success: Default::default(),
            failure: Default::default(),
        }
    }
}

impl From<LoadRepoError> for SkipReason {
    fn from(value: LoadRepoError) -> Self {
        match value {
            LoadRepoError::Io(_) | LoadRepoError::GitFail(_) | LoadRepoError::NoCommit { .. } => {
                SkipReason::GitFail
            }
            LoadRepoError::NoConfig => SkipReason::NoConfig,
            LoadRepoError::BadConfig(e) => SkipReason::BadConfig(e.to_string()),
        }
    }
}

impl std::fmt::Display for SkipReason {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SkipReason::GitFail => f.write_str("Git checkout failed"),
            SkipReason::NoConfig => f.write_str("No config.yaml file found"),
            SkipReason::BadConfig(e) => write!(f, "Failed to read config file: '{e}'"),
        }
    }
}

fn try_read_string(path: &Path) -> Result<String, Error> {
    std::fs::read_to_string(path).map_err(|error| Error::ReadFile {
        path: path.to_owned(),
        error,
    })
}

fn try_read_json<T: DeserializeOwned>(path: impl AsRef<Path>) -> Result<T, Error> {
    let path = path.as_ref();
    try_read_string(path).and_then(|content| {
        serde_json::from_str(&content).map_err(|error| Error::ParseJson {
            path: path.to_owned(),
            error,
        })
    })
}

fn try_write_str(s: &str, path: &Path) -> Result<(), Error> {
    std::fs::write(path, s).map_err(|error| Error::WriteFile {
        path: path.to_owned(),
        error,
    })
}

fn try_write_json<T: Serialize>(obj: &T, path: &Path) -> Result<(), Error> {
    serde_json::to_string_pretty(&obj)
        .map_err(|error| Error::WriteJson {
            path: path.to_owned(),
            error,
        })
        .and_then(|json_str| try_write_str(&json_str, path))
}

fn try_create_dir(path: &Path) -> Result<(), Error> {
    std::fs::create_dir_all(path).map_err(|error| Error::CreateDir {
        path: path.to_owned(),
        error,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn serde_target_id() {
        let id = TargetId {
            path: PathBuf::from("../my/file.is_here"),
            build: BuildType::GfTools,
        };

        let to_json = serde_json::to_string(&id).unwrap();
        let from_json: TargetId = serde_json::from_str(&to_json).unwrap();
        assert_eq!(id, from_json)
    }
}
