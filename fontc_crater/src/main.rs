//! run bulk operations on fonts

use std::{
    collections::{BTreeMap, BTreeSet},
    path::{Path, PathBuf},
    time::Instant,
};

use clap::Parser;
use fontc::JobTimer;
use google_fonts_sources::RepoInfo;
use write_fonts::types::Tag;

mod args;
mod error;
mod sources;

use sources::RepoList;

use args::{Args, Tasks};
use error::Error;

fn main() {
    env_logger::init();
    let args = Args::parse();
    if let Err(e) = run(&args) {
        eprintln!("{e}");
    }
}

fn run(args: &Args) -> Result<(), Error> {
    if !args.font_cache.exists() {
        std::fs::create_dir_all(&args.font_cache).map_err(Error::CacheDir)?;
    }
    let sources = RepoList::get_or_create(&args.font_cache, args.fonts_repo.as_deref())?;

    match args.command {
        Tasks::Compile => {
            compile_all(&sources.sources, &args.font_cache, args.out_path.as_deref())?
        }
    };
    sources.save(&args.font_cache)?;
    Ok(())
}

/// Results of all runs
#[derive(Clone, Debug, Default, serde::Serialize, serde::Deserialize)]
struct Results {
    success: BTreeSet<PathBuf>,
    failure: BTreeMap<PathBuf, String>,
    panic: BTreeSet<PathBuf>,
    skipped: BTreeMap<PathBuf, SkipReason>,
}

/// The output of trying to run on one font.
///
/// We don't use a normal Result because failure is okay, we will report it all at the end.
enum RunResult {
    Skipped(SkipReason),
    Success,
    Fail(String),
    Panic,
}

/// Reason why we did not run a font
#[derive(Clone, Debug, serde::Deserialize, serde::Serialize)]
enum SkipReason {
    /// Checkout failed
    GitFail,
    /// There was no config.yaml file
    NoConfig,
}

fn compile_all(
    sources: &[RepoInfo],
    cache_dir: &Path,
    out_path: Option<&Path>,
) -> Result<(), Error> {
    let results = sources
        .iter()
        .flat_map(|info| {
            let font_dir = cache_dir.join(&info.repo_name);
            fetch_and_run_repo(&font_dir, info)
        })
        .collect::<Results>();

    if let Some(path) = out_path {
        let as_json = serde_json::to_string_pretty(&results).map_err(Error::OutputJson)?;
        std::fs::write(path, as_json).map_err(|error| Error::WriteFile {
            path: path.to_owned(),
            error,
        })?;
    } else {
        results.print_summary();
    }
    Ok(())
}

// one repo can contain multiple sources, so we return a vec.
fn fetch_and_run_repo(font_dir: &Path, repo: &RepoInfo) -> Vec<(PathBuf, RunResult)> {
    if !font_dir.exists() && clone_repo(font_dir, &repo.repo_url).is_err() {
        return vec![(font_dir.to_owned(), RunResult::Skipped(SkipReason::GitFail))];
    }

    let source_dir = font_dir.join("sources");
    let configs = repo
        .config_files
        .iter()
        .flat_map(|filename| {
            let config_path = source_dir.join(filename);
            load_config(&config_path)
        })
        .collect::<Vec<_>>();

    if configs.is_empty() {
        return vec![(
            font_dir.to_owned(),
            RunResult::Skipped(SkipReason::NoConfig),
        )];
    };

    // collect to set in case configs duplicate sources
    let sources = configs
        .iter()
        .flat_map(|c| c.sources.iter())
        .map(|source| source_dir.join(source))
        .collect::<BTreeSet<_>>();

    sources
        .into_iter()
        .map(|source| {
            let result = compile_one(&source);
            (source, result)
        })
        .collect()
}

fn compile_one(source_path: &Path) -> RunResult {
    let tempdir = tempfile::tempdir().unwrap();
    let args = fontc::Args::new(tempdir.path(), source_path.to_owned());
    let timer = JobTimer::new(Instant::now());
    eprintln!("compiling {}", source_path.display());
    match std::panic::catch_unwind(|| fontc::run(args, timer)) {
        Ok(Ok(_)) => RunResult::Success,
        Ok(Err(e)) => RunResult::Fail(e.to_string()),
        Err(_) => RunResult::Panic,
    }
}

// on fail returns contents of stderr
fn clone_repo(to_dir: &Path, repo: &str) -> Result<(), String> {
    assert!(!to_dir.exists());
    eprintln!("cloning '{repo}' to {}", to_dir.display());
    let output = std::process::Command::new("git")
        // if a repo requires credentials fail instead of waiting
        .env("GIT_TERMINAL_PROMPT", "0")
        .arg("clone")
        .args(["--depth", "1"])
        .arg(repo)
        .arg(to_dir)
        .output()
        .expect("failed to execute git command");

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        eprintln!("clone failed: '{stderr}'");
        return Err(stderr.into_owned());
    }
    Ok(())
}

/// Parse and return a config.yaml file for the provided font source
fn load_config(config_path: &Path) -> Option<Config> {
    let contents = match std::fs::read_to_string(config_path) {
        Ok(contents) => contents,
        Err(e) => {
            eprintln!("failed to load config at '{}': {e}", config_path.display());
            return None;
        }
    };
    match serde_yaml::from_str(&contents) {
        Ok(config) => Some(config),
        Err(e) => {
            eprintln!("BAD YAML: {contents}: '{e}'");
            None
        }
    }
}

impl FromIterator<(PathBuf, RunResult)> for Results {
    fn from_iter<T: IntoIterator<Item = (PathBuf, RunResult)>>(iter: T) -> Self {
        let mut out = Results::default();
        for (path, reason) in iter.into_iter() {
            match reason {
                RunResult::Skipped(reason) => {
                    out.skipped.insert(path, reason);
                }
                RunResult::Success => {
                    out.success.insert(path);
                }
                RunResult::Fail(reason) => {
                    out.failure.insert(path, reason);
                }
                RunResult::Panic => {
                    out.panic.insert(path);
                }
            }
        }
        out
    }
}

impl Results {
    fn print_summary(&self) {
        let total = self.success.len() + self.failure.len() + self.panic.len() + self.skipped.len();

        println!(
            "\ncompiled {total} fonts: {} skipped, {} panics, {} failures {} success",
            self.skipped.len(),
            self.panic.len(),
            self.failure.len(),
            self.success.len(),
        );

        if self.skipped.is_empty() {
            println!("\n#### {} fonts were skipped ####", self.skipped.len());
            for (path, reason) in &self.skipped {
                println!("{}: {}", path.display(), reason);
            }
        }

        if !self.panic.is_empty() {
            println!(
                "\n#### {} fonts panicked the compiler: ####",
                self.panic.len()
            );

            for path in &self.panic {
                println!("{}", path.display())
            }
        }

        if !self.failure.is_empty() {
            println!("\n#### {} fonts failed to compile ####", self.failure.len());
            for path in self.failure.keys() {
                println!("{}", path.display());
            }
        }
    }
}

/// Google fonts config file ('config.yaml')
#[derive(Clone, Debug, serde::Deserialize)]
#[serde(rename_all = "camelCase")]
// there are a few fields of this that we dont' care about but parse anyway?
// maybe we will want them later?
#[allow(dead_code)]
struct Config {
    sources: Vec<String>,
    family_name: Option<String>,
    #[serde(default)]
    build_variable: bool,
    #[serde(default)]
    axis_order: Vec<Tag>,
}

impl std::fmt::Display for SkipReason {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            SkipReason::GitFail => f.write_str("Git checkout failed"),
            SkipReason::NoConfig => f.write_str("No config.yaml file found"),
        }
    }
}
