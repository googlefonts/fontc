//! run bulk operations on fonts

use std::{
    collections::{BTreeMap, BTreeSet},
    path::{Path, PathBuf},
    sync::atomic::{AtomicUsize, Ordering},
    time::Instant,
};

use clap::Parser;
use fontc::JobTimer;
use google_fonts_sources::RepoInfo;
use rayon::prelude::*;

mod args;
mod error;
mod sources;
mod ttx_diff_runner;

use serde::{de::DeserializeOwned, Serialize};
use sources::{Config, RepoList};

use args::{Args, Commands, ReportArgs};
use error::Error;
use ttx_diff_runner::{DiffError, DiffOutput};

fn main() {
    env_logger::init();
    let args = Args::parse();
    if let Err(e) = run(&args) {
        eprintln!("{e}");
    }
}

fn run(args: &Args) -> Result<(), Error> {
    let run_args = match &args.command {
        Commands::Compile(args) => args,
        Commands::Diff(args) => args,
        Commands::Report(args) => return generate_report(args),
    };

    if !run_args.font_cache.exists() {
        try_create_dir(&run_args.font_cache)?;
    }
    let sources = RepoList::get_or_create(&run_args.font_cache, run_args.fonts_repo.as_deref())?;

    let pruned = run_args.n_fonts.map(|n| prune_sources(&sources.sources, n));
    let inputs = pruned.as_ref().unwrap_or(&sources.sources);

    match args.command {
        Commands::Compile { .. } => run_all(
            inputs,
            &run_args.font_cache,
            run_args.out_path.as_deref(),
            compile_one,
        )?,
        Commands::Diff { .. } => {
            ttx_diff_runner::assert_can_run_script();
            run_all(
                inputs,
                &run_args.font_cache,
                run_args.out_path.as_deref(),
                ttx_diff_runner::run_ttx_diff,
            )?;
        }
        Commands::Report { .. } => unreachable!("handled above"),
    };
    sources.save(&run_args.font_cache)?;
    Ok(())
}

fn generate_report(args: &ReportArgs) -> Result<(), Error> {
    let contents = try_read_string(&args.json_path)?;
    // let's just try and detect the type of the json?
    if let Ok(results) = serde_json::from_str::<Results<DiffOutput, DiffError>>(&contents) {
        ttx_diff_runner::print_report(&results, args.verbose);
    } else {
        let results = serde_json::from_str::<Results<(), String>>(&contents)
            .map_err(|error| Error::ParseJson {
                path: args.json_path.clone(),
                error,
            })
            // for a while a map of (string: null) was being serialized as a sequence?
            // so for now we just try parsing both forms
            .or_else(|_| deserialize_compile_json(&contents, &args.json_path))?;
        results.print_summary(args.verbose)
    }
    Ok(())
}

// a map of (string, ()) gets serialized as a list by serde_json
fn deserialize_compile_json(json_str: &str, path: &Path) -> Result<Results<(), String>, Error> {
    #[derive(serde::Deserialize)]
    struct Helper {
        success: Vec<PathBuf>,
        failure: BTreeMap<PathBuf, String>,
        panic: BTreeSet<PathBuf>,
        skipped: BTreeMap<PathBuf, SkipReason>,
    }

    serde_json::from_str(json_str)
        .map_err(|error| Error::ParseJson {
            path: path.to_owned(),
            error,
        })
        .map(
            |Helper {
                 success,
                 failure,
                 panic,
                 skipped,
             }| Results {
                success: success.into_iter().map(|p| (p, ())).collect(),
                failure,
                panic,
                skipped,
            },
        )
}

// only generic so I can write tests
fn prune_sources<T: Clone>(sources: &[T], n_items: usize) -> Vec<T> {
    if n_items == 0 || sources.is_empty() {
        return Vec::new();
    }

    if n_items >= sources.len() {
        return sources.to_owned();
    }

    // this is probably very dumb? I just want to use modular arithmetic to
    // take a consistent subset of the input items, and I'm bad at math.
    // I'm sure there is a better way to do this...

    let ratio = (n_items as f32) / sources.len() as f32;
    let modus = if ratio <= 0.5 {
        // floor here and ceil below because we want to err on taking more items,
        // since we will iter().take() the correct number below
        (1. / ratio).floor() as usize
    } else {
        (1. / (1. - ratio)).ceil() as usize
    };

    let filter_fn = |n| {
        // basically: if we want to take 1/8 of items we do n % 6 == 0,
        // and if we want to take 7/8 of items we do n % 6 != 0
        if ratio <= 0.5 {
            n % modus == 0
        } else {
            n % modus != 0
        }
    };

    sources
        .iter()
        .enumerate()
        .filter_map(|(i, x)| filter_fn(i).then_some(x.clone()))
        .take(n_items)
        .collect()
}

/// Results of all runs
#[derive(Clone, Debug, serde::Serialize, serde::Deserialize)]
struct Results<T, E> {
    success: BTreeMap<PathBuf, T>,
    failure: BTreeMap<PathBuf, E>,
    panic: BTreeSet<PathBuf>,
    skipped: BTreeMap<PathBuf, SkipReason>,
}

/// The output of trying to run on one font.
///
/// We don't use a normal Result because failure is okay, we will report it all at the end.
enum RunResult<T, E> {
    Skipped(SkipReason),
    Success(T),
    Fail(E),
    Panic,
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

fn run_all<T: serde::Serialize + Send, E: serde::Serialize + Send>(
    sources: &[RepoInfo],
    cache_dir: &Path,
    out_path: Option<&Path>,
    runner: impl Fn(&Path) -> RunResult<T, E> + Send + Sync,
) -> Result<(), Error> {
    let mut skipped: Vec<(_, RunResult<T, E>)> = Vec::new();
    let mut targets = Vec::new();
    for source in sources {
        let font_dir = cache_dir.join(&source.repo_name);
        match get_targets_for_repo(&font_dir, source) {
            Ok(repo_targets) => targets.extend(repo_targets),
            Err(e) => skipped.push((font_dir, RunResult::Skipped(e))),
        }
    }
    let total_targets = targets.len();
    let counter = AtomicUsize::new(0);
    let results = targets
        .into_par_iter()
        .map(|target| {
            let i = counter.fetch_add(1, Ordering::Relaxed);
            eprintln!("running {} ({i}/{total_targets})", target.display());
            let r = runner(&target);
            (target, r)
        })
        .collect::<Vec<_>>();
    let results = results
        .into_iter()
        .chain(skipped)
        .collect::<Results<_, _>>();

    if let Some(path) = out_path {
        try_write_json(&results, path)?;
    } else {
        results.print_summary(true);
    }
    Ok(())
}

// one repo can contain multiple sources, so we return a vec.
fn get_targets_for_repo(font_dir: &Path, repo: &RepoInfo) -> Result<Vec<PathBuf>, SkipReason> {
    if !font_dir.exists() && clone_repo(font_dir, &repo.repo_url).is_err() {
        return Err(SkipReason::GitFail);
    }

    let source_dir = font_dir.join("sources");
    let configs = repo
        .config_files
        .iter()
        .map(|filename| {
            let config_path = source_dir.join(filename);
            Config::load(&config_path)
        })
        .collect::<Result<Vec<_>, _>>();

    let configs = match configs {
        Ok(c) if c.is_empty() => return Err(SkipReason::NoConfig),
        Err(e) => return Err(SkipReason::BadConfig(e.to_string())),
        Ok(c) => c,
    };

    // collect to set in case configs duplicate sources
    let sources = configs
        .iter()
        .flat_map(|c| c.sources.iter())
        .map(|source| source_dir.join(source))
        .collect::<BTreeSet<_>>();

    Ok(sources.into_iter().collect())
}

fn compile_one(source_path: &Path) -> RunResult<(), String> {
    let tempdir = tempfile::tempdir().unwrap();
    let args = fontc::Args::new(tempdir.path(), source_path.to_owned());
    let timer = JobTimer::new(Instant::now());
    match std::panic::catch_unwind(|| fontc::run(args, timer)) {
        Ok(Ok(_)) => RunResult::Success(()),
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

impl<T, E> FromIterator<(PathBuf, RunResult<T, E>)> for Results<T, E> {
    fn from_iter<I: IntoIterator<Item = (PathBuf, RunResult<T, E>)>>(iter: I) -> Self {
        let mut out = Results::default();
        for (path, reason) in iter.into_iter() {
            match reason {
                RunResult::Skipped(reason) => {
                    out.skipped.insert(path, reason);
                }
                RunResult::Success(output) => {
                    out.success.insert(path, output);
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

impl<T, E> Results<T, E> {
    fn print_summary(&self, verbose: bool) {
        let total = self.success.len() + self.failure.len() + self.panic.len() + self.skipped.len();

        println!(
            "\ncompiled {total} fonts: {} skipped, {} panics, {} failures {} success",
            self.skipped.len(),
            self.panic.len(),
            self.failure.len(),
            self.success.len(),
        );
        if !verbose {
            return;
        }

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

impl<T, E> Default for Results<T, E> {
    fn default() -> Self {
        Self {
            success: Default::default(),
            failure: Default::default(),
            panic: Default::default(),
            skipped: Default::default(),
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

fn try_read_json<T: DeserializeOwned>(path: &Path) -> Result<T, Error> {
    try_read_string(path).and_then(|content| {
        serde_json::from_str(&content).map_err(|error| Error::ParseJson {
            path: path.to_owned(),
            error,
        })
    })
}

fn try_write_json<T: Serialize>(obj: &T, path: &Path) -> Result<(), Error> {
    serde_json::to_string_pretty(&obj)
        .map_err(|error| Error::WriteJson {
            path: path.to_owned(),
            error,
        })
        .and_then(|json_str| {
            std::fs::write(path, json_str).map_err(|error| Error::WriteFile {
                path: path.to_owned(),
                error,
            })
        })
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
    fn prune_items_smoke_test() {
        let items = (0usize..100).collect::<Vec<_>>();
        assert_eq!(prune_sources(&items, 100).len(), 100);
        assert_eq!(prune_sources(&items, 200).len(), 100);
        assert_eq!(prune_sources(&items, 101).len(), 100);
        assert_eq!(prune_sources(&items, 20).len(), 20);
        assert_eq!(prune_sources(&items, 80).len(), 80);
        assert_eq!(prune_sources(&items, 9).len(), 9);
        assert_eq!(
            prune_sources(&items, 9),
            &[0, 11, 22, 33, 44, 55, 66, 77, 88]
        );
    }
}
