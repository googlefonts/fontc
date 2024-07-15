//! run bulk operations on fonts

use std::{
    collections::{BTreeMap, BTreeSet},
    path::{Path, PathBuf},
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

use sources::{Config, RepoList};

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

    let pruned = args.n_fonts.map(|n| prune_sources(&sources.sources, n));
    let inputs = pruned.as_ref().unwrap_or(&sources.sources);

    match args.command {
        Tasks::Compile => run_all(
            inputs,
            &args.font_cache,
            args.out_path.as_deref(),
            compile_one,
        )?,
        Tasks::Diff => {
            ttx_diff_runner::assert_can_run_script();
            run_all(
                inputs,
                &args.font_cache,
                args.out_path.as_deref(),
                ttx_diff_runner::run_ttx_diff,
            )?;
        }
    };
    sources.save(&args.font_cache)?;
    Ok(())
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
}

fn run_all<T: serde::Serialize + Send, E: serde::Serialize + Send>(
    sources: &[RepoInfo],
    cache_dir: &Path,
    out_path: Option<&Path>,
    runner: impl Fn(&Path) -> RunResult<T, E> + Send + Sync,
) -> Result<(), Error> {
    let results = sources
        .par_iter()
        .flat_map(|info| {
            let font_dir = cache_dir.join(&info.repo_name);
            fetch_and_run_repo(&font_dir, info, |p| runner(p))
        })
        .collect::<Vec<_>>();
    let results = results.into_iter().collect::<Results<_, _>>();

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
fn fetch_and_run_repo<T: Send, E: Send>(
    font_dir: &Path,
    repo: &RepoInfo,
    runner: impl Fn(&Path) -> RunResult<T, E> + Send + Sync,
) -> Vec<(PathBuf, RunResult<T, E>)> {
    if !font_dir.exists() && clone_repo(font_dir, &repo.repo_url).is_err() {
        return vec![(font_dir.to_owned(), RunResult::Skipped(SkipReason::GitFail))];
    }

    let source_dir = font_dir.join("sources");
    let configs = repo
        .config_files
        .iter()
        .flat_map(|filename| {
            let config_path = source_dir.join(filename);
            Config::load(&config_path)
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
            eprintln!("running {}", source.display());
            let result = runner(&source);
            (source, result)
        })
        .collect()
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
        }
    }
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
