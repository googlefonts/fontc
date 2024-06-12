//! run bulk operations on fonts

use std::{
    collections::BTreeMap,
    path::{Path, PathBuf},
    time::Instant,
};

use clap::Parser;
use fontc::JobTimer;
use serde::Deserialize;
use write_fonts::types::Tag;

mod args;
mod error;

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
    let input = load_input_file(&args.input_list)?;
    match args.command {
        Tasks::Compile => compile_all(input, &args.font_cache, args.out_path.as_deref())?,
    };
    Ok(())
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
enum SkipReason {
    /// Checkout failed
    GitFail,
    /// There was no config.yaml file
    NoConfig,
}

fn compile_all(
    inputs: BTreeMap<String, String>,
    cache_dir: &Path,
    out_path: Option<&Path>,
) -> Result<(), Error> {
    let results = inputs
        .into_iter()
        .flat_map(|(font, info)| {
            let font_dir = cache_dir.join(font);
            fetch_and_run_repo(&font_dir, &info)
        })
        .collect::<BTreeMap<_, _>>();

    let mut panics = Vec::new();
    let mut fails = Vec::new();
    let mut skipped = Vec::new();
    let mut success = 0;
    for (path, result) in &results {
        match result {
            RunResult::Fail(err) => fails.push((path, err)),
            RunResult::Panic => panics.push(path),
            RunResult::Skipped(reason) => skipped.push((path, reason)),
            RunResult::Success => success += 1,
        }
    }

    let total = panics.len() + fails.len() + skipped.len() + success;
    println!(
        "\ncompiled {total} fonts: {} skipped, {} panics, {} failures {success} success",
        skipped.len(),
        panics.len(),
        fails.len()
    );
    if !skipped.is_empty() {
        println!("\n#### {} fonts were skipped ####", skipped.len());
        for (path, reason) in &skipped {
            println!("{}: {}", path.display(), reason);
        }
    }

    if !panics.is_empty() {
        println!("\n#### {} fonts panicked the compiler: ####", panics.len());
        for path in panics {
            println!("{}", path.display())
        }
    }

    if !fails.is_empty() {
        println!("\n#### {} fonts failed to compile ####", fails.len());
        for (path, _) in &fails {
            println!("{}", path.display());
        }
    }

    if let Some(path) = out_path {
        let as_map = fails
            .into_iter()
            .map(|(p, e)| (p, e.to_string()))
            .collect::<BTreeMap<_, _>>();
        let as_json = serde_json::to_string_pretty(&as_map).unwrap();
        std::fs::write(path, as_json).map_err(Error::OutputFile)?;
    }
    Ok(())
}

// one repo can contain multiple sources, so we return a vec.
fn fetch_and_run_repo(font_dir: &Path, repo: &str) -> Vec<(PathBuf, RunResult)> {
    if !font_dir.exists() && clone_repo(font_dir, repo).is_err() {
        return vec![(font_dir.to_owned(), RunResult::Skipped(SkipReason::GitFail))];
    }

    let Some(config) = get_config(font_dir) else {
        return vec![(
            font_dir.to_owned(),
            RunResult::Skipped(SkipReason::NoConfig),
        )];
    };

    config
        .sources
        .into_iter()
        .map(|source| {
            let source_path = font_dir.join("sources").join(source);
            let result = compile_one(&source_path);
            (source_path, result)
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
    let url = format!("https://github.com/{repo}",);
    eprintln!("cloning '{url}' to {}", to_dir.display());
    let output = std::process::Command::new("git")
        // if a repo requires credentials fail instead of waiting
        .env("GIT_TERMINAL_PROMPT", "0")
        .arg("clone")
        .args(["--depth", "1"])
        .arg(url)
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

/// parse a map of 'font name': 'repository' pairs.
fn load_input_file(path: &Path) -> Result<BTreeMap<String, String>, Error> {
    let contents = std::fs::read_to_string(path).map_err(Error::InputFile)?;
    let result: BTreeMap<String, String> =
        serde_json::from_str(&contents).map_err(Error::InputJson)?;
    Ok(result)
}

/// Parse and return a config.yaml file for the provided font source
fn get_config(font_dir: &Path) -> Option<Config> {
    let config_path = get_config_path(font_dir)?;
    let contents = std::fs::read_to_string(config_path).unwrap();
    match serde_yaml::from_str(&contents) {
        Ok(config) => Some(config),
        Err(e) => {
            eprintln!("BAD YAML: {contents}: '{e}'");
            None
        }
    }
}

/// Look for a file like 'config.yaml' in a google fonts font checkout.
///
/// This will look for all files that begin with 'config' and have either the
/// 'yaml' or 'yml' extension; if multiple files match this pattern it will
/// return the one with the shortest name.
fn get_config_path(font_dir: &Path) -> Option<PathBuf> {
    #[allow(clippy::ptr_arg)] // we don't use &Path so we can pass this to a closure below
    fn looks_like_config_file(path: &PathBuf) -> bool {
        let (Some(stem), Some(extension)) =
            (path.file_stem().and_then(|s| s.to_str()), path.extension())
        else {
            return false;
        };
        stem.starts_with("config") && (extension == "yaml" || extension == "yml")
    }

    let sources_dir = font_dir.join("sources");
    let contents = std::fs::read_dir(sources_dir).ok()?;
    let mut config_files = contents
        .filter_map(|entry| entry.ok().map(|e| e.path()))
        .filter(looks_like_config_file)
        .collect::<Vec<_>>();

    // if multiple exist just... take the shortest one?
    config_files.sort_by_key(|p| p.to_str().map(|s| s.len()).unwrap_or(usize::MAX));
    config_files.into_iter().next()
}

/// Google fonts config file ('config.yaml')
#[derive(Clone, Debug, Deserialize)]
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
