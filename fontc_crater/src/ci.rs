//! Running in CI.
//!
//! This handles the whole CI workflow.
//!
//! Unlike a normal run, this is expecting to have preexisting results, and to
//! generate a fuller report that includes comparison with past runs.

use std::{
    collections::BTreeMap,
    fmt::Write,
    path::{Path, PathBuf},
    process::Command,
};

use chrono::{DateTime, TimeZone, Utc};
use google_fonts_sources::{Config, RepoInfo};
use serde::de::DeserializeOwned;

use crate::{
    args::CiArgs,
    error::Error,
    ttx_diff_runner::{DiffError, DiffOutput},
    BuildType, Results, Target,
};

mod html;

static SUMMARY_FILE: &str = "summary.json";
static SOURCES_FILE: &str = "sources.json";

type DiffResults = Results<DiffOutput, DiffError>;

/// A summary of a single CI run

#[derive(Clone, Debug, serde::Serialize, serde::Deserialize)]
struct RunSummary {
    began: DateTime<Utc>,
    finished: DateTime<Utc>,
    fontc_rev: String,
    #[serde(default)]
    pip_freeze_sha: String,
    results_file: PathBuf,
    // the name of the file listing targets used by this run.
    // it is intended that when this list is updated, the filename is changed.
    input_file: PathBuf,
    stats: super::ttx_diff_runner::Summary,
}

impl RunSummary {
    fn try_load_results(&self, target_dir: &Path) -> Option<Result<DiffResults, Error>> {
        let report_path = target_dir.join(&self.results_file);
        if !report_path.exists() {
            return None;
        }
        Some(super::try_read_json(&report_path))
    }
}

pub(super) fn run_ci(args: &CiArgs) -> Result<(), Error> {
    if !args.html_only {
        super::ttx_diff_runner::assert_can_run_script();
        run_crater_and_save_results(args)?;
    }
    html::generate(&args.out_dir)?;
    // now we want to generate an html report, based on this info.
    Ok(())
}

fn load_json_if_exists_else_default<T: DeserializeOwned + Default>(
    path: &Path,
) -> Result<T, Error> {
    if path.exists() {
        super::try_read_json(path)
    } else {
        Ok(Default::default())
    }
}

fn run_crater_and_save_results(args: &CiArgs) -> Result<(), Error> {
    if !args.out_dir.exists() {
        super::try_create_dir(&args.out_dir)?;
    }

    let summary_file = args.out_dir.join(SUMMARY_FILE);
    let mut prev_runs: Vec<RunSummary> = load_json_if_exists_else_default(&summary_file)?;
    // todo: fontc_repo should be checked out by us, and have a known path
    let fontc_rev = super::get_git_rev(None).unwrap();
    let pip_freeze_sha = dbg!(super::pip_freeze_sha());
    if let Some(last_run) = prev_runs.last() {
        if last_run.fontc_rev == fontc_rev
            && Some(last_run.input_file.as_os_str()) == args.to_run.file_name()
            && pip_freeze_sha == last_run.pip_freeze_sha
        {
            log::info!("no changes since last run, skipping");
            return Ok(());
        }
    }

    let inputs: Vec<RepoInfo> = super::try_read_json(&args.to_run)?;

    let out_file = result_path_for_current_date();
    let out_path = args.out_dir.join(&out_file);
    // for now we are going to be cloning each repo freshly
    let cache_dir = args.cache_dir();
    log::info!("using cache dir {}", cache_dir.display());

    // we want to build fontc & normalizer once, and then move them out of the
    // build directory so that they aren't accidentally rebuilt or deleted
    // while we're running
    let temp_bin_dir = tempfile::tempdir().expect("couldn't create tempdir");
    let (fontc_path, normalizer_path) = precompile_rust_binaries(temp_bin_dir.path());

    log::info!("compiled fontc to {}", fontc_path.display());
    log::info!("compiled otl-normalizeer to {}", normalizer_path.display());

    let (targets, source_repos) = make_targets(&cache_dir, &inputs);
    let n_targets = targets.len();

    let context = super::ttx_diff_runner::TtxContext {
        fontc_path,
        normalizer_path,
        cache_dir,
    };

    let began = Utc::now();
    let results = super::run_all(targets, &context, super::ttx_diff_runner::run_ttx_diff)?
        .into_iter()
        .map(|(target, result)| (target.id(), result))
        .collect();
    let finished = Utc::now();

    let elapsed = format_elapsed_time(&began, &finished);
    log::info!("completed {n_targets} targets in {elapsed}");

    let summary = super::ttx_diff_runner::Summary::new(&results);
    if Some(&summary) == prev_runs.last().map(|run| &run.stats) {
        log::info!("output identical to last run, skipping");
        return Ok(());
    }
    let input_file = args
        .to_run
        .file_name()
        .map(PathBuf::from)
        .unwrap_or_else(|| args.to_run.clone());

    let summary = RunSummary {
        began,
        finished,
        fontc_rev,
        pip_freeze_sha,
        results_file: out_file.into(),
        input_file,
        stats: summary,
    };

    prev_runs.push(summary);

    super::try_write_json(&results, &out_path)?;
    super::try_write_json(&prev_runs, &summary_file)?;

    // we write the map of target -> source repo to a separate file because
    // otherwise we're basically duplicating it for each run.
    let sources_file = args.out_dir.join(SOURCES_FILE);
    super::try_write_json(&source_repos, &sources_file)
}

fn result_path_for_current_date() -> String {
    let now = chrono::Utc::now();
    let timestamp = now.format("%Y-%m-%d-%H%M%S");
    format!("{timestamp}.json")
}

fn make_targets(cache_dir: &Path, repos: &[RepoInfo]) -> (Vec<Target>, BTreeMap<PathBuf, String>) {
    let mut targets = Vec::new();
    let mut repo_list = BTreeMap::new();
    for repo in repos {
        let Ok(iter) = repo.iter_configs(cache_dir) else {
            log::warn!(
                "error reading repo '{}'",
                repo.repo_path(cache_dir).display()
            );
            continue;
        };
        for config_path in iter {
            let config = match Config::load(&config_path) {
                Ok(x) => x,
                Err(e) => {
                    log::warn!("failed to load config '{}': '{e}'", config_path.display());
                    continue;
                }
            };
            for source in &config.sources {
                let src_path = config_path
                    .parent()
                    .expect("config path always in sources dir")
                    .join(source);
                if !src_path.exists() {
                    log::warn!("source file '{}' is missing", src_path.display());
                    continue;
                }
                let src_path = src_path
                    .strip_prefix(cache_dir)
                    .expect("source is always in cache dir")
                    .to_path_buf();
                repo_list.insert(src_path.clone(), repo.repo_url.clone());
                targets.extend(targets_for_source(&src_path, &config_path, &config))
            }
        }
    }
    (targets, repo_list)
}

fn targets_for_source(
    src_path: &Path,
    config_path: &Path,
    config: &Config,
) -> impl Iterator<Item = Target> {
    let default = Some(Target {
        config: config_path.to_owned(),
        source: src_path.to_owned(),
        build: BuildType::Default,
    });

    let gftools = should_build_in_gftools_mode(src_path, config).then(|| Target {
        config: config_path.to_owned(),
        source: src_path.to_owned(),
        build: BuildType::GfTools,
    });
    [default, gftools].into_iter().flatten()
}

fn should_build_in_gftools_mode(src_path: &Path, config: &Config) -> bool {
    // skip noto, which have an implicitly different recipe provider
    //https://github.com/googlefonts/oxidize/blob/main/text/2024-06-26-fixes-and-nonstandard-builds.md#noto
    if src_path
        .file_stem()
        .and_then(|stem| stem.to_str().map(|s| s.to_lowercase().starts_with("noto")))
        .unwrap_or(false)
    {
        return false;
    }

    // if there is a recipe provider other than googlefonts, we skip, because
    // it could be doing anything; see above
    config
        .recipe_provider
        .as_ref()
        .filter(|provider| *provider != "googlefonts")
        .is_none()
}

fn format_elapsed_time<Tmz: TimeZone>(start: &DateTime<Tmz>, end: &DateTime<Tmz>) -> String {
    let delta = end.clone().signed_duration_since(start);
    let mut out = String::new();
    let hours = delta.num_hours();
    let mins = delta.num_minutes() - hours * 60;
    let secs = delta.num_seconds() - (hours * 60 + mins) * 60;
    assert!(!hours.is_negative() | mins.is_negative() | secs.is_negative());
    if delta.num_hours() > 0 {
        write!(&mut out, "{hours}h").unwrap();
    }
    write!(&mut out, "{mins}m").unwrap();
    write!(&mut out, "{secs}s").unwrap();
    out
}

fn precompile_rust_binaries(temp_dir: &Path) -> (PathBuf, PathBuf) {
    let fontc = compile_crate_or_die("fontc");
    let normalizer = compile_crate_or_die("otl-normalizer");

    (
        copy_file_into_dir(&fontc, temp_dir),
        copy_file_into_dir(&normalizer, temp_dir),
    )
}

fn copy_file_into_dir(file_path: &Path, dir_path: &Path) -> PathBuf {
    let new_file_path = dir_path.join(file_path.file_name().unwrap());
    std::fs::copy(file_path, &new_file_path).expect("failed to copy binary to tempdir");
    new_file_path
}

// if we can't compile fontc / otl-normalizer there's nothing we can do?
fn compile_crate_or_die(name: &str) -> PathBuf {
    let status = Command::new("cargo")
        .args(["build", "-p", name, "--release"])
        .status()
        .expect("failed to run cargo build");
    if !status.success() {
        panic!("cargo build '{name}' failed");
    }

    expect_binary_target(name)
}

fn expect_binary_target(name: &str) -> PathBuf {
    let cwd = std::env::current_dir().expect("cwd exists and is readable");
    let target_dir = cwd.join("target/release");
    let target = target_dir.join(name).canonicalize().unwrap();
    assert!(
        target.is_file(),
        "missing target for '{name}' ({} is not a file)",
        target.display()
    );
    target
}
