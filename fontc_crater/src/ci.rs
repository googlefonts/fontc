//! Running in CI.
//!
//! This handles the whole CI workflow.
//!
//! Unlike a normal run, this is expecting to have preexisting results, and to
//! generate a fuller report that includes comparison with past runs.

use std::{
    collections::BTreeMap,
    path::{Path, PathBuf},
};

use chrono::{DateTime, Utc};
use google_fonts_sources::{Config, RepoInfo};
use serde::de::DeserializeOwned;

use crate::{
    args::CiArgs,
    error::Error,
    ttx_diff_runner::{DiffError, DiffOutput},
    Results, Target,
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
    if let Some(last_run) = prev_runs.last() {
        if last_run.fontc_rev == fontc_rev
            && Some(last_run.input_file.as_os_str()) == args.to_run.file_name()
        {
            log::info!("fontc rev & inputs is unchanged from last run, skipping");
            return Ok(());
        }
    }

    let inputs: Vec<RepoInfo> = super::try_read_json(&args.to_run)?;

    let out_file = result_path_for_current_date();
    let out_path = args.out_dir.join(&out_file);
    // for now we are going to be cloning each repo freshly
    let cache_dir = args.cache_dir();
    log::info!("using cache dir {}", cache_dir.display());

    let (targets, source_repos) = make_targets(&cache_dir, &inputs);
    let began = Utc::now();
    let results = super::run_all(targets, &cache_dir, super::ttx_diff_runner::run_ttx_diff)?
        .into_iter()
        .map(|(target, result)| (target.id(), result))
        .collect();
    let finished = Utc::now();

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
                targets.push(Target {
                    _config: config_path.to_owned(),
                    source: src_path,
                })
            }
        }
    }
    (targets, repo_list)
}
