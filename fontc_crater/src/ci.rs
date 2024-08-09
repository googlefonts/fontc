//! Running in CI.
//!
//! This handles the whole CI workflow.
//!
//! Unlike a normal run, this is expecting to have preexisting results, and to
//! generate a fuller report that includes comparison with past runs.

use std::path::PathBuf;

use chrono::{DateTime, Utc};
use google_fonts_sources::RepoInfo;

use crate::{args::CiArgs, error::Error};

static SUMMARY_FILE: &str = "summary.json";

/// A summary of a single CI run

#[derive(Clone, Debug, serde::Serialize, serde::Deserialize)]
struct RunSummary {
    began: DateTime<Utc>,
    finished: DateTime<Utc>,
    fontc_rev: String,
    report_file: PathBuf,
    // the name of the file listing targets used by this run.
    // it is intended that when this list is updated, the filename is changed.
    input_file: PathBuf,
    stats: super::ttx_diff_runner::Summary,
}

pub(super) fn run_ci(args: &CiArgs) -> Result<(), Error> {
    super::ttx_diff_runner::assert_can_run_script();
    run_crater_and_save_results(args)?;
    // now we want to generate an html report, based on this info.
    Ok(())
}

fn run_crater_and_save_results(args: &CiArgs) -> Result<(), Error> {
    if !args.out_dir.exists() {
        super::try_create_dir(&args.out_dir)?;
    }

    let summary_file = args.out_dir.join(SUMMARY_FILE);
    let mut prev_runs: Vec<RunSummary> = if summary_file.exists() {
        super::try_read_json(&summary_file)?
    } else {
        Default::default()
    };

    let fontc_rev = get_git_rev();
    if prev_runs
        .last()
        .map(|prev| prev.fontc_rev == fontc_rev)
        .unwrap_or(false)
    {
        // eventually we will want an argument to force rerunning, which we'll
        // use when we update fontmake or the input data set
        eprintln!("fontc rev is unchange from last run, skipping");
        std::process::exit(0);
    }

    let inputs: Vec<RepoInfo> = super::try_read_json(&args.to_run)?;
    let out_file = result_path_for_current_date();
    let out_path = args.out_dir.join(&out_file);
    // for now we are going to be cloning each repo freshly
    let cache_dir = tempfile::tempdir().unwrap();

    let began = Utc::now();
    let results = super::run_all(
        &inputs,
        cache_dir.path(),
        super::ttx_diff_runner::run_ttx_diff,
    )?;
    let finished = Utc::now();

    super::try_write_json(&results, &out_path)?;
    let summary = super::ttx_diff_runner::Summary::new(&results);
    let summary = RunSummary {
        began,
        finished,
        fontc_rev,
        report_file: out_file.into(),
        input_file: args.to_run.clone(),
        stats: summary,
    };

    prev_runs.push(summary);
    super::try_write_json(&prev_runs, &summary_file)
}

fn result_path_for_current_date() -> String {
    let now = chrono::Utc::now();
    let timestamp = now.format("%Y-%m-%d-%H%M%S");
    format!("{timestamp}.json")
}

fn get_git_rev() -> String {
    let output = std::process::Command::new("git")
        .arg("rev-parse")
        .arg("--short")
        .arg("HEAD")
        .output()
        .expect("git rev-parse HEAD should not fail if repo exists");
    std::str::from_utf8(&output.stdout)
        .expect("rev is always ascii/hex string")
        .trim()
        .to_owned()
}
