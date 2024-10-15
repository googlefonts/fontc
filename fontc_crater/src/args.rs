//! CLI args

use std::path::PathBuf;

use clap::{Parser, Subcommand};

#[derive(Debug, PartialEq, Parser)]
#[command(about = "compile multiple fonts and report the results")]
pub(super) struct Args {
    #[command(subcommand)]
    pub(super) command: Commands,
}

#[derive(Debug, Subcommand, PartialEq)]
pub(super) enum Commands {
    Compile(RunArgs),
    Diff(RunArgs),
    Report(ReportArgs),
    Ci(CiArgs),
}

#[derive(Debug, PartialEq, clap::Args)]
pub(super) struct RunArgs {
    /// Directory to store font sources.
    ///
    /// Reusing this directory saves us having to clone all the repos on each run.
    ///
    /// This directory is also used to write cached results during repo discovery.
    pub(super) font_cache: PathBuf,
    /// Optional path to write out results (as json)
    #[arg(short = 'o', long = "out")]
    pub(super) out_path: Option<PathBuf>,
    /// for debugging, execute only a given number of fonts
    #[arg(long)]
    pub(super) n_fonts: Option<usize>,
}

#[derive(Debug, PartialEq, clap::Args)]
pub(super) struct CiArgs {
    /// Path to a json list of repos + revs to run.
    pub(super) to_run: PathBuf,
    /// Directory where results are written.
    ///
    /// This should be consistent between runs.
    #[arg(short = 'o', long = "out")]
    pub(super) out_dir: PathBuf,
    /// only generate html (for the provided out_dir)
    #[arg(long)]
    pub(super) html_only: bool,
}

#[derive(Debug, PartialEq, clap::Args)]
pub(super) struct ReportArgs {
    pub(super) json_path: PathBuf,
    #[arg(short, long)]
    pub(super) verbose: bool,
}
