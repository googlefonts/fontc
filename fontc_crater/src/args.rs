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
}

#[derive(Debug, PartialEq, clap::Args)]
pub(super) struct RunArgs {
    /// Directory to store font sources.
    ///
    /// Reusing this directory saves us having to clone all the repos on each run.
    ///
    /// This directory is also used to write cached results during repo discovery.
    pub(super) font_cache: PathBuf,
    /// Path to local checkout of google/fonts repository
    #[arg(short, long)]
    pub(super) fonts_repo: Option<PathBuf>,
    /// Optional path to write out results (as json)
    #[arg(short = 'o', long = "out")]
    pub(super) out_path: Option<PathBuf>,
    /// for debugging, execute only a given number of fonts
    #[arg(long)]
    pub(super) n_fonts: Option<usize>,
}

#[derive(Debug, PartialEq, clap::Args)]
pub(super) struct ReportArgs {
    pub(super) json_path: PathBuf,
    #[arg(short, long)]
    pub(super) verbose: bool,
}
