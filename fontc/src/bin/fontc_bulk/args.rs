//! CLI args

use std::path::PathBuf;

use clap::{Parser, ValueEnum};

#[derive(Debug, Clone, PartialEq, Parser)]
#[command(about = "compile multiple fonts and report the results")]
pub(super) struct Args {
    /// The task to perform with each font
    pub(super) command: Tasks,
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
}

#[derive(Clone, Debug, PartialEq, Eq, ValueEnum)]
pub(super) enum Tasks {
    Compile,
    // this will expand to include at least 'ttx_diff'
}
