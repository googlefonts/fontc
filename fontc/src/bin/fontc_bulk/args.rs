//! CLI args

use std::path::PathBuf;

use clap::{Parser, ValueEnum};

#[derive(Debug, Clone, PartialEq, Parser)]
#[command(about = "compile multiple fonts and report the results")]
pub(super) struct Args {
    /// The task to perform with each font
    pub(super) command: Tasks,
    /// Path to json file containing a list of fonts to compile.
    ///
    /// This file should be a dictionary in the "name": "repo".
    #[arg(long = "fonts", default_value = "resources/testdata/gf_fonts.json")]
    pub(super) input_list: PathBuf,
    /// Directory to store font sources
    #[arg(long = "cache", default_value = "build/font_cache")]
    pub(super) font_cache: PathBuf,
    /// Optional path to write out results (as json)
    #[arg(short = 'o', long = "out")]
    pub(super) out_path: Option<PathBuf>,
}

#[derive(Clone, Debug, PartialEq, Eq, ValueEnum)]
pub(super) enum Tasks {
    Compile,
    // this will expand to include at least 'ttx_diff'
}
