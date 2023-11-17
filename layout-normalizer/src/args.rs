use std::{path::PathBuf, str::FromStr};

#[derive(Clone, Debug, clap::Parser)]
pub(crate) struct Args {
    pub font_path: PathBuf,
    #[arg(short, long)]
    /// Optional destination path for writing output. Default is stdout.
    pub out: Option<PathBuf>,
    /// Target table to print, one of gpos/gsub/all (case insensitive)
    #[arg(short, long)]
    pub table: Option<Table>,
    /// Index of font to examine, if target is a font collection
    #[arg(short, long)]
    pub index: Option<u32>,
}

/// What table to print
#[derive(Clone, Debug, Default)]
pub enum Table {
    #[default]
    All,
    Gpos,
    Gsub,
}

impl FromStr for Table {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        static ERR_MSG: &str = "expected one of 'gsub', 'gpos', 'all'";
        match s.to_ascii_lowercase().trim() {
            "gpos" => Ok(Self::Gpos),
            "gsub" => Ok(Self::Gsub),
            "all" => Ok(Self::All),
            _ => Err(ERR_MSG),
        }
    }
}
