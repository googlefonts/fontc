//! Generating a normalized text representation for layout tables
//!
//! This currently supports a subset of GPOS (kerning and marks)

mod args;
mod common;
mod error;
mod glyph_names;
mod gpos;
mod gsub;
mod variations;

use clap::Parser;
use error::Error;
use read_fonts::{FileRef, FontRef, ReadError};

fn main() -> Result<(), Error> {
    let args = args::Args::parse();
    let data = std::fs::read(&args.font_path).map_err(|inner| Error::Load {
        path: args.font_path.clone(),
        inner,
    })?;

    let font = get_font(&data, args.index)?;
    let name_map = glyph_names::make_name_map(&font)?;
    let to_print = args.table.unwrap_or_default();
    if matches!(to_print, args::Table::All | args::Table::Gpos) {
        gpos::print(&font, &name_map)?;
    }

    if matches!(to_print, args::Table::All | args::Table::Gsub) {
        gsub::print(&font)?;
    }

    Ok(())
}

fn get_font(bytes: &[u8], idx: Option<u32>) -> Result<FontRef, Error> {
    let font = FileRef::new(bytes).map_err(Error::FontRead)?;
    match (font, idx.unwrap_or(0)) {
        (FileRef::Font(font), 0) => Ok(font),
        (FileRef::Font(_), other) => Err(Error::FontRead(ReadError::InvalidCollectionIndex(other))),
        (FileRef::Collection(collection), idx) => collection.get(idx).map_err(Error::FontRead),
    }
}
