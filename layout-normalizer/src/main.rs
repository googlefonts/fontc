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

use std::{
    fs::File,
    io::{BufWriter, Write},
};

use clap::Parser;
use error::Error;
use write_fonts::read::{FileRef, FontRef, ReadError};

fn main() -> Result<(), Error> {
    let args = args::Args::parse();
    let data = std::fs::read(&args.font_path).map_err(|inner| Error::Load {
        path: args.font_path.clone(),
        inner,
    })?;

    let mut write_target: Box<dyn Write> = match args.out.as_ref() {
        Some(path) => File::create(path)
            .map_err(|inner| Error::FileWrite {
                path: path.to_owned(),
                inner,
            })
            .map(|f| Box::new(BufWriter::new(f)))?,
        None => Box::new(std::io::stdout()),
    };

    let font = get_font(&data, args.index)?;
    let name_map = glyph_names::make_name_map(&font)?;
    let to_print = args.table.unwrap_or_default();
    if matches!(to_print, args::Table::All | args::Table::Gpos) {
        gpos::print(&mut write_target, &font, &name_map)?;
    }

    if matches!(to_print, args::Table::All | args::Table::Gsub) {
        gsub::print(&mut write_target, &font, &name_map)?;
    }
    write_target.flush().unwrap();

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
