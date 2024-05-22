//! CLI app for printing normalized layout tables

use std::{
    fs::File,
    io::{BufWriter, Write},
};

use clap::Parser;
use otl_normalizer::{args, Error, NameMap};
use write_fonts::read::{FileRef, FontRef, ReadError, TableProvider};

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
    let name_map = NameMap::from_font(&font)?;
    let to_print = args.table.unwrap_or_default();
    let gdef = font.gdef().ok();
    if matches!(to_print, args::Table::All | args::Table::Gpos) {
        if let Ok(gpos) = font.gpos() {
            writeln!(&mut write_target, "# GPOS #")?;
            otl_normalizer::print_gpos(&mut write_target, &gpos, gdef.as_ref(), &name_map)?;
        }
    }

    if matches!(to_print, args::Table::All | args::Table::Gsub) {
        if let Ok(gsub) = font.gsub() {
            writeln!(&mut write_target, "# GSUB #")?;
            otl_normalizer::print_gsub(&mut write_target, &gsub, gdef.as_ref(), &name_map)?;
        }
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
