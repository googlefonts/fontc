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

    let font = get_font(&data, args.index)?;
    // exit early if there's no work, so we don't bother creating an empty file
    if !is_there_something_to_do(&font, &args) {
        return Ok(());
    }

    let mut write_target: Box<dyn Write> = match args.out.as_ref() {
        Some(path) => File::create(path)
            .map_err(|inner| Error::FileWrite {
                path: path.to_owned(),
                inner,
            })
            .map(|f| Box::new(BufWriter::new(f)))?,
        None => Box::new(std::io::stdout()),
    };

    let name_map = NameMap::from_font(&font)?;
    let to_print = args.table;
    let gdef = font.gdef().ok();

    if matches!(to_print, args::Table::All | args::Table::Gdef) {
        if let Some(gdef) = gdef.as_ref().filter(|gdef| gdef.lig_caret_list().is_some()) {
            writeln!(&mut write_target, "# GDEF #")?;
            otl_normalizer::print_gdef(&mut write_target, gdef, &name_map)?;
        }
    }

    if matches!(to_print, args::Table::All | args::Table::Gpos) {
        if let Ok(gpos) = font.gpos() {
            writeln!(&mut write_target, "# GPOS #")?;
            otl_normalizer::print_gpos(&mut write_target, &gpos, gdef.as_ref(), &name_map)?;
        }
    }

    if matches!(to_print, args::Table::All | args::Table::Gsub) {
        if let Ok(_gsub) = font.gsub() {
            // we don't currently handle GSUB, and it's not clear we want to?
            //writeln!(&mut write_target, "# GSUB #")?;
            //otl_normalizer::print_gsub(&mut write_target, &gsub, gdef.as_ref(), &name_map)?;
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

fn is_there_something_to_do(font: &FontRef, args: &args::Args) -> bool {
    match args.table {
        // gdef is meaningless without one of these two
        args::Table::All => font.gpos().is_ok() || font.gsub().is_ok(),
        args::Table::Gpos => font.gpos().is_ok(),
        args::Table::Gsub => font.gsub().is_ok(),
        args::Table::Gdef => font
            .gdef()
            .map(|gdef| gdef.lig_caret_list().is_some())
            .unwrap_or(false),
    }
}
