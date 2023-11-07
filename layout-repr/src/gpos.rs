use read_fonts::{types::Tag, FontRef, TableProvider};

use crate::error::Error;

pub(crate) fn print(font: &FontRef) -> Result<(), Error> {
    println!("gpos goes here");
    let _table = font
        .gpos()
        .map_err(|_| Error::MissingTable(Tag::new(b"GPOS")))?;
    Ok(())
}
