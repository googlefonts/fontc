use read_fonts::FontRef;

use crate::error::Error;

pub(crate) fn print(_font: &FontRef) -> Result<(), Error> {
    println!("gsub goes here");
    Ok(())
}
