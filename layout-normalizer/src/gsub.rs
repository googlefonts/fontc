use std::io;

use read_fonts::FontRef;

use crate::{error::Error, glyph_names::NameMap};

pub(crate) fn print(
    _f: &mut dyn io::Write,
    _font: &FontRef,
    _names: &NameMap,
) -> Result<(), Error> {
    Ok(())
}
