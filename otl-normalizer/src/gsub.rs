use std::io;

use write_fonts::read::tables::{gdef::Gdef, gsub::Gsub};

use crate::{error::Error, glyph_names::NameMap};

/// Print normalized GSUB layout rules for the provided font
pub fn print(
    _f: &mut dyn io::Write,
    _table: &Gsub,
    _gdef: Option<&Gdef>,
    _names: &NameMap,
) -> Result<(), Error> {
    //TODO: do we *ever* want to support this? GPOS seems like the big one?
    Ok(())
}
