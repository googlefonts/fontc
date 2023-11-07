use read_fonts::{types::Tag, FontRef, TableProvider};

use crate::{common, error::Error, glyph_names::NameMap};

pub(crate) fn print(font: &FontRef, _names: &NameMap) -> Result<(), Error> {
    println!("# GPOS #");
    let table = font
        .gpos()
        .map_err(|_| Error::MissingTable(Tag::new(b"GPOS")))?;
    let script_list = table.script_list().unwrap();
    let feature_list = table.feature_list().unwrap();
    let lang_systems = common::get_lang_systems(&script_list, &feature_list);
    for sys in &lang_systems {
        println!("{}: {}/{}", sys.feature, sys.script, sys.lang);
    }

    Ok(())
}
