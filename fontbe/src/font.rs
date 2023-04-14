//! Merge tables into a font

use fontdrasil::orchestration::Work;
use log::debug;
use read_fonts::{
    tables::{
        avar::Avar, cmap::Cmap, fvar::Fvar, glyf::Glyf, gvar::Gvar, head::Head, hhea::Hhea,
        hmtx::Hmtx, loca::Loca, maxp::Maxp, name::Name, os2::Os2, post::Post,
    },
    types::Tag,
    TopLevelTable,
};
use write_fonts::FontBuilder;

use crate::{
    error::Error,
    orchestration::{BeWork, Bytes, Context, WorkId},
};

struct FontWork {}

pub fn create_font_work() -> Box<BeWork> {
    Box::new(FontWork {})
}

enum TableType {
    Static,
    Variable,
}

const TABLES_TO_MERGE: &[(WorkId, Tag, TableType, usize)] = &[
    (WorkId::Avar, Avar::TAG, TableType::Variable, 0),
    (WorkId::Cmap, Cmap::TAG, TableType::Static, 0),
    (WorkId::Fvar, Fvar::TAG, TableType::Variable, 0),
    (WorkId::Head, Head::TAG, TableType::Static, 0),
    (WorkId::Hhea, Hhea::TAG, TableType::Static, 0),
    (WorkId::Hmtx, Hmtx::TAG, TableType::Static, 0),
    (WorkId::Glyf, Glyf::TAG, TableType::Static, 0),
    (WorkId::Gvar, Gvar::TAG, TableType::Variable, 0),
    (WorkId::Loca, Loca::TAG, TableType::Static, 1),
    (WorkId::Maxp, Maxp::TAG, TableType::Static, 0),
    (WorkId::Name, Name::TAG, TableType::Static, 0),
    (WorkId::Os2, Os2::TAG, TableType::Static, 0),
    (WorkId::Post, Post::TAG, TableType::Static, 0),
];

impl Work<Context, Error> for FontWork {
    /// Glue binary tables into a font
    fn exec(&self, context: &Context) -> Result<(), Error> {
        // Lets go right ahead and believe those bytes are a font
        let mut builder = FontBuilder::default();

        // A fancier implementation would mmap the files. We basic.
        let is_static = context
            .ir
            .get_init_static_metadata()
            .variable_axes
            .is_empty();
        for (work_id, tag, table_type, skip_bytes) in TABLES_TO_MERGE {
            if is_static && matches!(table_type, TableType::Variable) {
                debug!("Skip {tag} because this is a static font");
                continue;
            }
            debug!("Grabbing {tag} for final font");
            let mut bytes = context.read_raw(work_id.clone()).map_err(Error::IoError)?;
            bytes.drain(0..*skip_bytes);
            builder.add_table(*tag, bytes);
        }

        context.set_font(Bytes::new(builder.build()));
        Ok(())
    }
}
