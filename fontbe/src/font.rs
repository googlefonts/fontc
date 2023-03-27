//! Merge tables into a font

use fontdrasil::orchestration::Work;
use log::trace;
use read_fonts::{
    tables::{
        cmap::Cmap, fvar::Fvar, glyf::Glyf, head::Head, hhea::Hhea, hmtx::Hmtx, loca::Loca,
        maxp::Maxp, name::Name, os2::Os2, post::Post,
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

const TABLES_TO_MERGE: &[(WorkId, Tag, TableType)] = &[
    (WorkId::Cmap, Cmap::TAG, TableType::Static),
    (WorkId::Fvar, Fvar::TAG, TableType::Variable),
    (WorkId::Head, Head::TAG, TableType::Static),
    (WorkId::Hhea, Hhea::TAG, TableType::Static),
    (WorkId::Hmtx, Hmtx::TAG, TableType::Static),
    (WorkId::Glyf, Glyf::TAG, TableType::Static),
    (WorkId::Loca, Loca::TAG, TableType::Static),
    (WorkId::Maxp, Maxp::TAG, TableType::Static),
    (WorkId::Name, Name::TAG, TableType::Static),
    (WorkId::Os2, Os2::TAG, TableType::Static),
    (WorkId::Post, Post::TAG, TableType::Static),
];

impl Work<Context, Error> for FontWork {
    /// Glue binary tables into a font
    fn exec(&self, context: &Context) -> Result<(), Error> {
        // Lets go right ahead and believe those bytes are a font
        let mut builder = FontBuilder::default();

        // A fancier implementation would mmap the files. We basic.
        let is_static = context.ir.get_init_static_metadata().axes.is_empty();
        for (work_id, tag, table_type) in TABLES_TO_MERGE {
            if is_static && matches!(table_type, TableType::Variable) {
                trace!("Skip {tag} because this is a static font");
                continue;
            }
            trace!("Grabbing {tag} for final font");
            let bytes = context.read_raw(work_id.clone()).map_err(Error::IoError)?;
            builder.add_table(*tag, bytes);
        }

        context.set_font(Bytes::new(builder.build()));
        Ok(())
    }
}
