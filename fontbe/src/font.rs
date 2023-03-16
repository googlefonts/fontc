//! Merge tables into a font

use fontdrasil::orchestration::Work;
use read_fonts::types::Tag;
use write_fonts::FontBuilder;

use crate::{
    error::Error,
    orchestration::{BeWork, Bytes, Context, WorkId},
};

struct FontWork {}

pub fn create_font_work() -> Box<BeWork> {
    Box::new(FontWork {})
}

// TODO the tables have the tag, but I'm not sure how to access it
const TABLES_TO_MERGE: &[(WorkId, Tag)] = &[
    (WorkId::Cmap, Tag::new(b"cmap")),
    (WorkId::Hmtx, Tag::new(b"hmtx")),
    (WorkId::Glyf, Tag::new(b"glyf")),
    (WorkId::Loca, Tag::new(b"loca")),
    (WorkId::Maxp, Tag::new(b"maxp")),
];

impl Work<Context, Error> for FontWork {
    /// Glue binary tables into a font
    fn exec(&self, context: &Context) -> Result<(), Error> {
        // Lets go right ahead and believe those bytes are a font
        let mut builder = FontBuilder::default();

        // A fancier implementation would mmap the files. We basic.
        for (work_id, tag) in TABLES_TO_MERGE {
            let bytes = context.read_raw(work_id.clone()).map_err(Error::IoError)?;
            builder.add_table(*tag, bytes);
        }

        context.set_font(Bytes::new(builder.build()));
        Ok(())
    }
}
