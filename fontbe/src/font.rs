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
    orchestration::{to_bytes, BeWork, Bytes, Context, WorkId},
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
    (WorkId::Avar, Avar::TAG, TableType::Variable),
    (WorkId::Cmap, Cmap::TAG, TableType::Static),
    (WorkId::Fvar, Fvar::TAG, TableType::Variable),
    (WorkId::Head, Head::TAG, TableType::Static),
    (WorkId::Hhea, Hhea::TAG, TableType::Static),
    (WorkId::Hmtx, Hmtx::TAG, TableType::Static),
    (WorkId::Glyf, Glyf::TAG, TableType::Static),
    (WorkId::Gvar, Gvar::TAG, TableType::Variable),
    (WorkId::Loca, Loca::TAG, TableType::Static),
    (WorkId::Maxp, Maxp::TAG, TableType::Static),
    (WorkId::Name, Name::TAG, TableType::Static),
    (WorkId::Os2, Os2::TAG, TableType::Static),
    (WorkId::Post, Post::TAG, TableType::Static),
];

fn bytes_for(context: &Context, id: WorkId) -> Result<Vec<u8>, Error> {
    let bytes = match id {
        WorkId::Avar => to_bytes(context.get_avar().as_ref()),
        WorkId::Cmap => to_bytes(&*context.get_cmap()),
        WorkId::Fvar => to_bytes(&*context.get_fvar()),
        WorkId::Head => to_bytes(&*context.get_head()),
        WorkId::Hhea => to_bytes(&*context.get_hhea()),
        WorkId::Hmtx => context.get_hmtx().get().to_vec(),
        WorkId::Glyf => context.get_glyf_loca().glyf.clone(),
        WorkId::Gvar => context.get_gvar().get().to_vec(),
        WorkId::Loca => context.get_glyf_loca().raw_loca.clone(),
        WorkId::Maxp => to_bytes(&*context.get_maxp()),
        WorkId::Name => to_bytes(&*context.get_name()),
        WorkId::Os2 => to_bytes(&*context.get_os2()),
        WorkId::Post => to_bytes(&*context.get_post()),
        _ => panic!("Missing a match for {id:?}"),
    };
    Ok(bytes)
}

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
        for (work_id, tag, table_type) in TABLES_TO_MERGE {
            if is_static && matches!(table_type, TableType::Variable) {
                debug!("Skip {tag} because this is a static font");
                continue;
            }
            debug!("Grabbing {tag} for final font");
            let bytes = bytes_for(context, work_id.clone())?;
            builder.add_table(*tag, bytes);
        }

        let font = builder.build();
        debug!("Assembled {} byte font", font.len());
        context.set_font(Bytes::new(font));
        Ok(())
    }
}
