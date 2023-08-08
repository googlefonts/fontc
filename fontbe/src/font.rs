//! Merge tables into a font

use std::collections::HashSet;

use fontdrasil::orchestration::{Access, Work};
use fontir::orchestration::WorkId as FeWorkId;
use log::debug;
use read_fonts::{
    tables::{
        avar::Avar, cmap::Cmap, fvar::Fvar, glyf::Glyf, gpos::Gpos, gsub::Gsub, gvar::Gvar,
        head::Head, hhea::Hhea, hmtx::Hmtx, loca::Loca, maxp::Maxp, name::Name, os2::Os2,
        post::Post, stat::Stat,
    },
    types::Tag,
    TopLevelTable,
};
use write_fonts::FontBuilder;

use crate::{
    error::Error,
    orchestration::{to_bytes, AnyWorkId, BeWork, Context, WorkId},
};

#[derive(Debug)]
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
    (WorkId::Gpos, Gpos::TAG, TableType::Static),
    (WorkId::Gsub, Gsub::TAG, TableType::Static),
    (WorkId::Gvar, Gvar::TAG, TableType::Variable),
    (WorkId::Loca, Loca::TAG, TableType::Static),
    (WorkId::Maxp, Maxp::TAG, TableType::Static),
    (WorkId::Name, Name::TAG, TableType::Static),
    (WorkId::Os2, Os2::TAG, TableType::Static),
    (WorkId::Post, Post::TAG, TableType::Static),
    (WorkId::Stat, Stat::TAG, TableType::Variable),
];

fn has(context: &Context, id: WorkId) -> bool {
    match id {
        WorkId::Avar => context.avar.try_get().is_some(),
        WorkId::Cmap => context.cmap.try_get().is_some(),
        WorkId::Fvar => context.fvar.try_get().is_some(),
        WorkId::Head => context.head.try_get().is_some(),
        WorkId::Hhea => context.hhea.try_get().is_some(),
        WorkId::Hmtx => context.hmtx.try_get().is_some(),
        WorkId::Glyf => context.glyf.try_get().is_some(),
        WorkId::Gpos => context.gpos.try_get().is_some(),
        WorkId::Gsub => context.gsub.try_get().is_some(),
        WorkId::Gvar => context.gvar.try_get().is_some(),
        WorkId::Loca => context.loca.try_get().is_some(),
        WorkId::Maxp => context.maxp.try_get().is_some(),
        WorkId::Name => context.name.try_get().is_some(),
        WorkId::Os2 => context.os2.try_get().is_some(),
        WorkId::Post => context.post.try_get().is_some(),
        WorkId::Stat => context.stat.try_get().is_some(),
        _ => false,
    }
}

fn bytes_for(context: &Context, id: WorkId) -> Result<Vec<u8>, Error> {
    // TODO: to_vec copies :(
    let bytes = match id {
        WorkId::Avar => to_bytes(context.avar.get().as_ref()),
        WorkId::Cmap => to_bytes(context.cmap.get().as_ref()),
        WorkId::Fvar => to_bytes(context.fvar.get().as_ref()),
        WorkId::Head => to_bytes(context.head.get().as_ref()),
        WorkId::Hhea => to_bytes(context.hhea.get().as_ref()),
        WorkId::Hmtx => context.hmtx.get().as_ref().get().to_vec(),
        WorkId::Glyf => context.glyf.get().as_ref().get().to_vec(),
        WorkId::Gpos => to_bytes(context.gpos.get().as_ref()),
        WorkId::Gsub => to_bytes(context.gsub.get().as_ref()),
        WorkId::Gvar => context.gvar.get().as_ref().get().to_vec(),
        WorkId::Loca => context.loca.get().as_ref().get().to_vec(),
        WorkId::Maxp => to_bytes(context.maxp.get().as_ref()),
        WorkId::Name => to_bytes(context.name.get().as_ref()),
        WorkId::Os2 => to_bytes(context.os2.get().as_ref()),
        WorkId::Post => to_bytes(context.post.get().as_ref()),
        WorkId::Stat => to_bytes(context.stat.get().as_ref()),
        _ => panic!("Missing a match for {id:?}"),
    };
    Ok(bytes)
}

impl Work<Context, AnyWorkId, Error> for FontWork {
    fn id(&self) -> AnyWorkId {
        WorkId::Font.into()
    }

    fn read_access(&self) -> Access<AnyWorkId> {
        Access::Set(HashSet::from([
            WorkId::Avar.into(),
            WorkId::Cmap.into(),
            WorkId::Fvar.into(),
            WorkId::Head.into(),
            WorkId::Hhea.into(),
            WorkId::Hmtx.into(),
            WorkId::Glyf.into(),
            WorkId::Gpos.into(),
            WorkId::Gsub.into(),
            WorkId::Gvar.into(),
            WorkId::Loca.into(),
            WorkId::Maxp.into(),
            WorkId::Name.into(),
            WorkId::Os2.into(),
            WorkId::Post.into(),
            WorkId::Stat.into(),
            FeWorkId::StaticMetadata.into(),
            WorkId::LocaFormat.into(),
        ]))
    }

    /// Glue binary tables into a font
    fn exec(&self, context: &Context) -> Result<(), Error> {
        // Lets go right ahead and believe those bytes are a font
        let mut builder = FontBuilder::default();

        // A fancier implementation would mmap the files. We basic.
        let is_static = context.ir.static_metadata.get().variable_axes.is_empty();
        for (work_id, tag, table_type) in TABLES_TO_MERGE {
            if is_static && matches!(table_type, TableType::Variable) {
                debug!("Skip {tag} because this is a static font");
                continue;
            }
            if !has(context, work_id.clone()) {
                debug!("Skip {tag} because we don't have it");
                continue;
            }
            debug!("Grabbing {tag} for final font");
            let bytes = bytes_for(context, work_id.clone())?;
            builder.add_raw(*tag, bytes);
        }

        debug!("Building font");
        let font = builder.build();
        debug!("Assembled {} byte font", font.len());
        context.font.set_unconditionally(font.into());
        Ok(())
    }
}
