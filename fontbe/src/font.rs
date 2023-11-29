//! Merge tables into a font

use std::{collections::HashSet, time::Instant};

use fontdrasil::orchestration::{Access, Work};
use fontir::orchestration::WorkId as FeWorkId;
use log::debug;
use write_fonts::{
    read::TopLevelTable,
    tables::{
        avar::Avar, cmap::Cmap, fvar::Fvar, gdef::Gdef, glyf::Glyf, gpos::Gpos, gsub::Gsub,
        gvar::Gvar, head::Head, hhea::Hhea, hmtx::Hmtx, hvar::Hvar, loca::Loca, maxp::Maxp,
        name::Name, os2::Os2, post::Post, stat::Stat,
    },
    types::Tag,
    FontBuilder,
};

use crate::{
    error::Error,
    orchestration::{to_bytes, AnyWorkId, BeWork, Context, WorkId, BinaryTables},
};

#[derive(Debug)]
struct PreliminaryFontWork {}

#[derive(Debug)]
struct FinalFontWork {}

pub fn create_font_work() -> Vec<Box<BeWork>> {
    vec![Box::new(PreliminaryFontWork {}), Box::new(FinalFontWork {})]
}

enum TableType {
    Static,
    Variable,
}

const NON_LAYOUT_TABLES: &[(WorkId, Tag, TableType)] = &[
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
    (WorkId::Stat, Stat::TAG, TableType::Variable),
    (WorkId::Hvar, Hvar::TAG, TableType::Variable),
];

const LAYOUT_TABLES: &[(WorkId, Tag, TableType)] = &[
    (WorkId::Gpos, Gpos::TAG, TableType::Static),
    (WorkId::Gsub, Gsub::TAG, TableType::Static),
    (WorkId::Gdef, Gdef::TAG, TableType::Static),
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
        WorkId::Gdef => context.gdef.try_get().is_some(),
        WorkId::Gvar => context.gvar.try_get().is_some(),
        WorkId::Loca => context.loca.try_get().is_some(),
        WorkId::Maxp => context.maxp.try_get().is_some(),
        WorkId::Name => context.name.try_get().is_some(),
        WorkId::Os2 => context.os2.try_get().is_some(),
        WorkId::Post => context.post.try_get().is_some(),
        WorkId::Stat => context.stat.try_get().is_some(),
        WorkId::Hvar => context.hvar.try_get().is_some(),
        _ => false,
    }
}

fn bytes_for(context: &Context, id: WorkId) -> Result<Option<Vec<u8>>, Error> {
    // TODO: to_vec copies :(
    let time = Instant::now();
    let bytes = match id {
        WorkId::Avar => to_bytes(context.avar.get().as_ref()),
        WorkId::Cmap => to_bytes(context.cmap.get().as_ref()),
        WorkId::Fvar => to_bytes(context.fvar.get().as_ref()),
        WorkId::Head => to_bytes(context.head.get().as_ref()),
        WorkId::Hhea => to_bytes(context.hhea.get().as_ref()),
        WorkId::Hmtx => Some(context.hmtx.get().as_ref().get().to_vec()),
        WorkId::Glyf => Some(context.glyf.get().as_ref().get().to_vec()),
        WorkId::Gpos => to_bytes(context.gpos.get().as_ref()),
        WorkId::Gsub => to_bytes(context.gsub.get().as_ref()),
        WorkId::Gdef => to_bytes(context.gdef.get().as_ref()),
        WorkId::Gvar => Some(context.gvar.get().as_ref().get().to_vec()),
        WorkId::Loca => Some(context.loca.get().as_ref().get().to_vec()),
        WorkId::Maxp => to_bytes(context.maxp.get().as_ref()),
        WorkId::Name => to_bytes(context.name.get().as_ref()),
        WorkId::Os2 => to_bytes(context.os2.get().as_ref()),
        WorkId::Post => to_bytes(context.post.get().as_ref()),
        WorkId::Stat => to_bytes(context.stat.get().as_ref()),
        WorkId::Hvar => to_bytes(context.hvar.get().as_ref()),
        _ => panic!("Missing a match for {id:?}"),
    };
    let time = Instant::now() - time;
    eprintln!("bytes_for {id:?} took {:.1}ms", time.as_secs_f64() * 1000.0);
    Ok(bytes)
}

impl Work<Context, AnyWorkId, Error> for PreliminaryFontWork {
    fn id(&self) -> AnyWorkId {
        WorkId::PreliminaryFont.into()
    }

    fn read_access(&self) -> Access<AnyWorkId> {
        // Everything *except* layout
        Access::Set(HashSet::from([
            AnyWorkId::Be(WorkId::Avar).into(),
            AnyWorkId::Be(WorkId::Cmap).into(),
            AnyWorkId::Be(WorkId::Fvar).into(),
            AnyWorkId::Be(WorkId::Head).into(),
            AnyWorkId::Be(WorkId::Hhea).into(),
            AnyWorkId::Be(WorkId::Hmtx).into(),
            AnyWorkId::Be(WorkId::Glyf).into(),
            AnyWorkId::Be(WorkId::Gvar).into(),
            AnyWorkId::Be(WorkId::Loca).into(),
            AnyWorkId::Be(WorkId::Maxp).into(),
            AnyWorkId::Be(WorkId::Name).into(),
            AnyWorkId::Be(WorkId::Os2).into(),
            AnyWorkId::Be(WorkId::Post).into(),
            AnyWorkId::Be(WorkId::Stat).into(),
            AnyWorkId::Be(WorkId::Hvar).into(),
            AnyWorkId::Be(WorkId::LocaFormat).into(),
            AnyWorkId::Fe(FeWorkId::StaticMetadata).into(),
        ]))
    }

    /// Glue binary tables into a font
    fn exec(&self, context: &Context) -> Result<(), Error> {
        debug!("Assembling preliminary font");
        // Lets go right ahead and believe those bytes are a font
        let mut tables = BinaryTables::default();

        // A fancier implementation would mmap the files. We basic.
        let is_static = context.ir.static_metadata.get().axes.is_empty();
        let mut len = 0;
        for (work_id, tag, table_type) in NON_LAYOUT_TABLES {
            if is_static && matches!(table_type, TableType::Variable) {
                debug!("Skip {tag} because this is a static font");
                continue;
            }
            if !has(context, work_id.clone()) {
                debug!("Skip {tag} because we don't have it");
                continue;
            }
            debug!("Grabbing {tag} for preliminary font");
            if let Some(bytes) = bytes_for(context, work_id.clone())? {
                len += bytes.len();
                tables.tables.insert(*tag, bytes);
            } else {
                debug!("No content for {tag}");
            }
        }

        debug!("Assembled {} table {} byte preliminary font", len, tables.tables.len());
        context.preliminary_font.set_unconditionally(tables);
        Ok(())
    }
}


impl Work<Context, AnyWorkId, Error> for FinalFontWork {
    fn id(&self) -> AnyWorkId {
        WorkId::FinalFont.into()
    }

    fn read_access(&self) -> Access<AnyWorkId> {
        // Preliminary font + layout
        Access::Set(HashSet::from([
            AnyWorkId::Be(WorkId::PreliminaryFont).into(),
            AnyWorkId::Be(WorkId::Gpos).into(),
            AnyWorkId::Be(WorkId::Gsub).into(),
            AnyWorkId::Be(WorkId::Gdef).into(),
        ]))
    }

    /// Glue binary tables into a font
    fn exec(&self, context: &Context) -> Result<(), Error> {
        debug!("Gathering parts of font");
        // Lets go right ahead and believe those bytes are a font
        let tables = context.preliminary_font.get();
        let mut builder = FontBuilder::default();

        // Add tables that are done by the preliminary cycle
        for (tag, bytes) in &tables.tables {
            builder.add_raw(*tag, bytes);
        }

        // Add tables that are not done by the preliminary cycle
        let is_static = context.ir.static_metadata.get().axes.is_empty();
        for (work_id, tag, table_type) in LAYOUT_TABLES {
            if is_static && matches!(table_type, TableType::Variable) {
                debug!("Skip {tag} because this is a static font");
                continue;
            }
            if !has(context, work_id.clone()) {
                debug!("Skip {tag} because we don't have it");
                continue;
            }
            debug!("Grabbing {tag} for final font");
            if let Some(bytes) = bytes_for(context, work_id.clone())? {
                builder.add_raw(*tag, bytes);
            } else {
                debug!("No content for {tag}");
            }
        }

        debug!("Building font");
        let font = builder.build();
        debug!("Assembled {} byte font", font.len());
        context.font.set_unconditionally(font.into());
        Ok(())
    }
}
