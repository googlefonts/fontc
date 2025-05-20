//! Merge tables into a font

use fontdrasil::orchestration::{Access, AccessBuilder, Work};
use fontir::orchestration::WorkId as FeWorkId;
use log::debug;
use write_fonts::{
    read::TopLevelTable,
    tables::{
        avar::Avar, cmap::Cmap, colr::Colr, cpal::Cpal, fvar::Fvar, gasp::Gasp, gdef::Gdef,
        glyf::Glyf, gpos::Gpos, gsub::Gsub, gvar::Gvar, head::Head, hhea::Hhea, hmtx::Hmtx,
        hvar::Hvar, loca::Loca, maxp::Maxp, meta::Meta, mvar::Mvar, name::Name, os2::Os2,
        post::Post, stat::Stat, vhea::Vhea, vmtx::Vmtx,
    },
    types::Tag,
    FontBuilder,
};

use crate::{
    error::Error,
    orchestration::{to_bytes, AnyWorkId, BeWork, Context, WorkId},
};

#[derive(Debug)]
struct FontWork {}

pub fn create_font_work() -> Box<BeWork> {
    Box::new(FontWork {})
}

fn is_variable_only(workid: &WorkId) -> bool {
    matches!(
        workid,
        WorkId::Avar | WorkId::Fvar | WorkId::Gvar | WorkId::Stat | WorkId::Hvar | WorkId::Mvar
    )
}

const TABLES_TO_MERGE: &[(WorkId, Tag)] = &[
    (WorkId::Avar, Avar::TAG),
    (WorkId::Cmap, Cmap::TAG),
    (WorkId::Colr, Colr::TAG),
    (WorkId::Cpal, Cpal::TAG),
    (WorkId::Fvar, Fvar::TAG),
    (WorkId::Head, Head::TAG),
    (WorkId::Hhea, Hhea::TAG),
    (WorkId::Hmtx, Hmtx::TAG),
    (WorkId::Gasp, Gasp::TAG),
    (WorkId::Glyf, Glyf::TAG),
    (WorkId::Gpos, Gpos::TAG),
    (WorkId::Gsub, Gsub::TAG),
    (WorkId::Gdef, Gdef::TAG),
    (WorkId::Gvar, Gvar::TAG),
    (WorkId::Loca, Loca::TAG),
    (WorkId::Maxp, Maxp::TAG),
    (WorkId::Name, Name::TAG),
    (WorkId::Os2, Os2::TAG),
    (WorkId::Post, Post::TAG),
    (WorkId::Stat, Stat::TAG),
    (WorkId::Hvar, Hvar::TAG),
    (WorkId::Mvar, Mvar::TAG),
    (WorkId::Meta, Meta::TAG),
    (WorkId::Vhea, Vhea::TAG),
    (WorkId::Vmtx, Vmtx::TAG),
];

fn has(context: &Context, id: WorkId) -> bool {
    match id {
        WorkId::Avar => context.avar.try_get().is_some(),
        WorkId::Cmap => context.cmap.try_get().is_some(),
        WorkId::Colr => context.colr.try_get().is_some(),
        WorkId::Cpal => context.cpal.try_get().is_some(),
        WorkId::Fvar => context.fvar.try_get().is_some(),
        WorkId::Head => context.head.try_get().is_some(),
        WorkId::Hhea => context.hhea.try_get().is_some(),
        WorkId::Hmtx => context.hmtx.try_get().is_some(),
        WorkId::Gasp => context.gasp.try_get().is_some(),
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
        WorkId::Mvar => context.mvar.try_get().is_some(),
        WorkId::Meta => context.meta.try_get().is_some(),
        WorkId::Vhea => context.vhea.try_get().is_some(),
        WorkId::Vmtx => context.vmtx.try_get().is_some(),
        _ => false,
    }
}

fn bytes_for(context: &Context, id: WorkId) -> Result<Option<Vec<u8>>, Error> {
    // TODO: to_vec copies :(
    let bytes = match id {
        WorkId::Avar => context.avar.get().as_ref().as_ref().and_then(to_bytes),
        WorkId::Cmap => to_bytes(context.cmap.get().as_ref()),
        WorkId::Colr => to_bytes(context.colr.get().as_ref()),
        WorkId::Cpal => to_bytes(context.cpal.get().as_ref()),
        WorkId::Fvar => to_bytes(context.fvar.get().as_ref()),
        WorkId::Head => to_bytes(context.head.get().as_ref()),
        WorkId::Hhea => to_bytes(context.hhea.get().as_ref()),
        WorkId::Hmtx => Some(context.hmtx.get().as_ref().get().to_vec()),
        WorkId::Gasp => to_bytes(context.gasp.get().as_ref()),
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
        WorkId::Mvar => to_bytes(context.mvar.get().as_ref()),
        WorkId::Meta => to_bytes(context.meta.get().as_ref()),
        WorkId::Vhea => to_bytes(context.vhea.get().as_ref()),
        WorkId::Vmtx => Some(context.vmtx.get().as_ref().get().to_vec()),
        _ => panic!("Missing a match for {id:?}"),
    };
    Ok(bytes)
}

impl Work<Context, AnyWorkId, Error> for FontWork {
    fn id(&self) -> AnyWorkId {
        WorkId::Font.into()
    }

    fn read_access(&self) -> Access<AnyWorkId> {
        AccessBuilder::new()
            .variant(WorkId::Avar)
            .variant(WorkId::Cmap)
            .variant(WorkId::Colr)
            .variant(WorkId::Cpal)
            .variant(WorkId::Fvar)
            .variant(WorkId::Head)
            .variant(WorkId::Hhea)
            .variant(WorkId::Hmtx)
            .variant(WorkId::Gasp)
            .variant(WorkId::Glyf)
            .variant(WorkId::Gpos)
            .variant(WorkId::Gsub)
            .variant(WorkId::Gdef)
            .variant(WorkId::Gvar)
            .variant(WorkId::Loca)
            .variant(WorkId::Maxp)
            .variant(WorkId::Name)
            .variant(WorkId::Os2)
            .variant(WorkId::Post)
            .variant(WorkId::Stat)
            .variant(WorkId::Hvar)
            .variant(WorkId::Mvar)
            .variant(WorkId::Meta)
            .variant(WorkId::LocaFormat)
            .variant(WorkId::Vhea)
            .variant(WorkId::Vmtx)
            .variant(FeWorkId::StaticMetadata)
            .variant(WorkId::ExtraFeaTables)
            .build()
    }

    /// Glue binary tables into a font
    fn exec(&self, context: &Context) -> Result<(), Error> {
        // Lets go right ahead and believe those bytes are a font
        let mut builder = FontBuilder::default();

        // currently this is a special case where we slap the table from FEA
        // directly into the font, if it exists; if we ever start compiling
        // this directly, we need to merge this at that point.
        assert!(
            !TABLES_TO_MERGE
                .iter()
                .any(|(_, tag)| *tag == Tag::new(b"BASE")),
            "if manually generating BASE, handle possible BASE table in FEA"
        );
        if let Some(base) = context
            .extra_fea_tables
            .try_get()
            .and_then(|fea| fea.base.clone())
        {
            log::info!("using BASE table from fea");
            builder
                .add_table(&base)
                .map_err(|e| Error::DumpTableError {
                    e: e.inner,
                    context: "dump BASE failed".into(),
                })?;
        }

        // A fancier implementation would mmap the files. We basic.
        let is_static = context.ir.static_metadata.get().axes.is_empty();
        for (work_id, tag) in TABLES_TO_MERGE {
            if !has(context, work_id.clone()) {
                debug!("Skip {tag} because we don't have it");
                continue;
            }
            debug!("Grabbing {tag} for final font");
            if let Some(bytes) = bytes_for(context, work_id.clone())? {
                if is_variable_only(work_id) && is_static {
                    log::warn!("We generated {tag} for a static font, which seems weird but okay");
                }
                builder.add_raw(*tag, bytes);
            } else {
                debug!("No content for {tag}");
            }
        }

        debug!("Building font");
        let font = builder.build();
        debug!("Assembled {} byte font", font.len());
        context.font.set(font.into());
        Ok(())
    }
}
