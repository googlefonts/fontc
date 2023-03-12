//! Generates a [cmap](https://learn.microsoft.com/en-us/typography/opentype/spec/cmap) table.

use fontdrasil::orchestration::Work;
use read_fonts::tables::cmap::PlatformId;
use write_fonts::{
    dump_table,
    tables::cmap::{Cmap, CmapSubtable, EncodingRecord},
};

use crate::{
    error::Error,
    orchestration::{BeWork, Context},
};

struct CmapWork {}

pub fn create_cmap_work() -> Box<BeWork> {
    Box::new(CmapWork {})
}

struct CmapEntry {
    codepoint: u16,
    glyph_id: u16,
}

// https://learn.microsoft.com/en-us/typography/opentype/spec/cmap#unicode-platform-platform-id--0
const UNICODE_BMP_ENCODING: u16 = 3;

impl Work<Context, Error> for CmapWork {
    /// Generate [cmap](https://learn.microsoft.com/en-us/typography/opentype/spec/cmap)
    ///
    /// For the time being just emit emit [format 4](https://learn.microsoft.com/en-us/typography/opentype/spec/cmap#format-4-segment-mapping-to-delta-values) so we can drive towards
    /// compiling working fonts. In time we may wish to additionally emit format 12 to support
    /// novel codepoints.
    fn exec(&self, context: &Context) -> Result<(), Error> {
        // cmap only accomodates single codepoint : glyph mappings; collect all of those
        let static_metadata = context.ir.get_static_metadata();

        let raw_cmap: Vec<CmapEntry> = static_metadata
            .glyph_order
            .iter()
            .map(|glyph_name| context.ir.get_glyph_ir(glyph_name))
            .enumerate()
            .flat_map(|(gid, glyph)| {
                glyph
                    .accessors
                    .iter()
                    .filter_map(|codepoints| {
                        if codepoints.len() == 1 {
                            Some(CmapEntry {
                                codepoint: *codepoints.first().unwrap() as u16,
                                glyph_id: gid as u16,
                            })
                        } else {
                            None
                        }
                    })
                    .collect::<Vec<_>>()
            })
            .collect();

        let mut end_code: Vec<u16> = Vec::new();
        let mut start_code: Vec<u16> = Vec::new();
        let mut id_deltas: Vec<i16> = Vec::new();
        let mut id_range_offsets: Vec<u16> = Vec::new();

        let prev_codepoint = u16::MAX - 1;
        for entry in raw_cmap {
            if prev_codepoint.checked_add(1).unwrap() != entry.codepoint {
                // Start a new run
                start_code.push(entry.codepoint);
                end_code.push(entry.codepoint);

                // we might need to reach further than an i16 can take us
                // using idRangeOffset ... but we're saving that for another day
                id_deltas.push(
                    (entry.glyph_id as i32 - entry.codepoint as i32)
                        .try_into()
                        .unwrap(),
                );
                id_range_offsets.push(0);
            } else {
                // Continue the prior run
                let last = end_code.last_mut().unwrap();
                *last = entry.codepoint;
            }
        }

        // close out
        start_code.push(0xFFFF);
        end_code.push(0xFFFF);
        id_deltas.push(1);
        id_range_offsets.push(0);

        let cmap = Cmap::new(vec![EncodingRecord::new(
            PlatformId::Unicode,
            UNICODE_BMP_ENCODING,
            CmapSubtable::create_format_4(
                0, // set to zero for all 'cmap' subtables whose platform IDs are other than Macintosh
                end_code, start_code, id_deltas,
            ),
        )]);
        let cmap = dump_table(&cmap).map_err(Error::CmapGenerationError)?;
        context.set_cmap(cmap);
        Ok(())
    }
}
