//! Generates a [cmap](https://learn.microsoft.com/en-us/typography/opentype/spec/cmap) table.

use std::collections::{BTreeMap, HashMap};

use fontdrasil::{
    orchestration::{Access, AccessBuilder, Work},
    types::GlyphName,
};
use fontir::{ir::GlyphOrder, orchestration::WorkId as FeWorkId};
use log::warn;

use write_fonts::{
    dump_table,
    tables::cmap::{
        Cmap, Cmap14, CmapSubtable, DefaultUvs, EncodingRecord, NonDefaultUvs, PlatformId,
        UnicodeRange, UvsMapping, VariationSelector,
    },
    types::{GlyphId, GlyphId16, Uint24},
};

use crate::{
    error::Error,
    orchestration::{AnyWorkId, BeWork, Context, WorkId},
};

// https://learn.microsoft.com/en-us/typography/opentype/spec/cmap#unicode-platform-platform-id--0
const UNICODE_VARIATION_SEQUENCES_ENCODING: u16 = 5;

#[derive(Debug)]
struct CmapWork {}

pub fn create_cmap_work() -> Box<BeWork> {
    Box::new(CmapWork {})
}

/// Build a [format 14] subtable, or `None` if there are no variation sequences.
///
/// A sequence is a default UVS when `mappings` already maps its base codepoint to
/// the same glyph, and a non-default UVS otherwise.
///
/// [format 14]: https://learn.microsoft.com/en-us/typography/opentype/spec/cmap#format-14-unicode-variation-sequences
fn variation_sequences_subtable(
    sequences: &BTreeMap<u32, BTreeMap<u32, GlyphName>>,
    mappings: &HashMap<u32, GlyphId16>,
    glyph_order: &GlyphOrder,
) -> Result<Option<Cmap14>, Error> {
    let mut var_selectors = Vec::new();
    for (selector, base_to_glyph) in sequences {
        let mut default_uvs = Vec::new();
        let mut non_default_uvs = Vec::new();
        for (codepoint, glyph_name) in base_to_glyph {
            let Some(gid) = glyph_order.glyph_id(glyph_name) else {
                warn!(
                    "Ignoring variation sequence U+{codepoint:04X} U+{selector:04X}: no glyph named '{glyph_name}'"
                );
                continue;
            };
            if mappings.get(codepoint) == Some(&gid) {
                default_uvs.push(*codepoint);
            } else {
                non_default_uvs.push(UvsMapping::new(Uint24::new(*codepoint), gid.to_u16()));
            }
        }
        if default_uvs.is_empty() && non_default_uvs.is_empty() {
            continue;
        }
        var_selectors.push(VariationSelector::new(
            Uint24::new(*selector),
            (!default_uvs.is_empty()).then(|| {
                let ranges = unicode_ranges(&default_uvs);
                DefaultUvs::new(ranges.len() as u32, ranges)
            }),
            (!non_default_uvs.is_empty())
                .then(|| NonDefaultUvs::new(non_default_uvs.len() as u32, non_default_uvs)),
        ));
    }
    if var_selectors.is_empty() {
        return Ok(None);
    }
    let mut cmap14 = Cmap14::new(0, var_selectors.len() as u32, var_selectors);
    // write-fonts computes the length of formats 4 and 12 but writes ours
    // verbatim, and it includes child tables the packer may dedup, so we
    // measure the packed bytes rather than sum the parts
    cmap14.length = dump_table(&cmap14)
        .map_err(|e| Error::DumpTableError {
            e,
            context: "cmap format 14".to_string(),
        })?
        .len() as u32;
    Ok(Some(cmap14))
}

/// Group sorted codepoints into runs of consecutive values.
///
/// <https://github.com/fonttools/fonttools/blob/90b9a6c9/Lib/fontTools/ttLib/tables/_c_m_a_p.py#L1559-L1571>
fn unicode_ranges(codepoints: &[u32]) -> Vec<UnicodeRange> {
    let mut ranges: Vec<UnicodeRange> = Vec::new();
    for codepoint in codepoints {
        if let Some(range) = ranges.last_mut() {
            let next = range.start_unicode_value.to_u32() + range.additional_count as u32 + 1;
            if next == *codepoint && range.additional_count < u8::MAX {
                range.additional_count += 1;
                continue;
            }
        }
        ranges.push(UnicodeRange::new(Uint24::new(*codepoint), 0));
    }
    ranges
}

impl Work<Context, AnyWorkId, Error> for CmapWork {
    fn id(&self) -> AnyWorkId {
        WorkId::Cmap.into()
    }

    fn read_access(&self) -> Access<AnyWorkId> {
        AccessBuilder::new()
            .variant(FeWorkId::StaticMetadata)
            .variant(FeWorkId::GlyphOrder)
            .variant(FeWorkId::ALL_GLYPHS)
            .build()
    }

    /// Generate [cmap](https://learn.microsoft.com/en-us/typography/opentype/spec/cmap)
    #[tracing::instrument(name = "fontbe::CmapWork::exec", skip_all)]
    fn exec(&self, context: &Context) -> Result<(), Error> {
        // cmap only accomodates single codepoint : glyph mappings; collect all of those
        let static_metadata = context.ir.static_metadata.get();
        let glyph_order = context.ir.glyph_order.get();

        let mappings = glyph_order
            .iter()
            .flat_map(|(gid, glyph_name)| {
                let glyph = context.ir.get_glyph(glyph_name.clone());
                glyph
                    .codepoints
                    .iter()
                    .map(|codepoint| (*codepoint, gid))
                    .collect::<Vec<_>>()
            })
            .collect::<Vec<_>>();

        let mut cmap = Cmap::from_mappings(mappings.iter().map(|(codepoint, gid)| {
            (
                char::from_u32(*codepoint).expect("We have an invalid codepoint!"),
                GlyphId::from(*gid),
            )
        }))?;

        if let Some(cmap14) = variation_sequences_subtable(
            &static_metadata.misc.unicode_variation_sequences,
            &mappings.into_iter().collect(),
            &glyph_order,
        )? {
            // Encoding records must be sorted by platform then encoding id
            let key = (PlatformId::Unicode, UNICODE_VARIATION_SEQUENCES_ENCODING);
            let idx = cmap
                .encoding_records
                .partition_point(|r| (r.platform_id, r.encoding_id) < key);
            cmap.encoding_records.insert(
                idx,
                EncodingRecord::new(key.0, key.1, CmapSubtable::Format14(cmap14)),
            );
        }

        context.cmap.set(cmap);
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use write_fonts::{
        read::{FontData, FontRead, tables::cmap as read_cmap},
        validate::Validate,
    };

    use super::*;

    fn glyph_order(names: &[&str]) -> GlyphOrder {
        names.iter().map(|n| GlyphName::from(*n)).collect()
    }

    fn sequences(entries: &[(u32, u32, &str)]) -> BTreeMap<u32, BTreeMap<u32, GlyphName>> {
        let mut sequences: BTreeMap<u32, BTreeMap<u32, GlyphName>> = BTreeMap::new();
        for (selector, codepoint, glyph_name) in entries {
            sequences
                .entry(*selector)
                .or_default()
                .insert(*codepoint, GlyphName::from(*glyph_name));
        }
        sequences
    }

    // (selector, default ranges as (start, additional_count), non-default (codepoint, gid))
    type Selector = (u32, Vec<(u32, u8)>, Vec<(u32, u16)>);

    fn build_and_read(
        entries: &[(u32, u32, &str)],
        mappings: &[(u32, u16)],
        glyph_order: &GlyphOrder,
    ) -> Option<Vec<Selector>> {
        let mappings = mappings
            .iter()
            .map(|(cp, gid)| (*cp, GlyphId16::new(*gid)))
            .collect();
        let cmap14 =
            variation_sequences_subtable(&sequences(entries), &mappings, glyph_order).unwrap()?;
        let bytes = dump_table(&cmap14).unwrap();
        assert_eq!(cmap14.length as usize, bytes.len());
        assert!(cmap14.validate().is_ok());
        let data = FontData::new(&bytes);
        let read = read_cmap::Cmap14::read(data).unwrap();
        assert_eq!(
            read.num_var_selector_records() as usize,
            cmap14.var_selector.len()
        );
        Some(
            read.var_selector()
                .iter()
                .map(|vs| {
                    let defaults = vs
                        .default_uvs(data)
                        .transpose()
                        .unwrap()
                        .map(|d| {
                            d.ranges()
                                .iter()
                                .map(|r| (r.start_unicode_value().to_u32(), r.additional_count()))
                                .collect()
                        })
                        .unwrap_or_default();
                    let non_defaults = vs
                        .non_default_uvs(data)
                        .transpose()
                        .unwrap()
                        .map(|nd| {
                            nd.uvs_mapping()
                                .iter()
                                .map(|m| (m.unicode_value().to_u32(), m.glyph_id()))
                                .collect()
                        })
                        .unwrap_or_default();
                    (vs.var_selector().to_u32(), defaults, non_defaults)
                })
                .collect(),
        )
    }

    #[test]
    fn no_sequences_no_subtable() {
        assert!(build_and_read(&[], &[(0x61, 1)], &glyph_order(&[".notdef", "a"])).is_none());
    }

    #[test]
    fn default_and_non_default_sequences() {
        let glyph_order = glyph_order(&[".notdef", "a", "b", "c", "a.uv001", "c.uv017"]);
        let mappings = [(0x61, 1), (0x62, 2), (0x63, 3), (0x1F170, 2)];
        let entries = [
            // VS1: a has an alternate, b and c use the default glyph
            (0xFE00, 0x61, "a.uv001"),
            (0xFE00, 0x62, "b"),
            (0xFE00, 0x63, "c"),
            // VS17 (U+E0100): a non-BMP default, a BMP alternate
            (0xE0100, 0x1F170, "b"),
            (0xE0100, 0x63, "c.uv017"),
            // VS16: only a default, same glyph as base
            (0xFE0F, 0x61, "a"),
        ];
        assert_eq!(
            build_and_read(&entries, &mappings, &glyph_order).unwrap(),
            vec![
                (0xFE00, vec![(0x62, 1)], vec![(0x61, 4)]),
                (0xFE0F, vec![(0x61, 0)], vec![]),
                (0xE0100, vec![(0x1F170, 0)], vec![(0x63, 5)]),
            ]
        );
    }

    #[test]
    fn same_glyph_different_codepoint_is_non_default() {
        // b maps U+0062; asking for b under U+0061 is a non-default sequence
        let glyph_order = glyph_order(&[".notdef", "a", "b"]);
        assert_eq!(
            build_and_read(
                &[(0xFE00, 0x61, "b")],
                &[(0x61, 1), (0x62, 2)],
                &glyph_order
            )
            .unwrap(),
            vec![(0xFE00, vec![], vec![(0x61, 2)])]
        );
    }

    #[test]
    fn missing_glyph_is_skipped() {
        let glyph_order = glyph_order(&[".notdef", "a"]);
        assert!(build_and_read(&[(0xFE00, 0x61, "a.uv001")], &[(0x61, 1)], &glyph_order).is_none());
    }

    #[test]
    fn default_ranges() {
        let ranges = |cps: &[u32]| {
            unicode_ranges(cps)
                .iter()
                .map(|r| (r.start_unicode_value.to_u32(), r.additional_count))
                .collect::<Vec<_>>()
        };
        assert_eq!(ranges(&[]), vec![]);
        assert_eq!(ranges(&[0x30]), vec![(0x30, 0)]);
        assert_eq!(
            ranges(&[0x30, 0x31, 0x32, 0x34, 0x1F170, 0x1F171]),
            vec![(0x30, 2), (0x34, 0), (0x1F170, 1)]
        );
        // additional_count is a u8; a longer run is split
        let long: Vec<u32> = (0x4E00..0x4E00 + 300).collect();
        assert_eq!(ranges(&long), vec![(0x4E00, 255), (0x4F00, 43)]);
    }
}
