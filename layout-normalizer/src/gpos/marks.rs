use std::{
    collections::{BTreeMap, HashMap, HashSet},
    fmt::Debug,
};

use write_fonts::read::{
    tables::gpos::{MarkBasePosFormat1, MarkMarkPosFormat1},
    types::GlyphId,
    ReadError,
};

use crate::{common::GlyphSet, glyph_names::NameMap, variations::DeltaComputer};

use super::{PrintNames, ResolvedAnchor};

#[derive(Clone, Debug, PartialEq, Eq)]
pub(super) struct MarkAttachmentRule {
    base: GlyphId,
    base_anchor: ResolvedAnchor,
    marks: BTreeMap<ResolvedAnchor, GlyphSet>,
}

impl PrintNames for MarkAttachmentRule {
    fn fmt_names(&self, f: &mut std::fmt::Formatter<'_>, names: &NameMap) -> std::fmt::Result {
        let base_name = names.get(self.base);
        writeln!(f, "{base_name} {}", self.base_anchor)?;
        for (i, (anchor, glyphs)) in self.marks.iter().enumerate() {
            if i != 0 {
                writeln!(f)?;
            }

            write!(f, "  {anchor} {}", glyphs.printer(names))?;
        }
        Ok(())
    }
}
impl Ord for MarkAttachmentRule {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        (&self.base, &self.marks.values().next()).cmp(&(&other.base, &other.marks.values().next()))
    }
}

impl PartialOrd for MarkAttachmentRule {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

pub(super) fn get_mark_base_rules(
    subtables: &[MarkBasePosFormat1],
    delta_computer: Option<&DeltaComputer>,
) -> Result<Vec<MarkAttachmentRule>, ReadError> {
    // so we only take the first coverage hit in each subtable, which means
    // we just need track what we've seen.
    let mut seen = HashSet::new();
    let mut result = Vec::new();
    for sub in subtables.iter() {
        append_mark_base_rules(sub, delta_computer, &mut seen, &mut result)?;
    }
    Ok(result)
}

// append the rules for a single subtable
fn append_mark_base_rules(
    subtable: &MarkBasePosFormat1,
    delta_computer: Option<&DeltaComputer>,
    seen: &mut HashSet<(GlyphId, GlyphId)>,
    result: &mut Vec<MarkAttachmentRule>,
) -> Result<(), ReadError> {
    let base_array = subtable.base_array()?;
    let base_records = base_array.base_records();
    let mark_array = subtable.mark_array()?;
    let mark_records = mark_array.mark_records();

    let cov_ix_to_mark_gid: HashMap<_, _> = subtable.mark_coverage()?.iter().enumerate().collect();

    for (base_ix, base_glyph) in subtable.base_coverage()?.iter().enumerate() {
        let base_record = base_records.get(base_ix)?;
        for (base_anchor_ix, base_anchor) in base_record
            .base_anchors(base_array.offset_data())
            .iter()
            .enumerate()
        {
            let Some(base_anchor) = base_anchor else {
                continue;
            };
            let base_anchor = base_anchor?;
            let base_anchor = ResolvedAnchor::new(&base_anchor, delta_computer)?;
            let mut marks = BTreeMap::default();
            for (mark_ix, mark_record) in mark_records.iter().enumerate() {
                let mark_class = mark_record.mark_class() as usize;
                if mark_class != base_anchor_ix {
                    continue;
                }
                let Some(mark_glyph) = cov_ix_to_mark_gid.get(&mark_ix) else {
                    continue;
                };

                if !seen.insert((base_glyph, *mark_glyph)) {
                    // this was included in a previous subtable, so skip it
                    continue;
                }

                let mark_anchor = mark_record.mark_anchor(mark_array.offset_data())?;
                let mark_anchor = ResolvedAnchor::new(&mark_anchor, delta_computer)?;
                marks
                    .entry(mark_anchor)
                    .or_insert_with(|| GlyphSet::from(*mark_glyph))
                    .add(*mark_glyph);
            }
            let group = MarkAttachmentRule {
                base: base_glyph,
                base_anchor,
                marks: marks
                    .into_iter()
                    .map(|(anchor, glyphs)| (anchor, glyphs))
                    .collect(),
            };
            result.push(group);
        }
    }
    Ok(())
}

pub(super) fn get_mark_mark_rules(
    subtables: &[MarkMarkPosFormat1],
    delta_computer: Option<&DeltaComputer>,
) -> Result<Vec<MarkAttachmentRule>, ReadError> {
    // so we only take the first coverage hit in each subtable, which means
    // we just need track what we've seen.
    let mut seen = HashSet::new();
    let mut result = Vec::new();
    for sub in subtables.iter() {
        append_mark_mark_rules(sub, delta_computer, &mut seen, &mut result)?;
    }
    Ok(result)
}

fn append_mark_mark_rules(
    subtable: &MarkMarkPosFormat1,
    delta_computer: Option<&DeltaComputer>,
    seen: &mut HashSet<(GlyphId, GlyphId)>,
    result: &mut Vec<MarkAttachmentRule>,
) -> Result<(), ReadError> {
    let base_array = subtable.mark2_array()?;
    let base_records = base_array.mark2_records();
    let mark_array = subtable.mark1_array()?;
    let mark_records = mark_array.mark_records();

    let cov_ix_to_mark_gid: HashMap<_, _> = subtable.mark1_coverage()?.iter().enumerate().collect();

    for (base_ix, base_glyph) in subtable.mark2_coverage()?.iter().enumerate() {
        let base_record = base_records.get(base_ix).unwrap();
        for (base_anchor_ix, base_anchor) in base_record
            .mark2_anchors(base_array.offset_data())
            .iter()
            .enumerate()
        {
            let Some(base_anchor) = base_anchor else {
                continue;
            };
            let base_anchor = base_anchor?;
            let base_anchor = ResolvedAnchor::new(&base_anchor, delta_computer)?;
            let mut marks = BTreeMap::default();
            for (mark_ix, mark_record) in mark_records.iter().enumerate() {
                let mark_class = mark_record.mark_class() as usize;
                if mark_class != base_anchor_ix {
                    continue;
                }
                let Some(mark_glyph) = cov_ix_to_mark_gid.get(&mark_ix) else {
                    continue;
                };

                if !seen.insert((base_glyph, *mark_glyph)) {
                    // this was included in a previous subtable, so skip it
                    continue;
                }

                let mark_anchor = mark_record.mark_anchor(mark_array.offset_data())?;
                let mark_anchor = ResolvedAnchor::new(&mark_anchor, delta_computer)?;
                marks
                    .entry(mark_anchor)
                    .or_insert_with(|| GlyphSet::from(*mark_glyph))
                    .add(*mark_glyph);
            }
            let group = MarkAttachmentRule {
                base: base_glyph,
                base_anchor,
                marks: marks
                    .into_iter()
                    .map(|(anchor, glyphs)| (anchor, glyphs))
                    .collect(),
            };
            result.push(group);
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use fea_rs::compile::MarkToBaseBuilder;
    use write_fonts::read::FontRead;

    use crate::test_helpers::SimpleMarkBaseBuilder;

    use super::*;

    // further decomposed for testing, so we just see one mark per entry
    #[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
    struct SimpleAnchorRule {
        base_gid: GlyphId,
        mark_gid: GlyphId,
        base_anchor: (i16, i16),
        mark_anchor: (i16, i16),
    }
    // convert from the enum back to the specific pairpos type.
    //
    // I want to change how these types work, but this is fine for now
    fn extract_rules(rules: Vec<MarkAttachmentRule>) -> Vec<SimpleAnchorRule> {
        rules
            .iter()
            .flat_map(|rule| {
                rule.marks.iter().flat_map(|(mark_anchor, mark_glyphs)| {
                    mark_glyphs.iter().map(|mark_gid| SimpleAnchorRule {
                        mark_gid,
                        mark_anchor: (mark_anchor.x.default, mark_anchor.y.default),
                        base_gid: rule.base,
                        base_anchor: (rule.base_anchor.x.default, rule.base_anchor.y.default),
                    })
                })
            })
            .collect()
    }
    type RawAnchor = (i16, i16);

    impl PartialEq<(u16, RawAnchor, u16, RawAnchor)> for SimpleAnchorRule {
        fn eq(&self, other: &(u16, RawAnchor, u16, RawAnchor)) -> bool {
            let (base_id, base_anchor, mark_id, mark_anchor) = *other;
            self.base_gid.to_u16() == base_id
                && self.base_anchor == base_anchor
                && self.mark_gid.to_u16() == mark_id
                && self.mark_anchor == mark_anchor
        }
    }

    #[test]
    fn first_subtable_wins() {
        let mut sub1 = MarkToBaseBuilder::default();
        sub1.add_mark(11, "top", (20, 20));
        sub1.add_mark(12, "top", (30, 30));
        sub1.add_base(1, "top", (200, 200));

        let mut sub2 = MarkToBaseBuilder::default();
        sub2.add_mark(10, "top", (-11, -11));
        sub2.add_mark(11, "top", (-22, -22)); // dupe, will be ignored
        sub2.add_base(1, "top", (404, 404));

        let sub1 = sub1.build_exactly_one_subtable();
        let sub2 = sub2.build_exactly_one_subtable();

        let sub1 = write_fonts::dump_table(&sub1).unwrap();
        let sub2 = write_fonts::dump_table(&sub2).unwrap();

        let sub1 =
            write_fonts::read::tables::gpos::MarkBasePosFormat1::read(sub1.as_slice().into())
                .unwrap();
        let sub2 =
            write_fonts::read::tables::gpos::MarkBasePosFormat1::read(sub2.as_slice().into())
                .unwrap();

        let rules = get_mark_base_rules(&[sub1, sub2], None).unwrap();
        let mut rules = extract_rules(rules);
        rules.sort_unstable();

        // (base gid, base anchor, mark gid, mark anchor)
        let expected: &[(u16, RawAnchor, u16, RawAnchor)] = &[
            (1, (404, 404), 10, (-11, -11)),
            (1, (200, 200), 11, (20, 20)),
            (1, (200, 200), 12, (30, 30)),
        ];
        assert_eq!(rules, expected,)
    }
}
