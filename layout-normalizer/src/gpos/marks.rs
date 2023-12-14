use std::{
    any::Any,
    collections::{BTreeMap, HashMap, HashSet},
    fmt::Debug,
};

use write_fonts::read::{
    tables::{
        gpos::{MarkBasePosFormat1, MarkMarkPosFormat1},
        layout::LookupFlag,
    },
    types::GlyphId,
    ReadError,
};

use crate::{common::GlyphSet, glyph_names::NameMap, variations::DeltaComputer};

use super::{AnyRule, LookupRule, LookupType, ResolvedAnchor};

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(super) struct MarkAttachmentRule {
    kind: LookupType,
    flags: LookupFlag,
    base: GlyphId,
    base_anchor: ResolvedAnchor,
    marks: BTreeMap<ResolvedAnchor, GlyphSet>,
    filter_set: Option<u16>,
}

impl AnyRule for MarkAttachmentRule {
    fn lookup_flags(&self) -> (LookupFlag, Option<u16>) {
        (self.flags, self.filter_set)
    }

    fn fmt_impl(&self, f: &mut std::fmt::Formatter<'_>, names: &NameMap) -> std::fmt::Result {
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

    fn lookup_type(&self) -> LookupType {
        self.kind
    }

    fn as_any(&self) -> &dyn Any {
        self
    }
}

pub(super) fn get_mark_base_rules(
    subtables: &[MarkBasePosFormat1],
    flags: LookupFlag,
    filter_set: Option<u16>,
    delta_computer: Option<&DeltaComputer>,
) -> Result<Vec<LookupRule>, ReadError> {
    // so we only take the first coverage hit in each subtable, which means
    // we just need track what we've seen.
    let mut seen = HashSet::new();
    let mut result = Vec::new();
    for sub in subtables.iter() {
        append_mark_base_rules(
            sub,
            flags,
            filter_set,
            delta_computer,
            &mut seen,
            &mut result,
        )?;
    }
    Ok(result)
}

// append the rules for a single subtable
fn append_mark_base_rules(
    subtable: &MarkBasePosFormat1,
    flags: LookupFlag,
    filter_set: Option<u16>,
    delta_computer: Option<&DeltaComputer>,
    visited: &mut HashSet<GlyphId>,
    result: &mut Vec<LookupRule>,
) -> Result<(), ReadError> {
    let base_array = subtable.base_array()?;
    let base_records = base_array.base_records();
    let mark_array = subtable.mark_array()?;
    let mark_records = mark_array.mark_records();

    let cov_ix_to_mark_gid: HashMap<_, _> = subtable.mark_coverage()?.iter().enumerate().collect();

    for (base_ix, base_glyph) in subtable.base_coverage()?.iter().enumerate() {
        if !visited.insert(base_glyph) {
            // this was included in a previous subtable, so skip it
            continue;
        }

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
                let mark_anchor = mark_record.mark_anchor(mark_array.offset_data())?;
                let mark_anchor = ResolvedAnchor::new(&mark_anchor, delta_computer)?;
                marks
                    .entry(mark_anchor)
                    .or_insert_with(|| GlyphSet::from(*mark_glyph))
                    .add(*mark_glyph);
            }
            let group = MarkAttachmentRule {
                flags,
                base: base_glyph,
                base_anchor,
                marks: marks
                    .into_iter()
                    .map(|(anchor, glyphs)| (anchor, glyphs))
                    .collect(),
                kind: LookupType::MarkToBase,
                filter_set,
            };
            result.push(LookupRule::MarkBase(group));
        }
    }
    Ok(())
}

pub(super) fn get_mark_mark_rules(
    subtables: &[MarkMarkPosFormat1],
    flags: LookupFlag,
    filter_set: Option<u16>,
    delta_computer: Option<&DeltaComputer>,
) -> Result<Vec<LookupRule>, ReadError> {
    // so we only take the first coverage hit in each subtable, which means
    // we just need track what we've seen.
    let mut seen = HashSet::new();
    let mut result = Vec::new();
    for sub in subtables.iter() {
        append_mark_mark_rules(
            sub,
            flags,
            filter_set,
            delta_computer,
            &mut seen,
            &mut result,
        )?;
    }
    Ok(result)
}

fn append_mark_mark_rules(
    subtable: &MarkMarkPosFormat1,
    flags: LookupFlag,
    filter_set: Option<u16>,
    delta_computer: Option<&DeltaComputer>,
    seen: &mut HashSet<GlyphId>,
    result: &mut Vec<LookupRule>,
) -> Result<(), ReadError> {
    let base_array = subtable.mark2_array()?;
    let base_records = base_array.mark2_records();
    let mark_array = subtable.mark1_array()?;
    let mark_records = mark_array.mark_records();

    let cov_ix_to_mark_gid: HashMap<_, _> = subtable.mark1_coverage()?.iter().enumerate().collect();

    for (base_ix, base_glyph) in subtable.mark2_coverage()?.iter().enumerate() {
        if !seen.insert(base_glyph) {
            continue;
        }
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
                let mark_anchor = mark_record.mark_anchor(mark_array.offset_data())?;
                let mark_anchor = ResolvedAnchor::new(&mark_anchor, delta_computer)?;
                marks
                    .entry(mark_anchor)
                    .or_insert_with(|| GlyphSet::from(*mark_glyph))
                    .add(*mark_glyph);
            }
            let group = MarkAttachmentRule {
                flags,
                base: base_glyph,
                base_anchor,
                marks: marks
                    .into_iter()
                    .map(|(anchor, glyphs)| (anchor, glyphs))
                    .collect(),
                kind: LookupType::MarkToMark,
                filter_set,
            };
            result.push(LookupRule::MarkMark(group));
        }
    }
    Ok(())
}
