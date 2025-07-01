use std::{
    collections::{BTreeMap, HashMap, HashSet},
    fmt::Debug,
};

use write_fonts::read::{
    tables::gpos::{MarkBasePosFormat1, MarkLigPosFormat1, MarkMarkPosFormat1},
    types::GlyphId16,
    ReadError,
};

use crate::{common::GlyphSet, glyph_names::NameMap, variations::DeltaComputer};

use super::{PrintNames, ResolvedAnchor};

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
enum BaseAnchors {
    Base(ResolvedAnchor),
    Liga(Vec<Option<ResolvedAnchor>>),
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct MarkAttachmentRule {
    pub base: GlyphId16,
    base_anchor: BaseAnchors,
    marks: BTreeMap<ResolvedAnchor, GlyphSet>,
}

impl PrintNames for MarkAttachmentRule {
    fn fmt_names(&self, f: &mut std::fmt::Formatter<'_>, names: &NameMap) -> std::fmt::Result {
        let base_name = names.get(self.base);
        match &self.base_anchor {
            BaseAnchors::Base(anchor) => writeln!(f, "{base_name} {anchor}")?,

            BaseAnchors::Liga(anchors) => {
                write!(f, "{base_name} (lig) [")?;
                for (i, anchor) in anchors.iter().enumerate() {
                    if i != 0 {
                        write!(f, ", ")?;
                    }
                    match anchor {
                        Some(a) => write!(f, "{a}"),
                        None => write!(f, "<NULL>"),
                    }?
                }
                writeln!(f, "]")?
            }
        }

        for (i, (anchor, glyphs)) in self.marks.iter().enumerate() {
            if i != 0 {
                writeln!(f)?;
            }

            write!(f, "  {anchor} {}", glyphs.printer(names))?;
        }
        Ok(())
    }
}

impl MarkAttachmentRule {
    pub(crate) fn iter_base_mark_pairs(&self) -> impl Iterator<Item = (GlyphId16, GlyphId16)> + '_ {
        self.marks
            .values()
            .flat_map(|set| set.iter())
            .map(|mark| (self.base, mark))
    }

    // returns `true` if we have marks remaining after removing this set
    pub(crate) fn remove_marks(&mut self, marks: &[GlyphId16]) -> bool {
        self.marks.retain(|_, v| {
            v.remove_glyphs(marks);
            !v.is_empty()
        });

        !self.marks.is_empty()
    }

    #[cfg(test)]
    pub(crate) fn iter_simple_rules(
        &self,
    ) -> impl Iterator<Item = super::test_helpers::SimpleAnchorRule> + '_ {
        self.marks.iter().flat_map(|(mark_anchor, mark_glyphs)| {
            mark_glyphs
                .iter()
                .map(|mark_gid| super::test_helpers::SimpleAnchorRule {
                    mark_gid,
                    mark_anchor: (mark_anchor.x.default, mark_anchor.y.default),
                    base_gid: self.base,
                    base_anchor: match &self.base_anchor {
                        BaseAnchors::Base(a) => (a.x.default, a.y.default),
                        BaseAnchors::Liga(_) => {
                            panic!("simple rules only for mark2base or mark2mark");
                        }
                    },
                })
        })
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
    seen: &mut HashSet<(GlyphId16, GlyphId16)>,
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
                base_anchor: BaseAnchors::Base(base_anchor),
                marks,
            };
            result.push(group);
        }
    }
    Ok(())
}

pub(super) fn get_mark_liga_rules(
    subtables: &[MarkLigPosFormat1],
    delta_computer: Option<&DeltaComputer>,
) -> Result<Vec<MarkAttachmentRule>, ReadError> {
    // so we only take the first coverage hit in each subtable, which means
    // we just need track what we've seen.
    let mut seen = HashSet::new();
    let mut result = Vec::new();
    for sub in subtables.iter() {
        append_mark_liga_rules(sub, delta_computer, &mut seen, &mut result)?;
    }
    Ok(result)
}

fn append_mark_liga_rules(
    subtable: &MarkLigPosFormat1,
    delta_computer: Option<&DeltaComputer>,
    _seen: &mut HashSet<(GlyphId16, GlyphId16)>,
    result: &mut Vec<MarkAttachmentRule>,
) -> Result<(), ReadError> {
    let lig_array = subtable.ligature_array()?;
    let lig_tables = lig_array.ligature_attaches();
    let mark_array = subtable.mark_array()?;
    let mark_records = mark_array.mark_records();
    let mark_count = subtable.mark_class_count() as usize;

    let cov_ix_to_mark_gid: HashMap<_, _> = subtable.mark_coverage()?.iter().enumerate().collect();

    // okay backing up:
    // a rule should be one ligature, and one mark class?
    for (lig_ix, lig_glyph) in subtable.ligature_coverage()?.iter().enumerate() {
        let lig_attach = lig_tables.get(lig_ix)?;

        for anchor_ix in 0..mark_count {
            // all the anchors in a given class, for this ligature
            let comp_anchors = lig_attach
                .component_records()
                .iter()
                .map(|comp| {
                    comp.and_then(|comp| {
                        comp.ligature_anchors(lig_attach.offset_data())
                            .get(anchor_ix)
                            .map(|anchor| {
                                anchor.and_then(|a| ResolvedAnchor::new(&a, delta_computer))
                            })
                            .transpose()
                    })
                })
                .collect::<Result<Vec<_>, _>>()?;

            let mut marks = BTreeMap::default();
            for (mark_ix, mark_record) in mark_records.iter().enumerate() {
                let mark_class = mark_record.mark_class() as usize;
                if mark_class != anchor_ix {
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
                base: lig_glyph,
                base_anchor: BaseAnchors::Liga(comp_anchors),
                marks,
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
    seen: &mut HashSet<(GlyphId16, GlyphId16)>,
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
                base_anchor: BaseAnchors::Base(base_anchor),
                marks,
            };
            result.push(group);
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use write_fonts::{read::FontRead, tables::gpos::builders::MarkToBaseBuilder};

    use super::super::test_helpers::{self, RawAnchor, SimpleMarkBaseBuilder};

    use super::*;

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
        let mut rules = test_helpers::SimpleAnchorRule::from_mark_rules(&rules);
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
