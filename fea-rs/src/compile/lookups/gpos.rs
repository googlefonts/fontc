//! GPOS subtable builders

use std::collections::{BTreeMap, BTreeSet, HashMap};

use write_fonts::{
    tables::{
        gpos::{self as write_gpos, AnchorTable, MarkRecord, ValueFormat, ValueRecord},
        layout::{CoverageTable, CoverageTableBuilder},
    },
    types::GlyphId,
};

use super::Builder;

type MarkClass = u16;

#[derive(Clone, Debug, Default)]
pub struct SinglePosBuilder {
    items: BTreeMap<GlyphId, ValueRecord>,
}

impl SinglePosBuilder {
    //TODO: should we track the valueformat here?
    pub fn insert(&mut self, glyph: GlyphId, record: ValueRecord) {
        self.items.insert(glyph, record);
    }

    pub(crate) fn can_add_rule(&self, glyph: GlyphId, value: &ValueRecord) -> bool {
        self.items
            .get(&glyph)
            .map(|existing| existing == value)
            .unwrap_or(true)
    }
}

impl Builder for SinglePosBuilder {
    type Output = Vec<write_gpos::SinglePos>;

    fn build(self) -> Self::Output {
        fn build_subtable(items: BTreeMap<GlyphId, &ValueRecord>) -> write_gpos::SinglePos {
            let first = *items.values().next().unwrap();
            let use_format_1 = first.format().is_empty() || items.values().all(|val| val == &first);
            let coverage: CoverageTableBuilder = items.keys().copied().collect();
            if use_format_1 {
                write_gpos::SinglePos::format_1(coverage.build(), first.clone())
            } else {
                write_gpos::SinglePos::format_2(
                    coverage.build(),
                    items.into_values().cloned().collect(),
                )
            }
        }
        const NEW_SUBTABLE_COST: usize = 10;

        // list of sets of glyph ids which will end up in their own subtables
        let mut subtables = Vec::new();
        let mut group_by_record: HashMap<&ValueRecord, BTreeMap<GlyphId, &ValueRecord>> =
            Default::default();

        // first group by specific record; glyphs that share a record can use
        // the more efficient format-1 subtable type
        for (gid, value) in &self.items {
            group_by_record
                .entry(value)
                .or_default()
                .insert(*gid, value);
        }
        let mut group_by_format: HashMap<ValueFormat, BTreeMap<GlyphId, &ValueRecord>> =
            Default::default();
        for (value, glyphs) in group_by_record {
            // if this saves us size, use format 1
            if glyphs.len() * value.encoded_size() > NEW_SUBTABLE_COST {
                subtables.push(glyphs);
                // else split based on value format; each format will be its own
                // format 2 table
            } else {
                group_by_format
                    .entry(value.format())
                    .or_default()
                    .extend(glyphs.into_iter());
            }
        }
        subtables.extend(group_by_format.into_values());

        let mut output = subtables
            .into_iter()
            .map(build_subtable)
            .collect::<Vec<_>>();

        // finally sort the subtables: first in decreasing order of size,
        // using first glyph id to break ties (matches feaLib)
        output.sort_unstable_by_key(|table| match table {
            write_gpos::SinglePos::Format1(table) => cmp_coverage_key(&table.coverage),
            write_gpos::SinglePos::Format2(table) => cmp_coverage_key(&table.coverage),
        });
        output
    }
}

fn cmp_coverage_key(coverage: &CoverageTable) -> impl Ord {
    (std::cmp::Reverse(coverage.len()), coverage.iter().next())
}

#[derive(Clone, Debug, Default)]
pub struct PairPosBuilder {
    items: BTreeMap<GlyphId, BTreeMap<GlyphId, (ValueRecord, ValueRecord)>>,
}

impl PairPosBuilder {
    pub fn insert(
        &mut self,
        glyph1: GlyphId,
        record1: ValueRecord,
        glyph2: GlyphId,
        record2: ValueRecord,
    ) {
        self.items
            .entry(glyph1)
            .or_default()
            .insert(glyph2, (record1, record2));
    }
}

impl Builder for PairPosBuilder {
    type Output = Vec<write_gpos::PairPos>;

    //FIXME: this always uses format 1.
    fn build(self) -> Self::Output {
        let mut split_by_format = BTreeMap::<_, BTreeMap<_, Vec<_>>>::default();
        for (g1, map) in self.items {
            for (g2, (v1, v2)) in map {
                split_by_format
                    .entry((v1.format(), v2.format()))
                    .or_default()
                    .entry(g1)
                    .or_default()
                    .push(write_gpos::PairValueRecord::new(g2, v1, v2));
            }
        }

        split_by_format
            .into_values()
            .map(|map| {
                let coverage: CoverageTableBuilder = map.keys().copied().collect();
                let pair_sets = map.into_values().map(write_gpos::PairSet::new).collect();
                write_gpos::PairPos::format_1(coverage.build(), pair_sets)
            })
            .collect()
    }
}

#[derive(Clone, Debug, Default)]
pub struct CursivePosBuilder {
    items: BTreeMap<GlyphId, write_gpos::EntryExitRecord>,
}

impl CursivePosBuilder {
    pub fn insert(
        &mut self,
        glyph: GlyphId,
        entry: Option<AnchorTable>,
        exit: Option<AnchorTable>,
    ) {
        let record = write_gpos::EntryExitRecord::new(entry, exit);
        self.items.insert(glyph, record);
    }
}

impl Builder for CursivePosBuilder {
    type Output = Vec<write_gpos::CursivePosFormat1>;

    fn build(self) -> Self::Output {
        let coverage: CoverageTableBuilder = self.items.keys().copied().collect();
        let records = self.items.into_values().collect();
        vec![write_gpos::CursivePosFormat1::new(
            coverage.build(),
            records,
        )]
    }
}

// shared between several tables
#[derive(Clone, Debug, Default)]
struct MarkList(BTreeMap<GlyphId, MarkRecord>);

impl MarkList {
    fn insert(
        &mut self,
        glyph: GlyphId,
        class: MarkClass,
        anchor: AnchorTable,
    ) -> Result<(), PreviouslyAssignedClass> {
        match self
            .0
            .insert(glyph, MarkRecord::new(class, anchor))
            .map(|rec| rec.mark_class)
        {
            Some(old_class) if old_class != class => Err(PreviouslyAssignedClass {
                glyph_id: glyph,
                class: old_class,
            }),
            _ => Ok(()),
        }
    }

    fn glyphs(&self) -> impl Iterator<Item = GlyphId> + Clone + '_ {
        self.0.keys().copied()
    }
}

impl Builder for MarkList {
    type Output = (CoverageTable, write_gpos::MarkArray);

    fn build(self) -> Self::Output {
        let coverage = self.0.keys().copied().collect::<CoverageTableBuilder>();
        let array = write_gpos::MarkArray::new(self.0.into_values().collect());
        (coverage.build(), array)
    }
}

#[derive(Clone, Debug, Default)]
pub struct MarkToBaseBuilder {
    marks: MarkList,
    mark_classes: BTreeSet<MarkClass>,
    bases: BTreeMap<GlyphId, Vec<(MarkClass, AnchorTable)>>,
}

/// An error indicating a given glyph is has be
pub struct PreviouslyAssignedClass {
    pub glyph_id: GlyphId,
    pub class: MarkClass,
}

impl MarkToBaseBuilder {
    /// Add a new mark glyph.
    ///
    /// If this glyph already exists in another mark class, we return the
    /// previous class; this is likely an error.
    pub fn insert_mark(
        &mut self,
        glyph: GlyphId,
        class: MarkClass,
        anchor: AnchorTable,
    ) -> Result<(), PreviouslyAssignedClass> {
        self.mark_classes.insert(class);
        self.marks.insert(glyph, class, anchor)
    }

    pub fn insert_base(&mut self, glyph: GlyphId, class: MarkClass, anchor: AnchorTable) {
        self.bases.entry(glyph).or_default().push((class, anchor))
    }

    pub fn base_glyphs(&self) -> impl Iterator<Item = GlyphId> + Clone + '_ {
        self.bases.keys().copied()
    }

    pub fn mark_glyphs(&self) -> impl Iterator<Item = GlyphId> + Clone + '_ {
        self.marks.glyphs()
    }
}

impl Builder for MarkToBaseBuilder {
    type Output = Vec<write_gpos::MarkBasePosFormat1>;

    fn build(self) -> Self::Output {
        let MarkToBaseBuilder {
            marks,
            bases,
            mark_classes,
        } = self;

        let (mark_coverage, mark_array) = marks.build();
        let base_coverage = bases.keys().copied().collect::<CoverageTableBuilder>();
        let base_records = bases
            .into_values()
            .map(|anchors| {
                let mut anchor_offsets: Vec<Option<AnchorTable>> = Vec::new();
                anchor_offsets.resize(mark_classes.len(), None);
                for (class, anchor) in anchors {
                    let class_idx = mark_classes.iter().position(|c| c == &class).unwrap();
                    anchor_offsets[class_idx] = Some(anchor);
                }
                write_gpos::BaseRecord::new(anchor_offsets)
            })
            .collect();
        let base_array = write_gpos::BaseArray::new(base_records);
        vec![write_gpos::MarkBasePosFormat1::new(
            mark_coverage,
            base_coverage.build(),
            mark_array,
            base_array,
        )]
    }
}

#[derive(Clone, Debug, Default)]
pub struct MarkToLigBuilder {
    marks: MarkList,
    mark_classes: BTreeSet<MarkClass>,
    ligatures: BTreeMap<GlyphId, Vec<BTreeMap<MarkClass, AnchorTable>>>,
}

impl MarkToLigBuilder {
    pub fn insert_mark(
        &mut self,
        glyph: GlyphId,
        class: MarkClass,
        anchor: AnchorTable,
    ) -> Result<(), PreviouslyAssignedClass> {
        self.mark_classes.insert(class);
        self.marks.insert(glyph, class, anchor)
    }

    pub fn add_lig(&mut self, glyph: GlyphId, components: Vec<BTreeMap<MarkClass, AnchorTable>>) {
        self.ligatures.insert(glyph, components);
    }

    pub fn mark_glyphs(&self) -> impl Iterator<Item = GlyphId> + Clone + '_ {
        self.marks.glyphs()
    }

    pub fn lig_glyphs(&self) -> impl Iterator<Item = GlyphId> + Clone + '_ {
        self.ligatures.keys().copied()
    }
}

impl Builder for MarkToLigBuilder {
    type Output = Vec<write_gpos::MarkLigPosFormat1>;

    fn build(self) -> Self::Output {
        let MarkToLigBuilder {
            marks,
            mark_classes,
            ligatures,
        } = self;

        let (mark_coverage, mark_array) = marks.build();
        // LigArray:
        // - [LigatureAttach] (one per ligature glyph)
        //    - [ComponentRecord] (one per component)
        //    - [Anchor] (one per mark-class)
        let ligature_coverage = ligatures.keys().copied().collect::<CoverageTableBuilder>();
        let ligature_array = ligatures
            .into_values()
            .map(|components| {
                let comp_records = components
                    .into_iter()
                    .map(|anchors| {
                        let mut anchor_offsets: Vec<Option<AnchorTable>> = Vec::new();
                        anchor_offsets.resize(mark_classes.len(), None);
                        for (class, anchor) in anchors {
                            let class_idx = mark_classes.iter().position(|c| c == &class).unwrap();
                            anchor_offsets[class_idx] = Some(anchor);
                        }
                        write_gpos::ComponentRecord::new(anchor_offsets)
                    })
                    .collect();
                write_gpos::LigatureAttach::new(comp_records)
            })
            .collect();
        let ligature_array = write_gpos::LigatureArray::new(ligature_array);
        vec![write_gpos::MarkLigPosFormat1::new(
            mark_coverage,
            ligature_coverage.build(),
            mark_array,
            ligature_array,
        )]
    }
}

#[derive(Clone, Debug, Default)]
pub struct MarkToMarkBuilder {
    attaching_marks: MarkList,
    mark_classes: BTreeSet<MarkClass>,
    base_marks: BTreeMap<GlyphId, Vec<(MarkClass, AnchorTable)>>,
}

impl MarkToMarkBuilder {
    pub fn insert_mark(
        &mut self,
        glyph: GlyphId,
        class: MarkClass,
        anchor: AnchorTable,
    ) -> Result<(), PreviouslyAssignedClass> {
        self.mark_classes.insert(class);
        self.attaching_marks.insert(glyph, class, anchor)
    }

    pub fn insert_base(&mut self, glyph: GlyphId, class: MarkClass, anchor: AnchorTable) {
        self.base_marks
            .entry(glyph)
            .or_default()
            .push((class, anchor))
    }

    pub fn mark1_glyphs(&self) -> impl Iterator<Item = GlyphId> + Clone + '_ {
        self.attaching_marks.glyphs()
    }

    pub fn mark2_glyphs(&self) -> impl Iterator<Item = GlyphId> + Clone + '_ {
        self.base_marks.keys().copied()
    }
}

impl Builder for MarkToMarkBuilder {
    type Output = Vec<write_gpos::MarkMarkPosFormat1>;

    fn build(self) -> Self::Output {
        let MarkToMarkBuilder {
            attaching_marks,
            base_marks,
            mark_classes,
        } = self;

        let (mark_coverage, mark_array) = attaching_marks.build();
        let mark2_coverage = base_marks.keys().copied().collect::<CoverageTableBuilder>();
        let mark2_records = base_marks
            .into_values()
            .map(|anchors| {
                let mut anchor_offsets: Vec<Option<AnchorTable>> = Vec::new();
                anchor_offsets.resize(mark_classes.len(), None);
                for (class, anchor) in anchors {
                    let class_idx = mark_classes.iter().position(|c| c == &class).unwrap();
                    anchor_offsets[class_idx] = Some(anchor);
                }
                write_gpos::Mark2Record::new(anchor_offsets)
            })
            .collect();
        let mark2array = write_gpos::Mark2Array::new(mark2_records);
        vec![write_gpos::MarkMarkPosFormat1::new(
            mark_coverage,
            mark2_coverage.build(),
            mark_array,
            mark2array,
        )]
    }
}
