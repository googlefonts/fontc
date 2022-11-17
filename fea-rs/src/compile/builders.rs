//! Builders for layout tables

use std::collections::{BTreeMap, BTreeSet};
use std::convert::TryFrom;

use font_types::{FixedSize, GlyphId};
use write_fonts::tables::{
    gpos::{self as write_gpos, AnchorTable, MarkRecord, ValueFormat, ValueRecord},
    gsub as write_gsub,
    layout::{CoverageTable, CoverageTableBuilder},
};

type MarkClass = u16;

pub trait Builder {
    type Output;
    fn build(self) -> Result<Self::Output, ()>;
}

#[derive(Clone, Debug, Default)]
struct SinglePosSubtable {
    format: ValueFormat,
    items: BTreeMap<GlyphId, ValueRecord>,
}

#[derive(Clone, Debug, Default)]
pub struct SinglePosBuilder {
    subtables: Vec<SinglePosSubtable>,
}

impl SinglePosBuilder {
    //TODO: should we track the valueformat here?
    pub fn insert(&mut self, glyph: GlyphId, record: ValueRecord) {
        self.get_subtable(record.format())
            .items
            .insert(glyph, record);
    }

    fn get_subtable(&mut self, format: ValueFormat) -> &mut SinglePosSubtable {
        if self.subtables.last().map(|sub| sub.format) != Some(format) {
            self.subtables.push(SinglePosSubtable {
                format,
                items: BTreeMap::new(),
            });
        }
        self.subtables.last_mut().unwrap()
    }
}

impl Builder for SinglePosBuilder {
    type Output = Vec<write_gpos::SinglePos>;

    fn build(self) -> Result<Self::Output, ()> {
        self.subtables.into_iter().map(Builder::build).collect()
    }
}

impl Builder for SinglePosSubtable {
    type Output = write_gpos::SinglePos;

    fn build(self) -> Result<Self::Output, ()> {
        let first_value = self.items.values().next().unwrap();
        let format_1 = self.items.values().all(|val| val == first_value);
        let coverage: CoverageTableBuilder = self.items.keys().copied().collect();
        if format_1 {
            Ok(write_gpos::SinglePos::format_1(
                coverage.build(),
                first_value.to_owned(),
            ))
        } else {
            Ok(write_gpos::SinglePos::format_2(
                coverage.build(),
                self.items.into_values().collect(),
            ))
        }
    }
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
    fn build(self) -> Result<Self::Output, ()> {
        let mut split_by_format = BTreeMap::new();
        for (g1, map) in self.items {
            for (g2, (v1, v2)) in map {
                split_by_format
                    .entry((v1.format(), v2.format()))
                    .or_insert(BTreeMap::default())
                    .entry(g1)
                    .or_insert(Vec::new())
                    .push(write_gpos::PairValueRecord::new(g2, v1, v2));
            }
        }

        Ok(split_by_format
            .into_iter()
            .map(|(_, map)| {
                let coverage: CoverageTableBuilder = map.keys().copied().collect();
                let pair_sets = map.into_values().map(write_gpos::PairSet::new).collect();
                write_gpos::PairPos::format_1(coverage.build(), pair_sets)
            })
            .collect())
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

    fn build(self) -> Result<Self::Output, ()> {
        let coverage: CoverageTableBuilder = self.items.keys().copied().collect();
        let records = self.items.into_values().collect();
        Ok(vec![write_gpos::CursivePosFormat1::new(
            coverage.build(),
            records,
        )])
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

    fn build(self) -> Result<Self::Output, ()> {
        let coverage = self.0.keys().copied().collect::<CoverageTableBuilder>();
        let array = write_gpos::MarkArray::new(self.0.into_values().collect());
        Ok((coverage.build(), array))
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

    fn build(self) -> Result<Self::Output, ()> {
        let MarkToBaseBuilder {
            marks,
            bases,
            mark_classes,
        } = self;

        let (mark_coverage, mark_array) = marks.build()?;
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
        Ok(vec![write_gpos::MarkBasePosFormat1::new(
            mark_coverage,
            base_coverage.build(),
            mark_array,
            base_array,
        )])
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

    fn build(self) -> Result<Self::Output, ()> {
        let MarkToLigBuilder {
            marks,
            mark_classes,
            ligatures,
        } = self;

        let (mark_coverage, mark_array) = marks.build()?;
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
        Ok(vec![write_gpos::MarkLigPosFormat1::new(
            mark_coverage,
            ligature_coverage.build(),
            mark_array,
            ligature_array,
        )])
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

    fn build(self) -> Result<Self::Output, ()> {
        let MarkToMarkBuilder {
            attaching_marks,
            base_marks,
            mark_classes,
        } = self;

        let (mark_coverage, mark_array) = attaching_marks.build()?;
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
        Ok(vec![write_gpos::MarkMarkPosFormat1::new(
            mark_coverage,
            mark2_coverage.build(),
            mark_array,
            mark2array,
        )])
    }
}

#[derive(Clone, Debug, Default)]
pub struct SingleSubBuilder {
    items: BTreeMap<GlyphId, (GlyphId, PossibleSingleSubFormat)>,
}

/// Used to divide pairs into subtables as needed.
#[derive(Clone, Debug, Copy, PartialEq, Eq)]
enum PossibleSingleSubFormat {
    // this pair can be format1
    Delta(i16),
    // this pair must be format 2 (delta not in i16 range)
    Format2,
}

impl SingleSubBuilder {
    pub fn insert(&mut self, target: GlyphId, replacement: GlyphId) {
        let delta = target.to_u16() as i32 - replacement.to_u16() as i32;
        let delta = i16::try_from(delta)
            .map(PossibleSingleSubFormat::Delta)
            .unwrap_or(PossibleSingleSubFormat::Format2);
        self.items.insert(target, (replacement, delta));
    }
}

impl Builder for SingleSubBuilder {
    type Output = Vec<write_gsub::SingleSubst>;

    fn build(self) -> Result<Self::Output, ()> {
        const COST_OF_EXTRA_SUB1F1_SUBTABLE: usize = 2 + // extra offset
2 + 2 + 2 + // format1 table itself
2 + 2; // extra coverage table

        const N_GLYPHS_TO_JUSTIFY_EXTRA_SUB1F1: usize =
            COST_OF_EXTRA_SUB1F1_SUBTABLE / GlyphId::RAW_BYTE_LEN;

        #[derive(Default)]
        struct SubtableMap {
            format1: BTreeMap<i16, Vec<(GlyphId, GlyphId)>>,
            format2: Vec<(GlyphId, GlyphId)>,
        }

        impl SubtableMap {
            fn from_builder(builder: SingleSubBuilder) -> Self {
                let mut this = SubtableMap::default();
                for (g1, (g2, delta)) in builder.items {
                    match delta {
                        PossibleSingleSubFormat::Delta(delta) => {
                            this.format1.entry(delta).or_default().push((g1, g2))
                        }
                        PossibleSingleSubFormat::Format2 => this.format2.push((g1, g2)),
                    }
                }
                this
            }

            fn len(&self) -> usize {
                self.format1.len() + usize::from(!self.format2.is_empty())
            }

            fn reduce(&mut self) {
                if self.len() <= 1 {
                    return;
                }

                //TODO: there is an optimization here where we preserve two
                //(and possibly three?) format1 tables if format2 does not already exist

                let SubtableMap { format1, format2 } = self;
                format1.retain(|_delta, pairs| {
                    if pairs.len() < N_GLYPHS_TO_JUSTIFY_EXTRA_SUB1F1 {
                        format2.extend(pairs.iter().copied());
                        false
                    } else {
                        true
                    }
                })
            }

            fn build(mut self) -> Vec<write_gsub::SingleSubst> {
                let mut result = Vec::with_capacity(self.len());
                if !self.format2.is_empty() {
                    self.format2.sort_unstable();
                    let coverage = self
                        .format2
                        .iter()
                        .copied()
                        .map(|(g1, _)| g1)
                        .collect::<CoverageTableBuilder>();
                    let subs = self.format2.into_iter().map(|(_, g2)| g2).collect();
                    result.push(write_gsub::SingleSubst::format_2(coverage.build(), subs));
                }

                for (delta, pairs) in self.format1 {
                    let coverage = pairs
                        .into_iter()
                        .map(|(g1, _)| g1)
                        .collect::<CoverageTableBuilder>();
                    result.push(write_gsub::SingleSubst::format_1(coverage.build(), delta));
                }
                result
            }
        }

        // optimal subtable generation:
        // TODO: the runtime efficiency of this implementation could be improved.
        // steps:
        // - sort all pairs into their 'preferred' subtables (everything that
        // can be in a format 1 table is)
        // - go through the format1 tables and move small ones into the format 2 table

        let mut map = SubtableMap::from_builder(self);
        map.reduce();
        Ok(map.build())
    }
}

#[derive(Clone, Debug, Default)]
pub struct MultipleSubBuilder {
    items: BTreeMap<GlyphId, Vec<GlyphId>>,
}

impl Builder for MultipleSubBuilder {
    type Output = Vec<write_gsub::MultipleSubstFormat1>;

    fn build(self) -> Result<Self::Output, ()> {
        let coverage = self.items.keys().copied().collect::<CoverageTableBuilder>();
        let seq_tables = self
            .items
            .into_values()
            .map(write_gsub::Sequence::new)
            .collect();
        Ok(vec![write_gsub::MultipleSubstFormat1::new(
            coverage.build(),
            seq_tables,
        )])
    }
}

impl MultipleSubBuilder {
    pub fn insert(&mut self, target: GlyphId, replacement: Vec<GlyphId>) {
        self.items.insert(target, replacement);
    }
}

#[derive(Clone, Debug, Default)]
pub struct AlternateSubBuilder {
    items: BTreeMap<GlyphId, Vec<GlyphId>>,
}

impl AlternateSubBuilder {
    pub fn insert(&mut self, target: GlyphId, replacement: Vec<GlyphId>) {
        self.items.insert(target, replacement);
    }
}

#[derive(Clone, Debug, Default)]
pub struct LigatureSubBuilder {
    items: BTreeMap<Vec<GlyphId>, GlyphId>,
}

impl LigatureSubBuilder {
    pub fn insert(&mut self, target: Vec<GlyphId>, replacement: GlyphId) {
        self.items.insert(target, replacement);
    }
}

//#[derive(Clone, Debug, Default)]
//pub struct SubBuilder {}
