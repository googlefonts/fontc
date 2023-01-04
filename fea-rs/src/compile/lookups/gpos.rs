//! GPOS subtable builders

use std::collections::{BTreeMap, HashMap};

use smol_str::SmolStr;
use write_fonts::{
    tables::{
        gpos::{self as write_gpos, AnchorTable, MarkRecord, ValueFormat, ValueRecord},
        layout::{CoverageTable, CoverageTableBuilder},
    },
    types::GlyphId,
};

use crate::types::GlyphClass;

use super::{Builder, ClassDefBuilder2};

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
    pairs: GlyphPairPosBuilder,
    classes: ClassPairPosBuilder,
}

#[derive(Clone, Debug, Default)]
struct GlyphPairPosBuilder(BTreeMap<GlyphId, BTreeMap<GlyphId, (ValueRecord, ValueRecord)>>);

#[derive(Clone, Debug)]
struct ClassPairPosSubtable {
    items: BTreeMap<GlyphClass, BTreeMap<GlyphClass, (ValueRecord, ValueRecord)>>,
    classdef_1: ClassDefBuilder2,
    classdef_2: ClassDefBuilder2,
}

impl Default for ClassPairPosSubtable {
    fn default() -> Self {
        Self {
            items: Default::default(),
            classdef_1: ClassDefBuilder2::new(true),
            classdef_2: Default::default(),
        }
    }
}

#[derive(Clone, Debug, Default)]
struct ClassPairPosBuilder(BTreeMap<(ValueFormat, ValueFormat), Vec<ClassPairPosSubtable>>);

impl ClassPairPosBuilder {
    fn insert(
        &mut self,
        class1: GlyphClass,
        record1: ValueRecord,
        class2: GlyphClass,
        record2: ValueRecord,
    ) {
        let key = (record1.format(), record2.format());
        let entry = self.0.entry(key).or_default();
        let add_sub = match entry.last() {
            None => true,
            Some(subtable) => !subtable.can_add(&class1, &class2),
        };
        if add_sub {
            entry.push(Default::default());
        }
        entry
            .last_mut()
            .unwrap()
            .add(class1, class2, record1, record2);
    }
}

impl ClassPairPosSubtable {
    fn can_add(&self, class1: &GlyphClass, class2: &GlyphClass) -> bool {
        self.classdef_1.can_add(class1) && self.classdef_2.can_add(class2)
    }

    fn add(
        &mut self,
        class1: GlyphClass,
        class2: GlyphClass,
        record1: ValueRecord,
        record2: ValueRecord,
    ) {
        self.classdef_1.checked_add(class1.clone());
        self.classdef_2.checked_add(class2.clone());
        self.items
            .entry(class1)
            .or_default()
            .insert(class2, (record1, record2));
    }
}

impl PairPosBuilder {
    pub(crate) fn insert_pair(
        &mut self,
        glyph1: GlyphId,
        record1: ValueRecord,
        glyph2: GlyphId,
        record2: ValueRecord,
    ) {
        self.pairs
            .0
            .entry(glyph1)
            .or_default()
            .insert(glyph2, (record1, record2));
    }

    pub(crate) fn insert_classes(
        &mut self,
        class1: GlyphClass,
        record1: ValueRecord,
        class2: GlyphClass,
        record2: ValueRecord,
    ) {
        self.classes.insert(class1, record1, class2, record2)
    }
}

impl Builder for PairPosBuilder {
    type Output = Vec<write_gpos::PairPos>;

    fn build(self) -> Self::Output {
        let mut out = self.pairs.build();
        out.extend(self.classes.build());
        out
    }
}

impl Builder for GlyphPairPosBuilder {
    type Output = Vec<write_gpos::PairPos>;

    fn build(self) -> Self::Output {
        let mut split_by_format = BTreeMap::<_, BTreeMap<_, Vec<_>>>::default();
        for (g1, map) in self.0 {
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

impl Builder for ClassPairPosBuilder {
    type Output = Vec<write_gpos::PairPos>;

    fn build(self) -> Self::Output {
        self.0
            .into_values()
            .flat_map(|subs| subs.into_iter().map(Builder::build))
            .collect()
    }
}

impl Builder for ClassPairPosSubtable {
    type Output = write_gpos::PairPos;

    fn build(self) -> Self::Output {
        assert!(!self.items.is_empty(), "filter before here");
        // we have a set of classes/values with a single valueformat

        // an empty record, if some pair of classes have no entry
        let empty_record = self
            .items
            .values()
            .next()
            .and_then(|val| val.values().next())
            .map(|(v1, v2)| {
                write_gpos::Class2Record::new(
                    empty_record_with_format(v1.format()),
                    empty_record_with_format(v2.format()),
                )
            })
            .unwrap();

        let (class1def, class1map) = self.classdef_1.build();
        let (class2def, class2map) = self.classdef_2.build();

        let coverage = self
            .items
            .keys()
            .flat_map(GlyphClass::iter)
            .collect::<CoverageTableBuilder>()
            .build();

        let mut out = vec![write_gpos::Class1Record::default(); self.items.len()];
        for (cls1, stuff) in self.items {
            let idx = class1map.get(&cls1).unwrap();
            let mut records = vec![empty_record.clone(); class2map.len() + 1];
            for (class, (v1, v2)) in stuff {
                let idx = class2map.get(&class).unwrap();
                records[*idx as usize] = write_gpos::Class2Record::new(v1.clone(), v2.clone());
            }
            out[*idx as usize] = write_gpos::Class1Record::new(records);
        }
        write_gpos::PairPos::format_2(coverage, class1def, class2def, out)
    }
}

fn empty_record_with_format(format: ValueFormat) -> ValueRecord {
    let mut result = ValueRecord::default();
    if format.contains(ValueFormat::X_PLACEMENT) {
        result.x_placement = Some(0);
    }
    if format.contains(ValueFormat::Y_PLACEMENT) {
        result.y_placement = Some(0);
    }
    if format.contains(ValueFormat::X_ADVANCE) {
        result.x_advance = Some(0);
    }
    if format.contains(ValueFormat::Y_ADVANCE) {
        result.y_advance = Some(0);
    }
    if format.intersects(
        ValueFormat::X_PLACEMENT_DEVICE
            | ValueFormat::Y_PLACEMENT_DEVICE
            | ValueFormat::X_ADVANCE_DEVICE
            | ValueFormat::Y_ADVANCE_DEVICE,
    ) {
        //FIXME idk here. I think we need to update code in write-fonts so that
        // we take value format into account when deciding what we write,
        // instead of just ignoring null values
        panic!("writing explicit null offsets is currently broken, sorry")
    }

    result
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
struct MarkList {
    glyphs: BTreeMap<GlyphId, MarkRecord>,
    // map class names to their idx for this table
    classes: HashMap<SmolStr, u16>,
}

impl MarkList {
    /// If this glyph is already part of another class, return the previous class name
    ///
    /// Otherwise return the u16 id for this class, in this lookup.
    fn insert(
        &mut self,
        glyph: GlyphId,
        class: SmolStr,
        anchor: AnchorTable,
    ) -> Result<u16, PreviouslyAssignedClass> {
        let next_id = self.classes.len().try_into().unwrap();
        let id = *self.classes.entry(class).or_insert(next_id);
        if let Some(prev) = self
            .glyphs
            .insert(glyph, MarkRecord::new(id, anchor))
            .filter(|prev| prev.mark_class != id)
        {
            let class = self
                .classes
                .iter()
                .find_map(|(name, idx)| (*idx == prev.mark_class).then(|| name.clone()))
                .unwrap();

            return Err(PreviouslyAssignedClass {
                glyph_id: glyph,
                class,
            });
        }
        Ok(id)
    }

    fn glyphs(&self) -> impl Iterator<Item = GlyphId> + Clone + '_ {
        self.glyphs.keys().copied()
    }

    fn get_class(&self, class_name: &SmolStr) -> u16 {
        *self
            .classes
            .get(class_name)
            .expect("marks added before bases")
    }
}

impl Builder for MarkList {
    type Output = (CoverageTable, write_gpos::MarkArray);

    fn build(self) -> Self::Output {
        let coverage = self.glyphs().collect::<CoverageTableBuilder>();
        let array = write_gpos::MarkArray::new(self.glyphs.into_values().collect());
        (coverage.build(), array)
    }
}

#[derive(Clone, Debug, Default)]
pub struct MarkToBaseBuilder {
    marks: MarkList,
    bases: BTreeMap<GlyphId, Vec<(u16, AnchorTable)>>,
}

/// An error indicating a given glyph is has be
pub struct PreviouslyAssignedClass {
    pub glyph_id: GlyphId,
    pub class: SmolStr,
}

impl MarkToBaseBuilder {
    /// Add a new mark glyph.
    ///
    /// If this glyph already exists in another mark class, we return the
    /// previous class; this is likely an error.
    pub fn insert_mark(
        &mut self,
        glyph: GlyphId,
        class: SmolStr,
        anchor: AnchorTable,
    ) -> Result<u16, PreviouslyAssignedClass> {
        self.marks.insert(glyph, class, anchor)
    }

    pub fn insert_base(&mut self, glyph: GlyphId, class: &SmolStr, anchor: AnchorTable) {
        let class = self.marks.get_class(class);
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
        let MarkToBaseBuilder { marks, bases } = self;
        let n_classes = marks.classes.len();

        let (mark_coverage, mark_array) = marks.build();
        let base_coverage = bases.keys().copied().collect::<CoverageTableBuilder>();
        let base_records = bases
            .into_values()
            .map(|anchors| {
                let mut anchor_offsets = vec![None; n_classes];
                for (class, anchor) in anchors {
                    anchor_offsets[class as usize] = Some(anchor);
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
    ligatures: BTreeMap<GlyphId, Vec<BTreeMap<SmolStr, AnchorTable>>>,
}

impl MarkToLigBuilder {
    pub fn insert_mark(
        &mut self,
        glyph: GlyphId,
        class: SmolStr,
        anchor: AnchorTable,
    ) -> Result<u16, PreviouslyAssignedClass> {
        self.marks.insert(glyph, class, anchor)
    }

    pub fn add_lig(&mut self, glyph: GlyphId, components: Vec<BTreeMap<SmolStr, AnchorTable>>) {
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
        let MarkToLigBuilder { marks, ligatures } = self;
        let n_classes = marks.classes.len();

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
                        let mut anchor_offsets = vec![None; n_classes];
                        for (class, anchor) in anchors {
                            let class_idx = marks.get_class(&class);
                            anchor_offsets[class_idx as usize] = Some(anchor);
                        }
                        write_gpos::ComponentRecord::new(anchor_offsets)
                    })
                    .collect();
                write_gpos::LigatureAttach::new(comp_records)
            })
            .collect();
        let ligature_array = write_gpos::LigatureArray::new(ligature_array);
        let (mark_coverage, mark_array) = marks.build();
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
    base_marks: BTreeMap<GlyphId, Vec<(u16, AnchorTable)>>,
}

impl MarkToMarkBuilder {
    pub fn insert_mark(
        &mut self,
        glyph: GlyphId,
        class: SmolStr,
        anchor: AnchorTable,
    ) -> Result<u16, PreviouslyAssignedClass> {
        self.attaching_marks.insert(glyph, class, anchor)
    }

    pub fn insert_base(&mut self, glyph: GlyphId, class: &SmolStr, anchor: AnchorTable) {
        let id = self.attaching_marks.get_class(class);
        self.base_marks.entry(glyph).or_default().push((id, anchor))
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
        } = self;
        let n_classes = attaching_marks.classes.len();

        let (mark_coverage, mark_array) = attaching_marks.build();
        let mark2_coverage = base_marks.keys().copied().collect::<CoverageTableBuilder>();
        let mark2_records = base_marks
            .into_values()
            .map(|anchors| {
                let mut anchor_offsets = vec![None; n_classes];
                for (class, anchor) in anchors {
                    anchor_offsets[class as usize] = Some(anchor);
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
