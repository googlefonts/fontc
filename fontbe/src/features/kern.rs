//! Generates an [FeaRsKerns] datastructure to be fed to fea-rs

use std::collections::{BTreeMap, HashMap, HashSet};

use fea_rs::{
    compile::{PairPosBuilder, ValueRecord as ValueRecordBuilder},
    GlyphSet,
};
use fontdrasil::{
    coords::NormalizedLocation,
    orchestration::{Access, AccessBuilder, Work},
    types::GlyphName,
};
use fontir::{
    ir::{KernGroup, KernPair, KernParticipant, KerningGroups, KerningInstance},
    orchestration::WorkId as FeWorkId,
};
use log::debug;
use ordered_float::OrderedFloat;
use write_fonts::types::GlyphId;

use crate::{
    error::Error,
    features::resolve_variable_metric,
    orchestration::{
        AllKerningPairs, AnyWorkId, BeWork, Context, FeaRsKerns, KernAdjustments, KernFragment,
        PairPosEntry, WorkId,
    },
};

/// On Linux it took ~0.01 ms per loop, try to get enough to make fan out worthwhile
/// based on empirical testing
const KERNS_PER_BLOCK: usize = 2048;

/// Accumulation of all the kerning from IR
#[derive(Debug)]
struct GatherIrKerningWork;

/// A fragment of the kerning from IR that can run in parallel with other fragments
#[derive(Debug)]
struct KerningFragmentWork {
    segment: usize,
}

/// Accumulate the result of fragment processing so we can feed it to fea-rs
#[derive(Debug)]
struct KerningGatherWork;

pub fn create_gather_ir_kerning_work() -> Box<BeWork> {
    Box::new(GatherIrKerningWork {})
}

pub fn create_kern_segment_work(kern_pairs: &AllKerningPairs) -> Vec<Box<BeWork>> {
    let segments = kern_pairs.adjustments.len().div_ceil(KERNS_PER_BLOCK);
    let mut work: Vec<Box<BeWork>> = Vec::with_capacity(segments);
    debug!(
        "Process {} kerning adjustments in {} chunks",
        kern_pairs.adjustments.len(),
        segments
    );
    for segment in 0..segments {
        work.push(Box::new(KerningFragmentWork { segment }));
    }
    work
}

pub fn create_kerns_work() -> Box<BeWork> {
    Box::new(KerningGatherWork {})
}

impl Work<Context, AnyWorkId, Error> for GatherIrKerningWork {
    fn id(&self) -> AnyWorkId {
        WorkId::GatherIrKerning.into()
    }

    fn read_access(&self) -> Access<AnyWorkId> {
        // Blocks execution until workload.rs handle_success figures out what we will read, e.g. IR kerning instances is done.
        // Updated when success for IR kerning groups is received. See https://github.com/googlefonts/fontc/pull/655.
        Access::Unknown
    }

    fn write_access(&self) -> Access<AnyWorkId> {
        Access::Variant(WorkId::GatherIrKerning.into())
    }

    /// Generate kerning data structures.
    fn exec(&self, context: &Context) -> Result<(), Error> {
        let glyph_order = context.ir.glyph_order.get();
        let ir_groups = context.ir.kerning_groups.get();
        let ir_kerns = context.ir.kerning_at.all();

        // convert the groups stored in the Kerning object into the glyph classes
        // expected by fea-rs:
        let glyph_classes = ir_groups
            .groups
            .iter()
            .map(|(class_name, glyph_set)| {
                let glyph_class: GlyphSet = glyph_set
                    .iter()
                    .map(|name| glyph_order.glyph_id(name).unwrap_or(GlyphId::NOTDEF))
                    .collect();
                (class_name.clone(), glyph_class)
            })
            .collect::<BTreeMap<_, _>>();

        // Add IR kerns to builder. IR kerns are split by location so put them back together again.
        let mut kern_by_pos: HashMap<_, _> = ir_kerns
            .iter()
            .map(|(_, ki)| (ki.location.clone(), ki.as_ref().to_owned()))
            .collect();

        align_kerning(&ir_groups, &mut kern_by_pos);
        // Use a BTreeMap  because it seems the order we process pairs matters. Maybe we should sort instead...?
        let mut adjustments: BTreeMap<KernPair, KernAdjustments> = Default::default();

        // We want to add items to locations in the same order as the group locations
        // so start with group locations and then find the matching kerning.
        ir_groups
            .locations
            .iter()
            .filter_map(|pos| kern_by_pos.get(pos))
            .flat_map(|instance| {
                instance
                    .kerns
                    .iter()
                    .map(|(pair, adjustment)| (pair, (instance.location.clone(), *adjustment)))
            })
            .for_each(|(pair, (location, adjustment))| {
                adjustments
                    .entry(pair.clone())
                    .or_default()
                    .insert(location, adjustment);
            });

        let adjustments: Vec<_> = adjustments.into_iter().collect();
        debug!(
            "{} ir kerns became {} classes and {} adjustments",
            ir_kerns.len(),
            glyph_classes.len(),
            adjustments.len()
        );
        context.all_kerning_pairs.set(AllKerningPairs {
            glyph_classes,
            adjustments,
        });
        Ok(())
    }
}

/// 'align' the kerning, ensuring each pair is defined for each location.
///
/// missing pairs are filled in via the UFO kerning value lookup algorithm:
///
/// <https://unifiedfontobject.org/versions/ufo3/kerning.plist/#kerning-value-lookup-algorithm>
fn align_kerning(
    groups: &KerningGroups,
    instances: &mut HashMap<NormalizedLocation, KerningInstance>,
) {
    let union_kerning = instances
        .values()
        .flat_map(|instance| instance.kerns.keys())
        .cloned()
        .collect::<HashSet<_>>();

    let side1_glyph_to_group_map = groups
        .groups
        .iter()
        .filter(|(group, _)| matches!(group, KernGroup::Side1(_)))
        .flat_map(|(group, glyphs)| glyphs.iter().map(move |glyph| (glyph, group)))
        .collect::<HashMap<_, _>>();
    let side2_glyph_to_group_map = groups
        .groups
        .iter()
        .filter(|(group, _)| matches!(group, KernGroup::Side2(_)))
        .flat_map(|(group, glyphs)| glyphs.iter().map(move |glyph| (glyph, group)))
        .collect::<HashMap<_, _>>();

    for instance in instances.values_mut() {
        let missing_pairs = union_kerning
            .iter()
            .filter(|pair| !instance.kerns.contains_key(pair))
            .collect::<Vec<_>>();

        for pair in missing_pairs {
            let value = lookup_kerning_value(
                pair,
                instance,
                &side1_glyph_to_group_map,
                &side2_glyph_to_group_map,
            );
            instance.kerns.insert(pair.to_owned(), value);
        }
    }
}

// <https://github.com/fonttools/fonttools/blob/a3b9eddcafca/Lib/fontTools/ufoLib/kerning.py#L1>
fn lookup_kerning_value(
    pair: &KernPair,
    kerning: &KerningInstance,
    side1_glyphs: &HashMap<&GlyphName, &KernGroup>,
    side2_glyphs: &HashMap<&GlyphName, &KernGroup>,
) -> OrderedFloat<f32> {
    // if already a group, return it, else look for group for glyph
    fn get_group_if_glyph(
        side: &KernParticipant,
        map: &HashMap<&GlyphName, &KernGroup>,
    ) -> Option<KernParticipant> {
        match side {
            KernParticipant::Glyph(glyph) => map
                .get(&glyph)
                .map(|group| KernParticipant::Group((*group).clone())),
            KernParticipant::Group(_) => Some(side.to_owned()),
        }
    }

    let (first, second) = pair;
    // for each side: if it's a group, we only check the group.
    // if it's a glyph, we check both the glyph as well as the group containing that glyph.
    let first_group = get_group_if_glyph(first, side1_glyphs);
    let second_group = get_group_if_glyph(second, side2_glyphs);
    let first = Some(first).filter(|side| side.is_glyph());
    let second = Some(second).filter(|side| side.is_glyph());

    for (first, second) in [
        (first.cloned(), second_group.clone()),
        (first_group.clone(), second.cloned()),
        (first_group.clone(), second_group.clone()),
    ] {
        if let Some(pair) = first.zip(second) {
            if let Some(value) = kerning.kerns.get(&pair) {
                return *value;
            }
        }
    }

    // then fallback to zero
    0.0.into()
}

impl Work<Context, AnyWorkId, Error> for KerningFragmentWork {
    fn id(&self) -> AnyWorkId {
        WorkId::KernFragment(self.segment).into()
    }

    fn read_access(&self) -> Access<AnyWorkId> {
        AccessBuilder::new()
            .variant(FeWorkId::StaticMetadata)
            .variant(WorkId::GatherIrKerning)
            .build()
    }

    fn exec(&self, context: &Context) -> Result<(), Error> {
        let static_metadata = context.ir.static_metadata.get();
        let arc_glyph_order = context.ir.glyph_order.get();
        let glyph_order = arc_glyph_order.as_ref();
        let kerning = context.all_kerning_pairs.get();
        let start = self.segment * KERNS_PER_BLOCK;
        let end = (start + KERNS_PER_BLOCK).min(kerning.adjustments.len());
        assert!(start <= end, "bad range {start}..{end}");

        // now for each kerning entry, directly add a rule to a builder:
        let our_kerns = &kerning.adjustments[start..end];
        let glyph_classes = &kerning.glyph_classes;

        let mut kerns = Vec::new();
        let gid = |name| {
            glyph_order
                .glyph_id(name)
                .ok_or_else(|| Error::MissingGlyphId(name.clone()))
        };
        for ((left, right), values) in our_kerns {
            let (default_value, deltas) = resolve_variable_metric(&static_metadata, values.iter())?;

            let mut x_adv_record = ValueRecordBuilder::new().with_x_advance(default_value);
            // only encode deltas if they aren't all zeros
            if deltas.iter().any(|v| v.1 != 0) {
                x_adv_record = x_adv_record.with_x_advance_device(deltas);
            }
            let empty = ValueRecordBuilder::new();

            match (left, right) {
                (KernParticipant::Glyph(left), KernParticipant::Glyph(right)) => {
                    let (left, right) = (gid(left)?, gid(right)?);
                    kerns.push(PairPosEntry::Pair(
                        left,
                        x_adv_record.clone(),
                        right,
                        empty.clone(),
                    ));
                }
                (KernParticipant::Group(left), KernParticipant::Group(right)) => {
                    let left = glyph_classes
                        .get(left)
                        .ok_or_else(|| Error::MissingKernGroup(left.clone()))?
                        .clone();
                    let right = glyph_classes
                        .get(right)
                        .ok_or_else(|| Error::MissingKernGroup(right.clone()))?
                        .clone();
                    kerns.push(PairPosEntry::Class(
                        left,
                        x_adv_record.clone(),
                        right,
                        empty.clone(),
                    ));
                }
                // if groups are mixed with glyphs then we enumerate the group
                (KernParticipant::Glyph(left), KernParticipant::Group(right)) => {
                    let gid0 = glyph_order
                        .glyph_id(left)
                        .ok_or_else(|| Error::MissingGlyphId(left.clone()))?;
                    let right = glyph_classes
                        .get(right)
                        .ok_or_else(|| Error::MissingKernGroup(right.clone()))?;
                    for gid1 in right.iter() {
                        kerns.push(PairPosEntry::Pair(
                            gid0,
                            x_adv_record.clone(),
                            gid1,
                            empty.clone(),
                        ));
                    }
                }
                (KernParticipant::Group(left), KernParticipant::Glyph(right)) => {
                    let left = glyph_classes
                        .get(left)
                        .ok_or_else(|| Error::MissingKernGroup(left.clone()))?;
                    let gid1 = gid(right)?;
                    for gid0 in left.iter() {
                        kerns.push(PairPosEntry::Pair(
                            gid0,
                            x_adv_record.clone(),
                            gid1,
                            empty.clone(),
                        ));
                    }
                }
            }
        }

        context.kern_fragments.set(KernFragment {
            segment: self.segment,
            kerns,
        });

        Ok(())
    }
}

impl Work<Context, AnyWorkId, Error> for KerningGatherWork {
    fn id(&self) -> AnyWorkId {
        WorkId::GatherBeKerning.into()
    }

    fn read_access(&self) -> Access<AnyWorkId> {
        Access::Unknown // https://github.com/googlefonts/fontc/issues/647: don't enable until KernFragment's spawn
    }

    fn exec(&self, context: &Context) -> Result<(), Error> {
        debug!("Gather be kerning");
        let arc_fragments = context.kern_fragments.all();
        let mut fragments: Vec<_> = arc_fragments
            .iter()
            .map(|(_, fragment)| fragment.as_ref())
            .collect();
        fragments.sort_by_key(|fragment| fragment.segment);

        let mut builder = PairPosBuilder::default();

        let mut entries = 0;
        for ppe in fragments
            .iter()
            .flat_map(|fragment| fragment.kerns.iter())
            .cloned()
        {
            ppe.add_to(&mut builder);
            entries += 1;
        }

        debug!("{entries} be kerns gathered");
        let mut kerns = FeaRsKerns::default();
        if !builder.is_empty() {
            kerns.lookups = vec![builder];
        }
        context.fea_rs_kerns.set(kerns);

        Ok(())
    }
}
