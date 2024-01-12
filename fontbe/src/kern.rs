//! Generates an [FeaRsKerns] datastructure to be fed to fea-rs

use std::collections::{BTreeMap, HashMap};

use fea_rs::{
    compile::{PairPosBuilder, ValueRecord as ValueRecordBuilder},
    GlyphSet,
};
use fontdrasil::orchestration::{Access, AccessBuilder, Work};
use fontir::{
    ir::{KernPair, KernParticipant},
    orchestration::WorkId as FeWorkId,
};
use log::debug;
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
        let kern_by_pos: HashMap<_, _> = ir_kerns
            .iter()
            .map(|(_, ki)| (ki.location.clone(), ki.as_ref()))
            .collect();
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
            let x_adv_record = ValueRecordBuilder::new()
                .with_x_advance(default_value)
                .with_x_advance_device(deltas);
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
                        .ok_or_else(|| Error::MissingGlyphId(left.clone()))?
                        .clone();
                    let right = glyph_classes
                        .get(right)
                        .ok_or_else(|| Error::MissingGlyphId(right.clone()))?
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
                        .ok_or_else(|| Error::MissingGlyphId(right.clone()))?;
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
                        .ok_or_else(|| Error::MissingGlyphId(left.clone()))?;
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
