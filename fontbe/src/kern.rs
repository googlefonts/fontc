//! Generates a [Kerning] datastructure to be fed to fea-rs

use std::collections::{BTreeMap, HashMap};

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
    ir::{GlyphOrder, KernPair, KernParticipant},
    orchestration::WorkId as FeWorkId,
};
use log::debug;
use write_fonts::types::GlyphId;

use crate::{
    error::Error,
    features::resolve_variable_metric,
    orchestration::{
        AnyWorkId, BeWork, Context, KernAdjustments, KernSegment, Kerning, Kerns, PairPosEntry,
        WorkId,
    },
};

/// On Linux it took ~0.01 ms per loop, try to get enough to make fan out worthwhile
/// based on empirical testing
const KERNS_PER_BLOCK: usize = 2048;

#[derive(Debug)]
struct KerningWork;

#[derive(Debug)]
struct KerningSegmentWork {
    segment: usize,
}

#[derive(Debug)]
struct KerningGatherWork;

pub fn create_kerning_work() -> Box<BeWork> {
    Box::new(KerningWork {})
}

pub fn create_kern_segment_work(kern_pairs: &Kerning) -> Vec<Box<BeWork>> {
    let segments = kern_pairs.adjustments.len().div_ceil(KERNS_PER_BLOCK);
    let mut work: Vec<Box<BeWork>> = Vec::with_capacity(segments);
    debug!(
        "Process {} kerning adjustments in {} chunks",
        kern_pairs.adjustments.len(),
        segments
    );
    for segment in 0..segments {
        work.push(Box::new(KerningSegmentWork { segment }));
    }
    work
}

pub fn create_kerns_work() -> Box<BeWork> {
    Box::new(KerningGatherWork {})
}

fn gid(glyph_order: &GlyphOrder, name: &GlyphName) -> Result<GlyphId, Error> {
    glyph_order
        .glyph_id(name)
        .map(|gid| GlyphId::new(gid as u16))
        .ok_or_else(|| Error::MissingGlyphId(name.clone()))
}

impl Work<Context, AnyWorkId, Error> for KerningWork {
    fn id(&self) -> AnyWorkId {
        WorkId::KernPairs.into()
    }

    fn read_access(&self) -> Access<AnyWorkId> {
        AccessBuilder::new()
            .variant(FeWorkId::GlyphOrder)
            .variant(FeWorkId::KerningGroups)
            .variant(FeWorkId::KerningAtLocation(NormalizedLocation::default()))
            .build()
    }

    fn write_access(&self) -> Access<AnyWorkId> {
        Access::Variant(WorkId::KernPairs.into())
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
                    .map(|name| GlyphId::new(glyph_order.glyph_id(name).unwrap_or(0) as u16))
                    .collect();
                (class_name.clone(), glyph_class)
            })
            .collect::<BTreeMap<_, _>>();

        // Add IR kerns to builder. IR kerns are split by location so put them back together again.
        // We want to iterate over (left, right), map<location: adjustment>
        let mut adjustments: HashMap<KernPair, KernAdjustments> = HashMap::new();
        ir_kerns
            .iter()
            .map(|(_, kerns)| kerns.as_ref())
            .flat_map(|kerns_at| {
                kerns_at
                    .kerns
                    .iter()
                    .map(|(pair, adjustment)| (pair, (kerns_at.location.clone(), *adjustment)))
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
        context.kern_pairs.set(Kerning {
            glyph_classes,
            adjustments,
        });
        Ok(())
    }
}

impl Work<Context, AnyWorkId, Error> for KerningSegmentWork {
    fn id(&self) -> AnyWorkId {
        WorkId::KernSegment(self.segment).into()
    }

    fn read_access(&self) -> Access<AnyWorkId> {
        AccessBuilder::new()
            .variant(FeWorkId::StaticMetadata)
            .variant(WorkId::KernPairs)
            .build()
    }

    fn exec(&self, context: &Context) -> Result<(), Error> {
        let static_metadata = context.ir.static_metadata.get();
        let arc_glyph_order = context.ir.glyph_order.get();
        let glyph_order = arc_glyph_order.as_ref();
        let kerning = context.kern_pairs.get();
        let start = self.segment * KERNS_PER_BLOCK;
        let end = (start + KERNS_PER_BLOCK).min(kerning.adjustments.len());
        assert!(start <= end, "bad range {start}..{end}");

        // now for each kerning entry, directly add a rule to a builder:
        let our_kerns = &kerning.adjustments[start..end];
        let glyph_classes = &kerning.glyph_classes;

        let mut kerns = Vec::new();
        for ((left, right), values) in our_kerns {
            let (default_value, deltas) = resolve_variable_metric(&static_metadata, values.iter())?;
            let x_adv_record = ValueRecordBuilder::new()
                .with_x_advance(default_value)
                .with_x_advance_device(deltas);
            let empty = ValueRecordBuilder::new();

            match (left, right) {
                (KernParticipant::Glyph(left), KernParticipant::Glyph(right)) => {
                    let (left, right) = (gid(glyph_order, left)?, gid(glyph_order, right)?);
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
                    let gid0 = GlyphId::new(
                        glyph_order
                            .glyph_id(left)
                            .ok_or_else(|| Error::MissingGlyphId(left.clone()))?
                            as u16,
                    );
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
                    let gid1 = gid(glyph_order, right)?;
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

        context.kern_segments.set(KernSegment {
            segment: self.segment,
            kerns,
        });

        Ok(())
    }
}

impl Work<Context, AnyWorkId, Error> for KerningGatherWork {
    fn id(&self) -> AnyWorkId {
        WorkId::Kerns.into()
    }

    fn read_access(&self) -> Access<AnyWorkId> {
        AccessBuilder::new()
            .variant(WorkId::KernPairs) // wait for pairs because kern segments don't spawn until pairs are done
            .variant(WorkId::KernSegment(0))
            .build()
    }

    fn exec(&self, context: &Context) -> Result<(), Error> {
        let segments = context.kern_segments.all();
        let mut builder = PairPosBuilder::default();

        for ppe in segments
            .iter()
            .flat_map(|(_, segment)| segment.kerns.iter())
            .cloned()
        {
            ppe.add_to(&mut builder);
        }

        let mut kerns = Kerns::default();
        if !builder.is_empty() {
            kerns.lookups = vec![builder];
        }
        context.kerns.set(kerns);

        Ok(())
    }
}
