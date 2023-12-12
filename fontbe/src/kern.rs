//! Generates a [Kerning] datastructure to be fed to fea-rs

use std::collections::{BTreeMap, HashMap};

use fea_rs::{
    compile::{PairPosBuilder, ValueRecord as ValueRecordBuilder},
    GlyphSet,
};
use fontdrasil::{
    coords::NormalizedLocation,
    orchestration::{Access, AccessBuilder, Work},
};
use fontir::{
    ir::{KernPair, KernParticipant},
    orchestration::WorkId as FeWorkId,
};
use ordered_float::OrderedFloat;
use write_fonts::types::GlyphId;

use crate::{
    error::Error,
    features::resolve_variable_metric,
    orchestration::{AnyWorkId, BeWork, Context, Kerning, WorkId},
};

#[derive(Debug)]
struct KerningWork {}

pub fn create_kerning_work() -> Box<BeWork> {
    Box::new(KerningWork {})
}

impl Work<Context, AnyWorkId, Error> for KerningWork {
    fn id(&self) -> AnyWorkId {
        WorkId::Kerning.into()
    }

    fn read_access(&self) -> Access<AnyWorkId> {
        AccessBuilder::new()
            .variant(FeWorkId::StaticMetadata)
            .variant(FeWorkId::GlyphOrder)
            .variant(FeWorkId::KerningGroups)
            .variant(FeWorkId::KerningAtLocation(NormalizedLocation::default()))
            .build()
    }

    /// Generate kerning data structures.
    fn exec(&self, context: &Context) -> Result<(), Error> {
        let static_metadata = context.ir.static_metadata.get();
        let glyph_order = context.ir.glyph_order.get();
        let ir_groups = context.ir.kerning_groups.get();
        let ir_kerns = context.ir.kerning_at.all();

        let gid = |name| {
            glyph_order
                .glyph_id(name)
                .map(|gid| GlyphId::new(gid as u16))
                .ok_or_else(|| Error::MissingGlyphId(name.clone()))
        };

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
                (class_name, glyph_class)
            })
            .collect::<BTreeMap<_, _>>();

        let mut builder = PairPosBuilder::default();

        // Add IR kerns to builder. IR kerns are split by location so put them back together again.
        // We want to iterate over (left, right), map<location: adjustment>
        let mut kerns: HashMap<&KernPair, Vec<(NormalizedLocation, OrderedFloat<f32>)>> =
            HashMap::new();
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
                kerns.entry(pair).or_default().push((location, adjustment))
            });

        // now for each kerning entry, directly add a rule to the builder:
        for ((left, right), values) in kerns {
            let (default_value, deltas) = resolve_variable_metric(&static_metadata, values.iter())?;
            let x_adv_record = ValueRecordBuilder::new()
                .with_x_advance(default_value)
                .with_x_advance_device(deltas);
            let empty = ValueRecordBuilder::new();

            match (left, right) {
                (KernParticipant::Glyph(left), KernParticipant::Glyph(right)) => {
                    let (left, right) = (gid(left)?, gid(right)?);
                    builder.insert_pair(left, x_adv_record.clone(), right, empty.clone());
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
                    builder.insert_classes(left, x_adv_record.clone(), right, empty.clone());
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
                        .get(&right)
                        .ok_or_else(|| Error::MissingGlyphId(right.clone()))?;
                    for gid1 in right.iter() {
                        builder.insert_pair(gid0, x_adv_record.clone(), gid1, empty.clone());
                    }
                }
                (KernParticipant::Group(left), KernParticipant::Glyph(right)) => {
                    let left = glyph_classes
                        .get(left)
                        .ok_or_else(|| Error::MissingGlyphId(left.clone()))?;
                    let gid1 = gid(right)?;
                    for gid0 in left.iter() {
                        builder.insert_pair(gid0, x_adv_record.clone(), gid1, empty.clone());
                    }
                }
            }
        }

        let mut kerning = Kerning::default();
        if !builder.is_empty() {
            kerning.lookups = vec![builder];
        }
        context.kerning.set(kerning);

        Ok(())
    }
}
