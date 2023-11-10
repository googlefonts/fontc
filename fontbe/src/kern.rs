//! Generates a [Kerning] datastructure to be fed to fea-rs

use std::collections::{BTreeMap, HashSet};

use fea_rs::{compile::ValueRecord as ValueRecordBuilder, GlyphSet};
use fontdrasil::orchestration::{Access, AllOrOne, Work};
use fontir::{ir::KernParticipant, orchestration::WorkId as FeWorkId};
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
        Access::Set(HashSet::from([
            AllOrOne::All(AnyWorkId::Fe(FeWorkId::StaticMetadata)),
            AllOrOne::All(AnyWorkId::Fe(FeWorkId::Kerning)),
            AllOrOne::All(AnyWorkId::Fe(FeWorkId::GlyphOrder)),
        ]))
    }

    /// Generate kerning data structures.
    fn exec(&self, context: &Context) -> Result<(), Error> {
        let static_metadata = context.ir.static_metadata.get();
        let glyph_order = context.ir.glyph_order.get();
        let ir_kerning = context.ir.kerning.get();
        let gid = |name| {
            glyph_order
                .glyph_id(name)
                .map(|gid| GlyphId::new(gid as u16))
                .ok_or_else(|| Error::MissingGlyphId(name.clone()))
        };

        // convert the groups stored in the Kerning object into the glyph classes
        // expected by fea-rs:
        let glyph_classes = ir_kerning
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

        let mut kerning = Kerning::default();

        // now for each kerning entry, directly add a rule to the builder:
        for ((left, right), values) in &ir_kerning.kerns {
            let (default_value, deltas) = resolve_variable_metric(&static_metadata, values)?;
            let x_adv_record = ValueRecordBuilder::new()
                .with_x_advance(default_value)
                .with_x_advance_device(deltas);

            match (left, right) {
                (KernParticipant::Glyph(left), KernParticipant::Glyph(right)) => {
                    kerning.add_pair(gid(left)?, x_adv_record.clone(), gid(right)?);
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
                    kerning.add_class(left, x_adv_record.clone(), right);
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
                        kerning.add_pair(gid0, x_adv_record.clone(), gid1);
                    }
                }
                (KernParticipant::Group(left), KernParticipant::Glyph(right)) => {
                    let left = glyph_classes
                        .get(left)
                        .ok_or_else(|| Error::MissingGlyphId(left.clone()))?;
                    let gid1 = gid(right)?;
                    for gid0 in left.iter() {
                        kerning.add_pair(gid0, x_adv_record.clone(), gid1);
                    }
                }
            }
        }

        context.kerning.set(kerning);

        Ok(())
    }
}
