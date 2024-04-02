//! Generates a [FeaRsMarks] datastructure to be fed to fea-rs

use std::{
    collections::{BTreeMap, BTreeSet},
    sync::Arc,
};

use fea_rs::compile::{MarkToBaseBuilder, MarkToMarkBuilder, PreviouslyAssignedClass};
use fontdrasil::{
    orchestration::{Access, AccessBuilder, Work},
    types::GlyphName,
};

use ordered_float::OrderedFloat;
use smol_str::SmolStr;

use crate::{
    error::Error,
    orchestration::{AnyWorkId, BeWork, Context, FeaRsMarks, MarkGroup, WorkId},
};
use fontir::{
    ir::{AnchorKind, GlyphAnchors, GlyphOrder, StaticMetadata},
    orchestration::WorkId as FeWorkId,
};

#[derive(Debug)]
struct MarkWork {}

pub fn create_mark_work() -> Box<BeWork> {
    Box::new(MarkWork {})
}

/// The canonical name shared for a given mark/base pair, e.g. `top` for `top`/`_top`
type MarkGroupName = SmolStr;

fn create_mark_to_base_groups(
    anchors: &BTreeMap<GlyphName, Arc<GlyphAnchors>>,
    glyph_order: &GlyphOrder,
) -> BTreeMap<MarkGroupName, MarkGroup> {
    let mut groups: BTreeMap<MarkGroupName, MarkGroup> = Default::default();
    for (glyph_name, glyph_anchors) in anchors.iter() {
        // We assume the anchor list to be small
        // considering only glyphs with anchors,
        //  - glyphs with *only* base anchors are bases
        //  - glyphs with *any* mark anchor are marks
        // We ignore glyphs missing from the glyph order (e.g. no-export glyphs)
        if !glyph_order.contains(glyph_name) {
            continue;
        }
        for anchor in glyph_anchors.anchors.iter() {
            match &anchor.kind {
                fontir::ir::AnchorKind::Base(group) => groups
                    .entry(group.clone())
                    .or_default()
                    .bases
                    .push((glyph_name.clone(), anchor.clone())),
                fontir::ir::AnchorKind::Mark(group) => groups
                    .entry(group.clone())
                    .or_default()
                    .marks
                    .push((glyph_name.clone(), anchor.clone())),
                _ => continue,
            }
        }
    }
    groups
}

fn create_mark_to_mark_groups(
    anchors: &BTreeMap<GlyphName, Arc<GlyphAnchors>>,
    glyph_order: &GlyphOrder,
) -> BTreeMap<MarkGroupName, MarkGroup> {
    // first find the set of glyphs that are marks, i.e. have any mark attachment point.
    let (mark_glyphs, mark_anchors): (BTreeSet<_>, BTreeSet<_>) = anchors
        .iter()
        .filter(|(name, anchors)| glyph_order.contains(name) && anchors.contains_marks())
        .flat_map(|(name, anchors)| {
            anchors.anchors.iter().filter_map(|anchor| {
                if let AnchorKind::Mark(group) = &anchor.kind {
                    Some((name.clone(), group.clone()))
                } else {
                    None
                }
            })
        })
        .unzip();

    // then iterate again, looking for glyphs that we have identified as marks,
    // but which also have participating base anchors
    let mut result = BTreeMap::<MarkGroupName, MarkGroup>::new();
    for (glyph, glyph_anchors) in anchors {
        if !mark_glyphs.contains(glyph) {
            continue;
        }

        for anchor in &glyph_anchors.anchors {
            if let AnchorKind::Base(group) = &anchor.kind {
                // only if this anchor is a base, AND we have a mark in the same group
                if mark_anchors.contains(group) {
                    assert_eq!(glyph, &glyph_anchors.glyph_name);
                    result
                        .entry(group.clone())
                        .or_default()
                        .bases
                        .push((glyph.clone(), anchor.clone()))
                }
            }
        }
    }

    // then add the anchors for the mark glyphs, if they exist
    for mark in mark_glyphs {
        let anchors = anchors.get(&mark).unwrap();
        for anchor in &anchors.anchors {
            if let AnchorKind::Mark(group) = &anchor.kind {
                // only add mark glyph if there is at least one base
                if let Some(group) = result.get_mut(group) {
                    group.marks.push((mark.clone(), anchor.clone()))
                }
            }
        }
    }
    result
}

impl Work<Context, AnyWorkId, Error> for MarkWork {
    fn id(&self) -> AnyWorkId {
        WorkId::Marks.into()
    }

    fn read_access(&self) -> Access<AnyWorkId> {
        AccessBuilder::new()
            .variant(FeWorkId::StaticMetadata)
            .variant(FeWorkId::GlyphOrder)
            .variant(FeWorkId::Anchor(GlyphName::NOTDEF))
            .build()
    }

    /// Generate mark data structures.
    fn exec(&self, context: &Context) -> Result<(), Error> {
        let static_metadata = context.ir.static_metadata.get();
        let glyph_order = context.ir.glyph_order.get();
        let raw_anchors = context.ir.anchors.all();
        let gid = |name| {
            glyph_order
                .glyph_id(name)
                .ok_or_else(|| Error::MissingGlyphId(name.clone()))
        };

        let mut anchors = BTreeMap::new();
        for (work_id, arc_glyph_anchors) in raw_anchors {
            let FeWorkId::Anchor(glyph_name) = work_id else {
                return Err(Error::ExpectedAnchor(work_id.clone()));
            };
            anchors.insert(glyph_name.clone(), arc_glyph_anchors.clone());
        }

        let groups = create_mark_to_base_groups(&anchors, &glyph_order);

        let mut all_marks = FeaRsMarks {
            glyphmap: glyph_order.iter().cloned().collect(),
            ..Default::default()
        };

        for (group_name, group) in groups.iter() {
            // if we have bases *and* marks produce mark to base
            if !group.bases.is_empty() && !group.marks.is_empty() {
                let mut mark_base = MarkToBaseBuilder::default();

                for (mark_name, mark_anchor) in group.marks.iter() {
                    let gid = gid(mark_name)?;
                    let anchor = resolve_anchor(mark_anchor, &static_metadata, mark_name)?;
                    mark_base
                        .insert_mark(gid, group_name.clone(), anchor)
                        .map_err(|err| map_prev_class_error(err, group_name, mark_name))?;
                }

                for (base_name, base_anchor) in group.bases.iter() {
                    let gid = gid(base_name)?;
                    let anchor = resolve_anchor(base_anchor, &static_metadata, base_name)?;
                    mark_base.insert_base(gid, group_name, anchor);
                }

                all_marks.mark_base.push(mark_base);
            }
        }

        let mkmk_groups = create_mark_to_mark_groups(&anchors, &glyph_order);
        for (group_name, MarkGroup { bases, marks }) in &mkmk_groups {
            if bases.is_empty() || marks.is_empty() {
                continue;
            }

            let mut mark_mark = MarkToMarkBuilder::default();
            for (mark_name, mark_anchor) in marks {
                let anchor = resolve_anchor(mark_anchor, &static_metadata, mark_name)?;
                mark_mark
                    .insert_mark1(gid(mark_name)?, group_name.clone(), anchor)
                    .map_err(|err| map_prev_class_error(err, group_name, mark_name))?;
            }

            for (base_name, base_anchor) in bases {
                let anchor = resolve_anchor(base_anchor, &static_metadata, base_name)?;
                mark_mark.insert_mark2(gid(base_name)?, group_name, anchor);
            }
            all_marks.mark_mark.push(mark_mark);
        }

        context.fea_rs_marks.set(all_marks);

        Ok(())
    }
}

fn map_prev_class_error(err: PreviouslyAssignedClass, class: &SmolStr, glyph: &GlyphName) -> Error {
    Error::PreviouslyAssignedMarkClass {
        old_class: err.class,
        new_class: class.to_owned(),
        glyph: glyph.to_owned(),
    }
}

fn resolve_anchor(
    anchor: &fontir::ir::Anchor,
    static_metadata: &StaticMetadata,
    glyph_name: &GlyphName, // just used for error reporting
) -> Result<fea_rs::compile::Anchor, Error> {
    let (x_values, y_values): (Vec<_>, Vec<_>) = anchor
        .positions
        .iter()
        .map(|(loc, pt)| {
            (
                (loc.clone(), OrderedFloat::from(pt.x as f32)),
                (loc.clone(), OrderedFloat::from(pt.y as f32)),
            )
        })
        .unzip();

    let (x_default, x_deltas) = crate::features::resolve_variable_metric(
        static_metadata,
        // If I do map(|(pos, value)| (pos, value)) to destructure the tuple and
        // convert &(T, V) => (&T, &V) as the values parameter expects, then
        // clippy complains about the seemingly no-op identity map:
        // https://rust-lang.github.io/rust-clippy/master/index.html#/map_identity
        x_values.iter().map(|item| (&item.0, &item.1)),
    )
    .map_err(|err| Error::AnchorDeltaError(glyph_name.to_owned(), err))?;
    let (y_default, y_deltas) = crate::features::resolve_variable_metric(
        static_metadata,
        y_values.iter().map(|item| (&item.0, &item.1)),
    )
    .map_err(|err| Error::AnchorDeltaError(glyph_name.to_owned(), err))?;

    let mut anchor = fea_rs::compile::Anchor::new(x_default, y_default);
    if x_deltas.iter().any(|v| v.1 != 0) {
        anchor = anchor.with_x_device(x_deltas);
    }
    if y_deltas.iter().any(|v| v.1 != 0) {
        anchor = anchor.with_y_device(y_deltas);
    }

    Ok(anchor)
}
