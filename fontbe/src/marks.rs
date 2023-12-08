//! Generates a [Marks] datastructure to be fed to fea-rs

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
use write_fonts::types::GlyphId;

use crate::{
    error::Error,
    orchestration::{AnyWorkId, BeWork, Context, MarkGroup, MarkGroupName, Marks, WorkId},
};
use fontir::{
    ir::{GlyphAnchors, GlyphOrder, StaticMetadata},
    orchestration::WorkId as FeWorkId,
};

#[derive(Debug)]
struct MarkWork {}

pub fn create_mark_work() -> Box<BeWork> {
    Box::new(MarkWork {})
}

/// The type of an anchor, used when generating mark features
#[derive(Clone, Debug, PartialEq, PartialOrd, Eq, Ord)]
pub(crate) enum AnchorInfo {
    Base(MarkGroupName),
    Mark(MarkGroupName),
}

impl AnchorInfo {
    pub(crate) fn new(name: &GlyphName) -> AnchorInfo {
        // _ prefix means mark. This convention appears to come from FontLab and is now everywhere.
        if name.as_str().starts_with('_') {
            AnchorInfo::Mark(MarkGroupName(name.as_str()[1..].into()))
        } else {
            AnchorInfo::Base(MarkGroupName(name.as_str().into()))
        }
    }

    pub(crate) fn group_name(&self) -> MarkGroupName {
        match self {
            AnchorInfo::Base(group_name) | AnchorInfo::Mark(group_name) => group_name.clone(),
        }
    }

    pub(crate) fn is_mark(&self) -> bool {
        matches!(self, AnchorInfo::Mark(_))
    }
}

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
        let mut base = true; // TODO: only a base if user rules allow it
        for anchor in glyph_anchors.anchors.iter() {
            let anchor_info = AnchorInfo::new(&anchor.name);
            if anchor_info.is_mark() {
                base = false;

                // TODO: only if user rules allow us to be a mark
                groups
                    .entry(anchor_info.group_name())
                    .or_default()
                    .marks
                    .push((glyph_name.clone(), anchor.clone()));
            }
        }

        if base {
            for anchor in glyph_anchors.anchors.iter() {
                let anchor_info = AnchorInfo::new(&anchor.name);
                groups
                    .entry(anchor_info.group_name())
                    .or_default()
                    .bases
                    .push((glyph_name.clone(), anchor.clone()));
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
        .filter(|(name, anchors)| glyph_order.contains(name) && contains_marks(anchors))
        .flat_map(|(name, anchors)| {
            anchors
                .anchors
                .iter()
                .map(|a| AnchorInfo::new(&a.name))
                .filter_map(|info| info.is_mark().then(|| (name.clone(), info.group_name())))
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
            let info = AnchorInfo::new(&anchor.name);
            // only if this anchor is a base, AND we have a mark in the same group
            if !info.is_mark() && mark_anchors.contains(&info.group_name()) {
                result
                    .entry(info.group_name())
                    .or_default()
                    .bases
                    .push((glyph_anchors.glyph_name.clone(), anchor.clone()));
            }
        }
    }

    for mark in mark_glyphs {
        let anchors = anchors.get(&mark).unwrap();
        for anchor in &anchors.anchors {
            let info = AnchorInfo::new(&anchor.name);
            let group = info.group_name();
            if !info.is_mark() {
                continue;
            }
            if let Some(group) = result.get_mut(&group) {
                group.marks.push((mark.clone(), anchor.clone()));
            }
        }
    }
    result
}

fn contains_marks(anchors: &GlyphAnchors) -> bool {
    anchors
        .anchors
        .iter()
        .any(|a| a.name.as_str().starts_with('_'))
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
                .map(|gid| GlyphId::new(gid as u16))
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

        let mut all_marks = Marks {
            glyphmap: glyph_order
                .iter()
                .cloned()
                .map(GlyphName::into_inner)
                .collect(),
            ..Default::default()
        };

        for (group_name, group) in groups.iter() {
            // if we have bases *and* marks produce mark to base
            if !group.bases.is_empty() && !group.marks.is_empty() {
                let mut mark_base = MarkToBaseBuilder::default();

                for (mark_name, mark_anchor) in group.marks.iter() {
                    let gid = gid(mark_name)?;
                    let anchor = resolve_anchor(mark_anchor, &static_metadata)?;
                    mark_base
                        .insert_mark(gid, group_name.0.clone(), anchor)
                        .map_err(|err| map_prev_class_error(err, &group_name.0, mark_name))?;
                }

                for (base_name, base_anchor) in group.bases.iter() {
                    let gid = gid(base_name)?;
                    let anchor = resolve_anchor(base_anchor, &static_metadata)?;
                    mark_base.insert_base(gid, &group_name.0, anchor);
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
                let anchor = resolve_anchor(mark_anchor, &static_metadata)?;
                mark_mark
                    .insert_mark1(gid(mark_name)?, group_name.0.clone(), anchor)
                    .map_err(|err| map_prev_class_error(err, &group_name.0, mark_name))?;
            }

            for (base_name, base_anchor) in bases {
                let anchor = resolve_anchor(base_anchor, &static_metadata)?;
                mark_mark.insert_mark2(gid(base_name)?, &group_name.0, anchor);
            }
            all_marks.mark_mark.push(mark_mark);
        }

        context.marks.set(all_marks);

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

    let (x_default, x_deltas) =
        crate::features::resolve_variable_metric(static_metadata, x_values)?;
    let (y_default, y_deltas) =
        crate::features::resolve_variable_metric(static_metadata, y_values)?;
    Ok(fea_rs::compile::Anchor::new(x_default, y_default)
        .with_x_device(x_deltas)
        .with_y_device(y_deltas))
}
