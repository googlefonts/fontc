//! Generates a [Marks] datastructure to be fed to fea-rs

use std::{
    collections::{BTreeMap, BTreeSet},
    sync::Arc,
};

use fontdrasil::{
    orchestration::{Access, Work},
    types::GlyphName,
};

use write_fonts::types::GlyphId;

use crate::{
    error::Error,
    orchestration::{
        AnyWorkId, BeWork, Context, MarkBase, MarkEntry, MarkGroup, MarkGroupName, MarkMark, Marks,
        WorkId,
    },
};
use fontir::{
    ir::{GlyphAnchors, GlyphOrder},
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
        Access::Custom(Arc::new(|id| {
            matches!(
                id,
                AnyWorkId::Fe(FeWorkId::StaticMetadata)
                    | AnyWorkId::Fe(FeWorkId::GlyphOrder)
                    | AnyWorkId::Fe(FeWorkId::Anchor(..))
            )
        }))
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
                let mut mark_base = MarkBase::new(group_name.0.clone());

                for (mark_name, mark_anchor) in group.marks.iter() {
                    mark_base.insert_mark(MarkEntry::new(
                        &static_metadata,
                        gid(mark_name)?,
                        mark_anchor,
                    )?);
                }

                for (base_name, base_anchor) in group.bases.iter() {
                    mark_base.insert_base(MarkEntry::new(
                        &static_metadata,
                        gid(base_name)?,
                        base_anchor,
                    )?);
                }

                all_marks.mark_base.push(mark_base);
            }
        }

        let mkmk_groups = create_mark_to_mark_groups(&anchors, &glyph_order);
        for (group_name, MarkGroup { bases, marks }) in &mkmk_groups {
            if bases.is_empty() || marks.is_empty() {
                continue;
            }

            let mut mark_mark = MarkMark::new(group_name.0.clone());
            for (mark_name, mark_anchor) in marks {
                mark_mark.insert_attaching_mark(MarkEntry::new(
                    &static_metadata,
                    gid(mark_name)?,
                    mark_anchor,
                )?);
            }

            for (base_name, base_anchor) in bases {
                mark_mark.insert_base_mark(MarkEntry::new(
                    &static_metadata,
                    gid(base_name)?,
                    base_anchor,
                )?);
            }
            all_marks.mark_mark.push(mark_mark);
        }

        context.marks.set(all_marks);

        Ok(())
    }
}
