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
    error::{BadAnchorName, Error},
    orchestration::{AnyWorkId, BeWork, Context, FeaRsMarks, MarkGroup, WorkId},
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

/// The canonical name shared for a given mark/base pair, e.g. `top` for `top`/`_top`
type MarkGroupName = SmolStr;

/// The type of a mark anchor, used when generating mark features
#[derive(Clone, Debug, PartialEq, PartialOrd, Eq, Ord)]
pub(crate) enum AnchorInfo {
    Base(MarkGroupName),
    Mark(MarkGroupName),
    Ligature {
        group_name: MarkGroupName,
        index: usize,
    },
}

impl AnchorInfo {
    // this logic from
    // <https://github.com/googlefonts/ufo2ft/blob/6787e37e6/Lib/ufo2ft/featureWriters/markFeatureWriter.py#L101>
    pub(crate) fn new(name: impl AsRef<str>) -> Result<AnchorInfo, BadAnchorName> {
        let name = name.as_ref();
        // _ prefix means mark. This convention appears to come from FontLab and is now everywhere.
        if let Some(mark) = name.strip_prefix('_') {
            if mark.is_empty() {
                return Err(BadAnchorName::NilMarkGroup);
            }
            if let Some((_, suffix)) = mark.rsplit_once('_') {
                if suffix.parse::<usize>().is_ok() {
                    return Err(BadAnchorName::NumberedMarkAnchor);
                }
            }
            return Ok(AnchorInfo::Mark(mark.into()));
        } else if let Some((name, suffix)) = name.rsplit_once('_') {
            // _1 suffix means a base in a ligature glyph
            if let Ok(index) = suffix.parse::<usize>() {
                if index == 0 {
                    return Err(BadAnchorName::ZeroIndex);
                } else {
                    return Ok(AnchorInfo::Ligature {
                        group_name: name.into(),
                        index,
                    });
                }
            }
        }
        Ok(AnchorInfo::Base(name.into()))
    }

    pub(crate) fn group_name(&self) -> MarkGroupName {
        match self {
            AnchorInfo::Base(group_name)
            | AnchorInfo::Mark(group_name)
            | AnchorInfo::Ligature { group_name, .. } => group_name.clone(),
        }
    }

    pub(crate) fn is_mark(&self) -> bool {
        matches!(self, AnchorInfo::Mark(_))
    }
}

fn create_mark_to_base_groups(
    anchors: &BTreeMap<GlyphName, Arc<GlyphAnchors>>,
    glyph_order: &GlyphOrder,
) -> Result<BTreeMap<MarkGroupName, MarkGroup>, Error> {
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
            let anchor_info =
                AnchorInfo::new(&anchor.name).map_err(|reason| Error::InvalidAnchorName {
                    glyph: glyph_name.clone(),
                    anchor: anchor.name.clone(),
                    reason,
                })?;
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
                let anchor_info =
                    AnchorInfo::new(&anchor.name).map_err(|reason| Error::InvalidAnchorName {
                        glyph: glyph_name.clone(),
                        anchor: anchor.name.clone(),
                        reason,
                    })?;
                groups
                    .entry(anchor_info.group_name())
                    .or_default()
                    .bases
                    .push((glyph_name.clone(), anchor.clone()));
            }
        }
    }
    Ok(groups)
}

fn create_mark_to_mark_groups(
    anchors: &BTreeMap<GlyphName, Arc<GlyphAnchors>>,
    glyph_order: &GlyphOrder,
) -> Result<BTreeMap<MarkGroupName, MarkGroup>, Error> {
    // first find the set of glyphs that are marks, i.e. have any mark attachment point.
    let (mut mark_glyphs, mut mark_anchors): (BTreeSet<_>, BTreeSet<_>) = Default::default();

    for (name, anchor) in anchors
        .iter()
        .filter(|(name, anchors)| glyph_order.contains(name) && contains_marks(anchors))
        .flat_map(|(name, anchors)| {
            anchors.anchors.iter().map(|a| {
                (
                    name.clone(),
                    AnchorInfo::new(&a.name).map_err(|reason| Error::InvalidAnchorName {
                        glyph: name.clone(),
                        anchor: a.name.clone(),
                        reason,
                    }),
                )
            })
        })
    {
        let anchor = anchor?;
        if anchor.is_mark() {
            mark_glyphs.insert(name.clone());
            mark_anchors.insert(anchor.group_name());
        }
    }

    // then iterate again, looking for glyphs that we have identified as marks,
    // but which also have participating base anchors
    let mut result = BTreeMap::<MarkGroupName, MarkGroup>::new();
    for (glyph, glyph_anchors) in anchors {
        if !mark_glyphs.contains(glyph) {
            continue;
        }

        for anchor in &glyph_anchors.anchors {
            let info =
                AnchorInfo::new(&anchor.name).map_err(|reason| Error::InvalidAnchorName {
                    glyph: glyph.clone(),
                    anchor: anchor.name.clone(),
                    reason,
                })?;
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
            let info =
                AnchorInfo::new(&anchor.name).map_err(|reason| Error::InvalidAnchorName {
                    glyph: mark.clone(),
                    anchor: anchor.name.clone(),
                    reason,
                })?;
            let group = info.group_name();
            if !info.is_mark() {
                continue;
            }
            if let Some(group) = result.get_mut(&group) {
                group.marks.push((mark.clone(), anchor.clone()));
            }
        }
    }
    Ok(result)
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
                .ok_or_else(|| Error::MissingGlyphId(name.clone()))
        };

        let mut anchors = BTreeMap::new();
        for (work_id, arc_glyph_anchors) in raw_anchors {
            let FeWorkId::Anchor(glyph_name) = work_id else {
                return Err(Error::ExpectedAnchor(work_id.clone()));
            };
            anchors.insert(glyph_name.clone(), arc_glyph_anchors.clone());
        }

        let groups = create_mark_to_base_groups(&anchors, &glyph_order)?;

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

        let mkmk_groups = create_mark_to_mark_groups(&anchors, &glyph_order)?;
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

#[cfg(test)]
mod tests {
    use super::*;

    // from
    // <https://github.com/googlefonts/ufo2ft/blob/6787e37e6/tests/featureWriters/markFeatureWriter_test.py#L34>
    #[test]
    fn anchor_names() {
        assert_eq!(AnchorInfo::new("top"), Ok(AnchorInfo::Base("top".into())));
        assert_eq!(AnchorInfo::new("top_"), Ok(AnchorInfo::Base("top_".into())));
        assert_eq!(AnchorInfo::new("top1"), Ok(AnchorInfo::Base("top1".into())));
        assert_eq!(
            AnchorInfo::new("_bottom"),
            Ok(AnchorInfo::Mark("bottom".into()))
        );
        assert_eq!(
            AnchorInfo::new("bottom_2"),
            Ok(AnchorInfo::Ligature {
                group_name: "bottom".into(),
                index: 2
            })
        );
        assert_eq!(
            AnchorInfo::new("top_right_1"),
            Ok(AnchorInfo::Ligature {
                group_name: "top_right".into(),
                index: 1
            })
        );
    }

    // from
    // <https://github.com/googlefonts/ufo2ft/blob/6787e37e6/tests/featureWriters/markFeatureWriter_test.py#L50-L59>
    #[test]
    fn bad_anchor_names() {
        assert_eq!(
            AnchorInfo::new("_top_2"),
            Err(BadAnchorName::NumberedMarkAnchor)
        );
        assert_eq!(AnchorInfo::new("_"), Err(BadAnchorName::NilMarkGroup));
        assert_eq!(AnchorInfo::new("top_0"), Err(BadAnchorName::ZeroIndex));
    }
}
