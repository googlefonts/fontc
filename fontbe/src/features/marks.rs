//! Generates a [FeaRsMarks] datastructure to be fed to fea-rs

use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};

use fea_rs::compile::{MarkToBaseBuilder, MarkToMarkBuilder};
use fontdrasil::{
    orchestration::{Access, AccessBuilder, Work},
    types::GlyphName,
};

use ordered_float::OrderedFloat;
use smol_str::SmolStr;
use write_fonts::{
    tables::gdef::GlyphClassDef,
    types::{GlyphId, Tag},
};

use crate::{
    error::Error,
    orchestration::{AnyWorkId, BeWork, Context, FeaRsMarks, WorkId},
};
use fontir::{
    ir::{self, AnchorKind, GlyphAnchors, GlyphOrder, StaticMetadata},
    orchestration::WorkId as FeWorkId,
};

#[derive(Debug)]
struct MarkWork {}

pub fn create_mark_work() -> Box<BeWork> {
    Box::new(MarkWork {})
}

/// The canonical name shared for a given mark/base pair, e.g. `top` for `top`/`_top`
type MarkGroupName = SmolStr;

#[allow(dead_code)] // a few currently unused fields
struct MarkLookupBuilder<'a> {
    gdef_classes: Option<HashMap<GlyphId, GlyphClassDef>>,
    // pruned, only the anchors we are using
    anchors: BTreeMap<GlyphName, Vec<&'a ir::Anchor>>,
    glyph_order: &'a GlyphOrder,
    static_metadata: &'a StaticMetadata,
    fea_scripts: HashSet<Tag>,
}

/// The bases and marks in a particular group, e.g. "top" or "bottom"
#[derive(Default, Clone, PartialEq)]
struct MarkGroup<'a> {
    bases: Vec<(GlyphName, &'a ir::Anchor)>,
    marks: Vec<(GlyphName, &'a ir::Anchor)>,
}

// a trait to abstract over two very similar builders
trait MarkAttachmentBuilder: Default {
    fn add_mark(&mut self, gid: GlyphId, group: &MarkGroupName, anchor: fea_rs::compile::Anchor);
    fn add_base(&mut self, gid: GlyphId, group: &MarkGroupName, anchor: fea_rs::compile::Anchor);
}

impl MarkAttachmentBuilder for MarkToBaseBuilder {
    fn add_mark(&mut self, gid: GlyphId, group: &MarkGroupName, anchor: fea_rs::compile::Anchor) {
        //FIXME: precheck groups to ensure no overlap
        let _ = self.insert_mark(gid, group.clone(), anchor);
    }

    fn add_base(&mut self, gid: GlyphId, group: &MarkGroupName, anchor: fea_rs::compile::Anchor) {
        self.insert_base(gid, group, anchor)
    }
}

impl MarkAttachmentBuilder for MarkToMarkBuilder {
    fn add_mark(&mut self, gid: GlyphId, group: &MarkGroupName, anchor: fea_rs::compile::Anchor) {
        //FIXME: precheck groups to ensure no overlap
        let _ = self.insert_mark1(gid, group.clone(), anchor);
    }

    fn add_base(&mut self, gid: GlyphId, group: &MarkGroupName, anchor: fea_rs::compile::Anchor) {
        self.insert_mark2(gid, group, anchor)
    }
}

impl<'a> MarkLookupBuilder<'a> {
    fn new(
        anchors: Vec<&'a GlyphAnchors>,
        glyph_order: &'a GlyphOrder,
        gdef_classes: Option<HashMap<GlyphId, GlyphClassDef>>,
        static_metadata: &'a StaticMetadata,
    ) -> Self {
        // first we want to narrow our input down to only anchors that are participating.
        let mut pruned = BTreeMap::new();
        let mut base_names = HashSet::new();
        let mut mark_names = HashSet::new();
        for anchors in anchors {
            // skip anchors for non-export glyphs
            if !glyph_order.contains(&anchors.glyph_name) {
                continue;
            }
            for anchor in &anchors.anchors {
                match &anchor.kind {
                    AnchorKind::Base(group) => {
                        base_names.insert(group);
                    }
                    AnchorKind::Mark(group) => {
                        mark_names.insert(group);
                    }
                    //TODO: ligatures
                    // skip non base/mark anchors
                    _ => continue,
                }
                pruned
                    .entry(anchors.glyph_name.clone())
                    .or_insert(Vec::new())
                    .push(anchor);
            }
        }

        let used_groups = base_names.intersection(&mark_names).collect::<HashSet<_>>();
        // we don't care about marks with no corresponding bases, & vice-versa see:
        // <https://github.com/googlefonts/ufo2ft/blob/6787e37e63530/Lib/ufo2ft/featureWriters/markFeatureWriter.py#L359>
        pruned.retain(|_, anchors| {
            anchors.retain(|anchor| {
                anchor
                    .mark_group_name()
                    .map(|group| used_groups.contains(&group))
                    .unwrap_or(false)
            });
            !anchors.is_empty()
        });
        Self {
            gdef_classes,
            anchors: pruned,
            glyph_order,
            fea_scripts: Default::default(),
            static_metadata,
        }
    }

    fn build(&self) -> Result<FeaRsMarks, Error> {
        let mark_base_groups = self.make_mark_to_base_groups();
        let mark_mark_groups = self.make_mark_to_mark_groups();

        let mark_base = self.make_lookups::<MarkToBaseBuilder>(mark_base_groups)?;
        let mark_mark = self.make_lookups::<MarkToMarkBuilder>(mark_mark_groups)?;
        Ok(FeaRsMarks {
            glyphmap: self.glyph_order.iter().cloned().collect(),
            mark_base,
            mark_mark,
        })
    }

    fn make_lookups<T: MarkAttachmentBuilder>(
        &self,
        groups: BTreeMap<MarkGroupName, MarkGroup>,
    ) -> Result<Vec<T>, Error> {
        groups
            .into_iter()
            .map(|(group_name, group)| {
                assert!(
                    !group.bases.is_empty() && !group.marks.is_empty(),
                    "prechecked"
                );

                let mut builder = T::default();
                for (mark_name, anchor) in group.marks {
                    // we already filtered to only things in glyph order
                    let gid = self.glyph_order.glyph_id(&mark_name).unwrap();
                    let anchor = resolve_anchor(anchor, self.static_metadata, &mark_name)?;
                    builder.add_mark(gid, &group_name, anchor);
                }

                for (base_name, anchor) in group.bases {
                    let gid = self.glyph_order.glyph_id(&base_name).unwrap();
                    let anchor = resolve_anchor(anchor, self.static_metadata, &base_name)?;
                    builder.add_base(gid, &group_name, anchor);
                }

                Ok(builder)
            })
            .collect()
    }

    fn make_mark_to_base_groups(&self) -> BTreeMap<MarkGroupName, MarkGroup<'a>> {
        let mut groups = BTreeMap::<_, MarkGroup>::new();
        for (glyph_name, anchors) in &self.anchors {
            for anchor in anchors {
                match &anchor.kind {
                    fontir::ir::AnchorKind::Base(group) => groups
                        .entry(group.clone())
                        .or_default()
                        .bases
                        .push((glyph_name.clone(), anchor)),
                    fontir::ir::AnchorKind::Mark(group) => groups
                        .entry(group.clone())
                        .or_default()
                        .marks
                        .push((glyph_name.clone(), anchor)),
                    _ => continue,
                }
            }
        }
        groups
    }

    fn make_mark_to_mark_groups(&self) -> BTreeMap<MarkGroupName, MarkGroup<'a>> {
        // first find the set of glyphs that are marks, i.e. have any mark attachment point.
        let (mark_glyphs, mark_anchors): (BTreeSet<_>, BTreeSet<_>) = self
            .anchors
            .iter()
            .flat_map(|(name, anchors)| {
                anchors.iter().filter_map(|anchor| {
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
        for (glyph, glyph_anchors) in &self.anchors {
            if !mark_glyphs.contains(glyph) {
                continue;
            }

            for anchor in glyph_anchors {
                if let AnchorKind::Base(group) = &anchor.kind {
                    // only if this anchor is a base, AND we have a mark in the same group
                    if mark_anchors.contains(group) {
                        result
                            .entry(group.clone())
                            .or_default()
                            .bases
                            .push((glyph.clone(), anchor))
                    }
                }
            }
        }

        // then add the anchors for the mark glyphs, if they exist
        for mark in mark_glyphs {
            let anchors = self.anchors.get(&mark).unwrap();
            for anchor in anchors {
                if let AnchorKind::Mark(group) = &anchor.kind {
                    // only add mark glyph if there is at least one base
                    if let Some(group) = result.get_mut(group) {
                        group.marks.push((mark.clone(), anchor))
                    }
                }
            }
        }
        result
    }
}

impl Work<Context, AnyWorkId, Error> for MarkWork {
    fn id(&self) -> AnyWorkId {
        WorkId::Marks.into()
    }

    fn read_access(&self) -> Access<AnyWorkId> {
        AccessBuilder::new()
            .variant(FeWorkId::StaticMetadata)
            .variant(FeWorkId::GlyphOrder)
            .variant(FeWorkId::ALL_ANCHORS)
            .build()
    }

    /// Generate mark data structures.
    fn exec(&self, context: &Context) -> Result<(), Error> {
        let static_metadata = context.ir.static_metadata.get();
        let glyph_order = context.ir.glyph_order.get();
        let raw_anchors = context.ir.anchors.all();

        let anchors = raw_anchors
            .iter()
            .map(|(_, anchors)| anchors.as_ref())
            .collect::<Vec<_>>();

        let ctx = MarkLookupBuilder::new(anchors, &glyph_order, None, &static_metadata);
        let all_marks = ctx.build()?;

        context.fea_rs_marks.set(all_marks);

        Ok(())
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
