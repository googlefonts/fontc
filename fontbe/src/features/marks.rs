//! Generates a [FeaRsMarks] datastructure to be fed to fea-rs

use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};

use fea_rs::{
    compile::{FeatureProvider, LookupId, PendingLookup},
    GlyphSet,
};
use fontdrasil::{
    orchestration::{Access, AccessBuilder, Work},
    types::GlyphName,
};

use ordered_float::OrderedFloat;
use smol_str::SmolStr;
use write_fonts::{
    read::collections::IntSet,
    tables::{
        gdef::GlyphClassDef,
        gpos::builders::{
            AnchorBuilder, CursivePosBuilder, MarkToBaseBuilder, MarkToLigBuilder,
            MarkToMarkBuilder,
        },
        layout::{builders::CaretValueBuilder, LookupFlag},
    },
    types::{GlyphId16, Tag},
};

use crate::{
    error::Error,
    orchestration::{
        AnyWorkId, BeWork, Context, FeaFirstPassOutput, FeaRsMarks, MarkLookups, WorkId,
    },
};
use fontir::{
    ir::{self, Anchor, AnchorKind, GlyphAnchors, GlyphOrder, StaticMetadata},
    orchestration::WorkId as FeWorkId,
    variations::DeltaError,
};

use super::{
    ot_tags::{INDIC_SCRIPTS, USE_SCRIPTS},
    properties::UnicodeShortName,
};

#[derive(Debug)]
struct MarkWork {}

pub fn create_mark_work() -> Box<BeWork> {
    Box::new(MarkWork {})
}

const MARK: Tag = Tag::new(b"mark");
const MKMK: Tag = Tag::new(b"mkmk");
const CURS: Tag = Tag::new(b"curs");
const ABVM: Tag = Tag::new(b"abvm");
const BLWM: Tag = Tag::new(b"blwm");
/// The canonical name shared for a given mark/base pair, e.g. `top` for `top`/`_top`
type GroupName = SmolStr;

struct MarkLookupBuilder<'a> {
    // extracted from public.openTypeCatgories/GlyphData.xml or FEA
    gdef_classes: HashMap<GlyphId16, GlyphClassDef>,
    // pruned, only the anchors we are using
    anchor_lists: BTreeMap<GlyphId16, Vec<&'a ir::Anchor>>,
    glyph_order: &'a GlyphOrder,
    static_metadata: &'a StaticMetadata,
    fea_first_pass: &'a FeaFirstPassOutput,
    // unicode names of scripts declared in FEA
    mark_glyphs: BTreeSet<GlyphId16>,
    lig_carets: BTreeMap<GlyphId16, Vec<CaretValueBuilder>>,
    char_map: HashMap<u32, GlyphId16>,
}

/// Abstract over the difference in anchor shape between mark2lig and mark2base/mark2mark
#[derive(Debug, Clone, PartialEq)]
enum BaseOrLigAnchors<T> {
    Base(T),
    Ligature(Vec<Option<T>>),
}

/// The bases and marks in a particular group, e.g. "top" or "bottom"
#[derive(Default, Debug, Clone, PartialEq)]
struct MarkGroup<'a> {
    bases: Vec<(GlyphId16, BaseOrLigAnchors<&'a ir::Anchor>)>,
    marks: Vec<(GlyphId16, &'a ir::Anchor)>,
    // if `true`, we will make a mark filter set from the marks in this group
    // (only true for mkmk)
    filter_glyphs: bool,
}

impl MarkGroup<'_> {
    //https://github.com/googlefonts/ufo2ft/blob/5a606b7884bb6da594e3cc56a169e5c3d5fa267c/Lib/ufo2ft/featureWriters/markFeatureWriter.py#L796
    fn make_filter_glyph_set(&self, filter_glyphs: &IntSet<GlyphId16>) -> Option<GlyphSet> {
        let all_marks = self
            .marks
            .iter()
            .map(|(gid, _)| *gid)
            .collect::<HashSet<_>>();
        self.filter_glyphs.then(|| {
            self.marks
                .iter()
                .filter_map(|(gid, _)| filter_glyphs.contains(*gid).then_some(*gid))
                .chain(
                    self.bases
                        .iter()
                        .filter_map(|(gid, _)| (!all_marks.contains(gid)).then_some(*gid)),
                )
                .collect()
        })
    }

    fn only_using_glyphs(&self, include: &IntSet<GlyphId16>) -> Option<MarkGroup> {
        let bases = self
            .bases
            .iter()
            .filter(|(gid, _)| include.contains(*gid))
            .map(|(gid, anchors)| (*gid, anchors.clone()))
            .collect::<Vec<_>>();
        if bases.is_empty() || self.marks.is_empty() {
            None
        } else {
            Some(MarkGroup {
                bases,
                marks: self.marks.clone(),
                filter_glyphs: self.filter_glyphs,
            })
        }
    }
}

// a trait to abstract over three very similar builders
trait MarkAttachmentBuilder: Default {
    fn add_mark(&mut self, gid: GlyphId16, group: &GroupName, anchor: AnchorBuilder);
    fn add_base(
        &mut self,
        gid: GlyphId16,
        group: &GroupName,
        anchor: BaseOrLigAnchors<AnchorBuilder>,
    );
}

impl MarkAttachmentBuilder for MarkToBaseBuilder {
    fn add_mark(&mut self, gid: GlyphId16, group: &GroupName, anchor: AnchorBuilder) {
        let _ = self.insert_mark(gid, group, anchor);
    }

    fn add_base(
        &mut self,
        gid: GlyphId16,
        group: &GroupName,
        anchor: BaseOrLigAnchors<AnchorBuilder>,
    ) {
        match anchor {
            BaseOrLigAnchors::Base(anchor) => self.insert_base(gid, group, anchor),
            BaseOrLigAnchors::Ligature(_) => panic!("lig anchors in mark2base builder"),
        }
    }
}

impl MarkAttachmentBuilder for MarkToMarkBuilder {
    fn add_mark(&mut self, gid: GlyphId16, group: &GroupName, anchor: AnchorBuilder) {
        let _ = self.insert_mark1(gid, group, anchor);
    }

    fn add_base(
        &mut self,
        gid: GlyphId16,
        group: &GroupName,
        anchor: BaseOrLigAnchors<AnchorBuilder>,
    ) {
        match anchor {
            BaseOrLigAnchors::Base(anchor) => self.insert_mark2(gid, group, anchor),
            BaseOrLigAnchors::Ligature(_) => panic!("lig anchors in mark2mark to builder"),
        }
    }
}

impl MarkAttachmentBuilder for MarkToLigBuilder {
    fn add_mark(&mut self, gid: GlyphId16, group: &GroupName, anchor: AnchorBuilder) {
        let _ = self.insert_mark(gid, group, anchor);
    }

    fn add_base(
        &mut self,
        gid: GlyphId16,
        group: &GroupName,
        anchors: BaseOrLigAnchors<AnchorBuilder>,
    ) {
        match anchors {
            BaseOrLigAnchors::Ligature(anchors) => self.insert_ligature(gid, group, anchors),
            BaseOrLigAnchors::Base(_) => panic!("base anchors passed to mark2lig builder"),
        }
    }
}

impl<'a> MarkLookupBuilder<'a> {
    fn new(
        anchors: Vec<&'a GlyphAnchors>,
        glyph_order: &'a GlyphOrder,
        static_metadata: &'a StaticMetadata,
        fea_first_pass: &'a FeaFirstPassOutput,
        char_map: HashMap<u32, GlyphId16>,
    ) -> Result<Self, Error> {
        let gdef_classes = super::get_gdef_classes(static_metadata, fea_first_pass, glyph_order);
        let lig_carets = get_ligature_carets(glyph_order, static_metadata, &anchors)?;
        // first we want to narrow our input down to only anchors that are participating.
        // in pythonland this is https://github.com/googlefonts/ufo2ft/blob/8e9e6eb66a/Lib/ufo2ft/featureWriters/markFeatureWriter.py#L380
        let mut pruned = BTreeMap::new();
        let mut base_groups = HashSet::new();
        let mut mark_groups = HashSet::new();
        let include = gdef_classes
            .iter()
            .filter_map(|(name, cls)| {
                matches!(
                    *cls,
                    GlyphClassDef::Base | GlyphClassDef::Mark | GlyphClassDef::Ligature
                )
                .then_some(name)
            })
            .collect::<HashSet<_>>();
        for anchors in anchors {
            // skip anchors for non-export glyphs
            if !glyph_order.contains(&anchors.glyph_name) {
                continue;
            }
            let gid = glyph_order.glyph_id(&anchors.glyph_name).unwrap();
            // skip glyphs that are not mark/lig/base, if we have any defined categories
            if !include.is_empty() && !include.contains(&gid) {
                continue;
            }
            for anchor in &anchors.anchors {
                match &anchor.kind {
                    AnchorKind::Base(group)
                    | AnchorKind::Ligature {
                        group_name: group, ..
                    } => {
                        base_groups.insert(group);
                    }
                    AnchorKind::Mark(group) => {
                        mark_groups.insert(group);
                    }
                    AnchorKind::ComponentMarker(_)
                    | AnchorKind::CursiveEntry
                    | AnchorKind::CursiveExit
                    | AnchorKind::Caret(_)
                    | AnchorKind::VCaret(_) => (),
                }
                pruned.entry(gid).or_insert(Vec::new()).push(anchor);
            }
        }

        let used_groups = base_groups
            .intersection(&mark_groups)
            .collect::<HashSet<_>>();
        // we don't care about marks with no corresponding bases, & vice-versa see:
        // <https://github.com/googlefonts/ufo2ft/blob/6787e37e63530/Lib/ufo2ft/featureWriters/markFeatureWriter.py#L359>
        pruned.retain(|_, anchors| {
            anchors.retain(|anchor| {
                anchor.is_cursive()
                    || anchor
                        .mark_group_name()
                        .map(|group| used_groups.contains(&group))
                        .unwrap_or_else(|| anchor.is_component_marker())
            });
            !anchors.is_empty()
        });

        let mark_glyphs = find_mark_glyphs(&pruned, &gdef_classes);
        Ok(Self {
            anchor_lists: pruned,
            glyph_order,
            static_metadata,
            fea_first_pass,
            gdef_classes,
            mark_glyphs,
            lig_carets,
            char_map,
        })
    }

    // corresponds to _makeFeatures in python
    fn build(&self) -> Result<FeaRsMarks, Error> {
        let mark_base_groups = self.make_mark_to_base_groups();
        let mark_mark_groups = self.make_mark_to_mark_groups();
        let mark_lig_groups = self.make_mark_to_liga_groups();

        let (abvm_glyphs, non_abvm_glyphs) = self.split_mark_and_abvm_blwm_glyphs()?;

        let todo = super::feature_writer_todo_list(
            &[MARK, MKMK, ABVM, BLWM, CURS],
            &self.fea_first_pass.ast,
        );

        let mut mark_mkmk = self.make_lookups(
            &mark_base_groups,
            &mark_mark_groups,
            &mark_lig_groups,
            &non_abvm_glyphs,
            |_| true,
        )?;
        if !todo.contains(&MARK) {
            mark_mkmk.mark_base.clear();
            mark_mkmk.mark_lig.clear();
        }
        if !todo.contains(&MKMK) {
            mark_mkmk.mark_mark.clear();
        }

        let curs = todo
            .contains(&CURS)
            .then(|| self.make_cursive_lookups())
            .transpose()?
            .unwrap_or_default();
        let (abvm, blwm) = if !abvm_glyphs.is_empty() {
            let abvm = todo
                .contains(&ABVM)
                .then(|| {
                    self.make_lookups(
                        &mark_base_groups,
                        &mark_mark_groups,
                        &mark_lig_groups,
                        &abvm_glyphs,
                        is_above_mark,
                    )
                })
                .transpose()?;
            let blwm = todo
                .contains(&BLWM)
                .then(|| {
                    self.make_lookups(
                        &mark_base_groups,
                        &mark_mark_groups,
                        &mark_lig_groups,
                        &abvm_glyphs,
                        is_below_mark,
                    )
                })
                .transpose()?;
            (abvm.unwrap_or_default(), blwm.unwrap_or_default())
        } else {
            Default::default()
        };

        Ok(FeaRsMarks {
            glyphmap: self.glyph_order.names().cloned().collect(),
            mark_mkmk,
            abvm,
            blwm,
            curs,
            lig_carets: self.lig_carets.clone(),
        })
    }

    fn make_lookups(
        &self,
        mark_base_groups: &BTreeMap<GroupName, MarkGroup>,
        mark_mark_groups: &BTreeMap<GroupName, MarkGroup>,
        mark_lig_groups: &BTreeMap<GroupName, MarkGroup>,
        include_glyphs: &IntSet<GlyphId16>,
        marks_filter: impl Fn(&GroupName) -> bool,
    ) -> Result<MarkLookups, Error> {
        let mark_base = self.make_lookups_type::<MarkToBaseBuilder>(
            mark_base_groups,
            include_glyphs,
            &marks_filter,
        )?;
        let mark_mark = self.make_lookups_type::<MarkToMarkBuilder>(
            mark_mark_groups,
            include_glyphs,
            &marks_filter,
        )?;
        let mark_lig = self.make_lookups_type::<MarkToLigBuilder>(
            mark_lig_groups,
            include_glyphs,
            &marks_filter,
        )?;
        Ok(MarkLookups {
            mark_base,
            mark_mark,
            mark_lig,
        })
    }

    // code shared between mark2base and mark2mark
    fn make_lookups_type<T: MarkAttachmentBuilder>(
        &self,
        groups: &BTreeMap<GroupName, MarkGroup>,
        include_glyphs: &IntSet<GlyphId16>,
        // filters based on the name of an anchor!
        marks_filter: &impl Fn(&GroupName) -> bool,
    ) -> Result<Vec<PendingLookup<T>>, Error> {
        let mut result = Vec::with_capacity(groups.len());
        for (name, group) in groups {
            if !marks_filter(name) {
                continue;
            }

            // reduce the group to only include glyphs used in this feature
            let Some(group) = group.only_using_glyphs(include_glyphs) else {
                continue;
            };

            let mut builder = T::default();
            let filter_set = group.make_filter_glyph_set(include_glyphs);
            let mut flags = LookupFlag::empty();
            if filter_set.is_some() {
                flags |= LookupFlag::USE_MARK_FILTERING_SET;
            }
            for (mark_gid, anchor) in &group.marks {
                let anchor = resolve_anchor_once(anchor, self.static_metadata)
                    .map_err(|e| self.convert_delta_error(e, *mark_gid))?;
                builder.add_mark(*mark_gid, name, anchor);
            }

            for (base_gid, anchor) in &group.bases {
                if !include_glyphs.contains(*base_gid) {
                    continue;
                }
                let anchor = resolve_anchor(anchor, self.static_metadata)
                    .map_err(|e| self.convert_delta_error(e, *base_gid))?;
                builder.add_base(*base_gid, name, anchor);
            }
            result.push(PendingLookup::new(vec![builder], flags, filter_set));
        }
        Ok(result)
    }

    fn make_mark_to_base_groups(&self) -> BTreeMap<GroupName, MarkGroup<'a>> {
        let mut groups = BTreeMap::<_, MarkGroup>::new();
        for (gid, anchors) in &self.anchor_lists {
            let is_mark = self.mark_glyphs.contains(gid);
            // if we have explicit gdef classes and this is not an expilcit base
            let is_not_base = !self.gdef_classes.is_empty()
                && (self.gdef_classes.get(gid)) != Some(&GlyphClassDef::Base);

            let treat_as_base = !(is_mark | is_not_base);
            for anchor in anchors {
                match &anchor.kind {
                    ir::AnchorKind::Base(group) if treat_as_base => groups
                        .entry(group.clone())
                        .or_default()
                        .bases
                        .push((*gid, BaseOrLigAnchors::Base(anchor))),
                    ir::AnchorKind::Mark(group) if is_mark => groups
                        .entry(group.clone())
                        .or_default()
                        .marks
                        .push((*gid, anchor)),
                    _ => continue,
                }
            }
        }
        groups
    }

    fn make_mark_to_mark_groups(&self) -> BTreeMap<GroupName, MarkGroup<'a>> {
        // first find the set of glyphs that are marks, i.e. have any mark attachment point.
        let (mark_glyphs, mark_anchors): (BTreeSet<_>, BTreeSet<_>) = self
            .anchor_lists
            .iter()
            .filter(|(name, _)| self.mark_glyphs.contains(*name))
            .flat_map(|(name, anchors)| {
                anchors.iter().filter_map(move |anchor| {
                    if let AnchorKind::Mark(group) = &anchor.kind {
                        Some((name, group))
                    } else {
                        None
                    }
                })
            })
            .unzip();

        // then iterate again, looking for glyphs that we have identified as marks,
        // but which also have participating base anchors
        let mut result = BTreeMap::<GroupName, MarkGroup>::new();
        for (gid, glyph_anchors) in &self.anchor_lists {
            if !mark_glyphs.contains(gid) {
                continue;
            }

            for anchor in glyph_anchors {
                if let AnchorKind::Base(group_name) = &anchor.kind {
                    // only if this anchor is a base, AND we have a mark in the same group
                    if mark_anchors.contains(group_name) {
                        let group = result.entry(group_name.clone()).or_default();
                        group.filter_glyphs = true;
                        group.bases.push((*gid, BaseOrLigAnchors::Base(anchor)))
                    }
                }
            }
        }

        // then add the anchors for the mark glyphs, if they exist
        for mark in mark_glyphs {
            let anchors = self.anchor_lists.get(mark).unwrap();
            for anchor in anchors {
                if let AnchorKind::Mark(group) = &anchor.kind {
                    // only add mark glyph if there is at least one base
                    if let Some(group) = result.get_mut(group) {
                        group.marks.push((*mark, anchor))
                    }
                }
            }
        }
        result
    }

    fn make_mark_to_liga_groups(&self) -> BTreeMap<GroupName, MarkGroup<'_>> {
        let mut groups = BTreeMap::<_, MarkGroup>::new();
        let mut liga_anchor_groups = HashSet::new();

        // a temporary buffer, reused for each glyph
        let mut component_groups = HashMap::new();

        // first do a pass to build up the ligature anchors and track the set
        // of mark classes used
        for (gid, anchors) in &self.anchor_lists {
            // skip anything that is definitely not a ligature glyph
            let might_be_liga = self.gdef_classes.is_empty()
                || (self.gdef_classes.get(gid) == Some(&GlyphClassDef::Ligature));

            if !might_be_liga {
                continue;
            }

            // skip any glyphs that don't have a ligature anchor
            let Some(max_index) = anchors.iter().filter_map(|a| a.ligature_index()).max() else {
                continue;
            };

            for anchor in anchors {
                let AnchorKind::Ligature { group_name, index } = &anchor.kind else {
                    continue;
                };
                liga_anchor_groups.insert(group_name);
                let component_anchors = component_groups
                    .entry(group_name.clone())
                    .or_insert_with(|| vec![None; max_index]);
                component_anchors[*index - 1] = Some(*anchor);
            }

            for (group_name, anchors) in component_groups.drain() {
                groups
                    .entry(group_name)
                    .or_default()
                    .bases
                    .push((*gid, BaseOrLigAnchors::Ligature(anchors)));
            }
        }

        // then we do another pass to add the marks in the used classes
        for (gid, anchors) in &self.anchor_lists {
            if !self.mark_glyphs.contains(gid) {
                continue;
            }
            for anchor in anchors {
                if let AnchorKind::Mark(group) = &anchor.kind {
                    if liga_anchor_groups.contains(group) {
                        groups
                            .entry(group.to_owned())
                            .or_default()
                            .marks
                            .push((*gid, anchor));
                    }
                }
            }
        }
        groups
    }

    fn make_cursive_lookups(&self) -> Result<Vec<PendingLookup<CursivePosBuilder>>, Error> {
        let mut builder = CursivePosBuilder::default();
        let flags = LookupFlag::IGNORE_MARKS | LookupFlag::RIGHT_TO_LEFT;
        let mut entries = BTreeMap::new();
        let mut affected_glyphs = BTreeSet::new();
        let mut exits = BTreeMap::new();

        for (gid, anchors) in &self.anchor_lists {
            for anchor in anchors {
                match anchor.kind {
                    AnchorKind::CursiveEntry => {
                        entries.insert(*gid, anchor);
                        affected_glyphs.insert(*gid);
                    }
                    AnchorKind::CursiveExit => {
                        exits.insert(*gid, anchor);
                        affected_glyphs.insert(*gid);
                    }
                    _ => {}
                }
            }
        }
        if affected_glyphs.is_empty() {
            return Ok(vec![]);
        }
        for gid in affected_glyphs {
            let entry_anchor = entries
                .get(&gid)
                .map(|anchor| resolve_anchor_once(anchor, self.static_metadata))
                .transpose()
                .map_err(|e| self.convert_delta_error(e, gid))?;
            let exit_anchor = exits
                .get(&gid)
                .map(|anchor| resolve_anchor_once(anchor, self.static_metadata))
                .transpose()
                .map_err(|e| self.convert_delta_error(e, gid))?;
            builder.insert(gid, entry_anchor, exit_anchor);
        }
        // In the future we might to do an LTR/RTL split, but for now return a
        // vector with one element.
        Ok(vec![PendingLookup::new(vec![builder], flags, None)])
    }

    fn convert_delta_error(&self, err: DeltaError, gid: GlyphId16) -> Error {
        let name = self.glyph_order.glyph_name(gid.into()).cloned().unwrap();
        Error::AnchorDeltaError(name, err)
    }

    //https://github.com/googlefonts/ufo2ft/blob/5a606b7884bb6da/Lib/ufo2ft/featureWriters/markFeatureWriter.py#L1119
    //
    // returns two sets: glyphs used in abvm/blwm, and glyphs used in mark
    fn split_mark_and_abvm_blwm_glyphs(
        &self,
    ) -> Result<(IntSet<GlyphId16>, IntSet<GlyphId16>), Error> {
        let scripts_using_abvm = scripts_using_abvm();
        let fea_scripts = super::get_script_language_systems(&self.fea_first_pass.ast)
            .into_keys()
            .collect::<HashSet<_>>();
        let filtered_scripts_using_abvm = (!fea_scripts.is_empty()).then(|| {
            scripts_using_abvm
                .intersection(&fea_scripts)
                .copied()
                .collect::<HashSet<_>>()
        });
        // if we had fea scripts, this is abvm scripts that are also in fea;
        // otherwise it is all abvm scripts
        let maybe_filtered = filtered_scripts_using_abvm
            .as_ref()
            .unwrap_or(&scripts_using_abvm);

        // this returns Option<bool> to replicate the ternary logic of
        // https://github.com/googlefonts/ufo2ft/blob/16ed156bd6a8b9bc/Lib/ufo2ft/util.py#L360
        let unicode_is_abvm = |uv: u32| -> Option<bool> {
            let mut saw_abvm = false;
            for script in super::properties::scripts_for_codepoint(uv) {
                if script == super::properties::COMMON_SCRIPT {
                    return None;
                }
                // attn: this uses the _filtered_ abvm scripts:
                saw_abvm |= maybe_filtered.contains(&script);
            }
            Some(saw_abvm)
        };

        // note that it's possible for a glyph to pass both these tests!
        let unicode_is_non_abvm = |uv: u32| -> Option<bool> {
            Some(
                super::properties::scripts_for_codepoint(uv)
                    // but this uses the unfiltered ones!
                    .any(|script| !scripts_using_abvm.contains(&script)),
            )
        };

        if scripts_using_abvm.is_empty()
            || !self
                .char_map
                .keys()
                .copied()
                .any(|uv| unicode_is_abvm(uv).unwrap_or(false))
        {
            // no abvm scripts: everything is a mark
            return Ok((
                Default::default(),
                self.glyph_order.iter().map(|(gid, _)| gid).collect(),
            ));
        }

        let gsub = self.fea_first_pass.gsub();
        let abvm_glyphs = super::properties::glyphs_matching_predicate(
            &self.char_map,
            unicode_is_abvm,
            gsub.as_ref(),
        )?;

        let mut non_abvm_glyphs = super::properties::glyphs_matching_predicate(
            &self.char_map,
            unicode_is_non_abvm,
            gsub.as_ref(),
        )?;
        // https://github.com/googlefonts/ufo2ft/blob/5a606b7884bb6da/Lib/ufo2ft/featureWriters/markFeatureWriter.py#L1156
        // TK: there's another bug here I think!? we can't trust char map, need
        // to union with glyph set.
        non_abvm_glyphs.extend(
            self.glyph_order
                .iter()
                .map(|(gid, _)| gid)
                .filter(|gid| !abvm_glyphs.contains(*gid)),
        );
        Ok((abvm_glyphs, non_abvm_glyphs))
    }
}

// matching current fonttools behaviour, we treat treat every non-bottom as a top:
// https://github.com/googlefonts/ufo2ft/blob/5a606b7884bb6da5/Lib/ufo2ft/featureWriters/markFeatureWriter.py#L998
fn is_above_mark(anchor_name: &GroupName) -> bool {
    !is_below_mark(anchor_name)
}

fn is_below_mark(anchor_name: &GroupName) -> bool {
    anchor_name.starts_with("bottom") || anchor_name == "nukta"
}

impl Work<Context, AnyWorkId, Error> for MarkWork {
    fn id(&self) -> AnyWorkId {
        WorkId::Marks.into()
    }

    fn read_access(&self) -> Access<AnyWorkId> {
        AccessBuilder::new()
            .variant(FeWorkId::StaticMetadata)
            .variant(FeWorkId::GlyphOrder)
            .variant(WorkId::FeaturesAst)
            .variant(FeWorkId::ALL_ANCHORS)
            .build()
    }

    /// Generate mark data structures.
    fn exec(&self, context: &Context) -> Result<(), Error> {
        let static_metadata = context.ir.static_metadata.get();
        let glyph_order = context.ir.glyph_order.get();
        let raw_anchors = context.ir.anchors.all();
        let fea_first_pass = context.fea_ast.get();

        let anchors = raw_anchors
            .iter()
            .map(|(_, anchors)| anchors.as_ref())
            .collect::<Vec<_>>();

        let glyphs = glyph_order
            .names()
            .map(|glyphname| context.ir.get_glyph(glyphname.clone()))
            .collect::<Vec<_>>();

        let char_map = glyphs
            .iter()
            .flat_map(|g| {
                let id = glyph_order.glyph_id(&g.name).unwrap();
                g.codepoints.iter().map(move |cp| (*cp, id))
            })
            .collect::<HashMap<_, _>>();

        // this code is roughly equivalent to what in pythonland happens in
        // setContext: https://github.com/googlefonts/ufo2ft/blob/8e9e6eb66a/Lib/ufo2ft/featureWriters/markFeatureWriter.py#L322
        let ctx = MarkLookupBuilder::new(
            anchors,
            &glyph_order,
            &static_metadata,
            &fea_first_pass,
            char_map,
        )?;
        let all_marks = ctx.build()?;

        context.fea_rs_marks.set(all_marks);

        Ok(())
    }
}

// in py this is set during _groupMarkGlyphsByAnchor; we try to match that logic
// https://github.com/googlefonts/ufo2ft/blob/8e9e6eb66/Lib/ufo2ft/featureWriters/markFeatureWriter.py#L412
fn find_mark_glyphs(
    anchors: &BTreeMap<GlyphId16, Vec<&Anchor>>,
    gdef_classes: &HashMap<GlyphId16, GlyphClassDef>,
) -> BTreeSet<GlyphId16> {
    anchors
        .iter()
        .filter(|(name, anchors)| {
            // if we have some classes, and this is not in the mark class:
            // not a mark glyph.
            if !gdef_classes.is_empty() && gdef_classes.get(*name) != Some(&GlyphClassDef::Mark) {
                return false;
            }
            // if we have no classes, or this is in the mark class,
            // then we just look for the presence of a mark anchor.
            anchors.iter().any(|a| a.is_mark())
        })
        .map(|(name, _)| name.to_owned())
        .collect()
}

fn resolve_anchor(
    anchor: &BaseOrLigAnchors<&ir::Anchor>,
    static_metadata: &StaticMetadata,
) -> Result<BaseOrLigAnchors<AnchorBuilder>, DeltaError> {
    match anchor {
        BaseOrLigAnchors::Base(anchor) => {
            resolve_anchor_once(anchor, static_metadata).map(BaseOrLigAnchors::Base)
        }
        BaseOrLigAnchors::Ligature(anchors) => anchors
            .iter()
            .map(|a| {
                a.map(|a| resolve_anchor_once(a, static_metadata))
                    .transpose()
            })
            .collect::<Result<_, _>>()
            .map(BaseOrLigAnchors::Ligature),
    }
}

fn resolve_anchor_once(
    anchor: &ir::Anchor,
    static_metadata: &StaticMetadata,
) -> Result<AnchorBuilder, DeltaError> {
    let (x_values, y_values): (Vec<_>, Vec<_>) = anchor
        .positions
        .iter()
        .map(|(loc, pt)| {
            (
                (loc.clone(), OrderedFloat::from(pt.x)),
                (loc.clone(), OrderedFloat::from(pt.y)),
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
    )?;
    let (y_default, y_deltas) = crate::features::resolve_variable_metric(
        static_metadata,
        y_values.iter().map(|item| (&item.0, &item.1)),
    )?;

    let mut anchor = AnchorBuilder::new(x_default, y_default);
    if x_deltas.iter().any(|v| v.1 != 0) {
        anchor = anchor.with_x_device(x_deltas);
    }
    if y_deltas.iter().any(|v| v.1 != 0) {
        anchor = anchor.with_y_device(y_deltas);
    }

    Ok(anchor)
}

// equivalent to
// <https://github.com/googlefonts/ufo2ft/blob/bb79cae53f/Lib/ufo2ft/featureWriters/gdefFeatureWriter.py#L56>
// the LigCaretList in GDEF is not universally used, but is used by at least CoreText
// and LibreOffice.
fn get_ligature_carets(
    glyph_order: &GlyphOrder,
    static_metadata: &StaticMetadata,
    anchors: &[&GlyphAnchors],
) -> Result<BTreeMap<GlyphId16, Vec<CaretValueBuilder>>, Error> {
    let mut out = BTreeMap::new();

    for glyph_anchor in anchors {
        let Some(gid) = glyph_order.glyph_id(&glyph_anchor.glyph_name) else {
            continue;
        };
        let carets = glyph_anchor
            .anchors
            .iter()
            .filter_map(|anchor| {
                make_caret_value(anchor, static_metadata, &glyph_anchor.glyph_name).transpose()
            })
            .collect::<Result<Vec<_>, _>>()?;
        if !carets.is_empty() {
            out.insert(gid, carets);
        }
    }
    Ok(out)
}

fn make_caret_value(
    anchor: &ir::Anchor,
    static_metadata: &StaticMetadata,
    glyph_name: &GlyphName,
) -> Result<Option<CaretValueBuilder>, Error> {
    if !matches!(anchor.kind, AnchorKind::Caret(_) | AnchorKind::VCaret(_)) {
        return Ok(None);
    }
    let is_vertical = matches!(anchor.kind, AnchorKind::VCaret(_));

    let values = anchor
        .positions
        .iter()
        .map(|(loc, pt)| {
            let pos = if is_vertical { pt.y } else { pt.x };
            (loc.clone(), OrderedFloat::from(pos))
        })
        .collect::<Vec<_>>();

    let (default, mut deltas) = crate::features::resolve_variable_metric(
        static_metadata,
        values.iter().map(|item| (&item.0, &item.1)),
    )
    .map_err(|err| Error::AnchorDeltaError(glyph_name.to_owned(), err))?;

    // don't bother encoding all zero deltas
    if deltas.iter().all(|d| d.1 == 0) {
        deltas.clear();
    }

    Ok(Some(CaretValueBuilder::Coordinate {
        default,
        deltas: deltas.into(),
    }))
}

impl FeatureProvider for FeaRsMarks {
    fn add_features(&self, builder: &mut fea_rs::compile::FeatureBuilder) {
        // a little helper reused for abvm/blwm
        fn add_all_lookups(
            builder: &mut fea_rs::compile::FeatureBuilder,
            lookups: &MarkLookups,
        ) -> Vec<LookupId> {
            let mut out = Vec::new();
            out.extend(
                lookups
                    .mark_base
                    .iter()
                    .map(|lk| builder.add_lookup(lk.clone())),
            );
            out.extend(
                lookups
                    .mark_lig
                    .iter()
                    .map(|lk| builder.add_lookup(lk.clone())),
            );
            out.extend(
                lookups
                    .mark_mark
                    .iter()
                    .map(|lk| builder.add_lookup(lk.clone())),
            );
            out
        }

        // add these first, matching fontmake
        let abvm_lookups = add_all_lookups(builder, &self.abvm);
        let blwm_lookups = add_all_lookups(builder, &self.blwm);

        let mut mark_lookups = Vec::new();
        let mut mkmk_lookups = Vec::new();
        let mut curs_lookups = Vec::new();

        for mark_base in self.mark_mkmk.mark_base.iter() {
            // each mark to base it's own lookup, whch differs from fontmake
            mark_lookups.push(builder.add_lookup(mark_base.clone()));
        }
        for mark_lig in self.mark_mkmk.mark_lig.iter() {
            mark_lookups.push(builder.add_lookup(mark_lig.clone()));
        }

        // If a mark has anchors that are themselves marks what we got here is a mark to mark
        for mark_mark in self.mark_mkmk.mark_mark.iter() {
            mkmk_lookups.push(builder.add_lookup(mark_mark.clone()));
        }

        for curs in self.curs.iter() {
            curs_lookups.push(builder.add_lookup(curs.clone()));
        }

        for (lookups, tag) in [
            (mark_lookups, MARK),
            (mkmk_lookups, MKMK),
            (curs_lookups, CURS),
            (abvm_lookups, ABVM),
            (blwm_lookups, BLWM),
        ] {
            if !lookups.is_empty() {
                builder.add_to_default_language_systems(tag, &lookups);
            }
        }

        if !self.lig_carets.is_empty()
            && builder
                .gdef()
                .map(|gdef| gdef.ligature_pos.is_empty())
                .unwrap_or(true)
        {
            builder.add_lig_carets(self.lig_carets.clone());
        }
    }
}

//https://github.com/googlefonts/ufo2ft/blob/16ed156bd/Lib/ufo2ft/featureWriters/markFeatureWriter.py#L338
fn scripts_using_abvm() -> HashSet<UnicodeShortName> {
    INDIC_SCRIPTS
        .iter()
        .chain(USE_SCRIPTS)
        .chain(std::iter::once(&"Khmr"))
        .filter_map(|s| UnicodeShortName::try_from_str(s).ok())
        .collect()
}

#[cfg(test)]
mod tests {
    use fea_rs::compile::Compilation;
    use fontdrasil::{
        agl,
        coords::{Coord, CoordConverter, NormalizedLocation},
        types::Axis,
    };
    use fontir::ir::{GdefCategories, NamedInstance};
    use kurbo::Point;

    use write_fonts::{
        dump_table,
        read::{
            tables::{gdef::Gdef as RGdef, gpos::Gpos as RGpos},
            FontRead,
        },
        tables::gdef::CaretValue as RawCaretValue,
    };

    use crate::features::test_helpers::{LayoutOutput, LayoutOutputBuilder};

    use super::*;

    fn char_for_glyph(name: &GlyphName) -> Option<char> {
        static MANUAL: &[(&str, char)] = &[
            ("brevecomb", '\u{0306}'),
            ("breveinvertedcomb", '\u{0311}'),
            ("macroncomb", '\u{0304}'),
            ("f_f", '\u{FB00}'),
            ("f_i", '\u{FB01}'),
            ("f_l", '\u{FB02}'),
            ("f_i_i", '\u{FB03}'),
            ("dottedCircle", '\u{25CC}'),
            ("nukta-kannada", '\u{0CBC}'),
            ("candrabindu-kannada", '\u{0C81}'),
            ("halant-kannada", '\u{0CCD}'),
            ("ka-kannada", '\u{0C95}'),
            ("taonethousand", '\u{0BF2}'),
            ("uni25CC", '\u{25CC}'),
        ];

        static UNMAPPED: &[&str] = &["ka-kannada.base", "a.alt"];

        let c = agl::char_for_agl_name(name.as_str()).or_else(|| {
            MANUAL
                .iter()
                .find_map(|(n, uv)| (name.as_str() == *n).then_some(*uv))
        });
        if c.is_none() && !UNMAPPED.iter().any(|except| *except == name.as_str()) {
            panic!("please add a manual charmap entry for '{name}'");
        }
        c
    }

    /// A helper for testing our mark generation code
    #[derive(Clone, Debug)]
    struct MarksInput<const N: usize> {
        prefer_gdef_categories_in_fea: bool,
        locations: [NormalizedLocation; N],
        anchors: BTreeMap<GlyphName, Vec<Anchor>>,
        categories: BTreeMap<GlyphName, GlyphClassDef>,
        char_map: HashMap<u32, GlyphName>,
        user_fea: &'static str,
    }

    struct AnchorBuilder<const N: usize> {
        locations: [NormalizedLocation; N],
        anchors: Vec<Anchor>,
    }

    impl Default for MarksInput<1> {
        fn default() -> Self {
            Self::new([&[0.0]])
        }
    }

    // when describing an anchor position we almost always use integers, but
    // for some tests we want to accept floats so we can verify rounding behaviour.
    // we define our own trait and impl it for only these two types so that the
    // compiler doesn't need additional type info when we use int/float literals.
    trait F32OrI16 {
        fn to_f64(self) -> f64;
    }

    impl F32OrI16 for f32 {
        fn to_f64(self) -> f64 {
            self as _
        }
    }

    impl F32OrI16 for i16 {
        fn to_f64(self) -> f64 {
            self as _
        }
    }

    impl<const N: usize> AnchorBuilder<N> {
        /// Add a new anchor, with positions defined for each of our locations
        ///
        /// The 'name' should be a raw anchor name, and should be known-good.
        /// Anchor name tests are in `fontir`.
        fn add<T: F32OrI16>(&mut self, anchor_name: &str, pos: [(T, T); N]) -> &mut Self {
            let positions = pos
                .into_iter()
                .enumerate()
                .map(|(i, pt)| {
                    (
                        self.locations[i].clone(),
                        Point::new(pt.0.to_f64(), pt.1.to_f64()),
                    )
                })
                .collect();
            let kind = AnchorKind::new(anchor_name).unwrap();
            self.anchors.push(Anchor { kind, positions });
            self
        }
    }

    impl<const N: usize> MarksInput<N> {
        /// Create test input with `N` locations
        fn new(locations: [&[f64]; N]) -> Self {
            const TAG_NAMES: [Tag; 3] = [Tag::new(b"axs1"), Tag::new(b"axs2"), Tag::new(b"axs3")];
            let locations = locations
                .iter()
                .map(|loc| {
                    loc.iter()
                        .enumerate()
                        .map(|(i, pos)| {
                            (
                                *TAG_NAMES.get(i).expect("too many axes in test"),
                                Coord::new(*pos),
                            )
                        })
                        .collect()
                })
                .collect::<Vec<_>>()
                .try_into()
                .unwrap();
            Self {
                user_fea: "languagesystem DFLT dflt;",
                locations,
                anchors: Default::default(),
                categories: Default::default(),
                char_map: Default::default(),
                prefer_gdef_categories_in_fea: false,
            }
        }

        /// Provide custom user FEA.
        ///
        /// By default we use a single 'languagesytem DFLT dflt' statement.
        fn set_user_fea(&mut self, fea: &'static str) -> &mut Self {
            self.user_fea = fea;
            self
        }

        /// Set whether or not to prefer GDEF categories defined in FEA vs in metadata
        ///
        /// this is 'true' for UFO sources and 'false' for glyphs sources; default
        /// here is false.
        fn set_prefer_fea_gdef_categories(&mut self, flag: bool) -> &mut Self {
            self.prefer_gdef_categories_in_fea = flag;
            self
        }

        /// Add a glyph with an optional GDEF category.
        ///
        /// the `anchors_fn` argument is a closure where anchors can be added
        /// to the newly added glyph.
        fn add_glyph(
            &mut self,
            name: &str,
            category: impl Into<Option<GlyphClassDef>>,
            mut anchors_fn: impl FnMut(&mut AnchorBuilder<N>),
        ) -> &mut Self {
            let name: GlyphName = name.into();
            if let Some(unic) = char_for_glyph(&name) {
                self.char_map.insert(unic as u32, name.clone());
            }

            if let Some(category) = category.into() {
                self.categories.insert(name.clone(), category);
            }
            let mut anchors = AnchorBuilder {
                locations: self.locations.clone(),
                anchors: Default::default(),
            };

            anchors_fn(&mut anchors);
            self.anchors.insert(name, anchors.anchors);
            self
        }

        fn make_layout_output(&self) -> LayoutOutput {
            let (min, default, max) = (Coord::new(-1.), Coord::new(0.0), Coord::new(1.0));
            let axes = self.locations[0]
                .axis_tags()
                .map(|tag| Axis {
                    name: tag.to_string(),
                    tag: *tag,
                    min,
                    default,
                    max,
                    hidden: false,
                    converter: CoordConverter::unmapped(min, default, max),
                    localized_names: Default::default(),
                })
                .collect();
            let named_instances = self
                .locations
                .iter()
                .enumerate()
                .map(|(i, loc)| NamedInstance {
                    name: format!("instance{i}"),
                    postscript_name: None,
                    location: loc.to_user(&axes),
                })
                .collect();
            let glyph_locations = self.locations.iter().cloned().collect();
            let categories = GdefCategories {
                categories: self.categories.clone(),
                prefer_gdef_categories_in_fea: self.prefer_gdef_categories_in_fea,
            };
            LayoutOutputBuilder::new()
                .with_axes(axes)
                .with_instances(named_instances)
                .with_locations(glyph_locations)
                .with_categories(categories)
                .with_user_fea(self.user_fea)
                .with_glyph_order(self.anchors.keys().cloned().collect())
                .build()
        }

        // you can pass in a closure and look at the builder; this is useful
        // for at least one test
        fn compile_and_inspect(&self, f: impl FnOnce(&MarkLookupBuilder)) -> Compilation {
            let layout_output = self.make_layout_output();
            let anchors = self
                .anchors
                .iter()
                .map(|(name, anchors)| GlyphAnchors::new(name.clone(), anchors.clone()))
                .collect::<Vec<_>>();
            let anchorsref = anchors.iter().collect();
            let char_map = self
                .char_map
                .iter()
                .map(|(uv, name)| (*uv, layout_output.glyph_order.glyph_id(name).unwrap()))
                .collect();

            let ctx = MarkLookupBuilder::new(
                anchorsref,
                &layout_output.glyph_order,
                &layout_output.static_metadata,
                &layout_output.first_pass_fea,
                char_map,
            )
            .unwrap();

            f(&ctx);

            let marks = ctx.build().unwrap();
            layout_output.compile(&marks)
        }

        // a thin wrapper, this is what most tests want to use
        fn compile(&self) -> Compilation {
            self.compile_and_inspect(|_| {})
        }

        /// Build the GPOS & GDEF tables and get a textual representation
        fn get_normalized_output(&mut self) -> String {
            let result = self.compile();
            // okay, so now we have some write-fonts tables; convert to read-fonts
            let Some(gpos) = result.gpos else {
                return String::new();
            };
            let gpos_bytes = write_fonts::dump_table(&gpos).unwrap();
            let gdef_bytes = result.gdef.map(|gdef| dump_table(&gdef).unwrap());
            let gpos = RGpos::read(gpos_bytes.as_slice().into()).unwrap();
            let gdef = gdef_bytes
                .as_ref()
                .map(|b| RGdef::read(b.as_slice().into()).unwrap());
            let mut buf = Vec::new();
            let names = self.anchors.keys().cloned().collect();

            // and pass these to layout normalizer
            otl_normalizer::print_gpos(&mut buf, &gpos, gdef.as_ref(), &names).unwrap();
            String::from_utf8(buf).unwrap()
        }
    }

    // does some cleanup so that we don't need to worry about indentation when comparing strings
    fn normalize_layout_repr(s: &str) -> String {
        s.trim().chars().filter(|c| *c != ' ').collect()
    }
    macro_rules! assert_eq_ignoring_ws {
        ($left:expr, $right:expr) => {
            let left = normalize_layout_repr(&$left);
            let right = normalize_layout_repr(&$right);
            pretty_assertions::assert_str_eq!(left, right)
        };
    }

    // a test font used in a bunch of python tests:
    // <https://github.com/googlefonts/ufo2ft/blob/779bbad84/tests/featureWriters/markFeatureWriter_test.py#L21>
    fn pytest_ufo() -> MarksInput<1> {
        let mut builder = MarksInput::default();
        builder
            .add_glyph("a", None, |anchors| {
                anchors.add("top", [(100, 200)]);
            })
            .add_glyph("f_i", None, |anchors| {
                anchors.add("top_1", [(100, 500)]);
                anchors.add("top_2", [(600, 500)]);
            })
            .add_glyph("acutecomb", None, |anchors| {
                anchors.add("_top", [(100, 200)]);
            })
            .add_glyph("tildecomb", None, |anchors| {
                anchors.add("_top", [(100, 200)]);
                anchors.add("top", [(100, 300)]);
            });
        builder
    }

    fn simple_test_input() -> MarksInput<1> {
        let mut out = MarksInput::default();
        out.add_glyph("A", GlyphClassDef::Base, |anchors| {
            anchors.add("top", [(100, 400)]);
        })
        .add_glyph("acutecomb", GlyphClassDef::Mark, |anchors| {
            anchors.add("_top", [(50, 50)]);
        });
        out
    }

    // sanity check that if we don't make empty lookups
    #[test]
    fn no_anchors_no_feature() {
        let out = MarksInput::default()
            .add_glyph("a", None, |_| {})
            .add_glyph("acutecomb", None, |_| {})
            .get_normalized_output();

        assert!(out.is_empty());
    }

    #[test]
    fn attach_a_mark_to_a_base() {
        let out = simple_test_input().get_normalized_output();
        assert_eq_ignoring_ws!(
            out,
            r#"
            # mark: DFLT/dflt
            # 1 MarkToBase rules
            # lookupflag LookupFlag(0)
            A @(x: 100, y: 400)
              @(x: 50, y: 50) acutecomb
            "#
        );
    }

    // https://github.com/googlefonts/ufo2ft/blob/779bbad84a/tests/featureWriters/markFeatureWriter_test.py#L611
    #[test]
    fn mark_mkmk_features() {
        let out = pytest_ufo().get_normalized_output();
        assert_eq_ignoring_ws!(
            out,
            r#"
            # mark: DFLT/dflt
            # 1 MarkToBase rules
            # lookupflag LookupFlag(0)
            a @(x: 100, y: 200)
              @(x: 100, y: 200) [acutecomb, tildecomb]
            # 1 MarkToLig rules
            # lookupflag LookupFlag(0)
            f_i (lig) [@(x: 100, y: 500), @(x: 600, y: 500)]
              @(x: 100, y: 200) [acutecomb, tildecomb]

            # mkmk: DFLT/dflt
            # 1 MarkToMark rules
            # lookupflag LookupFlag(16)
            # filter glyphs: [acutecomb, tildecomb]
            tildecomb @(x: 100, y: 300)
              @(x: 100, y: 200) [acutecomb, tildecomb]
            "#
        );
    }

    // reduced from testdata/glyphs3/Oswald-AE-comb
    //
    // this ends up mostly being a test about how we generate mark filtering sets..
    #[test]
    fn oswald_ae_test_case() {
        let mut input = MarksInput::default();
        let out = input
            .add_glyph("A", None, |anchors| {
                anchors
                    .add("bottom", [(234, 0)])
                    .add("ogonek", [(411, 0)])
                    .add("top", [(234, 810)]);
            })
            .add_glyph("E", None, |anchors| {
                anchors
                    .add("topleft", [(20, 810)])
                    .add("bottom", [(216, 0)])
                    .add("ogonek", [(314, 0)])
                    .add("top", [(217, 810)]);
            })
            .add_glyph("acutecomb", None, |anchors| {
                anchors.add("top", [(0, 810)]).add("_top", [(0, 578)]);
            })
            .add_glyph("brevecomb", None, |anchors| {
                anchors.add("top", [(0, 810)]).add("_top", [(0, 578)]);
            })
            .add_glyph("tildecomb", None, |anchors| {
                anchors.add("top", [(0, 742)]).add("_top", [(0, 578)]);
            })
            .add_glyph("macroncomb", None, |anchors| {
                anchors.add("top", [(0, 810)]).add("_top", [(0, 578)]);
            })
            .add_glyph("breveinvertedcomb", None, |anchors| {
                anchors
                    .add("top", [(0, 810)])
                    .add("top.a", [(0, 810)])
                    .add("_top.a", [(0, 578)]);
            })
            .get_normalized_output();

        assert_eq_ignoring_ws!(
            out,
            // this gets an abvm feature because we don't specify any languagesystems
            r#"
            # abvm: DFLT/dflt
            # 2 MarkToMark rules
            # lookupflag LookupFlag(16)
            # filter glyphs: [acutecomb,macroncomb]
            acutecomb @(x: 0, y: 810)
              @(x: 0, y: 578) [acutecomb,brevecomb,macroncomb,tildecomb]
            macroncomb @(x: 0, y: 810)
              @(x: 0, y: 578) [acutecomb,brevecomb,macroncomb,tildecomb]

            # mark: DFLT/dflt
            # 2 MarkToBase rules
            # lookupflag LookupFlag(0)
            A @(x: 234, y: 810)
              @(x: 0, y: 578) [acutecomb,brevecomb,macroncomb,tildecomb]
            E @(x: 217, y: 810)
              @(x: 0, y: 578) [acutecomb,brevecomb,macroncomb,tildecomb]

            # mkmk: DFLT/dflt
            # 6 MarkToMark rules
            # lookupflag LookupFlag(16)
            # filter glyphs: [acutecomb,brevecomb,breveinvertedcomb,macroncomb,tildecomb]
            acutecomb @(x: 0, y: 810)
              @(x: 0, y: 578) [acutecomb,brevecomb,macroncomb,tildecomb]
            brevecomb @(x: 0, y: 810)
              @(x: 0, y: 578) [acutecomb,brevecomb,macroncomb,tildecomb]
            breveinvertedcomb @(x: 0, y: 810)
              @(x: 0, y: 578) [acutecomb,brevecomb,macroncomb,tildecomb]
            # filter glyphs: [breveinvertedcomb]
            breveinvertedcomb @(x: 0, y: 810)
              @(x: 0, y: 578) breveinvertedcomb
            # filter glyphs: [acutecomb,brevecomb,breveinvertedcomb,macroncomb,tildecomb]
            macroncomb @(x: 0, y: 810)
              @(x: 0, y: 578) [acutecomb,brevecomb,macroncomb,tildecomb]
            tildecomb @(x: 0, y: 742)
              @(x: 0, y: 578) [acutecomb,brevecomb,macroncomb,tildecomb]

            "#
        );
    }

    #[test]
    fn abvm_blwm_features() {
        let mut input = MarksInput::default();
        let out = input
            .add_glyph("dottedCircle", None, |anchors| {
                anchors
                    .add("top", [(297, 552)])
                    .add("topright", [(491, 458)])
                    .add("bottom", [(297, 0)]);
            })
            .add_glyph("nukta-kannada", None, |anchors| {
                anchors.add("_bottom", [(0, 0)]);
            })
            .add_glyph("candrabindu-kannada", None, |anchors| {
                anchors.add("_top", [(0, 547)]);
            })
            .add_glyph("halant-kannada", None, |anchors| {
                anchors.add("_topright", [(-456, 460)]);
            })
            .add_glyph("ka-kannada", None, |anchors| {
                anchors.add("bottom", [(290, 0)]);
            })
            .add_glyph("ka-kannada.base", None, |anchors| {
                anchors
                    .add("top", [(291, 547)])
                    .add("topright", [(391, 460)])
                    .add("bottom", [(290, 0)]);
            })
            .set_user_fea(
                "
            languagesystem DFLT dflt;
            languagesystem knda dflt;
            languagesystem knd2 dflt;

            feature psts {
                sub ka-kannada' halant-kannada by ka-kannada.base;
            } psts;",
            )
            .get_normalized_output();

        assert_eq_ignoring_ws!(
            out,
            r#"
                # abvm: DFLT/dflt, knd2/dflt, knda/dflt
                # 2 MarkToBase rules
                # lookupflag LookupFlag(0)
                ka-kannada.base @(x: 291, y: 547)
                  @(x: 0, y: 547) candrabindu-kannada
                ka-kannada.base @(x: 391, y: 460)
                  @(x: -456, y: 460) halant-kannada

                # blwm: DFLT/dflt, knd2/dflt, knda/dflt
                # 2 MarkToBase rules
                # lookupflag LookupFlag(0)
                ka-kannada @(x: 290, y: 0)
                  @(x: 0, y: 0) nukta-kannada
                ka-kannada.base @(x: 290, y: 0)
                  @(x: 0, y: 0) nukta-kannada

                # mark: DFLT/dflt, knd2/dflt,knda/dflt
                # 3 MarkToBase rules
                # lookupflag LookupFlag(0)
                dottedCircle @(x: 297, y: 0)
                  @(x: 0, y: 0) nukta-kannada
                dottedCircle @(x: 297, y: 552)
                  @(x: 0, y: 547) candrabindu-kannada
                dottedCircle @(x: 491, y: 458)
                  @(x: -456, y: 460) halant-kannada

                "#
        );
    }

    #[test]
    fn include_unmapped_glyph_with_no_abvm() {
        // make sure that we are including all glyphs (even those only reachable
        // via GSUB closure) in the case where we do not have any abvm glyphs
        let mut out = MarksInput::default();
        let out = out
            // a.alt is only reachable via substitution
            .set_user_fea(
                "languagesystem latn dflt;
        feature test {
            sub a by a.alt;
        } test;",
            )
            .add_glyph("a", None, |anchors| {
                anchors.add("top", [(100, 0)]);
            })
            .add_glyph("a.alt", None, |anchors| {
                anchors.add("top", [(110, 0)]);
            })
            .add_glyph("acutecomb", None, |anchors| {
                anchors.add("_top", [(202, 0)]);
            })
            .get_normalized_output();

        assert_eq_ignoring_ws!(
            out,
            r#"
            # mark: latn/dflt
            # 2 MarkToBase rules
            # lookupflag LookupFlag(0)
            a @(x: 100, y: 0)
              @(x: 202, y: 0) acutecomb
            a.alt @(x: 110, y: 0)
              @(x: 202, y: 0) acutecomb
          "#
        );
    }

    #[test]
    fn include_unmapped_non_abvm_glyph_with_abvm() {
        // make sure that we are including all glyphs non-abvm glyphs (even
        // those only reachable via GSUB closure) in the case where we have
        // both abvm & non-abvm glyphs
        let mut out = MarksInput::default();
        out
            // a.alt is only reachable via substitution
            .set_user_fea(
                "languagesystem latn dflt;
                languagesystem knda dflt;
        ",
            )
            // this glyph is unreachable
            .add_glyph("ka-kannada.base", None, |_| {})
            .add_glyph("nukta-kannada", None, |_| {})
            .compile_and_inspect(|ctx| {
                let (abvm, non_abvm) = ctx.split_mark_and_abvm_blwm_glyphs().unwrap();
                let nukta = ctx.glyph_order.glyph_id("nukta-kannada").unwrap();
                let ka = ctx.glyph_order.glyph_id("ka-kannada.base").unwrap();
                assert!(abvm.contains(nukta));
                // all unreachable glyphs get stuffed into non-abvm
                // (although maybe this can change in the future, and we
                // can just drop them?)
                assert!(non_abvm.contains(ka));
            });
    }
    #[test]
    fn custom_fea() {
        let out = simple_test_input()
            .set_user_fea("languagesystem latn dflt;")
            .get_normalized_output();
        assert_eq_ignoring_ws!(
            out,
            r#"
            # mark: latn/dflt
            # 1 MarkToBase rules
            # lookupflag LookupFlag(0)
            A @(x: 100, y: 400)
              @(x: 50, y: 50) acutecomb
            "#
        );
    }

    // shared between two tests below
    fn gdef_test_input() -> MarksInput<1> {
        let mut out = simple_test_input();
        out.set_user_fea(
            r#"
            @Bases = [A];
            @Marks = [acutecomb];
            table GDEF {
                GlyphClassDef @Bases, [], @Marks,;
            } GDEF;
            "#,
        )
        // is not in the FEA classes defined above
        .add_glyph("gravecomb", GlyphClassDef::Mark, |anchors| {
            anchors.add("_top", [(5, 15)]);
        });
        out
    }

    #[test]
    fn prefer_fea_gdef_categories_true() {
        let out = gdef_test_input()
            .set_prefer_fea_gdef_categories(true)
            .get_normalized_output();
        assert_eq_ignoring_ws!(
            out,
            r#"
            # mark: DFLT/dflt
            # 1 MarkToBase rules
            # lookupflag LookupFlag(0)
            A @(x: 100, y: 400)
              @(x: 50, y: 50) acutecomb
            "#
        );
    }

    #[test]
    fn prefer_fea_gdef_categories_false() {
        let out = gdef_test_input()
            .set_prefer_fea_gdef_categories(false)
            .get_normalized_output();
        assert_eq_ignoring_ws!(
            out,
            r#"
            # mark: DFLT/dflt
            # 1 MarkToBase rules
            # lookupflag LookupFlag(0)
            A @(x: 100, y: 400)
              @(x: 5, y: 15) gravecomb
              @(x: 50, y: 50) acutecomb
            "#
        );
    }

    // reproduces a real failure, where we would incorrectly classify this glyph
    // as both abvm & not-abvm.
    #[test]
    fn non_abvm_glyphs_use_unfiltered_scripts() {
        let _ = MarksInput::default()
            .set_user_fea(
                r#"
                languagesystem DFLT dflt;
                languagesystem sinh dflt;
                languagesystem taml dflt;
                "#,
            )
            .add_glyph("taonethousand", None, |_| {})
            .compile_and_inspect(|builder| {
                let taonethousand = builder.glyph_order.glyph_id("taonethousand").unwrap();
                let (abvm, non_abvm) = builder.split_mark_and_abvm_blwm_glyphs().unwrap();
                assert!(abvm.contains(taonethousand));
                assert!(!non_abvm.contains(taonethousand));
            });
    }

    #[test]
    fn glyph_in_abvm_but_not_if_no_abvm_lang() {
        let dotbelowcomb_char = char_for_glyph(&GlyphName::new("dotbelowcomb")).unwrap();
        let dotbelow_scripts =
            super::super::properties::scripts_for_codepoint(dotbelowcomb_char as _)
                .collect::<Vec<_>>();
        let abvm_scripts = super::scripts_using_abvm();

        // this codepoint is used in both abvm and non-abvm scripts
        assert!(dotbelow_scripts.iter().any(|sc| abvm_scripts.contains(sc)));
        assert!(dotbelow_scripts.iter().any(|sc| !abvm_scripts.contains(sc)));

        let _ = MarksInput::default()
            // but if the file contains only non-abvm scripts,
            .set_user_fea(
                r#"
                languagesystem DFLT dflt;
                languagesystem latn dflt;
                "#,
            )
            .add_glyph("dotbelowcomb", None, |_| {})
            .compile_and_inspect(|builder| {
                let dotbelowcomb = builder.glyph_order.glyph_id("dotbelowcomb").unwrap();
                let (abvm, non_abvm) = builder.split_mark_and_abvm_blwm_glyphs().unwrap();
                // it should only be in the non-abvm set.
                assert!(!abvm.contains(dotbelowcomb));
                assert!(non_abvm.contains(dotbelowcomb));
            });
    }

    #[test]
    fn abvm_closure_excludes_glyphs_with_common_script() {
        let uni25cc = char_for_glyph(&GlyphName::new("uni25CC")).unwrap();
        assert_eq!(
            super::super::properties::scripts_for_codepoint(uni25cc as _).collect::<Vec<_>>(),
            [super::super::properties::COMMON_SCRIPT]
        );
        let _ = MarksInput::default()
            .set_user_fea(
                "languagesystem latn dflt;
                languagesystem knda dflt;

                feature derp {
                    sub ka-kannada by uni25CC;
                } derp;
        ",
            )
            .add_glyph("ka-kannada", None, |_| {})
            .add_glyph("uni25CC", None, |_| {})
            .compile_and_inspect(|ctx| {
                let (abvm, non_abvm) = ctx.split_mark_and_abvm_blwm_glyphs().unwrap();
                // even though this is reachable by substitution from an abvm glyph,
                // we don't want it to go in abvm
                let uni25cc = ctx.glyph_order.glyph_id("uni25CC").unwrap();
                let ka = ctx.glyph_order.glyph_id("ka-kannada").unwrap();
                assert!(!abvm.contains(uni25cc));
                assert!(abvm.contains(ka));
                assert!(non_abvm.contains(uni25cc));
            });
    }

    //https://github.com/googlefonts/ufo2ft/blob/779bbad84a/tests/featureWriters/markFeatureWriter_test.py#L1438
    #[test]
    fn multiple_anchor_classes_base() {
        let out = MarksInput::default()
            .add_glyph("a", None, |anchors| {
                anchors.add("topA", [(515, 581)]);
            })
            .add_glyph("e", None, |anchors| {
                anchors.add("topE", [(-21, 396)]);
            })
            .add_glyph("acutecomb", None, |anchors| {
                anchors
                    .add("_topA", [(-175, 589)])
                    .add("_topE", [(-175, 572)]);
            })
            .get_normalized_output();

        assert_eq_ignoring_ws!(
            out,
            r#"
                # mark: DFLT/dflt
                # 2 MarkToBase rules
                # lookupflag LookupFlag(0)
                a @(x: 515, y: 581)
                  @(x: -175, y: 589) acutecomb
                e @(x: -21, y: 396)
                  @(x: -175, y: 572) acutecomb
                "#
        );
    }

    // https://github.com/googlefonts/ufo2ft/blob/779bbad84a/tests/featureWriters/markFeatureWriter_test.py#L175
    #[test]
    fn ligature_null_anchor() {
        let out = MarksInput::default()
            .add_glyph("f_i_i", None, |anchors| {
                anchors
                    .add("top_1", [(250, 600)])
                    .add("top_2", [(500, 600)])
                    .add("_3", [(0, 0)]);
            })
            .add_glyph("acutecomb", None, |anchors| {
                anchors.add("_top", [(100, 200)]);
            })
            .get_normalized_output();

        assert_eq_ignoring_ws!(
            out,
            r#"
    # mark: DFLT/dflt
    # 1 MarkToLig rules
    # lookupflag LookupFlag(0)
    f_i_i (lig) [@(x: 250, y: 600), @(x: 500, y: 600), <NULL>]
    @(x: 100, y: 200) acutecomb
    "#
        );
    }

    // https://github.com/googlefonts/ufo2ft/blob/779bbad84a/tests/featureWriters/markFeatureWriter_test.py#L1543
    #[test]
    fn multiple_anchor_classes_liga() {
        let out = MarksInput::default()
            .add_glyph("f_i", None, |anchors| {
                anchors
                    .add("top_1", [(100, 500)])
                    .add("top_2", [(600, 500)]);
            })
            .add_glyph("f_f", None, |anchors| {
                anchors
                    .add("topOther_1", [(101, 501)])
                    .add("topOther_2", [(601, 501)]);
            })
            .add_glyph("f_l", None, |anchors| {
                anchors
                    .add("top_1", [(102, 502)])
                    .add("topOther_2", [(602, 502)]);
            })
            .add_glyph("acutecomb", None, |anchors| {
                anchors.add("_top", [(100, 200)]);
                anchors.add("_topOther", [(150, 250)]);
            })
            .get_normalized_output();

        // we generate conflicting lookups for f_l, but topOther should win
        // because we order lookups lexicographically over the mark group names
        assert_eq_ignoring_ws!(
            out,
            r#"
                # mark: DFLT/dflt
                # 3 MarkToLig rules
                # lookupflag LookupFlag(0)
                f_f (lig) [@(x: 101, y: 501), @(x: 601, y: 501)]
                  @(x: 150, y: 250) acutecomb
                f_i (lig) [@(x: 100, y: 500), @(x: 600, y: 500)]
                  @(x: 100, y: 200) acutecomb
                f_l (lig) [<NULL>, @(x: 602, y: 502)]
                  @(x: 150, y: 250) acutecomb
                "#
        );
    }

    #[test]
    fn basic_lig_carets() {
        let out = MarksInput::default()
            .add_glyph("f_i", None, |anchors| {
                anchors.add("caret_1", [(100, 0)]);
            })
            .add_glyph("f_l", None, |anchors| {
                anchors.add("vcaret_1", [(0, -222)]);
            })
            .compile();

        let gdef = out.gdef.as_ref().unwrap();
        let lig_carets = gdef.lig_caret_list.as_ref().unwrap();
        let ff = &lig_carets.lig_glyphs[0].caret_values;
        assert_eq!(ff.len(), 1);
        let caret = ff[0].as_ref();
        assert!(
            matches!(caret, RawCaretValue::Format1(t) if t.coordinate == 100),
            "{caret:?}"
        );

        let f_l = &lig_carets.lig_glyphs[1].caret_values;
        assert_eq!(f_l.len(), 1);
        let caret = f_l[0].as_ref();
        assert!(
            matches!(caret, RawCaretValue::Format1(t) if t.coordinate == -222),
            "{caret:?}"
        );
    }

    #[test]
    fn lig_caret_nop_deltas() {
        use write_fonts::read::tables::gdef as rgdef;
        // these values are taken from Oswald.glyphs
        let out = MarksInput::new([&[-1.0], &[0.0], &[1.0]])
            .add_glyph("f_f", None, |anchors| {
                anchors.add("caret_1", [(10.0, 0.0), (10., 0.0), (10., 0.)]);
            })
            .compile();

        let gdef_bytes = write_fonts::dump_table(&out.gdef.unwrap()).unwrap();
        let gdef = rgdef::Gdef::read(gdef_bytes.as_slice().into()).unwrap();
        let lig_carets = gdef.lig_caret_list().unwrap().unwrap();
        let ff = lig_carets.lig_glyphs().get(0).unwrap();
        let rgdef::CaretValue::Format1(caret) = ff.caret_values().get(0).unwrap() else {
            panic!("wrong caret format");
        };
        assert_eq!(caret.coordinate(), 10)
    }

    #[test]
    fn lig_caret_rounding() {
        use write_fonts::read::tables::gdef as rgdef;
        // these values are taken from Oswald.glyphs
        let out = MarksInput::new([&[-1.0], &[0.0], &[1.0]])
            .add_glyph("f_f", None, |anchors| {
                anchors.add("caret_1", [(239.0, 0.0), (270.5, 0.0), (304.5, 0.)]);
            })
            .compile();

        // we don't have good API on the write-fonts varstore for fetching the
        // delta values, so we convert to read fonts first
        let gdef_bytes = write_fonts::dump_table(&out.gdef.unwrap()).unwrap();
        let gdef = rgdef::Gdef::read(gdef_bytes.as_slice().into()).unwrap();
        let lig_carets = gdef.lig_caret_list().unwrap().unwrap();
        let varstore = gdef.item_var_store().unwrap().unwrap();
        let ivs_data = varstore.item_variation_data().get(0).unwrap().unwrap();
        let ff = lig_carets.lig_glyphs().get(0).unwrap();
        let caret = ff.caret_values().get(0).unwrap();
        let rgdef::CaretValue::Format3(caret) = caret else {
            panic!("expected variable caret!");
        };
        let default = caret.coordinate();
        let mut values = ivs_data
            .delta_set(0)
            .map(|delta| default + (delta as i16))
            .collect::<Vec<_>>();
        values.insert(1, default);

        assert_eq!(values, [239, 271, 305]);
    }

    #[test]
    fn mark_anchors_rounding() {
        // These values are taken from Teachers-Italic.glyphs. The composite glyph
        // 'ordfeminine' inherits a 'top' base anchor from a scaled 'a' component, so the
        // propagated anchor ends up with float coordinates; these are expected to be
        // rounded before deltas get computed in order to match the output of fontmake:
        // https://github.com/googlefonts/fontc/issues/1043#issuecomment-2444388789
        let out = MarksInput::new([&[0.0], &[0.75], &[1.0]])
            .add_glyph("a", None, |anchors| {
                anchors.add("top", [(377.0, 451.0), (386.0, 450.0), (389.0, 450.0)]);
            })
            .add_glyph("ordfeminine", None, |anchors| {
                anchors.add("top", [(282.75, 691.25), (289.5, 689.5), (291.75, 689.5)]);
            })
            .add_glyph("acutecomb", None, |anchors| {
                anchors.add("_top", [(218.0, 704.0), (265.0, 707.0), (282.0, 708.0)]);
            })
            .get_normalized_output();

        assert_eq_ignoring_ws!(
            out,
            r#"
                # mark: DFLT/dflt
                # 2 MarkToBase rules
                # lookupflag LookupFlag(0)
                a @(x: 377 {386,389}, y: 451 {450,450})
                @(x: 218 {265,282}, y: 704 {707,708}) acutecomb
                ordfeminine @(x: 283 {290,292}, y: 691 {690,690})
                @(x: 218 {265,282}, y: 704 {707,708}) acutecomb
            "#
        );
    }
}
