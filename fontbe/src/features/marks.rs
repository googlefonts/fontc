//! Generates a [FeaRsMarks] datastructure to be fed to fea-rs

use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};

use fea_rs::{
    compile::{
        Anchor as FeaAnchor, CaretValue, CursivePosBuilder, FeatureProvider, MarkToBaseBuilder,
        MarkToLigBuilder, MarkToMarkBuilder, NopFeatureProvider, NopVariationInfo, PendingLookup,
    },
    typed::{AstNode, LanguageSystem},
    GlyphSet, Opts, ParseTree,
};
use fontdrasil::{
    orchestration::{Access, AccessBuilder, Work},
    types::GlyphName,
};

use ordered_float::OrderedFloat;
use smol_str::SmolStr;
use write_fonts::{
    tables::{gdef::GlyphClassDef, layout::LookupFlag},
    types::{GlyphId16, Tag},
};

use crate::{
    error::Error,
    orchestration::{AnyWorkId, BeWork, Context, FeaRsMarks, WorkId},
};
use fontir::{
    ir::{self, Anchor, AnchorKind, GlyphAnchors, GlyphOrder, StaticMetadata},
    orchestration::WorkId as FeWorkId,
};

use super::DFLT_SCRIPT;

#[derive(Debug)]
struct MarkWork {}

pub fn create_mark_work() -> Box<BeWork> {
    Box::new(MarkWork {})
}

/// The canonical name shared for a given mark/base pair, e.g. `top` for `top`/`_top`
type GroupName = SmolStr;

struct MarkLookupBuilder<'a> {
    // extracted from public.openTypeCatgories/GlyphData.xml or FEA
    gdef_classes: BTreeMap<GlyphName, GlyphClassDef>,
    // pruned, only the anchors we are using
    anchor_lists: BTreeMap<GlyphName, Vec<&'a ir::Anchor>>,
    glyph_order: &'a GlyphOrder,
    static_metadata: &'a StaticMetadata,
    // we don't currently use this, because just adding lookups to all scripts works?
    _fea_scripts: HashSet<Tag>,
    mark_glyphs: BTreeSet<GlyphName>,
    lig_carets: BTreeMap<GlyphId16, Vec<CaretValue>>,
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
    bases: Vec<(GlyphName, BaseOrLigAnchors<&'a ir::Anchor>)>,
    marks: Vec<(GlyphName, &'a ir::Anchor)>,
    // if `true`, we will make a mark filter set from the marks in this group
    // (only true for mkmk)
    filter_glyphs: bool,
}

impl MarkGroup<'_> {
    fn make_filter_glyph_set(&self, glyph_order: &GlyphOrder) -> Option<GlyphSet> {
        self.filter_glyphs.then(|| {
            self.marks
                .iter()
                .map(|(name, _)| glyph_order.glyph_id(name).unwrap())
                .chain(
                    self.bases
                        .iter()
                        .map(|(name, _)| glyph_order.glyph_id(name).unwrap()),
                )
                .collect()
        })
    }
}

// a trait to abstract over three very similar builders
trait MarkAttachmentBuilder: Default {
    fn add_mark(&mut self, gid: GlyphId16, group: &GroupName, anchor: FeaAnchor);
    fn add_base(&mut self, gid: GlyphId16, group: &GroupName, anchor: BaseOrLigAnchors<FeaAnchor>);
}

impl MarkAttachmentBuilder for MarkToBaseBuilder {
    fn add_mark(&mut self, gid: GlyphId16, group: &GroupName, anchor: FeaAnchor) {
        let _ = self.insert_mark(gid, group.clone(), anchor);
    }

    fn add_base(&mut self, gid: GlyphId16, group: &GroupName, anchor: BaseOrLigAnchors<FeaAnchor>) {
        match anchor {
            BaseOrLigAnchors::Base(anchor) => self.insert_base(gid, group, anchor),
            BaseOrLigAnchors::Ligature(_) => panic!("lig anchors in mark2base builder"),
        }
    }
}

impl MarkAttachmentBuilder for MarkToMarkBuilder {
    fn add_mark(&mut self, gid: GlyphId16, group: &GroupName, anchor: FeaAnchor) {
        let _ = self.insert_mark1(gid, group.clone(), anchor);
    }

    fn add_base(&mut self, gid: GlyphId16, group: &GroupName, anchor: BaseOrLigAnchors<FeaAnchor>) {
        match anchor {
            BaseOrLigAnchors::Base(anchor) => self.insert_mark2(gid, group, anchor),
            BaseOrLigAnchors::Ligature(_) => panic!("lig anchors in mark2mark to builder"),
        }
    }
}

impl MarkAttachmentBuilder for MarkToLigBuilder {
    fn add_mark(&mut self, gid: GlyphId16, group: &GroupName, anchor: FeaAnchor) {
        let _ = self.insert_mark(gid, group.clone(), anchor);
    }

    fn add_base(
        &mut self,
        gid: GlyphId16,
        group: &GroupName,
        anchors: BaseOrLigAnchors<FeaAnchor>,
    ) {
        match anchors {
            BaseOrLigAnchors::Ligature(anchors) => {
                self.insert_ligature(gid, group.clone(), anchors)
            }
            BaseOrLigAnchors::Base(_) => panic!("base anchors passed to mark2lig builder"),
        }
    }
}

impl<'a> MarkLookupBuilder<'a> {
    fn new(
        anchors: Vec<&'a GlyphAnchors>,
        glyph_order: &'a GlyphOrder,
        static_metadata: &'a StaticMetadata,
        ast: &ParseTree,
    ) -> Result<Self, Error> {
        let gdef_classes = get_gdef_classes(static_metadata, ast, glyph_order);
        let fea_scripts = get_fea_scripts(ast);
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
            // skip glyphs that are not mark/lig/base, if we have any defined categories
            if !include.is_empty() && !include.contains(&anchors.glyph_name) {
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
                pruned
                    .entry(anchors.glyph_name.clone())
                    .or_insert(Vec::new())
                    .push(anchor);
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
            _fea_scripts: fea_scripts,
            static_metadata,
            gdef_classes,
            mark_glyphs,
            lig_carets,
        })
    }

    fn build(&self) -> Result<FeaRsMarks, Error> {
        let mark_base_groups = self.make_mark_to_base_groups();
        let mark_mark_groups = self.make_mark_to_mark_groups();
        let mark_lig_groups = self.make_mark_to_liga_groups();

        let mark_base = self.make_lookups::<MarkToBaseBuilder>(mark_base_groups)?;
        let mark_mark = self.make_lookups::<MarkToMarkBuilder>(mark_mark_groups)?;
        let mark_lig = self.make_lookups::<MarkToLigBuilder>(mark_lig_groups)?;
        let curs = self.make_cursive_lookups()?;
        Ok(FeaRsMarks {
            glyphmap: self.glyph_order.iter().cloned().collect(),
            mark_base,
            mark_mark,
            mark_lig,
            curs,
            lig_carets: self.lig_carets.clone(),
        })
    }

    // code shared between mark2base and mark2mark
    fn make_lookups<T: MarkAttachmentBuilder>(
        &self,
        groups: BTreeMap<GroupName, MarkGroup>,
    ) -> Result<Vec<PendingLookup<T>>, Error> {
        groups
            .into_iter()
            .filter(|(_, group)| !(group.bases.is_empty() || group.marks.is_empty()))
            .map(|(group_name, group)| {
                let mut builder = T::default();
                let filter_set = group.make_filter_glyph_set(self.glyph_order);
                let mut flags = LookupFlag::empty();
                if filter_set.is_some() {
                    flags |= LookupFlag::USE_MARK_FILTERING_SET;
                }
                for (mark_name, anchor) in group.marks {
                    // we already filtered to only things in glyph order
                    let gid = self.glyph_order.glyph_id(&mark_name).unwrap();
                    let anchor = resolve_anchor_once(anchor, self.static_metadata, &mark_name)?;
                    builder.add_mark(gid, &group_name, anchor);
                }

                for (base_name, anchor) in group.bases {
                    let gid = self.glyph_order.glyph_id(&base_name).unwrap();
                    let anchor = resolve_anchor(anchor, self.static_metadata, &base_name)?;
                    builder.add_base(gid, &group_name, anchor);
                }
                Ok(PendingLookup::new(vec![builder], flags, filter_set))
            })
            .collect()
    }

    fn make_mark_to_base_groups(&self) -> BTreeMap<GroupName, MarkGroup<'a>> {
        let mut groups = BTreeMap::<_, MarkGroup>::new();
        for (glyph_name, anchors) in &self.anchor_lists {
            let is_mark = self.mark_glyphs.contains(glyph_name);
            // if we have explicit gdef classes and this is not an expilcit base
            let is_not_base = !self.gdef_classes.is_empty()
                && (self.gdef_classes.get(glyph_name)) != Some(&GlyphClassDef::Base);

            let treat_as_base = !(is_mark | is_not_base);
            for anchor in anchors {
                match &anchor.kind {
                    ir::AnchorKind::Base(group) if treat_as_base => groups
                        .entry(group.clone())
                        .or_default()
                        .bases
                        .push((glyph_name.clone(), BaseOrLigAnchors::Base(anchor))),
                    ir::AnchorKind::Mark(group) if is_mark => groups
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
        for (glyph, glyph_anchors) in &self.anchor_lists {
            if !mark_glyphs.contains(glyph) {
                continue;
            }

            for anchor in glyph_anchors {
                if let AnchorKind::Base(group_name) = &anchor.kind {
                    // only if this anchor is a base, AND we have a mark in the same group
                    if mark_anchors.contains(group_name) {
                        let group = result.entry(group_name.clone()).or_default();
                        group.filter_glyphs = true;
                        group
                            .bases
                            .push((glyph.clone(), BaseOrLigAnchors::Base(anchor)))
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
                        group.marks.push((mark.clone(), anchor))
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
        for (glyph_name, anchors) in &self.anchor_lists {
            // skip anything that is definitely not a ligature glyph
            let might_be_liga = self.gdef_classes.is_empty()
                || (self.gdef_classes.get(glyph_name) == Some(&GlyphClassDef::Ligature));

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
                    .push((glyph_name.clone(), BaseOrLigAnchors::Ligature(anchors)));
            }
        }

        // then we do another pass to add the marks in the used classes
        for (glyph_name, anchors) in &self.anchor_lists {
            if !self.mark_glyphs.contains(glyph_name) {
                continue;
            }
            for anchor in anchors {
                if let AnchorKind::Mark(group) = &anchor.kind {
                    if liga_anchor_groups.contains(group) {
                        groups
                            .entry(group.to_owned())
                            .or_default()
                            .marks
                            .push((glyph_name.clone(), anchor));
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

        for (glyph_name, anchors) in &self.anchor_lists {
            for anchor in anchors {
                match anchor.kind {
                    AnchorKind::CursiveEntry => {
                        entries.insert(glyph_name, anchor);
                        affected_glyphs.insert(glyph_name);
                    }
                    AnchorKind::CursiveExit => {
                        exits.insert(glyph_name, anchor);
                        affected_glyphs.insert(glyph_name);
                    }
                    _ => {}
                }
            }
        }
        if affected_glyphs.is_empty() {
            return Ok(vec![]);
        }
        for glyph_name in affected_glyphs {
            let gid = self.glyph_order.glyph_id(glyph_name).unwrap();
            let entry_anchor = entries
                .get(glyph_name)
                .map(|anchor| resolve_anchor_once(anchor, self.static_metadata, glyph_name))
                .transpose()?;
            let exit_anchor = exits
                .get(glyph_name)
                .map(|anchor| resolve_anchor_once(anchor, self.static_metadata, glyph_name))
                .transpose()?;
            builder.insert(gid, entry_anchor, exit_anchor);
        }
        // In the future we might to do an LTR/RTL split, but for now return a
        // vector with one element.
        Ok(vec![PendingLookup::new(vec![builder], flags, None)])
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
            .variant(WorkId::FeaturesAst)
            .variant(FeWorkId::ALL_ANCHORS)
            .build()
    }

    /// Generate mark data structures.
    fn exec(&self, context: &Context) -> Result<(), Error> {
        let static_metadata = context.ir.static_metadata.get();
        let glyph_order = context.ir.glyph_order.get();
        let raw_anchors = context.ir.anchors.all();
        let ast = context.fea_ast.get();

        let anchors = raw_anchors
            .iter()
            .map(|(_, anchors)| anchors.as_ref())
            .collect::<Vec<_>>();

        // this code is roughly equivalent to what in pythonland happens in
        // setContext: https://github.com/googlefonts/ufo2ft/blob/8e9e6eb66a/Lib/ufo2ft/featureWriters/markFeatureWriter.py#L322
        let ctx = MarkLookupBuilder::new(anchors, &glyph_order, &static_metadata, &ast.ast)?;
        let all_marks = ctx.build()?;

        context.fea_rs_marks.set(all_marks);

        Ok(())
    }
}

// in py this is set during _groupMarkGlyphsByAnchor; we try to match that logic
// https://github.com/googlefonts/ufo2ft/blob/8e9e6eb66/Lib/ufo2ft/featureWriters/markFeatureWriter.py#L412
fn find_mark_glyphs(
    anchors: &BTreeMap<GlyphName, Vec<&Anchor>>,
    gdef_classes: &BTreeMap<GlyphName, GlyphClassDef>,
) -> BTreeSet<GlyphName> {
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

fn get_fea_scripts(ast: &ParseTree) -> HashSet<Tag> {
    ast.typed_root()
        .statements()
        .filter_map(LanguageSystem::cast)
        .map(|langsys| langsys.script().to_raw())
        .filter(|tag| *tag != DFLT_SCRIPT)
        .collect()
}

fn get_gdef_classes(
    meta: &StaticMetadata,
    ast: &ParseTree,
    glyph_order: &GlyphOrder,
) -> BTreeMap<GlyphName, GlyphClassDef> {
    let glyph_map = glyph_order.iter().cloned().collect();
    // if we prefer classes defined in fea, compile the fea and see if we have any
    if meta.gdef_categories.prefer_gdef_categories_in_fea {
        if let Some(gdef_classes) =
            fea_rs::compile::compile::<NopVariationInfo, NopFeatureProvider>(
                ast,
                &glyph_map,
                None,
                None,
                Opts::new().compile_gpos(false),
            )
            .ok()
            .and_then(|mut comp| comp.0.gdef_classes.take())
        {
            return gdef_classes
                .into_iter()
                .filter_map(|(g, cls)| {
                    glyph_order
                        .glyph_name(g.to_u16() as _)
                        .cloned()
                        .map(|name| (name, cls))
                })
                .collect();
        }
    }
    // otherwise (we don't care about FEA or nothing is defined) we use the
    // classes in StaticMetadata (which are from public.openTypeCategories or
    // GlyphData.xml)
    meta.gdef_categories.categories.clone()
}

fn resolve_anchor(
    anchor: BaseOrLigAnchors<&ir::Anchor>,
    static_metadata: &StaticMetadata,
    glyph_name: &GlyphName,
) -> Result<BaseOrLigAnchors<FeaAnchor>, Error> {
    match anchor {
        BaseOrLigAnchors::Base(anchor) => {
            resolve_anchor_once(anchor, static_metadata, glyph_name).map(BaseOrLigAnchors::Base)
        }
        BaseOrLigAnchors::Ligature(anchors) => anchors
            .into_iter()
            .map(|a| {
                a.map(|a| resolve_anchor_once(a, static_metadata, glyph_name))
                    .transpose()
            })
            .collect::<Result<_, _>>()
            .map(BaseOrLigAnchors::Ligature),
    }
}

fn resolve_anchor_once(
    anchor: &ir::Anchor,
    static_metadata: &StaticMetadata,
    glyph_name: &GlyphName, // just used for error reporting
) -> Result<FeaAnchor, Error> {
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

    let mut anchor = FeaAnchor::new(x_default, y_default);
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
) -> Result<BTreeMap<GlyphId16, Vec<CaretValue>>, Error> {
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
) -> Result<Option<CaretValue>, Error> {
    if !matches!(anchor.kind, AnchorKind::Caret(_) | AnchorKind::VCaret(_)) {
        return Ok(None);
    }
    let is_vertical = matches!(anchor.kind, AnchorKind::VCaret(_));

    let values = anchor
        .positions
        .iter()
        .map(|(loc, pt)| {
            let pos = if is_vertical { pt.y } else { pt.x };
            (loc.clone(), OrderedFloat::from(pos as f32))
        })
        .collect::<Vec<_>>();

    let (default, deltas) = crate::features::resolve_variable_metric(
        static_metadata,
        values.iter().map(|item| (&item.0, &item.1)),
    )
    .map_err(|err| Error::AnchorDeltaError(glyph_name.to_owned(), err))?;
    Ok(Some(CaretValue::Coordinate {
        default,
        deltas: deltas.into(),
    }))
}

impl FeatureProvider for FeaRsMarks {
    fn add_features(&self, builder: &mut fea_rs::compile::FeatureBuilder) {
        let mut mark_lookups = Vec::new();
        let mut mkmk_lookups = Vec::new();
        let mut curs_lookups = Vec::new();

        for mark_base in self.mark_base.iter() {
            // each mark to base it's own lookup, whch differs from fontmake
            mark_lookups.push(builder.add_lookup(mark_base.clone()));
        }
        for mark_lig in self.mark_lig.iter() {
            mark_lookups.push(builder.add_lookup(mark_lig.clone()));
        }

        // If a mark has anchors that are themselves marks what we got here is a mark to mark
        for mark_mark in self.mark_mark.iter() {
            mkmk_lookups.push(builder.add_lookup(mark_mark.clone()));
        }

        for curs in self.curs.iter() {
            curs_lookups.push(builder.add_lookup(curs.clone()));
        }

        if !mark_lookups.is_empty() {
            builder.add_to_default_language_systems(Tag::new(b"mark"), &mark_lookups);
        }
        if !mkmk_lookups.is_empty() {
            builder.add_to_default_language_systems(Tag::new(b"mkmk"), &mkmk_lookups);
        }
        if !curs_lookups.is_empty() {
            builder.add_to_default_language_systems(Tag::new(b"curs"), &curs_lookups);
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

#[cfg(test)]
mod tests {
    use std::{ffi::OsStr, sync::Arc};

    use fea_rs::compile::Compilation;
    use fontdrasil::{
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

    use crate::features::FeaVariationInfo;

    use super::*;

    /// A helper for testing our mark generation code
    #[derive(Clone, Debug)]
    struct MarksInput<const N: usize> {
        prefer_gdef_categories_in_fea: bool,
        locations: [NormalizedLocation; N],
        anchors: BTreeMap<GlyphName, Vec<Anchor>>,
        categories: BTreeMap<GlyphName, GlyphClassDef>,
        user_fea: Arc<str>,
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
        fn new(locations: [&[f32]; N]) -> Self {
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
                user_fea: "languagesystem DFLT dflt;".into(),
                locations,
                anchors: Default::default(),
                categories: Default::default(),
                prefer_gdef_categories_in_fea: false,
            }
        }

        /// Provide custom user FEA.
        ///
        /// By default we use a single 'languagesytem DFLT dflt' statement.
        fn set_user_fea(&mut self, fea: &str) -> &mut Self {
            self.user_fea = fea.into();
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

        fn make_static_metadata(&self) -> StaticMetadata {
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
                })
                .collect::<Vec<_>>();
            let axis_map = axes.iter().map(|a| (a.tag, a)).collect();
            let named_instances = self
                .locations
                .iter()
                .enumerate()
                .map(|(i, loc)| NamedInstance {
                    name: format!("instance{i}"),
                    location: loc.to_user(&axis_map),
                })
                .collect();
            let glyph_locations = self.locations.iter().cloned().collect();
            let categories = GdefCategories {
                categories: self.categories.clone(),
                prefer_gdef_categories_in_fea: self.prefer_gdef_categories_in_fea,
            };
            StaticMetadata::new(
                1000,
                Default::default(),
                axes,
                named_instances,
                glyph_locations,
                Default::default(),
                42.,
                categories,
                None,
            )
            .unwrap()
        }

        fn run_marks_writer(
            &self,
            static_metadata: &StaticMetadata,
            ast: &ParseTree,
        ) -> FeaRsMarks {
            let anchors = self
                .anchors
                .iter()
                .map(|(name, anchors)| GlyphAnchors::new(name.clone(), anchors.clone()))
                .collect::<Vec<_>>();
            let anchorsref = anchors.iter().collect();
            let glyph_order = self.anchors.keys().cloned().collect();

            let ctx =
                MarkLookupBuilder::new(anchorsref, &glyph_order, static_metadata, ast).unwrap();

            ctx.build().unwrap()
        }

        fn compile(&self) -> Compilation {
            let static_metadata = self.make_static_metadata();
            let fea = self.user_fea.clone();
            let glyph_map = self.anchors.keys().cloned().collect();
            // first get the AST, which we need to use as input
            let (ast, _) = fea_rs::parse::parse_root(
                "memory".into(),
                Some(&glyph_map),
                Box::new(move |x: &OsStr| {
                    if x == "memory" {
                        Ok(fea.clone())
                    } else {
                        unreachable!("our FEA has no include statements");
                    }
                }),
            )
            .unwrap();

            // then run the marks code in this module:
            let marks = self.run_marks_writer(&static_metadata, &ast);
            let var_info = FeaVariationInfo::new(&static_metadata);
            // then compile with fea-rs, passing in our generated marks:
            let (result, _) = fea_rs::compile::compile(
                &ast,
                &glyph_map,
                Some(&var_info),
                Some(&marks),
                Default::default(),
            )
            .unwrap();
            result
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
            # mark: DFLT/dflt ## 1 MarkToBase rules
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
            # mark: DFLT/dflt ## 1 MarkToBase rules
            # lookupflag LookupFlag(0)
            a @(x: 100, y: 200)
              @(x: 100, y: 200) [acutecomb, tildecomb]
            # 1 MarkToLig rules
            # lookupflag LookupFlag(0)
            f_i (lig) [@(x: 100, y: 500), @(x: 600, y: 500)]
              @(x: 100, y: 200) [acutecomb, tildecomb]

            # mkmk: DFLT/dflt ## 1 MarkToMark rules
            # lookupflag LookupFlag(16)
            # filter glyphs: [acutecomb, tildecomb]
            tildecomb @(x: 100, y: 300)
              @(x: 100, y: 200) [acutecomb, tildecomb]
            "#
        );
    }

    #[test]
    fn custom_fea() {
        let out = simple_test_input()
            .set_user_fea("languagesystem latn dflt;")
            .get_normalized_output();
        assert_eq_ignoring_ws!(
            out,
            r#"
            # mark: latn/dflt ## 1 MarkToBase rules
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
            # mark: DFLT/dflt ## 1 MarkToBase rules
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
            # mark: DFLT/dflt ## 1 MarkToBase rules
            # lookupflag LookupFlag(0)
            A @(x: 100, y: 400)
              @(x: 5, y: 15) gravecomb
              @(x: 50, y: 50) acutecomb
            "#
        );
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
                # mark: DFLT/dflt ## 2 MarkToBase rules
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
    # mark: DFLT/dflt ## 1 MarkToLig rules
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
                # mark: DFLT/dflt ## 3 MarkToLig rules
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
            .add_glyph("v_v", None, |anchors| {
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

        let vv = &lig_carets.lig_glyphs[1].caret_values;
        assert_eq!(vv.len(), 1);
        let caret = vv[0].as_ref();
        assert!(
            matches!(caret, RawCaretValue::Format1(t) if t.coordinate == -222),
            "{caret:?}"
        );
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
                # mark: DFLT/dflt ## 2 MarkToBase rules
                # lookupflag LookupFlag(0)
                a @(x: 377 {386,389}, y: 451 {450,450})
                @(x: 218 {265,282}, y: 704 {707,708}) acutecomb
                ordfeminine @(x: 283 {290,292}, y: 691 {690,690})
                @(x: 218 {265,282}, y: 704 {707,708}) acutecomb
            "#
        );
    }
}
