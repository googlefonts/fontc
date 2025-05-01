//! Generates an [FeaRsKerns] datastructure to be fed to fea-rs

use std::{
    borrow::Cow,
    collections::{BTreeMap, BTreeSet, HashMap, HashSet},
};

use fea_rs::{
    compile::{FeatureKey, FeatureProvider},
    GlyphSet, ParseTree,
};
use fontdrasil::{
    coords::NormalizedLocation,
    orchestration::{Access, AccessBuilder, Work},
    types::GlyphName,
};
use fontir::{
    ir::{self, GlyphOrder, KernGroup, KerningGroups, KerningInstance, StaticMetadata},
    orchestration::WorkId as FeWorkId,
};
use icu_properties::props::BidiClass;
use log::debug;
use ordered_float::OrderedFloat;
use write_fonts::{
    read::{collections::IntSet, tables::gsub::Gsub, ReadError},
    tables::{
        gdef::GlyphClassDef,
        gpos::builders::{PairPosBuilder, ValueRecordBuilder},
        layout::LookupFlag,
    },
    types::{GlyphId16, Tag},
};

use crate::{
    error::Error,
    features::{
        properties::{ScriptDirection, UnicodeShortName, COMMON_SCRIPT, INHERITED_SCRIPT},
        resolve_variable_metric,
    },
    orchestration::{
        AllKerningPairs, AnyWorkId, BeWork, Context, FeaFirstPassOutput, FeaRsKerns,
        KernAdjustments, KernFragment, KernPair, KernSide, WorkId,
    },
};

use super::{properties::CharMap, PendingLookup, DFLT_LANG, DFLT_SCRIPT};

/// On Linux it took ~0.01 ms per loop, try to get enough to make fan out worthwhile
/// based on empirical testing
const KERNS_PER_BLOCK: usize = 2048;
const KERN: Tag = Tag::new(b"kern");
// we don't currently compile this feature, but we will, and it is referenced
// in places because our impl is based on fonttools.
const DIST: Tag = Tag::new(b"dist");

/// Accumulation of all the kerning from IR
#[derive(Debug)]
struct GatherIrKerningWork;

/// A fragment of the kerning from IR that can run in parallel with other fragments
#[derive(Debug)]
struct KerningFragmentWork {
    segment: usize,
}

/// Accumulate the result of fragment processing so we can feed it to fea-rs
#[derive(Debug)]
struct KerningGatherWork;

/// Whether or not a given mark glyph is a spacing mark, e.g. has width
#[derive(Clone, Copy, Debug)]
enum MarkSpacing {
    Spacing,
    NonSpacing,
}

pub fn create_gather_ir_kerning_work() -> Box<BeWork> {
    Box::new(GatherIrKerningWork {})
}

pub fn create_kern_segment_work(kern_pairs: &AllKerningPairs) -> Vec<Box<BeWork>> {
    let segments = kern_pairs.adjustments.len().div_ceil(KERNS_PER_BLOCK);
    let mut work: Vec<Box<BeWork>> = Vec::with_capacity(segments);
    debug!(
        "Process {} kerning adjustments in {} chunks",
        kern_pairs.adjustments.len(),
        segments
    );
    for segment in 0..segments {
        work.push(Box::new(KerningFragmentWork { segment }));
    }
    work
}

pub fn create_kerns_work() -> Box<BeWork> {
    Box::new(KerningGatherWork {})
}

impl Work<Context, AnyWorkId, Error> for GatherIrKerningWork {
    fn id(&self) -> AnyWorkId {
        WorkId::GatherIrKerning.into()
    }

    fn read_access(&self) -> Access<AnyWorkId> {
        // Blocks execution until workload.rs handle_success figures out what we will read, e.g. IR kerning instances is done.
        // Updated when success for IR kerning groups is received. See https://github.com/googlefonts/fontc/pull/655.
        Access::Unknown
    }

    fn write_access(&self) -> Access<AnyWorkId> {
        Access::Variant(WorkId::GatherIrKerning.into())
    }

    /// Generate kerning data structures.
    fn exec(&self, context: &Context) -> Result<(), Error> {
        let glyph_order = context.ir.glyph_order.get();
        let ir_groups = context.ir.kerning_groups.get();
        let ir_kerns = context.ir.kerning_at.all();

        // convert the groups stored in the Kerning object into the glyph classes
        // expected by fea-rs:
        let groups = ir_groups
            .groups
            .iter()
            .filter_map(|(class_name, glyph_set)| {
                let glyph_class: GlyphSet = glyph_set
                    .iter()
                    // drop any unknown glyphs
                    .filter_map(|name| {
                        let r = glyph_order.glyph_id(name);
                        if r.is_none() {
                            log::warn!(
                                "Skipping unknown glyph '{name}' in kern group '{class_name}'"
                            );
                        }
                        r
                    })
                    .collect();
                if glyph_class.is_empty() {
                    log::warn!("Dropping empty kern group '{class_name}");
                    None
                } else {
                    Some((class_name.clone(), glyph_class))
                }
            })
            .collect::<BTreeMap<_, _>>();

        // Add IR kerns to builder. IR kerns are split by location so put them back together again.
        let mut kern_by_pos: HashMap<_, _> = ir_kerns
            .iter()
            .map(|(_, ki)| (ki.location.clone(), ki.as_ref().to_owned()))
            .collect();

        align_kerning(&ir_groups, &mut kern_by_pos);
        let mut adjustments: HashMap<ir::KernPair, KernAdjustments> = Default::default();

        // We want to add items to locations in the same order as the group locations
        // so start with group locations and then find the matching kerning.
        ir_groups
            .locations
            .iter()
            .filter_map(|pos| kern_by_pos.get(pos))
            .flat_map(|instance| {
                instance
                    .kerns
                    .iter()
                    .map(|(pair, adjustment)| (pair, (instance.location.clone(), *adjustment)))
            })
            .for_each(|(pair, (location, adjustment))| {
                adjustments
                    .entry(pair.clone())
                    .or_default()
                    .insert(location, adjustment);
            });

        let adjustments: Vec<_> = adjustments
            .into_iter()
            // drop any rule that references a non-existent group or glyph:
            .filter(|((left, right), _)| {
                for side in [left, right] {
                    match side {
                        ir::KernSide::Group(name) if !groups.contains_key(name) => {
                            log::warn!("Unknown kern class '{name}' will be skipped");
                            return false;
                        }
                        ir::KernSide::Glyph(name) if glyph_order.glyph_id(name).is_none() => {
                            log::warn!("Unknown kern glyph '{name}' will be skipped");
                            return false;
                        }
                        _ => (),
                    }
                }
                true
            })
            .collect();
        debug!(
            "{} ir kerns became {} classes and {} adjustments",
            ir_kerns.len(),
            groups.len(),
            adjustments.len()
        );
        context.all_kerning_pairs.set(AllKerningPairs {
            groups,
            adjustments,
        });
        Ok(())
    }
}

/// 'align' the kerning, ensuring each pair is defined for each location.
///
/// missing pairs are filled in via the UFO kerning value lookup algorithm:
///
/// <https://unifiedfontobject.org/versions/ufo3/kerning.plist/#kerning-value-lookup-algorithm>
///
/// in pythonland this happens in ufo2ft, here:
/// <https://github.com/googlefonts/ufo2ft/blob/5fd168e65a0b0a/Lib/ufo2ft/featureWriters/kernFeatureWriter.py#L442>
fn align_kerning(
    groups: &KerningGroups,
    instances: &mut HashMap<NormalizedLocation, KerningInstance>,
) {
    // all pairs defined in at least one instance
    let all_known_pairs = instances
        .values()
        .flat_map(|instance| instance.kerns.keys())
        .cloned()
        .collect::<HashSet<_>>();

    let side1_glyph_to_group_map = groups
        .groups
        .iter()
        .filter(|(group, _)| matches!(group, KernGroup::Side1(_)))
        .flat_map(|(group, glyphs)| glyphs.iter().map(move |glyph| (glyph, group)))
        .collect::<HashMap<_, _>>();
    let side2_glyph_to_group_map = groups
        .groups
        .iter()
        .filter(|(group, _)| matches!(group, KernGroup::Side2(_)))
        .flat_map(|(group, glyphs)| glyphs.iter().map(move |glyph| (glyph, group)))
        .collect::<HashMap<_, _>>();

    for instance in instances.values_mut() {
        align_instance(
            &all_known_pairs,
            &mut instance.kerns,
            &side1_glyph_to_group_map,
            &side2_glyph_to_group_map,
        )
    }
}

fn align_instance(
    all_pairs: &HashSet<ir::KernPair>,
    instance: &mut BTreeMap<ir::KernPair, OrderedFloat<f64>>,
    side1_glyphs: &HashMap<&GlyphName, &KernGroup>,
    side2_glyphs: &HashMap<&GlyphName, &KernGroup>,
) {
    let mut buf = Vec::new();
    // iterate the pairs that are not present in this instance
    for pair in all_pairs.iter().filter(|pair| !instance.contains_key(pair)) {
        let value = lookup_kerning_value(pair, instance, side1_glyphs, side2_glyphs);

        // accumulate any additions and add at the end, otherwise newly added
        // additions could influence the calculation of subsequent values
        buf.push((pair, value));
    }
    // when done all pairs, add them to the instance
    for (pair, value) in buf {
        instance.insert(pair.to_owned(), value);
    }
}

// <https://github.com/fonttools/fonttools/blob/a3b9eddcafca/Lib/fontTools/ufoLib/kerning.py#L1>
fn lookup_kerning_value(
    pair: &ir::KernPair,
    kerning: &BTreeMap<ir::KernPair, OrderedFloat<f64>>,
    side1_glyphs: &HashMap<&GlyphName, &KernGroup>,
    side2_glyphs: &HashMap<&GlyphName, &KernGroup>,
) -> OrderedFloat<f64> {
    // if already a group, return it, else look for group for glyph
    fn get_group_if_glyph(
        side: &ir::KernSide,
        map: &HashMap<&GlyphName, &KernGroup>,
    ) -> Option<ir::KernSide> {
        match side {
            ir::KernSide::Glyph(glyph) => map
                .get(&glyph)
                .map(|group| ir::KernSide::Group((*group).clone())),
            ir::KernSide::Group(_) => Some(side.to_owned()),
        }
    }

    let (first, second) = pair;
    // for each side: if it's a group, we only check the group.
    // if it's a glyph, we check both the glyph as well as the group containing that glyph.
    let first_group = get_group_if_glyph(first, side1_glyphs);
    let second_group = get_group_if_glyph(second, side2_glyphs);
    let first = Some(first).filter(|side| side.is_glyph());
    let second = Some(second).filter(|side| side.is_glyph());

    for (first, second) in [
        (first.cloned(), second_group.clone()),
        (first_group.clone(), second.cloned()),
        (first_group.clone(), second_group.clone()),
    ] {
        if let Some(pair) = first.zip(second) {
            if let Some(value) = kerning.get(&pair) {
                return *value;
            }
        }
    }

    // then fallback to zero
    0.0.into()
}

impl Work<Context, AnyWorkId, Error> for KerningFragmentWork {
    fn id(&self) -> AnyWorkId {
        WorkId::KernFragment(self.segment).into()
    }

    fn read_access(&self) -> Access<AnyWorkId> {
        AccessBuilder::new()
            .variant(FeWorkId::StaticMetadata)
            .variant(WorkId::GatherIrKerning)
            .build()
    }

    fn exec(&self, context: &Context) -> Result<(), Error> {
        let static_metadata = context.ir.static_metadata.get();
        let glyph_order = context.ir.glyph_order.get();
        let kerning = context.all_kerning_pairs.get();
        let start = self.segment * KERNS_PER_BLOCK;
        let end = (start + KERNS_PER_BLOCK).min(kerning.adjustments.len());
        assert!(start <= end, "bad range {start}..{end}");

        // now for each kerning entry, directly add a rule to a builder:
        let our_kerns = &kerning.adjustments[start..end];

        let mut kerns = Vec::with_capacity(our_kerns.len());
        for ((side1, side2), values) in our_kerns {
            let (default_value, deltas) = resolve_variable_metric(&static_metadata, values.iter())
                .map_err(|error| Error::KernDeltaError {
                    pair: (side1.clone(), side2.clone()),
                    error,
                })?;

            let mut value = ValueRecordBuilder::new().with_x_advance(default_value);
            // only encode deltas if they aren't all zeros
            if deltas.iter().any(|v| v.1 != 0) {
                value = value.with_x_advance_device(deltas);
            }
            // groups and glyphs have already been validated
            let side1 = KernSide::from_ir_side(side1, &glyph_order, &kerning.groups).unwrap();
            let side2 = KernSide::from_ir_side(side2, &glyph_order, &kerning.groups).unwrap();
            kerns.push(KernPair {
                side1,
                side2,
                value,
            })
        }

        context.kern_fragments.set(KernFragment {
            segment: self.segment,
            kerns,
        });

        Ok(())
    }
}

impl Work<Context, AnyWorkId, Error> for KerningGatherWork {
    fn id(&self) -> AnyWorkId {
        WorkId::GatherBeKerning.into()
    }

    fn read_access(&self) -> Access<AnyWorkId> {
        Access::Unknown // https://github.com/googlefonts/fontc/issues/647: don't enable until KernFragment's spawn
    }

    fn exec(&self, context: &Context) -> Result<(), Error> {
        debug!("Gather be kerning");
        let arc_fragments = context.kern_fragments.all();
        let ast = context.fea_ast.get();
        let glyph_order = context.ir.glyph_order.get();
        let meta = context.ir.static_metadata.get();
        let mut pairs: Vec<_> = arc_fragments
            .iter()
            .flat_map(|(_, fragment)| fragment.kerns.iter())
            .collect();
        pairs.sort();

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
        let non_spacing_glyphs = glyphs
            .iter()
            .filter(|g| g.sources().values().all(|instance| instance.width == 0.0))
            .map(|g| glyph_order.glyph_id(&g.name).unwrap())
            .collect::<HashSet<_>>();

        let lookups = finalize_kerning(
            &pairs,
            &ast,
            &meta,
            &glyph_order,
            char_map,
            non_spacing_glyphs,
        )?;
        context.fea_rs_kerns.set(lookups);
        Ok(())
    }
}

//. take the kerning fragments and generate the kerning lookups.
//
// This includes much of the logic from the ufo2ft KernFeatureWriter
fn finalize_kerning(
    pairs: &[&KernPair],
    ast: &FeaFirstPassOutput,
    meta: &StaticMetadata,
    glyph_order: &GlyphOrder,
    char_map: HashMap<u32, GlyphId16>,
    non_spacing_glyphs: HashSet<GlyphId16>,
) -> Result<FeaRsKerns, Error> {
    let todo = super::feature_writer_todo_list(&[KERN, DIST], &ast.ast);
    if pairs.is_empty() || todo.is_empty() {
        log::info!("no kerning work to do");
        return Ok(Default::default());
    }
    let known_scripts = guess_font_scripts(&ast.ast, &char_map);
    let glyph_classes = super::get_gdef_classes(meta, ast, glyph_order);

    let mark_glyphs = glyph_order
        .iter()
        .filter_map(|(gid, _)| {
            let is_mark = glyph_classes.get(&gid) == Some(&GlyphClassDef::Mark);
            is_mark.then(|| {
                let spacing = if non_spacing_glyphs.contains(&gid) {
                    MarkSpacing::NonSpacing
                } else {
                    MarkSpacing::Spacing
                };
                (gid, spacing)
            })
        })
        .collect();

    let split_ctx = KernSplitContext::new(&char_map, &known_scripts, ast.gsub(), mark_glyphs)?;

    let lookups = split_ctx.make_lookups(pairs);
    let (lookups_by_script, lookups) = split_lookups_by_script(lookups);

    let kern_features = todo
        .contains(&KERN)
        .then(|| assign_lookups_to_scripts(lookups_by_script.clone(), &ast.ast, KERN));
    let dist_features = todo
        .contains(&DIST)
        .then(|| assign_lookups_to_scripts(lookups_by_script, &ast.ast, DIST));
    let features = kern_features
        .into_iter()
        .flatten()
        .chain(dist_features.into_iter().flatten())
        .collect();
    debug_ordered_lookups(&features, &lookups);
    Ok(FeaRsKerns { lookups, features })
}

/// Given a map of `[scripts] -> [lookups]`, convert it into a map of
/// `script -> [lookup index]`.
///
/// Also returns the ordered lookups.
fn split_lookups_by_script(
    lookups: BTreeMap<BTreeSet<UnicodeShortName>, Vec<PendingLookup<PairPosBuilder>>>,
) -> (
    BTreeMap<UnicodeShortName, Vec<usize>>,
    Vec<PendingLookup<PairPosBuilder>>,
) {
    let mut lookups_by_script = BTreeMap::new();
    let mut ordered_lookups = Vec::new();

    for (scripts, lookups) in lookups {
        for lookup in lookups {
            let idx = ordered_lookups.len();
            ordered_lookups.push(lookup);
            for script in &scripts {
                lookups_by_script
                    .entry(script.to_owned())
                    .or_insert(Vec::new())
                    .push(idx);
            }
        }
    }
    (lookups_by_script, ordered_lookups)
}

/// returns a vec of lookups (as a vec of subtables), along with a map of features -> lookups
/// (by order in the first vec)
///
/// this based on
/// <https://github.com/googlefonts/ufo2ft/blob/f6b4f42460b/Lib/ufo2ft/featureWriters/kernFeatureWriter.py#L772>
fn assign_lookups_to_scripts(
    //lookups: BTreeMap<BTreeSet<UnicodeShortName>, Vec<PendingLookup<PairPosBuilder>>>,
    mut lookups_by_script: BTreeMap<UnicodeShortName, Vec<usize>>,
    ast: &ParseTree,
    // one of 'kern' or 'dist'
    current_feature: Tag,
) -> BTreeMap<FeatureKey, Vec<usize>> {
    let dflt_langs = vec![DFLT_LANG];
    let dist_enabled_scripts = super::properties::dist_feature_enabled_scripts();
    let is_kern_feature = current_feature == KERN;
    assert!(is_kern_feature || current_feature == DIST);

    let fea_langs_by_script = super::get_fea_language_systems(ast);

    let mut default_lookups = Vec::new();
    if let Some(common_lookups) = lookups_by_script
        .get(&COMMON_SCRIPT)
        .filter(|_| is_kern_feature)
    {
        log::debug!("found {} default lookups", common_lookups.len());
        default_lookups.extend(common_lookups.iter().copied());
    }

    //inDesign bugfix:
    // <https://github.com/googlefonts/ufo2ft/blob/f6b4f42460b/Lib/ufo2ft/featureWriters/kernFeatureWriter.py#L785>
    if is_kern_feature {
        let (mut ltr_lookups, mut rtl_lookups) = (Vec::new(), Vec::new());
        for (script, lookups) in lookups_by_script
            .iter()
            .filter(|(script, _)| !dist_enabled_scripts.contains(*script))
        {
            match ScriptDirection::for_script(script) {
                ScriptDirection::LeftToRight => ltr_lookups.extend(lookups.iter().copied()),
                ScriptDirection::RightToLeft => rtl_lookups.extend(lookups.iter().copied()),
                ScriptDirection::Auto => (),
            }
        }

        // if we have any LTR lookups, we add those to defaults, otherwise add RTL lookups
        if !ltr_lookups.is_empty() {
            default_lookups.extend(ltr_lookups);
        } else {
            default_lookups.extend(rtl_lookups);
        }
        default_lookups.sort_unstable();
    }

    let mut features = BTreeMap::new();
    if !default_lookups.is_empty() {
        let languages = fea_langs_by_script.get(&DFLT_SCRIPT).unwrap_or(&dflt_langs);
        for lang in languages {
            features.insert(
                FeatureKey::new(current_feature, *lang, DFLT_SCRIPT),
                default_lookups.clone(),
            );
        }
    }

    // remove these, which need to get added to all other languagesystems
    let common_lookups = lookups_by_script.remove(&COMMON_SCRIPT);
    let inherited_lookups = lookups_by_script.remove(&INHERITED_SCRIPT);

    let dflt_lookups = match (common_lookups, inherited_lookups) {
        (Some(mut a), Some(b)) => {
            a.extend(b);
            a.sort_unstable();
            a.dedup();
            a
        }
        (Some(a), None) | (None, Some(a)) => a,
        (None, None) => Vec::new(),
    };

    for (script, mut lookups) in lookups_by_script
        .into_iter()
        .filter(|(script, _)| dist_enabled_scripts.contains(script) != is_kern_feature)
    {
        lookups.extend(dflt_lookups.iter().copied());
        lookups.sort_unstable();
        lookups.dedup();

        for tag in super::properties::script_to_ot_tags(&script) {
            let languages = fea_langs_by_script.get(&tag).unwrap_or(&dflt_langs);
            for lang in languages {
                features.insert(
                    FeatureKey::new(current_feature, *lang, tag),
                    lookups.clone(),
                );
            }
        }
    }
    features
}

fn debug_ordered_lookups(
    features: &BTreeMap<FeatureKey, Vec<usize>>,
    lookups: &[PendingLookup<PairPosBuilder>],
) {
    for (i, lookup) in lookups.iter().enumerate() {
        let total_rules = lookup.subtables().iter().map(|x| x.len()).sum::<usize>();
        log::trace!("lookup {i}, {total_rules} rules");
    }

    let mut feature_keys = features.keys().collect::<Vec<_>>();
    feature_keys.sort();
    for feature in feature_keys {
        let indices = features.get(feature).unwrap();
        log::trace!("feature {feature:?}, lookups {indices:?}");
    }
}

/// All the state needed for splitting kern pairs by script & writing direction
struct KernSplitContext {
    /// map of all mark glyphs + whether they are spacing or not
    mark_glyphs: HashMap<GlyphId16, MarkSpacing>,
    glyph_scripts: HashMap<GlyphId16, HashSet<UnicodeShortName>>,
    bidi_glyphs: BTreeMap<BidiClass, IntSet<GlyphId16>>,
    opts: KernSplitOptions,
    dflt_scripts: HashSet<UnicodeShortName>,
    common_scripts: HashSet<UnicodeShortName>,
}

// unused for now, but included so that our impl can more closely follow fonttools
struct KernSplitOptions {
    ignore_marks: bool,
}

impl Default for KernSplitOptions {
    fn default() -> Self {
        Self { ignore_marks: true }
    }
}

impl KernSplitContext {
    fn new(
        char_map: &HashMap<u32, GlyphId16>,
        known_scripts: &HashSet<UnicodeShortName>,
        gsub: Option<Gsub>,
        mark_glyphs: HashMap<GlyphId16, MarkSpacing>,
    ) -> Result<Self, ReadError> {
        let glyph_scripts =
            super::properties::scripts_by_glyph(char_map, known_scripts, gsub.as_ref())?;
        let bidi_glyphs = super::properties::glyphs_by_bidi_class(char_map, gsub.as_ref())?;

        Ok(Self {
            mark_glyphs,
            glyph_scripts,
            bidi_glyphs,
            opts: KernSplitOptions::default(),
            dflt_scripts: HashSet::from([COMMON_SCRIPT, INHERITED_SCRIPT]),
            common_scripts: HashSet::from([COMMON_SCRIPT]),
        })
    }

    // <https://github.com/googlefonts/ufo2ft/blob/cea60d71dfc/Lib/ufo2ft/featureWriters/kernFeatureWriter.py#L242>
    fn make_lookups(
        &self,
        pairs: &[&KernPair],
    ) -> BTreeMap<BTreeSet<UnicodeShortName>, Vec<PendingLookup<PairPosBuilder>>> {
        if !self.opts.ignore_marks {
            let pairs = pairs.iter().map(|x| Cow::Borrowed(*x)).collect::<Vec<_>>();
            return self.make_split_script_kern_lookups(&pairs, false);
        }

        let (base_pairs, mark_pairs) = self.split_base_and_mark_pairs(pairs);
        let mut result = BTreeMap::new();
        if !base_pairs.is_empty() {
            result = self.make_split_script_kern_lookups(&base_pairs, false);
        }
        if !mark_pairs.is_empty() {
            for (scripts, lookups) in self.make_split_script_kern_lookups(&mark_pairs, true) {
                result.entry(scripts).or_default().extend(lookups);
            }
        }
        result
    }

    /// Split these pairpos rules into lookups based on script.
    ///
    /// Returns a map of scripts: `[lookup]`, where 'scripts' is a set of scripts
    /// referencing the lookups.
    ///
    // <https://github.com/googlefonts/ufo2ft/blob/f6b4f42460b/Lib/ufo2ft/featureWriters/kernFeatureWriter.py#L694>
    fn make_split_script_kern_lookups(
        &self,
        pairs: &[Cow<KernPair>],
        are_marks: bool,
    ) -> BTreeMap<BTreeSet<UnicodeShortName>, Vec<PendingLookup<PairPosBuilder>>> {
        let mut lookups_by_script = BTreeMap::new();
        let kerning_per_script = self.split_kerns(pairs);
        let mut bidi_buf = BTreeSet::new(); // we can reuse this for each pair
        for (scripts, pairs) in kerning_per_script {
            let mut builder = PairPosBuilder::default();
            for mut pair in pairs {
                bidi_buf.clear();
                for (direction, glyphs) in &self.bidi_glyphs {
                    if !pair.glyphs_are_disjoint(glyphs) {
                        bidi_buf.insert(*direction);
                    }
                }
                if bidi_buf.contains(&BidiClass::LeftToRight)
                    && bidi_buf.contains(&BidiClass::RightToLeft)
                {
                    log::warn!(
                        "skipping kern pair with ambigous direction: {}/{}",
                        pair.side1,
                        pair.side2,
                    );
                    continue;
                }

                let script_direction = ScriptDirection::for_script(scripts.first().unwrap());
                assert!(scripts
                    .iter()
                    .all(|x| ScriptDirection::for_script(x) == script_direction));
                let script_is_rtl = matches!(script_direction, ScriptDirection::RightToLeft);
                let pair_is_rtl = script_is_rtl && !bidi_buf.contains(&BidiClass::LeftToRight);
                if pair_is_rtl {
                    pair.make_rtl_compatible();
                }
                pair.add_to(&mut builder);
            }
            let lookup = self.make_lookup(builder, !are_marks);
            lookups_by_script.insert(scripts, vec![lookup]);
        }
        lookups_by_script
    }

    // logic from
    // <https://github.com/googlefonts/ufo2ft/blob/cea60d71dfc/Lib/ufo2ft/featureWriters/kernFeatureWriter.py#L385>
    fn make_lookup(
        &self,
        builder: PairPosBuilder,
        ignore_marks: bool,
    ) -> PendingLookup<PairPosBuilder> {
        let mut flags = LookupFlag::empty();
        let mut filter_class = None;
        if ignore_marks && self.opts.ignore_marks {
            let spacing_marks: GlyphSet = self
                .mark_glyphs
                .iter()
                .filter_map(|(gid, spacing)| {
                    matches!(spacing, MarkSpacing::Spacing).then_some(*gid)
                })
                .collect();
            if spacing_marks.is_empty() {
                flags |= LookupFlag::IGNORE_MARKS;
            } else {
                filter_class = Some(spacing_marks);
                flags |= LookupFlag::USE_MARK_FILTERING_SET;
            }
        }
        PendingLookup::new(vec![builder], flags, filter_class)
    }

    // <https://github.com/googlefonts/ufo2ft/blob/f6b4f42460b/Lib/ufo2ft/featureWriters/kernFeatureWriter.py#L842>
    fn split_kerns(
        &self,
        pairs: &[Cow<KernPair>],
    ) -> HashMap<BTreeSet<UnicodeShortName>, Vec<KernPair>> {
        let mut kerning_per_script = HashMap::new();
        for pair in pairs {
            // filter out zero-value class pairs:
            // https://github.com/googlefonts/ufo2ft/blob/5a606b7884bb6d/Lib/ufo2ft/featureWriters/kernFeatureWriter.py#L431
            if pair.side1.is_group() && pair.side2.is_group() && pair.value.is_all_zeros() {
                continue;
            }
            for (scripts, pair) in self.partition_by_script(pair) {
                kerning_per_script
                    .entry(scripts)
                    .or_insert(Vec::new())
                    .push(pair);
            }
        }

        kerning_per_script = merge_scripts(kerning_per_script);
        for scripts in kerning_per_script.keys().filter(|x| x.len() > 1) {
            log::debug!("merged kerning lookups for {scripts:?}");
        }

        kerning_per_script
    }

    // <https://github.com/googlefonts/ufo2ft/blob/f6b4f42460b340c/Lib/ufo2ft/featureWriters/kernFeatureWriter.py#L865>
    fn partition_by_script<'b>(
        &self,
        pair: &'b KernPair,
    ) -> impl Iterator<Item = (BTreeSet<UnicodeShortName>, KernPair)> + 'b {
        //TODO: we could definitely make a reusable context and save all this
        //reallocation
        let mut resolved_scripts = HashMap::new();
        let mut side1_directions = HashMap::new();
        let mut side2_directions = HashMap::new();
        for glyph in pair.first_glyphs() {
            let mut scripts = self.glyph_scripts.get(&glyph).unwrap_or(&self.dflt_scripts);
            if !scripts.is_disjoint(&self.dflt_scripts) {
                // if has any dflt script, just treat it as common
                scripts = &self.common_scripts;
            }
            resolved_scripts.insert(glyph, scripts.to_owned());
            for direction in scripts.iter().map(ScriptDirection::for_script) {
                side1_directions
                    .entry(direction)
                    .or_insert(HashSet::new())
                    .insert(glyph);
            }
        }

        for glyph in pair.second_glyphs() {
            let mut scripts = self.glyph_scripts.get(&glyph).unwrap_or(&self.dflt_scripts);
            if !scripts.is_disjoint(&self.dflt_scripts) {
                scripts = &self.common_scripts;
            }
            resolved_scripts.insert(glyph, scripts.to_owned());
            for direction in scripts.iter().map(ScriptDirection::for_script) {
                side2_directions
                    .entry(direction)
                    .or_insert(HashSet::new())
                    .insert(glyph);
            }
        }

        let mut product = side1_directions.into_iter().flat_map(move |s1d| {
            side2_directions
                .clone()
                .into_iter()
                .map(move |s2d| (s1d.clone(), s2d))
        });

        std::iter::from_fn(move || loop {
            // we need to loop here so that we can skip some items

            let ((side1_dir, side1_glyphs), (side2_dir, side2_glyphs)) = product.next()?;

            let side1: GlyphSet = side1_glyphs.iter().copied().collect();
            let side1_scripts = side1
                .iter()
                .flat_map(|gid| resolved_scripts.get(&gid).unwrap().iter().copied())
                .collect::<HashSet<_>>();
            let side2: GlyphSet = side2_glyphs.iter().copied().collect();
            let side2_scripts = side2
                .iter()
                .flat_map(|gid| resolved_scripts.get(&gid).unwrap().iter().copied())
                .collect::<HashSet<_>>();

            if !side1_dir.plays_nicely_with(&side2_dir) {
                log::warn!("skipping kerning pair {side1:?}/{side2:?} with mixed direction {side1_dir:?}/{side2_dir:?}");
                continue;
            }

            let mut scripts = side1_scripts
                .iter()
                .copied()
                .chain(side2_scripts.iter().copied())
                .collect::<BTreeSet<_>>();
            if ![side1_scripts, side2_scripts]
                .iter()
                .all(|x| x.contains(&COMMON_SCRIPT))
            {
                scripts.remove(&COMMON_SCRIPT);
            }
            return Some((scripts, pair.with_new_glyphs(side1, side2)));
        })
    }

    /// returns vecs of base and mark pairs.
    ///
    /// Where possible the input items will be reused, but if a given entry contains
    /// mixed items we will need to allocate new entries.
    ///
    /// see
    /// <https://github.com/googlefonts/ufo2ft/blob/cea60d71dfc/Lib/ufo2ft/featureWriters/kernFeatureWriter.py#L441>
    fn split_base_and_mark_pairs<'b>(
        &self,
        pairs: &[&'b KernPair],
    ) -> (Vec<Cow<'b, KernPair>>, Vec<Cow<'b, KernPair>>) {
        enum GlyphSetContent {
            Empty,
            BasesOnly,
            MarksOnly,
            Mixed,
        }

        // little helper to tell us what's in a glyphset
        fn classify_kernside_contents(
            side: &KernSide,
            marks: &HashMap<GlyphId16, MarkSpacing>,
        ) -> GlyphSetContent {
            side.iter().fold(GlyphSetContent::Empty, |val, gid| {
                match (val, marks.contains_key(&gid)) {
                    (GlyphSetContent::Empty, true) => GlyphSetContent::MarksOnly,
                    (GlyphSetContent::Empty, false) => GlyphSetContent::BasesOnly,
                    (GlyphSetContent::MarksOnly, true) => GlyphSetContent::MarksOnly,
                    (GlyphSetContent::BasesOnly, false) => GlyphSetContent::BasesOnly,
                    _ => GlyphSetContent::Mixed,
                }
            })
        }

        /// split a glyphset into (bases, marks)
        fn split_glyphs(
            glyphs: &KernSide,
            marks: &HashMap<GlyphId16, MarkSpacing>,
        ) -> (KernSide, KernSide) {
            match glyphs {
                KernSide::Glyph(gid) if !marks.contains_key(gid) => {
                    (KernSide::Glyph(*gid), KernSide::empty())
                }
                KernSide::Glyph(gid) => (KernSide::empty(), KernSide::Glyph(*gid)),
                KernSide::Group(glyphs) => {
                    let (x, y): (IntSet<_>, IntSet<_>) =
                        glyphs.iter().partition(|gid| !marks.contains_key(gid));
                    (KernSide::Group(x), KernSide::Group(y))
                }
            }
        }

        if self.mark_glyphs.is_empty() {
            return (
                pairs.iter().map(|x| Cow::Borrowed(*x)).collect(),
                Vec::new(),
            );
        }

        let (mut base_pairs, mut mark_pairs) = (Vec::new(), Vec::new());

        for pair in pairs {
            match (&pair.side1, &pair.side2) {
                (KernSide::Glyph(side1), KernSide::Glyph(side2))
                    if !self.mark_glyphs.contains_key(side1)
                        && !self.mark_glyphs.contains_key(side2) =>
                {
                    base_pairs.push(Cow::Borrowed(*pair))
                }
                (KernSide::Glyph(_), KernSide::Glyph(_)) => mark_pairs.push(Cow::Borrowed(*pair)),

                // handle the case where all are marks or bases, first:
                (side1, side2) => {
                    let side1_cls = classify_kernside_contents(side1, &self.mark_glyphs);
                    let side2_cls = classify_kernside_contents(side2, &self.mark_glyphs);

                    match (side1_cls, side2_cls) {
                        (GlyphSetContent::Empty, _) | (_, GlyphSetContent::Empty) => continue,
                        (GlyphSetContent::BasesOnly, GlyphSetContent::BasesOnly) => {
                            base_pairs.push(Cow::Borrowed(*pair));
                            continue;
                        }
                        (GlyphSetContent::BasesOnly, GlyphSetContent::MarksOnly)
                        | (GlyphSetContent::MarksOnly, GlyphSetContent::BasesOnly)
                        | (GlyphSetContent::MarksOnly, GlyphSetContent::MarksOnly) => {
                            mark_pairs.push(Cow::Borrowed(*pair));
                            continue;
                        }
                        (GlyphSetContent::Mixed, _) | (_, GlyphSetContent::Mixed) => {
                            let (side1_bases, side1_marks) = split_glyphs(side1, &self.mark_glyphs);
                            let (side2_bases, side2_marks) = split_glyphs(side2, &self.mark_glyphs);

                            if !side1_bases.is_empty() && !side2_bases.is_empty() {
                                base_pairs.push(Cow::Owned(KernPair {
                                    side1: side1_bases.clone(),
                                    value: pair.value.clone(),
                                    side2: side2_bases.clone(),
                                }));
                            }
                            // these various combos all go in the marks group
                            for (side1, side2) in [
                                (&side1_bases, &side2_marks),
                                (&side1_marks, &side2_bases),
                                (&side1_marks, &side2_marks),
                            ] {
                                if !side1.is_empty() && !side2.is_empty() {
                                    mark_pairs.push(Cow::Owned(KernPair {
                                        side1: side1.clone(),
                                        value: pair.value.clone(),
                                        side2: side2.clone(),
                                    }));
                                }
                            }
                        }
                    }
                }
            }
        }
        (base_pairs, mark_pairs)
    }
}

impl FeatureProvider for FeaRsKerns {
    fn add_features(&self, builder: &mut fea_rs::compile::FeatureBuilder) {
        if self.is_empty() {
            return;
        }
        // convert the lookups into lookup ids
        let lookup_ids = self
            .lookups
            .iter()
            .map(|lookup| builder.add_lookup(lookup.clone()))
            .collect::<Vec<_>>();

        for (feature, ids) in &self.features {
            // get the generated lookup ids based on the stored lookup indices
            let ids = ids.iter().map(|idx| lookup_ids[*idx]).collect();
            builder.add_feature(*feature, ids);
        }
    }
}

// <https://github.com/googlefonts/ufo2ft/blob/cea60d71dfcf0b1c/Lib/ufo2ft/featureWriters/baseFeatureWriter.py#L401>
fn guess_font_scripts(
    ast: &ParseTree,
    glyphs: &HashMap<u32, GlyphId16>,
) -> HashSet<UnicodeShortName> {
    let mut scripts = scripts_for_chars(glyphs);
    // add scripts explicitly defined in fea
    scripts.extend(super::get_script_language_systems(ast).keys().cloned());
    scripts
}

/// return the set of scripts (based on unicode data) that use this set of glyphs
fn scripts_for_chars(glyphs: &HashMap<u32, GlyphId16>) -> HashSet<UnicodeShortName> {
    glyphs
        .iter_glyphs()
        .filter_map(|(_, codepoint)| super::properties::single_script_for_codepoint(codepoint))
        .collect()
}

// <https://github.com/googlefonts/ufo2ft/blob/f6b4f42460b340c/Lib/ufo2ft/featureWriters/kernFeatureWriter.py#L946>
fn merge_scripts(
    kerning_per_script: HashMap<BTreeSet<UnicodeShortName>, Vec<KernPair>>,
) -> HashMap<BTreeSet<UnicodeShortName>, Vec<KernPair>> {
    let mut sets = kerning_per_script.keys().cloned().collect::<Vec<_>>();
    let mut buf = Vec::with_capacity(sets.len());
    let mut did_merge = true;

    while did_merge {
        did_merge = false;
        while let Some(mut common) = sets.pop() {
            sets.retain(|scripts| {
                if scripts.is_disjoint(&common) {
                    true
                } else {
                    common.extend(scripts.iter().copied());
                    did_merge = true;
                    false
                }
            });
            buf.push(common);
        }
        // all items from sets were moved into buf; now move them back
        std::mem::swap(&mut sets, &mut buf);
        assert!(buf.is_empty());
    }

    let mut result = sets
        .iter()
        .map(|set| (set.clone(), Vec::new()))
        .collect::<HashMap<_, _>>();

    for (scripts, pairs) in kerning_per_script {
        let merged_script = sets
            .iter()
            .find(|merged| !merged.is_disjoint(&scripts))
            .unwrap();
        result.get_mut(merged_script).unwrap().extend(pairs);
    }

    // sort all the pairs; fonttools does that after returning but here works?
    result.values_mut().for_each(|pairs| pairs.sort_unstable());
    result
}

#[cfg(test)]
mod tests {

    use fontir::ir::GdefCategories;
    use write_fonts::read::FontRead;

    use crate::features::test_helpers::LayoutOutputBuilder;

    use super::*;

    // just a helper so we can use the same names as fonttools does in their tests
    fn glyph_name_for_char(c: char) -> GlyphName {
        static EXTRA_GLYPH_NAMES: &[(char, &str)] = &[
            ('\u{302}', "circumflexcomb"),
            ('\u{0430}', "a-cy"),
            ('\u{0431}', "be-cy"),
            ('\u{627}', "alef-ar"),
            ('\u{631}', "reh-ar"),
            ('\u{632}', "zain-ar"),
            ('\u{644}', "lam-ar"),
            ('\u{64E}', "fatha-ar"),
            ('\u{664}', "four-ar"),
            ('\u{667}', "seven-ar"),
            ('\u{CBE}', "aaMatra_kannada"),
            ('\u{CD6}', "ailength_kannada"),
            ('\u{10A06}', "u10A06"),
            ('\u{10A1E}', "u10A1E"),
        ];
        EXTRA_GLYPH_NAMES
            .binary_search_by(|probe| probe.0.cmp(&c))
            .map(|idx| EXTRA_GLYPH_NAMES[idx].1)
            .ok()
            .or_else(|| fontdrasil::agl::agl_name_for_char(c))
            .unwrap()
            .into()
    }

    struct KernInput {
        charmap: HashMap<u32, GlyphId16>,
        pairs: Vec<KernPair>,
        non_spacing: HashSet<GlyphId16>,
        opentype_categories: BTreeMap<GlyphName, GlyphClassDef>,
        glyph_order: GlyphOrder,
        user_fea: &'static str,
    }

    trait ToKernSide {
        fn to_kern_side(&self, input: &KernInput) -> KernSide;
    }

    impl ToKernSide for char {
        fn to_kern_side(&self, input: &KernInput) -> KernSide {
            KernSide::Glyph(*input.charmap.get(&(*self as u32)).unwrap())
        }
    }

    impl ToKernSide for &str {
        fn to_kern_side(&self, input: &KernInput) -> KernSide {
            KernSide::Glyph(input.glyph_order.glyph_id(*self).unwrap())
        }
    }

    impl<const N: usize> ToKernSide for [char; N] {
        fn to_kern_side(&self, input: &KernInput) -> KernSide {
            KernSide::Group(
                self.iter()
                    .map(|c| *input.charmap.get(&(*c as u32)).unwrap())
                    .collect(),
            )
        }
    }

    impl<const N: usize> ToKernSide for [&'static str; N] {
        fn to_kern_side(&self, input: &KernInput) -> KernSide {
            KernSide::Group(
                self.iter()
                    .map(|c| input.glyph_order.glyph_id(*c).unwrap())
                    .collect(),
            )
        }
    }

    impl KernInput {
        fn new(chars: &[char]) -> Self {
            let charmap = chars
                .iter()
                .enumerate()
                .map(|(i, chr)| {
                    let gid = GlyphId16::new((i + 1) as _);
                    (*chr as u32, gid)
                })
                .collect();

            let glyph_order = std::iter::once(GlyphName::NOTDEF)
                .chain(chars.iter().map(|c| glyph_name_for_char(*c)))
                .collect();

            Self {
                charmap,
                glyph_order,
                pairs: Default::default(),
                non_spacing: Default::default(),
                user_fea: "",
                opentype_categories: Default::default(),
            }
        }

        fn with_user_fea(mut self, fea: &'static str) -> Self {
            self.user_fea = fea;
            self
        }

        fn with_opentype_category_marks(mut self, mark_glyphs: &[char]) -> Self {
            self.opentype_categories = mark_glyphs
                .iter()
                .map(|c| {
                    (
                        self.glyph_order
                            .glyph_name(self.charmap.get(&(*c as u32)).copied().unwrap().into())
                            .unwrap()
                            .to_owned(),
                        GlyphClassDef::Mark,
                    )
                })
                .collect();
            self
        }

        // manually indicate some glyphs are 'nonspacing'. In real code, this is
        // determined by checking the glyohs' advance widths.
        fn with_nonspacing_glyphs(mut self, glyphs: &[char]) -> Self {
            self.non_spacing.extend(
                glyphs
                    .iter()
                    .map(|c| self.charmap.get(&(*c as u32)).unwrap()),
            );
            self
        }

        /// Adds glyph names that don't have unicode values associated
        ///
        /// used to test that we figure out BIDI & script correctly
        fn with_unmapped_glyphs<const N: usize>(mut self, names: [&str; N]) -> Self {
            self.glyph_order
                .extend(names.into_iter().map(GlyphName::new));
            self
        }

        fn with_rule(mut self, side1: impl ToKernSide, side2: impl ToKernSide, val: i16) -> Self {
            let side1 = side1.to_kern_side(&self);
            let side2 = side2.to_kern_side(&self);
            self.pairs.push(KernPair {
                side1,
                side2,
                value: ValueRecordBuilder::new().with_x_advance(val),
            });
            self
        }

        /// Returns the raw lookups/features as well as otl-normalizer output
        fn build(self) -> (FeaRsKerns, String) {
            let pairs = self.pairs.iter().collect::<Vec<_>>();
            let categories = GdefCategories {
                prefer_gdef_categories_in_fea: self.opentype_categories.is_empty(),
                categories: self.opentype_categories,
            };
            let layout_output = LayoutOutputBuilder::new()
                .with_categories(categories)
                .with_user_fea(self.user_fea)
                .with_glyph_order(self.glyph_order.clone())
                .build();
            let kerns = finalize_kerning(
                &pairs,
                &layout_output.first_pass_fea,
                &layout_output.static_metadata,
                &self.glyph_order,
                self.charmap,
                self.non_spacing,
            )
            .unwrap();

            let comp = layout_output.compile(&kerns);
            let gpos_bytes = write_fonts::dump_table(comp.gpos.as_ref().unwrap()).unwrap();
            let gpos =
                write_fonts::read::tables::gpos::Gpos::read(gpos_bytes.as_slice().into()).unwrap();
            let mut buf = Vec::new();
            let names = self.glyph_order.names().cloned().collect();
            otl_normalizer::print_gpos(&mut buf, &gpos, None, &names).unwrap();
            let norm_out = String::from_utf8(buf).unwrap();
            (kerns, norm_out)
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

    const ACUTE_COMB: char = '\u{0301}';
    const CIRCUM_COMB: char = '\u{302}';
    const KERN_DFLT_DFLT: FeatureKey = FeatureKey::new(KERN, DFLT_LANG, DFLT_SCRIPT);
    const KERN_LATN_DFLT: FeatureKey = FeatureKey::new(KERN, DFLT_LANG, Tag::new(b"latn"));
    const KERN_CYRL_DFLT: FeatureKey = FeatureKey::new(KERN, DFLT_LANG, Tag::new(b"cyrl"));

    #[test]
    fn split_latin_and_cyrillic() {
        const A_CY: char = 'а';
        const BE_CY: char = 'б';
        let kerns = KernInput::new(&['a', 'b', A_CY, BE_CY])
            .with_rule('a', 'b', 5)
            .with_rule('a', 'a', 7)
            .with_rule(A_CY, BE_CY, 12)
            .build()
            .0;

        assert_eq!(
            kerns.features.keys().cloned().collect::<Vec<_>>(),
            [KERN_DFLT_DFLT, KERN_CYRL_DFLT, KERN_LATN_DFLT]
        );

        assert_eq!(
            kerns
                .lookups_for_feature(&KERN_LATN_DFLT)
                .iter()
                .map(|lk| flags_and_rule_count(lk).1)
                .sum::<usize>(),
            2
        );
        assert_eq!(
            kerns
                .lookups_for_feature(&KERN_CYRL_DFLT)
                .iter()
                .map(|lk| flags_and_rule_count(lk).1)
                .sum::<usize>(),
            1
        );
    }

    fn flags_and_rule_count(lookup: &PendingLookup<PairPosBuilder>) -> (LookupFlag, usize) {
        (
            lookup.flags(),
            lookup
                .subtables()
                .iter()
                .map(|sub| sub.len())
                .sum::<usize>(),
        )
    }

    #[test]
    fn mark_to_base_kern() {
        let kerns = KernInput::new(&['A', 'B', 'C', ACUTE_COMB])
            .with_nonspacing_glyphs(&[ACUTE_COMB])
            //FIXME: there's no way this should only work with explicit FEA
            .with_user_fea("table GDEF { GlyphClassDef [A B C], , [acutecomb], ; } GDEF;")
            .with_rule('A', ACUTE_COMB, -55)
            .with_rule('B', 'C', -30)
            .with_rule('A', 'C', -30)
            .build()
            .0;
        assert_eq!(
            kerns.features.keys().cloned().collect::<Vec<_>>(),
            [KERN_DFLT_DFLT, KERN_LATN_DFLT]
        );

        assert_eq!(kerns.lookups.len(), 2);

        let bases = &kerns.lookups[0];
        let marks = &kerns.lookups[1];

        assert_eq!(
            (flags_and_rule_count(bases), flags_and_rule_count(marks)),
            ((LookupFlag::IGNORE_MARKS, 2), (LookupFlag::empty(), 1)),
        );
    }

    #[test]
    fn mark_to_base_kern_no_fea() {
        let kerns = KernInput::new(&['A', 'B', 'C', ACUTE_COMB])
            .with_nonspacing_glyphs(&[ACUTE_COMB])
            .with_opentype_category_marks(&[ACUTE_COMB])
            .with_rule('A', ACUTE_COMB, -55)
            .with_rule('B', 'C', -30)
            .with_rule('A', 'C', -30)
            .build()
            .0;
        assert_eq!(
            kerns.features.keys().cloned().collect::<Vec<_>>(),
            [KERN_DFLT_DFLT, KERN_LATN_DFLT]
        );

        assert_eq!(kerns.lookups.len(), 2);

        let bases = &kerns.lookups[0];
        let marks = &kerns.lookups[1];

        assert_eq!(
            (flags_and_rule_count(bases), flags_and_rule_count(marks)),
            ((LookupFlag::IGNORE_MARKS, 2), (LookupFlag::empty(), 1)),
        );
    }

    #[test]
    fn mark_to_base_only() {
        let kerns = KernInput::new(&['A', 'B', 'C', ACUTE_COMB])
            .with_nonspacing_glyphs(&[ACUTE_COMB])
            .with_user_fea("table GDEF { GlyphClassDef [A B C], , [acutecomb], ; } GDEF;")
            .with_rule('A', ACUTE_COMB, -55)
            .build()
            .0;

        let lookups = kerns.lookups_for_feature(&KERN_LATN_DFLT);
        assert_eq!(lookups.len(), 1);
        assert_eq!(flags_and_rule_count(lookups[0]), (LookupFlag::empty(), 1));
    }

    #[test]
    fn mark_to_base_mixed_class() {
        let kerns = KernInput::new(&['A', 'B', 'C', ACUTE_COMB, CIRCUM_COMB])
            .with_nonspacing_glyphs(&[ACUTE_COMB, CIRCUM_COMB])
            .with_user_fea(
                "table GDEF { GlyphClassDef [A B C], , [acutecomb, circumflexcomb], ; } GDEF;",
            )
            .with_rule('A', 'A', 12)
            .with_rule(['A', 'B'], [ACUTE_COMB, CIRCUM_COMB, 'C'], -55)
            .build()
            .0;

        let lookups = kerns.lookups_for_feature(&KERN_LATN_DFLT);
        assert_eq!(kerns.lookups.len(), 2);

        let bases = &lookups[0];
        let marks = &lookups[1];

        assert_eq!(
            (flags_and_rule_count(bases), flags_and_rule_count(marks)),
            ((LookupFlag::IGNORE_MARKS, 2), (LookupFlag::empty(), 1)),
        );
    }

    #[test]
    fn mark_to_base_mixed_class_no_fea() {
        let kerns = KernInput::new(&['A', 'B', 'C', ACUTE_COMB, CIRCUM_COMB])
            .with_nonspacing_glyphs(&[ACUTE_COMB, CIRCUM_COMB])
            .with_opentype_category_marks(&[ACUTE_COMB, CIRCUM_COMB])
            .with_rule('A', 'A', 12)
            .with_rule(['A', 'B'], [ACUTE_COMB, CIRCUM_COMB, 'C'], -55)
            .build()
            .0;

        let lookups = kerns.lookups_for_feature(&KERN_LATN_DFLT);
        assert_eq!(kerns.lookups.len(), 2);

        let bases = &lookups[0];
        let marks = &lookups[1];

        assert_eq!(
            (flags_and_rule_count(bases), flags_and_rule_count(marks)),
            ((LookupFlag::IGNORE_MARKS, 2), (LookupFlag::empty(), 1)),
        );
    }

    const FOUR_AR: char = '\u{0664}';
    const SEVEN_AR: char = '\u{0667}';
    const ALEF_AR: char = '\u{00627}';

    //https://github.com/googlefonts/ufo2ft/blob/d82816873ed/tests/featureWriters/kernFeatureWriter_test.py#L548
    #[test]
    fn arabic_numerals_1() {
        let kerns = KernInput::new(&[FOUR_AR, SEVEN_AR])
            .with_rule(FOUR_AR, SEVEN_AR, -30)
            .build()
            .0;

        assert_eq!(kerns.features.len(), 1);
        assert_eq!(kerns.lookups.len(), 1);

        let lookups = kerns.lookups_for_feature(&KERN_DFLT_DFLT);
        assert_eq!(
            flags_and_rule_count(lookups[0]),
            (LookupFlag::IGNORE_MARKS, 1)
        );
    }

    const KERN_ARAB_DFLT: FeatureKey = FeatureKey::new(KERN, DFLT_LANG, Tag::new(b"arab"));
    // in fonttools, part of the same test as arabic_numerals_1, but we
    // split it up for legibility
    #[test]
    fn arabic_numerals_2() {
        let kerns = KernInput::new(&[FOUR_AR, SEVEN_AR, ALEF_AR])
            .with_rule(FOUR_AR, SEVEN_AR, -30)
            .build()
            .0;
        assert_eq!(kerns.lookups.len(), 1);
        assert_eq!(
            kerns.lookups_for_feature(&KERN_DFLT_DFLT),
            kerns.lookups_for_feature(&KERN_ARAB_DFLT)
        );
    }

    // ensure the lookup gets registered for all languagesystems
    #[test]
    fn arabic_numerals_3() {
        const KERN_THAA_DFLT: FeatureKey = FeatureKey::new(KERN, DFLT_LANG, Tag::new(b"thaa"));
        let kerns = KernInput::new(&[FOUR_AR, SEVEN_AR, ALEF_AR])
            .with_user_fea("languagesystem DFLT dflt; languagesystem Thaa dflt;")
            .with_rule(FOUR_AR, SEVEN_AR, -30)
            .build()
            .0;
        assert_eq!(kerns.lookups.len(), 1);
        assert_eq!(
            kerns.features.keys().collect::<Vec<_>>(),
            [&KERN_DFLT_DFLT, &KERN_ARAB_DFLT, &KERN_THAA_DFLT]
        );
    }

    //https://github.com/googlefonts/ufo2ft/blob/d82816873/tests/featureWriters/kernFeatureWriter_test.py#L659
    #[test]
    fn skip_zero_class_kerns() {
        let kerns = KernInput::new(&['A', 'B', 'C', 'D', 'E', 'F', 'G', 'H'])
            .with_rule(['A', 'B'], ['C', 'D'], 10)
            .with_rule(['E', 'F'], ['C', 'D'], -10)
            .with_rule(['A', 'B'], 'D', 15)
            .with_rule('A', ['C', 'D'], 5)
            .with_rule('G', 'H', -5)
            .with_rule(['A', 'B'], ['G', 'H'], 0)
            .build()
            .1;

        assert_eq_ignoring_ws!(
            kerns,
            r#"
            # kern: DFLT/dflt, latn/dflt
            # 6 PairPos rules
            # lookupflag LookupFlag(8)
            A 5 [C,D]
            B 10 C
            B 15 D
            E -10 [C,D]
            F -10 [C,D]
            G -5 H
            "#
        );
    }

    #[test]
    fn skip_zero_class_kerns_actually_though() {
        let (kerns, normalized) = KernInput::new(&['A', 'B', 'C', 'D', 'E', 'F'])
            .with_rule('A', 'B', 10)
            .with_rule(['C', 'D'], ['E', 'F'], 0)
            .build();

        // we can't just use normalizer because it erases the difference
        // (since it's a noop)
        assert_eq!(kerns.lookups.len(), 1);
        let lookup = &kerns.lookups[0];
        assert_eq!(lookup.subtables().len(), 1);
        let subtable = &lookup.subtables()[0];
        assert_eq!(subtable.len(), 1);

        assert_eq_ignoring_ws!(
            normalized,
            r#"
        # kern: DFLT/dflt, latn/dflt
        # 1 PairPos rules
        # lookupflag LookupFlag(8)
        A 10 B
        "#
        );
    }

    //https://github.com/googlefonts/ufo2ft/blob/d82816873ed40a8/tests/featureWriters/kernFeatureWriter_test.py#L717
    #[test]
    fn kern_uniqueness() {
        const QUESTION_DOWN: char = '\u{BF}';

        let kerns = KernInput::new(&[QUESTION_DOWN, 'y'])
            .with_rule([QUESTION_DOWN], ['y'], 15)
            .with_rule([QUESTION_DOWN], 'y', 35)
            .with_rule(QUESTION_DOWN, ['y'], -35)
            .with_rule(QUESTION_DOWN, 'y', 10)
            .build()
            .1;

        assert_eq_ignoring_ws!(
            kerns,
            r#"
            # kern: DFLT/dflt, latn/dflt
            # 1 PairPos rules
            # lookupflag LookupFlag(8)
            questiondown 10 y
            "#
        );
    }

    const AACUTE: char = '\u{C1}';
    const REH_AR: char = '\u{631}';
    const ZAIN_AR: char = '\u{632}';
    const LAM_AR: char = '\u{644}';

    //https://github.com/googlefonts/ufo2ft/blob/d82816873ed40a801515/tests/featureWriters/kernFeatureWriter_test.py#L765
    #[test]
    fn kern_ltr_and_rtl() {
        let (_kerns, normalized) = KernInput::new(&[
            '4', '7', 'A', 'V', AACUTE, ALEF_AR, REH_AR, ZAIN_AR, LAM_AR, FOUR_AR, SEVEN_AR,
        ])
        .with_unmapped_glyphs(["alef-ar.isol", "lam-ar.init", "reh-ar.fina"])
        .with_user_fea(
            "
            languagesystem DFLT dflt;
            languagesystem latn dflt;
            languagesystem latn TRK;
            languagesystem arab dflt;
            languagesystem arab URD;

            feature init {
                script arab;
                sub lam-ar by lam-ar.init;
                language URD;
            } init;

            feature fina {
                script arab;
                sub reh-ar by reh-ar.fina;
                language URD;
            } fina;

            feature isol {
                script arab;
                sub alef-ar by alef-ar.isol;
            } isol;
                ",
        )
        .with_rule(['A', AACUTE], 'V', -40)
        .with_rule('7', '4', -25)
        .with_rule("reh-ar.fina", "lam-ar.init", -80)
        .with_rule(
            ["reh-ar", "zain-ar", "reh-ar.fina"],
            ["alef-ar", "alef-ar.isol"],
            -100,
        )
        .with_rule(FOUR_AR, SEVEN_AR, -30)
        .build();

        assert_eq_ignoring_ws!(
            normalized,
            r#"
            # kern: DFLT/dflt, latn/dflt, latn/TRK
            # 3 PairPos rules
            # lookupflag LookupFlag(8)
            seven -25 four
            A -40 V
            Aacute -40 V

            # kern: arab/dflt, arab/URD
            # 6 PairPos rules
            # lookupflag LookupFlag(8)
            seven -25 four
            reh-ar <-100 0 -100 0> [alef-ar, alef-ar.isol]
            zain-ar <-100 0 -100 0> [alef-ar, alef-ar.isol]
            four-ar -30 seven-ar
            reh-ar.fina <-100 0 -100 0> [alef-ar, alef-ar.isol]
            reh-ar.fina <-80 0 -80 0> lam-ar.init
            "#
        );
    }

    const FATHA_AR: char = '\u{064E}';

    //https://github.com/googlefonts/ufo2ft/blob/d82816873ed40a801515/tests/featureWriters/kernFeatureWriter_test.py#L869
    #[test]
    fn kern_ltr_and_rtl_with_marks() {
        let (_kerns, normalized) = KernInput::new(&[
            '4', '7', 'A', 'V', AACUTE, ACUTE_COMB, ALEF_AR, REH_AR, ZAIN_AR, LAM_AR, FOUR_AR,
            SEVEN_AR, FATHA_AR,
        ])
        .with_unmapped_glyphs(["alef-ar.isol", "lam-ar.init", "reh-ar.fina"])
        .with_user_fea(
            "
            languagesystem DFLT dflt;
            languagesystem latn dflt;
            languagesystem latn TRK;
            languagesystem arab dflt;
            languagesystem arab URD;

            feature init {
                script arab;
                sub lam-ar by lam-ar.init;
                language URD;
            } init;

            feature fina {
                script arab;
                sub reh-ar by reh-ar.fina;
                language URD;
            } fina;

            feature isol {
                script arab;
                sub alef-ar by alef-ar.isol;
            } isol;

            @Bases = [A V Aacute alef-ar reh-ar zain-ar lam-ar
                      alef-ar.isol lam-ar.init reh-ar.fina];
            @Marks = [acutecomb fatha-ar];
            table GDEF {
                GlyphClassDef @Bases, [], @Marks, ;
            } GDEF;
                ",
        )
        .with_nonspacing_glyphs(&[ACUTE_COMB, FATHA_AR])
        .with_rule(['A', AACUTE], 'V', -40)
        .with_rule('7', '4', -25)
        .with_rule("reh-ar.fina", "lam-ar.init", -80)
        .with_rule(
            ["reh-ar", "zain-ar", "reh-ar.fina"],
            ["alef-ar", "alef-ar.isol"],
            -100,
        )
        .with_rule(FOUR_AR, SEVEN_AR, -30)
        .with_rule('V', ACUTE_COMB, 70)
        .with_rule("reh-ar", "fatha-ar", 80)
        .build();

        assert_eq_ignoring_ws!(
            normalized,
            r#"
            # kern: DFLT/dflt, latn/dflt, latn/TRK
            # 4 PairPos rules
            # lookupflag LookupFlag(8)
            seven -25 four
            A -40 V
            # lookupflag LookupFlag(0)
            V 70 acutecomb
            # lookupflag LookupFlag(8)
            Aacute -40 V

            # kern: arab/dflt, arab/URD
            # 7 PairPos rules
            # lookupflag LookupFlag(8)
            seven -25 four
            reh-ar <-100 0 -100 0> [alef-ar, alef-ar.isol]
            # lookupflag LookupFlag(0)
            reh-ar <80 0 80 0> fatha-ar
            # lookupflag LookupFlag(8)
            zain-ar <-100 0 -100 0> [alef-ar, alef-ar.isol]
            four-ar -30 seven-ar
            reh-ar.fina <-100 0 -100 0> [alef-ar, alef-ar.isol]
            reh-ar.fina <-80 0 -80 0> lam-ar.init
            "#
        );
    }

    const AAMATRA_KANNADA: char = '\u{0CBE}';
    const AILENGTH_KANNADA: char = '\u{0CD6}';

    #[test]
    fn dist_ltr() {
        let (_kerns, normalized) = KernInput::new(&[AAMATRA_KANNADA, AILENGTH_KANNADA])
            .with_user_fea(
                "
            languagesystem DFLT dflt;
            languagesystem latn dflt;
            languagesystem knda dflt;
            languagesystem knd2 dflt;
                ",
            )
            .with_rule([AAMATRA_KANNADA], [AILENGTH_KANNADA], 34)
            .build();

        assert_eq_ignoring_ws!(
            normalized,
            r#"
            # dist: knd2/dflt, knda/dflt
            # 1 PairPos rules
            # lookupflag LookupFlag(8)
            aaMatra_kannada 34 ailength_kannada
            "#
        );
    }

    const U10A1E: char = '\u{10A1E}';
    const U10A06: char = '\u{10A06}';
    #[test]
    fn dist_rtl() {
        let (_kerns, normalized) = KernInput::new(&[U10A06, U10A1E])
            .with_user_fea(
                "
            languagesystem DFLT dflt;
            languagesystem arab dflt;
            languagesystem khar dflt;
                ",
            )
            .with_rule(U10A1E, U10A06, 117)
            .build();

        assert_eq_ignoring_ws!(
            normalized,
            r#"
            # dist: khar/dflt
            # 1 PairPos rules
            # lookupflag LookupFlag(8)
            u10A1E <117 0 117 0> u10A06
            "#
        );
    }

    #[test]
    fn dist_ltr_and_rtl() {
        let (_kerns, normalized) =
            KernInput::new(&[AAMATRA_KANNADA, AILENGTH_KANNADA, U10A06, U10A1E])
                .with_user_fea(
                    "
            languagesystem DFLT dflt;
            languagesystem knda dflt;
            languagesystem knd2 dflt;
            languagesystem khar dflt;
                ",
                )
                .with_rule([AAMATRA_KANNADA], [AILENGTH_KANNADA], 34)
                .with_rule(U10A1E, U10A06, 117)
                .build();

        assert_eq_ignoring_ws!(
            normalized,
            r#"
            # dist: khar/dflt
            # 1 PairPos rules
            # lookupflag LookupFlag(8)
            u10A1E <117 0 117 0> u10A06

            # dist: knd2/dflt, knda/dflt
            # 1 PairPos rules
            # lookupflag LookupFlag(8)
            aaMatra_kannada 34 ailength_kannada
            "#
        );
    }

    #[test]
    fn prefer_user_fea() {
        let (_kerns, normalized) = KernInput::new(&['a', 'b', 'c', 'd'])
            .with_user_fea(
                r#"
                feature kern {
                    lookupflag IgnoreMarks;
                    pos a b 20;
                    pos a c 22;
                } kern;
                "#,
            )
            .with_rule('c', 'd', -5)
            .build();

        assert_eq_ignoring_ws!(
            normalized,
            r#"
            # kern: DFLT/dflt
            # 2 PairPos rules
            # lookupflag LookupFlag(8)
            a 20 b
            a 22 c
            "#
        );
    }

    // we had a bug where we were updating the kerning values in place, which
    // meant the order in which we handled pairs could influence the results
    #[test]
    fn alignment_determinism() {
        let g1 = GlyphName::new("a");
        let g2 = GlyphName::new("b");
        let side1 = ir::KernGroup::Side1("aa".into());
        let side2 = ir::KernGroup::Side2("bb".into());
        let side1_glyphs = HashMap::from([(&g1, &side1)]);
        let side2_glyphs = HashMap::from([(&g2, &side2)]);

        let glyph_glyph: ir::KernPair = (g1.clone().into(), g2.clone().into());
        let glyph_group: ir::KernPair = (g1.clone().into(), side2.clone().into());
        let group_glyph: ir::KernPair = (side1.clone().into(), g2.clone().into());
        let group_group: ir::KernPair = (side1.clone().into(), side2.clone().into());

        let all_pairs = HashSet::from([
            glyph_glyph.clone(),
            glyph_group.clone(),
            group_glyph.clone(),
            group_group.clone(),
        ]);
        let mut kerns = BTreeMap::new();
        kerns.insert(group_group.clone(), OrderedFloat::from(-70.));
        kerns.insert(group_glyph.clone(), 10.0.into());
        // explanation:
        // we need to align glyph_glyph and glyph_group.
        // - if we do glyph_group first, we will use the group_group value of
        //   -70, and then when we do glyph_glyph we will use this value, since
        //   glyph_group is preferred to group_glyph
        // - but if we do glyph_glyph first, we will use the value from
        //   group_glyph, which is set.

        // run a few times because triggering depended on hashmap iteration order
        for _ in 0..20 {
            align_instance(&all_pairs, &mut kerns, &side1_glyphs, &side2_glyphs);
            assert_eq!(kerns.get(&glyph_glyph).map(|x| x.0), Some(10.0f64));
        }
    }

    #[test]
    // https://github.com/googlefonts/fontc/issues/1121
    fn default_language_systems() {
        let fea = "\
            languagesystem DFLT dflt;
            languagesystem DFLT MAH;";
        let (ast, errs) = fea_rs::parse::parse_string(fea);
        assert!(errs.is_empty());
        // make one dummy lookup
        let script = UnicodeShortName::try_from_str("Latn").unwrap();
        let lookups = BTreeMap::from([(script, vec![1])]);
        let features = assign_lookups_to_scripts(lookups, &ast, KERN);
        let dflt_mah = FeatureKey::new(KERN, Tag::new(b"MAH "), DFLT_SCRIPT);
        // ensure that the feature was registered for the DFLT/Mah language system
        assert!(features.contains_key(&dflt_mah));
    }
}
