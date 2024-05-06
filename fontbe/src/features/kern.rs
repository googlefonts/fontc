//! Generates an [FeaRsKerns] datastructure to be fed to fea-rs

use std::{
    borrow::Cow,
    collections::{BTreeMap, BTreeSet, HashMap, HashSet},
    sync::Arc,
};

use fea_rs::{
    compile::{
        FeatureKey, NopFeatureProvider, NopVariationInfo, PairPosBuilder,
        ValueRecord as ValueRecordBuilder,
    },
    typed::{AstNode, LanguageSystem},
    GlyphMap, GlyphSet, Opts, ParseTree,
};
use fontdrasil::{
    coords::NormalizedLocation,
    orchestration::{Access, AccessBuilder, Work},
    types::GlyphName,
};
use fontir::{
    ir::{self, Glyph, KernGroup, KerningGroups, KerningInstance},
    orchestration::WorkId as FeWorkId,
};
use icu_properties::BidiClass;
use log::debug;
use ordered_float::OrderedFloat;
use write_fonts::{
    read::{tables::gsub::Gsub, FontRead, ReadError},
    tables::{gdef::GlyphClassDef, layout::LookupFlag},
    types::{GlyphId, Tag},
};

use crate::{
    error::Error,
    features::{
        properties::{ScriptDirection, UnicodeShortName, COMMON_SCRIPT, INHERITED_SCRIPT},
        resolve_variable_metric,
    },
    orchestration::{
        AllKerningPairs, AnyWorkId, BeWork, Context, FeaRsKerns, KernAdjustments, KernFragment,
        KernPair, KernSide, WorkId,
    },
};

use super::{properties::CharMap, PendingLookup};

/// On Linux it took ~0.01 ms per loop, try to get enough to make fan out worthwhile
/// based on empirical testing
const KERNS_PER_BLOCK: usize = 2048;
const KERN: Tag = Tag::new(b"kern");
// we don't currently compile this feature, but we will, and it is referenced
// in places because our impl is based on fonttools.
const DIST: Tag = Tag::new(b"dist");
const DFLT_SCRIPT: Tag = Tag::new(b"DFLT");
const DFLT_LANG: Tag = Tag::new(b"dflt");

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
fn align_kerning(
    groups: &KerningGroups,
    instances: &mut HashMap<NormalizedLocation, KerningInstance>,
) {
    let union_kerning = instances
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
        let missing_pairs = union_kerning
            .iter()
            .filter(|pair| !instance.kerns.contains_key(pair))
            .collect::<Vec<_>>();

        for pair in missing_pairs {
            let value = lookup_kerning_value(
                pair,
                instance,
                &side1_glyph_to_group_map,
                &side2_glyph_to_group_map,
            );
            instance.kerns.insert(pair.to_owned(), value);
        }
    }
}

// <https://github.com/fonttools/fonttools/blob/a3b9eddcafca/Lib/fontTools/ufoLib/kerning.py#L1>
fn lookup_kerning_value(
    pair: &ir::KernPair,
    kerning: &KerningInstance,
    side1_glyphs: &HashMap<&GlyphName, &KernGroup>,
    side2_glyphs: &HashMap<&GlyphName, &KernGroup>,
) -> OrderedFloat<f32> {
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
            if let Some(value) = kerning.kerns.get(&pair) {
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
        let glyph_map = glyph_order.iter().cloned().collect();
        let mut fragments: Vec<_> = arc_fragments
            .iter()
            .map(|(_, fragment)| fragment.as_ref())
            .collect();
        fragments.sort_by_key(|fragment| fragment.segment);

        let glyphs_and_gids = glyph_order
            .iter()
            .enumerate()
            .map(|(i, glyphname)| {
                (
                    context.ir.glyphs.get(&FeWorkId::Glyph(glyphname.clone())),
                    GlyphId::new(i as u16),
                )
            })
            .collect::<Vec<_>>();
        let lookups = self.finalize_kerning(&fragments, &ast.ast, &glyph_map, glyphs_and_gids)?;
        context.fea_rs_kerns.set(lookups);
        Ok(())
    }
}

impl KerningGatherWork {
    //. take the kerning fragments and generate the kerning lookups.
    //
    // This includes much of the logic from the ufo2ft KernFeatureWriter
    fn finalize_kerning(
        &self,
        fragments: &[&KernFragment],
        ast: &ParseTree,
        glyph_map: &GlyphMap,
        glyphs: Vec<(Arc<Glyph>, GlyphId)>,
    ) -> Result<FeaRsKerns, Error> {
        // ignore diagnostics, they'll get logged during actual GSUB compilation
        let (compilation, _) = fea_rs::compile::compile::<NopVariationInfo, NopFeatureProvider>(
            ast,
            glyph_map,
            None,
            None,
            Opts::new().compile_gpos(false),
        )
        .map_err(|err| {
            Error::FeaCompileError(fea_rs::compile::error::CompilerError::CompilationFail(err))
        })?;

        let gsub = compilation
            .gsub
            .as_ref()
            .map(write_fonts::dump_table)
            .transpose()
            .expect("if this doesn't compile we will already panic when we try to add it to the context");
        let gsub = gsub
            .as_ref()
            .map(|data| write_fonts::read::tables::gsub::Gsub::read(data.as_slice().into()))
            .transpose()?;

        let gdef = compilation.gdef_classes;

        let mut pairs = fragments
            .iter()
            .flat_map(|frag| frag.kerns.iter())
            .collect::<Vec<_>>();
        pairs.sort();

        let known_scripts = guess_font_scripts(ast, &glyphs);
        let mark_glyphs = glyphs
            .iter()
            .filter_map(|(glyph, gid)| {
                let is_mark = gdef
                    .as_ref()
                    .map(|gdef| gdef.get(gid) == Some(&GlyphClassDef::Mark))
                    .unwrap_or(false);
                is_mark.then(|| {
                    let spacing = if glyph
                        .sources()
                        .values()
                        .all(|instance| instance.width != 0.0)
                    {
                        MarkSpacing::Spacing
                    } else {
                        MarkSpacing::NonSpacing
                    };
                    (*gid, spacing)
                })
            })
            .collect();
        let split_ctx = KernSplitContext::new(&glyphs, &known_scripts, gsub, mark_glyphs)?;

        let lookups = split_ctx.make_lookups(&pairs);
        let (lookups, features) = self.assign_lookups_to_scripts(lookups, ast, KERN);
        Ok(FeaRsKerns { lookups, features })
    }

    /// returns a vec of lookups (as a vec of subtables), along with a map of features -> lookups
    /// (by order in the first vec)
    ///
    /// this based on
    /// <https://github.com/googlefonts/ufo2ft/blob/f6b4f42460b/Lib/ufo2ft/featureWriters/kernFeatureWriter.py#L772>
    fn assign_lookups_to_scripts(
        &self,
        lookups: BTreeMap<BTreeSet<UnicodeShortName>, Vec<PendingLookup<PairPosBuilder>>>,
        ast: &ParseTree,
        // one of 'kern' or 'dist'
        current_feature: Tag,
    ) -> (
        Vec<PendingLookup<PairPosBuilder>>,
        BTreeMap<FeatureKey, Vec<usize>>,
    ) {
        let dflt_langs = vec![DFLT_LANG];

        let is_kern_feature = current_feature == KERN;
        assert!(is_kern_feature || current_feature == DIST);
        let mut lookups_by_script = BTreeMap::new();
        let mut ordered_lookups = Vec::new();

        let fea_langs_by_script: BTreeMap<_, _> = get_script_language_systems(ast)
            .into_values()
            .flat_map(|x| x.into_iter())
            .collect();

        // in python this part happens earlier, as part of splitKerning.
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
        let dist_enabled_scripts = super::properties::dist_feature_enabled_scripts();
        let (mut ltr_lookups, mut rtl_lookups) = (Vec::new(), Vec::new());
        for (script, lookups) in lookups_by_script
            .iter()
            .filter(|(script, _)| !dist_enabled_scripts.contains(script))
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

        let mut features = BTreeMap::new();
        if !default_lookups.is_empty() {
            let languages = fea_langs_by_script.get(&DFLT_SCRIPT).unwrap_or(&dflt_langs);
            for lang in languages {
                features.insert(
                    FeatureKey::new(KERN, *lang, DFLT_SCRIPT),
                    default_lookups.clone(),
                );
            }
        }

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

        for (script, mut lookups) in lookups_by_script {
            lookups.extend(dflt_lookups.iter().copied());
            lookups.sort_unstable();
            lookups.dedup();

            for tag in super::properties::script_to_ot_tags(&script) {
                let languages = fea_langs_by_script.get(&tag).unwrap_or(&dflt_langs);
                for lang in languages {
                    features.insert(FeatureKey::new(KERN, *lang, tag), lookups.clone());
                }
            }
        }

        debug_ordered_lookups(&features, &ordered_lookups);
        (ordered_lookups, features)
    }
}

fn debug_ordered_lookups(
    features: &BTreeMap<FeatureKey, Vec<usize>>,
    lookups: &[PendingLookup<PairPosBuilder>],
) {
    for (i, lookup) in lookups.iter().enumerate() {
        let total_rules = lookup.subtables.iter().map(|x| x.len()).sum::<usize>();
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
    mark_glyphs: HashMap<GlyphId, MarkSpacing>,
    glyph_scripts: HashMap<GlyphId, HashSet<UnicodeShortName>>,
    bidi_glyphs: HashMap<BidiClass, HashSet<GlyphId>>,
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
        glyphs: &impl CharMap,
        known_scripts: &HashSet<UnicodeShortName>,
        gsub: Option<Gsub>,
        mark_glyphs: HashMap<GlyphId, MarkSpacing>,
    ) -> Result<Self, ReadError> {
        let glyph_scripts =
            super::properties::scripts_by_glyph(glyphs, known_scripts, gsub.as_ref())?;
        let bidi_glyphs = super::properties::glyphs_by_bidi_class(glyphs, gsub.as_ref())?;

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
        let mut bidi_buf = HashSet::new(); // we can reuse this for each pair
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
            marks: &HashMap<GlyphId, MarkSpacing>,
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
            marks: &HashMap<GlyphId, MarkSpacing>,
        ) -> (KernSide, KernSide) {
            match glyphs {
                KernSide::Glyph(gid) if !marks.contains_key(gid) => {
                    (KernSide::Glyph(*gid), KernSide::empty())
                }
                KernSide::Glyph(gid) => (KernSide::empty(), KernSide::Glyph(*gid)),
                KernSide::Group(glyphs) => {
                    let (x, y): (Vec<_>, Vec<_>) =
                        glyphs.iter().partition(|gid| !marks.contains_key(gid));
                    (KernSide::Group(x.into()), KernSide::Group(y.into()))
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

// <https://github.com/googlefonts/ufo2ft/blob/cea60d71dfcf0b1c/Lib/ufo2ft/featureWriters/baseFeatureWriter.py#L401>
fn guess_font_scripts(ast: &ParseTree, glyphs: &impl CharMap) -> HashSet<UnicodeShortName> {
    let mut scripts = scripts_for_chars(glyphs);
    // add scripts explicitly defined in fea
    scripts.extend(get_script_language_systems(ast).keys().cloned());
    scripts
}

/// return the set of scripts (based on unicode data) that use this set of glyphs
fn scripts_for_chars(glyphs: &impl CharMap) -> HashSet<UnicodeShortName> {
    glyphs
        .iter_glyphs()
        .filter_map(|(_, codepoint)| {
            let mut scripts = super::properties::unicode_script_extensions(codepoint);
            // only if a codepoint has a single script do know it is supported
            match (scripts.next(), scripts.next()) {
                (Some(script), None) => Some(script),
                _ => None,
            }
        })
        .collect()
}

// <https://github.com/googlefonts/ufo2ft/blob/cea60d71dfcf0b1c0fa4e133e/Lib/ufo2ft/featureWriters/ast.py#L23>
/// returns a map of unicode script names to (ot_script, `[ot_lang]`)
fn get_script_language_systems(ast: &ParseTree) -> HashMap<UnicodeShortName, Vec<(Tag, Vec<Tag>)>> {
    let mut languages_by_script = HashMap::new();
    for langsys in ast
        .typed_root()
        .statements()
        .filter_map(LanguageSystem::cast)
    {
        languages_by_script
            .entry(langsys.script().to_raw())
            .or_insert(Vec::new())
            .push(langsys.language().to_raw())
    }

    let mut unic_script_to_languages = HashMap::new();
    for (ot_script, langs) in languages_by_script {
        let Some(unicode_script) = super::properties::ot_tag_to_script(ot_script) else {
            if ot_script != DFLT_SCRIPT {
                log::warn!("no unicode script for OT script tag {ot_script}");
            }
            continue;
        };
        unic_script_to_languages
            .entry(unicode_script)
            .or_insert(Vec::new())
            .push((ot_script, langs));
    }

    unic_script_to_languages
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
    use super::*;

    const LATN: UnicodeShortName = unsafe { UnicodeShortName::from_bytes_unchecked(*b"Latn") };
    struct MockCharMap(HashMap<char, GlyphId>);

    impl CharMap for MockCharMap {
        fn iter_glyphs(&self) -> impl Iterator<Item = (GlyphId, u32)> {
            self.0.iter().map(|(uv, gid)| (*gid, *uv as u32))
        }
    }

    impl MockCharMap {
        fn make_rule(&self, left: char, right: char, val: i16) -> KernPair {
            KernPair {
                side1: KernSide::Glyph(self.get(left)),
                side2: KernSide::Glyph(self.get(right)),
                value: ValueRecordBuilder::new().with_x_advance(val),
            }
        }

        fn make_class_rule(&self, left: &[char], right: &[char], val: i16) -> KernPair {
            let left = left.iter().map(|c| self.get(*c)).collect();
            let right = right.iter().map(|c| self.get(*c)).collect();

            KernPair {
                side1: KernSide::Group(left),
                side2: KernSide::Group(right),
                value: ValueRecordBuilder::new().with_x_advance(val),
            }
        }

        fn get(&self, c: char) -> GlyphId {
            self.0.get(&c).copied().unwrap()
        }
    }

    impl FromIterator<char> for MockCharMap {
        fn from_iter<T: IntoIterator<Item = char>>(iter: T) -> Self {
            Self(
                iter.into_iter()
                    .enumerate()
                    .map(|(i, c)| (c, GlyphId::new(i as u16 + 1)))
                    .collect(),
            )
        }
    }

    #[test]
    fn split_latin_and_cyrillic() {
        const A_CY: char = 'а';
        const BE_CY: char = 'б';
        let charmap: MockCharMap = ['a', 'b', A_CY, BE_CY].into_iter().collect();
        let known_scripts = scripts_for_chars(&charmap);
        let pairs = [('a', 'b', 5i16), ('a', 'a', 7), (A_CY, BE_CY, 12)]
            .into_iter()
            .map(|(a, b, val)| charmap.make_rule(a, b, val))
            .collect::<Vec<_>>();
        let ctx =
            KernSplitContext::new(&charmap, &known_scripts, None, Default::default()).unwrap();

        let pairs_ref = pairs.iter().collect::<Vec<_>>();

        let result = ctx.make_lookups(&pairs_ref);
        assert_eq!(result.len(), 2);

        let cyrillic: BTreeSet<_> = [UnicodeShortName::from_str("Cyrl").unwrap()]
            .into_iter()
            .collect();

        let latn: BTreeSet<_> = [LATN].into_iter().collect();

        let cyr_rules = result.get(&cyrillic).unwrap();
        assert_eq!(
            cyr_rules
                .iter()
                .flat_map(|x| x.subtables.iter().map(|sub| sub.len()))
                .sum::<usize>(),
            1
        );

        let latn_rules = result.get(&latn).unwrap();
        assert_eq!(
            latn_rules
                .iter()
                .flat_map(|x| x.subtables.iter().map(|sub| sub.len()))
                .sum::<usize>(),
            2
        );
    }

    fn flags_and_rule_count(lookup: &PendingLookup<PairPosBuilder>) -> (LookupFlag, usize) {
        (
            lookup.flags,
            lookup.subtables.iter().map(|sub| sub.len()).sum::<usize>(),
        )
    }

    #[test]
    fn mark_to_base_kern() {
        const ACUTE_COMB: char = '\u{0301}';
        let charmap: MockCharMap = ['A', 'B', 'C', ACUTE_COMB].into_iter().collect();
        let known_scripts = scripts_for_chars(&charmap);
        let mark_glyphs = [(charmap.get(ACUTE_COMB), MarkSpacing::NonSpacing)]
            .into_iter()
            .collect();
        let pairs = [('A', ACUTE_COMB, -55), ('B', 'C', -30), ('A', 'C', -22)]
            .into_iter()
            .map(|(a, b, val)| charmap.make_rule(a, b, val))
            .collect::<Vec<_>>();

        let ctx = KernSplitContext::new(&charmap, &known_scripts, None, mark_glyphs).unwrap();

        let pairs_ref = pairs.iter().collect::<Vec<_>>();
        let result = ctx.make_lookups(&pairs_ref);

        let latn: BTreeSet<_> = [LATN].into_iter().collect();
        assert_eq!(result.len(), 1);
        let lookups = result.get(&latn).unwrap();
        assert_eq!(lookups.len(), 2);

        let bases = &lookups[0];
        let marks = &lookups[1];
        assert_eq!(
            (flags_and_rule_count(bases), flags_and_rule_count(marks)),
            ((LookupFlag::IGNORE_MARKS, 2), (LookupFlag::empty(), 1)),
        );
    }

    #[test]
    fn mark_to_base_only() {
        const ACUTE_COMB: char = '\u{0301}';
        let charmap: MockCharMap = ['A', 'B', 'C', ACUTE_COMB].into_iter().collect();
        let known_scripts = scripts_for_chars(&charmap);
        let mark_glyphs = [(charmap.get(ACUTE_COMB), MarkSpacing::NonSpacing)]
            .into_iter()
            .collect();
        let pairs = vec![charmap.make_rule('A', ACUTE_COMB, -55)];

        let ctx = KernSplitContext::new(&charmap, &known_scripts, None, mark_glyphs).unwrap();

        let pairs_ref = pairs.iter().collect::<Vec<_>>();
        let result = ctx.make_lookups(&pairs_ref);

        let latn: BTreeSet<_> = [LATN].into_iter().collect();
        assert_eq!(result.len(), 1);
        let lookups = result.get(&latn).unwrap();
        assert_eq!(lookups.len(), 1);

        assert_eq!(flags_and_rule_count(&lookups[0]), (LookupFlag::empty(), 1));
    }

    #[test]
    fn mark_to_base_mixed_class() {
        const ACUTE_COMB: char = '\u{0301}';
        const CIRCUM_COMB: char = '\u{302}';
        let charmap: MockCharMap = ['A', 'B', 'C', ACUTE_COMB, CIRCUM_COMB]
            .into_iter()
            .collect();
        let known_scripts = scripts_for_chars(&charmap);
        let mark_glyphs = [
            (charmap.get(ACUTE_COMB), MarkSpacing::NonSpacing),
            (charmap.get(CIRCUM_COMB), MarkSpacing::NonSpacing),
        ]
        .into_iter()
        .collect();
        let pairs = vec![
            charmap.make_rule('A', 'A', 12),
            charmap.make_class_rule(&['A', 'B'], &[ACUTE_COMB, CIRCUM_COMB, 'C'], -55),
        ];
        let ctx = KernSplitContext::new(&charmap, &known_scripts, None, mark_glyphs).unwrap();

        let pairs_ref = pairs.iter().collect::<Vec<_>>();
        let result = ctx.make_lookups(&pairs_ref);

        let latn: BTreeSet<_> = [LATN].into_iter().collect();
        assert_eq!(result.len(), 1);
        let lookups = result.get(&latn).unwrap();
        assert_eq!(lookups.len(), 2);

        // we should end up splitting this rule into two lookups, because class2 has mixed
        // base & mark glyphs
        let bases = &lookups[0];
        let marks = &lookups[1];

        assert_eq!(
            (flags_and_rule_count(bases), flags_and_rule_count(marks)),
            ((LookupFlag::IGNORE_MARKS, 2), (LookupFlag::empty(), 1)),
        );
    }
}
