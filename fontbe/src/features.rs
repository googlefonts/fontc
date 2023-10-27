//! Feature binary compilation.

use std::{
    collections::{BTreeMap, HashMap, HashSet},
    error::Error as StdError,
    ffi::{OsStr, OsString},
    fmt::Display,
    fs,
    path::PathBuf,
    sync::Arc,
};

use fea_rs::{
    compile::{
        Compilation, FeatureBuilder, FeatureProvider, MarkToBaseBuilder, MarkToMarkBuilder,
        PairPosBuilder, VariationInfo,
    },
    parse::{SourceLoadError, SourceResolver},
    Compiler, GlyphClass, GlyphMap, GlyphName as FeaRsGlyphName,
};
use font_types::{GlyphId, Tag};
use fontdrasil::{coords::NormalizedLocation, types::Axis};
use fontir::{
    ir::{Anchor, Features, GlyphAnchors, GlyphOrder, KernParticipant, Kerning, StaticMetadata},
    orchestration::{Flags, WorkId as FeWorkId},
};

use log::{debug, error, trace, warn};
use ordered_float::OrderedFloat;

use fontdrasil::{
    orchestration::{Access, Work},
    types::GlyphName,
};
use smol_str::SmolStr;
use write_fonts::{
    tables::gpos::{AnchorFormat1, AnchorTable, ValueRecord},
    tables::layout::LookupFlag,
    OtRound,
};

use crate::{
    error::Error,
    orchestration::{AnyWorkId, BeWork, Context, WorkId},
};

#[derive(Debug)]
pub struct FeatureWork {}

// I did not want to make a struct
// I did not want to clone the content
// I do not like this construct
// I do find the need to lament
struct InMemoryResolver {
    content_path: OsString,
    content: Arc<str>,
    // Our fea might be generated in memory, such as to inject generated kerning,
    // while compiling a disk-based source with a well defined include path
    include_dir: Option<PathBuf>,
}

impl SourceResolver for InMemoryResolver {
    fn get_contents(&self, rel_path: &OsStr) -> Result<Arc<str>, SourceLoadError> {
        if rel_path == &*self.content_path {
            return Ok(self.content.clone());
        }
        let Some(include_dir) = &self.include_dir else {
            return Err(SourceLoadError::new(
                rel_path.to_os_string(),
                NoIncludePathError::new(),
            ));
        };
        let path = include_dir
            .join(rel_path)
            .canonicalize()
            .map_err(|e| SourceLoadError::new(rel_path.to_os_string(), e))?;
        if !path.is_file() {
            return Err(SourceLoadError::new(
                rel_path.to_os_string(),
                Error::FileExpected(path),
            ));
        }
        trace!("Resolved {rel_path:?} to {path:?}");
        let contents = fs::read_to_string(path)
            .map_err(|e| SourceLoadError::new(rel_path.to_os_string(), e))?;
        Ok(Arc::from(contents.as_str()))
    }
}

#[derive(Debug)]
struct NoIncludePathError {}

impl NoIncludePathError {
    fn new() -> NoIncludePathError {
        NoIncludePathError {}
    }
}

impl std::error::Error for NoIncludePathError {}

impl Display for NoIncludePathError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("No include path available")?;
        Ok(())
    }
}

struct FeaVariationInfo<'a> {
    axes: HashMap<Tag, (usize, &'a Axis)>,
    static_metadata: &'a StaticMetadata,
}

impl<'a> FeaVariationInfo<'a> {
    fn new(static_metadata: &'a StaticMetadata) -> FeaVariationInfo<'a> {
        FeaVariationInfo {
            axes: static_metadata
                .axes
                .iter()
                .enumerate()
                .map(|(i, a)| (a.tag, (i, a)))
                .collect(),
            static_metadata,
        }
    }
}

/// When generating mark features it's handy to know what is a base and what is a mark
#[derive(Copy, Clone, Debug, PartialEq)]
enum AnchorType {
    Base,
    Mark,
}

impl AnchorType {
    fn group_name(&self, glyph_name: GlyphName) -> GlyphName {
        match self {
            AnchorType::Base => glyph_name,
            AnchorType::Mark => glyph_name.as_str()[1..].into(),
        }
    }
}

trait AnchorInfo {
    fn anchor_type(&self) -> AnchorType;
}

impl AnchorInfo for GlyphName {
    fn anchor_type(&self) -> AnchorType {
        // _ prefix means mark. This convention appears to come from FontLab and is now everywhere.
        if self.as_str().starts_with('_') {
            AnchorType::Mark
        } else {
            AnchorType::Base
        }
    }
}

#[derive(Default, Debug)]
struct MarkGroup<'a> {
    //glyph_name: GlyphName,
    bases: Vec<(GlyphName, &'a Anchor)>,
    marks: Vec<(GlyphName, &'a Anchor)>,
}

struct FeatureWriter<'a> {
    kerning: &'a Kerning,
    glyph_map: &'a GlyphOrder,
    static_metadata: &'a StaticMetadata,
    raw_anchors: &'a Vec<(FeWorkId, Arc<GlyphAnchors>)>,
}

impl<'a> FeatureWriter<'a> {
    fn new(
        static_metadata: &'a StaticMetadata,
        kerning: &'a Kerning,
        glyph_map: &'a GlyphOrder,
        raw_anchors: &'a Vec<(FeWorkId, Arc<GlyphAnchors>)>,
    ) -> Self {
        FeatureWriter {
            kerning,
            static_metadata,
            glyph_map,
            raw_anchors,
        }
    }

    fn glyph_id(&self, glyph_name: &GlyphName) -> Option<GlyphId> {
        self.glyph_map
            .glyph_id(glyph_name)
            .map(|val| Some(GlyphId::new(val as u16)))
            .unwrap_or_default()
    }

    //TODO: at least for kerning, we should be able to generate the lookups
    //as a separate worktask, and then just add them here.
    fn add_kerning_features(&self, builder: &mut FeatureBuilder) {
        if self.kerning.is_empty() {
            return;
        }

        // a little helper closure used below
        let name_to_gid = |name| self.glyph_id(name).unwrap_or(GlyphId::NOTDEF);

        // convert the groups stored in the Kerning object into the glyph classes
        // expected by fea-rs:
        let glyph_classes = self
            .kerning
            .groups
            .iter()
            .map(|(class_name, glyph_set)| {
                let glyph_class: GlyphClass = glyph_set
                    .iter()
                    .map(|name| GlyphId::new(self.glyph_map.glyph_id(name).unwrap_or(0) as u16))
                    .collect();
                (class_name, glyph_class)
            })
            .collect::<HashMap<_, _>>();

        let mut ppos_subtables = PairPosBuilder::default();

        // now for each kerning entry, directly add a rule to the builder:
        for ((left, right), values) in &self.kerning.kerns {
            let (default_value, deltas) = self
                .resolve_variable_metric(values)
                .expect("FIGURE OUT ERRORS");

            let mut x_adv_record = ValueRecord::new().with_x_advance(default_value);
            let empty_record = ValueRecord::default();

            if !deltas.is_empty() {
                let var_idx = builder.add_deltas(deltas);
                x_adv_record = x_adv_record.with_x_advance_device(var_idx);
            }

            match (left, right) {
                (KernParticipant::Glyph(left), KernParticipant::Glyph(right)) => {
                    let gid0 = name_to_gid(left);
                    let gid1 = name_to_gid(right);
                    ppos_subtables.insert_pair(gid0, x_adv_record, gid1, empty_record);
                }
                (KernParticipant::Group(left), KernParticipant::Group(right)) => {
                    let left = glyph_classes.get(left).unwrap().clone();
                    let right = glyph_classes.get(right).unwrap().clone();
                    ppos_subtables.insert_classes(left, x_adv_record, right, empty_record);
                }
                // if groups are mixed with glyphs then we enumerate the group
                (KernParticipant::Glyph(left), KernParticipant::Group(right)) => {
                    let gid0 = name_to_gid(left);
                    let right = glyph_classes.get(right).unwrap();
                    for gid1 in right {
                        ppos_subtables.insert_pair(
                            gid0,
                            x_adv_record.clone(),
                            *gid1,
                            empty_record.clone(),
                        );
                    }
                }
                (KernParticipant::Group(left), KernParticipant::Glyph(right)) => {
                    let left = glyph_classes.get(left).unwrap();
                    let gid1 = name_to_gid(right);
                    for gid0 in left {
                        ppos_subtables.insert_pair(
                            *gid0,
                            x_adv_record.clone(),
                            gid1,
                            empty_record.clone(),
                        );
                    }
                }
            }
        }

        // now we have a builder for the pairpos subtables, so we can make
        // a lookup:
        let lookups = vec![builder.add_lookup(LookupFlag::empty(), None, vec![ppos_subtables])];
        builder.add_to_default_language_systems(Tag::new(b"kern"), &lookups)
    }

    /// Generate mark to base and mark to mark features
    ///
    /// Based on notes from f2f at W3C TPAC Spain and inspection of fea written by fontmake.
    ///
    /// Emit one lookup per mark class, it's simpler and may be more compact. See discussions in:
    /// * <https://github.com/googlefonts/ufo2ft/issues/762>
    /// * <https://github.com/googlefonts/ufo2ft/issues/591>
    /// * <https://github.com/googlefonts/ufo2ft/issues/563>
    //TODO: could we generate as a separate task, and then just add here.
    fn add_marks(&self, builder: &mut FeatureBuilder) -> Result<(), Error> {
        let mut anchors = HashMap::new();
        for (work_id, arc_glyph_anchors) in self.raw_anchors {
            let glyph_anchors: &GlyphAnchors = arc_glyph_anchors;
            let FeWorkId::Anchor(glyph_name) = work_id else {
                return Err(Error::ExpectedAnchor(work_id.clone()));
            };
            anchors.insert(glyph_name.clone(), glyph_anchors);
        }

        let mut groups: HashMap<GlyphName, MarkGroup> = Default::default();
        for (glyph_name, glyph_anchors) in anchors.iter() {
            // We assume the anchor list to be small
            // considering only glyphs with anchors,
            //  - glyphs with *only* base anchors are bases
            //  - glyphs with *any* mark anchor are marks

            let mut base = true; // TODO: only a base if user rules allow it
            for anchor in glyph_anchors.anchors.iter() {
                let anchor_type = anchor.name.anchor_type();
                if anchor_type == AnchorType::Mark {
                    base = false;

                    // TODO: only if user rules allow us to be a mark
                    groups
                        .entry(anchor_type.group_name(anchor.name.clone()))
                        .or_default()
                        .marks
                        .push(((glyph_name).clone(), anchor));
                }
            }

            if base {
                for anchor in glyph_anchors.anchors.iter() {
                    let anchor_type = anchor.name.anchor_type();
                    groups
                        .entry(anchor_type.group_name(anchor.name.clone()))
                        .or_default()
                        .bases
                        .push(((glyph_name).clone(), anchor));
                }
            }
        }

        // Build the actual mark base and mark mark constructs using fea-rs builders

        let mut mark_base_lookups = Vec::new();
        let mut mark_mark_lookups = Vec::new();

        for (group_name, group) in groups.iter() {
            let mark_class_name: SmolStr = format!("MC_{group_name}").into();

            // if we have bases *and* marks produce mark to base
            for (base_name, base) in group.bases.iter() {
                let Some(base_gid) = self.glyph_id(base_name) else {
                    return Err(Error::MissingGlyphId(base_name.clone()));
                };
                let mark_class = format!("MC_{}", base.name).into();
                let default_pos = base.default_pos();
                let anchor = AnchorTable::Format1(AnchorFormat1::new(
                    default_pos.x.ot_round(),
                    default_pos.y.ot_round(),
                ));

                let mut mark_base = MarkToBaseBuilder::default();
                mark_base.insert_base(base_gid, &mark_class, anchor);

                // each in it's own lookup, whch differs from fontmake
                mark_base_lookups.push(builder.add_lookup(
                    LookupFlag::default(),
                    None,
                    vec![mark_base],
                ));

                // TODO: variations
                // TODO how do you use self.resolve_variable_metric(values) on 2d values
            }

            // If a mark has anchors that are themselves marks what we got here is a mark to mark

            let mut mark_mark = MarkToMarkBuilder::default();
            let mut filter_set = Vec::new();

            for (mark_name, mark_anchor) in group.marks.iter() {
                let Some(glyph_anchors) = anchors.get(mark_name) else {
                    continue;
                };
                if !glyph_anchors
                    .anchors
                    .iter()
                    .any(|a| AnchorType::Mark == a.name.anchor_type())
                {
                    continue;
                }
                let Some(mark_gid) = self.glyph_id(mark_name) else {
                    return Err(Error::MissingGlyphId(mark_name.clone()));
                };

                let base_name = AnchorType::Mark.group_name(mark_anchor.name.clone()); // TODO Ew
                let Some(anchor_my_anchor) =
                    glyph_anchors.anchors.iter().find(|a| a.name == base_name)
                else {
                    debug!("No anchor_my_anchor for {base_name}");
                    continue;
                };
                let default_pos = anchor_my_anchor.default_pos();
                let anchor = AnchorTable::Format1(AnchorFormat1::new(
                    default_pos.x.ot_round(),
                    default_pos.y.ot_round(),
                ));

                // TODO: variations
                // TODO how do you use self.resolve_variable_metric(values) on 2d values

                mark_mark
                    .insert_mark1(mark_gid, mark_class_name.clone(), anchor)
                    .map_err(Error::PreviouslyAssignedClass)?;
                filter_set.push(mark_gid);
            }
            mark_mark_lookups.push(builder.add_lookup(
                LookupFlag::default(),
                Some(filter_set.into()),
                vec![mark_mark],
            ));
        }

        builder.add_to_default_language_systems(Tag::new(b"mark"), &mark_base_lookups);
        builder.add_to_default_language_systems(Tag::new(b"mkmk"), &mark_mark_lookups);

        Ok(())
    }

    //NOTE: this is basically identical to the same method on FeaVariationInfo,
    //except they have slightly different inputs?
    fn resolve_variable_metric(
        &self,
        values: &BTreeMap<NormalizedLocation, OrderedFloat<f32>>,
    ) -> Result<
        (
            i16,
            Vec<(write_fonts::tables::variations::VariationRegion, i16)>,
        ),
        Error,
    > {
        let var_model = &self.static_metadata.variation_model;

        let point_seqs = values
            .iter()
            .map(|(pos, value)| (pos.to_owned(), vec![value.0 as f64]))
            .collect();
        let raw_deltas: Vec<_> = var_model
            .deltas(&point_seqs)
            .expect("FIXME: MAKE OUR ERROR TYPE SUPPORT ? HERE")
            .into_iter()
            .map(|(region, values)| {
                assert!(values.len() == 1, "{} values?!", values.len());
                (region, values[0])
            })
            .collect();

        let default_value: i16 = raw_deltas
            .iter()
            .filter_map(|(region, value)| {
                let scaler = region.scalar_at(&var_model.default).into_inner();
                match scaler {
                    scaler if scaler == 0.0 => None,
                    scaler => Some(scaler * *value as f32),
                }
            })
            .sum::<f32>()
            .ot_round();

        let mut deltas = Vec::with_capacity(raw_deltas.len());
        for (region, value) in raw_deltas.iter().filter(|(r, _)| !r.is_default()) {
            // https://learn.microsoft.com/en-us/typography/opentype/spec/otvarcommonformats#variation-regions
            // Array of region axis coordinates records, in the order of axes given in the 'fvar' table.
            let mut region_axes = Vec::with_capacity(self.static_metadata.axes.len());
            for axis in self.static_metadata.axes.iter() {
                let Some(tent) = region.get(&axis.tag) else {
                    todo!("FIXME: add this error conversion!")
                };
                region_axes.push(tent.to_region_axis_coords());
            }
            deltas.push((
                write_fonts::tables::variations::VariationRegion { region_axes },
                value.ot_round(),
            ));
        }

        Ok((default_value, deltas))
    }
}

impl<'a> FeatureProvider for FeatureWriter<'a> {
    fn add_features(&self, builder: &mut FeatureBuilder) {
        self.add_kerning_features(builder);
        self.add_marks(builder).unwrap(); // TODO where my error handling
    }
}

#[derive(Debug)]
struct UnsupportedLocationError(NormalizedLocation);

impl UnsupportedLocationError {
    fn new(loc: NormalizedLocation) -> UnsupportedLocationError {
        UnsupportedLocationError(loc)
    }
}

impl std::error::Error for UnsupportedLocationError {}

impl Display for UnsupportedLocationError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "No variation model for {:?}", self.0)
    }
}

#[derive(Debug)]
struct MissingTentError(Tag);

impl MissingTentError {
    fn new(tag: Tag) -> MissingTentError {
        MissingTentError(tag)
    }
}

impl std::error::Error for MissingTentError {}

impl Display for MissingTentError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Missing a tent for {}", self.0)
    }
}

impl<'a> VariationInfo for FeaVariationInfo<'a> {
    fn axis(&self, axis_tag: font_types::Tag) -> Option<(usize, &Axis)> {
        self.axes.get(&axis_tag).map(|(i, a)| (*i, *a))
    }

    fn resolve_variable_metric(
        &self,
        values: &HashMap<NormalizedLocation, i16>,
    ) -> Result<
        (
            i16,
            Vec<(write_fonts::tables::variations::VariationRegion, i16)>,
        ),
        Box<(dyn StdError + 'static)>,
    > {
        let var_model = &self.static_metadata.variation_model;

        // Compute deltas using f64 as 1d point and delta, then ship them home as i16
        let point_seqs: HashMap<_, _> = values
            .iter()
            .map(|(pos, value)| (pos.clone(), vec![*value as f64]))
            .collect();

        // We only support use when the point seq is at a location our variation model supports
        // TODO: get a model for the location we are asked for so we can support sparseness
        for loc in point_seqs.keys() {
            if !var_model.supports(loc) {
                return Err(Box::new(UnsupportedLocationError::new(loc.clone())));
            }
        }

        // Only 1 value per region for our input
        let deltas: Vec<_> = var_model
            .deltas(&point_seqs)?
            .into_iter()
            .map(|(region, values)| {
                assert!(values.len() == 1, "{} values?!", values.len());
                (region, values[0])
            })
            .collect();

        // Compute the default on the unrounded deltas
        let default_value = deltas
            .iter()
            .filter_map(|(region, value)| {
                let scaler = region.scalar_at(&var_model.default).into_inner();
                match scaler {
                    scaler if scaler == 0.0 => None,
                    scaler => Some(scaler * *value as f32),
                }
            })
            .sum::<f32>()
            .ot_round();

        // Produce the desired delta type
        let mut fears_deltas = Vec::with_capacity(deltas.len());
        for (region, value) in deltas.iter().filter(|(r, _)| !r.is_default()) {
            // https://learn.microsoft.com/en-us/typography/opentype/spec/otvarcommonformats#variation-regions
            // Array of region axis coordinates records, in the order of axes given in the 'fvar' table.
            let mut region_axes = Vec::with_capacity(self.static_metadata.axes.len());
            for axis in self.static_metadata.axes.iter() {
                let Some(tent) = region.get(&axis.tag) else {
                    return Err(Box::new(MissingTentError::new(axis.tag)));
                };
                region_axes.push(tent.to_region_axis_coords());
            }
            fears_deltas.push((
                write_fonts::tables::variations::VariationRegion { region_axes },
                value.ot_round(),
            ));
        }

        Ok((default_value, fears_deltas))
    }
}

impl FeatureWork {
    pub fn create() -> Box<BeWork> {
        Box::new(FeatureWork {})
    }

    fn compile(
        &self,
        static_metadata: &StaticMetadata,
        features: &Features,
        glyph_order: &GlyphOrder,
        kerning: &Kerning,
        raw_anchors: &Vec<(FeWorkId, Arc<GlyphAnchors>)>,
    ) -> Result<Compilation, Error> {
        let var_info = FeaVariationInfo::new(static_metadata);
        let feature_writer = FeatureWriter::new(static_metadata, kerning, glyph_order, raw_anchors);
        let fears_glyph_map = create_glyphmap(glyph_order);
        let compiler = match features {
            Features::File {
                fea_file,
                include_dir,
            } => {
                let mut compiler = Compiler::new(OsString::from(fea_file), &fears_glyph_map);
                if let Some(include_dir) = include_dir {
                    compiler = compiler.with_project_root(include_dir)
                }
                compiler
            }
            Features::Memory {
                fea_content,
                include_dir,
            } => {
                let root = OsString::new();
                let mut compiler =
                    Compiler::new(root.clone(), &fears_glyph_map).with_resolver(InMemoryResolver {
                        content_path: root,
                        content: Arc::from(fea_content.as_str()),
                        include_dir: include_dir.clone(),
                    });
                if let Some(include_dir) = include_dir {
                    compiler = compiler.with_project_root(include_dir)
                }
                compiler
            }
            Features::Empty => {
                // There is no user feature file but we could still generate kerning, marks, etc
                let root = OsString::new();
                Compiler::new(root.clone(), &fears_glyph_map).with_resolver(InMemoryResolver {
                    content_path: root,
                    content: Arc::from(""),
                    include_dir: None,
                })
            }
        }
        .with_variable_info(&var_info)
        .with_feature_writer(&feature_writer);
        compiler.compile().map_err(Error::FeaCompileError)
    }
}

fn write_debug_fea(context: &Context, is_error: bool, why: &str, fea_content: &str) {
    if !context.flags.contains(Flags::EMIT_DEBUG) {
        if is_error {
            warn!("Debug fea not written for '{why}' because --emit_debug is off");
        }
        return;
    }
    let debug_file = context.debug_dir().join("features.fea");
    match fs::write(&debug_file, fea_content) {
        Ok(..) => {
            if is_error {
                warn!("{}; fea written to {:?}", why, debug_file)
            } else {
                debug!("fea written to {:?}", debug_file);
            }
        }
        Err(e) => error!("{}; failed to write fea to {:?}: {}", why, debug_file, e),
    };
}

fn create_glyphmap(glyph_order: &GlyphOrder) -> GlyphMap {
    if glyph_order.is_empty() {
        warn!("Glyph order is empty; feature compile improbable");
    }
    glyph_order
        .iter()
        .map(|n| Into::<FeaRsGlyphName>::into(n.as_str()))
        .collect()
}

impl Work<Context, AnyWorkId, Error> for FeatureWork {
    fn id(&self) -> AnyWorkId {
        WorkId::Features.into()
    }

    fn read_access(&self) -> Access<AnyWorkId> {
        Access::Set(HashSet::from([
            AnyWorkId::Fe(FeWorkId::GlyphOrder),
            AnyWorkId::Fe(FeWorkId::StaticMetadata),
            AnyWorkId::Fe(FeWorkId::Kerning),
            AnyWorkId::Fe(FeWorkId::Features),
        ]))
    }

    fn also_completes(&self) -> Vec<AnyWorkId> {
        vec![
            WorkId::Gpos.into(),
            WorkId::Gsub.into(),
            WorkId::Gdef.into(),
        ]
    }

    fn exec(&self, context: &Context) -> Result<(), Error> {
        let static_metadata = context.ir.static_metadata.get();
        let glyph_order = context.ir.glyph_order.get();
        let features = context.ir.features.get();
        let kerning = context.ir.kerning.get();
        let anchors = context.ir.anchors.all();

        let result = self.compile(
            &static_metadata,
            features.as_ref(),
            glyph_order.as_ref(),
            kerning.as_ref(),
            anchors.as_ref(),
        );
        if result.is_err() || context.flags.contains(Flags::EMIT_DEBUG) {
            if let Features::Memory { fea_content, .. } = features.as_ref() {
                write_debug_fea(context, result.is_err(), "compile failed", fea_content);
            }
        }
        let result = result?;

        debug!(
            "Built features, gpos? {} gsub? {} gdef? {}",
            result.gpos.is_some(),
            result.gsub.is_some(),
            result.gdef.is_some(),
        );
        if let Some(gpos) = result.gpos {
            context.gpos.set_unconditionally(gpos.into());
        }
        if let Some(gsub) = result.gsub {
            context.gsub.set_unconditionally(gsub.into());
        }
        if let Some(gdef) = result.gdef {
            context.gdef.set_unconditionally(gdef.into());
        }

        // Enables the assumption that if the file exists features were compiled
        if context.flags.contains(Flags::EMIT_IR) {
            fs::write(
                context
                    .persistent_storage
                    .paths
                    .target_file(&WorkId::Features),
                "1",
            )
            .map_err(Error::IoError)?;
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::collections::{HashMap, HashSet};

    use fea_rs::compile::VariationInfo;
    use font_types::Tag;
    use fontdrasil::{
        coords::{CoordConverter, DesignCoord, NormalizedCoord, UserCoord},
        types::Axis,
    };
    use fontir::ir::StaticMetadata;

    use super::FeaVariationInfo;

    fn weight_variable_static_metadata(min: f32, def: f32, max: f32) -> StaticMetadata {
        let min_wght_user = UserCoord::new(min);
        let def_wght_user = UserCoord::new(def);
        let max_wght_user = UserCoord::new(max);
        let wght = Tag::new(b"wght");
        let min_wght = vec![(wght, NormalizedCoord::new(-1.0))].into();
        let def_wght = vec![(wght, NormalizedCoord::new(0.0))].into();
        let max_wght = vec![(wght, NormalizedCoord::new(1.0))].into();
        StaticMetadata::new(
            1024,
            Default::default(),
            vec![
                Axis {
                    name: "Weight".to_string(),
                    tag: Tag::new(b"wght"),
                    min: min_wght_user,
                    default: def_wght_user,
                    max: max_wght_user,
                    hidden: false,
                    converter: CoordConverter::new(
                        vec![
                            // the design values don't really matter
                            (min_wght_user, DesignCoord::new(0.0)),
                            (def_wght_user, DesignCoord::new(1.0)),
                            (max_wght_user, DesignCoord::new(2.0)),
                        ],
                        1,
                    ),
                },
                // no-op 'point' axis, should be ignored
                Axis {
                    name: "Width".to_string(),
                    tag: Tag::new(b"wdth"),
                    min: UserCoord::new(0.0),
                    default: UserCoord::new(0.0),
                    max: UserCoord::new(0.0),
                    hidden: false,
                    converter: CoordConverter::new(vec![], 0),
                },
            ],
            Default::default(),
            HashSet::from([min_wght, def_wght, max_wght]),
            Default::default(),
        )
        .unwrap()
    }

    fn is_default(region: &write_fonts::tables::variations::VariationRegion) -> bool {
        region.region_axes.iter().all(|axis_coords| {
            axis_coords.start_coord.to_f32() == 0.0
                && axis_coords.peak_coord.to_f32() == 0.0
                && axis_coords.end_coord.to_f32() == 0.0
        })
    }

    #[test]
    fn resolve_kern() {
        let _ = env_logger::builder().is_test(true).try_init();
        let wght = Tag::new(b"wght");
        let static_metadata = weight_variable_static_metadata(300.0, 400.0, 700.0);
        let var_info = FeaVariationInfo::new(&static_metadata);

        let (default, regions) = var_info
            .resolve_variable_metric(&HashMap::from([
                (vec![(wght, NormalizedCoord::new(-1.0))].into(), 10),
                (vec![(wght, NormalizedCoord::new(0.0))].into(), 15),
                (vec![(wght, NormalizedCoord::new(1.0))].into(), 20),
            ]))
            .unwrap();
        assert!(!regions.iter().any(|(r, _)| is_default(r)));
        let region_values: Vec<_> = regions.into_iter().map(|(_, v)| v + default).collect();
        assert_eq!((15, vec![10, 20]), (default, region_values));
    }
}
