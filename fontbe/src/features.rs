//! Feature binary compilation.

use std::{
    borrow::Cow,
    collections::{BTreeMap, HashMap, HashSet},
    fmt::Display,
    fs,
    path::{Path, PathBuf},
    sync::Arc,
};

use feature_variations::FeatureVariationsProvider;
use log::{debug, error, trace, warn};
use ordered_float::OrderedFloat;

use fea_rs::{
    compile::{
        error::CompilerError, Compilation, FeatureBuilder, FeatureProvider, NopFeatureProvider,
        PendingLookup, VariationInfo,
    },
    parse::{FileSystemResolver, SourceLoadError, SourceResolver},
    typed::{AstNode, LanguageSystem},
    DiagnosticSet, GlyphMap, Opts, ParseTree,
};

use fontir::{
    ir::{FeaturesSource, GlyphOrder, StaticMetadata},
    orchestration::{Flags, WorkId as FeWorkId},
    variations::{DeltaError, VariationModel},
};

use fontdrasil::{
    coords::NormalizedLocation,
    orchestration::{Access, AccessBuilder, Work},
    types::Axis,
};
use properties::UnicodeShortName;
use write_fonts::{
    tables::{gdef::GlyphClassDef, layout::ClassDef, variations::VariationRegion},
    types::{GlyphId16, NameId, Tag},
    OtRound,
};

use crate::{
    error::Error,
    orchestration::{
        AnyWorkId, BeWork, Context, ExtraFeaTables, FeaFirstPassOutput, FeaRsKerns, FeaRsMarks,
        WorkId,
    },
};

mod feature_variations;
mod kern;
mod marks;
mod ot_tags;
mod properties;
#[cfg(test)]
mod test_helpers;

pub use kern::{create_gather_ir_kerning_work, create_kern_segment_work, create_kerns_work};
pub use marks::create_mark_work;

const DFLT_SCRIPT: Tag = Tag::new(b"DFLT");
const DFLT_LANG: Tag = Tag::new(b"dflt");

#[derive(Debug)]
pub struct FeatureFirstPassWork {}

#[derive(Debug)]
pub struct FeatureCompilationWork {}

// I did not want to make a struct
// I did not want to clone the content
// I do not like this construct
// I do find the need to lament
struct InMemoryResolver {
    content_path: PathBuf,
    content: Arc<str>,
    // Our fea might be generated in memory, such as to inject generated kerning,
    // while compiling a disk-based source with a well defined include path
    include_dir: Option<PathBuf>,
}

impl InMemoryResolver {
    fn empty() -> Self {
        InMemoryResolver {
            content_path: Default::default(),
            content: "".into(),
            include_dir: None,
        }
    }
}

impl SourceResolver for InMemoryResolver {
    fn get_contents(&self, rel_path: &Path) -> Result<Arc<str>, SourceLoadError> {
        if rel_path == &*self.content_path {
            return Ok(self.content.clone());
        }
        let Some(include_dir) = &self.include_dir else {
            return Err(SourceLoadError::new(
                rel_path.to_path_buf(),
                NoIncludePathError::new(),
            ));
        };
        let path = include_dir
            .join(rel_path)
            .canonicalize()
            .map_err(|e| SourceLoadError::new(rel_path.to_path_buf(), e))?;
        if !path.is_file() {
            return Err(SourceLoadError::new(
                rel_path.to_path_buf(),
                Error::FileExpected(path),
            ));
        }
        trace!("Resolved {rel_path:?} to {path:?}");
        let contents = fs::read_to_string(path)
            .map_err(|e| SourceLoadError::new(rel_path.to_path_buf(), e))?;
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

pub(crate) struct FeaVariationInfo<'a> {
    axes: HashMap<Tag, (usize, &'a Axis)>,
    static_metadata: &'a StaticMetadata,
}

impl<'a> FeaVariationInfo<'a> {
    pub(crate) fn new(static_metadata: &'a StaticMetadata) -> FeaVariationInfo<'a> {
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

/// Return GDEF classes.
///
/// If the source is one where we prefer classes declared explicitly in FEA,
/// and those exist, return those; otherwise return computed classes (from
/// public.openTypeCatgories or from Glyphs.xml, depending on the source type)
pub(crate) fn get_gdef_classes(
    meta: &StaticMetadata,
    ast: &FeaFirstPassOutput,
    glyph_order: &GlyphOrder,
) -> HashMap<GlyphId16, GlyphClassDef> {
    ast.gdef_classes
        .as_ref()
        .filter(|_| meta.gdef_categories.prefer_gdef_categories_in_fea)
        .cloned()
        .unwrap_or_else(|| {
            meta.gdef_categories
                .categories
                .iter()
                .filter_map(|(name, category)| {
                    glyph_order.glyph_id(name).map(|gid| (gid, *category))
                })
                .collect()
        })
}

//NOTE: this is basically identical to the same method on FeaVariationInfo,
//except they have slightly different inputs?
pub(crate) fn resolve_variable_metric<'a>(
    static_metadata: &StaticMetadata,
    values: impl Iterator<Item = (&'a NormalizedLocation, &'a OrderedFloat<f64>)>,
) -> Result<(i16, Vec<(VariationRegion, i16)>), DeltaError> {
    let point_seqs: HashMap<_, _> = values
        .into_iter()
        .map(|(pos, value)| {
            // The master values for anchor positions or kerning adjustments are
            // expected to be rounded before computing the deltas, because instancing
            // a VF at the masters' location is expected to be equivalent to building
            // individual masters as static fonts. fontmake does the same, see
            // https://github.com/googlefonts/fontc/issues/1043
            let value: f64 = value.into_inner().ot_round();
            (pos.to_owned(), vec![value])
        })
        .collect();
    let locations: HashSet<_> = point_seqs.keys().collect();
    let global_locations: HashSet<_> = static_metadata.variation_model.locations().collect();

    // Try to reuse the global model, or make a new sub-model only with the locations we
    // are asked for so we can support sparseness
    let var_model: Cow<'_, VariationModel> = if locations == global_locations {
        Cow::Borrowed(&static_metadata.variation_model)
    } else {
        Cow::Owned(
            VariationModel::new(
                locations.into_iter().cloned().collect(),
                static_metadata.axes.clone(),
            )
            .unwrap(),
        )
    };

    let raw_deltas: Vec<_> = var_model
        .deltas(&point_seqs)?
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
            (scaler != 0.0).then_some(*value * scaler)
        })
        .sum::<f64>()
        .ot_round();

    let mut deltas = Vec::with_capacity(raw_deltas.len());
    for (region, value) in raw_deltas.iter().filter(|(r, _)| !r.is_default()) {
        // https://learn.microsoft.com/en-us/typography/opentype/spec/otvarcommonformats#variation-regions
        // Array of region axis coordinates records, in the order of axes given in the 'fvar' table.
        let mut region_axes = Vec::with_capacity(static_metadata.axes.len());
        for axis in static_metadata.axes.iter() {
            // all axes in static metadata must be present in region returned
            // by variation model, which uses those same axes
            let coords = region.get(&axis.tag).unwrap().to_region_axis_coords();
            region_axes.push(coords);
        }
        deltas.push((
            write_fonts::tables::variations::VariationRegion { region_axes },
            value.ot_round(),
        ));
    }

    Ok((default_value, deltas))
}

struct FeatureWriter<'a> {
    kerning: &'a FeaRsKerns,
    marks: &'a FeaRsMarks,
    feature_variations: Option<FeatureVariationsProvider>,
}

impl<'a> FeatureWriter<'a> {
    fn new(
        kerning: &'a FeaRsKerns,
        marks: &'a FeaRsMarks,
        feature_variations: Option<FeatureVariationsProvider>,
    ) -> Self {
        FeatureWriter {
            marks,
            kerning,
            feature_variations,
        }
    }

    /// We did most of the work in the kerning job, take the data and populate a builder
    fn add_kerning_features(&self, builder: &mut FeatureBuilder) {
        self.kerning.add_features(builder);
    }

    /// Generate mark to base and mark to mark features
    ///
    /// Based on notes from f2f at W3C TPAC Spain and inspection of fea written by fontmake.
    ///
    /// See [markFeatureWriter.py](https://github.com/googlefonts/ufo2ft/blob/main/Lib/ufo2ft/featureWriters/markFeatureWriter.py)
    /// for the fontmake implementation.
    ///
    /// We emit one lookup per mark class, it's simpler and may be more compact. See discussions in:
    /// * <https://github.com/googlefonts/ufo2ft/issues/762>
    /// * <https://github.com/googlefonts/ufo2ft/issues/591>
    /// * <https://github.com/googlefonts/ufo2ft/issues/563>
    //TODO: could we generate as a separate task, and then just add here.
    fn add_marks(&self, builder: &mut FeatureBuilder) {
        self.marks.add_features(builder);
    }

    /// Add any feature variations
    fn add_feature_variations(&self, builder: &mut FeatureBuilder) {
        if let Some(variations) = self.feature_variations.as_ref() {
            variations.add_features(builder);
        }
    }
}

impl FeatureProvider for FeatureWriter<'_> {
    fn add_features(&self, builder: &mut FeatureBuilder) {
        self.add_kerning_features(builder);
        self.add_marks(builder);
        self.add_feature_variations(builder);
    }
}

impl VariationInfo for FeaVariationInfo<'_> {
    type Error = Error;
    fn axis(&self, axis_tag: Tag) -> Option<(usize, &Axis)> {
        self.axes.get(&axis_tag).map(|(i, a)| (*i, *a))
    }

    fn resolve_variable_metric(
        &self,
        values: &HashMap<NormalizedLocation, i16>,
    ) -> Result<(i16, Vec<(VariationRegion, i16)>), Error> {
        // Compute deltas using f64 as 1d point and delta, then ship them home as i16
        let point_seqs: HashMap<_, _> = values
            .iter()
            .map(|(pos, value)| (pos.clone(), vec![*value as f64]))
            .collect();

        let locations: HashSet<_> = point_seqs.keys().collect();
        let global_locations: HashSet<_> =
            self.static_metadata.variation_model.locations().collect();

        // Try to reuse the global model, or make a new sub-model only with the locations we
        // are asked for so we can support sparseness
        let var_model: Cow<'_, VariationModel> = if locations == global_locations {
            Cow::Borrowed(&self.static_metadata.variation_model)
        } else {
            Cow::Owned(
                VariationModel::new(
                    locations.into_iter().cloned().collect(),
                    self.static_metadata.axes.clone(),
                )
                .unwrap(),
            )
        };

        // Only 1 value per region for our input
        let deltas: Vec<_> = var_model
            .deltas(&point_seqs)
            .map_err(Error::DeltaError)?
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
                (scaler != 0.0).then_some(*value * scaler)
            })
            .sum::<f64>()
            .ot_round();

        // Produce the desired delta type
        let mut fears_deltas = Vec::with_capacity(deltas.len());
        for (region, value) in deltas.iter().filter(|(r, _)| !r.is_default()) {
            fears_deltas.push((
                region.to_write_fonts_variation_region(&self.static_metadata.axes),
                value.ot_round(),
            ));
        }

        Ok((default_value, fears_deltas))
    }

    fn axis_count(&self) -> u16 {
        self.axes.len().try_into().unwrap()
    }

    fn resolve_glyphs_number_value(
        &self,
        name: &str,
    ) -> Result<HashMap<NormalizedLocation, f64>, Error> {
        Ok(self
            .static_metadata
            .number_values
            .iter()
            .map(|(loc, names)| (loc.clone(), names.get(name).copied().unwrap_or_default().0))
            .collect())
    }
}

impl FeatureCompilationWork {
    pub fn create() -> Box<BeWork> {
        Box::new(FeatureCompilationWork {})
    }

    fn compile(
        &self,
        static_metadata: &StaticMetadata,
        glyph_order: &GlyphOrder,
        ast: &FeaFirstPassOutput,
        kerns: &FeaRsKerns,
        marks: &FeaRsMarks,
    ) -> Result<Compilation, Error> {
        let feature_variations = static_metadata
            .variations
            .as_ref()
            .map(|ir_variations| {
                feature_variations::make_gsub_feature_variations(
                    ir_variations,
                    static_metadata,
                    glyph_order,
                )
            })
            .transpose()?;
        let var_info = FeaVariationInfo::new(static_metadata);
        let feature_writer = FeatureWriter::new(kerns, marks, feature_variations);
        // we've already validated the AST, so we only need to compile
        match fea_rs::compile::compile(
            &ast.ast,
            &marks.glyphmap,
            Some(&var_info),
            Some(&feature_writer),
            Opts::new(),
        ) {
            Ok((result, warnings)) => {
                log_fea_warnings("compilation", &warnings);
                Ok(result)
            }
            Err(errors) => Err(Error::FeaCompileError(CompilerError::CompilationFail(
                errors,
            ))),
        }
    }
}

fn write_debug_glyph_order(context: &Context, glyphs: &GlyphOrder) {
    let glyph_order_file = context.debug_dir().join("glyph_order.txt");
    let glyph_order = glyphs.names().map(|g| g.as_str()).collect::<Vec<_>>();
    let glyph_order = glyph_order.join("\n");
    if let Err(e) = fs::write(glyph_order_file, glyph_order) {
        log::error!("failed to write glyph order to debug/glyph_order.txt: '{e}'");
    }
}

fn write_debug_fea(context: &Context, is_error: bool, why: &str, fea_content: &str) {
    if !context.flags.contains(Flags::EMIT_DEBUG) {
        if is_error {
            warn!("Debug fea not written for '{why}' because --emit-debug is off");
        }
        return;
    }

    let debug_file = context.debug_dir().join("features.fea");
    match fs::write(&debug_file, fea_content) {
        Ok(_) if is_error => warn!("{why}; fea written to {debug_file:?}"),
        Ok(_) => debug!("fea written to {debug_file:?}"),
        Err(e) => error!("{why}; failed to write fea to {debug_file:?}: {e}"),
    };
}

impl Work<Context, AnyWorkId, Error> for FeatureFirstPassWork {
    fn id(&self) -> AnyWorkId {
        WorkId::FeaturesAst.into()
    }

    fn read_access(&self) -> Access<AnyWorkId> {
        AccessBuilder::new()
            .variant(FeWorkId::GlyphOrder)
            .variant(FeWorkId::StaticMetadata)
            .variant(FeWorkId::Features)
            .build()
    }

    fn exec(&self, context: &Context) -> Result<(), Error> {
        let features = context.ir.features.get();
        let glyph_order = context.ir.glyph_order.get();
        let static_metadata = context.ir.static_metadata.get();
        let glyph_map = glyph_order.names().cloned().collect();

        let result = self.parse(&features, &glyph_map);

        if context.flags.contains(Flags::EMIT_DEBUG) {
            write_debug_glyph_order(context, &glyph_order);
        }
        if let FeaturesSource::Memory { fea_content, .. } = features.as_ref() {
            write_debug_fea(context, result.is_err(), "compile failed", fea_content);
        }

        let ast = result?;
        // after parsing we validate; we only need to do this once, and future
        // work can trust the AST.
        self.validate(&ast, &glyph_map, &static_metadata)?;
        let var_info = FeaVariationInfo::new(&static_metadata);

        let (compilation, _) = fea_rs::compile::compile::<_, NopFeatureProvider>(
            &ast,
            &glyph_map,
            Some(&var_info),
            None,
            Opts::new().compile_gpos(false),
        )
        .map_err(|err| {
            Error::FeaCompileError(fea_rs::compile::error::CompilerError::CompilationFail(err))
        })?;
        context
            .fea_ast
            .set(FeaFirstPassOutput::new(ast, compilation)?);
        Ok(())
    }
}

impl FeatureFirstPassWork {
    pub fn create() -> Box<BeWork> {
        Box::new(Self {})
    }

    fn parse(&self, features: &FeaturesSource, glyph_map: &GlyphMap) -> Result<ParseTree, Error> {
        let (resolver, root_path) = get_resolver_and_root_path(features);
        let (tree, diagnostics) = fea_rs::parse::parse_root(root_path, Some(glyph_map), resolver)
            .map_err(CompilerError::SourceLoad)?;
        if diagnostics.has_errors() {
            return Err(CompilerError::ParseFail(diagnostics).into());
        }
        log_fea_warnings("parsing", &diagnostics);
        Ok(tree)
    }

    fn validate(
        &self,
        ast: &ParseTree,
        glyph_map: &GlyphMap,
        static_metadata: &StaticMetadata,
    ) -> Result<(), Error> {
        let var_info = FeaVariationInfo::new(static_metadata);
        let diagnostics = fea_rs::compile::validate(ast, glyph_map, Some(&var_info));
        if diagnostics.has_errors() {
            return Err(CompilerError::ValidationFail(diagnostics).into());
        }
        log_fea_warnings("validation", &diagnostics);
        Ok(())
    }
}

fn get_resolver_and_root_path(features: &FeaturesSource) -> (Box<dyn SourceResolver>, PathBuf) {
    match features {
        FeaturesSource::File {
            fea_file,
            include_dir,
        } => {
            let project_root = include_dir
                .clone()
                .or_else(|| fea_file.parent().map(PathBuf::from))
                .unwrap_or_default();
            (
                Box::new(FileSystemResolver::new(project_root)),
                fea_file.clone(),
            )
        }
        FeaturesSource::Memory {
            fea_content,
            include_dir,
        } => (
            Box::new(InMemoryResolver {
                include_dir: include_dir.to_owned(),
                content_path: PathBuf::new(),
                content: fea_content.as_str().into(),
            }),
            PathBuf::new(),
        ),
        FeaturesSource::Empty => (Box::new(InMemoryResolver::empty()), Default::default()),
    }
}

impl Work<Context, AnyWorkId, Error> for FeatureCompilationWork {
    fn id(&self) -> AnyWorkId {
        WorkId::Features.into()
    }

    fn read_access(&self) -> Access<AnyWorkId> {
        AccessBuilder::new()
            .variant(FeWorkId::GlyphOrder)
            .variant(WorkId::FeaturesAst)
            .variant(WorkId::GatherBeKerning)
            .variant(WorkId::Marks)
            .build()
    }

    fn also_completes(&self) -> Vec<AnyWorkId> {
        vec![
            WorkId::Gpos.into(),
            WorkId::Gsub.into(),
            WorkId::Gdef.into(),
            WorkId::ExtraFeaTables.into(),
        ]
    }

    fn exec(&self, context: &Context) -> Result<(), Error> {
        let static_metadata = context.ir.static_metadata.get();
        let ast = context.fea_ast.get();
        let glyph_order = context.ir.glyph_order.get();
        let kerns = context.fea_rs_kerns.get();
        let marks = context.fea_rs_marks.get();

        let mut result = self.compile(
            &static_metadata,
            &glyph_order,
            &ast,
            kerns.as_ref(),
            marks.as_ref(),
        )?;
        if result.gdef_classes.is_none() && !static_metadata.gdef_categories.categories.is_empty() {
            // the FEA did not contain an explicit GDEF block with glyph categories,
            // so let's use the ones from the source, if present (i.e. from
            // `public.openTypeCatgories` or computed from GlyphData.xml

            let gdef = result.gdef.get_or_insert_with(Default::default);
            let class_def: ClassDef = static_metadata
                .gdef_categories
                .categories
                .iter()
                .filter_map(|(name, cls)| glyph_order.glyph_id(name).map(|id| (id, *cls as u16)))
                .collect();

            gdef.glyph_class_def.set(class_def);
        }

        debug!(
            "Built features, gpos? {} gsub? {} gdef? {}",
            result.gpos.is_some(),
            result.gsub.is_some(),
            result.gdef.is_some(),
        );

        if result.name.is_some() {
            let max_existing_name_id: NameId = static_metadata
                .names
                .keys()
                .map(|key| key.name_id)
                .max()
                .unwrap_or(NameId::LAST_RESERVED_NAME_ID)
                .max(NameId::LAST_RESERVED_NAME_ID);

            if max_existing_name_id > NameId::LAST_RESERVED_NAME_ID {
                result.remap_name_ids(max_existing_name_id.to_u16() + 1);
            }
        }
        if let Some(gpos) = result.gpos.take() {
            context.gpos.set(gpos);
        }
        if let Some(gsub) = result.gsub.take() {
            context.gsub.set(gsub);
        }
        if let Some(gdef) = result.gdef.take() {
            context.gdef.set(gdef);
        }

        // if fea generated tables other than GPOS/GSUB/GDEF, stash them
        // so we can merge later on
        if result.has_non_layout_tables() {
            let extras = ExtraFeaTables::from(result);
            // we're currently only handling 'name'; if other tables are in
            // here we probably need to do something with them too, so let's warn
            extras.log_unhandled_extras();
            context.extra_fea_tables.set(extras);
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

fn log_fea_warnings(stage: &str, warnings: &DiagnosticSet) {
    assert!(!warnings.has_errors(), "of course we checked this already");
    if !warnings.is_empty() {
        log::warn!(
            "FEA {stage} produced {} warnings:\n{}",
            warnings.len(),
            warnings.display()
        );
    }
}

/// returns a map of opentype script: [opentype lang], for the languagesystems in FEA
fn get_fea_language_systems(ast: &ParseTree) -> BTreeMap<Tag, Vec<Tag>> {
    let mut languages_by_script = BTreeMap::new();
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
    languages_by_script
}

// <https://github.com/googlefonts/ufo2ft/blob/cea60d71dfcf0b1c0fa4e133e/Lib/ufo2ft/featureWriters/ast.py#L23>
/// returns a map of unicode script names to (ot_script, `[ot_lang]`)
fn get_script_language_systems(ast: &ParseTree) -> HashMap<UnicodeShortName, Vec<(Tag, Vec<Tag>)>> {
    let languages_by_script = get_fea_language_systems(ast);
    let mut unic_script_to_languages = HashMap::new();
    for (ot_script, langs) in languages_by_script {
        let Some(unicode_script) = properties::ot_tag_to_script(ot_script) else {
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

/// Return the set of features from the list that we need to generate.
///
/// This ignores features that already exist in the FEA, and for which there
/// is no insertion mark.
fn feature_writer_todo_list(features: &[Tag], ast: &ParseTree) -> HashSet<Tag> {
    use fea_rs::typed;
    let mut result = features.iter().copied().collect::<HashSet<_>>();
    let mut existing_features = HashMap::new();
    for feature in ast
        .typed_root()
        .statements()
        .filter_map(typed::Feature::cast)
    {
        let tag = feature.tag().to_raw();
        if result.contains(&tag) {
            *existing_features.entry(tag).or_insert(false) |= feature.has_insert_marker();
        }
    }
    for (tag, has_marker) in existing_features {
        if !has_marker {
            log::warn!(
                "Skipping generating feature '{tag}', which is manually declared
                in FEA and has no insertion comment."
            );
            result.remove(&tag);
        }
    }
    result
}

#[cfg(test)]
mod tests {
    use std::collections::{HashMap, HashSet};

    use fea_rs::compile::VariationInfo;
    use fontdrasil::{
        coords::{CoordConverter, NormalizedCoord, UserCoord},
        types::Axis,
    };
    use fontir::ir::StaticMetadata;

    use super::*;

    fn weight_variable_static_metadata() -> StaticMetadata {
        let wght = Tag::new(b"wght");
        let min_wght = vec![(wght, NormalizedCoord::new(-1.0))].into();
        let def_wght = vec![(wght, NormalizedCoord::new(0.0))].into();
        let max_wght = vec![(wght, NormalizedCoord::new(1.0))].into();
        StaticMetadata::new(
            1024,
            Default::default(),
            vec![
                Axis::for_test("wght"),
                // no-op 'point' axis, should be ignored
                Axis {
                    name: "Width".to_string(),
                    tag: Tag::new(b"wdth"),
                    min: UserCoord::new(0.0),
                    default: UserCoord::new(0.0),
                    max: UserCoord::new(0.0),
                    hidden: false,
                    converter: CoordConverter::new(vec![], 0),
                    localized_names: Default::default(),
                },
            ],
            Default::default(),
            HashSet::from([min_wght, def_wght, max_wght]),
            Default::default(),
            Default::default(),
            Default::default(),
            None,
            false,
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
        let static_metadata = weight_variable_static_metadata();
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
