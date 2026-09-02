//! Feature binary compilation.

use std::{
    borrow::Cow,
    collections::{BTreeMap, HashMap, HashSet},
    fmt::Display,
    fs,
    path::{Path, PathBuf},
    sync::Arc,
};

pub use feature_variations::FeatureVariationsProvider;
use log::{debug, error, trace, warn};
use ordered_float::OrderedFloat;

use fea_rs::{
    DiagnosticSet, GlyphMap, Opts, ParseTree,
    compile::{
        Compilation, FeatureBuilder, FeatureProvider, NopFeatureProvider, PendingCompilation,
        PendingLookup, VariationInfo, error::CompilerError,
    },
    parse::{FileSystemResolver, SourceLoadError, SourceResolver},
    typed::{AstNode, LanguageSystem},
};

use fontir::{
    ir::{
        self, FeatureGenerationPlan, FeatureGenerationSettings, FeatureSources, FeatureWriterMode,
        FeaturesSource, GdefCategories, GlyphOrder, StaticMetadata,
    },
    orchestration::WorkId as FeWorkId,
};

use fontdrasil::{
    coords::NormalizedLocation,
    orchestration::{Access, AccessBuilder, Work},
    types::Axis,
    variations::{DeltaError, VariationModel},
};
use properties::UnicodeShortName;
use write_fonts::{
    OtRound,
    tables::{gdef::GlyphClassDef, layout::ClassDef, variations::VariationRegion},
    types::{GlyphId16, NameId, Tag},
};

use crate::{
    error::Error,
    orchestration::{
        AnyWorkId, BeWork, Context, ExtraFeaTables, FeaFirstPassOutput, FeaRsKerns, FeaRsMarks,
        FeaSourceIdx, WorkId,
    },
};

mod feature_variations;
mod kern;
mod marks;
mod ot_tags;
pub mod properties;
#[cfg(test)]
mod test_helpers;

pub use kern::{create_gather_ir_kerning_work, create_kern_segment_work, create_kerns_work};
pub use marks::create_mark_work;

const DFLT_SCRIPT: Tag = Tag::new(b"DFLT");
const DFLT_LANG: Tag = Tag::new(b"dflt");

const CURS: Tag = Tag::new(b"curs");
const KERN: Tag = Tag::new(b"kern");
const DIST: Tag = Tag::new(b"dist");
const MARK: Tag = Tag::new(b"mark");
const MKMK: Tag = Tag::new(b"mkmk");
const ABVM: Tag = Tag::new(b"abvm");
const BLWM: Tag = Tag::new(b"blwm");

/// Parse, validate, and first-pass compile one of the font's FEA sources.
///
/// A designspace can have a features.fea per master; each distinct source is
/// compiled by its own instance of this work. Only the default master's output
/// ([`WorkId::DEFAULT_FEATURES_AST`]) is consumed today, see
/// [`FeatureCompilationWork`].
#[derive(Debug)]
pub struct FeatureFirstPassWork {
    idx: FeaSourceIdx,
}

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

pub struct FeaVariationInfo<'a> {
    axes: HashMap<Tag, (usize, &'a Axis)>,
    static_metadata: &'a StaticMetadata,
}

impl<'a> FeaVariationInfo<'a> {
    pub fn new(static_metadata: &'a StaticMetadata) -> FeaVariationInfo<'a> {
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
    gdef_categories: &GdefCategories,
    ast: &FeaFirstPassOutput,
    glyph_order: &GlyphOrder,
) -> HashMap<GlyphId16, GlyphClassDef> {
    ast.gdef_classes.clone().unwrap_or_else(|| {
        gdef_categories
            .categories
            .iter()
            .filter_map(|(name, category)| glyph_order.glyph_id(name).map(|gid| (gid, *category)))
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
        Cow::Owned(VariationModel::new(
            locations.into_iter().cloned().collect(),
            static_metadata.axes.axis_order(),
        ))
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
    // tags whose generated lookups must be appended after all user lookups
    append_features: Vec<Tag>,
}

impl<'a> FeatureWriter<'a> {
    fn new(
        kerning: &'a FeaRsKerns,
        marks: &'a FeaRsMarks,
        feature_variations: Option<FeatureVariationsProvider>,
        append_features: Vec<Tag>,
    ) -> Self {
        FeatureWriter {
            marks,
            kerning,
            feature_variations,
            append_features,
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
        // writers running in `append` mode ignore insertion markers; their
        // lookups land after all user-defined ones.
        for tag in &self.append_features {
            builder.force_append(*tag);
        }
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
        fontdrasil::variations::resolve_variable_metric(
            &self.static_metadata.variation_model,
            &self.static_metadata.axes,
            values,
        )
        .map_err(Error::DeltaError)
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

    #[allow(clippy::too_many_arguments)]
    fn compile(
        &self,
        static_metadata: &StaticMetadata,
        glyph_order: &GlyphOrder,
        ast: &FeaFirstPassOutput,
        kerns: &FeaRsKerns,
        marks: &FeaRsMarks,
        plan: &FeatureGenerationPlan,
        compile_debg: bool,
    ) -> Result<Compilation, Error> {
        let var_info = FeaVariationInfo::new(static_metadata);
        let feature_writer =
            self.feature_writer(static_metadata, glyph_order, kerns, marks, plan)?;
        // we've already validated the AST, so we only need to compile
        match fea_rs::compile::compile(
            &ast.ast,
            &marks.glyphmap,
            Some(&var_info),
            Some(&feature_writer),
            Opts::new().compile_debg(compile_debg),
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

    /// Compile each master's FEA on its own and merge the results.
    ///
    /// Used when the masters of a designspace do not all share a features.fea.
    /// Every master is compiled to pre-build state, those are merged into one
    /// variable compilation, and only then does the feature writer run: the
    /// generated kerning and marks, the `ItemVariationStore`, and everything
    /// else that exists once per font are produced a single time.
    #[allow(clippy::too_many_arguments)]
    fn compile_merged(
        &self,
        static_metadata: &StaticMetadata,
        glyph_order: &GlyphOrder,
        features: &FeatureSources,
        asts: &HashMap<FeaSourceIdx, Arc<FeaFirstPassOutput>>,
        kerns: &FeaRsKerns,
        marks: &FeaRsMarks,
        plan: &FeatureGenerationPlan,
        compile_debg: bool,
    ) -> Result<Compilation, Error> {
        let opts = Opts::new().compile_debg(compile_debg);
        let masters = master_compilations(features, asts, static_metadata, &marks.glyphmap, &opts)?;
        let var_info = FeaVariationInfo::new(static_metadata);
        let merged = fea_rs::compile::merge(masters, &var_info)?;

        let feature_writer =
            self.feature_writer(static_metadata, glyph_order, kerns, marks, plan)?;
        match merged.finish(Some(&feature_writer)) {
            Ok((result, warnings)) => {
                log_fea_warnings("compilation", &warnings);
                Ok(result)
            }
            Err(errors) => Err(Error::FeaCompileError(CompilerError::CompilationFail(
                errors,
            ))),
        }
    }

    fn feature_writer<'a>(
        &self,
        static_metadata: &StaticMetadata,
        glyph_order: &GlyphOrder,
        kerns: &'a FeaRsKerns,
        marks: &'a FeaRsMarks,
        plan: &FeatureGenerationPlan,
    ) -> Result<FeatureWriter<'a>, Error> {
        let feature_variations = static_metadata
            .variations
            .as_ref()
            .map(|ir_variations| {
                feature_variations::FeatureVariationsProvider::new(
                    ir_variations,
                    static_metadata,
                    glyph_order,
                )
            })
            .transpose()?;
        Ok(FeatureWriter::new(
            kerns,
            marks,
            feature_variations,
            append_forced_tags(plan),
        ))
    }
}

/// Compile each master's FEA into the pre-build state that [`merge`] consumes.
///
/// The masters that share a source each get their own compilation of it: the
/// values in a master's feature file are that master's contribution to the
/// variation model, not something to interpolate through. The default master
/// comes first, as `merge` requires.
///
/// [`merge`]: fea_rs::compile::merge
fn master_compilations(
    features: &FeatureSources,
    asts: &HashMap<FeaSourceIdx, Arc<FeaFirstPassOutput>>,
    static_metadata: &StaticMetadata,
    glyph_map: &GlyphMap,
    opts: &Opts,
) -> Result<Vec<(NormalizedLocation, PendingCompilation)>, Error> {
    let mut masters = Vec::new();
    for (idx, master) in features.iter().enumerate() {
        // a source with no master is font-wide (a .glyphs file); we only get
        // here when the sources are per-master, so this would be a bug
        if master.locations.is_empty() {
            return Err(Error::VariableFeaSourceWithoutMaster(
                master.source.to_string(),
            ));
        }
        let ast = asts
            .get(&idx)
            .unwrap_or_else(|| panic!("no first pass output for fea source {idx}"));
        for design_location in &master.locations {
            let location = design_location
                .to_normalized(&static_metadata.all_source_axes)
                .map_err(|error| Error::VariableFeaBadLocation {
                    fea: master.source.to_string(),
                    error,
                })?
                // point axes are not in the variation model
                .subset_axes(&static_metadata.axes);
            let pending = fea_rs::compile::compile_for_merge(&ast.ast, glyph_map, opts.clone())
                .map_err(|errors| Error::FeaCompileError(CompilerError::CompilationFail(errors)))?;
            masters.push((location, pending));
        }
    }

    let Some(default) = masters
        .iter()
        .position(|(location, _)| location == static_metadata.default_location())
    else {
        return Err(Error::VariableFeaNoDefaultMaster(features.n_sources()));
    };
    masters.swap(0, default);
    Ok(masters)
}

fn write_debug_glyph_order(debug_dir: &Path, glyphs: &GlyphOrder) {
    let glyph_order_file = debug_dir.join("glyph_order.txt");
    let glyph_order = glyphs.names().map(|g| g.as_str()).collect::<Vec<_>>();
    let glyph_order = glyph_order.join("\n");
    if let Err(e) = fs::write(&glyph_order_file, glyph_order) {
        log::error!("failed to write glyph order to {glyph_order_file:?}: '{e}'");
    }
}

fn write_debug_fea(
    context: &Context,
    is_error: bool,
    why: &str,
    fea_content: &str,
    idx: FeaSourceIdx,
) {
    let Some(debug_dir) = context.debug_dir.as_ref() else {
        if is_error {
            warn!("Debug fea not written for '{why}' because --emit-debug is off");
        }
        return;
    };
    // one file per source; the default master keeps the historical name
    let debug_file = match idx {
        0 => debug_dir.join("features.fea"),
        idx => debug_dir.join(format!("features_{idx}.fea")),
    };
    match fs::write(&debug_file, fea_content) {
        Ok(_) if is_error => warn!("{why}; fea written to {debug_file:?}"),
        Ok(_) => debug!("fea written to {debug_file:?}"),
        Err(e) => error!("{why}; failed to write fea to {debug_file:?}: {e}"),
    };
}

impl Work<Context, AnyWorkId, Error> for FeatureFirstPassWork {
    fn id(&self) -> AnyWorkId {
        WorkId::FeaturesAst(self.idx).into()
    }

    fn read_access(&self) -> Access<AnyWorkId> {
        AccessBuilder::new()
            .variant(FeWorkId::GlyphOrder)
            .variant(FeWorkId::StaticMetadata)
            .variant(FeWorkId::Features)
            .build()
    }

    #[tracing::instrument(name = "fontbe::FeatureFirstPassWork::exec", skip_all)]
    fn exec(&self, context: &Context) -> Result<(), Error> {
        let all_features = context.ir.features.get();
        let features = &all_features
            .get(self.idx)
            .unwrap_or_else(|| panic!("no fea source {}", self.idx))
            .source;
        let glyph_order = context.ir.glyph_order.get();
        let static_metadata = context.ir.static_metadata.get();
        let glyph_map = GlyphMap::new(glyph_order.names().cloned())?;

        let result = self.parse(features, &glyph_map);

        if self.is_default()
            && let Some(debug_dir) = context.debug_dir.as_ref()
        {
            write_debug_glyph_order(debug_dir, &glyph_order);
        }
        if let FeaturesSource::Memory { fea_content, .. } = features {
            write_debug_fea(
                context,
                result.is_err(),
                "compile failed",
                fea_content,
                self.idx,
            );
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
            .fea_asts
            .set(FeaFirstPassOutput::new(self.idx, ast, compilation)?);
        Ok(())
    }
}

impl FeatureFirstPassWork {
    pub fn create(idx: FeaSourceIdx) -> Box<BeWork> {
        Box::new(Self { idx })
    }

    /// True if this is the default master's source
    fn is_default(&self) -> bool {
        WorkId::FeaturesAst(self.idx) == WorkId::DEFAULT_FEATURES_AST
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
            .variant(FeWorkId::Features)
            // every master's fea, not just the default's: they must all
            // compile, and we need to know if they disagree
            .variant(WorkId::ALL_FEATURE_ASTS)
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

    #[tracing::instrument(name = "fontbe::FeatureCompilationWork::exec", skip_all)]
    fn exec(&self, context: &Context) -> Result<(), Error> {
        let static_metadata = context.ir.static_metadata.get();
        let gdef_categories = context.ir.gdef_categories.get();
        let features = context.ir.features.get();
        let glyph_order = context.ir.glyph_order.get();
        let kerns = context.fea_rs_kerns.get();
        let marks = context.fea_rs_marks.get();

        // Resolve the plan once for this work unit; separate work units (kern,
        // marks) resolve their own, since the work graph precludes threading one.
        let plan = ir::resolve_feature_generation(&static_metadata.misc.feature_generation);
        let mut result = if features.n_sources() > 1 {
            let asts = context
                .fea_asts
                .all()
                .into_iter()
                .map(|(_, ast)| (ast.idx, ast))
                .collect();
            self.compile_merged(
                &static_metadata,
                &glyph_order,
                &features,
                &asts,
                kerns.as_ref(),
                marks.as_ref(),
                &plan,
                context.compile_debg,
            )?
        } else {
            self.compile(
                &static_metadata,
                &glyph_order,
                &context.default_fea_ast(),
                kerns.as_ref(),
                marks.as_ref(),
                &plan,
                context.compile_debg,
            )?
        };
        if plan.gdef && result.gdef_classes.is_none() && !gdef_categories.categories.is_empty() {
            // the FEA did not contain an explicit GDEF block with glyph categories,
            // so let's use the ones from the source, if present (i.e. from
            // `public.openTypeCatgories` or computed from GlyphData.xml

            let class_def: ClassDef = gdef_categories
                .categories
                .iter()
                .filter_map(|(name, cls)| glyph_order.glyph_id(name).map(|id| (id, *cls as u16)))
                .collect();

            // could be class_def.is_empty() when
            // https://github.com/googlefonts/fontations/pull/1836 lands
            if class_def.iter().next().is_some() {
                let gdef = result.gdef.get_or_insert_with(Default::default);
                gdef.glyph_class_def.set(class_def);
            }
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
            context.extra_fea_tables.set(ExtraFeaTables::from(result));
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

/// The effective tags a writer generates, applying its `features` subset option.
fn settings_tags(settings: &FeatureGenerationSettings, all: &[Tag]) -> Vec<Tag> {
    match &settings.features {
        Some(subset) => all.iter().copied().filter(|t| subset.contains(t)).collect(),
        None => all.to_vec(),
    }
}

/// Tags whose generated lookups must be appended (insertion markers ignored).
///
/// A tag is force-appended exactly when its writer is enabled in `append` mode;
/// this is independent of the FEA, so it needs no AST.
fn append_forced_tags(plan: &FeatureGenerationPlan) -> Vec<Tag> {
    let mut tags = Vec::new();
    for (settings, all) in [
        (&plan.curs, [CURS].as_slice()),
        (&plan.kern, [KERN, DIST].as_slice()),
        (&plan.mark, [MARK, MKMK, ABVM, BLWM].as_slice()),
    ] {
        let Some(settings) = settings else { continue };
        if settings.mode == FeatureWriterMode::Append {
            tags.extend(settings_tags(settings, all));
        }
    }
    tags
}

/// Decide which of a writer's `tags` to generate and whether each is appended.
///
/// `settings` is `None` when the writer is disabled, in which case nothing
/// generates. In `Skip` mode a tag whose feature is manually declared in the FEA
/// without an insertion marker is dropped (fontc's historical behavior); `Append`
/// mode ignores markers and keeps every tag. The returned value maps each retained
/// tag to whether its lookups must be appended after all user lookups.
fn feature_writer_todo_list(
    tags: &[Tag],
    settings: Option<&FeatureGenerationSettings>,
    ast: &ParseTree,
) -> BTreeMap<Tag, bool> {
    use fea_rs::typed;
    let Some(settings) = settings else {
        return BTreeMap::new();
    };
    let wanted = settings_tags(settings, tags);
    if settings.mode == FeatureWriterMode::Append {
        return wanted.into_iter().map(|tag| (tag, true)).collect();
    }

    let wanted_set = wanted.iter().copied().collect::<HashSet<_>>();
    let mut existing_features = HashMap::new();
    for feature in ast
        .typed_root()
        .statements()
        .filter_map(typed::Feature::cast)
    {
        let tag = feature.tag().to_raw();
        if wanted_set.contains(&tag) {
            *existing_features.entry(tag).or_insert(false) |= feature.has_insert_marker();
        }
    }
    let mut result = wanted
        .into_iter()
        .map(|tag| (tag, false))
        .collect::<BTreeMap<_, _>>();
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
                    converter: CoordConverter::new(vec![], 0).unwrap(),
                    localized_names: Default::default(),
                },
            ],
            Default::default(),
            HashSet::from([min_wght, def_wght, max_wght]),
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
        let _ = tracing_subscriber::fmt().with_test_writer().try_init();
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

    #[test]
    fn variable_metric_bare_default_matches_explicit_default() {
        fn compile_layout_tables(fea: &str) -> Result<(Vec<u8>, Vec<u8>), DiagnosticSet> {
            let ast = parse_fea(fea);
            let static_metadata = weight_variable_static_metadata();
            let var_info = FeaVariationInfo::new(&static_metadata);
            let glyph_map = fea_rs::GlyphMap::new([".notdef", "p", "y"]).unwrap();

            let diagnostics = fea_rs::compile::validate(&ast, &glyph_map, Some(&var_info));
            assert!(!diagnostics.has_errors(), "{diagnostics:?}");

            let (compilation, _) = fea_rs::compile::compile::<
                _,
                fea_rs::compile::NopFeatureProvider,
            >(
                &ast, &glyph_map, Some(&var_info), None, Default::default()
            )?;
            Ok((
                write_fonts::dump_table(compilation.gpos.as_ref().unwrap()).unwrap(),
                write_fonts::dump_table(compilation.gdef.as_ref().unwrap()).unwrap(),
            ))
        }

        let bare_default =
            compile_layout_tables("feature kern {\n    pos p y (-12 wght=700:22);\n} kern;\n")
                .unwrap();
        let explicit_default = compile_layout_tables(
            "feature kern {\n    pos p y (wght=400:-12 wght=700:22);\n} kern;\n",
        )
        .unwrap();

        assert_eq!(bare_default, explicit_default);
        // Both `10` and `wght=400:20` target the default; reject rather than choose one.
        assert!(
            compile_layout_tables(
                "feature kern {\n    pos p y (10 wght=400:20 wght=700:30);\n} kern;\n",
            )
            .is_err()
        );
    }

    fn parse_fea(fea: &str) -> ParseTree {
        fea_rs::parse::parse_string(fea.to_string()).0
    }

    fn settings(mode: FeatureWriterMode) -> FeatureGenerationSettings {
        FeatureGenerationSettings {
            mode,
            features: None,
        }
    }

    // a manual kern block, optionally with an insertion marker
    fn manual_kern_fea(marker: bool) -> String {
        let marker = if marker { "# Automatic Code\n" } else { "" };
        format!("languagesystem DFLT dflt;\nfeature kern {{\n{marker}    pos A B -40;\n}} kern;\n")
    }

    #[test]
    fn todo_disabled_generates_nothing() {
        let ast = parse_fea("languagesystem DFLT dflt;");
        let todo = feature_writer_todo_list(&[KERN, DIST], None, &ast);
        assert!(todo.is_empty());
    }

    #[test]
    fn todo_skip_no_user_block() {
        let ast = parse_fea("languagesystem DFLT dflt;");
        let todo = feature_writer_todo_list(
            &[KERN, DIST],
            Some(&settings(FeatureWriterMode::Skip)),
            &ast,
        );
        assert_eq!(todo, BTreeMap::from([(KERN, false), (DIST, false)]));
    }

    #[test]
    fn todo_skip_manual_block_without_marker_drops_tag() {
        let ast = parse_fea(&manual_kern_fea(false));
        let todo = feature_writer_todo_list(
            &[KERN, DIST],
            Some(&settings(FeatureWriterMode::Skip)),
            &ast,
        );
        // kern is manually declared with no marker, so it drops; dist stays.
        assert_eq!(todo, BTreeMap::from([(DIST, false)]));
    }

    #[test]
    fn todo_skip_manual_block_with_marker_keeps_tag() {
        let ast = parse_fea(&manual_kern_fea(true));
        let todo = feature_writer_todo_list(
            &[KERN, DIST],
            Some(&settings(FeatureWriterMode::Skip)),
            &ast,
        );
        // the insertion marker means kern still generates, landing at the marker.
        assert_eq!(todo, BTreeMap::from([(KERN, false), (DIST, false)]));
    }

    #[test]
    fn todo_append_ignores_markerless_manual_block() {
        let ast = parse_fea(&manual_kern_fea(false));
        let todo = feature_writer_todo_list(
            &[KERN, DIST],
            Some(&settings(FeatureWriterMode::Append)),
            &ast,
        );
        // append mode ignores markers: every tag stays and is force-appended.
        assert_eq!(todo, BTreeMap::from([(KERN, true), (DIST, true)]));
    }

    #[test]
    fn todo_respects_features_subset() {
        let ast = parse_fea("languagesystem DFLT dflt;");
        let settings = FeatureGenerationSettings {
            mode: FeatureWriterMode::Skip,
            features: Some(vec![KERN]),
        };
        let todo = feature_writer_todo_list(&[KERN, DIST], Some(&settings), &ast);
        assert_eq!(todo, BTreeMap::from([(KERN, false)]));
    }

    #[test]
    fn append_forced_tags_only_for_append_writers() {
        // key absent -> everything enabled in skip mode -> nothing appended.
        let plan = ir::resolve_feature_generation(&None);
        assert!(append_forced_tags(&plan).is_empty());

        // kern in append mode -> its tags are forced; disabled writers contribute none.
        let plan = ir::resolve_feature_generation(&Some(vec![
            fontir::ir::FeatureWriterSpec {
                writer: fontir::ir::KnownFeatureWriter::Kern,
                mode: FeatureWriterMode::Append,
                features: None,
            },
            fontir::ir::FeatureWriterSpec {
                writer: fontir::ir::KnownFeatureWriter::Curs,
                mode: FeatureWriterMode::Skip,
                features: None,
            },
        ]));
        let tags = append_forced_tags(&plan)
            .into_iter()
            .collect::<HashSet<_>>();
        assert_eq!(tags, HashSet::from([KERN, DIST]));
    }
}
