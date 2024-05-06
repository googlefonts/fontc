//! Feature binary compilation.

use std::{
    cell::RefCell,
    collections::HashMap,
    ffi::{OsStr, OsString},
    fmt::Display,
    fs,
    path::PathBuf,
    sync::Arc,
    time::Instant,
};

use log::{debug, error, trace, warn};
use ordered_float::OrderedFloat;

use fea_rs::{
    compile::{error::CompilerError, Compilation, FeatureBuilder, FeatureProvider, VariationInfo},
    parse::{FileSystemResolver, SourceLoadError, SourceResolver},
    DiagnosticSet, GlyphMap, Opts, ParseTree,
};

use fontir::{
    ir::{FeaturesSource, GlyphOrder, StaticMetadata},
    orchestration::{Flags, WorkId as FeWorkId},
    variations::DeltaError,
};

use fontdrasil::{
    coords::NormalizedLocation,
    orchestration::{Access, AccessBuilder, Work},
    types::Axis,
};
use write_fonts::{
    tables::layout::LookupFlag, tables::variations::VariationRegion, types::Tag, OtRound,
};

use crate::{
    error::Error,
    orchestration::{AnyWorkId, BeWork, Context, FeaAst, FeaRsKerns, FeaRsMarks, WorkId},
};

mod common;
mod kern;
mod marks;
mod ot_tags;
mod properties;

pub(crate) use common::PendingLookup;
pub use kern::{create_gather_ir_kerning_work, create_kern_segment_work, create_kerns_work};
pub use marks::create_mark_work;

#[derive(Debug)]
pub struct FeatureParsingWork {}

#[derive(Debug)]
pub struct FeatureCompilationWork {}

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

//NOTE: this is basically identical to the same method on FeaVariationInfo,
//except they have slightly different inputs?
pub(crate) fn resolve_variable_metric<'a>(
    static_metadata: &StaticMetadata,
    values: impl Iterator<Item = (&'a NormalizedLocation, &'a OrderedFloat<f32>)>,
) -> Result<(i16, Vec<(VariationRegion, i16)>), DeltaError> {
    let var_model = &static_metadata.variation_model;

    let point_seqs = values
        .into_iter()
        .map(|(pos, value)| (pos.to_owned(), vec![value.0 as f64]))
        .collect();
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
    timing: RefCell<Vec<(&'static str, Instant)>>,
}

impl<'a> FeatureWriter<'a> {
    fn new(kerning: &'a FeaRsKerns, marks: &'a FeaRsMarks) -> Self {
        FeatureWriter {
            marks,
            kerning,
            timing: Default::default(),
        }
    }

    /// We did most of the work in the kerning job, take the data and populate a builder
    fn add_kerning_features(&self, builder: &mut FeatureBuilder) -> Result<(), Error> {
        if self.kerning.is_empty() {
            return Ok(());
        }
        // convert the lookups into lookup ids
        let lookup_ids = self
            .kerning
            .lookups
            .iter()
            .map(|lookup| {
                builder.add_lookup(
                    lookup.flags,
                    lookup.mark_filter_set.clone(),
                    lookup.subtables.clone(),
                )
            })
            .collect::<Vec<_>>();

        for (feature, ids) in &self.kerning.features {
            // get the generated lookup ids based on the stored lookup indices
            let ids = ids.iter().map(|idx| lookup_ids[*idx]).collect();
            builder.add_feature(*feature, ids);
        }

        {
            self.timing
                .borrow_mut()
                .push(("End add kerning", Instant::now()));
        }
        Ok(())
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
    fn add_marks(&self, builder: &mut FeatureBuilder) -> Result<(), Error> {
        {
            self.timing
                .borrow_mut()
                .push(("Start add marks", Instant::now()));
        }
        let marks = self.marks;

        // Build the actual mark base and mark mark constructs using fea-rs builders

        let mut mark_base_lookups = Vec::new();
        let mut mark_mark_lookups = Vec::new();

        for mark_base in marks.mark_base.iter() {
            // each mark to base it's own lookup, whch differs from fontmake
            mark_base_lookups.push(builder.add_lookup(
                LookupFlag::default(),
                None,
                vec![mark_base.to_owned()],
            ));
        }

        // If a mark has anchors that are themselves marks what we got here is a mark to mark
        for mark_mark in marks.mark_mark.iter() {
            mark_mark_lookups.push(builder.add_lookup(
                LookupFlag::default(),
                None,
                vec![mark_mark.to_owned()],
            ));
        }

        if !mark_base_lookups.is_empty() {
            builder.add_to_default_language_systems(Tag::new(b"mark"), &mark_base_lookups);
        }
        if !mark_mark_lookups.is_empty() {
            builder.add_to_default_language_systems(Tag::new(b"mkmk"), &mark_mark_lookups);
        }

        {
            self.timing
                .borrow_mut()
                .push(("End add marks", Instant::now()));
        }
        Ok(())
    }
}

impl<'a> FeatureProvider for FeatureWriter<'a> {
    fn add_features(&self, builder: &mut FeatureBuilder) {
        // TODO where my error handling
        self.add_kerning_features(builder).unwrap();
        self.add_marks(builder).unwrap();
    }
}

impl<'a> VariationInfo for FeaVariationInfo<'a> {
    type Error = Error;
    fn axis(&self, axis_tag: Tag) -> Option<(usize, &Axis)> {
        self.axes.get(&axis_tag).map(|(i, a)| (*i, *a))
    }

    fn resolve_variable_metric(
        &self,
        values: &HashMap<NormalizedLocation, i16>,
    ) -> Result<(i16, Vec<(VariationRegion, i16)>), Error> {
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
                return Err(Error::NoVariationModel(loc.clone()));
            }
        }

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
}

impl FeatureCompilationWork {
    pub fn create() -> Box<BeWork> {
        Box::new(FeatureCompilationWork {})
    }

    fn compile(
        &self,
        static_metadata: &StaticMetadata,
        ast: &FeaAst,
        kerns: &FeaRsKerns,
        marks: &FeaRsMarks,
    ) -> Result<Compilation, Error> {
        let var_info = FeaVariationInfo::new(static_metadata);
        let feature_writer = FeatureWriter::new(kerns, marks);
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
    let glyph_order = glyphs.iter().map(|g| g.to_string()).collect::<Vec<_>>();
    let glyph_order = glyph_order.join("\n");
    if let Err(e) = fs::write(glyph_order_file, glyph_order) {
        log::error!("failed to write glyph order to debug/glyph_order.txt: '{e}'");
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
        Ok(_) if is_error => warn!("{}; fea written to {:?}", why, debug_file),
        Ok(_) => debug!("fea written to {:?}", debug_file),
        Err(e) => error!("{}; failed to write fea to {:?}: {}", why, debug_file, e),
    };
}

impl Work<Context, AnyWorkId, Error> for FeatureParsingWork {
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
        let glyph_map = glyph_order.iter().cloned().collect();

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

        context.fea_ast.set(ast.into());
        Ok(())
    }
}

impl FeatureParsingWork {
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

fn get_resolver_and_root_path(features: &FeaturesSource) -> (Box<dyn SourceResolver>, OsString) {
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
                fea_file.to_owned().into_os_string(),
            )
        }
        FeaturesSource::Memory {
            fea_content,
            include_dir,
        } => (
            Box::new(InMemoryResolver {
                include_dir: include_dir.to_owned(),
                content_path: OsString::new(),
                content: fea_content.as_str().into(),
            }),
            OsString::new(),
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
        ]
    }

    fn exec(&self, context: &Context) -> Result<(), Error> {
        let static_metadata = context.ir.static_metadata.get();
        let ast = context.fea_ast.get();
        let kerns = context.fea_rs_kerns.get();
        let marks = context.fea_rs_marks.get();

        let result = self.compile(&static_metadata, &ast, kerns.as_ref(), marks.as_ref())?;

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

fn log_fea_warnings(stage: &str, warnings: &DiagnosticSet) {
    assert!(!warnings.has_errors(), "of course we checked this already");
    if !warnings.is_empty() {
        log::debug!(
            "FEA {stage} produced {} warnings:\n{}",
            warnings.len(),
            warnings.display()
        );
    }
}

#[cfg(test)]
mod tests {
    use std::collections::{HashMap, HashSet};

    use fea_rs::compile::VariationInfo;
    use fontdrasil::{
        coords::{CoordConverter, DesignCoord, NormalizedCoord, UserCoord},
        types::Axis,
    };
    use fontir::ir::StaticMetadata;

    use super::*;

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
            Default::default(),
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
