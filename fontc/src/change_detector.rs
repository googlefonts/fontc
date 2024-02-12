//! tracking changes during compilation

use std::{ffi::OsStr, fmt::Debug, fs, path::Path};

use bitflags::bitflags;
use fontbe::{
    orchestration::{AnyWorkId, WorkId as BeWorkIdentifier},
    paths::Paths as BePaths,
};
use fontra2fontir::source::FontraIrSource;

use crate::{create_timer, timing::JobTimer, work::AnyWork, workload::Workload, Config, Error};
use fontdrasil::{coords::NormalizedLocation, types::GlyphName};
use fontir::{
    orchestration::WorkId as FeWorkIdentifier,
    paths::Paths as IrPaths,
    source::{Input, Source},
};
use glyphs2fontir::source::GlyphsIrSource;
use ufo2fontir::source::DesignSpaceIrSource;

use indexmap::IndexSet;
use regex::Regex;

bitflags! {
    #[derive(Clone, Copy, Debug)]
    pub struct BuildFlags: u32 {
        /// We need to generate the direct output artifacts
        const BUILD_OUTPUTS = 0b0001;
        /// We need to rebuild anything that depends on us
        const BUILD_DEPENDENTS = 0b0010;
        const BUILD_ALL = Self::BUILD_OUTPUTS.bits() | Self::BUILD_DEPENDENTS.bits();
    }
}

/// Figures out what changed and helps create work to build updated versions
///
/// Uses [Source] to abstract over whether source in .glyphs, .designspace, etc.
pub struct ChangeDetector {
    glyph_name_filter: Option<Regex>,
    ir_paths: IrPaths,
    ir_source: Box<dyn Source>,
    prev_inputs: Input,
    current_inputs: Input,
    be_paths: BePaths,
    emit_ir: bool,
    skip_features: bool,
    static_metadata_changed: bool,
    glyph_order_changed: bool,
    glyphs_changed: IndexSet<GlyphName>,
    glyphs_deleted: IndexSet<GlyphName>,
}

impl Debug for ChangeDetector {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "ChangeDetector")
    }
}

impl ChangeDetector {
    pub fn new(
        config: Config,
        ir_paths: IrPaths,
        be_paths: BePaths,
        prev_inputs: Input,
        timer: &mut JobTimer,
    ) -> Result<ChangeDetector, Error> {
        let time = create_timer(AnyWorkId::InternalTiming("new change detector"), 0)
            .queued()
            .run();

        let mut ir_source = ir_source(config.args.source())?;
        let mut current_inputs = ir_source.inputs().map_err(Error::FontIrError)?;

        let glyph_name_filter = config
            .args
            .glyph_name_filter
            .clone()
            .map(|reg| reg.into_inner());

        if let Some(regex) = &glyph_name_filter {
            current_inputs.glyphs.retain(|glyph_name, _| {
                let result = regex.is_match(glyph_name.as_str());
                if !result {
                    log::trace!("'{glyph_name}' does not match --glyph_name_filter");
                }
                result
            });
        }

        let static_metadata_changed = current_inputs.static_metadata != prev_inputs.static_metadata
            || !ir_paths
                .target_file(&FeWorkIdentifier::StaticMetadata)
                .is_file();

        let glyph_order_changed = static_metadata_changed
            || !ir_paths
                .target_file(&FeWorkIdentifier::GlyphOrder)
                .is_file();

        let glyphs_changed = if static_metadata_changed || glyph_order_changed {
            current_inputs.glyphs.keys().cloned().collect()
        } else {
            current_inputs
                .glyphs
                .iter()
                .filter_map(
                    |(glyph_name, curr_state)| match prev_inputs.glyphs.get(glyph_name) {
                        Some(prev_state) => {
                            // If the input changed or the output doesn't exist a rebuild is probably in order
                            (prev_state != curr_state
                                || !ir_paths
                                    .target_file(&FeWorkIdentifier::Glyph(glyph_name.clone()))
                                    .exists())
                            .then_some(glyph_name)
                        }
                        None => Some(glyph_name),
                    },
                )
                .cloned()
                .collect()
        };

        let glyphs_deleted = prev_inputs
            .glyphs
            .keys()
            .filter(|glyph_name| !current_inputs.glyphs.contains_key(*glyph_name))
            .cloned()
            .collect();

        timer.add(time.complete());

        Ok(ChangeDetector {
            glyph_name_filter,
            ir_paths,
            ir_source,
            prev_inputs,
            current_inputs,
            be_paths,
            emit_ir: config.args.incremental,
            skip_features: config.args.skip_features,
            static_metadata_changed,
            glyph_order_changed,
            glyphs_changed,
            glyphs_deleted,
        })
    }

    pub fn glyph_name_filter(&self) -> Option<&Regex> {
        self.glyph_name_filter.as_ref()
    }

    pub fn current_inputs(&self) -> &Input {
        &self.current_inputs
    }

    pub fn ir_source(&self) -> &dyn Source {
        self.ir_source.as_ref()
    }

    pub fn ir_paths(&self) -> &IrPaths {
        &self.ir_paths
    }

    pub fn be_paths(&self) -> &BePaths {
        &self.be_paths
    }

    fn target_exists(&self, work_id: &AnyWorkId) -> bool {
        match work_id {
            AnyWorkId::Fe(work_id) => self.ir_paths.target_file(work_id).is_file(),
            AnyWorkId::Be(work_id) => self.be_paths.target_file(work_id).is_file(),
            AnyWorkId::InternalTiming(..) => false,
        }
    }

    fn input_changed(&self, work_id: &AnyWorkId) -> bool {
        match work_id {
            AnyWorkId::Fe(FeWorkIdentifier::StaticMetadata) => {
                self.current_inputs.static_metadata != self.prev_inputs.static_metadata
            }
            AnyWorkId::Fe(FeWorkIdentifier::GlobalMetrics) => {
                self.current_inputs.global_metrics != self.prev_inputs.global_metrics
            }
            AnyWorkId::Be(BeWorkIdentifier::GlyfFragment(glyph_name)) => {
                self.current_inputs.glyphs.get(glyph_name)
                    != self.prev_inputs.glyphs.get(glyph_name)
            }
            AnyWorkId::Be(BeWorkIdentifier::GvarFragment(glyph_name)) => {
                self.current_inputs.glyphs.get(glyph_name)
                    != self.prev_inputs.glyphs.get(glyph_name)
            }
            _ => panic!("input_changed does not yet support {work_id:?}"),
        }
    }

    fn output_exists(&self, work: &AnyWork) -> bool {
        self.target_exists(&work.id())
            && work
                .also_completes()
                .iter()
                .all(|id| self.target_exists(id))
    }

    pub fn should_skip_features(&self) -> bool {
        self.skip_features
    }

    /// Not all work ... works ... with this method; notably muts support input_changed.
    pub(crate) fn simple_should_run(&self, work: &AnyWork) -> bool {
        let work_id = work.id();
        !self.output_exists(work) || self.input_changed(&work_id)
    }

    /// Simple work has simple, static, dependencies. Anything that depends on all-of-type
    /// (e.g. all glyph ir) is not (yet) amenable to this path.
    pub fn add_simple_work(&self, workload: &mut Workload, work: AnyWork) {
        let output_exists = self.output_exists(&work);
        let input_changed = self.input_changed(&work.id());
        let run = input_changed || !output_exists;
        workload.add(work, run);
    }

    pub fn create_workload(&mut self, timer: JobTimer) -> Result<Workload, Error> {
        let mut workload = Workload::new(self, timer);

        // Create work roughly in the order it would typically occur
        // Work is eligible to run as soon as all dependencies are complete
        // so this is NOT the definitive execution order

        let source = self.ir_source.as_ref();

        // Source => IR
        self.add_simple_work(
            &mut workload,
            source
                .create_static_metadata_work(&self.current_inputs)?
                .into(),
        );
        self.add_simple_work(
            &mut workload,
            source
                .create_global_metric_work(&self.current_inputs)?
                .into(),
        );

        Ok(workload)
    }

    // TODO: could this be solved based on work id and also completes?

    pub fn static_metadata_ir_change(&self) -> bool {
        self.static_metadata_changed
    }

    pub fn glyph_order_ir_change(&self) -> bool {
        self.glyph_order_changed
    }

    pub fn global_metrics_ir_change(&self) -> bool {
        self.current_inputs.global_metrics != self.prev_inputs.global_metrics
            || !self
                .ir_paths
                .target_file(&FeWorkIdentifier::GlobalMetrics)
                .is_file()
    }

    pub fn feature_ir_change(&self) -> bool {
        self.glyph_order_ir_change()
            || self.current_inputs.features != self.prev_inputs.features
            || !self
                .ir_paths
                .target_file(&FeWorkIdentifier::Features)
                .is_file()
            || !self
                .ir_paths
                .target_file(&FeWorkIdentifier::KerningGroups)
                .is_file()
    }

    pub fn feature_be_change(&self) -> bool {
        self.feature_ir_change()
            || self.kerning_be_change()
            || self.mark_be_change()
            || !self
                .be_paths
                .target_file(&BeWorkIdentifier::Features)
                .is_file()
    }

    pub fn mark_be_change(&self) -> bool {
        // Glyphs produce anchors and we need anchors
        !self.glyphs_changed.is_empty()
            || !self
                .be_paths
                .target_file(&BeWorkIdentifier::Marks)
                .is_file()
    }

    pub fn kerning_be_change(&self) -> bool {
        self.kerning_groups_ir_change()
            || !self
                .be_paths
                .target_file(&BeWorkIdentifier::GatherIrKerning)
                .is_file()
    }

    pub fn kerning_groups_ir_change(&self) -> bool {
        self.static_metadata_ir_change()
            || self.glyph_order_ir_change()
            || self.current_inputs.features != self.prev_inputs.features
            || !self
                .ir_paths
                .target_file(&FeWorkIdentifier::KerningGroups)
                .is_file()
    }

    pub fn kerning_at_ir_change(&self, at: NormalizedLocation) -> bool {
        self.kerning_groups_ir_change()
            || self.current_inputs.features != self.prev_inputs.features
            || !self
                .ir_paths
                .target_file(&FeWorkIdentifier::KernInstance(at))
                .is_file()
    }

    pub fn avar_be_change(&self) -> bool {
        self.static_metadata_ir_change()
            || !self.be_paths.target_file(&BeWorkIdentifier::Avar).is_file()
    }

    pub fn stat_be_change(&self) -> bool {
        self.static_metadata_ir_change()
            || !self.be_paths.target_file(&BeWorkIdentifier::Stat).is_file()
    }

    pub fn fvar_be_change(&self) -> bool {
        self.static_metadata_ir_change()
            || !self.be_paths.target_file(&BeWorkIdentifier::Fvar).is_file()
    }

    pub fn post_be_change(&self) -> bool {
        self.static_metadata_ir_change()
            || self.glyph_order_ir_change()
            || !self.be_paths.target_file(&BeWorkIdentifier::Post).is_file()
    }

    pub fn glyphs_changed(&self) -> &IndexSet<GlyphName> {
        &self.glyphs_changed
    }

    pub fn glyphs_deleted(&self) -> &IndexSet<GlyphName> {
        &self.glyphs_deleted
    }

    pub fn finish_successfully(self) -> Result<(), Error> {
        if self.emit_ir {
            let current_sources =
                serde_yaml::to_string(&self.current_inputs).map_err(Error::YamlSerError)?;
            fs::write(self.ir_paths.ir_input_file(), current_sources).map_err(Error::IoError)
        } else {
            Ok(())
        }
    }
}

fn ir_source(source: &Path) -> Result<Box<dyn Source>, Error> {
    if !source.exists() {
        return Err(Error::FileExpected(source.to_path_buf()));
    }
    let ext = source
        .extension()
        .and_then(OsStr::to_str)
        .ok_or_else(|| Error::UnrecognizedSource(source.to_path_buf()))?;
    match ext {
        "designspace" => Ok(Box::new(DesignSpaceIrSource::new(source.to_path_buf())?)),
        "ufo" => Ok(Box::new(DesignSpaceIrSource::new(source.to_path_buf())?)),
        "glyphs" => Ok(Box::new(GlyphsIrSource::new(source.to_path_buf()))),
        "glyphspackage" => Ok(Box::new(GlyphsIrSource::new(source.to_path_buf()))),
        "fontra" => Ok(Box::new(FontraIrSource::new(source.to_path_buf())?)),
        _ => Err(Error::UnrecognizedSource(source.to_path_buf())),
    }
}
