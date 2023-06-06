//! tracking changes during compilation

use std::{ffi::OsStr, fs, path::Path};

use fontbe::{orchestration::WorkId as BeWorkIdentifier, paths::Paths as BePaths};

use crate::{Config, Error};
use fontdrasil::types::GlyphName;
use fontir::{
    orchestration::WorkId as FeWorkIdentifier,
    paths::Paths as IrPaths,
    source::{Input, Source},
};
use glyphs2fontir::source::GlyphsIrSource;
use ufo2fontir::source::DesignSpaceIrSource;

use indexmap::IndexSet;
use regex::Regex;

//FIXME: clarify the role of this type.
/// Tracks changes during incremental compilation and... what, exactly?
pub struct ChangeDetector {
    glyph_name_filter: Option<Regex>,
    ir_paths: IrPaths,
    ir_source: Box<dyn Source>,
    prev_inputs: Input,
    current_inputs: Input,
    be_paths: BePaths,
    emit_ir: bool,
}

impl ChangeDetector {
    pub fn new(
        config: Config,
        ir_paths: IrPaths,
        prev_inputs: Input,
    ) -> Result<ChangeDetector, Error> {
        // What sources are we dealing with?
        let mut ir_source = ir_source(&config.args.source)?;
        let mut current_inputs = ir_source.inputs().map_err(Error::FontIrError)?;
        let be_paths = BePaths::new(ir_paths.build_dir());

        let glyph_name_filter = config
            .args
            .glyph_name_filter
            .map(|raw_filter| Regex::new(&raw_filter))
            .transpose()
            .map_err(Error::BadRegex)?;

        if let Some(regex) = &glyph_name_filter {
            current_inputs.glyphs.retain(|glyph_name, _| {
                let result = regex.is_match(glyph_name.as_str());
                if !result {
                    log::trace!("'{glyph_name}' does not match --glyph_name_filter");
                }
                result
            });
        }

        Ok(ChangeDetector {
            glyph_name_filter,
            ir_paths,
            ir_source,
            prev_inputs,
            current_inputs,
            be_paths,
            emit_ir: config.args.emit_ir,
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

    pub fn init_static_metadata_ir_change(&self) -> bool {
        self.current_inputs.static_metadata != self.prev_inputs.static_metadata
            || !self
                .ir_paths
                .target_file(&FeWorkIdentifier::InitStaticMetadata)
                .is_file()
    }

    pub fn final_static_metadata_ir_change(&self) -> bool {
        self.current_inputs.static_metadata != self.prev_inputs.static_metadata
            || !self
                .ir_paths
                .target_file(&FeWorkIdentifier::FinalizeStaticMetadata)
                .is_file()
    }

    pub fn global_metrics_ir_change(&self) -> bool {
        self.current_inputs.global_metrics != self.prev_inputs.global_metrics
            || !self
                .ir_paths
                .target_file(&FeWorkIdentifier::GlobalMetrics)
                .is_file()
    }

    pub fn feature_ir_change(&self) -> bool {
        self.final_static_metadata_ir_change()
            || self.current_inputs.features != self.prev_inputs.features
            || !self
                .ir_paths
                .target_file(&FeWorkIdentifier::Features)
                .is_file()
    }

    pub fn feature_be_change(&self) -> bool {
        self.feature_ir_change()
            || !self
                .be_paths
                .target_file(&BeWorkIdentifier::Features)
                .is_file()
    }

    pub fn avar_be_change(&self) -> bool {
        self.final_static_metadata_ir_change()
            || !self.be_paths.target_file(&BeWorkIdentifier::Avar).is_file()
    }

    pub fn stat_be_change(&self) -> bool {
        self.final_static_metadata_ir_change()
            || !self.be_paths.target_file(&BeWorkIdentifier::Stat).is_file()
    }

    pub fn fvar_be_change(&self) -> bool {
        self.final_static_metadata_ir_change()
            || !self.be_paths.target_file(&BeWorkIdentifier::Fvar).is_file()
    }

    pub fn post_be_change(&self) -> bool {
        self.final_static_metadata_ir_change()
            || !self.be_paths.target_file(&BeWorkIdentifier::Post).is_file()
    }

    pub fn glyphs_changed(&self) -> IndexSet<GlyphName> {
        let glyph_iter = self.current_inputs.glyphs.iter();

        if self.init_static_metadata_ir_change() {
            return glyph_iter.map(|(name, _)| name).cloned().collect();
        }
        glyph_iter
            .filter_map(
                |(glyph_name, curr_state)| match self.prev_inputs.glyphs.get(glyph_name) {
                    Some(prev_state) => {
                        // If the input changed or the output doesn't exist a rebuild is probably in order
                        (prev_state != curr_state
                            || !self
                                .ir_paths
                                .target_file(&FeWorkIdentifier::Glyph(glyph_name.clone()))
                                .exists())
                        .then_some(glyph_name)
                    }
                    None => Some(glyph_name),
                },
            )
            .cloned()
            .collect()
    }

    pub fn glyphs_deleted(&self) -> IndexSet<GlyphName> {
        self.prev_inputs
            .glyphs
            .keys()
            .filter(|glyph_name| !self.current_inputs.glyphs.contains_key(glyph_name))
            .cloned()
            .collect()
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
        "designspace" => Ok(Box::new(DesignSpaceIrSource::new(source.to_path_buf()))),
        "glyphs" => Ok(Box::new(GlyphsIrSource::new(source.to_path_buf()))),
        _ => Err(Error::UnrecognizedSource(source.to_path_buf())),
    }
}
