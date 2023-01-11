//! Feature binary compilation.

use std::path::{Path, PathBuf};

use fea_rs::{Diagnostic, GlyphMap, GlyphName as FeaRsGlyphName};
use log::{debug, trace, warn};
use write_fonts::FontBuilder;

use fontdrasil::orchestration::Work;

use crate::{
    error::Error,
    orchestration::{BeWork, Context},
};

pub struct FeatureWork {
    build_dir: PathBuf,
}

impl FeatureWork {
    pub fn create(build_dir: &Path) -> Box<BeWork> {
        let build_dir = build_dir.to_path_buf();
        Box::from(FeatureWork { build_dir })
    }
}

fn check_diagnostics(
    fea_file: &Path,
    op: &str,
    diagnostics: &Vec<Diagnostic>,
    formatter: impl Fn(&Diagnostic) -> String,
) -> Result<(), Error> {
    let mut err = false;
    for diagnostic in diagnostics {
        if diagnostic.is_error() {
            warn!("{:?} {} error {}", fea_file, op, formatter(diagnostic));
            err = true;
        } else {
            debug!("{:?} {} {}", fea_file, op, formatter(diagnostic));
        }
    }
    if err {
        return Err(Error::FeaError(format!("{:?} {} failed", fea_file, op)));
    }
    Ok(())
}

impl FeatureWork {
    /// Inspired by (as in shameless copy of) how the fea-rs binary flows.
    fn compile(&self, fea_file: &Path, glyph_order: GlyphMap) -> Result<FontBuilder, Error> {
        // Will you not parse?!
        let parse =
            fea_rs::parse_root_file(fea_file, Some(&glyph_order), Some(self.build_dir.clone()))
                .map_err(|e| Error::FeaError(format!("{:?} parsing {:?}", e, fea_file)))?;
        let (tree, diagnostics) = parse.generate_parse_tree();
        check_diagnostics(fea_file, "generate parse tree", &diagnostics, |d| {
            format!("{:?}", d)
        })?;

        // Maybe even compile?
        let compilation = match fea_rs::compile::compile(&tree, &glyph_order) {
            Ok(compilation) => {
                check_diagnostics(fea_file, "compile", &compilation.warnings, |d| {
                    tree.format_diagnostic(d)
                })?;
                trace!("Compiled {:?} successfully", fea_file);
                compilation
            }
            Err(errors) => {
                check_diagnostics(fea_file, "compile", &errors, |d| tree.format_diagnostic(d))?;
                unreachable!("errors aren't ... errors?!");
            }
        };

        // Capture the binary tables we got from the features for future merge into final font
        // TODO do we want to do the whole blob or to emit table-by-table?
        let font = compilation
            .build_raw(&glyph_order, Default::default())
            .map_err(|_| {
                Error::FeaError(format!(
                    "{:?} build_raw failed; no useful diagnostic available",
                    fea_file
                ))
            })?;
        Ok(font)
    }
}

impl Work<Context, Error> for FeatureWork {
    fn exec(&self, context: &Context) -> Result<(), Error> {
        let features = context.ir.get_features();
        let fea_file = match &features.fea_file {
            Some(file) => file,
            None => {
                // set a default in place so subsequent compiles skip this step
                trace!("No fea file, dull compile");
                context.set_features(FontBuilder::default());
                return Ok(());
            }
        };
        let glyph_order = &context.ir.get_static_metadata().glyph_order;
        if glyph_order.is_empty() {
            warn!("Glyph order is empty; feature compile improbable");
        }
        let glyph_map = glyph_order
            .iter()
            .map(|n| Into::<FeaRsGlyphName>::into(n.as_str()))
            .collect();
        let font = self.compile(fea_file, glyph_map)?;
        context.set_features(font);
        Ok(())
    }
}
