//! Feature binary compilation.

use std::{
    ffi::{OsStr, OsString},
    fmt::Display,
    fs,
    sync::Arc,
};

use fea_rs::{
    compile::Compilation,
    parse::{SourceLoadError, SourceResolver},
    Compiler, GlyphMap, GlyphName as FeaRsGlyphName,
};
use fontir::{ir::Features, orchestration::Flags};
use log::{debug, error, warn};

use fontdrasil::orchestration::Work;

use crate::{
    error::Error,
    orchestration::{BeWork, Context, WorkId},
};

pub struct FeatureWork {}

// I did not want to make a struct
// I did not want to clone the content
// I do not like this construct
// I do find the need to lament
struct InMemoryResolver {
    content_path: OsString,
    content: Arc<str>,
}

impl SourceResolver for InMemoryResolver {
    fn get_contents(&self, path: &OsStr) -> Result<Arc<str>, SourceLoadError> {
        if path == &*self.content_path {
            return Ok(self.content.clone());
        }
        Err(SourceLoadError::new(
            path.to_os_string(),
            NotSupportedError::new(),
        ))
    }
}

#[derive(Debug)]
struct NotSupportedError {}

impl NotSupportedError {
    fn new() -> NotSupportedError {
        NotSupportedError {}
    }
}

impl std::error::Error for NotSupportedError {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        None
    }
}

impl Display for NotSupportedError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("Not supported")?;
        Ok(())
    }
}

impl FeatureWork {
    pub fn create() -> Box<BeWork> {
        Box::new(FeatureWork {})
    }

    fn compile(&self, features: &Features, glyph_order: GlyphMap) -> Result<Compilation, Error> {
        let compiler = match features {
            Features::File {
                fea_file,
                include_dir,
            } => {
                let mut compiler = Compiler::new(OsString::from(fea_file), &glyph_order);
                if let Some(include_dir) = include_dir {
                    compiler = compiler.with_project_root(include_dir)
                }
                compiler
            }
            Features::Memory(fea_content) => {
                let root = OsString::new();
                Compiler::new(root.clone(), &glyph_order).with_resolver(InMemoryResolver {
                    content_path: root,
                    content: Arc::from(fea_content.as_str()),
                })
            }
            Features::Empty => panic!("compile isn't supposed to be called for Empty"),
        };
        compiler.compile().map_err(Error::FeaCompileError)
    }
}

fn write_debug_fea(context: &Context, is_error: bool, why: &str, fea_content: &str) {
    let debug_file = context.debug_dir().join("glyphs.fea");
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

impl Work<Context, WorkId, Error> for FeatureWork {
    fn id(&self) -> WorkId {
        WorkId::Features
    }

    fn exec(&self, context: &Context) -> Result<(), Error> {
        let features = context.ir.get_features();
        if !matches!(*features, Features::Empty) {
            let glyph_order = &context.ir.get_final_static_metadata().glyph_order;
            if glyph_order.is_empty() {
                warn!("Glyph order is empty; feature compile improbable");
            }
            let glyph_map = glyph_order
                .iter()
                .map(|n| Into::<FeaRsGlyphName>::into(n.as_str()))
                .collect();

            let result = self.compile(&features, glyph_map);
            if result.is_err() || context.flags.contains(Flags::EMIT_DEBUG) {
                if let Features::Memory(fea_content) = &*features {
                    write_debug_fea(context, result.is_err(), "compile failed", fea_content);
                }
            }
            let result = result?;

            debug!(
                "Built features, gpos? {} gsub? {}",
                result.gpos.is_some(),
                result.gsub.is_some()
            );
            if let Some(gpos) = result.gpos {
                context.set_gpos(gpos);
            }
            if let Some(gsub) = result.gsub {
                context.set_gsub(gsub);
            }
        } else {
            debug!("No fea file, dull compile");
        }
        // Enables the assumption that if the file exists features were compiled
        if context.flags.contains(Flags::EMIT_IR) {
            fs::write(context.paths.target_file(&WorkId::Features), "1").map_err(Error::IoError)?;
        }
        Ok(())
    }
}
