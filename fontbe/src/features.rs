//! Feature binary compilation.

use std::{
    collections::HashSet,
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
use fontir::{
    ir::Features,
    orchestration::{Flags, WorkId as FeWorkId},
};
use log::{debug, error, warn};

use fontdrasil::orchestration::{Access, Work};

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

impl Work<Context, AnyWorkId, Error> for FeatureWork {
    fn id(&self) -> AnyWorkId {
        WorkId::Features.into()
    }

    fn read_access(&self) -> Access<AnyWorkId> {
        Access::Set(HashSet::from([
            FeWorkId::Features.into(),
            FeWorkId::GlyphOrder.into(),
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
        let features = context.ir.features.get();
        if !matches!(*features, Features::Empty) {
            let glyph_order = &context.ir.glyph_order.get();
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
        } else {
            debug!("No fea file, dull compile");
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
