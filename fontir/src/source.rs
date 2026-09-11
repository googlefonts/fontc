//! Generic model of font sources.

use std::{collections::HashMap, path::Path};

use fontdrasil::coords::NormalizedLocation;
use fontdrasil::orchestration::Work;
use serde::Deserialize;

use crate::{
    error::Error,
    orchestration::{Context, Flags, IrWork, WorkId},
};

/// The typed representation of a nanoemoji-style COLRv1 configuration.
#[derive(Clone, Debug, Deserialize)]
pub struct EmojiConfig {
    pub family: String,
    pub output_file: String,
    pub color_format: String,
    pub clipbox_quantization: u16,
    #[serde(default)]
    pub axis: HashMap<String, EmojiAxis>,
    #[serde(default)]
    pub master: HashMap<String, EmojiMaster>,
}

#[derive(Clone, Debug, Deserialize)]
pub struct EmojiAxis {
    pub name: String,
    pub default: f64,
}

#[derive(Clone, Debug, Deserialize)]
pub struct EmojiMaster {
    pub style_name: String,
    pub srcs: Vec<String>,
    #[serde(default)]
    pub position: HashMap<String, f64>,
}

/// A source of data from which one could compile a font.
///
/// Expected to be implemented once per font format, e.g. one for .glyphs, one for ufo+ds, etc.
pub trait Source {
    /// path is to the root entry, e.g. .glyphs file, .designspace, etc
    fn new(root: &Path) -> Result<Self, Error>
    where
        Self: Sized;

    /// Create a function that could be called to generate [crate::ir::StaticMetadata].
    ///
    /// When run work should update [crate::orchestration::Context] with new [crate::ir::StaticMetadata].
    fn create_static_metadata_work(&self) -> Result<Box<IrWork>, Error>;

    /// Create a function that could be called to generate [crate::ir::StaticMetadata].
    ///
    /// When run work should update[crate::orchestration::Context] with new [crate::ir::GlobalMetrics].
    fn create_global_metric_work(&self) -> Result<Box<IrWork>, Error>;

    /// Create a function that could be called to generate IR for glyphs.
    ///
    /// Batched because some formats require IO to figure out the work.
    /// Expected to return a Vec aligned with the glyph_names input. That is,
    /// result vec nth entry is the work for the nth glyph name.
    ///
    /// When run work should update [crate::orchestration::Context] with [crate::ir::Glyph] and [crate::ir::Anchor]
    /// for the glyph name.
    fn create_glyph_ir_work(&self) -> Result<Vec<Box<IrWork>>, Error>;

    /// Create a function that could be called to generate or identify fea file(s).
    ///
    /// When run work should update [crate::orchestration::Context] with [crate::ir::FeatureSources].
    fn create_feature_ir_work(&self) -> Result<Box<IrWork>, Error>;

    /// Create a function that could be called to produce the kerning locations.
    ///
    /// When run work should update [crate::orchestration::Context] with [crate::ir::KerningLocations].
    fn create_kerning_locations_ir_work(&self) -> Result<Box<IrWork>, Error>;

    /// Create a function that could be called to generate or identify kerning for a location.
    ///
    /// When run work should update [crate::orchestration::Context] with [crate::ir::KerningInstance].
    fn create_kerning_instance_ir_work(&self, at: NormalizedLocation)
    -> Result<Box<IrWork>, Error>;

    /// Create a function that could be called to generate [crate::ir::ColorPalettes].
    ///
    /// When run work should update [crate::orchestration::Context] with new [crate::ir::ColorPalettes].
    fn create_color_palette_work(&self) -> Result<Box<IrWork>, Error>;

    /// Create a function that could be called to generate [crate::ir::ColorGlyphs].
    ///
    /// When run work should update [crate::orchestration::Context] with new [crate::ir::ColorGlyphs].
    fn create_color_glyphs_work(&self) -> Result<Box<IrWork>, Error>;

    /// Returns compilation flags derived from source file settings.
    ///
    /// CLI flags will be combined with these using bitwise OR.
    /// See <https://github.com/googlefonts/fontc/issues/1701>
    fn compilation_flags(&self) -> Flags {
        Flags::empty() // default: no flags from source
    }
}

/// A source backed by a nanoemoji-style COLRv1 configuration.
///
/// The configuration is read by the compiler frontend before this source is
/// constructed. The work needed to turn that configuration into IR is not
/// implemented yet.
#[derive(Debug)]
pub struct EmojiSource {
    config: EmojiConfig,
}

impl EmojiSource {
    /// Construct an emoji source from an already-read configuration.
    pub fn from_config(config: EmojiConfig) -> Self {
        Self { config }
    }
}

impl Source for EmojiSource {
    fn new(_root: &Path) -> Result<Self, Error> {
        todo!()
    }

    fn create_static_metadata_work(&self) -> Result<Box<IrWork>, Error> {
        Ok(Box::new(EmojiWork {
            config: self.config.clone(),
        }))
    }

    fn create_global_metric_work(&self) -> Result<Box<IrWork>, Error> {
        todo!()
    }

    fn create_glyph_ir_work(&self) -> Result<Vec<Box<IrWork>>, Error> {
        todo!()
    }

    fn create_feature_ir_work(&self) -> Result<Box<IrWork>, Error> {
        todo!()
    }

    fn create_kerning_locations_ir_work(&self) -> Result<Box<IrWork>, Error> {
        todo!()
    }

    fn create_kerning_instance_ir_work(
        &self,
        _at: NormalizedLocation,
    ) -> Result<Box<IrWork>, Error> {
        todo!()
    }

    fn create_color_palette_work(&self) -> Result<Box<IrWork>, Error> {
        todo!()
    }

    fn create_color_glyphs_work(&self) -> Result<Box<IrWork>, Error> {
        todo!()
    }
}

#[derive(Debug)]
struct EmojiWork {
    config: EmojiConfig,
}

impl Work<Context, WorkId, Error> for EmojiWork {
    fn id(&self) -> WorkId {
        WorkId::StaticMetadata
    }

    fn exec(&self, _context: &Context) -> Result<(), Error> {
        let _ = &self.config;
        todo!()
    }
}
