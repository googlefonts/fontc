//! Generic model of font sources.

use std::{
    collections::{HashMap, HashSet},
    path::Path,
};

use fontdrasil::orchestration::Work;
use fontdrasil::{
    coords::{CoordConverter, NormalizedLocation, UserCoord, UserLocation},
    types::{Axes, Axis},
};
use serde::Deserialize;
use write_fonts::types::{NameId, Tag};

use crate::{
    error::Error,
    ir::{NameKey, NamedInstance, StaticMetadata},
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

    fn exec(&self, context: &Context) -> Result<(), Error> {
        let mut axes = self
            .config
            .axis
            .iter()
            .map(|(tag_name, axis)| {
                let tag = tag_name.parse::<Tag>().map_err(|cause| Error::InvalidTag {
                    raw_tag: tag_name.clone(),
                    cause,
                })?;
                let positions = self
                    .config
                    .master
                    .values()
                    .map(|master| {
                        master
                            .position
                            .get(tag_name)
                            .copied()
                            .unwrap_or(axis.default)
                    })
                    .collect::<Vec<_>>();
                let min = positions
                    .iter()
                    .copied()
                    .reduce(f64::min)
                    .unwrap_or(axis.default);
                let max = positions
                    .iter()
                    .copied()
                    .reduce(f64::max)
                    .unwrap_or(axis.default);

                Ok(Axis {
                    name: axis.name.clone(),
                    tag,
                    min: UserCoord::new(min),
                    default: UserCoord::new(axis.default),
                    max: UserCoord::new(max),
                    hidden: false,
                    converter: CoordConverter::default_normalization(
                        UserCoord::new(min),
                        UserCoord::new(axis.default),
                        UserCoord::new(max),
                    ),
                    localized_names: Default::default(),
                })
            })
            .collect::<Result<Vec<_>, Error>>()?;
        axes.sort_by_key(|axis| axis.tag);

        let defaults: HashMap<_, _> = axes.iter().map(|axis| (axis.tag, axis.default)).collect();
        let axis_tags = axes.iter().map(|axis| axis.tag).collect::<HashSet<_>>();
        let all_axes = Axes::new(axes.clone());

        let mut global_locations = HashSet::new();
        let mut named_instances = Vec::new();
        let mut masters = self.config.master.iter().collect::<Vec<_>>();
        masters.sort_by_key(|(name, _)| *name);
        for (_, master) in masters {
            let location: UserLocation = axis_tags
                .iter()
                .map(|tag| {
                    (
                        *tag,
                        UserCoord::new(
                            master
                                .position
                                .get(&tag.to_string())
                                .copied()
                                .unwrap_or_else(|| defaults[tag].to_f64()),
                        ),
                    )
                })
                .collect();
            global_locations.insert(location.to_normalized(&all_axes)?);
            named_instances.push(NamedInstance {
                name: master.style_name.clone(),
                postscript_name: None,
                location,
            });
        }

        let names = HashMap::from([
            (
                NameKey::new(NameId::FAMILY_NAME, &self.config.family),
                self.config.family.clone(),
            ),
            (
                NameKey::new(NameId::TYPOGRAPHIC_FAMILY_NAME, &self.config.family),
                self.config.family.clone(),
            ),
        ]);
        let static_metadata = StaticMetadata::new(
            1024,
            names,
            axes,
            named_instances,
            global_locations,
            None,
            0.0,
            None,
            false,
        )?;
        context.static_metadata.set(static_metadata);
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use fontdrasil::orchestration::Work;

    fn config() -> EmojiConfig {
        EmojiConfig {
            family: "Test Color".to_string(),
            output_file: "test.ttf".to_string(),
            color_format: "glyf_colr_1".to_string(),
            clipbox_quantization: 32,
            axis: HashMap::from([(
                "wght".to_string(),
                EmojiAxis {
                    name: "Weight".to_string(),
                    default: 400.0,
                },
            )]),
            master: HashMap::from([
                (
                    "bold".to_string(),
                    EmojiMaster {
                        style_name: "Bold".to_string(),
                        srcs: vec!["bold.svg".to_string()],
                        position: HashMap::from([(String::from("wght"), 700.0)]),
                    },
                ),
                (
                    "regular".to_string(),
                    EmojiMaster {
                        style_name: "Regular".to_string(),
                        srcs: vec!["regular.svg".to_string()],
                        position: HashMap::new(),
                    },
                ),
            ]),
        }
    }

    fn execute(config: EmojiConfig) -> std::sync::Arc<StaticMetadata> {
        let work = EmojiWork { config };
        let root = Context::new_root(Flags::empty());
        let context = root.copy_for_work(work.read_access(), work.write_access());
        work.exec(&context).unwrap();
        root.static_metadata.get()
    }

    #[test]
    fn builds_static_metadata_from_config() {
        let metadata = execute(config());
        let axis = metadata.all_source_axes.iter().next().unwrap();

        assert_eq!(metadata.units_per_em, 1024);
        assert_eq!(axis.name, "Weight");
        assert_eq!(axis.min.to_f64(), 400.0);
        assert_eq!(axis.default.to_f64(), 400.0);
        assert_eq!(axis.max.to_f64(), 700.0);
        assert_eq!(
            metadata
                .names
                .get(&NameKey::new(NameId::FAMILY_NAME, "Test Color")),
            Some(&"Test Color".to_string())
        );
        assert_eq!(
            metadata
                .named_instances
                .iter()
                .map(|instance| instance.name.as_str())
                .collect::<Vec<_>>(),
            vec!["Bold", "Regular"]
        );
    }

    #[test]
    fn rejects_invalid_axis_tags() {
        let mut config = config();
        config.axis.insert(
            "invalid".to_string(),
            EmojiAxis {
                name: "Invalid".to_string(),
                default: 0.0,
            },
        );
        let work = EmojiWork { config };
        let result = work.exec(&Context::new_root(Flags::empty()));

        assert!(matches!(result, Err(Error::InvalidTag { raw_tag, .. }) if raw_tag == "invalid"));
    }
}
