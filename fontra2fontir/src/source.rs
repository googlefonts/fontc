use std::{collections::HashSet, path::Path, sync::Arc};

use fontdrasil::{
    coords::{CoordConverter, DesignCoord, NormalizedLocation, UserCoord},
    orchestration::{Access, AccessBuilder, Work},
    types::GlyphName,
};
use fontir::{
    error::Error,
    ir::{FeatureSources, FeaturesSource, KerningInstance, PreliminaryGdefCategories},
    orchestration::{Context, IrWork, WorkId},
    source::Source,
};
use log::{debug, warn};

use crate::{
    fontra::{Font, Kerning},
    kernutils::{
        classify_glyphs_by_direction, flip_kerning_direction, merge_kerning,
        split_kerning_by_direction,
    },
    toir::{
        kerning_sources, to_ir_gdef_categories, to_ir_global_metrics, to_ir_kern_groups,
        to_ir_kerning_instance, to_ir_kerning_locations, to_ir_static_metadata,
    },
};

pub struct FontraIrSource {
    font_data: Arc<Font>,
    gdef_categories: Arc<PreliminaryGdefCategories>,
}

impl Source for FontraIrSource {
    fn new(fontra_dir: &Path) -> Result<Self, Error> {
        let mut font_data = Font::load(fontra_dir)?;
        pin_discrete_axes(&mut font_data)?;
        if let Some(kerning) = font_data
            .kerning
            .get(HORIZONTAL_KERNING_TYPE)
            .map(|kerning| {
                let axes: Vec<_> = font_data
                    .axes
                    .axes
                    .iter()
                    .filter_map(|axis| match axis {
                        crate::fontra::Axis::Continuous(axis) => Some(axis.clone()),
                        crate::fontra::Axis::Discrete(_) => None,
                    })
                    .collect();
                let feature_text = match font_data.features.language.as_str() {
                    "fea" => font_data.features.text.as_str(),
                    _ => "",
                };
                let (ltr_glyphs, rtl_glyphs) = classify_glyphs_by_direction(
                    &font_data.glyph_map,
                    feature_text,
                    &axes,
                    fontra_dir,
                );
                flip_rtl_kerning(kerning, &ltr_glyphs, &rtl_glyphs)
            })
        {
            font_data
                .kerning
                .insert(HORIZONTAL_KERNING_TYPE.to_string(), kerning);
        }
        let gdef_categories = to_ir_gdef_categories(&font_data.glyph_infos);

        Ok(FontraIrSource {
            font_data: Arc::new(font_data),
            gdef_categories: Arc::new(gdef_categories),
        })
    }

    fn create_static_metadata_work(&self) -> Result<Box<IrWork>, Error> {
        Ok(Box::new(StaticMetadataWork {
            font_data: self.font_data.clone(),
            gdef_categories: self.gdef_categories.clone(),
        }))
    }

    fn create_global_metric_work(&self) -> Result<Box<IrWork>, Error> {
        Ok(Box::new(GlobalMetricsWork {
            font_data: self.font_data.clone(),
        }))
    }

    fn create_glyph_ir_work(&self) -> Result<Vec<Box<IrWork>>, Error> {
        Ok(Vec::new())
    }

    fn create_feature_ir_work(&self) -> Result<Box<IrWork>, Error> {
        Ok(Box::new(FeatureWork {
            font_data: self.font_data.clone(),
        }))
    }

    fn create_kerning_locations_ir_work(&self) -> Result<Box<IrWork>, Error> {
        Ok(Box::new(KerningLocationsWork {
            font_data: self.font_data.clone(),
        }))
    }

    fn create_kerning_instance_ir_work(
        &self,
        at: NormalizedLocation,
    ) -> Result<Box<IrWork>, Error> {
        Ok(Box::new(KerningInstanceWork {
            font_data: self.font_data.clone(),
            location: at,
        }))
    }

    fn create_color_palette_work(&self) -> Result<Box<IrWork>, Error> {
        Ok(Box::new(NoopWork(WorkId::ColorPalettes)))
    }

    fn create_color_glyphs_work(&self) -> Result<Box<IrWork>, Error> {
        Ok(Box::new(NoopWork(WorkId::PaintGraph)))
    }
}

/// <https://github.com/fontra/fontra/blob/2a19b8bd1/src/fontra/backends/designspace.py#L1490-L1503>
fn flip_rtl_kerning(
    kerning: &Kerning,
    ltr_glyphs: &HashSet<GlyphName>,
    rtl_glyphs: &HashSet<GlyphName>,
) -> Kerning {
    if rtl_glyphs.is_empty() {
        return kerning.clone();
    }

    // UFO3's kerning direction is "writing direction", but kerning in Fontra
    // is "visual left to right", so let's flip any right-to-left kerning.
    let (ltr_kerning, rtl_kerning) = split_kerning_by_direction(kerning, ltr_glyphs, rtl_glyphs);
    let rtl_kerning = flip_kerning_direction(&rtl_kerning);
    merge_kerning(&ltr_kerning, &rtl_kerning)
}

/// Pin every discrete axis to its default value like Fontra's
/// [`subset-axes`](https://github.com/fontra/fontra/blob/2a19b8bd1/src/fontra/workflow/actions/axes.py#L231-L300)
/// filter.
fn pin_discrete_axes(font_data: &mut Font) -> Result<(), Error> {
    let mut pinned: Vec<(String, f64)> = Vec::new();
    for axis in font_data.axes.axes.iter() {
        let crate::fontra::Axis::Discrete(axis) = axis else {
            continue;
        };
        // Source locations are in design space, the axis default is in
        // user space, so map it through the axis mapping, like Fontra's
        // getDefaultSourceLocation:
        // https://github.com/fontra/fontra/blob/2a19b8bd1/src/fontra/workflow/actions/axes.py#L385-L393
        let default = if axis.mapping.is_empty() {
            axis.default_value
        } else {
            let examples = axis
                .mapping
                .iter()
                .map(|[user, design]| (UserCoord::new(*user), DesignCoord::new(*design)))
                .collect();
            let converter = CoordConverter::new(examples, 0)?;
            UserCoord::new(axis.default_value)
                .to_design(&converter)
                .to_f64()
        };
        pinned.push((axis.name.to_string(), default));
    }
    if pinned.is_empty() {
        return Ok(());
    }
    for (name, default) in &pinned {
        warn!("pinning discrete axis {name:?} to its default {default}");
    }
    font_data
        .axes
        .axes
        .retain(|axis| matches!(axis, crate::fontra::Axis::Continuous(_)));

    let at_default = |location: &crate::fontra::Location| {
        pinned
            .iter()
            .all(|(name, default)| location.get(name).map(|v| v == default).unwrap_or(true))
    };
    font_data
        .sources
        .retain(|_, source| at_default(&source.location));
    let retained: std::collections::HashSet<String> = font_data.sources.keys().cloned().collect();

    for kerning in font_data.kerning.values_mut() {
        let keep: Vec<bool> = kerning
            .source_identifiers
            .iter()
            .map(|identifier| retained.contains(identifier))
            .collect();
        if keep.iter().all(|keep| *keep) {
            continue;
        }
        kerning.source_identifiers = kerning
            .source_identifiers
            .iter()
            .zip(&keep)
            .filter(|(_, keep)| **keep)
            .map(|(identifier, _)| identifier.clone())
            .collect();
        for side2_values in kerning.values.values_mut() {
            for values in side2_values.values_mut() {
                *values = values
                    .iter()
                    .zip(&keep)
                    .filter(|(_, keep)| **keep)
                    .map(|(value, _)| *value)
                    .collect();
            }
        }
    }
    Ok(())
}

#[derive(Debug)]
struct StaticMetadataWork {
    font_data: Arc<Font>,
    gdef_categories: Arc<PreliminaryGdefCategories>,
}

impl Work<Context, WorkId, Error> for StaticMetadataWork {
    fn id(&self) -> WorkId {
        WorkId::StaticMetadata
    }

    fn also_completes(&self) -> Vec<WorkId> {
        vec![
            WorkId::PreliminaryGlyphOrder,
            WorkId::PreliminaryGdefCategories,
        ]
    }

    #[tracing::instrument(name = "fontra2fontir::StaticMetadataWork::exec", skip_all)]
    fn exec(&self, context: &Context) -> Result<(), Error> {
        debug!(
            "Static metadata for {}",
            self.font_data
                .font_info
                .family_name
                .as_deref()
                .unwrap_or("<nameless family>")
        );
        context
            .preliminary_glyph_order
            .set(self.font_data.glyph_map.keys().cloned().collect());
        context
            .preliminary_gdef_categories
            .set(self.gdef_categories.as_ref().clone());
        context
            .static_metadata
            .set(to_ir_static_metadata(&self.font_data)?);
        Ok(())
    }
}

#[derive(Debug)]
struct GlobalMetricsWork {
    font_data: Arc<Font>,
}

impl Work<Context, WorkId, Error> for GlobalMetricsWork {
    fn id(&self) -> WorkId {
        WorkId::GlobalMetrics
    }

    fn read_access(&self) -> Access<WorkId> {
        Access::Variant(WorkId::StaticMetadata)
    }

    fn exec(&self, context: &Context) -> Result<(), Error> {
        debug!(
            "Global metrics for {}",
            self.font_data
                .font_info
                .family_name
                .as_deref()
                .unwrap_or("<nameless family>")
        );
        let static_metadata = context.static_metadata.get();
        context
            .global_metrics
            .set(to_ir_global_metrics(&static_metadata, &self.font_data)?);
        Ok(())
    }
}

#[derive(Debug)]
struct FeatureWork {
    font_data: Arc<Font>,
}

impl Work<Context, WorkId, Error> for FeatureWork {
    fn id(&self) -> WorkId {
        WorkId::Features
    }

    fn exec(&self, context: &Context) -> Result<(), Error> {
        debug!("Generate features");
        let features = &self.font_data.features;
        // "fea" is the only feature format Fontra supports. Skip anything else.
        let source = match features.language.as_str() {
            "fea" if !features.text.is_empty() => {
                FeaturesSource::from_string(features.text.clone())
            }
            "fea" => FeaturesSource::empty(),
            other => {
                if !features.text.is_empty() {
                    warn!("Ignoring features in unsupported language {other:?}");
                }
                FeaturesSource::empty()
            }
        };
        context.features.set(FeatureSources::single(source));
        Ok(())
    }
}

pub(crate) const HORIZONTAL_KERNING_TYPE: &str = "kern";
// TODO: support other kerning types

#[derive(Debug)]
struct KerningLocationsWork {
    font_data: Arc<Font>,
}

impl Work<Context, WorkId, Error> for KerningLocationsWork {
    fn id(&self) -> WorkId {
        WorkId::KerningLocations
    }

    fn read_access(&self) -> Access<WorkId> {
        Access::Variant(WorkId::StaticMetadata)
    }

    fn exec(&self, context: &Context) -> Result<(), Error> {
        debug!("Generate IR for kerning");
        for kern_type in self.font_data.kerning.keys() {
            if kern_type != HORIZONTAL_KERNING_TYPE {
                warn!("the {kern_type:?} kerning is dropped");
            }
        }
        let static_metadata = context.static_metadata.get();
        context.kerning_locations.set(to_ir_kerning_locations(
            &static_metadata,
            &self.font_data,
            self.font_data.kerning.get(HORIZONTAL_KERNING_TYPE),
        ));
        Ok(())
    }
}

#[derive(Debug)]
struct KerningInstanceWork {
    font_data: Arc<Font>,
    location: NormalizedLocation,
}

impl Work<Context, WorkId, Error> for KerningInstanceWork {
    fn id(&self) -> WorkId {
        WorkId::KernInstance(self.location.clone())
    }

    fn read_access(&self) -> Access<WorkId> {
        AccessBuilder::new()
            .variant(WorkId::StaticMetadata)
            .variant(WorkId::GlyphOrder)
            .build()
    }

    fn exec(&self, context: &Context) -> Result<(), Error> {
        debug!("Generate kerning at {:?}", self.location);
        let static_metadata = context.static_metadata.get();
        let glyph_order = context.glyph_order.get();

        let mut instance = KerningInstance {
            location: self.location.clone(),
            ..Default::default()
        };
        if let Some(kerning) = self.font_data.kerning.get(HORIZONTAL_KERNING_TYPE) {
            let source_idx = kerning_sources(&static_metadata, &self.font_data, kerning)
                .find(|(_, location)| *location == self.location)
                .map(|(idx, _)| idx);
            match source_idx {
                Some(idx) => {
                    instance = to_ir_kerning_instance(kerning, idx, &self.location, &glyph_order)
                }
                // The default location has no kerning, but we still want the groups.
                None => instance.groups = to_ir_kern_groups(kerning, &glyph_order),
            }
        }
        context.kerning_at.set(instance);
        Ok(())
    }
}

/// A work that produces nothing.
#[derive(Debug)]
struct NoopWork(WorkId);

impl Work<Context, WorkId, Error> for NoopWork {
    fn id(&self) -> WorkId {
        self.0.clone()
    }

    fn exec(&self, _context: &Context) -> Result<(), Error> {
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use fontdrasil::{
        orchestration::{Access, AccessBuilder},
        types::GlyphName,
    };
    use fontir::{ir::NameKey, orchestration::Flags};
    use pretty_assertions::assert_eq;
    use write_fonts::{
        tables::gdef::GlyphClassDef,
        types::{NameId, Tag},
    };

    use crate::test::testdata_dir;

    use super::*;

    fn context_for(fontra_dir: &str) -> (FontraIrSource, Context) {
        let source = FontraIrSource::new(&testdata_dir().join(fontra_dir)).unwrap();
        (source, Context::new_root(Flags::empty()))
    }

    #[test]
    fn pin_discrete_axes_drops_off_default_sources() {
        let mut font_data = Font::load(&testdata_dir().join("MutatorSans.fontra")).unwrap();
        assert_eq!(3, font_data.axes.axes.len());
        assert!(font_data.sources.contains_key("light-condensed-italic"));

        pin_discrete_axes(&mut font_data).unwrap();

        assert_eq!(
            vec!["weight", "width"],
            font_data
                .axes
                .axes
                .iter()
                .map(|a| a.name().as_str())
                .collect::<Vec<_>>()
        );
        assert!(!font_data.sources.contains_key("light-condensed-italic"));
        let kerning = font_data.kerning.get(HORIZONTAL_KERNING_TYPE).unwrap();
        assert!(
            !kerning
                .source_identifiers
                .contains(&"light-condensed-italic".to_string())
        );
        for side2_values in kerning.values.values() {
            for values in side2_values.values() {
                assert!(values.len() <= kerning.source_identifiers.len());
            }
        }
    }

    #[test]
    fn pin_discrete_axes_keeps_kern_values_aligned() {
        // The dropped source is not the last one, so the remaining values
        // have to be picked out rather than truncated.
        let mut font_data = Font::load(&testdata_dir().join("MutatorSans.fontra")).unwrap();
        let kerning = font_data.kerning.get_mut(HORIZONTAL_KERNING_TYPE).unwrap();
        kerning.source_identifiers = vec![
            "light-condensed-italic".to_string(),
            "light-condensed".to_string(),
            "bold-condensed".to_string(),
        ];
        for side2_values in kerning.values.values_mut() {
            for values in side2_values.values_mut() {
                *values = vec![Some(-1.0), Some(-2.0), Some(-3.0)];
            }
        }

        pin_discrete_axes(&mut font_data).unwrap();

        let kerning = font_data.kerning.get(HORIZONTAL_KERNING_TYPE).unwrap();
        assert_eq!(
            vec!["light-condensed", "bold-condensed"],
            kerning.source_identifiers
        );
        let values = kerning
            .values
            .values()
            .next()
            .unwrap()
            .values()
            .next()
            .unwrap();
        assert_eq!(&vec![Some(-2.0), Some(-3.0)], values);
    }

    #[test]
    fn compile_raqq() {
        let (source, context) = context_for("Raqq.fontra");

        let task_context = context.copy_for_work(
            Access::None,
            AccessBuilder::new()
                .variant(WorkId::StaticMetadata)
                .variant(WorkId::PreliminaryGlyphOrder)
                .variant(WorkId::PreliminaryGdefCategories)
                .build(),
        );
        source
            .create_static_metadata_work()
            .unwrap()
            .exec(&task_context)
            .unwrap();

        let static_metadata = context.static_metadata.get();
        assert_eq!(800, static_metadata.units_per_em);
        assert_eq!(
            vec![Tag::new(b"SPAC"), Tag::new(b"MSHQ")],
            static_metadata
                .axes
                .iter()
                .map(|a| a.tag)
                .collect::<Vec<_>>()
        );
        let name = |id: NameId| {
            static_metadata
                .names
                .get(&NameKey::new_bmp_only(id))
                .map(String::as_str)
        };
        assert_eq!(Some("Raqq"), name(NameId::FAMILY_NAME));
        assert_eq!(Some("Regular"), name(NameId::SUBFAMILY_NAME));
        assert_eq!(
            Some("Copyright 2021–2024 The Raqq Project Authors (github.com/aliftype/raqq)"),
            name(NameId::COPYRIGHT_NOTICE)
        );
        assert_eq!(Some("Khaled Hosny"), name(NameId::DESIGNER));
        assert_eq!(Some("Alif Type"), name(NameId::MANUFACTURER));
        assert_eq!(Some("https://aliftype.com"), name(NameId::VENDOR_URL));
        // Synthesized from versionMajor/versionMinor and vendorID.
        assert_eq!(Some("Version 0.000"), name(NameId::VERSION_STRING));
        assert_eq!(Some("0.000;ALIF;Raqq-Regular"), name(NameId::UNIQUE_ID));

        let glyph_order = context.preliminary_glyph_order.get();
        assert!(glyph_order.contains(&GlyphName::new(".notdef")));
        assert!(glyph_order.contains(&GlyphName::new("beh-ar")));

        let gdef = context.preliminary_gdef_categories.get();
        assert!(gdef.infer_from_anchors);
        assert_eq!(
            Some(&GlyphClassDef::Mark),
            gdef.categories.get(&GlyphName::new("dammatan-ar"))
        );
        assert_eq!(
            Some(&GlyphClassDef::Ligature),
            gdef.categories.get(&GlyphName::new("fehDotless_alef-ar"))
        );
        assert!(
            gdef.mark_category_glyphs
                .contains(&GlyphName::new("dammatan-ar"))
        );
    }

    fn glyph_map(fontra_dir: &str) -> Vec<(GlyphName, Vec<u32>)> {
        let source = FontraIrSource::new(&testdata_dir().join(fontra_dir)).unwrap();
        source
            .font_data
            .glyph_map
            .iter()
            .map(|(name, codepoints)| (name.clone(), codepoints.clone()))
            .collect()
    }

    #[test]
    fn glyph_map_of_minimal() {
        assert_eq!(
            vec![(GlyphName::new(".notdef"), vec![])],
            glyph_map("minimal.fontra")
        );
    }

    #[test]
    fn glyph_map_of_2glyphs() {
        assert_eq!(
            vec![
                (GlyphName::new(".notdef"), vec![]),
                (GlyphName::new("u20089"), vec![0x20089]),
            ],
            glyph_map("2glyphs.fontra")
        );
    }

    #[test]
    fn glyph_map_0_1_n_codepoints() {
        assert_eq!(
            vec![
                (".notdef", vec![]),
                ("A", vec![0x0041, 0x0061]),
                ("Aacute", vec![0x00C1, 0x00E1]),
                (
                    "handshake_mediumlight_medium",
                    vec![0x1FAF1, 0x1F3FC, 0x200D, 0x1FAF2, 0x1F3FD]
                ),
                ("space", vec![0x0020]),
            ]
            .into_iter()
            .map(|(name, codepoints)| (GlyphName::new(name), codepoints))
            .collect::<Vec<_>>(),
            glyph_map("codepoints.fontra"),
        )
    }
}
