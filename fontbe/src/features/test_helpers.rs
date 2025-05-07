//! Helpers for testing layout code

use std::{collections::HashSet, path::Path, sync::Arc};

use fea_rs::compile::{Compilation, FeatureProvider};
use fontdrasil::{coords::NormalizedLocation, types::Axes};
use fontir::ir::{GdefCategories, GlyphOrder, NamedInstance, StaticMetadata};

use crate::orchestration::FeaFirstPassOutput;

use super::FeaVariationInfo;

/// A builder for constructing  [`LayoutOutput`]
pub(crate) struct LayoutOutputBuilder {
    axes: Axes,
    categories: Option<GdefCategories>,
    named_instances: Vec<NamedInstance>,
    glyph_locations: HashSet<NormalizedLocation>,
    user_fea: Arc<str>,
    glyph_order: GlyphOrder,
}

/// A helper for compiling layout tables (including with user-provided features)
pub(crate) struct LayoutOutput {
    pub(crate) static_metadata: StaticMetadata,
    pub(crate) glyph_order: GlyphOrder,
    pub(crate) first_pass_fea: FeaFirstPassOutput,
}

impl LayoutOutputBuilder {
    pub(crate) fn new() -> Self {
        Default::default()
    }

    pub(crate) fn with_axes(&mut self, axes: Axes) -> &mut Self {
        self.axes = axes;
        self
    }

    pub(crate) fn with_categories(&mut self, categories: GdefCategories) -> &mut Self {
        self.categories = categories.into();
        self
    }

    pub(crate) fn with_instances(&mut self, instances: Vec<NamedInstance>) -> &mut Self {
        self.named_instances = instances;
        self
    }

    pub(crate) fn with_locations(&mut self, locations: HashSet<NormalizedLocation>) -> &mut Self {
        self.glyph_locations = locations;
        self
    }

    pub(crate) fn with_user_fea(&mut self, user_fea: &str) -> &mut Self {
        self.user_fea = user_fea.into();
        self
    }

    pub(crate) fn with_glyph_order(&mut self, glyph_order: GlyphOrder) -> &mut Self {
        self.glyph_order = glyph_order;
        self
    }

    pub(crate) fn build(&self) -> LayoutOutput {
        let static_metadata = StaticMetadata::new(
            1000,
            Default::default(),
            self.axes.clone().into_inner(),
            self.named_instances.clone(),
            self.glyph_locations.clone(),
            Default::default(),
            42.0,
            self.categories.clone().unwrap_or_default(),
            None,
            false,
        )
        .unwrap();

        let glyph_map = self.glyph_order.names().cloned().collect();
        // first get the AST, which we need to use as input
        let fea = self.user_fea.clone();
        let (ast, _) = fea_rs::parse::parse_root(
            "memory".into(),
            Some(&glyph_map),
            Box::new(move |x: &Path| {
                if x == Path::new("memory") {
                    Ok(fea.clone())
                } else {
                    unreachable!("our FEA has no include statements");
                }
            }),
        )
        .unwrap();

        let first_pass_fea = FeaFirstPassOutput::for_test(ast, &glyph_map).unwrap();
        LayoutOutput {
            static_metadata,
            glyph_order: self.glyph_order.clone(),
            first_pass_fea,
        }
    }
}

impl LayoutOutput {
    pub(crate) fn compile(&self, feature_writer: &impl FeatureProvider) -> Compilation {
        let var_info = FeaVariationInfo::new(&self.static_metadata);
        let glyph_map = self.glyph_order.names().cloned().collect();
        fea_rs::compile::compile(
            &self.first_pass_fea.ast,
            &glyph_map,
            Some(&var_info),
            Some(feature_writer),
            Default::default(),
        )
        .unwrap()
        .0
    }
}

impl Default for LayoutOutputBuilder {
    fn default() -> Self {
        Self {
            axes: Default::default(),
            categories: None,
            named_instances: Default::default(),
            glyph_locations: Default::default(),
            user_fea: "languagesystem DFLT dflt;".into(),
            glyph_order: Default::default(),
        }
    }
}
