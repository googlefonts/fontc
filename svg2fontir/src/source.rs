use std::collections::HashMap;
use std::path::{Path, PathBuf};
use std::sync::Arc;

use fontdrasil::{
    coords::NormalizedLocation,
    orchestration::{Access, Work},
    types::GlyphName,
};
use fontir::{
    error::{BadSource, Error},
    ir::{
        self, GlobalMetricsBuilder, GlyphOrder, NameKey, PreliminaryGdefCategories, StaticMetadata,
    },
    orchestration::{Context, IrWork, WorkId},
    source::Source,
};
use kurbo::{Affine, BezPath};
use log::warn;

use crate::{color, parse, toir, transform};

/// Source implementation for a directory of SVG files.
///
/// Each SVG file is named `u{HEX}.svg` (e.g., `u0041.svg`, `u1F600.svg`).
/// The SVGs are parsed and converted to IR for COLR/CPAL color font compilation.
#[derive(Debug)]
#[allow(dead_code)]
pub struct SvgIrSource {
    svg_dir: PathBuf,
    /// Parsed SVG trees indexed by glyph name.
    svgs: Arc<HashMap<GlyphName, usvg::Tree>>,
    /// Outline glyph paths (layer glyphs for COLR).
    outline_glyphs: Arc<HashMap<GlyphName, BezPath>>,
    /// Codepoints indexed by glyph name (parsed from filename).
    glyph_codepoints: HashMap<GlyphName, u32>,
    /// Font metadata from CLI flags.
    upem: u16,
    family_name: String,
    ascender: i16,
    descender: i16,
    /// The viewBox→font transform (computed once).
    font_transform: Affine,
}

impl SvgIrSource {
    /// Create a new SVG source from a directory of SVG files.
    ///
    /// `dir` must contain SVG files named `u{HEX}.svg`.
    /// `family_name` is used for the font family name.
    /// `upem` is the units per em.
    /// `ascender` and `descender` are font-wide vertical metrics.
    pub fn new(
        dir: &Path,
        family_name: String,
        upem: u16,
        ascender: i16,
        descender: i16,
    ) -> Result<Self, Error> {
        // 1. Discover SVGs
        let svg_paths = parse::discover_svgs(dir)
            .map_err(|e| BadSource::custom(dir, format!("Failed to discover SVGs: {e}")))?;

        if svg_paths.is_empty() {
            return Err(BadSource::custom(dir, "No SVG files found").into());
        }

        // Collect codepoints before consuming svg_paths
        let glyph_codepoints: HashMap<GlyphName, u32> = svg_paths
            .iter()
            .map(|(name, (_, codepoint))| (name.clone(), *codepoint))
            .collect();

        // 2. Parse all SVGs and collect data for viewBox extraction
        let mut svgs: HashMap<GlyphName, usvg::Tree> = HashMap::new();
        let mut svg_data: HashMap<GlyphName, Vec<u8>> = HashMap::new();
        for (name, (path, _)) in &svg_paths {
            let (tree, data) = parse::parse_svg(path)
                .map_err(|e| BadSource::custom(path, format!("Failed to parse SVG: {e}")))?;
            svgs.insert(name.clone(), tree);
            svg_data.insert(name.clone(), data);
        }

        // 3. Determine viewBox from first SVG (all should be consistent)
        let first_name = svgs.keys().next().unwrap();
        let first_tree = svgs.get(first_name).unwrap();
        let first_data = svg_data.get(first_name).unwrap();
        let view_box = parse::extract_viewbox(first_tree, first_data);
        // Warn if other SVGs have different viewBox (optional)
        for (name, tree) in &svgs {
            if name == first_name {
                continue;
            }
            let data = svg_data.get(name).unwrap();
            let other_view_box = parse::extract_viewbox(tree, data);
            if view_box != other_view_box {
                warn!(
                    "SVG {} has different viewBox than first SVG; using first viewBox",
                    name
                );
            }
        }

        // 4. Compute font transform
        let font_transform = transform::viewbox_to_font_transform(view_box, upem);

        // 5. Build paint graph (which also extracts outline paths)
        let result = toir::svg_to_paint_graph(&svgs, font_transform);

        Ok(SvgIrSource {
            svg_dir: dir.to_path_buf(),
            svgs: Arc::new(svgs),
            outline_glyphs: Arc::new(result.outline_glyphs),
            glyph_codepoints,
            upem,
            family_name,
            ascender,
            descender,
            font_transform,
        })
    }
}

impl Source for SvgIrSource {
    fn new(root: &Path) -> Result<Self, Error> {
        // This is called by fontc's Input::create_source().
        // Since SVG needs CLI metadata, we use defaults here.
        // The actual values are set via Options passthrough (see sub-plan 9).
        Self::new(root, "SVG Font".to_string(), 1000, 800, -200)
    }

    fn create_static_metadata_work(&self) -> Result<Box<IrWork>, Error> {
        let mut glyph_names = self.svgs.keys().cloned().collect::<Vec<_>>();
        glyph_names.extend(self.outline_glyphs.keys().cloned());
        glyph_names.sort();
        glyph_names.dedup();
        Ok(Box::new(StaticMetadataWork {
            upem: self.upem,
            family_name: self.family_name.clone(),
            glyph_names,
        }))
    }

    fn create_global_metric_work(&self) -> Result<Box<IrWork>, Error> {
        Ok(Box::new(GlobalMetricsWork {
            ascender: self.ascender,
            descender: self.descender,
            upem: self.upem,
        }))
    }

    fn create_glyph_ir_work(&self) -> Result<Vec<Box<IrWork>>, Error> {
        // Collect all glyph names: SVG base glyphs + outline layer glyphs.
        // SVG base glyphs that have no path at index 0 (e.g. fill:none first path)
        // are not in outline_glyphs, but still need a Glyph IR entry (empty outline)
        // so GlyphOrderWork can find them.
        let mut all_names: std::collections::HashSet<GlyphName> =
            self.outline_glyphs.keys().cloned().collect();
        all_names.extend(self.svgs.keys().cloned());

        let works: Vec<Box<IrWork>> = all_names
            .into_iter()
            .map(|name| {
                let path = self
                    .outline_glyphs
                    .get(&name)
                    .cloned()
                    .unwrap_or_default();
                Box::new(GlyphIrWork {
                    codepoint: self.glyph_codepoints.get(&name).copied(),
                    upem: self.upem,
                    path,
                    name,
                }) as Box<IrWork>
            })
            .collect();
        Ok(works)
    }

    fn create_color_palette_work(&self) -> Result<Box<IrWork>, Error> {
        Ok(Box::new(ColorPaletteWork {
            svgs: self.svgs.clone(),
        }))
    }

    fn create_color_glyphs_work(&self) -> Result<Box<IrWork>, Error> {
        Ok(Box::new(ColorGlyphsWork {
            svgs: self.svgs.clone(),
            font_transform: self.font_transform,
        }))
    }

    fn create_feature_ir_work(&self) -> Result<Box<IrWork>, Error> {
        Ok(Box::new(EmptyFeaturesWork))
    }

    fn create_kerning_group_ir_work(&self) -> Result<Box<IrWork>, Error> {
        Ok(Box::new(EmptyKerningGroupsWork))
    }

    fn create_kerning_instance_ir_work(
        &self,
        _at: NormalizedLocation,
    ) -> Result<Box<IrWork>, Error> {
        Ok(Box::new(EmptyKerningInstanceWork))
    }
}

// Work structs

#[derive(Debug)]
struct StaticMetadataWork {
    upem: u16,
    family_name: String,
    glyph_names: Vec<GlyphName>,
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

    fn exec(&self, context: &Context) -> Result<(), Error> {
        let mut names = HashMap::new();
        names.insert(
            NameKey::new(1.into(), &self.family_name),
            self.family_name.clone(),
        );
        // No axes for static SVG font
        let axes = Vec::new();
        let named_instances = Vec::new();
        let global_locations = std::iter::once(NormalizedLocation::new()).collect();
        let postscript_names = None;
        let italic_angle = 0.0;
        let number_values = None;
        let build_vertical = false;

        let static_metadata = StaticMetadata::new(
            self.upem,
            names,
            axes,
            named_instances,
            global_locations,
            postscript_names,
            italic_angle,
            number_values,
            build_vertical,
        )
        .map_err(Error::VariationModelError)?;

        context.static_metadata.set(static_metadata);

        // Set preliminary glyph order
        let mut glyph_order = GlyphOrder::new();
        let mut glyph_names = self.glyph_names.clone();
        glyph_names.sort();
        glyph_order.extend(glyph_names);
        context.preliminary_glyph_order.set(glyph_order);

        // No source-level glyph categories, use default
        context
            .preliminary_gdef_categories
            .set(PreliminaryGdefCategories::default());

        Ok(())
    }
}

#[derive(Debug)]
struct GlobalMetricsWork {
    ascender: i16,
    descender: i16,
    upem: u16,
}

impl Work<Context, WorkId, Error> for GlobalMetricsWork {
    fn id(&self) -> WorkId {
        WorkId::GlobalMetrics
    }

    fn read_access(&self) -> Access<WorkId> {
        Access::Variant(WorkId::StaticMetadata)
    }

    fn exec(&self, context: &Context) -> Result<(), Error> {
        let static_metadata = context.static_metadata.get();
        let mut metrics_builder = GlobalMetricsBuilder::new();
        let default_location = NormalizedLocation::new();
        metrics_builder.populate_defaults(
            &default_location,
            self.upem,
            None, // x_height
            Some(self.ascender as f64),
            Some(self.descender as f64),
            Some(0.0), // italic_angle
        );
        let metrics = metrics_builder.build(&static_metadata.axes)?;
        context.global_metrics.set(metrics);
        Ok(())
    }
}

#[derive(Debug)]
struct GlyphIrWork {
    name: GlyphName,
    path: BezPath,
    codepoint: Option<u32>,
    upem: u16,
}

impl Work<Context, WorkId, Error> for GlyphIrWork {
    fn id(&self) -> WorkId {
        WorkId::Glyph(self.name.clone())
    }

    fn exec(&self, context: &Context) -> Result<(), Error> {
        let default_location = NormalizedLocation::new();
        let instance = ir::GlyphInstance {
            width: self.upem as f64,
            height: None,
            vertical_origin: None,
            contours: vec![self.path.clone()],
            components: vec![],
        };
        let codepoints = self
            .codepoint
            .map(|cp| std::collections::HashSet::from([cp]))
            .unwrap_or_default();
        let glyph = ir::Glyph::new(
            self.name.clone(),
            true,
            codepoints,
            std::collections::HashMap::from([(default_location, instance)]),
        )
        .map_err(Error::BadGlyph)?;
        context.glyphs.set(glyph);
        Ok(())
    }
}

#[derive(Debug)]
struct ColorPaletteWork {
    svgs: Arc<HashMap<GlyphName, usvg::Tree>>,
}

impl Work<Context, WorkId, Error> for ColorPaletteWork {
    fn id(&self) -> WorkId {
        WorkId::ColorPalettes
    }

    fn exec(&self, context: &Context) -> Result<(), Error> {
        if let Some(palettes) = color::collect_colors_from_svgs(&self.svgs) {
            context.colors.set(palettes);
        }
        // If no colors, leave context.colors unset? The default is empty.
        Ok(())
    }
}

#[derive(Debug)]
struct ColorGlyphsWork {
    svgs: Arc<HashMap<GlyphName, usvg::Tree>>,
    font_transform: Affine,
}

impl Work<Context, WorkId, Error> for ColorGlyphsWork {
    fn id(&self) -> WorkId {
        WorkId::PaintGraph
    }

    fn exec(&self, context: &Context) -> Result<(), Error> {
        let result = toir::svg_to_paint_graph(&self.svgs, self.font_transform);
        context.paint_graph.set(result.paint_graph);
        // Note: outline_glyphs are already stored in the source, but they need to be
        // integrated with the glyph IR works. That's handled by create_glyph_ir_work.
        Ok(())
    }
}

#[derive(Debug)]
struct EmptyFeaturesWork;

impl Work<Context, WorkId, Error> for EmptyFeaturesWork {
    fn id(&self) -> WorkId {
        WorkId::Features
    }

    fn exec(&self, context: &Context) -> Result<(), Error> {
        // No features for SVG fonts
        context.features.set(ir::FeaturesSource::empty());
        Ok(())
    }
}

#[derive(Debug)]
struct EmptyKerningGroupsWork;

impl Work<Context, WorkId, Error> for EmptyKerningGroupsWork {
    fn id(&self) -> WorkId {
        WorkId::KerningGroups
    }

    fn exec(&self, context: &Context) -> Result<(), Error> {
        // No kerning for SVG fonts
        context.kerning_groups.set(ir::KerningGroups::default());
        Ok(())
    }
}

#[derive(Debug)]
struct EmptyKerningInstanceWork;

impl Work<Context, WorkId, Error> for EmptyKerningInstanceWork {
    fn id(&self) -> WorkId {
        // This is a dummy id; actual kerning instances are per location.
        // Since there are no axes, we just need a single location.
        WorkId::KernInstance(NormalizedLocation::new())
    }

    fn exec(&self, context: &Context) -> Result<(), Error> {
        // No kerning for SVG fonts
        let kerning = ir::KerningInstance {
            location: NormalizedLocation::new(),
            kerns: Default::default(),
        };
        context.kerning_at.set_unconditionally(kerning);
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::path::Path;

    fn testdata_dir() -> PathBuf {
        Path::new("../resources/testdata/svg").to_path_buf()
    }

    #[test]
    fn test_source_creation() {
        let source = SvgIrSource::new(
            &testdata_dir().join("simple_glyphs"),
            "Test".to_string(),
            1000,
            800,
            -200,
        );
        assert!(source.is_ok());
        let source = source.unwrap();
        assert_eq!(source.upem, 1000);
        assert_eq!(source.svgs.len(), 2);
    }

    #[test]
    fn test_static_metadata_work() {
        let source = SvgIrSource::new(
            &testdata_dir().join("simple_glyphs"),
            "Test".to_string(),
            1000,
            800,
            -200,
        )
        .unwrap();
        let work = source.create_static_metadata_work().unwrap();
        assert_eq!(work.id(), WorkId::StaticMetadata);
    }

    #[test]
    fn test_glyph_ir_work_count() {
        let source = SvgIrSource::new(
            &testdata_dir().join("multi_layer"),
            "Test".to_string(),
            1000,
            800,
            -200,
        )
        .unwrap();
        let works = source.create_glyph_ir_work().unwrap();
        // u1F600.svg has 4 paths → 4 layer glyphs
        assert_eq!(works.len(), 4);
    }

    #[test]
    fn test_color_palette_work() {
        let source = SvgIrSource::new(
            &testdata_dir().join("simple_glyphs"),
            "Test".to_string(),
            1000,
            800,
            -200,
        )
        .unwrap();
        let work = source.create_color_palette_work().unwrap();
        assert_eq!(work.id(), WorkId::ColorPalettes);
    }

    #[test]
    fn test_color_glyphs_work() {
        let source = SvgIrSource::new(
            &testdata_dir().join("simple_glyphs"),
            "Test".to_string(),
            1000,
            800,
            -200,
        )
        .unwrap();
        let work = source.create_color_glyphs_work().unwrap();
        assert_eq!(work.id(), WorkId::PaintGraph);
    }
}
