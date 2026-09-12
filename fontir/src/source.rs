//! Generic model of font sources.

use std::{
    collections::{BTreeMap, BTreeSet, HashMap, HashSet},
    fs,
    path::{Path, PathBuf},
};

use fontdrasil::orchestration::{Access, AccessBuilder, Work};
use fontdrasil::{
    coords::{CoordConverter, NormalizedLocation, UserCoord, UserLocation},
    types::{Axes, Axis, GlyphName},
};
use indexmap::IndexMap;
use kurbo::{BezPath, Point, Shape};
use ordered_float::OrderedFloat;
use serde::Deserialize;
use tiny_skia_path::PathSegment;
use usvg::Tree;
use write_fonts::types::{NameId, Tag};

use crate::{
    error::{BadSource, Error},
    ir::{
        Color, ColorGlyphs, ColorPalettes, ColorStop, FeatureSources, FeaturesSource,
        GlobalMetricsBuilder, GlyphBuilder, GlyphInstance, KerningInstance, KerningLocations,
        NameKey, NamedInstance, Paint, PaintGlyph, PaintLinearGradient, PaintRadialGradient,
        PaintSolid, PreliminaryGdefCategories, StaticMetadata,
    },
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
    #[serde(skip)]
    pub source_dir: PathBuf,
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
/// constructed.
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
        Ok(Box::new(EmojiGlobalMetricsWork {
            config: self.config.clone(),
        }))
    }

    fn create_glyph_ir_work(&self) -> Result<Vec<Box<IrWork>>, Error> {
        let mut glyphs = BTreeMap::<GlyphName, Vec<EmojiGlyphSource>>::new();
        for (master_name, master) in &self.config.master {
            for source in &master.srcs {
                let path = self.config.source_dir.join(source);
                let Some(stem) = path.file_stem().and_then(|stem| stem.to_str()) else {
                    return Err(Error::InvalidEntry("SVG filename", source.clone()));
                };
                // TODO: Support multi-codepoint emoji sequences as ligatures/components.
                if is_multi_codepoint_emoji(stem) {
                    continue;
                }
                let name = stem.into();
                let codepoints = codepoints_from_glyph_name(stem)?;
                glyphs.entry(name).or_default().push(EmojiGlyphSource {
                    master_name: master_name.clone(),
                    path,
                    codepoints,
                });
            }
        }

        let mut preliminary_names = Vec::new();
        let mut works = glyphs
            .into_iter()
            .map(|(name, sources)| {
                let glyph_name: GlyphName = name;
                let layer_count = sources
                    .iter()
                    .map(|source| parse_svg_paths(&source.path, 1024).map(|paths| paths.len()))
                    .collect::<Result<Vec<_>, Error>>()?
                    .into_iter()
                    .max()
                    .unwrap_or_default();
                preliminary_names.push(glyph_name.clone());
                let painted_indices = sources
                    .iter()
                    .map(|source| {
                        parse_svg_paths(&source.path, 1024).map(|paths| {
                            paths
                                .into_iter()
                                .enumerate()
                                .filter_map(|(index, path)| path.paint.map(|_| index))
                                .collect::<BTreeSet<_>>()
                        })
                    })
                    .collect::<Result<Vec<_>, Error>>()?;
                let layer_indices = painted_indices
                    .into_iter()
                    .flatten()
                    .collect::<BTreeSet<_>>()
                    .into_iter()
                    .collect::<Vec<_>>();
                preliminary_names.extend(
                    layer_indices
                        .iter()
                        .copied()
                        .map(|index| emoji_layer_name(&glyph_name, index)),
                );
                let codepoints = sources
                    .iter()
                    .flat_map(|source| source.codepoints.iter().copied())
                    .collect();
                Ok(Box::new(EmojiGlyphWork {
                    glyph_name,
                    codepoints,
                    sources,
                    master_positions: self
                        .config
                        .master
                        .iter()
                        .map(|(name, master)| (name.clone(), master.position.clone()))
                        .collect(),
                    layer_count,
                    layer_indices,
                }) as Box<IrWork>)
            })
            .collect::<Result<Vec<_>, Error>>()?;
        works.push(Box::new(EmojiGdefWork) as Box<IrWork>);
        works.push(Box::new(EmojiPreliminaryGlyphOrderWork {
            names: preliminary_names,
        }) as Box<IrWork>);
        Ok(works)
    }

    fn create_feature_ir_work(&self) -> Result<Box<IrWork>, Error> {
        Ok(Box::new(EmojiFeatureWork))
    }

    fn create_kerning_locations_ir_work(&self) -> Result<Box<IrWork>, Error> {
        Ok(Box::new(EmojiKerningLocationsWork))
    }

    fn create_kerning_instance_ir_work(
        &self,
        at: NormalizedLocation,
    ) -> Result<Box<IrWork>, Error> {
        Ok(Box::new(EmojiKerningInstanceWork { location: at }))
    }

    fn create_color_palette_work(&self) -> Result<Box<IrWork>, Error> {
        Ok(Box::new(EmojiColorPaletteWork {
            config: self.config.clone(),
        }))
    }

    fn create_color_glyphs_work(&self) -> Result<Box<IrWork>, Error> {
        Ok(Box::new(EmojiColorGlyphWork {
            config: self.config.clone(),
        }))
    }
}

#[derive(Debug)]
struct EmojiWork {
    config: EmojiConfig,
}

#[derive(Debug)]
struct EmojiGlobalMetricsWork {
    config: EmojiConfig,
}

#[derive(Debug)]
struct EmojiFeatureWork;

#[derive(Debug)]
struct EmojiKerningLocationsWork;

#[derive(Debug)]
struct EmojiKerningInstanceWork {
    location: NormalizedLocation,
}

#[derive(Debug)]
struct EmojiGdefWork;

#[derive(Debug)]
struct EmojiPreliminaryGlyphOrderWork {
    names: Vec<GlyphName>,
}

#[derive(Debug)]
struct EmojiGlyphSource {
    master_name: String,
    path: PathBuf,
    codepoints: HashSet<u32>,
}

#[derive(Debug)]
struct EmojiGlyphWork {
    glyph_name: GlyphName,
    codepoints: HashSet<u32>,
    sources: Vec<EmojiGlyphSource>,
    master_positions: HashMap<String, HashMap<String, f64>>,
    layer_count: usize,
    layer_indices: Vec<usize>,
}

#[derive(Debug)]
struct EmojiColorPaletteWork {
    config: EmojiConfig,
}

#[derive(Debug)]
struct EmojiColorGlyphWork {
    config: EmojiConfig,
}

#[derive(Debug)]
struct SvgPath {
    instance: GlyphInstance,
    paint: Option<SvgPaint>,
}

#[derive(Clone, Debug, PartialEq)]
enum SvgPaint {
    Solid(Color),
    Linear(PaintLinearGradient),
    Radial(PaintRadialGradient),
}

impl SvgPaint {
    fn colors(&self) -> impl Iterator<Item = Color> + '_ {
        match self {
            Self::Solid(color) => {
                Box::new(std::iter::once(*color)) as Box<dyn Iterator<Item = Color>>
            }
            Self::Linear(gradient) => Box::new(gradient.color_line.iter().map(|stop| stop.color)),
            Self::Radial(gradient) => Box::new(gradient.color_line.iter().map(|stop| stop.color)),
        }
    }

    fn to_paint(&self) -> Paint {
        match self {
            Self::Solid(color) => Paint::Solid(Box::new(PaintSolid {
                color: Some(*color),
            })),
            Self::Linear(gradient) => Paint::LinearGradient(Box::new(gradient.clone())),
            Self::Radial(gradient) => Paint::RadialGradient(Box::new(gradient.clone())),
        }
    }
}

fn codepoints_from_glyph_name(name: &str) -> Result<HashSet<u32>, Error> {
    let Some(codepoints) = name.strip_prefix("emoji_u") else {
        return Ok(HashSet::new());
    };
    let codepoints: HashSet<_> = codepoints
        .split('_')
        .map(|codepoint| {
            u32::from_str_radix(codepoint, 16)
                .map_err(|_| Error::InvalidEntry("emoji codepoint", codepoint.to_owned()))
        })
        .collect::<Result<_, _>>()?;
    Ok(if codepoints.len() == 1 {
        codepoints
    } else {
        HashSet::new()
    })
}

fn is_multi_codepoint_emoji(name: &str) -> bool {
    name.strip_prefix("emoji_u")
        .is_some_and(|codepoints| codepoints.split('_').count() > 1)
}

fn master_location(
    position: &HashMap<String, f64>,
    axes: &Axes,
) -> Result<NormalizedLocation, Error> {
    let location: UserLocation = axes
        .iter()
        .map(|axis| {
            (
                axis.tag,
                UserCoord::new(
                    position
                        .get(&axis.tag.to_string())
                        .copied()
                        .unwrap_or_else(|| axis.default.to_f64()),
                ),
            )
        })
        .collect();
    Ok(location.to_normalized(axes)?)
}

#[cfg(test)]
fn parse_svg(path: &Path, units_per_em: u16) -> Result<GlyphInstance, Error> {
    let paths = parse_svg_paths(path, units_per_em)?;
    Ok(GlyphInstance {
        width: f64::from(units_per_em),
        height: Some(f64::from(units_per_em)),
        vertical_origin: Some(f64::from(units_per_em)),
        contours: paths
            .into_iter()
            .flat_map(|path| path.instance.contours)
            .collect(),
        components: Vec::new(),
    })
}

fn parse_svg_paths(path: &Path, units_per_em: u16) -> Result<Vec<SvgPath>, Error> {
    let data = fs::read(path).map_err(|source| Error::BadSource(BadSource::new(path, source)))?;
    let options = usvg::Options {
        resources_dir: path.parent().map(Path::to_owned),
        ..Default::default()
    };
    let tree = Tree::from_data(&data, &options).map_err(|source| {
        Error::BadSource(BadSource::custom(
            path,
            format!("Unable to parse SVG: {source}"),
        ))
    })?;
    let width = tree.size().width() as f64;
    let height = tree.size().height() as f64;
    if width <= 0.0 || height <= 0.0 {
        return Err(Error::InvalidEntry("SVG size", path.display().to_string()));
    }
    let scale_x = f64::from(units_per_em) / width;
    let scale_y = f64::from(units_per_em) / height;
    let mut paths = Vec::new();
    collect_svg_paths(
        tree.root(),
        &mut paths,
        scale_x,
        scale_y,
        height,
        path,
        units_per_em,
    )?;
    Ok(paths)
}

fn collect_svg_paths(
    group: &usvg::Group,
    paths: &mut Vec<SvgPath>,
    scale_x: f64,
    scale_y: f64,
    height: f64,
    svg_path: &Path,
    units_per_em: u16,
) -> Result<(), Error> {
    for node in group.children() {
        match node {
            usvg::Node::Group(group) => collect_svg_paths(
                group,
                paths,
                scale_x,
                scale_y,
                height,
                svg_path,
                units_per_em,
            )?,
            usvg::Node::Path(path) => {
                let transform = path.abs_transform();
                let transform_point = |point: tiny_skia_path::Point| {
                    let x =
                        f64::from(transform.sx * point.x + transform.kx * point.y + transform.tx);
                    let y =
                        f64::from(transform.ky * point.x + transform.sy * point.y + transform.ty);
                    Point::new(x * scale_x, (height - y) * scale_y)
                };
                let mut bez_path = BezPath::new();
                for segment in path.data().segments() {
                    match segment {
                        PathSegment::MoveTo(point) => bez_path.move_to(transform_point(point)),
                        PathSegment::LineTo(point) => bez_path.line_to(transform_point(point)),
                        PathSegment::QuadTo(p0, p1) => {
                            bez_path.quad_to(transform_point(p0), transform_point(p1))
                        }
                        PathSegment::CubicTo(p0, p1, p2) => bez_path.curve_to(
                            transform_point(p0),
                            transform_point(p1),
                            transform_point(p2),
                        ),
                        PathSegment::Close => bez_path.close_path(),
                    }
                }
                if !bez_path.is_empty() {
                    let paint = path
                        .fill()
                        .map(|fill| {
                            convert_svg_paint(
                                fill,
                                bez_path.bounding_box(),
                                scale_x,
                                scale_y,
                                height,
                                svg_path,
                                units_per_em,
                            )
                        })
                        .transpose()?;
                    paths.push(SvgPath {
                        instance: GlyphInstance {
                            width: f64::from(units_per_em),
                            height: Some(f64::from(units_per_em)),
                            vertical_origin: Some(f64::from(units_per_em)),
                            contours: vec![bez_path],
                            components: Vec::new(),
                        },
                        paint,
                    });
                }
            }
            usvg::Node::Image(_) | usvg::Node::Text(_) => {}
        }
    }
    Ok(())
}

fn convert_svg_paint(
    fill: &usvg::Fill,
    bbox: kurbo::Rect,
    scale_x: f64,
    scale_y: f64,
    height: f64,
    svg_path: &Path,
    _units_per_em: u16,
) -> Result<SvgPaint, Error> {
    let opacity = fill.opacity().get();
    let space = GradientSpace {
        bbox,
        scale_x,
        scale_y,
        height,
    };
    match fill.paint() {
        usvg::Paint::Color(color) => Ok(SvgPaint::Solid(Color {
            r: color.red,
            g: color.green,
            b: color.blue,
            a: (opacity * 255.0).round() as u8,
        })),
        usvg::Paint::LinearGradient(gradient) => {
            if gradient.spread_method() != usvg::SpreadMethod::Pad {
                return Err(unsupported_svg_paint(svg_path, "non-pad gradient spread"));
            }
            let stops = gradient
                .stops()
                .iter()
                .map(|stop| ColorStop {
                    offset: OrderedFloat(stop.offset().get()),
                    color: Color {
                        r: stop.color().red,
                        g: stop.color().green,
                        b: stop.color().blue,
                        a: 255,
                    },
                    alpha: OrderedFloat(opacity * stop.opacity().get()),
                })
                .collect();
            let p0 = space.point(gradient.x1(), gradient.y1(), gradient.transform());
            let p1 = space.point(gradient.x2(), gradient.y2(), gradient.transform());
            Ok(SvgPaint::Linear(PaintLinearGradient {
                color_line: stops,
                p0,
                p1,
                p2: None,
            }))
        }
        usvg::Paint::RadialGradient(gradient) => {
            if gradient.spread_method() != usvg::SpreadMethod::Pad {
                return Err(unsupported_svg_paint(svg_path, "non-pad gradient spread"));
            }
            let stops = gradient
                .stops()
                .iter()
                .map(|stop| ColorStop {
                    offset: OrderedFloat(stop.offset().get()),
                    color: Color {
                        r: stop.color().red,
                        g: stop.color().green,
                        b: stop.color().blue,
                        a: 255,
                    },
                    alpha: OrderedFloat(opacity * stop.opacity().get()),
                })
                .collect();
            let p0 = space.point(gradient.fx(), gradient.fy(), gradient.transform());
            let p1 = space.point(gradient.cx(), gradient.cy(), gradient.transform());
            let r1 = space.radius(
                gradient.cx(),
                gradient.cy(),
                gradient.r().get(),
                gradient.transform(),
            );
            let r0 = space.radius(
                gradient.fx(),
                gradient.fy(),
                gradient.fr().get(),
                gradient.transform(),
            );
            Ok(SvgPaint::Radial(PaintRadialGradient {
                color_line: stops,
                p0,
                r0: Some(OrderedFloat(r0)),
                p1,
                r1: Some(OrderedFloat(r1)),
            }))
        }
        usvg::Paint::Pattern(_) => Err(unsupported_svg_paint(svg_path, "pattern fill")),
    }
}

fn unsupported_svg_paint(path: &Path, paint: &str) -> Error {
    Error::UnsupportedConstruct(format!("unsupported SVG {paint} in '{}'", path.display()))
}

fn transformed_svg_point(x: f32, y: f32, transform: tiny_skia_path::Transform) -> (f64, f64) {
    (
        f64::from(transform.sx * x + transform.kx * y + transform.tx),
        f64::from(transform.ky * x + transform.sy * y + transform.ty),
    )
}

struct GradientSpace {
    bbox: kurbo::Rect,
    scale_x: f64,
    scale_y: f64,
    height: f64,
}

impl GradientSpace {
    fn point(&self, x: f32, y: f32, transform: tiny_skia_path::Transform) -> Point {
        let (x, y) = transformed_svg_point(x, y, transform);
        let point = Point::new(x * self.scale_x, (self.height - y) * self.scale_y);
        Point::new(
            (point.x - self.bbox.x0) / self.bbox.width(),
            (point.y - self.bbox.y0) / self.bbox.height(),
        )
    }

    fn radius(&self, x: f32, y: f32, radius: f32, transform: tiny_skia_path::Transform) -> f32 {
        if radius == 0.0 {
            return 0.0;
        }
        let center = self.point(x, y, transform);
        let edge = self.point(x + radius, y, transform);
        center.distance(edge) as f32
    }
}

impl Work<Context, WorkId, Error> for EmojiGlyphWork {
    fn id(&self) -> WorkId {
        WorkId::Glyph(self.glyph_name.clone())
    }

    fn read_access(&self) -> Access<WorkId> {
        Access::Variant(WorkId::StaticMetadata)
    }

    fn write_access(&self) -> Access<WorkId> {
        let mut access = AccessBuilder::new()
            .specific_instance(WorkId::Glyph(self.glyph_name.clone()))
            .specific_instance(WorkId::Anchor(self.glyph_name.clone()));
        for index in self.layer_indices.iter().copied() {
            access =
                access.specific_instance(WorkId::Glyph(emoji_layer_name(&self.glyph_name, index)));
        }
        access.build()
    }

    fn also_completes(&self) -> Vec<WorkId> {
        let mut completed = vec![WorkId::Anchor(self.glyph_name.clone())];
        completed.extend(
            self.layer_indices
                .iter()
                .copied()
                .map(|index| WorkId::Glyph(emoji_layer_name(&self.glyph_name, index))),
        );
        completed
    }

    fn exec(&self, context: &Context) -> Result<(), Error> {
        let metadata = context.static_metadata.get();
        let mut instances = HashMap::new();
        let mut layer_instances = vec![HashMap::new(); self.layer_count];
        for source in &self.sources {
            let position = self
                .master_positions
                .get(&source.master_name)
                .ok_or_else(|| Error::UnknownEntry("master", source.master_name.clone()))?;
            let location = master_location(position, &metadata.all_source_axes)?;
            let paths = parse_svg_paths(&source.path, metadata.units_per_em)?;
            let instance = GlyphInstance {
                width: f64::from(metadata.units_per_em),
                height: Some(f64::from(metadata.units_per_em)),
                vertical_origin: Some(f64::from(metadata.units_per_em)),
                contours: paths
                    .iter()
                    .flat_map(|path| path.instance.contours.clone())
                    .collect(),
                components: Vec::new(),
            };
            instances.insert(location.clone(), instance);
            for (index, path) in paths.into_iter().enumerate() {
                if path.paint.is_some() {
                    layer_instances[index].insert(location.clone(), path.instance);
                }
            }
        }

        let glyph = GlyphBuilder {
            name: self.glyph_name.clone(),
            emit_to_binary: true,
            codepoints: self.codepoints.clone(),
            sources: instances,
        }
        .build()?;
        context.glyphs.set(glyph);
        for (index, sources) in layer_instances.into_iter().enumerate() {
            if sources.is_empty() {
                continue;
            }
            context.glyphs.set(
                GlyphBuilder {
                    name: emoji_layer_name(&self.glyph_name, index),
                    emit_to_binary: true,
                    codepoints: HashSet::new(),
                    sources,
                }
                .build()?,
            );
        }
        Ok(())
    }
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
            global_locations.insert(master_location(&master.position, &all_axes)?);
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

impl Work<Context, WorkId, Error> for EmojiGlobalMetricsWork {
    fn id(&self) -> WorkId {
        WorkId::GlobalMetrics
    }

    fn read_access(&self) -> Access<WorkId> {
        Access::Variant(WorkId::StaticMetadata)
    }

    fn exec(&self, context: &Context) -> Result<(), Error> {
        let metadata = context.static_metadata.get();
        let mut builder = GlobalMetricsBuilder::new();
        let mut locations = HashSet::new();
        for master in self.config.master.values() {
            locations.insert(master_location(
                &master.position,
                &metadata.all_source_axes,
            )?);
        }
        for location in locations {
            builder.populate_defaults(&location, metadata.units_per_em, None, None, None, None);
        }
        context
            .global_metrics
            .set(builder.build(&metadata.all_source_axes)?);
        Ok(())
    }
}

impl Work<Context, WorkId, Error> for EmojiFeatureWork {
    fn id(&self) -> WorkId {
        WorkId::Features
    }

    fn write_access(&self) -> Access<WorkId> {
        Access::Variant(WorkId::Features)
    }

    fn exec(&self, context: &Context) -> Result<(), Error> {
        context
            .features
            .set(FeatureSources::single(FeaturesSource::Empty));
        Ok(())
    }
}

impl Work<Context, WorkId, Error> for EmojiKerningLocationsWork {
    fn id(&self) -> WorkId {
        WorkId::KerningLocations
    }

    fn write_access(&self) -> Access<WorkId> {
        Access::Variant(WorkId::KerningLocations)
    }

    fn exec(&self, context: &Context) -> Result<(), Error> {
        context.kerning_locations.set(KerningLocations::default());
        Ok(())
    }
}

impl Work<Context, WorkId, Error> for EmojiKerningInstanceWork {
    fn id(&self) -> WorkId {
        WorkId::KernInstance(self.location.clone())
    }

    fn read_access(&self) -> Access<WorkId> {
        Access::Variant(WorkId::KerningLocations)
    }

    fn write_access(&self) -> Access<WorkId> {
        Access::SpecificInstanceOfVariant(WorkId::KernInstance(self.location.clone()))
    }

    fn exec(&self, context: &Context) -> Result<(), Error> {
        context.kerning_at.set(KerningInstance {
            location: self.location.clone(),
            ..Default::default()
        });
        Ok(())
    }
}

impl Work<Context, WorkId, Error> for EmojiGdefWork {
    fn id(&self) -> WorkId {
        WorkId::PreliminaryGdefCategories
    }

    fn write_access(&self) -> Access<WorkId> {
        Access::Variant(WorkId::PreliminaryGdefCategories)
    }

    fn exec(&self, context: &Context) -> Result<(), Error> {
        context
            .preliminary_gdef_categories
            .set(PreliminaryGdefCategories::default());
        Ok(())
    }
}

impl Work<Context, WorkId, Error> for EmojiPreliminaryGlyphOrderWork {
    fn id(&self) -> WorkId {
        WorkId::PreliminaryGlyphOrder
    }

    fn read_access(&self) -> Access<WorkId> {
        AccessBuilder::new()
            .variant(WorkId::StaticMetadata)
            .variant(WorkId::ALL_GLYPHS)
            .build()
    }

    fn write_access(&self) -> Access<WorkId> {
        Access::Variant(WorkId::PreliminaryGlyphOrder)
    }

    fn exec(&self, context: &Context) -> Result<(), Error> {
        context
            .preliminary_glyph_order
            .set(self.names.iter().cloned().collect());
        Ok(())
    }
}

impl Work<Context, WorkId, Error> for EmojiColorPaletteWork {
    fn id(&self) -> WorkId {
        WorkId::ColorPalettes
    }

    fn read_access(&self) -> Access<WorkId> {
        Access::None
    }

    fn write_access(&self) -> Access<WorkId> {
        Access::Variant(WorkId::ColorPalettes)
    }

    fn exec(&self, context: &Context) -> Result<(), Error> {
        let mut colors = Vec::new();
        let mut sources = Vec::new();
        for master in self.config.master.values() {
            sources.extend(
                master
                    .srcs
                    .iter()
                    .map(|source| self.config.source_dir.join(source)),
            );
        }
        sources.sort();
        sources.dedup();

        for source in sources {
            for path in parse_svg_paths(&source, 1024)? {
                if let Some(paint) = path.paint {
                    for color in paint.colors() {
                        if !colors.contains(&color) {
                            colors.push(color);
                        }
                    }
                }
            }
        }

        if let Some(palettes) = ColorPalettes::new(vec![colors])? {
            context.colors.set(palettes);
        }
        Ok(())
    }
}

impl Work<Context, WorkId, Error> for EmojiColorGlyphWork {
    fn id(&self) -> WorkId {
        WorkId::PaintGraph
    }

    fn read_access(&self) -> Access<WorkId> {
        AccessBuilder::new()
            .variant(WorkId::StaticMetadata)
            .variant(WorkId::ColorPalettes)
            .variant(WorkId::ALL_GLYPHS)
            .build()
    }

    fn write_access(&self) -> Access<WorkId> {
        Access::Variant(WorkId::PaintGraph)
    }

    fn exec(&self, context: &Context) -> Result<(), Error> {
        let metadata = context.static_metadata.get();
        let Some(palettes) = context.colors.try_get() else {
            return Ok(());
        };
        let palette = palettes.palettes.first().cloned().unwrap_or_default();
        let mut sources = BTreeMap::<GlyphName, Vec<(String, PathBuf)>>::new();
        for (master_name, master) in &self.config.master {
            for source in &master.srcs {
                let path = self.config.source_dir.join(source);
                let Some(stem) = path.file_stem().and_then(|stem| stem.to_str()) else {
                    return Err(Error::InvalidEntry("SVG filename", source.clone()));
                };
                // TODO: Support multi-codepoint emoji sequences as ligatures/components.
                if is_multi_codepoint_emoji(stem) {
                    continue;
                }
                sources
                    .entry(stem.into())
                    .or_default()
                    .push((master_name.clone(), path));
            }
        }

        let mut base_glyphs = IndexMap::new();
        for (base_name, glyph_sources) in sources {
            let mut layer_paints = BTreeMap::<usize, SvgPaint>::new();

            for (_master_name, path) in glyph_sources {
                for (index, svg_path) in parse_svg_paths(&path, metadata.units_per_em)?
                    .into_iter()
                    .enumerate()
                {
                    let Some(paint) = svg_path.paint else {
                        continue;
                    };
                    for color in paint.colors() {
                        if !palette.contains(&color) {
                            return Err(Error::InvalidEntry(
                                "SVG color",
                                format!("{color:?} is missing from the color palette"),
                            ));
                        }
                    }
                    if let Some(previous) = layer_paints.insert(index, paint.clone())
                        && previous != paint
                    {
                        return Err(Error::UnsupportedConstruct(format!(
                            "paint for layer {index} of '{base_name}' varies between masters"
                        )));
                    }
                }
            }

            let mut paints = Vec::new();
            for index in layer_paints.keys().copied() {
                let layer_name = emoji_layer_name(&base_name, index);
                paints.push(Paint::Glyph(Box::new(PaintGlyph {
                    name: layer_name,
                    paint: layer_paints[&index].to_paint(),
                })));
            }

            if !paints.is_empty() {
                let paint = if paints.len() == 1 {
                    paints.pop().unwrap()
                } else {
                    Paint::Layers(paints.into())
                };
                base_glyphs.insert(base_name, paint);
            }
        }

        if !base_glyphs.is_empty() {
            context.paint_graph.set(ColorGlyphs { base_glyphs });
        }
        Ok(())
    }
}

fn emoji_layer_name(base_name: &GlyphName, index: usize) -> GlyphName {
    format!("{}.color{}", base_name.as_str(), index).into()
}

#[cfg(test)]
mod tests {
    use super::*;
    use fontdrasil::orchestration::Work;
    use kurbo::Shape;

    fn config() -> EmojiConfig {
        EmojiConfig {
            family: "Test Color".to_string(),
            output_file: "test.ttf".to_string(),
            color_format: "glyf_colr_1".to_string(),
            clipbox_quantization: 32,
            source_dir: PathBuf::new(),
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

    #[test]
    fn parses_svg_paths_into_font_coordinates() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("emoji_u1f600.svg");
        std::fs::write(
            &path,
            r#"<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 128 128">
                <rect x="0" y="0" width="64" height="64"/>
            </svg>"#,
        )
        .unwrap();

        let instance = parse_svg(&path, 1024).unwrap();
        assert_eq!(instance.width, 1024.0);
        assert_eq!(instance.contours.len(), 1);
        assert_eq!(
            instance.contours[0].bounding_box(),
            kurbo::Rect::new(0.0, 512.0, 512.0, 1024.0)
        );
        assert_eq!(
            codepoints_from_glyph_name("emoji_u1f600").unwrap(),
            HashSet::from([0x1f600])
        );
        assert!(
            codepoints_from_glyph_name("emoji_u1f469_200d_1f4bb")
                .unwrap()
                .is_empty()
        );
        assert!(is_multi_codepoint_emoji("emoji_u1f469_200d_1f4bb"));
    }

    #[test]
    fn builds_color_palette_and_paint_graph() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("emoji_u1f600.svg");
        std::fs::write(
            &path,
            r##"<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 128 128">
                <path fill="#ff0000" d="M0 0h64v64H0z"/>
                <path fill="#0000ff" d="M64 64h64v64H64z"/>
            </svg>"##,
        )
        .unwrap();

        let mut config = config();
        config.source_dir = dir.path().to_owned();
        config.master.get_mut("regular").unwrap().srcs = vec!["emoji_u1f600.svg".to_owned()];
        config.master.remove("bold");

        let root = Context::new_root(Flags::empty());
        let static_work = EmojiWork {
            config: config.clone(),
        };
        let static_context =
            root.copy_for_work(static_work.read_access(), static_work.write_access());
        static_work.exec(&static_context).unwrap();

        for glyph_work in EmojiSource::from_config(config.clone())
            .create_glyph_ir_work()
            .unwrap()
        {
            let glyph_context =
                root.copy_for_work(glyph_work.read_access(), glyph_work.write_access());
            glyph_work.exec(&glyph_context).unwrap();
        }

        let palette_work = EmojiColorPaletteWork {
            config: config.clone(),
        };
        let palette_context =
            root.copy_for_work(palette_work.read_access(), palette_work.write_access());
        palette_work.exec(&palette_context).unwrap();
        assert_eq!(root.colors.get().palettes[0].len(), 2);

        let color_work = EmojiColorGlyphWork { config };
        let color_context = root.copy_for_work(color_work.read_access(), color_work.write_access());
        color_work.exec(&color_context).unwrap();

        let paints = &root.paint_graph.get().base_glyphs[&GlyphName::from("emoji_u1f600")];
        assert!(matches!(paints, Paint::Layers(layers) if layers.len() == 2));
        assert!(
            root.glyphs
                .try_get(&WorkId::Glyph("emoji_u1f600.color0".into()))
                .is_some()
        );
        assert!(
            root.glyphs
                .try_get(&WorkId::Glyph("emoji_u1f600.color1".into()))
                .is_some()
        );
    }

    #[test]
    fn parses_linear_and_radial_gradients() {
        let dir = tempfile::tempdir().unwrap();
        let path = dir.path().join("emoji_u1f600.svg");
        std::fs::write(
            &path,
            r##"<svg xmlns="http://www.w3.org/2000/svg" viewBox="0 0 128 128">
                <defs>
                    <linearGradient id="linear" gradientUnits="userSpaceOnUse" x1="0" y1="0" x2="128" y2="128">
                        <stop offset="0" stop-color="#ff0000"/>
                        <stop offset="1" stop-color="#00ff00" stop-opacity="0.5"/>
                    </linearGradient>
                    <radialGradient id="radial" gradientUnits="userSpaceOnUse" cx="96" cy="96" r="64" fx="80" fy="80">
                        <stop offset="0" stop-color="#0000ff"/>
                        <stop offset="1" stop-color="#ffffff"/>
                    </radialGradient>
                </defs>
                <path fill="url(#linear)" d="M0 0h64v64H0z"/>
                <path fill="url(#radial)" d="M64 64h64v64H64z"/>
            </svg>"##,
        )
        .unwrap();

        let paths = parse_svg_paths(&path, 1024).unwrap();
        assert_eq!(paths.len(), 2);
        let SvgPaint::Linear(linear) = &paths[0].paint.as_ref().unwrap() else {
            panic!("expected linear gradient");
        };
        assert_eq!(linear.color_line.len(), 2);
        assert_eq!(linear.color_line[1].alpha, OrderedFloat(0.5));
        assert_eq!(linear.p0, Point::new(0.0, 1.0));
        assert_eq!(linear.p1, Point::new(2.0, -1.0));

        let SvgPaint::Radial(radial) = &paths[1].paint.as_ref().unwrap() else {
            panic!("expected radial gradient");
        };
        assert_eq!(radial.color_line.len(), 2);
        assert_eq!(radial.p1, Point::new(0.5, 0.5));
        assert_eq!(radial.r1, Some(OrderedFloat(1.0)));
        assert_eq!(radial.r0, Some(OrderedFloat(0.0)));
    }

    #[test]
    #[ignore = "requires the sibling noto-emoji checkout and is intentionally expensive"]
    fn builds_noto_emoji_color_ir() {
        let config_path = Path::new("/home/wmedrano/src/noto-emoji/colrv1/all.toml");
        let contents = std::fs::read_to_string(config_path).unwrap();
        let mut config: EmojiConfig = toml::from_str(&contents).unwrap();
        config.source_dir = config_path.parent().unwrap().to_owned();

        let root = Context::new_root(Flags::empty());
        let static_work = EmojiWork {
            config: config.clone(),
        };
        let static_context =
            root.copy_for_work(static_work.read_access(), static_work.write_access());
        static_work.exec(&static_context).unwrap();

        let palette_work = EmojiColorPaletteWork {
            config: config.clone(),
        };
        let palette_context =
            root.copy_for_work(palette_work.read_access(), palette_work.write_access());
        palette_work.exec(&palette_context).unwrap();

        let color_work = EmojiColorGlyphWork { config };
        let color_context = root.copy_for_work(color_work.read_access(), color_work.write_access());
        color_work.exec(&color_context).unwrap();

        assert!(!root.colors.get().palettes[0].is_empty());
        assert!(!root.paint_graph.get().base_glyphs.is_empty());
    }
}
