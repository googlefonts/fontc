//! The ufo2ft color lib interchange format: indexed palettes and static paint graphs.

use std::collections::{HashMap, HashSet};

use fontdrasil::types::GlyphName;
use fontir::{
    error::Error,
    ir::{
        Color, ColorGlyphs, ColorPalettes, ColorReference, ColorStop, GlyphOrder,
        GradientCoordinates, Paint, PaintGlyph, PaintLinearGradient, PaintRadialGradient,
        PaintSolid, PaintTransform, PaintTranslate,
    },
};
use indexmap::IndexMap;
use kurbo::{Affine, Point, Rect};
use plist::{Dictionary, Value};
use write_fonts::{OtRound, tables::colr::Extend};

pub(crate) const UFO2FT_COLOR_PALETTES: &str = "com.github.googlei18n.ufo2ft.colorPalettes";
pub(crate) const UFO2FT_COLOR_LAYERS: &str = "com.github.googlei18n.ufo2ft.colorLayers";
pub(crate) const UFO2FT_COLR_CLIP_BOXES: &str = "com.github.googlei18n.ufo2ft.colrClipBoxes";

fn invalid(message: impl Into<String>) -> Error {
    Error::InvalidEntry("color", message.into())
}

fn number(value: &Value) -> Option<f64> {
    value
        .as_real()
        .or_else(|| value.as_signed_integer().map(|value| value as f64))
        .filter(|value| value.is_finite())
}

pub(crate) fn parse_color_palettes(value: &Value) -> Result<Vec<Vec<Color>>, Error> {
    let palettes = value
        .as_array()
        .ok_or_else(|| invalid(format!("{UFO2FT_COLOR_PALETTES} must be an array")))?;
    if palettes.len() > u16::MAX as usize {
        return Err(invalid(format!(
            "{UFO2FT_COLOR_PALETTES}: too many palettes"
        )));
    }
    palettes
        .iter()
        .map(|palette| {
            let colors = palette
                .as_array()
                .ok_or_else(|| invalid(format!("{UFO2FT_COLOR_PALETTES}: each palette must be an array")))?;
            if colors.len() > u16::MAX as usize {
                return Err(invalid(format!("{UFO2FT_COLOR_PALETTES}: too many palette entries")));
            }
            colors
                .iter()
                .map(|color| {
                    let rgba = color.as_array().filter(|rgba| rgba.len() == 4).ok_or_else(|| {
                        invalid(format!("{UFO2FT_COLOR_PALETTES}: each color must contain 4 RGBA components"))
                    })?;
                    let mut components = [0; 4];
                    for (result, value) in components.iter_mut().zip(rgba) {
                        let value = number(value).filter(|value| (0.0..=1.0).contains(value)).ok_or_else(|| {
                            invalid(format!("{UFO2FT_COLOR_PALETTES}: color component must be a finite number in 0..=1"))
                        })?;
                        // Match Python's round(v * 255), as used by fontTools buildCPAL.
                        *result = (value * 255.0).round_ties_even() as u8;
                    }
                    let [r, g, b, a] = components;
                    Ok(Color { r, g, b, a })
                })
                .collect()
        })
        .collect()
}

/// Parse both the legacy array of (glyph, palette index) pairs and COLRv1 paints.
pub(crate) fn parse_color_layers(
    value: &Value,
    palettes: &ColorPalettes,
    glyph_order: &GlyphOrder,
    clip_boxes: Option<&Value>,
) -> Result<ColorGlyphs, Error> {
    let layers = value
        .as_dictionary()
        .ok_or_else(|| invalid(format!("{UFO2FT_COLOR_LAYERS} must be a dictionary")))?;
    let mut base_glyphs = IndexMap::with_capacity(layers.len());
    for (name, value) in layers {
        let parser = PaintParser {
            base: name,
            palettes,
            glyph_order,
        };
        parser.glyph(&Value::String(name.clone()))?;
        let paint = if let Some(layers) = value.as_array().filter(|layers| {
            layers.iter().all(|layer| {
                layer
                    .as_array()
                    .is_some_and(|pair| pair.first().is_some_and(|name| name.as_string().is_some()))
            })
        }) {
            let paints = layers
                .iter()
                .map(|layer| {
                    let pair = layer.as_array().unwrap();
                    if pair.len() != 2 {
                        return Err(parser.error("expected [glyphName, paletteIndex]"));
                    }
                    Ok(Paint::Glyph(Box::new(PaintGlyph {
                        name: parser.glyph(&pair[0])?,
                        paint: Paint::Solid(Box::new(PaintSolid {
                            color: parser.palette_index(&pair[1])?,
                            alpha: 1.0.into(),
                        })),
                    })))
                })
                .collect::<Result<Vec<_>, Error>>()?;
            parser.layers(paints)?
        } else {
            parser.paint(value, 0)?
        };
        base_glyphs.insert(name.as_str().into(), paint);
    }
    validate_references(&base_glyphs)?;
    let clip_boxes = parse_clip_boxes(clip_boxes, &base_glyphs)?;
    Ok(ColorGlyphs {
        base_glyphs,
        clip_boxes,
    })
}

struct PaintParser<'a> {
    base: &'a str,
    palettes: &'a ColorPalettes,
    glyph_order: &'a GlyphOrder,
}

impl PaintParser<'_> {
    fn error(&self, message: impl std::fmt::Display) -> Error {
        invalid(format!(
            "{UFO2FT_COLOR_LAYERS}: glyph '{}': {message}",
            self.base
        ))
    }

    fn required<'a>(&self, dict: &'a Dictionary, key: &str) -> Result<&'a Value, Error> {
        dict.get(key)
            .ok_or_else(|| self.error(format!("missing {key}")))
    }

    fn dictionary<'a>(&self, value: &'a Value, field: &str) -> Result<&'a Dictionary, Error> {
        value
            .as_dictionary()
            .ok_or_else(|| self.error(format!("{field} must be a dictionary")))
    }

    fn fields(&self, dict: &Dictionary, allowed: &[&str]) -> Result<(), Error> {
        for key in dict.keys() {
            if !allowed.contains(&key.as_str()) {
                return Err(self.error(format!("unsupported field '{key}'")));
            }
        }
        Ok(())
    }

    fn glyph(&self, value: &Value) -> Result<GlyphName, Error> {
        let name = value
            .as_string()
            .ok_or_else(|| self.error("glyph name must be a string"))?;
        if !self.glyph_order.contains(name) {
            return Err(self.error(format!("missing glyph '{name}'")));
        }
        Ok(name.into())
    }

    fn palette_index(&self, value: &Value) -> Result<ColorReference, Error> {
        let index = value
            .as_signed_integer()
            .ok_or_else(|| self.error("palette index must be an integer"))?;
        let index = u16::try_from(index)
            .map_err(|_| self.error(format!("palette index {index} out of u16 range")))?;
        if index != 0xffff
            && self
                .palettes
                .palettes
                .first()
                .is_none_or(|palette| index as usize >= palette.len())
        {
            return Err(self.error(format!("palette index {index} out of range")));
        }
        Ok(ColorReference::PaletteIndex(index))
    }

    fn numeric(&self, value: &Value, field: &str, min: f64, max: f64) -> Result<f64, Error> {
        number(value)
            .filter(|value| (min..=max).contains(value))
            .ok_or_else(|| self.error(format!("{field} must be a finite number in {min}..={max}")))
    }

    fn coordinate(&self, dict: &Dictionary, field: &str, unsigned: bool) -> Result<f64, Error> {
        let value = number(self.required(dict, field)?)
            .ok_or_else(|| self.error(format!("{field} must be a finite number")))?;
        let rounded: f64 = value.ot_round();
        let (min, max) = if unsigned {
            (0.0, 65535.0)
        } else {
            (-32768.0, 32767.0)
        };
        if !(min..=max).contains(&rounded) {
            return Err(self.error(format!("{field} out of range {min}..={max}")));
        }
        Ok(rounded)
    }

    fn fixed14(
        &self,
        value: &Value,
        field: &str,
        min: f64,
        max: f64,
    ) -> Result<ordered_float::OrderedFloat<f32>, Error> {
        let value = self.numeric(value, field, min, max)?;
        // Quantize before narrowing to f32: narrowing first can move a value
        // across an F2Dot14 rounding boundary.
        let rounded: f64 = (value * 16384.0).ot_round();
        Ok(((rounded / 16384.0) as f32).into())
    }

    fn alpha(&self, dict: &Dictionary) -> Result<ordered_float::OrderedFloat<f32>, Error> {
        dict.get("Alpha")
            .map(|value| self.fixed14(value, "Alpha", 0.0, 1.0))
            .unwrap_or_else(|| Ok(1.0.into()))
    }

    fn layers(&self, mut paints: Vec<Paint>) -> Result<Paint, Error> {
        match paints.len() {
            0 => Err(self.error("no color layers")),
            1 => Ok(paints.pop().unwrap()),
            _ => Ok(Paint::Layers(Box::new(paints))),
        }
    }

    fn color_line(&self, value: &Value) -> Result<(Vec<ColorStop>, Extend), Error> {
        let dict = self.dictionary(value, "ColorLine")?;
        self.fields(dict, &["Extend", "ColorStop"])?;
        let extend = match dict.get("Extend") {
            None => Extend::Pad,
            Some(value) => match (
                value.as_signed_integer(),
                value.as_string().map(str::to_ascii_lowercase).as_deref(),
            ) {
                (Some(0), _) | (_, Some("pad")) => Extend::Pad,
                (Some(1), _) | (_, Some("repeat")) => Extend::Repeat,
                (Some(2), _) | (_, Some("reflect")) => Extend::Reflect,
                _ => return Err(self.error("Extend must be pad, repeat, reflect, or 0..=2")),
            },
        };
        let stops = self
            .required(dict, "ColorStop")?
            .as_array()
            .ok_or_else(|| self.error("ColorStop must be an array"))?;
        if stops.len() > u16::MAX as usize {
            return Err(self.error("too many color stops"));
        }
        let stops = stops
            .iter()
            .map(|value| {
                let dict = self.dictionary(value, "ColorStop")?;
                self.fields(dict, &["StopOffset", "PaletteIndex", "Alpha"])?;
                Ok(ColorStop {
                    offset: self.fixed14(
                        self.required(dict, "StopOffset")?,
                        "StopOffset",
                        -2.0,
                        2.0 - 1.0 / 16384.0,
                    )?,
                    color: self.palette_index(self.required(dict, "PaletteIndex")?)?,
                    alpha: self.alpha(dict)?,
                })
            })
            .collect::<Result<_, Error>>()?;
        Ok((stops, extend))
    }

    fn paint(&self, value: &Value, depth: usize) -> Result<Paint, Error> {
        if depth > 256 {
            return Err(self.error("paint nesting exceeds 256 levels"));
        }
        if let Some(layers) = value.as_array() {
            return self.layers(
                layers
                    .iter()
                    .map(|value| self.paint(value, depth + 1))
                    .collect::<Result<_, _>>()?,
            );
        }
        let dict = self.dictionary(value, "Paint")?;
        let format = self
            .required(dict, "Format")?
            .as_signed_integer()
            .ok_or_else(|| self.error("Format must be an integer"))?;
        let fields: &[&str] = match format {
            1 => &["Format", "Layers"],
            2 => &["Format", "PaletteIndex", "Alpha"],
            4 => &["Format", "ColorLine", "x0", "y0", "x1", "y1", "x2", "y2"],
            6 => &["Format", "ColorLine", "x0", "y0", "r0", "x1", "y1", "r1"],
            10 => &["Format", "Glyph", "Paint"],
            11 => &["Format", "Glyph"],
            12 => &["Format", "Paint", "Transform"],
            14 => &["Format", "Paint", "dx", "dy"],
            _ => {
                return Err(Error::UnsupportedConstruct(format!(
                    "{UFO2FT_COLOR_LAYERS}: glyph '{}': unsupported COLRv1 paint format {format}",
                    self.base
                )));
            }
        };
        self.fields(dict, fields)?;
        let child = || self.paint(self.required(dict, "Paint")?, depth + 1);
        let point = |x, y| -> Result<Point, Error> {
            Ok((
                self.coordinate(dict, x, false)?,
                self.coordinate(dict, y, false)?,
            )
                .into())
        };
        Ok(match format {
            1 => {
                let layers = self
                    .required(dict, "Layers")?
                    .as_array()
                    .ok_or_else(|| self.error("Layers must be an array"))?;
                self.layers(
                    layers
                        .iter()
                        .map(|value| self.paint(value, depth + 1))
                        .collect::<Result<_, _>>()?,
                )?
            }
            2 => Paint::Solid(Box::new(PaintSolid {
                color: self.palette_index(self.required(dict, "PaletteIndex")?)?,
                alpha: self.alpha(dict)?,
            })),
            4 => {
                let (color_line, extend) = self.color_line(self.required(dict, "ColorLine")?)?;
                Paint::LinearGradient(Box::new(PaintLinearGradient {
                    color_line,
                    extend,
                    coordinates: GradientCoordinates::Absolute,
                    p0: point("x0", "y0")?,
                    p1: point("x1", "y1")?,
                    p2: Some(point("x2", "y2")?),
                }))
            }
            6 => {
                let (color_line, extend) = self.color_line(self.required(dict, "ColorLine")?)?;
                Paint::RadialGradient(Box::new(PaintRadialGradient {
                    color_line,
                    extend,
                    coordinates: GradientCoordinates::Absolute,
                    p0: point("x0", "y0")?,
                    p1: point("x1", "y1")?,
                    r0: Some((self.coordinate(dict, "r0", true)? as f32).into()),
                    r1: Some((self.coordinate(dict, "r1", true)? as f32).into()),
                }))
            }
            10 => Paint::Glyph(Box::new(PaintGlyph {
                name: self.glyph(self.required(dict, "Glyph")?)?,
                paint: child()?,
            })),
            11 => Paint::ColrGlyph(self.glyph(self.required(dict, "Glyph")?)?),
            12 => {
                let transform = self.required(dict, "Transform")?;
                let fields = ["xx", "yx", "xy", "yy", "dx", "dy"];
                let mut matrix = [0.0; 6];
                if let Some(values) = transform.as_array() {
                    if values.len() != 6 {
                        return Err(self.error("Transform must contain 6 values"));
                    }
                    for ((result, value), field) in matrix.iter_mut().zip(values).zip(fields) {
                        *result = self.numeric(value, field, -32768.0, 32768.0 - 1.0 / 65536.0)?;
                    }
                } else {
                    let transform = self.dictionary(transform, "Transform")?;
                    self.fields(transform, &fields)?;
                    for (result, field) in matrix.iter_mut().zip(fields) {
                        *result = self.numeric(
                            self.required(transform, field)?,
                            field,
                            -32768.0,
                            32768.0 - 1.0 / 65536.0,
                        )?;
                    }
                }
                Paint::Transform(Box::new(PaintTransform {
                    paint: child()?,
                    transform: Affine::new(matrix),
                }))
            }
            14 => Paint::Translate(Box::new(PaintTranslate {
                paint: child()?,
                dx: self.coordinate(dict, "dx", false)?,
                dy: self.coordinate(dict, "dy", false)?,
            })),
            _ => unreachable!(),
        })
    }
}

fn validate_references(base_glyphs: &IndexMap<GlyphName, Paint>) -> Result<(), Error> {
    let mut complete = HashSet::new();
    let mut active = HashSet::new();
    for root in base_glyphs.keys() {
        let mut pending = vec![(root, false)];
        while let Some((name, exiting)) = pending.pop() {
            if exiting {
                active.remove(name);
                complete.insert(name);
                continue;
            }
            if complete.contains(name) {
                continue;
            }
            if !active.insert(name) {
                return Err(invalid(format!(
                    "{UFO2FT_COLOR_LAYERS}: glyph '{root}': cyclic PaintColrGlyph reference to '{name}'"
                )));
            }
            let paint = base_glyphs.get(name).ok_or_else(|| invalid(format!("{UFO2FT_COLOR_LAYERS}: glyph '{root}': PaintColrGlyph target '{name}' has no color paint")))?;
            pending.push((name, true));
            let mut paints = vec![paint];
            while let Some(paint) = paints.pop() {
                match paint {
                    Paint::Layers(layers) => paints.extend(layers.iter()),
                    Paint::Glyph(glyph) => paints.push(&glyph.paint),
                    Paint::Transform(transform) => paints.push(&transform.paint),
                    Paint::Translate(translate) => paints.push(&translate.paint),
                    Paint::ColrGlyph(name) => pending.push((name, false)),
                    _ => (),
                }
            }
        }
    }
    Ok(())
}

fn parse_clip_boxes(
    value: Option<&Value>,
    base_glyphs: &IndexMap<GlyphName, Paint>,
) -> Result<HashMap<GlyphName, Rect>, Error> {
    let mut result = HashMap::new();
    let Some(value) = value else {
        return Ok(result);
    };
    let invalid = |message: &str| invalid(format!("{UFO2FT_COLR_CLIP_BOXES}: {message}"));
    for record in value
        .as_array()
        .ok_or_else(|| invalid("must be an array"))?
    {
        let pair = record
            .as_array()
            .filter(|pair| pair.len() == 2)
            .ok_or_else(|| invalid("expected [glyphNames, box]"))?;
        let names = pair[0]
            .as_array()
            .ok_or_else(|| invalid("glyphNames must be an array"))?;
        let coords = pair[1]
            .as_array()
            .ok_or_else(|| invalid("box must be an array"))?;
        if coords.len() == 5 {
            return Err(Error::UnsupportedConstruct(format!(
                "{UFO2FT_COLR_CLIP_BOXES}: variable clip box for {names:?}"
            )));
        }
        if coords.len() != 4 {
            return Err(invalid("box must contain 4 coordinates"));
        }
        let mut bounds = [0.0; 4];
        for (i, (bound, value)) in bounds.iter_mut().zip(coords).enumerate() {
            let value =
                number(value).ok_or_else(|| invalid("box coordinate must be a finite number"))?;
            // fontTools intRect rounds outwards to cover the source box.
            *bound = if i < 2 { value.floor() } else { value.ceil() };
            if !(-32768.0..=32767.0).contains(bound) {
                return Err(invalid("box coordinate out of int16 range"));
            }
        }
        let [x0, y0, x1, y1] = bounds;
        if x0 > x1 || y0 > y1 {
            return Err(invalid("box minimum exceeds maximum"));
        }
        for name in names {
            let name = name
                .as_string()
                .ok_or_else(|| invalid("glyph name must be a string"))?;
            if !base_glyphs.contains_key(name) {
                return Err(invalid(&format!("glyph '{name}' has no color paint")));
            }
            if result
                .insert(name.into(), Rect::new(x0, y0, x1, y1))
                .is_some()
            {
                return Err(invalid(&format!("duplicate clip box for glyph '{name}'")));
            }
        }
    }
    Ok(result)
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;

    #[rstest]
    #[case("[[[0, 0, 0]]]", "4 RGBA components")]
    #[case("[[[0, 0, 0, -0.1]]]", "finite number in 0..=1")]
    #[case("[[[0, 0, 0, .nan]]]", "finite number in 0..=1")]
    #[case("[[[0, 0, 0, '1']]]", "finite number in 0..=1")]
    fn rejects_invalid_palette(#[case] source: &str, #[case] expected: &str) {
        let value = serde_yaml::from_str(source).unwrap();
        let error = parse_color_palettes(&value).unwrap_err().to_string();
        assert!(error.contains(UFO2FT_COLOR_PALETTES), "{error}");
        assert!(
            error.contains(expected),
            "expected {expected:?}, got {error}"
        );
    }

    // These are source-format failures, distinct from the successful binary
    // round trips in fontc. Each case must identify the offending base glyph.
    #[rstest]
    #[case("a: []", "no color layers")]
    #[case("a: [[a, -1]]", "out of u16 range")]
    #[case("a: [[a, 65536]]", "out of u16 range")]
    #[case("a: [[a, 1]]", "palette index 1 out of range")]
    #[case("a: {Format: 3, PaletteIndex: 0}", "unsupported COLRv1 paint format 3")]
    #[case("a: {Format: 8}", "unsupported COLRv1 paint format 8")]
    #[case("a: {Format: 32}", "unsupported COLRv1 paint format 32")]
    #[case("a: {Format: '2'}", "Format must be an integer")]
    #[case("a: {Format: 2, PaletteIndex: 0, Alpha: 1.1}", "Alpha must be")]
    #[case(
        "a: {Format: 2, PaletteIndex: 0, VarIndexBase: 0}",
        "unsupported field 'VarIndexBase'"
    )]
    #[case("a: {Format: 10, Glyph: a}", "missing Paint")]
    #[case(
        "a: {Format: 10, Glyph: missing, Paint: {Format: 2, PaletteIndex: 0}}",
        "missing glyph 'missing'"
    )]
    #[case("a: {Format: 11, Glyph: b}", "target 'b' has no color paint")]
    #[case("a: {Format: 11, Glyph: a}", "cyclic PaintColrGlyph")]
    #[case(
        "a: {Format: 11, Glyph: b}\nb: {Format: 11, Glyph: a}",
        "cyclic PaintColrGlyph"
    )]
    #[case(
        "a: {Format: 14, dx: 32768, dy: 0, Paint: {Format: 2, PaletteIndex: 0}}",
        "dx out of range"
    )]
    #[case(
        "a: {Format: 12, Transform: [1, 0, 0, 1, .inf, 0], Paint: {Format: 2, PaletteIndex: 0}}",
        "dx must be a finite number"
    )]
    #[case(
        "a: {Format: 4, ColorLine: {Extend: 3, ColorStop: []}}",
        "Extend must be"
    )]
    #[case(
        "a: {Format: 4, ColorLine: {ColorStop: [{StopOffset: 2, PaletteIndex: 0}]}}",
        "StopOffset must be"
    )]
    #[case(
        "a: {Format: 6, ColorLine: {ColorStop: []}, x0: 0, y0: 0, x1: 0, y1: 0, r0: -1, r1: 100}",
        "r0 out of range"
    )]
    fn rejects_invalid_paint(#[case] source: &str, #[case] expected: &str) {
        let value = serde_yaml::from_str(source).unwrap();
        let palettes = ColorPalettes {
            palettes: vec![vec![Color {
                r: 0,
                g: 0,
                b: 0,
                a: 255,
            }]],
        };
        let glyph_order = ["a", "b"].map(GlyphName::from).into_iter().collect();
        let error = parse_color_layers(&value, &palettes, &glyph_order, None)
            .unwrap_err()
            .to_string();
        assert!(error.contains("glyph 'a'"), "{error}");
        assert!(
            error.contains(expected),
            "expected {expected:?}, got {error}"
        );
    }

    #[rstest]
    #[case("[[[a], [0, 0, 100, 100, 0]]]", "variable clip box")]
    #[case("[[[b], [0, 0, 100, 100]]]", "glyph 'b' has no color paint")]
    #[case("[[[a], [0, 0, 40000, 100]]]", "out of int16 range")]
    #[case("[[[a], [100, 0, 0, 100]]]", "minimum exceeds maximum")]
    fn rejects_invalid_clip_box(#[case] source: &str, #[case] expected: &str) {
        let value = serde_yaml::from_str(source).unwrap();
        let base_glyphs =
            IndexMap::from_iter([(GlyphName::from("a"), Paint::ColrGlyph("a".into()))]);
        let error = parse_clip_boxes(Some(&value), &base_glyphs)
            .unwrap_err()
            .to_string();
        assert!(error.contains(UFO2FT_COLR_CLIP_BOXES), "{error}");
        assert!(
            error.contains(expected),
            "expected {expected:?}, got {error}"
        );
    }
}
