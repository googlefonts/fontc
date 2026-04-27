use std::collections::HashMap;
use std::path::{Path, PathBuf};

use fontdrasil::types::GlyphName;
use log::warn;

use crate::color::GradientInfo;

#[derive(Debug)]
#[allow(dead_code)]
pub(crate) struct ExtractedPath {
    pub segments: Vec<PathSegment>,
    pub fill: Option<usvg::Fill>,
    pub transform: usvg::Transform,
    pub opacity: f32,
}

#[derive(Debug, Clone)]
#[allow(dead_code)]
pub(crate) enum PathSegment {
    MoveTo(f32, f32),
    LineTo(f32, f32),
    QuadTo(f32, f32, f32, f32),
    CubicTo(f32, f32, f32, f32, f32, f32),
    Close,
}

#[allow(dead_code)]
pub(crate) fn discover_svgs(
    dir: &Path,
) -> Result<HashMap<GlyphName, (PathBuf, u32)>, std::io::Error> {
    let mut glyphs = HashMap::new();
    for entry in std::fs::read_dir(dir)? {
        let entry = entry?;
        let path = entry.path();
        if !path.is_file() {
            continue;
        }
        let Some(ext) = path.extension() else {
            continue;
        };
        if ext != "svg" {
            continue;
        }
        let Some(stem) = path.file_stem() else {
            continue;
        };
        let Some(codepoint) = parse_codepoint_from_filename(stem.to_string_lossy().as_ref()) else {
            warn!("skipping non-matching SVG: {}", path.display());
            continue;
        };
        let Ok(c) = char::from_u32(codepoint).ok_or(()) else {
            warn!("invalid codepoint U+{:X} in {}", codepoint, path.display());
            continue;
        };
        let name = GlyphName::new(c.to_string());
        glyphs.insert(name, (path, codepoint));
    }
    Ok(glyphs)
}

#[allow(dead_code)]
pub(crate) fn parse_codepoint_from_filename(name: &str) -> Option<u32> {
    let name = name.strip_suffix(".svg").unwrap_or(name);
    // Support both "u{hex}" and "emoji_u{hex}" (noto-emoji naming convention).
    let hex = name
        .strip_prefix("emoji_u")
        .or_else(|| name.strip_prefix('u'))?;
    if hex.is_empty() || hex.len() > 6 {
        return None;
    }
    u32::from_str_radix(hex, 16).ok()
}

#[allow(dead_code)]
pub(crate) fn parse_svg(path: &Path) -> Result<(usvg::Tree, Vec<u8>), ParseError> {
    let data = std::fs::read(path).map_err(ParseError::Io)?;
    let tree = usvg::Tree::from_data(&data, &usvg::Options::default()).map_err(ParseError::Svg)?;
    Ok((tree, data))
}

#[derive(Debug)]
#[allow(dead_code)]
pub(crate) enum ParseError {
    Io(std::io::Error),
    Svg(usvg::Error),
}

impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ParseError::Io(e) => write!(f, "IO error: {e}"),
            ParseError::Svg(e) => write!(f, "SVG parse error: {e:?}"),
        }
    }
}

impl std::error::Error for ParseError {}

#[allow(dead_code)]
pub(crate) fn extract_paths(tree: &usvg::Tree) -> Vec<ExtractedPath> {
    let mut result = Vec::new();
    collect_paths(tree.root(), &mut result);
    result
}

fn collect_paths(group: &usvg::Group, result: &mut Vec<ExtractedPath>) {
    let opacity = group.opacity().get();
    for node in group.children() {
        match node {
            usvg::Node::Path(p) => {
                if !p.is_visible() {
                    continue;
                }
                let segments = path_segments(p.data());
                let fill = p.fill().cloned();
                let transform = p.abs_transform();
                result.push(ExtractedPath {
                    segments,
                    fill,
                    transform,
                    opacity,
                });
            }
            usvg::Node::Group(g) => {
                collect_paths(g, result);
            }
            _ => {}
        }
    }
}

fn path_segments(path: &usvg::tiny_skia_path::Path) -> Vec<PathSegment> {
    path.segments()
        .map(|seg| match seg {
            usvg::tiny_skia_path::PathSegment::MoveTo(p) => PathSegment::MoveTo(p.x, p.y),
            usvg::tiny_skia_path::PathSegment::LineTo(p) => PathSegment::LineTo(p.x, p.y),
            usvg::tiny_skia_path::PathSegment::QuadTo(p0, p1) => {
                PathSegment::QuadTo(p0.x, p0.y, p1.x, p1.y)
            }
            usvg::tiny_skia_path::PathSegment::CubicTo(p0, p1, p2) => {
                PathSegment::CubicTo(p0.x, p0.y, p1.x, p1.y, p2.x, p2.y)
            }
            usvg::tiny_skia_path::PathSegment::Close => PathSegment::Close,
        })
        .collect()
}

#[allow(dead_code)]
pub(crate) fn extract_gradient_defs(tree: &usvg::Tree) -> HashMap<String, GradientInfo> {
    let mut gradients = HashMap::new();
    for lg in tree.linear_gradients() {
        let id = lg.id().to_string();
        let stops = lg
            .stops()
            .iter()
            .map(|s| super::color::ColorStopInfo {
                offset: s.offset().get(),
                r: s.color().red,
                g: s.color().green,
                b: s.color().blue,
                alpha: s.opacity().get(),
            })
            .collect();
        gradients.insert(
            id,
            GradientInfo::Linear {
                x1: lg.x1(),
                y1: lg.y1(),
                x2: lg.x2(),
                y2: lg.y2(),
                stops,
            },
        );
    }
    for rg in tree.radial_gradients() {
        let id = rg.id().to_string();
        let stops = rg
            .stops()
            .iter()
            .map(|s| super::color::ColorStopInfo {
                offset: s.offset().get(),
                r: s.color().red,
                g: s.color().green,
                b: s.color().blue,
                alpha: s.opacity().get(),
            })
            .collect();
        gradients.insert(
            id,
            GradientInfo::Radial {
                cx: rg.cx(),
                cy: rg.cy(),
                r: rg.r().get(),
                fx: rg.fx(),
                fy: rg.fy(),
                stops,
            },
        );
    }
    gradients
}

/// Extract the viewBox rectangle from raw SVG data.
///
/// Parses the viewBox attribute using roxmltree. If missing, defaults to (0,0,width,height)
/// where width/height are taken from the SVG's width/height attributes, or defaults to 100x100.
/// Returns (x, y, width, height).
#[allow(dead_code)]
pub(crate) fn extract_viewbox_from_data(data: &[u8]) -> Option<(f32, f32, f32, f32)> {
    let document = usvg::roxmltree::Document::parse(std::str::from_utf8(data).ok()?).ok()?;
    let root = document.root_element();
    // Get width/height attributes (default 100)
    let width = root
        .attribute("width")
        .and_then(|s| s.parse::<f32>().ok())
        .unwrap_or(100.0);
    let height = root
        .attribute("height")
        .and_then(|s| s.parse::<f32>().ok())
        .unwrap_or(100.0);
    // Parse viewBox attribute
    if let Some(view_box_str) = root.attribute("viewBox") {
        let parts: Vec<&str> = view_box_str.split_whitespace().collect();
        if parts.len() == 4 {
            let x = parts[0].parse::<f32>().ok()?;
            let y = parts[1].parse::<f32>().ok()?;
            let w = parts[2].parse::<f32>().ok()?;
            let h = parts[3].parse::<f32>().ok()?;
            return Some((x, y, w, h));
        }
    }
    // Default viewBox based on width/height
    Some((0.0, 0.0, width, height))
}

/// Extract the viewBox from a parsed SVG tree.
///
/// This function is a convenience wrapper that extracts viewBox from the raw SVG data.
/// Since usvg does not expose viewBox publicly, we need to parse it ourselves.
/// The tree parameter is unused but kept for compatibility; we'll compute viewBox from
/// the original data passed to parse_svg.
#[allow(dead_code)]
pub(crate) fn extract_viewbox(tree: &usvg::Tree, data: &[u8]) -> (f32, f32, f32, f32) {
    extract_viewbox_from_data(data).unwrap_or((0.0, 0.0, tree.size().width(), tree.size().height()))
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;

    fn testdata_dir() -> PathBuf {
        Path::new("../resources/testdata/svg").to_path_buf()
    }

    #[rstest]
    #[case("u0041", Some(0x0041))]
    #[case("u1F600", Some(0x1F600))]
    #[case("u0041_u0301", None)]
    #[case("readme.txt", None)]
    #[case("uGGGG", None)]
    #[case("0041", None)]
    #[case("U0041", None)]
    #[case("u", None)]
    #[case("u1234567", None)]
    fn test_parse_codepoint(#[case] input: &str, #[case] expected: Option<u32>) {
        assert_eq!(parse_codepoint_from_filename(input), expected);
    }

    #[test]
    fn test_discover_svgs_simple() {
        let dir = testdata_dir().join("simple_glyphs");
        let glyphs = discover_svgs(&dir).unwrap();
        assert_eq!(glyphs.len(), 2);
        assert!(glyphs.contains_key(&GlyphName::new("A")));
        assert!(glyphs.contains_key(&GlyphName::new("B")));
    }

    #[test]
    fn test_parse_simple_svg() {
        let path = testdata_dir().join("simple_glyphs/u0041.svg");
        let (tree, _data) = parse_svg(&path).unwrap();
        let paths = extract_paths(&tree);
        assert!(!paths.is_empty());
        assert!(paths[0].fill.is_some());
    }

    #[test]
    fn test_parse_multi_layer_svg() {
        let path = testdata_dir().join("multi_layer/u1F600.svg");
        let (tree, _data) = parse_svg(&path).unwrap();
        let paths = extract_paths(&tree);
        assert!(paths.len() >= 3);
    }

    #[test]
    fn test_parse_gradient_svg() {
        let path = testdata_dir().join("gradient_glyphs/u2B50.svg");
        let (tree, _data) = parse_svg(&path).unwrap();
        let gradients = extract_gradient_defs(&tree);
        assert!(!gradients.is_empty());
    }

    #[test]
    fn test_parse_opacity_group() {
        let path = testdata_dir().join("opacity_group/u2728.svg");
        let (tree, _data) = parse_svg(&path).unwrap();
        let paths = extract_paths(&tree);
        assert!(paths.len() >= 2);
        assert!(paths[0].opacity < 1.0);
    }

    #[test]
    fn test_parse_nested_groups() {
        let path = testdata_dir().join("nested_groups/u26A1.svg");
        let (tree, _data) = parse_svg(&path).unwrap();
        let paths = extract_paths(&tree);
        assert!(!paths.is_empty());
    }
}
