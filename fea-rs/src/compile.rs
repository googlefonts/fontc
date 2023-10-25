//! Compiling OpenType Layout tables

use write_fonts::types::GlyphId;

use crate::{parse::ParseTree, Diagnostic, GlyphMap, GlyphName};

use self::{
    compile_ctx::CompilationCtx,
    error::{FontGlyphOrderError, GlyphOrderError},
};

#[cfg(feature = "norad")]
use self::error::UfoGlyphOrderError;

pub use compiler::Compiler;
pub use opts::Opts;
pub use output::Compilation;
pub use variations::{AxisInfo, AxisLocation, VariationInfo};

#[cfg(any(test, feature = "test", feature = "cli"))]
pub use variations::MockVariationInfo;

mod compile_ctx;
mod compiler;
pub mod error;
mod features;
mod glyph_range;
mod language_system;
mod lookups;
mod opts;
mod output;
mod tables;
mod tags;
mod validate;
mod valuerecordext;
mod variations;

/// Run the validation pass, returning any diagnostics.
pub(crate) fn validate(
    node: &ParseTree,
    glyph_map: &GlyphMap,
    fvar: Option<&dyn VariationInfo>,
) -> Vec<Diagnostic> {
    let mut ctx = validate::ValidationCtx::new(node.source_map(), glyph_map, fvar);
    ctx.validate_root(&node.typed_root());
    ctx.errors
}

/// A helper function for extracting the glyph order from a UFO
///
/// If the public.glyphOrder key is missing, or the glyphOrder is malformed,
/// this will return `None`.
#[cfg(feature = "norad")]
pub fn get_ufo_glyph_order(font: &norad::Font) -> Result<GlyphMap, UfoGlyphOrderError> {
    static GLYPH_ORDER_KEY: &str = "public.glyphOrder";

    font.lib
        .get(GLYPH_ORDER_KEY)
        .ok_or(UfoGlyphOrderError::KeyNotSet)?
        .as_array()
        .and_then(|name_array| {
            name_array
                .iter()
                .map(|val| val.as_string().map(GlyphName::new))
                .collect()
        })
        .ok_or(UfoGlyphOrderError::Malformed)
}

/// A helper function for extracting glyph order from a font with a 'post' table
///
/// If 'post' is missing or malformed, this will return `None`.
pub fn get_post_glyph_order(font_data: &[u8]) -> Result<GlyphMap, FontGlyphOrderError> {
    use write_fonts::{
        from_obj::ToOwnedTable,
        read::{tables::post::DEFAULT_GLYPH_NAMES, FontRef, TableProvider},
        tables::post::Post,
    };
    let post: Post = FontRef::new(font_data)?.post()?.to_owned_table();
    post.glyph_name_index
        .as_ref()
        .and_then(|items| {
            items
                .iter()
                .map(|name_idx| match *name_idx {
                    i @ 0..=257 => Some(GlyphName::new(DEFAULT_GLYPH_NAMES[i as usize])),
                    i => post
                        .string_data
                        .as_ref()
                        .unwrap()
                        .get((i - 258) as usize)
                        .map(GlyphName::new),
                })
                .collect()
        })
        .ok_or(FontGlyphOrderError::MissingNames)
}

/// Extract a glyph order from an ordered list of glyph names.
///
/// Input must contain one glyph per line.
pub fn parse_glyph_order(glyphs: &str) -> Result<GlyphMap, GlyphOrderError> {
    let map: GlyphMap = glyphs
        .lines()
        .filter(|l| !l.is_empty() && !l.starts_with('#'))
        .map(|line| {
            if line.bytes().any(|b| b.is_ascii_whitespace()) {
                Err(GlyphOrderError::NameError {
                    name: line.to_owned(),
                })
            } else {
                Ok(GlyphName::new(line))
            }
        })
        .collect::<Result<_, _>>()?;
    if map.get(".notdef") != Some(GlyphId::NOTDEF) {
        Err(GlyphOrderError::MissingNotDef)
    } else {
        Ok(map)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn load_glyph_map() {
        let raw = std::fs::read_to_string("./test-data/simple_glyph_order.txt").unwrap();
        let glyph_map = parse_glyph_order(&raw).unwrap();
        assert_eq!(glyph_map.len(), 215);
        assert_eq!(glyph_map.get("space"), Some(GlyphId::new(1)));
        assert_eq!(glyph_map.get("e.fina"), Some(GlyphId::new(214)));
        assert!(!glyph_map.contains("e.nada"));
    }
}
