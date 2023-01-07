//! Compiling OpenType Layout tables

use crate::{parse::ParseTree, Diagnostic, GlyphMap, GlyphName};

use self::{compile_ctx::CompilationCtx, validate::ValidationCtx};

pub use output::Compilation;

mod common;
mod compile_ctx;
mod features;
mod glyph_range;
mod language_system;
mod lookups;
mod output;
mod tables;
mod validate;
mod valuerecordext;

/// Run the validation pass, returning any diagnostics.
pub fn validate(node: &ParseTree, glyph_map: &GlyphMap) -> Vec<Diagnostic> {
    validate_impl(node, glyph_map).errors
}

/// Run the compilation pass (including validation)
pub fn compile(node: &ParseTree, glyph_map: &GlyphMap) -> Result<Compilation, Vec<Diagnostic>> {
    let ctx = validate_impl(node, glyph_map);
    if ctx.errors.iter().any(Diagnostic::is_error) {
        return Err(ctx.errors);
    }

    let mut ctx = compile_impl(node, glyph_map);
    ctx.build()
}

fn validate_impl<'a>(node: &'a ParseTree, glyph_map: &'a GlyphMap) -> ValidationCtx<'a> {
    let mut ctx = validate::ValidationCtx::new(glyph_map, node.source_map());
    ctx.validate_root(&node.typed_root());
    ctx
}

fn compile_impl<'a>(node: &'a ParseTree, glyph_map: &'a GlyphMap) -> CompilationCtx<'a> {
    let mut ctx = CompilationCtx::new(glyph_map, node.source_map());
    ctx.compile(&node.typed_root());
    ctx
}

static GLYPH_ORDER_KEY: &str = "public.glyphOrder";

/// A helper function for extracting the glyph order from a UFO
///
/// If the public.glyphOrder key is missing, or the glyphOrder is malformed,
/// this will return `None`.
pub fn get_ufo_glyph_order(font: &norad::Font) -> Option<GlyphMap> {
    font.lib
        .get(GLYPH_ORDER_KEY)
        .and_then(|val| val.as_array())
        .and_then(|name_array| {
            name_array
                .iter()
                .map(|val| val.as_string().map(GlyphName::new))
                .collect()
        })
}

/// A helper function for extracting glyph order from a font with a 'post' table
///
/// If 'post' is missing or malformed, this will return `None`.
pub fn get_post_glyph_order(font_data: &[u8]) -> Option<GlyphMap> {
    use write_fonts::{
        from_obj::ToOwnedTable,
        read::{tables::post::DEFAULT_GLYPH_NAMES, FontRef, TableProvider},
        tables::post::Post,
    };
    let font = FontRef::new(font_data).ok()?;
    let post: Post = font.post().ok()?.to_owned_table();
    post.glyph_name_index
        .as_ref()?
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
}
