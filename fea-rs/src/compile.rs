use crate::{parse::ParseTree, Diagnostic, GlyphMap};

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
