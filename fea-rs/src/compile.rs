use crate::{token_tree::typed, Diagnostic, GlyphMap, Node};

use self::{compile_ctx::CompilationCtx, validate::ValidationCtx};

pub use compile_ctx::Compilation;

mod compile_ctx;
mod glyph_range;
mod lookups;
mod validate;

/// Run the validation pass, returning any diagnostics.
pub fn validate(node: &Node, glyph_map: &GlyphMap) -> Vec<Diagnostic> {
    validate_impl(node, glyph_map).errors
}

/// Run the compilation pass (including validation)
pub fn compile<'a>(node: &Node, glyph_map: &'a GlyphMap) -> Result<Compilation, Vec<Diagnostic>> {
    let ctx = validate_impl(node, glyph_map);
    if ctx.errors.iter().any(Diagnostic::is_error) {
        return Err(ctx.errors);
    }

    let mut ctx = compile_impl(node, glyph_map);
    ctx.build()
}

fn validate_impl<'a>(node: &Node, glyph_map: &'a GlyphMap) -> ValidationCtx<'a> {
    let mut ctx = validate::ValidationCtx::new(glyph_map);
    if let Some(node) = typed::Root::try_from_node(node) {
        ctx.validate_root(&node);
    } else {
        panic!("validate called with invalid root node '{}'", node.kind());
    }
    ctx
}

fn compile_impl<'a>(node: &Node, glyph_map: &'a GlyphMap) -> CompilationCtx<'a> {
    let mut ctx = CompilationCtx::new(glyph_map);
    if let Some(node) = typed::Root::try_from_node(node) {
        ctx.compile(&node);
    } else {
        panic!("compile called with invalid root node '{}'", node.kind());
    }
    ctx
}
