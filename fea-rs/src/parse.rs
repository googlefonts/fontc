//! Load and tokenize sources.
//!
//! In general, you should not need to use this module directly; it is exposed
//! so that it can be used for things like syntax highlighting.

mod context;
pub(crate) mod grammar;
mod lexer;
mod parser;
mod source;
mod tree;

use std::{
    path::{Path, PathBuf},
    sync::Arc,
};

pub use lexer::TokenSet;
pub use source::{FileSystemResolver, SourceLoadError, SourceResolver};
pub use tree::ParseTree;

pub(crate) use context::{IncludeStatement, ParseContext};
pub(crate) use parser::Parser;
pub(crate) use source::{FileId, Source, SourceList, SourceMap};

use crate::{DiagnosticSet, GlyphMap};

/// Attempt to parse a feature file from disk, including its imports.
///
/// In general, you should not need to use this method directly; instead use one
/// of the methods in the [`compile`][crate::compile] module.
///
/// The `project_root` argument is optional, and is intended for the case of
/// handling UFO files, where imports are resolved relative to the root directory,
/// and not the feature file itself.
///
/// The `glyph_map`, if provided, is used to disambiguate between certain tokens
/// that are allowed in FEA syntax but which are also legal glyph names. If it
/// is absent, and these names are encountered, we will report an error.
///
/// If you are compiling from memory, or otherwise want to handle loading files
/// and resolving imports, you can use [`parse_root`] instead.
pub fn parse_root_file(
    path: impl Into<PathBuf>,
    glyph_map: Option<&GlyphMap>,
    project_root: Option<PathBuf>,
) -> Result<(ParseTree, DiagnosticSet), SourceLoadError> {
    let path = path.into();
    let project_root =
        project_root.unwrap_or_else(|| path.parent().map(PathBuf::from).unwrap_or_default());
    let resolver = source::FileSystemResolver::new(project_root);
    parse_root(path, glyph_map, Box::new(resolver))
}

/// Entry point for parsing.
///
/// This method provides full control over how sources are located and include
/// statements are resolved, by allowing you to provide a custom [`SourceResolver`].
///
/// The `path` argument is identifies the root source; it will be resolved against
/// the provided `resolver`.
///
/// The `glyph_map`, if provided, is used to disambiguate between certain tokens
/// that are allowed in FEA syntax but which are also legal glyph names. If you
/// are not compiling the parse results, you can omit it.
///
/// This returns an error if a source could not be located; otherwise it returns
/// a parse tree and a set of diagnostics, even if parsing fails. The caller must
/// check that there are no errors (via [`DiagnosticSet::has_errors`]) to know
/// whether or not parsing was successful.
pub fn parse_root(
    path: PathBuf,
    glyph_map: Option<&GlyphMap>,
    resolver: Box<dyn SourceResolver>,
) -> Result<(ParseTree, DiagnosticSet), SourceLoadError> {
    context::ParseContext::parse(path, glyph_map, resolver).map(|ctx| ctx.generate_parse_tree())
}

/// Convenience method to parse a block of FEA from memory.
///
/// This is useful for things like testing or syntax highlighting of a single file,
/// but it cannot handle includes, or handle ambiguous glyph names.
///
/// The input text can be any of `&str`, `String`, or `Arc<str>`.
///
/// # Panics
///
/// Panics if the input contains any include statements.
pub fn parse_string(text: impl Into<Arc<str>>) -> (ParseTree, DiagnosticSet) {
    const SRC_NAME: &str = "parse::parse_string";
    let text = text.into();
    parse_root(
        SRC_NAME.into(),
        None,
        Box::new(move |s: &Path| {
            if s == Path::new(SRC_NAME) {
                Ok(text.clone())
            } else {
                Err(SourceLoadError::new(
                    s.to_path_buf(),
                    "parse_string cannot handle imports",
                ))
            }
        }),
    )
    .unwrap()
}

/// Parse an arbitrary block of FEA text with a specific parsing function.
///
/// This can be used to parse any part of the grammar, including elements that
/// are not valid at the top level.
#[cfg(test)]
pub(crate) fn parse_node(text: &str, parser_fn: impl FnOnce(&mut Parser)) -> crate::Node {
    let mut sink = crate::token_tree::AstSink::new(text, FileId::CURRENT_FILE, None);
    let mut parser = Parser::new(text, &mut sink);
    parser_fn(&mut parser);
    let (root, _errs, _) = sink.finish();
    root
}
