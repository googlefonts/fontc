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

use std::{ffi::OsString, path::PathBuf, sync::Arc};

pub use lexer::TokenSet;
pub use source::{FileSystemResolver, SourceLoadError, SourceResolver};
pub use tree::ParseTree;

pub(crate) use context::{IncludeStatement, ParseContext};
pub(crate) use parser::Parser;
pub(crate) use source::{FileId, Source, SourceList, SourceMap};

use crate::{Diagnostic, GlyphMap, Node};

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
) -> Result<(ParseTree, Vec<Diagnostic>), SourceLoadError> {
    let path = path.into();
    let project_root =
        project_root.unwrap_or_else(|| path.parent().map(PathBuf::from).unwrap_or_default());
    let resolver = source::FileSystemResolver::new(project_root);
    parse_root(path.into_os_string(), glyph_map, resolver)
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
pub fn parse_root(
    path: OsString,
    glyph_map: Option<&GlyphMap>,
    resolver: impl SourceResolver + 'static,
) -> Result<(ParseTree, Vec<Diagnostic>), SourceLoadError> {
    context::ParseContext::parse(path, glyph_map, Box::new(resolver))
        .map(|ctx| ctx.generate_parse_tree())
}

/// Convenience method to parse a block of FEA from memory.
///
/// This is useful for things like testing or syntax highlighting of a single file,
/// but it cannot handle imports, or handle ambiguous glyph names.
///
/// The input text can be any of `&str`, `String`, or `Arc<str>`.
pub fn parse_string(text: impl Into<Arc<str>>) -> (Node, Vec<Diagnostic>) {
    let source = source::Source::new("<parse::parse_string>", text.into());
    let (node, errs, _) = context::parse_src(&source, None);
    (node, errs)
}
