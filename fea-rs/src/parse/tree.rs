//! the result of a parsing operation

use std::rc::Rc;

use super::source::Source;
use super::{FileId, SourceList, SourceMap};
use crate::{token_tree::typed, Diagnostic, Node};

/// A fully parsed feature file, with attached imports and a sourcemap.
///
/// As well as representing the entire AST, this type also allows mapping tokens
/// and other spans back to a position in a particular source file.
///
/// This is cheap to clone, so it can be attached to diagnostics, allowing them
/// to print themselves where needed.
#[derive(Clone, Debug)]
pub struct ParseTree {
    pub(crate) root: Node,
    pub(crate) sources: Rc<SourceList>,
    pub(crate) map: Rc<SourceMap>,
}

impl ParseTree {
    /// The root node for this parse tree
    pub fn root(&self) -> &Node {
        &self.root
    }

    /// The root node, as typed AST node
    pub fn typed_root(&self) -> typed::Root {
        typed::Root::try_from_node(&self.root).expect("parse tree has invalid root node type")
    }

    /// Return a refernce to the source map
    pub fn source_map(&self) -> &SourceMap {
        &self.map
    }

    /// Return the source for this id, if it exists in the source map
    pub fn get_source(&self, id: FileId) -> Option<&Source> {
        self.sources.get(&id)
    }

    /// Generate a string suitable for presenting a [`Diagnostic`] to the user.
    ///
    /// This associates the message with the appropriate source location and
    /// syntax highlighting.
    pub fn format_diagnostic(&self, err: &Diagnostic) -> String {
        let mut s = String::new();
        let source = self.get_source(err.message.file).unwrap();
        crate::util::highlighting::write_diagnostic(&mut s, err, source, None);
        s
    }
}
