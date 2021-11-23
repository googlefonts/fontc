//! the result of a parsing operation

use super::source::Source;
use super::{FileId, SourceList, SourceMap};
use crate::{token_tree::typed, Diagnostic, Node};

/// A fully parsed feature file, with attached imports and a sourcemap.
pub struct ParseTree {
    pub(crate) root: Node,
    pub(crate) sources: SourceList,
    pub(crate) map: SourceMap,
}

impl ParseTree {
    pub fn root(&self) -> &Node {
        &self.root
    }

    pub fn typed_root(&self) -> typed::Root {
        typed::Root::try_from_node(&self.root).expect("parse tree has invalid root node type")
    }

    pub fn source_map(&self) -> &SourceMap {
        &self.map
    }

    pub fn get_source(&self, id: FileId) -> Option<&Source> {
        self.sources.get(&id)
    }

    pub fn format_diagnostic(&self, err: &Diagnostic) -> String {
        let mut s = String::new();
        let source = self.get_source(err.message.file).unwrap();
        crate::util::highlighting::write_diagnostic(&mut s, err, source, None);
        s
    }
}
