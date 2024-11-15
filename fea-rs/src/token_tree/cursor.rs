//! A cursor for navigating through a tree.

use super::{stack::Stack, Kind, Node, NodeOrToken, Token};

pub(crate) struct Cursor<'a> {
    pos: usize,
    // the current root. This is not directly accessible.
    current: NodeRef<'a>,
    parents: Stack<NodeRef<'a>, 4>,
}

struct NodeRef<'a> {
    node: &'a Node,
    /// true if we are pointing at a child node, but have not yet descended.
    fresh: bool,
    // the idx of the node's current child
    idx: usize,
}

impl<'a> Cursor<'a> {
    pub fn new(root: &'a Node) -> Self {
        Cursor {
            pos: root.abs_pos as usize,
            current: NodeRef {
                node: root,
                fresh: true,
                idx: 0,
            },
            parents: Stack::default(),
        }
    }

    /// Our current depth in the tree
    #[allow(dead_code)] // only used when debugging
    pub fn depth(&self) -> usize {
        self.parents.len()
    }

    /// The start position of the current token
    pub fn pos(&self) -> usize {
        self.pos
    }

    /// The kind of the current parent node.
    pub fn parent_kind(&self) -> Kind {
        self.current.node.kind
    }

    pub fn next_token(&mut self) -> Option<&'a Token> {
        loop {
            let current = self.current();
            self.advance();
            match current {
                Some(NodeOrToken::Node(_)) => (),
                Some(NodeOrToken::Token(t)) => return Some(t),
                None => break None,
            }
        }
    }

    /// advance the cursor, stepping over nodes.
    pub fn step_over(&mut self) {
        let len = self.current().map(NodeOrToken::text_len).unwrap_or(0);
        self.current.advance();
        self.pos += len;
    }

    /// Advance the cursor.
    ///
    /// This descends or returns into and from child nodes.
    pub fn advance(&mut self) {
        self.pos += self.text_len_if_at_token().unwrap_or(0);
        match self.current() {
            Some(NodeOrToken::Token(_)) => {
                self.current.advance();
            }
            Some(NodeOrToken::Node(node)) => {
                if self.current.fresh {
                    self.descend(node);
                } else {
                    self.current.advance();
                }
            }
            // we need to pop parent, which we do below
            None => (),
        }

        // if we are finished a node (including when we just advanced) we restore
        // the previous unfinished parent
        if self.current().is_none() {
            assert!(self.current.is_done());
            while self.current.is_done() {
                match self.parents.pop() {
                    Some(parent) => self.current = parent,
                    // this must be the root node, and it must be finished
                    None => return,
                }
                // after ascending, we need to advance
                self.current.advance();
            }
        }
    }

    fn text_len_if_at_token(&self) -> Option<usize> {
        match self.current()? {
            NodeOrToken::Token(t) => Some(t.text.len()),
            _ => None,
        }
    }

    /// The current node or token.
    ///
    /// This is only `None` if the cursor is advanced past the end of the tree.
    ///
    /// This will never point to the root node itself, only its descendents.
    //TODO: we could solve this with a dummy root, do we care?
    pub fn current(&self) -> Option<&'a NodeOrToken> {
        self.current.current()
    }

    // move down into a child node.
    //
    // invariant: `node` is a child of `self.current`
    fn descend(&mut self, node: &'a Node) {
        // move the current node onto the parent set
        let new_current = NodeRef {
            node,
            fresh: true,
            idx: 0,
        };
        let mut prev = std::mem::replace(&mut self.current, new_current);
        prev.fresh = false;
        self.parents.push(prev);
    }

    pub fn descend_current(&mut self) {
        let new_current = self
            .current()
            .and_then(NodeOrToken::as_node)
            .expect("descend_current expects current to be Node");
        self.descend(new_current)
    }

    /// Return to a parent node.
    ///
    /// This always sets the cursor position to the start position of the
    /// *current* node (which is within the parent).
    pub fn ascend(&mut self) {
        let len = self.current.prev_items_len();
        if let Some(parent) = self.parents.pop() {
            self.current = parent;
        }
        self.pos -= len;
    }
}

impl<'a> NodeRef<'a> {
    fn current(&self) -> Option<&'a NodeOrToken> {
        self.node.children.get(self.idx)
    }

    fn children(&self) -> &[NodeOrToken] {
        self.node.children.as_ref()
    }

    fn advance(&mut self) -> Option<&'a NodeOrToken> {
        self.fresh = true;
        let idx = self.idx;
        self.idx += 1;
        self.node.children.get(idx)
    }

    /// The length of the items previously visited
    fn prev_items_len(&self) -> usize {
        self.children()[..self.idx]
            .iter()
            .map(NodeOrToken::text_len)
            .sum()
    }

    fn is_done(&self) -> bool {
        self.idx >= self.node.children.len()
    }
}

impl std::fmt::Debug for NodeRef<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        std::fmt::Debug::fmt(self.node, f)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    static SAMPLE_FEA: &str = include_str!("../../test-data/fonttools-tests/mini.fea");

    #[test]
    fn abs_positions() {
        let (ast, errs) = crate::parse::parse_string(SAMPLE_FEA);
        assert!(errs.is_empty());
        let mut last_end = 0;
        for token in ast.root().iter_tokens() {
            assert_eq!(
                token.range().start,
                last_end,
                "{:?}: '{}'",
                token.range(),
                token.text
            );
            last_end = token.range().end;
        }
    }

    #[test]
    fn ascend_jump() {
        let (ast, _errs) = crate::parse::parse_string(SAMPLE_FEA);
        let mut cursor = ast.root().cursor();
        cursor.advance();
        cursor.advance();
        cursor.advance();
        cursor.advance();
        cursor.advance();
        cursor.advance();
        let pre_descent = cursor.pos();
        cursor.descend_current();
        cursor.ascend();
        assert_eq!(cursor.pos(), pre_descent);
        cursor.descend_current();
        cursor.advance();
        cursor.ascend();
        assert_eq!(cursor.pos(), pre_descent);
    }

    fn at_node(cursor: &Cursor, kind: Kind) -> bool {
        cursor
            .current()
            .and_then(NodeOrToken::as_node)
            .map(|n| n.kind == kind)
            .unwrap_or(false)
    }

    fn at_token(cursor: &Cursor, kind: Kind) -> bool {
        cursor
            .current()
            .and_then(NodeOrToken::as_token)
            .map(|n| n.kind == kind)
            .unwrap_or(false)
    }

    #[test]
    fn advance() {
        let (ast, errs) = crate::parse::parse_string("feature kern { pos a b -20; }kern;");
        assert!(errs.is_empty());
        let mut cursor = ast.root().cursor();
        assert!(
            at_node(&cursor, Kind::FeatureNode),
            "{:?}",
            cursor.current()
        );
        cursor.advance();
        assert!(at_token(&cursor, Kind::FeatureKw), "{:?}", cursor.current());
        cursor.advance();
        assert!(at_token(&cursor, Kind::Whitespace));
        cursor.advance();
        assert!(at_token(&cursor, Kind::Tag));
        cursor.advance();
        cursor.advance();
        assert!(at_token(&cursor, Kind::LBrace), "{:?}", cursor.current());
        cursor.advance();
        assert!(
            at_token(&cursor, Kind::Whitespace),
            "{:?}",
            cursor.current()
        );
        cursor.advance();
        assert!(at_node(&cursor, Kind::GposType2), "{:?}", cursor.current());
        cursor.advance();
        cursor.advance();
        cursor.advance();
        cursor.advance();
        cursor.advance();
        assert!(at_token(&cursor, Kind::GlyphName), "{:?}", cursor.current());
        cursor.advance();
        cursor.advance();
        assert!(
            at_node(&cursor, Kind::ValueRecordNode),
            "{:?}",
            cursor.current()
        );
        cursor.advance();
        assert!(at_token(&cursor, Kind::Number), "{:?}", cursor.current());
        cursor.advance();
        assert!(at_token(&cursor, Kind::Semi), "{:?}", cursor.current());
    }
}
