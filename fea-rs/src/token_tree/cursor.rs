//! A cursor for navigating through a tree.

use super::{stack::Stack, Kind, Node, NodeOrToken, Token};

pub struct Cursor<'a> {
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
        if let Some(child) = root.children.first() {
            child.set_abs_pos(root.abs_pos.get() as usize);
        }
        Cursor {
            pos: root.abs_pos.get() as usize,
            current: NodeRef {
                node: root,
                fresh: true,
                idx: 0,
            },
            parents: Stack::default(),
        }
    }

    /// Our current depth in the tree
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
        if let Some(current) = self.current() {
            current.set_abs_pos(self.pos);
        }
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
        if let Some(current) = self.current() {
            current.set_abs_pos(self.pos);
        }
    }

    /// Move the cursor to the token containing the offset `pos`.
    ///
    ///// panics if pos is less than the cursor's current pos.
    pub fn seek(&mut self, pos: usize) {
        // first ascend until pos is in front of us.
        self.move_to_start_of_current_node();
        while pos < self.pos || pos >= self.pos + self.current.node.text_len() {
            self.ascend();
            self.move_to_start_of_current_node();
        }

        // now we want to find the position of the node that contains
        // this position. if none exists, we're at a token?

        assert_eq!(self.current.idx, 0);
        loop {
            let rel_pos = pos - self.pos;
            let idx = self.current.idx_for_pos(rel_pos);
            let len: usize = self.current.children()[..idx]
                .iter()
                .map(NodeOrToken::text_len)
                .sum();
            self.pos += len;
            self.current.jump(idx);
            match self.current() {
                Some(NodeOrToken::Token(_)) => break,
                Some(NodeOrToken::Node(node)) => self.descend(node),
                None => unreachable!(),
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

    fn move_to_start_of_current_node(&mut self) {
        let len = self.current.prev_items_len();
        self.current.jump(0);
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

    fn jump(&mut self, idx: usize) {
        self.fresh = true;
        self.idx = idx;
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

    /// return the idx of the child containing `pos`.
    ///
    /// `pos` is relative to the current node.
    fn idx_for_pos(&mut self, pos: usize) -> usize {
        assert!(pos < self.node.text_len());
        let mut text_pos = 0;
        for (i, child) in self.children().iter().enumerate() {
            if (text_pos..text_pos + child.text_len()).contains(&pos) {
                return i;
            }
            text_pos += child.text_len();
        }
        unreachable!()
    }
}

impl std::fmt::Debug for NodeRef<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        std::fmt::Debug::fmt(self.node, f)
    }
}

#[cfg(test)]
mod tests {
    use crate::parse::Source;

    use super::*;

    static SAMPLE_FEA: &str = include_str!("../../test-data/fonttools-tests/mini.fea");

    #[test]
    fn seek() {
        let fea = Source::from_text(SAMPLE_FEA);
        let (root, _errs, _) = crate::parse_src(&fea, None);

        let mut cursor = root.cursor();
        cursor.seek(192);
        assert_eq!(
            cursor.current().and_then(NodeOrToken::token_text).unwrap(),
            "DFLT"
        );
        cursor.seek(300);
        assert_eq!(
            cursor.current().and_then(NodeOrToken::token_text).unwrap(),
            "substitute"
        );
    }

    #[test]
    fn abs_positions() {
        let fea = Source::from_text(SAMPLE_FEA);
        let (root, errs, _) = crate::parse_src(&fea, None);
        assert!(errs.is_empty());
        let mut last_end = 0;
        for token in root.iter_tokens() {
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
        let fea = Source::from_text(SAMPLE_FEA);
        let (root, _errs, _) = crate::parse_src(&fea, None);
        let mut cursor = root.cursor();
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
        let fea = Source::from_text("feature kern { pos a b -20; }kern;");
        let (root, errs, _) = crate::parse_src(&fea, None);
        assert!(errs.is_empty());
        let mut cursor = root.cursor();
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
