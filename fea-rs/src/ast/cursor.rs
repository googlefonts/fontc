//! A cursor for navigating through a tree.

use super::{stack::Stack, Node, NodeOrToken, Token};

pub struct Cursor<'a> {
    pos: usize,
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
            pos: 0,
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
}

impl<'a> NodeRef<'a> {
    fn current(&self) -> Option<&'a NodeOrToken> {
        self.node.children.get(self.idx)
    }

    fn advance(&mut self) -> Option<&'a NodeOrToken> {
        self.fresh = true;
        let idx = self.idx;
        self.idx += 1;
        self.node.children.get(idx)
    }

    fn is_done(&self) -> bool {
        self.idx >= self.node.children.len()
    }
}
