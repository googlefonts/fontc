use std::{mem::MaybeUninit, sync::Arc};

use smol_str::SmolStr;

use crate::{
    parse::{SyntaxError, TreeSink},
    Kind,
};

#[derive(PartialEq, Eq, Clone, Copy)]
struct SyntaxKind(u16);

#[derive(Debug, PartialEq, Eq, Clone, PartialOrd, Ord, Hash)]
pub struct Node {
    kind: Kind,
    // start of this node relative to start of parent node
    //rel_pos: usize,
    text_len: usize,
    children: Arc<[NodeOrToken]>,
}

#[derive(Debug, PartialEq, Eq, Clone, PartialOrd, Ord, Hash)]
pub struct Token {
    pub kind: Kind,
    pub text: SmolStr,
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum NodeOrToken {
    Node(Node),
    Token(Token),
}

#[derive(Clone, Debug, Default)]
struct TreeBuilder {
    //TODO: reuse tokens
    //token_cache: HashMap<Arc<Token>>,
    // the kind of the parent, and the index in children of the first child.
    parents: Vec<(Kind, usize)>,
    children: Vec<NodeOrToken>,
}

pub struct AstSink<'a> {
    text: &'a str,
    text_pos: usize,
    builder: TreeBuilder,
    errors: Vec<SyntaxError>,
}

pub struct Cursor<'a> {
    pos: usize,
    current: NodeRef<'a>,
    parents: ParentStack<NodeRef<'a>, 4>,
}

struct NodeRef<'a> {
    node: &'a Node,
    // the idx of the node's current child
    idx: usize,
}

impl<'a> Cursor<'a> {
    fn new(root: &'a Node) -> Self {
        Cursor {
            pos: 0,
            current: NodeRef { node: root, idx: 0 },
            parents: ParentStack::new(),
        }
    }

    /// Our current depth in the tree
    pub fn depth(&self) -> usize {
        self.parents.len
    }

    /// The start position of the current token
    pub fn pos(&self) -> usize {
        self.pos
    }

    pub fn next_token(&mut self) -> Option<&'a Token> {
        let cur_len = self.current.current_token_len();
        match self.current.advance() {
            Some(NodeOrToken::Token(token)) => {
                self.pos += cur_len;
                Some(token)
            }
            Some(NodeOrToken::Node(node)) => {
                self.descend(node);
                self.next_token()
            }
            None => {
                assert!(self.current.is_done());
                while self.current.is_done() {
                    match self.parents.pop() {
                        Some(parent) => self.current = parent,
                        // this must be the root node, and it must be finished
                        None => return None,
                    }
                }
                self.next_token()
            }
        }
    }

    // move down into a child node.
    //
    // invariant: `node` is a child of `self.current`
    fn descend(&mut self, node: &'a Node) {
        // move the current node onto the parent set
        let new_current = NodeRef { node, idx: 0 };
        let prev = std::mem::replace(&mut self.current, new_current);
        self.parents.push(prev);
    }
}

/// Store a stack of objects, only allocating if depth 'N' is exceeded.
struct ParentStack<T, const N: usize> {
    parents: [MaybeUninit<T>; N],
    len: usize,
    fallback: Vec<T>,
}

impl<T, const N: usize> ParentStack<T, N> {
    const INIT: MaybeUninit<T> = MaybeUninit::uninit();
    fn new() -> Self {
        Self {
            parents: [Self::INIT; N],
            len: 0,
            fallback: Vec::new(),
        }
    }

    fn push(&mut self, item: T) {
        if self.len < N {
            // items beyond len are always uninit
            unsafe { self.parents[self.len].as_mut_ptr().write(item) };
        } else {
            self.fallback.push(item);
        }
        self.len += 1;
    }

    fn pop(&mut self) -> Option<T> {
        let parent = match self.len {
            0 => None,
            i if i <= N => {
                let parent = std::mem::replace(&mut self.parents[i - 1], MaybeUninit::uninit());
                // items below `len` must have been written
                Some(unsafe { parent.assume_init() })
            }
            _ => self.fallback.pop(),
        };
        self.len = self.len.saturating_sub(1);
        parent
    }
}

impl<'a> NodeRef<'a> {
    fn current_token_len(&self) -> usize {
        self.node
            .children
            .get(self.idx)
            .map(NodeOrToken::text_len)
            .unwrap_or(0)
    }

    fn advance(&mut self) -> Option<&'a NodeOrToken> {
        let idx = self.idx;
        self.idx += 1;
        self.node.children.get(idx)
    }

    fn is_done(&self) -> bool {
        self.idx >= self.node.children.len()
    }
}

impl TreeSink for AstSink<'_> {
    fn token(&mut self, kind: Kind, len: usize) {
        let token_text = &self.text[self.text_pos..self.text_pos + len];
        self.builder.token(kind, token_text);
        self.text_pos += len;
    }

    fn start_node(&mut self, kind: Kind) {
        self.builder.start_node(kind);
    }

    fn finish_node(&mut self) {
        self.builder.finish_node();
    }

    fn error(&mut self, error: SyntaxError) {
        self.errors.push(error)
    }
}

impl<'a> AstSink<'a> {
    pub fn new(text: &'a str) -> Self {
        AstSink {
            text,
            text_pos: 0,
            builder: TreeBuilder::default(),
            errors: Vec::new(),
        }
    }

    pub fn finish(self) -> (Node, Vec<SyntaxError>) {
        let node = self.builder.finish();
        (node, self.errors)
    }
}

impl Node {
    fn new(kind: Kind, children: Vec<NodeOrToken>) -> Self {
        let text_len = children
            .iter()
            .map(|child| match child {
                NodeOrToken::Token(t) => t.text.len(),
                NodeOrToken::Node(node) => node.text_len,
            })
            .sum();

        Node {
            kind,
            text_len,
            children: children.into(),
        }
    }

    pub fn cursor(&self) -> Cursor {
        Cursor::new(self)
    }

    pub fn iter_tokens(&self) -> impl Iterator<Item = &Token> {
        let mut cursor = Cursor::new(self);
        std::iter::from_fn(move || cursor.next_token())
    }

    pub fn kind(&self) -> Kind {
        self.kind
    }

    pub fn text_len(&self) -> usize {
        self.text_len
    }

    pub fn children(&self) -> impl Iterator<Item = &NodeOrToken> {
        self.children.iter()
    }
}

impl TreeBuilder {
    fn start_node(&mut self, kind: Kind) {
        let len = self.children.len();
        self.parents.push((kind, len));
    }

    fn token(&mut self, kind: Kind, text: &str) {
        let token = Token {
            kind,
            text: SmolStr::from(text),
        };

        self.children.push(NodeOrToken::Token(token));
    }

    fn finish_node(&mut self) {
        let (kind, first_child) = self.parents.pop().unwrap();
        let node = Node::new(kind, self.children.split_off(first_child));
        //self.children.truncate(first_child);
        self.children.push(NodeOrToken::Node(node));
    }

    fn finish(mut self) -> Node {
        assert_eq!(self.children.len(), 1);
        self.children.pop().unwrap().into_node().unwrap()
    }
}

impl NodeOrToken {
    pub fn text_len(&self) -> usize {
        match self {
            NodeOrToken::Node(n) => n.text_len,
            NodeOrToken::Token(t) => t.text.len(),
        }
    }

    pub fn into_node(self) -> Option<Node> {
        match self {
            NodeOrToken::Node(node) => Some(node),
            NodeOrToken::Token(_) => None,
        }
    }

    pub fn into_token(self) -> Option<Token> {
        match self {
            NodeOrToken::Node(_) => None,
            NodeOrToken::Token(token) => Some(token),
        }
    }

    pub fn as_node(&self) -> Option<&Node> {
        match self {
            NodeOrToken::Node(node) => Some(node),
            NodeOrToken::Token(_) => None,
        }
    }

    pub fn as_token(&self) -> Option<&Token> {
        match self {
            NodeOrToken::Node(_) => None,
            NodeOrToken::Token(token) => Some(token),
        }
    }
}

impl Token {
    pub fn as_str(&self) -> &str {
        &self.text
    }
}

#[cfg(test)]
mod tests {
    use crate::Parser;

    use super::*;

    #[test]
    fn token_iter() {
        let fea = include_str!("../test-data/mini.fea");
        let mut sink = AstSink::new(fea);
        let mut parser = Parser::new(fea, &mut sink);
        crate::root(&mut parser);
        let (root, _errs) = sink.finish();
        let reconstruct = root.iter_tokens().map(Token::as_str).collect::<String>();

        crate::assert_eq_str!(fea, reconstruct);
    }
}
