use std::sync::Arc;

use smol_str::SmolStr;

use crate::{
    parse::{SyntaxError, TreeSink},
    Kind,
};

use self::cursor::Cursor;

mod cursor;
mod stack;

#[derive(PartialEq, Eq, Clone, Copy)]
struct SyntaxKind(u16);

#[derive(Debug, PartialEq, Eq, Clone, PartialOrd, Ord, Hash)]
pub struct Node {
    pub kind: Kind,
    // start of this node relative to start of parent node.
    // we can use this to more efficiently move to a given offset
    // TODO: remove if unused
    rel_pos: usize,
    pub text_len: usize,
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
    fn new(kind: Kind, mut children: Vec<NodeOrToken>) -> Self {
        let mut text_len = 0;
        for child in &mut children {
            if let NodeOrToken::Node(n) = child {
                n.rel_pos += text_len;
            }
            text_len += child.text_len();
        }

        Node {
            kind,
            text_len,
            rel_pos: 0,
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

    fn token(&mut self, kind: Kind, text: impl Into<SmolStr>) {
        let token = Token {
            kind,
            text: text.into(),
        };

        self.children.push(NodeOrToken::Token(token));
    }

    fn finish_node(&mut self) {
        let (kind, first_child) = self.parents.pop().unwrap();
        let node = Node::new(kind, self.children.split_off(first_child));
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
