use std::sync::Arc;
//use std::collections::HashMap;

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

//impl<N: fmt::Display, T: fmt::Display> fmt::Display for NodeOrToken<N, T> {
//fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
//match self {
//NodeOrToken::Node(node) => fmt::Display::fmt(node, f),
//NodeOrToken::Token(token) => fmt::Display::fmt(token, f),
//}
//}
//}
