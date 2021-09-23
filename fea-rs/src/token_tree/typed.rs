//! typing for ast nodes. based on rust-analyzer.

use std::ops::Range;

use smol_str::SmolStr;

use crate::{types::InvalidTag, Kind, Node, NodeOrToken};

use super::Token;

pub trait AstNode {
    fn cast(node: &NodeOrToken) -> Option<Self>
    where
        Self: Sized;

    fn range(&self) -> Range<usize>;
}

macro_rules! ast_token {
    ($typ:ident, $kind:expr) => {
        pub struct $typ {
            inner: Token,
        }

        impl $typ {
            #[allow(unused)]
            pub fn text(&self) -> &SmolStr {
                &self.inner.text
            }
        }

        impl AstNode for $typ {
            fn cast(node: &NodeOrToken) -> Option<Self> {
                if let NodeOrToken::Token(t) = node {
                    if t.kind == $kind {
                        return Some(Self { inner: t.clone() });
                    }
                }
                None
            }

            fn range(&self) -> std::ops::Range<usize> {
                self.inner.range()
            }
        }
    };
}

macro_rules! ast_node {
    ($typ:ident, $kind:expr) => {
        pub struct $typ {
            inner: Node,
        }

        impl $typ {
            #[allow(unused)]
            pub fn iter(&self) -> impl Iterator<Item = &NodeOrToken> {
                self.inner.iter_children()
            }
        }

        impl AstNode for $typ {
            fn cast(node: &NodeOrToken) -> Option<Self> {
                if let NodeOrToken::Node(inner) = node {
                    if inner.kind == $kind {
                        return Some(Self {
                            inner: inner.clone(),
                        });
                    }
                }
                None
            }

            fn range(&self) -> std::ops::Range<usize> {
                self.inner.range()
            }
        }
    };
}

ast_token!(Cid, Kind::Cid);
ast_token!(GlyphName, Kind::GlyphName);
ast_token!(Tag, Kind::Tag);
ast_token!(GlyphClassName, Kind::NamedGlyphClass);
ast_node!(GlyphRange, Kind::GlyphRange);
ast_node!(GlyphClassDef, Kind::GlyphClassDefNode);
ast_node!(GlyphClass, Kind::GlyphClass);
ast_node!(LanguageSystem, Kind::LanguageSystemNode);

impl LanguageSystem {
    pub fn script(&self) -> Tag {
        self.inner.iter_children().find_map(Tag::cast).unwrap()
    }

    pub fn language(&self) -> Tag {
        self.inner
            .iter_children()
            .skip_while(|t| t.kind() != Kind::Tag)
            .skip(1)
            .find_map(Tag::cast)
            .unwrap()
    }
}

impl Tag {
    pub fn parse(&self) -> Result<crate::types::Tag, InvalidTag> {
        self.inner.text.parse()
    }
}

impl GlyphClassDef {
    pub fn class_name(&self) -> GlyphClassName {
        self.inner
            .iter_children()
            .find_map(GlyphClassName::cast)
            .unwrap()
    }

    pub fn class_alias(&self) -> Option<GlyphClassName> {
        //TODO: ensure this returns non in presence of named glyph class inside class block
        self.iter()
            .skip_while(|t| t.kind() != Kind::Eq)
            .find_map(GlyphClassName::cast)
    }

    pub fn class_def(&self) -> Option<GlyphClass> {
        self.inner.iter_children().find_map(GlyphClass::cast)
    }
}

impl Cid {
    pub fn parse(&self) -> u32 {
        self.inner.text.parse().expect("cid is already validated")
    }
}

impl GlyphRange {
    pub fn start(&self) -> &Token {
        self.iter()
            .find(|i| i.kind() == Kind::Cid || i.kind() == Kind::GlyphName)
            .and_then(NodeOrToken::as_token)
            .unwrap()
    }

    pub fn end(&self) -> &Token {
        self.iter()
            .skip_while(|t| t.kind() != Kind::Hyphen)
            .find(|i| i.kind() == Kind::Cid || i.kind() == Kind::GlyphName)
            .and_then(NodeOrToken::as_token)
            .unwrap()
    }
}
