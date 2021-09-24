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
        #[derive(Clone, Debug)]
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
        #[derive(Clone, Debug)]
        pub struct $typ {
            inner: Node,
        }

        impl $typ {
            #[allow(unused)]
            pub fn iter(&self) -> impl Iterator<Item = &NodeOrToken> {
                self.inner.iter_children()
            }

            //#[allow(unused)]
            //pub fn node(&self) -> &Node {
            //&self.inner
            //}
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
ast_token!(Number, Kind::Number);
ast_token!(Metric, Kind::Metric);
ast_node!(GlyphRange, Kind::GlyphRange);
ast_node!(GlyphClassDef, Kind::GlyphClassDefNode);
ast_node!(MarkClassDef, Kind::MarkClassNode);
ast_node!(Anchor, Kind::AnchorNode);
ast_node!(AnchorDef, Kind::AnchorDefNode);
ast_node!(GlyphClass, Kind::GlyphClass);
ast_node!(LanguageSystem, Kind::LanguageSystemNode);
ast_node!(Include, Kind::IncludeNode);
ast_node!(Feature, Kind::FeatureNode);
ast_node!(Script, Kind::ScriptNode);
ast_node!(Language, Kind::LanguageNode);
ast_node!(LookupFlag, Kind::LookupFlagNode);

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

impl MarkClassDef {
    pub fn glyph_class(&self) -> &NodeOrToken {
        for item in self.iter() {
            if item.kind() == Kind::GlyphName
                || item.kind() == Kind::NamedGlyphClass
                || item.kind() == Kind::GlyphClass
                || item.kind() == Kind::Cid
            {
                return item;
            }
        }
        unreachable!()
    }

    pub fn anchor(&self) -> Anchor {
        self.iter().find_map(Anchor::cast).unwrap()
    }

    pub fn mark_class_name(&self) -> GlyphClassName {
        self.iter()
            .skip_while(|t| t.kind() != Kind::AnchorNode)
            .find_map(GlyphClassName::cast)
            .unwrap()
    }
}

impl AnchorDef {
    pub fn anchor(&self) -> Anchor {
        self.iter().find_map(Anchor::cast).unwrap()
    }

    pub fn name(&self) -> &Token {
        self.iter()
            .find(|t| t.kind() == Kind::Ident)
            .and_then(NodeOrToken::as_token)
            .expect("pre-validated")
    }
}

impl Anchor {
    pub fn coords(&self) -> Option<(Metric, Metric)> {
        let tokens = self.iter();
        let mut first = None;

        for token in tokens {
            if let Some(metric) = Metric::cast(token) {
                if let Some(prev) = first.take() {
                    return Some((prev, metric));
                } else {
                    first = Some(metric);
                }
            }
        }
        None
    }

    pub fn contourpoint(&self) -> Option<Number> {
        self.iter().find_map(Number::cast)
    }

    pub fn null(&self) -> Option<&Token> {
        self.iter()
            .find(|t| t.kind() == Kind::NullKw)
            .and_then(NodeOrToken::as_token)
    }

    pub fn name(&self) -> Option<&Token> {
        self.iter()
            .find(|t| t.kind() == Kind::Ident)
            .and_then(NodeOrToken::as_token)
    }
}

impl Number {
    pub fn parse(&self) -> i32 {
        self.text().parse().expect("already validated")
    }

    pub fn parse_unsigned(&self) -> Option<u32> {
        self.text().parse().ok()
    }
}

impl Metric {
    pub fn parse(&self) -> i32 {
        self.text().parse().expect("already validated")
    }
}

impl Feature {
    pub fn tag(&self) -> Tag {
        self.iter().find_map(Tag::cast).unwrap()
    }
}

impl Script {
    pub fn tag(&self) -> Tag {
        self.iter().find_map(Tag::cast).unwrap()
    }
}

impl Language {
    pub fn tag(&self) -> Tag {
        self.iter().find_map(Tag::cast).unwrap()
    }

    pub fn include_dflt(&self) -> Option<&Token> {
        self.iter()
            .find(|t| t.kind() == Kind::IncludeDfltKw)
            .and_then(NodeOrToken::as_token)
    }

    pub fn exclude_dflt(&self) -> Option<&Token> {
        self.iter()
            .find(|t| t.kind() == Kind::ExcludeDfltKw)
            .and_then(NodeOrToken::as_token)
    }

    pub fn required(&self) -> Option<&Token> {
        self.iter()
            .find(|t| t.kind() == Kind::RequiredKw)
            .and_then(NodeOrToken::as_token)
    }
}

impl LookupFlag {
    pub fn number(&self) -> Option<Number> {
        self.iter().find_map(Number::cast)
    }
}
