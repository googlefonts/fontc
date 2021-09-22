use std::{
    collections::HashMap,
    ops::{Deref, Range},
    rc::Rc,
    str::FromStr,
};

use smol_str::SmolStr;

use crate::{
    ast::Token,
    types::{GlyphId, GlyphIdent, InvalidTag, LanguageSystem, Tag},
    Kind, Node, NodeOrToken, SyntaxError, TokenSet,
};

#[derive(Clone, Debug, Default)]
struct ValidationContext<'a> {
    glyph_map: HashMap<GlyphIdent, GlyphId>,
    token_buf: Vec<&'a Token>,
    errors: Vec<SyntaxError>,
    seen_non_default_script: bool,
    language_systems: Vec<LanguageSystem>,
    glyph_class_defs: HashMap<SmolStr, Rc<[GlyphId]>>,
}

#[derive(Clone, Debug)]
struct ValidationError {
    range: Range<usize>,
    kind: ErrKind,
}

#[derive(Clone, Debug)]
enum ErrKind {
    UnexpectedToken { expected: Kind, found: Token },
    UnexpectedNode { expected: Kind, found: Kind },
    MissingToken { expected: Kind },
    InvalidTag(InvalidTag),
}

impl ValidationContext<'_> {
    fn error(&mut self, range: Range<usize>, message: String) {
        self.errors.push(SyntaxError { range, message })
    }
}

pub fn validate(root: &mut Node) -> Result<(), Vec<SyntaxError>> {
    let mut ctx = ValidationContext::default();

    assert_eq!(root.kind, Kind::SourceFile);
    for item in root.children_mut() {
        match item {
            NodeOrToken::Node(n) => match n.kind {
                Kind::LanguageSystemNode => ctx.add_language_system(n),
                Kind::GlyphClassDefNode => ctx.add_glyph_class_def(n),
                Kind::MarkClassNode => mark_class_def(n, &mut ctx),
                Kind::AnchorDefNode => anchor_def(n, &mut ctx),
                Kind::ValueRecordNode => value_record_def(n, &mut ctx),
                Kind::IncludeNode => include(n, &mut ctx),
                Kind::FeatureNode => feature(n, &mut ctx),
                Kind::TableNode => table(n, &mut ctx),
                Kind::AnonBlockNode => anon(n, &mut ctx),
                Kind::LookupBlockNode => lookup_block_top_level(n, &mut ctx),
                other => ctx.error(
                    n.rel_pos()..n.rel_pos() + n.text_len(),
                    format!("Invalid root node '{}'", other),
                ),
            },
            NodeOrToken::Token(t)
                if matches!(t.kind, Kind::Comment | Kind::Whitespace | Kind::Semi) =>
            {
                ()
            }
            NodeOrToken::Token(t) => panic!("unexpected top-level token {} ('{}')", t.kind, t.text),
        }
    }
    Ok(())
}

const TO_SKIP: TokenSet = TokenSet::new(&[Kind::Whitespace, Kind::Comment, Kind::Semi]);

/// works like try!, except that it reports any errors to the context before
/// bailing.
#[macro_export]
macro_rules! report_errs {
    ($val:expr) => {
        match $val {
            Ok(v) => v,
            Err(_) => return Err(()),
        }
    };
    ($ctx:expr, $val:expr $(,)?) => {
        match &$val {
            Ok(_) => (),
            Err(err) => $ctx.error(err.range.clone(), err.kind.to_string()),
        }
    };
    ($ctx:expr, $($val:expr),+ $(,)?) => {
        {
            $($crate::report_errs!($ctx, $val);)+
            ($($crate::report_errs!($val)),+,)
        }
    };
}

impl<'a> ValidationContext<'a> {
    fn add_language_system(&mut self, node: &mut Node) {
        assert_eq!(node.kind, Kind::LanguageSystemNode);
        assert!(!node.error);
        let mut scanner = Scanner::new(node, TO_SKIP);
        let _keyword = scanner.next();
        debug_assert!(_keyword.unwrap().kind() == Kind::LanguagesystemKw);

        // if this is an error free LanguageSystemNode then these have already
        // been validated
        let script_token = scanner.expect_next_token(Kind::Tag).unwrap();
        let lang_token = scanner.expect_next_token(Kind::Tag).unwrap();
        let script = Tag::from_str(script_token.as_str()).unwrap();
        let language = Tag::from_str(lang_token.as_str()).unwrap();

        if script == Tag::DFLT_SCRIPT {
            if self.seen_non_default_script {
                self.error(
                    script_token.range(),
                    "DFLT script must come before any other script tags.".into(),
                );
            } else {
                self.seen_non_default_script = true;
            }
        }
        self.language_systems
            .push(LanguageSystem { script, language })
    }

    fn add_glyph_class_def(&mut self, node: &'a mut Node) {
        let mut scanner = Scanner::new(node, TO_SKIP);
        let name = scanner.expect_next_token(Kind::NamedGlyphClass).unwrap();
        let assign_types = TokenSet::new(&[Kind::GlyphClass, Kind::NamedGlyphClass]);
        let item = scanner.next_match(assign_types).unwrap();
        match item {
            NodeOrToken::Node(n) if n.kind == Kind::GlyphClass => {
                // resolve this class
            }
            &NodeOrToken::Token(t) if t.kind == Kind::NamedGlyphClass => {
                match self.glyph_class_defs.get(t.as_str()).cloned() {
                    Some(existing) => {
                        self.glyph_class_defs.insert(name.text.clone(), existing);
                    }
                    None => self.error(
                        t.range(),
                        format!("Named glyph class '{}' has not been defined", t.as_str()),
                    ),
                }
            }
        }
    }

    fn resolve_glyph_class(&mut self, node: &'a mut Node) -> Result<Rc<[GlyphId]>, ()> {
        const GLYPH_CLASS_TYPES: TokenSet = TokenSet::new()
        assert_eq!(node.kind, Kind::GlyphClass);

    }
}

fn glyph_class_def(node: &mut Node, ctx: &mut ValidationContext) {}

fn mark_class_def(node: &mut Node, ctx: &mut ValidationContext) {}
fn anchor_def(node: &mut Node, ctx: &mut ValidationContext) {}
fn value_record_def(node: &mut Node, ctx: &mut ValidationContext) {}
fn include(node: &mut Node, ctx: &mut ValidationContext) {}
fn feature(node: &mut Node, ctx: &mut ValidationContext) {}
fn table(node: &mut Node, ctx: &mut ValidationContext) {}
fn anon(node: &mut Node, ctx: &mut ValidationContext) {}
fn lookup_block_top_level(node: &mut Node, ctx: &mut ValidationContext) {}

struct Scanner<'a> {
    items: &'a [NodeOrToken],
    to_skip: TokenSet,
    idx: usize,
    pos: usize,
}

impl<'a> Scanner<'a> {
    fn new(node: &'a Node, to_skip: TokenSet) -> Self {
        Scanner {
            items: &node.children,
            to_skip,
            idx: 0,
            pos: node.rel_pos(),
        }
    }

    fn pos(&self) -> usize {
        self.pos
    }

    fn current_text_len(&self) -> usize {
        self.items
            .get(self.idx)
            .map(NodeOrToken::text_len)
            .unwrap_or(0)
    }

    fn next(&mut self) -> Option<&'a NodeOrToken> {
        loop {
            let idx = self.idx;
            self.idx += 1;
            let item = self.items.get(idx)?;
            if idx > 0 {
                self.pos += self
                    .items
                    .get(idx - 1)
                    .map(NodeOrToken::text_len)
                    .unwrap_or(0);
            }
            if !self.to_skip.contains(item.kind()) {
                return Some(item);
            }
        }
    }

    fn next_match(&mut self, kind: TokenSet) -> Option<&'a NodeOrToken> {
        loop {
            let next = self.next()?;
            if next.matches(kind) {
                return Some(next);
            }
        }
    }

    fn expect_next_token(&mut self, kind: Kind) -> Result<&'a Token, ValidationError> {
        let next = self.next();
        let range = self.pos..self.pos + self.current_text_len();
        let err_kind = match next {
            Some(NodeOrToken::Token(t)) if t.kind == kind => return Ok(t),
            Some(NodeOrToken::Token(t)) => ErrKind::UnexpectedToken {
                expected: kind,
                found: t.clone(),
            },
            Some(NodeOrToken::Node(n)) => ErrKind::UnexpectedNode {
                expected: kind,
                found: n.kind,
            },
            None => ErrKind::MissingToken { expected: kind },
        };
        Err(ValidationError {
            range,
            kind: err_kind,
        })
    }

    fn expect_next_node(&mut self, kind: Kind) -> Option<&Node> {
        self.next()
            .and_then(NodeOrToken::as_node)
            .filter(|node| node.kind == kind)
    }
}

impl std::fmt::Display for ErrKind {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::UnexpectedToken { expected, found } => {
                write!(f, "Expected '{}', found token '{}'", expected, found.text)
            }
            Self::UnexpectedNode { expected, found } => {
                write!(f, "Expected '{}', found node type '{}'", expected, found)
            }
            Self::MissingToken { expected } => write!(f, "Missing expected '{}' token", expected),
            Self::InvalidTag(e) => std::fmt::Display::fmt(e, f),
        }
    }
}

impl std::error::Error for ErrKind {}

impl From<InvalidTag> for ErrKind {
    fn from(src: InvalidTag) -> ErrKind {
        ErrKind::InvalidTag(src)
    }
}
