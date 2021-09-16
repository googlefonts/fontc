//! After parsing, we do validation.
//!
//! This ensures that all nodes are well formed, and updates the tree in place.
//!
//! - verify & disambiguate glyph names (a name containing a hyphen might be
//!   either a development glyph name or a glyph range?)
//! - properly classify gsub/gpos rules
//!

use std::ops::Range;

use crate::{ast::TreeBuilder, types::GlyphMap, Kind, Node, NodeOrToken, SyntaxError, TokenSet};

#[derive(Clone, Debug)]
struct ValidationContext<'a> {
    glyph_map: &'a GlyphMap,
    errors: Vec<SyntaxError>,
    rel_pos: usize,
}

impl<'a> ValidationContext<'a> {
    fn new(glyph_map: &'a GlyphMap) -> Self {
        Self {
            glyph_map,
            errors: Vec::new(),
            rel_pos: 0,
        }
    }

    fn error(&mut self, range: Range<usize>, message: String) {
        let range = range.start + self.rel_pos..range.end + self.rel_pos;
        self.errors.push(SyntaxError { range, message })
    }

    // for a node that may contain a GlyphClass child node: we want to verify
    // all of the children.
    fn validate_contained_glyph_class(&mut self, node: &mut Node) {
        self.rel_pos += node.rel_pos();
        for child in node.children_mut() {
            if let NodeOrToken::Node(child) = child {
                if child.kind() == Kind::GlyphClass {
                    self.validate_glyph_class(child);
                }
            }
        }
        self.rel_pos -= node.rel_pos()
    }

    fn validate_glyph_class(&mut self, node: &mut Node) {
        const IGNORE: TokenSet = TokenSet::new(&[
            Kind::Whitespace,
            Kind::Comment,
            Kind::Comma,
            Kind::Backslash,
            Kind::LSquare,
            Kind::RSquare,
        ]);
        assert_eq!(node.kind, Kind::GlyphClass);

        self.rel_pos += node.rel_pos();
        for child in node.children_mut().filter(|n| !n.matches(IGNORE)) {
            match child {
                NodeOrToken::Token(t) if t.kind == Kind::Cid => (),
                NodeOrToken::Token(t) if t.kind == Kind::GlyphName => {
                    match validate_glyph_name(t.as_str().as_bytes()) {
                        NameType::Valid => (),
                        NameType::Invalid(idx) => {
                            let chr = t.as_str()[idx..].chars().next().unwrap();
                            self.error(t.range(), format!("Invalid char '{}' in glyph name", chr));
                        }
                        NameType::MaybeRange if self.glyph_map.contains(t.as_str()) => (),
                        NameType::MaybeRange => {
                            match try_split_range(t.as_str(), &self.glyph_map) {
                                Ok(node) => child.replace(node),
                                Err(msg) => {
                                    self.error(t.range(), format!("Ambiguous glyph range: {}", msg))
                                }
                            }
                        }
                    }
                }
                // this is a name that contains a hyphen. Is it a range?
                NodeOrToken::Node(node) if node.kind == Kind::GlyphRange => {}
                other => panic!("unexpected type in glyph class: {:?}", other),
            }
        }
        self.rel_pos -= node.rel_pos();
    }
}

pub(crate) enum NameType {
    Valid,
    MaybeRange,
    Invalid(usize),
}

/// try to split a glyph containing hyphens into a glyph range.
fn try_split_range(text: &str, glyph_map: &GlyphMap) -> Result<Node, String> {
    let mut solution = None;

    // we try all possible split points
    for idx in text
        .bytes()
        .enumerate()
        .filter_map(|(idx, b)| (b == b'-').then(|| idx))
    {
        let (head, tail) = text.split_at(idx);
        if glyph_map.contains(head) && glyph_map.contains(tail.trim_start_matches('-')) {
            if let Some(prev_idx) = solution.replace(idx) {
                let (head1, tail1) = text.split_at(prev_idx);
                let (head2, tail2) = text.split_at(idx);
                let message = format!("the name '{}' contains multiple possible glyph ranges ({} to {} and {} to {}). Please insert spaces around the '-' to clarify your intent.", text, head1, tail1.trim_end_matches('-'), head2, tail2.trim_end_matches('-'));
                return Err(message);
            }
        }
    }

    // if we have a solution, generate a new node
    solution
        .map(|idx| {
            let mut builder = TreeBuilder::default();
            builder.start_node(Kind::GlyphRange);
            let (head, tail) = text.split_at(idx);
            builder.token(Kind::GlyphName, head);
            builder.token(Kind::Hyphen, "-");
            builder.token(Kind::GlyphName, tail.trim_start_matches('-'));
            builder.finish_node(false);
            builder.finish()
        })
        .ok_or_else(|| {
            format!(
                "'{}' is neither a known glyph or a range of known glyphs",
                text
            )
        })
}

pub(crate) fn validate_glyph_name(name: &[u8]) -> NameType {
    fn validate_glyph_body(bytes: &[u8]) -> NameType {
        let mut range = false;
        for (idx, byte) in bytes.iter().enumerate() {
            match byte {
                b'a'..=b'z'
                | b'A'..=b'Z'
                | b'0'..=b'9'
                | b'.'
                | b'_'
                | b'*'
                | b'+'
                | b':'
                | b'^'
                | b'|'
                | b'~' => (),
                b'-' => range = true,
                _ => return NameType::Invalid(idx + 1),
            }
        }
        if range {
            NameType::MaybeRange
        } else {
            NameType::Valid
        }
    }

    let (first, rest) = name.split_first().expect("glyph names are not empty");
    match first {
        b'_' | b'a'..=b'z' | b'A'..=b'Z' => validate_glyph_body(rest),
        b'.' if name == b".notdef" => NameType::Valid,
        _ => NameType::Invalid(0),
    }
}

pub fn validate(root: &mut Node, glyph_map: &GlyphMap) -> Vec<SyntaxError> {
    let mut ctx = ValidationContext::new(glyph_map);

    assert_eq!(root.kind, Kind::SourceFile);
    for item in root.children_mut() {
        match item {
            NodeOrToken::Node(n) => match n.kind {
                Kind::LanguageSystemNode => (),
                Kind::GlyphClassDefNode => ctx.validate_contained_glyph_class(n),
                Kind::MarkClassNode => ctx.validate_contained_glyph_class(n),
                Kind::AnchorDefNode => (),
                Kind::ValueRecordNode => (),
                Kind::IncludeNode => (),
                Kind::FeatureNode => (),
                Kind::TableNode => (),
                Kind::AnonBlockNode => (),
                Kind::LookupBlockNode => (),
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
    ctx.errors
}
