use std::{fmt::Write, io::Write as _};

use std::{ops::Range, sync::Arc};

use smol_str::SmolStr;

use crate::parse::{FileId, IncludeStatement};
use crate::{diagnostic::Diagnostic, GlyphMap, Level};

use self::cursor::Cursor;
use typed::AstNode as _;

mod cursor;
mod edit;
mod rewrite;
mod stack;
mod token;
pub mod typed;

use rewrite::ReparseCtx;
pub use token::Kind;

/// A node in the token tree.
///
/// A node is tagged with a `Kind`, and includes any number of child nodes or tokens.
#[derive(PartialEq, Eq, Clone, PartialOrd, Ord)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Node {
    /// The ``Kind` of this node.
    kind: Kind,

    // NOTE: the absolute position within the tree is not known when the node
    // is created; this is updated (and correct) only when the node has been
    // accessed via a `Cursor`.
    abs_pos: u32,
    /// The length of the text spanned by this node
    text_len: u32,
    /// true if an error was encountered in this node.
    ///
    /// This is not recursive; it is only true for the direct parent of an error span.
    pub error: bool,
    //NOTE: children should not be accessed directly, but only via a cursor.
    // this ensures that their positions are updated correctly.
    children: Arc<Vec<NodeOrToken>>,
}

/// A token is a chunk of text, tagged with a `Kind`.
#[derive(Debug, PartialEq, Eq, Clone, PartialOrd, Ord)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub struct Token {
    /// The [`Kind`] of this token
    pub kind: Kind,
    /// The absolute position in the source where this token starts
    abs_pos: u32,
    /// The token text
    pub text: SmolStr,
}

/// Either a [`Node`] or a [`Token`].
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
#[cfg_attr(feature = "serde", derive(serde::Serialize, serde::Deserialize))]
pub enum NodeOrToken {
    /// A node
    Node(Node),
    /// A token
    Token(Token),
}

#[derive(Clone, Debug, Default)]
pub(crate) struct TreeBuilder {
    //TODO: reuse tokens
    //token_cache: HashMap<Arc<Token>>,
    // the kind of the parent, and the index in children of the first child.
    parents: Vec<(Kind, usize)>,
    children: Vec<NodeOrToken>,
}

/// Consumes tokens during parsing, building up an AST.
pub(crate) struct AstSink<'a> {
    text: &'a str,
    file_id: FileId,
    text_pos: usize,
    builder: TreeBuilder,
    // reuseable buffer for reparsing
    reparse_buf: Vec<NodeOrToken>,
    glyph_map: Option<&'a GlyphMap>,
    errors: Vec<Diagnostic>,
    include_statement_count: usize,
    cur_node_contains_error: bool,
}

//NOTE: the inner type is option because we reuse this in the `typed` module,
//and there we want to be able to easily return an empty iterator when iter() is
//called on a token.
//
/// An iterator over the children of a node.
#[derive(Default)]
pub struct ChildIter<'a>(Option<Cursor<'a>>);

impl<'a> AstSink<'a> {
    pub fn new(text: &'a str, file_id: FileId, glyph_map: Option<&'a GlyphMap>) -> Self {
        AstSink {
            file_id,
            text,
            text_pos: 0,
            builder: TreeBuilder::default(),
            glyph_map,
            errors: Vec::new(),
            cur_node_contains_error: false,
            include_statement_count: 0,
            reparse_buf: Default::default(),
        }
    }

    pub(crate) fn token(&mut self, kind: Kind, len: usize) {
        let token_text = &self.text[self.text_pos..self.text_pos + len];
        let to_add = self.validate_token(kind, token_text);
        self.builder.push_raw(to_add);
        self.text_pos += len;
    }

    pub(crate) fn start_node(&mut self, kind: Kind) {
        self.builder.start_node(kind);
    }

    pub(crate) fn finish_node(&mut self, kind: Option<Kind>) {
        let cur_kind = kind
            .or_else(|| self.builder.parents.last().map(|x| x.0))
            .unwrap();
        let kind = self.maybe_rewrite_current_node(cur_kind).or(kind);
        self.builder.finish_node(self.cur_node_contains_error, kind);
        self.cur_node_contains_error = false;
        // if this is an include statement we store a copy.
        if self.builder.children.last().map(|n| n.kind()) == Some(Kind::IncludeNode) {
            self.include_statement_count += 1;
        }
    }

    pub(crate) fn current_node_has_error(&self) -> bool {
        self.cur_node_contains_error
    }

    pub(crate) fn error(&mut self, mut error: Diagnostic) {
        let is_hard_error = error.level == Level::Error;
        error.message.file = self.file_id;
        self.errors.push(error);
        self.cur_node_contains_error = is_hard_error;
    }

    pub fn finish(self) -> (Node, Vec<Diagnostic>, Vec<IncludeStatement>) {
        let mut node = self.builder.finish();
        node.update_positions_from_root();
        let mut includes = Vec::new();
        if self.include_statement_count > 0 {
            node.find_include_nodes(&mut includes, self.include_statement_count);
        }
        (node, self.errors, includes)
    }

    #[cfg(test)]
    pub fn errors(&self) -> &[Diagnostic] {
        &self.errors
    }

    /// called before adding a token.
    ///
    /// We can perform additional validation here. Currently it is mostly for
    /// disambiguating glyph names that might be ranges.
    fn validate_token(&mut self, kind: Kind, text: &str) -> NodeOrToken {
        if kind == Kind::GlyphNameOrRange {
            if let Some(map) = self.glyph_map {
                if map.contains(text) {
                    return Token::new(Kind::GlyphName, text.into()).into();
                }
                match try_split_range(text, map) {
                    Ok(node) => return node.into(),
                    Err(message) => {
                        let range = self.text_pos..self.text_pos + text.len();
                        self.error(Diagnostic::error(FileId::CURRENT_FILE, range, message));
                    }
                }
            }
        }
        Token::new(kind, text.into()).into()
    }

    /// Called before finishing a node.
    ///
    /// This is an opportunity for us to rewrite this node's tree, which is
    /// something that we do for rules with contextual glyphs.
    ///
    /// This lets us provide much better information about the rule to the
    /// rest of the compilation pipeline.
    ///
    /// The reason that we have to do this after the first pass is because
    /// determining whether or not a rule is contextual requires arbitrary
    /// lookahead at the parser level. Instead of writing an arbitrary lookahead
    /// parser, we instead rescan the children after parsing, grouping them
    /// into things like backtrack/context/lookahead sequences.
    fn maybe_rewrite_current_node(&mut self, cur_kind: Kind) -> Option<Kind> {
        match cur_kind {
            _ if self.cur_node_contains_error => None,
            Kind::GsubNodeNeedsRewrite => {
                Some(self.rewrite_current_node(rewrite::reparse_contextual_sub_rule))
            }
            Kind::GposNodeNeedsRewrite => {
                Some(self.rewrite_current_node(rewrite::reparse_contextual_pos_rule))
            }
            _ => None,
        }
    }

    fn rewrite_current_node(&mut self, rewrite_fn: impl FnOnce(&mut ReparseCtx) -> Kind) -> Kind {
        assert!(self.reparse_buf.is_empty());
        self.builder.move_current_children(&mut self.reparse_buf);
        // temporarily take the buffer to satisfy borrowck
        let mut buf = std::mem::take(&mut self.reparse_buf);
        let items_start_offset: usize = buf.iter().map(NodeOrToken::text_len).sum();

        let mut reparse_ctx = ReparseCtx {
            in_buf: &mut buf,
            text_pos: self.text_pos - items_start_offset,
            sink: self,
        };
        let new_kind = rewrite_fn(&mut reparse_ctx);
        // put back the buffer so we can reuse the storage next time
        assert!(
            reparse_ctx.in_buf.is_empty(),
            "rewrite finished with unhandled items"
        );
        buf.clear();
        std::mem::swap(&mut self.reparse_buf, &mut buf);
        new_kind
    }

    fn push_raw(&mut self, n: NodeOrToken) {
        self.builder.push_raw(n);
    }
}

impl Node {
    fn new(kind: Kind, children: Vec<NodeOrToken>, error: bool) -> Self {
        let text_len = children.iter().map(|x| x.text_len() as u32).sum();
        Node {
            kind,
            text_len,
            abs_pos: 0,
            children: children.into(),
            error,
        }
    }

    /// recursively compute and update the positions of each child.
    ///
    /// This should only be called on a root node; it assumes the position
    /// of the callee is `0`.
    ///
    /// This is required in order for us to correctly associate diagnostics
    /// with their locations in the source.
    pub(crate) fn update_positions_from_root(&mut self) {
        self.update_positions_recurse(0)
    }

    fn update_positions_recurse(&mut self, mut pos: usize) {
        self.abs_pos = pos as _;
        let Some(children) = Arc::get_mut(&mut self.children) else {
            panic!("update_positions should only be called on a newly created node");
        };

        for child in children {
            child.update_positions(pos);
            pos += child.text_len();
        }
    }

    /// Construct a new cursor for navigating the node's children
    pub(crate) fn cursor(&self) -> Cursor {
        Cursor::new(self)
    }

    /// Iterate over tokens, descending into child nodes.
    pub fn iter_tokens(&self) -> impl Iterator<Item = &Token> {
        let mut cursor = self.cursor();
        std::iter::from_fn(move || cursor.next_token())
    }

    /// Iterate over this node's direct children, without descending.
    pub fn iter_children(&self) -> ChildIter {
        ChildIter(Some(self.cursor()))
    }

    /// The `Kind` of the node
    pub fn kind(&self) -> Kind {
        self.kind
    }

    /// The total length of all tokens that are descendents of this node.
    pub fn text_len(&self) -> usize {
        self.text_len as usize
    }

    /// The range in the original source of this node.
    ///
    /// Only correct if this node is accessed via a cursor.
    pub fn range(&self) -> Range<usize> {
        let start = self.abs_pos as usize;
        start..start + (self.text_len as usize)
    }

    /// Create a new tree, replacing the provided ranges with the provided
    /// nodes.
    ///
    /// if skip_parent is true, children of inserted nodes are added directly,
    /// without their parent nodes. (we use this when resolving includes
    ///
    /// range start/end just fall on token boundaries.
    pub(crate) fn edit(&self, edits: Vec<(Range<usize>, Node)>, skip_parent: bool) -> Node {
        edit::apply_edits(self, edits, skip_parent)
    }

    fn find_include_nodes(&self, collect: &mut Vec<IncludeStatement>, num: usize) {
        for item in self.iter_children() {
            if let Some(node) = item.as_node() {
                if let Some(include) = typed::Include::cast(item) {
                    collect.push(IncludeStatement(include));
                    if collect.len() == num {
                        return;
                    }
                } else {
                    node.find_include_nodes(collect, num);
                }
            }
        }
    }

    #[doc(hidden)]
    // used in some tests for debugging
    pub fn debug_print_structure(&self, include_tokens: bool) {
        let mut cursor = self.cursor();
        while let Some(thing) = cursor.current() {
            match thing {
                NodeOrToken::Node(node) => {
                    let depth = cursor.depth();
                    let _ = writeln!(
                        std::io::stderr(),
                        "{}{} ({}..{})",
                        &crate::util::SPACES[..depth * 2],
                        node.kind,
                        cursor.pos(),
                        cursor.pos() + node.text_len()
                    );
                }
                NodeOrToken::Token(t) if include_tokens => eprint!("{}", t.as_str()),
                _ => (),
            }
            cursor.advance();
        }
    }

    #[doc(hidden)]
    pub fn simple_parse_tree(&self) -> String {
        let mut result = String::new();
        self.parse_tree_impl(0, &mut result).unwrap();
        result
    }

    fn parse_tree_impl(&self, depth: usize, buf: &mut String) -> std::fmt::Result {
        use crate::util::SPACES;
        let mut pos = self.abs_pos;
        writeln!(
            buf,
            "{}{}@[{}; {})",
            &SPACES[..depth * 2],
            self.kind,
            pos,
            pos + self.text_len
        )?;
        let depth = depth + 1;
        for child in self.iter_children() {
            match child {
                NodeOrToken::Token(Token { kind, text, .. }) => {
                    let spaces = &SPACES[..depth * 2];
                    write!(buf, "{}{}@{}", spaces, kind, pos)?;
                    if kind.is_trivia() {
                        writeln!(buf, " \"{}\"", text.escape_debug())?;
                    } else {
                        writeln!(buf, " \"{}\"", text)?;
                    }
                    pos += text.len() as u32;
                }
                NodeOrToken::Node(node) => {
                    node.parse_tree_impl(depth + 1, buf)?;
                    pos += node.text_len;
                }
            }
        }
        Ok(())
    }
}

impl<'a> Iterator for ChildIter<'a> {
    type Item = &'a NodeOrToken;

    fn next(&mut self) -> Option<Self::Item> {
        let current = self.0.as_ref()?.current();
        self.0.as_mut()?.step_over();
        current
    }
}

impl TreeBuilder {
    pub(crate) fn start_node(&mut self, kind: Kind) {
        let len = self.children.len();
        self.parents.push((kind, len));
    }

    pub(crate) fn token(&mut self, kind: Kind, text: impl Into<SmolStr>) {
        let token = Token::new(kind, text.into());
        self.push_raw(token.into());
    }

    fn push_raw(&mut self, item: NodeOrToken) {
        self.children.push(item)
    }

    /// copy the children of the currently open node into a buffer.
    ///
    /// This is only used as part of reparsing.
    fn move_current_children(&mut self, to_buf: &mut Vec<NodeOrToken>) {
        if let Some(idx) = self.parents.last().map(|(_, idx)| idx).copied() {
            to_buf.extend(self.children.drain(idx..));
        }
    }

    pub(crate) fn finish_node(&mut self, error: bool, new_kind: Option<Kind>) {
        let (kind, first_child) = self.parents.pop().unwrap();
        let kind = new_kind.unwrap_or(kind);
        let node = Node::new(kind, self.children.split_off(first_child), error);
        self.push_raw(node.into());
    }

    pub(crate) fn finish(mut self) -> Node {
        assert_eq!(self.children.len(), 1);
        self.children.pop().unwrap().into_node().unwrap()
    }
}

impl NodeOrToken {
    fn update_positions(&mut self, pos: usize) {
        match self {
            NodeOrToken::Token(t) => t.abs_pos = pos as _,
            NodeOrToken::Node(n) => n.update_positions_recurse(pos),
        }
    }

    /// `true` if this is a single token.
    pub fn is_token(&self) -> bool {
        matches!(self, NodeOrToken::Token(_))
    }

    /// If this is a single token, return that token's text
    pub fn token_text(&self) -> Option<&str> {
        self.as_token().map(Token::as_str)
    }

    /// The `Kind` of this node or token
    pub fn kind(&self) -> Kind {
        match self {
            NodeOrToken::Node(n) => n.kind,
            NodeOrToken::Token(t) => t.kind,
        }
    }

    /// `true` If this is a glyph name, a CID, or a glyph class (either inline or named)
    pub fn is_glyph_or_glyph_class(&self) -> bool {
        matches!(
            self.kind(),
            Kind::GlyphName | Kind::Cid | Kind::GlyphClass | Kind::NamedGlyphClass
        )
    }

    /// The range in the source text of this node or token.
    ///
    /// Note: this is only accurate if the token was accessed via a cursor.
    pub fn range(&self) -> Range<usize> {
        match self {
            NodeOrToken::Token(t) => t.range(),
            NodeOrToken::Node(n) => n.range(),
        }
    }

    /// The length of this token or node's text
    pub fn text_len(&self) -> usize {
        match self {
            NodeOrToken::Node(n) => n.text_len as usize,
            NodeOrToken::Token(t) => t.text.len(),
        }
    }

    /// If this is a `Node`, return it
    pub fn into_node(self) -> Option<Node> {
        match self {
            NodeOrToken::Node(node) => Some(node),
            NodeOrToken::Token(_) => None,
        }
    }

    /// If this is a `Node`, return a reference to it
    pub fn as_node(&self) -> Option<&Node> {
        match self {
            NodeOrToken::Node(node) => Some(node),
            NodeOrToken::Token(_) => None,
        }
    }

    /// IF this is a token, return a reference to it.
    pub fn as_token(&self) -> Option<&Token> {
        match self {
            NodeOrToken::Node(_) => None,
            NodeOrToken::Token(token) => Some(token),
        }
    }
}

impl From<Node> for NodeOrToken {
    fn from(src: Node) -> NodeOrToken {
        NodeOrToken::Node(src)
    }
}

impl From<Token> for NodeOrToken {
    fn from(src: Token) -> NodeOrToken {
        NodeOrToken::Token(src)
    }
}

impl Token {
    fn new(kind: Kind, text: SmolStr) -> Self {
        Token {
            kind,
            text,
            abs_pos: 0,
        }
    }

    /// The raw text for this token
    pub fn as_str(&self) -> &str {
        &self.text
    }

    /// The position of this token in its source.
    pub fn range(&self) -> Range<usize> {
        self.abs_pos as usize..self.abs_pos as usize + self.text.len()
    }
}

/// try to split a glyph containing hyphens into a glyph range.
fn try_split_range(text: &str, glyph_map: &GlyphMap) -> Result<Node, String> {
    let mut solution = None;

    // we try all possible split points
    for idx in text
        .bytes()
        .enumerate()
        .filter_map(|(idx, b)| (b == b'-').then_some(idx))
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
            builder.finish_node(false, None);
            builder.finish()
        })
        .ok_or_else(|| {
            format!(
                "'{}' is neither a known glyph or a range of known glyphs",
                text
            )
        })
}

impl Node {
    fn debug_impl(&self, f: &mut std::fmt::Formatter, depth: usize) -> std::fmt::Result {
        use crate::util::SPACES;

        let ws = &SPACES[..depth * 2];
        write!(
            f,
            "\n{ws}{}:  abs {} len {} children {}",
            self.kind,
            self.abs_pos,
            self.text_len,
            self.children.len()
        )?;
        let ws = &SPACES[..(depth + 1) * 2];
        for child in self.iter_children() {
            match child {
                NodeOrToken::Token(t) => write!(f, "\n{}'{}' {}", ws, t.text, t.kind)?,
                NodeOrToken::Node(n) => n.debug_impl(f, depth + 1)?,
            }
        }
        Ok(())
    }
}

impl std::fmt::Debug for Node {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.debug_impl(f, 0)
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    static SAMPLE_FEA: &str = include_str!("../test-data/fonttools-tests/mini.fea");

    #[test]
    fn token_iter() {
        let (root, _errs) = crate::parse::parse_string(SAMPLE_FEA);
        let reconstruct = root.iter_tokens().map(Token::as_str).collect::<String>();
        crate::assert_eq_str!(SAMPLE_FEA, reconstruct);
    }
}
