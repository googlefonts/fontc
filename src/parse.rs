//! Convert raw tokens into semantic events

use std::ops::Range;

use super::lexer::{Kind as RawKind, Lexer, Token as RawToken};

pub(crate) struct Parser<'a> {
    lexer: Lexer<'a>,
    sink: &'a mut dyn TreeSink,
    text: &'a str,
    pos: usize,
    errors: Vec<SyntaxError>,
    current: RawToken,
    next: RawToken,
}

impl<'a> Parser<'a> {
    pub(crate) fn new(text: &'a str, sink: &'a mut dyn TreeSink) -> Self {
        let mut this = Parser {
            lexer: Lexer::new(text),
            sink,
            text,
            current: RawToken::EMPTY,
            next: RawToken::EMPTY,
            errors: Default::default(),
            pos: Default::default(),
        };

        // preload the first two tokens; this accumulates any errors
        this.advance();
        this.advance();
        this
    }

    fn current_range(&self) -> Range<usize> {
        self.pos..self.pos + self.current.len
    }

    pub(crate) fn start_node(&mut self, kind: Kind) {
        self.sink.start_node(kind);
    }

    pub(crate) fn finish_node(&mut self) {
        self.sink.finish_node();
    }

    pub(crate) fn current_token_text(&self) -> &str {
        &self.text[self.current_range()]
    }

    fn do_bump<const N: usize>(&mut self, kind: Kind) {
        let mut len = 0;
        for _ in 0..N {
            len += self.current.len;
            self.advance();
        }
        self.sink.token(kind, len);
    }

    fn advance(&mut self) {
        self.pos += self.current.len;
        self.current = self.next;
        self.next = self.lexer.next_token();

        // replace sentinal tokens.
        //
        // There are several tokens we receive from the lexer that communicate
        // an error condition. We don't use these directly, but record the error
        // and replace them with a different token type.
        if let Some((replace_kind, error)) = match self.next.kind {
            RawKind::StringUnterminated => Some((
                RawKind::String,
                "Missing trailing `\"` character to terminate string.",
            )),
            RawKind::NumberHexEmpty => Some((
                RawKind::NumberHex,
                "Missing digits after hexidecimal prefix.",
            )),
            _ => None,
        } {
            self.next.kind = replace_kind;
            let next_pos = self.pos + self.current.len;
            let range = next_pos..next_pos + self.next.len;
            self.errors.push(SyntaxError {
                range,
                message: error.into(),
            })
        }
    }

    /// Eat if we're at this raw token. Return `true` if we eat.
    pub(crate) fn eat(&mut self, raw: RawKind) -> bool {
        if self.current.kind == raw {
            self.eat_raw();
            return true;
        }
        false
    }

    /// Eat the next token, regardless of what it is.
    pub(crate) fn eat_raw(&mut self) {
        self.do_bump::<1>(Kind::from_raw_kind(self.current.kind));
    }

    /// Eat the next token, giving it an explicit kind.
    ///
    /// Necessary for handling keywords, which are not known to the lexer.
    pub(crate) fn eat_remap(&mut self, kind: Kind) {
        self.do_bump::<1>(kind);
    }

    /// combine two tokens into one
    pub(crate) fn eat_remap2(&mut self, kind: Kind) {
        self.do_bump::<2>(kind);
    }

    pub(crate) fn at_eof(&self) -> bool {
        self.current.kind == RawKind::Eof
    }

    pub(crate) fn eat_trivia(&mut self) {
        while let RawKind::Whitespace | RawKind::Comment = self.current.kind {
            self.eat_raw()
        }
    }

    pub(crate) fn err_and_bump(&mut self, error: impl Into<String>) {
        self.err(error);
        self.eat_raw();
    }

    /// Error, and advance unless the current token matches a predicate.
    pub(crate) fn err_recover(
        &mut self,
        error: impl Into<String>,
        predicate: impl TokenComparable,
    ) {
        self.err(error);
        let range = self.current_range();
        if !predicate.matches(self.current.kind, &self.text.as_bytes()[range]) {
            self.eat_raw();
        }
    }

    /// write an error, do not advance
    pub(crate) fn err(&mut self, error: impl Into<String>) {
        let err = SyntaxError {
            range: self.current_range(),
            message: error.into(),
        };
        self.sink.error(err);
    }

    /// consume if the token matches, otherwise error without advancing
    pub(crate) fn expect(&mut self, kind: RawKind) -> bool {
        if self.current.kind == kind {
            self.eat_raw();
            true
        } else {
            self.err(format!("Expected {}, found {}", kind, self.current.kind));
            false
        }
    }

    pub(crate) fn expect_tag(&mut self) -> bool {
        if self.current.kind == RawKind::Ident {
            if self.current_range().len() <= 4 {
                self.eat_remap(Kind::Tag);
            } else {
                // this is an error, but we continue parsing
                self.eat_raw();
                self.err("Tag must be four or fewer characters.");
            }
            true
        } else {
            self.err(format!("expected tag, found {}", self.current.kind));
            false
        }
    }

    pub(crate) fn current_match(&self, token: impl TokenComparable) -> bool {
        let range = self.current_range();
        token.matches(self.current.kind, &self.text.as_bytes()[range])
    }

    pub(crate) fn next_match(&self, token: impl TokenComparable) -> bool {
        let next_pos = self.pos + self.current.len;
        let next_range = next_pos..next_pos + self.next.len;
        token.matches(self.next.kind, &self.text.as_bytes()[next_range])
    }

    /// If current token is ident, return underlying bytes
    pub(crate) fn ident(&self) -> Option<&[u8]> {
        if self.current.kind == RawKind::Ident {
            Some(&self.text.as_bytes()[self.current_range()])
        } else {
            None
        }
    }
}

pub(crate) trait TokenComparable {
    fn matches(&self, kind: RawKind, bytes: &[u8]) -> bool;
}

impl TokenComparable for &[u8] {
    fn matches(&self, _: RawKind, bytes: &[u8]) -> bool {
        self == &bytes
    }
}

impl TokenComparable for RawKind {
    fn matches(&self, kind: RawKind, _bytes: &[u8]) -> bool {
        self == &kind
    }
}

//impl TokenComparable for char {
//fn matches(&self, kind: RawKind, bytes: &[u8]) -> bool {
//match (self, kind) {
//(';', RawKind::Semi) => true,
//(',', RawKind::Comma) => true,
//('@', RawKind::At) => true,
//('\\', RawKind::Backslash) => true,
//('-', RawKind::Hyphen) => true,
//('=', RawKind::Eq) => true,
//('{', RawKind::LBrace) => true,
//('}', RawKind::RBrace) => true,
//('[', RawKind::LSquare) => true,
//(']', RawKind::RSquare) => true,
//('(', RawKind::LParen) => true,
//(')', RawKind::RParen) => true,
//('<', RawKind::LAngle) => true,
//('>', RawKind::RAngle) => true,
//('\'', RawKind::SingleQuote) => true,
//_ => false,
//}
//}
//}

impl TokenComparable for &[&[u8]] {
    fn matches(&self, kind: RawKind, bytes: &[u8]) -> bool {
        self.contains(&bytes)
    }
}

// taken from rust-analzyer
/// `TreeSink` abstracts details of a particular syntax tree implementation.
pub(crate) trait TreeSink {
    /// Adds new token to the current branch.
    fn token(&mut self, kind: Kind, len: usize);

    /// Start new branch and make it current.
    fn start_node(&mut self, kind: Kind);

    /// Finish current branch and restore previous
    /// branch as current.
    fn finish_node(&mut self);

    fn error(&mut self, error: SyntaxError);
}

#[derive(Debug, Clone)]
enum Event {
    /// start a new node
    Start(Kind),
    /// finish the current node
    Finish,
    Token(Kind, usize),
    Error(SyntaxError),
}

#[derive(Clone, Debug, Default)]
pub(crate) struct DebugSink(Vec<Event>);

impl TreeSink for DebugSink {
    fn token(&mut self, kind: Kind, len: usize) {
        self.0.push(Event::Token(kind, len))
    }

    fn start_node(&mut self, kind: Kind) {
        self.0.push(Event::Start(kind))
    }

    fn finish_node(&mut self) {
        self.0.push(Event::Finish);
    }

    fn error(&mut self, error: SyntaxError) {
        self.0.push(Event::Error(error))
    }
}

impl DebugSink {
    pub(crate) fn token_tree(&self) -> String {
        self.to_string()
    }
}

impl std::fmt::Display for DebugSink {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let mut pos = 0;
        let mut indent = 0;
        let mut node_stack = Vec::new();
        static WS: &str = "                                                                                                                        ";
        for event in &self.0 {
            match event {
                Event::Start(kind) => {
                    writeln!(f, "{}START {}", &WS[..indent], kind)?;
                    node_stack.push(kind);
                    indent += 2;
                }
                Event::Token(kind, len) => {
                    writeln!(f, "{}{}..{} {}", &WS[..indent], pos, pos + len, kind)?;
                    pos += len;
                }
                Event::Finish => {
                    indent -= 2;
                    writeln!(f, "{}END {}", &WS[..indent], node_stack.pop().unwrap())?;
                }
                Event::Error(_) => (),
            }
        }
        Ok(())
    }
}

//fn kind_for_raw_kind(raw: &RawKind) -> (Kind, Option<String>) {
//let kind = match raw {
//RawKind::String { terminated: false } => return (Kind::String, Some("Missing closing \".".into())),
//RawKind::String { terminated: true } => Kind::String,

//RawKind::At => Kind::At,
//RawKind::Semi => Kind::Semi,
//RawKind::Comma => Kind::Comma,
//RawKind::At => Kind::At,
//RawKind::Backslash => Kind::Backslash,
//RawKind::Hyphen => Kind::Hyphen, // also minus
//RawKind::Eq => Kind::Eq,
//RawKind::LBrace => Kind::LBrace,
//RawKind::RBrace => Kind::RBrace,
//RawKind::LSquare => Kind::LSquare,
//RawKind::RSquare => Kind::RSquare,
//RawKind::LParen => Kind::LParen,
//RawKind::RParen => Kind::RParen,
//RawKind::LAngle => Kind::LAngle,
//RawKind::RAngle => Kind::RAngle,
//RawKind::SingleQuote => Kind::SingleQuote,
//}

//}

#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) struct Token {
    pub(crate) len: usize,
    pub(crate) kind: Kind,
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub(crate) enum Kind {
    Tombstone, // a placeholder value, should never be seen
    Eof,

    SourceFile,
    Whitespace,
    Comment,

    NumberHex,
    NumberFloat,
    NumberDec,

    // symbols
    Semi,
    Comma,
    At,
    Backslash,
    Hyphen, // also minus
    Eq,
    LBrace,
    RBrace,
    LSquare,
    RSquare,
    LParen,
    RParen,
    LAngle,
    RAngle,
    SingleQuote,

    String,
    // including unresolved symbols for which we need more context
    Ident,

    // <http://adobe-type-tools.github.io/afdko/OpenTypeFeatureFileSpecification.html#2h-tags>
    Tag,
    Path,
    GlyphName,
    GlyphClass,
    Cid,

    // keywords:
    // top-level keywords
    TableKw,
    LookupKw,
    LanguagesystemKw,
    AnchorDefKw,
    FeatureKw,
    MarkClassKw,
    AnonKw,
    //AnonymousKw,

    // other keywords
    AnchorKw,
    ByKw,
    ContourpointKw,
    CursiveKw,
    DeviceKw,
    EnumKw,
    //EnumerateKw,
    ExcludeDfltKw,
    FromKw,
    IgnoreKw,
    IgnoreBaseGlyphsKw,
    IgnoreLigaturesKw,
    IgnoreMarksKw,
    IncludeKw,
    IncludeDfltKw,
    LanguageKw,
    LookupflagKw,
    MarkKw,
    MarkAttachmentTypeKw,
    NameIdKw,
    NullKw,
    ParametersKw,
    PosKw,
    //PositionKw,
    RequiredKw,
    ReversesubKw,
    RightToLeftKw,
    RsubKw,
    ScriptKw,
    SubKw,
    //SubstituteKw,
    SubtableKw,
    UseExtensionKw,
    UseMarkFilteringSetKw,
    ValueRecordDefKw,
}

impl Kind {
    // nothing fancy, idents are still idents etc
    pub(crate) fn from_raw_kind(raw: RawKind) -> Kind {
        match raw {
            RawKind::Ident => Kind::Ident,
            RawKind::String => Kind::String,
            RawKind::NumberDec => Kind::NumberDec, // also octal; we disambiguate based on context
            RawKind::NumberHex => Kind::NumberHex,
            RawKind::NumberFloat => Kind::NumberFloat,
            RawKind::Whitespace => Kind::Whitespace,
            RawKind::Semi => Kind::Semi,
            RawKind::Comma => Kind::Comma,
            RawKind::At => Kind::At,
            RawKind::Backslash => Kind::Backslash,
            RawKind::Hyphen => Kind::Hyphen, // also minus
            RawKind::Eq => Kind::Eq,
            RawKind::LBrace => Kind::LBrace,
            RawKind::RBrace => Kind::RBrace,
            RawKind::LSquare => Kind::LSquare,
            RawKind::RSquare => Kind::RSquare,
            RawKind::LParen => Kind::LParen,
            RawKind::RParen => Kind::RParen,
            RawKind::LAngle => Kind::LAngle,
            RawKind::RAngle => Kind::RAngle,
            RawKind::SingleQuote => Kind::SingleQuote,
            RawKind::Comment => Kind::Comment,
            RawKind::Eof => Kind::Eof,
            RawKind::Tombstone => Kind::Tombstone,
            RawKind::StringUnterminated | RawKind::NumberHexEmpty => {
                panic!("invalid raw token kind")
            }
        }
    }

    pub(crate) fn from_keyword(word: &str) -> Option<Kind> {
        match word {
            "anchor" => Some(Kind::AnchorKw),
            "anchorDef" => Some(Kind::AnchorDefKw),
            "anon" | "anonymous" => Some(Kind::AnonKw),
            "by" => Some(Kind::ByKw),
            "contourpoint" => Some(Kind::ContourpointKw),
            "cursive" => Some(Kind::CursiveKw),
            "device" => Some(Kind::DeviceKw), //[ Not implemented ];
            "enum" | "enumerate" => Some(Kind::EnumKw),
            "exclude_dflt" | "excludeDFLT" => Some(Kind::ExcludeDfltKw),
            "feature" => Some(Kind::FeatureKw), //(used as a block and as a statement);
            "from" => Some(Kind::FromKw),
            "ignore" => Some(Kind::IgnoreKw), //(used with substitute and position);
            "IgnoreBaseGlyphs" => Some(Kind::IgnoreBaseGlyphsKw),
            "IgnoreLigatures" => Some(Kind::IgnoreLigaturesKw),
            "IgnoreMarks" => Some(Kind::IgnoreMarksKw),
            "include" => Some(Kind::IncludeKw),
            "include_dflt" | "includeDFLT" => Some(Kind::IncludeDfltKw),
            "language" => Some(Kind::LanguageKw),
            "languagesystem" => Some(Kind::LanguagesystemKw),
            "lookup" => Some(Kind::LookupKw),
            "lookupflag" => Some(Kind::LookupflagKw),
            "mark" => Some(Kind::MarkKw),
            "MarkAttachmentType" => Some(Kind::MarkAttachmentTypeKw),
            "markClass" => Some(Kind::MarkClassKw),
            "nameid" => Some(Kind::NameIdKw),
            "NULL" => Some(Kind::NullKw), //(used in substitute, device, value record, anchor);
            "parameters" => Some(Kind::ParametersKw),
            "pos" | "position" => Some(Kind::PosKw),
            "required" => Some(Kind::RequiredKw), //[ Not implemented ];
            "reversesub" | "rsub" => Some(Kind::RsubKw),
            "RightToLeft" => Some(Kind::RightToLeftKw),
            "script" => Some(Kind::ScriptKw),
            "substitute" | "sub" => Some(Kind::SubKw),
            "subtable" => Some(Kind::SubtableKw),
            "table" => Some(Kind::TableKw),
            "useExtension" => Some(Kind::UseExtensionKw),
            "UseMarkFilteringSet" => Some(Kind::UseMarkFilteringSetKw),
            "valueRecordDef" => Some(Kind::ValueRecordDefKw),
            _ => None,
        }
    }
}

impl std::fmt::Display for Kind {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Tombstone => write!(f, "Tombstone"), // a placeholder value, should never be seen
            Self::Eof => write!(f, "Eof"),
            Self::SourceFile => write!(f, "FILE"),
            Self::Whitespace => write!(f, "WS"),
            Self::Comment => write!(f, "Comment"),
            Self::NumberHex => write!(f, "NumberHex"),
            Self::NumberFloat => write!(f, "NumberFloat"),
            Self::NumberDec => write!(f, "NumberDec"),
            Self::Semi => write!(f, "Semi"),
            Self::Comma => write!(f, "Comma"),
            Self::At => write!(f, "At"),
            Self::Backslash => write!(f, "Backslash"),
            Self::Hyphen => write!(f, "Hyphen"), // also minus
            Self::Eq => write!(f, "Eq"),
            Self::LBrace => write!(f, "LBrace"),
            Self::RBrace => write!(f, "RBrace"),
            Self::LSquare => write!(f, "LSquare"),
            Self::RSquare => write!(f, "RSquare"),
            Self::LParen => write!(f, "LParen"),
            Self::RParen => write!(f, "RParen"),
            Self::LAngle => write!(f, "LAngle"),
            Self::RAngle => write!(f, "RAngle"),
            Self::SingleQuote => write!(f, "SingleQuote"),
            Self::String => write!(f, "String"),
            Self::Ident => write!(f, "Ident"),
            Self::Tag => write!(f, "Tag"),
            Self::Path => write!(f, "Path"),
            Self::GlyphClass => write!(f, "GlyphClass"),
            Self::GlyphName => write!(f, "GlyphName"),
            Self::Cid => write!(f, "GlyphName"),
            Self::TableKw => write!(f, "TableKw"),
            Self::LookupKw => write!(f, "LookupKw"),
            Self::LanguagesystemKw => write!(f, "LanguagesystemKw"),
            Self::AnchorDefKw => write!(f, "AnchorDefKw"),
            Self::FeatureKw => write!(f, "FeatureKw"),
            Self::MarkClassKw => write!(f, "MarkClassKw"),
            Self::AnonKw => write!(f, "AnonKw"),
            Self::AnchorKw => write!(f, "AnchorKw"),
            Self::ByKw => write!(f, "ByKw"),
            Self::ContourpointKw => write!(f, "ContourpointKw"),
            Self::CursiveKw => write!(f, "CursiveKw"),
            Self::DeviceKw => write!(f, "DeviceKw"),
            Self::EnumKw => write!(f, "EnumKw"),
            Self::ExcludeDfltKw => write!(f, "ExcludeDfltKw"),
            Self::FromKw => write!(f, "FromKw"),
            Self::IgnoreKw => write!(f, "IgnoreKw"),
            Self::IgnoreBaseGlyphsKw => write!(f, "IgnoreBaseGlyphsKw"),
            Self::IgnoreLigaturesKw => write!(f, "IgnoreLigaturesKw"),
            Self::IgnoreMarksKw => write!(f, "IgnoreMarksKw"),
            Self::IncludeKw => write!(f, "IncludeKw"),
            Self::IncludeDfltKw => write!(f, "IncludeDfltKw"),
            Self::LanguageKw => write!(f, "LanguageKw"),
            Self::LookupflagKw => write!(f, "LookupflagKw"),
            Self::MarkKw => write!(f, "MarkKw"),
            Self::MarkAttachmentTypeKw => write!(f, "MarkAttachmentTypeKw"),
            Self::NameIdKw => write!(f, "NameIdKw"),
            Self::NullKw => write!(f, "NullKw"),
            Self::ParametersKw => write!(f, "ParametersKw"),
            Self::PosKw => write!(f, "PosKw"),
            Self::RequiredKw => write!(f, "RequiredKw"),
            Self::ReversesubKw => write!(f, "ReversesubKw"),
            Self::RightToLeftKw => write!(f, "RightToLeftKw"),
            Self::RsubKw => write!(f, "RsubKw"),
            Self::ScriptKw => write!(f, "ScriptKw"),
            Self::SubKw => write!(f, "SubKw"),
            Self::SubtableKw => write!(f, "SubtableKw"),
            Self::UseExtensionKw => write!(f, "UseExtensionKw"),
            Self::UseMarkFilteringSetKw => write!(f, "UseMarkFilteringSetKw"),
            Self::ValueRecordDefKw => write!(f, "ValueRecordDefKw"),
        }
    }
}

#[derive(Debug, Clone)]
pub(crate) struct SyntaxError {
    message: String,
    range: Range<usize>,
}

//fn root(parser: &mut Parser, sink: &mut dyn TreeSink) {
//sink.start_node(Kind::SourceFile);
//parser.eat_trivia(sink);

//sink.finish_node();
//}

//const HI: () = b"hi";
