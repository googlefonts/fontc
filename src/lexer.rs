//! Scan a FEA file, producing a sequence of tokens.
//!
//! This is the first step in our parsing process. The tokens produced here
//! have no semantic information; for instance we do not try to distinguish a
//! keyword from a glyph name. Instead we are just describing the most basic
//! structure of the document.

//use text_unit::TextUnit;

const EOF: u8 = 0x0;

pub(crate) struct Lexer<'a> {
    input: &'a str,
    pos: usize,
    after_backslash: bool,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) struct Token {
    pub(crate) len: usize,
    pub(crate) kind: Kind,
}

impl Token {
    pub const EMPTY: Token = Token {
        len: 0,
        kind: Kind::Tombstone,
    };
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) enum Kind {
    Tombstone,  // a placeholder value
    Eof,        // the end of the input stream
    SourceFile, // scope of a file

    // a name or a keyword or any other block of non-whitespace.
    // we will frequently have to disambiguate this based on context.
    Ident,

    String,
    StringUnterminated, // an error handled at a higher level
    NumberDec,          // also octal; we disambiguate based on context
    NumberHex,          // an error handled at a higher level
    NumberHexEmpty,     // naked 0x
    NumberFloat,

    Whitespace,
    Comment,

    // special symbols
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
    RightToLeftKw,
    RsubKw,
    //ReversesubKw,
    ScriptKw,
    SubKw,
    //SubstituteKw,
    SubtableKw,
    UseExtensionKw,
    UseMarkFilteringSetKw,
    ValueRecordDefKw,
    //Anonymous,
}

impl<'a> Lexer<'a> {
    pub(crate) fn new(input: &'a str) -> Self {
        Lexer {
            input,
            pos: 0,
            after_backslash: false,
        }
    }

    fn nth(&self, index: usize) -> u8 {
        self.input
            .as_bytes()
            .get(self.pos + index)
            .copied()
            .unwrap_or(EOF)
    }

    fn bump(&mut self) -> Option<u8> {
        let pos = self.pos;
        let next = self.input.as_bytes().get(pos).copied();
        self.pos += if next.is_some() { 1 } else { 0 };
        next
    }

    pub(crate) fn next_token(&mut self) -> Token {
        let start_pos = self.pos;
        let first = self.bump().unwrap_or(EOF);
        let kind = match first {
            EOF => Kind::Eof,
            byte if is_ascii_whitespace(byte) => self.whitespace(),
            b'#' => self.comment(),
            b'"' => self.string(),
            b'0' => self.number(true),
            b'1'..=b'9' => self.number(false),
            b';' => Kind::Semi,
            b',' => Kind::Comma,
            b'@' => self.glyph_class_name(),
            b'\\' => Kind::Backslash,
            //b'\\' => self.backslash(),
            b'-' => Kind::Hyphen,
            b'=' => Kind::Eq,
            b'{' => Kind::LBrace,
            b'}' => Kind::RBrace,
            b'[' => Kind::LSquare,
            b']' => Kind::RSquare,
            b'(' => Kind::LParen,
            b')' => Kind::RParen,
            b'<' => Kind::LAngle,
            b'>' => Kind::RAngle,
            b'\'' => Kind::SingleQuote,
            _ => self.ident(),
        };

        self.after_backslash = matches!(kind, Kind::Backslash);

        let len = self.pos - start_pos;
        Token { len, kind }
    }

    fn whitespace(&mut self) -> Kind {
        while is_ascii_whitespace(self.nth(0)) {
            self.bump();
        }
        Kind::Whitespace
    }

    fn comment(&mut self) -> Kind {
        while [b'\n', EOF].contains(&self.nth(0)) {
            self.bump();
        }
        Kind::Comment
    }

    fn string(&mut self) -> Kind {
        loop {
            match self.nth(0) {
                b'"' => {
                    self.bump();
                    break Kind::String;
                }
                EOF => break Kind::StringUnterminated,
                _ => {
                    self.bump();
                }
            }
        }
    }

    fn number(&mut self, leading_zero: bool) -> Kind {
        if leading_zero && [b'x', b'X'].contains(&self.nth(0)) {
            self.bump();
            if self.nth(0).is_ascii_hexdigit() {
                self.eat_hex_digits();
                Kind::NumberHex
            } else {
                Kind::NumberHexEmpty
            }
        } else {
            self.eat_decimal_digits();
            if self.nth(0) == b'.' {
                self.bump();
                self.eat_decimal_digits();
                Kind::NumberFloat
            } else {
                Kind::NumberDec
            }
        }
    }

    fn eat_hex_digits(&mut self) {
        while self.nth(0).is_ascii_hexdigit() {
            self.bump();
        }
    }

    fn eat_decimal_digits(&mut self) {
        while self.nth(0).is_ascii_digit() {
            self.bump();
        }
    }

    fn glyph_class_name(&mut self) -> Kind {
        self.eat_ident();
        Kind::GlyphClass
    }

    fn eat_ident(&mut self) {
        loop {
            match self.nth(0) {
                EOF => break,
                b if is_ascii_whitespace(b) => break,
                b'-' => (),
                b if is_special(b) => break,
                _ => (),
            }
            self.bump();
        }
    }

    /// super dumb for now; we eat anything that isn't whitespace or special char.
    fn ident(&mut self) -> Kind {
        let start_pos = self.pos.saturating_sub(1);
        self.eat_ident();

        if self.after_backslash {
            return Kind::Ident;
        }

        let raw_token = &self.input.as_bytes()[start_pos..self.pos];
        Kind::from_keyword(raw_token).unwrap_or(Kind::Ident)
    }
}

pub(crate) fn iter_tokens(text: &str) -> impl Iterator<Item = Token> + '_ {
    let mut cursor = Lexer::new(text);
    std::iter::from_fn(move || {
        let next = cursor.next_token();
        match next.kind {
            Kind::Eof => None,
            _ => Some(next),
        }
    })
}

// [\ , ' - ; < = > @ \ ( ) [ ] { }]
fn is_special(byte: u8) -> bool {
    (39..=45).contains(&byte)
        || (59..=64).contains(&byte)
        || (91..=93).contains(&byte)
        || byte == 123
        || byte == 125
}

pub(crate) fn tokenize(text: &str) -> Vec<Token> {
    iter_tokens(text).collect()
}

fn is_ascii_whitespace(byte: u8) -> bool {
    byte == b' ' || (0x9..=0xD).contains(&byte)
}

impl Kind {
    pub(crate) fn has_contents(&self) -> bool {
        matches!(
            self,
            Self::Ident
                | Self::String
                | Self::StringUnterminated
                | Self::NumberFloat
                | Self::NumberHex
                | Self::NumberHexEmpty
                | Self::NumberDec
                | Self::Comment
                | Self::Whitespace
        )
    }

    pub(crate) fn from_keyword(word: &[u8]) -> Option<Kind> {
        eprintln!("{}", std::str::from_utf8(word).unwrap());
        match word {
            b"anchor" => Some(Kind::AnchorKw),
            b"anchorDef" => Some(Kind::AnchorDefKw),
            b"anon" | b"anonymous" => Some(Kind::AnonKw),
            b"by" => Some(Kind::ByKw),
            b"contourpoint" => Some(Kind::ContourpointKw),
            b"cursive" => Some(Kind::CursiveKw),
            b"device" => Some(Kind::DeviceKw), //[ Not implemented ];
            b"enum" | b"enumerate" => Some(Kind::EnumKw),
            b"exclude_dflt" | b"excludeDFLT" => Some(Kind::ExcludeDfltKw),
            b"feature" => Some(Kind::FeatureKw), //(used as a block and as a statement);
            b"from" => Some(Kind::FromKw),
            b"ignore" => Some(Kind::IgnoreKw), //(used with substitute and position);
            b"IgnoreBaseGlyphs" => Some(Kind::IgnoreBaseGlyphsKw),
            b"IgnoreLigatures" => Some(Kind::IgnoreLigaturesKw),
            b"IgnoreMarks" => Some(Kind::IgnoreMarksKw),
            b"include" => Some(Kind::IncludeKw),
            b"include_dflt" | b"includeDFLT" => Some(Kind::IncludeDfltKw),
            b"language" => Some(Kind::LanguageKw),
            b"languagesystem" => Some(Kind::LanguagesystemKw),
            b"lookup" => Some(Kind::LookupKw),
            b"lookupflag" => Some(Kind::LookupflagKw),
            b"mark" => Some(Kind::MarkKw),
            b"MarkAttachmentType" => Some(Kind::MarkAttachmentTypeKw),
            b"markClass" => Some(Kind::MarkClassKw),
            b"nameid" => Some(Kind::NameIdKw),
            b"NULL" => Some(Kind::NullKw), //(used in substitute, device, value record, anchor);
            b"parameters" => Some(Kind::ParametersKw),
            b"pos" | b"position" => Some(Kind::PosKw),
            b"required" => Some(Kind::RequiredKw), //[ Not implemented ];
            b"reversesub" | b"rsub" => Some(Kind::RsubKw),
            b"RightToLeft" => Some(Kind::RightToLeftKw),
            b"script" => Some(Kind::ScriptKw),
            b"substitute" | b"sub" => Some(Kind::SubKw),
            b"subtable" => Some(Kind::SubtableKw),
            b"table" => Some(Kind::TableKw),
            b"useExtension" => Some(Kind::UseExtensionKw),
            b"UseMarkFilteringSet" => Some(Kind::UseMarkFilteringSetKw),
            b"valueRecordDef" => Some(Kind::ValueRecordDefKw),
            _ => None,
        }
    }
}

impl std::fmt::Display for Kind {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Eof => write!(f, "EOF"),
            Self::Tombstone => write!(f, "X_X"),
            Self::SourceFile => write!(f, "FILE"),
            Self::Ident => write!(f, "ID"),
            Self::StringUnterminated => write!(f, "STR OPEN"),
            Self::String => write!(f, "STR"),
            Self::NumberDec => write!(f, "DEC"),
            Self::NumberHexEmpty => write!(f, "HEX EMPTY"),
            Self::NumberHex => write!(f, "HEX"),
            Self::NumberFloat => write!(f, "FLOAT"),
            Self::Whitespace => write!(f, "WS"),
            Self::Semi => write!(f, ";"),
            Self::Comma => write!(f, ","),
            Self::At => write!(f, "@"),
            Self::Backslash => write!(f, "\\"),
            Self::Hyphen => write!(f, "-"), // also minus
            Self::Eq => write!(f, "="),
            Self::LBrace => write!(f, "{{"),
            Self::RBrace => write!(f, "}}"),
            Self::LSquare => write!(f, "["),
            Self::RSquare => write!(f, "]"),
            Self::LParen => write!(f, "("),
            Self::RParen => write!(f, ")"),
            Self::LAngle => write!(f, "<"),
            Self::RAngle => write!(f, ">"),
            Self::SingleQuote => write!(f, "'"),
            Self::Comment => write!(f, "#"),

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
            Self::RightToLeftKw => write!(f, "RightToLeftKw"),
            Self::RsubKw => write!(f, "RsubKw"),
            //Self::ReversesubKw => write!(f, "ReversesubKw"),
            Self::ScriptKw => write!(f, "ScriptKw"),
            Self::SubKw => write!(f, "SubKw"),
            Self::SubtableKw => write!(f, "SubtableKw"),
            Self::UseExtensionKw => write!(f, "UseExtensionKw"),
            Self::UseMarkFilteringSetKw => write!(f, "UseMarkFilteringSetKw"),
            Self::ValueRecordDefKw => write!(f, "ValueRecordDefKw"),
        }
    }
}

pub(crate) fn debug_tokens(tokens: &[Token]) -> Vec<String> {
    let mut result = Vec::new();
    let mut pos = 0;
    for token in tokens {
        result.push(format!("{}..{} {}", pos, pos + token.len, token.kind));
        pos += token.len;
    }
    result
}

pub(crate) fn debug_tokens2(tokens: &[Token], src: &str) -> Vec<String> {
    let mut result = Vec::new();
    let mut pos = 0;
    for token in tokens {
        let text = if token.kind.has_contents() {
            format!("{}({})", token.kind, &src[pos..pos + token.len])
        } else {
            format!("{}", token.kind)
        };
        result.push(text);
        pos += token.len;
    }
    result
}

// microbenchmarks to do, one day. Who do you think will win??

//fn is_special_match(byte: u8) -> bool {
//match byte {
//b';' | b',' | b'@' | b'\\' | b'-' | b'=' | b'{' | b'}' | b'[' | b']' | b'(' | b')'
//| b'<' | b'>' | b'\'' => true,
//_ => false,
//}
//}

//fn is_special_ranges(byte: u8) -> bool {
//(39..=45).contains(&byte)
//|| (59..=64).contains(&byte)
//|| (91..=93).contains(&byte)
//|| byte == 123
//|| byte == 125
//}

//fn is_special_bsearch(byte: u8) -> bool {
//[39, 40, 41, 44, 45, 59, 60, 61, 62, 64, 91, 92, 93, 123, 125]
//.binary_search(&byte)
//.is_ok()
//}

//// could improve this by sorting by frequency
//fn is_special_linear_scan(byte: u8) -> bool {
//[39, 40, 41, 44, 45, 59, 60, 61, 62, 64, 91, 92, 93, 123, 125].contains(&byte)
//}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty_hex() {
        let fea = "0x 0x11 0xzz";
        let tokens = tokenize(fea);
        let token_strs = debug_tokens(&tokens);
        assert_eq!(token_strs[0], "0..2 HEX EMPTY");
        assert_eq!(token_strs[1], "2..3 WS");
        assert_eq!(token_strs[2], "3..7 HEX");
        assert_eq!(token_strs[3], "7..8 WS");
        assert_eq!(token_strs[4], "8..10 HEX EMPTY");
        assert_eq!(token_strs[5], "10..12 ID");
    }

    #[test]
    fn languagesystem() {
        let fea = "languagesystem dflt cool;";
        let tokens = tokenize(fea);
        assert_eq!(tokens[0].len, 14);
        let token_strs = debug_tokens2(&tokens, fea);
        assert_eq!(token_strs[0], "LanguagesystemKw");
        assert_eq!(token_strs[1], "WS( )");
        assert_eq!(token_strs[2], "ID(dflt)");
        assert_eq!(token_strs[3], "WS( )");
        assert_eq!(token_strs[4], "ID(cool)");
        assert_eq!(token_strs[5], ";");
    }

    #[test]
    fn escaping_keywords() {
        let fea = "sub \\sub \\rsub";
        let tokens = tokenize(fea);
        let token_strs = debug_tokens2(&tokens, fea);
        assert_eq!(token_strs[0], "SubKw");
        assert_eq!(token_strs[1], "WS( )");
        assert_eq!(token_strs[2], "\\");
        assert_eq!(token_strs[3], "ID(sub)");
        assert_eq!(token_strs[4], "WS( )");
        assert_eq!(token_strs[5], "\\");
        assert_eq!(token_strs[6], "ID(rsub)");
    }
}
