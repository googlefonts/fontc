//! Scan a FEA file, producing a sequence of tokens.
//!
//! This is the first step in our parsing process. The tokens produced here
//! have no semantic information; for instance we do not try to distinguish a
//! keyword from a glyph name. Instead we are just describing the most basic
//! structure of the document.
//!
//! The `Lexer` type is driven by a [Parser].
//!
//! [Parser]: super::Parser

mod lexeme;
mod token_set;

pub(crate) use lexeme::{Kind, Lexeme};
pub use token_set::TokenSet;

const EOF: u8 = 0x0;

pub(crate) struct Lexer<'a> {
    input: &'a str,
    pos: usize,
    after_backslash: bool,
    after_number_or_float: bool,
    in_path: ExpectingPath,
}

// simple state machine for tracking whether we should be parsing a path.
//
// paths are complicated because suddenly we stop tokenizing, and just
// glom everything together up to the closing parens.
#[derive(Clone, Copy, Default)]
enum ExpectingPath {
    #[default]
    Ready,
    // we have seen the 'include' keyword. This means if the next token is a paren,
    // we enter the 'InPath' state.
    SawInclude,
    InPath,
}

impl ExpectingPath {
    fn in_path(self) -> bool {
        matches!(self, ExpectingPath::InPath)
    }

    fn transition(&mut self, kind: Kind) {
        *self = match (*self, kind) {
            (ExpectingPath::Ready, Kind::IncludeKw) => ExpectingPath::SawInclude,
            (ExpectingPath::SawInclude, Kind::LParen) => ExpectingPath::InPath,
            // don't transition if we see whitespace after include, e.g,
            // include (hi.fea)
            (ExpectingPath::SawInclude, Kind::Whitespace) => ExpectingPath::SawInclude,
            _ => ExpectingPath::Ready,
        }
    }
}

impl<'a> Lexer<'a> {
    pub(crate) fn new(input: &'a str) -> Self {
        Lexer {
            input,
            pos: 0,
            after_backslash: false,
            after_number_or_float: false,
            in_path: Default::default(),
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
        self.pos += usize::from(next.is_some());
        next
    }

    pub(crate) fn next_token(&mut self) -> Lexeme {
        let start_pos = self.pos;
        let first = self.bump().unwrap_or(EOF);
        let kind = match first {
            EOF => Kind::Eof,
            _ if self.in_path.in_path() => self.path(),
            byte if is_ascii_whitespace(byte) => self.whitespace(),
            b'#' => self.comment(),
            b'"' => self.string(),
            b'0'..=b'9' if self.after_backslash => self.cid(),
            b'0' => self.number(true),
            b'1'..=b'9' => self.number(false),
            b';' => Kind::Semi,
            b':' => Kind::Colon,
            b',' => Kind::Comma,
            b'@' => self.glyph_class_name(),
            b'\\' => Kind::Backslash,
            b'-' => self.hyphen_or_minus(),
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
            b'$' => Kind::Dollar,
            b'*' => Kind::Asterisk,
            b'+' => Kind::Plus,
            b'/' => Kind::Slash,
            b'n' | b'u' | b'd' if self.after_number_or_float => Kind::NumberSuffix,
            _ => self.ident(),
        };
        self.in_path.transition(kind);

        self.after_backslash = matches!(kind, Kind::Backslash);
        self.after_number_or_float = matches!(kind, Kind::Number | Kind::Float);

        let len = self.pos - start_pos;
        Lexeme { len, kind }
    }

    fn whitespace(&mut self) -> Kind {
        while is_ascii_whitespace(self.nth(0)) {
            self.bump();
        }
        Kind::Whitespace
    }

    fn comment(&mut self) -> Kind {
        while ![b'\n', EOF].contains(&self.nth(0)) {
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

    fn hyphen_or_minus(&mut self) -> Kind {
        if self.nth(0) == b'0' {
            // octal, so this is a hyphen (and an error)
            if self.nth(1).is_ascii_digit() {
                return Kind::Hyphen;
            }
            // hex: ditto
            if [b'x', b'X'].contains(&self.nth(1)) {
                return Kind::Hyphen;
            }
        }
        if self.nth(0).is_ascii_digit() {
            return self.number(false);
        }

        Kind::Hyphen
    }

    fn number(&mut self, leading_zero: bool) -> Kind {
        if leading_zero && self.nth(0) != b'.' {
            if [b'x', b'X'].contains(&self.nth(0)) {
                self.bump();
                if self.nth(0).is_ascii_hexdigit() {
                    self.eat_hex_digits();
                    Kind::Hex
                } else {
                    Kind::HexEmpty
                }
            } else if self.nth(0).is_ascii_digit() {
                self.eat_octal_digits();
                Kind::Octal
            } else {
                // just '0'
                Kind::Number
            }
        } else {
            self.eat_decimal_digits();
            if self.nth(0) == b'.' {
                self.bump();
                self.eat_decimal_digits();
                Kind::Float
            } else {
                Kind::Number
            }
        }
    }

    fn eat_octal_digits(&mut self) {
        while matches!(self.nth(0), b'0'..=b'7') {
            self.bump();
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

    fn cid(&mut self) -> Kind {
        self.eat_decimal_digits();
        Kind::Cid
    }

    fn glyph_class_name(&mut self) -> Kind {
        self.eat_ident();
        Kind::NamedGlyphClass
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

    fn path(&mut self) -> Kind {
        while !matches!(self.nth(0), EOF | b')') {
            self.bump();
        }
        Kind::Path
    }
}

#[cfg(test)]
pub(crate) fn tokenize(text: &str) -> Vec<Lexeme> {
    iter_tokens(text).collect()
}

#[cfg(test)]
pub(crate) fn iter_tokens(text: &str) -> impl Iterator<Item = Lexeme> + '_ {
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

fn is_ascii_whitespace(byte: u8) -> bool {
    byte == b' ' || (0x9..=0xD).contains(&byte)
}

#[cfg(test)]
pub(crate) fn debug_tokens(tokens: &[Lexeme]) -> Vec<String> {
    let mut result = Vec::new();
    let mut pos = 0;
    for token in tokens {
        result.push(format!("{}..{} {}", pos, pos + token.len, token.kind));
        pos += token.len;
    }
    result
}

#[cfg(test)]
pub(crate) fn debug_tokens2(tokens: &[Lexeme], src: &str) -> Vec<String> {
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
    fn numbers() {
        let fea = "0 001 10 1. 1.0 -1 -1. -1.5";
        let tokens = tokenize(fea);
        let token_strs = debug_tokens2(&tokens, fea);
        assert_eq!(token_strs[0], "NUM(0)");
        assert_eq!(token_strs[2], "OCT(001)");
        assert_eq!(token_strs[4], "NUM(10)");
        assert_eq!(token_strs[6], "FLOAT(1.)");
        assert_eq!(token_strs[8], "FLOAT(1.0)");
        assert_eq!(token_strs[10], "NUM(-1)");
        assert_eq!(token_strs[12], "FLOAT(-1.)");
    }

    #[test]
    fn bad_numbers() {
        let fea = "-00 -0x1 -0x -ff";
        let tokens = tokenize(fea);
        let token_strs = debug_tokens2(&tokens, fea);
        assert_eq!(token_strs[0], "-");
        assert_eq!(token_strs[1], "OCT(00)");
        assert_eq!(token_strs[3], "-");
        assert_eq!(token_strs[4], "HEX(0x1)");
        assert_eq!(token_strs[6], "-");
        assert_eq!(token_strs[7], "HEX EMPTY(0x)");
        assert_eq!(token_strs[9], "-");
        assert_eq!(token_strs[10], "ID(ff)");
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

    #[test]
    fn cid_versus_ident() {
        let fea = "@hi =[\\1-\\2 a - b];";
        let tokens = tokenize(fea);
        let token_strs = debug_tokens2(&tokens, fea);
        assert_eq!(token_strs[0], "@GlyphClass(@hi)");
        assert_eq!(token_strs[1], "WS( )");
        assert_eq!(token_strs[2], "=");
        assert_eq!(token_strs[3], "[");
        assert_eq!(token_strs[4], "\\");
        assert_eq!(token_strs[5], "CID(1)");
        assert_eq!(token_strs[6], "-");
        assert_eq!(token_strs[7], "\\");
        assert_eq!(token_strs[8], "CID(2)");
        assert_eq!(token_strs[9], "WS( )");
        assert_eq!(token_strs[10], "ID(a)");
        assert_eq!(token_strs[11], "WS( )");
        assert_eq!(token_strs[12], "-");
        assert_eq!(token_strs[13], "WS( )");
        assert_eq!(token_strs[14], "ID(b)");
        assert_eq!(token_strs[15], "]");
        assert_eq!(token_strs[16], ";");
    }

    #[test]
    fn trivia() {
        let fea = "# OpenType 4.h\n# -@,\nlanguagesystem DFLT cool;";
        let tokens = tokenize(fea);
        let token_strs = debug_tokens2(&tokens, fea);
        assert_eq!(token_strs[0], "#(# OpenType 4.h)");
        assert_eq!(token_strs[1], "WS(\n)");
        assert_eq!(token_strs[2], "#(# -@,)");
        assert_eq!(token_strs[3], "WS(\n)");
        assert_eq!(token_strs[4], "LanguagesystemKw");
        assert_eq!(token_strs[5], "WS( )");
        assert_eq!(token_strs[6], "ID(DFLT)");
        assert_eq!(token_strs[7], "WS( )");
        assert_eq!(token_strs[8], "ID(cool)");
        assert_eq!(token_strs[9], ";");
    }

    #[test]
    fn suffixes_good() {
        let fea = "1n -5.3u 31.1d 0n";
        let tokens = tokenize(fea);
        let token_strs = debug_tokens2(&tokens, fea);
        assert_eq!(token_strs[0], "NUM(1)");
        assert_eq!(token_strs[1], "SUFFIX(n)");
        assert_eq!(token_strs[3], "FLOAT(-5.3)");
        assert_eq!(token_strs[4], "SUFFIX(u)");
        assert_eq!(token_strs[6], "FLOAT(31.1)");
        assert_eq!(token_strs[7], "SUFFIX(d)");
        assert_eq!(token_strs[9], "NUM(0)");
        assert_eq!(token_strs[10], "SUFFIX(n)");
    }

    #[test]
    fn include_with_spaces() {
        let fea = "include ( path.fea );";
        let tokens = tokenize(fea);
        let token_strs = debug_tokens2(&tokens, fea);
        assert_eq!(token_strs[0], "IncludeKw");
        assert_eq!(token_strs[1], "WS( )");
        assert_eq!(token_strs[2], "(");
        assert_eq!(token_strs[3], "Path( path.fea )");
        assert_eq!(token_strs[4], ")");
        assert_eq!(token_strs[5], ";");
        assert!(token_strs.get(6).is_none());
    }
}
