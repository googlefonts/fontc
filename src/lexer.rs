//! Scan a FEA file, producing a sequence of tokens.
//!
//! This is the first step in our parsing process. The tokens produced here
//! have no semantic information; for instance we do not try to distinguish a
//! keyword from a glyph name. Instead we are just describing the most basic
//! structure of the document.

//use text_unit::TextUnit;

const EOF: u8 = 0x0;

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
    // a name or a keyword or any other block of non-whitespace.
    // we will frequently have to disambiguate this based on context.
    Ident,
    String,
    StringUnterminated,
    NumberDec, // also octal; we disambiguate based on context
    NumberHex,
    NumberHexEmpty, // naked 0x
    NumberFloat,
    Whitespace,
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
    Comment,
    //Anonymous,
    Eof,
    Tombstone,
}

pub(crate) struct Lexer<'a> {
    input: &'a str,
    pos: usize,
}

impl<'a> Lexer<'a> {
    pub(crate) fn new(input: &'a str) -> Self {
        Lexer { input, pos: 0 }
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
            b'@' => Kind::At,
            b'\\' => Kind::Backslash,
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

    /// super dumb for now; we eat anything that isn't whitespace or special char.
    fn ident(&mut self) -> Kind {
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
        Kind::Ident
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
}

impl std::fmt::Display for Kind {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
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
            //Self::Anonymous(_) => write!(f, ""),
            Self::Eof => write!(f, "EOF"),
            Self::Tombstone => write!(f, "X_X"),
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
        assert_eq!(token_strs[0], "ID(languagesystem)");
        assert_eq!(token_strs[1], "WS( )");
        assert_eq!(token_strs[2], "ID(dflt)");
        assert_eq!(token_strs[3], "WS( )");
        assert_eq!(token_strs[4], "ID(cool)");
        assert_eq!(token_strs[5], ";");
    }
}
