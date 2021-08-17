use std::convert::TryInto;
use std::ops::Range;

const EOF: u8 = 0x0;

#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) struct Token {
    len: u32,
    kind: Kind,
}

pub(crate) struct SyntaxError {
    range: Range<u32>,
    kind: ErrorKind,
}

#[derive(Debug, Clone, Copy)]
#[repr(u8)]
enum ErrorKind {
    UnterminatedString,
    BadHex,
}

#[derive(Debug, Clone, Copy, PartialEq)]
#[repr(u8)]
enum Kind {
    // a name or a keyword or any other block of non-whitespace.
    // we will frequently have to disambiguate this based on context.
    Ident,
    String,
    NumberDec, // also octal; we disambiguate based on context
    NumberHex,
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
}

struct Cursor<'a> {
    input: &'a str,
    pos: usize,
}

impl<'a> Cursor<'a> {
    fn new(input: &'a str) -> Self {
        Cursor { input, pos: 0 }
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
        self.pos += 1;
        self.input.as_bytes().get(pos).copied()
    }

    fn next_token(&mut self) -> (Token, Option<ErrorKind>) {
        let start_pos = self.pos;
        let first = self.bump().unwrap_or(EOF);
        let (kind, error) = match first {
            EOF => (Kind::Eof, None),
            byte if is_ascii_whitespace(byte) => self.whitespace(),
            b'#' => self.comment(),
            b'"' => self.string(),
            b'0' => self.number(true),
            b'1'..=b'9' => self.number(false),
            b';' => (Kind::Semi, None),
            b',' => (Kind::Comma, None),
            b'@' => (Kind::At, None),
            b'\\' => (Kind::Backslash, None),
            b'-' => (Kind::Hyphen, None),
            b'=' => (Kind::Eq, None),
            b'{' => (Kind::LBrace, None),
            b'}' => (Kind::RBrace, None),
            b'[' => (Kind::LSquare, None),
            b']' => (Kind::RSquare, None),
            b'(' => (Kind::LParen, None),
            b')' => (Kind::RParen, None),
            b'<' => (Kind::LAngle, None),
            b'>' => (Kind::RAngle, None),
            b'\'' => (Kind::SingleQuote, None),
            _ => self.ident(),
        };
        let len: u32 = (self.pos - start_pos).try_into().unwrap();
        (Token { len, kind }, error)
    }

    fn whitespace(&mut self) -> (Kind, Option<ErrorKind>) {
        while is_ascii_whitespace(self.nth(0)) {
            self.bump();
        }
        (Kind::Whitespace, None)
    }

    fn comment(&mut self) -> (Kind, Option<ErrorKind>) {
        while [b'\n', EOF].contains(&self.nth(0)) {
            self.bump();
        }
        (Kind::Comment, None)
    }

    fn string(&mut self) -> (Kind, Option<ErrorKind>) {
        loop {
            match self.nth(0) {
                b'"' => {
                    self.bump();
                    return (Kind::String, None);
                }
                EOF => {
                    return (Kind::String, Some(ErrorKind::UnterminatedString));
                }
                _ => {
                    self.bump();
                }
            }
        }
    }

    fn number(&mut self, leading_zero: bool) -> (Kind, Option<ErrorKind>) {
        if leading_zero && [b'x', b'X'].contains(&self.nth(0)) {
            self.bump();
            if !self.nth(0).is_ascii_hexdigit() {
                return (Kind::NumberHex, Some(ErrorKind::BadHex));
            }
            self.eat_hex_digits();
            (Kind::NumberHex, None)
        } else {
            self.eat_decimal_digits();
            if self.nth(0) == b'.' {
                self.bump();
                self.eat_decimal_digits();
                (Kind::NumberFloat, None)
            } else {
                (Kind::NumberDec, None)
            }
        }
    }

    fn eat_hex_digits(&mut self) {
        //let mut seen_digit = false;
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
    fn ident(&mut self) -> (Kind, Option<ErrorKind>) {
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
        (Kind::Ident, None)
    }
}

// [\ , ' - ; < = > @ \ ( ) [ ] { }]
fn is_special(byte: u8) -> bool {
    (39..=45).contains(&byte)
        || (59..=64).contains(&byte)
        || (91..=93).contains(&byte)
        || byte == 123
        || byte == 125
}

pub(crate) fn tokenize(text: &str) -> (Vec<Token>, Vec<SyntaxError>) {
    if text.is_empty() {
        return Default::default();
    }

    let mut tokens = Vec::new();
    let mut errors = Vec::new();

    let mut cursor = Cursor::new(text);

    loop {
        let token_start: u32 = cursor.pos.try_into().unwrap();
        let (token, error) = cursor.next_token();
        if matches!(token.kind, Kind::Eof) {
            break (tokens, errors);
        }
        tokens.push(token);
        let range = token_start..token.len;
        if let Some(kind) = error {
            errors.push(SyntaxError { kind, range });
        }
    }
}

fn is_ascii_whitespace(byte: u8) -> bool {
    byte == b' ' || (0x9..=0xD).contains(&byte)
}

impl std::fmt::Display for Kind {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
    Self::Ident => write!(f, "ID"),
    Self::String => write!(f, "STR"),
    Self::NumberDec => write!(f, "DEC"),
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
    //Self::Anonymous => write!(f, ""),
    Self::Eof => write!(f, "EOF"),
        }
    }
}

pub(crate) fn debug_tokens(tokens: &[Token]) -> String {
    let mut result = String::new();
    let mut pos = 0;
    for token in tokens {
        result.push_str(&format!("{}..{} {}\n", pos, pos + token.len, token.kind));
        pos += token.len;
    }
    result
}

