use text_unit::TextUnit;

const EOF: u8 = 0x0;

#[derive(Debug, Clone, Copy, PartialEq)]
pub(crate) struct Token {
    len: TextUnit,
    kind: Kind,
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum Kind {
    // a name or a keyword or any other block of non-whitespace.
    // we will frequently have to disambiguate this based on context.
    Ident,
    String { terminated: bool },
    NumberDec, // also octal; we disambiguate based on context
    NumberHex { empty: bool },
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

    fn next_token(&mut self) -> Token {
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

        let len = TextUnit::from_usize(self.pos - start_pos);
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
        let terminated = loop {
            match self.nth(0) {
                b'"' => {
                    self.bump();
                    break true;
                }
                EOF => break false,
                _ => {
                    self.bump();
                }
            }
        };
        Kind::String { terminated }
    }

    fn number(&mut self, leading_zero: bool) -> Kind {
        if leading_zero && [b'x', b'X'].contains(&self.nth(0)) {
            self.bump();
            let empty = !self.nth(0).is_ascii_hexdigit();
            self.eat_hex_digits();
            Kind::NumberHex { empty }
            //if !self.nth(0).is_ascii_hexdigit() {
            //Kind::NumberHex { empty: true }
            //} else {
            //self.eat_hex_digits();
            //Kind::NumberHex { empty: false }
            //}
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

pub(crate) fn iter_tokens<'a>(text: &'a str) -> impl Iterator<Item = Token> + 'a {
    let mut cursor = Cursor::new(text);
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

impl std::fmt::Display for Kind {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Ident => write!(f, "ID"),
            Self::String { terminated: false } => write!(f, "STR OPEN"),
            Self::String { .. } => write!(f, "STR"),
            Self::NumberDec => write!(f, "DEC"),
            Self::NumberHex { empty: true } => write!(f, "HEX EMPTY"),
            Self::NumberHex { .. } => write!(f, "HEX"),
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
        }
    }
}

pub(crate) fn debug_tokens(tokens: &[Token]) -> Vec<String> {
    let mut result = Vec::new();
    let mut pos = TextUnit::default();
    for token in tokens {
        result.push(format!("{}..{} {}", pos, pos + token.len, token.kind));
        pos += token.len;
    }
    result
}

// microbenchmarks to do, one day

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
}
