use std::collections::BTreeMap;
use std::{borrow::Cow, fmt::Debug};

use kurbo::{Affine, Point};
use ordered_float::OrderedFloat;

use ascii_plist_derive::FromPlist;
use smol_str::SmolStr;

/// A plist dictionary
pub type Dictionary = BTreeMap<SmolStr, Plist>;

/// An array of plist values
pub type Array = Vec<Plist>;

/// An enum representing a property list.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Plist {
    Dictionary(Dictionary),
    Array(Array),
    String(String),
    Integer(i64),
    Float(OrderedFloat<f64>),
    Data(Vec<u8>),
}

#[derive(Clone, Debug, PartialEq, Eq, thiserror::Error)]
pub enum Error {
    #[error("Unexpected character '{0}'")]
    UnexpectedChar(char),
    #[error("Unterminated string")]
    UnclosedString,
    #[error("Unterminated data block")]
    UnclosedData,
    #[error("Data block did not contain valid paired hex digits")]
    BadData,
    #[error("Unknown escape code")]
    UnknownEscape,
    #[error("Invalid unicode escape sequence: '{0}'")]
    InvalidUnicodeEscape(String),
    #[error("Expected string, found '{token_name}")]
    NotAString { token_name: &'static str },
    #[error("Missing '='")]
    ExpectedEquals,
    #[error("Missing ','")]
    ExpectedComma,
    #[error("Missing ';'")]
    ExpectedSemicolon,
    #[error("Missing '{{'")]
    ExpectedOpenBrace,
    #[error("Missing '}}'")]
    ExpectedCloseBrace,
    #[error("Missing '('")]
    ExpectedOpenParen,
    #[error("Missing ')'")]
    ExpectedCloseParen,
    #[error("Expected character '{0}'")]
    ExpectedChar(char),
    #[error("Expected numeric value")]
    ExpectedNumber,
    #[error("Expected string value")]
    ExpectedString,
    #[error("Expected '{expected}', found '{found}'")]
    UnexpectedDataType {
        expected: &'static str,
        found: &'static str,
    },
    #[error("Unexpected token '{name}'")]
    UnexpectedToken { name: &'static str },
    #[error("Expected {value_type}, found '{actual}'")]
    UnexpectedNumberOfValues {
        value_type: &'static str,
        actual: usize,
    },
    #[error("parsing failed: '{0}'")]
    Parse(String),
}

#[derive(Debug, PartialEq)]
pub(crate) enum Token<'a> {
    Eof,
    OpenBrace,
    OpenParen,
    Data(Vec<u8>),
    String(Cow<'a, str>),
    Atom(&'a str),
}

fn is_numeric(b: u8) -> bool {
    b.is_ascii_digit() || b == b'.' || b == b'-'
}

fn is_alnum(b: u8) -> bool {
    // https://github.com/opensource-apple/CF/blob/3cc41a76b1491f50813e28a4ec09954ffa359e6f/CFOldStylePList.c#L79
    is_numeric(b)
        || b.is_ascii_uppercase()
        || b.is_ascii_lowercase()
        || b == b'_'
        || b == b'$'
        || b == b'/'
        || b == b':'
        || b == b'.'
        || b == b'-'
}

// Used for serialization; make sure UUID's get quoted
fn is_alnum_strict(b: u8) -> bool {
    is_alnum(b) && b != b'-'
}

fn is_hex_upper(b: u8) -> bool {
    b.is_ascii_digit() || (b'A'..=b'F').contains(&b)
}

fn is_ascii_whitespace(b: u8) -> bool {
    b == b' ' || b == b'\t' || b == b'\r' || b == b'\n'
}

fn numeric_ok(s: &str) -> bool {
    let s = s.as_bytes();
    if s.is_empty() {
        return false;
    }
    let s = if s.len() > 1 && (*s.first().unwrap(), *s.last().unwrap()) == (b'"', b'"') {
        &s[1..s.len()]
    } else {
        s
    };
    if s.iter().all(|&b| is_hex_upper(b)) && !s.iter().all(|&b| b.is_ascii_digit()) {
        return false;
    }
    if s.len() > 1 && s[0] == b'0' {
        return !s.iter().all(|&b| b.is_ascii_digit());
    }
    // Prevent parsing of "infinity", "inf", "nan" as numbers, we
    // want to keep them as strings (e.g. glyphname)
    // https://doc.rust-lang.org/std/primitive.f64.html#grammar
    if s.eq_ignore_ascii_case(b"infinity")
        || s.eq_ignore_ascii_case(b"inf")
        || s.eq_ignore_ascii_case(b"nan")
    {
        return false;
    }
    true
}

fn skip_ws(s: &str, mut ix: usize) -> usize {
    while ix < s.len() && is_ascii_whitespace(s.as_bytes()[ix]) {
        ix += 1;
    }
    ix
}

fn escape_string(buf: &mut String, s: &str) {
    if !s.is_empty() && s.as_bytes().iter().all(|&b| is_alnum_strict(b)) {
        buf.push_str(s);
    } else {
        buf.push('"');
        let mut start = 0;
        let mut ix = start;
        while ix < s.len() {
            let b = s.as_bytes()[ix];
            match b {
                b'"' | b'\\' => {
                    buf.push_str(&s[start..ix]);
                    buf.push('\\');
                    start = ix;
                }
                _ => (),
            }
            ix += 1;
        }
        buf.push_str(&s[start..]);
        buf.push('"');
    }
}

impl Plist {
    pub fn parse(s: &str) -> Result<Plist, Error> {
        let (plist, _ix) = Plist::parse_rec(s, 0)?;
        // TODO: check that we're actually at eof
        Ok(plist)
    }

    fn name(&self) -> &'static str {
        match self {
            Plist::Array(..) => "array",
            Plist::Dictionary(..) => "dictionary",
            Plist::Float(..) => "float",
            Plist::Integer(..) => "integer",
            Plist::String(..) => "string",
            Plist::Data(..) => "data",
        }
    }

    pub fn get(&self, key: &str) -> Option<&Plist> {
        match self {
            Plist::Dictionary(d) => d.get(key),
            _ => None,
        }
    }

    pub fn as_dict(&self) -> Option<&BTreeMap<SmolStr, Plist>> {
        match self {
            Plist::Dictionary(d) => Some(d),
            _ => None,
        }
    }

    pub fn as_array(&self) -> Option<&[Plist]> {
        match self {
            Plist::Array(a) => Some(a),
            _ => None,
        }
    }

    pub fn as_str(&self) -> Option<&str> {
        match self {
            Plist::String(s) => Some(s),
            _ => None,
        }
    }

    pub fn as_i64(&self) -> Option<i64> {
        match self {
            Plist::Integer(i) => Some(*i),
            _ => None,
        }
    }

    pub fn as_f64(&self) -> Option<f64> {
        match self {
            Plist::Integer(i) => Some(*i as f64),
            Plist::Float(f) => Some((*f).into_inner()),
            _ => None,
        }
    }

    pub fn expect_dict(self) -> Result<Dictionary, Error> {
        match self {
            Plist::Dictionary(dict) => Ok(dict),
            _other => Err(Error::UnexpectedDataType {
                expected: "dictionary",
                found: _other.name(),
            }),
        }
    }

    pub fn expect_array(self) -> Result<Array, Error> {
        match self {
            Plist::Array(array) => Ok(array),
            _other => Err(Error::UnexpectedDataType {
                expected: "array",
                found: _other.name(),
            }),
        }
    }

    pub fn expect_string(self) -> Result<String, Error> {
        match self {
            Plist::String(string) => Ok(string),
            _other => Err(Error::UnexpectedDataType {
                expected: "string",
                found: _other.name(),
            }),
        }
    }

    pub fn expect_data(self) -> Result<Vec<u8>, Error> {
        match self {
            Plist::Data(bytes) => Ok(bytes),
            _other => Err(Error::UnexpectedDataType {
                expected: "data",
                found: _other.name(),
            }),
        }
    }

    fn parse_rec(s: &str, ix: usize) -> Result<(Plist, usize), Error> {
        let (tok, mut ix) = Token::lex(s, ix)?;
        match tok {
            Token::Atom(s) => Ok((Plist::parse_atom(s), ix)),
            Token::String(s) => Ok((Plist::String(s.into()), ix)),
            Token::Data(bytes) => Ok((Plist::Data(bytes), ix)),
            Token::OpenBrace => {
                let mut dict = BTreeMap::new();
                loop {
                    if let Some(ix) = Token::expect(s, ix, b'}') {
                        return Ok((Plist::Dictionary(dict), ix));
                    }
                    let (key, next) = Token::lex(s, ix)?;
                    let key_str = Token::try_into_smolstr(key)?;
                    let next = Token::expect(s, next, b'=');
                    if next.is_none() {
                        return Err(Error::ExpectedEquals);
                    }
                    let (val, next) = Self::parse_rec(s, next.unwrap())?;
                    dict.insert(key_str, val);
                    if let Some(next) = Token::expect(s, next, b';') {
                        ix = next;
                    } else {
                        return Err(Error::ExpectedSemicolon);
                    }
                }
            }
            Token::OpenParen => {
                let mut list = Vec::new();
                loop {
                    if let Some(ix) = Token::expect(s, ix, b')') {
                        return Ok((Plist::Array(list), ix));
                    }
                    let (val, next) = Self::parse_rec(s, ix)?;
                    list.push(val);
                    if let Some(ix) = Token::expect(s, next, b')') {
                        return Ok((Plist::Array(list), ix));
                    }
                    if let Some(next) = Token::expect(s, next, b',') {
                        ix = next;
                        if let Some(next) = Token::expect(s, next, b')') {
                            return Ok((Plist::Array(list), next));
                        }
                    } else {
                        return Err(Error::ExpectedComma);
                    }
                }
            }
            _ => Err(Error::UnexpectedToken { name: tok.name() }),
        }
    }

    fn parse_atom(s: &str) -> Plist {
        if numeric_ok(s) {
            if let Ok(num) = s.parse() {
                return Plist::Integer(num);
            }
            if let Ok(num) = s.parse() {
                return Plist::Float(num);
            }
        }
        Plist::String(s.into())
    }

    #[allow(clippy::inherent_to_string, unused)]
    pub fn to_string(&self) -> String {
        let mut s = String::new();
        self.push_to_string(&mut s);
        s
    }

    fn push_to_string(&self, s: &mut String) {
        match self {
            Plist::Array(a) => {
                s.push('(');
                let mut delim = "\n";
                for el in a {
                    s.push_str(delim);
                    el.push_to_string(s);
                    delim = ",\n";
                }
                s.push_str("\n)");
            }
            Plist::Dictionary(a) => {
                s.push_str("{\n");
                let mut keys: Vec<_> = a.keys().collect();
                keys.sort();
                for k in keys {
                    let el = &a[k];
                    // TODO: quote if needed?
                    escape_string(s, k);
                    s.push_str(" = ");
                    el.push_to_string(s);
                    s.push_str(";\n");
                }
                s.push('}');
            }
            Plist::String(st) => escape_string(s, st),
            Plist::Integer(i) => {
                s.push_str(&format!("{i}"));
            }
            Plist::Float(f) => {
                s.push_str(&format!("{f}"));
            }
            Plist::Data(data) => {
                s.push('<');
                for byte in data {
                    s.extend(hex_digits_for_byte(*byte))
                }
                s.push('>');
            }
        }
    }
}

impl FromPlist for Plist {
    fn parse(tokenizer: &mut Tokenizer) -> Result<Self, Error> {
        let Tokenizer { content, idx } = tokenizer;
        let (val, end_idx) = Self::parse_rec(content, *idx)?;
        *idx = end_idx;
        Ok(val)
    }
}

impl Default for Plist {
    fn default() -> Self {
        // kind of arbitrary but seems okay
        Plist::Array(Vec::new())
    }
}

fn hex_digits_for_byte(byte: u8) -> [char; 2] {
    fn to_hex_digit(val: u8) -> char {
        match val {
            0..=9 => ('0' as u32 as u8 + val).into(),
            10..=15 => (('a' as u32 as u8) + val - 10).into(),
            _ => unreachable!("only called with values in range 0..=15"),
        }
    }

    [to_hex_digit(byte >> 4), to_hex_digit(byte & 0x0f)]
}

fn byte_from_hex(hex: [u8; 2]) -> Result<u8, Error> {
    fn hex_digit_to_byte(digit: u8) -> Result<u8, Error> {
        match digit {
            b'0'..=b'9' => Ok(digit - b'0'),
            b'a'..=b'f' => Ok(digit - b'a' + 10),
            b'A'..=b'F' => Ok(digit - b'A' + 10),
            _ => Err(Error::BadData),
        }
    }
    let maj = hex_digit_to_byte(hex[0])? << 4;
    let min = hex_digit_to_byte(hex[1])?;
    Ok(maj | min)
}

impl<'a> Token<'a> {
    fn lex(s: &'a str, ix: usize) -> Result<(Token<'a>, usize), Error> {
        let start = skip_ws(s, ix);
        if start == s.len() {
            return Ok((Token::Eof, start));
        }
        let b = s.as_bytes()[start];
        match b {
            b'{' => Ok((Token::OpenBrace, start + 1)),
            b'(' => Ok((Token::OpenParen, start + 1)),
            b'<' => {
                let data_start = start + 1;
                let data_end = data_start
                    + s.as_bytes()[data_start..]
                        .iter()
                        .position(|b| *b == b'>')
                        .ok_or(Error::UnclosedData)?;
                let chunks = s.as_bytes()[data_start..data_end].chunks_exact(2);
                if !chunks.remainder().is_empty() {
                    return Err(Error::BadData);
                }
                let data = chunks
                    .map(|x| byte_from_hex(x.try_into().unwrap()))
                    .collect::<Result<_, _>>()?;
                Ok((Token::Data(data), data_end + 1))
            }
            b'"' => {
                let mut ix = start + 1;
                let mut cow_start = ix;
                let mut buf = String::new();
                while ix < s.len() {
                    let b = s.as_bytes()[ix];
                    match b {
                        b'"' => {
                            // End of string
                            let string = if buf.is_empty() {
                                s[cow_start..ix].into()
                            } else {
                                buf.push_str(&s[cow_start..ix]);
                                buf.into()
                            };
                            return Ok((Token::String(string), ix + 1));
                        }
                        b'\\' => {
                            buf.push_str(&s[cow_start..ix]);
                            if ix + 1 == s.len() {
                                return Err(Error::UnclosedString);
                            }
                            let (c, len) = parse_escape(&s[ix..])?;
                            buf.push(c);
                            ix += len;
                            cow_start = ix;
                        }
                        _ => ix += 1,
                    }
                }
                Err(Error::UnclosedString)
            }
            _ => {
                if is_alnum(b) {
                    let mut ix = start + 1;
                    while ix < s.len() {
                        if !is_alnum(s.as_bytes()[ix]) {
                            break;
                        }
                        ix += 1;
                    }
                    Ok((Token::Atom(&s[start..ix]), ix))
                } else {
                    Err(Error::UnexpectedChar(s[start..].chars().next().unwrap()))
                }
            }
        }
    }

    fn try_into_smolstr(self) -> Result<SmolStr, Error> {
        match self {
            Token::Atom(s) => Ok(s.into()),
            Token::String(s) => Ok(s.into()),
            _ => Err(Error::NotAString {
                token_name: self.name(),
            }),
        }
    }

    pub fn as_str(&self) -> Option<&str> {
        match self {
            Token::Atom(s) => Some(*s),
            Token::String(s) => Some(s),
            Token::Eof => None,
            Token::OpenBrace => None,
            Token::OpenParen => None,
            Token::Data(_) => None,
        }
    }

    fn expect(s: &str, ix: usize, delim: u8) -> Option<usize> {
        let ix = skip_ws(s, ix);
        if ix < s.len() {
            let b = s.as_bytes()[ix];
            if b == delim {
                return Some(ix + 1);
            }
        }
        None
    }

    pub(crate) fn name(&self) -> &'static str {
        match self {
            Token::Atom(..) => "Atom",
            Token::String(..) => "String",
            Token::Eof => "Eof",
            Token::OpenBrace => "OpenBrace",
            Token::OpenParen => "OpenParen",
            Token::Data(_) => "Data",
        }
    }
}

fn parse_escape(s: &str) -> Result<(char, usize), Error> {
    // checked before this is called
    assert!(s.starts_with('\\') && s.len() > 1);

    let mut ix = 1;
    let b = s.as_bytes()[ix];
    match b {
        b'"' | b'\\' => Ok((b as _, 2)),
        b'n' => Ok(('\n', 2)),
        b'r' => Ok(('\r', 2)),
        b't' => Ok(('\t', 2)),
        // unicode escapes
        b'U' if s.len() >= 3 => {
            // here we will parse up to 4 hexdigits:
            // https://github.com/opensource-apple/CF/blob/3cc41a76b1491f5/CFOldStylePList.c#L150C2-L150C6
            ix += 1;
            let (val, len) = parse_hex_digit(&s.as_bytes()[ix..])?;
            ix += len;
            let result = if !is_surrogate(val) || !s.as_bytes()[ix..].starts_with(b"\\U") {
                // we can't cast! this is a utf-16 value, not a codepoint
                char::decode_utf16([val]).next()
            } else {
                ix += 2;
                let (val2, len) = parse_hex_digit(&s.as_bytes()[ix..])?;
                ix += len;
                char::decode_utf16([val, val2]).next()
            };
            result
                .transpose()
                .ok()
                .flatten()
                .ok_or_else(|| Error::InvalidUnicodeEscape(s[..ix].to_string()))
                .map(|c| (c, ix))
        }
        b'0'..=b'3' if s.len() >= 4 => {
            // octal escape
            let b1 = s.as_bytes()[ix + 1];
            let b2 = s.as_bytes()[ix + 2];
            if (b'0'..=b'7').contains(&b1) && (b'0'..=b'7').contains(&b2) {
                let oct = (b - b'0') * 64 + (b1 - b'0') * 8 + (b2 - b'0');
                ix += 3;
                Ok((oct as _, ix))
            } else {
                Err(Error::UnknownEscape)
            }
        }
        _ => Err(Error::UnknownEscape),
    }
}

fn is_surrogate(val: u16) -> bool {
    matches!(val, 0xD800..=0xDFFF)
}

// parse up to four hex digits as a u16
// returns an error if the first byte is not a valid ascii hex digit,
// then will read up to four bytes
fn parse_hex_digit(bytes: &[u8]) -> Result<(u16, usize), Error> {
    match bytes {
        &[] => Err(Error::UnknownEscape),
        &[one, ..] if !one.is_ascii_hexdigit() => Err(Error::UnknownEscape),
        other => Ok(other
            .iter()
            .take(4)
            .map_while(|b| (*b as char).to_digit(16).map(|x| x as u16))
            .fold((0u16, 0usize), |(num, len), hexval| {
                ((num << 4) + hexval, len + 1)
            })),
    }
}

impl From<String> for Plist {
    fn from(x: String) -> Plist {
        Plist::String(x)
    }
}

impl From<i64> for Plist {
    fn from(x: i64) -> Plist {
        Plist::Integer(x)
    }
}

impl From<f64> for Plist {
    fn from(x: f64) -> Plist {
        Plist::Float(x.into())
    }
}

impl From<Vec<Plist>> for Plist {
    fn from(x: Vec<Plist>) -> Plist {
        Plist::Array(x)
    }
}

impl From<Dictionary> for Plist {
    fn from(x: Dictionary) -> Plist {
        Plist::Dictionary(x)
    }
}

pub(crate) fn parse_int(s: &str) -> Result<i64, Error> {
    if numeric_ok(s) {
        if let Ok(num) = s.parse::<i64>() {
            return Ok(num);
        }
        if let Ok(num) = s.parse::<f64>() {
            return Ok(num as i64);
        }
    }
    Err(Error::ExpectedNumber)
}

pub(crate) fn parse_float(s: &str) -> Result<f64, Error> {
    if numeric_ok(s) {
        if let Ok(num) = s.parse::<f64>() {
            return Ok(num);
        }
    }
    Err(Error::ExpectedNumber)
}

/// This type can be parsed from a Plist string
pub trait FromPlist
where
    Self: Sized,
{
    fn parse(tokenizer: &mut Tokenizer) -> Result<Self, Error>;

    fn parse_plist(plist: &str) -> Result<Self, Error> {
        Tokenizer::new(plist).parse()
    }
}

impl<T> FromPlist for Vec<T>
where
    T: FromPlist,
{
    fn parse(tokenizer: &mut Tokenizer<'_>) -> Result<Self, crate::plist::Error> {
        tokenizer.parse_delimited_vec(VecDelimiters::CSV_IN_PARENS)
    }
}

impl<T: FromPlist> FromPlist for BTreeMap<SmolStr, T> {
    fn parse(tokenizer: &mut Tokenizer) -> Result<Self, Error> {
        tokenizer.parse_map()
    }
}

impl<T> FromPlist for Option<T>
where
    T: FromPlist,
{
    fn parse(tokenizer: &mut Tokenizer<'_>) -> Result<Self, crate::plist::Error> {
        Ok(Some(tokenizer.parse()?))
    }
}

pub struct Tokenizer<'a> {
    content: &'a str,
    idx: usize,
}

impl Debug for Tokenizer<'_> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let start = self.idx;
        let end = (start + 16).min(self.content.len());
        f.debug_struct("Tokenizer")
            .field("content", &&self.content[start..end])
            .field("idx", &self.idx)
            .finish()
    }
}

impl<'a> Tokenizer<'a> {
    pub fn new(content: &'a str) -> Tokenizer<'a> {
        Tokenizer { content, idx: 0 }
    }

    pub(crate) fn peek(&mut self) -> Result<Token<'a>, Error> {
        let (tok, _) = Token::lex(self.content, self.idx)?;
        Ok(tok)
    }

    pub(crate) fn lex(&mut self) -> Result<Token<'a>, Error> {
        let (tok, idx) = Token::lex(self.content, self.idx)?;
        self.idx = idx;
        Ok(tok)
    }

    pub(crate) fn eat(&mut self, delim: u8) -> Result<(), Error> {
        let Some(idx) = Token::expect(self.content, self.idx, delim) else {
            return Err(Error::ExpectedChar(delim as char));
        };
        self.idx = idx;
        Ok(())
    }

    /// Jump over the next thing, regardless of whether it's simple (atom or string) or complex
    /// (bracketed or braced construct)
    ///
    /// Named to match parse_rec.
    pub(crate) fn skip_rec(&mut self) -> Result<(), Error> {
        match self.lex()? {
            Token::Atom(..) | Token::String(..) | Token::Data(..) => Ok(()),
            Token::OpenBrace => loop {
                if self.eat(b'}').is_ok() {
                    return Ok(());
                }
                let key = self.lex()?;
                Token::try_into_smolstr(key)?;
                self.eat(b'=')?;
                self.skip_rec()?;
                self.eat(b';')?;
            },
            Token::OpenParen => {
                if self.eat(b')').is_ok() {
                    return Ok(());
                }
                loop {
                    self.skip_rec()?;
                    if self.eat(b')').is_ok() {
                        return Ok(());
                    }
                    self.eat(b',')?;
                    if self.eat(b')').is_ok() {
                        return Ok(());
                    }
                }
            }
            other => Err(Error::UnexpectedToken { name: other.name() }),
        }
    }

    pub(crate) fn parse_delimited_vec<T>(
        &mut self,
        delim: VecDelimiters,
    ) -> Result<Vec<T>, crate::plist::Error>
    where
        T: FromPlist,
    {
        let mut list = Vec::new();
        self.eat(delim.start)?;
        loop {
            if self.eat(delim.end).is_ok() {
                return Ok(list);
            }
            list.push(self.parse()?);
            if self.eat(delim.end).is_ok() {
                return Ok(list);
            }
            self.eat(delim.sep)?;
            // handle possible traliing separator
            if self.eat(delim.end).is_ok() {
                return Ok(list);
            }
        }
    }

    pub(crate) fn parse_map<T: FromPlist>(&mut self) -> Result<BTreeMap<SmolStr, T>, Error> {
        self.eat(b'{')?;
        let mut map = BTreeMap::new();
        loop {
            if self.eat(b'}').is_ok() {
                break;
            }
            let key = self.parse::<SmolStr>()?;
            self.eat(b'=')?;
            map.insert(key, self.parse()?);
            self.eat(b';')?;
        }
        Ok(map)
    }

    pub(crate) fn parse<T>(&mut self) -> Result<T, crate::plist::Error>
    where
        T: FromPlist,
    {
        T::parse(self)
    }
}

#[derive(Debug, Default, PartialEq, FromPlist)]
struct Features {
    features: Vec<Feature>,
}

#[derive(Debug, Default, PartialEq, FromPlist)]
struct Feature {
    automatic: i64,
    disabled: Option<i64>,
    code: String,
    name: Option<String>,
}

pub(crate) struct VecDelimiters {
    start: u8,
    end: u8,
    sep: u8,
}

impl VecDelimiters {
    pub(crate) const CSV_IN_PARENS: VecDelimiters = VecDelimiters {
        start: b'(',
        end: b')',
        sep: b',',
    };
    pub(crate) const CSV_IN_BRACES: VecDelimiters = VecDelimiters {
        start: b'{',
        end: b'}',
        sep: b',',
    };
}

impl FromPlist for OrderedFloat<f64> {
    fn parse(tokenizer: &mut Tokenizer<'_>) -> Result<Self, Error> {
        let val: f64 = tokenizer.parse()?;
        Ok(val.into())
    }
}

impl FromPlist for f64 {
    fn parse(tokenizer: &mut Tokenizer<'_>) -> Result<Self, Error> {
        match tokenizer.lex()? {
            Token::Atom(val) => parse_float(val),
            Token::String(val) => parse_float(&val),
            _ => Err(Error::ExpectedNumber),
        }
    }
}

impl FromPlist for i64 {
    fn parse(tokenizer: &mut Tokenizer<'_>) -> Result<Self, Error> {
        match tokenizer.lex()? {
            Token::Atom(val) => parse_int(val),
            Token::String(val) => parse_int(&val),
            _ => Err(Error::ExpectedNumber),
        }
    }
}

impl FromPlist for bool {
    fn parse(tokenizer: &mut Tokenizer<'_>) -> Result<Self, Error> {
        match tokenizer.lex()? {
            Token::Atom(val) => parse_int(val).map(|v| v == 1),
            Token::String(val) => parse_int(&val).map(|v| v == 1),
            _ => Err(Error::ExpectedNumber),
        }
    }
}

impl FromPlist for String {
    fn parse(tokenizer: &mut Tokenizer<'_>) -> Result<Self, Error> {
        match tokenizer.lex()? {
            Token::Atom(val) => Ok(val.to_string()),
            Token::String(val) => Ok(val.to_string()),
            _ => Err(Error::ExpectedString),
        }
    }
}

impl FromPlist for SmolStr {
    fn parse(tokenizer: &mut Tokenizer<'_>) -> Result<Self, Error> {
        match tokenizer.lex()? {
            Token::Atom(val) => Ok(val.into()),
            Token::String(val) => Ok(val.into()),
            _ => Err(Error::ExpectedString),
        }
    }
}

/// Hand-written because Glyphs 2 points don't look like Glyphs 3 points
impl FromPlist for Point {
    fn parse(tokenizer: &mut Tokenizer<'_>) -> Result<Self, Error> {
        let delims = if let Token::OpenBrace = tokenizer.peek()? {
            VecDelimiters::CSV_IN_BRACES
        } else {
            VecDelimiters::CSV_IN_PARENS
        };
        let coords: Vec<f64> = tokenizer.parse_delimited_vec(delims)?;
        if coords.len() != 2 {
            return Err(Error::Parse("wrong number of coords in point".to_string()));
        }
        Ok((coords[0], coords[1]).into())
    }
}

/// Hand-written because it's a String that becomes a Thing
impl FromPlist for Affine {
    fn parse(tokenizer: &mut Tokenizer<'_>) -> Result<Self, Error> {
        let tok = tokenizer.lex()?;
        let raw = match &tok {
            Token::Atom(val) => *val,
            Token::String(val) => val,
            _ => return Err(Error::ExpectedString),
        };
        let raw = &raw[1..raw.len() - 1];
        let coords: Vec<f64> = raw.split(", ").map(|c| c.parse().unwrap()).collect();
        Ok(Affine::new([
            coords[0], coords[1], coords[2], coords[3], coords[4], coords[5],
        ]))
    }
}

#[cfg(test)]
mod tests {
    use std::collections::BTreeMap;

    use super::*;

    #[test]
    fn parse_unquoted_strings() {
        let contents = r#"
        {
            name = "UFO Filename";
            value1 = ../../build/instance_ufos/Testing_Rg.ufo;
            value2 = _;
            value3 = $;
            value4 = /;
            value5 = :;
            value6 = .;
            value7 = -;
        }
        "#;

        let plist = Plist::parse(contents).unwrap();
        let plist_expected = Plist::Dictionary(BTreeMap::from_iter([
            ("name".into(), Plist::String("UFO Filename".into())),
            (
                "value1".into(),
                Plist::String("../../build/instance_ufos/Testing_Rg.ufo".into()),
            ),
            ("value2".into(), Plist::String("_".into())),
            ("value3".into(), Plist::String("$".into())),
            ("value4".into(), Plist::String("/".into())),
            ("value5".into(), Plist::String(":".into())),
            ("value6".into(), Plist::String(".".into())),
            ("value7".into(), Plist::String("-".into())),
        ]));
        assert_eq!(plist, plist_expected);
    }

    #[test]
    fn parse_int_map() {
        let contents = r#"
        {
            foo = 5;
            bar = 32;
        }"#;

        let foobar = BTreeMap::<SmolStr, i64>::parse_plist(contents).unwrap();
        assert_eq!(foobar.get("foo"), Some(&5));
        assert_eq!(foobar.get("bar"), Some(&32));
    }

    #[test]
    #[should_panic(expected = "ExpectedNumber")]
    fn parse_map_fail() {
        let contents = r#"
        {
            foo = hello;
            bar = 32;
        }"#;

        let _foobar = BTreeMap::<SmolStr, i64>::parse_plist(contents).unwrap();
    }

    #[test]
    fn parse_binary_data() {
        let contents = r#"
        {
            mydata = <deadbeef>;
        }
            "#;
        let plist = Plist::parse(contents).unwrap();
        let data = plist.get("mydata").unwrap().clone().expect_data().unwrap();
        assert_eq!(data, [0xde, 0xad, 0xbe, 0xef])
    }

    #[test]
    fn hex_to_ascii() {
        assert_eq!(hex_digits_for_byte(0x01), ['0', '1']);
        assert_eq!(hex_digits_for_byte(0x00), ['0', '0']);
        assert_eq!(hex_digits_for_byte(0xff), ['f', 'f']);
        assert_eq!(hex_digits_for_byte(0xf0), ['f', '0']);
        assert_eq!(hex_digits_for_byte(0x0f), ['0', 'f']);
    }

    #[test]
    fn ascii_to_hex() {
        assert_eq!(byte_from_hex([b'0', b'1']), Ok(0x01));
        assert_eq!(byte_from_hex([b'0', b'0']), Ok(0x00));
        assert_eq!(byte_from_hex([b'f', b'f']), Ok(0xff));
        assert_eq!(byte_from_hex([b'f', b'0']), Ok(0xf0));
        assert_eq!(byte_from_hex([b'0', b'f']), Ok(0x0f));
    }

    // in arrays the trailing comma is optional but supported
    #[test]
    fn array_optional_trailing_comma() {
        let _ = env_logger::builder().is_test(true).try_init();
        // we include a list that is not parsed in derive because that
        // takes a second codepath.
        let trailing = r#"
        {
            items = (
                "a",
                "b",
            );
            skip_me = (
                "c",
                "d",
            );
        }"#;

        let no_trailing = r#"
        {
            items = (
                "a",
                "b"
            );
            skip_me = (
                "c",
                "d"
            );
        }"#;

        #[derive(Default, FromPlist)]
        struct TestMe {
            items: Vec<String>,
        }

        let trailing = TestMe::parse_plist(trailing).unwrap();
        assert_eq!(trailing.items, ["a", "b"]);
        let no_trailing = TestMe::parse_plist(no_trailing).unwrap();
        assert_eq!(trailing.items, no_trailing.items);
    }

    #[test]
    fn parse_to_plist_type() {
        let plist_str = r#"
        {
            name = "meta";
            value = (
                {
                    data = latn;
                    tag = dlng;
                    num = 5;
                },
                {
                    data = "latn,cyrl";
                    tag = slng;
                    num = -3.0;
                }
            );
        }"#;

        let plist = Plist::parse_plist(plist_str).unwrap();
        let root = plist.expect_dict().unwrap();
        assert_eq!(root.get("name").unwrap().as_str(), Some("meta"));
        let value = root.get("value").unwrap().as_array().unwrap();
        assert_eq!(value.len(), 2);
        let first = value[0].as_dict().unwrap();
        assert_eq!(first.get("data").and_then(Plist::as_str), Some("latn"));
        assert_eq!(first.get("tag").and_then(Plist::as_str), Some("dlng"));
        assert_eq!(first.get("num").and_then(Plist::as_i64), Some(5));
        let second = value[1].as_dict().unwrap();
        assert_eq!(
            second.get("data").and_then(Plist::as_str),
            Some("latn,cyrl")
        );
        assert_eq!(second.get("tag").and_then(Plist::as_str), Some("slng"));
        assert_eq!(second.get("num").and_then(Plist::as_f64), Some(-3.0));
    }

    #[test]
    fn parse_hex_digit_sanity() {
        assert_eq!(parse_hex_digit(b"2019"), Ok((0x2019, 4)));
        assert_eq!(parse_hex_digit(b"201"), Ok((0x201, 3)));
        assert_eq!(parse_hex_digit(b"201z"), Ok((0x201, 3)));
        assert_eq!(parse_hex_digit(b"fu"), Ok((0xf, 1)));
        assert_eq!(parse_hex_digit(b"z"), Err(Error::UnknownEscape));
    }

    // partially borrowed from from python: https://github.com/fonttools/openstep-plist/blob/2fa77b267d67/tests/test_parser.py#L135
    #[test]
    fn escape_parsing_good() {
        for (input, expected, expected_len) in [
            ("\\n", '\n', 2),                    // octal escape
            ("\\012", '\n', 4),                  // octal escape
            ("\\U2019", '\u{2019}', 6),          // unicode escape (’)
            ("\\UD83D\\UDCA9", '\u{1F4A9}', 12), // surrogate pair (💩)
        ] {
            let (result, len) = parse_escape(input).unwrap();
            {
                assert_eq!((result, len), (expected, expected_len));
            }
        }
    }

    #[test]
    fn escape_parsing_bad() {
        assert_eq!(
            parse_escape("\\UD83D"),
            Err(Error::InvalidUnicodeEscape("\\UD83D".to_string()))
        );
    }

    #[test]
    fn parsing_escape_in_string() {
        for (input, expected, expected_len) in [
            ("\"a\\012b\"", "a\nb", 8),
            ("\"a\\nb\"", "a\nb", 6),
            ("\"a\\U000Ab\"", "a\nb", 10),
        ] {
            let (token, len) = Token::lex(input, 0).unwrap();
            assert_eq!(token, Token::String(Cow::Borrowed(expected)));
            assert_eq!(len, expected_len);
        }
    }

    #[test]
    fn parse_quoted_and_unquoted_ints_and_bools() {
        assert_eq!(
            (Ok(1), Ok(1), Ok(true), Ok(true), Ok(false), Ok(false)),
            (
                Tokenizer::new("1").parse::<i64>(),
                Tokenizer::new("\"1\"").parse::<i64>(),
                Tokenizer::new("1").parse::<bool>(),
                Tokenizer::new("\"1\"").parse::<bool>(),
                Tokenizer::new("0").parse::<bool>(),
                Tokenizer::new("\"0\"").parse::<bool>(),
            )
        );
    }
}
