use std::collections::BTreeMap;
use std::{borrow::Cow, fmt::Debug};

use kurbo::{Affine, Point};
use ordered_float::OrderedFloat;

use plist_derive::FromPlist;
use smol_str::SmolStr;

/// A plist dictionary
pub type Dictionary = BTreeMap<String, Plist>;

/// An array of plist values
pub type Array = Vec<Plist>;

/// An enum representing a property list.
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Plist {
    Dictionary(BTreeMap<String, Plist>),
    Array(Vec<Plist>),
    String(String),
    Integer(i64),
    Float(OrderedFloat<f64>),
}

#[derive(Clone, Debug, thiserror::Error)]
pub enum Error {
    #[error("Unexpected character '{0}'")]
    UnexpectedChar(char),
    #[error("Unterminated string")]
    UnclosedString,
    #[error("Unknown escape code")]
    UnknownEscape,
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

    #[error("Oops, this parser is just broken")]
    SomethingWentWrong,
}

#[derive(Debug)]
pub(crate) enum Token<'a> {
    Eof,
    OpenBrace,
    OpenParen,
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
        }
    }

    pub fn get(&self, key: &str) -> Option<&Plist> {
        match self {
            Plist::Dictionary(d) => d.get(key),
            _ => None,
        }
    }

    pub fn as_dict(&self) -> Option<&BTreeMap<String, Plist>> {
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

    fn parse_rec(s: &str, ix: usize) -> Result<(Plist, usize), Error> {
        let (tok, mut ix) = Token::lex(s, ix)?;
        match tok {
            Token::Atom(s) => Ok((Plist::parse_atom(s), ix)),
            Token::String(s) => Ok((Plist::String(s.into()), ix)),
            Token::OpenBrace => {
                let mut dict = BTreeMap::new();
                loop {
                    if let Some(ix) = Token::expect(s, ix, b'}') {
                        return Ok((Plist::Dictionary(dict), ix));
                    }
                    let (key, next) = Token::lex(s, ix)?;
                    let key_str = Token::try_into_string(key)?;
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
                if let Some(ix) = Token::expect(s, ix, b')') {
                    return Ok((Plist::Array(list), ix));
                }
                loop {
                    let (val, next) = Self::parse_rec(s, ix)?;
                    list.push(val);
                    if let Some(ix) = Token::expect(s, next, b')') {
                        return Ok((Plist::Array(list), ix));
                    }
                    if let Some(next) = Token::expect(s, next, b',') {
                        ix = next;
                    } else {
                        return Err(Error::ExpectedComma);
                    }
                }
            }
            _ => Err(Error::SomethingWentWrong),
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
        }
    }
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
                            ix += 1;
                            if ix == s.len() {
                                return Err(Error::UnclosedString);
                            }
                            let b = s.as_bytes()[ix];
                            match b {
                                b'"' | b'\\' => cow_start = ix,
                                b'n' => {
                                    buf.push('\n');
                                    cow_start = ix + 1;
                                }
                                b'r' => {
                                    buf.push('\r');
                                    cow_start = ix + 1;
                                }
                                _ => {
                                    if (b'0'..=b'3').contains(&b) && ix + 2 < s.len() {
                                        // octal escape
                                        let b1 = s.as_bytes()[ix + 1];
                                        let b2 = s.as_bytes()[ix + 2];
                                        if (b'0'..=b'7').contains(&b1)
                                            && (b'0'..=b'7').contains(&b2)
                                        {
                                            let oct =
                                                (b - b'0') * 64 + (b1 - b'0') * 8 + (b2 - b'0');
                                            buf.push(oct as char);
                                            ix += 2;
                                            cow_start = ix + 1;
                                        } else {
                                            return Err(Error::UnknownEscape);
                                        }
                                    } else {
                                        return Err(Error::UnknownEscape);
                                    }
                                }
                            }
                            ix += 1;
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

    fn try_into_string(self) -> Result<String, Error> {
        match self {
            Token::Atom(s) => Ok(s.into()),
            Token::String(s) => Ok(s.into()),
            Token::Eof => Err(Error::NotAString { token_name: "eof" }),
            Token::OpenBrace => Err(Error::NotAString { token_name: "{" }),
            Token::OpenParen => Err(Error::NotAString { token_name: "(" }),
        }
    }

    pub fn as_str(&self) -> Option<&str> {
        match self {
            Token::Atom(s) => Some(*s),
            Token::String(s) => Some(s),
            Token::Eof => None,
            Token::OpenBrace => None,
            Token::OpenParen => None,
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
        }
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

impl From<BTreeMap<String, Plist>> for Plist {
    fn from(x: BTreeMap<String, Plist>) -> Plist {
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

impl<'a> Debug for Tokenizer<'a> {
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
            Token::Atom(..) => Ok(()),
            Token::String(..) => Ok(()),
            Token::OpenBrace => loop {
                if self.eat(b'}').is_ok() {
                    return Ok(());
                }
                let key = self.lex()?;
                Token::try_into_string(key)?;
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
                }
            }
            _ => Err(Error::SomethingWentWrong),
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
        }
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
    pub(crate) const SEMICOLON_SV_IN_BRACES: VecDelimiters = VecDelimiters {
        start: b'{',
        end: b'}',
        sep: b';',
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
            return Err(Error::SomethingWentWrong);
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

    use crate::Plist;

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
}
