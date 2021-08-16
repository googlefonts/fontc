use std::ops::Range;

fn main() {

}

struct Lexer<'a> {
    input: &'a str,
    pos: usize,
}

impl<'a> Iterator for Lexer<'_> {
    type Item = Token;

    fn next(&mut self) -> Option<Self::Item> {
        if self.pos >= self.input.len()
        
        None
    }

}

#[derive(Debug, Clone, PartialEq)]
struct Token {
    span: Range<u32>,
    kind: Kind,
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum Kind {
    // a name or a keyword or any other block of non-whitespace
    Literal,
    String,
    Number,
    Special(Special),
    Whitespace,
    Comment,
    Anonymous,
    Eof,
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum Number {
    Decimal,
    Octal,
    Hex,
    Float,
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum Special {
    Pound,
    Semi,
    Comma,
    At,
    Backslash,
    Hyphen,
    Eq,
    SingleQuote,
    OpenDelim(Delim),
    CloseDelim(Delim),
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum Delim {
    Brace, // { }
    Square, // [ ]
    Angle, // < >
    Paren, // ( )
}

