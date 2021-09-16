use std::{ops::Deref, str::FromStr};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Tag([u8; 4]);

impl Tag {
    pub const DFLT_SCRIPT: Tag = Tag([b'D', b'F', b'L', b'T']);
}

/// An error representing an invalid tag.
#[derive(Debug, Clone)]
pub struct InvalidTag(String);

impl FromStr for Tag {
    type Err = InvalidTag;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s.as_bytes() {
            &[a] => Ok(Tag([a, b' ', b' ', b' '])),
            &[a, b] => Ok(Tag([a, b, b' ', b' '])),
            &[a, b, c] => Ok(Tag([a, b, c, b' '])),
            &[a, b, c, d] => Ok(Tag([a, b, c, d])),
            _ => Err(InvalidTag(s.to_string())),
        }
    }
}

impl AsRef<str> for Tag {
    fn as_ref(&self) -> &str {
        // safety: tag can only be constructed from valid utf-8 (via FromStr)
        unsafe { std::str::from_utf8_unchecked(&self.0) }
    }
}

impl Deref for Tag {
    type Target = str;

    fn deref(&self) -> &Self::Target {
        unsafe { std::str::from_utf8_unchecked(&self.0) }
    }
}

impl std::fmt::Display for InvalidTag {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "'{}' is not a valid tag", self.0)
    }
}

impl std::error::Error for InvalidTag {}
