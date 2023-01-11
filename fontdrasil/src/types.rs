//! Basic types useful for font compilation.
//!
//! Particularly types where it's nice for FE and BE to match.

use std::fmt::{Debug, Display};

use serde::{Deserialize, Serialize};
use smol_str::SmolStr;

#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct GlyphName(SmolStr);

impl GlyphName {
    pub fn as_str(&self) -> &str {
        self.0.as_str()
    }

    pub fn empty() -> GlyphName {
        "".into()
    }
}

impl From<&String> for GlyphName {
    fn from(value: &String) -> Self {
        GlyphName(value.into())
    }
}

impl From<String> for GlyphName {
    fn from(value: String) -> Self {
        GlyphName(value.into())
    }
}

impl From<&str> for GlyphName {
    fn from(value: &str) -> Self {
        GlyphName(value.into())
    }
}

impl Debug for GlyphName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.as_str())
    }
}

impl Display for GlyphName {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.as_str())
    }
}
