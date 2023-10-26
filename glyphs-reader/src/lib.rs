//! Lightweight library for reading and writing Glyphs font files.

pub mod error;
mod font;
mod plist;

pub use font::{
    Axis, Component, FeatureSnippet, Font, FontMaster, Glyph, InstanceType, Layer, Node, NodeType,
    Path, Shape,
};
pub use plist::Plist;
