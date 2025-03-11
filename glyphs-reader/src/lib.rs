//! Lightweight library for reading and writing Glyphs font files.

pub mod error;
mod font;
pub mod glyphdata;
mod glyphdata_bundled;
mod glyphslib_enums;
mod plist;
mod propagate_anchors;

pub use font::{
    Axis, Component, CustomParameters, FeatureSnippet, Font, FontMaster, Glyph, InstanceType,
    Layer, Node, NodeType, Path, Shape,
};
pub use plist::Plist;
