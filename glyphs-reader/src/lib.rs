//! Lightweight library for reading and writing Glyphs font files.

mod corner_components;
pub mod error;
mod font;
pub mod glyphdata;
mod glyphdata_bundled;
mod glyphslib_enums;
mod plist;
mod propagate_anchors;
mod smart_components;

pub use font::{
    Anchor, Axis, AxisRule, Color, ColorStop, Component, CustomParameters, FeatureSnippet, Font,
    FontMaster, Glyph, InstanceType, Layer, LayerAttributes, Node, NodeType, Path, Shape,
    ShapeAttributes, glyphs_to_opentype_lang_id,
};
pub use plist::Plist;
