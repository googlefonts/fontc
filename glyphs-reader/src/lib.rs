//! Lightweight library for reading and writing Glyphs font files.

mod corner_components;
pub mod error;
mod font;
pub mod glyphdata;
mod glyphdata_bundled;
mod glyphslib_enums;
mod plist;
mod smart_components;

pub use corner_components::{
    BadCornerComponent, BadCornerComponentReason, apply_corner_components,
};
pub use font::{
    Alignment, Anchor, Axis, AxisRule, Color, ColorStop, Component, CustomParameters,
    FeatureSnippet, Font, FontMaster, Glyph, Hint, HintType, InstanceType, Layer, LayerAttributes,
    Node, NodeType, Path, Scale, Shape, ShapeAttributes, glyphs_to_opentype_lang_id,
};
pub use plist::Plist;
