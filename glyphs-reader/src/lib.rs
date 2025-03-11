//! Lightweight library for reading and writing Glyphs font files.

pub mod error;
mod font;
pub mod glyphdata;

// Generated parallel data arrays
#[rustfmt::skip]
mod glyphslib_categories;
#[rustfmt::skip]
mod glyphslib_subcategories;
#[rustfmt::skip]
mod glyphslib_codepoints;
#[rustfmt::skip]
mod glyphslib_names;
#[rustfmt::skip]
mod glyphslib_production_names;
#[rustfmt::skip]
mod glyphslib_production_name_to_idx;

mod plist;
mod propagate_anchors;

pub use font::{
    Axis, Component, CustomParameters, FeatureSnippet, Font, FontMaster, Glyph, InstanceType,
    Layer, Node, NodeType, Path, Shape,
};
pub use plist::Plist;
