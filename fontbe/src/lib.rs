//! Backend of the `fontc` font compiler.
pub mod avar;
pub mod cmap;
pub mod colr;
pub mod cpal;
pub mod error;
pub mod features;
pub mod font;
pub mod fvar;
pub mod gasp;
pub mod glyphs;
pub mod gvar;
pub mod head;
pub mod hvar;
pub mod meta;
pub mod metrics_and_limits;
pub mod mvar;
pub mod name;
pub mod orchestration;
pub mod os2;
pub mod paths;
pub mod post;
pub mod stat;
#[cfg(test)]
mod test_util;
pub mod vertical_metrics;
