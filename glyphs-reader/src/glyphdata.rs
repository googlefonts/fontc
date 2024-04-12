//! determining glyph properties
//!
//! This module provides access to glyph info extracted from bundled
//! (and potentially user-provided) data files.

// NOTE: we define the types and parsing code in a separate file, so that
// we can borrow it in our build.rs script without causing a cycle
mod glyphdata_impl;
pub use glyphdata_impl::*;

static BUNDLED_DATA: &[u8] = include_bytes!(concat!(env!("OUT_DIR"), "/glyphdata.bin"));

fn load_bundled_data() -> Vec<GlyphInfo> {
    bincode::deserialize(BUNDLED_DATA).unwrap()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_bundled_data() {
        let data = load_bundled_data();
        assert_eq!(data.len(), 73329);
    }
}
