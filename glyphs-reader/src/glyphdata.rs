//! determining glyph properties
//!
//! This module provides access to glyph info extracted from bundled
//! (and potentially user-provided) data files.

// NOTE: we define the types and parsing code in a separate file, so that
// we can borrow it in our build.rs script without causing a cycle
mod glyphdata_impl;
use std::{
    collections::{HashMap, HashSet},
    path::Path,
};

pub use glyphdata_impl::*;
use smol_str::SmolStr;

static BUNDLED_DATA: &[u8] = include_bytes!(concat!(env!("OUT_DIR"), "/glyphdata.bin"));

/// A queryable set of glyph data
///
/// This is generally expensive to create, and is intended to be cached, or
/// used behind a OnceCell. It is never modified after initial creation.
pub struct GlyphData {
    // The info for all the glyphs we know of.
    data: Vec<GlyphInfo>,
    // the values in all maps are indices into the `data` vec. we use u32 to save space.
    name_map: HashMap<SmolStr, u32>,
    unicode_map: HashMap<u32, u32>,
    alt_name_map: HashMap<SmolStr, u32>,
}

impl GlyphData {
    /// Create a new data set, optionally loading user provided overrides
    pub fn new(user_overrides: Option<&Path>) -> Result<Self, GlyphDataError> {
        let user_overrides = user_overrides
            .map(|path| {
                let bytes = std::fs::read(path).map_err(|err| GlyphDataError::UserFile {
                    path: path.to_owned(),
                    reason: err.kind(),
                });
                bytes.and_then(|xml| parse_entries(&xml))
            })
            .transpose()?;
        let bundled = load_bundled_data();
        let all_entries = match user_overrides {
            Some(user_overrides) => merge_data(bundled, user_overrides),
            None => bundled,
        };

        Ok(Self::new_impl(all_entries))
    }

    fn new_impl(entries: Vec<GlyphInfo>) -> Self {
        let mut name_map = HashMap::with_capacity(entries.len());
        let mut unicode_map = HashMap::with_capacity(entries.len());
        let mut alt_name_map = HashMap::new();

        for (i, entry) in entries.iter().enumerate() {
            name_map.insert(entry.name.clone(), i as u32);
            if let Some(cp) = entry.unicode {
                unicode_map.insert(cp, i as _);
            }
            for alt in &entry.alt_names {
                alt_name_map.insert(alt.clone(), i as _);
            }
        }

        Self {
            data: entries,
            name_map,
            unicode_map,
            alt_name_map,
        }
    }

    /// Look up info for a glyph by name
    ///
    /// This checks primary names first, and alternates afterwards.
    pub fn get_by_name(&self, name: impl AsRef<str>) -> Option<&GlyphInfo> {
        let name = name.as_ref();
        self.name_map
            .get(name)
            .or_else(|| self.alt_name_map.get(name))
            .and_then(|idx| self.data.get(*idx as usize))
    }

    /// Look up info for a glyph by codepoint
    pub fn get_by_codepoint(&self, codepoint: u32) -> Option<&GlyphInfo> {
        self.unicode_map
            .get(&codepoint)
            .and_then(|idx| self.data.get(*idx as usize))
    }
}

fn load_bundled_data() -> Vec<GlyphInfo> {
    bincode::deserialize(BUNDLED_DATA).unwrap()
}

fn merge_data(mut base: Vec<GlyphInfo>, overrides: Vec<GlyphInfo>) -> Vec<GlyphInfo> {
    let skip_names = overrides
        .iter()
        .map(|info| &info.name)
        .collect::<HashSet<_>>();
    base.retain(|info| !skip_names.contains(&info.name));
    base.extend(overrides);
    base
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_bundled_data() {
        let data = load_bundled_data();
        assert_eq!(data.len(), 73329);
    }

    #[test]
    fn simple_overrides() {
        let overrides = vec![GlyphInfo {
            name: "A".into(),
            category: Category::Mark,
            subcategory: Subcategory::SpacingCombining,
            unicode: Some(b'A' as u32),
            production: None,
            alt_names: Default::default(),
        }];
        let bundled = load_bundled_data();
        let merged = merge_data(bundled, overrides);
        let data = GlyphData::new_impl(merged);

        assert_eq!(data.get_by_name("A").unwrap().category, Category::Mark);
    }

    #[test]
    fn overrides_from_file() {
        let data = GlyphData::new(Some(Path::new("./data/GlyphData_override_test.xml"))).unwrap();
        assert_eq!(data.get_by_name("zero").unwrap().category, Category::Other);
        assert_eq!(data.get_by_name("C").unwrap().category, Category::Number);
        assert_eq!(
            data.get_by_name("Yogh").unwrap().production,
            Some("Yolo".into())
        );
    }
}
