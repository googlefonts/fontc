//! Building the `Debg` debug table.
//!
//! The `Debg` table stores source-level information about each lookup,
//! compatible with the [fontTools feaLib implementation][feaLib], but unlike
//! feaLib, we always set feature to null.
//!
//! [feaLib]: https://github.com/fonttools/fonttools/blob/bb1109e9/Lib/fontTools/feaLib/builder.py#L860-L864
//!
//! Format: raw UTF-8 JSON with the schema:
//! ```json
//! {
//!   "com.github.fonttools.feaLib": {
//!     "GSUB": { "0": ["file.fea:4:5", "lookup_name", null] },
//!     "GPOS": { "0": ["file.fea:8:1", null, null] }
//!   }
//! }
//! ```

use std::ops::Range;

use smol_str::SmolStr;

use crate::parse::ParseTree;
use write_fonts::types::Tag;

/// The 4-byte tag for the Debg table.
pub(crate) const DEBG_TAG: Tag = Tag::new(b"Debg");

/// The top-level key used in the Debg JSON.
const LOOKUP_DEBUG_INFO_KEY: &str = "com.github.fonttools.feaLib";

/// Debug information for a single lookup.
///
/// Stores the global source range and optional name; resolved to
/// `"file:line:col"` when building the final Debg bytes.
#[derive(Debug, Clone)]
pub(crate) struct LookupDebugInfo {
    pub range: Range<usize>,
    pub name: Option<SmolStr>,
}

/// Builder for the `Debg` table.
#[derive(Clone, Debug)]
pub(crate) struct DebgBuilder {
    gsub: Vec<Option<LookupDebugInfo>>,
    gpos: Vec<Option<LookupDebugInfo>>,
}

impl DebgBuilder {
    pub(crate) fn new(
        gsub: Vec<Option<LookupDebugInfo>>,
        gpos: Vec<Option<LookupDebugInfo>>,
    ) -> Self {
        Self { gsub, gpos }
    }

    pub(crate) fn build(&self, tree: &ParseTree) -> Vec<u8> {
        let mut out = String::new();
        out.push_str(&format!("{{\"{LOOKUP_DEBUG_INFO_KEY}\":{{"));

        let tables: Vec<_> = [("GSUB", &self.gsub), ("GPOS", &self.gpos)]
            .into_iter()
            .filter(|(_, infos)| infos.iter().any(|i| i.is_some()))
            .map(|(tag, infos)| {
                let entries: Vec<_> = infos
                    .iter()
                    .enumerate()
                    .filter_map(|(idx, info)| {
                        let info = info.as_ref()?;
                        let (file_id, local_range) =
                            tree.source_map().resolve_range(info.range.clone());
                        let location = tree
                            .get_source(file_id)
                            .map(|source| {
                                let (line, col) = source.line_col_for_offset(local_range.start);
                                format!("\"{}:{line}:{col}\"", source.path().display())
                            })
                            .unwrap_or_default();
                        let name = match &info.name {
                            Some(s) => format!("\"{s}\""),
                            None => "null".into(),
                        };
                        Some(format!("\"{idx}\":[{location},{name},null]"))
                    })
                    .collect();
                format!("\"{tag}\":{{{}}}", entries.join(","))
            })
            .collect();
        out.push_str(&tables.join(","));

        out.push_str("}}");
        out.into_bytes()
    }
}
