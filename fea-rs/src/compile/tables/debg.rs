//! Building the `Debg` debug table.
//!
//! The `Debg` table stores source-level information about each lookup,
//! compatible with fontTools.feaLib format, but unlike feaLib, we always set
//! feature to null
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

use std::sync::Arc;

use crate::parse::{FileId, SourceList};
use write_fonts::types::Tag;

/// The 4-byte tag for the Debg table.
pub(crate) const DEBG_TAG: Tag = Tag::new(b"Debg");

/// The top-level key used in the Debg JSON.
const LOOKUP_DEBUG_INFO_KEY: &str = "com.github.fonttools.feaLib";

/// Debug information for a single lookup.
///
/// The source location is stored as a raw `(FileId, local_offset)` pair,
/// resolved to a `"file:line:col"` string when building the final Debg bytes.
#[derive(Debug, Clone)]
pub(crate) struct LookupDebugInfo {
    pub location: (FileId, usize),
    pub name: Option<String>,
}

/// Builder for the `Debg` table.
#[derive(Clone, Debug)]
pub(crate) struct DebgBuilder {
    gsub: Vec<Option<LookupDebugInfo>>,
    gpos: Vec<Option<LookupDebugInfo>>,
    source_list: Arc<SourceList>,
}

impl DebgBuilder {
    pub(crate) fn new(
        gsub: Vec<Option<LookupDebugInfo>>,
        gpos: Vec<Option<LookupDebugInfo>>,
        source_list: Arc<SourceList>,
    ) -> Self {
        Self {
            gsub,
            gpos,
            source_list,
        }
    }

    pub(crate) fn build(&self) -> Vec<u8> {
        let source_list = &self.source_list;
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
                        let (file_id, offset) = info.location;
                        let location = source_list
                            .get(&file_id)
                            .map(|source| {
                                let (line, col) = source.line_col_for_offset(offset);
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
