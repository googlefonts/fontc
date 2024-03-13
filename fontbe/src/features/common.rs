use fea_rs::GlyphSet;
use write_fonts::tables::layout::LookupFlag;

/// A lookup generated outside of user FEA
///
/// This will be merged into any user-provided features during compilation.
#[derive(Debug, Default, Clone, PartialEq, serde::Serialize, serde::Deserialize)]
pub struct PendingLookup<T> {
    pub(crate) subtables: Vec<T>,
    pub(crate) flags: LookupFlag,
    pub(crate) mark_filter_set: Option<GlyphSet>,
}

impl<T> PendingLookup<T> {
    pub(crate) fn new(
        subtables: Vec<T>,
        flags: LookupFlag,
        mark_filter_set: Option<GlyphSet>,
    ) -> Self {
        Self {
            subtables,
            flags,
            mark_filter_set,
        }
    }
}
