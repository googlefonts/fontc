//! Errors from merging per-master compilations.

use smol_str::SmolStr;
use write_fonts::{tables::gdef::GlyphClassDef, types::GlyphId16};

/// An error encountered while merging per-master compilations.
///
/// `master` is the index into the input at which the problem was found; the
/// master at index 0 is the default, and every other master is compared
/// against it.
#[derive(Debug, Clone, PartialEq, thiserror::Error)]
#[non_exhaustive]
#[allow(missing_docs)]
pub enum MergeError {
    #[error("no masters to merge")]
    NoMasters,
    #[error("masters {first} and {second} have the same location")]
    DuplicateLocation { first: usize, second: usize },
    #[error("master {master}: languagesystem statements differ from the default master")]
    LanguageSystems { master: usize },
    #[error("master {master}: '# Automatic Code' markers differ from the default master")]
    InsertMarkers { master: usize },
    #[error("master {master}: conditionset definitions differ from the default master")]
    ConditionSets { master: usize },
    #[error("master {master}: compilation options differ from the default master")]
    Options { master: usize },
    #[error("master {master}: mark filtering sets differ from the default master")]
    MarkFilterSets { master: usize },
    #[error("master {master}: mark attachment classes differ from the default master")]
    MarkAttachClasses { master: usize },
    #[error("master {master}: named lookups differ from the default master")]
    NamedLookups { master: usize },
    #[error("master {master}: GSUB lookups differ from the default master")]
    Gsub { master: usize },
    #[error("master {master}: feature definitions differ from the default master")]
    Features { master: usize },
    #[error("master {master}: GDEF Attach statements differ from the default master")]
    GdefAttach { master: usize },
    #[error(
        "master {master}: glyph {glyph} has GDEF class {found:?}, but {expected:?} in another master"
    )]
    GlyphClassConflict {
        master: usize,
        glyph: GlyphId16,
        expected: GlyphClassDef,
        found: GlyphClassDef,
    },
    #[error(
        "master {master}: glyph {glyph} is in mark class '{found}', but '{expected}' in another master"
    )]
    MarkClassConflict {
        master: usize,
        glyph: GlyphId16,
        expected: SmolStr,
        found: SmolStr,
    },
    //TODO: remove once GPOS lookups are merged
    #[error("master {master}: GPOS lookups differ from the default master (not yet supported)")]
    Gpos { master: usize },
    //TODO: remove once ligature carets are merged
    #[error(
        "master {master}: GDEF ligature carets differ from the default master (not yet supported)"
    )]
    LigatureCarets { master: usize },
}
