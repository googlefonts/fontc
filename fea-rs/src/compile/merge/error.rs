//! Errors from merging per-master compilations.

use std::fmt;

use smol_str::SmolStr;
use write_fonts::{tables::gdef::GlyphClassDef, types::GlyphId16};

use super::super::FeatureKey;
use crate::Kind;

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
    #[error(
        "master {master}: {found} GPOS lookups, but the default master has {expected}. \
         Inline values in contextual rules create anonymous lookups, so this can be \
         caused by values that differ between masters"
    )]
    LookupCount {
        master: usize,
        expected: usize,
        found: usize,
    },
    #[error("master {master}: {lookup} is a different kind of lookup than in the default master")]
    LookupType { master: usize, lookup: LookupRef },
    #[error("master {master}: {lookup} has different lookupflags than in the default master")]
    LookupFlags { master: usize, lookup: LookupRef },
    #[error(
        "master {master}: {lookup} does not use extension lookups the way the default master does"
    )]
    Extension { master: usize, lookup: LookupRef },
    #[error(
        "master {master}: {lookup} has {found} subtables, but the default master has {expected}"
    )]
    SubtableCount {
        master: usize,
        lookup: LookupRef,
        expected: usize,
        found: usize,
    },
    #[error(
        "master {master}: {lookup} is a contextual lookup that differs from the default master"
    )]
    ContextualDiffers { master: usize, lookup: LookupRef },
    #[error("{lookup}: a value is present in some masters but missing at the default master")]
    MissingAtDefault { lookup: LookupRef },
    #[error(
        "{lookup}: an explicit device table differs between masters; device tables cannot be interpolated"
    )]
    DeviceDiffers { lookup: LookupRef },
    #[error(
        "{lookup}: an anchor with a contour point differs between masters; such anchors cannot vary"
    )]
    AnchorPoint { lookup: LookupRef },
    #[error("{lookup}: failed to compute deltas: {message}")]
    Deltas { lookup: LookupRef, message: String },
    //TODO: remove once every lookup type can be merged
    #[error("{lookup} differs between masters, and merging {kind} lookups is not supported yet")]
    Unsupported { lookup: LookupRef, kind: Kind },
    //TODO: remove once ligature carets are merged
    #[error(
        "master {master}: GDEF ligature carets differ from the default master (not yet supported)"
    )]
    LigatureCarets { master: usize },
}

/// Identifies a lookup in a [`MergeError`].
#[derive(Debug, Clone, PartialEq)]
pub struct LookupRef {
    /// The position in the GPOS lookup list.
    pub index: usize,
    /// The lookup's name, if it came from a named lookup block.
    pub name: Option<SmolStr>,
    /// A feature that references this lookup, if any does.
    pub feature: Option<FeatureKey>,
}

impl fmt::Display for LookupRef {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "GPOS lookup {}", self.index)?;
        if let Some(name) = &self.name {
            write!(f, " ('{name}')")?;
        }
        if let Some(feature) = &self.feature {
            write!(f, " in feature {feature:?}")?;
        }
        Ok(())
    }
}
