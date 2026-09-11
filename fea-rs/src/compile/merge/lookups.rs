//! Aligning and merging GPOS lookups across masters.

use write_fonts::tables::layout::{LookupFlag, builders::LookupBuilder};

use super::{LookupRef, MergeCtx, MergeError};
use crate::{
    Kind,
    compile::{
        LookupId, VariationInfo,
        lookups::{FilterSetId, PositionLookup},
    },
};

impl<V: VariationInfo> MergeCtx<'_, V> {
    pub(super) fn merge_gpos(&mut self) -> Result<(), MergeError> {
        let expected = self.merged.lookups.gpos().len();
        for (i, other) in self.others.iter().enumerate() {
            let found = other.lookups.gpos().len();
            if found != expected {
                return Err(MergeError::LookupCount {
                    master: i + 1,
                    expected,
                    found,
                });
            }
        }
        let gpos = (0..expected)
            .map(|index| {
                let lookups: Vec<_> = std::iter::once(&self.merged)
                    .chain(&self.others)
                    .map(|master| &master.lookups.gpos()[index])
                    .collect();
                merge_lookup(&lookups, index, self)
            })
            .collect::<Result<Vec<_>, _>>()?;
        self.merged.lookups.set_gpos(gpos);
        Ok(())
    }

    pub(super) fn lookup_ref(&self, index: usize) -> LookupRef {
        let id = LookupId::Gpos(index);
        let name = self
            .merged
            .lookups
            .named()
            .iter()
            .find_map(|(name, named_id)| (*named_id == id).then(|| name.clone()));
        let feature = self
            .merged
            .features
            .iter()
            .find_map(|(key, lookups)| lookups.base.contains(&id).then_some(*key));
        LookupRef {
            index,
            name,
            feature,
        }
    }

    pub(super) fn unsupported(&self, index: usize, kind: Kind) -> MergeError {
        MergeError::Unsupported {
            lookup: self.lookup_ref(index),
            kind,
        }
    }

    fn contextual_differs(&self, inner: &[&PositionLookup], index: usize) -> MergeError {
        let master = inner
            .iter()
            .position(|lookup| *lookup != inner[0])
            .unwrap_or_default();
        MergeError::ContextualDiffers {
            master,
            lookup: self.lookup_ref(index),
        }
    }
}

/// Merge one lookup across all masters.
///
/// `lookups` has one entry per master, the default first.
fn merge_lookup<V: VariationInfo>(
    lookups: &[&PositionLookup],
    index: usize,
    ctx: &MergeCtx<'_, V>,
) -> Result<PositionLookup, MergeError> {
    if lookups.iter().all(|lookup| *lookup == lookups[0]) {
        return Ok(lookups[0].clone());
    }

    let use_extension = matches!(lookups[0], PositionLookup::Extension(_));
    let mut inner = Vec::with_capacity(lookups.len());
    for (master, lookup) in lookups.iter().enumerate() {
        match (use_extension, lookup) {
            (true, PositionLookup::Extension(wrapped)) => inner.push(&**wrapped),
            (false, PositionLookup::Extension(_)) | (true, _) => {
                return Err(MergeError::Extension {
                    master,
                    lookup: ctx.lookup_ref(index),
                });
            }
            (false, other) => inner.push(*other),
        }
    }

    let kind = inner[0].kind();
    if let Some(master) = inner.iter().position(|lookup| lookup.kind() != kind) {
        return Err(MergeError::LookupType {
            master,
            lookup: ctx.lookup_ref(index),
        });
    }

    // view every master's lookup as the given variant, checking the headers agree
    macro_rules! aligned {
        ($variant:ident) => {
            align(&inner, index, ctx, |lookup| match lookup {
                PositionLookup::$variant(builder) => builder,
                _ => unreachable!("all lookups have the kind of inner[0], checked above"),
            })
        };
    }

    let merged = match inner[0] {
        PositionLookup::Single(_) => {
            PositionLookup::Single(ctx.merge_single_pos(aligned!(Single)?, index)?)
        }
        PositionLookup::Pair(_) => {
            PositionLookup::Pair(ctx.merge_pair_pos(aligned!(Pair)?, index)?)
        }
        PositionLookup::Cursive(_) => {
            PositionLookup::Cursive(merge_indexwise(aligned!(Cursive)?, index, kind, ctx)?)
        }
        PositionLookup::MarkToBase(_) => {
            PositionLookup::MarkToBase(merge_indexwise(aligned!(MarkToBase)?, index, kind, ctx)?)
        }
        PositionLookup::MarkToLig(_) => {
            PositionLookup::MarkToLig(merge_indexwise(aligned!(MarkToLig)?, index, kind, ctx)?)
        }
        PositionLookup::MarkToMark(_) => {
            PositionLookup::MarkToMark(merge_indexwise(aligned!(MarkToMark)?, index, kind, ctx)?)
        }
        PositionLookup::Contextual(_) => {
            aligned!(Contextual)?;
            return Err(ctx.contextual_differs(&inner, index));
        }
        PositionLookup::ChainedContextual(_) => {
            aligned!(ChainedContextual)?;
            return Err(ctx.contextual_differs(&inner, index));
        }
        PositionLookup::Extension(_) => unreachable!("unwrapped above"),
    };
    Ok(if use_extension {
        PositionLookup::Extension(Box::new(merged))
    } else {
        merged
    })
}

/// One lookup viewed across all masters, with the common header pulled out.
pub(super) struct AlignedLookup<'a, T> {
    flags: LookupFlag,
    mark_set: Option<FilterSetId>,
    /// The subtables of each master, the default first.
    pub(super) per_master: Vec<&'a [T]>,
}

/// View every master's lookup as one lookup type, checking that the headers agree.
///
/// `extract` pulls the builder for that type out of a `PositionLookup`; the
/// caller guarantees every lookup is of that type.
fn align<'a, T, V: VariationInfo>(
    lookups: &[&'a PositionLookup],
    index: usize,
    ctx: &MergeCtx<'_, V>,
    extract: impl Fn(&'a PositionLookup) -> &'a LookupBuilder<T>,
) -> Result<AlignedLookup<'a, T>, MergeError> {
    let builders: Vec<_> = lookups.iter().map(|lookup| extract(lookup)).collect();
    let first = builders[0];
    for (master, builder) in builders.iter().enumerate() {
        if builder.flags != first.flags || builder.mark_set != first.mark_set {
            return Err(MergeError::LookupFlags {
                master,
                lookup: ctx.lookup_ref(index),
            });
        }
    }
    Ok(AlignedLookup {
        flags: first.flags,
        mark_set: first.mark_set,
        per_master: builders
            .iter()
            .map(|builder| builder.subtables.as_slice())
            .collect(),
    })
}

impl<T> AlignedLookup<'_, T> {
    /// Merge subtable `i` of every master into subtable `i` of the result.
    ///
    /// For lookup types where matching is per subtable this is the only
    /// correct alignment, so differing subtable counts are an error.
    fn indexwise<V: VariationInfo>(
        &self,
        index: usize,
        ctx: &MergeCtx<'_, V>,
        merge_one: impl Fn(&[&T]) -> Result<T, MergeError>,
    ) -> Result<Vec<T>, MergeError> {
        let expected = self.per_master[0].len();
        for (master, subtables) in self.per_master.iter().enumerate() {
            if subtables.len() != expected {
                return Err(MergeError::SubtableCount {
                    master,
                    lookup: ctx.lookup_ref(index),
                    expected,
                    found: subtables.len(),
                });
            }
        }
        (0..expected)
            .map(|i| {
                let row: Vec<_> = self.per_master.iter().map(|subs| &subs[i]).collect();
                merge_one(&row)
            })
            .collect()
    }

    pub(super) fn build(self, subtables: Vec<T>) -> LookupBuilder<T> {
        LookupBuilder {
            flags: self.flags,
            mark_set: self.mark_set,
            subtables,
        }
    }
}

//TODO: replace with real per-type merging; for now identical subtables pass through
fn merge_indexwise<T: PartialEq + Clone, V: VariationInfo>(
    aligned: AlignedLookup<'_, T>,
    index: usize,
    kind: Kind,
    ctx: &MergeCtx<'_, V>,
) -> Result<LookupBuilder<T>, MergeError> {
    let subtables = aligned.indexwise(index, ctx, |row| {
        if row.iter().all(|subtable| *subtable == row[0]) {
            Ok(row[0].clone())
        } else {
            Err(ctx.unsupported(index, kind))
        }
    })?;
    Ok(aligned.build(subtables))
}

#[cfg(test)]
mod tests {
    use write_fonts::types::Tag;

    use super::super::{MergeError, test_helpers::*};
    use crate::Kind;

    #[test]
    fn value_divergence_is_unsupported_for_now() {
        let a = "markClass acute <anchor 0 0> @TOP; feature mkmk { pos mark grave <anchor 0 0> mark @TOP; } mkmk;";
        let b = "markClass acute <anchor 0 0> @TOP; feature mkmk { pos mark grave <anchor 0 5> mark @TOP; } mkmk;";
        assert_eq!(
            merge_masters(&[a, b]).err(),
            Some(MergeError::Unsupported {
                lookup: lookup_ref(0, None, Some(Tag::new(b"mkmk"))),
                kind: Kind::GposType6,
            })
        );
    }

    #[test]
    fn lookup_count_differs_via_anonymous_lookups() {
        // both rules position 'b', so the second value needs its own anonymous lookup
        let a = "feature kern { pos a b' 10 c; pos a b' 10 f_i; } kern;";
        let b = "feature kern { pos a b' 10 c; pos a b' 20 f_i; } kern;";
        assert_eq!(
            merge_masters(&[a, b]).err(),
            Some(MergeError::LookupCount {
                master: 1,
                expected: 2,
                found: 3,
            })
        );
    }

    #[test]
    fn lookup_type_differs() {
        let a = "feature kern { pos a b -20; } kern;";
        let b = "feature kern { pos a -20; } kern;";
        assert_eq!(
            merge_masters(&[a, b]).err(),
            Some(MergeError::LookupType {
                master: 1,
                lookup: lookup_ref(0, None, Some(Tag::new(b"kern"))),
            })
        );
    }

    #[test]
    fn lookup_flags_differ() {
        let a = "feature kern { lookupflag IgnoreMarks; pos a b -20; } kern;";
        let b = "feature kern { pos a b -20; } kern;";
        assert_eq!(
            merge_masters(&[a, b]).err(),
            Some(MergeError::LookupFlags {
                master: 1,
                lookup: lookup_ref(0, None, Some(Tag::new(b"kern"))),
            })
        );
    }

    #[test]
    fn extension_differs() {
        let a = "lookup K useExtension { pos a b -20; } K; feature kern { lookup K; } kern;";
        let b = "lookup K { pos a b -20; } K; feature kern { lookup K; } kern;";
        assert_eq!(
            merge_masters(&[a, b]).err(),
            Some(MergeError::Extension {
                master: 1,
                lookup: lookup_ref(0, Some("K"), Some(Tag::new(b"kern"))),
            })
        );
    }

    #[test]
    fn contextual_differs() {
        let a = "feature calt { pos a b' 10 c; } calt;";
        let b = "feature calt { pos a b' 10 f_i; } calt;";
        assert_eq!(
            merge_masters(&[a, b]).err(),
            Some(MergeError::ContextualDiffers {
                master: 1,
                lookup: lookup_ref(0, None, Some(Tag::new(b"calt"))),
            })
        );
    }

    #[test]
    fn subtable_count_differs() {
        let a = "markClass acute <anchor 0 0> @TOP; feature mark { pos base a <anchor 0 0> mark @TOP; subtable; pos base b <anchor 0 0> mark @TOP; } mark;";
        let b = "markClass acute <anchor 0 0> @TOP; feature mark { pos base a <anchor 0 0> mark @TOP; pos base b <anchor 0 0> mark @TOP; } mark;";
        assert_eq!(
            merge_masters(&[a, b]).err(),
            Some(MergeError::SubtableCount {
                master: 1,
                lookup: lookup_ref(0, None, Some(Tag::new(b"mark"))),
                expected: 2,
                found: 1,
            })
        );
    }

    #[test]
    fn lookup_ref_display() {
        assert_eq!(
            lookup_ref(3, Some("K"), Some(Tag::new(b"kern"))).to_string(),
            "GPOS lookup 3 ('K') in feature kern: DFLT/dflt"
        );
        assert_eq!(lookup_ref(1, None, None).to_string(), "GPOS lookup 1");
    }
}
