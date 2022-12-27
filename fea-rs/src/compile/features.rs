//! Logic for tracking features during compilation

use std::collections::{HashMap, HashSet};

use write_fonts::{
    tables::layout::SizeParams,
    types::{GlyphId, Tag},
};

use super::tables::{NameBuilder, NameSpec};

/// State required to generate the aalt feature.
///
/// This is a special and annoying case. We create this object when we encounter
/// the aalt feature block, and then we use this to generate the aalt lookups
/// once we've finished processing the input.
#[derive(Clone, Debug, Default)]
pub(crate) struct AaltFeature {
    aalt_features: Vec<Tag>,
    pub(crate) all_alts: HashMap<GlyphId, Vec<GlyphId>>,
    // to avoid duplicates
    all_pairs: HashSet<(GlyphId, GlyphId)>,
}

/// Helper for compiling the `size` feature
#[derive(Clone, Debug, Default)]
pub(crate) struct SizeFeature {
    pub design_size: u16,
    pub identifier: u16,
    pub range_start: u16,
    pub range_end: u16,
    pub names: Vec<NameSpec>,
}

/// If we are at the root of one of four magic features, we have special behaviour.
#[derive(Clone, Copy, Debug, Default, PartialEq, Eq)]
pub(crate) enum SpecialVerticalFeatureState {
    /// we are not in a special vertical feature
    #[default]
    Ready,
    /// we are at the root of a special vertical feature (and so should behave specially)
    Root,
    /// we are inside a lookup in a special vertical feature (and so should not
    /// behave specially)
    InnerLookup,
}

impl SizeFeature {
    pub(crate) fn build(&self, names: &mut NameBuilder) -> SizeParams {
        let name_entry = if self.identifier == 0 {
            assert!(self.names.is_empty());
            0
        } else {
            assert!(!self.names.is_empty());
            names.add_anon_group(&self.names)
        };
        SizeParams {
            design_size: self.design_size,
            identifier: self.identifier,
            name_entry,
            range_start: self.range_start,
            range_end: self.range_end,
        }
    }
}

impl AaltFeature {
    pub(crate) fn add_feature_reference(&mut self, feature: Tag) {
        self.aalt_features.push(feature);
    }

    pub(crate) fn features(&self) -> &[Tag] {
        &self.aalt_features
    }

    pub(crate) fn add(&mut self, target: GlyphId, alt: GlyphId) {
        if self.all_pairs.insert((target, alt)) {
            self.all_alts.entry(target).or_default().push(alt);
        }
    }
}

impl Extend<(GlyphId, GlyphId)> for AaltFeature {
    fn extend<T: IntoIterator<Item = (GlyphId, GlyphId)>>(&mut self, iter: T) {
        for (target, alt) in iter.into_iter() {
            self.add(target, alt)
        }
    }
}

impl SpecialVerticalFeatureState {
    const VERTICAL_FEATURES: &[Tag] = &[
        Tag::new(b"valt"),
        Tag::new(b"vhal"),
        Tag::new(b"vkrn"),
        Tag::new(b"vpal"),
    ];

    pub(crate) fn begin_feature(&mut self, tag: Tag) {
        if Self::VERTICAL_FEATURES.contains(&tag) {
            *self = Self::Root;
        }
    }

    pub(crate) fn end_feature(&mut self) {
        *self = Self::Ready;
    }

    pub(crate) fn begin_lookup_block(&mut self) {
        if *self == Self::Root {
            *self = Self::InnerLookup;
        }
    }

    pub(crate) fn end_lookup_block(&mut self) {
        if *self == Self::InnerLookup {
            *self = Self::Root;
        }
    }

    pub(crate) fn in_eligible_vertical_feature(&self) -> bool {
        *self == Self::Root
    }
}
