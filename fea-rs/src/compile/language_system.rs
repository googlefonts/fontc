//! helpers for managing tracking language systems

use std::collections::HashSet;

use write_fonts::types::Tag;

use super::{common, lookups::FeatureKey};

/// A script/language pair
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct LanguageSystem {
    pub script: Tag,
    pub language: Tag,
}

/// Track languagesystem statements
///
/// Seeing no statements is the same as seeing 'DFLT dflt'.
#[derive(Clone, Debug)]
pub(crate) struct DefaultLanguageSystems {
    has_explicit_entry: bool,
    items: HashSet<LanguageSystem>,
}

impl DefaultLanguageSystems {
    pub(crate) fn insert(&mut self, system: LanguageSystem) {
        if !self.has_explicit_entry {
            self.items.clear();
            self.has_explicit_entry = true;
        }
        self.items.insert(system);
    }

    pub(crate) fn iter(&self) -> impl Iterator<Item = LanguageSystem> + '_ {
        self.items.iter().copied()
    }
}

impl LanguageSystem {
    pub(crate) fn to_feature_key(self, feature: Tag) -> FeatureKey {
        let LanguageSystem { script, language } = self;
        FeatureKey {
            feature,
            language,
            script,
        }
    }
}

impl Default for LanguageSystem {
    fn default() -> Self {
        Self {
            script: common::tags::SCRIPT_DFLT,
            language: common::tags::LANG_DFLT,
        }
    }
}

impl Default for DefaultLanguageSystems {
    fn default() -> Self {
        Self {
            has_explicit_entry: false,
            items: HashSet::from_iter([LanguageSystem::default()]),
        }
    }
}
