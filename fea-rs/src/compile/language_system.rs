//! helpers for managing tracking language systems

use std::{collections::HashSet, rc::Rc};

use write_fonts::types::Tag;

use super::{lookups::FeatureKey, tags};

/// A script/language pair
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
#[allow(missing_docs)]
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
    // this is me being fancy, because we clone this everytime we start a lookup.
    items: Rc<HashSet<LanguageSystem>>,
}

impl DefaultLanguageSystems {
    pub(crate) fn insert(&mut self, system: LanguageSystem) {
        if !self.has_explicit_entry {
            Rc::get_mut(&mut self.items).unwrap().clear();
            self.has_explicit_entry = true;
        }
        Rc::get_mut(&mut self.items).unwrap().insert(system);
    }

    pub(crate) fn contains(&self, key: &LanguageSystem) -> bool {
        self.items.contains(key)
    }

    pub fn iter(&self) -> impl Iterator<Item = LanguageSystem> + '_ {
        self.items.iter().copied()
    }
}

impl LanguageSystem {
    /// Generate a `FeatureKey` for this langauge system.
    pub fn to_feature_key(self, feature: Tag) -> FeatureKey {
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
            script: tags::SCRIPT_DFLT,
            language: tags::LANG_DFLT,
        }
    }
}

impl Default for DefaultLanguageSystems {
    fn default() -> Self {
        Self {
            has_explicit_entry: false,
            items: Rc::new(HashSet::from_iter([LanguageSystem::default()])),
        }
    }
}
