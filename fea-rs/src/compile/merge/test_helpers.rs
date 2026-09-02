//! Shared helpers for the merge tests.

use fontdrasil::{coords::NormalizedLocation, types::GlyphName};

use super::{MergeError, merge};
use crate::{
    GlyphMap,
    compile::{self, MockVariationInfo, NopFeatureProvider, Opts, PendingCompilation},
    parse,
};

pub(super) fn glyph_map() -> GlyphMap {
    GlyphMap::new(
        [".notdef", "a", "b", "c", "f_i", "acute", "grave"]
            .iter()
            .map(|name| GlyphName::new(*name)),
    )
    .unwrap()
}

pub(super) fn var_info() -> MockVariationInfo {
    MockVariationInfo::new(&[("wght", 100, 400, 900)])
}

pub(super) fn location(wght: f64) -> NormalizedLocation {
    NormalizedLocation::for_pos(&[("wght", wght)])
}

pub(super) fn pending(fea: &str) -> PendingCompilation {
    let (tree, diagnostics) = parse::parse_string(fea);
    assert!(!diagnostics.has_errors(), "{}", diagnostics.display());
    compile::compile_for_merge(&tree, &glyph_map(), Opts::new()).unwrap()
}

pub(super) fn merge_masters(feas: &[&str]) -> Result<PendingCompilation, MergeError> {
    let masters = feas
        .iter()
        .enumerate()
        .map(|(i, fea)| (location(i as f64 / feas.len() as f64), pending(fea)))
        .collect();
    merge(masters, &var_info())
}

pub(super) fn merged_binary(feas: &[&str]) -> Vec<u8> {
    merge_masters(feas)
        .unwrap()
        .finish::<NopFeatureProvider>(None)
        .unwrap()
        .0
        .to_binary(&glyph_map())
        .unwrap()
}

pub(super) fn one_shot_binary(fea: &str) -> Vec<u8> {
    let (tree, _) = parse::parse_string(fea);
    let (compilation, _) = compile::compile(
        &tree,
        &glyph_map(),
        Some(&var_info()),
        None::<&NopFeatureProvider>,
        Opts::new(),
    )
    .unwrap();
    compilation.to_binary(&glyph_map()).unwrap()
}

pub(super) const KITCHEN_SINK: &str = "\
languagesystem DFLT dflt;
languagesystem latn dflt;
markClass acute <anchor 100 200> @TOP;
markClass grave <anchor 100 200> @TOP;
table GDEF {
GlyphClassDef [a b], [f_i], [acute grave], ;
LigatureCaretByPos f_i 300;
} GDEF;
lookup MARKS {
pos base [a b] <anchor 150 500> mark @TOP;
} MARKS;
feature liga {
sub a b by f_i;
} liga;
feature kern {
pos a b -20;
pos a c <-10 0 -10 0>;
} kern;
feature mark {
lookup MARKS;
} mark;
";
