//! Normalized repreentation of GSUB lookups

use std::{
    collections::{HashMap, HashSet},
    io,
};

use write_fonts::{
    read::{
        tables::{
            gdef::Gdef,
            gsub::{
                Gsub, MultipleSubstFormat1, SingleSubst, SingleSubstFormat1, SingleSubstFormat2,
                SubstitutionLookupList, SubstitutionSubtables,
            },
        },
        ReadError,
    },
    types::GlyphId,
};

use crate::{
    common::{self, Lookup, PrintNames, SingleRule},
    error::Error,
    glyph_names::NameMap,
};

/// Print normalized GSUB layout rules
pub fn print(
    f: &mut dyn io::Write,
    table: &Gsub,
    gdef: Option<&Gdef>,
    names: &NameMap,
) -> Result<(), Error> {
    let mark_glyph_sets = gdef
        .and_then(|gdef| gdef.mark_glyph_sets_def())
        .transpose()
        .unwrap();

    let script_list = table.script_list().unwrap();
    let feature_list = table.feature_list().unwrap();
    let lang_systems = common::get_lang_systems(&script_list, &feature_list);
    let lookup_rules = LookupRules::for_lookups(&table.lookup_list().unwrap());

    for sys in &lang_systems {
        writeln!(f,)?;
        sys.fmt_header(f)?;

        let singlesub = lookup_rules.singlesub_rules(&sys.lookups);
        let multisub = lookup_rules.multisub_rules(&sys.lookups);
        common::print_rules(f, "SingleSub", &singlesub, names, mark_glyph_sets.as_ref())?;
        common::print_rules(f, "MultiSub", &multisub, names, mark_glyph_sets.as_ref())?;
    }

    Ok(())
}

#[derive(Clone, Debug, Default)]
struct LookupRules {
    singlesub: Vec<Lookup<SingleSubRule>>,
    multisub: Vec<Lookup<MultiSubRule>>,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
struct SingleSubRule {
    target: GlyphId,
    replacement: GlyphId,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
struct MultiSubRule {
    target: GlyphId,
    replacement: Vec<GlyphId>,
}

impl LookupRules {
    fn for_lookups(lookups: &SubstitutionLookupList) -> Self {
        let mut result = Self::default();
        for (id, lookup) in lookups.lookups().iter().enumerate() {
            let lookup = lookup.unwrap();
            let flag = lookup.lookup_flag();
            let mark_filter_id = flag
                .use_mark_filtering_set()
                .then_some(lookup.mark_filtering_set());
            let subtables = lookup.subtables().unwrap();

            match subtables {
                SubstitutionSubtables::Single(subs) => {
                    let subs = subs.iter().flat_map(|sub| sub.ok()).collect::<Vec<_>>();
                    let rules = get_singlesub_rules(&subs).unwrap();
                    result
                        .singlesub
                        .push(Lookup::new(id, rules, flag, mark_filter_id));
                }
                SubstitutionSubtables::Multiple(subs) => {
                    let subs = subs.iter().flat_map(|sub| sub.ok()).collect::<Vec<_>>();
                    let rules = get_multisub_rules(&subs).unwrap();
                    result
                        .multisub
                        .push(Lookup::new(id, rules, flag, mark_filter_id));
                }
                _ => (),
            }
        }
        result
    }

    fn singlesub_rules<'a>(&'a self, lookups: &[u16]) -> Vec<SingleRule<'a, SingleSubRule>> {
        // so... we want to normalize these. specifically, we want to track the 'final output
        // glyph' for any given input glyph.
        //
        // this means we need to track something like "routes to glyph"?
        // because say we have,
        //
        // a -> b
        // b -> c
        // c -> d
        //
        // we want to produce a final rule, a -> d.
        //
        // this means that we want to store our rules keyed on their previous
        // replacement glyph, since we will update these rules if they are impacted
        // by subsequent substitutions.
        //
        // NOTE: we *don't* want to combine things within the *same* lookup!
        let mut prev_lookup_rules = HashMap::<_, Vec<_>>::new();
        let mut current = Vec::new();

        for lookup in self
            .singlesub
            .iter()
            .filter(|lookup| lookups.contains(&lookup.lookup_id))
        {
            for rule in lookup.iter() {
                let target = rule.rule().target;
                let replacement = rule.rule().replacement;
                // check if we need to update any previous rules, and move them
                // into the 'current' batch:
                current.extend(prev_lookup_rules.remove(&target).into_iter().flat_map(|x| {
                    x.into_iter().map(|mut r: SingleRule<'a, SingleSubRule>| {
                        r.rule_mut().replacement = replacement;
                        r
                    })
                }));
                current.push(rule);
            }
            // now we're done this lookup, so move the rules into the main
            // list so that they can be modified by subsequent lookups if needed:
            for rule in current.drain(..) {
                prev_lookup_rules
                    .entry(rule.rule().replacement)
                    .or_default()
                    .push(rule);
            }
        }

        // now combine them all, sort, and dedup
        let mut all_rules: Vec<_> = prev_lookup_rules
            .into_values()
            .flat_map(Vec::into_iter)
            .collect();
        all_rules.sort_unstable();
        all_rules.dedup();
        all_rules
    }

    fn multisub_rules<'a>(&'a self, lookups: &[u16]) -> Vec<SingleRule<'a, MultiSubRule>> {
        // normalization here is very simple: we're just going to do first-writer-wins
        // on targets.
        let mut seen = HashSet::new();
        let mut result = self
            .multisub
            .iter()
            .filter(|lookup| lookups.contains(&lookup.lookup_id))
            .flat_map(|lookup| lookup.iter())
            .filter(|rule| seen.insert(rule.rule().target))
            .collect::<Vec<_>>();
        result.sort_unstable();
        result.dedup();
        result
    }
}

fn get_singlesub_rules(subtables: &[SingleSubst]) -> Result<Vec<SingleSubRule>, ReadError> {
    // so we only take the first coverage hit in each subtable, which means
    // we just need track what we've seen.
    let mut seen = HashSet::new();
    let mut result = Vec::new();
    for sub in subtables {
        match sub {
            SingleSubst::Format1(sub) => append_singlesub_f1_rules(sub, &mut seen, &mut result),
            SingleSubst::Format2(sub) => append_singlesub_f2_rules(sub, &mut seen, &mut result),
        }
    }
    Ok(result)
}

fn append_singlesub_f1_rules(
    subtable: &SingleSubstFormat1,
    seen: &mut HashSet<GlyphId>,
    result: &mut Vec<SingleSubRule>,
) {
    let coverage = subtable.coverage().unwrap();
    let delta = subtable.delta_glyph_id() as i32;
    for target in coverage.iter() {
        let out = (target.to_u16() as i32) + delta;
        let Ok(out) = u16::try_from(out) else {
            // bad data is possible, ignore
            continue;
        };

        if seen.insert(target) {
            result.push(SingleSubRule {
                target,
                replacement: GlyphId::new(out),
            })
        }
    }
}

fn append_singlesub_f2_rules(
    subtable: &SingleSubstFormat2,
    seen: &mut HashSet<GlyphId>,
    result: &mut Vec<SingleSubRule>,
) {
    let coverage = subtable.coverage().unwrap();
    for (target, replacement) in coverage.iter().zip(subtable.substitute_glyph_ids().iter()) {
        if seen.insert(target) {
            result.push(SingleSubRule {
                target,
                replacement: replacement.get(),
            })
        }
    }
}

fn get_multisub_rules(subtables: &[MultipleSubstFormat1]) -> Result<Vec<MultiSubRule>, ReadError> {
    let mut result = Vec::new();
    let mut seen = HashSet::new();

    for sub in subtables {
        let coverage = sub.coverage()?;
        let replacements = sub.sequences();
        for (target, sequence) in coverage.iter().zip(replacements.iter()) {
            let sequence = sequence?;
            if seen.insert(target) {
                result.push(MultiSubRule {
                    target,
                    replacement: sequence
                        .substitute_glyph_ids()
                        .iter()
                        .map(|gid| gid.get())
                        .collect(),
                });
            }
        }
    }

    Ok(result)
}

impl PrintNames for SingleSubRule {
    fn fmt_names(&self, f: &mut std::fmt::Formatter<'_>, names: &NameMap) -> std::fmt::Result {
        let g1 = names.get(self.target);
        let g2 = names.get(self.replacement);
        write!(f, "{g1} -> {g2}")
    }
}

impl PrintNames for MultiSubRule {
    fn fmt_names(&self, f: &mut std::fmt::Formatter<'_>, names: &NameMap) -> std::fmt::Result {
        let g1 = names.get(self.target);
        write!(f, "{g1} -> [")?;
        for (i, g2) in self.replacement.iter().enumerate() {
            if i > 0 {
                write!(f, ",")?;
            }
            let g2 = names.get(*g2);
            write!(f, "{g2}")?;
        }
        write!(f, "]")
    }
}

#[cfg(test)]
mod tests {
    use fea_rs::compile::{Builder, SingleSubBuilder};
    use write_fonts::{
        read::{tables::gsub as rgsub, FontRead},
        tables::{
            gsub as wgsub, layout as wlayout,
            layout::{Lookup, LookupFlag},
            variations::ivs_builder::VariationStoreBuilder,
        },
    };

    use super::*;

    fn get_subtable_format(lookup: &rgsub::SubstitutionLookup, idx: usize) -> Option<u16> {
        match lookup.subtables().unwrap() {
            SubstitutionSubtables::Single(subs) => subs.get(idx).ok().map(|x| x.subst_format()),
            SubstitutionSubtables::Multiple(subs) => subs.get(idx).ok().map(|_| 1),
            SubstitutionSubtables::Alternate(subs) => subs.get(idx).ok().map(|_| 1),
            SubstitutionSubtables::Ligature(subs) => subs.get(idx).ok().map(|_| 1),
            _ => None,
        }
    }

    #[test]
    fn single_sub_combination() {
        let mut lookup1 = SingleSubBuilder::default();
        lookup1.insert(GlyphId::new(2), GlyphId::new(5));

        let mut lookup2 = SingleSubBuilder::default();
        lookup2.insert(GlyphId::new(5), GlyphId::new(6));
        // force this to be a format2 lookup
        lookup2.insert(GlyphId::new(3), GlyphId::new(5));

        let mut varstore = VariationStoreBuilder::new(0);

        let lookup1 = lookup1.build(&mut varstore);
        let lookup2 = lookup2.build(&mut varstore);
        let lookup1 =
            wgsub::SubstitutionLookup::Single(Lookup::new(LookupFlag::empty(), lookup1, 0));
        let lookup2 =
            wgsub::SubstitutionLookup::Single(Lookup::new(LookupFlag::empty(), lookup2, 0));
        let lookup_list = wlayout::LookupList::new(vec![lookup1, lookup2]);
        let lookup_list = write_fonts::dump_table(&lookup_list).unwrap();
        let lookup_list =
            rgsub::SubstitutionLookupList::read(lookup_list.as_slice().into()).unwrap();

        // sanity check that we have the subtable formats we expect
        assert_eq!(
            get_subtable_format(&lookup_list.lookups().get(0).unwrap(), 0),
            Some(1)
        );
        assert_eq!(
            get_subtable_format(&lookup_list.lookups().get(1).unwrap(), 0),
            Some(2)
        );
        let rules = LookupRules::for_lookups(&lookup_list);
        let rules = rules.singlesub_rules(&[0, 1]);
        let rules = rules
            .iter()
            .map(|r| (r.rule().target.to_u16(), r.rule().replacement.to_u16()))
            .collect::<Vec<_>>();

        assert_eq!(rules, &[(2, 6), (3, 5), (5, 6)]);
    }
}
