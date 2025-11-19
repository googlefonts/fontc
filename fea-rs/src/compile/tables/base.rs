//! The BASE table

use std::collections::HashMap;

use write_fonts::{
    tables::base::{self as write_base, BaseLangSysRecord},
    types::Tag,
};

/// Baseline tags, used in the BASE table.
///
/// These are guaranteed to be sorted lexicographically.
///
/// Baseline tags are used in the BASE table to provide additional font metric
/// values that may apply to particular scripts or usage contexts.
///
/// See <https://learn.microsoft.com/en-us/typography/opentype/spec/baselinetags>
pub static BASELINE_TAGS: &[Tag] = &[HANG, ICFB, ICFT, IDEO, IDTP, MATH, ROMN];

/// The hanging baseline.
///
/// This is the horizontal line from which syllables seem to hang in Tibetan
/// and other similar scripts.
pub const HANG: Tag = Tag::new(b"hang");
/// Ideographic character face bottom edge.
pub const ICFB: Tag = Tag::new(b"icfb");
/// Ideographic character face top edge.
pub const ICFT: Tag = Tag::new(b"icft");
/// Ideographic em-box bottom edge.
pub const IDEO: Tag = Tag::new(b"ideo");
/// Ideographic em-box top edge.
pub const IDTP: Tag = Tag::new(b"idtp");
/// The baseline about which characters in mathematical formulas are centered.
pub const MATH: Tag = Tag::new(b"math");
/// The baseline used by alphabetic scripts such as Latin, Cyrillic and Greek.
pub const ROMN: Tag = Tag::new(b"romn");

#[derive(Clone, Debug, Default)]
pub(crate) struct BaseBuilder {
    vert: BaseAxisBuilder,
    horiz: BaseAxisBuilder,
}

#[derive(Clone, Debug, Default)]
pub(crate) struct BaseAxisBuilder {
    tag_list: Vec<Tag>,
    script_list: Vec<ScriptRecord>,
    min_max: HashMap<Tag, Vec<(Tag, MinMax)>>,
}

#[derive(Clone, Debug)]
pub(crate) struct ScriptRecord {
    pub script: Tag,
    pub default_baseline_tag: Tag,
    pub values: Vec<i16>,
}

#[derive(Clone, Debug)]
pub(crate) struct MinMax {
    pub min: i16,
    pub max: i16,
    // we don't implement per-feature minmax, matching fonttools
    //pub _features: Vec<FeatMinMax>,
}

#[derive(Clone, Debug)]
#[expect(dead_code, reason = "not yet implemented")]
pub(crate) struct FeatMinMax {
    pub feature: Tag,
    pub min: i16,
    pub max: i16,
}

impl BaseAxisBuilder {
    pub(crate) fn new(
        mut tag_list: Vec<Tag>,
        mut script_list: Vec<ScriptRecord>,
        minmax: Vec<(Tag, Tag, (i16, i16))>,
    ) -> Self {
        tag_list.sort_unstable();
        script_list.sort_unstable_by_key(|rec| rec.script);
        let mut minmaxmap = HashMap::new();
        for (script, lang, (min, max)) in minmax {
            minmaxmap
                .entry(script)
                .or_insert(Vec::new())
                .push((lang, MinMax { min, max }));
        }

        Self {
            tag_list,
            script_list,
            min_max: minmaxmap,
        }
    }

    fn build(&self) -> Option<write_base::Axis> {
        if self.tag_list.is_empty() {
            return None;
        }

        // a little helper fn
        fn get_dflt_and_lang_sys_minmax(
            minmax: Option<&Vec<(Tag, MinMax)>>,
        ) -> (Option<write_base::MinMax>, Vec<BaseLangSysRecord>) {
            let mut dflt = None;
            let mut rest = Vec::new();

            for (tag, table) in minmax.into_iter().flatten() {
                if *tag == Tag::new(b"dflt") {
                    dflt = Some(table.compile());
                } else {
                    rest.push(BaseLangSysRecord::new(*tag, table.compile()));
                }
            }
            rest.sort_unstable_by_key(|rec| rec.base_lang_sys_tag);

            (dflt, rest)
        }

        let records = self.script_list.iter().map(|rec| {
            let minmax = self.min_max.get(&rec.script);
            let (default_min_max, base_lang_sys_records) = get_dflt_and_lang_sys_minmax(minmax);

            write_base::BaseScriptRecord::new(
                rec.script,
                write_base::BaseScript::new(
                    Some(write_base::BaseValues::new(
                        self.tag_list
                            .iter()
                            .position(|x| *x == rec.default_baseline_tag)
                            .expect("validated") as _,
                        rec.values
                            .iter()
                            .map(|coord| write_base::BaseCoord::format_1(*coord))
                            .collect(),
                    )),
                    default_min_max,
                    base_lang_sys_records,
                ),
            )
        });
        Some(write_base::Axis::new(
            Some(write_base::BaseTagList::new(self.tag_list.clone())),
            write_base::BaseScriptList::new(records.collect()),
        ))
    }
}

impl BaseBuilder {
    pub(crate) fn new(horiz: BaseAxisBuilder, vert: BaseAxisBuilder) -> Self {
        Self { horiz, vert }
    }

    pub(crate) fn build(&self) -> write_base::Base {
        let horiz = self.horiz.build();
        let vert = self.vert.build();
        write_base::Base::new(horiz, vert)
    }
}

impl MinMax {
    fn compile(&self) -> write_base::MinMax {
        write_base::MinMax::new(
            Some(write_base::BaseCoord::format_1(self.min)),
            Some(write_base::BaseCoord::format_1(self.max)),
            Vec::new(),
        )
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn base_tags_sorted() {
        assert!(BASELINE_TAGS.is_sorted());
    }
}
