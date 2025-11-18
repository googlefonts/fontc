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
    pub horiz_tag_list: Vec<Tag>,
    pub horiz_script_list: Vec<ScriptRecord>,
    // maps script -> (lang, minmax). lang can be dflt.
    pub horiz_min_max: HashMap<Tag, Vec<(Tag, MinMax)>>,
    pub vert_tag_list: Vec<Tag>,
    pub vert_script_list: Vec<ScriptRecord>,
    pub vert_min_max: HashMap<Tag, Vec<(Tag, MinMax)>>,
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

impl BaseBuilder {
    pub(crate) fn build(&self) -> write_base::Base {
        let mut result = write_base::Base::default();
        if !self.horiz_tag_list.is_empty() {
            assert!(!self.horiz_script_list.is_empty(), "validate this");
            let haxis = BaseBuilder::build_axis(
                &self.horiz_tag_list,
                &self.horiz_script_list,
                &self.horiz_min_max,
            );
            result.horiz_axis = haxis.into();
        }
        if !self.vert_tag_list.is_empty() && !self.vert_script_list.is_empty() {
            assert!(!self.vert_script_list.is_empty(), "validate this");
            let vaxis = BaseBuilder::build_axis(
                &self.vert_tag_list,
                &self.vert_script_list,
                &self.vert_min_max,
            );
            result.vert_axis = vaxis.into();
        }
        result
    }

    fn build_axis(
        tag_list: &[Tag],
        script_list: &[ScriptRecord],
        minmax: &HashMap<Tag, Vec<(Tag, MinMax)>>,
    ) -> write_base::Axis {
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

            (dflt, rest)
        }

        let records = script_list.iter().map(|rec| {
            let minmax = minmax.get(&rec.script);
            let (default_min_max, base_lang_sys_records) = get_dflt_and_lang_sys_minmax(minmax);

            write_base::BaseScriptRecord::new(
                rec.script,
                write_base::BaseScript::new(
                    Some(write_base::BaseValues::new(
                        tag_list
                            .iter()
                            .position(|x| *x == rec.default_baseline_tag)
                            .expect("validate this") as _,
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
        write_base::Axis::new(
            Some(write_base::BaseTagList::new(tag_list.to_owned())),
            write_base::BaseScriptList::new(records.collect()),
        )
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
