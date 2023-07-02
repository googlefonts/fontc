//! The BASE table

use write_fonts::{tables::base as write_base, types::Tag};

#[derive(Clone, Debug, Default)]
pub(crate) struct BaseBuilder {
    pub horiz_tag_list: Vec<Tag>,
    pub horiz_script_list: Vec<ScriptRecord>,
    pub vert_tag_list: Vec<Tag>,
    pub vert_script_list: Vec<ScriptRecord>,
}

#[derive(Clone, Debug)]
pub(crate) struct ScriptRecord {
    pub script: Tag,
    pub default_baseline_tag: Tag,
    pub values: Vec<i16>,
}

impl BaseBuilder {
    pub(crate) fn build(&self) -> write_base::Base {
        let mut result = write_base::Base::default();
        if !self.horiz_tag_list.is_empty() {
            assert!(!self.horiz_script_list.is_empty(), "validate this");
            let haxis = BaseBuilder::build_axis(&self.horiz_tag_list, &self.horiz_script_list);
            result.horiz_axis = haxis.into();
        }
        if !self.vert_tag_list.is_empty() && !self.vert_script_list.is_empty() {
            assert!(!self.vert_script_list.is_empty(), "validate this");
            let vaxis = BaseBuilder::build_axis(&self.vert_tag_list, &self.vert_script_list);
            result.vert_axis = vaxis.into();
        }
        result
    }

    fn build_axis(tag_list: &[Tag], script_list: &[ScriptRecord]) -> write_base::Axis {
        let records = script_list.iter().map(|rec| {
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
                    None,
                    Vec::new(),
                ),
            )
        });
        write_base::Axis::new(
            Some(write_base::BaseTagList::new(tag_list.to_owned())),
            write_base::BaseScriptList::new(records.collect()),
        )
    }
}
