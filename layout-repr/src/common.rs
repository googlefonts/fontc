use read_fonts::{
    tables::layout::{FeatureList, ScriptList},
    types::Tag,
};

/// A set of lookups for a specific feature and language system
pub(crate) struct Feature {
    pub(crate) feature: Tag,
    pub(crate) script: Tag,
    pub(crate) lang: Tag,
    pub(crate) lookups: Vec<u16>,
}

impl Feature {
    fn sort_key(&self) -> impl Ord {
        // make it so we always put DFLT/dflt above other tags
        fn tag_to_int(tag: Tag) -> u32 {
            if tag == Tag::new(b"DFLT") {
                0
            } else if tag == Tag::new(b"dflt") {
                1
            } else {
                u32::from_be_bytes(tag.to_be_bytes())
            }
        }

        (
            tag_to_int(self.feature),
            tag_to_int(self.script),
            tag_to_int(self.lang),
        )
    }
}

pub(crate) fn get_lang_systems(
    script_list: &ScriptList,
    feature_list: &FeatureList,
) -> Vec<Feature> {
    let data = script_list.offset_data();

    let lang_sys_iter = script_list.script_records().iter().flat_map(|script| {
        let script_tag = script.script_tag();
        let script = script.script(data).unwrap();
        let maybe_default = script
            .default_lang_sys()
            .transpose()
            .unwrap()
            .map(|dflt| (script_tag, Tag::new(b"dflt"), dflt.feature_indices()));
        let lang_sys_iter = script.lang_sys_records().iter().map(move |lang_sys| {
            let lang_tag = lang_sys.lang_sys_tag();
            let lang = lang_sys.lang_sys(script.offset_data()).unwrap();
            (script_tag, lang_tag, lang.feature_indices())
        });
        maybe_default.into_iter().chain(lang_sys_iter)
    });

    let mut result = Vec::new();
    for (script, lang, feature_indices) in lang_sys_iter {
        let data = feature_list.offset_data();

        for idx in feature_indices {
            let rec = feature_list
                .feature_records()
                .get(idx.get() as usize)
                .unwrap();
            let feature = rec.feature(data).unwrap();
            let lookups = feature
                .lookup_list_indices()
                .iter()
                .map(|x| x.get())
                .collect();
            result.push(Feature {
                feature: rec.feature_tag(),
                script,
                lang,
                lookups,
            })
        }
    }

    result.sort_unstable_by_key(|sys| sys.sort_key());

    result
}
