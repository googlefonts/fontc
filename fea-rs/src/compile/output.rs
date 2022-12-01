//! The result of a compilation

use std::collections::BTreeMap;

use write_fonts::{
    dump_table,
    read::{FontRef, TableProvider},
    tables::layout::{FeatureParams, SizeParams},
    types::Tag,
    FontBuilder,
};

use super::{
    common,
    lookups::{AllLookups, FeatureKey, LookupId},
    tables::{NameSpec, Tables},
};
use crate::Diagnostic;

/// The output of a compilation operation.
///
/// This losslessly contains all of the information parsed from the FEA necessary
/// for generating the various OpenType tables.
//TODO: ability to generate new errors during this final compilation step
pub struct Compilation {
    pub warnings: Vec<Diagnostic>,
    pub(crate) tables: Tables,
    pub(crate) lookups: AllLookups,
    pub(crate) features: BTreeMap<FeatureKey, Vec<LookupId>>,
    pub(crate) size: Option<SizeFeature>,
}

#[derive(Clone, Debug, Default)]
pub struct SizeFeature {
    pub params: SizeParams,
    pub names: Vec<NameSpec>,
}

impl Compilation {
    /// Attempt to update the provided font with the results of this compilation.

    #[allow(clippy::result_unit_err)] //TODO: figure out error reporting
    pub fn apply(&self, font: &FontRef) -> Result<Vec<u8>, ()> {
        let mut builder = FontBuilder::default();
        if let Some(head_raw) = &self.tables.head {
            let head = head_raw.build(font);
            builder.add_table(Tag::new(b"head"), dump_table(&head).unwrap());
        }

        //TODO: can this contain some subset of keys? should we preserve
        //existing values in this case?
        if let Some(hhea_raw) = self.tables.hhea.as_ref() {
            let data = dump_table(hhea_raw).unwrap();
            builder.add_table(Tag::new(b"hhea"), data);
        }

        if let Some(vhea_raw) = self.tables.vhea.as_ref() {
            let data = dump_table(vhea_raw).unwrap();
            builder.add_table(Tag::new(b"vhea"), data);
        }

        //TODO: OS/2
        //if let Some(os2) = self.tables.OS2.as_ref() {
        //let data = dump_table(hhea_raw).unwrap();
        //font.add_table(Tag::new(b"hhea"), data);
        //}

        if let Some(gdef) = &self.tables.GDEF {
            builder.add_table(Tag::new(b"GDEF"), gdef.build().unwrap());
        }

        //TODO: reuse existing name table if present
        //let mut name = font
        //.tables
        //.name()
        //.unwrap()
        //.map(|t| t.into_owned())
        //.unwrap_or_else(|| tables::name::Name {
        //records: Vec::new(),
        //});

        let mut name_builder = self.tables.name.clone();
        if let Some(stat_raw) = self.tables.stat.as_ref() {
            let stat = stat_raw.build(&mut name_builder);
            builder.add_table(Tag::new(b"STAT"), dump_table(&stat).unwrap());
        }

        let (gsub, mut gpos) = self.lookups.build(&self.features);

        if let Some(size) = self.size.as_ref() {
            name_builder.add_anon_group(&size.names);
            let gpos = gpos.as_mut().unwrap();
            for record in gpos.feature_list.feature_records.iter_mut() {
                if record.feature_tag == common::tags::SIZE {
                    record.feature.feature_params = FeatureParams::Size(size.params.clone()).into();
                }
            }
        }

        if let Some(gsub) = gsub {
            builder.add_table(Tag::new(b"GSUB"), dump_table(&gsub).unwrap());
        }

        if let Some(gpos) = gpos {
            builder.add_table(Tag::new(b"GPOS"), dump_table(&gpos).unwrap());
        }

        if let Some(name) = name_builder.build() {
            builder.add_table(Tag::new(b"name"), dump_table(&name).unwrap());
        }

        for record in font.table_directory.table_records() {
            if !builder.contains(record.tag()) {
                let data = font.data_for_tag(record.tag()).unwrap();
                builder.add_table(record.tag(), data);
            }
        }

        Ok(builder.build())
    }
}
