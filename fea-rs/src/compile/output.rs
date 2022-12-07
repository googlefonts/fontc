//! The result of a compilation

use std::collections::{BTreeMap, HashMap};

use write_fonts::{
    dump_table,
    read::{FontRef, TableProvider},
    tables::layout::{FeatureParams, SizeParams, StylisticSetParams},
    types::Tag,
    FontBuilder,
};

use super::{
    common,
    lookups::{AllLookups, FeatureKey, LookupId},
    tables::{NameSpec, Tables},
};

use crate::{Diagnostic, GlyphMap};

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
    /// Compile this output into a new binary font.
    #[allow(clippy::result_unit_err)] //TODO: figure out error reporting
    pub fn build_raw(&self, _glyph_map: &GlyphMap) -> Result<FontBuilder<'static>, ()> {
        self.apply(None)
    }

    /// Attempt to update the provided font with the results of this compilation.
    //TODO: I hate it? we should figure out what a rational approach to this is. What stuff do we
    //expect to have just living in the font by this point? Why can't we just start from a namelist
    //and FEA file, and go from there? maybe let's try?
    #[allow(clippy::result_unit_err)] //TODO: figure out error reporting
    pub fn apply<'a>(&self, font: impl Into<Option<FontRef<'a>>>) -> Result<FontBuilder<'a>, ()> {
        let font = font.into();
        let mut builder = FontBuilder::default();
        if let Some(head_raw) = &self.tables.head {
            let head = head_raw.build(font.as_ref());
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

        if let Some(base) = &self.tables.base {
            let data = dump_table(&base.build()).unwrap();
            builder.add_table(Tag::new(b"BASE"), data);
        }

        //TODO: reuse any existing names if name table present
        let mut name_builder = self.tables.name.clone();
        if let Some(stat_raw) = self.tables.stat.as_ref() {
            let stat = stat_raw.build(&mut name_builder);
            builder.add_table(Tag::new(b"STAT"), dump_table(&stat).unwrap());
        }

        let (mut gsub, mut gpos) = self.lookups.build(&self.features);

        let mut feature_params = HashMap::new();
        if let Some(size) = self.size.as_ref() {
            name_builder.add_anon_group(&size.names);
            feature_params.insert(
                (common::tags::GPOS, common::tags::SIZE),
                FeatureParams::Size(size.params.clone()),
            );
        }

        for (tag, names) in self.tables.stylistic_sets.iter() {
            let id = name_builder.add_anon_group(names);
            let params = FeatureParams::StylisticSet(StylisticSetParams::new(id));
            feature_params.insert((common::tags::GSUB, *tag), params);
        }

        for (tag, cv_params) in self.tables.character_variants.iter() {
            let params = cv_params.build(&mut name_builder);
            feature_params.insert(
                (common::tags::GSUB, *tag),
                FeatureParams::CharacterVariant(params),
            );
        }

        // actually add feature_params as appropriate
        if !feature_params.is_empty() {
            if let Some(gsub) = gsub.as_mut() {
                for record in gsub.feature_list.feature_records.iter_mut() {
                    if let Some(params) =
                        feature_params.get(&(common::tags::GSUB, record.feature_tag))
                    {
                        record.feature.feature_params = params.clone().into();
                    }
                }
            }
            if let Some(gpos) = gpos.as_mut() {
                for record in gpos.feature_list.feature_records.iter_mut() {
                    if let Some(params) =
                        feature_params.get(&(common::tags::GPOS, record.feature_tag))
                    {
                        record.feature.feature_params = params.clone().into();
                    }
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

        if let Some(font) = font {
            for record in font.table_directory.table_records() {
                if !builder.contains(record.tag()) {
                    let data = font.data_for_tag(record.tag()).unwrap();
                    builder.add_table(record.tag(), data);
                }
            }
        }

        Ok(builder)
    }
}
