//! The result of a compilation

use std::collections::{BTreeMap, HashMap, HashSet};

use write_fonts::{
    dump_table,
    read::{FontRef, TableProvider, TopLevelTable},
    tables::{
        layout::{FeatureParams, StylisticSetParams},
        maxp::Maxp,
    },
    types::Tag,
    FontBuilder,
};

use super::{
    error::BinaryCompilationError,
    features::SizeFeature,
    lookups::{AllLookups, FeatureKey, LookupId},
    tables::Tables,
    tags, Opts,
};

use crate::{Diagnostic, GlyphMap};

/// The output of a compilation operation.
///
/// This losslessly contains all of the information parsed from the FEA necessary
/// for generating the various OpenType tables.
//TODO: ability to generate new errors during this final compilation step
pub struct Compilation {
    /// Any warnings that were generated during compilation
    pub warnings: Vec<Diagnostic>,
    pub(crate) tables: Tables,
    pub(crate) lookups: AllLookups,
    pub(crate) features: BTreeMap<FeatureKey, Vec<LookupId>>,
    pub(crate) required_features: HashSet<FeatureKey>,
    pub(crate) size: Option<SizeFeature>,
}

impl Compilation {
    /// Generate all the final tables and add them to a builder.
    ///
    /// This builder can be used to get generate the final binary.
    pub fn assemble(
        &self,
        glyph_map: &GlyphMap,
        opts: Opts,
    ) -> Result<FontBuilder<'static>, BinaryCompilationError> {
        let mut builder = self.apply(None)?;
        // because we often inspect our output with ttx, and ttx fails if maxp is
        // missing, we create a maxp table.
        let maxp = Maxp::new(glyph_map.len().try_into().unwrap());
        builder.add_table(Tag::new(b"maxp"), dump_table(&maxp).unwrap());
        if opts.make_post_table {
            let post = glyph_map.make_post_table();
            builder.add_table(Tag::new(b"post"), dump_table(&post).unwrap());
        }
        Ok(builder)
    }

    //FIXME: this is left over from a previous API. `font` is always none.
    //This should be removed and merged with `build_raw`, above.
    fn apply<'a>(
        &self,
        font: impl Into<Option<FontRef<'a>>>,
    ) -> Result<FontBuilder<'a>, BinaryCompilationError> {
        let font = font.into();
        let mut builder = FontBuilder::default();
        if let Some(head_raw) = &self.tables.head {
            let head = head_raw.build(font.as_ref());
            builder.add_table(Tag::new(b"head"), dump_table(&head).unwrap());
        }

        //TODO: can this contain some subset of keys? should we preserve
        //existing values in this case?
        if let Some(hhea_raw) = self.tables.hhea.as_ref() {
            let data = dump_table(hhea_raw)?;
            builder.add_table(Tag::new(b"hhea"), data);
        }

        if let Some(vhea_raw) = self.tables.vhea.as_ref() {
            let data = dump_table(vhea_raw)?;
            builder.add_table(Tag::new(b"vhea"), data);
        }

        if let Some(os2) = self.tables.os2.as_ref() {
            let table = os2.build();
            let data = dump_table(&table)?;
            builder.add_table(write_fonts::tables::os2::Os2::TAG, data);
        }

        if let Some(gdef) = &self.tables.gdef {
            builder.add_table(Tag::new(b"GDEF"), gdef.build()?);
        }

        if let Some(base) = &self.tables.base {
            let data = dump_table(&base.build())?;
            builder.add_table(Tag::new(b"BASE"), data);
        }

        //TODO: reuse any existing names if name table present
        let mut name_builder = self.tables.name.clone();
        if let Some(stat_raw) = self.tables.stat.as_ref() {
            let stat = stat_raw.build(&mut name_builder);
            builder.add_table(Tag::new(b"STAT"), dump_table(&stat)?);
        }

        let (mut gsub, mut gpos) = self.lookups.build(&self.features, &self.required_features);

        let mut feature_params = HashMap::new();
        if let Some(size) = self.size.as_ref() {
            feature_params.insert(
                (tags::GPOS, tags::SIZE),
                FeatureParams::Size(size.build(&mut name_builder)),
            );
        }

        for (tag, names) in self.tables.stylistic_sets.iter() {
            let id = name_builder.add_anon_group(names);
            let params = FeatureParams::StylisticSet(StylisticSetParams::new(id));
            feature_params.insert((tags::GSUB, *tag), params);
        }

        for (tag, cv_params) in self.tables.character_variants.iter() {
            let params = cv_params.build(&mut name_builder);
            feature_params.insert((tags::GSUB, *tag), FeatureParams::CharacterVariant(params));
        }

        // actually add feature_params as appropriate
        if !feature_params.is_empty() {
            if let Some(gsub) = gsub.as_mut() {
                for record in gsub.feature_list.feature_records.iter_mut() {
                    if let Some(params) = feature_params.get(&(tags::GSUB, record.feature_tag)) {
                        record.feature.feature_params = params.clone().into();
                    }
                }
            }
            if let Some(gpos) = gpos.as_mut() {
                for record in gpos.feature_list.feature_records.iter_mut() {
                    if let Some(params) = feature_params.get(&(tags::GPOS, record.feature_tag)) {
                        record.feature.feature_params = params.clone().into();
                    }
                }
            }
        }

        if let Some(gsub) = gsub {
            builder.add_table(Tag::new(b"GSUB"), dump_table(&gsub)?);
        }

        if let Some(gpos) = gpos {
            builder.add_table(Tag::new(b"GPOS"), dump_table(&gpos)?);
        }

        if let Some(name) = name_builder.build() {
            builder.add_table(Tag::new(b"name"), dump_table(&name)?);
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
