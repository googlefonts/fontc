//! Generates a [post](https://learn.microsoft.com/en-us/typography/opentype/spec/post) table.

use fontdrasil::orchestration::{Access, AccessBuilder, Work};
use fontir::orchestration::WorkId as FeWorkId;
use std::collections::HashMap;
use write_fonts::{
    tables::post::Post,
    types::{FWord, Fixed},
    OtRound,
};

use crate::{
    error::Error,
    orchestration::{AnyWorkId, BeWork, Context, WorkId},
};

#[derive(Debug)]
struct PostWork {}

pub fn create_post_work() -> Box<BeWork> {
    Box::new(PostWork {})
}

impl Work<Context, AnyWorkId, Error> for PostWork {
    fn id(&self) -> AnyWorkId {
        WorkId::Post.into()
    }

    fn read_access(&self) -> Access<AnyWorkId> {
        AccessBuilder::new()
            .variant(FeWorkId::StaticMetadata)
            .variant(FeWorkId::GlyphOrder)
            .variant(FeWorkId::GlobalMetrics)
            .build()
    }

    /// Generate [post](https://learn.microsoft.com/en-us/typography/opentype/spec/post)
    fn exec(&self, context: &Context) -> Result<(), Error> {
        // For now we build a v2 table by default, like fontmake does.
        // TODO optionally drop glyph names with format 3.0.
        // TODO a more serious post
        let static_metadata = context.ir.static_metadata.get();
        let metrics = context
            .ir
            .global_metrics
            .get()
            .at(static_metadata.default_location());
        let glyph_order = context.ir.glyph_order.get();

        let mut post = if let Some(rename_map) = &static_metadata.postscript_names {
            // rename glyphs for 'production' using the provided rename map
            let mut seen = HashMap::new();
            let final_glyph_names: Vec<_> = glyph_order
                .names()
                .map(|g| {
                    let mut name = rename_map.get(g).unwrap_or(g).to_string();
                    // Adobe Glyph List spec forbids any characters not in [A-Za-z0-9._];
                    // it also says glyphs must not start with a digit or period (except
                    // .notdef) and shouldn't exceed 63 chars, but ufo2ft only enforces
                    // the first rule so we simply follow that.
                    // https://github.com/googlefonts/ufo2ft/blob/2f11b0f/Lib/ufo2ft/postProcessor.py#L220-L233
                    // https://github.com/adobe-type-tools/agl-specification
                    name.retain(|c| c.is_ascii_alphanumeric() || c == '.' || c == '_');
                    // make duplicates unique by adding a .N number suffix to match ufo2ft:
                    if let Some(n) = seen.get(&name) {
                        let mut n = *n;
                        while seen.contains_key(&format!("{name}.{n}")) {
                            n += 1;
                        }
                        seen.insert(name.clone(), n + 1);
                        name = format!("{name}.{n}");
                    }
                    seen.insert(name.clone(), 1);
                    name
                })
                .collect();

            Post::new_v2(final_glyph_names.iter().map(|g| g.as_str()))
        } else {
            // use the original glyph names as-is
            Post::new_v2(glyph_order.names().map(|g| g.as_str()))
        };

        post.is_fixed_pitch = static_metadata.misc.is_fixed_pitch.unwrap_or_default() as u32;
        post.italic_angle = Fixed::from_f64(static_metadata.italic_angle.into_inner());
        post.underline_position = FWord::new(metrics.underline_position.ot_round());
        post.underline_thickness = FWord::new(metrics.underline_thickness.ot_round());
        context.post.set(post);
        Ok(())
    }
}
