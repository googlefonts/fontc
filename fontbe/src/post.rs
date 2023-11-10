//! Generates a [post](https://learn.microsoft.com/en-us/typography/opentype/spec/post) table.

use fontdrasil::orchestration::{Access, AccessBuilder, Work};
use fontir::orchestration::WorkId as FeWorkId;
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
        let postscript_names = &static_metadata.postscript_names;
        let glyph_order = context.ir.glyph_order.get();
        let mut post = Post::new_v2(
            glyph_order
                .iter()
                .map(|g| postscript_names.get(g).unwrap_or(g).as_str()),
        );
        post.italic_angle = Fixed::from_f64(static_metadata.italic_angle.into_inner());
        post.underline_position = FWord::new(metrics.underline_position.ot_round());
        post.underline_thickness = FWord::new(metrics.underline_thickness.ot_round());
        context.post.set_unconditionally(post.into());
        Ok(())
    }
}
