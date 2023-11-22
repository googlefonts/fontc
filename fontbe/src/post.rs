//! Generates a [post](https://learn.microsoft.com/en-us/typography/opentype/spec/post) table.

use std::collections::HashSet;

use fontdrasil::orchestration::{Access, Work};
use fontir::orchestration::WorkId as FeWorkId;
use write_fonts::{tables::post::Post, types::FWord};

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
        Access::Set(HashSet::from([
            FeWorkId::StaticMetadata.into(),
            FeWorkId::GlyphOrder.into(),
        ]))
    }

    /// Generate [post](https://learn.microsoft.com/en-us/typography/opentype/spec/post)
    fn exec(&self, context: &Context) -> Result<(), Error> {
        // For now we build a v2 table by default, like fontmake does.
        // TODO optionally drop glyph names with format 3.0.
        // TODO a more serious post
        let static_metadata = context.ir.static_metadata.get();
        let postscript_names = &static_metadata.postscript_names;
        let glyph_order = context.ir.glyph_order.get();
        let mut post = Post::new_v2(
            glyph_order
                .iter()
                .map(|g| postscript_names.get(g).unwrap_or(g).as_str()),
        );
        post.underline_position = FWord::new(static_metadata.misc.underline_position.0 as i16);
        post.underline_thickness = FWord::new(static_metadata.misc.underline_thickness.0 as i16);
        context.post.set_unconditionally(post.into());
        Ok(())
    }
}
