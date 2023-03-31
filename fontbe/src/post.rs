//! Generates a [post](https://learn.microsoft.com/en-us/typography/opentype/spec/post) table.

use fontdrasil::orchestration::Work;
use write_fonts::tables::post::Post;

use crate::{
    error::Error,
    orchestration::{BeWork, Context},
};

struct PostWork {}

pub fn create_post_work() -> Box<BeWork> {
    Box::new(PostWork {})
}

impl Work<Context, Error> for PostWork {
    /// Generate [post](https://learn.microsoft.com/en-us/typography/opentype/spec/post)
    fn exec(&self, context: &Context) -> Result<(), Error> {
        // For now we build a v2 table by default, like fontmake does.
        // TODO optionally drop glyph names with format 3.0.
        // TODO a more serious post
        let static_metadata = context.ir.get_final_static_metadata();
        let post = Post::new_v2(static_metadata.glyph_order.iter().map(|g| g.as_str()));
        context.set_post(post);
        Ok(())
    }
}
