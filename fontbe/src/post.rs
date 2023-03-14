//! Generates a [post](https://learn.microsoft.com/en-us/typography/opentype/spec/post) table.

use fontdrasil::orchestration::Work;
use read_fonts::types::Version16Dot16;
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
        // TODO a more serious post
        let post = Post { version: Version16Dot16::VERSION_3_0, ..Default::default() };
        context.set_post(post);
        Ok(())
    }
}
