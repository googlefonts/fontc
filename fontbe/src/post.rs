//! Generates a [post](https://learn.microsoft.com/en-us/typography/opentype/spec/post) table.

use fontdrasil::orchestration::Work;
use read_fonts::types::{FWord, Fixed};
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
        let post = Post::new(
            Fixed::from_i32(0),
            FWord::new(0),
            FWord::new(16),
            0,
            0,
            0,
            0,
            0,
        );
        context.set_post(post);
        Ok(())
    }
}
