//! Generates a [OS/2](https://learn.microsoft.com/en-us/typography/opentype/spec/os2) table.

use fontdrasil::orchestration::Work;
use write_fonts::tables::os2::Os2;

use crate::{
    error::Error,
    orchestration::{BeWork, Context},
};

struct Os2Work {}

pub fn create_os2_work() -> Box<BeWork> {
    Box::new(Os2Work {})
}

impl Work<Context, Error> for Os2Work {
    /// Generate [OS/2](https://learn.microsoft.com/en-us/typography/opentype/spec/os2)
    fn exec(&self, context: &Context) -> Result<(), Error> {
        //let static_metadata = context.ir.get_final_static_metadata();
        let os2 = Os2 {
            ..Default::default()
        };
        context.set_os2(os2);
        Ok(())
    }
}
