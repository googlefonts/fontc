use std::{collections::HashSet, fs, path::PathBuf};

use fontir::error::Error;
use fontir::source::{Input, Paths, Source, Work};

use crate::openstep_plist::Plist;

pub struct GlyphsIrSource {
    glyphs_file: PathBuf,
    ir_paths: Paths,
}

impl GlyphsIrSource {
    pub fn new(glyphs_file: PathBuf, ir_paths: Paths) -> GlyphsIrSource {
        GlyphsIrSource {
            glyphs_file,
            ir_paths,
        }
    }
}

impl Source for GlyphsIrSource {
    fn inputs(&mut self) -> Result<Input, Error> {
        // We have to read the glyphs file then shred it to figure out if anything changed
        let raw_plist = fs::read_to_string(&self.glyphs_file).unwrap();
        let plist = Plist::parse(&raw_plist).unwrap();
        let Plist::Dictionary(_, _) = plist else {
            return Err(Error::ParseError(self.glyphs_file.to_path_buf(), "Root is not a dict".to_string()));
        };

        // TODO something real
        Ok(Input {
            ..Default::default()
        })
    }

    fn create_glyph_ir_work(
        &self,
        _glyph_names: &HashSet<&str>,
        _input: &Input,
    ) -> Result<Vec<Box<dyn Work<()>>>, fontir::error::Error> {
        todo!("TODO write glyph IR to {:#?}", self.ir_paths.glyph_ir_dir());
    }
}
