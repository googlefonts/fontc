use fontir::error::{Error, WorkError};
use fontir::source::{Input, Paths, Source, Work};
use fontir::stateset::StateSet;
use glyphs_reader::{Font, FromPlist, Plist};
use log::debug;
use std::collections::HashSet;
use std::path::Path;
use std::{collections::HashMap, fs, path::PathBuf};

pub struct GlyphsIrSource {
    glyphs_file: PathBuf,
    ir_paths: Paths,
    cache: Option<Cache>,
}

impl GlyphsIrSource {
    pub fn new(glyphs_file: PathBuf, ir_paths: Paths) -> GlyphsIrSource {
        GlyphsIrSource {
            glyphs_file,
            ir_paths,
            cache: None,
        }
    }
}

struct Cache {
    global_metadata: StateSet,
    _font: Font,
}

impl Cache {
    fn is_valid_for(&self, global_metadata: &StateSet) -> bool {
        self.global_metadata == *global_metadata
    }
}

fn fix_glyphs_named_infinity(glyphs_file: &Path, plist: &mut Plist) -> Result<(), Error> {
    let Plist::Dictionary(root_dict) = plist else {
        return Err(Error::ParseError(glyphs_file.to_path_buf(), "Root must be a dict".to_string()));
    };
    if !root_dict.contains_key("glyphs") {
        return Ok(());
    }
    let Plist::Array(glyphs) = root_dict.get_mut("glyphs").unwrap() else {
        return Err(Error::ParseError(glyphs_file.to_path_buf(), "Must have a glyphs array".to_string()));
    };
    for glyph in glyphs.iter_mut() {
        let Plist::Dictionary(glyph) = glyph else {
            return Err(Error::ParseError(glyphs_file.to_path_buf(), "Glyph must be a dict".to_string()));
        };
        if !glyph.contains_key("glyphname") {
            continue;
        }
        if let Plist::Float(..) = glyph.get("glyphname").unwrap() {
            glyph.insert(
                "glyphname".to_string(),
                Plist::String("infinity".to_string()),
            );
        }
    }
    Ok(())
}

fn read_glyphs_file(glyphs_file: &Path) -> Result<Font, Error> {
    let raw_content = fs::read_to_string(glyphs_file)?;
    let mut raw_content = Plist::parse(&raw_content)
        .map_err(|e| Error::ParseError(glyphs_file.to_path_buf(), format!("{:#?}", e)))?;
    fix_glyphs_named_infinity(glyphs_file, &mut raw_content)?;
    Ok(Font::from_plist(raw_content))
}

fn glyph_identifier(glyph_name: &str) -> String {
    format!("/glyph/{glyph_name}")
}

fn glyph_states(font: &Font) -> Result<HashMap<String, StateSet>, Error> {
    let mut glyph_states = HashMap::new();

    for glyph in font.glyphs.iter() {
        let mut state = StateSet::new();
        state.track_memory(glyph_identifier(&glyph.glyphname), &glyph)?;
        glyph_states.insert(glyph.glyphname.clone(), state);
    }

    Ok(glyph_states)
}

impl GlyphsIrSource {
    // When things like upem may have changed forget incremental and rebuild the whole thing
    fn global_rebuild_triggers(&self, font: &Font) -> Result<StateSet, Error> {
        // Naive mk1: if anything other than glyphs and date changes do a global rebuild
        // TODO experiment with actual glyphs saves to see what makes sense
        let mut state = StateSet::new();
        state.track_memory("/font_master".to_string(), &font.font_master)?;
        for (key, plist) in font.other_stuff.iter() {
            if key == "date" {
                continue;
            }
            state.track_memory(format!("/{}", key), &plist)?;
        }
        Ok(state)
    }
}

impl Source for GlyphsIrSource {
    fn inputs(&mut self) -> Result<Input, Error> {
        // We have to read the glyphs file then shred it to figure out if anything changed
        let font = read_glyphs_file(&self.glyphs_file)?;
        let glyphs = glyph_states(&font)?;
        let global_metadata = self.global_rebuild_triggers(&font)?;

        self.cache = Some(Cache {
            global_metadata: global_metadata.clone(),
            _font: font,
        });

        Ok(Input {
            global_metadata,
            glyphs,
        })
    }

    fn create_glyph_ir_work(
        &self,
        glyph_names: &HashSet<&str>,
        input: &Input,
    ) -> Result<Vec<Box<dyn Work<()>>>, fontir::error::Error> {
        let mut work: Vec<Box<dyn Work<()>>> = Vec::new();

        // Do we have a plist cache?
        // TODO: consider just recomputing here instead of failing
        if !self
            .cache
            .as_ref()
            .map(|pc| pc.is_valid_for(&input.global_metadata))
            .unwrap_or(false)
        {
            return Err(Error::UnableToCreateGlyphIrWork);
        }

        for glyph_name in glyph_names {
            work.push(Box::from(
                self.create_work_for_one_glyph(glyph_name, input)?,
            ));
        }

        Ok(work)
    }
}

impl GlyphsIrSource {
    fn create_work_for_one_glyph(
        &self,
        glyph_name: &str,
        input: &Input,
    ) -> Result<GlyphIrWork, Error> {
        let glyph_name = glyph_name.to_string();
        let _stateset = input
            .glyphs
            .get(&glyph_name)
            .ok_or_else(|| Error::NoStateForGlyph(glyph_name.clone()))?;

        Ok(GlyphIrWork {
            glyph_name: glyph_name.clone(),
            ir_file: self.ir_paths.glyph_ir_file(&glyph_name),
        })
    }
}

struct GlyphIrWork {
    glyph_name: String,
    ir_file: PathBuf,
}

impl Work<()> for GlyphIrWork {
    fn exec(&self) -> Result<(), WorkError> {
        debug!("Generate {:#?} for {}", self.ir_file, self.glyph_name);
        fs::write(&self.ir_file, &self.glyph_name).map_err(WorkError::IoError)?;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use std::{
        collections::{HashMap, HashSet},
        path::{Path, PathBuf},
    };

    use fontir::stateset::StateSet;

    use super::{glyph_states, read_glyphs_file};

    fn testdata_dir() -> PathBuf {
        let dir = Path::new("../resources/testdata");
        assert!(dir.is_dir());
        dir.to_path_buf()
    }

    fn glyphs3_dir() -> PathBuf {
        testdata_dir().join("glyphs3")
    }

    fn glyph_state_for_file(dir: &Path, filename: &str) -> HashMap<String, StateSet> {
        let glyphs_file = dir.join(filename);
        let font = read_glyphs_file(&glyphs_file).unwrap();
        glyph_states(&font).unwrap()
    }

    #[test]
    fn find_glyphs() {
        let expected_keys = HashSet::from(["space", "hyphen", "exclam"]);
        assert_eq!(
            expected_keys,
            glyph_state_for_file(&glyphs3_dir(), "WghtVar.glyphs")
                .keys()
                .map(|k| k.as_str())
                .collect::<HashSet<&str>>()
        );
        assert_eq!(
            expected_keys,
            glyph_state_for_file(&glyphs3_dir(), "WghtVar_HeavyHyphen.glyphs")
                .keys()
                .map(|k| k.as_str())
                .collect::<HashSet<&str>>()
        );
    }

    #[test]
    fn detect_changed_glyphs() {
        let keys = HashSet::from(["space", "hyphen", "exclam"]);

        let g1 = glyph_state_for_file(&glyphs3_dir(), "WghtVar.glyphs");
        let g2 = glyph_state_for_file(&glyphs3_dir(), "WghtVar_HeavyHyphen.glyphs");

        let changed = keys
            .iter()
            .filter_map(|key| {
                let key = key.to_string();
                if g1.get(&key).unwrap() == g2.get(&key).unwrap() {
                    return None;
                }
                Some(key)
            })
            .collect::<HashSet<String>>();
        assert_eq!(HashSet::from(["hyphen".to_string()]), changed);
    }

    // unquoted infinity likes to parse as a float which is suboptimal for glyph names. Survive.
    // Observed on Work Sans and Lexend.
    #[test]
    fn survive_unquoted_infinity() {
        // Read a minimal glyphs file that reproduces the error
        read_glyphs_file(&glyphs3_dir().join("infinity.glyphs")).unwrap();
    }
}
