use fontir::error::{Error, WorkError};
use fontir::source::{Input, Paths, Source, Work};
use fontir::stateset::StateSet;
use log::debug;
use std::cmp::min;
use std::collections::HashSet;
use std::ops::Range;
use std::path::Path;
use std::{collections::HashMap, fs, path::PathBuf};

use crate::openstep_plist::Plist;

pub struct GlyphsIrSource {
    glyphs_file: PathBuf,
    ir_paths: Paths,
    plist_cache: Option<PlistCache>,
}

impl GlyphsIrSource {
    pub fn new(glyphs_file: PathBuf, ir_paths: Paths) -> GlyphsIrSource {
        GlyphsIrSource {
            glyphs_file,
            ir_paths,
            plist_cache: None,
        }
    }
}

struct PlistCache {
    global_metadata: StateSet,
    _root_dict: HashMap<String, Plist>,
}

impl PlistCache {
    fn is_valid_for(&self, global_metadata: &StateSet) -> bool {
        self.global_metadata == *global_metadata
    }
}

fn glyph_identifier(glyph_name: &str) -> String {
    format!("/glyph/{glyph_name}")
}

fn read_glyphs_file(glyphs_file: &Path) -> Result<(HashMap<String, Plist>, String), Error> {
    let raw_plist = fs::read_to_string(glyphs_file).map_err(Error::IoError)?;
    let plist = Plist::parse(&raw_plist).unwrap();
    let Plist::Dictionary(root_dict, _) = plist else {
        return Err(Error::ParseError(glyphs_file.to_path_buf(), "Root is not a dict".to_string()));
    };
    Ok((root_dict, raw_plist))
}

fn glyph_name(
    glyphs_file: &Path,
    glyph: &HashMap<String, Plist>,
    range: &Range<usize>,
    raw_plist: &str,
) -> Result<String, Error> {
    // In an exciting turn of events it appears there are unquoted names that parse as floats
    Ok(match glyph.get("glyphname") {
        Some(Plist::String(glyph_name, _)) => glyph_name.clone(),
        Some(Plist::Float(_, r)) => String::from(raw_plist[r.start..r.end].trim()),
        _ => {
            let (s, e) = (range.start, min(range.start + 8, range.end));
            let message = format!("Missing glyphname at {:#?}: {}", range, &raw_plist[s..e]);
            debug!("Parse failed:\n{:#?}", glyph);
            return Err(Error::ParseError(glyphs_file.to_path_buf(), message));
        }
    })
}

fn glyphs(
    glyphs_file: &Path,
    root_dict: &HashMap<String, Plist>,
    raw_plist: &str,
) -> Result<HashMap<String, StateSet>, Error> {
    let Some(Plist::Array(raw_glyphs, _)) = root_dict.get("glyphs") else {
        return Err(Error::ParseError(glyphs_file.to_path_buf(), "No glyphs array".to_string()));
    };

    let mut glyphs = HashMap::new();
    for glyph in raw_glyphs {
        let Plist::Dictionary(glyph, range) = glyph else {
            return Err(Error::ParseError(glyphs_file.to_path_buf(), "Glyphs must be dicts".to_string()));
        };
        let glyph_name = glyph_name(glyphs_file, glyph, range, raw_plist)?;
        debug!("Parse {}", &glyph_name);
        let mut change_tracker = StateSet::new();
        change_tracker.track_slice(
            glyph_identifier(&glyph_name),
            &raw_plist[range.start..range.end],
        )?;
        glyphs.insert(glyph_name, change_tracker);
    }

    Ok(glyphs)
}

impl GlyphsIrSource {
    // When things like upem may have changed forget incremental and rebuild the whole thing
    fn global_rebuild_triggers(
        &self,
        root_dict: &HashMap<String, Plist>,
        raw_plist: &str,
    ) -> Result<StateSet, Error> {
        // Naive mk1: if anything other than glyphs and date changes do a global rebuild
        // TODO experiment with actual glyphs saves to see what makes sense
        let mut state = StateSet::new();
        for (key, plist) in root_dict {
            if key == "glyphs" || key == "date" {
                continue;
            }
            let r = plist.range();
            state.track_slice(format!("/{}", key), &raw_plist[r.start..r.end])?;
        }
        Ok(state)
    }
}

impl Source for GlyphsIrSource {
    fn inputs(&mut self) -> Result<Input, Error> {
        // We have to read the glyphs file then shred it to figure out if anything changed
        let (root_dict, raw_plist) = read_glyphs_file(&self.glyphs_file)?;

        let glyphs = glyphs(&self.glyphs_file, &root_dict, &raw_plist)?;
        let global_metadata = self.global_rebuild_triggers(&root_dict, &raw_plist)?;
        self.plist_cache = Some(PlistCache {
            global_metadata: global_metadata.clone(),
            _root_dict: root_dict,
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
            .plist_cache
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
        f64::INFINITY,
        path::{Path, PathBuf},
    };

    use fontir::stateset::StateSet;

    use crate::plist::Plist;

    use super::{glyph_name, glyphs, read_glyphs_file};

    fn testdata_dir() -> PathBuf {
        let dir = Path::new("../resources/testdata");
        assert!(dir.is_dir());
        dir.to_path_buf()
    }

    fn glyphs_in_file(filename: &str) -> HashMap<String, StateSet> {
        let glyphs_file = testdata_dir().join(filename);
        let (root_dict, raw_plist) = read_glyphs_file(&glyphs_file).unwrap();
        glyphs(&glyphs_file, &root_dict, &raw_plist).unwrap()
    }

    #[test]
    fn find_glyphs() {
        let expected_keys = HashSet::from(["space", "hyphen", "exclam"]);
        assert_eq!(
            expected_keys,
            glyphs_in_file("WghtVar.glyphs")
                .keys()
                .map(|k| k.as_str())
                .collect::<HashSet<&str>>()
        );
        assert_eq!(
            expected_keys,
            glyphs_in_file("WghtVar_HeavyHyphen.glyphs")
                .keys()
                .map(|k| k.as_str())
                .collect::<HashSet<&str>>()
        );
    }

    #[test]
    fn detect_changed_glyphs() {
        let keys = HashSet::from(["space", "hyphen", "exclam"]);

        let g1 = glyphs_in_file("WghtVar.glyphs");
        let g2 = glyphs_in_file("WghtVar_HeavyHyphen.glyphs");

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

    #[test]
    fn access_glyph_name_string() {
        let mut glyph = HashMap::new();
        glyph.insert(
            "glyphname".to_string(),
            Plist::String("space".to_string(), 0..1),
        );
        assert_eq!(
            "space",
            glyph_name(
                Path::new("f.glyphs"),
                &glyph,
                &(1usize..12usize),
                "I'm a plist"
            )
            .unwrap()
        );
    }

    // glyphname = infinity (unquoted) has a cool trick where it parses as a float
    #[test]
    fn access_glyph_name_float() {
        let mut glyph = HashMap::new();
        glyph.insert("glyphname".to_string(), Plist::Float(INFINITY, 12..20));
        assert_eq!(
            "infinity",
            glyph_name(
                Path::new("f.glyphs"),
                &glyph,
                &(1usize..12usize),
                "glyphname = infinity"
            )
            .unwrap()
        );
    }
}
