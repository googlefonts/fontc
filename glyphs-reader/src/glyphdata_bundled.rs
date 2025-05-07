//! Accessors for bundled glyphsLib data

use std::{
    cmp::Ordering, collections::HashMap, marker::PhantomData, str::from_utf8_unchecked,
    sync::LazyLock,
};

use smol_str::SmolStr;

use crate::glyphdata::{Category, ProductionName, QueryResult, Script, Subcategory};

type U24 = usize;

fn read_u24le(bytes: &[u8]) -> usize {
    ((bytes[2] as usize) << 16) | ((bytes[1] as usize) << 8) | (bytes[0] as usize)
}

pub(crate) trait BundledEntry {
    fn element_size() -> usize;
    fn from_slice(raw: &[u8]) -> Self;
}

impl BundledEntry for U24 {
    fn element_size() -> usize {
        3
    }

    fn from_slice(raw: &[u8]) -> Self {
        read_u24le(raw)
    }
}

impl BundledEntry for (U24, U24) {
    fn element_size() -> usize {
        6
    }

    fn from_slice(raw: &[u8]) -> Self {
        (read_u24le(&raw[0..3]), read_u24le(&raw[3..6]))
    }
}

struct ArrayOf<T: BundledEntry>(&'static [u8], PhantomData<T>);

impl<T> ArrayOf<T>
where
    T: BundledEntry,
{
    const fn new(data: &'static [u8]) -> Self {
        Self(data, PhantomData)
    }

    fn len(&self) -> usize {
        self.0.len() / T::element_size()
    }

    fn get(&self, i: usize) -> Option<T> {
        let start = T::element_size() * i;
        let end = start + T::element_size();
        if end > self.0.len() {
            return None;
        }
        Some(T::from_slice(&self.0[start..end]))
    }
}

const NAME_OFFSETS: ArrayOf<U24> = ArrayOf::new(include_bytes!("../resources/name_offsets.dat"));
const NAMES: &[u8] = include_bytes!("../resources/names.dat");

const PROD_NAME_OFFSETS: ArrayOf<U24> =
    ArrayOf::new(include_bytes!("../resources/prod_name_offsets.dat"));
const PROD_NAMES: &[u8] = include_bytes!("../resources/prod_names.dat");
const PROD_NAME_PREDICTABLE_BITMAP: &[u8] = include_bytes!("../resources/prod_name_bitmap.dat");

const CODEPOINT_TO_INFO_IDX: ArrayOf<(U24, U24)> =
    ArrayOf::new(include_bytes!("../resources/codepoints_to_idx.dat"));

const CODEPOINTS: ArrayOf<U24> = ArrayOf::new(include_bytes!("../resources/codepoints.dat"));
const CATEGORIES: ArrayOf<Category> = ArrayOf::new(include_bytes!("../resources/categories.dat"));
const SUBCATEGORIES: ArrayOf<Option<Subcategory>> =
    ArrayOf::new(include_bytes!("../resources/subcategories.dat"));
const SCRIPTS: ArrayOf<Option<Script>> = ArrayOf::new(include_bytes!("../resources/scripts.dat"));

fn offset(offsets: &ArrayOf<U24>, target: &[u8], idx: usize) -> usize {
    // The last offset extends to EOF
    if idx == offsets.len() {
        return target.len();
    }
    offsets
        .get(idx)
        .unwrap_or_else(|| panic!("Asked for offset {idx} but we only have {}", offsets.len()))
}

fn name(idx: usize) -> &'static str {
    let start = offset(&NAME_OFFSETS, NAMES, idx);
    let end = offset(&NAME_OFFSETS, NAMES, idx + 1);
    // SAFETY: we only write ascii names in glyphs-reader/data/update.py
    unsafe { from_utf8_unchecked(&NAMES[start..end]) }
}

fn custom_prod_name(idx: usize) -> (ProductionName, usize) {
    let start = offset(&PROD_NAME_OFFSETS, PROD_NAMES, idx);
    let end = offset(&PROD_NAME_OFFSETS, PROD_NAMES, idx + 1);
    let (idx_slice, str_slice) = PROD_NAMES[start..end].split_at(3);
    let idx = U24::from_slice(idx_slice);
    let name = unsafe { from_utf8_unchecked(str_slice) };
    (ProductionName::Custom(SmolStr::new_static(name)), idx)
}

// PROD_NAME_OFFSETS/PROD_NAMES let us bsearch the index into the GlyphData for a given production name.
// The map below is for when we need to go in the other direction, i.e. look up the (optional)
// production name given a GlyphData index.  LazyLock ensures that this is only built once
// on first access, in a thread-safe way.
static REVERSE_PROD_NAMES: LazyLock<HashMap<usize, ProductionName>> = LazyLock::new(|| {
    let mut map = HashMap::new();
    for i in 0..PROD_NAME_OFFSETS.len() {
        let (name, idx) = custom_prod_name(i);
        map.insert(idx, name);
    }
    map
});

fn bsearch<T: Ord>(len: usize, needle: T, get: impl Fn(usize) -> (T, usize)) -> Option<usize> {
    let mut upper = len as i32 - 1;
    let mut lower = 0;
    while lower <= upper {
        let mid = ((lower + upper) / 2) as usize;
        let (c, i) = get(mid);
        match c.cmp(&needle) {
            Ordering::Equal => return Some(i),
            Ordering::Less => lower = mid as i32 + 1,
            Ordering::Greater => upper = mid as i32 - 1,
        }
    }
    None
}

fn has_predictable_prod_name(cp: u32) -> bool {
    // See if predictable name bit is set for this codepoint
    // Most production names work this way
    let i = (cp / 8) as usize;
    let bit = 1 << cp.rem_euclid(8);
    let bits = PROD_NAME_PREDICTABLE_BITMAP
        .get(i)
        .copied()
        .unwrap_or_default();
    bit & bits == bit
}

pub(crate) fn find_pos_by_prod_name(needle: ProductionName) -> Option<usize> {
    match needle {
        ProductionName::Bmp(cp) | ProductionName::NonBmp(cp) => {
            if has_predictable_prod_name(cp) {
                find_pos_by_codepoint(cp)
            } else {
                None
            }
        }
        ProductionName::Custom(..) => {
            // See if this matches against the (relatively small) set of names that break the basic patterns
            bsearch(PROD_NAME_OFFSETS.len(), needle, custom_prod_name)
        }
    }
}

pub(crate) fn find_pos_by_name(needle: &str) -> Option<usize> {
    bsearch(NAME_OFFSETS.len(), needle, |i| (name(i), i))
}

pub(crate) fn find_pos_by_codepoint(needle: u32) -> Option<usize> {
    bsearch(CODEPOINT_TO_INFO_IDX.len(), needle as usize, |i| {
        CODEPOINT_TO_INFO_IDX.get(i).unwrap()
    })
}

pub(crate) fn get(i: usize) -> Option<QueryResult> {
    if i >= NAMES.len() {
        return None;
    }
    let category = CATEGORIES
        .get(i)
        .unwrap_or_else(|| panic!("We have names[{i}] but not categories[{i}] ?!"));
    let subcategory = SUBCATEGORIES
        .get(i)
        .unwrap_or_else(|| panic!("We have names[{i}] but not subcategory[{i}] ?!"));
    let codepoint = CODEPOINTS
        .get(i)
        .unwrap_or_else(|| panic!("We have names[{i}] but not codepoints[{i}] ?!"));
    let script = SCRIPTS
        .get(i)
        .unwrap_or_else(|| panic!("We have names[{i}] but not scripts[{i}] ?!"));
    let codepoint = if codepoint > 0 {
        Some(codepoint as u32)
    } else {
        None
    };
    let production_name = match codepoint {
        Some(cp) if has_predictable_prod_name(cp) => Some(ProductionName::from(cp)),
        _ => REVERSE_PROD_NAMES.get(&i).cloned(),
    };

    Some(QueryResult {
        category,
        subcategory,
        codepoint,
        script,
        production_name,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    fn result_for_idx(i: usize) -> (&'static str, QueryResult) {
        (name(i), get(i).unwrap())
    }

    fn result_for_codepoint(cp: u32) -> (&'static str, QueryResult) {
        find_pos_by_codepoint(cp).map(result_for_idx).unwrap()
    }

    #[test]
    fn how_many_names() {
        assert!(
            NAME_OFFSETS.len() > 50000,
            "Oddly few names: {}",
            NAME_OFFSETS.len()
        );
    }

    #[test]
    fn bundled_lengths() {
        let num_names = NAME_OFFSETS.len();
        assert_eq!(
            (num_names, num_names),
            (CATEGORIES.len(), SUBCATEGORIES.len())
        )
    }

    #[test]
    fn wry_cat_by_codepoint() {
        assert_eq!(
            (
                "catFaceWithWrySmile",
                QueryResult {
                    category: Category::Symbol,
                    subcategory: Some(Subcategory::Emoji),
                    codepoint: Some(0x1F63C),
                    script: None,
                    production_name: Some("u1F63C".into()),
                }
            ),
            result_for_codepoint(0x1F63C)
        );
    }

    #[test]
    fn find_pos_by_name_empty() {
        assert_eq!(None, find_pos_by_name(" "));
    }

    #[test]
    fn find_pos_by_name_first() {
        assert_eq!(Some(0), find_pos_by_name(name(0)));
    }

    #[test]
    fn find_pos_by_name_last() {
        let last = NAME_OFFSETS.len() - 1;
        assert_eq!(Some(last), find_pos_by_name(name(last)));
    }

    #[test]
    fn find_pos_by_prod_name_bmp() {
        assert_eq!(
            (
                "quotedblbasereversed",
                QueryResult {
                    category: Category::Punctuation,
                    subcategory: Some(Subcategory::Quote),
                    codepoint: Some(0x2E42),
                    script: None,
                    production_name: Some("uni2E42".into()),
                }
            ),
            result_for_idx(find_pos_by_prod_name("uni2E42".into()).unwrap())
        );
    }

    #[test]
    fn find_pos_by_prod_name_non_bmp() {
        assert_eq!(
            (
                "tilde.tag",
                QueryResult {
                    category: Category::Symbol,
                    subcategory: Some(Subcategory::Format),
                    codepoint: Some(0xE007E),
                    script: None,
                    production_name: Some("uE007E".into()),
                }
            ),
            result_for_idx(find_pos_by_prod_name("uE007E".into()).unwrap())
        );
    }

    #[test]
    fn find_pos_by_prod_name_empty() {
        assert_eq!(None, find_pos_by_prod_name(" ".into()));
    }

    #[test]
    fn find_pos_by_prod_name_dotnull() {
        // .null is the first record with a prod name at time of writing
        assert_eq!(
            (
                ".null",
                QueryResult {
                    category: Category::Separator,
                    subcategory: None,
                    codepoint: None,
                    script: None,
                    production_name: Some(".null".into()),
                }
            ),
            result_for_idx(find_pos_by_prod_name(".null".into()).unwrap())
        );
    }

    #[test]
    fn find_pos_by_name_zzz() {
        // This was crashing
        find_pos_by_name("zzz");
    }

    #[test]
    fn find_pos_by_prod_name_zzz() {
        // This was crashing
        find_pos_by_prod_name("zzz".into());
    }

    #[test]
    fn find_pos_by_prod_name_multi_codepoint() {
        // .null is the first record with a prod name at time of writing
        assert_eq!(
            (
                "AndorraFlag",
                QueryResult {
                    category: Category::Symbol,
                    subcategory: Some(Subcategory::Emoji),
                    codepoint: None,
                    script: None,
                    production_name: Some("u1F1E61F1E9".into()),
                }
            ),
            result_for_idx(find_pos_by_prod_name("u1F1E61F1E9".into()).unwrap())
        );
    }

    #[test]
    fn script() {
        assert_eq!(
            vec![None, Some(Script::Latin), Some(Script::Elbasan)],
            vec!['>' as u32, 'A' as u32, 0x10527]
                .into_iter()
                .map(result_for_codepoint)
                .map(|r| r.1.script)
                .collect::<Vec<_>>()
        )
    }
}
