//! Accessors for bundled glyphsLib data

use std::{cmp::Ordering, str::from_utf8_unchecked};

use crate::glyphdata::{Category, QueryResult, Subcategory};

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

// T is a data member because const PhantomData didn't cseem to want to cooperate
struct ArrayOf<T: BundledEntry>(&'static [u8], T);

impl<T> ArrayOf<T>
where
    T: BundledEntry,
{
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

const NAME_OFFSETS: ArrayOf<U24> = ArrayOf(include_bytes!("../resources/name_offsets.dat"), 0);
const NAMES: &[u8] = include_bytes!("../resources/names.dat");

const CODEPOINT_TO_INFO_IDX: ArrayOf<(U24, U24)> =
    ArrayOf(include_bytes!("../resources/codepoints_to_idx.dat"), (0, 0));

const CODEPOINTS: ArrayOf<U24> = ArrayOf(include_bytes!("../resources/codepoints.dat"), 0);
const CATEGORIES: ArrayOf<Category> = ArrayOf(
    include_bytes!("../resources/categories.dat"),
    Category::Other,
);
const SUBCATEGORIES: ArrayOf<Option<Subcategory>> =
    ArrayOf(include_bytes!("../resources/subcategories.dat"), None);

fn name_offset(idx: usize) -> usize {
    // The last offset extends to EOF
    if idx == NAME_OFFSETS.len() {
        return NAMES.len();
    }
    NAME_OFFSETS.get(idx).unwrap()
}

fn name(idx: usize) -> &'static str {
    let start = name_offset(idx);
    let end = name_offset(idx + 1);
    // SAFETY: we only write ascii names in glyphs-reader/data/update.py
    unsafe { from_utf8_unchecked(&NAMES[start..end]) }
}

pub(crate) fn find_pos_by_name(needle: &str) -> Option<usize> {
    let mut upper = NAME_OFFSETS.len();
    let mut lower = 0;
    while lower <= upper {
        let mid = (lower + upper) / 2;
        let c = name(mid).cmp(needle);
        match c {
            Ordering::Equal => return Some(mid),
            Ordering::Less => lower = mid + 1,
            Ordering::Greater => upper = mid - 1,
        }
    }
    None
}

pub(crate) fn find_pos_by_codepoint(needle: u32) -> Option<usize> {
    let mut upper = CODEPOINT_TO_INFO_IDX.len();
    let mut lower = 0;
    while lower <= upper {
        let mid = (lower + upper) / 2;
        let (cp, i) = CODEPOINT_TO_INFO_IDX.get(mid).unwrap();
        let c = cp.cmp(&(needle as usize));
        match c {
            Ordering::Equal => return Some(i),
            Ordering::Less => lower = mid + 1,
            Ordering::Greater => upper = mid - 1,
        }
    }
    None
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
    let codepoint = if codepoint > 0 {
        Some(codepoint as u32)
    } else {
        None
    };

    Some(QueryResult {
        category,
        subcategory,
        codepoint,
    })
}

#[cfg(test)]
mod tests {
    use super::*;

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
                }
            ),
            find_pos_by_codepoint(0x1F63C)
                .map(|i| (name(i), get(i).unwrap()))
                .unwrap()
        );
    }
}
